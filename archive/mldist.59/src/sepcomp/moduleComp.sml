(* Copyright 1989 by AT&T Bell Laboratories *)
(* Modules compiler for New Jersey ML.   Nick Rothwell, LFCS, January 1989. *)

functor ModuleComp(structure Absyn: BAREABSYN
		     sharing Absyn = BareAbsyn
		   structure Lambda: LAMBDA
	           structure Opt: OPT sharing Opt.L = Lambda
		   structure Machm: CODEGENERATOR
		  ): MODULE_COMPILER =
  struct
    structure Lambda = Lambda
    structure Absyn = Absyn

    type lvar = Access.lvar
    type code = string

    abstype LambDynModule =
	    LAMBDYNMODULE of {entry: Entry, lvars_R: lvar list} list
			(* The lvars are in reverse order of declaration,
			   hence `lvars_R'. *)
	and CodeDynModule =
	    CODEDYNMODULE of {self: code, subModules: CodeDynModule list}
			(* The code is for a function:
			   "(M1, ..., Mn) -> looker -> array*rubbish"
			   where each Mi is a "looker -> array*rubbish" *)

	and Entry = LAMBDAentry of Lambda.lexp -> Lambda.lexp	(* unclosed *)
		  | IMPORTentry of CodeDynModule
    with
      val emptyDynModule = LAMBDYNMODULE []

      exception AddDeclaration
      fun addDeclaration(source, dec, lvars, LAMBDYNMODULE entries) =
        let
	  val newEntry =
	    {entry=LAMBDAentry(Translate.transDec source dec), lvars_R=rev lvars}
        in if !(#anyErrors source) 
	    then raise AddDeclaration
	    else LAMBDYNMODULE(newEntry :: entries)
	end

      fun importDynModule(LAMBDYNMODULE inner, LAMBDYNMODULE outer) =
        LAMBDYNMODULE(inner @ outer)

      fun abstractDynModule(codeDynModule, lvars) =
        LAMBDYNMODULE [{entry=IMPORTentry codeDynModule, lvars_R=rev lvars}]

     (* compiler: takes a LambDynModule to a CodeDynModule. We tie together
        all the lambdas as a set of nested LET declarations. Where we import
	already-compiled modules, these are abstracted to be arguments to the
	final lambda expression. *)

     (* Some local lambda-building stuff. This should probably go in a wee
        structure of its own. *)

      local
        open Lambda
      in
        fun apply(lvar1, lvar2) = APP(VAR lvar1, VAR lvar2)

	fun let_1(lvar, exp1, exp2) = APP(FN(lvar, exp2), exp1)

        fun fn_N(name, lvars, exp) =
	  let
	    val vecLvar = Access.namedLvar name
	    val vecExp = VAR vecLvar
	    fun doit(lv :: rest, n) =
	          let_1(lv, SELECT(n, vecExp), doit(rest, n+1))
	      | doit(nil, _) = exp
	  in
	    FN(vecLvar, doit(lvars, 0))
	  end

        fun let_N(name, lvars, exp1, exp2) = APP(fn_N(name, lvars, exp2), exp1)

	fun record lvars = RECORD(map VAR lvars)
      end

     (* foldLambda: takes a list of entries (assumed to be in same order
	as the actual module declarations), and generates one big lambda.
	We close it later. foldLambda returns
	{lambda, imports, importLvars}. *)

      val importSym = Symbol.symbol "importModule"
      val importEntrySym = Symbol.symbol "importEntry"
      fun foldLambda({entry=LAMBDAentry thisFn, lvars_R} :: rest,
		     looker, allVars_R
		    ) =
	    let
	      val {lambda=next, imports, importLvars} =
		foldLambda(rest, looker, lvars_R @ allVars_R)
	    in
	      {lambda=thisFn next,	(* apply this fn->lambda to the
					   argument (that's how transDec
					   works...) *)
	       imports=imports,
	       importLvars=importLvars
	      }
	    end

	| foldLambda({entry=IMPORTentry subMod, lvars_R} :: rest,
		     looker, allVars_R
		    ) =
	    let
	      val modLvar = Access.namedLvar importSym
	      val {lambda=next, imports, importLvars} =
		foldLambda(rest, looker, lvars_R @ allVars_R)
	    in
	      {lambda=let_N(importEntrySym, rev lvars_R,
			    apply(modLvar,looker), next
			   ),
	       imports=subMod :: imports,
	       importLvars = modLvar :: importLvars
	      }
	    end

	| foldLambda(nil, _, allVars_R) =
	    {lambda=record(rev allVars_R), imports=nil, importLvars=nil}
			(* Make sure the lvars comprising the eventual 
			   record are the right way round!! *)

      exception CompileDynModule
      val lookerSym = Symbol.symbol "looker" 
      and importsSym = Symbol.symbol "imports"
      fun compileDynModule opt (LAMBDYNMODULE entries): CodeDynModule =
	let
	  val looker = Access.namedLvar lookerSym

	  val {lambda=openLambda, imports, importLvars} =
	    foldLambda(rev entries, looker, nil)
		(* reverse "entries" because a LambDynModule has the most
		   recent declaration at the front, and we want to nest the
		   lambda with the old stuff outermost - as well as get the
		   correct ordering of the lvars in the final result. *)

	  val closedLambda =
	    Opt.bareCloseTop{lambda=openLambda,
	    		     looker=looker,
			     extras=ProcessFile.getCore(),
			     keepFree=looker :: importLvars
			    }
		(* Close the lambda, using the already-generated looker lvar -
		   this had to be generated in advance, since each imported
		   module is activated as M(looker). We DON'T want to close
		   with respect to the modLvars, since we're just about to
		   lambda-bind these. Keep the looker's lvar free as well,
		   otherwise it tries to find itself... Gnnnh! *)

	  val finalLambda = fn_N(importsSym, importLvars, closedLambda)
		(* That's what we want: "fn (M1, ..., Mn) => fn looker => ..."
		   where the Mi's we apply to are each a "fn looker => ...".
		   In fact, I suspect that "imports" and "importLvars" are
		   both in reverse order, but as long as they're
		   coherent... *)

	  val code = Machm.generate(opt finalLambda,
				fn _ => fn s => 
				(print("Real constant out of range: "^s^"\n");
			 	 raise CompileDynModule))
	in
	  CODEDYNMODULE{self=code, subModules=imports}
	end

     (* executeDynModule: returns a "fn lookup => ...result...". If the module
        doesn't import anything, then we just apply the code to NIL. If the
	module imports M1...Mn, then we prime these, and build an array of
	them. *)

      type object = System.Unsafe.object
      type looker = lvar -> object
      type result = object array
      type module = looker -> result

      fun executeDynModule(CODEDYNMODULE{self, subModules}): module =
        let
	  val me: module array -> looker -> result = System.Unsafe.boot self
	  val subModules = arrayoflist(map executeDynModule subModules)
	in me subModules
	end

    end
  end;
