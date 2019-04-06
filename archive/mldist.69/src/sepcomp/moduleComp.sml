(* Copyright 1989 by AT&T Bell Laboratories *)
(* Modules compiler for New Jersey ML.   Nick Rothwell, LFCS, January 1989,
   revised and simplified by Andrew Appel, 1990. *)

functor ModuleComp(structure Machm: CODEGENERATOR): MODULE_COMPILER =
struct
  type lvar = Access.lvar and code = string

  local open Lambda
  in fun fn_N(lvars, exp) =
      let val v = Access.mkLvar()
          fun doit(lv::rest,n) = APP(FN(lv,doit(rest,n+1)), SELECT(n, VAR v))
            | doit(nil, _) = exp
       in FN(v, doit(lvars, 0))
      end

     fun let_N(lvars, exp1) exp2 = APP(fn_N(lvars, exp2), exp1)
  end

  datatype CodeDynModule = CODE of {self: code, subModules: CodeDynModule list}
    (* The code is for a function: "looker -> (M1, ..., Mn) -> array*rubbish"
      		   where each Mi is an "array*rubbish" *)

  type LambDynModule = {lambda:  Lambda.lexp -> Lambda.lexp,
      		  	imports:  CodeDynModule list,
      		  	importLvars: lvar list,
      		  	lvars: lvar list}

  exception MkLambda
  fun mkLambda(env,source, dec, lvars) =
       {lambda=Translate.transDec env source dec,
		imports=nil, importLvars=nil, lvars = lvars}
	before (if !(#anyErrors source) then raise MkLambda else ())

  fun importDynModule({lambda=lam1,imports=im1,importLvars=imL1,lvars=vl1},
      		      {lambda=lam2,imports=im2,importLvars=imL2,lvars=vl2}) =
       {lambda= lam2 o lam1, imports= im1 @ im2, importLvars= imL1 @ imL2,
	lvars = vl1 @ vl2}

  fun abstractDynModule(codeDynModule, lvars) =
          let val modLvar = Access.mkLvar()
           in {lambda=let_N(lvars, Lambda.VAR modLvar),
	       imports=[codeDynModule], importLvars = [modLvar], lvars = lvars}
          end

   (* compiler: takes a LambDynModule to a CodeDynModule. We tie together
      all the lambdas as a set of nested LET declarations. Where we import
      already-compiled modules, these are abstracted to be arguments to the
      final lambda expression. *)

   exception CompileDynModule
   fun compileDynModule {lambda,imports,importLvars,lvars} =
      let val openLambda = lambda(Lambda.RECORD(map Lambda.VAR lvars))
          val lambda' = fn_N(importLvars, openLambda)
          val finalLambda = Opt.closetop(lambda', ProcessFile.getCore())
          val mash = Convert.convert o Reorder.reorder
          val code = Machm.generate(mash finalLambda, NONE,
      	  	       fn _ => fn s => 
      			(print("Real constant out of range: "^s^"\n");
      		 	 raise CompileDynModule))
       in CODE{self=code, subModules=imports}
      end

    type looker = lvar -> System.Unsafe.object
    type result = System.Unsafe.object array

    fun executeDynModule lookup (CODE{self, subModules}) : result =
      let val me: looker -> result array -> result = System.Unsafe.boot self
       in me lookup (arrayoflist(map (executeDynModule lookup) subModules))
      end
end
