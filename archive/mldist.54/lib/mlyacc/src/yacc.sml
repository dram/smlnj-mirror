structure ParseGen  : sig
		   val parseGen : string -> unit
		 end  =
  struct
    open Grammar
    structure MakeTable = mkMakeTable(structure IntGrammar=IntGrammar
				      structure LrTable = LrTable)
    structure Verbose = mkVerbose(structure Errs = MakeTable.Errs)
    structure PrintStruct = mkMakeStruct(structure LrTable = MakeTable.LrTable)
    open Header

    (* approx. maximum length of a line *)

    val lineLength = 70

    (* record type describing names of structures in the program being
 	generated *)

    datatype names = NAMES 
			of {miscStruct : string,  (* Misc{n} struct name *)
			    tableStruct : string, (* LR table structure *)
			    tokenStruct : string, (* Tokens{n} struct name *)
			    actionsStruct : string, (* Actions structure *)
			    valueStruct: string, (* semantic value structure *)
			    ecStruct : string,  (* error correction structure *)
			    arg: string, (* user argument for parser *)
			    tokenSig : string,  (* TOKENS{n} signature *)
			    miscSig :string, (* Signature for Misc structure *)
			    dataStruct:string, (* name of structure in Misc *)
						(* which holds parser data *)
			    dataSig:string (* signature for this structure *)
					
		 	    }

    (* common functions and values used in printing out program *)

    datatype values = VALS
		      of {say : string -> unit,
			  saydot : string -> unit,
			  sayln : string -> unit,
			  pureActions: bool,
			  pos_type : string,
			  arg_type : string,
			  ntvoid : string,
			  termvoid : string,
			  start : Grammar.nonterm,
			  hasType : Grammar.symbol -> bool,

			  (* actual (user) name of terminal *)

			  termToString : Grammar.term -> string,
			  symbolToString : Grammar.symbol -> string,

			  (* type symbol comes from the HDR structure,
			     and equals  (string * int) right now *)

			  term : (symbol * ty) list,
			  nonterm : (symbol * ty) list,
			  terms : Grammar.term list}
			  
    structure SymbolHash = Hash(type elem = string
	    		        val gt = (op >) : string*string -> bool)

    structure TermTable = Table(type key = Grammar.term
				val gt = fn (T i,T j) => i > j)

    structure SymbolTable = Table(
	type key = Grammar.symbol
	val gt = fn (TERM(T i),TERM(T j)) => i>j
		  | (NONTERM(NT i),NONTERM(NT j)) => i>j
		  | (NONTERM _,TERM _) => true
		  | (TERM _,NONTERM _) => false)

    (* printTypes: function to print the following types in the LrValues
       structure and a structure containing the datatype svalue:

		type svalue -- it holds semantic values on the parse
				   stack
		type pos -- the type of line numbers
		type result -- the type of the value that results
				   from the parse

	The type svalue is set equal to the datatype svalue declared
	in the structure named by valueStruct.  The datatype svalue
	is declared inside the structure named by valueStruct to deal
	with the scope of constructors.
    *)

    val printTypes = fn (VALS {say,sayln,term,nonterm,symbolToString,pos_type,
				 arg_type,
				 termvoid,ntvoid,saydot,hasType,start,
				 pureActions,...},
			   NAMES {valueStruct,...},symbolType) =>
     let val prConstr = fn ((name,_),SOME s) => 
			   say (" | " ^ name ^ " of " ^
			          (if pureActions then "" else "unit -> ") ^
				" (" ^ s ^ ")"
				)
			 | _ => ()
     in sayln "local open Header in";
	sayln ("type pos = " ^ pos_type);
	sayln ("type arg = " ^ arg_type);
	sayln ("structure " ^ valueStruct ^ " = ");
	sayln "struct";
	say ("datatype svalue = " ^ termvoid ^ " | " ^ ntvoid ^ " of" ^
	     (if pureActions then "" else " unit -> ") ^ " unit");
	app prConstr term;
	app prConstr nonterm;
	sayln "\nend";
	sayln ("type svalue = " ^ valueStruct ^ ".svalue");
	say "type result = ";
	case symbolType (NONTERM start)
	of NONE => sayln "unit"
	 | SOME t => (say t; sayln "");
	sayln "end"
    end

     (* function to print Tokens{n} structure *)

    val printTokenStruct =
     fn (VALS {say, sayln, termToString, hasType,termvoid,terms,
	       pureActions,...},
	 NAMES {miscStruct,tableStruct,valueStruct,
		tokenStruct,tokenSig,dataStruct,...}) =>
		(sayln ("structure " ^ tokenStruct ^ " : " ^ tokenSig ^ " =");
		 sayln "struct";
	         sayln ("type svalue = " ^ dataStruct ^ ".svalue");
		 sayln "type ('a,'b) token = ('a,'b) Token.token";
		 let val f = fn term as T i =>
			(say "fun "; say (termToString term);
			 say " (";
		         if (hasType (TERM term)) then say "i," else ();
			 say "p1,p2) = Token.TOKEN (";
			 say (dataStruct ^ "." ^ tableStruct ^ ".T ");
			 say (makestring i);
			 say ",(";
			 say (dataStruct ^ "." ^ valueStruct ^ ".");
			 if (hasType (TERM term)) then 
			    (say (termToString term);
			     if pureActions then say " i"
			     else say " (fn () => i)")
			 else say termvoid;
			 say ",";
			 sayln "p1,p2))")
		in app f terms
		end;
		sayln "end")
			 
    (* function to print signatures out - takes print function which
	does not need to insert line breaks *)

    val printSigs = fn (VALS {term,...},
			NAMES {tokenSig,tokenStruct,miscSig,
				dataStruct, dataSig, ...},
			say) =>
		(say "signature ";
		 say tokenSig;
		 say " =\n";
		 say "sig\n";
		 say	"type ('a,'b) token\n\
			\type svalue\n";
		 app (fn ((s,_),ty) => (say "val "; say s; say ": (";
					case ty
					of NONE => () 
					 | SOME l => (say "(";
						      say l;
						      say ") * ");
					say "'a * 'a) ->";
					say "(svalue,'a) token\n"
				       )) term;
		 say "end\n";
		 say "signature ";
		 say miscSig;
		 say " =\n";
		 say "sig\n";
		 say "structure ";
		 say ("Tokens : " ^ tokenSig ^ "\n");
		 say ("structure " ^ dataStruct ^ ":" ^ dataSig ^ "\n");
		 say ("sharing type " ^ dataStruct ^ ".Token.token = Tokens.token\n");
		 say ("sharing type " ^ dataStruct ^
				 ".svalue = Tokens.svalue\n");
		 say "end\n")
		
    (* function to print structure for error correction *)

    val printEC = fn (keyword : term list,
		      preferred : term list,
		      subst : (term * term) list,
		      noshift : term list,
		      value : (term * string) list,
		      VALS {termToString, say,sayln,terms,saydot,hasType,
			    termvoid,pureActions,...},
		      NAMES {ecStruct,tableStruct,valueStruct,...}) =>
       let

	 (* subst is a list of pairs (sym,sym'), where sym is a preferred
	    substitution for sym'.  Construct a list of (sym,[..syms...]) where
	    the elements of the list are all the preferred substitions for
	    the sym *)
	
	 val subst =
	   let fun f ((sym,sym'),table) =
		case TermTable.find(sym',table)
		  of SOME l => TermTable.insert((sym',sym :: l),table)
		   | NONE => TermTable.insert((sym',[sym]),table)
	   in TermTable.make_list (fold f subst TermTable.empty)
	   end

	 val sayterm = fn (T i) => (say "(T "; say (makestring i); say ")")

	 val printBoolCase = fn ( l : term list) =>
	    (say "fn ";
	     app (fn t => (sayterm t; say " => true"; say " | ")) l;
	     sayln "_ => false")

	 val printTermList = fn (l : term list) =>
	    (app (fn t => (sayterm t; say " :: ")) l; sayln "nil")

	 val printSubst = fn (l : (term * (term list)) list) =>
	    (sayln "val preferred_subst =";
	     say "fn ";
	     app (fn (t,l') =>
		    (sayterm t; say " =>";
		     app (fn t => (sayterm t; say "::")) l';
		     sayln "nil"; say "|"
		    )
		 ) l;
	     sayln " _ => nil")

	 val printErrValues = fn (l : (term * string) list) =>
	    (sayln "val errtermvalue=";
	     sayln "let open Header in";
	     say "fn ";
	     app (fn (t,s) =>
		    (sayterm t; say " => ";
		     saydot valueStruct; say (termToString t);
		     say "(";
		     if pureActions then () else say "fn () => ";
		     say "("; say s; say "))";
		     sayln " | "
		    )
		 ) l;
	    say "_ => ";
	    say (valueStruct ^ ".");
	    sayln termvoid; sayln "end")
	      

	  val printNames = fn () =>
		let val f = fn term =>
			 (sayterm term; say " => "; say "\"";
			  say (termToString term); sayln "\""; say "  | ")
		in (sayln "val showTerminal =";
		    say "fn ";
		    app f terms;
		    sayln "_ => \"bogus-term\"")
		end

	   val ecTerms = 
		List.fold (fn (t,r) =>
		  if hasType (TERM t) orelse exists (fn (a,_)=>a=t) value
		    then r
		    else t::r)
		terms nil
				  
	in  say "structure ";
	    say ecStruct;
	    sayln "=";
	    sayln "struct";
	    say "open ";
	    sayln tableStruct;
	    sayln "val is_keyword =";
	    printBoolCase keyword;
	    sayln "val preferred_insert =";
	    printBoolCase preferred;
	    printSubst subst;
	    sayln "val noShift = ";
	    printBoolCase noshift;
	    printNames ();
	    printErrValues value;
	    say "val terms = ";
	    printTermList ecTerms;
	    sayln "end"
	end

    val printAction = fn (rules,
			  VALS {hasType,say,sayln,termvoid,ntvoid,
			        symbolToString,saydot,start,pureActions,...},
			  NAMES {actionsStruct,valueStruct,tableStruct,arg,...}) =>
	 let val is_nonterm = fn (NONTERM i) => true | _ => false
	     val numberRhs = fn r =>
		List.revfold (fn (e,(r,table)) =>
			let val num = case SymbolTable.find(e,table)
				       of SOME i => i
					| NONE => 1
			 in ((e,num,hasType e orelse is_nonterm e)::r,
			     SymbolTable.insert((e,num+1),table))
			 end) r (nil,SymbolTable.empty)

	     val saySym = say o symbolToString

	     val printCase = fn (i:int, r as {lhs=lhs as (NT lhsNum),prec,
				        rhs,code,rulenum}) =>
		(say "("; say(makestring i); say ",";

		   (* prToken: print an argument *)

		 let val prToken = fn (sym,num : int,typed) =>
		 	let val symString = symbolToString sym
			    val symNum = symString ^ (makestring num)
			in say "(_,(";
			   if not (hasType sym) then
			      (if is_nonterm sym then
				   (saydot valueStruct;
				    say ntvoid; say  " "; say symNum)
			      else say "_")
			   else	
			       (saydot valueStruct; say symString;
				say " ("; say symNum;
			         if num=1 andalso pureActions
				     then say (" as " ^ symString)
				 else ();
				 say ")"
				);
			   say ",";
			   if num=1 then (say (symString ^ "left as "))
				    else ();
			   say (symNum ^ "left");
			   say ",";
			   if num=1 then (say (symString ^ "right as "))
				    else ();
			   say (symNum ^ "right");
			   say ")) :: "
		        end
		    val (numberedRhs,_) = numberRhs rhs

		    (* print arguments *)

		    val _ = (app prToken numberedRhs; sayln "rest671) =>")

		    (* remove terminals in argument list w/o types *)

		    val argsWithTypes =
			  fold (fn ((_,_,false),r) => r
				 | (s as (_,_,true),r) => s::r) numberedRhs nil

		    val prBody =
			(sayln "let val result = ";
			 saydot valueStruct;
			 if hasType (NONTERM lhs)
			    then saySym (NONTERM lhs)
			    else say ntvoid;
			 say " (";
			 if pureActions then 
				(say "(("; say code; say ")")
			 else if argsWithTypes = nil then
				(say " fn () => (("; say code; say ")")
			 else
 				 (sayln "fn () => (let ";
				  revapp (fn (sym,num : int,_) =>
				 let val symString = symbolToString sym
				     val symNum = symString ^ (makestring num)
				 in (say "val ";
				     if num=1 then (say symString; say " as ")
				     else ();
				     say symNum;
				     say " = ";
				     say symNum;
				     sayln "()")
				 end) argsWithTypes;
				 say " in ("; say code; say ") end "
				);
			  if hasType (NONTERM lhs) 
			      then ()
			      else say "; ()";
			 sayln "))";
			 say "in ("; say (tableStruct ^ ".NT ");
			 say (makestring lhsNum);
			 say ",(result,";
			 case rhs
			  of nil => say "defaultPos,defaultPos"
			   | r => let val (rsym,rnum,_) = hd(numberedRhs)
				      val (lsym,lnum,_) = hd(rev numberedRhs)
				  in say (symbolToString lsym ^
					  (makestring lnum) ^ "left,");
				     say (symbolToString rsym ^
					  (makestring rnum) ^ "right")
 				  end;
			 sayln "),rest671)";
			 sayln "end")
		in ()
		end)
	  val prRules = fn () =>
	     (sayln "fn (i392,defaultPos,stack,";
	      say   "    ("; say arg; sayln "):arg) =>";
	      sayln "case (i392,stack)";
	      say "of ";
	      app (fn (rule as {rulenum,...}) =>
		   (printCase(rulenum,rule); say "| ")) rules;
	     sayln "_ => raise (mlyAction i392)")

   	in say "structure ";
	   say actionsStruct;
	   sayln " =";
	   sayln "struct ";
	   sayln "exception mlyAction of int";
	   sayln "val actions = ";
	   sayln "let open Header";
	   sayln "in";
	   prRules();
	   sayln "end";
	   say "val void = ";
	   saydot valueStruct;
	   sayln termvoid;
	   say "val extract = ";
	   say "fn a => (fn ";
	   saydot valueStruct;
	   if hasType (NONTERM start)
	      then say (symbolToString (NONTERM start))
	      else say "ntVOID";
	   sayln " x => x";
	   sayln "| _ => let exception ParseInternal";
	   say "\tin raise ParseInternal end) a ";
	   sayln (if pureActions then "" else "()");
	   sayln "end"
	end

    val make_parser = fn ((header,
	 {eop,prefer,keyword,nonterm,prec, subst,
	  term, control,value} : declData,
	  rules : rule list),spec) =>
     let
	val verbose = List.exists (fn a=>a=VERBOSE) control
	val defaultReductions = not (List.exists (fn a=>a=NODEFAULT) control)
	val pos_type =
	   let fun f nil = NONE
		 | f ((POS s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val start =
	   let fun f nil = NONE
		 | f ((START_SYM s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val name =
	   let fun f nil = NONE
		 | f ((PARSER_NAME s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val header_decl =
	   let fun f nil = NONE
		 | f ((FUNCTOR s)::r) = SOME s 
		 | f (_::r) = f r
	   in f control
	   end
	val arg_decl =
	   let fun f nil = ("()","unit")
		 | f ((PARSE_ARG s)::r) = s 
		 | f (_::r) = f r
	   in f control
	   end

	val noshift =
	   let fun f nil = nil
		 | f ((NSHIFT s)::r) = s 
		 | f (_::r) = f r
	   in f control
	   end

	val pureActions =
	   let fun f nil = false
		 | f ((PURE)::r) = true 
		 | f (_::r) = f r
	   in f control
	   end

	val term =
	 case term
	   of NONE => (cerror "missing %term definition"; nil)
	    | SOME l => l

	val nonterm =
	 case nonterm
	  of NONE => (cerror "missing %nonterm definition"; nil)
	   | SOME l => l

	val pos_type =
	 case pos_type
	  of NONE => (cerror "missing %pos definition"; "")
	   | SOME l => l


	val termHash = 
	  fold (fn (((name,line),_),table) =>
		if SymbolHash.exists(name,table) then
		   (error ("duplicate definition of " ^ name ^ " in %term") line;
		    table)
		else SymbolHash.add(name,table)) term SymbolHash.empty

	val isTerm = fn name => SymbolHash.exists(name,termHash)

	val symbolHash = 
	  fold (fn (((name,line),_),table) =>
	     if SymbolHash.exists(name,table) then
	       (if (isTerm name) then
		  (error (name ^ " is defined as a terminal and a nonterminal")
		   line; table)
		else 
		  (error ("duplicate definition of " ^ name ^ " in %nonterm")
		   line; table)
		)
	     else SymbolHash.add(name,table)) nonterm termHash

	fun makeUniqueId s =
		if SymbolHash.exists(s,symbolHash) then makeUniqueId (s ^ "'")
		else s

	val _ = if (!errflag) then raise SemanticError else ()

	val numTerms = SymbolHash.size termHash
	val numNonterms = SymbolHash.size symbolHash - numTerms

	val symError = fn sym => fn err => fn (name,line) =>
	    error (name ^ " in " ^ err ^ " is not defined as a " ^ sym) line

	val termNum : string -> symbol -> term =
	  let val termError = symError "terminal" 
	  in fn stmt =>
	     let val stmtError = termError stmt
	     in fn symbol as (name,_) =>
	        case SymbolHash.find(name,symbolHash)
	        of NONE => (stmtError symbol; T ~1)
	         | SOME i => T (if i<numTerms then i
			        else (stmtError symbol; ~1))
	     end
	  end
			
	val nontermNum : string -> symbol -> nonterm =
	  let val nontermError = symError "nonterminal" 
	  in fn stmt =>
	     let val stmtError = nontermError stmt
	     in fn symbol as (name,_) =>
	        case SymbolHash.find(name,symbolHash)
	        of NONE => (stmtError symbol; NT ~1)
	         | SOME i => if i>=numTerms then NT (i-numTerms)
			     else (stmtError symbol;NT ~1)
	     end
	  end

	val symbolNum : string -> symbol -> Grammar.symbol =
	  let val symbolError = symError "symbol" 
	  in fn stmt =>
	     let val stmtError = symbolError stmt
	     in fn symbol as (name,_) =>
	        case SymbolHash.find(name,symbolHash)
	        of NONE => (stmtError symbol; NONTERM (NT ~1))
	         | SOME i => if i>=numTerms then NONTERM(NT (i-numTerms))
			     else TERM(T i)
	     end
	  end

(* map all symbols in the following values to terminals and check that
   the symbols are defined as terminals:

	eop : symbol list
	keyword: symbol list
	prefer: symbol list
	prec: (lexvalue * (symbol list)) list
	subst: (symbol * symbol) list
*)

	val eop = map (termNum "%eop") eop
	val keyword = map (termNum "%keyword") keyword
	val prefer = map (termNum "%prefer") prefer
	val prec = map (fn (a,l) => 
			(a,case a
			   of LEFT => map (termNum "%left") l
			    | RIGHT => map (termNum "%right") l
			    | NONASSOC => map (termNum "%nonassoc") l
			)) prec
	val subst =
	 let val mapTerm = termNum "%subst"
	 in map (fn (a,b) => (mapTerm a,mapTerm b)) subst
	 end
	val noshift = map (termNum "%noshift") noshift
	val value =
	  let val mapTerm = termNum "%value"
	  in map (fn (a,b) => (mapTerm a,b)) value
	  end
	val (rules,_) =
	   let val symbolNum = symbolNum "rule"
	       val nontermNum = nontermNum "rule"
	       val termNum = termNum "%prec tag"
           in List.fold
	   (fn ({lhs,rhs,code,prec},(l,n)) =>
	     ( {lhs=nontermNum lhs,rhs=map symbolNum rhs,
	        code=code,prec=case prec
				of NONE => NONE
				 | SOME t => SOME (termNum t),
		 rulenum=n}::l,n-1))
		 rules (nil,length rules-1)
	end

	val _ = if (!errflag) then raise SemanticError else ()

	(* termToString: map terminals back to strings *)

	val termToString =
	   let val data = array(numTerms,"")
	       val unmap = fn ((name,_),_) =>
			update(data,case SymbolHash.find(name,symbolHash)
				    of SOME i => i,name)
	       val _ = app unmap term
	   in fn T i =>
		if DEBUG andalso (i<0 orelse i>=numTerms)
		  then "bogus-num" ^ (makestring i)
		  else data sub i
	   end

	val nontermToString = 
	   let val data = array(numNonterms,"")
	       val unmap = fn ((name,_),_) =>
			update(data,case SymbolHash.find(name,symbolHash)
				    of SOME i => i-numTerms,name)
	       val _ = app unmap nonterm
	   in fn NT i =>
		if DEBUG andalso (i<0 orelse i>=numNonterms)
		  then "bogus-num" ^ (makestring i)
		  else data sub i
	   end

(* create functions mapping terminals to precedence numbers and rules to
   precedence numbers.

  Precedence statements are listed in order of ascending (tighter binding)
  precedence in the specification.   We receive a list composed of pairs
  containing the kind of precedence (left,right, or assoc) and a list of
  terminals associated with that precedence.  The list has the same order as
  the corresponding declarations did in the specification.

  Internally, a tighter binding has a higher precedence number.  We give
  precedences using multiples of 3:

		p+2 = right associative (force shift of symbol)
		p+1 = precedence for rule
		p = left associative (force reduction of rule)

  Nonassociative terminals are given also given a precedence of p+1.  The
table generator detects when the associativity of a nonassociative terminal
is being used to resolve a shift/reduce conflict by checking if the
precedences of the rule and the terminal are equal.

  A rule is given the precedence of its rightmost terminal *)

	val termPrec =
	    let val precData = array(numTerms, NONE : int option)
	        val addPrec = fn termPrec => fn term as (T i) =>
		   case precData sub i
		   of SOME _ =>
		     cerror("multiple precedences specified for terminal " ^
			    (termToString term))
		    | NONE => update(precData,i,termPrec)
		val termPrec = fn ((LEFT,_) ,i) => i
			      | ((RIGHT,_),i) => i+2
			      | ((NONASSOC,l),i) => i+1
		val _ = revfold (fn (args as ((_,l),i)) =>
			        (app (addPrec (SOME (termPrec args))) l; i+3))
			  prec 0
	   in fn (T i) =>
		if  DEBUG andalso (i < 0 orelse i >= numTerms) then
			NONE
		else precData sub i
	   end

	val rulePrec = 
	   let fun findRightTerm (nil,r) = r
	         | findRightTerm (TERM t :: tail,r) =
				 findRightTerm(tail,SOME t)
		 | findRightTerm (_ :: tail,r) = findRightTerm(tail,r)
	   in fn rhs =>
		 case findRightTerm(rhs,NONE)
		 of NONE => NONE
		  | SOME term => 
		       case termPrec term
		       of SOME i => SOME (i - (i mod 3) + 1)
		        | a => a
	   end

	val grammarRules =
	  let val conv = fn {lhs,rhs,code,prec,rulenum} =>
		{lhs=lhs,rhs =rhs,precedence=
			case prec
			  of SOME t => termPrec t
			   | _ => rulePrec rhs,
	         rulenum=rulenum}
	  in map conv rules
	  end

    (* get start symbol *)

	val start =
	 case start
	   of NONE => #lhs (hd grammarRules)
	    | SOME name => 
		nontermNum "%start" name

	val symbolType = 
	   let val data = array(numTerms+numNonterms,NONE : ty)
	       val unmap = fn ((name,_),ty) =>
			update(data,case SymbolHash.find(name,symbolHash)
				    of SOME i => i,ty)
	       val _ = (app unmap term; app unmap nonterm)
	   in fn NONTERM(NT i) =>
		if DEBUG andalso (i<0 orelse i>=numNonterms)
		  then NONE
		  else data sub (i+numTerms)
	       | TERM (T i) =>
		if DEBUG andalso (i<0 orelse i>=numTerms)
		  then NONE
		  else data sub i
	   end

	val symbolToString = 
	     fn NONTERM i => nontermToString i
	      | TERM i => termToString i

	val grammar  = GRAMMAR {rules=grammarRules,
				 terms=numTerms,nonterms=numNonterms,
				 eop = eop, start=start,noshift=noshift,
				 termToString = termToString,
				 nontermToString = nontermToString,
				 precedence = termPrec}

	val name' = case name
			 of NONE => ""
		          | SOME (s,_) => s
		    
	val names = NAMES {miscStruct=name' ^ "LrValsFun",
			   valueStruct="MlyValue",
			   tableStruct="LrTable",
			   tokenStruct="Tokens",
			   actionsStruct="Actions",
			   ecStruct="EC",
			   arg= #1 arg_decl,
			   tokenSig = name' ^ "_TOKENS",
			   miscSig = name' ^ "_LRVALS",
			   dataStruct = "ParserData",
			   dataSig = "PARSER_DATA"}
		       


	val (table,stateErrs,corePrint,errs) =
		 MakeTable.mkTable(grammar,defaultReductions)
	
    in if verbose then
	 let val f = open_out (spec ^ ".desc")
	     val say = output f
	     val printRule =
	        let val rules = arrayoflist grammarRules
	        in fn say => 
		   let val prRule = fn {lhs,rhs,precedence,rulenum} =>
		     ((say o nontermToString) lhs; say " : ";
		      app (fn s => (say (symbolToString s); say " ")) rhs)
	           in fn i => prRule (rules sub i)
	           end
	        end
	 in Verbose.printVerbose
	    {termToString=termToString,nontermToString=nontermToString,
	     table=table, stateErrs=stateErrs,errs = errs,
	     print=say, printCores=corePrint,printRule=printRule};
	    close_out f
	 end
        else ();

        let val result = open_out (spec ^ ".sml")
 	    val sigs = open_out (spec ^ ".sig")
	    val pos = ref 0
	    val pr = output result
	    val say = fn s => let val l = String.length s
			           val newPos = (!pos) + l
			      in if newPos > lineLength 
				    then (pr "\n"; pos := l)
				    else (pos := newPos);
				   pr s
			      end
	    val saydot = fn s => (say (s ^ "."))
	    val sayln = fn t => (pr t; pr "\n"; pos := 0)
	    val termvoid = makeUniqueId "VOID"
	    val ntvoid = makeUniqueId "ntVOID"
	    val hasType = fn s => case symbolType s
				  of NONE => false
				   | _ => true
	    val terms = let fun f n = if n=numTerms then nil
				      else (T n) :: f(n+1)
		        in f 0
		        end
            val values = VALS {say=say,sayln=sayln,saydot=saydot,
		 	       termvoid=termvoid, ntvoid = ntvoid,
			       hasType=hasType, pos_type = pos_type,
			       arg_type = #2 arg_decl,
			       start=start,pureActions=pureActions,
			       termToString=termToString,
			       symbolToString=symbolToString,term=term,
			       nonterm=nonterm,terms=terms}

	    val (NAMES {miscStruct,tableStruct,dataStruct,...}) = names

         in case header_decl
	    of NONE => (say "functor "; say miscStruct; 
			sayln "(structure Token : TOKEN)")
	     | SOME s => say s;
	    sayln " = ";
	    sayln "struct";
	    sayln ("structure " ^ dataStruct ^ "=");
	    sayln "struct";
	    sayln "structure Header = ";
	    sayln "struct";
	    sayln header;
	    sayln "end";
	    sayln "structure LrTable = Token.LrTable";
	    sayln "structure Token = Token";
	    sayln "local open LrTable in ";
	    PrintStruct.makeStruct{table=table,print=pr, name = "table"};
	    sayln "end";
	    printTypes(values,names,symbolType);
	    printEC (keyword,prefer,subst,noshift,value,values,names);
	    printAction(rules,values,names);
	    sayln "end";
	    printTokenStruct(values,names);
	    sayln "end";
	    printSigs(values,names,output sigs);    
	    close_out sigs;
	    close_out result;
	    MakeTable.Errs.printSummary (output std_out) errs
	end		
    end

    val parseGen = fn spec =>
		(Header.errflag := false;
		 Header.infile := spec;
		 let val (result,_) = ParseGenParser.parse spec
		 in make_parser(result,spec)
		 end)
end
