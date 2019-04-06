(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

structure Header =
  struct

	val DEBUG = true

	type Lineno = int
	val lineno = ref 1
	val infile = ref ""
	val errflag = ref false

	val error = fn t => fn (l:int) =>
	   let val pr = output std_out
	   in pr (!infile); pr ", line "; pr (makestring l); pr ": Error: ";
	      pr t; pr "\n"; errflag := true
	   end

	val warn = fn t => fn (l:int) =>
	   let val pr = output std_out
	   in pr (!infile); pr ", line "; pr (makestring l); pr ": Warning: ";
	      pr t; pr "\n"
	   end

	val cerror = fn t =>
	   let val pr = output std_out
	   in pr (!infile); pr ", line ";
	      pr (makestring (!lineno)); pr ": Error: ";
	      pr t; pr "\n"; errflag := true
	   end

	exception SemanticError

	type symbol = string*int
	type ty = string option

	datatype lexvalue = LEFT | RIGHT | NONASSOC
	datatype control = NODEFAULT | VERBOSE | PARSER_NAME of symbol |
			   FUNCTOR of string  | START_SYM of symbol |
			   NSHIFT of symbol list | POS of string | PURE |
			   PARSE_ARG of string * string
			   
	type declData = {eop : symbol list,
			 keyword : symbol list,
			 nonterm : (symbol*ty) list option,
			 prec : (lexvalue * (symbol list)) list,
			 prefer : symbol list,
			 subst: (symbol*symbol) list,
			 term : (symbol*ty) list option,
			 control : control list,
			 value : (symbol * string) list}

	type rhsData = {rhs:symbol list,code:string, prec:symbol option} list
	type rule = {lhs : symbol, rhs : symbol list,
		     code : string, prec : symbol option}

	fun join_decls
	      ({eop=e,control=c,keyword=k,nonterm=n,prec, prefer=p,
		subst=su,term=t,value=v}:declData,
	       {eop=e',control=c',keyword=k',nonterm=n',prec=prec', prefer=p',
		subst=su',term=t',value=v'} : declData) =
	  let val ignore_dup = fn s => (cerror ("ignoring duplicate " ^ s ^
						"declaration"))
	      val join = fn (e,NONE,NONE) => NONE
			  | (e,NONE,a) => a
			  | (e,a,NONE) => a
			  | (e,a,b) => (ignore_dup e; a)
	      fun mergeControl (nil,a) = [a]
		| mergeControl (l as h::t,a) =
		     case (h,a)
	  	     of (PARSER_NAME _,PARSER_NAME _) => (ignore_dup "%name"; l)
		      | (FUNCTOR _,FUNCTOR _) => (ignore_dup "%header"; l)
		      | (PARSE_ARG _,PARSE_ARG _) => (ignore_dup "%arg"; l)
		      | (START_SYM _,START_SYM _) => (ignore_dup "%start"; l)
		      | (POS _,POS _) => (ignore_dup "%pos"; l)
		      | (NSHIFT a,NSHIFT b) => (NSHIFT (a@b)::t)
		      | _ => h :: mergeControl(t,a)
	      fun loop (nil,r) = r
		| loop (h::t,r) = mergeControl(r,h)
	 in {eop=e@e',control=loop(c',c),keyword=k'@k,
	    nonterm=join("%nonterm",n,n'), prec=prec@prec',
	    prefer = p@p', subst=su@su', term=join("%term",t,t'),value=v@v'} : declData
	end
end;
