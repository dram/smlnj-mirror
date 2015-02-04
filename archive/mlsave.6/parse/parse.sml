(* parse.sml *)

(* sharing constraints don't work ---
structure Foo = struct
  type info = Access.path * Basics.context
  type symtable = Basics.binding Table.table
end

functor Parse(EnvAccess: ENVACCESS 
		sharing structure EnvAccess.Access = Access
		    and structure EnvAccess.Env.Table = Table
		    and structure EnvAccess.Basics = Basics
		    and type EnvAccess.Env.info = Foo.info
		    and type EnvAccess.Env.symtable = Foo.symtable
		    and type EnvAccess.Env.binding = Basics.binding) : PARSE = 
*)

structure Parse (* : PARSE *) = struct

structure Basics = Basics
structure BareAbsyn = BareAbsyn

local
  open ErrorMsg
  open Symbol
  open PrintUtil
  open Lex
  open Lex.Token (* Token *)
  open Access
  open Basics
  open Absyn
  open BasicTypes
  open EnvAccess.Env.Table (* Table *)
  open EnvAccess.Env (* Env *)
  open EnvAccess
  open FirstSets

  infix -->

  val sort3 = Sort.sort (fn ((a,_,_),(b,_,_)) => name a > name b)

(* -- debugging
   val openOld = fn x => (print "openOld\n"; openOld x)
   val openNew = fn x => (print "openNew\n"; openNew x)
   val openRec = fn x => (print "openRec\n"; openRec x)
   val close = fn x => (print "close\n"; close x)
   val closeStr = fn x => (print "closeStr\n"; closeStr x)
*)

  fun protect((enter,exit),doit) =
      let val t = enter()
       in (doit() before exit t)
           handlex Interrupt => (exit t; raisex Interrupt)
		|| unbound with sym => 
			    (exit t; impossible ("Unbound identifier: " ^ 
					    Symbol.name sym))
(*		|| ? => (exit t; raisex Syntax)   turned off for debugging *)
      end

  val protectScope = (mark,close)

in

(* debugging *)

val debugStr = ref false
val debug = ref false

(* utility functions *)

(* kludge for recognizing unit type *)
val unitName = SymbolTable.stringToSymbol("unit")
fun isUnitTy(CONty(ref(tyc),_)) = 
    (eqTycon(tyc,!unitTycon) orelse
     Symbol.eq(tycName(tyc),unitName)
     handle _ => false)
  | isUnitTy _ = false

val nestingLevel = ref 0;
val protectNest = ((fn () => (!nestingLevel before inc nestingLevel)),
		   (fn i => nestingLevel := i))
fun topLevel() = !nestingLevel = 1

fun reset() =
    (nestingLevel := 0)

fun expop () =
    case !nextToken
      of EQUAL => lookFIX (!idValue)
       | ASTERISK => lookFIX (!idValue)
       | ID => lookFIX(!idValue)
       | _ => NONfix

fun patop () =
    case !nextToken
      of ASTERISK => lookFIX (!idValue)
       | ID => lookFIX(!idValue)
       | _ => NONfix

val bogusID = SymbolTable.stringToSymbol "bogus"
val bogusExnID = SymbolTable.stringToSymbol "Bogus"
val bogusExp = VARexp(ref(VALvar{access=LVAR(mkLvar()),
	       			 name=bogusID,
				 vtype=ref UNKNOWNty}))

fun ident () =
    if !nextToken = ID orelse !nextToken = ASTERISK orelse !nextToken = EQUAL
      then getSymbol()
      else (complain "identifier expected"; bogusID)

fun nonfix_ident() =
    if (!nextToken = ID orelse !nextToken = ASTERISK)
	    andalso lookFIX(!idValue)=NONfix
       then getSymbol()
       else (complain "nonfix-identifier expected"; bogusID)
    
fun opid() =
    case !nextToken
      of ID	=> nonfix_ident()
       | ASTERISK => nonfix_ident()
       | OP	=> (advance(); 
		    case !nextToken
		     of ID => getSymbol()
		      | ASTERISK => getSymbol()
		      | EQUAL => getSymbol()
		      | _ => (complain "op not followed by identifier"; bogusID)
		    )
       | _ => (complain "identifier or OP expected"; bogusID)


fun rightAssoc(elem:(unit->'a), tok:token, cons:('a*'b->'b), single:('a->'b))
    : 'b =
    let fun ra() = 
            let val e1 = elem()
            in if at(tok) then cons(e1,ra()) else single e1
            end
    in  ra()
    end;

fun leftAssoc(elem, tok, cons, single) =
    let fun la e = if at tok then la(cons(e,elem())) else single e
     in la(elem())
    end

fun precedence(elem,g,checkop) =
     let fun parse(f, bp, e) =
	    case checkop()
	     of INfix(lbp,rbp) =>
	        if lbp > bp
		 then let val id = getSymbol()
		          val rhs = parse ((fn x=>g(id,e,x)),rbp,elem())
		       in parse(f,bp,rhs)
		      end
	         else f e
	      | _ => f e
      in parse((fn x=>x), ~100, elem())
     end

fun synchAND() =
    if topLevel()
        then (while not (!nextToken=AND orelse firstTdec(!nextToken))
       	        do advance();
              if at(AND) then () else raisex Syntax)
        else raisex Syntax

fun andList(elem) =
    let val e1 = elem()
     in (if at(AND) then e1 :: andList(elem) else [e1])
(*	handlex Syntax =>
	  (synchAND(); andList(elem))  turned off for debugging *)
    end;

fun andListProtect(elem) = andList (fn () => protect(protectScope,elem))

fun discard _ = ()

fun single x = [x]

fun varcon (VARbind v) = VARexp(ref v)
  | varcon (CONbind d) = CONexp d
  | varcon _ = impossible "parse.39"

fun lookID(id : symbol): exp = 
     varcon (lookVARCON id)
     handlex unboundrec => VARexp(getPatchVar id)

val lookIDinStr = varcon o lookVARCONinStr

fun openOldStr(ap: Access.path, str: Structure) =
    case str
      of STRstr{table,env,...} => openOld((ap,env),table)
       | INDstr _ => impossible "openOldStr -- INDstr arg"

(* parsing functions *)

(* qualified id interpretation *)

fun spath() =
    case !nextToken
      of IDDOT => getSymbol() :: spath()
       | ID => [getSymbol()]
       | ASTERISK => [getSymbol()]
       | _ => (complain "incomplete qualified identifier -- spath"; [])

fun strPath() : spath =
    case !nextToken
      of IDDOT => getSymbol() :: strPath()
       | ID => []    (* leave last identifier *)
       | EQUAL => []
       | ASTERISK => []
       | _ => (complain "incomplete qualified identifier"; [])

fun symPath() : spath * symbol =
    (strPath(),ident())

fun qid(lookLast: Structure * symbol * Access.path -> 'a): 'a =
    let fun getStr([],str,ap) = (str,ap)
	  | getStr(id::rest,STRstr{table,env={s,...},...},ap) =
	      let val STRvar{access=SLOT n,binding,...} = lookSTRinTable(table,id)
	       in getStr(rest,
		  	 (case binding of INDstr i => s sub i | _ => binding),
			 n::ap)
	      end
	val (firstId::rest, lastId)  = symPath()
	val STRvar{access=PATH(ap),binding,...} = lookSTR(firstId)
	val (endStr,ap) = getStr(rest,binding,ap)
     in lookLast(endStr,lastId,ap)
    end
    
(* record labels *)

fun selector() =
    let fun sel1 id = 
	    let val v = namedLvar id
		val tyref = ref UNKNOWNty
		val v1 = VALvar{name=id,access=LVAR(v),vtype=tyref}
		val v2 = VALvar{name=id,access=PATH[v],vtype=tyref}
	     in FNexp[RULE(RECORDpat{fields=[(id,VARpat v1)],
				     flex=true,
				     typ=ref UNKNOWNty, pats=ref nil},
			   VARexp(ref v2))]
	    end
     in case !nextToken
	  of ID => sel1(ident())
	   | INT => sel1(SymbolTable.stringToSymbol(makestring(!intValue))
		         before advance())
	   | _ => (complain "illegal selector function"; bogusExp)
    end

fun noAbbrev x _ = (complain "illegal record-name element abbreviation"; x)

fun labels(parseOne, separator, dotsOK, abbrev) =
    if !nextToken = ID
        then let fun lablist () = 
		  case !nextToken
		   of ID => field(ident())
		    | INT => field(SymbolTable.stringToSymbol(makestring(!intValue))
				   before advance())
		    | DOTDOTDOT => nil
		    | _ => condemn("label expected")
	         and field id =(id,
			        if at(separator) then parseOne()
						  else abbrev(id),
			         ref 0)
			       :: (if at(COMMA) then lablist() else nil)
		 val l = lablist()
		 val dots = at(DOTDOTDOT)
	         val sl = sort3 l
	      in if length l > length sl
		     then complain "duplicate label in record"
		     else ();
		 if dots andalso not dotsOK
		    then complain "use of ... outside pattern" else ();
		 checkToken(RBRACE);
		 (l,sl,dots)
	     end
	else (checkToken(RBRACE); (nil,nil,false))

exceptionx clausal : symbol * pat   (* for returning clausal patterns from pat *)

(* types *)
(* BUG: no arity checking when types are formed *)

fun tyParser(tyconQid) = 
    let fun ty0() =
	    rightAssoc(ty1,ARROW,
		       (fn (t,ts) => CONty(arrowTycon, [t,ts])),
		       (fn t => t))
	and ty1() = 
	    case rightAssoc(ty2,ASTERISK,op::,single)
	      of [t] => t
	       | l => tupleTy l
	and ty2() =
	    (* incorporates tyapp and aty nonterminals *)
	    let	fun qid_s(t) =
		    case !nextToken
		      of ID => qid_s(CONty(lookPatchTYC(getSymbol()), [t]))
		       | IDDOT => qid_s(CONty(tyconQid(), [t]))
		       | _ => t
	     in qid_s(case !nextToken
		       of LPAREN =>
			    let val t1 = (advance(); ty0())
			     in if at(RPAREN)
				then t1
				else if at(COMMA)
				then let val tl = t1 :: ty_pc()
				      in checkToken(RPAREN);
					 case !nextToken
					   of ID => CONty(lookPatchTYC(ident()), tl)
					    | IDDOT => CONty(tyconQid(), tl)
					    | _ => condemn "expected type constructor"
				     end
				else (complain "expected RPAREN or COMMA in type args";
				      t1)
			    end
			| ID	=> CONty(lookPatchTYC(ident()),[])
			| IDDOT => CONty(tyconQid(),[])
			| Token.TYVAR => VARty(lookTyvar(getSymbol()))
			| LBRACE =>
			    (advance();
			     let val (l,sl,_) = labels(ty0,COLON,false,
						       noAbbrev UNKNOWNty)
			      in recordTy(map (fn (id,ty,_) => (id, ty)) sl)
			     end)
			| _      => condemn( "expected a type expression"))
	    end
	and ty_pc() = rightAssoc(ty0,COMMA,op::,single)
     in ty0
    end

val ty = tyParser(fn () => qid(fn(str,id,_) => lookTYCinStr(str,id)))


(* expressions -- including local declarations *)

fun exp () =
    case !nextToken
     of FN =>   (advance(); FNexp(match()))
      | CASE => CASEexp( (advance(); exp()),
			 (checkToken(OF); match()) )
      | WHILE => WHILEexp( (advance(); exp()), (checkToken(DO); exp()) )
      | IF => IFexp( (advance(); exp()), (checkToken(THEN); exp()),
       	             (checkToken(ELSE); exp()) )
      | RAISEX => RAISEXexp((advance(); 
			     case !nextToken
			       of ID => lookEXN(fixExnName(getSymbol()))
			        | IDDOT => qid lookFixedEXNinStr
			        | _ => (complain "exception name expected\
					       \ after raise";
				        unboundEXN bogusExnID)),
			    (if !nextToken = WITH
			     then (advance(); SOME(exp()))
			     else NONE))
      | RAISE => RAISEexp(advance(); exp())
      | _ => let val e = exp1()
              in if !nextToken = HANDLEX
		 then (advance(); HANDLEexp(e,handlerx()))
		 else if !nextToken = HANDLE
		 then (advance(); HANDLEexp(e,HANDLER(FNexp(match()))))
		 else e
             end

and match () = rightAssoc(rule,BAR,op::,single)

and rule () = 
     let val bl = ref nil
      in protect(protectScope,
	    (fn () => RULE(pat(bl,true)
		           handlex clausal with (id,_) => 
			     condemn("undefined op in pattern: "^name id),
		           (checkToken(DARROW); checkBinding(!bl);
			    bindVARs(!bl); exp()))))
     end

and handlerx () = HANDLERX(rightAssoc(hrulex,BARBAR,op::,single))

and hrulex () =
     let fun withClause(exn) =
	     case !nextToken
	       of WITH   => (advance(); WITHhrule(exn, match()))
		| DARROW => (advance(); ARROWhrule(exn,exp()))
		| _ => (complain "expected => or WITH";
		        ARROWhrule(exn,bogusExp))
      in case !nextToken
	   of ID => withClause(lookEXN(fixExnName(getSymbol())))
	    | IDDOT => withClause(qid lookFixedEXNinStr)
	    | QUERY => (advance(); checkToken(DARROW); QUERYhrule(exp()))
	    | _ => (complain "improper exception name after handle";
		    QUERYhrule(bogusExp))
     end

and exp_ps () = rightAssoc(exp,SEMICOLON,op::,single)

and exp1 () = leftAssoc(exp2,ORELSE,ORELSEexp,(fn x=>x))

and exp2 () = leftAssoc(exp3,ANDALSO,ANDALSOexp,(fn x=>x))

and exp3() = let val e = precedence(exp5, 
		(fn(id,a,b)=>APPexp(lookID(id),TUPLEexp[a,b])), expop)
	      in if at(COLON) then CONSTRAINTexp(e,ty()) else e
	     end

and exp5 () =
     let fun loop e = if firstAexp()
			    then loop(APPexp(e,aexp()))
			    else e
      in loop(aexp())
     end

(* note that IF WHILE CASE RAISE RAISEX FN  are matched below, but
   are not in firstAexp.  This is intentional *)

and aexp () =
     case !nextToken
       of ID	 => lookID(nonfix_ident())
        | OP	 => lookID(opid())
	| IDDOT  => qid(lookIDinStr)
        | INT	 => INTexp(!intValue) before advance()
        | REAL	 => REALexp(!realValue) before advance()
        | STRING => STRINGexp(!stringValue) before advance()
	| HASH => (advance(); selector())
        | LBRACE => ( advance(); exp_brace() )
        | LPAREN => ( advance(); exp_paren() )
        | LBRACKET => ( advance(); exp_bracket() )
        | LET	 => protect(protectScope,
		     (fn()=>(advance();
		     	     (LETexp(ldecs(), (checkToken(IN); SEQexp(exp_ps()))))
			     before checkToken(END))))
        | FN =>   exp()
        | CASE => exp()
        | WHILE => exp()
        | IF => exp()
	| RAISE => exp()
        | RAISEX => exp()
        | _	 => (complain "atomic expression expected"; bogusExp)

and exp_brace () =
    let val (l,sl,_) = labels(exp,EQUAL,false,noAbbrev bogusExp)
	fun assign (i,(_,_,r)::tl) = (r:=i; assign(i+1,tl))
	  | assign (n,nil) = ()
     in assign(0,sl);
	RECORDexp(map (fn (id,e,ref n) => (LABEL{name=id,number=n},e)) l)
    end

and exp_paren () =
     if at(RPAREN)
        then unitExp (* TUPLEexp(nil) *)
        else let val e = exp()
              in case !nextToken
		   of RPAREN => (advance(); e)
		    | COMMA =>
		       (advance();
			TUPLEexp(e::exp_pc()) before checkToken(RPAREN))
		    | SEMICOLON =>
		       (advance();
			SEQexp(e::exp_ps()) before checkToken(RPAREN))
		    | _ => (complain "expected comma, right paren, or semicolon"; e)
             end

and exp_bracket () =
     if at(RBRACKET)
        then LISTexp(nil)
        else LISTexp(exp() ::
              if !nextToken = RBRACKET
	       	 then (advance(); nil)
       		 else (checkToken(COMMA);
       		       exp_pc() before checkToken(RBRACKET)))

and exp_pc () = rightAssoc(exp,COMMA,op::,single)

and pat (bl: binder list ref, full: bool) =
    (* full false means parse atomic pattern *)
   let fun restrictLAYEREDpat(x as (VARpat _, _)) = LAYEREDpat x
         | restrictLAYEREDpat(y,z) =
	      (complain "pattern to left of AS must be a variable"; z)

       fun pat0 () = rightAssoc(pat1,AS,restrictLAYEREDpat,(fn x=>x))

       and pat1 () = 
	   let val e = precedence(
		         pat2, 
		         (fn(id,a,b)=>
			    APPpat(lookCON(id),TUPLEpat[a,b])
			    handlex unbound => 
			      raisex clausal with (id, TUPLEpat[a,b])),
			 patop)
	    in if at(COLON) then CONSTRAINTpat(e,ty()) else e
	   end

       and pat2 () =
	   let fun useCon(dcon as DATACON{const,name,...}) =
		    case (const,firstApat())
		      of (true,false) => CONpat(dcon)
		       | (false,true) => APPpat(dcon,apat())
		       | (_,x) => (complain("improper use of constructor "^
			              Symbol.name(name)^" in pattern");
				   (if x then (apat(); ()) else ());
				   WILDpat)
	       fun simpleId(id) =
		   useCon(lookCON(id))
		   handlex unbound => 
		     if firstApat()
		       then raisex clausal with (id, apat())
		       else VARpat(newVAR(bl,id))
	    in case !nextToken
		 of ID => (if lookFIX(!idValue) = NONfix
			    then ()
			    else complain("pattern starts with infix: "
					 ^ name(!idValue));
			   simpleId(getSymbol()))
		  | OP => simpleId(opid())
		  | IDDOT => useCon(qid lookCONinStr)
		  | _ => apat()
	   end

       and pat_id(id) = 
	   (case lookCON(id)
	     of	dcon as DATACON{const=true,...} => CONpat(dcon)
	      | _ => (complain("nonconstant data constructor: " ^ name(id));
		      WILDpat))
	   handlex unbound => VARpat(newVAR(bl,id))

       and apat() = 
	   case !nextToken
	     of OP	=> pat_id(opid())
	      |	ID	=> pat_id(nonfix_ident())
	      | IDDOT   => CONpat(qid(lookCONinStr))
	      |	INT	=> INTpat(!intValue) before advance()
	      | REAL    => REALpat(!realValue) before advance()
	      | STRING  => STRINGpat(!stringValue) before advance()
	      | WILD	=> (advance(); WILDpat)
	      |	LPAREN =>  (advance(); pat_paren())
	      |	LBRACKET => (advance(); pat_bracket())
	      |	LBRACE =>  (advance(); pat_brace())
	      | _ => (complain "expected an atomic pattern"; WILDpat)

       and pat_paren () =
	    if at(RPAREN)
	       then unitPat
	       else let val p = pat0()
		     in case !nextToken of
			    RPAREN => (advance(); p)
			  | COMMA =>
			     (advance();
			      TUPLEpat(p::pat_pc()) before checkToken(RPAREN))
			  | _ => (complain "expected right paren or comma\
					   \ (in pattern)";
				  p)
		    end

       and pat_bracket () =
	   LISTpat(if at(RBRACKET)
		     then nil
		     else pat_pc() before checkToken(RBRACKET))

(* bug:  we allow  {a,b,c} to stand for {a=a,b=b,c=c} but we don't
    allow {a as zzz} to stand for {a=a as zzz}
*)

       and pat_id_as id =
	    if at(AS) then LAYEREDpat(pat_id id,pat0())
	              else pat_id id

       and pat_brace () =
	   let val (_,sl,dots) = labels(pat0,EQUAL,true,pat_id_as)
	    in RECORDpat{
	         fields = map (fn(id,pat,_) => (id,pat)) sl,
		 flex = dots,
		 typ = ref UNKNOWNty,
		 pats = ref nil}
	   end

       and pat_pc() = rightAssoc(pat0,COMMA,op::,single)

    in if full then pat0() else apat()
   end

(* variable bindings *)

and vb() =
    if at(REC)
    then VALRECdec(rvb_pa())
    else VALdec(vb_pa())

and vb_pa () =
    let val bl = ref nil
	fun vb () = 
	    protect(protectTyvars (OPENbtv(ref[])),
	      (fn () =>
		let val pat=pat(bl,true)
			    handlex clausal with (id,_) =>
			      condemn("undefined op in pattern: "^name id)
		    and exp=( checkToken(EQUAL); exp() )
		    and tyvars=currentTyvars()
		 in case (pat,exp)
		      of (CONSTRAINTpat(VARpat(VALvar{name,vtype,...}), ty),
			   VARexp(ref(VALvar{access as INLINE _,...})))
			   => let val _::rest = !bl
				  val w = VALvar{name=name,vtype=vtype,access=access}
			       in bl := (name,VARbind w) :: rest;
			          VB{pat=CONSTRAINTpat(VARpat w, ty),
				     exp=exp,tyvars=tyvars}
			      end

		       | (VARpat(VALvar{name,vtype,...}),
			   VARexp(ref(VALvar{access as INLINE _,...})))
			   => let val _::rest = !bl
				  val w = VALvar{name=name,vtype=vtype,access=access}
			       in bl := (name,VARbind w) :: rest;
			          VB{pat=VARpat w, exp=exp, tyvars=tyvars}
			      end
		       | _ => VB{pat=pat,exp=exp,tyvars=tyvars}
		end))
     in andListProtect(vb)
	before bindVARs(!bl)
    end

and rvb_pa () = 
    let val bl = ref nil
	fun rvb () =  protect(protectTyvars(OPENbtv(ref[])),
	      (fn () =>  (* record bug *)
		let val var=newVAR(bl,opid())
		    and resultty=constraint_op()
		    and e = ( checkToken(EQUAL); exp() )
		    and tyvars=currentTyvars()
		 in case e of FNexp _ => ()
		       | _ => complain "fn expression required in val rec declaration";
		    RVB{var = var, resultty = resultty, exp = e, tyvars = tyvars}
		end))
     in protect(protectPatchList, (fn()=>
	  protect(protectScope, (fn()=>
	    (openRec(); andListProtect(rvb)) ))
	  before bindVARs(!bl) ))
    end

and fb_pa () = 
    let val bl = ref nil
	fun fb () = protect(protectTyvars(OPENbtv(ref[])),
	  (fn () =>
	     let val funSymbol: symbol option ref = ref NONE
	         val clauses=rightAssoc(clause(funSymbol),BAR,op::,single)
	      in FB{var = let val SOME id = !funSymbol in newVAR(bl,id) end,
		    clauses = clauses,
		    tyvars = currentTyvars()} 
	     end))
     in protect(protectPatchList, (fn()=>
	  protect(protectScope, (fn()=>
	    (openRec(); FUNdec(andListProtect(fb))) ))
	  before bindVARs(!bl) ))
    end

and clause funsym () = 
    let val bl = ref nil
        fun pat_p () = if firstApat()
			 then (pat(bl,false)  (* atomic pattern *)
			       handlex clausal with (id,_) =>
				 condemn("undefined op in pattern: "^name id))
			      :: pat_p ()
			 else nil
     in (pat(bl,true); condemn("no defined function in clausal lhs"))
	handlex clausal with (id,pat1) =>
          (case !funsym
	     of NONE => funsym := SOME id
	      | SOME f => if Symbol.eq(id,f) then ()
		            else complain "identifiers in clauses don't match";
	   let val pats = pat1::pat_p()
	       val resultty = constraint_op()
	       val exp = protect(protectScope,
			   (fn()=>(checkToken(EQUAL); bindVARs(!bl); exp())))
	    in CLAUSE{pats=pats, resultty=resultty, exp=exp}
	   end)
    end

and constraint () = (checkToken(COLON); ty())

and constraint_op() =
    if at(COLON)
    then SOME(ty())
    else NONE

and tb() = 
    let	fun tb1() =
	    let val args = tyvars()
		val name = nonfix_ident()
	     in protect(protectTyvars(CLOSEDbtv(args)),
		  (fn()=>
		    !(bindTYC(name,mkTYPEtyc(name,args,(checkToken(EQUAL); ty()))))))
	    end
     in TYPEdec(andList(tb1))
    end

and tyvars() =
    case !nextToken
      of Token.TYVAR => [mkTyvar(!idValue,BOUND)] before advance()
       | LPAREN =>
	    (advance();
	     tyvar_pc() before
	     checkToken(RPAREN))
       | _ => nil

and tyvar_pc() = rightAssoc(tyvar,COMMA,op::,single)

and tyvar() = mkTyvar(!idValue,BOUND) before checkToken(Token.TYVAR)

and db() = protect((enterDb,exitDb),(fn()=>DATATYPEdec(andList(db1 (mkDATAtyc,ty)))))

and db1 (newTyc,ty) () =
    let val args = tyvars()
   	val name = nonfix_ident()
	val dcons = ref nil
	val tyc = bindTYC(name,newTyc(name,args,dcons))
	val rangeType = CONty(tyc, map VARty args)
	fun constr() =
	    let val sym = opid()
		val const = not(at(OF))
		val vtype = if const then rangeType
				else CONty(arrowTycon, [ty(), rangeType])
	     in (sym,const,vtype)
            end
         in protect(protectTyvars(CLOSEDbtv(args)),
	     (fn()=>
	      let val dcl = (checkToken(EQUAL); rightAssoc(constr,BAR,op::,single))
	          val sl = sort3 dcl
                  fun bindlist ((sym,const,vtype)::tl) =
	                 bindCON(sym, 
			   DATACON{name = sym,
				   const = const,
				   rep = ref UNDECIDED,
				   vtype = vtype,
				   dcons = dcons})
	                 :: bindlist(tl)
	            | bindlist nil = nil
	       in if length sl < length dcl
	       	    then complain "duplicate constructor name" else ();
                  dcons := bindlist(sl);
		  ConRep.boxed(!dcons);
		  !tyc
	      end))
    end

and eb() = EXCEPTIONdec(andList(eb1))

and eb1() =
    let val name = ident()
	val _ = if looksLikeExn name then ()
		else warn "Exception name should be capitalized"
	val (edef,etype) =
	    case !nextToken
	     of OF => (NONE, SOME(advance(); ty()))
	      | EQUAL => (SOME(advance(); case !nextToken
			       of IDDOT => qid lookEXNinStr
			        | ID => lookEXN(getSymbol())
				| _ => unboundEXN(bogusID) ),
			 NONE)
	      | _ => (NONE,NONE)
        val exn = case edef
		    of NONE =>
		         DATACON{name=name,
				 const=(case etype of NONE=>true
						    | SOME _  => false),
				 vtype=(case etype
					  of NONE => exnTy
					   | SOME t => t-->exnTy),
				 rep=ref(VARIABLE(LVAR(namedLvar(name)))),
				 dcons= exnDcons}
		     | SOME(DATACON{name=n,const,vtype,rep,dcons}) =>
			    DATACON{name=name,const=const,vtype=vtype,rep=rep,
			    	    dcons=dcons}  (* changes only name *)
     in bindEXN(name, exn);
	EB{exn=exn, ty=etype, def=edef}
    end


and ebx() = EXCEPTIONdec(andList(eb1x))

and eb1x() =
    let val name = fixExnName(ident())
	val etype = constraint_op()
	val (const,vtype) = case etype
			      of NONE => (true,exnTy)
			       | SOME t => if isUnitTy(t)
					   then (true,exnTy)
					   else (false,t-->exnTy)
	val edef = if at(EQUAL)
	           then SOME(case !nextToken
			       of IDDOT => qid lookFixedEXNinStr
			        | ID => lookEXN(fixExnName(getSymbol()))
				| _ => unboundEXN(bogusExnID) )
		   else NONE
        val exn = case edef
		    of NONE => 
		         DATACON{name=name, const=const, vtype=vtype,
				 rep=ref(VARIABLE(LVAR(namedLvar(name)))),
				 dcons= exnDcons}
		     | SOME(DATACON{name=n,const,vtype,rep,dcons}) =>
			    DATACON{name=name,const=const,vtype=vtype,rep=rep,
			    	    dcons=dcons}  (* changes only name *)
     in bindEXN(name, exn);
	EB{exn=exn, ty=etype, def=edef}
    end

and ldec() =
   protect(protectNest,
    (fn () =>
     (case !nextToken
	of VAL =>
	      (advance(); vb())
         | FUN =>
	      (advance(); fb_pa())
         | TYPE =>
	      (advance(); tb())
         | DATATYPE =>
	      (advance(); db())
	 | EXCEPTION =>
	      (advance(); eb())
         | EXCEPTIONX =>
	      (advance(); ebx())
         | LOCAL =>
	     let val lev1 = mark()
		 val ld1 = (advance(); ldecs())
		 val lev2 = mark()
		 val ld2 = (checkToken(IN); ldecs())
	      in splice(lev1,lev2);
		 checkToken(END);
		 LOCALdec(ld1,ld2)
	     end
	 | Token.OPEN =>  (* confusion with Env.OPEN when Env not constrained *)
	      let val strs = (advance(); qid_p())
	       in List.app 
		    (fn (STRvar{access=PATH(p),binding,...}) => openOldStr(p,binding)
		        | _ => impossible "parse.32")
		    strs;
		  OPENdec strs
	      end
         | INFIX =>
	      let val prec = case (advance(); optprec()) of SOME n=>n|NONE=>0
	       in map (fn i => bindFIX(i, infixleft prec)) (ops());
	          SEQdec(nil)
	      end
         | INFIXR =>
	      let val prec = case (advance(); optprec()) of SOME n=>n|NONE=>0
	       in map (fn i => bindFIX(i, infixright prec)) (ops());
	          SEQdec(nil)
	      end
         | NONFIX =>
	      (advance();
	       map (fn i => bindFIX(i,NONfix)) (ops()); SEQdec(nil))
	 | OVERLOAD =>
	      let val id = (advance(); ident())
		  val scheme = (checkToken(COLON);
		    protect(protectScope, (fn () =>  (* localize tyvars *)
		      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
		        ty() before  (* generalize type variables *)
			(List.app (fn (TYVAR{status,...}) => status := BOUND)
			  (currentTyvars())))))))
		  val _ = if !debug
			    then (prstr "Overload scheme: ";
			          PrintType.printType(scheme); newline())
			    else ()
		  fun option() =
		      let val VARexp(ref (v as VALvar{vtype,...})) = exp()
		       in if !debug
			    then (prstr "Overload option type: ";
				  PrintType.printType(!vtype); newline())
			    else ();
			  {indicators = TypesUtil.matchType(scheme,!vtype),
			   variant = v}
		      end
		  val l = (checkToken(AS); andList(option))
	       in bindVAR(id,OVLDvar{name=id, options = ref l, scheme = scheme});
		  SEQdec nil
	      end
         | _ => (complain "expected a declaration"; vb())
      )))

and ldecs() =
  let fun ldecs() =
    ldec() ::
    ( at(SEMICOLON);
      if firstLdec(!nextToken)
        then ldecs()
        else nil )
   in case ldecs() of [dec] => dec | seq => SEQdec seq
  end

and optprec() = if at(INT) then SOME(!intValue) else NONE

and qid_p(): structureVar list =  (* error if no indentifier's ? *)
    case !nextToken
      of ID => lookSTR(ident())::qid_p()
       | IDDOT => qid(lookSTRinStr)::qid_p()
       | _ => nil

and ops() =
  let fun ops1() =
        case !nextToken
          of ID => (!idValue) :: (advance(); ops1())
	   | EQUAL => (!idValue) :: (advance(); ops1())
	   | ASTERISK => (!idValue) :: (advance(); ops1())
           | _ => nil
      val l = ops1()
   in if l=nil then complain("operator or identifier expected") else ();
      l
  end


(* signatures *)

fun sign () =
    let val tycCount = ref 0
	and strCount = ref 0
	fun nextTycStamp () = (!tycCount before inc tycCount)
	fun nextStrStamp () = (!strCount before inc strCount)
        fun sgn() = 
	    let val tComps = ref ([] : tycon list)
		and tCount = ref 0
		fun tNext x = (tComps := (x :: !tComps);
			       INDtyc[!tCount before inc tCount])
		val sComps = ref ([] : Structure list)
		and sCount = ref 0
		fun sNext x = (sComps := (x :: !sComps);
			       INDstr(!sCount before inc sCount))
		fun newDATAtycSpec(name,params,dcons) =
		     tNext(DATAtyc{stamp=nextTycStamp(),name=name,
		    		   params=params,dcons=dcons})
		fun tyconQid() : tycon ref =
		    let fun get([id],STRstr{table,...}) = 
		              let val ref(INDtyc x) = lookTYCinTable(table,id)
			       in x
			      end
			  | get(id::rest,STRstr{table,env={s,...},...}) =
			      let val STRvar{binding=INDstr k,...} =
					lookSTRinTable(table,id)
			       in k::get(rest, s sub k)
			      end
			val (firstId::rest, lastId)  = symPath()
			val (STRvar{binding=INDstr i,...},_) = 
			      lookSTR_sig firstId  (* lookSTRinSig ? *)
		        handlex unbound =>
			     condemn("unbound str: " ^ name firstId)
    		     in ref(INDtyc(i::get(rest@[lastId], nth(rev(!sComps),i))))
		    end
		val ty = tyParser(tyconQid)

		fun spec_s() =
		     if firstSpec(!nextToken)
			 then (spec(); at(SEMICOLON); spec_s())
			 else ()
		
		and spec() =
		    case !nextToken
		      of STRUCTURE => (advance(); strspec())
		       | DATATYPE => (advance(); dtyspec())
		       | TYPE => (advance(); tyspec())
		       | VAL => (advance(); valspec())
		       | EXCEPTION => (advance(); exnspec())
		       | EXCEPTIONX => (advance(); exnspecx())
		       | _ => condemn("expected a spec (component of signature)")
		
		and strspec() = 
		    rightAssoc(strspec1,AND,discard,discard)
		
		and strspec1() =
		    let val name = ident()
			val sgn = (checkToken(COLON); sgn())
		     in if !debug
			  then (print "strspec1: "; printSym name; newline())
			  else ();
			(lookSTR_sig name; complain("duplicate structure \
					\in signature: "^(Symbol.name name)))
			 handlex unbound =>   
			   bindSTR(name,STRvar{name=name,access=LVAR(0),
					    binding= sNext sgn})
		    end
		
		and dtyspec() =
		    protect((enterDb,exitDb),
			    (fn()=>rightAssoc(db1(newDATAtycSpec,ty),
			    		      AND,discard,discard)))
		
		and tyspec() = 
		    rightAssoc(tyspec1,AND,discard,discard)
		
		and tyspec1() =
		    let val arity = length(tyvars())
			val name = ident()
		     in 
			if !debug
			  then (print "tyspec1: "; printSym name; newline())
			  else ();
			bindTYC(name, tNext(VARtyc{name=name,stamp=nextTycStamp(),
				      	           arity=arity}))
		    end
		
		and valspec() =     
		    rightAssoc(valspec1,AND,discard,discard)
		
		and valspec1() =
		    let val name = 
			    case !nextToken
			      of ID => getSymbol()
			       | ASTERISK => getSymbol()
			       | EQUAL => getSymbol()
			       | _ => condemn "val spec -- identifier expected"
			val typ =
			    protect(protectScope, (fn () =>
			      (* localize type variables *)
			      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
				(checkToken(COLON); ty()) before
				(* generalize type variables *)
				(List.app (fn (TYVAR{status,...}) => status := BOUND)
					  (currentTyvars()))))))
		     in if !debug
			  then (print "valspec1: "; printSym name; newline())
			  else ();
			bindVAR(name,mkVALvar(name,ref typ))
		    end
		
		and exnspec() = 
		    rightAssoc(exnspec1,AND,discard,discard)
		
		and exnspec1() =
		    let val name = ident()
			val (const,vtype) =
			    if at(OF) then (false,
			    protect(protectScope, (fn () =>
			      (* localize type variables *)
			      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
				ty() before  (* generalize type variables *)
				(List.app (fn (TYVAR{status,...}) => status := BOUND)
					  (currentTyvars()))))))
			      --> exnTy)
			  else (true,exnTy)
		     in if !debug
			  then (print "exnspec1: "; printSym name; newline())
			  else ();
			bindEXN(name, DATACON{name=name, const=const, vtype=vtype,
					      rep=ref(VARIABLE(LVAR(0))),
					      dcons=exnDcons})
		    end
		
		and exnspecx() = 
		    rightAssoc(exnspec1x,AND,discard,discard)
		
		and exnspec1x() =
		    let val name = fixExnName(ident())
			val ty =
			    protect(protectScope, (fn () =>
			      (* localize type variables *)
			      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
				(checkToken(COLON); ty()) before
				(* generalize type variables *)
				(List.app (fn (TYVAR{status,...}) => status := BOUND)
					  (currentTyvars()))))))
			val const = isUnitTy(ty)
		     in if !debug
			  then (print "exnspec1: "; printSym name; newline())
			  else ();
			bindEXN(name, DATACON{name=name,const=const,
				      	      vtype=(if const then exnTy
						     else ty-->exnTy),
					      rep=ref(VARIABLE(LVAR(0))),
					      dcons=exnDcons})
		    end

	     in case !nextToken
		  of ID => (* for subsignatures only (?) !! *)
			let val SIGvar{binding,...} = lookSIG(!idValue) before advance()
			    val sgn as STRstr{kind=SIGkind{stampcounts={s,t},...},...} =
				     SigMatch.newsig {s= !strCount,t= !tycCount} binding
			 in tycCount := t + !tycCount;
			    strCount := s + !strCount;
			    sgn
			end
		   | Token.SIG => 
			let val s0 = (advance(); mark())
			    val s1 = (openStr();
					  List.app openOld (!pervasives);
					  mark())
			    val _ =  (spec_s(); checkToken(END))
			    val (bindings,table) = buildSigTable s1
		         in STRstr{stamp=nextStrStamp(),
				   sign= SOME (!genStrStamp()),
				   table=table,
				   env={s=arrayoflist(rev(!sComps)),
				        t=arrayoflist(rev(!tComps))},
				   kind=SIGkind{shpaths=[],bindings=bindings,
						stampcounts={t= !tycCount,
							s= !strCount}}}
			    before close s0
			end
		   | _ => condemn("expected a signature or signature-identifier")
	    end
     in sgn()
    end

(* share-specs go in signatures
and sharespec_op() : shareSpec list option =
    if at(SHARING)
       then SOME(rightAssoc(sharespec,AND,op ::,single))
       else NONE

and sharespec() : shareSpec =
    case !nextToken
      of STRUCTURE => (advance(); STRshare(patheqn()))
       | TYPE => (advance(); TYCshare(patheqn()))
       | _ => condemn("expected structure or type")

and patheqn() : spath list =
    rightAssoc(spath,EQUAL,op ::,single)
    (* BUG -- should check that length > 1 *)
*)

fun sigconstraint () =
    (checkToken(COLON);
     sign())

fun sigconstraint_op () =
    if !nextToken = COLON
      then (advance(); SOME(sign()))
      else NONE

(* signature bindings *)

fun sigb() = 
    let fun sigb1() =
	    let val name = ident()
	     in checkToken(EQUAL);
		let val sgn = sign()
		 in bindSIG(name, SIGvar{name=name,binding=sgn});
		prstr "signature "; printSym name; newline();
		    sgn
		end
	    end
     in rightAssoc(sigb1,AND,discard,discard)
    end

(* structure expressions *)

fun str(constraint: Structure option) =
    case !nextToken
      of IDDOT =>
	   (if !debugStr then (prstr "str--IDDOT\n") else ();
	    let val strVar as STRvar{binding,...} = qid(lookSTRinStr)
	     in case constraint
		  of NONE => (VARstr strVar, binding, NONE)
		   | SOME sgn =>
		       let val (str,thin) = SigMatch.sigMatch(SigMatch.defaultMapfns,
					    		      sgn,binding)
		        in (VARstr strVar, str, thin)
		       end
	    end)
       | Token.STRUCT => 
	   (advance();
	    if !debugStr then (prstr "str--STRUCT\n") else ();
	    let val s0 = mark()
	        val s1 = (openStr(); mark())
		val s2=  (List.app openOld (!pervasives); mark())
	        val body = if !nextToken = END then nil else sdecs()
	     in checkToken(END);
		(case constraint
		  of NONE =>
		       let val (thin,table) = buildStrTable s2
			in (STRUCTstr{body=body,locations=thin},
			    mkSTR(NONE,table,emptyStrenv),
			    NONE)
		       end
		   | SOME sgn =>  (* inconsistent type of locations *)
		       let val (struc,thin) =
			         (splice(s1,s2);
				  SigMatch.sigFill(SigMatch.defaultMapfns,
						    !genStrStamp(),sgn))
		        in (STRUCTstr{body=body,locations=thin}, struc, NONE)
		       end)
		before close s0
	    end)
       | ID => 
	    let val id = getSymbol()
	     in if at(LPAREN)
	          then (if !debugStr
		          then (prstr "str--ID(FCTAPP): "; printSym id; newline())
			  else ();
		       let 
val _ = print "fctapp1\n"
			   val fctVar as FCTvar{binding=fct,...} = lookFCT id
val _ = print "fctapp2\n"
			   val (argexp,argstr,_) = str(NONE) before checkToken(RPAREN)
val _ = print "fctapp3\n"
			   val (result,thin1) = SigMatch.fctApply(fct,argstr)
val _ = print "fctapp4\n"
			   val strexp = APPstr{oper=fctVar, 
				               argexp=argexp,
					       argthin=thin1}
		        in case constraint
			     of NONE =>	(strexp,result,NONE)
			      | SOME sgn =>
				  let
val _ = print "fctapp5\n"
 val (struc,thin2) =
				        SigMatch.sigMatch(SigMatch.defaultMapfns,
							  sgn,result)
				   in (strexp,result,thin2)
				  end
		       end)
	          else let val strVar as STRvar{binding,...} = lookSTR id
	   	        in if !debugStr
			     then (prstr "str: id = "; 
				   printSym id; prstr "\n  ";
				   PrintBasics.printStr binding; newline())
			     else ();
		           case constraint
			    of NONE => (VARstr strVar, binding, NONE)
			     | SOME sgn =>
			        let val (str,thin) =
					  SigMatch.sigMatch(SigMatch.defaultMapfns,
					  		    sgn,binding)
				 in (VARstr strVar, str, thin)
				end
		       end
	    end
       | _ => condemn "expected a structure-expression"

and sdecs() =
    sdec() ::
    ( at(SEMICOLON);
      if firstSdec(!nextToken)
         then sdecs()
         else nil )

and sdec() =
    if at(STRUCTURE)
      then STRdec(strb())
      else let val dec = ldec()
	    in Typecheck.decType(dec); dec
	   end

(* structure bindings *)

(* only single structure decs for now
and strb() =
    rightAssoc(strb1, AND, op ::, single)
*)
and strb() =  (* was strb1 *)
    let val name = ident()
	val _ = if !debugStr
		  then (prstr "strb:\n  name="; printSym name; newline())
		  else ()
	val constraint = sigconstraint_op()
	val _ = if !debugStr
		  then (case constraint of NONE => prstr "  no constraint\n"
				        | SOME _ => prstr "  constraint\n")
		  else ()
	val (strexp,str,thin) = (checkToken(EQUAL); str(constraint))
	val _ = if !debugStr
		  then ((*prstr "def absyn:\n"; PrintAbsyn.printStrexp(strexp);*)
		        prstr "def str: "; PrintBasics.printStr str; newline())
		  else ()
	val strVar = STRvar{access=LVAR(namedLvar(name)),
			    name=name,
			    binding=str}
     in	if !debugStr then prstr "binding str\n" else ();
	bindSTR(name, strVar);
	prstr "structure "; printSym name; newline();
	STRB{strvar=strVar, def=strexp, constraint=constraint, thin=thin}
    end


(* functor bindings *)

(* only single functor decs for now
and fncb() =
    rightAssoc(fncb1, AND, op ::, single)
*)

and fncb(): fctb =  (* was fncb1 *)
    let val name = ident()
	val param as STRvar{name=pname,binding=argstr,...} =
	      (checkToken(LPAREN); param() before checkToken(RPAREN))
	val resSign = sigconstraint_op() before checkToken(EQUAL)
	val lev = mark()
	val STRstr{kind=SIGkind{stampcounts={t=tCount,s=sCount},...},...} = argstr
	val tycStampCount = ref tCount
	val strStampCount = ref sCount
	val old_genTycStamp = !genTycStamp
	val old_genStrStamp = !genStrStamp
     in protect(((fn () =>
		 (genTycStamp := (fn () => !tycStampCount before inc tycStampCount);
	          genStrStamp := (fn () => !strStampCount before inc strStampCount))),
		(fn () => (genTycStamp := old_genTycStamp;
			   genStrStamp := old_genStrStamp))),
	   ( fn() =>
	let val _ = bindSTR(pname,param)
	    val (strexp,body,thin) = str(resSign)
	    val fctv = FCTvar{name=name, 
			      access=LVAR(namedLvar(name)),
			      binding=FUNCTOR{param=argstr,tycCount=(!tycStampCount),
			    		      body=body}}
	    val fctb = FCTB{fctvar=fctv, param=param, def=strexp, thin=thin,
		            constraint=resSign}
	 in close(lev);
	    bindFCT(name,fctv);
	    prstr "functor "; printSym name; newline();
	    fctb
        end))
    end

and param() : structureVar =
    let val name = ident()
	val sgn = sigconstraint()
     in STRvar{name=name,
	       access=LVAR(namedLvar(name)),
	       binding=sgn}
    end


(* top level declarations *)

exceptionx Eof

fun tdec() =
   let fun protect f = let val m = mark()
		        in (f() before (if !anyErrors then close m else ()))
			   handlex Syntax => (close m; raisex Syntax)
		       end
    in case !nextToken
      of SIGNATURE => (advance(); protect sigb; tdec())
       | Token.FUNCTOR => (advance(); FCTdec(protect fncb))
       | STRUCTURE => (advance(); STRdec(protect strb))
       | SEMICOLON => (advance(); tdec())
       | EOF => raisex Eof
       | _ => condemn "signature, functor, or structure expected"
   end
     handlex Syntax => (anyErrors := false; tryAgain())

and tryAgain() =
    if !nextToken = EOF then raisex Eof
    else if firstTdec(!nextToken ) then tdec()
    else (advance(); tryAgain())

val itsym = SymbolTable.stringToSymbol "it"

fun interdec() =
   let fun protect f = let val m = mark()
		        in (f() before (if !anyErrors then close m else ()))
			   handlex Syntax => (close m; raisex Syntax)
		       end
    in case !nextToken
      of SIGNATURE => (advance(); protect sigb; interdec())
       | Token.FUNCTOR => (advance(); FCTdec(protect fncb))
       | STRUCTURE => (advance(); STRdec(protect strb))
       | SEMICOLON => (advance(); interdec())
       | EOF => raisex Eof
       | _ => let val dec = if firstLdec(!nextToken) then protect ldec
			      else if firstExp()
			        then VALdec[VB{exp=protect exp,
			           pat=let val v = newVAR(ref nil, itsym)
				        in bindVAR(itsym,v);
					    VARpat v
				       end,
			           tyvars=nil}]
			        else condemn "declaration or expression expected"
		in Typecheck.decType dec; dec
	       end
   end
     handlex Syntax => (Lex.flush(); advance(); anyErrors := false; interdec())

end (* local *)

end (* structure Parse *)
