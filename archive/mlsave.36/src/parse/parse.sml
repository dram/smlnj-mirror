(* parse.sml *)

structure Parse : PARSE = struct

structure Basics = Basics
structure BareAbsyn = BareAbsyn
structure Env = Env

local
  open ErrorMsg Symbol PrintUtil Lex
  open Lex.Token (* Token *)
  open Access Basics BasicTypes TypesUtil Absyn
  open EnvAccess.Env (* Env *)
  open EnvAccess
  open FirstSets
  open Misc

  infix -->
in

(* constants *)

val maxTypSpecs = 100  (*maximum number of type specs in a signature *)
val maxStrSpecs = 100  (*maximum number of structure specs in a signature *)

(* utility functions *)

val nestingLevel = ref 0;
val protectNest = ((fn () => (!nestingLevel before inc nestingLevel)),
		   (fn i => nestingLevel := i))
fun topLevel() = !nestingLevel = 1

fun reset() = (nestingLevel := 0)

fun expop () =
    case !nextToken
      of EQUAL => lookFIX(EQUALsym)
       | ASTERISK => lookFIX(ASTERISKsym)
       | ID s => lookFIX(s)
       | _ => NONfix

fun patop () =
    case !nextToken
      of ASTERISK => lookFIX (ASTERISKsym)
       | ID s => lookFIX(s)
       | _ => NONfix

fun ident() =
    case !nextToken
      of ID s => (advance();s)
       | ASTERISK => (advance();ASTERISKsym)
       | EQUAL => (advance();EQUALsym)
       | tok => (complain("expected identifier, found " ^ tokenName tok);
		 bogusID)

fun nonfix_ident() =
	if (case !nextToken of
	      ID s => lookFIX(s)=NONfix
	    | ASTERISK => lookFIX(ASTERISKsym)=NONfix
	    | _ => false)
	 then getSymbol()
	 else (complain("expected nonfix-identifier, found "
			  ^ tokenName(!nextToken));
		 bogusID)
    
fun opid() =
    case !nextToken
      of ID s	=> nonfix_ident()
       | ASTERISK => nonfix_ident()
       | OP	=> (advance(); 
		    case !nextToken
		     of ID s => getSymbol()
		      | ASTERISK => getSymbol()
		      | EQUAL => getSymbol()
		      | tok => (complain ("op not followed by identifier, found "
				        ^ tokenName tok); bogusID))
       | tok => (complain("expected identifier or OP, found " ^ tokenName tok);
		 bogusID)

fun getSTR id = lookSTR id
		 handle Unbound => 
		    (complain("unbound structure name: " ^ name id);
		     bogusSTR)

fun getEXN id = lookCON(id) handle Unbound => unboundEXN id

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
              if at(AND) then () else raise Syntax)
        else raise Syntax

fun andList(elem) =
    let val e1 = elem()
     in (if at(AND) then e1 :: andList(elem) else [e1])
(*	handle Syntax =>
	  (synchAND(); andList(elem))  turned off for debugging *)
    end

fun andListProtect(elem) = andList (fn () => protect(protectScope,elem))

(* parsing functions *)

(* qualified id interpretation *)

fun symPath() =
    case !nextToken
      of IDDOT s => getSymbol() :: symPath()
       | ID s => [getSymbol()]
       | ASTERISK => [getSymbol()]
       | EQUAL => [getSymbol()]
       | _ => (complain "incomplete qualified identifier"; [])

fun qid(lookLast) = lookPath(symPath(),lookLast)

(* record labels *)

fun selector() =
    let fun sel1 id = 
	    let val v = namedLvar id
		val tyref = ref UNDEFty
		val v1 = VALvar{name=id,access=LVAR(v),vtype=tyref}
		val v2 = VALvar{name=id,access=PATH[v],vtype=tyref}
	     in FNexp[RULE(RECORDpat{fields=[(id,VARpat v1)],
				     flex=true,
				     typ=ref UNDEFty, pats=ref nil},
			   VARexp(ref v2))]
	    end
     in case !nextToken
	  of ID _ => sel1(ident())
	   | INT i => let val s = makestring i
		       in if i < 1
			  then complain ("nonpositive integer label in selector,\
			                 \ found " ^ s)
			  else ();
			  sel1(Symbol.symbol(s))
		      end
	              before advance()
	   | _ => (complain "illegal selector function"; bogusExp)
    end

fun labels(parseOne, separator, dotsOK, abbrev) =
    if (case !nextToken
          of ID _ => true
	   | INT _ => true
	   | DOTDOTDOT => true
	   | _ => false)
    then let fun lablist () = 
	         case !nextToken
		   of ID _ => field(ident(),abbrev)
		    | INT i => let val s = makestring i
			       in advance();
				  if i < 1
				  then complain ("nonpositive integer label, \
						 \found " ^ s)
				  else ();
				  field(Symbol.symbol(s),
				          (fn id =>
					    condemn("numeric label abbreviation" ^
						    Symbol.name id)))
			       end
		    | DOTDOTDOT => nil
		    | tok => (complain("expected label, found " ^
					tokenName tok); nil)
	     and field(id,abbrev) =
		 (id,
		  if at(separator) then parseOne()
		    else if !nextToken = COMMA orelse
			    !nextToken = COLON orelse
			    !nextToken = AS orelse
			    !nextToken = RBRACE then abbrev(id)
		    else condemn("expected " ^ Token.tokenName separator ^
			         " after label, found " ^
				 tokenName(!nextToken)),
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

exception Clausal of symbol * pat   (* for returning clausal patterns from pat *)


(* types *)

fun noAbbrev(_) = 
    (complain "expected colon after label in record type, found comma";
     UNDEFty)	    
fun ty() =
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
	      of ID s =>
		   qid_s(CONty(!lookArTYC(getSymbol(),1), [t]))
	       | IDDOT s =>
		   qid_s(CONty(!lookPathArTYC(symPath(),1), [t]))
	       | _ => t
     in qid_s(case !nextToken
	       of LPAREN =>
		    let val t1 = (advance(); ty())
		     in if at(RPAREN)
			then t1
			else if at(COMMA)
			then let val tys = t1 :: ty_pc()
				 val arity = length tys
			      in checkToken(RPAREN);
				 case !nextToken
				  of ID s =>
				      CONty(!lookArTYC(ident(),arity),
					    tys)
				   | IDDOT s => 
				      CONty(!lookPathArTYC(symPath(),arity),
					    tys)
				   | tok => condemn("expected type \
					    \constructor, found "
					    ^ tokenName tok)
			     end
			else (complain("expected RPAREN or COMMA in type\
				\args, found " ^ tokenName(!nextToken));
			      t1)
		    end
		| ID s	=> CONty(!lookArTYC(ident(),0),[])
		| IDDOT s => CONty(!lookPathArTYC(symPath(),0),[])
		| Token.TYVAR s => VARty(lookTyvar(getSymbol()))
		| LBRACE =>
		    (advance();
		     let val (l,sl,_) = labels(ty,COLON,false, noAbbrev)
		      in recordTy(map (fn (id,ty,_) => (id, ty)) sl)
		     end)
		| tok => condemn("expected a type expression, found token "
				 ^ tokenName tok))
    end
and ty_pc() = rightAssoc(ty,COMMA,op::,single)


fun markexp f x = if !System.Control.Debug.debugging
		 then let val one = (!ErrorMsg.fileName,!ErrorMsg.lineNum)
		      val e = f x
		      val two = (!ErrorMsg.fileName,!ErrorMsg.lineNum)
		   in case e
			of MARKexp _ => e
		         | e' => if one=two then MARKexp(e',one,one)
					    else MARKexp(e',one,two)
		  end
		else f x

fun markdec f x =  if !System.Control.Debug.debugging
		 then let val one = (!ErrorMsg.fileName,!ErrorMsg.lineNum)
		      val e = f x
		      val two = (!ErrorMsg.fileName,!ErrorMsg.lineNum)
		   in case e
			of MARKdec _ => e
		         | e' => if one=two then MARKdec(e',one,one)
					    else MARKdec(e',one,two)
		  end
		else f x


(* expressions -- including local declarations *)

fun exp () =
    case !nextToken
     of FN => (advance(); FNexp(match()))
      | CASE => CASEexp((advance(); exp()),
			(checkToken(OF); match()))
      | WHILE => WHILEexp((advance(); exp()), (checkToken(DO); exp()))
      | IF => IFexp((advance(); exp()), (checkToken(THEN); exp()),
       	            (checkToken(ELSE); exp()))
      | RAISE => RAISEexp(advance(); exp())
      | _ => let val e = exp1()
              in if !nextToken = HANDLE
		 then (advance(); HANDLEexp(e,HANDLER(FNexp(match()))))
		 else e
             end

and match () = rightAssoc(rule,BAR,op::,single)

and rule () = 
     let val bl = ref nil : (symbol * var) list ref
      in protect(protectScope,
	    (fn () => RULE(pat(bl,true)
		           handle Clausal(id,_) => 
			     condemn("undefined op in pattern: "^name id),
		           (checkToken(DARROW);
			    if !nextToken=EQUAL then advance() else ();
(* Capitalization convention
		            app checkBinding (!bl);
*)
			    bindVARs(!bl); markexp exp()))))
     end

and exp_ps () = rightAssoc(exp,SEMICOLON,op::,single)

and exp1 () = leftAssoc(exp2,ORELSE,ORELSEexp,(fn x=>x))

and exp2 () = leftAssoc(exp3,ANDALSO,ANDALSOexp,(fn x=>x))

and exp3() = let val e = precedence(markexp exp5, 
		(fn(id,a,b)=>markexp APPexp(markexp lookID(id),
				    markexp TUPLEexp[a,b])), expop)
	      in if at(COLON) then CONSTRAINTexp(e,ty()) else e
	     end

and exp5 () =
     let fun loop e = if firstAexp(!nextToken)
			    then loop(APPexp(e,markexp aexp ()))
			    else e
      in loop(markexp aexp ())
     end

(* note that IF WHILE CASE RAISE FN  are matched below, but
   are not in firstAexp.  This is intentional *)

and aexp () =
     case !nextToken
       of ID _	 => lookID(nonfix_ident())
        | OP	 => lookID(opid())
	| IDDOT s  => qid(lookIDinStr)
        | INT i	 => INTexp(i) before advance()
        | REAL s => REALexp(s) before advance()
        | STRING s => STRINGexp(s) before advance()
	| HASH => (advance(); selector())
        | LBRACE => ( advance(); exp_brace() )
        | LPAREN => ( advance(); exp_paren() )
        | LBRACKET => ( advance(); exp_bracket() )
        | LET	 => protect(protectScope,
		     (fn()=>(advance();
		     	     (LETexp(ldecs[], (checkToken(IN); SEQexp(exp_ps()))))
			     before checkToken(END))))
        | FN =>   exp()
        | CASE => exp()
        | WHILE => exp()
        | IF => exp()
	| RAISE => exp()
        | tok	 => (complain ("atomic expression expected, found " ^
			       tokenName tok);
	            bogusExp)

and exp_brace () =
    let val (l,sl,_) =
	    labels(exp,EQUAL,false,
	           (fn x => (complain "illegal record-name element abbreviation";
		             bogusExp)))
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
		    | tok => (complain ("expected comma, right paren, or\
			     \ semicolon, found " ^ tokenName tok); e)
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

and pat (bl: (symbol * var) list ref, full: bool) =
    (* full false means parse atomic pattern *)
   let fun restrictLAYEREDpat(x as (VARpat _, _)) = LAYEREDpat x
         | restrictLAYEREDpat(y,z) =
	      (complain "pattern to left of AS must be a variable"; z)

       fun pat0 () = rightAssoc(pat1,AS,restrictLAYEREDpat,(fn x=>x))

       and pat1 () = 
	   let val e = precedence(
		         pat2, 
		         (fn(id,a,b)=>
			    APPpat(lookCON id, TUPLEpat[a,b])
			    handle Unbound => 
			      raise Clausal(id, TUPLEpat[a,b])),
			 patop)
	    in if at(COLON) then CONSTRAINTpat(e,ty()) else e
	   end

       and pat2 () =
	   let fun useCon(dcon as DATACON{const,name,...}) =
		    case (const,firstApat(!nextToken))
		      of (true,false) => CONpat(dcon)
		       | (false,true) => APPpat(dcon,apat())
		       | (_,x) => (complain("improper use of constructor "^
			              Symbol.name(name)^" in pattern");
				   (if x then (apat(); ()) else ());
				   WILDpat)
	       fun simpleId(id) =
		   useCon(lookCON id)
		   handle Unbound => 
		     if firstApat(!nextToken)
		       then raise Clausal(id, apat())
		       else VARpat(newVAR(bl,id))
	    in case !nextToken
		 of ID s => (if lookFIX(s) = NONfix
			    then ()
			    else complain("pattern starts with infix: "
					 ^ name(s));
			   simpleId(getSymbol()))
		  | OP => simpleId(opid())
		  | IDDOT s => useCon(qid lookCONinStr)
		  | _ => apat()
	   end

       and pat_id(id) = 
	   (case lookCON id
	     of	dcon as DATACON{const=true,...} => CONpat(dcon)
	      | _ => (complain("nonconstant data constructor: " ^ name(id));
		      WILDpat))
	   handle Unbound => VARpat(newVAR(bl,id))

       and apat() = 
	   case !nextToken
	     of OP	=> pat_id(opid())
	      |	ID s	=> pat_id(nonfix_ident())
	      | IDDOT s   => CONpat(qid(lookCONinStr))
	      |	INT i	=> INTpat(i) before advance()
	      | REAL s  => REALpat(s) before advance()
	      | STRING s => STRINGpat(s) before advance()
	      | WILD	=> (advance(); WILDpat)
	      |	LPAREN =>  (advance(); pat_paren())
	      |	LBRACKET => (advance(); pat_bracket())
	      |	LBRACE =>  (advance(); pat_brace())
	      | tok => (complain("expected an atomic pattern, found " 
			         ^ tokenName tok); WILDpat)

       and pat_paren () =
	    if at(RPAREN)
	       then unitPat
	       else let val p = pat0()
		     in case !nextToken of
			    RPAREN => (advance(); p)
			  | COMMA =>
			     (advance();
			      TUPLEpat(p::pat_pc()) before checkToken(RPAREN))
			  | tok => (complain ("expected right paren or comma\
				   \ (in pattern), found " ^ tokenName tok);
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
	    let val e = pat_id id
                val e' = if at(COLON) then CONSTRAINTpat(e,ty()) else e
	     in if at(AS) then LAYEREDpat(e',pat0()) else e'
	    end

       and pat_brace () =
	   let val (_,sl,dots) = labels(pat0,EQUAL,true,pat_id_as)
	    in RECORDpat{
	         fields = map (fn(id,pat,_) => (id,pat)) sl,
		 flex = dots,
		 typ = ref UNDEFty,
		 pats = ref nil}
	   end

       and pat_pc() = rightAssoc(pat0,COMMA,op::,single)

    in if full then pat0() else apat()
   end

(* variable bindings *)

and recdec x = VALRECdec(rvb_pa x)
and valdec x = VALdec(vb_pa x)
and vb x = markdec (if at(REC) then recdec else valdec) x    

and vb_pa () =
    let val bl = ref nil : (symbol * var) list ref
	fun vb () = 
	    protect(protectTyvars(NONE),
	      (fn () =>
		let val pat = pat(bl,true)
			      handle Clausal(id,_) =>
			        condemn("undefined op in pattern: "^name id)
		    and exp = (checkToken(EQUAL); exp())
		    and tvs = currentTyvars()
		 in case (pat,exp)
		      of (CONSTRAINTpat(VARpat(VALvar{name,vtype,...}), ty),
			   VARexp(ref(VALvar{access as INLINE _,...})))
			   => let val _::rest = !bl
				  val w = VALvar{name=name,vtype=vtype,access=access}
			       in bl := (name,w) :: rest;
			          VB{pat=CONSTRAINTpat(VARpat w, ty),
				     exp=exp,tyvars=tvs}
			      end

		       | (VARpat(VALvar{name,vtype,...}),
			   VARexp(ref(VALvar{access as INLINE _,...})))
			   => let val _::rest = !bl
				  val w = VALvar{name=name,vtype=vtype,access=access}
			       in bl := (name,w) :: rest;
			          VB{pat=VARpat w, exp=exp, tyvars=tvs}
			      end
		       | _ => VB{pat=pat,exp=exp,tyvars=tvs}
		end))
     in andListProtect(vb)
	before bindVARs(!bl)
    end

and rvb_pa () = 
    let val bl = ref nil : (symbol * var) list ref
	fun rvb () =  protect(protectTyvars(NONE),
	      (fn () =>  (* record bug *)
		let val var=newVAR(bl,opid())
		    and resultty=constraint_op()
		    and e = ( checkToken(EQUAL); exp() )
		    and tvs=currentTyvars()
		 in case e of FNexp _ => ()
		       | MARKexp(FNexp _,_,_) => ()
		       | _ => complain "fn expression required in val rec declaration";
		    RVB{var = var, resultty = resultty, exp = e, tyvars = tvs}
		end))
     in protect(protectPatchList, (fn()=>
	  protect(protectScope, (fn()=>
	    (openRec(); andListProtect(rvb)) ))
	  before bindVARs(!bl) ))
    end

and fb_pa () = 
    let val bl = ref nil : (symbol * var) list ref
	fun fb () = protect(protectTyvars(NONE),
	  (fn () =>
	     let val funSymbol: symbol option ref = ref NONE
	         val clauses=rightAssoc(clause(funSymbol),BAR,op::,single)
		 val CLAUSE{pats=p1,...}::_ = clauses
		 val len = length p1
	      in if exists (fn CLAUSE{pats,...} => length pats <> len) clauses
		  then complain "not all clauses have the same number of patterns"
		  else ();
		 FB{var = let val SOME id = !funSymbol in newVAR(bl,id) end,
		    clauses = clauses,
		    tyvars = currentTyvars()} 
	     end))
     in protect(protectPatchList, fn()=>
	  protect(protectScope, fn()=>
	    (openRec(); markdec (FUNdec o andListProtect) fb))
	  before bindVARs(!bl))
    end

and clause funsym () = 
    let val bl = ref nil : (symbol * var) list ref
        fun pat_p () = if firstApat(!nextToken)
			 then (pat(bl,false)  (* atomic pattern *)
			       handle Clausal(id,_) =>
				 condemn("undefined op in pattern: "^name id))
			      :: pat_p ()
			 else nil
     in (pat(bl,true); condemn("no defined function in clausal lhs"))
	handle Clausal(id,pat1) =>
          (case !funsym
	     of NONE => funsym := SOME id
	      | SOME f => if Symbol.eq(id,f) then ()
		            else complain "identifiers in clauses don't match";
	   let val pats = pat1::pat_p()
	       val resultty = constraint_op()
	       val exp = protect(protectScope,
			   (fn()=>(checkToken(EQUAL);
			           if !nextToken=DARROW then advance() else ();	
				   bindVARs(!bl); markexp exp ())))
	    in CLAUSE{pats=pats, resultty=resultty, exp=exp}
	   end)
    end

and constraint () = (checkToken(COLON); ty())

and constraint_op() =
    if at(COLON)
    then SOME(ty())
    else NONE

and tb(path,notwith) = 
    let	fun tb1() =
	    let fun equalargs([],[]) = true
		  | equalargs(tv::rest,VARty(tv')::rest') =
		      tv = tv' andalso equalargs(rest,rest')
		  | equalargs _ = false
		val args = tyvars()
		val name = ident()
		val _ = checkToken(EQUAL)
		val typ = protect(protectTyvars(SOME args), ty)
	        val _ = TypesUtil.bindTyvars args;
		val binding =
		      case typ
			of CONty(tycref as ref(TYCON{stamp,arity,eq,path=path',kind}),
				 args') =>
			     if equalargs(args,args')
			     then case kind
				    of UNDEFtyc _ =>
				        (tycref :=
					  TYCON{stamp=stamp,arity=arity,eq=eq,
						path=path',
						kind=UNDEFtyc(SOME(name::path))};
					 tycref)
				     | _ => ref(TYCON{stamp=stamp,arity=arity,eq=eq,
					    	      path=name::path,kind=kind})
			     else ref(mkDEFtyc(name::path,
				  	       TYFUN{arity=length args, body=typ},
					       if notwith 
					       then if isEqType typ then YES else NO
					       else MAYBE))
			 | _ => ref(mkDEFtyc(name::path,
					     TYFUN{arity=length args, body=typ},
					     if notwith 
					     then if isEqType typ then YES else NO
					     else MAYBE))
	     in bindTYC(name,binding);
		TB{tyc=binding,def=typ}
	    end
     in TYPEdec(andList(tb1))
    end

and tyvars() =
    case !nextToken
      of Token.TYVAR s => [mkTyvar(mkUBOUND(s))] before advance()
       | LPAREN =>
	    (advance();
	     tyvar_pc() before
	     checkToken(RPAREN))
       | _ => nil

and tyvar_pc() = rightAssoc(tyvar,COMMA,op::,single)

and tyvar() = mkTyvar(mkUBOUND(
	       case !nextToken
	        of Token.TYVAR s => (advance(); s)
		 | tok => (complain ("expected type variable, found "
				    ^ tokenName tok); bogusID)))

and db(path) =
    let val (datatycs,withtycs) =
		protect(protectDb(), (fn()=>
		  (andList(db1(ty,path)),
		   if at(WITHTYPE)
		   then let val TYPEdec x = tb(path,false) in x end
		   else nil)))
	val checkeq = defineEqTycon (fn x => x)
     in app (fn (ref tyc) => checkeq tyc) datatycs;
	app (fn TB{tyc,...} => checkeq(!tyc)) withtycs;
	DATATYPEdec{datatycs=datatycs,withtycs=withtycs}
    end

and db1(parsety,path) () =
    let val args = tyvars()
   	val name = ident()
	val arity = length args
	val rangeType = CONty(!lookArTYC(name,arity), map VARty args)
	fun constr() =
	    let val sym = (if at OP
			   then warn "unnecessary op in datatype declaration"
			   else ();
			   ident())
		val const = not(at(OF))
		val typ = if const then rangeType
				else CONty(arrowTycon, [parsety(), rangeType])
	     in (sym,const,typ)
            end
     in protect(protectTyvars(SOME args),
	 (fn()=>
	   let val dcl = (checkToken(EQUAL); rightAssoc(constr,BAR,op::,single))
	       val sdcl = sort3 dcl
	       val sign = ConRep.boxed(sdcl)
               fun binddcons ((sym,const,typ)::restdcl,rep::restsign) =
		   let val dcon =
		       DATACON{name = sym, const = const, rep = rep, sign = sign,
			       typ = if arity > 0
				     then POLYty
					   {sign=mkPolySign arity,
					    tyfun=TYFUN{arity=arity,body=typ}}
				     else typ}
		    in bindCON(sym, dcon);
		       dcon :: binddcons(restdcl,restsign)
		   end
	         | binddcons ([],[]) = []
	         | binddcons _ = impossible "Parse.db1.fn.binddcons"
	     in if length sdcl < length dcl
	       	    then complain "duplicate constructor name" else ();
		TypesUtil.bindTyvars args;
		let val tycref = ref(mkDATAtyc(name::path,arity,
				               binddcons(sdcl,sign),MAYBE))
		 in bindTYC(name,tycref);
		    tycref
		end
	    end))
    end

and ab(path) =
    let val mAbstype = openScope()
	val DATATYPEdec{datatycs,withtycs} = db(path)
	val withtycons = map (fn TB{tyc,...} => tyc) withtycs
	val _ = makeAbstract(datatycs,withtycons)
	val mWith = openScope()
	val body = (checkToken(WITH); ldecs(path))
	fun bind tyc = bindTYC(tycName(!tyc), tyc)
     in checkToken(END);
	splice(mAbstype,mWith);
	app bind datatycs;
	app bind withtycons;
	ABSTYPEdec{abstycs=datatycs,withtycs=withtycs,body=body}
    end

and eb() = EXCEPTIONdec(andList(eb1))

and eb1() =
    let val name = ident()
(* Capitalization convention
	val _ = if looksLikeExn name then ()
		else warn "Exception name should be capitalized"
*)
     in case !nextToken
	  of OF =>
	      (advance();
	       let val etype = ty()
		   val exn = DATACON{name = name,
				     const = false,
				     typ = etype --> exnTy,
				     rep = VARIABLE(LVAR(namedLvar(name))),
				     sign = []}
	        in bindCON(name,exn);
		   EBgen{exn=exn,etype=SOME etype}
	       end)
	   | EQUAL =>
	       (advance();
	        let val edef as DATACON{const,typ,rep,sign,...} = 
		        case !nextToken
			  of IDDOT s => qid lookEXNinStr
			   | ID s => getEXN(getSymbol())
			   | tok =>
			     (complain("expected exception name, found token"
				       ^ tokenName tok);
			      unboundEXN(bogusID))
		    val exn = DATACON{name=name,const=const,typ=typ,sign=sign,
			      	      rep=VARIABLE(LVAR(namedLvar(name)))}
		 in bindCON(name,exn);
		    EBdef{exn=exn,edef=edef}
		end)
	   | _ =>
	       let val exn = DATACON{name = name,
			             const = true,
				     typ = exnTy,
				     rep = VARIABLE(LVAR(namedLvar(name))),
				     sign = []}
	        in bindCON(name,exn);
		   EBgen{exn=exn,etype=NONE}
	       end
    end


and ebx() = EXCEPTIONdec(andList(eb1x))

and eb1x() =
    let val name = ident()
	val etype = constraint_op()
	val (const,typ) = case etype
			      of NONE => (true,exnTy)
			       | SOME t => if isUnitTy(t)
					   then (true,exnTy)
					   else (false,t-->exnTy)
	val edef = if at(EQUAL)
	           then SOME(case !nextToken
			       of IDDOT _ => qid lookEXNinStr
			        | ID s => getEXN(getSymbol())
				| _ => unboundEXN(bogusExnID) )
		   else NONE
        val exn = case edef
		    of NONE => 
		         DATACON{name=name, const=const, typ=typ,
				 rep=VARIABLE(LVAR(namedLvar(name))),
				 sign=[]}
		     | SOME(DATACON{name=n,const,typ,rep,sign}) =>
			    DATACON{name=name,const=const,typ=typ,rep=rep,
			    	    sign=sign}  (* changes only name *)
     in bindCON(name, exn);
	case edef
	  of NONE => EBgen{exn=exn,etype=etype}
	   | SOME exn' => EBdef{exn=exn,edef=exn'}
    end

and ldec(path) =
   protect(protectNest,
    (fn () =>
     (case !nextToken
	of VAL =>
	      (advance(); vb())
         | FUN =>
	      (advance(); fb_pa())
         | TYPE =>
	      (advance(); tb(path,true))
         | DATATYPE =>
	      (advance(); db(path))
         | ABSTYPE =>
	      (advance(); ab(path))
	 | EXCEPTION =>
	      (advance(); eb())
         | Token.LOCAL =>
	     let val envLocal = openScope()
		 val ld1 = (advance(); ldecs[])
		 val envIn = (checkToken(IN); openScope(); current())
		 val ld2 = ldecs(path)
	      in checkToken(END);
		 splice(envLocal,envIn);
		 LOCALdec(ld1,ld2)
	     end
	 | Token.OPEN =>  (* confusion with Env.OPEN when Env not constrained *)
	      let val strs = (advance(); qid_p())
	       in app openStructureVar strs;
		  OPENdec strs
	      end
         | INFIX =>
	      let val prec = case (advance(); optprec()) of SOME n=>n|NONE=>0
	       in app (fn i => bindFIX(i,FIXvar{name=i,binding=infixleft prec})) (ops());
	          SEQdec(nil)
	      end
         | INFIXR =>
	      let val prec = case (advance(); optprec()) of SOME n=>n|NONE=>0
	       in app (fn i => bindFIX(i,FIXvar{name=i,binding=infixright prec})) (ops());
	          SEQdec(nil)
	      end
         | NONFIX =>
	      (advance();
	       app (fn i => bindFIX(i,FIXvar{name=i,binding=NONfix})) (ops()); SEQdec(nil))
	 | OVERLOAD =>
	      let val id = (advance(); ident())
		  val scheme = (checkToken(COLON);
		    protect(protectScope, (fn () =>  (* localize tyvars *)
		      protect(protectTyvars(NONE), (fn () =>
		        let val body = ty()  (* generalize type variables *)
			    val tvs = currentTyvars()
			 in TypesUtil.bindTyvars tvs;
			    TYFUN{arity=length(tvs),body=body}
			end)))))
		  fun option() =
		      let val VARexp(ref (v as VALvar{vtype,...})) = exp()
		       in {indicator = TypesUtil.matchScheme(scheme,!vtype),
			   variant = v}
		      end
		  val l = (checkToken(AS); andList(option))
	       in bindVAR(id,OVLDvar{name=id,options=ref l,scheme=scheme});
		  SEQdec nil
	      end
         | tok => (complain ("expected a declaration, found " ^
			    tokenName tok); vb()))))

and ldecs(path) =
  let fun ldecs() =
      if firstLdec(!nextToken)
        then ldec(path) :: (at(SEMICOLON); ldecs())
        else []
   in case ldecs() of [dec] => dec | seq => SEQdec seq
  end

and optprec() = case !nextToken of INT i => (advance();SOME(i)) | _ => NONE

and qid_p(): structureVar list =  (* error if no identifier's ? *)
    case !nextToken
      of ID s => getSTR(ident()) :: qid_p()
       | IDDOT _ => qid(lookSTRinStr)::qid_p()
       | _ => nil

and ops() =
  let fun ops1() =
        case !nextToken
          of ID s => (s) :: (advance(); ops1())
	   | EQUAL => (EQUALsym) :: (advance(); ops1())
	   | ASTERISK => (ASTERISKsym) :: (advance(); ops1())
           | _ => nil
   in case ops1()
	of [] => (complain("operator or identifier expected, found "
		  ^ tokenName (!nextToken)); [])
         | l => l
  end


(* signatures *)

val undefTyc = INDtyc []
val undefStr = INDstr(~1)

fun addzeros(0,l) = l
  | addzeros(n,l) = addzeros(n-1,0::l)

fun sign (context: sigContext) =
    let fun sgn(depth: int, context) = 
	    let val tComps = array(maxTypSpecs,undefTyc)
		and tCount = ref 0
		fun tNext x = (update(tComps,!tCount,x);
			       INDtyc[!tCount before inc tCount])
		val sComps = array(maxStrSpecs,undefStr)
 		and sCount = ref 1  (* slot 0 reserved for parent (if any) *)
		fun sNext x = (update(sComps,!sCount,x);
			       INDstr(!sCount before inc sCount))
		val tempenv = {t=tComps,s=sComps}
		fun pairs (nil : spath list list) : (spath*spath) list = nil
		  | pairs ((a::b::r) :: s) = (a,b) :: pairs((b::r) :: s)
		  | pairs ( _ :: s ) = pairs s
		val strSharing : spath list list ref = ref nil
		val typeSharing : spath list list ref = ref nil

		fun dumpSig(sgn as STRstr{kind=SIGkind{stampcounts={s,t},...},
					  ...}) =
		    let val tBase = !tCount
			and sBase = !sCount
			val STRstr{table,env={s=senv,t=tenv},...} = 
			      let val sgn = SigMatch.newsig
					     {s= !strStampCount,t= !tycStampCount} 
					     sgn
			       in tycStampCount := t + !tycStampCount;
				  strStampCount := s + !strStampCount;
				  sgn
			      end

			fun adjust(CONty(ref(INDtyc(p)),args)) =
			      let val newp = case p
					       of [i] => [i+tBase]
						| i::r => (i+sBase)::r
						| _ => impossible "Parse.adjust"
			       in CONty(ref(INDtyc(newp)), map adjust args)
			      end
			  | adjust(CONty(reftyc,args)) =
			      CONty(reftyc, map adjust args)
			  | adjust(POLYty{sign,tyfun=TYFUN{arity,body}}) =
			      POLYty{sign=sign,
			      	     tyfun=TYFUN{arity=arity,body=adjust body}}
			  | adjust ty = ty

			fun rebind(index,s,VARbind(VALvar{name,vtype,...})) =
			      ibindVAR(index,s,mkVALvar(name,ref(adjust(!vtype))))
			  | rebind(index,s,CONbind(DATACON{name,const,typ,rep,sign})) =
			      ibindCON(index,s,DATACON{name=name,const=const,rep=rep,
			      			        sign=sign,
						        typ=adjust typ})
			  | rebind(index,s,TYCbind(ref(INDtyc[i]))) = 
			      (update(tComps,tBase+i,tenv sub i);
			       ibindTYC(index,s,ref(INDtyc[tBase+i])))
			  | rebind(index,s,STRbind(STRvar{name,binding=INDstr i,...})) =
			      (update(sComps,sBase+i,senv sub i);
			       ibindSTR(index,s,STRvar{access=LVAR 0,
						       name=name,
						       binding=INDstr(sBase+i)}))
			  | rebind(index,s,FIXbind fixityvar) =
			      ibindFIX(index,s,fixityvar)
			  | rebind _ = impossible "dumpSig"

		     in IntStrMap.app rebind table;
			tCount := !tCount + Array.length tenv;
			sCount := !sCount + Array.length senv
		    end
		  | dumpSig _ = impossible "Parse.sign.sig.dumpSig"

		fun spec_s() =
		     if firstSpec(!nextToken)
			 then (spec(); at(SEMICOLON); spec_s())
			 else ()
		
		and spec() =
		    case !nextToken
		      of STRUCTURE => (advance(); strspec())
		       | DATATYPE => (advance(); dtyspec())
		       | TYPE => (advance(); tyspec NO)
		       | EQTYPE => (advance(); tyspec YES)
		       | VAL => (advance(); valspec())
		       | EXCEPTION => (advance(); exnspec())
		       | INFIX => (advance(); infixspec(infixleft))
		       | INFIXR => (advance(); infixspec(infixright))
		       | NONFIX => 
			   (advance();
			    app (fn i => bindFIX(i,FIXvar{name=i,binding=NONfix})) (ops()))
		       | SHARING => (advance(); sharespec())
		       | INCLUDE => (advance(); includespec())
		       | tok => condemn("expected a spec (component of signature)\
				\ found " ^ tokenName tok)

		and includespec() =
		    let val name = ident()
			val SIGvar{binding,...} = lookSIG name
		     in dumpSig(binding)
		    end

		and strspec() = 
		    rightAssoc(strspec1,AND,discard,discard)
		
		and strspec1() =
		    let val name = ident()
		     in checkToken(COLON);
			bindSTR(name,STRvar{name=name,access=LVAR(0),
					    binding= sNext(sgn(depth+1,SIGN))})
		    end
		
		and dtyspec() =
		      app (defineEqTycon (tyconInContext tempenv))
			 (protect(protectDb(), (fn() =>
			    map (fn (r as ref tyc) => (r := tNext tyc; tyc))
				(rightAssoc(db1(ty,[]),AND,op ::,single)))))

		and tyspec eq = 
		    rightAssoc(tyspec1 eq, AND, discard, discard)
		
		and tyspec1 eq () =
		    let val arity = length(tyvars())
			val name = ident()
			val tycref = ref(tNext(mkABStyc([name],arity,eq)))
		     in bindTYC(name, tycref);
			tycref
		    end
		
		and valspec() =     
		    rightAssoc(valspec1,AND,discard,discard)
		
		and valspec1() =
		    let val name = 
			    (if at OP
			     then warn "unnecessary op in val specification"
			     else ();
			     case !nextToken
			      of ID s => getSymbol()
			       | ASTERISK => getSymbol()
			       | EQUAL => getSymbol()
			       | tok =>
			          (complain("val spec: expected identifier, found "
				   ^ tokenName tok); bogusID))
			val _ = checkToken(COLON)
			val typ =
			    protect(protectScope, (fn () =>
			      (* localize type variables *)
			      protect(protectTyvars(NONE), (fn () =>
				let val body = ty()
				    val tvs = currentTyvars()
				 in case tvs
				      of [] => body
				       | _ =>
					 let val sign = TypesUtil.bindTyvars1 tvs
					  in POLYty
					      {sign = sign,
					       tyfun = TYFUN{arity = length tvs, 
							     body = body}}
					 end
				end))))
		     in bindVAR(name,mkVALvar(name,ref typ))
		    end
		
		and exnspec() = 
		    rightAssoc(exnspec1,AND,discard,discard)
		
		and exnspec1() =
		    let val name = ident()
			val (const,typ) =
			    if at(OF) then
			      (false,
			       protect(protectScope, (fn () =>
				 (* localize type variables *)
				 protect(protectTyvars(NONE), (fn () =>
				   let val body = ty()
				       val tvs = currentTyvars()
				    in case length tvs
					 of 0 => body --> exnTy
					  | n => 
					    (TypesUtil.bindTyvars tvs;
					     POLYty
					      {sign = mkPolySign n,
					       tyfun = TYFUN{arity = n,
							     body = body --> exnTy}})
				   end)))))
			    else (true,exnTy)
		     in bindCON(name, DATACON{name=name, const=const, typ=typ,
					      rep=VARIABLE(LVAR(0)),
					      sign=[]})
		    end
		
		and infixspec(mkinfix) =
		    let val prec = case optprec() of SOME n=>n|NONE=>0
		     in app (fn i => bindFIX(i,FIXvar{name=i,binding=mkinfix prec})) (ops())
		    end

		and sharespec() =
		    rightAssoc(sharespec1,AND,discard,discard)
		
		and sharespec1() =
		    case !nextToken
		      of TYPE => (advance(); typeSharing := patheqn() :: !typeSharing)
		       | ID s => strSharing := patheqn() :: !strSharing
		       | IDDOT _ => strSharing := patheqn() :: !strSharing
		       | tok => condemn("unexpected token after \"sharing\": "
					^tokenName tok)
		
		and patheqn() : spath list =
		    rightAssoc(symPath,EQUAL,op ::,single)

		fun body() =
		    let val stamp = genStrStamp()
			val _ = openStr()
			val _ = openNew{path=[~depth],strenv=tempenv}
			val savedlookArTYC = !lookArTYC
			val savedlookPathArTYC = !lookPathArTYC
			val _ = protect(
			         ((fn () => (lookArTYC := lookArTYCinSig depth;
					     lookPathArTYC :=
					       lookPathArTYCinSig depth)),
				  (fn () => (lookArTYC := savedlookArTYC;
					     lookPathArTYC := savedlookPathArTYC))),
				 spec_s)
			val (bindings,table) = buildSigTable ()
			val senv = ArrayExt.copy(sComps,!sCount)
			val env = {s=senv, t=ArrayExt.copy(tComps,!tCount)}
			val sShare = pairs(!strSharing)
			val tShare = pairs(!typeSharing)
			val shareSpec =
			      if null sShare andalso null tShare
			      then {s=[],t=[]}
			      else Sharing.doSharing(table,env,{s=sShare,t=tShare})
		        val newsig =
			      STRstr{stamp=stamp,
				     sign=genSigStamp(),
				     table=table,
				     env=env,
				     kind=SIGkind{share=shareSpec,
						  bindings=bindings,
						  stampcounts={t= !tycStampCount,
							       s= !strStampCount}}}
		     in ArrayExt.app((SigMatch.setParent newsig),senv,1);
			newsig
		    end (* fun body *)

	     in case !nextToken
		  of ID s =>
			let val name = s before advance()
			    val SIGvar{binding,...} = lookSIG(name)
			    val sgn as STRstr{kind=SIGkind{stampcounts={s,t},...},...} =
				  SigMatch.newsig {s= !strStampCount,t= !tycStampCount} 
				    binding
			 in tycStampCount := t + !tycStampCount;
			    strStampCount := s + !strStampCount;
			    sgn
			end
		   | Token.SIG => (advance(); body() before checkToken(END))
		   | tok => 
		       (case context
		         of FCTPARAM => body()
			  | SIGN => condemn("expected a signature or \
				            \signature-identifier, \
					    \found: "^tokenName tok))
	    end (* fun sgn *)
	val savedTycStampCount = !tycStampCount
	val savedStrStampCount = !strStampCount
     in protect(((fn () => ()),
		 (fn () => (tycStampCount := savedTycStampCount;
			    strStampCount := savedStrStampCount))),
	(fn() =>
	  (tycStampCount := 0;
	   strStampCount := 0;
	   sgn(1,context))))
    end

fun sigconstraint () =
    (checkToken(COLON);
     sign(SIGN))

fun sigconstraint_op () =
    if !nextToken = COLON
      then (advance(); SOME(sign(SIGN)))
      else NONE

(* signature bindings *)

fun sigb() = 
    let fun sigb1() =
	    let val name = ident()
	     in checkToken(EQUAL);
	     	let val sigvar = SIGvar{name=name,binding=sign(SIGN)}
		 in bindSIG(name, sigvar);
		    sigvar
		end
	    end
     in rightAssoc(sigb1,AND,op ::,single)
    end

(* structure expressions *)

fun str(abs: bool, constraint: Structure option, path: symbol list) =
    case !nextToken
      of IDDOT _ =>
	   let val strVar as STRvar{binding,...} = qid(lookSTRinStr)
	    in case constraint
		 of NONE => (VARstr strVar, binding, NONE)
		  | SOME sgn =>
		      let val (str,thin) = SigMatch.match(abs,path,sgn,binding)
		       in (VARstr strVar, str, thin)
		      end
	   end
       | Token.STRUCT => 
	   (advance();
	    let val _ = openStr()
		val body = sdecs(path)
	     in (case constraint
		   of NONE =>
		       (case !strMode
			  of FCTBODY =>
			      let val (thin,table,env) = buildFctTable ()
			       in (STRUCTstr{body=body,locations=thin},
				   mkSTR(path,table,env),
				   NONE)
			      end
			   | REGULAR =>
			      let val (thin,table) = buildStrTable ()
			       in (STRUCTstr{body=body,locations=thin},
				   mkSTR(path,table,emptyStrenv),
				   NONE)
			      end)
		    | SOME sgn => 
		       let val (str,thin) =
				 SigMatch.realize(abs,path,genStrStamp(),sgn)
			in closeStr();
			   (STRUCTstr{body=body,locations=thin}, str, NONE)
		       end)
		before checkToken(END)
	    end)
       | ID s => 
	   let val id = getSymbol()
	    in if at(LPAREN)
	       then let val fctVar as FCTvar{binding=fct,...} = lookFCT id
			val (argexp,argstr) =
			      (* parse arg without using parameter sig *)
			     (if !nextToken = RPAREN
			      then (STRUCTstr{body=[],locations=[]},nullStr)
			      else if firstSdec(!nextToken)
			      then let val _ = openStr()
				       val body = sdecs([anonParamName])
				       val (thin,table) = buildStrTable ()
				    in (STRUCTstr{body=body,locations=thin},
					mkSTR([anonParamName],table,
					      emptyStrenv))
				   end
			      else let val FUNCTOR{paramName,...} = fct
				       val (strexp,str,_) =
					     str(false,NONE,[paramName])
				    in (strexp,str)
				   end)
			     before checkToken(RPAREN)
			val (result,thin1) = 
			      SigMatch.applyFunctor(fct,argstr,path)
			val strexp = APPstr{oper=fctVar,
					    argexp=argexp,
					    argthin=thin1}
		     in case constraint
			  of NONE => (strexp,result,NONE)
			   | SOME sgn =>
			       let val (thinned,thin2) =
				       SigMatch.match(abs,path,sgn,result)
				in (strexp,thinned,thin2)
			       end
		    end
	       else let val strVar as STRvar{binding,...} = getSTR id
		     in case constraint
			 of NONE => (VARstr strVar, binding, NONE)
			  | SOME sgn =>
			     let val (str,thin) =
				     SigMatch.match(abs,path,sgn,binding)
			      in (VARstr strVar, str, thin)
			     end
		    end
	   end
       | tok => condemn("expected a structure-expression, found " ^
			 tokenName tok)

and sdecs(path) =
    if firstSdec(!nextToken)
    then sdec(path) :: (at(SEMICOLON); sdecs(path))
    else nil

and sdec(path) =
    if at(STRUCTURE)
      then STRdec(strb(false,path))
    else if at(ABSTRACTION)
      then ABSdec(strb(true,path))
    else if at(SIGNATURE)   (* monster structure hack *)
      then (warn "signature found inside structure";
	    SIGdec(sigb()))
    else if at(Token.FUNCTOR)   (* monster structure hack *)
      then (warn "functor found inside structure";
	    FCTdec(fctb()))
    else let val dec = ldec(path)
	  in Typecheck.decType(dec); dec
	 end

(* structure bindings *)

and strb(abstract,path) =
    let fun strb1() =
	let val name = ident()
	    val constraint = 
		  if abstract
		  then SOME(sigconstraint())
		  else sigconstraint_op()
	    val _ = checkToken(EQUAL) 
	    val (strexp,str,thin) = str(abstract,constraint,name::path)
	    val strVar = STRvar{access=LVAR(namedLvar(name)),
				name=name,
				binding=str}
	 in (name, strVar,
	     STRB{strvar=strVar, def=strexp, constraint=constraint, thin=thin})
	end
     in map (fn (name,strVar,strSyn) => (bindSTR(name,strVar); strSyn))
	    (rightAssoc(strb1, AND, op ::, single))
    end


(* functor bindings *)

and fctb() =
    map (fn (name,fctVar,fctSyn) => (bindFCT(name,fctVar); fctSyn))
	(rightAssoc(fctb1, AND, op ::, single))

and fctb1() =
    let val name = ident()
	val mEntry = openScope()
	val param as STRvar{name = pname,
		            binding = pstr as
				       STRstr{kind = SIGkind{stampcounts =
					              {t=tycCount,s=strCount},...},...},
			    ...} =
	     (checkToken(LPAREN);
	      (case !nextToken
		of RPAREN => nullParamVar
	         | ID s => let val name = ident()
			       val param = STRvar{name=name,
					   	  access=LVAR(namedLvar(name)),
						  binding=sigconstraint()}
			    in bindSTR(name,param); param
			   end
		 | tok => if firstSpec(tok)
			  then let val plvar = namedLvar(anonParamName)
				   val pstr as STRstr{env,table,...} = sign(FCTPARAM)
				in openOld({path=[plvar],strenv=env},table);
				   STRvar{name=anonParamName,
					  access=LVAR(plvar),
					  binding=pstr}
			       end
			  else condemn ("expected functor formal, found "
				       ^tokenName tok))
	      before checkToken(RPAREN))
	val resSign = sigconstraint_op() before checkToken(EQUAL)
     in protect(((fn () => (!tycStampCount,!strStampCount)),
		 (fn (t,s) => (tycStampCount := t; strStampCount := s;
			       strMode := REGULAR))),
	(fn() =>
	  (tycStampCount := tycCount;
	   strStampCount := strCount;
	   strMode := case resSign of NONE => FCTBODY | _ => REGULAR;
	   let val (strexp,body,thin) = str(false,resSign,[])
	       val fctv = FCTvar{name=name, 
				 access=LVAR(namedLvar(name)),
				 binding=FUNCTOR{paramName=pname,
				 		 param=pstr,
						 tycCount=(!tycStampCount),
						 body=body}}
	       val fb = FCTB{fctvar=fctv, param=param, def=strexp, thin=thin,
			     constraint=resSign}
	    in resetEnv(mEntry);
	       (name,fctv,fb)
	   end)))
    end


(* top level declarations *)

fun importdec()=
    let fun loop() = 
            (case !nextToken of 
               SEMICOLON => []
             | STRING s  => (advance(); s :: loop())
             | _ => condemn("string constant (file name) expected, found " ^
                            tokenName (!nextToken))
            )
        val files = loop()
     in case files of
          [] => condemn("string constant (file name) expected, found " ^
                            tokenName (!nextToken))
        | _  => files
    end

exception Eof

fun tdec() =
    case !nextToken
      of SIGNATURE => (advance(); SIGdec(sigb()))
       | Token.FUNCTOR => (debugmsg "tdec(Functor)"; advance(); FCTdec(fctb()))
       | STRUCTURE => (debugmsg "tdec(Structure)"; advance(); STRdec(strb(false,[])))
       | ABSTRACTION => (advance(); ABSdec(strb(true,[])))
       | Token.OPEN =>
	      let val strs = (advance(); qid_p())
	       in app openStructureVar strs;
		  OPENdec strs
	      end
       | SEMICOLON => (advance(); tdec())
       | EOF => raise Eof
       | tok => condemn("signature, functor, or structure expected, found " ^
			 tokenName tok)

val itsym = Symbol.symbol "it"

exception Import of BareAbsyn.dec

fun interdec() =
    let fun top() =
	(case !nextToken
	   of SIGNATURE => (advance(); SIGdec(sigb()))
	    | Token.FUNCTOR => (advance(); FCTdec(fctb()))
	    | STRUCTURE => (advance(); STRdec(strb(false,[])))
	    | ABSTRACTION => (advance(); STRdec(strb(true,[])))
            | IMPORT=>(advance(); raise Import(IMPORTdec(importdec())))
	    | EOF => raise Eof
	    | tok => let val dec =
			     if firstLdec(!nextToken)
			     then ldec([])
			     else if firstExp(!nextToken)
			     then VALdec[VB
				  (protect(protectTyvars(NONE),(fn() =>
				    {exp=exp(),
				     pat=let val v = newVAR(ref nil,itsym)
					  in bindVAR(itsym,v);
					     VARpat v
					 end,
				     tyvars=currentTyvars()})))]
			     else condemn("declaration or expression expected, found " ^
					   tokenName tok)
		      in Typecheck.decType(dec); dec
		     end)
	fun tops() = top() :: (if !nextToken = SEMICOLON
					orelse !nextToken = EOF
				then nil
				else tops())
     in if at SEMICOLON then interdec()
	else (toplevel := false;
	      case tops() of [dec] => dec | seq => SEQdec seq
	     ) handle Import importdec => importdec
	     
    end

end (* local *)

end (* structure Parse *)
