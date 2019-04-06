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

(* possibly this should be moved to Lex *)
fun fullTokenName ID = "ID " ^ Symbol.name(!idValue)
  | fullTokenName tok = tokenName tok

val nestingLevel = ref 0;
val protectNest = ((fn () => (!nestingLevel before inc nestingLevel)),
		   (fn i => nestingLevel := i))
fun topLevel() = !nestingLevel = 1

fun reset() =
    (nestingLevel := 0)

fun expop () =
    case !nextToken
      of EQUAL => lookFIX(!idValue)
       | ASTERISK => lookFIX(!idValue)
       | ID => lookFIX(!idValue)
       | _ => NONfix

fun patop () =
    case !nextToken
      of ASTERISK => lookFIX (!idValue)
       | ID => lookFIX(!idValue)
       | _ => NONfix

fun ident() =
    case !nextToken
      of ID => getSymbol()
       | ASTERISK => getSymbol()
       | EQUAL => getSymbol()
       | tok => (complain("expected identifier, found " ^ tokenName tok);
		 bogusID)

fun nonfix_ident() =
	let val tok = !nextToken
	in  if (tok = ID orelse tok = ASTERISK)
		andalso lookFIX(!idValue)=NONfix
	    then getSymbol()
	    else (complain("expected nonfix-identifier, found " ^ fullTokenName tok);
		 bogusID)
	end
    
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
       | tok => (complain("expected identifier or OP, found " ^ fullTokenName tok);
		 bogusID)

fun getSTR id = lookSTR id
		 handle Unbound => 
		    (complain("unbound structure name: " ^ name id);
		     bogusSTR)

fun getEXN id = lookEXN(id) handle Unbound => unboundEXN id

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
      of IDDOT => getSymbol() :: symPath()
       | ID => [getSymbol()]
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
	  of ID => sel1(ident())
	   | INT => let val i = !intValue
			val s = makestring i
		     in if i < 1
			 then complain "nonpositive integer label in selector"
			 else ();
		        sel1(Symbols.stringToSymbol(s))
		    end
	            before advance()
	   | _ => (complain "illegal selector function"; bogusExp)
    end

fun labels(parseOne, separator, dotsOK, abbrev) =
    if !nextToken = ID orelse !nextToken = INT orelse !nextToken = DOTDOTDOT
    then let fun lablist () = 
	         case !nextToken
		   of ID => field(ident(),abbrev)
		    | INT => let val i = !intValue
				 val s = makestring i
			      in if i < 1
				   then complain "nonpositive integer label"
				   else ();
				 field(Symbols.stringToSymbol(s),
				        (fn id =>
					  condemn("numeric label abbreviation" ^
						  Symbol.name id)))
			     end
			     before advance()
		    | DOTDOTDOT => nil
		    | tok => condemn("expected label, found " ^ fullTokenName tok)
	     and field(id,abbrev) =
		 (id,
		  if at(separator) then parseOne()
		    else if !nextToken = COMMA orelse
			    !nextToken = AS orelse
			    !nextToken = RBRACE then abbrev(id)
		    else condemn("expected " ^ Token.tokenName separator ^
			         " after label, found " ^
				 fullTokenName(!nextToken)),
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
fun tyParser(tyconQid) = 
    let fun noAbbrev(_) = 
	    (complain "expected colon after label in record type, found comma";
	     UNDEFty)	    
        fun ty0() =
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
		      of ID => qid_s(CONty(lookPatchTYC(getSymbol(),1), [t]))
		       | IDDOT => qid_s(CONty(tyconQid(1), [t]))
		       | _ => t
	     in qid_s(case !nextToken
		       of LPAREN =>
			    let val t1 = (advance(); ty0())
			     in if at(RPAREN)
				then t1
				else if at(COMMA)
				then let val tys = t1 :: ty_pc()
					 val arity = length tys
				      in checkToken(RPAREN);
					 case !nextToken
					   of ID => CONty(lookPatchTYC(ident(),arity),
						          tys)
					    | IDDOT => CONty(tyconQid(arity), tys)
					    | tok => condemn("expected type \
					             \constructor, found "
						     ^ fullTokenName tok)
				     end
				else (complain("expected RPAREN or COMMA in type\
				        \args, found " ^ fullTokenName(!nextToken));
				      t1)
			    end
			| ID	=> CONty(lookPatchTYC(ident(),0),[])
			| IDDOT => CONty(tyconQid(0),[])
			| Token.TYVAR => VARty(lookTyvar(getSymbol()))
			| LBRACE =>
			    (advance();
			     let val (l,sl,_) = labels(ty0,COLON,false, noAbbrev)
			      in recordTy(map (fn (id,ty,_) => (id, ty)) sl)
			     end)
			| tok => condemn("expected a type expression, found token "
				         ^ fullTokenName tok))
	    end
	and ty_pc() = rightAssoc(ty0,COMMA,op::,single)
     in ty0
    end

fun tyconPath(arity:int) =
    let val tycon = qid(fn(str,id,_) => lookTYCinStr(str,id))
     in if tyconArity(!tycon) <> arity
	then complain("type constructor "^(Symbol.name(tycName(!tycon)))^
		      " applied to wrong number of arguments: "^makestring arity)
	else ();
        tycon
    end

val ty = tyParser(tyconPath)
     


(* expressions -- including local declarations *)

fun exp () =
    case !nextToken
     of FN => (advance(); FNexp(match()))
      | CASE => CASEexp((advance(); exp()),
			(checkToken(OF); match()))
      | WHILE => WHILEexp((advance(); exp()), (checkToken(DO); exp()))
      | IF => IFexp((advance(); exp()), (checkToken(THEN); exp()),
       	            (checkToken(ELSE); exp()))
      | RAISEX => 
	  RAISEXexp((advance(); 
		     case !nextToken
		       of ID => getEXN(fixExnName(getSymbol()))
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
     let val bl = ref nil : binder list ref
      in protect(protectScope,
	    (fn () => RULE(pat(bl,true)
		           handle Clausal(id,_) => 
			     condemn("undefined op in pattern: "^name id),
		           (checkToken(DARROW);
			    if !nextToken=EQUAL then advance() else ();
(* Capitalization convention
		            checkBinding(!bl);
*)
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
	   of ID => withClause(getEXN(fixExnName(getSymbol())))
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
     let fun loop e = if firstAexp(!nextToken)
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
		     	     (LETexp(ldecs[], (checkToken(IN); SEQexp(exp_ps()))))
			     before checkToken(END))))
        | FN =>   exp()
        | CASE => exp()
        | WHILE => exp()
        | IF => exp()
	| RAISE => exp()
        | RAISEX => exp()
        | _	 => (complain "atomic expression expected"; bogusExp)

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
	   (case lookCON id
	     of	dcon as DATACON{const=true,...} => CONpat(dcon)
	      | _ => (complain("nonconstant data constructor: " ^ name(id));
		      WILDpat))
	   handle Unbound => VARpat(newVAR(bl,id))

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

and vb() =
    if at(REC)
    then VALRECdec(rvb_pa())
    else VALdec(vb_pa())

and vb_pa () =
    let val bl = ref nil : binder list ref
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
			       in bl := (name,VARbind w) :: rest;
			          VB{pat=CONSTRAINTpat(VARpat w, ty),
				     exp=exp,tyvars=tvs}
			      end

		       | (VARpat(VALvar{name,vtype,...}),
			   VARexp(ref(VALvar{access as INLINE _,...})))
			   => let val _::rest = !bl
				  val w = VALvar{name=name,vtype=vtype,access=access}
			       in bl := (name,VARbind w) :: rest;
			          VB{pat=VARpat w, exp=exp, tyvars=tvs}
			      end
		       | _ => VB{pat=pat,exp=exp,tyvars=tvs}
		end))
     in andListProtect(vb)
	before bindVARs(!bl)
    end

and rvb_pa () = 
    let val bl = ref nil : binder list ref
	fun rvb () =  protect(protectTyvars(NONE),
	      (fn () =>  (* record bug *)
		let val var=newVAR(bl,opid())
		    and resultty=constraint_op()
		    and e = ( checkToken(EQUAL); exp() )
		    and tvs=currentTyvars()
		 in case e of FNexp _ => ()
		       | _ => complain "fn expression required in val rec declaration";
		    RVB{var = var, resultty = resultty, exp = e, tyvars = tvs}
		end))
     in protect(protectPatchList, (fn()=>
	  protect(protectScope, (fn()=>
	    (openRec(); andListProtect(rvb)) ))
	  before bindVARs(!bl) ))
    end

and fb_pa () = 
    let val bl = ref nil : binder list ref
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
     in protect(protectPatchList, (fn()=>
	  protect(protectScope, (fn()=>
	    (openRec(); FUNdec(andListProtect(fb))) ))
	  before bindVARs(!bl) ))
    end

and clause funsym () = 
    let val bl = ref nil : binder list ref
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
				   bindVARs(!bl); exp())))
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
      of Token.TYVAR => [mkTyvar(mkUBOUND(!idValue))] before advance()
       | LPAREN =>
	    (advance();
	     tyvar_pc() before
	     checkToken(RPAREN))
       | _ => nil

and tyvar_pc() = rightAssoc(tyvar,COMMA,op::,single)

and tyvar() = mkTyvar(mkUBOUND(!idValue)) before checkToken(Token.TYVAR)

and db(path) =
    let val (datatycs,withtycs) =
		protect(protectDb, (fn()=>
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
	val rangeType = CONty(lookPatchTYC(name,arity), map VARty args)
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
    let val mAbstype = mark()
	val DATATYPEdec{datatycs,withtycs} = db(path)
	val withtycons = map (fn TB{tyc,...} => tyc) withtycs
	val _ = makeAbstract(datatycs,withtycons)
	val mWith = mark()
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
	        in bindEXN(name,exn);
		   EBgen{exn=exn,etype=SOME etype}
	       end)
	   | EQUAL =>
	       (advance();
	        let val edef as DATACON{const,typ,rep,sign,...} = 
		        case !nextToken
			  of IDDOT => qid lookEXNinStr
			   | ID => getEXN(getSymbol())
			   | tok =>
			     (complain("expected exception name, found token"
				       ^ fullTokenName tok);
			      unboundEXN(bogusID))
		    val exn = DATACON{name=name,const=const,typ=typ,sign=sign,
			      	      rep=VARIABLE(LVAR(namedLvar(name)))}
		 in bindEXN(name,exn);
		    EBdef{exn=exn,edef=edef}
		end)
	   | _ =>
	       let val exn = DATACON{name = name,
			             const = true,
				     typ = exnTy,
				     rep = VARIABLE(LVAR(namedLvar(name))),
				     sign = []}
	        in bindEXN(name,exn);
		   EBgen{exn=exn,etype=NONE}
	       end
    end


and ebx() = EXCEPTIONdec(andList(eb1x))

and eb1x() =
    let val name = fixExnName(ident())
	val etype = constraint_op()
	val (const,typ) = case etype
			      of NONE => (true,exnTy)
			       | SOME t => if isUnitTy(t)
					   then (true,exnTy)
					   else (false,t-->exnTy)
	val edef = if at(EQUAL)
	           then SOME(case !nextToken
			       of IDDOT => qid lookFixedEXNinStr
			        | ID => getEXN(fixExnName(getSymbol()))
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
     in bindEXN(name, exn);
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
         | EXCEPTIONX =>
	      (advance(); ebx())
         | LOCAL =>
	     let val mLocal = mark()
		 val ld1 = (advance(); ldecs[])
		 val mIn = (checkToken(IN); mark())
		 val ld2 = ldecs(path)
	      in checkToken(END);
		 splice(mLocal,mIn);
		 LOCALdec(ld1,ld2)
	     end
	 | Token.OPEN =>  (* confusion with Env.OPEN when Env not constrained *)
	      let val strs = (advance(); qid_p())
	       in app openStructureVar strs;
		  OPENdec strs
	      end
         | INFIX =>
	      let val prec = case (advance(); optprec()) of SOME n=>n|NONE=>0
	       in app (fn i => bindFIX(i, infixleft prec)) (ops());
	          SEQdec(nil)
	      end
         | INFIXR =>
	      let val prec = case (advance(); optprec()) of SOME n=>n|NONE=>0
	       in app (fn i => bindFIX(i, infixright prec)) (ops());
	          SEQdec(nil)
	      end
         | NONFIX =>
	      (advance();
	       app (fn i => bindFIX(i,NONfix)) (ops()); SEQdec(nil))
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
         | _ => (complain "expected a declaration"; vb())
      )))

and ldecs(path) =
  let fun ldecs() =
      if firstLdec(!nextToken)
        then ldec(path) :: (at(SEMICOLON); ldecs())
        else []
   in case ldecs() of [dec] => dec | seq => SEQdec seq
  end

and optprec() = if at(INT) then SOME(!intValue) else NONE

and qid_p(): structureVar list =  (* error if no identifier's ? *)
    case !nextToken
      of ID => getSTR(ident()) :: qid_p()
       | IDDOT => qid(lookSTRinStr)::qid_p()
       | _ => nil

and ops() =
  let fun ops1() =
        case !nextToken
          of ID => (!idValue) :: (advance(); ops1())
	   | EQUAL => (!idValue) :: (advance(); ops1())
	   | ASTERISK => (!idValue) :: (advance(); ops1())
           | _ => nil
   in case ops1()
	of [] => (complain("operator or identifier expected"); [])
         | l => l
  end


(* signatures *)

fun sign (context: sigContext) =
    let fun sgn(context) = 
	    let val tComps = array(maxTypSpecs,INDtyc [])
		and tCount = ref 0
		fun tNext x = (update(tComps,!tCount,x);
			       INDtyc[!tCount before inc tCount])
		val sComps = array(maxStrSpecs,INDstr(~1))
		and sCount = ref 0
		fun sNext x = (update(sComps,!sCount,x);
			       INDstr(!sCount before inc sCount))
		fun tyconQid(arity) : tycon ref =
		    let val path as first::rest = symPath()
			fun get([id],STRstr{table,env={t,...},...}) = 
		             (let val ref(INDtyc(p as [i])) =
				      lookTYCinTable(table,id)
			       in if tyconArity(t sub i) <> arity
 				  then (complain("type constructor has wrong\
					 \ number of arguments in signature");
				        prstr "  name: ";
					printSequence "." printSym path; newline();
					prstr "  no of args: ";
					prstr(makestring arity); newline())
				  else ();
				  p
			      end
			      handle Table.Notfound_Table =>
			        (complain "unbound type constructor in signature";
				 prstr "  name: "; printSequence "." printSym path;
				 newline();
				 raise Syntax))
			  | get(id::rest,STRstr{table,env={s,...},...}) =
			      let val STRvar{binding=INDstr k,...} =
					lookSTRinTable(table,id)
					handle Table.Notfound_Table =>
					(complain "unbound type constructor \
					          \in signature";
					 prstr "  name: ";
					 printSequence "." printSym path;
					 newline();
					 raise Syntax)
			       in k::get(rest, s sub k)
			      end
		     in let val (STRvar{binding=INDstr i,...},_) = 
			           lookSTRinSig first
			 in ref(INDtyc(i::get(rest, sComps sub i)))
			end
    		        handle Unbound =>
			let val tycon = lookPath(path,(fn(str,id,_) =>
						     lookTYCinStr(str,id)))
			 in if tyconArity(!tycon) <> arity
			    then (complain("type constructor has wrong\
					 \ number of arguments in signature");
				  prstr "  name: ";
				  printSequence "." printSym path; newline();
				  prstr "  no of args: ";
				  prstr(makestring arity); newline())
			    else ();
			    tycon
			end
			handle Subscript =>
			condemn "free ref to sibling struct in sig not implemented"
		    end
		val ty = tyParser(tyconQid)
		fun pairs (nil : spath list list) : (spath*spath) list = nil
		  | pairs ((a::b::r) :: s) = (a,b) :: pairs((b::r) :: s)
		  | pairs ( _ :: s ) = pairs s
		val strSharing : spath list list ref = ref nil
		val typeSharing : spath list list ref = ref nil

		fun dumpSig(sgn as STRstr{table,env={s=senv,t=tenv},
					  kind=SIGkind{stampcounts={s,t},...},
					  ...}) =
		    let val tBase = !tCount
			and sBase = !sCount
			val str = 
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
			       in CONty(ref(INDtyc(newp)), map adjust args)
			      end
			  | adjust(CONty(reftyc,args)) =
			      CONty(reftyc, map adjust args)
			  | adjust(POLYty{sign,tyfun=TYFUN{arity,body}}) =
			      POLYty{sign=sign,
			      	     tyfun=TYFUN{arity=arity,body=adjust body}}
			  | adjust ty = ty

			fun rebind(id,VARbind(VALvar{vtype,...})) =
			      bindVAR(id,mkVALvar(id,ref(adjust(!vtype))))
			  | rebind(id,CONbind(DATACON{name,const,typ,rep,sign})) = 
			      bindCON(id,DATACON{name=name,const=const,rep=rep,
			      			 sign=sign,
						 typ=adjust typ})
			  | rebind(id,TYCbind(ref(INDtyc[i]))) = 
			      (update(tComps,tBase+i,tenv sub i);
			       bindTYC(id,ref(INDtyc[tBase+i])))
			  | rebind(id,STRbind(STRvar{name,binding=INDstr i,...})) = 
			      (update(sComps,sBase+i,senv sub i);
			       bindSTR(id,STRvar{access=LVAR 0,
						 name=name,
						 binding=INDstr(sBase+i)}))
			  | rebind(id, FIXbind fixity) = bindFIX(id,fixity)
			  | rebind _ = impossible "dumpSig"

		     in Table.app(table,rebind);
			tCount := !tCount + length tenv;
			sCount := !sCount + length senv
		    end

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
		       | EXCEPTIONX => (advance(); exnspecx())
		       | INFIX => (advance(); infixspec(infixleft))
		       | INFIXR => (advance(); infixspec(infixright))
		       | NONFIX => (advance();
				    app (fn i => bindFIX(i,NONfix)) (ops()))
		       | SHARING => (advance(); sharespec())
		       | INCLUDE => (advance(); includespec())
		       | _ => condemn("expected a spec (component of signature)")
		

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
					    binding= sNext(sgn(SIGN))})
		    end
		
		and dtyspec() =
		    let fun findtyc(INDtyc[i]) = tComps sub i
			  | findtyc tyc = tyc
		     in app (defineEqTycon (tyconInContext {t=tComps,s=sComps}))
			 (protect(protectDb, (fn() =>
			    map (fn (r as ref tyc) => (r := tNext tyc; tyc))
				(rightAssoc(db1(ty,[]),AND,op ::,single)))))
		    end

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
			      of ID => getSymbol()
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
			    if at(OF) then (false,
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
		     in bindEXN(name, DATACON{name=name, const=const, typ=typ,
					      rep=VARIABLE(LVAR(0)),
					      sign=[]})
		    end
		
		and exnspecx() = 
		    rightAssoc(exnspec1x,AND,discard,discard)
		
		and exnspec1x() =
		    let val name = fixExnName(ident())
			val ty =
			    protect(protectScope, (fn () =>
			      (* localize type variables *)
			      protect(protectTyvars(NONE), (fn () =>
				(checkToken(COLON);
				 let val body = ty()
				     val tvs = currentTyvars()
				     val arity = length tvs
				  in case length tvs
				       of 0 => body
					| n => 
					  (TypesUtil.bindTyvars tvs;
					   POLYty{sign=mkPolySign n,
						  tyfun=TYFUN{arity = n,
							      body = body}})
				 end)))))
			val const = isUnitTy(ty)
		     in bindEXN(name, DATACON{name=name,const=const,
				      	      typ=(if const then exnTy
						     else ty-->exnTy),
					      rep=VARIABLE(LVAR(0)),
					      sign=[]})
		    end

		and infixspec(mkinfix) =
		    let val prec = case optprec() of SOME n=>n|NONE=>0
		     in app (fn i => bindFIX(i, mkinfix prec)) (ops())
		    end

		and sharespec() =
		    rightAssoc(sharespec1,AND,discard,discard)
		
		and sharespec1() =
		    case !nextToken
		      of TYPE => (advance(); typeSharing := patheqn() :: !typeSharing)
		       | ID => strSharing := patheqn() :: !strSharing
		       | IDDOT => strSharing := patheqn() :: !strSharing
		       | tok => condemn("unexpected token after \"sharing\": "
					^tokenName tok)
		
		and patheqn() : spath list =
		    rightAssoc(symPath,EQUAL,op ::,single)

		fun body() =
		    let val stamp = genStrStamp()
			val _ = openStr()
			val _ = spec_s()
			val (bindings,table) = buildSigTable ()
			val env={s=copyarray(sComps,!sCount),
				 t=copyarray(tComps,!tCount)}
			val sShare = pairs(!strSharing)
			val tShare = pairs(!typeSharing)
			val shareSpec =
			      if null sShare andalso null tShare
			      then {s=[],t=[]}
			      else Sharing.doSharing(table,env,{s=sShare,t=tShare})
		     in STRstr{stamp=stamp,
			       sign=SOME(genSigStamp()),
			       table=table,
			       env=env,
			       kind=SIGkind{share=shareSpec,
					    bindings=bindings,
					    stampcounts={t= !tycStampCount,
							 s= !strStampCount}}}
		    end

	     in case !nextToken
		  of ID =>
			let val name = !idValue before advance()
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
	    end
     in protect(((fn () => (!tycStampCount,!strStampCount)),
		 (fn (t,s) => (tycStampCount := t; strStampCount := s))),
	(fn() =>
	  (tycStampCount := 0;
	   strStampCount := 0;
	   sgn(context))))
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
      of IDDOT =>
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
				   mkSTR(path,NONE,table,env),
				   NONE)
			      end
			   | REGULAR =>
			      let val (thin,table) = buildStrTable ()
			       in (STRUCTstr{body=body,locations=thin},
				   mkSTR(path,NONE,table,emptyStrenv),
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
       | ID => 
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
					mkSTR([anonParamName],NONE,table,
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
			 fullTokenName tok)

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
    let datatype pmode = NULL | SINGLE | SPREAD
	val name = ident()
	val (param as STRvar{name=pname,access=LVAR(plvar),binding=pstr},
	     pmode) =
	     (checkToken(LPAREN);
	      (case !nextToken
		of RPAREN => (nullParamVar,NULL)
	         | ID => let val name = ident()
			  in (STRvar{name=name,
				     access=LVAR(namedLvar(name)),
				     binding=sigconstraint()},
			      SINGLE)
			 end
		 | tok => if firstSpec(tok)
			  then (STRvar{name=anonParamName,
			               access=LVAR(namedLvar(anonParamName)),
				       binding=sign(FCTPARAM)},
				SPREAD)
			  else condemn ("expected functor formal, found "
				       ^tokenName tok))
	      before checkToken(RPAREN))
	val resSign = sigconstraint_op() before checkToken(EQUAL)
	val mEntry = mark()
	val STRstr{env,table,
	    	   kind=SIGkind{stampcounts={t=tycCount,s=strCount},...},
		   ...} = pstr
     in protect(((fn () => (!tycStampCount,!strStampCount)),
		 (fn (t,s) => (tycStampCount := t; strStampCount := s;
			       strMode := REGULAR))),
	(fn() =>
	  (tycStampCount := tycCount;
	   strStampCount := strCount;
	   strMode := case resSign of NONE => FCTBODY | _ => REGULAR;
	   case pmode
	    of SINGLE => bindSTR(pname,param)
	     | SPREAD => openOld(([plvar],env),table)
	     | NULL => ();
	   let val (strexp,body,thin) = str(false,resSign,[])
	       val fctv = FCTvar{name=name, 
				 access=LVAR(namedLvar(name)),
				 binding=FUNCTOR{paramName=pname,
				 		 param=pstr,
						 tycCount=(!tycStampCount),
						 body=body}}
	       val fb = FCTB{fctvar=fctv, param=param, def=strexp, thin=thin,
			     constraint=resSign}
	    in close(mEntry);
	       (name,fctv,fb)
	   end)))
    end


(* top level declarations *)

exception Eof

fun tdec() =
    case !nextToken
      of SIGNATURE => (advance(); SIGdec(sigb()))
       | Token.FUNCTOR => (advance(); FCTdec(fctb()))
       | STRUCTURE => (advance(); STRdec(strb(false,[])))
       | ABSTRACTION => (advance(); ABSdec(strb(true,[])))
       | Token.OPEN =>
	      let val strs = (advance(); qid_p())
	       in app openStructureVar strs;
		  OPENdec strs
	      end
       | SEMICOLON => (advance(); tdec())
       | EOF => raise Eof
       | tok => condemn("signature, functor, or structure expected, found " ^
			 fullTokenName tok)

val itsym = Symbols.stringToSymbol "it"

fun interdec() =
    let fun top() =
	(case !nextToken
	   of SIGNATURE => (advance(); SIGdec(sigb()))
	    | Token.FUNCTOR => (advance(); FCTdec(fctb()))
	    | STRUCTURE => (advance(); STRdec(strb(false,[])))
	    | ABSTRACTION => (advance(); STRdec(strb(true,[])))
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
					   fullTokenName tok)
		      in Typecheck.decType(dec); dec
		     end)
	fun tops() = top() :: (if !nextToken = SEMICOLON
					orelse !nextToken = EOF
				then nil
				else tops())
    in  if at SEMICOLON then interdec()
	else (toplevel := false;
	      case tops() of [dec] => dec | seq => SEQdec seq)
    end

end (* local *)

end (* structure Parse *)
