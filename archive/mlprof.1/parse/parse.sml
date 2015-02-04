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

  infix -->

  fun for l f = app f l

  val sort3 = Sort.sort (fn ((a,_,_),(b,_,_)) => name a > name b)

  fun protect((enter,exit),doit) =
      let val t = enter()
       in (doit() before exit t)
           handle exn => (exit t; raise exn)
      end

  val protectScope = (mark,close)

in

(* constants *)

val maxTypSpecs = 100  (*maximum number of type specs in a signature *)
val maxStrSpecs = 100  (*maximum number of structure specs in a signature *)

(* structure mode *)
datatype strmode = REGULAR | FCTBODY
val strMode = ref REGULAR

(* utility functions *)

(* possibly this should be moved to Lex *)
fun fullTokenName ID = "ID " ^ Symbol.name(!idValue)
  | fullTokenName tok = tokenName tok

(* kludge for recognizing unit type *)
val unitName = Symbols.stringToSymbol("unit")
fun isUnitTy(CONty(ref(tyc),_)) = 
    (eqTycon(tyc,!unitTycon) orelse
     Symbol.eq(tycName(tyc),unitName)
     handle _ => false)
  | isUnitTy _ = false

fun checkNonCircular(l : tycon list) =
  let fun less(TYCON{name=a,...},TYCON{kind=DEFtyc(TYFUN{body,...}),...}) =
        let fun find(CONty(ref(UNDEFtyc b), args)) = 
		    Symbol.eq(a,b) orelse exists find args
	      | find(CONty(_, args)) = exists find args
	      | find _ = false
         in find body
	end
   in (Topsort.topsort2 less l; ())
       handle Topsort.Cycle => complain "circular withtype declaration"
  end


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

val bogusID = Symbols.stringToSymbol "bogus"
val bogusExnID = Symbols.stringToSymbol "Bogus"
val bogusExp = VARexp(ref(VALvar{access=LVAR(mkLvar()),
	       			 name=bogusID,
				 vtype=ref UNDEFty}))

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

fun discard _ = ()

fun single x = [x]

fun varcon (VARbind v) = VARexp(ref v)
  | varcon (CONbind d) = CONexp d
  | varcon _ = impossible "parse.39"

fun lookID(id : symbol): exp = 
     varcon (lookVARCON id handle Unbound => unboundVAR id)
     handle Unboundrec => VARexp(getPatchVar id)

val lookIDinStr = varcon o lookVARCONinStr

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
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTRinTable(table,id)
		      handle Notfound_Table =>
		      condemn ("unbound intermediate structure in path: " ^ name id)
	       in getStr(rest,
		  	 (case binding of INDstr i => s sub i | _ => binding),
			 n::ap)
	      end
	val (firstId::rest, lastId)  = symPath()
	val STRvar{access=PATH(ap),binding,...} =
	      lookSTR(firstId)
	      handle Unbound => condemn("unbound head of path: " ^ name firstId)
	val (endStr,ap) = getStr(rest,binding,ap)
     in lookLast(endStr,lastId,ap)
    end
    
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
	   | INT => sel1(Symbols.stringToSymbol(makestring(!intValue))
		         before advance())
	   | _ => (complain "illegal selector function"; bogusExp)
    end

fun noAbbrev x _ = (complain "illegal record-name element abbreviation"; x)

fun labels(parseOne, separator, dotsOK, abbrev) =
    if !nextToken = ID
        then let fun lablist () = 
		  case !nextToken
		   of ID => field(ident())
		    | INT => field(Symbols.stringToSymbol(makestring(!intValue))
				   before advance())
		    | DOTDOTDOT => nil
		    | tok => condemn("expected label, found " ^ fullTokenName tok)
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

exception Clausal of symbol * pat   (* for returning clausal patterns from pat *)

fun makeAbstract(datatycs,withtycs) =
    let val (stamps,abstycs,dconss) =
	    let fun loop(TYCON{stamp,name,home,arity,kind=DATAtyc dcons}::rest,
		    	 stamps,abstycs,dconss) =
		      loop(rest,stamp::stamps,
			   TYCON{stamp=stamp,name=name,home=home,arity=arity,
			         kind=ABStyc}
			     ::abstycs,
			   !dcons::dconss)
		  | loop([],stamps,abstycs,dconss) = (stamps,abstycs,dconss)
	     in loop(datatycs,[],[],[])
	    end
        fun subst(tycref as ref(TYCON{stamp,...})) =
	      let fun find(stamp'::stamps,tyc::tycs) =
		        if stamp = stamp' then tycref := tyc else find(stamps,tycs)
		    | find([],_) = ()
	       in find(stamps,abstycs)
	      end
	  | subst _ = ()
        fun substType(CONty(reftyc,args)) =
	      (subst reftyc; app substType args)
	  | substType _ = ()
     in for dconss (app (fn DATACON{typ,...} => substType typ));
	for withtycs (fn TYCON{kind=DEFtyc(TYFUN{body,...}),...} => substType body);
	abstycs
    end

(* functor parameter defaults *)
datatype sigContext = SIGN | FCTPARAM

val anonName = Symbols.stringToSymbol "Anon"
val anonParamName = Symbols.stringToSymbol "Arg"
val nullStamp = genStrStamp()
val nullSig = STRstr{stamp=0,sign=SOME nullStamp,table=Table.new(),
		     env=emptyStrenv,
	             kind=SIGkind{share={s=[],t=[]},bindings=[],stampcounts={s=1,t=0}}}
val nullStr = STRstr{stamp=genStrStamp(),sign=SOME nullStamp,table=Table.new(),
	             env=emptyStrenv, kind=STRkind{name=anonName,home=NONE}}
val nullParamVar = STRvar{name=anonParamName,
		          access=LVAR(namedLvar(anonParamName)),
			  binding=nullSig}


(* copyarray -- used in sign to copy environment arrays *)
fun copyarray(a,n) =
    (* assume n <= length a *)
    let val new = array(n,a sub 0)
	fun loop i = (update(new,i,a sub i); loop(i+1))
     in loop 0
	handle Subscript => new
    end


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
					    | tok => condemn("expected type\
					             \constructor, found "
						     ^ fullTokenName tok)
				     end
				else (complain("expected RPAREN or COMMA in type\
				        \args, found " ^ fullTokenName(!nextToken));
				      t1)
			    end
			| ID	=> CONty(lookPatchTYC(ident()),[])
			| IDDOT => CONty(tyconQid(),[])
			| Token.TYVAR => VARty(lookTyvar(getSymbol()))
			| LBRACE =>
			    (advance();
			     let val (l,sl,_) = labels(ty0,COLON,false,
						       noAbbrev UNDEFty)
			      in recordTy(map (fn (id,ty,_) => (id, ty)) sl)
			     end)
			| tok => condemn("expected a type expression, found token "
				         ^ fullTokenName tok))
	    end
	and ty_pc() = rightAssoc(ty0,COMMA,op::,single)
     in ty0
    end

val ty = tyParser(fn () => qid(fn(str,id,_) => lookTYCinStr(str,id)))


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
     let val bl = ref nil
      in protect(protectScope,
	    (fn () => RULE(pat(bl,true)
		           handle Clausal(id,_) => 
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
		   useCon(lookCON(id))
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
	   (case lookCON(id)
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
	    if at(AS) then LAYEREDpat(pat_id id,pat0())
	              else pat_id id

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
    let val bl = ref nil
	fun vb () = 
	    protect(protectTyvars (OPENbtv(ref[])),
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
    let val bl = ref nil
	fun rvb () =  protect(protectTyvars(OPENbtv(ref[])),
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
		val name = ident()
		val _ = checkToken(EQUAL)
		val typ = protect(protectTyvars(CLOSEDbtv(args)), ty)
		fun equalargs([],[]) = true
		  | equalargs(tv::rest,VARty(tv')::rest') =
		      tv = tv' andalso equalargs(rest,rest')
		  | equalargs _ = false
	     in TypesUtil.bindTyvars args;
		!(bindTYC(name,
			  case typ
			    of CONty(ref tyc,args') =>
			         if equalargs(args,args')
				 then tyc
				 else mkDEFtyc(name,NONE,TYFUN{arity=length args,
							       body=typ})
			     | _ => mkDEFtyc(name,NONE,TYFUN{arity=length args,
							     body=typ})))
	    end
     in TYPEdec(andList(tb1))
    end

and tyvars() =
    case !nextToken
      of Token.TYVAR => [mkTyvar(UBOUND(!idValue))] before advance()
       | LPAREN =>
	    (advance();
	     tyvar_pc() before
	     checkToken(RPAREN))
       | _ => nil

and tyvar_pc() = rightAssoc(tyvar,COMMA,op::,single)

and tyvar() = mkTyvar(UBOUND(!idValue)) before checkToken(Token.TYVAR)

and db() = protect(protectDb,
	    (fn()=>DATATYPEdec
	            {datatycs=andList(db1((fn x => x),ty)),
		     withtycs=if at(WITHTYPE)
		              then let val TYPEdec x = tb() in x end
			      else nil}))

and db1 (newTyc,ty) () =
    let val args = tyvars()
   	val name = ident()
	val dcons = ref(nil: datacon list)
	val reftyc = bindTYC(name,newTyc(mkDATAtyc(name,length args,NONE,dcons)))
	val rangeType = CONty(reftyc, map VARty args)
	val arity = length args
	fun constr() =
	    let val sym = (if at OP
			   then warn "unnecessary op in datatype declaration"
			   else ();
			   ident())
		val const = not(at(OF))
		val typ = if const then rangeType
				else CONty(arrowTycon, [ty(), rangeType])
	     in (sym,const,typ)
            end
     in protect(protectTyvars(CLOSEDbtv(args)),
	 (fn()=>
	   let val dcl = (checkToken(EQUAL); rightAssoc(constr,BAR,op::,single))
	       val sdcl = sort3 dcl
	       val sign = ConRep.boxed(sdcl)
               fun binddcons ((sym,const,typ)::restdcl,rep::restsign) =
	             bindCON(sym, 
		       DATACON{name = sym,
			       const = const,
			       rep = rep,
			       typ = if arity > 0
				     then POLYty(TYFUN{arity=arity,body=typ})
				     else typ,
			       sign = sign})
	             :: binddcons(restdcl,restsign)
	         | binddcons ([],[]) = []
	     in if length sdcl < length dcl
	       	    then condemn "duplicate constructor name" else ();
		TypesUtil.bindTyvars args;
                dcons := binddcons(sdcl,sign);
		!reftyc
	    end))
    end

and ab() =
    let val mAbstype = mark()
	val DATATYPEdec{datatycs,withtycs} = db()
	val abstycs = makeAbstract(datatycs,withtycs)
	val mWith = mark()
	val body = (checkToken(WITH); ldecs())
	fun bind tyc = bindTYC(tycName tyc, tyc)
     in checkToken(END);
	splice(mAbstype,mWith);
	app bind abstycs;
	app bind withtycs;
	ABSTYPEdec{abstycs=abstycs,withtycs=withtycs,body=body}
    end

and eb() = EXCEPTIONdec(andList(eb1))

and eb1() =
    let val name = ident()
	val _ = if looksLikeExn name then ()
		else warn "Exception name should be capitalized"
	val (edef,etype) =
	    case !nextToken
	     of OF => let val etype = (advance(); ty())
		       in if at(EQUAL) then
			   (SOME(case !nextToken
				   of IDDOT => qid lookEXNinStr
				    | ID => getEXN(getSymbol())
				    | _ => unboundEXN(bogusID) ),
			    SOME(etype))
			  else (NONE, SOME(etype))
		      end
	      | EQUAL => (SOME(advance();
			       case !nextToken
			         of IDDOT => qid lookEXNinStr
			          | ID => getEXN(getSymbol())
				  | _ => unboundEXN(bogusID) ),
			 NONE)
	      | _ => (NONE,NONE)
        val exn = case edef
		    of NONE =>
		         DATACON{name=name,
				 const=(case etype of NONE=>true
						    | SOME _  => false),
				 typ=(case etype
					  of NONE => exnTy
					   | SOME t => t-->exnTy),
				 rep=VARIABLE(LVAR(namedLvar(name))),
				 sign=[]}
		     | SOME(DATACON{name=n,const,typ,rep,sign}) =>
			    DATACON{name=name,const=const,typ=typ,rep=rep,
			    	    sign=[]}  (* changes only name *)
     in bindEXN(name, exn);
	EB{exn=exn, ty=etype, def=edef}
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
         | ABSTYPE =>
	      (advance(); ab())
	 | EXCEPTION =>
	      (advance(); eb())
         | EXCEPTIONX =>
	      (advance(); ebx())
         | LOCAL =>
	     let val mLocal = mark()
		 val ld1 = (advance(); ldecs())
		 val mIn = (checkToken(IN); mark())
		 val ld2 = ldecs()
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
		      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
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

and ldecs() =
  let fun ldecs() =
      if firstLdec(!nextToken)
        then ldec() :: (at(SEMICOLON); ldecs())
        else nil
   in case ldecs() of [dec] => dec | seq => SEQdec seq
  end

and optprec() = if at(INT) then SOME(!intValue) else NONE

and qid_p(): structureVar list =  (* error if no indentifier's ? *)
    case !nextToken
      of ID => let val id = ident()
	        in (lookSTR id
		    handle Unbound =>
		      condemn("unbound str name in open: " ^ name id))
		   ::qid_p()
	       end
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
		fun tyconQid() : tycon ref =
		    let fun get([id],STRstr{table,...}) = 
		             (let val ref(INDtyc x) = lookTYCinTable(table,id)
			       in x
			      end
			      handle Notfound_Table =>
			        condemn("unbound type id in sig spec: " ^ name id))
			  | get(id::rest,STRstr{table,env={s,...},...}) =
			      let val STRvar{binding=INDstr k,...} =
					lookSTRinTable(table,id)
					handle Notfound_Table =>
					condemn("unbound structure id in sig spec: "
					          ^ name id)
			       in k::get(rest, s sub k)
			      end
			val firstId::rest = spath()
			val (STRvar{binding=INDstr i,...},(p,_)) = 
			      lookSTR_sig firstId  (* lookSTRinSig ? *)
			      handle Unbound =>
			        condemn("free structure id in sig: "
				        ^ name firstId)
		     in case p (* check if pervasive structure found *)
			  of [] => ()
			   | _ => condemn("pervasive structure id in sig: "
				          ^ name firstId);
    		        ref(INDtyc(i::get(rest, sComps sub i)))
		    end
		val ty = tyParser(tyconQid)
		fun pairs (nil : spath list list) : (spath*spath) list = nil
		  | pairs ((a::b::r) :: s) = (a,b) :: pairs((b::r) :: s)
		  | pairs ( _ :: s ) = pairs s
		val strSharing : spath list list ref = ref nil
		val typeSharing : spath list list ref = ref nil

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
		       | INFIX => (advance(); infixspec(infixleft))
		       | INFIXR => (advance(); infixspec(infixright))
		       | NONFIX => (advance();
				    app (fn i => bindFIX(i,NONfix)) (ops()))
		       | SHARING => (advance(); sharespec())
		       | _ => condemn("expected a spec (component of signature)")
		
		and strspec() = 
		    rightAssoc(strspec1,AND,discard,discard)
		
		and strspec1() =
		    let val name = ident()
		     in checkToken(COLON);
			bindSTR(name,STRvar{name=name,access=LVAR(0),
					    binding= sNext(sgn(SIGN))})
		    end
		
		and dtyspec() =
		    protect(protectDb, (fn() =>
		      rightAssoc(db1(tNext,ty),AND,discard,discard)))
		
		and tyspec() = 
		    rightAssoc(tyspec1,AND,discard,discard)
		
		and tyspec1() =
		    let val arity = length(tyvars())
			val name = ident()
		     in bindTYC(name, tNext(mkABStyc(name,arity,NONE)))
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
			      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
				let val body = ty()
				    val tvs = currentTyvars()
				 in case tvs
				      of [] => body
				       | _ => (TypesUtil.bindTyvars tvs;
					       POLYty(TYFUN{arity = length tvs,
							    body = body}))
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
			      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
				let val body = ty()
				    val tvs = currentTyvars()
				 in case tvs
				      of [] => body --> exnTy
				       | _ => (TypesUtil.bindTyvars tvs;
					       POLYty(TYFUN{arity = length tvs,
							    body = body --> exnTy}))
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
			      protect(protectTyvars(OPENbtv(ref[])), (fn () =>
				(checkToken(COLON);
				 let val body = ty()
				     val tvs = currentTyvars()
				  in case tvs
				       of [] => body
					| _ => (TypesUtil.bindTyvars tvs;
						POLYty(TYFUN{arity = length tvs,
							     body = body}))
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
		    rightAssoc(spath,EQUAL,op ::,single)

		fun body() =
		    let val stamp = genStrStamp()
			val msig =  mark()
			val mbody = (openStr(); openPervasives(); mark())
			val _ = spec_s()
			val (bindings,table) = buildSigTable mbody
			val _ = close msig
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

fun str(abs: bool, constraint: Structure option, param: structureVar option) =
    case !nextToken
      of IDDOT =>
	    let val strVar as STRvar{binding,...} = qid(lookSTRinStr)
	     in case constraint
		  of NONE => (VARstr strVar, binding, NONE)
		   | SOME sgn =>
		       let val (str,thin) = SigMatch.match(abs,sgn,binding)
		        in (VARstr strVar, str, thin)
		       end
	    end
       | Token.STRUCT => 
	   (advance();
	    let val s0 = mark()
		val s1 = (openStr(); mark())
		val s2 = (openPervasives();
			  case param
			    of NONE => ()
			     | SOME strvar => openStructureVar(strvar);
			  mark())
		val body = sdecs()
	     in (case constraint
		   of NONE =>
		       (case !strMode
			  of FCTBODY =>
			      let val (thin,table,env) = buildFctTable s2
			       in (STRUCTstr{body=body,locations=thin},
				   mkSTR(anonName,NONE,NONE,table,env),
				   NONE)
			      end
			   | REGULAR =>
			      let val (thin,table) = buildStrTable s2
			       in (STRUCTstr{body=body,locations=thin},
				   mkSTR(anonName,NONE,NONE,table,emptyStrenv),
				   NONE)
			      end)
		    | SOME sgn => 
		       let val (str,thin) =
				(splice(s1,s2);
				 SigMatch.realize(abs,genStrStamp(),sgn))
			in (STRUCTstr{body=body,locations=thin}, str, NONE)
		       end)
	        before (close s0; checkToken(END))
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
			       then let val s0 = mark()
					val _  = openStr()
					val s2 = (openPervasives(); mark())
					val body = sdecs()
					val (thin,table) = buildStrTable s2
				     in close s0;
					(STRUCTstr{body=body,locations=thin},
					 mkSTR(anonName,NONE,NONE,table,emptyStrenv))
				    end
			       else let val (strexp,str,_) = str(false,NONE,NONE)
				     in (strexp,str)
				    end)
			      before checkToken(RPAREN)
			 val (result,thin1) = SigMatch.applyFunctor(fct,argstr)
			 val strexp = APPstr{oper=fctVar, 
					     argexp=argexp,
					     argthin=thin1}
		      in case constraint
			   of NONE =>	(strexp,result,NONE)
			    | SOME sgn =>
				let val (struc,thin2) =
					SigMatch.match(abs,sgn,result)
				 in (strexp,result,thin2)
				end
		     end
		else let val strVar as STRvar{binding,...} =
			      lookSTR id handle Unbound => 
			        condemn("unbound structure id: "^name id)
		      in case constraint
			  of NONE => (VARstr strVar, binding, NONE)
			   | SOME sgn =>
			      let val (str,thin) = SigMatch.match(abs,sgn,binding)
			       in (VARstr strVar, str, thin)
			      end
		     end
	    end
       | tok => condemn("expected a structure-expression, found " ^
			 fullTokenName tok)

and sdecs() =
    if firstSdec(!nextToken)
    then sdec() :: (at(SEMICOLON); sdecs())
    else nil

and sdec() =
    if at(STRUCTURE)
      then STRdec(strb(false))
    else if at(ABSTRACTION)
      then ABSdec(strb(true))
    else let val dec = ldec()
	  in Typecheck.decType(dec); dec
	 end

(* structure bindings *)

and strb(abstract) =
    let fun strb1() =
	let val name = ident()
	    val constraint = 
		  if abstract
		  then SOME(sigconstraint())
		  else sigconstraint_op()
	    val _ = checkToken(EQUAL) 
	    val (strexp,str,thin) = str(abstract,constraint,NONE)
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

fun fctb() =
    map (fn (name,fctVar,fctSyn) => (bindFCT(name,fctVar); fctSyn))
	(rightAssoc(fctb1, AND, op ::, single))

and fctb1() =
    let val name = ident()
	datatype pmode = NULL | SINGLE | SPREAD
	val (param as STRvar{name=pname,access=LVAR(plvar),binding=pstr},pmode) =
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
	val lev = mark()
	val STRstr{kind=SIGkind{stampcounts={t=tycCount,s=strCount},...},...} = pstr
     in protect(((fn () => (!tycStampCount,!strStampCount)),
		 (fn (t,s) => (tycStampCount := t; strStampCount := s;
			       strMode := REGULAR))),
	(fn() =>
	  (tycStampCount := tycCount;
	   strStampCount := strCount;
	   strMode := case resSign of NONE => FCTBODY | _ => REGULAR;
	   case pmode
	    of SINGLE => (bindSTR(pname,param); ())
	     | _ => ();
	   let val (strexp,body,thin) = 
		    str(false,resSign,
		        case pmode 
			  of SPREAD => SOME(STRvar{name=pname,access=PATH[plvar],
				       		   binding=pstr})
			   | _ => NONE)
	       val fctv = FCTvar{name=name, 
				 access=LVAR(namedLvar(name)),
				 binding=FUNCTOR{param=pstr,
						 tycCount=(!tycStampCount),
						 body=body}}
	       val fb = FCTB{fctvar=fctv, param=param, def=strexp, thin=thin,
			     constraint=resSign}
	    in close(lev);
	       (name,fctv,fb)
	   end)))
    end


(* top level declarations *)

exception Eof

fun tdec() =
    case !nextToken
      of SIGNATURE => (advance(); SIGdec(sigb()))
       | Token.FUNCTOR => (advance(); FCTdec(fctb()))
       | STRUCTURE => (advance(); STRdec(strb(false)))
       | ABSTRACTION => (advance(); ABSdec(strb(true)))
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
	    | STRUCTURE => (advance(); STRdec(strb(false)))
	    | ABSTRACTION => (advance(); STRdec(strb(true)))
	    | EOF => raise Eof
	    | tok => let val dec =
			     if firstLdec(!nextToken)
			     then ldec()
			     else if firstExp(!nextToken)
			     then VALdec[VB
				    {exp=exp(),
				     pat=let val v = newVAR(ref nil,itsym)
					  in bindVAR(itsym,v);
					     VARpat v
					 end,
				     tyvars=nil}]
			     else condemn("declaration or expression expected, found " ^
					   fullTokenName tok)
		      in Typecheck.decType dec; dec
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
