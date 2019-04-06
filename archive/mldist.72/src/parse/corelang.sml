(* Copyright 1989 by AT&T Bell Laboratories *)
signature CORELANG =
sig type 'a precStack
    type stampgen
    type 'a evstamped
    type 'a epathvstamped
    type 'a epathed
    type 'a enved
    type 'a susp
    type 'a uvars
    type 'a withenv
(*  open Symbol Types Fixity Absyn -- this would replace following three lines *)
    type exp type pat type fixity type label type symbol
    type ty type vb type dec type rvb type tyvar type tycon type datacon
    type eb type rule type tb 

    val lookID : Modules.env * Symbol.symbol * ErrorMsg.complainer
	                     -> BareAbsyn.exp
 
    val exp_start:	exp * fixity * ErrorMsg.complainer -> exp precStack
    val exp_parse:	exp precStack * exp * fixity * ErrorMsg.complainer -> exp precStack
    val exp_finish:	exp precStack * ErrorMsg.complainer -> exp
    val make_app_pat:	(pat * fixity * ErrorMsg.complainer) list -> pat
    val pat_id:		Modules.env -> symbol -> pat
    val checkUniq:      ErrorMsg.complainer * string -> Symbol.symbol list -> unit
    val make_recordTy:  (symbol * ty) list * ErrorMsg.complainer -> ty
    val makeRECORDexp:  (symbol * exp) list * ErrorMsg.complainer -> exp
    val completeMatch:  rule list -> rule list
    val layered: 	pat * pat * ErrorMsg.complainer -> pat
    val makeRECORDpat:	((label * pat) list * bool) * ErrorMsg.complainer -> pat
    val qid_pat:	Modules.env -> Modules.spath * ErrorMsg.complainer -> pat
    val valbind:	pat enved uvars * exp evstamped uvars -> vb list evstamped
    val makeEB:		symbol -> eb list withenv epathvstamped
    val makeEBof:	symbol * ty enved * ErrorMsg.complainer
	                  -> eb list withenv epathvstamped
    val makeEBeq:	symbol * Modules.spath * ErrorMsg.complainer
	                  -> eb list withenv epathvstamped
    val makeVALdec:	vb list evstamped * ErrorMsg.complainer -> dec withenv epathvstamped
    val makeSEQdec:	dec withenv epathvstamped * dec withenv epathvstamped -> dec withenv epathvstamped
    val makeLOCALdec:	dec withenv epathvstamped * dec withenv epathvstamped -> dec withenv epathvstamped
    val makeOPENdec:	Modules.structureVar list enved -> dec withenv epathvstamped
    val makeHANDLEexp:  exp * rule list -> exp
    val makeRULE:	pat enved * exp evstamped * ErrorMsg.complainer -> rule evstamped
    type rawrvb
    val makeVALRECdec:	rawrvb list enved * ErrorMsg.complainer -> dec withenv epathvstamped
    type rawclause
    val checkFB:	rawclause list * ErrorMsg.complainer -> rawclause list
    val makecl:		(pat * fixity * ErrorMsg.complainer) list *
		        (pat * fixity * ErrorMsg.complainer) list 
				-> Symbol.symbol * pat list
    val makeFUNdec:	rawclause list list enved uvars * ErrorMsg.complainer -> dec withenv epathvstamped
    val makeTB:		tyvar list * symbol * ty enved uvars * ErrorMsg.complainer -> 
				bool -> tb list withenv epathvstamped
    val nullTB:		bool -> tb list withenv epathvstamped
    val makeDB:		(symbol * int * datacon list withenv epathed) list
                        * (bool -> tb list withenv epathvstamped)
                        * ErrorMsg.complainer ->
				dec withenv epathvstamped
    val makeDB':	tyvar list * symbol * (Modules.env * ty -> (symbol * bool * ty) list) uvars
				* ErrorMsg.complainer  -> datacon list withenv epathed
    val makeTYPEdec:    tb list withenv * ErrorMsg.complainer -> dec withenv
    val makeABSTYPEdec: (symbol * int * datacon list withenv epathed) list 
			  * (bool -> tb list withenv epathvstamped) 
			  * dec withenv epathvstamped
	                  * ErrorMsg.complainer -> dec withenv epathvstamped
    val makeOVERLOADdec: symbol * ty enved uvars * exp list evstamped -> dec withenv epathvstamped
    val makeFIXdec:	fixity * symbol list -> dec withenv epathvstamped
    val makeEXCEPTIONdec:  eb list withenv * ErrorMsg.complainer -> dec withenv
    val toplevelexp:	Modules.env * exp evstamped uvars * (BareAbsyn.linenum*BareAbsyn.linenum->ErrorMsg.complainer) * (BareAbsyn.linenum*BareAbsyn.linenum) -> dec withenv
end

structure CoreLang: CORELANG = struct    

open Types Variables Fixity Modules
open ErrorMsg Symbol PrintUtil EqTypes
open Access BasicTypes TypesUtil Absyn TyvarSet
open ModuleUtil Misc

type stampgen = Stamps.scope
(* type 'a stamped = stampsets -> 'a *)
type 'a susp = unit -> 'a
type 'a epathed = Modules.env * symbol list -> 'a
type 'a uvars = 'a * tyvarset
type 'a evstamped = Modules.env * tyvarset * stampgen -> 'a
type 'a epathvstamped = Modules.env * spath * tyvarset * stampgen -> 'a
type 'a enved = Modules.env -> 'a

type 'a withenv = 'a * Modules.env

fun impossible err s = (err BUG s; raise ErrorMsg.Syntax)

fun lookID arg = varcon (lookShortVARCON arg)

abstype 'a precStack = INf of int * 'a * 'a precStack
		   | NONf of 'a * 'a precStack
		   | NILf
with fun precedence (app,pair) =
 let fun ensure_nonfix (e,NONfix,_) = e
       | ensure_nonfix (e,INfix _,err) = 
	  (err COMPLAIN "nonfix identifier required"; e)

     fun start(e,f,err) = NONf(ensure_nonfix(e,f,err), NILf)
	  
     fun parse(NONf(e,r), e',NONfix,err) = NONf(app err(e,e'),r)
       | parse(p as INf _, x,f,err) = NONf(ensure_nonfix(x,f,err), p)
       | parse(p as NILf, _,_,err) = impossible err "Corelang.parse NILf"
       | parse(p as NONf(e1,INf(bp,e2,NONf(e3,r))), e4, f as INfix(lbp,rbp),err)=
	    if lbp > bp then INf(rbp,e4,p)
	     else (if lbp = bp
			then err WARN "mixed left- and right-associative \
				      \operators of same precedence"
			else ();
	           parse(NONf(app err(e2,pair err (e3,e1)),r),e4,f,err))
       | parse(p as NONf _, e',INfix(lbp,rbp),_) = INf(rbp,e',p)
     
     fun finish (NONf(e1,INf(_,e2,NONf(e3,r))),err) = 
		     finish(NONf(app err(e2,pair err (e3,e1)),r),err)
       | finish (NONf(e1,NILf),_) = e1
       | finish (INf(_,e1,NONf(e2,p)),err) = 
		     (err COMPLAIN "nonfix identifier required";
		      finish(NONf(app err(e2,e1),p),err))
       | finish (NILf,err) = impossible err "Corelang.finish NILf"
       | finish _ = ErrorMsg.impossible "Corelang.finish"

  in {start=start,parse=parse,finish=finish}
 end
end

val {start=exp_start, parse=exp_parse, finish=exp_finish} = 
	precedence(fn _ => APPexp, fn _ => fn (a,b) => TUPLEexp[a,b])

fun APPpat' (CONpat dcon,pat) = APPpat(dcon,pat)
  | APPpat' _ = ErrorMsg.impossible "CoreLang.APPpat'"

fun clean_pat err (CONpat(DATACON{const=false,name,...})) = 
	(err COMPLAIN ("data constructor "^Symbol.name name^
		       " used without argument in pattern");
	 WILDpat)
  | clean_pat err p = p

fun apply_pat err (CONpat(d as DATACON{const=false,...}),p) = 
		APPpat(d, clean_pat err p)
  | apply_pat err (CONpat(d as DATACON{name,...}),_) = 
	(err COMPLAIN ("constant data constructor "^Symbol.name name^
		       " applied to argument in pattern");
	 WILDpat)
  | apply_pat err _ = 
	(err COMPLAIN ("non-constructor applied to argument in pattern");
	 WILDpat)

val {start=pat_start, parse=pat_parse0, finish=pat_finish0} =
	precedence(apply_pat, 
		   fn err => fn (ap1,ap2) =>
			TUPLEpat[clean_pat err ap1, clean_pat err ap2])

fun pat_parse(ap,(p,f,err)) = pat_parse0(ap, p, f,err)
fun pat_finish(ap,err) = clean_pat err (pat_finish0(ap,err))

fun pat_id env id = (case lookShortVARCON (env,id,fn _ => raise Env.Unbound)
                      of CONbind c => CONpat c 
		       | _ => VARpat(mkVALvar id))
                    handle Env.Unbound => VARpat(mkVALvar id)

fun checkUniq (err,message) l =
 let val l' = Sort.sort Symbol.symbolGt l
     fun f (x::y::rest) = (if Symbol.eq(x,y) 
			      then err COMPLAIN(message^ ": " ^ Symbol.name x)
			      else ();
			   f(y::rest))
      | f _ = ()
  in f l'
 end

fun bindVARp (patlist,err) =
 let val vl = ref (nil: symbol list)
     val env = ref(Env.empty: Modules.env)
     val rec f =
           fn VARpat(v as VALvar{name=[name],...})=> 
		    (if Symbol.eq(name,EQUALsym)
			    then err WARN "rebinding =" else ();
		     env := Env.bind(name,VARbind v,!env); 
	 	     vl := name :: !vl)
	    | RECORDpat{fields,...} => app(fn(_,pat)=>f pat) fields
	    | APPpat(_,pat) => f pat
	    | CONSTRAINTpat(pat,_) => f pat
	    | LAYEREDpat(p1,p2) => (f p1; f p2)
	    | _ => ()
 in app f patlist;
    checkUniq (err,"duplicate variable in pattern(s)") (!vl);
    !env
 end

local fun gtr((a,_),(b,_)) = 
		     let val a' = Symbol.name a and b' = Symbol.name b
		         val zero = ord "0" and nine = ord "9"
			 val a0 = ordof(a',0) and b0 = ordof(b',0)
		      in if a0 >= zero andalso a0 <= nine
			  then if b0 >= zero andalso b0 <= nine
				 then size a' > size b' orelse
					  size a' = size b' andalso a' > b'
				 else false
			  else if b0 >= zero andalso b0 <= nine
				then true
				else a' > b'
		     end
      val sort = Sort.sort gtr
 in fun sortRecord(l,err) =
	   (checkUniq(err, "duplicate label in record") (map #1 l);
	    sort l)
end

fun make_recordTy l_err = recordTy(sortRecord l_err)

fun layered((x as VARpat _), y, _) = LAYEREDpat(x,y)
  | layered(CONSTRAINTpat(x as VARpat _,t), y, _) = 
					LAYEREDpat(x,CONSTRAINTpat(y,t))
  | layered(x,y,err) = (err COMPLAIN "pattern to left of AS must be variable";
			y)

fun makeRECORDexp(fields,err) =
  let val fields' = map (fn(id,exp)=> (id,(exp,ref 0))) fields
      fun assign(i,(_,(_,r))::tl) = (r := i; assign(i+1,tl))
	| assign(_,nil) = ()
      fun f(i,(id,(exp,ref n))::r) = (LABEL{name=id,number=n},exp)::f(i+1,r)
        | f(_,nil) = nil
   in assign(0, sortRecord(fields',err)); RECORDexp(f(0,fields'))
  end

val exnID = Symbol.tycSymbol "exn"

fun makeHANDLEexp(exp,rules) =
    let fun anywild (RULE(WILDpat,_)) = true
	  | anywild (RULE(VARpat _,_)) = true
	  | anywild _ = false
	val rules =
	    if exists anywild rules then rules
	    else let val v = mkVALvar exnID
		     val r = RULE(VARpat v,
				  RAISEexp(VARexp(ref(v))))
		  in completeMatch' r rules
		 end
     in HANDLEexp(exp, HANDLER(FNexp rules))
    end
	
fun makeRECORDpat((l,flex),err) =
	  RECORDpat{fields= sortRecord(l,err), 
		    flex=flex, typ=ref UNDEFty, pats=ref nil}

fun qid_pat env (qid,err) = 
 CONpat(case lookVARCON (env,qid,err)
        of VARbind c => (err COMPLAIN (formatQid qid^
			     " is a variable.  It must be a constructor.");
			 bogusCON)
         | CONbind c => c
         | _ => ErrorMsg.impossible "CoreLang.qid_pat")

fun valbind ((pat,pv),(exp,ev)) (env,tv,st) =
    let val localtyvars = diff_tyvars(union_tyvars(pv,ev),tv)
	val localtyvarlist = get_tyvars localtyvars
	val downtyvars = union_tyvars(localtyvars,tv)
        val pat = pat env and exp = exp(env,downtyvars,st)
     in [VB{exp=exp,tyvars=localtyvarlist,
	    pat=case (pat,exp)
		 of (CONSTRAINTpat(VARpat(VALvar{name,typ,...}), ty),
	     	     VARexp(ref(VALvar{access as INLINE _,...}))) =>
	               CONSTRAINTpat(VARpat(VALvar{name=name,typ=typ,
			access=access}),ty)
		  | (VARpat(VALvar{name, typ,...}),
	     	     VARexp(ref(VALvar{access as INLINE _,...}))) =>
			VARpat(VALvar{name=name,typ=typ,access=access})
		  | _ => pat}]
    end

fun makeEB (id:symbol) (_) =
    let val exn = DATACON{name=id,const=true,typ=exnTy,
			  rep=VARIABLEc(PATH[namedLvar id]), sign=[]}
     in ([EBgen{exn=exn,etype=NONE}], Env.bind(id, CONbind exn, Env.empty))
    end

fun makeEBof (id,etype,err) (env,_,_,_) =
   let val ty = etype env
       val exn = DATACON{name=id,const=false,typ=(ty --> exnTy),
			 rep=VARIABLE(PATH[namedLvar id]), sign=[]}
    in ([EBgen{exn=exn,etype=SOME ty}], Env.bind(id,CONbind exn, Env.empty))
   end

fun makeEBeq (id,qid,err) (env,_,_,_) =
  let val edef as DATACON{const,typ,rep,sign,...} = lookEXN(env,qid,err)
      val exn = DATACON{name=id,const=const,typ=typ,sign=sign,
			rep=if const then VARIABLEc(PATH[namedLvar id])
			    else VARIABLE(PATH[namedLvar id])}
   in ([EBdef{exn=exn,edef=edef}], Env.bind(id,CONbind exn,Env.empty))
  end


fun makeVALdec (vb,err) (env,path,tv,st) =
   let val l = vb(env,tv,st)
    in (VALdec l,bindVARp (map (fn VB{pat,...}=>pat) l, err))
   end

fun makeSEQdec (d1,d2) ($ as (env,path,tv,st)) =
  let val (d1',env1) = d1 $
      val (d2',env2) = d2 (Env.atop(env1,env),path,tv,st)
      val d' = 
        case (d1',d2')
        of (SEQdec a, SEQdec b) => SEQdec(a@b)
         | (SEQdec a, b) => SEQdec(a@[b])
         | (a, SEQdec b) => SEQdec(a::b)
         | (a,b) => SEQdec[a,b]
  in (d',Env.atop(env2,env1))
  end
     
fun makeLOCALdec (ldecs1,ldecs2) (env,path,tv,st) =
  let val (ld1,env1) = ldecs1(env,[],tv,st)
      val (ld2,env2) = ldecs2(Env.atop(env1,env),path,tv,st)
  in (LOCALdec(ld1,ld2), env2)
  end

fun makeOPENdec qid_p (env,_,_,_) = 
   let val strs = qid_p env
       fun openit (str,env) = openStructureVar (env,str)
    in (OPENdec strs, revfold openit strs Env.empty)
   end

fun makeRULE(pat,exp,err) (env,tv,st) =
     let val p = pat env
         val env' = Env.atop(bindVARp ([p],err), env)
     in RULE(p,exp (env',tv,st))
     end

type rawrvb = {name:symbol,ty:ty option enved uvars,match:rule list evstamped uvars}

fun makeVALRECdec (rvb,err) (env,path,tv,st) =
    let val rvbs = rvb env
	val tyvars = fold (fn({match=(_,mv),ty=(_,cv),name},v)=>
				union_tyvars(union_tyvars(mv,cv),v)) 
			rvbs no_tyvars
	val downtyvars = union_tyvars(tyvars,tv)
	val localtyvarlist = get_tyvars(diff_tyvars(tyvars,tv))
        val env' = ref(Env.empty: Modules.env)
	fun makevar (p as {name,...}:rawrvb) = 
	      let val v = mkVALvar name
	       in env' := Env.bind(name,VARbind v,!env'); (v,p)
	      end
	val rvbs' = map makevar rvbs
        val env'' = Env.atop(!env', env)
	fun makervb(v,{ty=(ty,_),match=(match,_),...}:rawrvb) =
	      RVB{var=v,resultty=ty env'',tyvars=localtyvarlist,
		  exp=FNexp(completeMatch(match(env'',downtyvars,st)))}
    in checkUniq(err,"duplicate function name in val rec dec") (map #name rvbs);
       (VALRECdec(map makervb rvbs'),!env')
    end

type rawclause = {name:symbol,pats:pat list,resultty:ty option,exp:exp evstamped,
		   err: ErrorMsg.complainer}

fun checkFB(clauses as ({name,pats,...}:rawclause)::rest, err) =
     let val len = length pats
      in if exists (fn {pats,...} => len <> length pats) rest
	   then err COMPLAIN "clauses don't all have same number of patterns"
	   else ();
	 if exists (fn {name=n,...} => not(Symbol.eq(n,name))) rest
	   then err COMPLAIN "clauses don't all have same function-name"
	   else ();
	 clauses
     end
  | checkFB _ = ErrorMsg.impossible "CoreLang.checkFB"

fun make_app_pat((p as (_,_,err))::rest) =
    let fun f(x,p::r) = f(pat_parse(x,p),r)
	  | f(x,nil) = pat_finish(x,err)
     in f(pat_start p, rest)
    end
  | make_app_pat _ = ErrorMsg.impossible "make_app_pat"

fun checkpat(p,NONfix,e) = clean_pat e p
  | checkpat(p,INfix _, e) = (e COMPLAIN "NONfix pattern required"; p)

fun funsym(VARpat(VALvar{name=[id],...}),err) = id
  | funsym(_,err) = (err COMPLAIN "illegal function symbol in clause"; bogusID)

fun makecl(pats as _::_, [(a,INfix _,e), pat]) =
		(funsym(a,e), [TUPLEpat[make_app_pat pats, checkpat pat]])
  | makecl([(a,NONfix,_),(b,INfix _,e),(c,NONfix,_)],pats) =
		(funsym(b,e), TUPLEpat[a,c] :: map checkpat pats)
  | makecl([],[(a,NONfix,_),(b,INfix _,e),(c,NONfix,_)]) =
		(funsym(b,e), [TUPLEpat[a,c]])
  | makecl([],(a,NONfix,e)::(pats as _::_)) =
		(funsym(a,e), map checkpat pats)
  | makecl([],(a,INfix _,e)::(pats as _::_)) =
		(e COMPLAIN "infix operator used without 'op' in fun dec";
		 (funsym(a,e), map checkpat pats))
  | makecl(_,(_,_,e)::_) = (e COMPLAIN "can't find function symbol in fun dec";
				(bogusID,[WILDpat]))
  | makecl((_,_,e)::_,_) = (e COMPLAIN "can't find function symbol in fun dec";
				(bogusID,[WILDpat]))
  | makecl _ = ErrorMsg.impossible "CoreLang.makecl"

fun makeFUNdec ((fb,fv),err) (env,_,tv,st) =
    let val localtyvars = diff_tyvars(fv,tv)
	val downtyvars = union_tyvars(localtyvars,tv)
	val localtyvarlist = get_tyvars localtyvars
        val env' = ref(Env.empty: Modules.env)
	fun makevar (p as ({name,...}:rawclause)::_) =
	     let val v = mkVALvar name
	      in env' := Env.bind(name,VARbind v,!env'); (v,p)
	     end
	  | makevar _ = ErrorMsg.impossible "makeFUNdec.makevar"
        val clauses = map makevar (fb env)
	val env'' = Env.atop(!env',env)
        fun makeclause{name,pats,resultty,exp,err} =
	   CLAUSE{pats=pats,resultty=resultty,
		  exp=exp(Env.atop(bindVARp(pats,err),env''),downtyvars,st)}
	fun evalclauses(v,l) = (v,map makeclause l)
	val fbs = map evalclauses clauses
	fun makefb (v as VALvar{name=[n],...},c) =
	    ((*bindVAR(n,v);  ???? *)
	     FB{var=v,clauses=c,tyvars=localtyvarlist})
	  | makefb _ = ErrorMsg.impossible "makeFUNdec.makefb"
     in checkUniq(err,"duplicate function names in fun dec") 
	          (map (fn(VALvar{name=[n],...},_)=>n) fbs);
        (FUNdec(map makefb fbs),!env')
    end

fun makeTB(args,name,(ty,tv),err) notwith (env,path,_,_) =
 let val typ = ty env
     val _ = (checkbound(tv,args,err);
	       TypesUtil.bindTyvars args; compressTy typ)
     val binding = DEFtyc{path=name::path, 
			  tyfun=TYFUN{arity=length args, body=typ}}
  in ([TB{tyc=binding,def=typ}],
      if notwith then Env.empty
      else Env.bind(name,TYCbind binding,Env.empty))
  end
       
val nullTB = fn _ => fn _ => (nil,Env.empty)

fun makeTYPEdec ((tbs,env),err) =
   let val _ = checkUniq(err, "duplicate type definition") 
                  (map (fn TB{tyc=DEFtyc{path=name::_,...},...} => name
			 | _ => ErrorMsg.impossible "CoreLang.makeTYPEdec")
		       tbs)
       val env' = ref env  
       fun bindtyc (TB{tyc as DEFtyc{path=name::_,...},...}) = 
	     env' := Env.bind(name,TYCbind tyc,!env')
         | bindtyc _ = ErrorMsg.impossible "makeTYPEdec.bindtyc"
   in app bindtyc tbs;
      (TYPEdec tbs, !env')
   end

fun makeDB(db,tb,err) (env,path,tv,scope:stampgen) =
  let val env' = ref(Env.empty: Modules.env)
      fun predefine (id,arity,db') = 
	  let val r = ref(DATAtyc nil)
	      val tyc = GENtyc{path=id::path,arity=arity,
			      stamp=Stamps.newStamp scope (), eq=ref DATA,
			      kind = r}
	   in env' := Env.bind(id,TYCbind tyc,!env');
	      (tyc,r,db')
	  end
      val db'' = map predefine db
      val (withtycs,env'') = tb false (Env.atop(!env',env),path,tv,scope)
      val _ = env' := Env.atop(env'', !env')
      fun redefine (tyc,r,f) = 
		let val (r',env'') = f(Env.atop(!env',env),path)
		in r := DATAtyc(r');
		   env' := Env.atop(env'',(!env'));
		   tyc
		end
      val datatycs = map redefine db''
   in checkUniq(err,"duplicate datatype name") (map #1 db);
      app (defineEqTycon (fn x => x)) datatycs;
      (DATATYPEdec{datatycs=datatycs,withtycs=withtycs},!env')
  end

fun makeDB'(args,name,(constrs,cv),err) (env,path) =
  let val arity = length args
      val rangeType = CONty(lookArTYC (env,[name],arity,err), 
			    map VARty args)
      val _ = (checkbound(cv,args,err); TypesUtil.bindTyvars args)
      val dcl = constrs (env,rangeType)
      val sdcl = sort3 dcl
      val sign = ConRep.boxed(sdcl)
      val env' = ref(Env.empty: Modules.env)
      fun binddcons ((sym,const,typ)::restdcl,rep::restsign) =
		let val _ = compressTy typ
		    val dcon = DATACON{name=sym, const=const, 
				       rep=rep, sign=sign,
			       typ = if arity > 0
				     then POLYty
					   {sign=mkPolySign arity, abs=0,
					    tyfun=TYFUN{arity=arity,body=typ}}
				     else typ}
		 in env' := Env.bind(sym, CONbind dcon,!env');
	 	    dcon :: binddcons(restdcl,restsign)
		end
	     | binddcons ([],[]) = []
	     | binddcons _ = impossible err "Corelang.makeDB'.binddcons"
  in if length sdcl < length dcl
       	    then err COMPLAIN "duplicate constructor name" else ();
     (binddcons(sdcl,sign),!env')
  end


fun makeABSTYPEdec(db,tb, ldecs, err) ($ as (env,path,tv,stamps)) =
    let val (DATATYPEdec{datatycs,withtycs},env0) = makeDB(db,tb,err) $
        val withtycons = map (fn TB{tyc,...} => tyc) withtycs 
        val (body,env'') = ldecs (Env.atop(env0,env),path,tv,stamps)
	val env' = ref env''
	val bind = fn tyc => 
		       env' := Env.bind(tycName tyc,TYCbind tyc,!env')
    in app bind datatycs;    (* will become abstycs during type checking *)
       app bind withtycons;  (* withtycs *)
       (ABSTYPEdec{abstycs=datatycs,withtycs=withtycs,body=body}, !env')
    end

fun makeOVERLOADdec(id,(ty,tyvars),exps) (env,path,tv,stamps) =
    let val tvs = get_tyvars tyvars
	val body = ty env
        val scheme = (TypesUtil.bindTyvars tvs;
		      TypesUtil.compressTy body;
		      TYFUN{arity=length tvs, body=body})
        fun option (MARKexp(e,_,_)) = option e
	  | option (VARexp(ref (v as VALvar{typ,...}))) =
              {indicator = TypesUtil.matchScheme(scheme,!typ), variant = v}
          | option _ = ErrorMsg.impossible "CoreLang.makeOVERLOADdec.option"
        val ovldvar = OVLDvar{name=id,
			      options=ref(map option (exps(env,tv,stamps))),
			      scheme=scheme}
    in (OVLDdec ovldvar, Env.bind(id,VARbind ovldvar,Env.empty))
    end

fun makeFIXdec(fixity,ops) _ =
    let fun bindfix (ident,env) = 
	  let val fixvar = FIXvar{name=ident,binding=fixity}
	  in Env.bind(ident,FIXbind fixvar,env)
	  end
    in (FIXdec {fixity=fixity,ops=ops},revfold bindfix ops Env.empty)
    end

fun makeEXCEPTIONdec((ebs,env),err) =
  let fun getname(EBgen{exn=DATACON{name,...},...}) = name
        | getname(EBdef{exn=DATACON{name,...},...}) = name
   in checkUniq (err, "duplicate exception declaration") (map getname ebs);
      (EXCEPTIONdec ebs,env)
  end

val itsym = Symbol.varSymbol "it"

fun toplevelexp(env,(exp,tyvars),err,loc) =
    let val exp = exp (env,tyvars,Stamps.freeScope)
	val pat = VARpat(mkVALvar itsym)
	val dec = VALdec[VB {exp = exp, pat = pat, tyvars = get_tyvars tyvars}]
    in Typecheck.decType(env,dec,true,err,loc);
       (dec,bindVARp([pat],err loc))
    end

end
