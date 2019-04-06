(* Copyright 1989 by AT&T Bell Laboratories *)
signature CORELANG =
sig type 'a precStack
    type stampsets
    type 'a vstamped
    type 'a pathvstamped
    type 'a pathstamped
    type 'a tpathed
    type 'a susp
    type 'a uvars
    type tyclooker
    type 'a tsusp
    open Symbol Basics Absyn
(*    type exp type pat type fixity type label type symbol
    type ty type vb type dec type rvb type tyvar type tycon type datacon
    type eb type rule type structureVar type tb 
*)
    val lookID : Symbol.symbol * ErrorMsg.complainer -> BareAbsn.exp
 
    val exp_start:	exp * fixity * ErrorMsg.complainer -> exp precStack
    val exp_parse:	exp precStack * exp * fixity * ErrorMsg.complainer -> exp precStack
    val exp_parse':	exp precStack * exp * fixity * ErrorMsg.complainer -> exp precStack
    val exp_finish:	exp precStack * ErrorMsg.complainer -> exp
    val make_app_pat:	(pat * fixity * ErrorMsg.complainer) list -> pat
    val pat_id:		symbol -> pat
    val checkUniq:      ErrorMsg.complainer * string -> Symbol.symbol list -> unit
    val make_recordTy:  (symbol * ty) list * ErrorMsg.complainer -> ty
    val makeRECORDexp:  (symbol * exp) list * ErrorMsg.complainer -> exp
    val completeMatch:  rule list -> rule list
    val layered: 	pat * pat * ErrorMsg.complainer -> pat
    val makeRECORDpat:	((label * pat) list * bool) * ErrorMsg.complainer -> pat
    val qid_pat:	symbol list * ErrorMsg.complainer -> pat
    val valbind:	pat susp uvars * exp vstamped uvars -> vb list vstamped
    val makeEB:		symbol -> eb list pathvstamped
    val makeEBof:	symbol * ty tsusp * ErrorMsg.complainer
	                  -> eb list pathvstamped
    val makeEBeq:	symbol * symbol list * ErrorMsg.complainer
	                  -> eb list pathvstamped
    val makeVALdec:	vb list vstamped * ErrorMsg.complainer -> dec pathvstamped
    val makeSEQdec:	dec pathvstamped * dec pathvstamped -> dec pathvstamped
    val makeLOCALdec:	dec pathvstamped * dec pathvstamped -> dec pathvstamped
    val makeOPENdec:	structureVar list susp -> dec pathvstamped
    val makeHANDLEexp:  exp * rule list -> exp
    val makeRULE:	pat susp * exp vstamped * ErrorMsg.complainer -> rule vstamped
    type rawrvb
    val makeVALRECdec:	rawrvb list susp * ErrorMsg.complainer -> dec pathvstamped
    type rawclause
    val checkFB:	rawclause list * ErrorMsg.complainer -> rawclause list
    val makecl:		(pat * fixity * ErrorMsg.complainer) list *
		        (pat * fixity * ErrorMsg.complainer) list 
				-> Symbol.symbol * pat list
    val makeFUNdec:	rawclause list list susp uvars -> dec pathvstamped
    val makeTB:		tyvar list * symbol * ty tsusp uvars * ErrorMsg.complainer -> 
				bool -> tb list pathvstamped
    val makeDB:		(symbol * int * datacon list tpathed) list * (bool -> tb list pathvstamped) ->
				dec pathvstamped
    val makeDB':	tyvar list * symbol * (ty * tyclooker -> (symbol * bool * ty) list) uvars
				* ErrorMsg.complainer  -> datacon list tpathed
    val makeTYPEdec:    tb list * ErrorMsg.complainer -> dec
    val makeABSTYPEdec: (symbol * int * datacon list tpathed) list 
			  * dec pathvstamped   ->   dec pathvstamped
    val makeOVERLOADdec: symbol * ty tsusp uvars * exp list vstamped -> dec pathvstamped
    val makeFIXdec:	fixity * symbol list -> dec pathvstamped
    val makeEXCEPTIONdec:  eb list * ErrorMsg.complainer -> dec
    val toplevelexp:	exp vstamped uvars * (BareAbsn.linenum*BareAbsn.linenum->ErrorMsg.complainer) * (BareAbsn.linenum*BareAbsn.linenum) -> dec
    val tyArg:	tyclooker
end


structure CoreLang: CORELANG = struct    
open ErrorMsg Symbol PrtUtil EqTypes
open Access Basics BasicTyp TypesUtl Absyn TyvarSet
open Env EnvAcc Misc

type stampsets = Stampset.stampsets
(* type 'a stamped = stampsets -> 'a *)
type 'a vstamped = tyvarset * stampsets -> 'a
type 'a pathstamped = symbol list * stampsets -> 'a
type 'a pathvstamped = symbol list * tyvarset * stampsets -> 'a
type 'a susp = unit -> 'a
type tyclooker = symbol list * int * (string->unit) -> tycon
type 'a tsusp = tyclooker -> 'a
type 'a tpathed = tyclooker * symbol list -> 'a
type 'a uvars = 'a * tyvarset

fun impossible err s = (err BUG s; raise ErrorMsg.Syntax)

fun lookID(id,err) = varcon (lookVARCON id)
     handle Unbound => (err COMPLAIN ("unbound variable " ^ Symbol.name id);
		      VARexp(ref(mkVALvar id)))


val tyArg = lookPathArTYC

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
       | parse(p as NILf, _,_,err) = impossible err "parse NILf"
       | parse(p as NONf(e1,INf(bp,e2,NONf(e3,r))), e4, f as INfix(lbp,rbp),err)=
	    if lbp > bp then INf(rbp,e4,p)
	     else (if lbp = bp
			then err WARN "mixed left- and right-associative \
				      \operators of same precedence"
			else ();
	           parse(NONf(app err(e2,pair err (e3,e1)),r),e4,f,err))
       | parse(p as NONf _, e',INfix(lbp,rbp),_) = INf(rbp,e',p)
     
     fun parse'(NONf(e,r), e',NONfix,err) = 
			(err COMPLAIN "subexpression should be parenthesized";
			 NONf(app err(e,e'),r))
       | parse' args = parse args
     
     fun finish (NONf(e1,INf(_,e2,NONf(e3,r))),err) = 
		     finish(NONf(app err(e2,pair err (e3,e1)),r),err)
       | finish (NONf(e1,NILf),_) = e1
       | finish (INf(_,e1,NONf(e2,p)),err) = 
		     (err COMPLAIN "nonfix identifier required";
		      finish(NONf(app err(e2,e1),p),err))
       | finish (NILf,err) = impossible err "finish NILf"

  in {start=start,parse=parse,parse'=parse',finish=finish}
 end
end

val {start=exp_start, parse=exp_parse, parse'=exp_parse', finish=exp_finish} = 
	precedence(fn _ => APPexp, fn _ => fn (a,b) => TUPLEexp[a,b])

fun APPpat' (CONpat dcon,pat) = APPpat(dcon,pat)

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

val {start=pat_start, parse=pat_parse0, parse'= _, finish=pat_finish0} =
	precedence(apply_pat, 
		   fn err => fn (ap1,ap2) =>
			TUPLEpat[clean_pat err ap1, clean_pat err ap2])

fun pat_parse(ap,(p,f,err)) = pat_parse0(ap, p, f,err)
fun pat_finish(ap,err) = clean_pat err (pat_finish0(ap,err))

fun pat_id id = CONpat(lookCON id) 
		   handle Unbound => VARpat(mkVALvar id)

fun checkUniq (err,message) l =
 let val l' = Sort.sort (fn (a,b) => Symbol.name a > Symbol.name b) l
     fun f (x::y::rest) = (if Symbol.eq(x,y) 
			      then err COMPLAIN(message^ ": " ^ Symbol.name x)
			      else ();
			   f(y::rest))
      | f _ = ()
  in f l'
 end

fun bindVARp(patlist,err) =
 let val vl = ref (nil: symbol list)
     val rec f =
           fn VARpat(v as VALvar{name=[name],...})=> 
		    (if Symbol.eq(name,EQUALsym)
			    then err WARN "rebinding =" else ();
		     bindVAR(name,v);
		     vl := name :: !vl)
	    | RECORDpat{fields,...} => app(fn(_,pat)=>f pat) fields
	    | APPpat(_,pat) => f pat
	    | CONSTRAINTpat(pat,_) => f pat
	    | LAYEREDpat(p1,p2) => (f p1; f p2)
	    | _ => ()
  in app f patlist 
     before checkUniq (err,"duplicate variable in pattern(s)") (!vl)
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

val exnID = Symbol.symbol "exn"

fun makeHANDLEexp(exp,rules) =
     let fun anywild (RULE(WILDpat,_)) = true
	   | anywild (RULE(VARpat _,_)) = true
	   | anywild _ = false
         val rules = if exists anywild rules then rules
		else let val v1 as VALvar{name,access=LVAR v,typ} = 
								mkVALvar exnID
			 val r = RULE(VARpat v1,
				      RAISEexp(VARexp(ref(VALvar{name=name,
						   access=PATH[v], typ=typ}))))
		      in completeMatch' r rules
		     end
      in HANDLEexp(exp, HANDLER(FNexp rules))
     end
	
fun makeRECORDpat((l,flex),err) =
	  RECORDpat{fields= sortRecord(l,err), 
		    flex=flex, typ=ref UNDEFty, pats=ref nil}

fun qid_pat (qid,err) = CONpat(lookPathCON(qid,err COMPLAIN))

fun valbind ((pat,pv),(exp,ev)) (tv,st) =
    let val localtyvars = diff_tyvars(union_tyvars(pv,ev),tv)
	val localtyvarlist = get_tyvars localtyvars
	val downtyvars = union_tyvars(localtyvars,tv)
        val pat = pat() and exp = exp(downtyvars,st)
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

val bogusCON = DATACON{name=bogusID,const=true,typ=ERRORty,
		       rep=UNDECIDED,sign=[]}
fun getEXN(id,err) = lookCON(id) 
	handle Unbound => (err COMPLAIN("unbound exn: " ^ Symbol.name id); 
			   bogusCON)

fun makeEB (id:symbol) (_) =
  let val exn = DATACON{name=id,const=true,typ=exnTy,
			rep=VARIABLE(LVAR(namedLvar id)), sign=[]}
   in bindCON(id,exn); [EBgen{exn=exn,etype=NONE}]
  end

fun makeEBof (id,etype,err) _ =
  let val ty = etype tyArg
      val exn = DATACON{name=id,const=false,typ=(ty --> exnTy),
			rep=VARIABLE(LVAR(namedLvar id)), sign=[]}
   in bindCON(id,exn); [EBgen{exn=exn,etype=SOME ty}]
  end

fun makeEBeq (id,qid,err) (_) =
  let val edef as DATACON{const,typ,rep,sign,...} =
	       case qid of [s] => getEXN(s,err)
			 | q => lookPathCON(q, err COMPLAIN)
      val exn = DATACON{name=id,const=const,typ=typ,sign=sign,
			rep=VARIABLE(LVAR(namedLvar id))}
   in bindCON(id,exn); [EBdef{exn=exn,edef=edef}]
  end

fun makeVALdec (vb,err) (path,tv,st) =
   let val l = vb(tv,st)
    in bindVARp (map (fn VB{pat,...}=>pat) l, err); VALdec l
   end

fun makeSEQdec (d1,d2) $ =
  case (d1 $, d2 $)
   of (SEQdec a, SEQdec b) => SEQdec(a@b)
    | (SEQdec a, b) => SEQdec(a@[b])
    | (a, SEQdec b) => SEQdec(a::b)
    | (a,b) => SEQdec[a,b]

fun makeLOCALdec (ldecs1,ldecs2) (path,tv,st) =
  let val envLocal=openScope()
      val ld1 = ldecs1([],tv,st)
      val envIn = (openScope(); current())
      val ld2 = ldecs2(path,tv,st)
   in splice(envLocal,envIn);
      LOCALdec(ld1,ld2)
  end

fun makeOPENdec qid_p _ = 
   let val strs = qid_p()
    in app openStructureVar strs;
       OPENdec strs
   end

fun makeRULE(pat,exp,err) st =
  protect(protectScope, fn() => 
     let val p = pat() in bindVARp([p],err); RULE(p, exp st) end)

type rawrvb = {name:symbol,ty:ty option susp uvars,match:rule list vstamped uvars}

fun makeVALRECdec (rvb,err) (path,tv,st) =
    let val rvbs = rvb()
	val tyvars = fold (fn({match=(_,mv),ty=(_,cv),name},v)=>
				union_tyvars(union_tyvars(mv,cv),v)) 
			rvbs no_tyvars
	val downtyvars = union_tyvars(tyvars,tv)
	val localtyvarlist = get_tyvars(diff_tyvars(tyvars,tv))
	fun makevar (p as {name,...}:rawrvb) = 
	      let val v = mkVALvar name
	       in bindVAR(name,v); (v,p)
	      end
	val rvbs' = map makevar rvbs
	fun makervb(v,{ty=(ty,_),match=(match,_),...}:rawrvb) =
	      RVB{var=v,resultty=ty(),tyvars=localtyvarlist,
		  exp=FNexp(completeMatch(match(downtyvars,st)))}
     in checkUniq(err,"duplicate function name in val rec dec") (map #name rvbs);
        VALRECdec(map makervb rvbs')
    end

type rawclause = {name:symbol,pats:pat list,resultty:ty option,exp:exp vstamped,
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

fun make_app_pat((p as (_,_,err))::rest) =
  let fun f(x,p::r) = f(pat_parse(x,p),r)
        | f(x,nil) = pat_finish(x,err)
   in f(pat_start p, rest)
  end

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

fun makeFUNdec (fb,fv) (_,tv,st) =
    let val localtyvars = diff_tyvars(fv,tv)
	val downtyvars = union_tyvars(localtyvars,tv)
	val localtyvarlist = get_tyvars localtyvars
	fun makevar (p as ({name,...}:rawclause)::_) =
		let val v = mkVALvar name
		 in bindVAR(name,v); (v,p)
		end
        fun makeclause{name,pats,resultty,exp,err} =
	  protect(protectScope, fn()=>
	    (bindVARp(pats,err);
	     CLAUSE{pats=pats,resultty=resultty,exp=exp(downtyvars,st)}))
	fun evalclauses(v,l) = (v,map makeclause l)
	val fbs = map evalclauses (map makevar (fb()))
	fun makefb (v as VALvar{name=[n],...},c) =
	    (bindVAR(n,v);
	     FB{var=v,clauses=c,tyvars=localtyvarlist})
     in FUNdec(map makefb fbs)
    end

fun makeTB(args,name,(ty,tv),err) notwith (path,_,_) =
 let val typ = ty tyArg
     val _ = (checkbound(tv,args,err);
	       TypesUtl.bindTyvars args; compressTy typ)
     val binding = DEFtyc{path=name::path, 
			  tyfun=TYFUN{arity=length args, body=typ}}
  in if notwith
       then ()
       else (bindTYC(name,binding); ());
     [TB{tyc=binding,def=typ}]
 end
       
fun makeTYPEdec(tbs,err) =
  (checkUniq(err, "duplicate type definition") 
             (map (fn TB{tyc=DEFtyc{path=name::_,...},...}=>name) tbs);
   app (fn TB{tyc as DEFtyc{path=name::_,...},...}=> bindTYC(name,tyc)) tbs;
   TYPEdec tbs)

fun makeDB(db,tb) (path,tv,stamps as {tycStamps,...}:stampsets) =
  let fun predefine (id,arity,db') = 
	  let val r = ref(DATAtyc nil)
	      val tyc = GENtyc{path=id::path,arity=arity,
			      stamp=Stampset.newStamp tycStamps, eq=ref DATA,
			      kind = r}
	   in bindTYC(id,tyc);
	      (tyc,r,db')
	  end
      val db'' = map predefine db
      val withtycs = tb false (path,tv,stamps)
      fun redefine (tyc,r,f) = (r := DATAtyc(f(tyArg,path)); tyc)
      val datatycs = map redefine db''
   in app (defineEqTycon (fn x => x)) datatycs;
      DATATYPEdec{datatycs=datatycs,withtycs=withtycs}
  end

fun makeDB'(args,name,(constrs,cv),err) (tyclooker,path) =
  let val arity = length args
      val rangeType = CONty(tyclooker([name],arity,err COMPLAIN), map VARty args)
      val _ = (checkbound(cv,args,err); TypesUtl.bindTyvars args)
      val dcl = constrs(rangeType,tyclooker)
      val sdcl = sort3 dcl
      val sign = ConRep.boxed(sdcl)
      fun binddcons ((sym,const,typ)::restdcl,rep::restsign) =
		let val _ = compressTy typ
		    val dcon = DATACON{name=sym, const=const, 
				       rep=rep, sign=sign,
			       typ = if arity > 0
				     then POLYty
					   {sign=mkPolySign arity, abs=0,
					    tyfun=TYFUN{arity=arity,body=typ}}
				     else typ}
		 in bindCON(sym, dcon);
	 	    dcon :: binddcons(restdcl,restsign)
		end
	     | binddcons ([],[]) = []
	     | binddcons _ = impossible err "Parse.db1.fn.binddcons"
  in if length sdcl < length dcl
       	    then err COMPLAIN "duplicate constructor name" else ();
     binddcons(sdcl,sign)
  end

fun makeABSTYPEdec(db,ldecs) $ =
    let val mAbstype = openScope()
	val DATATYPEdec{datatycs,withtycs} = makeDB(db,fn _=> fn _ => nil) $
	val withtycons = map (fn TB{tyc,...} => tyc) withtycs
	val mWith = (openScope(); current())
	val body = ldecs $
	fun bind tyc = bindTYC(tycName tyc, tyc)
     in splice(mAbstype,mWith);
	app bind datatycs;  (* will become abstycs during type checking *)
	app bind withtycons;
	ABSTYPEdec{abstycs=datatycs,withtycs=withtycs,body=body}
    end

fun makeOVERLOADdec(id,(ty,tyvars),exps) (path,tv,stamps) =
    let val tvs = get_tyvars tyvars
	val body = ty tyArg
        val scheme = (TypesUtl.bindTyvars tvs;
		      TypesUtl.compressTy body;
		      TYFUN{arity=length tvs, body=body})
        fun option (MARKexp(e,_,_)) = option e
	  | option (VARexp(ref (v as VALvar{typ,...}))) =
              {indicator = TypesUtl.matchScheme(scheme,!typ), variant = v}
     in bindVAR(id,OVLDvar{name=id,options=ref(map option (exps(tv,stamps))),
		scheme=scheme});
        SEQdec nil
    end

fun makeFIXdec(fixity,ops) _ =
 (app (fn ident => bindFIX(ident,FIXvar{name=ident,binding=fixity})) ops;
  SEQdec nil)

fun makeEXCEPTIONdec(ebs,err) =
  let fun getname(EBgen{exn=DATACON{name,...},...}) = name
        | getname(EBdef{exn=DATACON{name,...},...}) = name
   in checkUniq (err, "duplicate exception declaration") (map getname ebs);
      EXCEPTIONdec ebs
  end

val itsym = Symbol.symbol "it"

fun toplevelexp((exp,tyvars),err,loc) =
    let val exp = exp (tyvars,Stampset.globalStamps)
	val pat = VARpat(mkVALvar itsym)
	val dec = VALdec[VB {exp = exp, pat = pat, tyvars = get_tyvars tyvars}]
     in bindVARp([pat],err loc);
	Typechk.decType(dec,true,err,loc); dec
    end

end
