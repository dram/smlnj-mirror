(* envaccess.sml *)

(* lookup and binding functions *)

structure EnvAccess (* : ENVACCESS *) = struct

structure Access = Access
structure Basics = Basics
structure Env = Env

infix before
fun a before b = a

local
  open ErrorMsg
  open PrintUtil
  open Access
  open Basics
  open Basics.Symbol
  open Absyn
  open Env
  open StrAccess
  open BasicTypes
in

(* debugging flag *)
val debugPatch = ref false
val debugBind = ref false
val debugBuild = ref false

(* type constructors *)

val checkTYC: binding -> tycon ref = 
    fn (TYCbind tycref) => tycref | _ => raisex Table.next;

val lookTYCinTable = Table.look checkTYC;

fun evalSYMtyc(newpath,name,PARAM{pnode,spath=oldpath,sign,...}) =
    let fun loop(pnode as STRpnode{sign,...}, p as id::rest) =
	      let val (_,pnode') = pnodeSon(pnode,id)
	       in loop(pnode',rest)
	      end
	      handlex pnodeSon => 
	        let val (sign' as SIG{env,...},_) = getSign(sign,p,[])
		    val param = PARAM{pnode=pnode,spath=p,sign=sign'}
		 in case !(lookTYCinTable(env,name))
		      of DATAtyc{stamp,context,name,params,dcons} =>
			   ref(DATAtyc{stamp=stamp,name=name,params=params,
				       dcons=dcons,
				       context=PARctx(param)})
		       | VARtyc{stamp,context,name,arity} =>
			   ref(VARtyc{stamp=stamp,name=name,arity=arity,
				      context=PARctx(param)})
		       | _ => Impossible "envaccess.309"
		end
	  | loop(pnode as STRpnode{sign=sign' as SIG{env,...},sons,...},[]) =
	      let val param = PARAM{pnode=pnode,spath=[],sign=sign'}
	       in case !(lookTYCinTable(env,name))
		    of DATAtyc{stamp,context,name,params,dcons} =>
			 let val (_,TYCpnode{stamp=s}) = pnodeSon(pnode,name)
			  in ref(DATAtyc{stamp=s,name=name,params=params,
					 dcons=dcons,context=SHRctx(param)})
			 end
			 handlex pnodeSon =>
			   ref(DATAtyc{stamp=stamp,name=name,params=params,
				       dcons=dcons,
				       context=PARctx(param)})
		     | VARtyc{stamp,context,name,arity} =>
			 let val (_,TYCpnode{stamp=s}) = pnodeSon(pnode,name)
			  in ref(VARtyc{stamp=s,name=name,arity=arity,
					context=SHRctx(param)})
			 end
			 handlex pnodeSon =>
			   ref(VARtyc{stamp=stamp,name=name,arity=arity,
				      context=PARctx(param)})
		     | _ => Impossible "envaccess.389"
	      end
	  | loop _ = Impossible "envaccess.38"
     in case oldpath
	  of [] => loop(pnode,newpath)
	   | _ =>
	        let val (sign' as SIG{env,...},_) = getSign(sign,newpath,[])
		    val param = PARAM{pnode=pnode,spath=oldpath@newpath,sign=sign'}
		 in case !(lookTYCinTable(env,name))
		      of DATAtyc{stamp,context,name,params,dcons} =>
			   ref(DATAtyc{stamp=stamp,name=name,params=params,
				       dcons=dcons,
				       context=PARctx(param)})
		       | VARtyc{stamp,context,name,arity} =>
			   ref(VARtyc{stamp=stamp,name=name,arity=arity,
				      context=PARctx(param)})
		       | _ => Impossible "envaccess.327"
		end
    end
  | evalSYMtyc _  = Impossible "envaccess.28"

fun lookTYCinStr(str: Structure, id: symbol) : tycon ref =
    (case str
      of DIRECT{context,table,...} =>
	   tyconInCtx(lookTYCinTable(table,id),context)
       | FCTAPP{body,env,context,...} =>
	   lookTYCinStr(reduceStr(body,RELctx(str,context)),id)
       | PARAM{sign=SIG{env,...},...} =>
	   tyconInCtx(lookTYCinTable(env,id),PARctx(str))
       | SPEC(SIG{env,...}) => lookTYCinTable(env,id))
    handlex Table.notfound => Condemn("unbound tycon in str: " ^ Name id)

and tyconInCtx(tyconref,ctx) : tycon ref =
    case !tyconref
      of TYPEtyc{stamp,context,name,params,def} =>
	   ref(TYPEtyc{stamp=stamp,name=name,params=params,def=def,
	   	   context = appendCtxMaybe(context,ctx)})
	   handlex appendContext => tyconref
       | DATAtyc{stamp,context,name,params,dcons} =>
	   (case (context,ctx)
	     of (SIGctx,PARctx(param as PARAM{pnode,...})) =>
		  let val (_,TYCpnode{stamp=s}) = pnodeSon(pnode,name)
		   in ref(DATAtyc{stamp=s,name=name,params=params,dcons=dcons,
		                  context=SHRctx(param)})
		  end
		  handlex pnodeSon =>
		    ref(DATAtyc{stamp=stamp,name=name,params=params,dcons=dcons,
		              context=ctx})
	      | (PARctx(PARAM{pnode,spath,...}),RELctx(FCTAPP{env,...},ctx')) =>
		  let val (str,_) = getStr(lookPnode(pnode,env),spath,ctx',[])
		   in lookTYCinStr(str,name)
		  end
	      | _ =>
		  ref(DATAtyc{stamp=stamp,name=name,params=params,dcons=dcons,
		              context = appendCtxMaybe(context,ctx)})
	          handlex appendContext => tyconref)
       | VARtyc{stamp,context,name,arity} =>
	   (case (context,ctx)
	     of (SIGctx,PARctx(param as PARAM{pnode,...})) =>
		  (* type component of a functor parameter *)
		  let val (_,TYCpnode{stamp=s}) = pnodeSon(pnode,name)
		   in ref(VARtyc{stamp=s,name=name,arity=arity,
		      		 context=SHRctx(param)})
		  end
		  handlex pnodeSon =>
		    ref(VARtyc{stamp=stamp,context=ctx,name=name,arity=arity})
	      | (SIGctx,TOPctx) =>
		  (* signature type component -- while parsing signature *)
		  tyconref
	      | (SIGctx,FCTctx) =>
		  (* signature type component -- while parsing signature *)
		  tyconref
	      | (PARctx(PARAM{pnode,spath,...}),RELctx(FCTAPP{env,...},ctx')) =>
		  (* type comp of structure bound to functor parameter *)
		  let val (str,_) = getStr(lookPnode(pnode,env),spath,ctx',[])
		   in lookTYCinStr(str,name)
		  end
	      | (PARctx _,FCTctx) =>
		  (* type def expansion inside a functor *)
		  tyconref
	      | _ => 
		(printSym name; newline();
		 PrintBasics.printContext context; newline();
		 PrintBasics.printContext ctx; newline();
		 Impossible "tyconInCtx -- VARtyc" ))
       | SYMtyc(spath,name) =>
	   let val PARctx(param) = ctx    (* ctx must be PARctx *)
	    in evalSYMtyc(spath,name,param)
	   end
       | _ => tyconref

fun lookTYC id =
    let val (tyconref,(_,ctx)) = look(false, lookTYCinTable) id
     in tyconInCtx(tyconref,ctx)
    end
    handlex unbound => Condemn("unbound tycon: " ^ Name id)

fun bindTYC(id: symbol, tc: tycon) =
    let val tc = ref tc
     in if !debugBind
	  then (prstr "bindTYC: "; printSym id; newline())
	  else ();
	add(id, TYCbind tc); tc
    end

local (* type constructor patching *)
  val inDatatype : bool ref = ref false;  (* context flag *)
  val tyconPatchList : tycon ref list ref = ref nil
in

  fun enterDb () = (
    tyconPatchList := nil;
    inDatatype := true );

  fun lookPatchTYC id =
    if !inDatatype
      then let val tc = ref (UNKNOWNtyc id)
            in tyconPatchList := tc :: !tyconPatchList;
	       tc
	   end
      else lookTYC id

  fun patchTycons (tc::l) =
      let val ref(UNKNOWNtyc id) = tc
       in tc := !(lookTYC id);
          patchTycons(l)
      end
    | patchTycons nil = ();

  fun exitDb () =
    ( patchTycons(!tyconPatchList); inDatatype := false )

end (* local -- patch *)


fun typeInCtx(ty: ty, ctx: context) : ty =
    (* this could also "meta-instantiate" any generic variables, since otherwise
       the type is copied twice; i.e. it could subsume freshty -- but this
       should not happen for the call of typeInCtx in varInStr *)
    let fun tic(ty) =
        (case ty
	  of CONty(tyconref, argtys) => 
	       CONty(tyconInCtx(tyconref,ctx),
		     map tic argtys)
	       handlex appendContext => 
	         CONty(tyconref, map tic argtys)
	   | VARty(TYVAR{status=ref(INSTANTIATED(ty')),...}) => tic(ty')
	   | VARty _ => ty
	   | UNKNOWNty => ty
	   | FLEXRECORDty _ => Impossible "typeInCtx -- FLEXRECORDty" )
     in case ctx
	  of TOPctx => ty
	   | FCTctx => ty
	   | SIGctx => ty
	   | _ => tic(ty)
    end

(* constructors *)

(* this version checks for "visibility", but has to be fixed for structures
val checkCON =
   (fn CONbind(c as DATACON{tycon = tyc as DATAtyc{name = id,...} ,...}) =>
         if eqTycon(!(lookTYC id), tyc) then c else raisex Table.next
     | _ => raisex Table.next)
*)

val checkCON = fn CONbind c => c | _ => raisex Table.next

fun dconApplied(dc as DATACON{name,const,vtype,rep,tycon},(ap,ctx)) : datacon =
    DATACON{name = name, const = const, tycon = tycon,
            rep = (case !rep
		     of VARIABLE(SLOT n) => ref(VARIABLE(PATH(n::ap)))
		      | VARIABLE(LVAR v) => ref(VARIABLE(PATH [v]))
		      | _ => rep),  (* defined exception *)
            vtype = typeInCtx(vtype,ctx)}

fun dconInStr(dc as DATACON{name,const,vtype,rep,tycon},ctx,slotNo) : datacon =
    DATACON{name = name, const = const, tycon = tycon,
            rep = (case !rep
		     of VARIABLE(access) => ref(VARIABLE(SLOT slotNo))
		      | _ => rep),
            vtype = typeInCtx(vtype,ctx)}

(* BUG? -- loss of identity of exception on thinning because of copied ref??? *)
fun exnInSig(DATACON{name,const,vtype,tycon,...},slot) =
    DATACON{name = name, const = const, vtype = vtype, tycon = tycon,
            rep = ref(VARIABLE(SLOT slot))}

val lookCONinTable = Table.look checkCON

fun lookCON id = 
    dconApplied(look(false,lookCONinTable) id)

fun lookCONinStr(str: Structure, id: symbol, ap: Access.path) : datacon =
    (case str
      of DIRECT{context,table,...} =>
	   dconApplied(lookCONinTable(table,id),(ap,context))
       | FCTAPP{body,env,context,...} =>
	   lookCONinStr(reduceStr(body,RELctx(str,context)),id,ap)
       | PARAM{sign=SIG{env,...},...} =>
	   dconApplied(lookCONinTable(env,id),(ap,PARctx(str)))   
       | SPEC(SIG{env,...}) => Condemn("lookCONinStr applied to SPEC"))
    handlex Table.notfound => Condemn("unbound data constructor in str: " ^ Name(id))

fun bindCON (id: symbol, c: datacon) =
    (add(id, CONbind c); c)


(* variables *)
(*  this version checks for "visibility" of dcon, but doesn't work with strs 
val checkCON' =  (* like checkCON, but returns binding *)
    fn (b as CONbind(DATACON{tycon = tyc as DATAtyc{name = id,...} ,...})) =>
         if eqTycon(!(lookTYC id), tyc) then b else raisex Table.next
     | _ => raisex Table.next
*)
val checkCON' =  (* like checkCON, but returns binding *)
    fn (b as CONbind(_)) => b | _ => raisex Table.next

val checkVARCON = 
    fn b as VARbind _ => b
     | b => checkCON' b

val checkVAR = fn VARbind v => v | _ => raisex Table.next

val lookVARCONinTable = Table.look checkVARCON

val lookCONinTable' = Table.look checkCON'

val lookVARinTable = Table.look checkVAR

val equalref = ref ~10
val notequalref = ref ~10
val assignref = ref ~10
val updateref = ref ~10

fun varApplied(v:var, (ap:Access.path, ctx:context)) : var =
    case v
      of VALvar{access,name,vtype} =>
	   VALvar{access =
		   (case access
		      of SLOT(n) => PATH(n::ap)
		       | LVAR(n) => PATH([n])
		       | INLINE _ => access
		       | PATH _ => Impossible "varApplied: access = PATH"),
		  vtype = (case (access,ap)
			    of (INLINE i, _) => 
			        if i= !equalref orelse i= !notequalref
					    then ref(newEqualityType())
			        else if i= !assignref then ref(newAssignType())
				else if i= !updateref then ref(newUpdateType())
				else vtype
			     | (_, nil) => vtype
			     | _ => ref(typeInCtx(!vtype,ctx))),
		  name = name}
       | _ => v  (* error?  -- what about OVLDvar bindings? *)

fun varInStr(v:var, ctx:context, slotNo) : var =
    case v
      of VALvar{access = INLINE(_),...} => v
       | VALvar{name,vtype,...} =>
	   VALvar{access = SLOT slotNo,
		  vtype = ref(typeInCtx(!vtype,ctx)),
		  name = name}
       | _ => v

fun varInSig(VALvar{name,vtype,...},slot) =
      VALvar{access=SLOT(slot),name=name,vtype=vtype}
  | varInSig(var,_) = var

fun unboundVAR id = 
    (Complain ("unbound variable " ^ Name id);
     VARbind(VALvar{access=LVAR(mkLvar()), name=id, vtype=ref UNKNOWNty}))

fun lookVARCONinStr(str: Structure, id: symbol, ap: Access.path) : binding =
    (case str
      of DIRECT{context,table,...} =>
	   (case lookVARCONinTable(table,id)
	     of VARbind(var) => VARbind(varApplied(var,(ap,context)))
	      | CONbind(dcon) => CONbind(dconApplied(dcon,(ap,context)))
	      | _ => Impossible "envaccess.381")
       | FCTAPP{body,env,context,...} =>
	   lookVARCONinStr(reduceStr(body,RELctx(str,context)),id,ap)
       | PARAM{sign=SIG{env,...},...} =>
	   (case lookVARCONinTable(env,id)
	     of VARbind(var) => VARbind(varApplied(var,(ap,PARctx(str))))
	      | CONbind(dcon) => CONbind(dconApplied(dcon,(ap,PARctx(str))))
	      | _ => Impossible "envaccess.281")
       | SPEC(SIG{env,...}) => Impossible "lookVARCONinStr applied to SPEC")
    handlex Table.notfound => unboundVAR id

fun lookVARLocal id = 
    varApplied(lookLocalRec (lookVARinTable, NONE) id)

fun lookVARCON id = 
    (case lookLocalRec (lookVARCONinTable, SOME lookCONinTable') id
      of (VARbind v, info) => VARbind(varApplied(v,info))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => Impossible "envaccess.228")
    handlex unbound => unboundVAR id

(* patching deferred variables *)

val varPatchList : var ref list ref = ref nil

fun getPatchVar id =
    let val v = ref (UNKNOWNvar id)
     in varPatchList := v :: !varPatchList;
	v
    end

 exceptionx patched

fun patchVars (pl as (varRef as ref(UNKNOWNvar id))::pl', tl) =
        ((varRef := lookVARLocal id; raisex patched)
  	  handlex unboundrec => 
		    patchVars(pl',varRef::tl)  (* not yet bound; try later *)
              || unbound => (* no more rec layers *)
		   let val VARbind v = unboundVAR id
		    in varRef := v; patchVars(pl',tl)
		   end
	      || patched => patchVars(pl', tl)
	)
  | patchVars (nil, tl) = tl

val protectPatchList =
    ((fn () => (if !debugPatch then prstr "*enterPatch*\n" else ();
	        !varPatchList before (varPatchList := nil))),
     (fn (vpl) => (if !debugPatch then prstr "*exitPatch*\n" else ();
		   varPatchList := patchVars(!varPatchList,vpl))))
	 (* bug -- exit function only works right for normal exit from protect *)

fun looksLikeExn id = substring(Symbol.Name id, 0, 2) = "e_"
			handlex substring => false

fun newVAR(bl: binder list ref, id: symbol) : var =
    let fun checkid ((i,b)::bl) =
	      if Symbol.Eq(i,id)
	        then Complain "repeated var in pattern"
	        else checkid bl
	  | checkid nil = ()
     in checkid(!bl);
	if looksLikeExn id
	     then Warn("exception name used as variable: " ^ Symbol.Name id)
	     else ();
        let val v = mkVALvar(id,ref UNKNOWNty)
	 in bl := (id, VARbind v) :: !bl;
	    v
        end
    end;

fun bindVAR(id: symbol, v: var) = add(id, VARbind v);

fun bindVARs(binders: binder list) = (* app add bl; *)
    app (fn (b as (s,bind)) =>
	    (if !debugBind
		then (prstr "bindVARs: "; printSym s; newline())
		else ();
	     add b))
	binders;


(* type variables *)

datatype BoundTyvars = CLOSEDbtv of tyvar list
		     | OPENbtv of tyvar list ref;

val boundTyvars = ref(CLOSEDbtv nil);

fun protectTyvars v = 
    ((fn () => (!boundTyvars before (boundTyvars := v))),
     (fn btv => boundTyvars := btv))

fun currentTyvars () = let val OPENbtv(ref l) = !boundTyvars in l end

fun checkTYV (TYVbind tyv) = tyv
  | checkTYV _ = raisex Table.next;

fun lookTYV id = 
    let val (tyv,_) = look(false, Table.look checkTYV) id
     in tyv
    end

fun bindTYV(id: symbol, tv: tyvar) =
    ( add(id, TYVbind tv); tv );

fun lookTyvar (id: symbol) =
    case !boundTyvars
      of CLOSEDbtv tyvs =>
	   let fun find ((tyv as TYVAR{name = s,...}) :: l) =
		   if Symbol.Eq(id,s)
		      then tyv
		      else find l
		 | find nil = 
		    (Complain "lookTyvar -- unbound tyvar in closed scope";
		     newTyvar (INSTANTIATED UNKNOWNty))
	    in find tyvs
	   end
       | OPENbtv polyTyvars =>
	   lookTYV id
	   handlex unbound =>
	     let val tyv = bindTYV(id, BasicTypes.mkTyvar(id,FIXED))
	      in polyTyvars := tyv :: !polyTyvars;
		 tyv
	     end;


(* exceptions *)

val checkEXN = fn EXNbind e => e | _ => raisex Table.next;

val lookEXNinTable = Table.look (* checkEXN *) checkCON

fun lookVARinBase id = varApplied(Env.lookBase lookVARinTable id)
fun lookEXNinBase id = dconApplied(Env.lookBase lookEXNinTable id)

infix -->

fun unboundEXN id =
	(Complain("unbound exn: " ^ Name id);
	 DATACON{name=id,const=false,vtype=UNKNOWNty-->exnTy,
	 rep=ref UNDECIDED,tycon = !exnTycon})

fun lookEXN id =
    dconApplied(look (false,lookEXNinTable) id)
    handlex unbound => unboundEXN id

fun lookEXNinStr(str,id,ap) =
    (case str
      of DIRECT{context,table,...} =>
	   dconApplied(lookEXNinTable(table,id),(ap,context))
       | FCTAPP{body,env,context,...} =>
	   lookEXNinStr(reduceStr(body,RELctx(str,context)),id,ap)
       | PARAM{sign=SIG{env,...},...} =>
	   dconApplied(lookEXNinTable(env,id),(ap,PARctx(str)))
       | SPEC(SIG{env,...}) => lookEXNinTable(env,id))
    handlex Table.notfound => unboundEXN id

fun bindEXN(id: symbol, e: datacon) =
    (if !debugBind
       then (prstr "bindEXN: "; printSym id; newline())
       else ();
     add(id, (* EXNbind *) CONbind e); e)


(* signatures *)

fun lookSIG id = 
    let val (sign,_) = 
	look(true, Table.look(fn SIGbind s => s | _ => raisex Table.next)) id
     in sign
    end
    handlex unbound => Condemn("unbound signature: " ^ Name id)
fun bindSIG(id: symbol, s: signatureVar) = (add(id,SIGbind s); s)


(* structures *)

fun strApplied(STRvar{name,access,binding,sign},(ap,ctx)) =
    STRvar{name=name,sign=sign,
	   binding=reduceStr(binding,ctx),
	    (* to relativize binding wrt ctx -- reduceStr is overkill *)
	   access=(case access
		     of SLOT(n) => PATH(n::ap)
		      | LVAR(n) => PATH [n]
		      | _ => Impossible "strApplied: access = PATH or INLINE")}

fun strInStr(STRvar{name,access,binding,sign},ctx,slot) =
    STRvar{name=name,sign=sign,
	   binding=reduceStr(binding,ctx),  (* this is overkill *)
	   access=SLOT(slot)}

fun strInSig(STRvar{name,binding,sign,...},slot) = 
    STRvar{access=SLOT(slot),name=name,binding=binding,sign=sign}

fun lookSTR id =
    strApplied(look (true,lookSTRinTable) id)
    handlex unbound => Condemn("unbound str: " ^ Name id)

fun lookSTRinStr(str,id,ap) =
    (case str
      of DIRECT{context,table,...} =>
	   strApplied(lookSTRinTable(table,id),(ap,context))
       | FCTAPP{body,env,context,...} =>
	   lookSTRinStr(reduceStr(body,RELctx(str,context)),id,ap)
       | PARAM{pnode,spath,sign} =>
	   (* strApplied(lookSTRinTable(env,id),(ap,PARctx(str))) -- wrong *)
	   let val (str as PARAM{sign,...},ap') =
		      extendParam(pnode,spath,sign,[id],ap)
	    in STRvar{name=id,sign=SOME sign,binding=str,access=PATH ap'}
	   end
       | SPEC(SIG{env,...}) => lookSTRinTable(env,id))
    handlex Table.notfound => Condemn("unbound str in str: " ^ Name id)

fun bindSTR(id: symbol, s: structureVar) = (add(id,STRbind s); s)


(* functors *)

val lookFCTinTable = Table.look(fn (FCTbind fv) => fv | _ => raisex Table.next)

fun lookFCT id = 
    let val (fv,_) = look(true,lookFCTinTable) id
     in fv
    end handlex unbound => Condemn("unbound functor identifier: " ^ Symbol.Name id)

fun bindFCT(id: symbol, f: functorVar) = (add(id,FCTbind f); f)


(* fixity bindings *)

fun lookFIX1 id = 
    let val (fix,_) = 
	  look(false, Table.look(fn FIXbind f => f | _ => raisex Table.next)) id
     in fix
    end

fun lookFIX id = lookFIX1 id handlex unbound => NONfix

fun bindFIX(id: symbol, f: fixity) = add(id, FIXbind f)


(* building structures *)

(* is this correct?  *)
fun tyconGt(DATAtyc _, _) = false
  | tyconGt(_, DATAtyc _) = true
  | tyconGt(_) = false
  (* DATAtycs come after other kinds *)

fun printBinding(VARbind _) = prstr "VARbind"
  | printBinding(CONbind _) = prstr "CONbind"
  | printBinding(EXNbind _) = prstr "EXNbind"
  | printBinding(TYCbind _) = prstr "TYCbind"
  | printBinding(TYVbind _) = prstr "TYVbind"
  | printBinding(SIGbind _) = prstr "SIGbind"
  | printBinding(STRbind _) = prstr "STRbind"
  | printBinding(FCTbind _) = prstr "FCTbind"
  | printBinding(FIXbind _) = prstr "FIXbind"

fun binderGt(((id1,FIXbind(_)),_),((id2,FIXbind(_)),_)) =
      Name(id1) > Name(id2)
  | binderGt(((_,FIXbind(_)),_),_) = true
  | binderGt(_,((_,FIXbind(_)),_)) = false
  | binderGt(((id1,VARbind(_)),_),((id2,VARbind(_)),_)) =
      Name(id1) > Name(id2)
  | binderGt(((_,VARbind(_)),_),_) = true
  | binderGt(_,((_,VARbind(_)),_)) = false
  | binderGt(((id1,EXNbind(_)),_),((id2,EXNbind(_)),_)) =
      Name(id1) > Name(id2)
  | binderGt(((_,EXNbind(_)),_),_) = true
  | binderGt(_,((_,EXNbind(_)),_)) = false
  | binderGt(((id1,CONbind(_)),_),((id2,CONbind(_)),_)) =
      Name(id1) > Name(id2)
  | binderGt(((_,CONbind(_)),_),_) = true
  | binderGt(_,((_,CONbind(_)),_)) = false
  | binderGt(((id1,TYCbind(ref tycon1)),_),((id2,TYCbind(ref tycon2)),_)) = 
(*      tyconGt(tycon1,tycon2) orelse     temporarily removed
      not(tyconGt(tycon2,tycon1)) andalso *) Name(id1) > Name(id2)
  | binderGt(((_,TYCbind(_)),_),_) = true
  | binderGt(_,((_,TYCbind(_)),_)) = false
  | binderGt(((id1,STRbind(_)),_),((id2,STRbind(_)),_)) =
      Name(id1) > Name(id2)
  | binderGt(((_,STRbind(_)),_),_) = true
  | binderGt(((id1,bind1),_),((id2,bind2),_)) = Impossible "4344 in envaccess"

fun build (iter: (unit -> binder * info) -> 'a) (s:marker) : 'a =
    let val sorttree = BinSort.mkSortTree()
     in if !debugBuild then prstr "build -- calling collectTable\n" else ();
	collectTable(s,BinSort.insert(binderGt,sorttree));
        iter(BinSort.generator sorttree)
    end

fun extendPath(LVAR(v): access, []: path) = [v]  (* locally defined *)
  | extendPath(SLOT(n), p) = n::p  (* element of opened structure *)
  | extendPath(PATH(p), _) = p  (* defined exception *)
  | extendPath(INLINE(n), _) = [n,0]  (* inline primitive *)
  | extendPath(access,path) =
      (PrintBasics.printAccess access; PrintBasics.printPath path;
       Impossible "extendPath" )

fun iterStr (gen : unit -> binder * info) :  path list * symtable =
    let val newtable = Table.new()
	val _ = if !debugBuild then prstr "entering iterStr\n" else ()
	fun fill (count) =
	    let val element as ((sym,binding),_) = gen()
	     in if !debugBuild
		  then (prstr "adding :"; printSym sym; prstr " ";
		        printBinding binding; newline())
	          else ();
		(case element
		  of ((id, VARbind(var as VALvar{access,...})),(path,ctx)) =>
		       (Table.add(newtable,
				  (id,VARbind(varInStr(var,ctx,count))));
			extendPath(access,path)::fill(count+1))
(*		   | ((id, EXNbind(exn as DATACON{rep=ref(VARIABLE(access)),...})),
		      (path,ctx)) =>
		       (Table.add(newtable,
				  (id,EXNbind(dconInStr(exn,ctx,count))));
			extendPath(access,path)::fill(count+1))
*)		   | ((id, CONbind(exn as DATACON{rep=ref(VARIABLE(access)),...})),
		      (path,ctx)) =>
		       (Table.add(newtable,
				  (id,CONbind(dconInStr(exn,ctx,count))));
			extendPath(access,path)::fill(count+1))
		   | ((id, STRbind(strVar as STRvar{access,...})),(path,ctx)) =>
		       (Table.add(newtable,
				  (id,STRbind(strInStr(strVar,ctx,count))));
			extendPath(access,path)::fill(count+1))
		   | ((id, TYCbind(tyconRef)),(_,ctx)) =>
		       (Table.add(newtable, (id,TYCbind(tyconInCtx(tyconRef,ctx))));
			fill(count))
		   | ((id, CONbind(dcon)),(path,ctx)) =>
		       (Table.add(newtable, (id,CONbind(dconInStr(dcon,ctx,0))));
			fill(count))
		   | (binding,_) => (Table.add(newtable,binding); fill(count)))
	    end
	    handlex BinSort.finished => []
     in (fill(0), newtable)
    end

val buildStrTable = build iterStr;

fun iterSig (gen : unit -> binder * info) : binding list * symtable =
    let val newtable = Table.new()
	fun fill (count) =
	     (case gen()
	       of ((id, VARbind(var)),_) =>
	            let val vb = VARbind(varInSig(var,count))
		     in Table.add(newtable,(id,vb));
		        vb::fill(count+1)
		    end
(*	        | ((id, EXNbind(exn)),_) =>
		    let val eb = EXNbind(exnInSig(exn,count))
		     in Table.add(newtable,(id,eb));
			eb::fill(count+1)
		    end
*)	        | ((id, CONbind(exn as DATACON{rep=ref(VARIABLE _),...})),_) =>
		    let val eb = CONbind(exnInSig(exn,count))
		     in Table.add(newtable,(id,eb));
			eb::fill(count+1)
		    end
	        | ((id, STRbind(strVar)),_) =>
		    let val sb = STRbind(strInSig(strVar,count))
	             in Table.add(newtable,(id,sb));
			sb::fill(count+1)
		    end
	        | (binder as (_, binding),_) =>
	            (Table.add(newtable,binder);
	             binding::fill(count)))
	     handlex BinSort.finished => []
     in (fill(0),newtable)
    end

val buildSigTable = build iterSig;

(* reset state of EnvAccess *)
fun reset() =
    (varPatchList := nil;
     boundTyvars := CLOSEDbtv nil)

end (* local *)

end (* functor EnvAccess *)

