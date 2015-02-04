(* envaccess.sml *)

(* lookup and binding functions *)

structure EnvAccess : ENVACCESS = struct

structure Access = Access
structure Basics = Basics
structure Env = Env

open ErrorMsg PrintUtil Access Basics Basics.Symbol BasicTypes TypesUtil Env

val debugBind = System.Control.debugBind

fun openStructureVar(STRvar{access=PATH p,binding,...}) =
    (case binding
      of STRstr{table,env,...} => openOld({path=p,strenv=env},table)
       | INDstr _ => impossible "EnvAccess.openStructureVar -- INDstr arg")
  | openStructureVar _ = impossible "EnvAccess.openStructureVar -- bad access value"

val bogusID = Symbol.symbol "bogus"

val bogusStrStamp = genStrStamp()

local val b = mkSTR([bogusID], newTable(), emptyStrenv)
 in val bogusSTR = STRvar{name=bogusID, access=PATH[0], binding=b}
    val bogusSTR' = STRvar{name=bogusID, access=SLOT 0, binding=b}
end

fun varIndex(id: symbol) = number(id)*namespaces
fun conIndex(id: symbol) = number(id)*namespaces
fun tycIndex(id: symbol) = number(id)*namespaces+1
fun tyvIndex(id: symbol) = number(id)*namespaces+2
fun sigIndex(id: symbol) = number(id)*namespaces+3
fun strIndex(id: symbol) = number(id)*namespaces+4
fun fctIndex(id: symbol) = number(id)*namespaces+5
fun fixIndex(id: symbol) = number(id)*namespaces+6

fun varKey(id: symbol) = (number(id)*namespaces, name(id))
fun conKey(id: symbol) = (number(id)*namespaces, name(id))
fun tycKey(id: symbol) = (number(id)*namespaces+1, name(id))
fun tyvKey(id: symbol) = (number(id)*namespaces+2, name(id))
fun sigKey(id: symbol) = (number(id)*namespaces+3, name(id))
fun strKey(id: symbol) = (number(id)*namespaces+4, name(id))
fun fctKey(id: symbol) = (number(id)*namespaces+5, name(id))
fun fixKey(id: symbol) = (number(id)*namespaces+6, name(id))

fun key(namespace:int, id:symbol) = 
    let val name = name id
     in (namespaces*(number id)+namespace, name)
    end


(* type constructors *)

val bogusTyc = mkDEFtyc([bogusID],TYFUN{arity=0,body=ERRORty},YES)

fun lookTYCinTable(table,id) =
    let val TYCbind tycref = IntStrMap.map table (tycKey id)
     in tycref
    end

fun lookTYCinStr(STRstr{table,env,stamp,...}: Structure, id: symbol) : tycon ref =
    ((case lookTYCinTable(table,id)
	of ref(INDtyc [i]) => let val {t,s} = env in ref(t sub i) end
	   (* paths of length > 1 should not occur in type bindings *)
	 | tyc => tyc)
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else complain("unbound type in structure: " ^ Symbol.name id);
	 ref bogusTyc))
  | lookTYCinStr _ = impossible "EnvAccess.lookTYC.inStr"

fun lookTYC' look (id:symbol) =
    case look(tycKey(id))
      of (TYCbind(tycref as ref(INDtyc[i])), {strenv={t,s},path}) =>
	    (ref(t sub i) 
	     handle Subscript => impossible "EnvAccess.lookTYC'")
       | (TYCbind tycref, _) => tycref
       | _ => impossible "EnvAccess.lookTYC'"

val lookTYC = lookTYC' look
val lookTYClocal = lookTYC' lookStrLocal

(* addzeros also defined in Parse *)
fun addzeros(0,l) = l
  | addzeros(n,l) = addzeros(n-1,0::l)

fun ibindTYC(index: int, s: string, tc: tycon ref) = add(index,s,TYCbind tc)
fun bindTYC(id: symbol, tc: tycon ref) =
    add(tycIndex id, name id, TYCbind tc)


(* tycon lookup with arity checking *)

fun checkArity(tycon, arity) =
    if tyconArity(tycon) <> arity
    then complain("type constructor "^(Symbol.name(tycName(tycon)))^
	          " has wrong number of arguments: "^makestring arity)
    else ()

fun lookArTYC0(id,arity) =
    let val tycref as ref tyc = lookTYC id
     in checkArity(tyc,arity);
        tycref
    end
    handle Unbound => 
      (complain("unbound type constructor: " ^ Symbol.name id);
       ref bogusTyc)

fun lookArTYCinSig (depth: int) (id: symbol, arity: int) =
    case look(tycKey id)
      of (TYCbind(tycref as ref(INDtyc[i])), {strenv={t,s},path=h::_}) =>
	   if h >= 0
	   then let val tyc = t sub i
		 in checkArity(tyc,arity);
		    ref tyc
		end
	   else (checkArity(t sub i, arity);
		 if depth+h = 0
		 then tycref
		 else ref(INDtyc(addzeros(depth+h,[i]))))
       | (TYCbind tycref, _) => (checkArity(!tycref,arity); tycref)
       | _ => impossible "EnvAccess.lookTYCinSig"

val lookArTYC : (symbol * int -> tycon ref) ref = ref lookArTYC0


(* patching type constructor references in datatype declarations *)

fun protectDb () =
    let val patchList : tycon ref list ref = ref []
	val savedLook = !lookArTYC
	fun localLook(id,ary) =
	    let val tycref = ref (mkUNDEFtyc(id,ary))
	     in patchList := tycref :: !patchList;
		tycref
	    end
	fun patch (tc::l) =
	    let val ref(TYCON{path=id::_,arity,kind=UNDEFtyc newpath,...}) = tc
	     in let val ref tycon = !lookArTYC(id,arity)
	         in tc := case newpath
			   of NONE => tycon
			    | SOME path => setTycPath(path,tycon)
	        end
		handle Unbound =>
		  complain("unbound type constructor (in datatype): " ^
		           Symbol.name id);
	        patch l
	    end
	  | patch nil = ()
     in ((fn () => lookArTYC := localLook),
         (fn () => (lookArTYC := savedLook; patch(!patchList))))
    end

(* constructors *)

fun dconApplied(DATACON{name,const,typ,rep,sign},{path,strenv}:info) : datacon =
    DATACON{name = name, const = const, sign=sign,
            rep = (case rep
		     of VARIABLE(SLOT n) => VARIABLE(PATH(n::path))
		      | VARIABLE(LVAR v) => VARIABLE(PATH [v])
		      | _ => rep),  (* nonexception datacon *)
            typ = typeInContext(typ,strenv)}

fun lookCONinTable(table,id) = 
    case IntStrMap.map table (varKey(id))
      of CONbind c => c
       | _ => raise UnboundTable

fun lookCON' lookfn id =
    case lookfn(varKey(id))
      of (CONbind c,info) => dconApplied(c,info)
       | _ => raise Unbound

val lookCON = lookCON' look
val lookCONlocal = lookCON' lookStrLocal

val bogusCON = DATACON{name=bogusID,const=true,typ=ERRORty,
		       rep=UNDECIDED,sign=[]}

fun lookCONinStr(STRstr{table,env,stamp,...},id,ap): datacon =
    (dconApplied(lookCONinTable(table,id),{path=ap,strenv=env})
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else complain("unbound constructor in structure: " ^ Symbol.name id);
	 bogusCON))
  | lookCONinStr _ = impossible "EnvAccess.lookCONinStr"

fun ibindCON (index: int, s: string, c: datacon) = add(index,s,CONbind c)
fun bindCON (id: symbol, c: datacon) = 
    add(conIndex id, name id, CONbind c)


(* variables *)

fun unboundVAR id = (complain ("unbound variable " ^ name id);
		     VARbind(mkVALvar(id, ref(VARty(mkTyvar defaultMETA)))))

fun varApplied(v: var, {path, strenv}: info) : var =
    case v
      of VALvar{access,name,vtype} =>
	   VALvar{access =
		    (case access
		       of SLOT(n) => PATH(n::path)
			| LVAR(n) => PATH([n])
			| INLINE _ => access
			| PATH _ => impossible "varApplied: access = PATH"),
		  vtype = 
		    if Prim.special(access)
		    then ref(!vtype)
		    else (case path
			   of [] => vtype
			    | _ => ref(typeInContext(!vtype,strenv))),
		  name = name}
       | _ => v

fun lookVARinTable(table, id) =
    case IntStrMap.map table (varKey id)
      of VARbind v => v
       | _ => raise UnboundTable

fun lookVARCONinTable(table,id) = IntStrMap.map table (varKey id)

fun lookVARCONinStr(STRstr{table,env,stamp,...},id,ap): binding =
    ((case lookVARCONinTable(table,id)
       of VARbind(var) => VARbind(varApplied(var,{path=ap,strenv=env}))
	| CONbind(dcon) => CONbind(dconApplied(dcon,{path=ap,strenv=env}))
	| _ => impossible "EnvAccess.lookVARCONinSTR")
     handle UnboundTable =>
	(if stamp=bogusStrStamp then ()
	 else complain("unbound variable or constructor in structure: "
		       ^ Symbol.name id);
	 CONbind bogusCON))
  | lookVARCONinStr _ = impossible "EnvAccess.lookVARCONinStr"

fun lookVARCON id = 
    case lookRec(varKey id)
      of LOCAL(VARbind v, info) => VARbind(varApplied(v,info))
       | LOCAL(CONbind d, info) => CONbind(dconApplied(d,info))
       | GLOBAL(CONbind d, info) => CONbind(dconApplied(d,info))
       | GLOBAL(VARbind _, _) => raise Unboundrec
       | _ => impossible "EnvAccess.lookVARCON"

fun lookVARCONlocal id = 
    case lookStrLocal(varKey id)
      of (VARbind v, info) => VARbind(varApplied(v,info))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => impossible "EnvAccess.lookVARCON"

fun lookVARRecLocal id = 
    case lookRecLocal(varKey id)
      of (VARbind v, info) => varApplied(v,info)
       | _ => impossible "EnvAccess.lookVARRecLocal"

(* patching deferred variables *)

val varPatchList : var ref list ref = ref nil

fun getPatchVar id =
    let val v = ref (UNKNOWNvar id)
     in varPatchList := v :: !varPatchList;
	v
    end

exception Patched

fun patchVars (pl as (varRef as ref(UNKNOWNvar id))::pl', tl) =
     ((varRef := lookVARRecLocal id; raise Patched)
       handle Unboundrec => 
		 patchVars(pl',varRef::tl)  (* not yet bound; try later *)
	    | Unbound => (* no more rec layers *)
		let val VARbind v = unboundVAR id
		 in varRef := v; patchVars(pl',tl)
		end
	    | Patched => patchVars(pl', tl))
  | patchVars (nil, tl) = tl
  | patchVars _ = impossible "EnvAccess.patchVars"

val protectPatchList =
    ((fn () => !varPatchList before (varPatchList := nil)),
     (fn (vpl) => varPatchList := patchVars(!varPatchList,vpl)))
	 (* bug -- exit function only works right for normal exit from protect *)

fun capitalized string =
    (* string starts with a capital letter *)
    let val firstchar = ordof(string,0)
     in firstchar >= Ascii.uc_a andalso firstchar <= Ascii.uc_z
    end

(* Could be used to enforce the Capitalization convention, but isn't *)
fun checkBinding(id: symbol,_) =
    if capitalized(Symbol.name id)
    then warn("Capitalized variable in rule: "^ Symbol.name id)
    else ()


fun newVAR(bl: (symbol * var) list ref, id: symbol) : var =
    let fun checkid ((i,b)::bl) =
	      if Symbol.eq(i,id)
	        then complain "repeated var in pattern"
	        else checkid bl
	  | checkid nil = ()
     in checkid(!bl);
        let val v = mkVALvar(id,ref UNDEFty)
	 in bl := (id, v) :: !bl;
	    v
        end
    end

fun ibindVAR(index: int, s: string, v: var) = add(index, s, VARbind v)
fun bindVAR(id: symbol, v: var) = add(varIndex id, name id, VARbind v)

fun bindVARs(binders: (symbol * var) list) =
    app (fn b as (id,bind) =>
	    (if !debugBind
	     then (print "bindVARs: "; printSym id; newline())
	     else ();
	     bindVAR b))
	binders


(* type variables *)

datatype mode = EXP | TYPEDEC

val tyvarsMode = ref(EXP)
val boundTyvars = ref([]:tyvar list)

fun protectTyvars NONE = 
    ((fn () => (!boundTyvars before (boundTyvars := []))),
     (fn btv => boundTyvars := btv))
  | protectTyvars (SOME tvs) = 
    ((fn () => (!boundTyvars before (boundTyvars := tvs; tyvarsMode := TYPEDEC))),
     (fn btv => (boundTyvars := btv; tyvarsMode := EXP)))

fun currentTyvars () = !boundTyvars

fun lookTYV id = 
    let val (TYVbind tyv, _) = lookStrLocal(tyvKey id) in tyv end

fun lookTyvar (id: symbol) =
    case !tyvarsMode
      of TYPEDEC =>
	   let fun find ((tv as ref(UBOUND{name=id',...}))::resttvs) =
		   if Symbol.eq(id,id')
		      then tv
		      else find(resttvs)
		 | find([]) =
		    (complain "lookTyvar -- unbound tyvar in closed scope";
		     mkTyvar(INSTANTIATED UNDEFty))
		 | find _ = impossible "EnvAccess.lookTyvar.find"
	    in find(!boundTyvars)
	   end
       | EXP =>
	   lookTYV id
	   handle Unbound =>  (* here we could check for weakness > 0 *)
	     let val tyv = mkTyvar(mkUBOUND id)
	      in add(tyvIndex id, name id, TYVbind tyv);
		 boundTyvars := tyv :: !boundTyvars;
		 tyv
	     end;


(* exceptions *)

fun notInitialLowerCase string =
    (* string does NOT start with lower-case alpha *)
    let val firstchar = ordof(string,0)
     in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
    end

(* Could be used to enforce the Capitalization convention *)
fun looksLikeExn sym = notInitialLowerCase(Symbol.name sym)

fun unboundEXN id =
    (complain("unbound exn: " ^ name id); bogusCON)

fun lookEXNinStr(STRstr{table,env,stamp,...},id,ap) =
    (dconApplied(lookCONinTable(table,id),{path=ap,strenv=env})
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else complain("unbound exception in path: " ^ Symbol.name id);
	 bogusCON))
  | lookEXNinStr _ = impossible "EnvAccess.lookEXNinStr"


(* signatures *)

val bogusSIGbody = 
    STRstr{stamp=genStrStamp(),
           sign=genSigStamp(),
           table=newTable(),
	   env=emptyStrenv,
	   kind=SIGkind{share={s=nil,t=nil},
		        bindings=nil,stampcounts={s=0,t=0}}}
val bogusSIG=SIGvar{name=bogusID,binding=bogusSIGbody}

fun lookSIG id = 
    let val (SIGbind sign,_) = look(sigKey id)
     in sign
    end
    handle Unbound => (complain("unbound signature: " ^ name id); bogusSIG)

fun bindSIG(id: symbol, s: signatureVar) = add(sigIndex id, name id, SIGbind s)


(* structures *)

fun strApplied(STRvar{name,access,binding},{path=ap,strenv={s,t}}) =
    STRvar{name=name,
	   binding=(case binding of INDstr i => s sub i | _ => binding),
	   access=(case access
		     of SLOT(n) => PATH(n::ap)
		      | LVAR(n) => PATH [n]
		      | _ => impossible "strApplied: access = PATH or INLINE")}

fun lookSTRinTable(table, id) = 
    let val STRbind strvar = IntStrMap.map table (strKey id) in strvar end

fun lookSTR0 id = 
    let val (STRbind str, info) = look(strKey id)
     in (str,info)
    end

fun lookSTR' look id =
    let val (STRbind str, info) = look(strKey id)
     in strApplied(str,info)
    end
val lookSTR = lookSTR' look
val lookSTRlocal = lookSTR' lookStrLocal

fun lookSTRinStr(STRstr{table,env,stamp,...},id,ap) =
    (strApplied(lookSTRinTable(table,id),{path=ap,strenv=env})
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else complain("unbound structure in path: " ^ Symbol.name id);
	 bogusSTR))
  | lookSTRinStr _ = impossible "EnvAccess.lookSTRinStr"

fun ibindSTR(index: int, s: string, strvar: structureVar) =
    add(index, s, STRbind strvar)
fun bindSTR(id: symbol, strvar: structureVar) =
    add(strIndex id, name id, STRbind strvar)


(* functors *)

val bogusFCT = FCTvar{name=bogusID, access=PATH[0],
		     binding=FUNCTOR{paramName=bogusID,
					param= bogusSIGbody,
					body= bogusSIGbody,
					tycCount=0}}

fun lookFCT id = 
    let val (FCTbind fv,_) = look(fctKey id) in fv end 
    handle Unbound =>
      (complain("unbound functor identifier: " ^ Symbol.name id);
	bogusFCT)

fun bindFCT(id: symbol, f: functorVar) = add(fctIndex id, name id, FCTbind f)


(* fixity bindings *)

fun lookFIX id = 
    if true (* !(Symbol.infixed id) *)
    then let val (FIXbind(FIXvar{binding,...}),_) = look(fixKey id)
	  in binding
	 end
	 handle Unbound => ((* Symbol.infixed id := false; *) NONfix)
    else NONfix

fun ibindFIX(index: int, s: string, f: fixityVar) = add(index, s, FIXbind f)
fun bindFIX(id: symbol, f: fixityVar) = 
    (add(fixIndex id, name id, FIXbind f) (* ;Symbol.infixed id := true *) )


(* lookup using symbolic path *)
fun lookPathinStr(str: Structure, ap: Access.path, spath as _::rest : symbol list,
		  lookLast: Structure * symbol * Access.path -> 'a) : 'a =
    let fun getStr([id],str,ap) = lookLast(str,id,ap)
	  | getStr(id::rest,STRstr{table,stamp,env={s,...},...},ap) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTRinTable(table,id)
		      handle UnboundTable => 
			(if stamp=bogusStrStamp then ()
		         else (complain("unbound intermediate structure: "
				        ^ name id);
		               print "  in path: ";
			       printSequence "." printSym spath;
		               newline());
		         bogusSTR')
	       in getStr(rest,
		  	 (case binding of INDstr i => s sub i | _ => binding),
			 n::ap)
	      end
	  | getStr _ = impossible "EnvAccess.lookPathinStr.getStr"
     in getStr(rest,str,ap)
    end
  | lookPathinStr _ = impossible "EnvAccess.lookPathinStr"

fun lookPath(spath as first::rest,
             lookLast: Structure * symbol * Access.path -> 'a) : 'a =
    let	val STRvar{access=PATH(ap),binding,...} =
	      lookSTR first
	      handle Unbound => 
	        (complain("unbound head structure: " ^ name first);
		 print "  in path: "; printSequence "." printSym spath;
		 newline();
		 bogusSTR)
     in lookPathinStr(binding,ap,spath,lookLast)
    end
  | lookPath _ = impossible "EnvAccess.lookPath"


fun lookPathArTYC0 (path: symbol list, arity: int) =
    let val tycref as ref tyc = lookPath(path, fn(str,id,_) => lookTYCinStr(str,id))
     in checkArity(tyc,arity);
	tycref
    end

fun lookPathArTYCinSig (depth: int) (spath as first::rest, arity) : tycon ref =
    let	fun complainUnbound() =
	    (complain "unbound type constructor in signature";
	     print "  name: "; printSequence "." printSym spath;
	     newline();
	     raise Syntax)
	fun get([id],STRstr{table,env={t,...},...}) = 
	     (let val ref(INDtyc(p as [i])) =
		      lookTYCinTable(table,id)
	       in checkArity(t sub i, arity);
		  p
	      end
	      handle UnboundTable => complainUnbound())
	  | get(id::rest,STRstr{table,env={s,...},...}) =
	      let val STRvar{binding=INDstr k,...} =
			lookSTRinTable(table,id)
			handle UnboundTable => complainUnbound()
	       in k::get(rest, s sub k)
	      end
	  | get _ = impossible "Parse.sign.sgn.lookPathArTYC.get"
	val leadStr = lookSTR0 first
		      handle Unbound => complainUnbound()
	fun lookInStr(str) =
	    let val tycref = 
		    lookPathinStr(str, [], spath,
			 (fn(str,id,_) => lookTYCinStr(str,id)))
	     in checkArity(!tycref,arity);
		tycref
	    end
     in case leadStr
	  of (STRvar{binding=INDstr i,...},{path=h::_,strenv={s,...}}) =>
	      if h < 0 (* indicates signature component *)
	      then ref(INDtyc(addzeros(depth+h,i::get(rest, s sub i))))
	      else lookInStr(s sub i)
	   | (STRvar{binding,...},_) => lookInStr binding
    end

val lookPathArTYC = ref lookPathArTYC0


(* functions to collect stale lvars for unbinding *)
exception LOOKLVAR

fun lookLvar (env: env) (key: int * string) =
    case lookEnv(env,key)
      of (VARbind(VALvar{access=LVAR v,...}),_) => v
       | (STRbind(STRvar{access=LVAR v,...}),_) => v
       | (FCTbind(FCTvar{access=LVAR v,...}),_) => v
       | _ => raise LOOKLVAR

fun runbound index =
    case index mod namespaces
      of 0 => true  (* var *)
       | 4 => true  (* structure *)
       | 5 => true  (* functor *)
       | _ => false

fun staleLvars(newenv,oldenv) : int list =
    let val lvarset = ref([] : int list)
	val get = lookLvar oldenv
        fun collect (i,s,_) = 
	    if runbound i
	    then (lvarset := get(i,s) :: !lvarset)
		 handle LOOKLVAR => ()
		      | Unbound => ()
	    else ()
     in appenv collect (newenv,oldenv);
        !lvarset
    end


(* building structures *)

(* comparing by bound symbol for runtime components *)
fun binderGt(bind1: binder, bind2: binder) =
    case (bind1,bind2)
      of ((ind1,_,FIXbind(_)),(ind2,_,FIXbind(_))) => ind1 > ind2
       | ((_,_,FIXbind(_)),_) => true
       | (_,(_,_,FIXbind(_))) => false
       | ((_,n1,VARbind(_)),(_,n2,VARbind(_))) => n1 > n2
       | ((_,_,VARbind(_)),_) => true
       | (_,(_,_,VARbind(_))) => false
       | ((_,n1,CONbind(_)),(_,n2,CONbind(_))) => n1 > n2
       | ((_,_,CONbind(_)),_) => true
       | (_,(_,_,CONbind(_))) => false
       | ((ind1,_,TYCbind(_)),(ind2,_,TYCbind(_))) => ind1 > ind2
       | ((_,_,TYCbind(_)),_) => true
       | (_,(_,_,TYCbind(_))) => false
       | ((_,n1,STRbind(_)),(_,n2,STRbind(_))) => n1 > n2
       | ((_,_,STRbind(_)),_) => true
       | (_,(_,_,STRbind(_))) => false
       | ((ind1,_,FCTbind(_)),(ind2,_,FCTbind(_))) => ind1 > ind2
       | ((_,_,FCTbind(_)),_) => true
       | (_,(_,_,FCTbind(_))) => false
       | ((ind1,_,SIGbind(_)),(ind2,_,SIGbind(_))) => ind1 > ind2
       | _ => impossible "EnvAccess.binderGt"

fun build (iter: (unit -> binder * info) -> 'a) () : 'a =
    let val sorttree = BinSort.mkSortTree():
		        (binder * info) BinSort.tree ref
	fun gt((binder1,_),(binder2,_)) = binderGt(binder1,binder2)
     in collectTable(BinSort.insert(gt,sorttree));
        iter(BinSort.generator(!sorttree))
    end

fun extendPath(LVAR(v): access, []: path) = PATH[v] (* locally defined *)
  | extendPath(SLOT(n), p) = PATH(n::p)  (* element of opened structure *)
  | extendPath(x as PATH _, _) = x  (* defined exception *)
  | extendPath(x as INLINE _, _) = x
  | extendPath(access,path) = impossible "extendPath in envaccess"

fun dconInStr(dc as DATACON{name,const,typ,rep,sign},env,slotNo) : datacon =
    DATACON{name = name, const = const, sign = sign,
            rep = (case rep
		     of VARIABLE(access) => VARIABLE(SLOT slotNo)
		      | _ => rep),
            typ = typeInContext(typ,env)}

fun iterStr (gen : unit -> binder * info)
	     : trans list * symtable =
    let val newtable = newTable()
	fun fill (count) =
	    (case gen()
	      of ((i, s, VARbind(var as VALvar{access,name,vtype})),
		  {path,strenv}) =>
		   (IntStrMap.add newtable
		     (i,s,
		        VARbind(
			  case access
			   of INLINE(_) => var
			    | _ =>
				VALvar{access = SLOT count,
				       vtype = ref(typeInContext(!vtype,strenv)),
				       name = name}));
		    VALtrans(extendPath(access,path))::fill(count+1))
	       | ((i, s, CONbind(exn as DATACON{rep=VARIABLE(access),...})),
		  {path,strenv}) =>
		    (IntStrMap.add newtable 
		       (i,s,CONbind(dconInStr(exn,strenv,count)));
		     VALtrans(extendPath(access,path))::fill(count+1))
	       | ((i, n, STRbind(STRvar{name,access,binding})),
	          {path,strenv={s,...}}) =>
		    (IntStrMap.add newtable
		     (i,n,STRbind(STRvar{name=name,
					binding=(case binding
						   of INDstr i => s sub i
						    | _ => binding),
					access=SLOT(count)}));
		    VALtrans(extendPath(access,path))::fill(count+1))
	       | ((i, s, TYCbind(tyconRef)),{strenv,...}) =>
		   (IntStrMap.add newtable
		      (i,s,TYCbind(ref(tyconInContext strenv (!tyconRef))));
		    fill(count))
	       | ((i, s, CONbind(dcon)),{strenv,...}) =>
		   (IntStrMap.add newtable (i,s,CONbind(dconInStr(dcon,strenv,0)));
		    fill(count))
	       | (binding,_) => (IntStrMap.add newtable binding; fill(count)))
	    handle BinSort.Finished => []
     in (fill(0), newtable)
    end

val buildStrTable = build iterStr;

fun iterSig (gen : unit -> binder * info)
	    : binding list * symtable =
    let val newtable = newTable()
	fun fill (count) =
	     (case gen()
	       of ((i, s,VARbind(VALvar{name,vtype,...})),_) =>
	            let val vb = VARbind(VALvar{access=SLOT(count),
				 		name=name,vtype=vtype})
		     in IntStrMap.add newtable (i,s,vb);
		        vb::fill(count+1)
		    end
	        | ((i,s,CONbind(DATACON{name,const,typ,sign,rep=VARIABLE _})),_) =>
		    let val eb = CONbind(DATACON{name = name, const = const,
					         typ = typ, sign = sign,
					         rep = VARIABLE(SLOT count)})
		     in IntStrMap.add newtable (i,s,eb);
			eb::fill(count+1)
		    end
	        | ((i,s,STRbind(STRvar{name,access,binding})),_) =>
		    let val sb = STRbind(STRvar{name=name,
				                binding=binding,
						access=SLOT(count)})
	             in IntStrMap.add newtable (i,s,sb);
			sb::fill(count+1)
		    end
	        | (binder as (_,_,FIXbind _),_) =>
	            (IntStrMap.add newtable binder;
	             fill(count))  (* put infix bindings in table only *)
	        | (binder as (_,_,binding),_) =>
	            (IntStrMap.add newtable binder;
	             binding::fill(count)))
	     handle BinSort.Finished => []
     in (fill(0),newtable)
    end

val buildSigTable = build iterSig;

val maxStrComps = 100
and maxTypeComps = 100

fun iterFct(gen : unit -> binder * info)
	   : trans list * symtable * strenv =
    let val newtable = newTable()
	val strComps = array(maxStrComps,INDstr(~1))
	and tycComps = array(maxTypeComps,INDtyc[])
	and strCount = ref 0
	and tycCount = ref 0
	local open Intmap
	      exception StampMap
	      val smap = new(32, StampMap) : int list intmap
	 in val addStamp = add smap
	    val mapStamp = map smap
	end
	exception Fail
	fun search(stamp,tyc) =
	    let fun search0(senv,scount,tenv,tcount,path) =
		    let fun tsearch i =
			    if i >= tcount
			    then raise Fail
			    else if tycStamp(tenv sub i)=stamp
			    then let val p = rev(i::path)
				  in addStamp(stamp,p);
				     p
				 end
			    else tsearch(i+1)
			fun ssearch i =
			    if i >= scount
			    then raise Fail
			    else let val STRstr{env={s,t},...} = senv sub i
				  in search0(s, Array.length s,
					     t, Array.length t, i::path)
				     handle Fail =>
				       ssearch(i+1)
				 end
		     in tsearch 0
			handle Fail =>
			  ssearch 0
		    end
	     in search0(strComps,!strCount,tycComps,!tycCount,[])
		handle Fail =>
		  let val path = [!tycCount]
		   in update(tycComps,!tycCount,tyc);
		      inc tycCount;
		      addStamp(stamp,path);
		      path
		  end
	    end
	fun abstractType(ty) =
	     case ty
	       of VARty(ref(INSTANTIATED ty')) => abstractType ty'
	        | FLEXRECORDty(ref(CLOSED ty')) => abstractType ty'
	        | CONty(tycref as ref tyc,args) =>
		   (case tyc
		      of INDtyc _ => () (* already been done *)
	               | TYCON{stamp,...} =>
		           if fixedStamp stamp then ()
			   else tycref := INDtyc(mapStamp stamp
						 handle StampMap =>
						   search(stamp,tyc));
	            app abstractType args)
	        | POLYty{tyfun=TYFUN{body,...},...} => abstractType body
	        | VARty _ => ()
		| _ => impossible "abstractType"
	fun fill (count) =
	     (case gen()
		of ((i,n,STRbind(STRvar{name,access,binding})),
		    {path,strenv={s,t}}) =>
		     (let val str as STRstr{stamp,...} = 
			      case binding
				of INDstr i => s sub i
				 | _ => binding
		       in if fixedStamp stamp
			  then IntStrMap.add newtable
				 (i,n,STRbind(STRvar{name=name,access=SLOT count,
						       binding=str}))
			  else (IntStrMap.add newtable
				 (i,n,STRbind(STRvar{name=name,access=SLOT count,
						       binding=INDstr(!strCount)}));
				update(strComps,!strCount,str);
				inc strCount)
		      end;
		      VALtrans(extendPath(access,path))::fill(count+1))
		 | ((i,n,TYCbind(ref tyc)),{strenv,...}) =>
		     (let val tyc = tyconInContext strenv tyc
		       in if fixedStamp(tycStamp tyc)
			  then IntStrMap.add newtable (i,n,TYCbind(ref tyc))
			  else (IntStrMap.add newtable
					  (i,n,TYCbind(ref(INDtyc[!tycCount])));
				update(tycComps,!tycCount,tyc);
				inc tycCount)
		      end;
		      fill(count))
		 | ((i,n,VARbind(var as VALvar{access,name,vtype})),
		    {path,strenv}) =>
		     (IntStrMap.add newtable
		       (i,n,VARbind(case access
				     of INLINE _ => 
					  var  (* ground type *)
				      | _ => let val typ =
						     typeInContext(!vtype,strenv)
					      in abstractType(typ);
						 VALvar{access=SLOT count,
							name=name,
							vtype=ref typ}
					     end));
		      VALtrans(extendPath(access,path))::fill(count+1))
		 | ((i,n,CONbind(DATACON{name,const,typ,rep,sign})),
		    {path,strenv}) =>
		    (let val typ = typeInContext(typ,strenv)
		      in abstractType(typ);
			 IntStrMap.add newtable
			  (i,n,CONbind(DATACON{name=name,const=const,sign=sign,
					      typ = typ,
					      rep = case rep
						      of VARIABLE(_) =>
							   VARIABLE(SLOT count)
						       | _ => rep}))
		     end;
		     case rep
		       of VARIABLE(access) =>
			    VALtrans(extendPath(access,path))::fill(count+1)
			| _ => fill(count))
		 | (binding,_) => (IntStrMap.add newtable binding; fill(count)))
	    handle BinSort.Finished => []
     in (fill(0), newtable,
	 {s=ArrayExt.copy(strComps,!strCount),
	  t=ArrayExt.copy(tycComps,!tycCount)})
    end

val buildFctTable = build iterFct

(* info extracted from Core structure *)
val exnBind = ref(bogusCON)
val exnMatch = ref(bogusCON)
val stringequalPath = ref[0]
val polyequalPath = ref[0]
val currentPath = ref[0]
val toplevelPath = ref[0]
val getDebugVar = ref(mkVALvar(Symbol.symbol "getDebug",
		               ref(VARty(mkTyvar defaultMETA))))

fun setCore(STRvar{access=PATH p,binding,...}) =
    let fun extractPath name = 
	    let val VARbind(VALvar{access=PATH p,...}) =
		      lookVARCONinStr(binding, Symbol.symbol name, p)
	     in p
	    end
     in exnBind := lookCONinStr(binding, Symbol.symbol "Bind", p);
	exnMatch := lookCONinStr(binding, Symbol.symbol "Match", p);
	stringequalPath := extractPath "stringequal";
	polyequalPath := extractPath "polyequal";
	currentPath := extractPath "current";
	toplevelPath := extractPath "toplevel";
        getDebugVar := let val VARbind x = lookVARCONinStr(binding,
				    Symbol.symbol "getDebug", p)
			in x
		       end
    end
  | setCore _ = impossible "EnvAccess.setCore"

(* reset state of EnvAccess *)
fun reset() =
    (varPatchList := nil;
     boundTyvars := [];
     tyvarsMode := EXP)

end (* structure EnvAccess *)

