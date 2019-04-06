(* Copyright 1989 by AT&T Bell Laboratories *)
structure EnvAccess : ENVACCESS = struct
(* lookup and binding functions *)

structure Access = Access
structure Basics = Basics
structure Env = Env

open ErrorMsg PrintUtil Access Basics Basics.Symbol BasicTypes TypesUtil Env
     NameSpace

val debugBind = System.Control.debugBind

fun openStructureVar(STRvar{access=PATH p,binding,...}) =
    (case binding
      of STRstr{table,env,...} => openOld({path=p,strenv=env},table)
       | INDstr _ => impossible "EnvAccess.openStructureVar -- INDstr arg"
       | SHRstr _ => impossible "EnvAccess.openStructureVar -- SHRstr arg"
       | NULLstr => impossible "EnvAccess.openStructureVar -- NULLstr arg")
  | openStructureVar _ = impossible "EnvAccess.openStructureVar -- bad access value"

val bogusID = Symbol.symbol "bogus"

val bogusStrStamp = Stampset.newStamp(Stampset.fixedStrStamps)

local val b = STRstr{stamp=bogusStrStamp, sign=0, table=newTable(), env=DIR,
		     kind=STRkind{path=[bogusID]}}
 in val bogusSTR = STRvar{name=[bogusID], access=PATH[0], binding=b}
    val bogusSTR' = STRvar{name=[bogusID], access=SLOT 0, binding=b}
end

(* type constructors *)

val bogusTyc = mkDEFtyc([bogusID],TYFUN{arity=0,body=ERRORty},YES,Stampset.globalStamps)

fun lookTYCinTable(table,id) =
    let val TYCbind tycref = IntStrMap.map table (tycKey id)
     in tycref
    end

fun lookTYCinStr(STRstr{table,env,stamp,...}: Structure, id: symbol,_,_,
		  err : string->unit) : tycon ref =
    ((case lookTYCinTable(table,id)
	of ref(INDtyc i) =>
	     (case env
	       of REL{s,t} => ref(t sub i)
	        | DIR => impossible "EnvAccess.lookTYCinStr 1")
	 | ref(SHRtyc p) => ref(getEpathTyc(p,env))
	 | tyc => tyc)
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else err("unbound type in structure: " ^ Symbol.name id);
	 ref bogusTyc))
  | lookTYCinStr _ = impossible "EnvAccess.lookTYCinStr 2"

fun lookTYC' look (id:symbol) =
    case look(tycKey(id))
      of (TYCbind(tycref as ref(INDtyc i)), {strenv=REL{s,t},path}) =>
	    (ref(t sub i) 
	     handle Subscript => 
	       impossible "EnvAccess.lookTYC' 1")
       | (TYCbind(tycref as ref(SHRtyc p)), {strenv,path}) =>
	    (ref(getEpathTyc(p,strenv)) 
	     handle Subscript => 
	       impossible "EnvAccess.lookTYC' 2")
       | (TYCbind tycref, _) => tycref
       | _ => impossible "EnvAccess.lookTYC' 3"

val lookTYC = lookTYC' look
val lookTYClocal = lookTYC' lookStrLocal

(* addzeros also defined in Parse *)
fun addzeros(0,l) = l
  | addzeros(n,l) = addzeros(n-1,0::l)

fun bindTYC(id: symbol, tc: tycon ref) =
    let val binding = TYCbind tc 
     in add(tycIndex id, name id, binding); binding
    end


(* tycon lookup with arity checking *)

fun checkArity(tycon, arity,err) =
    if tyconArity(tycon) <> arity
    then err("type constructor "^(Symbol.name(tycName(tycon)))^
	          " has wrong number of arguments: "^makestring arity)
    else ()

fun lookArTYC(id,arity,err) =
    let val tycref as ref tyc = lookTYC id
     in checkArity(tyc,arity,err);
        tycref
    end
    handle Unbound => 
      (err("unbound type constructor: " ^ Symbol.name id);
       ref bogusTyc)

fun lookArTYCinSig (depth: int) (id: symbol, arity: int, err) =
    (case look(tycKey id)
      of (TYCbind(tycref as ref(INDtyc i)), {strenv=REL{s,t},path=h::r}) =>
	   if h >= 0
	   then let val tyc = t sub i
		 in checkArity(tyc,arity,err);
		    ref tyc
		end
	   else (checkArity(t sub i, arity,err);
		 ref(RELtyc(addzeros(depth+h,r@[i]))))
       | (TYCbind(tycref as ref(SHRtyc p)), {strenv,path}) =>
	   let val tyc = getEpathTyc(p,strenv)
	    in checkArity(tyc,arity,err);
	       ref tyc
	   end
       | (TYCbind tycref, _) => (checkArity(!tycref,arity,err); tycref)
       | _ => impossible "EnvAccess.lookTYCinSig")
    handle Unbound => 
      (err("unbound type constructor in signature: " ^ Symbol.name id);
       ref bogusTyc)

(* constructors *)

fun dconApplied(DATACON{name,const,typ,rep,sign},{path,strenv}:info) : datacon =
    DATACON{name = name, const = const, sign=sign,
            rep = (case rep
		     of VARIABLE(SLOT n) => VARIABLE(PATH(n::path))
		      | VARIABLE(LVAR v) => VARIABLE(PATH [v])
		      | _ => rep),  (* nonexception datacon *)
            typ = ref(typeInContext(!typ,strenv))}

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

val bogusCON = DATACON{name=bogusID,const=true,typ=ref ERRORty,
		       rep=UNDECIDED,sign=[]}

fun lookCONinStr(STRstr{table,env,stamp,...},id,ap,qid,err): datacon =
    (dconApplied(lookCONinTable(table,id),{path=ap,strenv=env})
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else err("unbound constructor in structure: " ^ Symbol.name id);
	 bogusCON))
  | lookCONinStr _ = impossible "EnvAccess.lookCONinStr"

fun bindCON (id: symbol, c: datacon) = 
    let val binding = CONbind c 
     in add(conIndex id, name id, binding); binding
    end

(* variables *)

fun varApplied(v: var, {path, strenv}: info, qid) : var =
    case v
      of VALvar{access,name,typ} =>
	   VALvar{access =
		    (case access
		       of SLOT(n) => PATH(n::path)
			| LVAR(n) => PATH([n])
			| INLINE _ => access
			| PATH _ => impossible "varApplied: access = PATH"),
		  typ = 
		    if Prim.special(access)
		    then ref(!typ)
		    else (case path
			   of [] => typ
			    | _ => ref(typeInContext(!typ,strenv))),
		  name = qid}
       | _ => v

fun lookVARinTable(table, id) =
    case IntStrMap.map table (varKey id)
      of VARbind v => v
       | _ => raise UnboundTable

fun lookVARCONinTable(table,id) = IntStrMap.map table (varKey id)

fun lookVARCONinStr(STRstr{table,env,stamp,...},id,ap,qid,err): binding =
    ((case lookVARCONinTable(table,id)
       of VARbind(var) => VARbind(varApplied(var,{path=ap,strenv=env},qid))
	| CONbind(dcon) => CONbind(dconApplied(dcon,{path=ap,strenv=env}))
	| _ => impossible "EnvAccess.lookVARCONinStr 1")
     handle UnboundTable =>
	(if stamp=bogusStrStamp then ()
	 else err("unbound variable or constructor in structure: "
		       ^ Symbol.name id);
	 CONbind bogusCON))
  | lookVARCONinStr(NULLstr,id,_,_,_) =
      (printSym id; print "\n"; impossible "EnvAccess.lookVARCONinStr 2")
  | lookVARCONinStr(_,id,_,_,_) =
      (printSym id; print "\n"; impossible "EnvAccess.lookVARCONinStr 3")

fun lookVARCON id = 
     case look(varKey id)
      of (VARbind v, info) => VARbind(varApplied(v,info,[id]))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => impossible "EnvAccess.lookVARCON"

fun lookVARCONlocal id = 
    case lookStrLocal(varKey id)
      of (VARbind v, info) => VARbind(varApplied(v,info,[id]))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => impossible "EnvAccess.lookVARCON"

fun bindVAR(id: symbol, v: var) = 
	let val binding = VARbind v
	 in add(varIndex id, name id, binding); binding
	end

(* exceptions *)

fun notInitialLowerCase string =
    (* string does NOT start with lower-case alpha *)
    let val firstchar = ordof(string,0)
     in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
    end

(* signatures *)

val bogusSIGStampsets = Stampset.newStampsets()
val bogusSIGbody = 
    STRstr{stamp=Stampset.newStamp(#strStamps bogusSIGStampsets),
           sign=Stampset.newStamp(Stampset.sigStamps),
           table=newTable(),
	   env=DIR,
	   kind=SIGkind{share={s=nil,t=nil},
		        bindings=nil,stamps=bogusSIGStampsets}}
val bogusSIG=SIGvar{name=bogusID,binding=bogusSIGbody}

fun lookSIG id = let val (SIGbind sign, _) = look(sigKey id) in sign end

fun bindSIG(id: symbol, s: signatureVar) = add(sigIndex id, name id, SIGbind s)

(* structures *)

fun strApplied(STRvar{name,access,binding},{path=ap,strenv},qid) =
    STRvar{name=qid,
	   binding=(case (binding,strenv)
		     of (INDstr i,REL{s,...}) => s sub i
		      | (SHRstr(i::r),REL{s,...}) => getEpath(r,s sub i)
		      | (STRstr _, _) => binding
		      | _ => impossible "strApplied: bad binding/env"),
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
     in strApplied(str,info,[id])
    end
val lookSTR = lookSTR' look
val lookSTRlocal = lookSTR' lookStrLocal

fun lookSTRinStr(STRstr{table,env,stamp,...},id,ap,qid,err) =
    (strApplied(lookSTRinTable(table,id),{path=ap,strenv=env},qid)
     handle UnboundTable => 
	(if stamp=bogusStrStamp then ()
	 else err("unbound structure in path: " ^ Symbol.name id);
	 bogusSTR))
  | lookSTRinStr _ = impossible "EnvAccess.lookSTRinStr"

fun bindSTR(id: symbol, strvar: structureVar) =
   let val binding = STRbind strvar
    in add(strIndex id, name id, binding);
       binding
   end

(* functors *)

fun lookFCT id = let val (FCTbind fv,_) = look(fctKey id) in fv end 

fun bindFCT(id: symbol, f: functorVar) = add(fctIndex id, name id, FCTbind f)

(* fixity bindings *)

fun lookFIX id = 
    if true (* !(Symbol.infixed id) *)
    then let val (FIXbind(FIXvar{binding,...}),_) = look(fixKey id)
	  in binding
	 end
	 handle Unbound => ((* Symbol.infixed id := false; *) NONfix)
    else NONfix

fun bindFIX(id: symbol, f: fixityVar) = 
   let val binding = FIXbind f
    in add(fixIndex id, name id, binding); binding
   end

(* lookup using symbolic path *)
fun lookPathinStr
      (str: Structure, ap: Access.path, spath as _::rest : symbol list,
       err: string -> unit,
       lookLast: Structure * symbol * Access.path * 
			symbol list * (string->unit) -> 'a) : 'a =
    let fun getStr([id],str,ap) = lookLast(str,id,ap,spath,err)
	  | getStr(id::rest,STRstr{table,stamp,env,...},ap) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTRinTable(table,id)
		      handle UnboundTable => 
			(if stamp=bogusStrStamp then ()
		         else (err("unbound intermediate structure: "
				        ^ name id);
		               print "  in path: ";
			       printSequence "." printSym spath;
		               newline());
		         bogusSTR')
	       in getStr(rest,
		  	 (case binding
			   of INDstr i => 
			      (case env
			        of REL{s,...} => s sub i
			         | DIR => impossible "lookPathinStr.getStr 1")
			    | SHRstr(i::r) => 
			      (case env
			        of REL{s,...} => getEpath(r,s sub i)
			         | DIR => impossible "lookPathinStr.getStr 2")
			    | _ => binding),
			 n::ap)
	      end
	  | getStr _ = impossible "EnvAccess.lookPathinStr.getStr"
     in getStr(rest,str,ap)
    end
  | lookPathinStr _ = impossible "EnvAccess.lookPathinStr"

fun lookPath (lookLast: Structure * symbol * Access.path * 
		symbol list * (string->unit) -> 'a)
	     (spath as first::rest, err: string->unit) : 'a =
    let	val STRvar{access=PATH(ap),binding,...} =
	      lookSTR first
	      handle Unbound => 
	        (err("unbound head structure: " ^ name first);
		 print "  in path: "; printSequence "." printSym spath;
		 newline();
		 bogusSTR)
     in lookPathinStr(binding,ap,spath,err,lookLast)
    end
  | lookPath _ _ = impossible "EnvAccess.lookPath"

val lookPathSTR = lookPath lookSTRinStr
val lookPathVARCON = lookPath lookVARCONinStr
val lookPathCON = lookPath lookCONinStr
val lookPathTYC = lookPath lookTYCinStr

fun lookPathArTYC ([id],a,err) = lookArTYC(id,a,err)
  | lookPathArTYC (path: symbol list, arity: int,err) =
    let val tycref as ref tyc = lookPathTYC (path, err)
     in checkArity(tyc,arity,err);
	tycref
    end

(* debug print functions *)
val prIntPath = printClosedSequence ("[",",","]") (print:int->unit)
fun prSymPath spath = printSequence "." printSym (rev spath)

fun lookPathArTYCinSig depth ([id],a,err) = lookArTYCinSig depth (id,a,err)
  | lookPathArTYCinSig (depth: int) (spath as first::rest, arity,err) : tycon ref =
    let	fun complainUnbound() =
	    (err "unbound type constructor in signature";
	     print "  name: "; printSequence "." printSym spath;
	     newline();
	     raise Syntax)
	(* second arg of get is expected to be a signature *)
	fun get([id],STRstr{table,env as REL{t,...},...}) = 
	     (case lookTYCinTable(table,id)
		   handle UnboundTable => complainUnbound()
	       of ref(INDtyc i) => (checkArity(t sub i, arity,err); [i])
	        | ref(SHRtyc p) => (checkArity(getEpathTyc(p,env), arity,err); p)
		| _ => impossible "lookPathArTYCinSig.get")
	  | get(id::rest,STRstr{table,env=REL{s,...},...}) =
	      let val STRvar{binding=INDstr k,...} =
			lookSTRinTable(table,id)
			handle UnboundTable => complainUnbound()
	       in k::get(rest, s sub k)
	      end
	  | get([],_) = impossible "EnvAccess.lookPathArTYCinSig.get - empty path"
	  | get(p,NULLstr) =
	     (prSymPath p; print "\n";
	      impossible "EnvAccess.lookPathArTYCinSig.get - NULLstr")
	  | get(p,INDstr _) =
	     (prSymPath p; print "\n";
	      impossible "EnvAccess.lookPathArTYCinSig.get - INDstr")
	  | get(p,SHRstr _) =
	     (prSymPath p; print "\n";
	      impossible "EnvAccess.lookPathArTYCinSig.get - SHRstr")
	  | get _ = impossible "EnvAccess.lookPathArTYCinSig.get - bad args"
	fun lookInStr(str) =
	    let val tycref = lookPathinStr(str, [], spath, err,lookTYCinStr)
	     in checkArity(!tycref,arity,err);
		tycref
	    end
	val leadStr = lookSTR0 first
		      handle Unbound => complainUnbound()
     in case leadStr
	  of (STRvar{binding=INDstr i,...},{path=h::r,strenv=REL{s,...}}) =>
	      if h < 0 (* indicates signature component *)
	      then ref(RELtyc(addzeros(depth+h,r@(i::get(rest, s sub i)))))
	      else lookInStr(s sub i)
	   | (STRvar{binding=SHRstr(i::r),...},{strenv=REL{s,...},...}) =>
	        lookInStr(getEpath(r, s sub i))
	   | (STRvar{binding as STRstr _,...},_) => lookInStr binding
	   | _ => impossible "EnvAccess.lookPathArTYCinSig - leadStr"
    end
  | lookPathArTYCinSig _ _ = impossible "lookPathArTYCinSig - bad arg"

(* functions to collect stale lvars for unbinding *)
exception NotStale

fun checkopen (oldenv,v) =
  let fun check (i,s,_) =
	let val (binding,{path,strenv}) = lookEnv(oldenv,(i,s))
	    fun checklast [x] = if x=v then raise NotStale else ()
	      | checklast (a::r) = checklast r
	      | checklast nil = ()
	 in checklast path;
	    case (binding,strenv)
	     of (STRbind(STRvar{binding=STRstr{table,...},...}),_) =>
			 (IntStrMap.app check table handle Unbound => ())
	      | (STRbind(STRvar{binding=INDstr i,...}),REL{s,...}) =>
			 (let val STRstr{table,...} = s sub i
			   in IntStrMap.app check table
			  end handle Unbound => ())
	      | _ => ()
        end
    in check
   end

fun staleLvars(newenv,oldenv) : int list =
    let val lvarset = ref([] : int list)
        fun collect (isb as (i,s,_)) = 
	  let val v = case lookEnv(oldenv,(i,s))
		       of (VARbind(VALvar{access=LVAR v,...}),_) => v
		        | (STRbind(STRvar{access=LVAR v,...}),_) =>
				 (checkopen(oldenv,v) isb; v)
		        | (FCTbind(FCTvar{access=LVAR v,...}),_) => v
		        | _ => raise NotStale
	   in lvarset := v :: !lvarset
	  end handle NotStale => ()
		   | Unbound => ()
     in appenv collect (newenv,oldenv);
        !lvarset
    end

end (* structure EnvAccess *)
