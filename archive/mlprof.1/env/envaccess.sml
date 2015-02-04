(* envaccess.sml *)

(* lookup and binding functions *)

structure EnvAccess : ENVACCESS = struct

structure Access = Access
structure Basics = Basics
structure Env = Env

open ErrorMsg PrintUtil Access Basics Basics.Symbol BasicTypes TypesUtil
     Absyn Env

val debugBind = System.Control.debugBind

(* managing pervasive structures *)

val pervasives = ref (nil : structureVar list)

fun openStructureVar(STRvar{access=PATH p,binding,...}) =
    (case binding
      of STRstr{table,env,...} => openOld((p,env),table)
       | INDstr _ => impossible "EnvAccess.openStructureVar -- INDstr arg")
  | openStructureVar _ = impossible "EnvAccess.openStructureVar -- bad access value"

fun setPervasives(strVars) =
    pervasives := strVars

fun openPervasives() =
    app openStructureVar (!pervasives)

(* val lookBase : (symtable * Table.Symbol.symbol -> 'a)
		      -> Table.Symbol.symbol -> 'a * info *)
fun lookBase (tblSearch) id = 
    let fun find (STRvar{access=PATH p,binding=STRstr{table,env,...},...}::r) =
	      ((find r) handle Table.Notfound_Table => 
		          (tblSearch(table,id),(p,env)))
	  | find nil = raise Table.Notfound_Table
     in find (!pervasives)
    end


(* type constructors *)

val checkTYC: binding -> tycon ref = 
    fn (TYCbind tycref) => tycref | _ => raise Table.Next

val lookTYCinTable = Table.look checkTYC

fun lookTYCinStr(STRstr{table,env,...}: Structure, id: symbol) : tycon ref =
    (case lookTYCinTable(table,id)
      of ref(INDtyc [i]) => let val {t,s} = env in ref(t sub i) end
(* what about paths of length > 1? *)
       | tyc => tyc)
    handle Table.Notfound_Table => condemn("unbound tycon in str: " ^ name id)

fun lookTYC id =
    (case look(false, lookTYCinTable) id
      of (tycref as ref(INDtyc[i]), (_,{t,s})) =>
	    (ref(t sub i) 
	     handle Subscript => tycref)  (* in signature *)
       | (tycref, _) => tycref)

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
  fun lookPatchTYC id =
    if !inDatatype
      then let val tc = ref (UNDEFtyc id)
            in tyconPatchList := tc :: !tyconPatchList;
	       tc
	   end
      else lookTYC id
	   handle Unbound => 
	     condemn("unbound type constructor: " ^ Symbol.name id)

  fun patchTycons (tc::l) =
      let val ref(UNDEFtyc id) = tc
       in tc := !(lookTYC id)
		handle Unbound =>
		  condemn("unbound type constructor (in dt): " ^ Symbol.name id);
          patchTycons(l)
      end
    | patchTycons nil = ();

  val protectDb =
      ((fn () => (tyconPatchList := nil; inDatatype := true)),
       (fn () => (patchTycons(!tyconPatchList); inDatatype := false)))

end (* local -- patch *)


(* constructors *)

val checkCON = fn CONbind c => c | _ => raise Table.Next

fun dconApplied(dc as DATACON{name,const,typ,rep,sign},(ap,env)) : datacon =
    DATACON{name = name, const = const, sign=sign,
            rep = (case rep
		     of VARIABLE(SLOT n) => VARIABLE(PATH(n::ap))
		      | VARIABLE(LVAR v) => VARIABLE(PATH [v])
		      | _ => rep),  (* defined exception *)
            typ = typeInContext(typ,env)}

fun dconInStr(dc as DATACON{name,const,typ,rep,sign},env,slotNo) : datacon =
    DATACON{name = name, const = const, sign = sign,
            rep = (case rep
		     of VARIABLE(access) => VARIABLE(SLOT slotNo)
		      | _ => rep),
            typ = typeInContext(typ,env)}

fun exnInSig(DATACON{name,const,typ,sign,...},slot) =
    DATACON{name = name, const = const, typ = typ, sign = sign,
            rep = VARIABLE(SLOT slot)}

val lookCONinTable = Table.look checkCON

fun lookCON id =
     dconApplied(look(false,lookCONinTable) id)

fun lookCONinStr(STRstr{table,env,...},id,ap): datacon =
    dconApplied(lookCONinTable(table,id),(ap,env))
    handle Table.Notfound_Table => condemn("unbound data constructor in str: "
				           ^ name(id))

fun bindCON (id: symbol, c: datacon) =
    (add(id, CONbind c); c)


(* variables *)

val checkCON' =  (* like checkCON, but returns binding *)
    fn (b as CONbind(_)) => b | _ => raise Table.Next

val checkVARCON = 
    fn b as VARbind _ => b
     | b => checkCON' b

val checkVAR = fn VARbind v => v | _ => raise Table.Next

val lookVARCONinTable = Table.look checkVARCON

val lookCONinTable' = Table.look checkCON'

val lookVARinTable = Table.look checkVAR

fun varApplied(v:var, (ap:Access.path, env:strenv)) : var =
    case v
      of VALvar{access,name,vtype} =>
	   VALvar{access =
		    (case access
		       of SLOT(n) => PATH(n::ap)
			| LVAR(n) => PATH([n])
			| INLINE _ => access
			| PATH _ => impossible "varApplied: access = PATH"),
		  vtype = 
		    if Prim.special(access)
		    then ref(!vtype)
		    else (case ap
			   of [] => vtype
			    | _ => ref(typeInContext(!vtype,env))),
		  name = name}
       | _ => v

fun varInStr(v:var, env:strenv, slotNo: int) : var =  (* ??? *)
    case v
      of VALvar{access = INLINE(_),...} => v
       | VALvar{name,vtype,...} =>
	   VALvar{access = SLOT slotNo,
		  vtype = ref(typeInContext(!vtype,env)),
		  name = name}
       | _ => v

fun varInSig(VALvar{name,vtype,...},slot) =
      VALvar{access=SLOT(slot),name=name,vtype=vtype}
  | varInSig(var,_) = var

fun unboundVAR id = 
    (complain ("unbound variable " ^ name id);
     VARbind(VALvar{access=LVAR(mkLvar()), name=id, vtype=ref UNDEFty}))

fun lookVARinBase id = varApplied(lookBase lookVARinTable id)

fun lookVARCONinStr(STRstr{table,env,...},id,ap): binding =
    (case lookVARCONinTable(table,id)
       of VARbind(var) => VARbind(varApplied(var,(ap,env)))
	| CONbind(dcon) => CONbind(dconApplied(dcon,(ap,env)))
	| _ => impossible "envaccess.381")
    handle Table.Notfound_Table => unboundVAR id

fun lookVARLocal id = 
    varApplied(lookLocalRec (lookVARinTable, NONE) id)

fun lookVARCON id = 
    case lookLocalRec (lookVARCONinTable, SOME lookCONinTable') id
      of (VARbind v, info) => VARbind(varApplied(v,info))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => impossible "envaccess.228"

(* patching deferred variables *)

val varPatchList : var ref list ref = ref nil

fun getPatchVar id =
    let val v = ref (UNKNOWNvar id)
     in varPatchList := v :: !varPatchList;
	v
    end

exception Patched

fun patchVars (pl as (varRef as ref(UNKNOWNvar id))::pl', tl) =
        ((varRef := lookVARLocal id; raise Patched)
  	  handle Unboundrec => 
		    patchVars(pl',varRef::tl)  (* not yet bound; try later *)
               | Unbound => (* no more rec layers *)
		   let val VARbind v = unboundVAR id
		    in varRef := v; patchVars(pl',tl)
		   end
	       | Patched => patchVars(pl', tl)
	)
  | patchVars (nil, tl) = tl

val protectPatchList =
    ((fn () => !varPatchList before (varPatchList := nil)),
     (fn (vpl) => varPatchList := patchVars(!varPatchList,vpl)))
	 (* bug -- exit function only works right for normal exit from protect *)

fun capitalized string =
    (* string starts with a capital letter *)
    let val firstchar = ordof(string,0)
     in firstchar >= Ascii.uc_a andalso firstchar <= Ascii.uc_z
    end

fun checkBinding(bl : binder list) =
     app (fn (id,_) => 
	    if capitalized(Symbol.name id)
	    then warn("Capitalized variable in rule: "^ Symbol.name id)
	    else ()) 
         bl

fun newVAR(bl: binder list ref, id: symbol) : var =
    let fun checkid ((i,b)::bl) =
	      if Symbol.eq(i,id)
	        then complain "repeated var in pattern"
	        else checkid bl
	  | checkid nil = ()
     in checkid(!bl);
        let val v = mkVALvar(id,ref UNDEFty)
	 in bl := (id, VARbind v) :: !bl;
	    v
        end
    end;

fun bindVAR(id: symbol, v: var) = add(id, VARbind v);

fun bindVARs(binders: binder list) = (* app add bl; *)
    app (fn (b as (s,bind)) =>
	    (if !debugBind
		then (prstr "bindVARs: "; printSym s; newline();
		      let val VARbind(VALvar{vtype=ref ty,...}) = bind
		       in PrintType.printType(ty); newline()
		      end)
		else ();
	     add b))
	binders;


(* type variables *)

datatype BoundTyvars = CLOSEDbtv of tyvar list
		     | OPENbtv of tyvar list ref;

val boundTyvars = ref(CLOSEDbtv([]));

fun protectTyvars v = 
    ((fn () => (!boundTyvars before (boundTyvars := v))),
     (fn btv => boundTyvars := btv))

fun currentTyvars () = let val OPENbtv(ref l) = !boundTyvars in l end

fun checkTYV (TYVbind tyv) = tyv
  | checkTYV _ = raise Table.Next

fun lookTYV id = 
    let val (tyv,_) = look(false, Table.look checkTYV) id
     in tyv
    end

fun bindTYV(id: symbol, tv: tyvar) =
    ( add(id, TYVbind tv); tv )

fun lookTyvar (id: symbol) =
    case !boundTyvars
      of CLOSEDbtv(tvenv) =>
	   let fun find ((tv as ref(UBOUND id'))::resttvs) =
		   if Symbol.eq(id,id')
		      then tv
		      else find(resttvs)
		 | find([]) =
		    (complain "lookTyvar -- unbound tyvar in closed scope";
		     mkTyvar(INSTANTIATED UNDEFty))
	    in find tvenv
	   end
       | OPENbtv polyTyvars =>
	   lookTYV id
	   handle Unbound =>
	     let val tyv = bindTYV(id, mkTyvar(UBOUND id))
	      in polyTyvars := tyv :: !polyTyvars;
		 tyv
	     end;


(* exceptions *)

fun notInitialLowerCase string =
    (* string does NOT start with lower-case alpha *)
    let val firstchar = ordof(string,0)
     in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
    end

fun looksLikeExn sym = notInitialLowerCase(Symbol.name sym)

local val offset = Ascii.uc_a - Ascii.lc_a in
  fun fixExnName s =
      if looksLikeExn s then s
      else let val name = Symbol.name s
	       val first = chr(ordof(name,0)+offset)
	       val rest = substring(name,1,length name - 1)
			handle Substring => ""
	    in Symbols.stringToSymbol(first ^ rest)
	   end
end

val lookEXNinTable = Table.look checkCON
fun lookEXNinBase id = dconApplied(lookBase lookEXNinTable id)

fun unboundEXN id =
    (complain("unbound exn: " ^ name id);
     DATACON{name=id,const=false,typ=UNDEFty,
     	     rep=UNDECIDED,sign=[]})

fun lookEXN id =
    dconApplied(look (false,lookEXNinTable) id)

fun lookEXNinStr(STRstr{table,env,...},id,ap) =
    dconApplied(lookEXNinTable(table,id),(ap,env))
    handle Table.Notfound_Table => unboundEXN id

val lookFixedEXNinStr = (fn (a,b,c) => lookEXNinStr(a, fixExnName b, c))

fun bindEXN(id: symbol, e: datacon) =
    (if !debugBind
       then (prstr "bindEXN: "; printSym id; newline())
       else ();
     add(id, CONbind e); e)


(* signatures *)

fun lookSIG id = 
    let val (sign,_) = 
	look(true, Table.look(fn SIGbind s => s | _ => raise Table.Next)) id
     in sign
    end
    handle Unbound => condemn("unbound signature: " ^ name id)

fun bindSIG(id: symbol, s: signatureVar) = (add(id,SIGbind s); s)


(* structures *)

fun strApplied(STRvar{name,access,binding},(ap,{s,t})) =
    STRvar{name=name,
	   binding=(case binding of INDstr i => s sub i | _ => binding),
	   access=(case access
		     of SLOT(n) => PATH(n::ap)
		      | LVAR(n) => PATH [n]
		      | _ => impossible "strApplied: access = PATH or INLINE")}

fun strInStr(STRvar{name,access,binding},{s,t},slot) =
    STRvar{name=name,
	   binding=(case binding of INDstr i => s sub i | _ => binding),
	   access=SLOT(slot)}

fun strInSig(STRvar{name,access,binding},slot) =
    STRvar{name=name,
	   binding=binding,
	   access=SLOT(slot)}

val checkSTR = fn STRbind s => s | _ => raise Table.Next
val lookSTRinTable = Table.look checkSTR

fun lookSTR_sig id = 
    look(false,lookSTRinTable) id

fun lookSTR id =
    strApplied(look (true,lookSTRinTable) id)

fun lookSTRinStr(STRstr{table,env,...},id,ap) =
    strApplied(lookSTRinTable(table,id),(ap,env))
    handle Table.Notfound_Table => condemn("unbound structure id in structure: " ^ name id)

fun bindSTR(id: symbol, s: structureVar) = add(id,STRbind s)


(* functors *)

val lookFCTinTable = Table.look(fn (FCTbind fv) => fv | _ => raise Table.Next)

fun lookFCT id = 
    let val (fv,_) = look(true,lookFCTinTable) id
     in fv
    end handle Unbound => condemn("unbound functor identifier: " ^ Symbol.name id)

fun bindFCT(id: symbol, f: functorVar) = (add(id,FCTbind f); f)


(* fixity bindings *)

fun lookFIX1 id = 
    let val (fix,_) = 
	  look(false, Table.look(fn FIXbind f => f | _ => raise Table.Next)) id
     in fix
    end

fun lookFIX id = lookFIX1 id handle Unbound => NONfix

fun bindFIX(id: symbol, f: fixity) = add(id, FIXbind f)


(* collecting stale lvars -- used by Invoke.getvars *)

fun getWHAT extract =
  fn id =>
   let val l = ref (nil : int list)
       fun f b = (l := extract b :: !l; raise Table.Next)
    in (look(true,Table.look f) id; ()) handle Unbound => ();
       rev(!l)
   end

val getVARvars = getWHAT(fn (VARbind(VALvar{access=LVAR v,...})) => v
			  | _ => raise Table.Next)
val getSTRvars = getWHAT(fn (STRbind(STRvar{access=LVAR v,...})) => v
		          | _ => raise Table.Next)
val getFCTvars = getWHAT(fn (FCTbind(FCTvar{access=LVAR v,...})) => v
			  | _ => raise Table.Next)


(* building structures *)

(* is this correct?  *)
fun tyconGt(TYCON{kind=DATAtyc _,...}, _) = false
  | tyconGt(_, TYCON{kind=DATAtyc _,...}) = true
  | tyconGt(_) = false
  (* DATAtycs come after other kinds *)

fun binderGt(((id1,FIXbind(_)),_),((id2,FIXbind(_)),_)) =
      name(id1) > name(id2)
  | binderGt(((_,FIXbind(_)),_),_) = true
  | binderGt(_,((_,FIXbind(_)),_)) = false
  | binderGt(((id1,VARbind(_)),_),((id2,VARbind(_)),_)) =
      name(id1) > name(id2)
  | binderGt(((_,VARbind(_)),_),_) = true
  | binderGt(_,((_,VARbind(_)),_)) = false
  | binderGt(((id1,CONbind(_)),_),((id2,CONbind(_)),_)) =
      name(id1) > name(id2)
  | binderGt(((_,CONbind(_)),_),_) = true
  | binderGt(_,((_,CONbind(_)),_)) = false
  | binderGt(((id1,TYCbind(ref tycon1)),_),((id2,TYCbind(ref tycon2)),_)) = 
(*      tyconGt(tycon1,tycon2) orelse     temporarily removed
      not(tyconGt(tycon2,tycon1)) andalso *) name(id1) > name(id2)
  | binderGt(((_,TYCbind(_)),_),_) = true
  | binderGt(_,((_,TYCbind(_)),_)) = false
  | binderGt(((id1,STRbind(_)),_),((id2,STRbind(_)),_)) =
      name(id1) > name(id2)
  | binderGt(((_,STRbind(_)),_),_) = true
  | binderGt(((id1,bind1),_),((id2,bind2),_)) = impossible "4344 in envaccess"

fun build (iter: (unit -> binder * info) -> 'a) (s:marker) : 'a =
    let val sorttree = BinSort.mkSortTree()
     in collectTable(s,BinSort.insert(binderGt,sorttree));
        iter(BinSort.generator sorttree)
    end

fun extendPath(LVAR(v): access, []: path) = PATH[v] (* locally defined *)
  | extendPath(SLOT(n), p) = PATH(n::p)  (* element of opened structure *)
  | extendPath(x as PATH _, _) = x  (* defined exception *)
  | extendPath(x as INLINE _, _) = x
  | extendPath(access,path) = impossible "extendPath in envaccess"

fun iterStr (gen : unit -> binder * info) :  trans list * symtable =
    let val newtable = Table.new()
	fun fill (count) =
	    let val element as ((sym,binding),_) = gen()
	     in case element
		  of ((id, VARbind(var as VALvar{access,...})),(path,env)) =>
		       (Table.add(newtable,
				  (id,VARbind(varInStr(var,env,count))));
			VALtrans(extendPath(access,path))::fill(count+1))
		   | ((id, CONbind(exn as DATACON{rep=VARIABLE(access),...})),
		      (path,env)) =>
		       (Table.add(newtable,
				  (id,CONbind(dconInStr(exn,env,count))));
			VALtrans(extendPath(access,path))::fill(count+1))
		   | ((id, STRbind(strVar as STRvar{access,...})),(path,env)) =>
		       (Table.add(newtable,
				  (id,STRbind(strInStr(strVar,env,count))));
			VALtrans(extendPath(access,path))::fill(count+1))
		   | ((id, TYCbind(tyconRef)),(_,env)) =>
		       (Table.add(newtable,
		       		 (id,TYCbind(ref(tyconInContext env (!tyconRef)))));
			fill(count))
		   | ((id, CONbind(dcon)),(path,env)) =>
		       (Table.add(newtable, (id,CONbind(dconInStr(dcon,env,0))));
			fill(count))
		   | (binding,_) => (Table.add(newtable,binding); fill(count))
	    end
	    handle BinSort.Finished => []
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
	        | ((id, CONbind(exn as DATACON{rep=VARIABLE _,...})),_) =>
		    let val eb = CONbind(exnInSig(exn,count))
		     in Table.add(newtable,(id,eb));
			eb::fill(count+1)
		    end
	        | ((id, STRbind(strVar)),_) =>
		    let val sb = STRbind(strInSig(strVar,count))
	             in Table.add(newtable,(id,sb));
			sb::fill(count+1)
		    end
	        | (binder as (_, FIXbind _),_) =>
	            (Table.add(newtable,binder);
	             fill(count))  (* put infix bindings in table only *)
	        | (binder as (_, binding),_) =>
	            (Table.add(newtable,binder);
	             binding::fill(count)))
	     handle BinSort.Finished => []
     in (fill(0),newtable)
    end

val buildSigTable = build iterSig;

val maxStrComps = 100
and maxTypeComps = 100

fun iterFct(gen : unit -> binder*info) : trans list * symtable * strenv =
    let val newtable = Table.new():binding Table.table
	val strComps = array(maxStrComps,INDstr(~1))
	and tycComps = array(maxTypeComps,INDtyc[])
	and strCount = ref 0
	and tycCount = ref 0
	fun copyarray(a,n) =
	    (* assume n <= length a *)
	    let val new = array(n,a sub 0)
		fun loop i = (update(new,i,a sub i); loop(i+1))
	     in loop 0
		handle Subscript => new
	    end
	local open Intmap
	      val smap = new():int list intmap
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
			    then let val p = i::path
				  in addStamp(stamp,p);
				     p
				 end
			    else tsearch(i+1)
			fun ssearch i =
			    if i >= scount
			    then raise Fail
			    else let val STRstr{env={s,t},...} = senv sub i
				  in search0(s,length s,t,length t,i::path)
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
	fun abstractType(CONty(tycref as ref tyc,args)) =
	     (case tyc
	       of INDtyc _ => ()  (* already been done *)
	        | _ => let val stamp = tycStamp(tyc)
			in if fixedStamp stamp then ()
			   else tycref := INDtyc(mapStamp stamp
						 handle Intmap.Intmap =>
						   search(stamp,tyc))
		       end;
	      app abstractType args)
	  | abstractType(POLYty(TYFUN{body,...})) = abstractType body
	  | abstractType(_) = ()
	fun fill (count) =
	     (case gen()
		of ((id, STRbind(STRvar{name,access,binding})),(path,{s,t})) =>
		     (let val str as STRstr{stamp,...} = 
			      case binding
				of INDstr i => s sub i
				 | _ => binding
		       in if fixedStamp stamp
			  then (Table.add(newtable,
				 (id,STRbind(STRvar{name=name,access=SLOT count,
						    binding=str})));
				())
			  else (Table.add(newtable,
				 (id,STRbind(STRvar{name=name,access=SLOT count,
						    binding=INDstr(!strCount)})));
				update(strComps,!strCount,str);
				inc strCount)
		      end;
		      VALtrans(extendPath(access,path))::fill(count+1))
		 | ((id, TYCbind(ref tyc)),(_,env)) =>
		     (let val tyc = tyconInContext env tyc
		       in if fixedStamp(tycStamp tyc)
			  then (Table.add(newtable,(id,TYCbind(ref tyc))); ())
			  else (Table.add(newtable,
					  (id,TYCbind(ref(INDtyc[!tycCount]))));
				update(tycComps,!tycCount,tyc);
				inc tycCount)
		      end;
		      fill(count))
		 | ((id, VARbind(var as VALvar{access,name,vtype})),(path,env)) =>
		     (Table.add(newtable,
		       (id,VARbind(case access
				     of INLINE _ => var  (* ground type *)
				      | _ => let val typ =
						     typeInContext(!vtype,env)
					      in abstractType(typ);
						 VALvar{access=SLOT count,
							name=name,
							vtype=ref typ}
					     end)));
		      VALtrans(extendPath(access,path))::fill(count+1))
		 | ((id, CONbind(DATACON{name,const,typ,rep,sign})),(path,env)) =>
		    (let val typ = typeInContext(typ,env)
		      in abstractType(typ);
			 Table.add(newtable,
			  (id,CONbind(DATACON{name=name,const=const,sign=sign,
					      typ = typ,
					      rep = case rep
						      of VARIABLE(_) =>
							   VARIABLE(SLOT count)
						       | _ => rep})))
		     end;
		     case rep
		       of VARIABLE(access) =>
			    VALtrans(extendPath(access,path))::fill(count+1)
			| _ => fill(count))
		 | (binding,_) => (Table.add(newtable,binding); fill(count)))
	    handle BinSort.Finished => []
     in (fill(0), newtable,
	 {s=copyarray(strComps,!strCount),
	  t=copyarray(tycComps,!tycCount)})
    end

val buildFctTable = build iterFct

(* reset state of EnvAccess *)
fun reset() =
    (varPatchList := nil;
     boundTyvars := CLOSEDbtv([]))

end (* functor EnvAccess *)

