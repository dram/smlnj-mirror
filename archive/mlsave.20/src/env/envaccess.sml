(* envaccess.sml *)

(* lookup and binding functions *)

structure EnvAccess : ENVACCESS = struct

structure Access = Access
structure Basics = Basics
structure Env = Env

open ErrorMsg PrintUtil Access Basics Basics.Symbol BasicTypes TypesUtil Env

val debugBind = System.Control.debugBind

val transStruct = true  (* structures are transparent -- free vars allowed *)

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


exception UnboundInStr of string

(* type constructors *)

val checkTYC: binding -> tycon ref = 
    fn (TYCbind tycref) => tycref | _ => raise Table.Next

val lookTYCinTable = Table.look checkTYC

fun lookTYCinStr(STRstr{table,env,...}: Structure, id: symbol) : tycon ref =
    (case lookTYCinTable(table,id)
      of ref(INDtyc [i]) => let val {t,s} = env in ref(t sub i) end
	 (* paths of length > 1 should not occur in type bindings *)
       | tyc => tyc)
    handle Table.Notfound_Table => raise UnboundInStr "type constructor"

fun lookTYC' deep id =
    (case look(deep, lookTYCinTable) id
      of (tycref as ref(INDtyc[i]), (_,{t,s})) =>
	    (ref(t sub i) 
	     handle Subscript => tycref)  (* in signature *)
       | (tycref, _) => tycref)

val lookTYC = lookTYC' transStruct
val lookTYClocal = lookTYC' false

fun bindTYC(id: symbol, tc: tycon ref) = add(id, TYCbind tc)


(* patching type constructor references in datatype declarations *)

val inDatatype : bool ref = ref false;  (* context flag *)
val tyconPatchList : tycon ref list ref = ref nil

fun lookPatchTYC(id,ary) =
  if !inDatatype
    then let val tycref = ref (mkUNDEFtyc(id,ary))
	  in tyconPatchList := tycref :: !tyconPatchList;
	     tycref
	 end
    else let val tycref as ref tycon = lookTYC id
	  in case tycon
	      of TYCON{arity,path=name::_,...} =>
		   if arity <> ary
		   then complain("type constructor ("^(Symbol.name(name))^
				 ") applied to wrong number of arguments: "
				 ^makestring ary)
		   else ()
	       | _ => ();
	    tycref
	end
	handle Unbound => 
	   condemn("unbound type constructor: " ^ Symbol.name id)

fun patchTycons (tc::l) =
    let val ref(TYCON{path=id::_,arity=ary,kind=UNDEFtyc newpath,...}) = tc
     in let val tycon = !(lookTYC id)
	 in case tycon
	      of TYCON{arity,path=name::_,...} =>
		   if arity <> ary
		   then complain("type constructor ("^(Symbol.name(name))^
				 ") applied to wrong number of arguments: "
				 ^makestring ary)
		   else ()
	       | _ => ();
	    tc := case newpath
		    of NONE => tycon
		     | SOME path => setTycPath(path,tycon)
	end
	handle Unbound =>
	  complain("unbound type constructor (in datatype): " ^ Symbol.name id);
	patchTycons(l)
    end
  | patchTycons nil = ();

val protectDb =
    ((fn () => (tyconPatchList := nil; inDatatype := true)),
     (fn () => (patchTycons(!tyconPatchList); inDatatype := false)))


(* constructors *)

val checkCON = fn CONbind c => c | _ => raise Table.Next

fun dconApplied(dc as DATACON{name,const,typ,rep,sign},(ap,env)) : datacon =
    DATACON{name = name, const = const, sign=sign,
            rep = (case rep
		     of VARIABLE(SLOT n) => VARIABLE(PATH(n::ap))
		      | VARIABLE(LVAR v) => VARIABLE(PATH [v])
		      | _ => rep),  (* nonexception datacon *)
            typ = typeInContext(typ,env)}

val lookCONinTable = Table.look checkCON

fun lookCON' deep id =
     dconApplied(look(deep,lookCONinTable) id)
val lookCON = lookCON' transStruct
val lookCONlocal = lookCON' false

fun lookCONinStr(STRstr{table,env,...},id,ap): datacon =
    dconApplied(lookCONinTable(table,id),(ap,env))
    handle Table.Notfound_Table => raise UnboundInStr "constructor"

fun bindCON (id: symbol, c: datacon) = add(id, CONbind c)


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

fun unboundVAR id = 
    (complain ("unbound variable " ^ name id);
     VARbind(mkVALvar(Symbols.stringToSymbol(name id ^ "?"),
                      ref(VARty(mkTyvar defaultMETA)))))

fun lookVARinBase id = varApplied(lookBase lookVARinTable id)

fun lookVARCONinStr(STRstr{table,env,...},id,ap): binding =
    (case lookVARCONinTable(table,id)
       of VARbind(var) => VARbind(varApplied(var,(ap,env)))
	| CONbind(dcon) => CONbind(dconApplied(dcon,(ap,env)))
	| _ => impossible "EnvAccess.lookVARCONinSTR")
    handle Table.Notfound_Table => raise UnboundInStr "variable or constructor"

fun lookVARLocal id = 
    varApplied(lookLocalRec (transStruct, lookVARinTable, NONE) id)

fun lookVARCON' deep id = 
    case lookLocalRec (deep, lookVARCONinTable, SOME lookCONinTable') id
      of (VARbind v, info) => VARbind(varApplied(v,info))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => impossible "EnvAccess.lookVARCON"
val lookVARCON = lookVARCON' transStruct
val lookVARCONlocal = lookVARCON' false

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

(* Used to enforce the Capitalization convention *)
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
		then (prstr "bindVARs: "; printSym s; newline())
		else ();
	     add b))
	binders;


(* type variables *)

datatype mode = EXP | TYPEDEC

val tyvarsMode = ref(EXP)
val boundTyvars = ref([]:tyvar list);

fun protectTyvars NONE = 
    ((fn () => (!boundTyvars before (boundTyvars := []))),
     (fn btv => boundTyvars := btv))
  | protectTyvars (SOME tvs) = 
    ((fn () => (!boundTyvars before (boundTyvars := tvs; tyvarsMode := TYPEDEC))),
     (fn btv => (boundTyvars := btv; tyvarsMode := EXP)))

fun currentTyvars () = !boundTyvars

fun checkTYV (TYVbind tyv) = tyv
  | checkTYV _ = raise Table.Next

fun lookTYV id = 
    let val (tyv,_) = look(false, Table.look checkTYV) id
     in tyv
    end

fun bindTYV(id: symbol, tv: tyvar) =
    ( add(id, TYVbind tv); tv )

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
	    in find(!boundTyvars)
	   end
       | EXP =>
	   lookTYV id
	   handle Unbound =>  (* here we could check for weakness > 0 *)
	     let val tyv = bindTYV(id, mkTyvar(mkUBOUND id))
	      in boundTyvars := tyv :: !boundTyvars;
		 tyv
	     end;


(* exceptions *)

fun notInitialLowerCase string =
    (* string does NOT start with lower-case alpha *)
    let val firstchar = ordof(string,0)
     in firstchar < Ascii.lc_a orelse firstchar > Ascii.lc_z
    end

(* Used to enforce the Capitalization convention *)
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

fun lookEXN' deep id =
    dconApplied(look (deep,lookEXNinTable) id)
val lookEXN = lookEXN' transStruct
val lookEXNlocal = lookEXN' false

fun lookEXNinStr(STRstr{table,env,...},id,ap) =
    dconApplied(lookEXNinTable(table,id),(ap,env))
    handle Table.Notfound_Table => raise UnboundInStr "exception constructor"

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

val checkSTR = fn STRbind s => s | _ => raise Table.Next
val lookSTRinTable = Table.look checkSTR

fun lookSTRinSig id = 
    look (false,lookSTRinTable) id

fun lookSTR' deep id =
    strApplied(look (deep,lookSTRinTable) id)
val lookSTR = lookSTR' transStruct
val lookSTRlocal = lookSTR' false

fun lookSTRinStr(STRstr{table,env,...},id,ap) =
    strApplied(lookSTRinTable(table,id),(ap,env))
    handle Table.Notfound_Table => raise UnboundInStr "structure"

fun bindSTR(id: symbol, s: structureVar) = add(id,STRbind s)


(* functors *)

val lookFCTinTable = Table.look(fn (FCTbind fv) => fv | _ => raise Table.Next)

fun lookFCT id = 
    let val (fv,_) = look (true,lookFCTinTable) id in fv end 
    handle Unbound =>
      condemn("unbound functor identifier: " ^ Symbol.name id)

fun bindFCT(id: symbol, f: functorVar) = (add(id,FCTbind f); f)


(* fixity bindings *)

fun lookFIX id = 
    let val (fix,_) = 
	  look (transStruct, Table.look(fn FIXbind f => f | _ => raise Table.Next))
	       id
     in fix
    end
    handle Unbound => NONfix

fun bindFIX(id: symbol, f: fixity) = add(id, FIXbind f)

(* lookup using symbolic path *)
fun lookPath(spath as first::rest,
             lookLast: Structure * symbol * Access.path -> 'a) : 'a =
    let fun getStr([id],str,ap) = 
	     (lookLast(str,id,ap)
	      handle UnboundInStr sort =>
	        (complain("unbound "^sort^": "^name id);
		 prstr "  ending path: "; printSequence "." printSym spath;
		 newline();
		 raise Syntax))
	  | getStr(id::rest,STRstr{table,env={s,...},...},ap) =
	      let val STRvar{access=SLOT n,binding,...} = 
		      lookSTRinTable(table,id)
		      handle Table.Notfound_Table =>
		      (complain("unbound intermediate structure: " ^ name id);
		       prstr "  in path: "; printSequence "." printSym spath;
		       newline();
		       raise Syntax)
	       in getStr(rest,
		  	 (case binding of INDstr i => s sub i | _ => binding),
			 n::ap)
	      end
	val STRvar{access=PATH(ap),binding,...} =
	      lookSTR first
	      handle Unbound => 
	        (complain("unbound head structure: " ^ name first);
		 prstr "  in path: "; printSequence "." printSym spath;
		 newline();
		 raise Syntax)
     in getStr(rest,binding,ap)
    end


(* collecting stale lvars -- used by Invoke.getvars *)

fun getWHAT extract id =
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

fun binderGt((id1,FIXbind(_)),(id2,FIXbind(_))) =
      name(id1) > name(id2)
  | binderGt((_,FIXbind(_)),_) = true
  | binderGt(_,(_,FIXbind(_))) = false
  | binderGt((id1,VARbind(_)),(id2,VARbind(_))) =
      name(id1) > name(id2)
  | binderGt((_,VARbind(_)),_) = true
  | binderGt(_,(_,VARbind(_))) = false
  | binderGt((id1,CONbind(_)),(id2,CONbind(_))) =
      name(id1) > name(id2)
  | binderGt((_,CONbind(_)),_) = true
  | binderGt(_,(_,CONbind(_))) = false
  | binderGt((id1,TYCbind(_)),(id2,TYCbind(_))) = 
      name(id1) > name(id2)
  | binderGt((_,TYCbind(_)),_) = true
  | binderGt(_,(_,TYCbind(_))) = false
  | binderGt((id1,STRbind(_)),(id2,STRbind(_))) =
      name(id1) > name(id2)
  | binderGt((_,STRbind(_)),_) = true
  | binderGt(_,(_,STRbind(_))) = false
  | binderGt((id1,FCTbind(_)),(id2,FCTbind(_))) =
      name(id1) > name(id2)
  | binderGt((_,FCTbind(_)),_) = true
  | binderGt(_,(_,FCTbind(_))) = false
  | binderGt((id1,SIGbind(_)),(id2,SIGbind(_))) =
      name(id1) > name(id2)


fun build (iter: (unit -> binder * info) -> 'a) () : 'a =
    let val sorttree = BinSort.mkSortTree(): (binder * info) BinSort.tree ref
	fun gt((binder1,_),(binder2,_)) = binderGt(binder1,binder2)
     in collectTable(BinSort.insert(gt,sorttree));
        iter(BinSort.generator sorttree)
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

fun iterStr (gen : unit -> binder * info) :  trans list * symtable =
    let val newtable = Table.new(): binding Table.table
	fun fill (count) =
	    let val element as ((sym,binding),_) = gen()
	     in case element
		  of ((id, VARbind(var as VALvar{access,name,vtype})),(path,env)) =>
		       (Table.add(newtable,
		         (id,VARbind(
			      case access
			       of INLINE(_) => var
				| _ =>
				    VALvar{access = SLOT count,
					   vtype = ref(typeInContext(!vtype,env)),
					   name = name})));
			VALtrans(extendPath(access,path))::fill(count+1))
		   | ((id, CONbind(exn as DATACON{rep=VARIABLE(access),...})),
		      (path,env)) =>
		       (Table.add(newtable,
				  (id,CONbind(dconInStr(exn,env,count))));
			VALtrans(extendPath(access,path))::fill(count+1))
		   | ((id, STRbind(STRvar{name,access,binding})),(path,{s,...})) =>
		       (Table.add(newtable,
			 (id,STRbind(STRvar{name=name,
					    binding=(case binding
						       of INDstr i => s sub i
							| _ => binding),
					    access=SLOT(count)})));
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
    let val newtable = Table.new() : binding Table.table
	fun fill (count) =
	     (case gen()
	       of ((id, VARbind(VALvar{name,vtype,...})),_) =>
	            let val vb = VARbind(VALvar{access=SLOT(count),
				 		name=name,vtype=vtype})
		     in Table.add(newtable,(id,vb));
		        vb::fill(count+1)
		    end
	        | ((id, CONbind(DATACON{name,const,typ,sign,rep=VARIABLE _})),_) =>
		    let val eb = CONbind(DATACON{name = name, const = const,
					         typ = typ, sign = sign,
					         rep = VARIABLE(SLOT count)})
		     in Table.add(newtable,(id,eb));
			eb::fill(count+1)
		    end
	        | ((id, STRbind(STRvar{name,access,binding})),_) =>
		    let val sb = STRbind(STRvar{name=name,
				                binding=binding,
						access=SLOT(count)})
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
    let val newtable = Table.new() : binding Table.table
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
			    then let val p = rev(i::path)
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
						 handle Intmap.Intmap =>
						   search(stamp,tyc));
	            app abstractType args)
	        | POLYty{tyfun=TYFUN{body,...},...} => abstractType body
	        | VARty _ => ()
		| _ => impossible "abstractType"
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
				     of INLINE _ => 
					  var  (* ground type *)
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
     boundTyvars := [];
     tyvarsMode := EXP)

end (* functor EnvAccess *)

