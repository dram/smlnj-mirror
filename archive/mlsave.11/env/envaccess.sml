(* envaccess.sml *)

(* lookup and binding functions *)

structure EnvAccess : ENVACCESS = struct

structure Access = Access
structure Basics = Basics
structure Env = Env

local
  open ErrorMsg
  open PrintUtil
  open Access
  open Basics
  open Basics.Symbol
  open Absyn
  open Env
  open BasicTypes
in

(* debugging flags *)
val debugPatch = ref false
val debugBind = ref false
val debugBuild = ref false


val pervasives = ref (nil : (info * symtable) list)

(* val lookBase : (symtable * Table.Symbol.symbol -> 'a)
		      -> Table.Symbol.symbol -> 'a * info *)
fun lookBase (tblSearch) id = 
    let fun f ((info,tbl)::r) = ((f r) handle Table.Notfound_Table => 
					        (tblSearch(tbl,id),info))
	  | f nil = raise Table.Notfound_Table
     in f (!pervasives)
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
    handle Unbound _ => condemn("unbound tycon: " ^ name id)

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


(* constructors *)

val checkCON = fn CONbind c => c | _ => raise Table.Next

fun dconApplied(dc as DATACON{name,const,vtype,rep,dcons},(ap,env)) : datacon =
    DATACON{name = name, const = const, dcons=dcons,
            rep = (case !rep
		     of VARIABLE(SLOT n) => ref(VARIABLE(PATH(n::ap)))
		      | VARIABLE(LVAR v) => ref(VARIABLE(PATH [v]))
		      | _ => rep),  (* defined exception *)
            vtype = typeInContext(vtype,env)}

fun dconInStr(dc as DATACON{name,const,vtype,rep,dcons},env,slotNo) : datacon =
    DATACON{name = name, const = const, dcons = dcons,
            rep = (case !rep
		     of VARIABLE(access) => ref(VARIABLE(SLOT slotNo))
		      | _ => rep),
            vtype = typeInContext(vtype,env)}

(* BUG? -- loss of identity of exception on thinning because of copied ref??? *)
fun exnInSig(DATACON{name,const,vtype,dcons,...},slot) =
    DATACON{name = name, const = const, vtype = vtype, dcons = dcons,
            rep = ref(VARIABLE(SLOT slot))}

val lookCONinTable = Table.look checkCON

fun lookCON id = 
    dconApplied(look(false,lookCONinTable) id)

fun lookCONinStr(STRstr{table,env,...},id,ap): datacon =
    dconApplied(lookCONinTable(table,id),(ap,env))
    handle Table.Notfound_Table => condemn("unbound data constructor in str: " ^ name(id))

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

val equalref = ref ~10
val notequalref = ref ~10
val assignref = ref ~10
val updateref = ref ~10

fun varApplied(v:var, (ap:Access.path, env:strenv)) : var =
    case v
      of VALvar{access,name,vtype} =>
	   VALvar{access =
		   (case access
		      of SLOT(n) => PATH(n::ap)
		       | LVAR(n) => PATH([n])
		       | INLINE _ => access
		       | PATH _ => impossible "varApplied: access = PATH"),
		  vtype = (case (access,ap)
			    of (INLINE i, _) => 
			        if i= !equalref orelse i= !notequalref
					    then ref(newEqualityType())
			        else if i= !assignref then ref(newAssignType())
				else if i= !updateref then ref(newUpdateType())
				else vtype
			     | (_, nil) => vtype
			     | _ => ref(typeInContext(!vtype,env))),
		  name = name}
       | _ => v  (* error?  -- what about OVLDvar bindings? *)

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
     VARbind(VALvar{access=LVAR(mkLvar()), name=id, vtype=ref UNKNOWNty}))

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
    (case lookLocalRec (lookVARCONinTable, SOME lookCONinTable') id
      of (VARbind v, info) => VARbind(varApplied(v,info))
       | (CONbind d, info) => CONbind(dconApplied(d,info))
       | _ => impossible "envaccess.228")
    handle Unbound _ => unboundVAR id

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
  	  handle Unboundrec _ => 
		    patchVars(pl',varRef::tl)  (* not yet bound; try later *)
               | Unbound _ => (* no more rec layers *)
		   let val VARbind v = unboundVAR id
		    in varRef := v; patchVars(pl',tl)
		   end
	       | Patched => patchVars(pl', tl)
	)
  | patchVars (nil, tl) = tl

val protectPatchList =
    ((fn () => (if !debugPatch then prstr "*enterPatch*\n" else ();
	        !varPatchList before (varPatchList := nil))),
     (fn (vpl) => (if !debugPatch then prstr "*exitPatch*\n" else ();
		   varPatchList := patchVars(!varPatchList,vpl))))
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
        let val v = mkVALvar(id,ref UNKNOWNty)
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

val boundTyvars = ref(CLOSEDbtv nil);

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
      of CLOSEDbtv tyvs =>
	   let fun find ((tyv as TYVAR{name = s,...}) :: l) =
		   if Symbol.eq(id,s)
		      then tyv
		      else find l
		 | find nil = 
		    (complain "lookTyvar -- unbound tyvar in closed scope";
		     newTyvar (INSTANTIATED UNKNOWNty))
	    in find tyvs
	   end
       | OPENbtv polyTyvars =>
	   lookTYV id
	   handle Unbound _ =>
	     let val tyv = bindTYV(id, mkTyvar(id,FIXED))
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
	    in SymbolTable.stringToSymbol(first ^ rest)
	   end
end

val lookEXNinTable = Table.look checkCON
fun lookEXNinBase id = dconApplied(lookBase lookEXNinTable id)

infix -->

fun unboundEXN id =
    (complain("unbound exn: " ^ name id);
     DATACON{name=id,const=false,vtype=UNKNOWNty-->exnTy,
     	     rep=ref UNDECIDED,dcons=exnDcons})

fun lookEXN id =
    dconApplied(look (false,lookEXNinTable) id)
    handle Unbound _ => unboundEXN id

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
    handle Unbound _ => condemn("unbound signature: " ^ name id)

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
    handle Unbound _ => condemn("unbound str: " ^ name id)

fun lookSTRinStr(STRstr{table,env,...},id,ap) =
    strApplied(lookSTRinTable(table,id),(ap,env))
    handle Table.Notfound_Table => condemn("unbound structure id in structure: " ^ name id)

fun bindSTR(id: symbol, s: structureVar) = add(id,STRbind s)


(* functors *)

val lookFCTinTable = Table.look(fn (FCTbind fv) => fv | _ => raise Table.Next)

fun lookFCT id = 
    let val (fv,_) = look(true,lookFCTinTable) id
     in fv
    end handle Unbound _ => condemn("unbound functor identifier: " ^ Symbol.name id)

fun bindFCT(id: symbol, f: functorVar) = (add(id,FCTbind f); f)


(* fixity bindings *)

fun lookFIX1 id = 
    let val (fix,_) = 
	  look(false, Table.look(fn FIXbind f => f | _ => raise Table.Next)) id
     in fix
    end

fun lookFIX id = lookFIX1 id handle Unbound _ => NONfix

fun bindFIX(id: symbol, f: fixity) = add(id, FIXbind f)


(* building structures *)

(* is this correct?  *)
fun tyconGt(DATAtyc _, _) = false
  | tyconGt(_, DATAtyc _) = true
  | tyconGt(_) = false
  (* DATAtycs come after other kinds *)

fun printBinding(VARbind _) = prstr "VARbind"
  | printBinding(CONbind _) = prstr "CONbind"
  | printBinding(TYCbind _) = prstr "TYCbind"
  | printBinding(TYVbind _) = prstr "TYVbind"
  | printBinding(SIGbind _) = prstr "SIGbind"
  | printBinding(STRbind _) = prstr "STRbind"
  | printBinding(FCTbind _) = prstr "FCTbind"
  | printBinding(FIXbind _) = prstr "FIXbind"

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
     in if !debugBuild then prstr "build -- calling collectTable\n" else ();
	collectTable(s,BinSort.insert(binderGt,sorttree));
        iter(BinSort.generator sorttree)
    end

fun extendPath(LVAR(v): access, []: path) = PATH[v] (* locally defined *)
  | extendPath(SLOT(n), p) = PATH(n::p)  (* element of opened structure *)
  | extendPath(x as PATH _, _) = x  (* defined exception *)
  | extendPath(x as INLINE _, _) = x
  | extendPath(access,path) = impossible "extendPath in envaccess"

fun iterStr (gen : unit -> binder * info) :  trans list * symtable =
    let val newtable = Table.new()
	val _ = if !debugBuild then prstr "entering iterStr\n" else ()
	fun fill (count) =
	    let val element as ((sym,binding),_) = gen()
	     in if !debugBuild
		  then (prstr "adding :"; printSym sym; prstr " ";
		        printBinding binding; newline())
	          else ();
		(case element
		  of ((id, VARbind(var as VALvar{access,...})),(path,env)) =>
		       (Table.add(newtable,
				  (id,VARbind(varInStr(var,env,count))));
			VALtrans(extendPath(access,path))::fill(count+1))
		   | ((id, CONbind(exn as DATACON{rep=ref(VARIABLE(access)),...})),
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
		       		 (id,TYCbind(ref(tyconInContext(!tyconRef,env)))));
			fill(count))
		   | ((id, CONbind(dcon)),(path,env)) =>
		       (Table.add(newtable, (id,CONbind(dconInStr(dcon,env,0))));
			fill(count))
		   | (binding,_) => (Table.add(newtable,binding); fill(count)))
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
	        | ((id, CONbind(exn as DATACON{rep=ref(VARIABLE _),...})),_) =>
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
	     handle BinSort.Finished => []
     in (fill(0),newtable)
    end

val buildSigTable = build iterSig;

(* reset state of EnvAccess *)
fun reset() =
    (varPatchList := nil;
     boundTyvars := CLOSEDbtv nil)

end (* local *)

end (* functor EnvAccess *)

