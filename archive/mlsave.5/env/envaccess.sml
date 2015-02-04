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
  open BasicTypes
in

(* debugging flag *)
val debugPatch = ref false
val debugBind = ref false
val debugBuild = ref false


val pervasives = ref (nil : (info * symtable) list)

(* val lookBase : (symtable * Table.Symbol.symbol -> 'a)
		      -> Table.Symbol.symbol -> 'a * info *)
fun lookBase (tblSearch) id = 
    let fun f ((info,tbl)::r) = (f r) handlex Table.notfound => 
					      (tblSearch(tbl,id),info)
	  | f nil = raisex Table.notfound
     in f (!pervasives)
    end

(* type constructors *)

val checkTYC: binding -> tycon ref = 
    fn (TYCbind tycref) => tycref | _ => raisex Table.next;

val lookTYCinTable = Table.look checkTYC;

fun lookTYCinStr(STRstr{table,env,...}: Structure, id: symbol) : tycon ref =
    (case lookTYCinTable(table,id)
      of ref(INDtyc [i]) => let val {t,s} = env in ref(t sub i) end
(* what about paths of length > 1? *)
       | tyc => tyc)
    handlex Table.notfound => Condemn("unbound tycon in str: " ^ Name id)

fun lookTYC id =
    (case look(false, lookTYCinTable) id
      of (tycref as ref(INDtyc[i]), (_,{t,s})) =>
	    (ref(t sub i) 
	     handlex subscript => tycref)  (* in signature *)
       | (tycref, _) => tycref)
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


(* constructors *)

val checkCON = fn CONbind c => c | _ => raisex Table.next

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
    handlex Table.notfound => Condemn("unbound data constructor in str: " ^ Name(id))

fun bindCON (id: symbol, c: datacon) =
    (add(id, CONbind c); c)


(* variables *)

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

fun varApplied(v:var, (ap:Access.path, env:strenv)) : var =
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
    (Complain ("unbound variable " ^ Name id);
     VARbind(VALvar{access=LVAR(mkLvar()), name=id, vtype=ref UNKNOWNty}))

fun lookVARinBase id = varApplied(lookBase lookVARinTable id)

fun lookVARCONinStr(STRstr{table,env,...},id,ap): binding =
    (case lookVARCONinTable(table,id)
       of VARbind(var) => VARbind(varApplied(var,(ap,env)))
	| CONbind(dcon) => CONbind(dconApplied(dcon,(ap,env)))
	| _ => Impossible "envaccess.381")
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
local val big_a = ord "A" and big_z = ord "Z" in
fun looksLikeExn id =
	let val firstchar = ordof(Symbol.Name id, 0)
	in  firstchar >= big_a andalso firstchar <= big_z
	end
end

fun checkBinding(bl : binder list) =
     app (fn (i,b) => if looksLikeExn i
		then Warn("Capitalized variable in rule (looks like exception): "
			 ^ Symbol.Name i)
			else ()) 
          bl

fun newVAR(bl: binder list ref, id: symbol) : var =
    let fun checkid ((i,b)::bl) =
	      if Symbol.Eq(i,id)
	        then Complain "repeated var in pattern"
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
	     let val tyv = bindTYV(id, mkTyvar(id,FIXED))
	      in polyTyvars := tyv :: !polyTyvars;
		 tyv
	     end;


(* exceptions *)

val lookEXNinTable = Table.look checkCON
fun lookEXNinBase id = dconApplied(lookBase lookEXNinTable id)

infix -->

fun unboundEXN id =
    (Complain("unbound exn: " ^ Name id);
     DATACON{name=id,const=false,vtype=UNKNOWNty-->exnTy,
     	     rep=ref UNDECIDED,dcons=EXNdcons})

fun lookEXN id =
    dconApplied(look (false,lookEXNinTable) id)
    handlex unbound => unboundEXN id

fun lookEXNinStr(STRstr{table,env,...},id,ap) =
    dconApplied(lookEXNinTable(table,id),(ap,env))
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

fun strApplied(STRvar{name,access,binding},(ap,{s,t})) =
    STRvar{name=name,
	   binding=(case binding of INDstr i => s sub i | _ => binding),
	   access=(case access
		     of SLOT(n) => PATH(n::ap)
		      | LVAR(n) => PATH [n]
		      | _ => Impossible "strApplied: access = PATH or INLINE")}

fun strInStr(STRvar{name,access,binding},{s,t},slot) =
    STRvar{name=name,
	   binding=(case binding of INDstr i => s sub i | _ => binding),
	   access=SLOT(slot)}

fun strInSig(STRvar{name,access,binding},slot) =
    STRvar{name=name,
	   binding=binding,
	   access=SLOT(slot)}

val checkSTR = fn STRbind s => s | _ => raisex Table.next
val lookSTRinTable = Table.look checkSTR

fun lookSTR_sig id = 
    look(false,lookSTRinTable) id

fun lookSTR id =
    strApplied(look (true,lookSTRinTable) id)
    handlex unbound => Condemn("unbound str: " ^ Name id)

fun lookSTRinStr(STRstr{table,env,...},id,ap) =
    strApplied(lookSTRinTable(table,id),(ap,env))
    handlex Table.notfound => Condemn("unbound str in str: " ^ Name id)

val lookSTRinTable = fn x => lookSTRinTable x 
	    handlex Table.notfound => 
		let val (_,id) = x in Condemn ("unbound str in str: "^Name id)
		end

fun bindSTR(id: symbol, s: structureVar) = add(id,STRbind s)


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

fun extendPath(LVAR(v): access, []: path) = PATH[v] (* locally defined *)
  | extendPath(SLOT(n), p) = PATH(n::p)  (* element of opened structure *)
  | extendPath(x as PATH _, _) = x  (* defined exception *)
  | extendPath(x as INLINE _, _) = x
  | extendPath(access,path) = Impossible "extendPath in envaccess"

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
(*		   | ((id, EXNbind(exn as DATACON{rep=ref(VARIABLE(access)),...})),
		      (path,env)) =>
		       (Table.add(newtable,
				  (id,EXNbind(dconInStr(exn,env,count))));
			VALtrans(extendPath(access,path))::fill(count+1))
*)		   | ((id, CONbind(exn as DATACON{rep=ref(VARIABLE(access)),...})),
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

