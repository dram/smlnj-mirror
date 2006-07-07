(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* moduleutil.sml *)

structure ModuleUtil : MODULEUTIL =
struct

local structure S   = Symbol
      structure SP  = SymPath
      structure IP  = InvPath
      structure CVP = ConvertPaths
      structure EP  = EntPath
      structure EPC = EntPathContext
      structure A   = Access
      structure T   = Types
      structure TU  = TypesUtil
      structure V   = VarCon
      structure B   = Bindings
      structure EE  = EntityEnv
      structure ST  = Stamps
      structure M   = Modules
      structure MI  = ModuleId
      structure SE  = StaticEnv
      structure POI = PrimOpId
      open Modules
in

(* debugging hooks *)
val say = Control_Print.say
val debugging = ElabDataControl.mudebugging (* ref false *)
fun debugmsg (msg: string) =
      if !debugging then (say msg; say "\n") else ()

fun bug s = ErrorMsg.impossible ("ModuleUtil: " ^ s)

(* 
 * Look up the entity corresponding to a given symbol in the `elements'
 * of a signature and the corresponding `entities' from a structure
 * realization.  The (dynamic) access fields of structures and
 * functors are adjusted before they are returned.  The static accesses
 * of types, structures, and functors are just returned.
 *
 * Used by the (structure and functor) matching functions.
 *)

exception Unbound of S.symbol

fun getSpec (elements, sym) = 
  let fun h [] = (debugmsg("$getSpec "^S.name sym); raise (Unbound sym))
        | h ((s, sp) :: elemr) = if S.eq(s, sym) then sp else h elemr
   in h elements
  end

(* 
 * the following might be used to speedup the signature lookup process 
 *
 * fun getSpec (elements, sym) = 
 *   Env.look(elements,sym)
 *   handle Env.Unbound => raise (Unbound sym)  
 *
 * we'll use more efficient represntations for elements in the future.
 *)  

(*** return the entity variable of a particular spec ***)
fun getSpecVar (STRspec{entVar,...}) = SOME entVar
  | getSpecVar (TYCspec{entVar,...}) = SOME entVar
  | getSpecVar (FCTspec{entVar,...}) = SOME entVar
  | getSpecVar _ = NONE

(*** The function getTyc is used in modules/sigmatch.sml only ***)
fun getTyc (elements, entEnv, sym) =
   case getSpec (elements, sym)
    of TYCspec{entVar,...} => (EE.lookTycEnt(entEnv,entVar), entVar)
     | _ => bug "getTyc: wrong spec"

(*** The function getStr is used in modules/sigmatch.sml only ***)
fun getStr (elements, entEnv, sym, dacc, dinfo) =
   case getSpec(elements, sym)
    of STRspec{sign, slot, def, entVar} => 
        (case EE.look(entEnv,entVar)
 	  of STRent entity => 
               (STR{sign = sign, rlzn = entity, access = A.selAcc(dacc,slot),
                    prim = POI.selStrPrimId (dinfo, slot)},
		entVar)
	   | _ => bug "getStr: bad entity")
     | _ => bug "getStr: wrong spec"

(*** The function getFct is used in modules/sigmatch.sml only ***)
fun getFct (elements, entEnv, sym, dacc, dinfo) =
   case getSpec(elements, sym)
    of FCTspec{sign, slot, entVar} => 
        (case EE.look(entEnv,entVar)
          of FCTent entity => 
               (FCT{sign = sign, rlzn = entity, access = A.selAcc(dacc,slot),
                    prim = POI.selStrPrimId (dinfo, slot)},
		entVar)
           | _ => bug "getFct: bad entity")
     | _ => bug "getFct: wrong spec"

val errorStrStamp = ST.special "ERRORstr"
val errorStrName = InvPath.IPATH[S.strSymbol "ERRORstr"]

fun getStrStamp(STR { rlzn = {stamp, ...}, ... }) = stamp
  | getStrStamp ERRORstr = errorStrStamp
  | getStrStamp _ = bug "getStrStamp"

fun getStrName(STR { rlzn = {rpath,...}, ... }) = rpath
  | getStrName ERRORstr = errorStrName
  | getStrName _ = bug "getStrName"

fun getStrs (STR { sign = SIG sg, rlzn = {entities,...}, access,prim,...}) =
    let val elements = #elements sg
    in
	List.mapPartial
	    (fn (sym,STRspec{sign,slot,def,entVar}) =>
		SOME(STR{sign = sign,
			 rlzn = EE.lookStrEnt(entities,entVar),
			 access = A.selAcc(access, slot), 
			 prim = POI.selStrPrimId (prim, slot)})
	      | _ => NONE)
	    elements
    end
  | getStrs ERRORstr = nil
  | getStrs _ = bug "getStrs"

fun getTycs (STR { sign = SIG sg, rlzn = {entities,...}, ... }) =
    let val elements = #elements sg
	val tycvars = List.mapPartial
                          (fn (sym,TYCspec{entVar,...}) => SOME entVar
			    | _ => NONE)
			  elements
    in
	List.map (fn tycVar => EE.lookTycEnt(entities,tycVar)) tycvars
    end
  | getTycs ERRORstr = nil
  | getTycs _ = bug "getTycs (2)"

fun getSigSymbols(SIG {symbols,...}) = symbols
  | getSigSymbols _ = nil

fun getStrSymbols(STR { sign, ... }) = getSigSymbols sign
  | getStrSymbols _ = nil

(*** Translate a tycon in a given entityEnv ***)
fun transTycon entEnv (T.PATHtyc{entPath,path,...}) =
      (EE.lookTycEP(entEnv,entPath)
	handle EE.Unbound =>
	  (debugmsg (String.concat["$transTycon ",
				   IP.toString path," ",
				   EP.entPathToString entPath]);
	   raise EE.Unbound))
  | transTycon _ tycon = tycon      


(*
 * Translate a type in a given entityEnv 
 *
 * We should never need to recurse inside each DEFtyc's body because
 * a DEFtycs is either rigid or has been relativized as a whole into 
 * a PATHtyc with an entPath somewhere before.
 *)
fun transType entEnv ty =
      TU.mapTypeFull (transTycon entEnv) ty
      handle EE.Unbound => (debugmsg "$transType"; raise EE.Unbound)

(*
val transTyconPhase = (Stats.makePhase "Compiler 033 4-transTycon") 
val transTycon = 
  fn x => fn y => (Stats.doPhase transTyconPhase (transTycon x) y)

val transTypePhase = (Stats.makePhase "Compiler 033 5-transType") 
val transType = 
  fn x => fn y => (Stats.doPhase transTypePhase (transType x) y)
*)

fun strDefToStr(CONSTstrDef str, _) = str
  | strDefToStr(VARstrDef(sign,entPath), entEnv) =
    STR{sign=sign,rlzn=EE.lookStrEP(entEnv,entPath),
        access=A.nullAcc, prim=POI.StrE []}

(* 
 * two pieces of essential structure information gathered during
 * the environment lookup. SIGINFO is returned if the structure
 * being searched is a STRSIG; otherwise it return STRINFO.
 *)
datatype strInfo = SIGINFO of EP.entPath  (* reverse order! *)
                 | STRINFO of strEntity * A.access * POI.strPrimElem

val bogusInfo = STRINFO (bogusStrEntity, A.nullAcc, POI.StrE [])

fun getStrElem (sym, sign as SIG {elements,...}, sInfo) = 
      (case getSpec (elements,sym)
        of STRspec{sign=subsig, slot, def, entVar} =>
            (let val newInfo = 
                  case sInfo
                   of SIGINFO ep => SIGINFO (entVar::ep)
                    | STRINFO ({entities,...}, dacc, dinfo) =>
                      STRINFO(EE.lookStrEnt(entities,entVar), 
                              A.selAcc(dacc,slot), POI.selStrPrimId (dinfo, slot))
              in (subsig, newInfo)
             end)
         | _ => bug "getStrElem: wrong spec case")

  | getStrElem (sym, sign, _) = (sign, bogusInfo)

fun getFctElem (sym, sign as SIG {elements,...},
	       sinfo as STRINFO(rlzn as {entities,...}, dacc, dinfo)) = 
      (case getSpec(elements, sym)
        of FCTspec{sign=subfsig, entVar, slot} =>
             FCT{sign=subfsig, rlzn=EE.lookFctEnt(entities,entVar),
                 access=A.selAcc(dacc, slot),
		 prim=POI.selStrPrimId (dinfo, slot)}
         | _ => bug "mkFctVar - bad spec")

  | getFctElem _ = ERRORfct

fun mkTyc (sym, sp, SIG {elements,...}, sInfo) =
      (case getSpec (elements, sym)
        of TYCspec{spec,entVar=ev,repl,scope} => 
             (case sInfo
               of SIGINFO ep => 
                    T.PATHtyc{arity=TU.tyconArity spec, entPath=rev(ev::ep),
			      path=CVP.invertSPath sp}
                | STRINFO ({entities,...}, _, _) =>
		  EE.lookTycEnt(entities, ev))
 
         | _ => bug "mkTyc: wrong spec case")

  | mkTyc _ = T.ERRORtyc

fun mkVal (sym, sp, sign as SIG {elements,...},
	  sInfo as STRINFO({entities,...}, dacc, dinfo)) : V.value =
    (case getSpec(elements, sym) of
	 VALspec{spec,slot} =>
         V.VAL(V.VALvar{access = A.selAcc(dacc,slot), 
			prim = POI.selValPrimFromStrPrim (dinfo, slot),
			path = sp,
			typ = ref(transType entities spec)})
       | CONspec{spec=T.DATACON{name, const, typ, rep, sign, lazyp},
		 slot} =>
         let val newrep =
                 case (rep, slot)
                  of (A.EXN _, SOME i) => A.EXN (A.selAcc(dacc,i))
                   | _ => rep
			     
         in
	     V.CON(T.DATACON{rep=newrep, name=name,
                             typ=transType entities typ, 
                             const=const, sign=sign, lazyp=lazyp})
         end
       | _ => bug "mkVal: wrong spec")
  | mkVal _ = V.VAL(V.ERRORvar)

fun mkStrBase (sym, sign, sInfo) = 
  let val (newsig, newInfo) = getStrElem (sym, sign, sInfo)
   in case newsig
       of ERRORsig => ERRORstr
	| _ =>
	  (case newInfo
	     of STRINFO(newrlzn, newacc, newinfo) => 
		STR{sign=newsig, rlzn=newrlzn, access=newacc,
		    prim=newinfo}
	      | SIGINFO ep => STRSIG{sign=newsig, entPath=rev ep})
  end

fun mkStr (sym, _, sign, sInfo) = mkStrBase (sym, sign, sInfo)

fun mkStrDef (sym, _, sign, sInfo) = 
  let val (newsig, newInfo) = getStrElem (sym, sign, sInfo)
   in case newsig
        of ERRORsig => CONSTstrDef ERRORstr
	 | _ =>
	   (case newInfo
	      of STRINFO (newrlzn, newacc, newinfo) => 
		  CONSTstrDef(STR{sign=newsig, rlzn=newrlzn,
				  access=newacc, prim=newinfo})
	       | SIGINFO ep => VARstrDef(newsig, rev ep))
  end

fun mkFct (sym, sp, sign, sInfo) = getFctElem (sym, sign, sInfo)

fun getPath makeIt (str, SP.SPATH spath, fullsp) =
  let fun loop([sym], sign, sInfo) = makeIt (sym, fullsp, sign, sInfo)
        | loop(sym::rest, sign, sInfo) = 
            let val (newsig, newsInfo) = getStrElem (sym, sign, sInfo)
             in loop(rest, newsig, newsInfo)
            end
        | loop _ = bug "getPath.loop"

   in case str 
       of STR { sign, rlzn, access, prim } =>
          loop(spath, sign, STRINFO(rlzn, access, prim))
        | STRSIG{sign, entPath} => 
            loop(spath, sign, SIGINFO (rev entPath))
        | _ => loop(spath, ERRORsig, bogusInfo)
  end

val getTycPath : M.Structure * SP.path * SP.path -> T.tycon =
      getPath mkTyc
val getValPath : M.Structure * SP.path * SP.path -> V.value =
      getPath mkVal
val getStrPath : M.Structure * SP.path * SP.path -> M.Structure =
      getPath mkStr
val getFctPath : M.Structure * SP.path * SP.path -> M.Functor =
      getPath mkFct
val getStrDef : M.Structure * SP.path * SP.path -> M.strDef =
      getPath mkStrDef

fun checkPathSig (sign: M.Signature, spath: SP.path) : S.symbol option =
    let val str = STRSIG{sign=sign,entPath=[]:EP.entPath}
        fun checkLast _ (sym,_,SIG {elements,...},_) =
	    (getSpec(elements,sym); ())
          | checkLast _ (sym,_,ERRORsig,_) = ()
     in getPath checkLast (str,spath,SP.empty);
	NONE
    end
    handle Unbound sym => SOME sym

fun errBinding sym =
    case S.nameSpace sym
     of S.VALspace => B.VALbind V.ERRORvar
      | S.TYCspace => B.TYCbind T.ERRORtyc
      | S.STRspace => B.STRbind M.ERRORstr
      | S.FCTspace => B.FCTbind M.ERRORfct
      | _ => raise (Unbound sym)

fun eqSign (SIG { stamp = s1, closed = true, ... },
	    SIG { stamp = s2, closed = true, ... }) = ST.eq (s1, s2)
  | eqSign _ = false

fun eqOrigin(STR s1, STR s2) = ST.eq (#stamp (#rlzn s1), #stamp (#rlzn s2))
  | eqOrigin _ = false

(* 
 * The following functions are used in CMStaticEnv and module elaboration
 * for building EntPathContexts.  They extract module ids from modules. 
 *)
val tycId = MI.tycId'

fun strId (STR sa) = MI.strId sa
  | strId _ = bug "strId"

fun strId2(SIG sa, rlzn : strEntity) = MI.strId2 (sa, rlzn)
  | strId2 _ = bug "strId2"

fun fctId (FCT fa) = MI.fctId fa
  | fctId _ = bug "fctId"

fun fctId2(sign, rlzn : fctEntity) = MI.fctId2 (sign, rlzn)

(*
 * The reason that relativizeType does not need to get inside 
 * DEFtyc is because of our assumptions that the body in DEFtyc
 * has already been relativized, when DEFtyc is elaborated; 
 * otherwise, this DEFtyc must be a rigid tycon.
 *)
fun relativizeTyc epContext : T.tycon -> T.tycon * bool = 
    let fun stamped tyc = let
	    val tyc_id = MI.tycId' tyc
	in
	    (* debugmsg ("mapTyc: "^ModuleId.idToString tyc_id); *)
	    case EPC.lookTycPath(epContext,tyc_id)
	     of NONE => (debugmsg "tyc not mapped 1"; (tyc,false))
	      | SOME entPath =>
		let val tyc' = T.PATHtyc{arity=TU.tyconArity tyc,
					 entPath=entPath,
					 path=TU.tycPath tyc}
		in
		    debugmsg("tyc mapped: "^
			     Symbol.name(TypesUtil.tycName tyc'));
		    (tyc',true)
		end
	end

	fun mapTyc (tyc as (T.GENtyc _ | T.DEFtyc _)) = stamped tyc
	  | mapTyc(tyc as T.PATHtyc _) =
             (* assume this is a local tycon within the current signature *)
	     (debugmsg "tyc not mapped 2";
	      (tyc,true))
	  | mapTyc tyc = (debugmsg "tyc not mapped 3"; (tyc,false))

	fun mapTyc' tyc = 
	    (debugmsg("mapTyc': "^(Symbol.name(TypesUtil.tycName tyc)));
	     mapTyc tyc)
    in mapTyc'
    end
        
fun relativizeType epContext ty : T.ty * bool =
    let val relative = ref false
	fun vizTyc tyc = 
	    let val (tyc',rel) = relativizeTyc epContext tyc
	     in relative := (!relative orelse rel);
		tyc'
	    end
     in (TU.mapTypeFull vizTyc ty, !relative)
    end


(*
val relativizeTypePhase = (Stats.makePhase "Compiler 033 2-vizType") 
val relativizeType = 
  fn x => fn y =>
   (Stats.doPhase relativizeTypePhase (relativizeType x) y)

*)

(* 
 * getBinding(sym,str): return binding for element sym of structure str
 *  - used only inside the function openStructure
 *  - raises ModuleUtil.Unbound if sym not found in sig 
 *)
fun getBinding (sym, str as STR st) =
    (case st of
	 {sign as SIG _, rlzn, access=dacc, prim=dinfo} =>
	 let val sinfo = STRINFO(rlzn, dacc, dinfo)
	     val entities = #entities rlzn
	 in
	     case S.nameSpace sym
	      of S.VALspace => 
		 (case mkVal (sym, SP.SPATH[sym], sign, sinfo)
                   of V.VAL v => B.VALbind v
		    | V.CON d => B.CONbind d)
	       | S.TYCspace =>
		 B.TYCbind (mkTyc (sym, SP.SPATH[sym], sign, sinfo))
	       | S.STRspace => B.STRbind (mkStrBase (sym, sign, sinfo))
	       | S.FCTspace => B.FCTbind (getFctElem (sym, sign, sinfo))
	       | sp => (debugmsg ("getBinding: "^S.symbolToString sym);
  			raise (Unbound sym))
	 end
       | { sign = ERRORsig, ... } => errBinding sym)
  | getBinding (sym, STRSIG{sign as SIG _,entPath=ep}) = 
     let val sinfo = SIGINFO(rev ep)
     in
	 case S.nameSpace sym
          of S.TYCspace =>
	     B.TYCbind (mkTyc (sym, SP.SPATH[sym], sign, sinfo))
           | S.STRspace => B.STRbind (mkStrBase (sym, sign, sinfo))
           | _ => (debugmsg ("getBinding: "^S.symbolToString sym);
                   raise (Unbound sym))
     end 
  | getBinding (sym, ERRORstr) = errBinding sym
  | getBinding _ = bug "getBinding - bad arg"

fun openStructure (env: SE.staticEnv, str) =
  let fun look sym =
	  getBinding (sym,str) handle Unbound _ => raise SE.Unbound
      val symbols = getStrSymbols str
      val genSyms = (fn () => symbols)
      val nenv = SE.special(look, genSyms)
   in SE.atop(nenv,env)
  end

(** Get a strPrimElem with all the primIds found in 
    a list of bindings 
 
    Used in Elaborator/elaborate/elabmod.sml and 
    SigMatch
 *)
fun strPrimElemInBinds [] = POI.StrE [] 
  | strPrimElemInBinds (bind::rest) =
    let
	val strPrims =
	   (case bind 
	     of B.STRbind (M.STR { prim, ... }) => prim 
	      | B.FCTbind (M.FCT { prim, ... }) => prim
	      | B.VALbind (V.VALvar {prim, ...}) => POI.PrimE prim
	      | B.CONbind _ => POI.PrimE POI.NonPrim
	      | _  => 
		  bug "unexpected binding in strPrimElemInBinds")
    in  
       (case (strPrimElemInBinds rest) of
	    (POI.StrE restPrims) =>
	      POI.StrE (strPrims :: restPrims)
	  | POI.PrimE id => POI.StrE ([POI.PrimE id]))
    end (* let *)

(* extract all signature names from a structure --
 *  doesn't look into functor components *)
fun getSignatureNames s = let
    fun fromSig sign = let
	fun sigNames(SIG {name,elements,...}, names) =
	    foldl (fn ((_,STRspec{sign,...}),ns) =>
		      sigNames(sign, ns)
		    | (_,ns) => ns)
		  (case name of SOME n => n::names | NONE => names)
		  elements
	  | sigNames(ERRORsig,names) = names
	fun removeDups (x::(rest as y::_),z) = 
	    if S.eq(x,y) then removeDups(rest,z) else removeDups(rest,x::z)
	  | removeDups (x::nil,z) = x::z
	  | removeDups (nil,z) = z
    in removeDups(ListMergeSort.sort S.symbolGt(sigNames(sign,nil)), nil)
    end
in
    case s of
	STR { sign, ... } => fromSig sign
      | STRSIG { sign, ... } => fromSig sign
      | ERRORstr => nil
end
end (* local *)
end (* structure ModuleUtil *)

