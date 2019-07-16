(* Copyright 1996 by AT&T Bell Laboratories *)
(* prim.sml *)

signature PRIM_ENV = 
sig 

  val primEnv : StaticEnv.staticEnv

end (* signature PRIM_ENV *)


structure PrimEnv : PRIM_ENV = 
struct

local
  structure S = Symbol
  structure M = Modules
  structure B = Bindings
  structure SP = SymPath
  structure IP = InvPath
  structure SE = StaticEnv
  structure EE = EntityEnv

  structure TU = TypesUtil
  structure MU = ModuleUtil

  structure ST = Stamps
  structure P = PrimOp
  structure V = VarCon

  structure A = Access
  structure II = InlInfo

  open Types BasicTypes Fixity
in

fun mkTycElement (name: string, tyc) = 
     (S.tycSymbol name, M.TYCspec{entVar=ST.special name, spec=tyc, scope=0})

(* 
 * Note: this function only applies to constructors but not exceptions;
 * exceptions will have a non-trivial slot number 
 *)
fun mkConElement (name, d) = 
    (S.varSymbol name, M.CONspec{spec=d, slot=NONE})

(* primTypes structure *)
val primTypes =
  let val primTycs =
            [("bool", boolTycon),
             ("list", listTycon),
             ("ref", refTycon),
             ("unit", unitTycon),
             ("int", intTycon),
             ("int32", int32Tycon),
             ("real", realTycon),
             ("word", wordTycon),
             ("word8", word8Tycon),
             ("word32", word32Tycon),
             ("cont", contTycon),
             ("control_cont", ccontTycon),
             ("array", arrayTycon),
             ("vector", vectorTycon),
             ("object", objectTycon),
             ("c_function", c_functionTycon),
             ("word8array", word8arrayTycon),
             ("real64array", real64arrayTycon),
             ("spin_lock", spin_lockTycon),
             ("string", stringTycon),
             ("char", charTycon),
             ("exn", exnTycon),
             ("frag", fragTycon)]

      val primCons = 
            [("true", trueDcon),
             ("false", falseDcon),
             ("::", consDcon),
             ("nil", nilDcon),
             ("ref", refDcon),
             ("QUOTE", QUOTEDcon),
             ("ANTIQUOTE", ANTIQUOTEDcon)]

      val tycElements = map mkTycElement primTycs
      val conElements = map mkConElement primCons
		 

      val allElements = tycElements@conElements
      val allSymbols = map #1 allElements

      val entities = foldr (fn ((_,M.TYCspec{spec,entVar,scope}),r) =>
			         EE.bind(entVar,M.TYCent spec,r)) 
                          EE.empty tycElements

      val entities = EntityEnv.mark(fn _ => ST.special"primEntEnv", entities)

   in M.STR{sign=M.SIG{name=SOME(S.sigSymbol "PRIMTYPES"), closed=true,
		       fctflag=false, stamp=ST.special "PrimTypesSig",
                       symbols=allSymbols,elements=allElements,
		       typsharing=nil,strsharing=nil,
		       boundeps=ref (SOME []), lambdaty=ref(NONE)},
	    rlzn={stamp=ST.special "PrimTypesStr",
		  entities=entities,
		  lambdaty=ref NONE, 
		  rpath=IP.IPATH[S.strSymbol "primTypes"]},

	    access=A.nullAcc, info=II.mkStrInfo []}

  end (* primTypes *)


(* uList structure *)
val uList =
  let val ev = ST.special "uListVar"
      val allElements = 
            [(S.tycSymbol "list", M.TYCspec{spec=ulistTycon,entVar=ev,scope=0}),
              mkConElement("nil", unilDcon),
              mkConElement("::", uconsDcon)]
      val allSymbols = map #1 allElements

   in M.STR{sign=M.SIG{name=NONE, closed=true, 
                       fctflag=false, stamp=ST.special "uListSig",
                       symbols=allSymbols, elements=allElements,
                       typsharing=nil, strsharing=nil,
                       boundeps=ref (SOME []), lambdaty=ref NONE}, 

            rlzn={stamp=ST.special "uListStr",
                  entities=EE.bind(ev,M.TYCent ulistTycon,EE.empty),
                  lambdaty=ref(NONE),
                  rpath=IP.IPATH[S.strSymbol "uList"]},

            access=A.nullAcc, info=II.mkStrInfo[]}
  end

(* inLine structure *)
val inLine =
  let val bottom = POLYty{sign=[false], tyfun=TYFUN{arity=1,body=IBOUND 0}}

      (*
       * Using bottom here is a major gross hack. In the future, the actual
       * type should be given in the P.allPrimops list. Right now, only
       * polymorphic primOps are given the type --- to order to support
       * the type-based analysis correctly. (ZHONG)
       *)

      fun mkVarElement((name, p, tyOp),(symbols,elements,dacc,offset)) =
        let val s = S.varSymbol name
            val t = case tyOp of NONE => bottom
                               | SOME x => x
            val sp = M.VALspec{spec=t, slot=offset}
            val d = II.mkPrimInfo(p, tyOp)
         in (s::symbols, (s,sp)::elements, d::dacc, offset+1)
        end
      
      val (allSymbols, allElements, infList, _) = 
            foldl mkVarElement ([],[],[],0) P.allPrimops

      val (allSymbols, allElements, infList) = 
            (rev allSymbols, rev allElements, rev infList)

   in M.STR{sign=M.SIG{name=NONE, closed=true, 
                       fctflag=false, stamp=ST.special "inLineSig",
                       symbols=allSymbols, elements=allElements,
                       typsharing=nil, strsharing=nil,
                       boundeps=ref (SOME []), lambdaty=ref NONE},

            rlzn={stamp=ST.special "inLineStr", entities=EE.empty,
                  lambdaty=ref(NONE),
                  rpath=IP.IPATH[S.strSymbol "inLine"]},

	    access=A.nullAcc, info=(II.mkStrInfo infList)}
  end

(* priming structures: PrimTypes and InLine *)
val nameofPT = S.strSymbol "PrimTypes"
val nameofUL = S.strSymbol "UnrolledList"
val nameofIL = S.strSymbol "InLine"

val primEnv =
      SE.bind(nameofIL,B.STRbind inLine,
          SE.bind(nameofUL,B.STRbind uList,
  	     SE.bind(nameofPT,B.STRbind primTypes,
                MU.openStructure(SE.empty,primTypes))))


end (* local *)
end (* structure PrimEnv *)



(*
 * $Log: prim.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:45  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.4  1997/12/03 21:22:07  dbm
 *   Fix for Word8Array.array equality problem (basis/tests/word8array.sml,
 *   test1).
 *   Added primitive types object, c_function, word8array, real64array,
 *   and spin_lock.
 *
 * Revision 1.3  1997/08/22  18:34:59  george
 *   Add the fctflag field to the primEnv signatures --zsh
 *
 * Revision 1.2  1997/07/15  16:07:41  dbm
 *   Adjust to change in representation of signatures and addition of
 *   scope field to TYCspec.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:36  george
 *   Version 109.24
 *
 *)
