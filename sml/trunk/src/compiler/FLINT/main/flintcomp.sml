(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* flintcomp.sml *)

functor FLINTComp (structure Gen: CPSGEN
                   val collect: unit -> Word8Vector.vector) : CODEGENERATOR =
struct

local structure CB = CompBasic
      structure CGC = Control.CG
      structure MachSpec = Gen.MachSpec
      structure Convert = Convert(MachSpec)
      structure CPStrans = CPStrans(MachSpec)
      structure CPSopt = CPSopt(MachSpec)
      structure Closure = Closure(MachSpec)
      structure Spill = Spill(MachSpec)
      structure CpsSplit = CpsSplitFun (MachSpec) 
in 

val architecture = Gen.MachSpec.architecture
fun bug s = ErrorMsg.impossible ("FLINTComp:" ^ s)
val say = Control.Print.say

fun phase x = Stats.doPhase (Stats.makePhase x)

val fcollect  = phase "Compiler 052 collect" (fn le => (Collect.collect le; le))
(*  val fcontract = phase "Compiler 052 fcontract" FContract.contract *)
val lcontract = phase "Compiler 052 lcontract" LContract.lcontract 
val specialize= phase "Compiler 053 specialize" Specialize.specialize
val wrapping  = phase "Compiler 054 wrapping" Wrapping.wrapping
val reify     = phase "Compiler 055 reify" Reify.reify
(* val lambdaopt = phase "Compiler 057 lambdaopt" LContract.lcontract *)

val convert   = phase "Compiler 060 convert" Convert.convert
val cpstrans  = phase "Compiler 065 cpstrans" CPStrans.cpstrans
val cpsopt    = phase "Compiler 070 cpsopt" CPSopt.reduce
val litsplit  = phase "Compiler 075 litsplit" Literals.litsplit
val lit2cps   = phase "Compiler 076 lit2cps" Literals.lit2cps
val closure   = phase "Compiler 080 closure"  Closure.closeCPS
val globalfix = phase "Compiler 090 globalfix" GlobalFix.globalfix
val spill     = if MachSpec.spillAreaSz < 500 * MachSpec.valueSize
                then phase "Compiler 100 spill" Spill.spill
                else fn x => x
val limit     = phase "Compiler 110 limit" Limit.nolimit
val codegen   = phase "Compiler 120 cpsgen" Gen.codegen

val closureD  = phase "Compiler 081 closureD"  Closure.closeCPS
val globalfixD= phase "Compiler 091 globalfixD" GlobalFix.globalfix
val spillD    = if MachSpec.spillAreaSz < 500 * MachSpec.valueSize
                then phase "Compiler 101 spillD" Spill.spill
                else fn x => x
val limitD    = phase "Compiler 110 limitD" Limit.nolimit
val codegenD  = phase "Compiler 121 cpsgenD" Gen.codegen

(** pretty printing for the FLINT and CPS code *)
val (prF, prC) = 
  let fun prGen (flag,printE) s e =
        if !flag then (say ("\n[After " ^ s ^ " ...]\n\n"); printE e; 
                       say "\n"; e) 
        else e
   in (prGen (CGC.printFlint, PPFlint.printProg),
       prGen (CGC.printit, PPCps.printcps0))
  end

(** writing out a term into a error output file *)
fun dumpTerm (printE, s, le) =
  let val outS = TextIO.openAppend s;
      val saveOut = !Control.Print.out
      fun done () =
        (TextIO.closeOut outS; Control.Print.out := saveOut)
   in Control.Print.out := {say = fn s => TextIO.output(outS,s),
                            flush = fn () => TextIO.flushOut outS};
      printE le handle x => (done () handle _ => (); raise x);
      done ()
  end (* function dumpTerm *)

(** compiling FLINT code into the binary machine code *)
fun flintcomp(flint, compInfo as {error, sourceName=src, ...}: CB.compInfo) = 
  let fun err severity s =
 	error (0,0) severity (concat["Real constant out of range: ",s,"\n"])

      fun check (checkE,printE,chkId) (enableChk,lvl,logId) e =
	(if !enableChk andalso checkE (e,lvl) then
	   (dumpTerm (printE, src ^ "." ^ chkId ^ logId, e);
	    bug (chkId ^ " typing errors " ^ logId))
	 else ();
	 e)
      fun chkF (b, s) = 
        check (ChkFlint.checkTop, PPFlint.printFundec, 
               "FLINT") (CGC.checkFlint, b, s)

      val _ = (chkF (false,"1") o prF "Translation/Normalization") flint
      val flint = (chkF (false,"2") o prF "Lcontract" o lcontract) flint
      val flint =
        if !CGC.specialize then
           (chkF (false,"3") o prF "Specialization" o specialize) flint
        else flint

      val flint = (chkF (false, "4") o prF "Wrapping" o wrapping) flint
      val flint = (chkF (true, "5") o prF "Reify" o reify) flint

      val (nc0, ncn, dseg) = 
        let val function = convert flint
            val _ = prC "convert" function
            val function = (prC "cpstrans" o cpstrans) function
            val function = 
              if !CGC.cpsopt then cpsopt (function,NONE,false) 
              else function
            val _ = prC "cpsopt" function

            val (function, dlit) = litsplit function
            val data = lit2cps dlit
            val _ = prC "cpsopt-code" function
            val _ = prC "cpsopt-data" data

            fun gen fx = 
              let val fx = (prC "closure" o closure) fx
                  val carg = globalfix fx
                  val carg = spill carg
                  val (carg, limit) = limit carg
               in codegen (carg, limit, err);
                  collect ()
              end

            fun gdata dd = 
              let val x = Control.CG.printit
                  val y = !x
                  val _ = (x := false)
                  val fx = (prC "closure" o closureD) dd
                  val carg = globalfixD fx
                  val carg = spillD carg
                  val (carg, limit) = limitD carg
               in codegenD (carg, limit, err);
                  (collect ()) before (x := y)
              end
         in case CpsSplit.cpsSplit function
             of (fun0 :: funn) => (gen fun0, map gen funn, gdata data)
              | [] => bug "unexpected case on gen in flintcomp"
        end
   in {c0=nc0, cn=ncn, data=dseg, name=ref (SOME src)}
  end (* function flintcomp *)

val flintcomp = phase "Compiler 050 flintcomp" flintcomp

end (* local *)
end (* structure FLINTComp *)

(*
 * $Log: flintcomp.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:40  george
 * Version 110.5
 *
 *)
