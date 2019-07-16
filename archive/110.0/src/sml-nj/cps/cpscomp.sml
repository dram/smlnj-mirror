(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *) 
(* cpscomp.sml *)


signature CPSCOMP = sig
    type csegments = { c0: Word8Vector.vector, cn: Word8Vector.vector list , name: string option ref}
    val compile:
	Lambda.lexp * Unsafe.Object.object option * ErrorMsg.complainer
	-> csegments
end
  
functor CPScomp(structure CPSgen : CPSGEN
		val collect: unit -> Word8Vector.vector) : CPSCOMP = 
struct
  type csegments = { c0: Word8Vector.vector, cn: Word8Vector.vector list , name : string option ref}

  structure MachSpec = CPSgen.MachSpec
  structure Convert = Convert(MachSpec)
  structure CPStrans = CPStrans(MachSpec)
  structure CPSopt = CPSopt(MachSpec)
  (* structure Closure = Closure(MachSpec) *)
  structure NewClosure = NClosure(MachSpec)
  structure Spill = Spill(MachSpec)
  structure CpsSplit = CpsSplitFun (MachSpec)

  fun phase x = Stats.doPhase(Stats.makePhase x)

  val convert   = phase "Compiler 060 Convert" Convert.convert
  val cpstrans  = phase "Compiler 065 CPStrans" CPStrans.cpstrans
  val cpsopt    = phase "Compiler 070 cpsopt" CPSopt.reduce
  val closure   = phase "Compiler 080 closure"  NewClosure.closeCPS
  val globalfix = phase "Compiler 090 globalfix" GlobalFix.globalfix
  val spill    = if MachSpec.spillAreaSz < 500 * MachSpec.valueSize
		     then phase "Compiler 100 spill" Spill.spill
		     else fn x => x
  val limit     = phase "Compiler 110 limit" Limit.nolimit
  val codegen   = phase "Compiler 120 cpsgen" CPSgen.codegen

  fun compile(lambda,argument,err) = let           
      val (function,table) = convert lambda
      val _ = if !Control.CG.printit then 
                (Control.Print.say "[After convert:] \n";
                 PPCps.printcps0 function )
              else ()

      val function = cpstrans function
      val _ = if !Control.CG.printit then 
                (Control.Print.say "[After cpstrans:] \n";
                 PPCps.printcps0 function )
              else ()

      val (function,table) = 
	     if !Control.CG.cpsopt then cpsopt(function,table,argument,false) 
	     else (function,table)

      val _ = if !Control.CG.printit then 
                (Control.Print.say "[After cpsopt:] \n";
                 PPCps.printcps0 function )
              else ()

      fun gen function = let
	  val function = closure function
          val _ = if !Control.CG.printit then 
                     (Control.Print.say "[After closure:] \n";
                      PPCps.printcps0 function )
                  else ()
	  val carg = globalfix function
	  val carg = spill carg
	  val (carg, limit) = limit carg
      in
	  codegen (carg, limit, err);
	  collect ()
      end

      (*
      fun dot f x = (print "."; f x)
      val gen = dot gen
      *)

      val fun0 :: funn = CpsSplit.cpsSplit function
      (*
      val _ = print (concat ["## split: ",
                             makestring (1 + length funn),
			     " pieces\n"])
      *)

      val c0 = gen fun0
      val cn = map gen funn

      (*
      val c = gen function
      val s = size c
      val s0 = size c0
      val sn = map size cn
      val s' = foldl (op +) s0 sn
      val _ = print (concat ("%% " ::
                           makestring s :: ":" :: makestring s' :: "=" ::
                           makestring s0 ::
                           foldr (fn (s, r) => "+" :: makestring s :: r)
                                 ["\n"] sn))
      *)
  in
      { c0 = c0, cn = cn , name=ref(NONE) }
  end

end (* CPScomp *)

(*
 * $Log: cpscomp.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.5  1997/08/28 12:38:01  jhr
 *   Removed option of using old closure conversion phase.
 *
 * Revision 1.4  1997/08/25  19:20:30  riccardo
 *   Added support for tagging code objects with their source/bin file name.
 *
 * Revision 1.3  1997/06/30  19:37:16  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.2  1997/04/18  15:39:13  george
 *   Fixing the infinite loop bug reported by Dino Oliva -- zsh
 *
 * Revision 1.1.1.1  1997/01/14  01:38:30  george
 *   Version 109.24
 *
 *)
