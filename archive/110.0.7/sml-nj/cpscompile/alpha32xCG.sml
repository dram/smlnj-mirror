(* alpha32CG.sml --- 32 bit DEC alpha code generator
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)


functor Alpha32XCG(structure Emitter : EMITTER_NEW
		    where I = Alpha32Instr
		    where F = Alpha32FlowGraph) :
  sig
    structure MLTreeGen : CPSGEN 
    val finish : unit -> unit
  end = 
struct

  structure I = Alpha32Instr
  structure C = Alpha32Cells
  structure R = Alpha32CpsRegs
  structure MLTree = Alpha32MLTree
  structure Region = Alpha32Instr.Region

  (* properties of instruction set *)
  structure Alpha32Props = 
    Alpha32Props(structure Alpha32Instr= I val exnptrR = [14])


  (* Label backpatching and basic block scheduling *)
  structure BBSched =
    BBSched2(structure Flowgraph = Alpha32FlowGraph
	     structure Jumps = Alpha32Jumps(Alpha32Instr)
	     structure Emitter = Emitter
	     structure Scheduler = NoScheduler(Alpha32Instr))

  fun error msg = ErrorMsg.impossible ("Alpha32CG." ^ msg)

  val stack = Region.stack

  (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : Alpha32FlowGraph.cluster -> Alpha32FlowGraph.cluster
    end =
  struct

   (* spill area management *)
    val initialSpillOffset = 128
    val spillOffset = ref initialSpillOffset
    fun newOffset n =
	if n > 4096
	then error "newOffset - spill area is too small"
	else spillOffset := n
    exception RegSpills and FregSpills

    val regSpills : int Intmap.intmap ref = ref(Intmap.new(0, RegSpills))
    val fregSpills : int Intmap.intmap ref = ref(Intmap.new(0, FregSpills))

    (* get spill location for general registers *)
    fun getRegLoc reg = Intmap.map (!regSpills) reg
      handle RegSpills => let
	  val offset = !spillOffset
	in
	  newOffset(offset+4);
	  Intmap.add (!regSpills) (reg, offset);
	  offset
        end

    (* get spill location for floating registers *)
    fun getFregLoc freg = Intmap.map (!fregSpills) freg
      handle FregSpills => let
	  val offset = !spillOffset
	  val fromInt = Word.fromInt
	  val aligned = Word.toIntX(Word.andb(fromInt offset+0w7, fromInt ~8))
	in
	  newOffset(aligned+8);
	  Intmap.add (!fregSpills) (freg, aligned);
	  aligned
	end

    fun mvInstr(rd, rs) = I.OPERATE{oper=I.BIS, ra=rs, rb=I.REGop 31, rc=rd} 
    fun fmvInstr(fd, fs) = I.FOPERATE{oper=I.CPYS, fa=fs, fb=fs, fc=fd} 

    fun spill (stClass, stOp, getLoc) {instr:I.instruction, reg} = let
      val offset = I.IMMop (getLoc(reg))
      fun spillInstr(src) = 
	[stClass{stOp=stOp, r=src, b=C.stackptrR, d=offset, mem=stack}]
    in
      case instr
      of I.COPY([rd], [rs], _) =>	 (* reg = rd *)
	  {code=spillInstr(rs),  instr=NONE,   proh=[]:int list}
       | I.FCOPY([fd], [fs], _) => 	 (* reg = fd *)
	  {code=spillInstr(fs),   instr=NONE,   proh=[]}
       | _ => {code=spillInstr(reg),  instr=SOME instr,  proh=[]}
    end

    fun reload (ldClass, ldOp, getLoc) {instr, reg} = let
      val offset = I.IMMop (getLoc(reg))
      fun reloadInstr(dst, rest) =
	ldClass{ldOp=ldOp, r=dst, b=C.stackptrR, d=offset, mem=stack}::rest
    in 
      case instr
      of I.COPY([rd], [rs], _) =>	(* reg = rs *)
	   {code=reloadInstr(rd, []),   proh=[]:int list}
       | I.FCOPY([fd], [fs], _) =>	(* reg = fs *)
	   {code=reloadInstr(fd, []), proh=[]}
       | _ => 
	   {code=reloadInstr(reg, [instr]), proh=[]}
    end

    fun spillInit () = 
      (spillOffset := initialSpillOffset;
       regSpills := Intmap.new(8, RegSpills);
       fregSpills := Intmap.new(8, FregSpills))

    structure GR = GetReg(val nRegs=32 val available=R.availR)
    structure FR = GetReg(val nRegs=32 val available=R.availF)

    structure Alpha32Ra = 
       Alpha32RegAlloc(structure P = Alpha32Props
		       structure I = Alpha32Instr
		       structure F = Alpha32FlowGraph
		       structure Asm = Alpha32AsmEmitter)

    (* register allocation for general purpose registers *)
    structure IntRa = 
      Alpha32Ra.IntRa
        (structure RaUser = struct
           structure I = Alpha32Instr

	   val getreg = GR.getreg
	   val spill = spill(I.STORE, I.STL, getRegLoc)
	   val reload = reload(I.LOAD, I.LDL, getRegLoc)
	   val nFreeRegs = length R.availR
	   val dedicated = R.dedicatedR
	   fun copyInstr(rds, rss) = I.COPY(rds, rss, ref NONE)
         end)

    (* register allocation for floating point registers *)
    structure FloatRa = 
      Alpha32Ra.FloatRa
        (structure RaUser = struct
	   structure I = Alpha32Instr

	   val getreg = FR.getreg
	   val spill = spill (I.FSTORE, I.STT, getFregLoc)
	   val reload = reload (I.FLOAD, I.LDT, getFregLoc)
	   val nFreeRegs = length R.availF
	   val dedicated = R.dedicatedF
	   fun copyInstr(fds, fss) = I.FCOPY(fds, fss, ref NONE)
         end)



    val iRegAlloc = IntRa.ra IntRa.REGISTER_ALLOCATION
    val fRegAlloc = FloatRa.ra FloatRa.REGISTER_ALLOCATION

    fun ra cluster = let
      fun intRa cluster = (GR.reset(); iRegAlloc cluster)
      fun floatRa cluster = (FR.reset(); fRegAlloc cluster)
    in spillInit(); (floatRa o intRa) cluster
    end
  end (* RegAllocation *)

  val codegen = BBSched.bbsched o RegAllocation.ra

  (* primitives for generation of DEC alpha instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = Alpha32FlowGraph
		  structure InsnProps = Alpha32Props
		  structure MLTree = MLTree
		  val codegen = codegen)

  (* compilation of CPS to MLRISC *)
  structure MLTreeGen = 
     MLRiscGen(structure MachineSpec=Alpha32XSpec
	       structure MLTreeComp=
		  Alpha32(structure Flowgen=FlowGraphGen
			  structure Alpha32Instr=Alpha32Instr
			  structure Alpha32MLTree=Alpha32MLTree
			  structure PseudoInstrs=Alpha32PseudoInstrs)
	       structure Cells=Alpha32Cells
	       structure C=Alpha32CpsRegs
	       structure ConstType=Alpha32Const
	       structure PseudoOp=PseudoOpsLittle)

  val finish = BBSched.finish
end


(*
 * $Log: alpha32xCG.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.9  1997/09/17 17:15:20  george
 *   dedicated registers are now part of the CPSREGS interface
 *
 * Revision 1.8  1997/08/29  11:04:03  george
 *   Spill area now starts at a new offset to account for the
 *   divl, divlu addresses on the stack.
 *
 * Revision 1.7  1997/07/28  20:04:42  george
 *   Added support for regions
 *
 * Revision 1.6  1997/07/17  12:35:53  george
 *   The constant type used to specialize MLTrees is now done more compactly.
 *
 * Revision 1.5  1997/07/15  16:01:53  dbm
 *   Change in where structure syntax.
 *
 * Revision 1.4  1997/07/02  13:25:28  george
 *   Generated better spill code, in which a new temporary is introduced
 *   to represent the register being spilled.
 *
 * Revision 1.3  1997/05/20  12:17:24  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.2  1997/03/06  19:08:49  george
 *   Use a more sensible union-find data structure to do the job.
 *
 * Revision 1.1.1.1  1997/01/14  01:38:33  george
 *   Version 109.24
 *
 *)
