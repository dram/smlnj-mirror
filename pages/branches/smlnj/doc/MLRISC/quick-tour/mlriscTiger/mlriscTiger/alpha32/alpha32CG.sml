(* Alpha32 code generator *)
structure  Alpha32CG = struct
  fun error msg = MLRiscErrorMsg.impossible ("Alpha32CG." ^ msg)

  structure F = Alpha32Frame
  structure C = Alpha32Cells
  structure I = Alpha32Instr
  structure R = Alpha32RMapTbls
  structure T = Alpha32MLTree
  structure Const = Alpha32IrConstant
  
  val mem = TigerRegions.memory

 (* properties of instruction set *)
  structure Alpha32Props = 
    Alpha32Props(structure Alpha32Instr= Alpha32Instr 
		 structure Shuffle = Alpha32Shuffle)

 (* emit assembly code *)
  structure AsmEmit = AsmEmit(structure F = Alpha32FlowGraph
			      structure E = Alpha32AsmEmitter)

 (* architecture parameter passing conventions *)
  structure Alpha32Arch = 
    Alpha32Arch(structure MLTree=Alpha32MLTree structure Frame=Alpha32Frame)

  structure Alpha32Rewrite = Alpha32Rewrite(Alpha32Instr)

  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto
  val uniq = SortedList.uniq

  val availF = uniq(10 upto 30)

 (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : Alpha32FlowGraph.cluster -> Alpha32FlowGraph.cluster
      val spillInit : unit -> unit
    end =
  struct
   (* create integer and floating point register allocators *)
    structure Alpha32Ra = 
       Alpha32RegAlloc(structure P = Alpha32Props
		       structure I = Alpha32Instr
		       structure F = Alpha32FlowGraph
		       structure Asm = Alpha32AsmEmitter)

    structure GR = GetReg(val nRegs=32 val available = F.availR)
    structure FR = GetReg(val nRegs=32 val available = availF)

    (* table to hold spills offsets *)
    exception RegSpills
    val regSpills : int Intmap.intmap ref = ref(Intmap.new(16, RegSpills))
    fun spillInit() = regSpills := Intmap.new(16, RegSpills)
    fun getRegLoc (reg:int) = Intmap.map (!regSpills) reg
      handle RegSpills => let
	  val F.InFrame slot = F.allocLocal (!F.currFrame) true
	in Intmap.add (!regSpills) (reg, slot);  slot
	end

   (* register allocation for general purpose registers *)
    fun spill{instr, reg, regmap} = let
      fun spillInstr(r) = let
	val offset = getRegLoc(reg)
	val cframe = !F.currFrame
	val cexp = Const.FRAMESLOT{slot=offset, frame=cframe}
      in
	[I.STORE{stOp=I.STL, r=r, b=F.FP, d=I.CONSTop cexp, mem=mem}]
      end
    in
      case instr
      of I.COPY{dst as [rd], src as [rs], tmp, impl} => 
	 if rd=reg then
	   {code=spillInstr(rs), proh=[], instr=NONE}
	 else let
	     val tmp=SOME(I.Direct C.asmTmpR)
	     val instr= I.COPY{dst=dst, src=src, tmp=tmp, impl=impl}
	   in {code=[], instr=SOME instr, proh=[]}
           end
       | _ => let
	     val newR = C.newReg()
	     val instr' = Alpha32Rewrite.rewriteDef(regmap, instr, reg, newR)
	   in {code=spillInstr(newR), proh=[newR], instr=SOME instr'}
	   end
    end

    fun reload{instr, reg, regmap} = let
      fun reloadInstr(r, rest) = let
	val offset = getRegLoc(reg)
	val tmp=C.newReg()
	val cframe = !F.currFrame
	val cexp = Const.FRAMESLOT{slot=offset, frame=cframe}
      in I.LOAD{ldOp=I.LDL, r=r, b=F.FP, d=I.CONSTop cexp, mem=mem}::rest
      end
    in
      case instr
      of I.COPY{dst as [rd], src as [rs], impl, tmp} =>
	     {code=reloadInstr(rd, []),  proh=[]}
       | _ => let
	     val newR = C.newReg()
	     val instr' = Alpha32Rewrite.rewriteUse(regmap, instr, reg, newR)
	   in {code=reloadInstr(newR, [instr']), proh=[newR]}
	   end
    end

    structure IntRa = 
      Alpha32Ra.IntRa
        (structure RaUser = struct
           structure I = Alpha32Instr

	   val getreg = GR.getreg
	   val spill = spill
	   val reload = reload
	   val nFreeRegs = length F.availR
	   val dedicated = F.dedicated
	   fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
	     I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
         end)

    structure FloatRa = 
      Alpha32Ra.FloatRa
        (structure RaUser = struct
           structure I = Alpha32Instr

	   val getreg = FR.getreg
	   val spill = fn _ => error "spill"
	   val reload = fn _ => error "reload"
	   val nFreeRegs = length availF
	   val dedicated = SortedList.remove(availF, uniq(0 upto 31))
	   fun copyInstr((rds, rss), I.FCOPY{tmp, ...}) = 
	     I.FCOPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
         end)

    val iRegAlloc = IntRa.ra IntRa.REGISTER_ALLOCATION
    val fRegAlloc = FloatRa.ra FloatRa.REGISTER_ALLOCATION

    fun ra cluster = (GR.reset(); fRegAlloc (iRegAlloc (cluster)))
  end (* RegAllocation *)

  (* code generation does only register allocation *)
  fun codegen cluster =
    (RegAllocation.spillInit();  AsmEmit.asmEmit (RegAllocation.ra cluster))

  (* primitives for generation of DEC alpha instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = Alpha32FlowGraph
		  structure InsnProps = Alpha32Props
		  structure MLTree = Alpha32MLTree
		  val codegen = codegen)

  structure MLTreeGen =
    IrMLRiscGen(structure MLTreeComp=
		   Alpha32(structure Flowgen=FlowGraphGen
			   structure Alpha32MLTree=Alpha32MLTree
			   structure Alpha32Instr=Alpha32Instr
			   structure PseudoInstrs=Alpha32PseudoInstrs)
		structure M = Alpha32MLTree
		structure C = Alpha32Cells
		structure F = Alpha32Frame
		structure Const = Alpha32IrConstant
		structure PseudoOps = Alpha32IrPseudoOps
		structure RMapTbls = Alpha32RMapTbls
		structure Arch=Alpha32Arch)

  val codegen = MLTreeGen.codegen
end


