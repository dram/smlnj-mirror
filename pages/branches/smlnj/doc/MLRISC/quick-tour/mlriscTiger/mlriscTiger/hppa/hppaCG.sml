structure HppaCG = struct
  fun error msg = MLRiscErrorMsg.impossible ("HppaCG." ^ msg)

  structure F = HppaFrame
  structure C = HppaCells
  structure I = HppaInstr
  structure R = HppaRMapTbls
  structure T = HppaMLTree
  structure Const = HppaIrConstant


 (* properties of instruction set *)
  structure HppaProps = 
    HppaProps(structure HppaInstr=HppaInstr 
	      structure Shuffle=HppaShuffle)

 (* emit assembly code *)
  structure AsmEmit = AsmEmit(structure F = HppaFlowGraph
			      structure E = HppaAsmEmitter)


  structure HppaLabelComp = HppaLabelComp(structure MLTree=HppaMLTree
					  structure Instr=HppaInstr)


  structure HppaMillicode = HppaMillicode(structure MLTree=HppaMLTree
					  structure Instr=HppaInstr
					  structure Frame=HppaFrame)

  structure HppaRewrite = HppaRewrite(HppaInstr)

  (* architecture parameter passing conventions *)
  structure HppaArch = 
    HppaArch(structure MLTree=HppaMLTree structure Frame=HppaFrame)

  structure PrintFlowGraph = 
    PrintFlowGraphFn(structure FlowGraph = HppaFlowGraph
		     structure Emitter = HppaAsmEmitter)

  fun upto (from,to) = if from>to then [] else from::(upto (from+1,to))
  infix upto
  val uniq = SortedList.uniq

  val availF = uniq ((4 upto 11) @ (22 upto 31))

 (* register allocation *)
  structure RegAllocation : 
    sig
      val ra : HppaFlowGraph.cluster -> HppaFlowGraph.cluster
      val spillInit : unit -> unit
    end =
  struct
   (* create integer and floating point register allocators *)
    structure HppaRa = 
       HppaRegAlloc(structure P = HppaProps
		       structure I = HppaInstr
		       structure F = HppaFlowGraph
		       structure Asm = HppaAsmEmitter)

    structure GR = GetReg(val nRegs=32 val available =F.availR)
    structure FR = GetReg(val nRegs=32 val available =availF)

    (* table to hold spills offsets *)
    exception RegSpills
    val regSpills : int Intmap.intmap ref = ref(Intmap.new(16, RegSpills))
    fun spillInit() = regSpills := Intmap.new(16, RegSpills)
    fun getRegLoc (reg:int) = Intmap.map (!regSpills) reg
      handle RegSpills => let
	  val F.InFrame slot = F.allocLocal (!F.currFrame) true
	in Intmap.add (!regSpills) (reg, slot);  slot
	end

    val rewriteDef = HppaRewrite.rewriteDef
    val rewriteUse = HppaRewrite.rewriteUse

   (* register allocation for general purpose registers *)
    fun spill{instr, reg, regmap} = let
      fun spillInstr(r) = let
	val slot = getRegLoc(reg)
	val offset = slot * 4
      in
	[I.STORE{st=I.STW, b=F.FP, d=I.IMMED offset, r=r, mem=()}]
      end
    in
      case instr
      of I.COPY{dst as [rd], src as [rs], tmp, impl} => 
	  if reg=rd then
	     {code=spillInstr(rs), proh=[], instr=NONE}
	  else let
	      val tmp=SOME(I.Direct C.asmTmpR)
	      val instr=I.COPY{dst=dst, src=src, tmp=tmp, impl=impl}
	    in {code=[], instr=SOME instr, proh=[]}
	    end
       | _ => let
	     val newR = C.newReg()
	     val instr' = HppaRewrite.rewriteDef(regmap, instr, reg, newR)
	   in
	     {code=spillInstr(newR), proh=[newR], instr=SOME instr'}
	   end
    end

    fun reload{instr, reg, regmap} = let
      fun reloadInstr(r, rest) = let
        val slot = getRegLoc(reg)
	val offset=slot * 4
      in
	I.LOADI{li=I.LDW, i=I.IMMED offset, r=F.FP, t=r, mem=()}::rest
      end
    in
      case instr
      of I.COPY{dst=[rd], src=[rs], ...} =>
	     {code=reloadInstr(rd, []), proh=[]}
       | _ => let
	     val newR = C.newReg()
	     val instr' = HppaRewrite.rewriteUse(regmap, instr, reg, newR)
	   in 
	     {code=reloadInstr(newR, [instr']), proh=[newR]}
	   end
    end

    structure IntRa = 
      HppaRa.IntRa
        (structure RaUser = struct
           structure I = HppaInstr

	   val getreg = GR.getreg
	   val spill = spill
	   val reload = reload
	   val nFreeRegs = length F.availR
	   val dedicated = F.dedicated
	   fun copyInstr((rds, rss), I.COPY{tmp, ...}) = 
	     I.COPY{dst=rds, src=rss, impl=ref NONE, tmp=tmp}
         end)

    structure FloatRa = 
      HppaRa.FloatRa
        (structure RaUser = struct
           structure I = HppaInstr

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
    (RegAllocation.spillInit();  
     if !TigerControl.prFlowgraph then 
       PrintFlowGraph.printCluster TextIO.stdOut "before ra" cluster
     else ();
     AsmEmit.asmEmit (RegAllocation.ra cluster))

  (* primitives for generation of DEC alpha instruction flowgraphs *)
  structure FlowGraphGen = 
     FlowGraphGen(structure Flowgraph = HppaFlowGraph
		  structure InsnProps = HppaProps
		  structure MLTree = HppaMLTree
		  val codegen = codegen)


  structure MLTreeGen =
    IrMLRiscGen(structure MLTreeComp=
		   Hppa(structure Flowgen=FlowGraphGen
			structure HppaMLTree=HppaMLTree
			structure HppaInstr=HppaInstr
			structure LabelComp=HppaLabelComp
			structure MilliCode=HppaMillicode)
		structure M = HppaMLTree
		structure C = HppaCells
		structure F = HppaFrame
		structure Const = HppaIrConstant
		structure PseudoOps = HppaIrPseudoOps
		structure RMapTbls = HppaRMapTbls
		structure Arch=HppaArch)

  val codegen = MLTreeGen.codegen

end