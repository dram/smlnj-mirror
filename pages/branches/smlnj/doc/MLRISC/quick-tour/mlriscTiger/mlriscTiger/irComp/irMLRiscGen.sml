signature MLRISC_GEN =  sig
  structure F : FRAME
  val codegen : F.frag -> unit		(* generate code *)
end 

functor IrMLRiscGen
  (structure MLTreeComp : MLTREECOMP
   structure M : MLTREE where Region = TigerRegions
   structure C : CELLS
   structure F : FRAME
   structure Const : IRCONSTANT
   structure PseudoOps : IR_PSEUDO_OPS
   structure RMapTbls : REGIGER_MAP_TABLES
   structure Arch : TARGET_CONVENTIONS
     sharing MLTreeComp.T = Arch.M = M
     sharing MLTreeComp.T.Constant = Const 
     sharing Const.F = Arch.F = F
     sharing MLTreeComp.T.PseudoOp = PseudoOps) : MLRISC_GEN =
struct
  structure M : MLTREE = MLTreeComp.T
  structure T = Tree
  structure LE = LabelExp
  structure F = F

  structure R = RMapTbls

  val mem = TigerRegions.memory

  exception IrMLRiscGen
  fun error msg = (print ("IrMLRiscGen." ^ msg ^ "\n"); raise IrMLRiscGen)

  val emit = MLTreeComp.mlriscComp
  val comp = MLTreeComp.mltreeComp

  val sp = F.SP

  fun codegen(F.STRING(lab, s)) = 
        comp(M.PSEUDO_OP(PseudoOps.STRING(R.lookupLabel lab, s)))
    | codegen(F.PROC{frame, body}) = let
	fun doMove(t1, t2) = let
	  fun isDedicated t = List.exists (fn (r:int) => r=t) F.dedicated
        in
	  if isDedicated t1 orelse isDedicated t2 then
	    emit(M.MV(t1, M.REG t2))
	  else emit(M.COPY([t1], [t2]))
        end

	fun stmAction(T.SEQ(a, b)) = (stmAction(a); stmAction(b))
	  | stmAction(T.LABEL lab) = comp(M.DEFINELABEL(R.lookupLabel lab))
	  | stmAction(T.JUMP(exp, dsts)) = 
	     emit(M.JMP(expAction exp, map R.lookupLabel dsts))
	  | stmAction(T.CJUMP(relop, e1, e2, labt, labf)) = let
	      val trueLab = R.lookupLabel labt
	      val falseLab = R.lookupLabel labf
	      fun bcc cond =
		emit(M.BCC(cond, 
			   M.CMP(cond,expAction e1, expAction e2, M.LR), 
			   trueLab))
	    in
	      case relop
	      of T.EQ => bcc M.EQ    | T.NE => bcc M.NEQ
	       | T.LT => bcc M.LT    | T.GT => bcc M.GT
	       | T.LE => bcc M.LE    | T.GE  => bcc M.GE
	       | T.ULT => bcc M.LTU  | T.ULE => bcc M.LEU
	       | T.UGT => bcc M.GTU  | T.UGE => bcc M.GEU
	    end
	  | stmAction(T.MOVE(T.TEMP t, T.CALL(e,el))) =
	      (callStm(e,el); 
	       doMove(R.lookupTemp t, R.lookupTemp F.RV))
	  | stmAction(T.MOVE(T.MEM(e), b)) = 
	      emit(M.STORE32(expAction e, expAction b, mem))
	  | stmAction(T.MOVE(T.TEMP t1, T.TEMP t2)) = 
	      doMove(R.lookupTemp t1, R.lookupTemp t2)
	  | stmAction(T.MOVE(T.TEMP t, e)) = 
	      emit(M.MV(R.lookupTemp t, expAction e))
	  | stmAction(T.MOVE _) = error "stmAction: MOVE"
	  | stmAction(T.EXP(T.CALL(e,el))) = callStm(e,el)
	  | stmAction(T.EXP e) = emit(M.MV(C.newReg(),expAction e))

	and expAction(T.BINOP(binop, e1, e2)) =
	    (case binop
	     of T.PLUS => M.ADD(expAction e1, expAction e2)
	      | T.MINUS => M.SUB(expAction e1, expAction e2, M.LR)
	      | T.MUL => M.MULT(expAction e1, expAction e2)
	      | T.DIV => M.DIVT(expAction e1, expAction e2, M.LR)
	      | T.AND => M.ANDB(expAction e1, expAction e2)
	      | T.OR => M.ORB(expAction e1, expAction e2)
	      | T.LSHIFT => M.SLL(expAction e1, expAction e2, M.LR)
	      | T.RSHIFT => M.SRL(expAction e1, expAction e2, M.LR)
	      | T.ARSHIFT => M.SRA(expAction e1, expAction e2, M.LR)
	      | T.XOR => M.XORB(expAction e1, expAction e2)
	     (*esac*))
	  | expAction(T.MEM e) = M.LOAD32(expAction e, mem)
	  | expAction(T.TEMP tmp) = M.REG(R.lookupTemp tmp)
	  | expAction(T.ESEQ(stm, exp)) = error "ESEQ"
	  | expAction(T.NAME label) = M.LABEL(LE.LABEL(R.lookupLabel label))
	  | expAction(T.CONST i) = M.LI i
	  | expAction(T.CALL(e, el)) = let
	      val tmp = C.newReg()
	    in callStm(e,el); emit(M.COPY([tmp],[F.RV])); M.REG tmp
	    end

	and callStm(f, args) = let
	  val proc = 
	   (case f
	    of T.NAME lab => M.LABEL(LE.LABEL(R.lookupLabel lab))
	     | exp => expAction exp
	    (*esac*))

	  val defs' = map (M.GPR o M.REG) (F.RV::F.callersaved)
	  val defs = 
	    case F.RA 
	      of NONE => defs' 
	       | SOME ra => M.GPR(M.REG ra)::defs'

	  (* evaluate each argument into a fresh temporary.
	   * Important not to use C.newReg().
	   *)
	  fun evalArg e = let 
	      val tmp = Temp.newtemp()
	  in stmAction(T.MOVE(T.TEMP tmp, e)); tmp
          end

	  (* move fresh temps containing arguments into formals *)
	  (* This is done to cater to arguments being calls  *)
	  fun mvToFormals(fmlRegs, temps) = let
	    val nFmls = length fmlRegs
	    val nTemps = length temps
	    val regs = map R.lookupTemp temps
          in
	    if nFmls > nTemps then let
	        val fmls = List.take(fmlRegs, nTemps)
	      in emit(M.COPY(fmls, regs)); fmls
	      end
	    else
	      (emit(M.COPY(fmlRegs, List.take(regs, nFmls)));
	       stmAction
	         (Arch.memArgs(nFmls, map T.TEMP (List.drop(temps, nFmls))));
	       fmlRegs)
	  end

	  val uses = 
	    map (M.GPR o M.REG) (mvToFormals(F.argRegs, map evalArg args))

	  val call =
	    Arch.mkCall{frame=frame, proc=proc, nArgs=length args,
			defs=defs, uses=uses}
	in app emit call
	end (* callStm *)

	val frameName = Symbol.name(F.name frame)

	fun slotAddr slot = let
	  val frameLoc = Const.FRAMESLOT{slot=slot, frame=frame}
        in M.ADD(M.REG F.FP, M.CONST frameLoc)
        end

        fun argAddr slot = let
	  val argLoc = Const.ARGSLOT{slot=slot, frame=frame}
        in M.ADD(M.REG F.FP, M.CONST argLoc)
        end

	fun changeView([], [], [], []) = ()
	  | changeView([], [], dst, src) = 
	      emit(M.COPY(map R.lookupTemp dst, map R.lookupTemp src))
	  | changeView(F.InReg t1::actuals, fml::formals, dst, src) = 
	    (case fml
	     of F.InReg t2 => changeView(actuals, formals, t2::dst, t1::src)
	      | F.InFrame slot => 
	          (emit(M.STORE32(slotAddr slot, M.REG(R.lookupTemp t1), mem));
		   changeView(actuals, formals, dst, src))
	      | F.InArg _ => error "changeView: InReg, InArg"
             (*esac*))
	  | changeView(F.InArg ia::actuals, fml::formals, dst, src) = 
	    (case fml
	     of F.InReg t2 => 
	         (emit(M.MV(R.lookupTemp t2, M.LOAD32(argAddr ia, mem)));
		  changeView(actuals, formals, dst, src))
	      | F.InFrame slot => 
		 (emit(M.STORE32(slotAddr slot, M.LOAD32(argAddr ia, mem), mem));
		  changeView(actuals, formals, dst, src))
	      | F.InArg _ => error "changeView: InArg, InArg"
            (*esac*))
	  | changeView _ = error "changeView: _"

	val liveOnEntry = 
	  case F.RA
	    of NONE => F.calleesaved
	     | SOME ra => ra::F.calleesaved

	fun tigerPrologue(temps) = 
	 (comp(M.PSEUDO_OP(PseudoOps.PROLOGUE frameName));
	  app emit (Arch.functionPrologue());
	  emit(M.COPY(temps, liveOnEntry));
	  changeView(F.actuals frame, F.formals frame, [], []))

	fun tigerEpilogue(temps) = 
	 (emit(M.COPY(liveOnEntry, temps));
	  app emit (Arch.functionEpilogue());
	  comp(M.ESCAPEBLOCK(map (M.GPR o M.REG) (F.SP::liveOnEntry)));
	  comp(M.PSEUDO_OP(PseudoOps.EPILOGUE frameName)))

        val stms = Canon.traceSchedule(Canon.basicBlocks (Canon.linearize body))
	val regmapRefs = (R.reset(); C.resetRegs())
	val temps = map (fn _ => C.newReg()) liveOnEntry
      in
	if !TigerControl.prTrees then
	  app (fn stm => (print "stm: "; 
			  Printtree.printtree(TextIO.stdOut, stm))) stms
	else ();
	F.currFrame := frame;
        comp M.BEGINCLUSTER;
        tigerPrologue(temps);
	app stmAction stms;
        tigerEpilogue(temps);
        comp(M.ENDCLUSTER regmapRefs)
      end (* codegen *)
end

