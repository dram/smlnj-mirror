(* vararg-c-call-fn.sml
 * 
 * Generate MLRISC code that makes a vararg call at runtime. The input to this code is a
 * list of Staged Allocation locations paired with arguments. This code places the arguments
 * in the correct locations.
 *
 * Mike Rainey (mrainey@cs.uchicago.edu)
 *)

functor VarargCCallFn (
    structure T : MLTREE
    structure CCall : C_CALL where T = T
  (* registers for passing parameters *)
    val gprParams : T.reg list
    val fprParams : T.reg list
  (* possible widths for passing parameters *)
    val gprTys : T.ty list
    val fprTys : T.ty list
  (* stack pointer register *)
    val spReg : T.rexp
  (* default register width *)
    val wordTy : T.ty
    val newReg : 'a -> CellsBasis.cell
  )  = struct

    structure T = T
    structure CB = CellsBasis
    structure CTy = CTypes

    datatype argument = I of int | R of real | B of bool | S of string

    fun concatMap f xs = List.concat (List.map f xs)

    val mem = T.Region.memory
    val stack = T.Region.stack
    val defaultWidthB = 8

    fun lit i = T.LI (T.I.fromInt (wordTy, i))
    fun gpr r = T.GPR (T.REG (wordTy, r))
    fun fpr (ty, f) = T.FPR (T.FREG (ty, f))
    val regToInt = CB.physicalRegisterNum

  (* encodings for the kinds of argument locations *)
    val GPR = 0
    val FPR = 1
    val STK = 2
    val FSTK = 3

  (* offsets into the zipped argument *)
    val argOff = 0
    val kindOff = 1
    val locOff = 2
    val tyOff = 3

  (* load a value from the zipped argument *)
    fun offZippedArg (ty, arg, off) = T.LOAD(ty, T.ADD(wordTy, arg, lit (off*defaultWidthB)), mem)

  (* load a floating-point value from the zipped argument *)
    fun offZippedArgF (ty, arg, off) = T.FLOAD(ty, T.ADD(wordTy, arg, lit (off*defaultWidthB)), mem)

    fun newLabel s = Label.label s ()

    fun labelOfReg (k, ty, r) = 
	    Label.global (k^Int.toString ty^"."^Int.toString (regToInt r)^".reg")

    local
      fun atTyLab (k, tys) = List.map (fn ty => (ty, newLabel (k^Int.toString ty^"."))) tys
      val gprLabs = atTyLab ("resolveGprs", gprTys)
      val fprLabs = atTyLab ("resolveFprs", fprTys)
      val stkLabs = atTyLab ("resolveStk", gprTys)
      val fstkLabs = atTyLab ("resolveFstk", fprTys)
    in
    fun resolveAtKindAndTyLab (k, ty) = let
	   val labs = (case k
			of "gpr" => gprLabs
			 | "fpr" => fprLabs
			 | "stk" => stkLabs
			 | "fstk" => fstkLabs
		      (* end case *))
	   val SOME (_, lab) = List.find (fn (ty', _) => ty = ty') labs
           in
	      lab
           end
    end

    local
      fun atKindLab k = (k, newLabel ("resolveTys."^k))
      val labs = List.map atKindLab ["gpr", "fpr", "stk", "fstk"]
    in
    fun resolveTysLab k = let
	    val SOME (_, lab) = List.find (fn (k', _) => k' = k) labs
            in
	        lab
	    end
    end

    val interpLab = newLabel "interp"
    val resolveKindsLab = newLabel "resolveKinds"
    val gotoCLab = newLabel "gotoC"

  (* store a gpr argument on the stack *)
    fun storeStk (arg, ty) = 
	    T.STORE(ty, T.ADD (wordTy, spReg, offZippedArg(wordTy, arg, locOff)), offZippedArg(ty, arg, argOff), mem)

    fun genStoreStkAtTy arg ty = [
	   T.DEFINE (resolveAtKindAndTyLab ("stk", ty)),
	   storeStk(arg, ty),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* store the argument at the stack offset *)
    fun genStoreStk arg tys = concatMap (genStoreStkAtTy arg) tys

  (* store a fpr argument on the stack *)
    fun storeFStk (arg, ty) =
	    T.FSTORE(ty, T.ADD (wordTy, spReg, offZippedArg(wordTy, arg, locOff)), offZippedArgF(ty, arg, argOff), mem)

  (* store the argument at the stack offset *)
    fun genStoreFStkAtTy arg ty = [
	   T.DEFINE (resolveAtKindAndTyLab ("fstk", ty)),
	   storeFStk (arg, ty),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* store the argument at the stack offset *)
    fun genStoreFStk arg tys = concatMap (genStoreFStkAtTy arg) tys

  (* place the argument into the parameter register and jump back to the interpreter *)
    fun genPutGpr arg ty r = [
	   T.DEFINE (labelOfReg ("gpr", ty, r)),
	   T.MV (ty, r, offZippedArg (ty, arg, argOff)), 
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* place the argument into the parameter register and jump back to the interpreter *)
    fun genPutFpr arg ty r = [
	   T.DEFINE (labelOfReg ("fpr", ty, r)),
	   T.FMV (ty, r, offZippedArgF (ty, arg, argOff)),
	   T.JMP (T.LABEL interpLab, [])
        ]

  (* resolve the function for loading the register *)
    fun genResolveReg arg k ty (r, instrs) = let
	   val cmp = T.CMP(wordTy, T.EQ, offZippedArg(wordTy, arg, locOff), lit (regToInt r))
           in
	      T.BCC(cmp, labelOfReg (k, ty, r)) :: instrs
	   end

  (* check the type of the argument *)
    fun checkTy arg k ty = 
	    T.BCC(T.CMP(wordTy, T.EQ, offZippedArg(wordTy, arg, tyOff), lit ty), resolveAtKindAndTyLab(k, ty))

  (* resolve the type of the argument at a given kind *)
    fun genResolveTysAtKind arg (k, tys) = 
	 T.DEFINE (resolveTysLab k) :: List.map (checkTy arg k) tys

  (* resolve the type of the argument *)
    fun genResolveTys arg = 
	    concatMap (genResolveTysAtKind arg) [("gpr", gprTys), ("fpr", fprTys), ("stk", gprTys), ("fstk", fprTys)]

  (* resolve registers at a fixed type *)
    fun genResolveRegsOfTy arg k ty regs = 
	    T.DEFINE (resolveAtKindAndTyLab (k, ty)) :: 
	    List.rev (T.JMP(T.LABEL interpLab, []) :: List.foldl (genResolveReg arg k ty) [] regs)

  (* resolve registers for loading function arguments *)
    fun genResolveRegs arg k tys regs = let
	    val resolves = List.map (genResolveRegsOfTy arg k) tys
            in
	       concatMap (fn f => f regs) resolves
	    end

  (* resolve an argument to a kind of location *)
    fun resolveKind arg (kEncoding, k) = 
	    T.BCC(T.CMP(wordTy, T.EQ, offZippedArg(wordTy, arg, kindOff), lit kEncoding), resolveTysLab k)

  (* resolve the argument to one of the location kinds *)
    fun genResolveKinds arg = 
	   T.DEFINE resolveKindsLab ::
	   List.map (resolveKind arg) [(GPR, "gpr"), (FPR, "fpr"), (STK, "stk"), (FSTK, "fstk")]

    fun resolveArgLocs arg = List.concat [
           genResolveRegs arg "gpr" gprTys gprParams,
	   genResolveRegs arg "fpr" fprTys fprParams,
	   genStoreStk arg gprTys,
	   genStoreFStk arg fprTys
        ]

  (* argument list encoding *)
    val NIL = 0
    val HD = 0
    val TL = 1

  (* load a value from the argument *)
    fun offArgs args 0 = T.LOAD (wordTy, T.REG(wordTy, args), mem)
      | offArgs args off = T.LOAD (wordTy, T.ADD (wordTy, T.REG(wordTy, args), lit(off*defaultWidthB)), mem)

  (* call the varargs C function *)
    fun genCallC cFun = let
	   val defs = List.map gpr CCall.callerSaveRegs @ List.map (fn r => fpr(64, r)) CCall.callerSaveFRegs
	   val uses = List.map gpr gprParams @ List.map (fn r => fpr(64, r)) fprParams
	   in
	      [
	       T.DEFINE gotoCLab,
	       T.CALL {funct=cFun, targets=[], defs=defs, uses=uses, region=mem, pops=0}
	      ]
	   end

  (* interpreter for varargs *)
    fun genInterp (args, argReg) = [
	   T.DEFINE interpLab,
	 (* loop through the args *)
	   T.BCC (T.CMP(wordTy, T.EQ, T.REG (wordTy, args), lit NIL), gotoCLab),
	   T.MV (wordTy, argReg, offArgs args HD),
	   T.MV(wordTy, args, offArgs args TL),
	   T.JMP (T.LABEL resolveKindsLab, [])		
        ]

    fun genPutGprs arg = let
	    val putGprs = List.map (genPutGpr arg) gprTys
            in
	       concatMap (fn f => concatMap f gprParams) putGprs
	    end

    fun genPutFprs arg = let
	    val putfprs = List.map (genPutFpr arg) fprTys
            in
	       concatMap (fn f => concatMap f fprParams) putfprs
	    end

  (* generate instructions for making a varargs call *)
    fun genVarargs (cFun, args) = let           
	    val argReg = newReg ()
	    val arg = T.REG(wordTy, argReg)
            in
	      List.concat [
	         genInterp(args, argReg),
		 genResolveKinds arg,
		 resolveArgLocs arg,
		 genResolveTys arg,
		 genPutGprs arg,
		 genPutFprs arg,
		 genCallC cFun
	      ]
	    end

    fun argToCTy (I _) = CTy.C_signed CTy.I_int
      | argToCTy (R _) = CTy.C_double
      | argToCTy (B _) = CTy.C_signed CTy.I_int
      | argToCTy (S _) = CTy.C_PTR

  (* runtime friendly representation of the C location *)
    fun encodeCLoc (CCall.C_GPR (ty, r)) = (GPR, regToInt r, ty)
      | encodeCLoc (CCall.C_FPR (ty, r)) = (FPR, regToInt r, ty)
      | encodeCLoc (CCall.C_STK (ty, off)) = (STK, T.I.toInt (wordTy, off), ty)
      | encodeCLoc (CCall.C_FSTK (ty, off)) = (FSTK, T.I.toInt (wordTy, off), ty)

  (* takes a vararg and a location and returns the vararg triplet *)
    fun varArg (arg, loc) = let
	   val (k, l, ty) = encodeCLoc loc
           in
	     (arg, k, l, ty)
	   end

  (* package the arguments with their locations *)
    fun encodeArgs args = let
	    val argTys = List.map argToCTy args
	    val {argLocs, argMem, ...} = CCall.layout {conv="c-call", retTy=CTy.C_void, paramTys=argTys}
	  (* expect single locations, as we do not pass aggregates to vararg functions *)
	    val argLocs = List.map List.hd argLocs
            in
  	        (ListPair.mapEq varArg (args, List.rev argLocs), argMem)
	    end

(*
    structure DL = DynLinkage

    fun main's s = DL.lib_symbol (DL.main_lib, s)
    val malloc_h = main's "malloc"
    val free_h = main's "free"

    exception OutOfMemory

    fun sys_malloc (n : Word32.word) =
	let val w_p = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * word -> string) list
		      -> Word32.word
	    val a = w_p (DL.addr malloc_h, n, [])
	in if a = 0w0 then raise OutOfMemory else a
	end

    fun sys_free (a : Word32.word) =
	let val p_u = RawMemInlineT.rawccall :
		      Word32.word * Word32.word * (unit * string -> unit) list
		      -> unit
	in p_u (DL.addr free_h, a, [])
	end

    fun alloc bytes = sys_malloc (Word32.toLargeWord bytes)
    fun free a = sys_free a

    type addr = Word32.word
    infix ++ 
    fun (a: addr) ++ i = a + Word32.fromInt i

    fun set' (p, w) = RawMemInlineT.w32s (p, w)
    fun nxt' p = p ++ 1

    fun cpML' { from, to } = let
	val n = String.size from
	fun loop (i, p) =
	    if i >= n then set' (p, 0w0)
	    else (set' (p, Word32.fromInt (Char.ord
					       (String.sub (from, i))));
		  loop (i+1, nxt' p))
    in
	loop (0, to)
    end

    fun dupML' s = let
	    val z = alloc (Word32.fromInt (String.size s + 1))
	in
	    cpML' { from = s, to = z };
	    z
	end

    fun encodeArg (I i) = Word32.fromInt i
      | encodeArg (S s) = dupML' s
      | encodeArg (R r) = raise Fail "todo"

    val defaultWidthB = Word32.fromInt defaultWidthB
    val argOffB = Word32.fromInt argOff * defaultWidthB
    val kindOffB = Word32.fromInt kindOff * defaultWidthB
    val locOffB = Word32.fromInt locOff * defaultWidthB
    val tyOffB = Word32.fromInt tyOff * defaultWidthB

    fun set (p, off, v) = set'(p+off, v)

    fun encodeZippedArg (arg, k, l, ty) = let
	  (* 4 elements x 8 bytes per element *)
	    val x = alloc (0w4 * defaultWidthB)
	    val _ = set(x, argOffB, encodeArg arg)
	    val _ = set(x, kindOffB, Word32.fromInt k)
	    val _ = set(x, locOffB, Word32.fromInt l)
	    val _ = set(x, tyOffB, Word32.fromInt ty)
	    in
	       x
	    end

    val hdOffB = Word32.fromInt HD * defaultWidthB
    val tlOffB = Word32.fromInt TL * defaultWidthB

    fun encodeZippedArgList args = let
	    fun loop [] = Word32.fromInt NIL
	      | loop (za :: zas) = let
		    val l = alloc(0w2 * defaultWidthB)
		    in
		        set(l, hdOffB, za);
			set(l, tlOffB, loop(zas));
			l
		    end
	    in
	        loop (List.map encodeZippedArg args)
	    end
*)				   
  end (* VarargCCallFn *)
