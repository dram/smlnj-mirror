(* ia32-svid.sml
 *
 * COPYRIGHT (c) 2000 Bell Labs, Lucent Technologies
 *
 * C function calls for the IA32 using the System V ABI
 *
 * Register conventions:
 *
 *    %eax	return value		(caller save)
 *    %ebx	global offset for PIC	(callee save)
 *    %ecx	scratch			(caller save)
 *    %edx	extra return/scratch	(caller save)
 *    %ebp	optional frame pointer	(callee save)
 *    %esp	stack pointer		(callee save)
 *    %esi	locals			(callee save)
 *    %edi	locals			(callee save)
 *
 *    %st(0)	top of FP stack; FP return value
 *    %st(1..7)	FP stack; must be empty on entry and return
 *
 * Calling convention:
 *
 *    Return result:
 *	+ Integer and pointer results are returned in %eax
 *	+ 64-bit integers (long long) returned in %eax/%edx
 *	+ Floating point results are returned in %st(0) (all types).
 *	+ Struct results are returned in space provided by the caller.
 *	  The address of this space is passed to the callee as an
 *	  implicit 0th argument, and on return %eax contains this
 *	  address.
 *
 *    Function arguments:
 *	+ Arguments are pushed on the stack right to left.
 *	+ Integral and pointer arguments take one word on the stack.
 *	+ float arguments take one word on the stack.
 *	+ double arguments take two words on the stack.  The i386 ABI does
 *	  not require double word alignment for these arguments.
 *	+ long double arguments take three words on the stack.
 *	+ struct arguments are padded out to word length.
 *
 * Questions:
 *    - what about stack frame alignment?
 *)

functor IA32SVID_CCalls (
    structure T : MLTREE
    val ix : (T.stm,T.rexp,T.fexp,T.ccexp) X86InstrExt.sext -> T.sext
  (* Note that the fast_loating_point flag must match the one passed
   * to the code generator module.
   *)
    val fast_floating_point : bool ref
  ) : C_CALLS = struct

    structure T  = T
    structure Ty = CTypes
    structure C = X86Cells
    structure IX = X86InstrExt

    fun error msg = MLRiscErrorMsg.error ("IA32SVID_CCalls", msg)

    datatype  c_arg 
      = ARG of T.rexp	    
      | FARG of T.fexp
      | ARGS of c_arg list

    val mem = T.Region.memory
    val stack = T.Region.memory

  (* MLRISC types *)
    val wordTy = 32
    val fltTy = 32
    val dblTy = 64
    val xdblTy = 80

  (* shorts and chars are promoted to 32-bits *)
    val naturalIntSz = wordTy

    val paramAreaOffset = 0 (* stack offset to param area *)

  (* This annotation is used to indicate that a call returns a fp value 
   * in %st(0) 
   *)
    val fpReturnValueInST0 = #create MLRiscAnnotations.RETURN_ARG C.ST0

    val sp = C.esp
    val spR = T.REG(wordTy, sp)

    fun fpr(sz,f) = T.FPR(T.FREG(sz, f))
    fun gpr(sz,r) = T.GPR(T.REG(sz, r))
    val eax = C.eax
    val st0 = C.ST(0)

  (* note that the caller saves includes the result register (%eax) *)
    val callerSaves = [gpr(wordTy, eax), gpr(wordTy, C.ecx), gpr(wordTy, C.edx)]

  (* C callee-save registers *)
    val calleeSaveRegs = [C.ebx, C.esi, C.edi]	(* C callee-save registers *)
    val calleeSaveFRegs = []			(* C callee-save floating-point registers *)

  (* align the address to the given alignment, which must be a power of 2 *)
    fun alignAddr (addr, align) = let
	  val mask = Word.fromInt(align-1)
	  in
	    Word.toIntX(Word.andb(Word.fromInt addr + mask, Word.notb mask))
	  end

    fun align4 addr = Word.toIntX(Word.andb(Word.fromInt addr + 0w3, Word.notb 0w3))

  (* size and natural alignment for integer types. *)
    fun sizeOfInt Ty.I_char = {ty = 8, sz = 1, align = 1}
      | sizeOfInt Ty.I_short = {ty = 16, sz = 2, align = 2}
      | sizeOfInt Ty.I_int = {ty = 32, sz = 4, align = 4}
      | sizeOfInt Ty.I_long = {ty = 32, sz = 4, align = 4}
      | sizeOfInt Ty.I_long_long = {ty = 64, sz = 8, align = 4}

  (* sizes of other C types *)
    val sizeOfPtr = {ty = 32, sz = 4, align = 4}

  (* compute the size and alignment information for a struct; tys is the list
   * of member types.
   * The total size is padded to agree with the struct's alignment.
   *)
    fun sizeOfStruct tys = let
	  fun ssz ([], maxAlign, offset) =
		{sz = alignAddr(offset, maxAlign), align = maxAlign}
	    | ssz (ty::tys, maxAlign, offset) = let
		  val {sz, align} = sizeOfTy ty
		  val offset = alignAddr(offset, align)
		  in
		    ssz (tys, Int.max(maxAlign, align), offset+sz)
		  end
	  in
	    ssz (tys, 1, 0)
	  end

  (* the size alignment of a union type is the maximum of the sizes and alignments of the
   * members.  The final size is padded to agree with the alignment.
   *)
    and sizeOfUnion tys = let
	  fun usz ([], maxAlign, maxSz) =
		{sz = alignAddr(maxSz, maxAlign), align = maxAlign}
	    | usz (ty::tys, maxAlign, maxSz) = let
		  val {sz, align} = sizeOfTy ty
		  in
		    usz (tys, Int.max(maxAlign, align), Int.max(sz, maxSz))
		  end
	  in
	    usz (tys, 1, 0)
	  end

    and sizeOfTy Ty.C_void = error "unexpected void argument type"
      | sizeOfTy Ty.C_float = {sz = 4, align = 4}
      | sizeOfTy Ty.C_double = {sz = 8, align = 4}
      | sizeOfTy Ty.C_long_double = {sz = 12, align = 4}
      | sizeOfTy (Ty.C_unsigned isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy (Ty.C_signed isz) = let
	  val {sz, align, ...} = sizeOfInt isz
	  in
	    {sz = sz, align = align}
	  end
      | sizeOfTy Ty.C_PTR = {sz = 4, align = 4}
      | sizeOfTy (Ty.C_ARRAY(ty, n)) = let
	  val {sz, align} = sizeOfTy ty
	  in
	    {sz = n*sz, align = align}
	  end
      | sizeOfTy (Ty.C_STRUCT tys) = sizeOfStruct tys
      | sizeOfTy (Ty.C_UNION tys) = sizeOfUnion tys

  (* the location of arguments/parameters; offsets are given with respect to the
   * low end of the parameter area (see paramAreaOffset above).
   *)
    datatype arg_location
      = Reg of T.ty * T.reg * T.I.machine_int option
					(* integer/pointer argument in register *)
      | FReg of T.fty * T.reg * T.I.machine_int option
					(* floating-point argument in register *)
      | Stk of T.ty * T.I.machine_int	(* integer/pointer argument in parameter area *)
      | FStk of T.fty * T.I.machine_int	(* floating-point argument in parameter area *)
      | Args of arg_location list

    fun layout {conv, retTy, paramTys} = let
	(* get the location of the result (resLoc) and the offset of the first
	 * parameter/argument.  If the result is a struct or union, then we also
	 * compute the size and alignment of the result type (structRetLoc).
	 *)
	  val (resLoc, structRetLoc, argOffset) = (case retTy
		 of Ty.C_void => (NONE, NONE, 0)
		  | Ty.C_float => (SOME(FReg(fltTy, st0, NONE)), NONE, 0)
		  | Ty.C_double => (SOME(FReg(dblTy, st0, NONE)), NONE, 0)
		  | Ty.C_long_double => (SOME(FReg(xdblTy, st0, NONE)), NONE, 0)
		  | Ty.C_unsigned I_long_long => raise Fail "register pair"
		  | Ty.C_signed I_long_long => raise Fail "register pair"
		  | Ty.C_PTR => (SOME(Reg(wordTy, eax, NONE)), NONE, 0)
		  | Ty.C_ARRAY _ => error "array return type"
		  | Ty.C_STRUCT tys => let
		      val {sz, align} = sizeOfStruct tys
		      in
			(SOME(Reg(wordTy, eax, NONE)), SOME{szb=sz, align=align}, 4)
		      end
		  | Ty.C_UNION tys => let
		      val {sz, align} = sizeOfUnion tys
		      in
			(SOME(Reg(wordTy, eax, NONE)), SOME{szb=sz, align=align}, 4)
		      end
		(* end case *))
	  fun assign ([], offset, locs) = (List.rev locs, align4 offset)
	    | assign (paramTy::params, offset, locs) = let
		fun next {ty, align, sz} = let
		      val offset = alignAddr (offset, align)
		      in
			assign (params, offset+sz, Stk(ty, IntInf.fromInt offset)::locs)
		      end
		fun nextFlt (ty, szb) = let
		      val offset = alignAddr (offset, 4)
		      in
			assign (params, offset+szb, FStk(ty, IntInf.fromInt offset)::locs)
		      end
		fun assignMem {sz, align} = let
		      fun f (nb, offset, locs') =
			    if (nb >= 4)
			      then f(nb-4, offset+4, Stk(wordTy, IntInf.fromInt offset)::locs')
			    else if (nb >= 2)
			      then f(nb-2, offset+2, Stk(16, IntInf.fromInt offset)::locs')
			    else if (nb = 1)
			      then f(nb, offset+1, Stk(8, IntInf.fromInt offset)::locs')
			      else assign(params, align4 offset, Args(List.rev locs')::locs)
		      in
			f (sz, offset, [])
		      end
		in
		  case paramTy
		   of Ty.C_void => error "void argument type"
		    | Ty.C_float => nextFlt (fltTy, 4)
		    | Ty.C_double => nextFlt (dblTy, 8)
		    | Ty.C_long_double => nextFlt (xdblTy, 12)
		    | Ty.C_unsigned iTy => next (sizeOfInt iTy)
		    | Ty.C_signed iTy => next (sizeOfInt iTy)
		    | Ty.C_PTR => next sizeOfPtr
		    | Ty.C_ARRAY _ => next sizeOfPtr
		    | Ty.C_STRUCT tys => assignMem(sizeOfStruct tys)
		    | Ty.C_UNION tys => assignMem(sizeOfUnion tys)
		  (* end case *)
		end
	  val (argLocs, argSz) = assign (paramTys, argOffset, [])
	  in {
	    argLocs = argLocs, argMem = {szb = argSz, align = 4},
	    resLoc = resLoc, structRetLoc = structRetLoc
	  } end

  (* List of registers defined by a C Call with the given return type; this list
   * is the result registers plus the caller-save registers.
   *)
    fun definedRegs (Ty.C_float) = fpr(fltTy, st0) :: callerSaves
      | definedRegs (Ty.C_double) = fpr(dblTy, st0) :: callerSaves
      | definedRegs (Ty.C_long_double) = fpr(xdblTy, st0) :: callerSaves
      | definedRegs (Ty.C_unsigned(Ty.I_long_long)) = gpr(wordTy, C.edx) :: callerSaves
      | definedRegs (Ty.C_signed(Ty.I_long_long)) = gpr(wordTy, C.edx) :: callerSaves
      | definedRegs _ = callerSaves

    fun fstp (32, f) = T.EXT(ix(IX.FSTPS(f)))
      | fstp (64, f) = T.EXT(ix(IX.FSTPL(f)))
      | fstp (80, f) = T.EXT(ix(IX.FSTPT(f)))
      | fstp (sz, f) = error ("fstp(" ^ Int.toString sz ^ ",_)")

    fun genCall {
	    name, proto, paramAlloc, structRet, saveRestoreDedicated, callComment, args
	  } = let
	  val {argLocs, argMem, resLoc, structRetLoc} = layout proto
	(* instruction to allocate space for arguments *)
	  val argAlloc = if ((#szb argMem > 0) andalso paramAlloc argMem)
		then [T.MV(wordTy, sp, T.SUB(wordTy, spR, T.LI(IntInf.fromInt(#szb argMem))))]
		else []
	(* for functions that return a struct/union, pass the location
	 * as an implicit first argument.
	 *)
	  val (args, argLocs) = (case structRetLoc
		 of SOME pos => (ARG(structRet pos)::args, Stk(wordTy, 0)::argLocs)
		  | NONE => (args, argLocs)
		(* end case *))
	(* generate instructions to copy arguments into argument area
	 * using %esp to address the argument area.
	 *)
	  val copyArgs = let
		fun offSP 0 = spR
		  | offSP offset = T.ADD(wordTy, spR, T.LI offset)
		fun f ([], [], stms) = List.rev stms
		  | f (arg::args, loc::locs, stms) = let
			val stms = (case (arg, loc)
			       of (ARG(rexp as T.REG _), Stk(mty, offset)) =>
				    T.STORE(mty, offSP offset, rexp, stack)
				      :: stms
				| (ARG rexp, Stk(mty, offset)) => let
				    val tmp = C.newReg()
				    in
				      T.STORE(wordTy, offSP offset, T.REG(wordTy, tmp), stack)
					:: T.MV(wordTy, tmp, rexp)
					:: stms
				    end
				| (ARG rexp, Args memLocs) => let
				    val (loadAddr, addrR) = (case rexp
					   of T.REG _ => ([], rexp)
					    | _ => let
						val r = C.newReg()
						in
						  ([T.MV(wordTy, r, rexp)], T.REG(wordTy, r))
						end
					  (* end case *))
				    fun addr 0 = addrR
				      | addr offset = T.ADD(wordTy, addrR, T.LI offset)
				    fun copy ([], stms) = stms
				      | copy (Stk(ty, offset)::locs, stms) = let
					  val tmp = C.newReg()
					  in
					    T.STORE(ty, offSP offset, T.REG(ty, tmp), stack)
					      :: T.MV(ty, tmp, addr offset)
					      :: stms
					  end
				      | copy _ = error "bogus memory location"
				    in
				      copy (memLocs, loadAddr @ stms)
				    end
				| (FARG(fexp as T.FREG _), FStk(ty, offset)) =>
				    T.FSTORE(ty, offSP offset, fexp, stack) :: stms
				| (FARG fexp, FStk(ty, offset)) => let
				    val tmp = C.newFreg()
				    in
				      T.FSTORE(ty, offSP offset, T.FREG(ty, tmp), stack)
					:: T.FMV(ty, tmp, fexp)
					:: stms
				    end
				| (ARGS _, _) => raise Fail "ARGS obsolete"
				| _ => error "impossible location"
			      (* end case *))
			in
			  f (args, locs, stms)
			end
		  | f _ = error "argument arity error"
		in
		  f (args, argLocs, [])
		end
	(* the SVID specifies that the caller pops arguments, but a the callee
	 * pops the arguments in a stdcall on Windows.  I'm not sure what other
	 * differences there might be between the SVID and Windows ABIs. (JHR)
	 *)
	  val calleePops = (case #conv proto
		 of (""|"ccall") => false
		  | "stdcall" => true
		  | conv => error (concat [
			"unknown calling convention \"", String.toString conv, "\""
		    ])
		(* end case *))
	  val defs = definedRegs(#retTy proto)
	  val { save, restore } = saveRestoreDedicated defs
	  val callStm = T.CALL{
		  funct=name, targets=[], defs=defs, uses=[], 
		  region = T.Region.memory,
		  pops = if calleePops then Int32.fromInt(#szb argMem) else 0
		}
	  val callStm = (case callComment
		 of NONE => callStm
		  | SOME c => T.ANNOTATION (callStm, #create MLRiscAnnotations.COMMENT c)
		(* end case *))
	(* If return type is floating point then add an annotation RETURN_ARG 
	 * This is currently a hack.  Eventually MLTREE *should* support
	 * return arguments for CALLs.
	 * --- Allen
	 *)
	  val callStm = if !fast_floating_point
		andalso ((#retTy proto = Ty.C_float)
		  orelse (#retTy proto = Ty.C_double)
		  orelse (#retTy proto = Ty.C_long_double))
		then T.ANNOTATION(callStm, fpReturnValueInST0)
		else callStm
	(* code to pop the arguments from the stack *)
	  val popArgs = if calleePops
		then []
		else [T.MV(wordTy, sp, T.ADD(wordTy, spR, T.LI(IntInf.fromInt(#szb argMem))))]
	(* code to copy the result into fresh pseudo registers *)
	  val (resultRegs, copyResult) = (case resLoc
		 of NONE => ([], [])
		  | SOME(Reg(ty, r, _)) => let
		      val resReg = C.newReg()
		      in
			([T.GPR(T.REG(ty, resReg))], [T.COPY(ty, [resReg], [r])])
		      end
		  | SOME(FReg(ty, r, _)) => let
		      val resReg = C.newFreg()
		      val res = [T.FPR(T.FREG(ty, r))]
		      in
        	      (* If we are using fast floating point mode then do NOT 
        	       * generate FSTP.
        	       * --- Allen 
        	       *)
			if !fast_floating_point
			  then (res, [T.FCOPY(ty, [resReg], [r])])
			  else (res, [fstp(ty, T.FREG(ty, resReg))])
		      end
		  | _ => error "bogus result location"
		(* end case *))
	(* assemble the call sequence *)
	  val callSeq = copyArgs @ save @ [callStm] @ restore @ popArgs @ copyResult
	  in
	    {callseq=callSeq, result=resultRegs}
	  end

  end

