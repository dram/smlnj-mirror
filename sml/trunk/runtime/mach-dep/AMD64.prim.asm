/*! \file AMD64.prim.asm
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "x86-syntax.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "mlstate-offsets.h"	/** this file is generated **/
#include "ml-limits.h"

#if defined(OPSYS_LINUX) && defined(__ELF__)
/* needed to disable the execution bit on the stack pages */
.section .note.GNU-stack,"",%progbits
#endif

/*
 * AMD64 function call conventions (System V ABI):
 *
 * 	Caller save registers: rax, rcx, rdx, rsi, rdi, r8-r11
 * 	Callee save registers: rbx, rbp, r12-15.
 *	Save frame pointer (ebx) first to match standard function prelude
 * 	Floating point state is caller-save.
 * 	The first six integer arguments are passed in registers: rdi, rsi,
 *	    rdx, rcx, r8, and r9.  Additional arguments are passed on the
 *	    stack (rightmost argument pushed first).
 * 	Word-sized result returned in %rax.
 *	The stack frame must be multiple of 16 bytes
 */

/* Registers (see compiler/CodeGen/amd64/amd64CpsRegs.sml): */
#define temp		RAX
#define misc0		RBX
#define misc1		RCX
#define misc2		RDX
#define misc3		R10
#define misc4		R11
#define misc5		R12
#define misc6		R13
#define stdcont		RSI
#define stdarg		RBP
#define	stdlink		R8
#define	stdclos		R9
#define allocptr	RDI
#define limitptr	R14
#define storeptr	R15
#define stackptr        RSP

/* other reg uses */
#define creturn 	RAX

/* Stack frame offsets are w.r.t. the stack pointer.  See
 *
 *	https://smlnj-gforge.cs.uchicago.edu/svn/smlnj/dev-notes/amd64-stack-frame.numbers
 *
 * for details.
 */
#define tempmem0	REGOFF(0,RSP)
#define tempmem1	REGOFF(8,RSP)
#define tempmem2	REGOFF(16,RSP)
#define tempmem3	REGOFF(24,RSP)
#define baseptr		REGOFF(32,RSP)	/* start address of module */
#define exncont		REGOFF(40,RSP)
#define pc		REGOFF(48,RSP)	/* gcLink */
#define varptr		REGOFF(56,RSP)
#define start_gc	REGOFF(64,RSP)	/* holds address of saveregs */
#define signBit		REGOFF(208,RSP) /* ?? */
#define negateSignBit	REGOFF(216,RSP) /* ?? */

/* we put the request code in tempmem before jumping to set_request */
#define request_w	tempmem0

#define ML_STATE_OFFSET 176
#define mlstate_ptr	REGOFF(ML_STATE_OFFSET, RSP)
#define ML_SPILL_SIZE	8192
#define CALLEE_SAVE_SZB 48	/* rbx, rbp, r12-15 */
#define ML_FRAME_SIZE	(8192)	/* FIXME */

/* NOTE: this include must come after the definition of stdlink, etc. */
#include "x86-macros.h"

/**********************************************************************/
	TEXT

/* sigh_return:
 */
ML_CODE_HDR(sigh_return_a)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	MOV	(IM(REQ_SIG_RETURN), request_w)
	JMP	(CSYM(set_request))

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */
ENTRY(sigh_resume)
	MOV	(IM(REQ_SIG_RESUME), request_w)
	JMP	(CSYM(set_request))

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ML_CODE_HDR(pollh_return_a)
	MOV	(IM(REQ_POLL_RETURN), request_w)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	JMP	(CSYM(set_request))

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ENTRY(pollh_resume)
	MOV	(IM(REQ_POLL_RESUME), request_w)
	JMP	(CSYM(set_request))

/* handle:
 */
ML_CODE_HDR(handle_a)
	MOV	(IM(REQ_EXN), request_w)
	MOVE	(stdlink,temp,pc)
	JMP	(CSYM(set_request))

/* return:
 */
ML_CODE_HDR(return_a)
	MOV	(IM(REQ_RETURN), request_w)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	JMP	(CSYM(set_request))

/* Request a fault. */
ENTRY(request_fault)
	MOV	(IM(REQ_FAULT), request_w)
	MOVE	(stdlink,temp,pc)
	JMP	(CSYM(set_request))

/* bind_cfun : (string * string) -> c_function
 */
ML_CODE_HDR(bind_cfun_a)
	CHECKLIMIT
	MOV	(IM(REQ_BIND_CFUN), request_w)
	JMP	(CSYM(set_request))

/* build_literals:
 */
ML_CODE_HDR(build_literals_a)
	CHECKLIMIT
	MOV	(IM(REQ_BUILD_LITERALS), request_w)
	JMP	(CSYM(set_request))

/* callc:
 */
ML_CODE_HDR(callc_a)
	CHECKLIMIT
	MOV	(IM(REQ_CALLC), request_w)
	JMP	(CSYM(set_request))

/* saveregs:
 * Entry point for GC.  Control is transfered using a `call` instruction,
 * so the return address is on the top of the stack.
 */
ENTRY(saveregs)
	POP	(pc)
	MOV	(IM(REQ_GC), request_w)
	/* fall into set_request */

/* set_request:
 * common code to switch execution from SML to runtime system.  The request
 * code will be in `tempmem` (on the stack).
 */
ENTRY(set_request)
	/* temp holds mlstate_ptr, valid request in request_w  */
	/* Save registers */
	MOV	(mlstate_ptr, temp)
	MOV	(allocptr, REGOFF(AllocPtrOffMSP,temp))
	MOV	(stdarg, REGOFF(StdArgOffMSP,temp))
	MOV	(stdcont, REGOFF(StdContOffMSP,temp))

#define	temp2 allocptr
	/* note that we have left ML code */
	MOV	(REGOFF(VProcOffMSP,temp), temp2)
	MOV	(IM(0), REGOFF(InMLOffVSP, temp2))

	/* Save stack-allocated CPS registers before the stack frame is popped. */
	MOVE	(exncont, temp2, REGOFF(ExnPtrOffMSP, temp))
	MOVE	(varptr,  temp2, REGOFF(VarPtrOffMSP, temp))
	MOVE	(pc,      temp2, REGOFF(PCOffMSP, temp))
#undef	temp2

	/* Save remaining registers */
	MOV	(limitptr, REGOFF(LimitPtrOffMSP, temp))
	MOV	(storeptr, REGOFF(StorePtrOffMSP, temp))
	MOV	(stdclos,  REGOFF(StdClosOffMSP, temp))
	MOV	(stdlink,  REGOFF(LinkRegOffMSP, temp))
	MOV	(misc0,    REGOFF(Misc0OffMSP, temp))
	MOV	(misc1,    REGOFF(Misc1OffMSP, temp))
	MOV	(misc2,    REGOFF(Misc2OffMSP, temp))

	/* return val of function is request code */
	MOV(request_w,creturn)

	/* Pop the stack frame and return to run_ml(). */
	ADD	(IM(ML_FRAME_SIZE+8), RSP)

	/* restore C callee-save registers */
	POP	(R15)
	POP	(R14)
	POP	(R13)
	POP	(R12)
	POP	(RBX)
	POP	(RBP)
	RET

/**********************************************************************/

/* restoreregs (ml_state_t *msp):
 *
 * Switch from C to SML.
 */
#ifdef OPSYS_WIN32
/* on Windows, `restoreregs` is a C wrapper around `asm_restoreregs` that
 * handles traps (see `runtime/mach-dep/win32-fault.c`)
 */
ENTRY(asm_restoreregs)
#else
ENTRY(restoreregs)
#endif
	/* save C callee-save registers */
	PUSH	(RBP)
	PUSH	(RBX)
	PUSH	(R12)
	PUSH	(R13)
	PUSH	(R14)
	PUSH	(R15)
	/* allocate the stack frame; at this point we have 7*8 bytes allocated,
	 * so we need an addition 8 bytes to get 16-byte alignment.
	 */
	SUB	(IM(ML_FRAME_SIZE+8), RSP)

	/* move the argument (MLState ptr) to the temp register */
	MOV	(RDI, temp)

#define temp2	RBX
      /* Initialize the ML stack frame. */
	MOVE	(REGOFF(ExnPtrOffMSP, temp), temp2, exncont)
	MOVE	(REGOFF(VarPtrOffMSP, temp), temp2, varptr)
	MOVE    (REGOFF(PCOffMSP, temp),     temp2, pc)
	LEA	(CODEADDR(CSYM(saveregs)), temp2)
	MOV	(temp2, start_gc)
	MOV	(temp, mlstate_ptr)

/* unclear what the following are being used for */
	MOV	($0x8000000000000000, temp2)
	MOV	(temp2, signBit)
	MOV	($0x7fffffffffffffff, temp2)
	MOV	(temp2, negateSignBit)
#undef	temp2

	/* Load ML registers. */
	MOV	(REGOFF(AllocPtrOffMSP, temp), allocptr)
	MOV	(REGOFF(LimitPtrOffMSP, temp), limitptr)
	MOV	(REGOFF(StorePtrOffMSP, temp), storeptr)
	MOV	(REGOFF(LinkRegOffMSP, temp),  stdlink)
	MOV	(REGOFF(StdClosOffMSP, temp),  stdclos)
	MOV	(REGOFF(StdContOffMSP, temp),  stdcont)
	MOV	(REGOFF(StdArgOffMSP, temp),   stdarg)
	MOV	(REGOFF(Misc0OffMSP, temp),    misc0)
	MOV	(REGOFF(Misc1OffMSP, temp),    misc1)
	MOV	(REGOFF(Misc2OffMSP, temp),    misc2)

	PUSH	(misc2)			/* free up a register   */
	PUSH	(temp)			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we are entering ML */
	MOV	(REGOFF(VProcOffMSP,temp), temp)  /* temp is now vsp */
#define vsp	temp
	MOV	(IM(1),REGOFF(InMLOffVSP,vsp))

	/* handle signals */
	MOV	(REGOFF(SigsRecvOffVSP,vsp),RDX)
	CMP	(REGOFF(SigsHandledOffVSP,vsp),RDX)

#undef  tmpreg
	JNE	(pending)

restore_and_jmp_ml:
	POP	(temp)			/* restore temp to msp */
	POP	(misc2)

jmp_ml:
	CMP	(limitptr, allocptr)
	JMP	(CODEPTR(REGOFF(PCOffMSP,temp)))	/* Jump to ML code. */


/* QUESTION: are these fields 32-bits? */
pending:
					/* Currently handling signal? */
	CMP	(IM(0), REGOFF(InSigHandlerOffVSP,vsp))
	JNE	(restore_and_jmp_ml)
					/* handler trap is now pending */
	MOV	(IM(1),HandlerPendingOffVSP(vsp))

	/* must restore here because limitptr is on stack */ /* XXX */
	POP	(temp)			/* restore temp to msp */
	POP	(misc2)

	MOV	(allocptr,limitptr)
	JMP	(jmp_ml)			/* Jump to ML code. */
#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ML_CODE_HDR(array_a)
	CHECKLIMIT
	MOV	(REGIND(stdarg),temp)		/* temp := length in words */
	SAR	(IM(1),temp)			/* temp := length untagged */
	CMP	(IM(SMALL_OBJ_SZW),temp)	/* small object? */
	JGE	(L_array_large)
	/* use misc0 and misc1 as temporary registers */
#define temp1 misc0
#define temp2 misc1
	PUSH	(misc0)
	PUSH	(misc1)
	/* build data object descriptor in temp1 */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_arr_data)),temp1)
	/* store descriptor and bump allocation pointer */
	MOV	(temp1,REGIND(allocptr))
	ADD	(IM(8),allocptr)
	/* allocate and initialize data object */
	MOV	(allocptr,temp1)		/* temp1 := array data ptr */
	MOV	(REGOFF(8,stdarg),temp2)	/* temp2 := initial value */
LABEL(L_array_lp)
	MOV	(temp2,REGIND(allocptr))	/* init array */
	ADD	(IM(8),allocptr)
	SUB	(IM(1),temp)
	JNE	(L_array_lp)
	/* Allocate array header */
	MOV	(IM(DESC_polyarr),REGIND(allocptr)) /* descriptor */
	ADD	(IM(8),allocptr)
	MOV	(REGIND(stdarg),temp)		/* temp := length */
	MOV	(allocptr, stdarg)		/* result := header addr */
	MOV	(temp1, REGIND(allocptr))	/* store pointer to data */
	MOV	(temp, REGOFF(8,allocptr))	/* store length */
	ADD	(IM(16),allocptr)
	/* restore misc0 and misc1 */
	POP	(misc1)
	POP	(misc0)
	CONTINUE
#undef temp1
#undef temp2

	/* large arrays are allocated in the runtime system */
LABEL(L_array_large)
	MOV	(stdlink,pc)
	MOV	(IM(REQ_ALLOC_ARRAY),request_w)
	JMP	(CSYM(set_request))


/* create_r : int -> realarray */
ML_CODE_HDR(create_r_a)
	CHECKLIMIT
	MOV	(stdarg,temp)		/* temp := length */
	SAR	(IM(1),temp)		/* temp := untagged length in words */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_r_large)

#define temp1 misc0
	PUSH	(misc0)			/* use misc0 as temp1 */

	/* allocate the data object */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)	/* temp1 := descriptor */
	OR	(IM(MAKE_TAG(DTAG_raw64)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(IM(8),allocptr)		/* allocptr++ */
	MOV	(allocptr,temp1)		/* temp1 := data object */
	SAL	(IM(3),temp)			/* temp := length in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */

	/* allocate the header object */
	MOV	(IM(DESC_real64arr),REGIND(allocptr))
	ADD	(IM(8),allocptr)		/* allocptr++ */
	MOV	(temp1,REGIND(allocptr))	/* header data */
	MOV	(stdarg,REGOFF(8,allocptr))	/* header length */
	MOV	(allocptr,stdarg)		/* stdarg := header obj */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */

	POP	(misc0)
	CONTINUE
#undef temp1

LABEL(L_create_r_large)
	MOV	(stdlink,pc)
	MOV	(IM(REQ_ALLOC_REALDARRAY),request_w)
	JMP	(CSYM(set_request))


/* create_b : int -> bytearray */
ML_CODE_HDR(create_b_a)
	CHECKLIMIT
	MOV	(stdarg,temp)		/* temp is tagged length */
	SAR	(IM(1),temp)		/* temp >>= 1; (untag length) */
	ADD	(IM(3),temp)		/* temp += 7; */
	SAR	(IM(3),temp)		/* temp >>= 3; (length in 8-byte words) */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_b_large)

#define temp1 misc0
	PUSH	(misc0)

	/* allocate the data object */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_raw)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(IM(8),allocptr)
	MOV	(allocptr,temp1)		/* temp1 is data object */
	SAL	(IM(3),temp)			/* temp is size in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */

	/* allocate the header object */
	MOV	(IM(DESC_word8arr),REGIND(allocptr))
	ADD	(IM(8),allocptr)
	MOV	(temp1,REGIND(allocptr))
	MOV	(stdarg,REGOFF(8,allocptr))
	MOV	(allocptr,stdarg)		/* stdarg := header */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */
	POP	(misc0)
	CONTINUE
#undef temp1

LABEL(L_create_b_large)
	MOV	(stdlink,pc)
	MOV	(IM(REQ_ALLOC_BYTEARRAY),request_w)
	JMP	(CSYM(set_request))


/* create_s : int -> string */
ML_CODE_HDR(create_s_a)
	CHECKLIMIT
	MOV	(stdarg,temp)
	SAR	(IM(1),temp)		/* untag */
	ADD	(IM(8),temp)		/* 7 + extra byte */
	SAR	(IM(2),temp)		/* length in words */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_s_large)

	PUSH	(misc0)
#define temp1 misc0

	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_raw)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(IM(8),allocptr)

	MOV	(allocptr,temp1)		/* temp1 is data obj */
	SAL	(IM(3),temp)			/* bytes len */
	ADD	(temp,allocptr)			/* allocptr += length */
	MOV	(IM(0),REGOFF((-8),allocptr))	/* zero out last word */

	/* allocate header obj */
	MOV	(IM(DESC_string),temp)	/* hdr descr */
	MOV	(temp,REGIND(allocptr))
	ADD	(IM(8),allocptr)
	MOV	(temp1,REGIND(allocptr))	/* hdr data */
	MOV	(stdarg,REGOFF(8,allocptr))	/* hdr length */
	MOV	(allocptr, stdarg)		/* stdarg is hdr obj */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */

	POP	(misc0)
#undef temp1
	CONTINUE

LABEL(L_create_s_large)
	MOVE	(stdlink, temp, pc)
	MOV	(IM(REQ_ALLOC_STRING),request_w)
	JMP	(CSYM(set_request))

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ML_CODE_HDR(create_v_a)
	CHECKLIMIT
	MOV	(REGIND(stdarg),temp)		/* temp = len tagged */
	PUSH	(misc0)
	PUSH	(misc1)
#define temp1 misc0
#define temp2 misc1
	MOV	(temp,temp1)
	SAR	(IM(1),temp1)			/* temp1 = untagged len */
	CMP	(IM(SMALL_OBJ_SZW),temp1)
	JGE	(L_create_v_large)

	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_vec_data)),temp1)
	MOV	(temp1,REGIND(allocptr))
	ADD	(IM(8),allocptr)
	MOV	(REGOFF(8,stdarg),temp1)	/* temp1 is list */
	MOV	(allocptr,stdarg)		/* stdarg is vector */

LABEL(L_create_v_lp)
	MOV	(REGIND(temp1),temp2)		/* hd */
	MOV	(temp2,REGIND(allocptr))	/* store into vector */
	ADD	(IM(8),allocptr)
	MOV	(REGOFF(8,temp1),temp1)		/* tl */
	CMP	(IM(ML_nil),temp1)		/* isNull? */
	JNE	L_create_v_lp

	/* allocate header object */
	MOV	(IM(DESC_polyvec),temp1)
	MOV	(temp1,REGIND(allocptr))
	ADD	(IM(8),allocptr)
	MOV	(stdarg,REGIND(allocptr))	/* data */
	MOV	(temp,REGOFF(8,allocptr))	/* len */
	MOV	(allocptr,stdarg)		/* result */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */

	POP	(misc1)
	POP	(misc0)
	CONTINUE
#undef temp1
#undef temp2

LABEL(L_create_v_large)
	MOVE	(stdlink, temp, pc)
	MOV	(IM(REQ_ALLOC_VECTOR),request_w)
	JMP	(CSYM(set_request))

/* try_lock: spin_lock -> bool.
 * low-level test-and-set style primitive for mutual-exclusion among
 * processors.	For now, we only provide a uni-processor trivial version.
 */
ML_CODE_HDR(try_lock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV	(REGIND(stdarg), temp)		/* Get old value of lock. */
	MOV	(IM(1), REGIND(stdarg))	/* Set the lock to ML_false. */
	MOV	(temp, stdarg)			/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock
 */
ML_CODE_HDR(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV	(IM(3), REGIND(stdarg))	/* Store ML_true into lock. */
	MOV	(IM(1), stdarg)		/* Return unit. */
	CONTINUE
#endif


/********************* Floating point functions. *********************/

/* rounding modes (see Table 4-14 in the Instruction Set Reference) */
#define	RND_TO_NEGINF	IM(9)
#define RND_TO_POSINF	IM(10)
#define RND_TO_ZERO	IM(11)

	TEXT
	.align 8

/* floor : real -> int
   Return the nearest integer that is less or equal to the argument.
	 Caller's responsibility to make sure arg is in range. */

ML_CODE_HDR(floor_a)
	MOVSD		(REGIND(stdarg), XMM0)
	ROUNDSD		(RND_TO_NEGINF, XMM0, XMM0)
	CVTTSD2SI	(XMM0, stdarg)
	CONTINUE

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ML_CODE_HDR(logb_a)
	/* DEPRECATED */
	CONTINUE


/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml). */

ML_CODE_HDR(scalb_a)
	CHECKLIMIT
	/* FIXME: need implementation */
	CONTINUE

END

/* end of AMD64.prim.asm */
