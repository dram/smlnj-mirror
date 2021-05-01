/*! \file AARCH64.prim.asm
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Assembly code for the AARCH64 (aka ARM64) target.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "mlstate-offsets.h"	/** this file is generated **/
#include "ml-limits.h"

/* AArch64 register usage and C function calling conventions:
 *
<<<<<<< .mine
 *	x0-x7		-- argument/result registers
 *	x8		-- indirect result location register
 *	x19-x28		-- callee-save registers
 *	x29		-- frame pointer
 *	x30		-- link register
 *	sp		-- stack pointer (x31)
||||||| .r7023
 *	r0-r7		-- argument/result registers
 *	r8		-- indirect result location register
 *	r19-r28		-- callee-save registers
 *	r29		-- frame pointer
 *	r30		-- link register
 *	sp		-- stack pointer (r31)
=======
 *	x0-x7		-- argument/result registers
 *	x8		-- indirect result location register
 *	x19-x28		-- callee-save registers
 *	x29		-- frame pointer (aka fp)
 *	x30		-- link register (aka lr)
 *	sp		-- stack pointer (aka x31)
>>>>>>> .r7086
 *
 *	x9-x15		-- temporary registers
 *	x16,x17		-- intra-procedure-call temporaries (IP0,IP1)
 *	x18		-- reserved for platform-specific use
 *
 * The stack pointer must be 16-byte aligned.
 */

/* SML Register conventions for AArch64:
 *
 *	alloc ptr
 *	limit ptr
 *	store ptr
 *	exn ptr
 *	var ptr
 *	GC link
 *	base ptr
 *	std link
 *	std clos
 *	std arg
 *	std cont
 *	misc0
 */

/* symbolic names for the CMachine and SML state registers; for some registers,
 * we need access to the 32-bit version, so we prefix those with either "x" (for
 * the 64-bit version) or "w" (for the 32-bit version).
 */
#define		allocptr	x24
#define 	limitptr	x25
#define 	storeptr	x26
#define 	exnptr		x27
#define 	varptr		x28
#define		xlink		x3
#define		wlink		w3
#define 	xclos		x2
#define 	wclos		w2
#define 	xarg		x0
#define 	warg		w0
#define 	xcont		x1
#define 	wcont		w1
#define		misc0		x4
#define		misc1		x5
#define 	misc2		x6

#define		xpc		lr
#define		wpc		w30

#define       	xtmp1		x17
#define       	wtmp1		w17
#define       	xtmp2		x21	/* also misc14 */
#define       	xtmp3		x22	/* also misc15 */
#define 	xtmp4		x23	/* also misc16 */
#define       	wtmp4		w23	/* also misc17 (32-bit view) */

#define MEM(base,offset)	[base, IM(offset)]
#define STK(offset)		MEM(sp,offset)

/* macro for returning from an SML compatible function via a return continuation */
#define CONTINUE					\
	    br		xcont

/* macro for checking the heap limit before  */
#define CHECKLIMIT(label)	 			\
	    cmp		limitptr,allocptr __SC__	\
	    b.hi	label __SC__			\
	    bl		CSYM(saveregs) __SC__		\
    label:

/**********************************************************************/
	TEXT

#define requestId	wtmp4

/* sigh_return:
 */
ALIGNED_ENTRY(sigh_return_a)
	mov	wlink,IM(ML_unit)		/* stdlink = UNIT */
	mov	wclos,IM(ML_unit)		/* stdclos = UNIT */
	mov	wgclink,IM(ML_unit)		/* pc = UNIT */
	mov	requestId,IM(REQ_SIG_RETURN)	/* requestId = REQ_SIG_RETURN */
	b	CSYM(set_request)

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */
ALIGNED_ENTRY(sigh_resume)
	mov	requestId,IM(REQ_SIG_RESUME)	/* requestId = REQ_SIG_RESUME */
	b	CSYM(set_request)

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ALIGNED_ENTRY(pollh_return_a)
	mov	w3,IM(ML_unit)			/* stdlink = UNIT */
	mov	w2,IM(ML_unit)			/* stdclos = UNIT */
	mov	wgclink,IM(ML_unit)		/* pc = UNIT */
	mov	requestId,IM(REQ_POLL_RETURN)	/* requestId = REQ_POLL_RETURN */
	b	CSYM(set_request)

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ALIGNED_ENTRY(pollh_resume)
	mov	requestId,IM(REQ_POLL_RESUME)	/* requestId = REQ_POLL_RESUME */
	b	CSYM(set_request)

/* handle:
 */
ALIGNED_ENTRY(handle_a)
	mov	xgclink,xstdlink
	mov	requestId,IM(REQ_EXN)		/* requestId = REQ_RETURN */
	b	CSYM(set_request)

/* return:
 */
ALIGNED_ENTRY(return_a)
	mov	wlink,IM(ML_unit)		/* stdlink = UNIT */
	mov	wclos,IM(ML_unit)		/* stdclos = UNIT */
	mov	wgclink,IM(ML_unit)		/* pc = UNIT */
	mov	requestId,IM(REQ_RETURN)	/* requestId = REQ_RETURN */
	b	CSYM(set_request)

/* Request a fault. */
ALIGNED_ENTRY(request_fault)
	mov	xgclink,xstdlink
	mov	requestId,IM(REQ_FAULT)		/* requestId = REQ_FAULT */
	b	CSYM(set_request)

/* bind_cfun : (string * string) -> c_function
 */
ALIGNED_ENTRY(bind_cfun_a)
	CHECKLIMIT
	mov	requestId,IM(REQ_BIND_CFUN)	/* requestId = REQ_BIND_CFUN */
	b	CSYM(set_request)

/* build_literals:
 */
ALIGNED_ENTRY(build_literals_a)
	CHECKLIMIT(build_literals_gc)
	mov	requestId,IM(REQ_BUILD_LITERALS)	/* requestId = REQ_BUILD_LITERALS */
	b	CSYM(set_request)

/* callc:
 */
ALIGNED_ENTRY(callc_a)
	CHECKLIMIT(callc_gc)
	mov	requestId,IM(REQ_CALLC)		/* requestId = REQ_CALLC */
	b	CSYM(set_request)

/* saveregs:
 * Entry point for GC.  Control is transfered using a `call` instruction,
 * so the return address is on the top of the stack.
 */
ALIGNED_ENTRY(saveregs)
	mov	requestId,IM(REQ_GC)		/* requestId = REQ_GC */
	/* fall into set_request */

/* set_request:
 * common code to switch execution from SML to runtime system.  The request
 * code will be in `requestId` (aka tmp4).
 */
ENTRY(set_request)
	ldr	xtmp1, STK(MSP_OFFSET)
    /* WARNING: here we use the "store pair" instructions to save registers
     * in the MLState struct, which means that this code depends on the layout
     * of the struct, which is as follows:
     *
     *		ml_val_t    *ml_allocPtr;
     *		ml_val_t    *ml_limitPtr;
     *		ml_val_t    ml_storePtr;
     *		ml_val_t    ml_exnCont;
     *		ml_val_t    ml_varReg;
     *		ml_val_t    ml_linkReg;
     *		ml_val_t    ml_closure;
     *		ml_val_t    ml_cont;
     *		ml_val_t    ml_calleeSave[3];
     *		ml_val_t    ml_arg;
     *		ml_val_t    ml_pc;
     *
     */
	stp	allocptr, limitptr, MEM(xtmp1, AllocPtrOffMSP)
	stp	storeptr, exnptr, MEM(xtmp1, StorePtrOffMSP)
	stp	varptr, xlink, MEM(xtmp1, VarPtrOffMSP)
	stp	xclos, xcont, MEM(xtmp1, StdClosOffMSP)
	stp	misc0, misc1, MEM(xtmp1, Misc0OffMSP)
	stp	misc2, xarg, MEM(xtmp1, Misc2OffMSP)
	st	xpc, MEM(xtmp1, PCOffMSP)
    /* note that we are leaving SML mode */
	ldr	xtmp