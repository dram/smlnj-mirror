/*! \file ARM64.prim.asm
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Assembly code for the ARM64 (aka AARCH64) target.
 */

#include "ml-base.h"
#include "asm-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "mlstate-offsets.h"	/** this file is generated **/
#include "ml-limits.h"

/* Register usage and C function calling conventions:
 *
 *	r0-r7		-- argument/result registers
 *	r8		-- indirect result location register
 *	r9-r15		-- temporary registers
 *	r16,r17		-- intra-procedure-call temporaries (IP0,IP1)
 *	r18		-- reserved for platform-specific use
 *	r19-r28		-- callee-save registers
 *	r29		-- frame pointer
 *	r30		-- link register
 *	sp		-- stack pointer (r31)
 *
 * The stack pointer must be 16-byte aligned.
 */

/* SML Register conventions for ARM64:
 *
 *	alloc ptr	-- x24
 *	limit ptr	-- x25
 *	store ptr	-- x26
 *	exn ptr		-- x27
 *	var ptr		-- x28
 *	GC link		-- lr (aka x30)
 *	base ptr	-- not defined for ARM64
 *	std link	-- x3
 *	std clos	-- x2
 *	std arg		-- x0
 *	std cont	-- x1
 *	misc0		-- x4
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
#define       	wtmp2		w21
#define       	xtmp3		x22	/* also misc15 */
#define 	xtmp4		x23	/* also misc16 */
#define       	wtmp4		w23	/* also misc17 (32-bit view) */

/* the zero register */
#define         xzero           xzr
#define         wzero           wzr

/* reference memory at address `base + offset` */
#define MEM(base,offset)	[base, IM(offset)]
/* reference a stack location */
#define STK(offset)		MEM(sp,offset)
/* pre-increment memory reference; address is base + offset and base := base + offset */
#define PREINC(base,offset)     [base, IM(offset)]!
/* post-increment memory reference; address is base and base := base + offset */
#define POSTINC(base,offset)     [base], IM(offset)

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
	mov	wpc,IM(ML_unit)		/* pc = UNIT */
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
	mov	wpc,IM(ML_unit)		/* pc = UNIT */
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
	mov	xpc,xlink
	mov	requestId,IM(REQ_EXN)		/* requestId = REQ_RETURN */
	b	CSYM(set_request)

/* return:
 */
ALIGNED_ENTRY(return_a)
	mov	wlink,IM(ML_unit)		/* stdlink = UNIT */
	mov	wclos,IM(ML_unit)		/* stdclos = UNIT */
	mov	wpc,IM(ML_unit)		/* pc = UNIT */
	mov	requestId,IM(REQ_RETURN)	/* requestId = REQ_RETURN */
	b	CSYM(set_request)

/* Request a fault. */
ALIGNED_ENTRY(request_fault)
	mov	xpc,xlink
	mov	requestId,IM(REQ_FAULT)		/* requestId = REQ_FAULT */
	b	CSYM(set_request)

/* bind_cfun : (string * string) -> c_function
 */
ALIGNED_ENTRY(bind_cfun_a)
	CHECKLIMIT(bind_cfun_limit)
	mov	requestId,IM(REQ_BIND_CFUN)	/* requestId = REQ_BIND_CFUN */
	b	CSYM(set_request)

/* build_literals:
 */
ALIGNED_ENTRY(build_literals_a)
	CHECKLIMIT(build_literals_limit)
	mov	requestId,IM(REQ_BUILD_LITERALS)	/* requestId = REQ_BUILD_LITERALS */
	b	CSYM(set_request)

/* callc:
 */
ALIGNED_ENTRY(callc_a)
	CHECKLIMIT(callc_limit)
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
	str	xpc, MEM(xtmp1, PCOffMSP)
    /* note that we are leaving SML mode */
/* TODO */

    /* return result is request code */
/* TODO */

    /* restore C callee-save registers */
/* TODO */

/**********************************************************************/

/* restoreregs (ml_state_t *msp):
 *
 * Switch from C to SML.
 */
ALIGNED_ENTRY(restoreregs)
    /* set up stack frame */
    /* save C callee-save registers */
/* TODO */



/**********************************************************************/

/** Primitive object allocation routines **/

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array
 */
ALIGNED_ENTRY(array_a)
	CHECKLIMIT(L_array_limit)
        ldr     xtmp1, MEM(xarg, 0)     /* xtmp1 := length of array (tagged) */
        asr     xtmp2, xtmp1, IM(1)     /* xtmp2 := (xtmp1 >> 1) -- untag length */
        cmp     xtmp2, IM(SMALL_OBJ_SZW) /* if (xtmp2 <= SMALL_OBJ_SZW) goto array_large */
        b.hi    L_array_large

        ldr     xarg, MEM(xarg,8)                   /* arg := initial data value */
        /* build descriptor in tmp4 */
        mov     wtmp4, IM(MAKE_TAG(DTAG_arr_data))
        orr     xtmp4, xtmp4, xtmp1, lsl IM(TAG_SHIFTW)
        str     xtmp4, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = descriptor */
        mov     xtmp3, allocptr                     /* tmp3 = array data object */

        /* initialization loop; note that we assume len > 0 */
L_array_lp:
        str     xarg, POSTINC(allocptr, WORD_SZB)   /* *allocptr++ = initial value */
        subs    xtmp2, xtmp2, IM(1)                 /* xtmp2 := xtmp2 - 1 */
        b.ne    L_array_lp                          /* if (xtmp2 <> 0) goto lp */

        /* allocate the header object */
        mov     wtmp4, IM(DESC_polyarr)
        str     xtmp4, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = DESC_polyarr */
        mov     xarg, allocptr                      /* arg = header object */
        str     xtmp3, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = data object */
        str     xtmp1, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = tagged length */
        CONTINUE

/* TODO */
L_array_large:                          /* else (xtmp2 > SMALL_OBJ_SZW) */
	mov	requestId,IM(REQ_ALLOC_ARRAY)
        mov     xpc,xlink
	b	CSYM(set_request)

/* create_v_a : (int * 'a list) -> 'a vector
 * Create a vector with elements taken from a non-null list.
 */
ALIGNED_ENTRY(create_v_a)
	CHECKLIMIT(L_create_v_limit)
/* TODO */

/* create_b : int -> bytearray
 * Create an uninitialized byte array of the given length.
 */
ALIGNED_ENTRY(create_b_a)
	CHECKLIMIT(L_create_b_limit)
        asr     xtmp1, xarg, IM(1)              /* tmp1 := untagged length */
        add     xtmp1, xtmp1, IM(7)
        asr     xtmp1, xtmp1, IM(3)             /* tmp1 := length in words */
        cmp     xtmp1, IM(SMALL_OBJ_SZW)        /* if (xtmp2 <= SMALL_OBJ_SZW) */
        b.hi    L_create_b_large                /*    then goto create_b_large */
        /* build descriptor in tmp2 */
        mov     wtmp2, IM(MAKE_TAG(DTAG_raw))
        orr     xtmp2, xtmp2, xarg, lsl IM(TAG_SHIFTW)
        str     xtmp2, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = descriptor */
        mov     xtmp3, allocptr                     /* tmp3 = data object */
        add     allocptr, allocptr, xtmp1, lsl IM(3) /* allocptr += length */
        /* allocate header object */
        mov     wtmp4, IM(DESC_word8arr)
        str     xtmp4, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = DESC_word8arr */
        mov     xtmp1, allocptr                     /* xtmp1 = header object */
        str     xtmp3, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = data object */
        str     xarg, POSTINC(allocptr, WORD_SZB)   /* *allocptr++ = tagged length */
        mov     xarg, xtmp1
        CONTINUE

L_create_b_large:                                   /* else (xtmp2 > SMALL_OBJ_SZW) */
	mov	requestId,IM(REQ_ALLOC_BYTEARRAY)
        mov     xpc,xlink
	b	CSYM(set_request)

/* create_s_a: int -> string
 * Create an uninitialized byte vector (aka string) of the given length.
 * This function includes an additional byte at the end to hold a null character.
 */
ALIGNED_ENTRY(create_s_a)
	CHECKLIMIT(L_create_s_limit)
        asr     xtmp1, xarg, IM(1)              /* tmp1 := untagged length */
        add     xtmp1, xtmp1, IM(8)
        asr     xtmp1, xtmp1, IM(3)             /* tmp1 := length in words (incl. null) */
        cmp     xtmp1, IM(SMALL_OBJ_SZW)        /* if (xtmp2 <= SMALL_OBJ_SZW) */
        b.hi    L_create_b_large                /*    then goto create_b_large */
        /* build descriptor in tmp2 */
        mov     wtmp2, IM(MAKE_TAG(DTAG_raw))
        orr     xtmp2, xtmp2, xarg, lsl IM(TAG_SHIFTW)
        str     xtmp2, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = descriptor */
        mov     xtmp3, allocptr                     /* tmp3 = data object */
        add     allocptr, allocptr, xtmp1, lsl IM(3) /* allocptr += length */
        str     xzero, MEM(allocptr, -8)            /* zero out last word */
        /* allocate header object */
        mov     wtmp4, IM(DESC_string)
        str     xtmp4, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = DESC_word8arr */
        mov     xtmp1, allocptr                     /* xtmp1 = header object */
        str     xtmp3, POSTINC(allocptr, WORD_SZB)  /* *allocptr++ = data object */
        str     xarg, POSTINC(allocptr, WORD_SZB)   /* *allocptr++ = tagged length */
        mov     xarg, xtmp1
        CONTINUE

L_create_s_large:                                   /* else (xtmp2 > SMALL_OBJ_SZW) */
	mov	requestId,IM(REQ_ALLOC_STRING)
        mov     xpc,xlink
	b	CSYM(set_request)


/* create_r : int -> real64array
 * Create an uninitialized real64 array of the given length.
 */
ALIGNED_ENTRY(create_r_a)
	CHECKLIMIT(L_create_r_limit)
/* TODO */

L_create_r_large:                                   /* else (xtmp2 > SMALL_OBJ_SZW) */
	mov	requestId,IM(REQ_ALLOC_REALDARRAY)
        mov     xpc,xlink
	b	CSYM(set_request)

/**********************************************************************/

/** Floating-point utility functions **/

/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 */
ALIGNED_ENTRY(logb_a)
        /* DEPRECATED */
	CONTINUE

/* floor : real -> int
 * Return the nearest integer that is less or equal to the argument.
 * It is the caller's responsibility to make sure arg is in range.
 */
ALIGNED_ENTRY(floor_a)
        ldr     d0, MEM(xarg, 0)
        fcvtms  xtmp1, d0
        mov     warg, IM(1)
        bfi     xarg, xtmp1, IM(1), IM(63)  /* bit-field insert xtmp1 into xarg */
        CONTINUE

#define SIGN_MASK	IM(0x8000000000000000)
#define EXP_MASK	IM(0x7ff0000000000000)
#define NOT_EXP_MASK	IM(0x800fffffffffffff)

/* scalb : real * int -> real
 *	scalb (x, n) = x * 2^n
 */
ALIGNED_ENTRY(scalb_a)
        ldr     xtmp1, MEM(xarg, 8)         /* xtmp1 := #2(arg) */
        asr     xtmp1, xtmp1,IM(1)          /* xtmp1 := (xtmp1 >> 1) -- untag */
        ldr     xarg, MEM(xarg, 0)          /* arg := #1(arg) -- ptr to boxed real */
        ldr     xtmp2, MEM(xarg, 0)         /* xtmp2 := bits(x) */
        ands    xtmp3, xtmp2, EXP_MASK      /* xmp3 := biased exponent */
        b.eq    L_scalb_return              /* if (xtmp3 == 0) return */
                                            /* xtmp1 := xtmp1 + (xtmp3 >> 52) */
        add     xtmp1, xtmp1, xtmp3, lsr IM(52)
        cmp     xtmp1, IM(0)                /* if (xtmp1 <= 0) goto scalb_under */
        b.le    L_scalb_under
        cmp     xtmp1, IM(2047)             /* if (xtmp1 >= 2047) goto scalb_over */
        b.ge    L_scalb_over
        and     xtmp2, xtmp2, NOT_EXP_MASK  /* xtmp2 := non-exponent bits */
                                            /* xtmp1 := xtmp2 | (xtmp1 << 52) */
        orr     xtmp1, xtmp2, xtmp1, lsl IM(52)

L_scalb_alloc:
        /* here we have the result in xtmp1 */
        mov     xtmp2, IM(DESC_reald)
        str     xtmp2, POSTINC(allocptr, 0) /* *allocptr++ = DESC_reald */
        mov     xarg, allocptr              /* arg = allocptr */
        str     xtmp1, POSTINC(allocptr, 0) /* *allocptr++ = tmp1 */

L_scalb_return:
        /* here the biased exponent was 0, which means that `x` is either 0.0 or
         * denormalized, so we just return `x`.  The boxed `x` is already in `xarg`.
         */
        CONTINUE

L_scalb_under:
        /* here we have underflow, so we return 0.0 */
        mov     xtmp1, IM(0)
        b       L_scalb_alloc

L_scalb_over:
        /* return infinity with the correct sign */
        and     xtmp1, xtmp2, SIGN_MASK     /* xtmp1 := sign(bits) */
        orr     xtmp1, xtmp1, EXP_MASK      /* xtmp1 := (xtmp1 | exponent(2047)) */
        b       L_scalb_alloc
