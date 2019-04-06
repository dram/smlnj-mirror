/* SPARC.prim.s
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 * SPARC runtime code for ML.  Registers are used as follows:
 *
 * %g7 : exception handler continuation
 * %g6 : freespace pointer
 * %g5 : store pointer
 * %g4 : heap limit pointer
 *
 * %i0 = arg
 * %i1 = continuation
 * %i2 = closure; can be ignored because contains no free vars
 * %i3 = base code pointer
 * %i5 = global pointer register (used for instrumentation purposes)
 *
 * %o0,o1,%g1-%g3,%l0-%l7,%i4 = misc. registers (contain only pointers or tagged ints)
 * %o5 = pointer temp.
 *
 * %o2,%o3 = used for args to ml_mul & ml_div, also used as temp registers
 * %o4,%o5 = temp registers
 *
 * %o6 = %sp (not used by ML)
 * %i6 = %fp (not used by ML)
 * %i7 = return address to C code (not used by ML)
 * %o7 = not used
 *
 * The ML state structure has the following layout for the SPARC (see "ml_state.h"):
 *
 *		+-------------------+
 *  MLState --> | ml_allocptr (%g6) |
 *		+-------------------+
 *	+4:	| ml_limitptr (%g4) |
 *		+-------------------+
 *	+8:	| ml_storeptr (%g5) |
 *		+-------------------+
 *	+12:	|       ml_pc       |
 *		+-------------------+
 *	+16:	|   ml_arg (%i0)    |
 *		+-------------------+
 *	+20:	|   ml_cont (%i1)   |
 *		+-------------------+
 *	+24:	| ml_closure (%i2)  |
 *		+-------------------+
 *	+28:	|     (%i3-%i5)     |
 *		+-------------------+
 *	+40:	| ml_exncont (%g7)  |
 *		+-------------------+
 *	+44:	|        (%g1)      |
 *		+-------------------+
 *	+48:	|     (%g2-%g3)     |
 *		+-------------------+
 *	+56:	|     (%l0-%l7)     |
 *		+-------------------+
 *	+88:	|     (%o0-%o1)     |
 *		+-------------------+
 *
 * There are three places in this file where garbage collection can be triggered:
 * in array_v, create_s_v, and scalb_v.  It it important that only
 * registers saved by _saveregs be live at these points. Also, only pointer
 * registers can contain heap pointers at these points.
 */

#include <machine/asm_linkage.h>
#include <machine/trap.h>
#include "tags.h"
#include "request.h"

/* Macros to fetch and store values in memory; use %o3 as an addressing register. */
#define FETCH(addr, reg)			\
	    sethi   %hi(addr),%o3;		\
	    ld	    [%o3+%lo(addr)],reg
#define STORE(reg, addr)			\
	    sethi   %hi(addr),%o3;		\
	    st	    reg,[%o3+%lo(addr)]


/* The ML stack frame has the following layout (set up by restoreregs):
 *
 *			+-----------------+
 *	%fp = %sp+80:	| temp for floor  |
 *			+-----------------+
 *	%sp+76:		| addr of _ml_div |
 *			+-----------------+
 *	%sp+72:		| addr of _ml_mul |
 *			+-----------------+
 *	%sp+68:		|    saved %g6    |
 *			+-----------------+
 *	%sp+64:		|    saved %g7    |
 *			+-----------------+
 *			|  space to save  |
 *			|  in and local   |
 *	%sp:		|    registers    |
 *			+-----------------+
 *
 * the size of the frame is
 */
#define ML_FRAMESIZE (WINDOWSIZE+20)

#define CLOSURE(name)				\
	    .global name;			\
	    .align  4;				\
	    .word   MAKE_DESC(1,tag_record);	\
    name:   .word   7f;				\
	    .word   7;	/* reg. mask */		\
	    .word   tag_backptr;		\
    7:

#define CONTINUE				\
	    ld	    [%i1],%i2;			\
	    jmp	    %i2;			\
	    nop

	.seg	"text"

	.global _saveregs, _restoreregs, _sigh_resume

/* sigh_return_c:
 * The return continuation for the ML signal handler.
 */
CLOSURE(_sigh_return_c)
	ba	set_request
	set	REQ_SIG_RETURN,%o4	/* delay slot */

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
_sigh_resume:
	ba	set_request
	set	REQ_SIG_RESUME,%o4	/* delay slot */

CLOSURE(_handle_c)
	ba	set_request
	set	REQ_EXN,%o4		/* delay slot */

CLOSURE(_return_c)
	ba	set_request
	set	REQ_RETURN,%o4		/* delay slot */

CLOSURE(_callc_v)
	taddcctv %g6,%g4,%g0
	set	REQ_CALLC,%o4
	/* fall through */

set_request:			      /* a quick return to run_ml() */
	STORE	(%o4, _request)
	STORE	(%g0, _inML)		/* note that we have left ML code */
	FETCH	(_MLState,%o3)
	dec	4,%g6			/* adjust store pointer */
	st	%g6,[%o3]		/* save allocptr */
	st	%g5,[%o3+8]		/* save storeptr */
	std	%i0,[%o3+16]		/* save %i0, %i1 */
	st	%i5,[%o3+36]		/* save globalptr */
	st	%g7,[%o3+40]		/* save exncont */
	ldd	[%sp+64],%g6		/* restore C registers %g6 & %g7. */
	ret
	restore				/* restore C register window (delay slot) */

_saveregs:
	STORE	(%g0, _inML)		/* note that we have left ML code */
	add	%i3,-4096,%i3		/* adjust the base code ptr (sub 4096) */
	FETCH	(_MLState, %o3)
	dec	4,%g6			/* adjust store pointer */
	st	%g6,[%o3]		/* save allocptr */
	st	%g5,[%o3+8]		/* save storeptr */
	std	%i0,[%o3+16]		/* save %i0, %i1 */
	std	%i2,[%o3+24]		/* save %i2, %i3 */
	std	%i4,[%o3+32]		/* save %i4 and globalptr */
	st	%g7,[%o3+40]		/* save exncont */
	st	%g1,[%o3+44]
	std	%g2,[%o3+48]
	std	%l0,[%o3+56]
	std	%l2,[%o3+64]
	std	%l4,[%o3+72]
	std	%l6,[%o3+80]
	std	%o0,[%o3+88]
	ldd	[%sp+64],%g6		/* restore C registers %g6 & %g7. */
	ret
	restore				/* restore C register window (delay slot) */

_restoreregs:
	save	%sp,-SA(ML_FRAMESIZE),%sp
	std	%g6,[%sp+64]		/* save C registers %g6 & %g7 */
	set	_ml_mul,%o5		/* set pointer to ml_mul */
	st	%o5,[%sp+72]
	set	_ml_div,%o5		/* set pointer to ml_div */
	st	%o5,[%sp+76]
	FETCH	(_MLState, %o3)
	ld	[%o3],%g6		/* restore allocptr */
	ld	[%o3+4],%g4		/* restore limitptr */
	ld	[%o3+8],%g5		/* restore storeptr */
	ld	[%o3+12],%o4		/* %o4 = the ML code address */
	ldd	[%o3+16],%i0
	ldd	[%o3+24],%i2
	ldd	[%o3+32],%i4
	ld	[%o3+40],%g7		/* restore exnptr */
	ld	[%o3+44],%g1
	ldd	[%o3+48],%g2
	ldd	[%o3+56],%l0
	ldd	[%o3+64],%l2
	ldd	[%o3+72],%l4
	ldd	[%o3+80],%l6
	ldd	[%o3+88],%o0
	inc	4,%g6			/* adjust store pointer */
	set	0x7ffffffc,%o5		/* adjust limit ptr */
	sub	%o5,%g4,%g4
	sub	%i3,-4096,%i3		/* adjust the base code ptr (add 4096) */
	set	1,%o5			/* note that we have entered ML code */
	STORE	(%o5, _inML)
	FETCH	(_NumPendingSigs, %o5)	/* check for pending signals */
	tst	%o5
	bne	2f
	nop
1:
	jmp	%o4			/* invoke the ML code */
	nop
2:				      /* there are pending signals */
	FETCH	(_maskSignals, %o5)	/* check if signals are masked */
	tst	%o5
	bne	1b
	FETCH	(_inSigHandler, %o5)
	tst	%o5			/* check if we are currently handling a signal */
	bne	1b
	set	1,%o5			/* (delay slot) */
	STORE	(%o5, _handlerPending)	/* note that a handler trap is pending */
	jmp	%o4			/* generate a trap on the next limit check */
	set	1,%g4			/* (delay slot) */


/* adjust_limit:
 * Adjust the heap limit pointer so that a trap will be generated on the next limit
 * check and then continue executing ML code.
 * NOTE: this code cannot trash any registers (other than %g4) or the condition code.
 * To achieve this we work inside a new register window.
 */
	.global	_adjust_limit
_adjust_limit:
	save	%sp,-SA(WINDOWSIZE),%sp
	FETCH	(_saved_pc, %o0)
	set	1,%g4
	jmp	%o0
	restore				/* (delay slot) */


/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
CLOSURE(_array_v)
1:
	ld	[%i0],%o0		/* get length into %o0 */
	ld	[%i0+4],%l0		/* get default into %l0 */
	sra	%o0,1,%o0		/* convert to sparc int */
	sll	%o0,width_tags,%o1	/* build the tag in %o1 */
	or	%o1,tag_array,%o1
	sll	%o0,2,%o0		/* scale length to bytes */
	add	%g4,%o0,%o4		/* check the heap limit */
	addcc	%g6,%o4,%g0
	bvs	3f			    /* we hit the limit, so trap safely */
	dec	4,%o0			/* length-- */
	st	%o1,[%g6-4]		/* store the tag */
	mov	%g6,%i0			/* result := object addr. */
2:					/* initialization loop */
	st	%l0,[%g6]		    /* store default. */
	deccc	4,%o0			    /* length-- */
	bge	2b
	inc	4,%g6			    /* freeptr++ (delay slot) */
	/* end loop */
	inc	4,%g6			/* freeptr++ */
	CONTINUE
	.word	7	/* reg. mask */
	.word	0
3:					/* we come here to do a safe GC trap. */
	add	%g0,0,%g0		    /* a nop to get PC adjust right */
	taddcctv %g6,%o4,%g0		    /* cause the GC trap. */
	ba	1b			    /* retry the allocation. */
	nop


/* create_b : int -> bytearray
 * create_s : int -> string
 * Create bytearray or string of given length.	This can cause GC.
 */
CLOSURE(_create_b_v)
	ba	2f
	mov	tag_bytearray,%l1   /* (delay slot) */

CLOSURE(_create_s_v)
	mov	tag_string,%l1
2:
	sra	%i0,1,%o0	    /* %o0 = length (sparc int) */
	sll	%o0,width_tags,%o2
	or	%o2,%l1,%o1	    /* build the tag in %o1 */
	add	%o0,3,%o0	    /* %o0 = length in words (no tag) */
	sra	%o0,2,%o0
	sll	%o0,2,%o0	    /* %o0 = length in bytes (no tag) */
	add	%g4,%o0,%o4	    /* Check the heap limit. */
	addcc	%g6,%o4,%g0
	bvs	3f			/* we hit the limit, so trap safely. */
	st	%o1,[%g6-4]	    /* store the tag */
	mov	%g6,%i0		    /* result := object addr */
	add	%o0,4,%o0	    /* %o0 = length in bytes (including tag) */
	add	%o0,%g6,%g6	    /* freeptr += length */
	CONTINUE
	.word	7	/* reg. mask */
	.word	0
3:				    /* we come here to do a safe GC trap. */
	add	%g0,0,%g0		/* a nop to get PC adjust right */
	taddcctv %g6,%o4,%g0		/* cause the GC trap. */
	ba	2b			/* retry the allocation. */
	nop


/* floor : real -> int
 * Return the floor of the argument or else raise Float("floor") if out of range.
 * We implement the range check by using an integer comparison with the high 32
 * bits of the real value (which contains the biased exponent).
 * (double)(2^30)   == [0x41d00000, 0x0]
 * (double)(-2^30)  == [0xc1d00000, 0x0]
 */
CLOSURE(_floor_v)
	ld	[%i0],%f0	    /* fetch arg into %f0, %f1. */
	ld	[%i0+4],%f1
	ld	[%i0],%o0	    /* %o0 gets high word. */
	tst	%o0		    /* negative ? */
	blt	1f
	nop
				/* handle positive case */
	set	0x41d00000,%o1	    /* %o1 = 2^30 */
	cmp	%o0,%o1		    /* if %o0 >= 2^30 then range error */
	bge	out_of_range
	nop
	fdtoi	%f0,%f2		    /* cvt to int (round towards 0) */
	st	%f2,[%sp+80]
	ld	[%sp+80],%o0	    /* %o0 gets int result (via stack temp). */
	ba	2f
	nop
1:				/* handle negative case. */
	set	0xc1d00000,%o1	    /* %o1 = -2^30 */
	cmp	%o0,%o1		    /* if %o0 < -2^30 then range error */
	bge	out_of_range	    /* not bl because of sign. */
	nop
	fdtoi	%f0,%f2		    /* cvt to int (round towards 0) */
	st	%f2,[%sp+80]
	fitod	%f2,%f4		    /* cvt back to real to check for fraction */
	fcmpd	%f0,%f4		    /* same value? */
	ld	[%sp+80],%o0	    /* %o0 gets int result (via stack temp). */
	fbe	2f		    /* check result of fcmpd */
	nop
	dec	%o0		    /* push one lower */
2:				/* cvt result to ML int, and continue */
	add	%o0,%o0,%o0
	add	%o0,1,%i0
	CONTINUE

out_of_range:			/* out of range */
	t	ST_INT_OVERFLOW		/* generate an Overflow exn.  We do this */
					/* via a trap to produce a SIGOVFL */


/* logb : real -> int
 * Extract and unbias the exponent, return 0 for a zero exponent.
 * The IEEE bias is 1023.
 */
CLOSURE(_logb_v)
	ld	[%i0],%o0		/* extract exponent. */
	srl	%o0,20,%o0
	andcc	%o0,0x7ff,%o0		/* if (exp == 0) */
	beq	1f
	nop
	sll	%o0,1,%o0		/* else unbias and cvt to ML int. */
	sub	%o0,2045,%i0		/* 2(n-1023)+1 == 2n-2045. */
1:	CONTINUE
2:	ba	1b
	set	1,%i0			/* return ML zero (delay slot) */


/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.	 Raise
 * Float("underflow") or Float("overflow") as appropriate.
 */
CLOSURE(_scalb_v)
	taddcctv %g6,%g4,%g0        /* check the heap limit. */
	ld	[%i0+4],%o0	    /* %o0 gets scale (second arg) */
	sra	%o0,1,%o0	    /* cvt scale to sparc int */
	ld	[%i0],%i0	    /* %i0 gets real (first arg) */
	ld	[%i0],%o1	    /* %o1 gets high word of real value. */
	set	0x7ff00000,%o3	    /* %o3 gets exponent mask. */
	andcc	%o1,%o3,%o4	    /* extract exponent into %o4. */
	beq	1f		    /* if 0 then return same */
	nop
	srl	%o4,20,%o4	    /* cvt exp to int (delay slot). */
	addcc	%o4,%o0,%o0	    /* %o0 = exp + scale */
	ble	under		    /* if new exp <= 0 then underflow */
	nop
	cmp	%o0,2047	    /* if new exp >= 2047 then overflow */
	bge	over
	nop
	andn	%o1,%o3,%o1	    /* mask out old exponent. */
	sll	%o0,20,%o0	    /* shift new exp to exponent position. */
	or	%o1,%o0,%o1	    /* set new exponent. */
	ld	[%i0+4],%o0	    /* %o0 gets low word of real value. */
	st	%o1,[%g6]	    /* allocate the new real value */
	st	%o0,[%g6+4]
7:	set	MAKE_DESC(8,tag_string),%o0
	st	%o0,[%g6-4]
	mov	%g6,%i0		    /* set result. */
	inc	12,%g6		    /* storeptr += 3 */
1:	CONTINUE

over:				/* handle overflow */
	t	ST_INT_OVERFLOW	    /* generate an Overflow exn.  We do this */
	/* never get here */	    /* via a trap to produce a SIGOVFL */

under:				/* handle underflow */
	st	%g0,[%g6]
	st	%g0,[%g6+4]
	ba	7b
	nop

/** Integer multiplication and division routines **
 *
 * The arguments are %o2, %o3 and the result is in %o2.
 * Note: this code assumes that .mul and .div don't use %g4 (the limitptr).
 */
	.global .mul, .div

/* ml_mul:
 * multiply %o2 by %o3, returning the result in %o2
 * Note: this code assumes that .mul doesn't trash any global or input
 * registers.
 */
_ml_mul:
	save	%sp,-SA(WINDOWSIZE),%sp
	mov	%i2,%o0
	call	.mul
	mov	%i3,%o1			/* (delay slot) */
	bnz	1f			/* if z is clear, then overflow */
	restore %o0,0,%o2		/* result in %o2 (delay slot) */
	retl
	nop
1:					/* handle overflow. */
	t	ST_INT_OVERFLOW		/* generate an Overflow exn.  We do this */
					/* via a trap to produce a SIGOVFL */

/* ml_div:
 * divide %o2 by %o3, returning the result in %o2.
 * Note: .div uses %g1, %g2 and %g3, so we must save them.  We do this using the
 * locals of the new window, since .div is a leaf routine.
 */
_ml_div:
	save	%sp,-SA(WINDOWSIZE),%sp
	addcc	%i3,%g0,%o1		/* %o1 is divisor (and check for zero) */
	bz	1f
				    /* save %g1, %g2 and %g3 (using new window) */
	mov	%g1,%l1			/* (delay slot) */
	mov	%g2,%l2
	mov	%g3,%l3
	call	.div
	mov	%i2,%o0			/* (delay slot) */
				    /* restore %g1, %g2 and %g3 */
	mov	%l3,%g3
	mov	%l2,%g2
	mov	%l1,%g1
	ret
	restore %o0,0,%o2		/* result in %o2 (delay slot) */
1:				    /* handle zero divide */
	restore				/* restore ML window */
	t	ST_DIV0			/* generate a Div exn.  We do this via a */
					/* trap to produce a SIGDIV */


/* set_fsr:
 * Load the floating-point status register with the given word.
 */
	.global	_set_fsr
_set_fsr:
	set	fsrtmp,%o1
	st	%o0,[%o1]
	retl
	ld	[%o1],%fsr		/* (delay slot) */
	.seg	"data"
fsrtmp:	.word	0


/* this bogosity is for export.c */
	.global _startptr
_startptr:
	.long	 start
