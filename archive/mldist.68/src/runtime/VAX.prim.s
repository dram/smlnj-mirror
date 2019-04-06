/* VAX.prim.s
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * The VAX registers are used as follows; the name in parentheses are the
 * MLState field names (see ml_state.h):
 *
 * r12 = free space pointer (ml_allocptr)
 * r8  = data limit (ml_limitptr)
 * r11 = store pointer (ml_storeptr)
 *
 * r0  = standard arg (ml_arg)
 * r1  = standard contiuation (ml_cont)
 * r2  = standard closure (ml_closure)
 * r13 = exception handler continuation (ml_exncont)
 *
 * r3-r7,r9,r10 = general purpose pointers
 * r15 = program counter (ml_pc)
 * r14 = stack pointer (unused by ML code)
 *
 * Note that the garbage collector only preserves root registers (r0-r7, r13).
 *
 * The ML state vector hase the following layout on the VAX (see "ml_state.h"):
 *
 *		+-------------------+
 *  MLState --> | ml_allocptr (r12) |
 *		+-------------------+
 *	+4:	| ml_limitptr (r8)  |
 *		+-------------------+
 *	+8:	| ml_storeptr (r11) |
 *		+-------------------+
 *	+12:	| ml_exncont (r13)  |
 *		+-------------------+
 *	+16:	|    ml_arg (r0)    |
 *		+-------------------+
 *	+20:	|   ml_cont (r1)    |
 *		+-------------------+
 *	+24:	|  ml_closure (r2)  |
 *		+-------------------+
 *	+28:	|      (r3-r7)      |
 *		+-------------------+
 *	+48:	|     (r10)         |
 *		+-------------------+
 *	+52:	|    ml_pc (r15)    |
 *		+-------------------+
 */

#include "tags.h"
#include "request.h"

#define CLOSURE(name)					\
	    .globl	name;				\
	    .align	2;				\
    name:   .long	MAKE_DESC(1,tag_record);	\
    	    .long	9f;				\
	    .long	7;  /* register mask */		\
	    .long	tag_backptr;			\
    9:

	.text
	.globl	_saveregs
	.globl	_restoreregs
	.globl	_sigh_resume

CLOSURE(_sigh_return_c)
	movl	$REQ_SIG_RETURN,_request
	jbr	_quicksave

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
_sigh_resume:
	movl	$REQ_SIG_RESUME,_request
	jbr	_quicksave

CLOSURE(_handle_c)
	movl	$REQ_EXN,_request
	jbr	_quicksave

CLOSURE(_return_c)
	movl	$REQ_RETURN,_request
	jbr	_quicksave

CLOSURE(_callc_v)
	addl3	r12,r8,r9		/* heap limit check */
	movl	$REQ_CALLC,_request
	/* fall into quicksave */

_quicksave:
	clrl	_inML			/* note that we have left ML code */
	movl	_MLState,r9
	subl3	$4,r12,(r9)
	movl	r11,8(r9)		/* save the storeptr */
	movl	r13,12(r9)		/* save the exncont */
	movq	r0,16(r9)
	movl	(sp)+,fp		/* restore the fp register */
	ret

_saveregs:
	clrl	_inML			/* note that we have left ML code */
	movl	_MLState,r9
	subl3	$4,r12,(r9)
	movl	r11,8(r9)		/* save the storeptr */
	movl	r13,12(r9)		/* save the exncont */
	movq	r0,16(r9)
	movq	r2,24(r9)
	movq	r4,32(r9)
	movq	r6,40(r9)
	movl	r10,48(r9)
	movl	(sp)+,fp		/* restore the fp register */
	ret

_restoreregs:
	.word	0x4ffc			/* save r2-r11; enable overflow trap */
	pushl	fp			/* save the fp register. */
	movl	_MLState,r9
	addl3	(r9),$4,r12
	subl3	4(r9),$0x7fffffff,r8
	incl	r8
	movl	8(r9),r11
	movl	12(r9),r13
	movq	16(r9),r0
	movq	24(r9),r2
	movq	32(r9),r4
	movq	40(r9),r6
	pushl	52(r9)
	movl	48(r9),r10
	movl	$1,_inML
	tstl	_NumPendingSigs
	jneq	2f
1:
	rsb
2:
	tstl	_maskSignals		/* are signals masked? */
	jneq	1b
	tstl	_inSigHandler		/* check if we are currently handling a signal */
	jneq	1b
	movl	$1,_handlerPending	/* note that a handler trap is pending */
	movl	$0x7fffffff,r8		/* force a trap on the next limit check */
	rsb

	.globl _savefpregs
_savefpregs:
	.word 0
	ret

	.globl _restorefpregs
_restorefpregs:
	.word 0
	ret


/* adjust_limit:
 * Adjust the heap limit pointer so that a trap will be generated on the next limit
 * check and then continue executing ML code.
 * NOTE: this code cannot trash any registers (other than r8) or the condition code.
 */
	.globl	_adjust_limit
_adjust_limit:
	movpsl	-(sp)
	pushl	_saved_pc
	movl	$0x7fffffff,r8	/* force a trap on the next limit check */
	rei			/* return to ML code, restoring the condition code */


CLOSURE(_array_v)
4:	ashl	$-1,(r0),r9	/* r9 = length */
	ashl	$2,r9,r10
        addl2	r12,r10
        addl2	$0x80000000,r10
        subl2	$4096,r10
        addl2	r8,r10
        jgeq	3f
	ashl	$width_tags,r9,r10
	bisl3	$tag_array,r10,-4(r12)
	movl	4(r0),r2		/* r2 = initial value */
	movl	r12,r0
	jbr	2f
1:	movl	r2,(r12)+		/* store default */
2:	sobgeq	r9,1b
	addl2	$4,r12
	jmp	*(r1)
3:      movl	$0x7fffffff,r8
	jbr     5f
	.align	2
	.long	7			/* register mask */
	.long	0			/* fake back pointer */
5:      addl3	r12,r8,r9		/* guaranteed to fault */
        jbr	4b

CLOSURE(_create_b_v)
	movl	$tag_bytearray,r4
	jbr	2f
CLOSURE(_create_s_v)
	movl	$tag_string,r4
2:	addl3	$13,r0,r10
	ashl	$-3,r10,r10
	ashl	$2,r10,r10	/* r9 = bytes in string including tag */
        movl	r10,r9
        addl2	r12,r10
        addl2	$0x80000000,r10
        subl2	$4096,r10
        addl2 	r8,r10
        jgeq 	3f
	ashl	$-1,r0,r10	/* r10 = length */
	ashl	$width_tags,r10,r10
	bisl3	r4,r10,-4(r12)	/* new tag */
	movl	r12,r0
	addl2	r9,r12
	jmp	*(r1)
3:      movl	$0x7fffffff,r8
	jbr     5f
	.align	2
	.long	7			/* register mask */
	.long	0			/* fake back pointer */
5:      addl3	r12,r8,r9		/* guaranteed to fault */
        jbr	2b


/* Floating exceptions raised (assuming ROP's are never passed to functions):
 *	DIVIDE BY ZERO - (div)
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 *
 * floor raises integer overflow if the float is out of 32-bit range,
 * so the float is tested before conversion, to make sure it is in (31-bit)
 * range */

CLOSURE(_floor_v)
        .byte 0xfd; cvtfl (r0),r9	# cvtgl
        .byte 0xfd; tstf (r0)		# tstg
	bgeq 1f
	.byte 0xfd; cvtlf r9,r4		# cvtlg, to handle negative
	.byte 0xfd; cmpf (r0),r4	# cmpg
	beql 2f
	decl r9
2:	clrq r4
1:	ashl $1,r9,r9
	bisl3 $1,r9,r0
	jmp *(r1)

CLOSURE(_logb_v)
	bicl3	$0xffff800f,(r0),r9	# grab exponent
	ashl	$-3,r9,r9
	subl2	$2048,r9		# unbias
	bisl3	$1,r9,r0
	jmp	*(r1)

CLOSURE(_scalb_v)
        addl3	r12,r8,r9
	bicl3	$1,4(r0),r9		# grab add value
	beql	1f			# 0?
        ashl	$3,r9,r9		# shift to exponent field
 	movl	(r0),r0			# grab old float
	bicl3	$0xffff800f,(r0),r10	# grab exponent
	addl2	r9,r10			# check out the new exponent
	bleq	under			# too small?
	cmpl	r10,$0x8000
	bgeq	over			# too large?
	movl	4(r0),4(r12)
	addl3	(r0),r9,(r12)
7:	movl 	$MAKE_DESC(8,tag_string),-4(r12)
	movl	r12,r0
	addl2	$12,r12
	jmp	*(r1)
1:      movl    (r0),r0
        jmp     *(r1)
over:	moval	_overflow_e0+4,r0
	movl	r13,r1
	jmp	*(r1)
under:	clrq	(r12)
	jbr	7b

/* this bogosity is for export.c */
	.globl	_startptr
_startptr: .long    start
