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
 * r14 = stack pointer (used by ML code only to point at float regs 
 *	               and ml_varptr) 
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
 *	+56:	|    ml_varptr      |
 *		+-------------------+
 *      +60:    |       inML        |
 *		+-------------------+
 *      +64:    |     request       |
 *		+-------------------+
 *      +68:    |   handlerPending  |
 *		+-------------------+
 *      +72:    |    inSigHandler   |
 *		+-------------------+
 *      +76:    |     maskSignals   |
 *		+-------------------+
 *      +80:    |   NumPendingSigs  |
 *		+-------------------+
 *      +84:    |     ioWaitFlag    |
 *		+-------------------+
 *      +88:    |     GCpending     |
 *		+-------------------+
 *      +92     |     saved_pc      |
 *              +-------------------+
 *              |       ....        |
 */

#include "mask.h"
#include "tags.h"
#include "request.h"

#define allocptr 	r12
#define limit 		r8
#define exncont		r13
#define stdarg		r0
#define stdcont		r1
#define stdclos		r2

#define inML 60
#define request 64
#define handlerPending 68
#define inSigHandler 72
#define maskSignals 76
#define NumPendingSigs 80
#define ioWaitFlag 84
#define GCpending 88
#define saved_pc 92

/* There is an ML stack frame used for floating point "registers" 
 * and the varptr  (see vax/vax.sml).
 * Space is allocated for this by restoreregs.  The layout is as follows:
 *
 *                      +-----------------+
 *      sp + 132        |   MLState ptr   |
 *			+-----------------+
 *	sp + 128:	|    varptr       +
 *			+-----------------+
 *			|   fp register   |
 *			+- - - - - - - - -+
 *	sp + 120:	|      15         |
 *			+-----------------+
 *			.	.         .
 *			.	.         .
 *			.	.         .
 *			+-----------------+
 *			|   fp register   |
 *			+- - - - - - - - -+
 *	sp:		|      0          |
 *			+-----------------+
 *
 */
#define N_FLOAT_REGS 16			/* must agree with vax/vax.sml */
#define ML_FRAME_SIZE  (N_FLOAT_REGS*8+4+4)
#define VARPTR_OFFSET  (N_FLOAT_REGS*8)
#define MLSTATE_OFFSET (N_FLOAT_REGS*8+4)

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
	movl	MLSTATE_OFFSET(sp),r9	
	movl	$REQ_SIG_RETURN,request(r9)
	jbr	_quicksave

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
_sigh_resume:
	movl	MLSTATE_OFFSET(sp),r9	
	movl	$REQ_SIG_RESUME,request(r9)
	jbr	_quicksave

CLOSURE(_handle_c)
	movl	MLSTATE_OFFSET(sp),r9
	movl	$REQ_EXN,request(r9)
	jbr	_quicksave

CLOSURE(_return_c)
	movl	MLSTATE_OFFSET(sp),r9
	movl	$REQ_RETURN,request(r9)
	jbr	_quicksave

CLOSURE(_callc_v)
	addl3	r12,r8,r9		/* heap limit check */
	movl	MLSTATE_OFFSET(sp),r9
	movl	$REQ_CALLC,request(r9)
	/* fall into quicksave */

_quicksave:
	clrl	inML(r9)		/* note that we have left ML code */
	subl3	$4,r12,(r9)
	movl	r11,8(r9)		/* save the storeptr */
	movl	r13,12(r9)		/* save the exncont */
	movq	r0,16(r9)
	movl	r2,24(r9)		/* save closure */
	movl	VARPTR_OFFSET(sp),56(r9)  /* save the varptr */
	addl2	$ML_FRAME_SIZE,sp	/* fix up sp */	
	movl	(sp)+,fp		/* restore the fp register */
	ret

_saveregs:
	movl	MLSTATE_OFFSET(sp),r9
	clrl	inML(r9)		/* note that we have left ML code */
	subl3	$4,r12,(r9)
	movl	r11,8(r9)		/* save the storeptr */
	movl	r13,12(r9)		/* save the exncont */
	movq	r0,16(r9)
	movq	r2,24(r9)
	movq	r4,32(r9)
	movq	r6,40(r9)
	movl	r10,48(r9)
	movl	VARPTR_OFFSET(sp),56(r9)	/* save the varptr */
	addl2	$ML_FRAME_SIZE,sp		/* fix up sp */
	movl	(sp)+,fp		/* restore the fp register */
	ret

_restoreregs:
	.word	0x4ffc			/* save r2-r11; enable overflow trap */
	movl	4(ap),r9	/* the MLState pointer is passed as an arg */
	pushl	fp			/* save the fp register. */
	addl3	(r9),$4,r12
	subl3	4(r9),$0x7fffffff,r8
	incl	r8
	movl	8(r9),r11
	movl	12(r9),r13
	movq	16(r9),r0
	movq	24(r9),r2
	movq	32(r9),r4
	movq	40(r9),r6
	movl	48(r9),r10
	subl2	$ML_FRAME_SIZE,sp 	/* get stack space for fpregs,varptr */
	movl	r9,MLSTATE_OFFSET(sp)	/* save ptr to MLState on stack */
	movl	56(r9),VARPTR_OFFSET(sp)		/* restore varptr */
	pushl	52(r9)
	movl	$1,inML(r9)
	tstl	GCpending(r9)
	jneq	3f
	tstl	NumPendingSigs(r9)
	jneq	2f
1:
	rsb
2:
	tstl	maskSignals(r9)		/* are signals masked? */
	jneq	1b
	tstl	inSigHandler(r9)	/* check if we are currently handling a signal */
	jneq	1b
	movl	$1,handlerPending(r9)	/* note that a handler trap is pending */
3:	movl	$0x7fffffff,r8		/* force a trap on the next limit check */
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
 * NOTE: this code cannot trash any registers (other than r8) or the condition
 * code.  We don't have a good solution for what to do when MAX_PROCS > 1 
 * since the _saved_pc has become part of the MLState vector.  So for now,
 * we leave it undefined...
 */
	.globl	_adjust_limit
_adjust_limit:
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movpsl	-(sp)
	pushl	_saved_pc
	movl	$0x7fffffff,r8	/* force a trap on the next limit check */
	rei			/* return to ML code, restoring the condition code */
#endif

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
	.long	closmask|0x10 	/* register mask  (std. regs + r4) */
	.long	0			/* fake back pointer */
5:      addl3	r12,r8,r9		/* guaranteed to fault */
        jbr	2b

/* create_v_v : int * 'a list -> 'a vector
 * 	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 * temps: r2,r9 and r10
 */
CLOSURE(_create_v_v)
#define ML_NIL 			$1
#define ML_LIST_HD(listp)	(listp)
#define ML_LIST_TL(listp) 	4(listp)
1:
	ashl	$-1,(stdarg),r9			/* r9 := length */
	ashl	$2,r9,r10			/* r10 := length in bytes */
	addl2	allocptr,r10			/* check space */
        addl2	$0x80000000,r10
	subl2	$4096,r10			
	addl2	limit,r10			
	jgeq	3f
	ashl	$width_tags,r9,r10		/* build descriptor */
	bisl3	$tag_record,r10,-4(allocptr) 	/* write out descriptor */
	movl	4(stdarg),r3			/* r3 := list */
	movl	allocptr,stdarg			/* return value */
2:
	movl	ML_LIST_HD(r3),(allocptr)+	/* store data */
	movl	ML_LIST_TL(r3),r3		/* cdr list */
	cmpl	r3, ML_NIL			/* continue ? */
	jneq	2b
	addl2	$4,allocptr			/* adjust allocptr */
	jmp	*(stdcont)			/* execute continuation */
3:
        movl	$0x7fffffff,r8
	jbr     5f
	.align	2
	.long	7			/* register mask */
	.long	0			/* fake back pointer */
5:      addl3	r12,r8,r9		/* guaranteed to fault */
        jbr	1b

/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.  For now, we only provide a uni-processor trivial version.
 */
CLOSURE(_try_lock_v)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	(r0),r9			# get old value of lock
	movl	$1,(r0)			# set the lock to ML_false
	movl	r9,r0			# return old value of lock
	jmp	*(r1)
#endif

/* unlock : releases a spin lock 
 */
CLOSURE(_unlock_v)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	$3,(r0)			# store ML_true into lock
	movl	$1,r0			# and return unit
	jmp	*(r1)
#endif

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
