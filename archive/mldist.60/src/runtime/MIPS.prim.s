/* MIPS.prim.s
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * The MIPS registers are used as follows; the names in parentheses are the
 * MLState field names (see ml_state.h):
 *
 * $23 = freespace pointer (ml_allocptr)
 * $19 = data limit (ml_limitptr)
 * $22 = store ptr (ml_storeptr)
 *
 * $2  = standard arg (ml_arg)
 * $3  = standard continuation (ml_cont)
 * $4  = standard closure (ml_closure)
 * $30 = exception handler continuation (ml_exncont)
 *
 * $5-$18 = general purpose pointers (ml_roots[5-18])
 * $21 = pointer temp.
 * $20,$24,$25 = arithtemps.
 *
 * $1 is reserved by the assembler ($at)
 * $26,$27 are reserved by the OS kernel
 * $28 = global pointer ($gp)
 * $29 = stack pointer (not used by ML)
 * $31 = return address (not used by ML)
 *
 * Note that the garbage collector only preserves root registers ($2-$18, $30).
 *
 * The ML state structure has the following layout for the MIPS (see "ml_state.h"):
 *
 *		+-------------------+
 *  MLState --> | ml_allocptr ($23) |
 *		+-------------------+
 *	+4:	| ml_limitptr ($19) |
 *		+-------------------+
 *	+8:	| ml_storeptr ($22) |
 *		+-------------------+
 *	+12:	|    ml_arg ($2)    |
 *		+-------------------+
 *	+16:	|   ml_cont ($3)    |
 *		+-------------------+
 *	+20:	|  ml_closure ($4)  |
 *		+-------------------+
 *	+24:	| ml_exncont ($30)  |
 *		+-------------------+
 *	+28:	|       ml_pc       |
 *		+-------------------+
 *	+32:	|     ($5-$18)      |
 *		+-------------------+
 */

#include "tags.h"
#include "request.h"

/* Symbolic definitions of some of the ML registers */
#define storeptr 22
#define allocptr 23
#define limit 19
#define stdarg 2
#define stdcont 3
#define stdclos 4
#define exncont 30
#define atmp1 24
#define atmp2 25
#define atmp3 20
#define ptrtmp 21

#define CLOSURE(name)						\
	    .globl	name;					\
	    .align	2; /* actually 4-byte alignment */	\
	    .set noreorder;					\
	    .word	MAKE_DESC(1, tag_record);		\
    name:   .word	9f; /* address of routine */		\
	    .word	1; /* here for historical reasons */	\
	    .word	tag_backptr;				\
	    .set reorder;					\
    9:

#define CONTINUE						\
	    lw		$10,0($stdcont);			\
	    j		$10

	.text

	.globl	request
	.globl	MLState
	.globl	saveregs
	.globl	handle_c
	.globl	return_c
	.globl	restoreregs

#define regspace	40
#define localspace	4
#define argbuild	16
#define framesize	(regspace+localspace+argbuild) /* must be multiple of 8 */
#define frameoffset	(0-localspace)


	.ent	saveregs

/* sig_return : ('a cont * 'a) -> 'b
 */
CLOSURE(sigh_return_c)
	li	$atmp1,REQ_SIG_RETURN
	b	set_request

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
	.globl	sigh_resume
sigh_resume:
	li	$atmp1,REQ_SIG_RESUME
	b	set_request

CLOSURE(handle_c) /* exception handler for ML functions called from C */
	li	$atmp1,REQ_EXN
	b	set_request

CLOSURE(return_c) /* continuation for ML functions called from C */
	li	$atmp1,REQ_RETURN
	b	set_request

CLOSURE(callc_v)
	add	$0,$allocptr,$limit
	li	$atmp1,REQ_CALLC
	/* fall through */

set_request:
	sw	$atmp1,request
	sw	$0,inML			/* note that we have left ML */
	lw	$ptrtmp,MLState		/* save the minimal ML state */
	sw	$allocptr,0($ptrtmp)
	sw	$storeptr,8($ptrtmp)
	sw	$stdarg,12($ptrtmp)
	sw	$stdcont,16($ptrtmp)
	sw	$exncont,24($ptrtmp)
restore_c_regs:
	lw	$31,argbuild+36($sp)	/* restore the C registers */
	lw	$30,argbuild+32($sp)
        lw      $23,argbuild+28($sp)
        lw      $22,argbuild+24($sp)
        lw      $21,argbuild+20($sp)
        lw      $20,argbuild+16($sp)
        lw      $19,argbuild+12($sp)
        lw      $18,argbuild+8($sp)
        lw      $17,argbuild+4($sp)
        lw      $16,argbuild($sp)
	addu	$sp,framesize		/* discard the stack frame */
	j	$31			/* return to run_ml() */

saveregs:
	sw	$0,inML			/* note that we have left ML */
	lw	$ptrtmp,MLState		/* save the ML state */
	sw	$allocptr,0($ptrtmp)
	sw	$storeptr,8($ptrtmp)
	sw	$stdarg,12($ptrtmp)
	sw	$stdcont,16($ptrtmp)
	sw	$stdclos,20($ptrtmp)
	sw	$exncont,24($ptrtmp)
	sw	$5,32($ptrtmp)		  /* save misc. roots */
	sw	$6,36($ptrtmp)
	sw	$7,40($ptrtmp)
	sw	$8,44($ptrtmp)
	sw	$9,48($ptrtmp)
	sw	$10,52($ptrtmp)
	sw	$11,56($ptrtmp)
	sw	$12,60($ptrtmp)
	sw	$13,64($ptrtmp)
	sw	$14,68($ptrtmp)
	sw	$15,72($ptrtmp)
	sw	$16,76($ptrtmp)
	sw	$17,80($ptrtmp)
	sw	$18,84($ptrtmp)
	b	restore_c_regs

	.end	saveregs

	.ent	restoreregs
restoreregs:
	subu	$sp,framesize		/* allocate a stack frame */
					/* save the C registers */
.mask 0xd0ff0000,0-localspace
	sw	$31,argbuild+36($sp)
	sw	$30,argbuild+32($sp)
        sw      $23,argbuild+28($sp)
        sw      $22,argbuild+24($sp)
        sw      $21,argbuild+20($sp)
        sw      $20,argbuild+16($sp)
        sw      $19,argbuild+12($sp)
        sw      $18,argbuild+8($sp)
        sw      $17,argbuild+4($sp)
        sw      $16,argbuild($sp)
					/* load the ML state */
	lw	$ptrtmp,MLState
	lw	$allocptr,0($ptrtmp)
	lw	$limit,4($ptrtmp)
	li	$atmp1,0x7fffffff	/* adjust the limit register */
	sub	$limit,$atmp1,$limit
	lw	$storeptr,8($ptrtmp)
	lw	$stdarg,12($ptrtmp)
	lw	$stdcont,16($ptrtmp)
	lw	$stdclos,20($ptrtmp)
	lw	$exncont,24($ptrtmp)
	lw	$atmp1,28($ptrtmp)	/* fetch the saved pc into atmp1 */
	lw	$5,32($ptrtmp)
	lw	$6,36($ptrtmp)
	lw	$7,40($ptrtmp)
	lw	$8,44($ptrtmp)
	lw	$9,48($ptrtmp)
	lw	$10,52($ptrtmp)
	lw	$11,56($ptrtmp)
	lw	$12,60($ptrtmp)
	lw	$13,64($ptrtmp)
	lw	$14,68($ptrtmp)
	lw	$15,72($ptrtmp)
	lw	$16,76($ptrtmp)
	lw	$17,80($ptrtmp)
	lw	$18,84($ptrtmp)
	li	$atmp2,1
.set	noreorder			/* the order here is important */
	sw	$atmp2,inML		/* note that we are entering ML code */
	lw	$atmp3,NumPendingSigs	/* check for pending signals */
	nop				/* (load delay slot) */
	bnez	$atmp3,1f
	nop				/* (branch delay slot) */
	j	$atmp1			/* jump to ML code */
	nop
1:				      /* there are pending signals, */
	lw	$atmp3,maskSignals	/* are signal masked? */
	nop				/* (load delay slot) */
	bnez	$atmp3,2f
	nop				/* (branch delay slot) */
	lw	$atmp3,inSigHandler	/* check for a pending handler */
	nop				/* (load delay slot) */
	bnez	$atmp3,2f
	nop				/* (branch delay slot) */
	sw	$atmp2,handlerPending	/* note the pending handler */
	li	$limit,0x7fffffff	/* trap on the next limit check. */
2:
	j	$atmp1			/* jump to ML code */
	nop				/* (branch delay slot) */
.set	reorder
	.end	restoreregs


/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
CLOSURE(array_v)
1:					/* jump back here after GC trap */
	lw	$atmp1,0($stdarg)	/* tagged length in $atmp1 */
	lw	$10,4($stdarg)		/* get initial value in $10 */
	sra	$atmp1,1		/* untagged length */
	sll	$atmp2,$atmp1,width_tags /* build descriptor in $atmp2 */
	ori	$atmp2,tag_array
	sll	$atmp1,2		/* get length in bytes into $atmp1 */
.set noreorder
	li	$atmp3,0x7fffffff
	sub	$atmp3,$atmp3,$limit	/* get "true" limit value */
	sub	$atmp3,$atmp3,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	blez	$atmp3,3f		/* do we have enough? */
	nop
.set reorder
	sw	$atmp2,0($allocptr)	/* store the descriptor */
	add	$allocptr,4		/* points to new object */
	add	$atmp3,$atmp1,$allocptr	/* beyond last word of new array */
	move	$stdarg,$allocptr	/* put ptr in return register */
					/* (return val = arg of continuation) */
2:					/* loop */
	sw	$10,0($allocptr)	  /* store the value */
        addi	$allocptr,4		  /* on to the next word */
	bne	$allocptr,$atmp3,2b	  /* if not off the end, repeat */
					/* end loop */
        CONTINUE

3:					/* we need to do a GC */
.set noreorder
	li	$limit,0x7fffffff	/* MAXINT into limit will cause GC trap */
	add	$0,$allocptr,$limit	/* trap */
	b	1b
	nop
.set reorder


/* create_b : int -> bytearray
 * create_s : int -> string
 * Create bytearray or string of given length.	This can cause GC.
 */
CLOSURE(create_b_v)
	li	$11,tag_bytearray	/* $11 gets tag */
	b	1f

CLOSURE(create_s_v)
	li	$11,tag_string		/* $11 gets tag */
1:					/* jump back here after a GC trap */
	addi	$atmp1,$stdarg,13	/* atmp1 = 2*n+14 */
	sra	$atmp1,3		/* length in words (including descriptor) */
	sll	$atmp1,2 		/* length in bytes (including descriptor) */
.set noreorder
	li	$atmp3,0x7fffffff
	sub	$atmp3,$atmp3,$limit	/* get "true" limit value */
	sub	$atmp3,$atmp3,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	blez	$atmp3,2f		/* do we have enough? */
	nop
.set reorder
	sra	$atmp2,$stdarg,1	/* build descriptor in atmp2 */
	sll	$atmp2,width_tags
	or	$atmp2,$11
	sw	$atmp2,0($allocptr)	/* store descriptor */
	addi	$stdarg,$allocptr,4	/* pointer to new string */
	add	$allocptr,$atmp1	/* advance allocptr */
	CONTINUE

2:					/* we need to do a GC */
.set noreorder
	li	$limit,0x7fffffff	/* MAXINT into limit will cause GC trap */
	add	$0,$allocptr,$limit	/* trap */
	b	1b
	nop
.set reorder


#ifdef MIPSEL
#define BIGPART 4
#else
#define BIGPART 0
#endif
#define LITTLEPART (4-BIGPART)

/* Floating exceptions raised (assuming ROP's are never passed to functions):
 *	DIVIDE BY ZERO - (div)
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 *
 * floor raises integer overflow if the float is out of 32-bit range,
 * so the float is tested before conversion, to make sure it is in (31-bit)
 * range */

maxint:	.double	1073741824.0

CLOSURE(floor_v)
	lwc1	$f4,LITTLEPART($stdarg)	/* get least significant word */
	lwc1	$f5,BIGPART($stdarg)	/* get most significant word */
	lwc1	$f2,maxint+LITTLEPART
	lwc1	$f3,maxint+BIGPART
	abs.d	$f6,$f4
	c.le.d	$f6,$f2
	cfc1	$20,$31			/* grab fpa control register */
	bc1f	over
	ori	$24,$20,0x03		/* set rounding bits to 11 */
	ctc1	$24,$31			/* return fpa control register */
	cvt.w.d $f6,$f4			/* convert to integer */
	ctc1	$20,$31			/* return fpa control register */
	mfc1	$stdarg,$f6		/* get in std argument register */
	add	$stdarg,$stdarg		/* make room for tag bit */
	add	$stdarg,1		/* add the tag bit */
	CONTINUE

CLOSURE(logb_v)
	lw 	$stdarg,BIGPART($stdarg)/* most significant part */
	srl 	$stdarg,20		/* throw out 20 low bits */
	andi	$stdarg,0x07ff		/* clear all but 11 low bits */
	sub 	$stdarg,1023		/* subtract 1023 */
	sll 	$stdarg,1		/* make room for tag bit */
	add	$stdarg,1		/* add the tag bit */
	CONTINUE

CLOSURE(scalb_v)
.set noreorder
	add	$0,$allocptr,$limit	/* causes GC */
.set reorder
	lw 	$atmp1,4($stdarg)	/* get tagged n */
	sra	$atmp1,1		/* get real n */
	beqz	$atmp1,9f		/* if zero, return the old float */
	lw	$ptrtmp,0($stdarg)	/* get pointer to float */
	lw 	$atmp2,BIGPART($ptrtmp)	/* most significant part */
	srl 	$atmp2,20		/* throw out 20 low bits */
	andi	$atmp2,0x07ff		/* clear all but 11 low bits */
	add	$atmp3,$atmp2,$atmp1	/* new := old + n */
	blt	$atmp3,1,under		/* punt if underflow */
	bgt	$atmp3,2046,over	/* or overflow */
	xor	$atmp3,$atmp2		/* at3 = new xor old */
	sll	$atmp3,20		/* put exponent in right position */
	lw	$atmp2,BIGPART($ptrtmp)	/* most significant word */
	xor	$atmp2,$atmp3		/* change to new exponent */
	sw	$atmp2,BIGPART+4($allocptr)	/* save */
	lw 	$atmp2,LITTLEPART($ptrtmp) /* get least significant word */
	sw	$atmp2,LITTLEPART+4($allocptr)	/* save lsw */
8:	li	$10,MAKE_DESC(8,tag_string) /* make descriptor */
	sw	$10,0($allocptr)	/* save descriptor */
	add	$stdarg,$allocptr,4	/* get pointer to new float */
	add	$allocptr,12		/* point to new free word */
	lw 	$10,0($stdcont)		/* grab continuation */
	j 	$10			/* return */

9:	lw	$stdarg,0($stdarg)	/* get old float */
	CONTINUE

over:	li	$20,0x7fffffff
	add	$20,$20		/* generate overflow exception */

under:	sw	$0,4($allocptr)		/* return 0.0 */
	sw	$0,8($allocptr)
	b	8b

/* set_fsr:
 * Turn on floating-point overflow, underflow and zero-divide exceptions.
 */
	.globl	set_fsr
	.ent	set_fsr
set_fsr:
	cfc1	$atmp1,$31		/* grab fpa control register */
	ori 	$atmp1,$atmp1,0x600	/* set O and Z bits */
	ctc1	$atmp1,$31		/* return fpa control register */
	j	$31
	.end	set_fsr

/* this bogosity is for export.c */
	.globl	startptr
startptr: .word    __start


