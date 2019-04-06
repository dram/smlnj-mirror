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
 * $20 = var ptr (ml_varptr)
 *
 * $2  = standard arg (ml_arg)
 * $3  = standard continuation (ml_cont)
 * $4  = standard closure (ml_closure)
 * $30 = exception handler continuation (ml_exncont)
 *
 * $5-$18 = general purpose pointers (ml_roots[5-18])
 * $21,25 = internal temps
 * $24 = instruction counter (if enabled)
 *
 * $1 is reserved by the assembler ($at)
 * $26,$27 are reserved by the OS kernel
 * $28 = global pointer ($gp)
 * $29 = stack pointer (not used by ML)
 * $31 = return address (not used by ML)
 *
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
 *	+88:	| ml_varptr ($20)   |
 *		+-------------------+
 *	+92:	| icounter ($24)    |
 *		+-------------------+
 *      +96:    |       inML        |
 *		+-------------------+
 *     +100:    |     request       |
 *		+-------------------+
 *     +104:    |   handlerPending  |
 *		+-------------------+
 *     +108:    |    inSigHandler   |
 *		+-------------------+
 *     +112:    |     maskSignals   |
 *		+-------------------+
 *     +116:    |   NumPendingSigs  |
 *		+-------------------+
 *     +120:    |     ioWaitFlag    |
 *		+-------------------+
 *     +124:    |     GCpending     |
 *		+-------------------+
 *              |        ...        |
 */

#include "mask.h"
#include "tags.h"
#include "request.h"
#include "fpregs.h"

/* Symbolic definitions of some of the ML registers */
/* NOTE:  All the following except ptrtmp are used as "general registers"
   by the ML code, and are not safe to use in signal-handling code.
   ptrtmp is used by ML code but is never live at the top of a procedure.
*/
#define storeptr 22
#define allocptr 23
#define limit 19
#define stdarg 2
#define stdcont 3
#define stdclos 4
#define exncont 30
#define varptr 20
#define atmp1 18   
#define atmp2 25  /* not gc'ed - shouldn't matter */
#define atmp3 17
#define tmp4 15
#define icounter 24


#define csave1 5
#define csave2 6
#define csave3 7
#define csave4 8
#define csave5 9
#define csave6 10
#define csave7 11
#define csave8 12
#define csave9 13

#define ptrtmp 21

#define ml_allocptr_offset 	0	/* offsets in MLState */
#define ml_cont_offset		16
#define inML                    96
#define request                100
#define handlerPending         104
#define inSigHandler           108
#define maskSignals            112
#define NumPendingSigs         116
#define ioWaitFlag             120
#define GCpending              124

#define CLOSURE(name)						\
	    .globl	name;					\
	    .align	2; /* actually 4-byte alignment */	\
	    .set noreorder;					\
    name:   .word	MAKE_DESC(1, tag_record);		\
            .word	9f; /* address of routine */		\
	    .word	closmask; /* register mask	       */	\
	    .word	tag_backptr;				\
	    .set reorder;					\
    9:


#if (CALLEESAVE > 0)
#define CONT(name)						\
	    .globl	name;					\
	    .align	2; /* actually 4-byte alignment */	\
	    .set noreorder;					\
	    .word	contmask; /* register mask   */	\
    name:   .word	tag_backptr;				\
	    .set reorder;
#else
#define CONT(name)						\
	    .globl	name;					\
	    .align	2; /* actually 4-byte alignment */	\
	    .set noreorder;					\
    name:   .word	MAKE_DESC(1, tag_record);		\
            .word	9f; /* address of routine */		\
	    .word	contmask; /* register mask	       */	\
	    .word	tag_backptr;				\
	    .set reorder;					\
    9:
#endif

#if (CALLEESAVE > 0)
#define CONTINUE						\
            j		$stdcont
#else
#define CONTINUE						\
	    lw		$ptrtmp,0($stdcont);			\
	    j		$ptrtmp
#endif

	.text

	.globl	saveregs
	.globl	handle_c
	.globl	return_c
	.globl	restoreregs
	.globl	savefpregs
	.globl	restorefpregs


#define regspace	44
#define localspace	4
#define argbuild	16
#define framesize	(regspace+localspace+argbuild) /* must be multiple of 8 */
#define frameoffset	(0-localspace)
#define MLSTATE_OFFSET  (argbuild+40)

	.globl	saveregs
	.ent	saveregs

/* sig_return : ('a cont * 'a) -> 'b
 */
CONT(sigh_return_c)
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

CONT(return_c) /* continuation for ML functions called from C */
	li	$atmp1,REQ_RETURN
	b	set_request

CLOSURE(callc_v)
	add	$0,$allocptr,$limit
	li	$atmp1,REQ_CALLC
	/* fall through */

	.globl	set_request
	.ent	set_request
set_request:
	lw	$ptrtmp,MLSTATE_OFFSET($sp)	/* save the minimal ML state */
	sw	$atmp1,request($ptrtmp)
	sw	$0,inML($ptrtmp)	/* note that we have left ML */
	sw	$allocptr,0($ptrtmp)
	sw	$storeptr,8($ptrtmp)
	sw	$stdarg,12($ptrtmp)
	sw	$stdcont,16($ptrtmp)
	sw	$stdclos,20($ptrtmp)
	sw	$exncont,24($ptrtmp)
        sw      $csave1,32($ptrtmp)
        sw      $csave2,36($ptrtmp)
        sw      $csave3,40($ptrtmp)
        sw      $csave4,44($ptrtmp)
        sw      $csave5,48($ptrtmp)
        sw      $csave6,52($ptrtmp)
        sw      $csave7,56($ptrtmp)
        sw      $csave8,60($ptrtmp)
        sw      $csave9,64($ptrtmp)
	sw	$varptr,88($ptrtmp)
	sw	$icounter,92($ptrtmp)
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
	lw	$ptrtmp,MLSTATE_OFFSET($sp)	/* save the ML state */
	sw	$0,inML($ptrtmp)	/* note that we have left ML */
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
	sw	$varptr,88($ptrtmp)
	sw	$icounter,92($ptrtmp)
	b	restore_c_regs

	.end	saveregs

	.ent	restoreregs
restoreregs:
	subu	$sp,framesize		/* allocate a stack frame */
					/* save the C registers */
.frame $sp,framesize,$0
.mask 0xc0ff0000,0-localspace
	sw	$4,MLSTATE_OFFSET($sp)	/* save MLState ptr for return to C */
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
					
	move    $ptrtmp,$4             /* MLState ptr should be in */
                                       /* C standard arg thanks to run_ml */
	lw	$allocptr,0($ptrtmp)
	lw	$limit,4($ptrtmp)
	li	$atmp1,0x7fffffff	/* adjust the limit register */
	sub	$limit,$atmp1,$limit
	lw	$storeptr,8($ptrtmp)
	li	$atmp1,1
.set	noreorder			/* the order here is important */
	sw	$atmp1,inML($ptrtmp)	/* note that we are entering ML code */
	lw	$atmp1,GCpending($ptrtmp)	/* check for gc sync */
	nop
	beqz	$atmp1,6f
	nop
	li	$limit,0x7fffffff	/* adjust the limit register */
6:
	lw	$atmp1,NumPendingSigs($ptrtmp)	/* check for pending signals */
	nop				/* (load delay slot) */
	bnez	$atmp1,1f
	nop				/* (branch delay slot) */
8:	lw	$stdarg,12($ptrtmp)
	lw	$stdcont,16($ptrtmp)
	lw	$stdclos,20($ptrtmp)
	lw	$exncont,24($ptrtmp)
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
	lw	$varptr,88($ptrtmp)
	lw	$icounter,92($ptrtmp)
	lw	$ptrtmp,28($ptrtmp)
	nop
	.globl	go
	.ent	go
go:	j	$ptrtmp			/* jump to ML code */
	nop
1:				      /* there are pending signals, */
	lw	$atmp1,maskSignals($ptrtmp)	/* are signal masked? */
	nop				/* (load delay slot) */
	bnez	$atmp1,8b
	nop				/* (branch delay slot) */
	lw	$atmp1,inSigHandler($ptrtmp)	/* check for a pending handler */
	nop				/* (load delay slot) */
	bnez	$atmp1,8b              
        li	$atmp1,1		/* (branch delay slot) */
	sw	$atmp1,handlerPending($ptrtmp)	/* note the pending handler */
	li	$limit,0x7fffffff	/* trap on the next limit check. */
	beqz 	$0,8b
	nop
.set	reorder
	.end	restoreregs

	.text
	.ent savefpregs			/* Only called from signal.c */
	.set reorder
savefpregs:
	move 	$atmp1, $4             /* MLState pointer should be in C */
					/* standard arg register.         */
	li      $atmp2, MAKE_DESC(NSAVED_FPREGS*8, tag_string)
	lw 	$atmp1, ml_allocptr_offset($atmp1)
        sw      $atmp2, 0($atmp1)       /* string tag */
	swc1	$f20,	4($atmp1)	/* fpr20 */
	swc1	$f21,   8($atmp1)
	swc1	$f22,   12($atmp1)	/* fpr22 */
	swc1	$f23,   16($atmp1)
	swc1	$f24,   20($atmp1)	/* fpr24 */
	swc1	$f25,   24($atmp1)
	swc1	$f26,   28($atmp1)	/* fpr26 */
	swc1	$f27,   32($atmp1)
	swc1	$f28,   36($atmp1)	/* fpr28 */
	swc1	$f29,   40($atmp1)
	j 	$31			/* return */
	.end	savefpregs

	.ent	restorefpregs		/* Only called from signal.c */
	.set 	reorder
restorefpregs:
	add	$atmp1, $4, 0		/* floats address passed as parm */
	lwc1	$f20,	0($atmp1)	/* retrieve float registers */
	lwc1	$f21,	4($atmp1)
	lwc1	$f22,	8($atmp1)
	lwc1	$f23,	12($atmp1)
	lwc1	$f24,	16($atmp1)
	lwc1	$f25,	20($atmp1)
	lwc1	$f26,	24($atmp1)
	lwc1	$f27,	28($atmp1)
	lwc1	$f28,	32($atmp1)
	lwc1	$f29,	36($atmp1)
	j	$31
	.end 	restorefpregs

/* try_lock : spin_lock -> bool
 * low-level primitive for mutual-exclusion among processors -- note,
 * the "atomicity" guaranteed by try_lock is only between processors and
 * not between signals.  
 */
CLOSURE(try_lock_v)
#if (MAX_PROCS > 1)
#ifdef SGI
.set noreorder
	lw	$stdarg,0($stdarg)
	nop
	sll	$stdarg,1
	xori	$stdarg,3
	andi	$stdarg,3
.set reorder
	CONTINUE
#endif SGI
#else (MAX_PROCS == 1)
	lw	$atmp1,0($stdarg)
	li	$atmp2,1		/* ML_false */
	sw	$atmp2,0($stdarg)
	move	$stdarg,$atmp1
	CONTINUE
#endif (MAX_PROCS > 1)

CLOSURE(unlock_v)
#if (MAX_PROCS > 1)
#ifdef SGI
	sw	$0,0($stdarg)
	li	$stdarg,1
	CONTINUE
#endif SGI
#else (MAX_PROCS == 1)
	li	$atmp1,3		/* ML_true */
	sw	$atmp1,0($stdarg)
	li	$stdarg,1		/* just return unit */
	CONTINUE
#endif (MAX_PROCS > 1)

/* masksigs : bool -> unit
 * when given ML_true, keeps signal handler from being executed.
 * when given ML_false, makes handlers executable.
 */
CLOSURE(masksigs_v)
.set	noreorder
	lw	$ptrtmp,MLSTATE_OFFSET($sp)
	sra	$stdarg,1
	lw	$atmp1,maskSignals($ptrtmp)
	beqz	$stdarg,1f			/* goto 1 if false */
	li	$stdarg,1			/* delay slot */

	beqz	$0,2f
	add	$atmp1,$stdarg,$atmp1		/* else increment maskSignals*/
						/* delay slot */

1:	sub	$atmp1,$atmp1,$stdarg		/* decrement maskSignals */
2:	sw	$atmp1,maskSignals($ptrtmp)	/* set maskSignals */
.set	reorder
	CONTINUE

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
CLOSURE(array_v)
1:					/* jump back here after GC trap */
	lw	$atmp1,0($stdarg)	/* tagged length in $atmp1 */
	lw	$tmp4,4($stdarg)		/* get initial value in $tmp4 */
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
	sw	$tmp4,0($allocptr)	  /* store the value */
        addi	$allocptr,4		  /* on to the next word */
	bne	$allocptr,$atmp3,2b	  /* if not off the end, repeat */
					/* end loop */
        CONTINUE

3:					/* we need to do a GC */
.set noreorder
	li	$limit,0x7fffffff	/* MAXINT into limit will cause GC trap */
	b	4f
	nop
	.word	closmask /* register mask */
	.word	0	/* space for back ptr */
4:	add	$0,$allocptr,$limit	/* trap */
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
	b	4f
	nop
	.word	closmask|0x200 /* register mask (std regs + $11) */
	.word	0	/* space for back ptr */
4:	add	$0,$allocptr,$limit	/* trap */
	b	1b
	nop
.set reorder

/* create_v_v : int * 'a list -> 'a vector
 * 	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
CLOSURE(create_v_v)
#define ML_NIL 		1
#define ML_LIST_HD(p)	0(p)
#define ML_LIST_TL(p) 	4(p)
1:					/* jump back here after GC trap */
	lw	$atmp1,0($stdarg)	/* atmp1 := tagged length */
	sra	$atmp1,1		/* untagged length */
	sll	$atmp2,$atmp1,width_tags/* build descriptor in $atmp2 */
	ori	$atmp2,tag_record	/* tag field */
	sll	$atmp1,2		/* get length in bytes into $atmp1 */
.set noreorder
	li	$atmp3,0x7fffffff
	sub	$atmp3,$atmp3,$limit	/* get "true" limit value */
	sub	$atmp3,$atmp3,$allocptr	/* subtract allocptr */
	sub	$atmp3,$atmp3,$atmp1	/* subtract requested bytes */
	blez	$atmp3,5f		/* do we have enough? */
	nop
.set reorder
	sw	$atmp2,0($allocptr)	/* store the descriptor */
	addi	$allocptr,4		/* points to new object */
        lw      $atmp2, 4($stdarg)      /* atmp2 := list */
	move	$stdarg,$allocptr	/* (return val = arg of continuation) */
        li      $atmp3, ML_NIL          /* atmp3 := NIL */
3:					/* loop */
        lw      $tmp4, ML_LIST_HD($atmp2) /* tmp4 := data */
        sw      $tmp4, 0($allocptr)     /* update vector */
        lw      $atmp2, ML_LIST_TL($atmp2)
        addi    $allocptr, 4            /* next index */
        bne     $atmp2, $atmp3, 3b      /* reached end? */
4:
        CONTINUE

5:					/* we need to do a GC */
.set noreorder
	li	$limit,0x7fffffff       /* MAXINT into limit will cause GC trap */
	b	6f
	nop
	.word	closmask /* register mask */
	.word	0	/* space for back ptr */
6:	add	$0,$allocptr,$limit	/* trap */
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
.set noreorder
maxint:	.double	1073741824.0
.set reorder
CLOSURE(floor_v)
	lwc1	$f4,LITTLEPART($stdarg)	/* get least significant word */
	lwc1	$f5,BIGPART($stdarg)	/* get most significant word */
	mtc1	$0,$f2			/* ($f2,$f3) := maxint */
 	lui	$atmp3,0x41d0
	mtc1	$atmp3,$f3
	abs.d	$f6,$f4
	c.le.d	$f6,$f2
	cfc1	$atmp3,$31			/* grab fpa control register */
	bc1f	over
	ori	$atmp2,$atmp3,0x03		/* set rounding bits to 11 */
	ctc1	$atmp2,$31			/* return fpa control register */
	cvt.w.d $f6,$f4			/* convert to integer */
	ctc1	$atmp3,$31			/* return fpa control register */
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
8:	li	$tmp4,MAKE_DESC(8,tag_string) /* make descriptor */
	sw	$tmp4,0($allocptr)	/* save descriptor */
	add	$stdarg,$allocptr,4	/* get pointer to new float */
	add	$allocptr,12		/* point to new free word */
        CONTINUE

9:	lw	$stdarg,0($stdarg)	/* get old float */
	CONTINUE

over:	li	$atmp3,0x7fffffff
	add	$atmp3,$atmp3		/* generate overflow exception */

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
	ori 	$atmp1,$atmp1,0xe00	/* set V, O, and Z bits */
	ctc1	$atmp1,$31		/* return fpa control register */
	j	$31
	.end	set_fsr

/* this bogosity is for export.c */
	.globl	startptr
startptr: .word    __start
