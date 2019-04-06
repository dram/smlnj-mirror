/* M68.prim.s
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * M68 primitive runtime code for SML/NJ.
 *
 * The m680x0 registers are used as follows; the names in parentheses are the
 * MLState field names (see ml_state.h):
 *
 * a6 = freespace pointer (ml_allocptr)
 * d5 = data limit (ml_limitptr)
 * d6 = store ptr (ml_storeptr)
 * d3 = var ptr (ml_varptr)
 *
 * a0 = standard arg (ml_arg)
 * a1 = standard continuation (ml_cont)
 * a2 = standard closure (ml_closure)
 * d7 = exception handler continuation (ml_exncont)
 *
 * a3,a4 = general purpose pointers (ml_roots[4] and ml_roots[5])
 * a5 = pointer temps
 * d0,d1 = arithtemps used by generic.sml
 * d2,d4 = arithtemps used by m68.sml
 *
 * a7 = stack ptr (not used by ML)
 *
 *
 * The ML state structure has the following layout for the mc680x0 (see "ml_state.h"):
 *
 *		+-------------------+
 *  MLState --> | ml_allocptr (a6)  |
 *		+-------------------+
 *	+4:	| ml_limitptr (d5)  |
 *		+-------------------+
 *	+8:	| ml_storeptr (d6)  |
 *		+-------------------+
 *	+12:	|  ml_exncont (d7)  |
 *		+-------------------+
 *	+16:	|   ml_arg (a0)     |
 *		+-------------------+
 *	+20:	|   ml_cont (a1)    |
 *		+-------------------+
 *	+24:	| ml_closure (a2)   |
 *		+-------------------+
 *	+28:	|     (a3, a4)      |
 *		+-------------------+
 *	+36:	| ml_varptr (d3)    |
 *		+-------------------+
 *	+40:	|       ml_pc       |
 *		+-------------------+
 *      +44:    |       inML        |
 *		+-------------------+
 *      +48:    |     request       |
 *		+-------------------+
 *      +52:    |   handlerPending  |
 *		+-------------------+
 *      +56:    |    inSigHandler   |
 *		+-------------------+
 *      +60:    |     maskSignals   |
 *		+-------------------+
 *      +64:    |   NumPendingSigs  |
 *		+-------------------+
 *      +68:    |     ioWaitFlag    |
 *		+-------------------+
 *      +72:    |     GCpending     |
 *		+-------------------+
 *      +76:    |      saved_pc     |
 *		+-------------------+
 *              |        ...        |
 */

#include "mask.h"
#include "tags.h"
#include "request.h"
#include "fpregs.h"

#define allocptr 	a6
#define limit		d5
#define storeptr	d6
#define exncont		d7
#define stdarg		a0
#define stdcont		a1
#define stdclos		a2

#define ptrtmp		a5

#define ml_allocptr_offset 0

#define inML 44
#define request 48
#define handlerPending 52
#define inSigHandler 56
#define maskSignals 60
#define NumPendingSigs 64
#define ioWaitFlag 68
#define GCpending 72
#define saved_pc 76

/* the save mask for saving C registers on callee save systems */
#define	CREGMASK	d2-d7/a2-a6
/* the MLState pointer is passed to restoreregs on the stack.  Once we
 * push the C-registers (11 of them) onto the stack, the MLState pointer
 * can be found at the following offset:
 */
#define MLSTATE_OFFSET ((11+1)*4)

#define CLOSURE(name)					\
	    .globl	name;				\
	    .align	2;				\
    name:   .long	MAKE_DESC(1,tag_record);	\
            .long	7f;				\
	    .long	7;	/* reg. mask */		\
	    .long	tag_backptr;			\
    7:

#define RAISE						\
	    movl	d7,a1; 				\
	    movl	a1@,a2; 			\
	    jmp		a2@

	.text
	.globl	_sigh_resume, _saveregs, _restoreregs

/* sigh_return_c:
 * The return continuation for the ML signal handler.
 */
CLOSURE(_sigh_return_c)
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_SIG_RETURN,a5@(request)
	jra	_quicksave

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont (a1).
 */
_sigh_resume:
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_SIG_RESUME,a5@(request)
	jra	_quicksave

CLOSURE(_handle_c)
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_EXN,a5@(request)
	jra	_quicksave

CLOSURE(_return_c)
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_RETURN,a5@(request)
	jra	_quicksave

CLOSURE(_callc_v)
	cmpl	a6,d5			/* check the heap limit */
	trapmi
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	#REQ_CALLC,a5@(request)
	/* fall into quicksave */

_quicksave:
	clrl	a5@(inML)		/* note that we have left ML code */
	movl	a6,a5@
	moveml	d6-d7/a0-a2,a5@(8)	/* save storeptr, exncont, arg, cont, closure */
	movl    d3,a5@(36)		/* save varptr */
	moveml	sp@+,CREGMASK		/* restore the C registers */
	rts				/* return to run_ml() */

_saveregs:
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	clrl	a5@(inML)		/* note that we have left ML code */
	movl	a6,a5@
	moveml	d6-d7/a0-a4,a5@(8)
	movl    d3,a5@(36)		/* save varptr */
	moveml	sp@+,CREGMASK		/* restore the C registers */
	rts				/* return to run_ml() */

_restoreregs:
	moveml	CREGMASK,sp@-		/* save the C registers */
	movl	sp@(MLSTATE_OFFSET),a5  /* get the MLState ptr off the stack */
	movl	a5@,a6
	moveml	a5@(4),d5-d7/a0-a4  
	movl    a5@(36),d3
	addql	#1,a5@(inML)		/* note that we are executing ML code */
	tstl	a5@(GCpending)
	jne	3f

	tstl	a5@(NumPendingSigs)	/* check for pending signals */
	jne	2f
1:
	movl	a5@(40),a5		/* fetch the saved pc */
	jmp	a5@
2:				      /* there are pending signals */
	tstl	a5@(maskSignals)		/* are signals masked? */
	jne	1b
	tstl	a5@(inSigHandler)	/* check if we are currently handling a signal */
	jne	1b
	addql	#1,a5@(handlerPending)	/* note that a handler trap is pending */
3:	clrl	d5			/* generate a trap on the next limit check */
	movl	a5@(40),a5		/* fetch the saved pc */
	jmp	a5@


	.globl _savefpregs
_savefpregs:
	link 	a6, #-4				/* temp. space */
	movl 	a0, a6@(-4)			/* save a0 */
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	_MLproc, a0			/* ml state vector */
#endif
	movl	a0@(ml_allocptr_offset), a0	/* next available heap addr */
	movl 	#MAKE_DESC(NSAVED_FPREGS*8,tag_string), a0@+
	fmoved  fp0, a0@+			/* save floating registers */
	fmoved	fp1, a0@+
	fmoved	fp2, a0@+
	fmoved	fp3, a0@+
	fmoved	fp4, a0@+
	fmoved	fp5, a0@+
	fmoved	fp6, a0@+
	fmoved	fp7, a0@+
	movl	a6@(-4), a0			/* restore a0 */
	unlk	a6
	rts

	.globl  _restorefpregs
_restorefpregs:
	link	a6, #-4				/* temp. space */
	movl	a0, a6@(-4)			/* save a0 */
	movl	a6@(8), a0			/* pointer to string of floats */
	fmoved	a0@+, fp0			/* restore floats */
	fmoved	a0@+, fp1
	fmoved	a0@+, fp2
	fmoved	a0@+, fp3
	fmoved	a0@+, fp4
	fmoved	a0@+, fp5
	fmoved	a0@+, fp6
	fmoved	a0@+, fp7
	movl	a6@(-4), a0			/* restore a0 */
	unlk	a6
	rts


/* adjust_limit:
 * Adjust the heap limit pointer so that a trap will be generated on the next limit
 * check and then continue executing ML code.
 * NOTE: this code cannot trash any registers  (other than d5) or the condition
 * codes.  Also, we need to come up with a solution for the case when
 * MAX_PROCS > 1 for getting at the saved_pc and condition codes.
 */
	.globl	_adjust_limit, _saved_pc
_adjust_limit:
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
        movw    cc,d5             /* save condition codes */ 
        movl    _saved_pc,sp@-
        movw    d5,sp@-           /* push the saved condition codes */
        clrl    d5                /* generate a trap on the next limit check */
        rtr                       /* return, restoring condition codes */
#endif

/* array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
CLOSURE(_array_v)
1:					/* jump back here after GC trap */
	movl	a0@,d1			/* get tagged length */
	asrl	#1,d1			/* d1 = length (untagged) */
	movl	d1,d2
	asll	#width_tags,d2
	orl	#tag_array,d2		/* d2 = new tag */
	asll	#2,d1
	movl	a6,d4
	addl	d1,d4
        cmpl	d4,d5			/* check the heap limit */
        jmi 	4f
	movl	a0@(4),d0		/* d0 = initial value */
	movl	d2,a6@+			/* write the tag */
	movl	a6,a0
	jra	3f
2:	movl	d0,a6@+			/* store default */
3:	subql	#4,d1
	jge 	2b
	movl	a1@,a2
	jmp	a2@

.align	2
	.long	7	/* reg. mask */
	.long	0	/* dummy space */
	clrl	d0			/* the pc gets adjusted to here by ghandle(), */
					/* so this must clear the condition codes */

4:					/* we need to do a GC before allocation */
	trapmi
	jra	1b

/* create_b : int -> bytearray
 * create_s : int -> string
 * Create bytearray or string of given length.	This can cause GC.
 */
CLOSURE(_create_b_v)
	movl	#tag_bytearray,d0
	jra	1f

CLOSURE(_create_s_v)
	movl	#tag_string,d0
1:				/* jump back to here after a GC trap */
	movl	a0,d1
	asrl	#1,d1		/* d1 = length */
	movl	d1,d2
	asll	#width_tags,d2
	addl	d0,d2		/* d2 = new tag */
	andl	#~3,d1		/* d1+4 = bytes in string, not including tag */
	movl	a6,d4
	addl	d1,d4
	cmpl	d4,d5		/* check the heap limit */
	jmi	2f
	movl	d2,a6@+		/* write the tag */
	movl	a6,a0
	addl	d1,a6
	clrl	a6@+		/* must clear the last 4 bytes */
	movl	a1@,a2
	jmp	a2@

2:				/* we need to do a GC before allocation */
	movl	d0,a3		  /* preserve tag across the garbage collection */
	jra	3f
	.align	2
/*	.long	closmask|8	This expression didn't work on the sun-3
                               assembler, so I put this in: */
	.long	0xf        /* reg. mask (std. regs. + a3) */
	.long	0	/* dummy space */
	clrl	d4		  /* the pc gets adjusted to here by ghandle(), so */
				  /* this instruction must clear the cond. codes */
3:	trapmi
	movl	a3,d0
	jra	1b		  /* restart the allocation */


/* create_v_v : int * 'a list -> 'a vector
 * 	creates a vector with elements taken from a list.
 *	n.b. The front end ensure that list cannot be nil.
 */
CLOSURE(_create_v_v)
#define ML_NIL	1
#define ML_LIST_HD(p) 	p@
#define ML_LIST_TL(p) 	p@(4)
1:						/* jump back here after GC */
	movl 	stdarg@,d1			/* d1 := tagged length */
	asrl	#1,d1				/* untagged length */
	movl	d1,d2				/* build descriptor in d2 */
	asll	#width_tags,d2			/* length field */
	orl	#tag_record,d2 			/* tag field */
	asll	#2,d1				/* d1 := length in bytes */
	movl	allocptr,d4	
	addl	d1,d4				/* d4 := new allocptr */
	cmpl	d4,limit			/* check heap limit */
	jmi	5f				/* invoke gc */

	movl	d2,allocptr@			/* store the descriptor */
	addl	#4,allocptr			/* points to new object */
	movl	stdarg@(4),ptrtmp		/* ptrtmp := list */
	movl	allocptr,stdarg 		/* return value */
	movl	#ML_NIL,d1			/* d1 := NIL */
3:						/* loop */
	movl	ML_LIST_HD(ptrtmp),allocptr@ 	/* update vector */
	movl 	ML_LIST_TL(ptrtmp),ptrtmp	/* cdr list */
	addl	#4, allocptr			/* next store location */
	cmpl	ptrtmp,d1			/* continue ? */
	bne	3b

	movl	stdcont@,stdclos		/* invoke continuation */
	jmp 	stdclos@
5:						/* do GC before allocation */
	trapmi
	jra	1b
	
		


/* try_lock: spin_lock -> bool. 
 * low-level test-and-set style primitive for mutual-exclusion among 
 * processors.  For now, we only provide a uni-processor trivial version.
 */
CLOSURE(_try_lock_v)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	a0@,d0			/* get old value of lock */
	movl	#1,a0@			/* set the lock to ML_false */
	movl	d0,a0			/* return old value of lock */
	movl	a1@,a2
	jmp	a2@
#endif

/* unlock : releases a spin lock 
 */
CLOSURE(_unlock_v)
#if (MAX_PROCS > 1)
	???
#else (MAX_PROCS == 1)
	movl	#3,a0@			/* store ML_true into lock */
	movl	#1,a0			/* and return unit         */
	movl	a1@,a2
	jmp	a2@
#endif

/* masksigs : Increments maskSignals when true, decrements it when false.
 */
CLOSURE(_masksigs_v)
	movl	sp@(MLSTATE_OFFSET),a5	/* get the MLState ptr off the stack */
	movl	a0,d0
	cmpl	#1,d0
	jeq	1f
	addql	#1,a5@(maskSignals)
	movl	#1,a0
	movl	a1@,a2
	jmp	a2@
1:	subql	#1,a5@(maskSignals)
	movl	#1,a0
	movl	a1@,a2
	jmp	a2@


/* Floating point primitives
 *
 * All the m68code for ML reals assumes that NaN's and INF's are never
 * generated and therefore do not need to be handled.
 * This code does not produce NaN's or INF's as long as none are passed to it
 * and overflow, underflow, and operr are handled.
 *
 * Floating exceptions raised (assuming NaN's and INF's are never passed to
 * functions):
 *	OPERR - (div) for 0.0/0.0 (does NOT also cause DZ)
 *	DZ - (div) for (in range) / 0.0
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate */

CLOSURE(_floor_v)
	fmoved	a0@,fp0
	ftstx	fp0		| handle positive and negative cases separately
	fblt	1f
/* positive numbers */
	fintrzx	fp0,fp0		| round towards zero (down)
	fmovel	fp0,d0
	asll	#1,d0
	trapv
	addql	#1,d0
	movl	d0,a0
	movl	a1@,a2
	jmp	a2@
/* negative numbers */
1:	fintrzx	fp0,fp1		| round towards zero (up)
	fmovel	fp1,d0
        asll	#1,d0
	trapv
	fcmpx	fp0,fp1
	fjeq	1f
	subql	#1,d0
	trapv
	movl	d0,a0
	movl	a1@,a2
	jmp	a2@
1:	addql	#1,d0
	movl	d0,a0
	movl	a1@,a2
	jmp	a2@

/* returns 0 on 0. */
CLOSURE(_logb_v)
	fgetexpd a0@,fp0
	fmovel	fp0,d0
	asll	#1,d0
	addql	#1,d0
	movl	d0,a0
	movl	a1@,a2
	jmp	a2@

CLOSURE(_scalb_v)
	lea	_overflow_e0+4,a0
	RAISE

#ifdef sun3
	.globl	_minitfp_		/* checks for 68881 and sets flags */
	.globl	_fp_state_mc68881	/* a flag that gets set */
#define fp_enabled 2 /* from /usr/src/lib/libc/sun/crt/fpcrttypes.h */
#endif
	.align	2
/* Enable/disable float operand error, overflow, and div.  If no 68881
   is present, nothing happens. */
	.globl _fpenable
	.globl _fpdisable
_fpenable:
#ifdef sun3
	jsr	_minitfp_		/* checks for 68881 and sets flags.
					   normally executed on startup,
					   but won't be if compiled without
					   -f68881 (for possible sun/50
					   compatibility).  This is just
					   to make sure. */
	cmpl	#fp_enabled,_fp_state_mc68881
	jne	1f
#endif
	fmovel	#0x3400,fpcr
1:	rts
_fpdisable:
#ifdef sun3
	cmpl	#fp_enabled,_fp_state_mc68881
	jne	1f
#endif sun3
	fmovel	#0,fpcr
1:	rts

/* Mathematical routines */

CLOSURE(_arctan_v)
        cmpl	a6,d5
        trapmi
	fatand	a0@,fp0
        jra	finishfloat
CLOSURE(_cos_v)
        cmpl	a6,d5
        trapmi
	fcosd	a0@,fp0
        jra	finishfloat
CLOSURE(_exp_v)
	fetoxd	a0@,fp0
        jra	finishfloat
CLOSURE(_ln_v)
        cmpl	a6,d5
        trapmi
	fmoved  a0@,fp0
	ftstx	fp0
	fble	1f
	flognx	fp0,fp0
        jra	finishfloat
1:	lea	_ln_e0+4,a0
	RAISE		
CLOSURE(_sin_v)
        cmpl	a6,d5
        trapmi
	fsind	a0@,fp0
        jra	finishfloat
CLOSURE(_sqrt_v)
        cmpl	a6,d5
        trapmi
	fmoved  a0@,fp0
	ftstx	fp0
	fblt	1f
	fsqrtx	fp0,fp0
        jra	finishfloat
1:	lea	_sqrt_e0+4,a0
	RAISE		

finishfloat:			/* allocate a heap object for the result */
	movl	#MAKE_DESC(8,tag_string),a6@+
	movl	a6,a0
	fmoved	fp0,a6@+
	movl	a1@,a2
	jmp	a2@


#ifdef NeXT
/* this is needed to avoid a bug in the NeXT's implementation of syscall */
	.globl	_syscall
_syscall:
	movl	sp@(4),d0	/* get the syscall code */
	movl	sp@,sp@(4)
	movl	d0,sp@		/* the stack now is: syscall#, return-pc, args ... */
	trap	#0		/* do the syscall */
				/* the stack now is: return-pc, args ... */
	bcs	2f		/* check for errors */
1:				/* return */
	movl	sp@,a0
	jmp	a0@
2:				/* an error, save the errno and return -1 */
	movl	d0,_errno
	moveq	#-1,d0
	jra	1b

#endif

/* this bogosity is for export.c */
	.globl	_startptr
_startptr:
	.long	start
