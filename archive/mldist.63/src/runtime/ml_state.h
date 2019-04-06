/* ml_state.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * This defines the layout of the ML state vector.  The ML state consists of
 * the current allocation pointer (ml_allocptr), the heap limit pointer (ml_limitptr),
 * the store list pointer (ML_storeptr), and a set of root registers (ml_roots[]).
 * There are five distinguished root registers, which are arranged in a machine
 * dependent fashion.
 */

#ifndef _ML_STATE_
#define _ML_STATE_

#ifdef SPARC
#   define NROOTS	    20		/* pc, %i0-%i5, %g7, %g1-%g3, %l0-%l7 */
#   define PC_INDX	    0
#   define EXN_INDX	    7		/* %g7 */
#   define ARG_INDX	    1		/* %i0 */
#   define CONT_INDX	    2		/* %i1 */
#   define CLOSURE_INDX	    3		/* %i2 */
#   define BASE_INDX	    4		/* %i3 */
#   define GLOBAL_INDX	    6		/* %i5 */
#endif

#ifdef M68
#   define NROOTS	    7		/* d7, a0-a4, pc */
#   define PC_INDX	    6
#   define EXN_INDX	    0		/* d7 */
#   define ARG_INDX	    1		/* a0 */
#   define CONT_INDX	    2		/* a1 */
#   define CLOSURE_INDX	    3		/* a2 */
#endif

#ifdef VAX
#   define NROOTS	    11		/* r0-r7,r10, r13, pc (r15) */
#   define PC_INDX	    10		/* a.k.a. r15 */
#   define EXN_INDX	    0		/* r13 */
#   define ARG_INDX	    1		/* r0 */
#   define CONT_INDX	    2		/* r1 */
#   define CLOSURE_INDX	    3		/* r2 */
#endif

#ifdef MIPS
#   define NROOTS	    21		/* $2-$4, $30, pc, $5-$18, $20, $25 */
#   define PC_INDX	    4
#   define EXN_INDX	    3		/* $30 */
#   define ARG_INDX	    0		/* $2 */
#   define CONT_INDX	    1		/* $3 */
#   define CLOSURE_INDX	    2		/* $4 */
#endif

#ifdef NS32
#   define NROOTS
#   define PC_INDX	    ?
#   define EXN_INDX	    ?		/* ?? */
#   define ARG_INDX	    ?		/* r0 */
#   define CONT_INDX	    ?		/* r1 */
#   define CLOSURE_INDX	    ?		/* r2 */
#endif


/* All ML values are represented by a (32-bit) word.  A value is either a tagged
 * integer (unboxed), or a pointer to a heap object (boxed).
 */
typedef unsigned int *ML_val_t;

typedef struct {
    int		ml_allocptr;		/* the pointer to the next word to allocate */
    int		ml_limitptr;		/* the heap limit pointer */
    int		ml_storeptr;		/* the list of store operations */
    ML_val_t	ml_roots[NROOTS];	/* the root registers */
#   define	ml_pc		ml_roots[PC_INDX]
#   define	ml_exncont	ml_roots[EXN_INDX]
#   define	ml_arg		ml_roots[ARG_INDX]
#   define	ml_cont		ml_roots[CONT_INDX]
#   define	ml_closure	ml_roots[CLOSURE_INDX]
#ifdef BASE_INDX
#   define	ml_baseptr	ml_roots[BASE_INDX]
#endif
#ifdef GLOBAL_INDX
#   define	ml_globalptr	ml_roots[GLOBAL_INDX]
#endif
} MLState_t;

extern MLState_t *MLState;

/* The number of root registers that the code generator might use as function
 * arguments (and thus might appear in a register mask).  The pc, exncont and
 * globalptr are excluded.
 */
#if defined(GLOBAL_INDX) && defined(BASE_INDX)
#   define N_ARG_REGS	(NROOTS-4)
#else
#   if defined(GLOBAL_INDX) || defined(BASE_INDX)
#       define N_ARG_REGS	(NROOTS-3)
#   else
#       define N_ARG_REGS	(NROOTS-2)
#   endif
#endif
#define ALL_ARGS_MASK	((1 << N_ARG_REGS)-1)
#define STD_ARGS_MASK	0x7

/* This table maps the register numbers of the argument registers used by the
 * code generator to the proper indices of the root vector.  The standard closure
 * is in slot-0, the standard arg in slot-1 and the standard continuation is in
 * slot-2.
 */
extern int	ArgRegMap[N_ARG_REGS];

#endif !_ML_STATE_
