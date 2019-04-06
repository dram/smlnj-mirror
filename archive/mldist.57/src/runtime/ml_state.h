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
#   define NROOTS	    19		/* pc, %i0-%i5, %g7, %g1-%g3, %l0-%l7 */
#   define PC_INDX	    0
#   define EXN_INDX	    7		/* %g7 */
#   define ARG_INDX	    1		/* %i0 */
#   define CONT_INDX	    2		/* %i1 */
#   define CLOSURE_INDX	    3		/* %i2 */
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
#   define NROOTS	    10		/* r0-r7, r13, pc (r15) */
#   define PC_INDX	    9		/* a.k.a. r15 */
#   define EXN_INDX	    0		/* r13 */
#   define ARG_INDX	    1		/* r0 */
#   define CONT_INDX	    2		/* r1 */
#   define CLOSURE_INDX	    3		/* r2 */
#endif

#ifdef MIPS
#   define NROOTS	    19		/* $2-$4, $30, pc, $5-$18 */
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


typedef struct {
    int		ml_allocptr;		/* the pointer to the next word to allocate */
    int		ml_limitptr;		/* the heap limit pointer */
    int		ml_storeptr;		/* the list of store operations */
    int		ml_roots[NROOTS];	/* the root registers */
#   define	ml_pc		ml_roots[PC_INDX]
#   define	ml_exncont	ml_roots[EXN_INDX]
#   define	ml_arg		ml_roots[ARG_INDX]
#   define	ml_cont		ml_roots[CONT_INDX]
#   define	ml_closure	ml_roots[CLOSURE_INDX]
#ifdef GLOBAL_INDX
#   define	ml_globalptr	ml_roots[GLOBAL_INDX]
#endif
} MLState_t;

extern MLState_t *MLState;

#ifndef GLOBAL_INDX
#define N_MISC_ROOTS		(NROOTS-5)
#else
#define N_MISC_ROOTS		(NROOTS-6)
#endif

extern int	MiscRootMap[N_MISC_ROOTS];

#endif !_ML_STATE_
