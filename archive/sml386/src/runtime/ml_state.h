/* ml_state.h   (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 * 
 * Altered 20 Dec. 1991 by: Yngvi S. Guttesen
 *                          Department of Computer Science
 *                          The Technical University of Denmark
 *                          DK-2800 Lyngby
 *
 * This defines the layout of the ML state vector.  The ML state consists of
 * the current allocation pointer (ml_allocptr), the heap limit pointer (ml_limitptr),
 * the store list pointer (ML_storeptr), and a set of root registers (ml_roots[]).
 * There are five distinguished root registers, which are arranged in a machine
 * dependent fashion.
 */

#ifndef _ML_STATE_
#define _ML_STATE_


/* NROOTS = PC, ARG_INDX, CONT_INDX,
 *          CLOSURE_INDX, MISC_REGS1-4 (EBX,EDX,ESI,EDI)
 */

#define NROOTS              9       /* must mach NROOTS in prim.asm ! */
#define PC_INDX             8
#define EXN_INDX            0
#define ARG_INDX            1
#define CONT_INDX           2
#define CLOSURE_INDX        3


/* All ML values are represented by a (32-bit) word.  A value is either a tagged
 * integer (unboxed), or a pointer to a heap object (boxed).
 */
typedef long ML_val_t;

typedef struct {
    long        ml_allocptr;            /* the pointer to the next word to allocate */
    long        ml_limitptr;            /* the heap limit pointer */
    long        ml_storeptr;            /* the list of store operations */
    ML_val_t	ml_roots[NROOTS];	/* the root registers */
#   define	ml_pc		ml_roots[PC_INDX]
#   define	ml_exncont	ml_roots[EXN_INDX]
#   define	ml_arg		ml_roots[ARG_INDX]
#   define	ml_cont		ml_roots[CONT_INDX]
#   define	ml_closure	ml_roots[CLOSURE_INDX]
} MLState_t;

extern MLState_t _based(runtime_seg) *MLState;

/* The number of root registers that the code generator might use as function
 * arguments (and thus might appear in a register mask).  The pc, exncont and
 * globalptr are excluded.
 */
#define N_ARG_REGS       (NROOTS-2)
#define ALL_ARGS_MASK	((1 << N_ARG_REGS)-1)
#define STD_ARGS_MASK	0x7

/* This table maps the register numbers of the argument registers used by the
 * code generator to the proper indices of the root vector.  The standard closure
 * is in slot-0, the standard arg in slot-1 and the standard continuation is in
 * slot-2.
 */
extern int	ArgRegMap[N_ARG_REGS];

#endif
