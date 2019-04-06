/* run_ml.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * These are routines for handling the ML/C interface.
 */

#include "ml_state.h"
#include "ml_types.h"
#include "request.h"
#include "cause.h"

/* Space for the ML state vector - use doubles to force quad-word alignment on
 * machines where it matters (eg., SPARC).
 */
static double	state[(sizeof(MLState_t)+7)/8];
MLState_t	*MLState = (MLState_t *)state;
int		MiscRootMap[N_MISC_ROOTS] =
#ifdef SPARC
	{ 4, 5, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 };
#endif
#ifdef M68
	{ 4, 5 };
#endif
#ifdef VAX
	{ 4, 5, 6, 7, 8 };
#endif
#ifdef MIPS
	{ 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 };
#endif
#ifdef NS32
	{ ?? };
#endif

int		request;

void raise_ml_exn ();
static void uncaught ();

extern void check_heap ();
extern void callgc0 ();


/* init_mlstate:
 * Initialize the ML state vector (except the allocation and limit pointers).
 */
void init_mlstate ()
{
    int		i;

    MLState->ml_storeptr = (int)STORLST_nil;
    for (i = 0;  i < NROOTS;  i++)
	MLState->ml_roots[i] = (int)ML_unit;

} /* end of init_mlstate. */


/* run_ml:
 */
static void run_ml ()
{
    while (1) {
	restoreregs ();

	switch (request) {
	  case REQ_RETURN:
#ifdef GCMON
	    dumpMon();
#endif GCMON
	    return;

	  case REQ_EXN: /* an uncaught exception */
	    uncaught (MLState->ml_arg);
	    return;

	  case REQ_FAULT: { /* a hardware fault */
		extern int	fault_exn;
		raise_ml_exn (fault_exn);
	    } break;

	  case REQ_GC:
	    callgc0 (CAUSE_GC);
	    break;

	  case REQ_CALLC: {
		int	    (*f)();
		ML_val_t    arg;

		MLState->ml_closure = MLState->ml_cont;
		MLState->ml_pc	    = CODE_ADDR(MLState->ml_cont);

		check_heap (32768);

		f   = (int (*)())REC_SELPTR(MLState->ml_arg, 0);
		arg = REC_SEL(MLState->ml_arg, 1);
		(*f)(arg);
	    } break;

	  case REQ_SIGNAL: {
		extern ML_val_t make_ml_sigh_arg();
		extern int return_c[], sighandler0[], handle_c[];
		register MLState_t *msp = MLState;

		msp->ml_arg     = (int)make_ml_sigh_arg();
		msp->ml_cont    = (int)PTR_MLtoC(return_c);  /* should never return */
		msp->ml_exncont = (int)PTR_MLtoC(handle_c);
		msp->ml_closure = sighandler0[1];
		msp->ml_pc      = CODE_ADDR(msp->ml_closure);
	    } break;

	  case REQ_SIG_RETURN: {
		extern int	handle_c[], inSigHandler;
		register MLState_t *msp = MLState;
		register ML_val_t kont = REC_SEL(msp->ml_arg, 0);

	      /* throw to the cont that we have received as an argument. */
		msp->ml_arg     = (int)REC_SEL(msp->ml_arg, 1);
		msp->ml_closure = (int)ML_unit;
		msp->ml_cont    = (int)REC_SEL(kont, 0);
		msp->ml_pc      = CODE_ADDR(kont[0]);
		msp->ml_exncont = (int)REC_SEL(kont, 1);
	      /* Note that we have finished handling the signal */
		inSigHandler	    = 0;
	    } break;

	  case REQ_SIG_RESUME: {
		register int    i, *p;

	      /* restore the state from the resumption closure (which is passed
	       * in ml_cont, since this is a two-argument function). */
		for (p = PTR_MLtoC(MLState->ml_cont)+1, i = 0;  i < NROOTS;  i++)
		    MLState->ml_roots[i] = *p++;
	    } break;

	  default:
	    die ("internal error: unknown request code = %d\n", request);
	    break;
	} /* end of switch */
    }

} /* end of run_ml. */


/* raise_ml_exn:
 * Modify the ML state, so that the given exception will be raised when ML is resumed.
 */
void raise_ml_exn (exn)
    ML_val_t	exn;
{
    register int	i, kont = MLState->ml_exncont;
  /* clear obsolete roots to reduce floating garbage */
    for (i = 0;  i < NROOTS;  i++)
	MLState->ml_roots[i] = (int)ML_unit;

    MLState->ml_arg	= (int)exn;
    MLState->ml_cont	= kont;
    MLState->ml_pc	= CODE_ADDR(kont);

} /* end of raise_ml_exn. */


/* apply_ml_fn:
 * Apply the ML closure f to arg and return the result.
 */
ML_val_t apply_ml_fn (f, arg)
    ML_val_t	    f, arg;
{
    register int	i;
    extern int handle_c[], return_c[];

  /* clear the ML root registers */
    for (i = 0;  i < NROOTS;  i++)
	MLState->ml_roots[i] = (int)ML_unit;

  /* initialize the calling context */
    MLState->ml_exncont	= (int)PTR_CtoML(handle_c);
    MLState->ml_arg	= (int)arg;
    MLState->ml_cont	= (int)PTR_CtoML(return_c);
    MLState->ml_closure	= (int)f;
    MLState->ml_pc	= CODE_ADDR(f);

    run_ml();

    return (ML_val_t)(MLState->ml_arg);

} /* end of apply_ml_fn */


/* restart_ml:
 * Restart an exported ML system.
 */
void restart_ml ()
{
    restart_gc ();
    setup_signals ();

    MLState->ml_arg = (int)ML_true;

    run_ml ();

#ifdef ADVICE
    call_endadvice();
#endif

    _exit(0);

} /* end of restart_ml */


/* uncaught:
 * Handle an uncaught exception.
 */
static void uncaught (e)
    ML_val_t	e;
{
    ML_val_t	val = REC_SEL(e, 0);
    ML_val_t	name = REC_SEL(REC_SEL(e, 1), 0);

    chatting("Uncaught exception %.*s with ", OBJ_LEN(name), PTR_MLtoC(name));

    if (! OBJ_isBOXED(val))
	chatting("%d\n", INT_MLtoC(val));
    else {
	int tag = OBJ_TAG(val);
	if ((tag == tag_string) || (tag == tag_embedded)) /* possible bug on reals */
	    chatting("\"%.*s\"\n", OBJ_LEN(val), PTR_MLtoC(val));
	else
	    chatting("<unknown>\n");
    }

    exit(1);

} /* end of uncaught */
