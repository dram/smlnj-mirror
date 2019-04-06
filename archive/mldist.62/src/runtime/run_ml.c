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

/* This table maps the register numbers of the code generator to the proper
 * indices of the root vector.  The order of entries in this table must
 * respect both the MLState vector layout and the order of the miscregs in
 * the C-machine implementation.  The pc and exncont are not included.
 */
int		ArgRegMap[N_ARG_REGS] = {
	CLOSURE_INDX, ARG_INDX, CONT_INDX,	/* the standard arg registers */
#ifdef SPARC
	/* misc. regs = %g1-%g3, %l0-%l7, %i4 */
	 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 5
#endif
#ifdef M68
	 4, 5
#endif
#ifdef VAX
	 4, 5, 6, 7, 8
#endif
#ifdef MIPS
	 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18
#endif
#ifdef NS32
	 ??
#endif
    };

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
	MLState->ml_roots[i] = ML_unit;

} /* end of init_mlstate. */


/* run_ml:
 */
static void run_ml ()
{
    register MLState_t *msp = MLState;

    while (1) {
	restoreregs ();

	switch (request) {
	  case REQ_RETURN:
#ifdef GCMON
	    dumpMon();
#endif GCMON
	    return;

	  case REQ_EXN: /* an uncaught exception */
	    uncaught (msp->ml_arg);
	    return;

	  case REQ_FAULT: { /* a hardware fault */
		extern ML_val_t	fault_exn;
		raise_ml_exn (fault_exn);
	    } break;

	  case REQ_GC:
	    callgc0 (CAUSE_GC, ((int *)PTR_MLtoC(MLState->ml_pc))[-2]);
/*	    callgc0 (CAUSE_GC, ALL_ARGS_MASK);*/
	    gcsignal();
	    break;

	  case REQ_CALLC: {
		int	    (*f)();
		ML_val_t    arg;

		msp->ml_closure = msp->ml_cont;
		msp->ml_pc	= CODE_ADDR(msp->ml_cont);

		check_heap (32768);

		f   = (int (*)())REC_SELPTR(msp->ml_arg, 0);
		arg = REC_SEL(msp->ml_arg, 1);
		(*f)(arg);
	    } break;

	  case REQ_SIGNAL: {
		extern ML_val_t make_ml_sigh_arg();
		extern ML_val_t sigh_return_c[], sighandler0[], handle_c[];

		msp->ml_arg     = make_ml_sigh_arg();
		msp->ml_cont    = PTR_CtoML(sigh_return_c);
		msp->ml_exncont = PTR_CtoML(handle_c);
		msp->ml_closure = sighandler0[1];
		msp->ml_pc      = CODE_ADDR(msp->ml_closure);
	    } break;

	  case REQ_SIG_RETURN: {
	      /* Handle the return from the ML signal handler.  The ml_arg
	       * contains the unit resumption continuation (2-arg fn).
	       */
		extern int	inSigHandler;
		register ML_val_t kont = msp->ml_arg;

	      /* throw to the cont that we have received as an argument. */
		msp->ml_arg     = ML_unit;
		msp->ml_closure = ML_unit;
		msp->ml_cont    = kont;
		msp->ml_pc      = CODE_ADDR(kont);
		msp->ml_exncont = ML_unit;
	      /* Note that we have finished handling the signal */
		inSigHandler	    = 0;
	    } break;

	  case REQ_SIG_RESUME: {
		register int    i;
		register ML_val_t *p = (ML_val_t *)(PTR_MLtoC(msp->ml_cont)+1);

	      /* restore the state from the resumption closure */
		for (i = 0;  i < NROOTS;  i++)
		    msp->ml_roots[i] = *p++;
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
    register int	i;
    register ML_val_t	kont = MLState->ml_exncont;

  /* clear obsolete roots to reduce floating garbage */
    for (i = 0;  i < NROOTS;  i++)
	MLState->ml_roots[i] = ML_unit;

    MLState->ml_arg	= exn;
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
    extern ML_val_t handle_c[], return_c[];

  /* clear the ML root registers */
    for (i = 0;  i < NROOTS;  i++)
	MLState->ml_roots[i] = ML_unit;

  /* initialize the calling context */
    MLState->ml_exncont	= PTR_CtoML(handle_c);
    MLState->ml_arg	= arg;
    MLState->ml_cont	= PTR_CtoML(return_c);
    MLState->ml_closure	= f;
    MLState->ml_pc	= CODE_ADDR(f);

    run_ml();

    return MLState->ml_arg;

} /* end of apply_ml_fn */


/* restart_ml:
 * Restart an exported ML system.
 */
void restart_ml ()
{
    restart_gc ();
    setup_signals ();

    MLState->ml_arg = ML_true;

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
