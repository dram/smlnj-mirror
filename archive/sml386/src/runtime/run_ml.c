/* run_ml.c     (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * These are routines for handling the ML-C interface.
 *
 * Altered 20 Dec. 1991 by:    Yngvi S. Guttesen
 *                             Department of Computer Science
 *                             The Technical University of Denmark
 *                             DK-2800 Lyngby
 */

#include "request.h"
#include "sml.h"
#include "ml_state.h"
#include "ml_types.h"
#include "cause.h"


/* This table maps the register numbers of the code generator to the proper
 * indices of the root vector.  The order of entries in this table must
 * respect both the MLState vector layout and the order of the miscregs in
 * the C-machine implementation.  The pc and exncont are not included.
 */
int		ArgRegMap[N_ARG_REGS] = {
	CLOSURE_INDX, ARG_INDX, CONT_INDX,	/* the standard arg registers */
         4, 5, 6, 7                             /* misc. regs.                */
    };


int             request;
ML_val_t        fault_exn;

extern MLState_t _far MLState_a ;
MLState_t _based(runtime_seg) *MLState=&MLState_a;

void raise_ml_exn ();
static void uncaught ();

extern void check_heap (long);
extern void callgc0 (long,long);
extern void restoreregs(void);


/* init_mlstate:
 * Initialize the ML state vector (except the allocation and limit pointers).
 */
void init_mlstate ()
{
    int		i;

    MLState->ml_storeptr = STORLST_nil;
    for (i = 0;  i < NROOTS;  i++)
	MLState->ml_roots[i] = ML_unit;

} /* end of init_mlstate. */


/* run_ml:
 */
static void run_ml ()
{
    MLState_t _based(runtime_seg) *msp = MLState;
    extern CATCHBUF CatchBuf;
    extern int exit_sml;
    while (1) {

	if (exit_sml)
	    Throw((LPCATCHBUF) &CatchBuf, 1) ;

	msg_loop();
	restoreregs();
	msg_loop();

        switch (request) {

          case REQ_EXN:
                uncaught(msp->ml_arg);
                return;

          case REQ_FAULT:
                raise_ml_exn(fault_exn);
                break;

	  case REQ_RETURN:
                return;

          case REQ_GC1:
                callgc0 (CAUSE_GC, REC_SEL(MLState->ml_pc,-2));
                break;
          case REQ_GC2:
                callgc0 (CAUSE_GC, REC_SEL(MLState->ml_pc,-2));
                break;
          case REQ_GC3:
                callgc0 (CAUSE_GC, REC_SEL(MLState->ml_pc,-2));
                break;

	  case REQ_CALLC: {
                long        (*f)();
                long        arg;

		msp->ml_closure = msp->ml_cont;
		msp->ml_pc	= CODE_ADDR(msp->ml_cont);

		check_heap (32768);

                f   = (long (*)())REC_SELPTR(msp->ml_arg, 0);
		arg = REC_SEL(msp->ml_arg, 1);
		(*f)(arg);
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
    MLState->ml_exncont = PTR_CtoML(LOWORD(handle_c));
    MLState->ml_arg	= arg;
    MLState->ml_cont    = PTR_CtoML(LOWORD(return_c));
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
/*    setup_signals (); ########################################### */

    MLState->ml_arg = ML_true;

    run_ml ();


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
