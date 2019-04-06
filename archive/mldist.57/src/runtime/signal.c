/* signal.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Machine independent signal support.
 */

#include <sys/signal.h>
#include <setjmp.h>
#include "ml_os.h"
#include "ml_types.h"
#include "request.h"

int		inML = 0;	/* This flag is set when we are executing ML code.  */
int		handlerPending	/* This flag is set when a handler trap is pending, */
		    = 0;	/* and cleared when handler trap is occurs.	    */
int		inSigHandler	/* This flag is set when a handler trap occurs and  */
		    = 0;	/* is cleared when the ML handler returns.	    */
int		ioWaitFlag = 0; /* This flag is set when we are waiting for a	    */
				/* blocking I/O operation to be ready.		    */
jmp_buf		IOWaitEnv;
int		NumPendingSigs	/* This is the total number of signals pending.	    */
		    = 0;

static char	sigEnabled[32]	    /* The state of the signals. */
  = {				    /*   -1 ==> default action (not used by ML) */
				    /*    0 ==> ignore (disabled in ML) */
				    /*    1 ==> enabled by ML */
    -1,  0,  0,   0, -1, -1, -1, -1,	/* unused HUP INT QUIT ILL TRAP ABRT EMT */
    -1, -1, -1,  -1,  0,  0,  0,  0,	/* FPE KILL BUS SEGV SYS PIPE ALRM TERM */
     0, -1, -1,  -1,  0, -1, -1,  0,	/* URG STOP TSTP CONT CHLD TTIN TTOU IO */
    -1, -1, -1,  -1,  0, -1,  0,  0	/* XCPU XFSZ VTALRM PROF WINCH LOST USR1 USR2 */
  };

static int	SigTbl[32];	    /* The number of pending signals of each type */
static int	SigCode = 1;	    /* The signal to be passed to ML */
static int	SigCount;	    /* The number of pending signals of type sigCode */

#ifndef MIPS
int		saved_pc;	    /* saved_pc to restore in adjust_limit */
#endif


/* sig_setup:
 * This (system independent) function is called by the (system dependent) function
 * ghandle to set things up for handling a signal in ML.  Since this is called by
 * ghandle, it is atomic w.r.t. signals.
 */
void sig_setup ()
{
    register int    nextSig = SigCode;

  /* determine which signal to handle */
    do {
	nextSig = (nextSig + 1) & 0x1f;
    } while (SigTbl[nextSig] == 0);

  /* record the signal and count */
    SigCode	    = nextSig;
    SigCount	    = SigTbl[nextSig];
    NumPendingSigs  -= SigCount;
    SigTbl[nextSig] = 0;

    handlerPending  = 0;
    inSigHandler    = 1;
    request	    = REQ_SIGNAL;

} /* end of sig_setup */


/* sig_handler:
 * The C signal handler for signals that are passed to the ML handler.
 */
SIGH_RET_TYPE sig_handler (sig, code, scp)
    int		    sig, code;
    struct sigcontext *scp;
{
    extern int	    adjust_limit[];

  /* record the signal */
    NumPendingSigs++;
    SigTbl[sig]++;

    if (ioWaitFlag) {
      /* We were waiting for a blocking I/O operation when the signal occurred,
       * so longjmp out of the operation (see io_wait() in "cfuns.c"). */
	_longjmp (IOWaitEnv, 1);
    }
    else if (inML && (! handlerPending) && (! inSigHandler)) {
	register int    pc = scp->sc_pc;

	handlerPending	= 1;
#ifdef MIPS
      /* adjust the heap limit directly on the MIPS */
	scp->sc_regs[19] = 0x7fffffff; 
#else
	scp->sc_pc	= (int)adjust_limit;
# ifdef SPARC
	if (pc+4 != scp->sc_npc) {
	  /* the pc is pointing to a delay slot, so back-up to the branch.
	   * Note: this relies on the assumption that the branch doesn't
	   * have a side-effect that interferes with the delay slot. */
	    pc -= 4;
	}
	scp->sc_npc	= ((int)adjust_limit)+4;
# endif
	saved_pc	= pc;
#endif
    }

} /* end of sig_handler */


/* handleprof:
 * The handler for profile signals.
 */
SIGH_RET_TYPE handleprof ()
{
   extern ML_val_t current0[];

   INT_incr(current0[1], 1);
}


#define SIGMASK		0xffffffff	/* mask all signals */

/* setup_signals:
 */
void setup_signals ()
{
    int			sig;

  /* set up the ML signals according to their state */
    for (sig = 1;  sig < 32;  sig++) {
	switch (sigEnabled[sig]) {
	  case -1: break;
	  /*case 0: SETSIG (sig, SIG_IGN, SIGMASK); break;*/
	  case 1: SETSIG (sig, sig_handler, SIGMASK); break;
	}
    }

    SETSIG (SIGVTALRM, handleprof, SIGMASK);

  /* setup the machine dependent signals. */
    setup_mach_sigs (SIGMASK);

} /* end of setup_signals */


/* enable_sig:
 */
void enable_sig (sig, enable)
    int		    sig, enable;
{
    if (enable) {
	sigEnabled[sig] = 1;
	SETSIG (sig, sig_handler, SIGMASK);
    }
    else {
	sigEnabled[sig] = 0;
	SETSIG (sig, SIG_IGN, SIGMASK);
    }

} /* end of enable_sig */


/* make_ml_sigh_arg:
 * Build the argument record for the ML signal handler.  It has the type
 *
 *   val sigHandler : (int * int * unit cont) -> 'a
 *
 * The first arg is the signal code, the second is the signal count and the
 * third is the resumption continuation.  The ML signal handler should never
 * return.
 */
ML_val_t make_ml_sigh_arg ()
{
    ML_val_t	resume_c, resume_k, arg;
    int		i;
    extern int	sigh_resume[];

  /* allocate the closure for resume */
    ML_alloc_write (0, MAKE_DESC(NROOTS+1, tag_record));
    ML_alloc_write (1, PTR_CtoML(sigh_resume));
    for (i = 0;  i < NROOTS;  i++) {
	ML_alloc_write (i+2, MLState->ml_roots[i]);
    }
    resume_c = ML_alloc(NROOTS+1);
  /* allocate the resume continuation */
    ML_alloc_write (0, MAKE_DESC(2, tag_record));
    ML_alloc_write (1, resume_c);
    ML_alloc_write (2, MLState->ml_exncont);
    resume_k = ML_alloc(2);
  /* allocate the ML signal handler's argument record */
    ML_alloc_write (0, MAKE_DESC(3, tag_record));
    ML_alloc_write (1, INT_CtoML(SigCode));
    ML_alloc_write (2, INT_CtoML(SigCount));
    ML_alloc_write (3, resume_k);  /* : unit cont */
    arg = ML_alloc (3);

    return arg;

} /* end of make_ml_sigh_arg. */
