
/* signal.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Machine independent signal support.
 */
#ifdef SGI
#define _BSD_SIGNALS
#endif SGI
#include <sys/signal.h>
#include <setjmp.h>
#include "ml_os.h"
#include "ml_types.h"
#include "request.h"
#include "ml_signal.h"
#include "prim.h"
#include "ml_state.h"

int		inML = 0;	/* This flag is set when we are executing ML code.  */
int		handlerPending	/* This flag is set when a handler trap is pending, */
		    = 0;	/* and cleared when handler trap is occurs.	    */
int		inSigHandler	/* This flag is set when a handler trap occurs and  */
		    = 0;	/* is cleared when the ML handler returns.	    */
int		maskSignals = 0;/* When set, signals are masked.		    */
int		ioWaitFlag = 0; /* This flag is set when we are waiting for a	    */
				/* blocking I/O operation to be ready.		    */
jmp_buf		SysCallEnv;
int		NumPendingSigs	/* This is the total number of signals pending.	    */
		    = 0;

static char	unix2ml[32]	/* map UNIX signal codes to ML signal codes */
  = {
    ML_NOSIG,	  ML_SIGHUP,	ML_SIGINT,    ML_SIGQUIT,	/* 0-3 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 4-7 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 8-11 */
    ML_NOSIG,	  ML_NOSIG,	ML_SIGALRM,   ML_SIGTERM,	/* 12-15 */
#ifdef HPUX
    ML_SIGUSR1,	  ML_SIGUSR2,	ML_SIGCHLD,   ML_NOSIG,		/* 16-19 */
    ML_NOSIG,	  ML_NOSIG,	ML_SIGIO,     ML_SIGWINCH,	/* 20-23 */
    ML_NOSIG,	  ML_SIGTSTP,	ML_SIGCONT,   ML_NOSIG,		/* 24-27 */
    ML_NOSIG,	  ML_SIGURG,	ML_NOSIG,     ML_NOSIG,		/* 28-31 */
#else !HPUX
#ifdef SGI
    ML_SIGUSR1,	  ML_SIGUSR2,	ML_SIGCHLD,   ML_NOSIG,		/* 16-19 */
    ML_NOSIG,	  ML_SIGTSTP,	ML_NOSIG,     ML_SIGIO,		/* 20-23 */
    ML_SIGURG,	  ML_SIGWINCH,	ML_NOSIG,     ML_NOSIG,		/* 24-27 */
    ML_SIGCONT,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 28-31 */
#else !HPUX && !SGI
    ML_SIGURG,	  ML_NOSIG,	ML_SIGTSTP,   ML_SIGCONT,	/* 16-19 */
    ML_SIGCHLD,	  ML_NOSIG,	ML_NOSIG,     ML_SIGIO,		/* 20-23 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 24-27 */
    ML_SIGWINCH,  ML_NOSIG,	ML_SIGUSR1,   ML_SIGUSR2	/* 28-31 */
#endif
#endif
  };

static struct siginfo_t {	/* Info about the ML signals */
    char	    unix_code;	    /* the unix signal code of this signal */
    char	    state;	    /* the state of this signal. */
} siginfo[NUM_ML_SIGS] =
{
    { SIGHUP,	    ML_SIG_DISABLED },
    { SIGINT,	    ML_SIG_DISABLED },
    { SIGQUIT,	    ML_SIG_DISABLED },
    { SIGALRM,	    ML_SIG_DISABLED },
    { SIGTERM,	    ML_SIG_DISABLED },
    { SIGURG,	    ML_SIG_DISABLED },
    { SIGCHLD,	    ML_SIG_DISABLED },
    { SIGIO,	    ML_SIG_DISABLED },
    { SIGWINCH,	    ML_SIG_DISABLED },
    { SIGUSR1,	    ML_SIG_DISABLED },
    { SIGUSR2,	    ML_SIG_DISABLED },
    { SIGTSTP,	    ML_SIG_DISABLED },
    { SIGCONT,	    ML_SIG_DISABLED },
    { SIG_NOT_UNIX, ML_SIG_DISABLED },	/* ML_SIGGC (garbage collection) */
};


static int	SigTbl[NUM_ML_SIGS];/* The number of pending signals of each type */
static int	SigCode = 0;	    /* The signal to be passed to ML */
static int	SigCount;	    /* The number of pending signals of type sigCode */

#if (!defined(MIPS) && !defined(C))
int		saved_pc;	    /* saved_pc to restore in adjust_limit */
#endif


/* sig_setup:
 * This (machine independent) function is called by the (machine dependent) function
 * ghandle to set things up for handling a signal in ML.  Since this is called by
 * ghandle, it is atomic w.r.t. signals.
 */
void sig_setup ()
{
    register int    nextSig = SigCode;

  /* determine which signal to handle */
    do {
	if ((nextSig += 1) == NUM_ML_SIGS)
	    nextSig = 0;
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
 * The C signal handler for signals that are to be passed to the ML handler.
 */
SIGH_RET_TYPE sig_handler (sig, code, scp)
    int		    sig, code;
    struct sigcontext *scp;
{
    extern int	    adjust_limit[];

  /* record the signal */
    NumPendingSigs++;
    SigTbl[unix2ml[sig]]++;

    if (!maskSignals) {
	if (ioWaitFlag) {
	  /* We were waiting for a blocking I/O operation when the signal occurred,
	   * so longjmp out of the operation (see io_wait() in "cfuns.c"). */
	    _longjmp (SysCallEnv, 1);
	}
	else if (inML && (! handlerPending) && (! inSigHandler)) {
#ifndef C
	    register int    pc = scp->sc_pc;
#endif

	    handlerPending	= 1;
#ifdef MIPS
      /* adjust the heap limit directly on the MIPS */
	    scp->sc_regs[19] = 0x7fffffff; 
#else
#ifdef C
	  plimit = 0;
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
#endif
	}
    }

} /* end of sig_handler */


/* handlesys:
 * The handler for SIGSYS.
 */
SIGH_RET_TYPE handlesys ()
{
  /* Long jump to back to ml_syscall. */
    _longjmp (SysCallEnv, 1);
}


/* handleprof:
 * The handler for profile signals.
 */
SIGH_RET_TYPE handleprof ()
{
   extern ML_val_t current0[];
   ML_val_t cur_barray = current0[1];
   cur_barray[1] = (unsigned int)INT_incr(cur_barray[1],1);
}


/* gcsignal:
 * Record a garbage collection signal (if enabled).  Return true, if a signal
 * was recorded.
 */
int gcsignal ()
{
    if (siginfo[ML_SIGGC].state == ML_SIG_ENABLED) {
	NumPendingSigs++;
	SigTbl[ML_SIGGC]++;
	return 1;
    }
    else
	return 0;

} /* end of gcsignal */



#define SIGMASK		0xffffffff	/* mask all signals */

/* setup_signals:
 */
void setup_signals ()
{
    int			ml_sig, sig;

  /* set up the ML signals according to their state */
    for (ml_sig = 0;  ml_sig < NUM_ML_SIGS;  ml_sig++) {
	sig = siginfo[ml_sig].unix_code;
	if (sig != SIG_NOT_UNIX)
	    switch (siginfo[ml_sig].state) {
	      case ML_SIG_DISABLED: SETSIG (sig, SIG_DFL, SIGMASK); break;
	      case ML_SIG_ENABLED: SETSIG (sig, sig_handler, SIGMASK); break;
	    }
    }

    SETSIG (SIGPIPE, SIG_IGN, 0);  /* will force an EPIPE error instead */
    SETSIG (SIGSYS, handlesys, SIGMASK);
    SETSIG (SIGVTALRM, handleprof, SIGMASK);

  /* setup the machine dependent signals. */
    setup_mach_sigs (SIGMASK);

} /* end of setup_signals */


/* enable_sig:
 */
void enable_sig (ml_sig, enable)
    int		    ml_sig, enable;
{
    int		    sig;

    siginfo[ml_sig].state = (enable ? ML_SIG_ENABLED : ML_SIG_DISABLED);
    if ((sig = siginfo[ml_sig].unix_code) != SIG_NOT_UNIX) {
	if (enable) {
	    SETSIG (sig, sig_handler, SIGMASK);
	} else {
	    SETSIG (sig, SIG_IGN, SIGMASK);
	}
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
 *
 * Heap layout.
 * At least 4K avail. heap assumed.
 *
 * +------------------+----+-+-+-+---+~+---------~
 * | STRING floatregs |desc|1|2|3|   |B| live regs
 * +------------------+----+-+-+-+-|-+~+---------~
 *         ^                       |
 *         |_______________________|
 * 
 */
ML_val_t make_ml_sigh_arg ()
{
    ML_val_t	resume_c, arg;
    int		i, n, mask;
    int 	*fpregs;

    /* 
     * save floating point registers.
     */
    savefpregs();
    fpregs = ((int *)(MLState->ml_allocptr)) + 1;
    MLState->ml_allocptr += (NSAVED_FPREGS*2 + 1) * sizeof(int);
    /* 
     * allocate the closure for resume
     */
    ML_alloc_write (1, PTR_CtoML(sigh_resume));
    ML_alloc_write (2, MLState->ml_pc);
    ML_alloc_write (3, MLState->ml_exncont);
    ML_alloc_write (4, fpregs);
    n=5; 
    /*
     * note that varptr and (if defined) icount are 
     * shared between mainline ML code and exn handler, so
     * are not saved/restored here.
     */

#if defined(BASE_INDX)
    ML_alloc_write (n, MLState->ml_baseptr);
    n++;
#endif
#if defined(C)
      mask = Cmask;
      ML_alloc_write (n,INT_CtoML(Cmask));
      n++;
#else !defined(C)   
    mask = ((int *)PTR_MLtoC(MLState->ml_pc))[-2];
#endif
    for (i = 0;  mask != 0;  i++) {
	if (mask & 0x1) {
	    ML_alloc_write (n, MLState->ml_roots[ArgRegMap[i]]);
	    n++;
	}
	mask >>= 1;
    }
    ML_alloc_write (0, MAKE_DESC(n-1, tag_record));
    resume_c = ML_alloc(n-1);

  /* allocate the ML signal handler's argument record */
    REC_ALLOC3(arg, INT_CtoML(SigCode), INT_CtoML(SigCount), resume_c);

    return arg;

} /* end of make_ml_sigh_arg. */


/* load_resume_state:
 * Load the ML state vector with the state preserved in a resumption continuation
 * built by make_ml_sigh_arg.
 */
void load_resume_state ()
{
    register MLState_t *msp = MLState;
    register ML_val_t *p = (ML_val_t *)(PTR_MLtoC(msp->ml_cont));
    register int    i, n, mask;

    msp->ml_pc = p[1];
    msp->ml_exncont = p[2];
    restorefpregs(p[3]);
    n = 4;
#if defined(BASE_INDX)
    MLState->ml_baseptr = p[n];
    n++;
#endif
#if defined(C)
    mask = ((unsigned int) p[n]) >> 1;
    n++;
#else
    mask = ((int *)PTR_MLtoC(msp->ml_pc))[-2];
#endif
    for (i = 0;  mask != 0;  i++) {
	if (mask & 0x1) {
	    msp->ml_roots[ArgRegMap[i]] = p[n++];
	}
	mask >>= 1;
    }

} /* end of load_resume_state. */
