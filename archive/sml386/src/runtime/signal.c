/* signal.c   (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Aletered 20 Dec. 1991 by:    Yngvi S. Guttesen
 *                              Department of Computer Science
 *                              The Technical University of Denmark
 *                              DK-2800 Lyngby
 *
 * Signals are not used in the Windows version !
 *
 * Machine independent signal support.
 */

#include "sml.h"
#include "signal.h"
#include "ml_os.h"
#include "ml_types.h"
#include "request.h"
#include "ml_signa.h"

int		inML = 0;	/* This flag is set when we are executing ML code.  */
int		handlerPending	/* This flag is set when a handler trap is pending, */
		    = 0;	/* and cleared when handler trap is occurs.	    */
int		inSigHandler	/* This flag is set when a handler trap occurs and  */
		    = 0;	/* is cleared when the ML handler returns.	    */
int		maskSignals = 0;/* When set, signals are masked.		    */
int		ioWaitFlag = 0; /* This flag is set when we are waiting for a	    */
				/* blocking I/O operation to be ready.		    */
int		NumPendingSigs	/* This is the total number of signals pending.	    */
		    = 0;

static char	unix2ml[32]	/* map UNIX signal codes to ML signal codes */
  = {
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 0-3 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 4-7 */
    ML_NOSIG,	  ML_NOSIG,	ML_NOSIG,     ML_NOSIG,		/* 8-11 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 12-15 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 16-19 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 20-23 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 24-27 */
    ML_NOSIG,     ML_NOSIG,     ML_NOSIG,     ML_NOSIG,         /* 28-31 */
  };

static struct siginfo_t {	/* Info about the ML signals */
    char	    unix_code;	    /* the unix signal code of this signal */
    char	    state;	    /* the state of this signal. */
} siginfo[NUM_ML_SIGS] =
{
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { 00,           ML_SIG_DISABLED },
    { SIG_NOT_UNIX, ML_SIG_DISABLED },	/* ML_SIGGC (garbage collection) */
};


static int	SigTbl[NUM_ML_SIGS];/* The number of pending signals of each type */
static int	SigCode = 0;	    /* The signal to be passed to ML */
static int	SigCount;	    /* The number of pending signals of type sigCode */

#ifndef MIPS
int		saved_pc;	    /* saved_pc to restore in adjust_limit */
#endif



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
  /* setup the machine dependent signals. */
    setup_mach_sigs (SIGMASK);

} /* end of setup_signals */


/* enable_sig:
 */
void enable_sig (ml_sig, enable)
    int		    ml_sig, enable;
{

} /* end of enable_sig */
