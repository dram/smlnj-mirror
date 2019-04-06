/* SPARC.dep.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * AUTHOR:  John Reppy
 *	    Cornell University
 *	    Ithaca, NY 14853
 *	    jhr@cs.cornell.edu
 *
 *    SPARC dependent code for SML/NJ runtime kernel.
 */

#include <sys/signal.h>
#include "ml_os.h"
#include "ml_state.h"
#include "request.h"

extern int	saveregs[], handlerPending, inML;

#include "tags.h"
#include "ml_types.h"
MACHINEID("sparc");

/* ghandle:
 *    Handler for GC signals.
 */
SIGH_RET_TYPE ghandle (sig, code, scp, addr)
    int sig, code, addr;
    struct sigcontext *scp;
{
    register unsigned int *pc = (unsigned int *)(scp->sc_pc);

    if (inML && (code == EMT_TAG)) {
      /* GC related fault.  Adjust the PC to be a valid root.  There are three
       * possible code sequences that may trigger GC (where n is an integer
       * and %r is a temp register).
       *
       *   taddcctv %g6,%g4,%g0	    1000 0001 0001 0001 1000 0000 0000 0100
       *
       *   add      %g4,n,%r	    10rr rrr0 0000 0001 001n nnnn nnnn nnnn
       *   taddcctv %g6,%r,%g0	    1000 0001 0001 0001 1000 0000 000r rrrr
       *
       *   sethi    %hi(n),%r	    00rr rrr1 00nn nnnn nnnn nnnn nnnn nnnn
       *   or       %r,%lo(n),%r    10rr rrr0 0001 0rrr rr1n nnnn nnnn nnnn
       *   add      %g4,%r,%r	    10rr rrr0 0000 0001 0000 0000 000r rrrr
       *   taddcctv %g6,%r,%g0	    1000 0001 0001 0001 1000 0000 000r rrrr
       */
	if (((int)pc)+4 != scp->sc_npc)
	    die ("SIGEMT not related to gc (bogus npc)\n");
	else if (pc[0] != 0x81118004 /* taddcctv %g6,%g4,%g0 */) {
	    if ((pc[0] & 0xffffffe0) != 0x81118000 /* taddcctv %g6,%r,%g0 */)
		die ("SIGEMT not related to gc (bogus test)\n");
	    else if (pc[-1] & 0x00002000 /* immed flag */)
		pc = (unsigned int *)((int)pc - 4);
	    else
		pc = (unsigned int *)((int)pc - 12);
	}
	if (handlerPending) {
	    sig_setup();
	}
	else
	    request = REQ_GC;
    }
    else
	die ("SIGEMT not related to gc (%s)\n", inML ? "bogus code" : "not in ML");

    MLState->ml_pc = PTR_CtoML(pc);
    scp->sc_pc     = (int)saveregs;
    scp->sc_npc    = ((int)saveregs)+4;

} /* end of ghandle */


/* fpe_handler:
 * Handle SIGFPE signals (integer and floating-point exceptions).
 */
SIGH_RET_TYPE fpe_handler (sig, code, scp)
    int		    sig, code;
    struct sigcontext *scp;
{
    if (! inML)
	die ("bogus signal not in ML: (%d, %#x)\n", sig, code);

    make_exn_code (sig, code);
    request	= REQ_FAULT;
    scp->sc_pc  = (int)saveregs;
    scp->sc_npc = ((int)saveregs)+4;

} /* end of fpe_handler */


/* setup_mach_sigs:
 *   Install the SPARC dependent signal handlers.
 */
void setup_mach_sigs (mask)
    int		    mask;
{
    SETSIG (SIGEMT, ghandle, mask);
    SETSIG (SIGFPE, fpe_handler, mask);
    SETSIG (SIGILL, fpe_handler, mask);  /* Sun4's running Mach */

    set_fsr (0x0f000000); /* enable FP exceptions NV, OF, UF & DZ */
}
