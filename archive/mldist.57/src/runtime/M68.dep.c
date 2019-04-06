/* M68.dep.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 *    M68 dependent code for the SML/NJ runtime kernel.
 */

#include <signal.h>
#include "ml_os.h"
#include "ml_state.h"
#include "request.h"

#include "tags.h"
#include "ml_types.h"
MACHINEID("m68");

/* ghandle:
 */
SIGH_RET_TYPE ghandle (sig, code, scp)
    int			sig, code;
    struct sigcontext	*scp;
{
    register unsigned short *pc = (unsigned short *)(scp->sc_pc);
    extern int		saveregs[], handlerPending, inML;

    if ((sig == GC_SIG) && (code == GC_CODE) && (pc[-1] == 0x5bfc /*trapmi*/))
  /* Note: versions of sony_news before Sept 1989 had pc pointer BEFORE
   * trap instruction, not after. */
    {
      /* GC related fault.  Adjust the PC to be a valid root.  There are three
       * possible code sequences that can trigger GC (see m68/m68.sml).
       *
       *   pc-4:  clrl   d0		(in M68.prim.s only)
       *   pc-2:  trapmi
       *
       *   pc-4:  cmpl   a6,d5
       *   pc-2:  trapmi
       *
       *   pc-12: movl   a6,d4
       *   pc-10: addl   #n,d4
       *   pc-4:  cmpl   d4,d5
       *   pc-2:  trapmi
       */
	if (pc[-2] == 0xba84 /* cmpl d4,d5 */)
	    pc = (unsigned short *)(((int)pc) - 12);
	else
	    pc = (unsigned short *)(((int)pc) - 4);

	if (handlerPending) {
	    sig_setup();
	}
	else
	    request = REQ_GC;
    }
    else if (inML) {
	make_exn_code (sig, code);
	request = REQ_FAULT;
    }
    else
	die ("bogus signal not in ML: (%d, %#x)\n", signal, code);

    MLState->ml_pc = (int)pc;
    scp->sc_pc     = (int)saveregs; 
}


/* setup_mach_sigs:
 * Setup tha M68 dependent signals (for GC, FPE, ...).
 */
void setup_mach_sigs (mask)
{
    SETSIG (SIGFPE, ghandle, mask)
    if (GC_SIG != SIGFPE)
        SETSIG (GC_SIG, ghandle, mask)
    fpenable();
}
