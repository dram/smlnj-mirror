/* VAX.dep.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * VAX dependent code for the SML/NJ runtime kernel.
 */

#include <sys/signal.h>
#include "ml_os.h"
#include "ml_state.h"
#include "request.h"

#include "tags.h"
#include "ml_types.h"
MACHINEID("vax");

extern MLState_ptr find_self();

/* ghandle:
 */
SIGH_RET_TYPE ghandle (sig, code, scp, addr)
    int			sig, code, addr;
    struct sigcontext	*scp;
{
    MLState_ptr MLState = find_self();
    register unsigned int *pc = (unsigned int *)(scp->sc_pc);
    extern int saveregs[];

    scp->sc_pc     = (int)saveregs; 

  /* there are two possible instruction sequences that can trigger a GC fault:
   *
   *   pc-4:  addl3 r12,r8,r9
   *
   *   pc-11: addl3 r12,$n,r9  (using 5-byte addressing mode)
   *   pc-3:  addl2 r8,r9
   */
    if (code==FPE_INTOVF_TRAP && MLState->inML)
       {if (pc[-1] == 0x59585cc1 /* addl3 r12,r8,r9 */)
	  pc = (unsigned int *)((int)pc - 4);
        else if ((pc[-1] & 0xffffff00) == 0x5958c000 /* addl2 r8,r9 */)
	  pc = (unsigned int *)((int)pc - 11);
        else {MLState->request = REQ_FAULT;
	      make_exn_code (MLState, sig, code);
	      return;
	     }
	/* Here, this is a GC related signal */
	MLState->ml_pc = PTR_CtoML(pc);
	if (MLState->handlerPending)
	  sig_setup(MLState);
	else
	  MLState->request = REQ_GC;
       }
    else if (MLState->inML)
             {MLState->request = REQ_FAULT;
	      make_exn_code (MLState, sig, code);
	      return;
	     }
    else
	die ("bogus signal not in ML: (%d, %#x)\n", signal, code);
} /* end of ghandle */

/* setup_mach_sigs:
 * Setup tha VAX dependent signals (for GC, FPE, ...).
 */
void setup_mach_sigs (mask)
{
    SETSIG(SIGFPE, ghandle, mask);
}
