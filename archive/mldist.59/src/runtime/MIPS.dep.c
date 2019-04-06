/* MIPS.dep.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 *    MIPS dependent code for SML/NJ runtime kernel.
 */

#include <mips/cpu.h>  /* for EXC_OV */
#include <syscall.h>
#include <signal.h>
#include "ml_os.h"
#include "ml_state.h"
#include "request.h"

#include "tags.h"
#include "ml_types.h"
#ifdef MIPSEL
MACHINEID("mipsl");
#else
MACHINEID("mipsb");
#endif

extern int		saveregs[], handlerPending, inML;

/* ghandle:
 *    Handler for GC signals (SIGFPE).
 */
SIGH_RET_TYPE ghandle (sig, code, scp, addr)
    int		sig, code, addr;
    struct sigcontext *scp;
{
    unsigned int	*pc = (unsigned int *)(scp->sc_pc);

    if (inML) {
	scp->sc_pc = (int)saveregs;

     /* there are three possible instruction sequences that can cause a GC fault:
      *
      *   pc:    add  $0,$23,$19
      *
      *   pc-4:  addi $20,$23,n
      *   pc:    add  $0,$20,$19
      *
      *   pc-12: lui  $at,hi16(n)
      *   pc-8:  addi $at,at,lo16(n)
      *   pc-4:  add  $20,$23,$at
      *   pc:    add  $0,$20,$19
      */
	if (pc[0] == 0x02f30020 /* add $0,$23,$19 */)
	    { /* skip */ }
	else if (pc[0] == 0x02930020 /* add $0,$20,$19 */) {
	    if ((pc[-1] & 0xffff0000) == 0x22970000 /* addi $20,$23,n */)
		pc = (unsigned int *)(((int)pc) - 4);
	    else
		pc = (unsigned int *)(((int)pc) - 12);
	}
	else {
	  /* this isn't a GC trap */
	    request = REQ_FAULT;
	    make_exn_code (sig, (code == 0) ? scp->sc_fpc_csr : code);
	    return;
	}

      /* Here we know this is a GC trap */

	MLState->ml_pc = PTR_CtoML(pc);
	if (handlerPending)
	    sig_setup ();
	else
	    request = REQ_GC;
    }
    else
	die ("bogus signal %d (%#x) not in ML\n", sig, code);

} /* end of ghandle */


/* trap_handler:
 * Handle SIGTRAP (integer overflow & zero divide).
 */
static SIGH_RET_TYPE trap_handler (sig, code, scp, addr)
    int		sig, code, addr;
    struct sigcontext *scp;
{
    if (! inML)
	die ("bogus signal in ML: (%d, %#x)\n", sig, code);

    request = REQ_FAULT;
    make_exn_code (sig, code);
    scp->sc_pc = (int)saveregs;

} /* end of trap_handler */


/* setup_mach_sigs:
 * Setup tha Mips dependent signals (for GC, FPE, ...).
 */
void setup_mach_sigs (mask)
{
    SETSIG(SIGFPE, ghandle, mask);
    SETSIG(SIGTRAP, trap_handler, mask);

    set_fsr();  /* enable floating-point exceptions */
}
