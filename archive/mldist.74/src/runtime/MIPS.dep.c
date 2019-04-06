/* MIPS.dep.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * MIPS dependent code for SML/NJ runtime kernel.
 */

#ifndef SGI
#include <mips/cpu.h>  /* for EXC_OV */
#else
#include <sys/sbd.h>  /* for EXC_OV */
#endif
#ifndef SGI
#include <syscall.h>
#endif SGI
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

extern int		saveregs[];
extern MLState_ptr      find_self();

/* ghandle:
 *    Handler for GC signals (SIGFPE).
 */
SIGH_RET_TYPE ghandle (sig, code, scp, addr)
    int		sig, code, addr;
    struct sigcontext *scp;
{
    MLState_ptr MLState = find_self();
    unsigned int	*pc = (unsigned int *)(scp->sc_pc);

#ifdef MP_DEBUG
    pchatting("[ghandle]\n");
#endif MP_DEBUG
    if (MLState->inML) {
	scp->sc_pc = (int)saveregs;
	MLState->amount = 0;
     /* there are three possible instruction sequences that can cause a GC fault:
      *
      *   pc:    add  $0,$23,$19
      *
      *   pc-4:  addi $21,$23,n
      *   pc:    add  $0,$21,$19
      *
      *   pc-12: lui  $at,hi16(n)
      *   pc-8:  addi $at,at,lo16(n)
      *   pc-4:  add  $21,$23,$at
      *   pc:    add  $0,$21,$19
      */
	if (pc[0] == 0x02f30020 /* add $0,$23,$19 */)
	    { /* skip */ }
	else if (pc[0] == 0x02d30020 /* add $0,$21,$19 */) {
	    if ((pc[-1] & 0xffff0000) == 0x22f50000 /* addi $21,$23,n */) {
	        MLState->amount = pc[-1] & 0x0000ffff;
		pc = (unsigned int *)(((int)pc) - 4);
	    } else {
	        MLState->amount = scp->sc_regs[1];
		pc = (unsigned int *)(((int)pc) - 12);
	      }
	}
	else {
	  /* this isn't a GC trap */
	    MLState->request = REQ_FAULT;
	    make_exn_code (MLState, sig, (code == 0) ? scp->sc_fpc_csr : code);
	    return;
	}

      /* Here we know this is a GC trap */

	MLState->ml_pc = PTR_CtoML(pc);
	if (MLState->handlerPending)
	    sig_setup (MLState);
	else
	    MLState->request = REQ_GC;
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
    MLState_ptr MLState = find_self();

    if (! (MLState->inML))
	die ("bogus signal in ML: (%d, %#x)\n", sig, code);

    MLState->request = REQ_FAULT;
    make_exn_code (MLState, sig, code);
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
