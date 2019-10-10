/*! \file unix-fault.c
 *
 * Common code for handling arithmetic traps and signals.
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common code for handling arithmetic traps.
 */

#if defined(__CYGWIN32__)

#include "cygwin-fault.c"

#else

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-globals.h"

/* this is temporary */
#define SELF_VPROC	(VProc[0])


/* local routines */
PVT SigReturn_t FaultHandler (/* int sig, SigInfo_t code, SigContext_t *scp */);


/* InitFaultHandlers:
 */
void InitFaultHandlers (ml_state_t *msp)
{

  /** Set up the Div and Overflow faults **/
#ifdef SIG_FAULT1
    SIG_SetHandler (SIG_FAULT1, FaultHandler);
#endif
#ifdef SIG_FAULT2
    SIG_SetHandler (SIG_FAULT2, FaultHandler);
#endif

  /** Initialize the floating-point unit **/
    SIG_InitFPE ();

} /* end of InitFaultHandlers */


/* FaultHandler:
 *
 * Handle arithmetic faults. Note that since floating-point arithmetic
 * is non-trapping in SML and since the compiler generates code to
 * explicitly test for division by zero, and arithmetic trap should be
 * mapped to Overflow.
 */
#if defined(HAS_POSIX_SIGS) && defined(HAS_UCONTEXT)

PVT SigReturn_t FaultHandler (int signal, siginfo_t *si, void *c)
{
    ucontext_t	    *scp = (ucontext_t *)c;
    ml_state_t	    *msp = SELF_VPROC->vp_state;
    extern Word_t   request_fault[];
    int		    code = SIG_GetCode(si, scp);

#ifdef SIGNAL_DEBUG
    SayDebug ("Fault handler: sig = %d, code = %d, inML = %d\n",
	signal, code, SELF_VPROC->vp_inMLFlag);
#endif

    if (! SELF_VPROC->vp_inMLFlag) {
	Die ("bogus fault not in ML: sig = %d, code = %#x, pc = %#x)\n",
	    signal, SIG_GetCode(si, scp), SIG_GetPC(scp));
    }

   /* Map the signal to Overflow */
    msp->ml_faultExn = OverflowId;
    msp->ml_faultPC = (Word_t)SIG_GetPC(scp);

    SIG_SetPC (scp, request_fault);

  /* I don't think that this call is still necessary, since we are only
   * dealing with integer overflow here! -- JHR (2019-10-10)
    SIG_ResetFPE (scp);
   */

} /* end of FaultHandler */

#else

PVT SigReturn_t FaultHandler (
    int		    signal,
#if (defined(TARGET_PPC) && defined(OPSYS_LINUX))
    SigContext_t    *scp)
#else
    SigInfo_t	    info,
    SigContext_t    *scp)
#endif
{
    ml_state_t	    *msp = SELF_VPROC->vp_state;
    extern Word_t   request_fault[];
    int		    code = SIG_GetCode(info, scp);

#ifdef SIGNAL_DEBUG
    SayDebug ("Fault handler: sig = %d, inML = %d\n",
	signal, SELF_VPROC->vp_inMLFlag);
#endif

    if (! SELF_VPROC->vp_inMLFlag)
	Die ("bogus fault not in ML: sig = %d, code = %#x, pc = %#x)\n",
	    signal, SIG_GetCode(info, scp), SIG_GetPC(scp));

   /* Map the signal to the appropriate ML exception. */
   /* Map the signal to Overflow */
    msp->ml_faultExn = OverflowId;
    msp->ml_faultPC = (Word_t)SIG_GetPC(scp);

    SIG_SetPC (scp, request_fault);

    SIG_ResetFPE (scp);

} /* end of FaultHandler */

#endif

#if ((defined(TARGET_RS6000) || defined(TARGET_PPC)) && defined(OPSYS_AIX))

/* SIG_GetCode:
 *
 * For  AIX, the overflow and divide by zero information is obtained
 * from information contained in the sigcontext structure.
 */
PVT int SIG_GetCode (SigInfo_t code, SigContext_t *scp)
{
    struct fp_sh_info	FPInfo;

    fp_sh_info (scp, &FPInfo, sizeof(struct fp_sh_info));

    return FPInfo.trap;

} /* end of SIG_GetCode */

#endif

#endif /* !defined(__CYGWIN32__) */
