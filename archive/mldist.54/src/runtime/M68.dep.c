/* Copyright 1989 by AT&T Bell Laboratories */
/* M68.dep.c
 *
 *    M68 dependent code for the SML/NJ runtime kernel.
 */

#include <signal.h>
#include "tags.h"

#ifndef FPE_TRAPV_TRAP
#ifdef NeXT
#define     FPE_TRAPV_TRAP      0x1c    /* TRAPV [cpTRAPcc TRAPcc] instr */
#endif
#ifdef MORE
#define     FPE_TRAPV_TRAP      FPE_INTOVF_TRAP
#endif
#endif

extern int cause, fault_code;
extern int saved_pc;
extern int saveregs[];


/* ghandle:
 */
ghandle (sig, code, scp, addr)
    int			sig, code, addr;
    struct sigcontext	*scp;
{
    saved_pc = scp->sc_pc;
    scp->sc_pc = (int)saveregs; 

#ifdef HPUX
    if (sig==SIGILL && code==7 && ((short*)saved_pc)[-1] == 23548)
    {	saved_pc-=4;
	cause=CAUSE_GC;
    }
#else
    /* Note: versions of sony_news before Sept 1989 had pc pointer BEFORE
   trap instruction, not after. */
    if (sig==SIGFPE && code==FPE_TRAPV_TRAP && ((short*)saved_pc)[-1] == 23548)
    {   saved_pc-=4;
	cause=CAUSE_GC;
    }
#endif
    else {
	fault_code=exnCode(sig,code);
	cause=CAUSE_FAULT;
    }
}

extern int handleprof();

void setupsignals()
{
  struct sigvec a;
  extern fpenable();

#ifdef HPUX
#define sigvec sigvector
#endif HPUX
  a.sv_handler	= (void (*)()) ghandle;
  a.sv_onstack	= 0;
  a.sv_mask	= 0;
  sigvec(SIGINT,&a,0);
  sigvec(SIGFPE,&a,0);
#ifdef HPUX
  sigvec(SIGILL,&a,0);
#else
  sigvec(SIGEMT,&a,0);  /* in case 68881 is not present */
#endif HPUX
  a.sv_handler = SIG_IGN;
  sigvec(SIGPIPE,&a,0);
  a.sv_handler = (void (*)()) handleprof;
  sigvec(SIGVTALRM,&a,0);
  fpenable();
}
