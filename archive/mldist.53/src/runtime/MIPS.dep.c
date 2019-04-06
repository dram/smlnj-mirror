/* Copyright 1989 by AT&T Bell Laboratories */
/* MIPS.dep.c
 *
 *    MIPS dependent code for SML/NJ runtime kernel.
 */

#include <syscall.h>
#include <sys/sysmips.h>
#include <machine/cachectl.h>
#include <mips/cpu.h>
#include <signal.h>
#include "tags.h"

#define limit 19
#define dataptr 23
#define my_arith 20
#define LIMITCHECK1 (040 | (dataptr << 21) | (limit << 16))
			    /* M.add(dataptr, limit', Reg 0) */
#define LIMITCHECK2 (040 | (my_arith << 21) | (limit << 16))
			    /* M.add(my_arithtemp, limit', Reg 0) */

extern int saveregs[];		/* keep compiler from looking in global area */
extern int saved_pc;

extern int cause, fault_code;	/* describe faults */

/* ghandle:
 *    Handler for GC signals.
 */
#ifdef MIPSEL
void
#else MIPSEL
int
#endif MIPSEL
ghandle(sig,code,scp,addr) int sig, code, addr; struct sigcontext *scp;
{
    saved_pc = scp->sc_pc;
    scp->sc_pc = (int)saveregs;
    if (sig == SIGFPE && code == EXC_OV) {
        if ((*(int *)(saved_pc)) == LIMITCHECK1) {
	    cause = CAUSE_GC;
	} else if ((*(int *)(saved_pc)) == LIMITCHECK2) {
	    saved_pc -= 4;
	    cause = CAUSE_GC;
	} else {
	    fault_code = exnCode(sig,code);
	    cause = CAUSE_FAULT;
	}
    } else {
        fault_code = exnCode(sig,code);
        cause = CAUSE_FAULT;
    }
}


#ifdef MIPSEL
extern void handleprof();	/* lie about type to make compiler work */
#else MIPSEL
extern int handleprof();
#endif MIPSEL

setupsignals ()
{
  struct sigvec a;

  a.sv_handler = ghandle;
  a.sv_mask	= sigmask(SIGINT);
  a.sv_onstack	= 0;
  a.sv_mask	= 0;
  sigvec(SIGINT,&a,0);
  sigvec(SIGFPE,&a,0);
  sigvec(SIGTRAP,&a,0);
  a.sv_handler = SIG_IGN;
  sigvec(SIGPIPE,&a,0);
  a.sv_handler = handleprof;
  sigvec(SIGVTALRM,&a,0);
}


/* Flush the instruction cache, starting at addr for size bytes */
FlushInstructionCache(addr,size)
int addr, size;
{
  syscall(SYS_sysmips, MIPS_CACHEFLUSH, addr, size, ICACHE, 0);
}

cachectl(addr,nbytes,op) 
    char *addr;
    int nbytes, op;
{
    return syscall(SYS_sysmips,MIPS_CACHECTL,addr,nbytes,op,0);
}

cacheflush(addr, nbytes, cache)
     char *addr;
     int nbytes, cache;
{
     return syscall(SYS_sysmips,MIPS_CACHEFLUSH,addr,nbytes,cache,0);
}

