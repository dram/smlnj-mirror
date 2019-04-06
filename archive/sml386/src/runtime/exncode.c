/* exncode.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * NOTE: this file should be merged with signal.c
 */

#ifdef MIPS
#ifndef SGI
#include <mips/cpu.h>   /* for EXC_OV */
#else
#include <sys/sbd.h>   /* for EXC_OV */
#endif
#endif
#include <signal.h>
#include "tags.h"
#include "ml_types.h"

extern int	div_e0[], overflow_e0[];

ML_val_t	fault_exn;

#ifdef HPUX
#  define CODE_FLTDIV		0x0400
#  define CODE_FLTOVF		0x1000
#  define CODE_FLTUND		0x0800
#endif

#ifdef NeXT
/* these are taken from the 0.9 include files */
#  define CODE_OVERFLOW		0x1c
#  define CODE_ZERODIV		0x14
#  define CODE_FLTDIV		0xc8
#  define CODE_FLTOVF		0xd4
#  define CODE_FLTUND		0xcc
#endif

/* make_exn_code:
 * Map a UNIX (signal, code) pair to an ML (exception, arg) pair.
 */
void make_exn_code (sig, code)
    int		sig, code;
{
    switch (sig) {
      case SIGFPE:
#ifdef HPUX
	if (code == 5) {
	    fault_exn = PTR_CtoML(div_e0+1);
	    return;
	}
	else
	    code &= 0x1c00;  /* grab exception status */
#endif HPUX
#ifdef MIPS
	if (code == EXC_OV) {
	    fault_exn = PTR_CtoML(overflow_e0+1);
	}
	else {
	  /* code contains the FP control/status register */
	    if (code & 0x4000) /* bit-14 is overflow */
		fault_exn = PTR_CtoML(overflow_e0+1);
	    else if (code & 0x18000) /* bit-15 is divide by zero,
				       bit-16 is 0/0 */
		fault_exn = PTR_CtoML(div_e0+1);
	    else if (code & 0x2000 /* bit-13 is underflow */)
		die("underflow should not trap");
	    else 
	        die("strange floating point error");
	}
	return;
#else
	switch (code) {
#ifdef CODE_OVERFLOW
	case CODE_OVERFLOW:
#endif
#ifdef FPE_TRAPV_TRAP
	case FPE_TRAPV_TRAP:
#endif
#ifdef FPE_INTOVF_TRAP
	case FPE_INTOVF_TRAP:
#endif
#ifdef FPE_FLTOVF_TRAP
	case FPE_FLTOVF_TRAP:
#endif
#ifdef FPE_FLTOVF_FAULT
	case FPE_FLTOVF_FAULT:
#endif
#ifdef K_INTOVF
	  case K_INTOVF:
#endif
#ifdef CODE_FLTOVF
	  case CODE_FLTOVF:
#endif
#ifdef K_FLTOVF
	  case K_FLTOVF:
#endif
	    fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
#ifdef CODE_ZERODIV
	  case CODE_ZERODIV:
#endif
#ifdef FPE_INTDIV_TRAP
	  case FPE_INTDIV_TRAP:
#endif
#ifdef FPE_FLTDIV_TRAP
	  case FPE_FLTDIV_TRAP:
#endif
#ifdef FPE_FLTDIV_FAULT
	  case FPE_FLTDIV_FAULT:
#endif
#ifdef CODE_FLTDIV
	  case CODE_FLTDIV:
#endif
#ifdef K_INTDIV
	  case K_INTDIV:
#endif
#ifdef K_FLTDIV
	  case K_FLTDIV:
#endif
	    fault_exn = PTR_CtoML(div_e0+1);
	    return;
#ifdef CODE_FLTUND
	  case CODE_FLTUND:
#endif
#ifdef K_FLTUND
	  case K_FLTUND:
#endif
#ifdef FPE_FLTUND_TRAP
	  case FPE_FLTUND_TRAP:
#endif
#ifdef FPE_FLTUND_FAULT
	  case FPE_FLTUND_FAULT:
#endif
		die("underflow should not trap");
	  default:
	        die("strange floating point error");
	}
#endif MIPS

      case SIGEMT:
	die("Floating point EMT trap (68881 not installed?)");

#ifdef HPUX
      case SIGILL:
	if (code == 7) {
	    fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
	}
	else
	    die ("exnCode: code was %d\n",code);
#endif HPUX

#ifdef MIPS
      case SIGTRAP:
	switch (code) {
	  case BRK_OVERFLOW:
	    fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
	  case BRK_DIVZERO:
	    fault_exn = PTR_CtoML(div_e0+1);
	    return;
	  default:
	    die("illegal BREAK instruction");
	}
#endif MIPS
    } /* end of switch */

} /* end of make_exn_code */
