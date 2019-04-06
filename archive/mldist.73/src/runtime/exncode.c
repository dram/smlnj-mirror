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

#ifdef HPUX
#  define CODE_FLTDIV		0x0400
#  define CODE_FLTOVF		0x1000
#  define CODE_FLTUND		0x0800
#  ifndef FPE_FLTOPERR_TRAP
#    define FPE_FLTOPERR_TRAP   0x2000
#  endif
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
void make_exn_code (MLState, sig, code)
    MLState_ptr MLState;
    int		sig, code;
{
    switch (sig) {
      case SIGFPE:
#ifdef HPUX
	if (code == 5) {
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
	    return;
	}
	else
	    code &= 0x3c00;  /* grab exception status */
#endif HPUX

/* MACH maps mips exceptions to the corresponding BSD exceptions; it
  does it right */

#if defined(MIPS) && !defined(MACH) 
	if (code == EXC_OV) {
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	}
	else {
	  /* code contains the FP control/status register */
	    if (code & 0x4000) /* bit-14 is overflow */
		MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    else if (code & 0x18000) /* bit-15 is divide by zero,
				       bit-16 is 0/0 */
		MLState->fault_exn = PTR_CtoML(div_e0+1);
	    else if (code & 0x2000 /* bit-13 is underflow */)
		die("underflow should not trap\n");
	    else 
	        die("strange floating point error, %d\n",code);
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
#if   defined(SPARC) && defined (MACH)
	case 0x8:
#endif 
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
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
#ifdef FPE_FLTOPERR_TRAP	/* sun-3&4s generate this on 0.0/0.0 */
	  case FPE_FLTOPERR_TRAP:
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
#if    defined(SPARC) && defined(MACH)
	  case 0x82:
#endif
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
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
		die("underflow should not trap\n");
	  default:
	        die("strange floating point error, %#x\n", code);
	}
#endif MIPS

      case SIGEMT:
	die("Floating point EMT trap (68881 not installed?)\n");

#ifdef HPUX
      case SIGILL:
	if (code == 7) {
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
	}
	else
	    die ("exnCode: code was %d\n",code);
#endif HPUX

#if     defined(SPARC) && defined(MACH)
      case SIGILL:
	if (code == 0x87) {
	      MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	      return;
	    } else
	      die ("SIGILL code 0x%x\n",code);
#endif   defined(SPARC) && defined(MACH)
#ifdef MIPS
      case SIGTRAP:
	switch (code) {
	  case BRK_OVERFLOW:
	    MLState->fault_exn = PTR_CtoML(overflow_e0+1);
	    return;
	  case BRK_DIVZERO:
	    MLState->fault_exn = PTR_CtoML(div_e0+1);
	    return;
	  default:
	    die("illegal BREAK instruction");
	}
#endif MIPS
    } /* end of switch */

} /* end of make_exn_code */
