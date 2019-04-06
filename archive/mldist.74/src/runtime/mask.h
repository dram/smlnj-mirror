/* mask.h 
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * CALLEESAVE = 1 case is avoided, this is consistant with the 
 * the code in cps directory(closure.sml generic.sml etc)
 */

#ifndef CALLEESAVE
#if defined(SPARC) || defined(MIPS)
#define CALLEESAVE 3
#else
#define CALLEESAVE 0
#endif
#endif

#define closmask ((1 << (CALLEESAVE + 3)) - 1)

/*
 * We avoid saying:
 *	if (CALLEESAVE == 0) ..
 * since this gives cpp running on HPUX trouble.
 */
#if CALLEESAVE
#define contmask ((1 << (CALLEESAVE + 3)) - 2)
#else
#define contmask closmask
#endif

