/* mask.h 
 *
 *   CALLEESAVE = 1 case is avoided, this is consistant with the 
 *   the code in cps directory(closure.sml generic.sml etc)
 */

#ifndef CALLEESAVE
#if defined(SPARC) || defined(MIPS)
#define CALLEESAVE 3
#else
#define CALLEESAVE 0
#endif
#endif

#define closmask ((1 << (CALLEESAVE + 3)) - 1)

#if (CALLEESAVE == 0)
#define contmask closmask
#else
#define contmask ((1 << (CALLEESAVE + 3)) - 2)
#endif

