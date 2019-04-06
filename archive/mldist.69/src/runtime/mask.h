/* mask.h 
 *
 *   CALLEESAVE = 1 case is avoided, this is consistant with the 
 *   the code in cps directory(closure.sml generic.sml etc)
 */

#if !defined(CALLEESAVE)
#define CALLEESAVE 0
#endif

#if (CALLEESAVE < 0)
#define CALLEESAVE 0
#endif

#if (CALLEESAVE == 1)
#define CALLEESAVE 2
#endif 

#define closmask ((1 << (CALLEESAVE + 3)) - 1)

#if (CALLEESAVE == 0)
#define contmask closmask
#else
#define contmask ((1 << (CALLEESAVE + 3)) - 2)
#endif

