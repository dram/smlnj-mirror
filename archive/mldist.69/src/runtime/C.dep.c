/* C.dep.c
 * ANSI C dependent code for the SML/NJ runtime kernel  -- only for m68000
   so far.
 *
 */

#include <signal.h>
#include <setjmp.h>
#include "ml_os.h"
#include "request.h"

#include "tags.h"
#include "ml_types.h"

extern jmp_buf top_level;
extern int inML;

MACHINEID("ansi-c");

static int handle_sig(sig,code)
       int sig, code;
{  if (inML) {
       make_exn_code (sig,code);
       request = REQ_FAULT;
       saveregs();
   }
   else 
       die("bogus signal not in ML: (%d, %x)\n",signal,code);
}

void setup_mach_sigs (mask)
{ SETSIG(SIGFPE,handle_sig,mask);
}

