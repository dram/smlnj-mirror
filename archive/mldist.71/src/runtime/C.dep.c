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
extern MLState_ptr find_self();

MACHINEID("ansi-c");

static int handle_sig(sig,code)
       int sig, code;
{ 
  MLState_ptr MLState = find_self();
  if (MLState->inML) {
       make_exn_code (MLState,sig,code);
       request = REQ_FAULT;
       saveregs(MLState);
   }
   else 
       die("bogus signal not in ML: (%d, %x)\n",signal,code);
}

void setup_mach_sigs (mask)
{ SETSIG(SIGFPE,handle_sig,mask);
}

