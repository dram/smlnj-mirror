/* dlerror.c
 *
 * COPYRIGHT (c) 2000 by Lucent Technologies, Bell Laboratories
 */

#ifndef OPSYS_WIN32
# include "ml-unixdep.h"
#else
# include <dlfcn.h>
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

#ifdef OPSYS_WIN32

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* roll-your-own dlerror... */
static int dl_error_read = 0;
static char *dl_error = NULL;

void dlerror_set (const char *fmt, const char *s)
{
  if (dl_error != NULL)
    free (dl_error);
  dl_error = malloc (strlen (fmt) + strlen (s) + 1);
  sprintf (dl_error, fmt, s);
  dl_error_read = 0;
}

char *dlerror (void)
{
  if (dl_error)
    if (dl_error_read) {
      free (dl_error);
      dl_error = NULL;
    } else
      dl_error_read = 1;

  return dl_error;
}
#endif

/* _ml_P_Dynload_dlerror : unit -> string option
 *
 * Extract error after unsuccessful dlopen/dlsym/dlclose.
 */
ml_val_t _ml_U_Dynload_dlerror (ml_state_t *msp, ml_val_t ml_handle)
{
  const char *e = dlerror ();
  ml_val_t r, s;

  if (e == NULL)
    r = OPTION_NONE;
  else {
    s = ML_CString (msp, e);
    OPTION_SOME (msp, r, s);
  }
  return r;
}
