/* utime.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <sys/types.h>
#include <utime.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_FileSys_utime : (string * int * int) -> unit
 *                        name     actime modtime
 *
 * Sets file access and modification times. If
 * actime = -1, then set both to current time.
 */
ml_val_t _ml_P_FileSys_utime (ml_state_t *msp, ml_val_t arg)
{
    char	    *path = REC_SELPTR(char, arg, 0);
    time_t          actime = REC_SELINT(arg, 1);
    time_t          modtime = REC_SELINT(arg, 2);
    int		    sts;

    if (actime == -1) {
      sts = utime (path, NIL(struct utimbuf *));
    }
    else {
      struct utimbuf tb;

      tb.actime = actime;
      tb.modtime = modtime;
      sts = utime (path, &tb);
    }

    CHK_RETURN_UNIT(msp, sts)

} /* end of _ml_P_FileSys_utime */
