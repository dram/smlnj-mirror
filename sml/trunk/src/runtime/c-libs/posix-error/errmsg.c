/* errmsg.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_Error_errmsg : int -> string
 *
 * Return the OS-dependent error message associated with error.
 */
ml_val_t _ml_P_Error_errmsg (ml_state_t *msp, ml_val_t arg)
{
    int		    errnum = INT_MLtoC(arg);
    ml_val_t	    s;

#if defined(HAS_STRERROR)
    char	    *msg = strerror(errnum);
    if (msg != 0)
	s = ML_CString (msp, msg);
    else {
	char		buf[64];
	sprintf(buf, "<unknown error %d>", errnum);
	s = ML_CString (msp, buf);
    }
#else
    if ((0 <= errnum) && (errnum < sys_nerr))
	s = ML_CString (msp, sys_errlist[errnum]);
    else {
	char		buf[64];
	sprintf(buf, "<unknown error %d>", errnum);
	s = ML_CString (msp, buf);
    }
#endif

    return s;

} /* end of _ml_P_Error_errmsg */
