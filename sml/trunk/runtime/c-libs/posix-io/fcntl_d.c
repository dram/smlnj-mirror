/* fcntl_d.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include <fcntl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_P_IO_fcntl_d : int * int -> int
 *
 * Duplicate an open file descriptor
 */
ml_val_t _ml_P_IO_fcntl_d (ml_state_t *msp, ml_val_t arg)
{
    int             fd;
    int             fd0 = REC_SELINT(arg, 0);
    int             fd1 = REC_SELINT(arg, 1);

    fd = fcntl(fd0, F_DUPFD, fd1);

    CHK_RETURN(msp, fd)

} /* end of _ml_P_IO_fcntl_d */
