/* getATMARK.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "sockets-osdep.h"
#include INCLUDE_TYPES_H
#include INCLUDE_SOCKET_H
#include <sys/ioctl.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_Sock_getATMARK : sock -> int
 */
ml_val_t _ml_Sock_getATMARK (ml_state_t *msp, ml_val_t arg)
{
    int		n, sts;

    sts = ioctl (INT_MLtoC(arg), SIOCATMARK, (char *)&n);

    if (sts < 0)
	return RAISE_SYSERR(msp, sts);
    else if (n == 0)
	return ML_false;
    else
	return ML_true;

} /* end of _ml_Sock_getATMARK */
