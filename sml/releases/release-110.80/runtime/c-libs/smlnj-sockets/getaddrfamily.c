/* getaddrfamily.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"

/* _ml_Sock_getaddrfamily : addr -> af
 *
 * Extract the family field, convert to host byteorder, and return it.
 */
ml_val_t _ml_Sock_getaddrfamily (ml_state_t *msp, ml_val_t arg)
{
    struct sockaddr *addr = GET_SEQ_DATAPTR(struct sockaddr, arg);

    return ML_SysConst (msp, &_Sock_AddrFamily, ntohs(addr->sa_family));

} /* end of _ml_Sock_getaddrfamily */
