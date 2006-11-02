/* gethostbyname.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include "sock-util.h"


/* _ml_NetDB_gethostbyname
 *     : string -> (string * string list * addr_family * addr list) option
 */
ml_val_t _ml_NetDB_gethostbyname (ml_state_t *msp, ml_val_t arg)
{
    return _util_NetDB_mkhostent (msp, gethostbyname (STR_MLtoC(arg)));

} /* end of _ml_NetDB_gethostbyname */
