/* getservbyname.c
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

/* _ml_NetDB_getservbyname
 *     : (string * string option) -> (string * string list * int * string) option
 */
ml_val_t _ml_NetDB_getservbyname (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	mlServ = REC_SEL(arg, 0);
    ml_val_t	mlProto = REC_SEL(arg, 1);
    char	*proto;

    if (mlProto == OPTION_NONE)
	proto = NIL(char *);
    else
	proto = STR_MLtoC(OPTION_get(mlProto));

    return _util_NetDB_mkservent (
	msp,
	getservbyname (STR_MLtoC(mlServ), proto));

} /* end of _ml_NetDB_getservbyname */
