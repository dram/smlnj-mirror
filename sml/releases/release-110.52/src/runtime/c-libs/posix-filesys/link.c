/* link.c
 *
 * COPYRIGHT (c) 1995 by AT&T Bell Laboratories.
 */

#include "ml-unixdep.h"
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"
#include <unistd.h>


/* _ml_P_FileSys_link : string * string -> unit
 *                      existing newname
 *
 * Creates a hard link from newname to existing file.
 */
ml_val_t _ml_P_FileSys_link (ml_state_t *msp, ml_val_t arg)
{
    int		sts;
    ml_val_t	existing = REC_SEL(arg, 0);
    ml_val_t	newname = REC_SEL(arg, 1);

    sts = link(STR_MLtoC(existing), STR_MLtoC(newname));

    CHK_RETURN_UNIT (msp, sts)

} /* end of _ml_P_FileSys_link */
