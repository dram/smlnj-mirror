/* gettime.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-timer.h"
#include "cfun-proto-list.h"

/* _ml_Time_gettime : unit -> (Int32.int * int * Int32.int * int * Int32.int * int)
 *
 * Return the total CPU time, system time and garbage collection time used by this
 * process so far.
 */
ml_val_t _ml_Time_gettime (ml_state_t *msp, ml_val_t arg)
{
    Time_t		t, s;
    ml_val_t		tSec, sSec, gcSec, res;
    vproc_state_t	*vsp = msp->ml_vproc;

    GetCPUTime (&t, &s);

    tSec = INT32_CtoML (msp, t.seconds);
    sSec = INT32_CtoML (msp, s.seconds);
    gcSec = INT32_CtoML (msp, vsp->vp_gcTime->seconds);
    REC_ALLOC6 (msp, res,
	tSec, INT_CtoML(t.uSeconds),
	sSec, INT_CtoML(s.uSeconds),
	gcSec, INT_CtoML(vsp->vp_gcTime->uSeconds));

    return res;

} /* end of _ml_Time_gettime */

/* _ml_Time_gettime_64 : unit -> Int64.int * Int64.int * Int64.int
 *
 * Return the total CPU time, system time and garbage collection time used by this
 * process so far.
 */
ml_val_t _ml_Time_gettime_64 (ml_state_t *msp, ml_val_t arg)
{
    Time_t		t, s;
    ml_val_t		cpuT, sysT, gcT, res;
    vproc_state_t	*vsp = msp->ml_vproc;

    GetCPUTime (&t, &s);

    cpuT = ML_AllocNanoseconds(msp, t.seconds, t.uSeconds);
    sysT = ML_AllocNanoseconds(msp, s.seconds, s.uSeconds);
    gcT = ML_AllocNanoseconds(msp, vsp->vp_gcTime->seconds, vsp->vp_gcTime->uSeconds);

    REC_ALLOC3(msp, res, cpuT, sysT, gcT);

    return res;

} /* end of _ml_Time_gettime */
