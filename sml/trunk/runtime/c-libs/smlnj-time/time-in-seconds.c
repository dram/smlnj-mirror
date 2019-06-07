/*! \file time-in-seconds.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-osdep.h"
#if defined(OPSYS_WIN32)
#  include <sys/types.h>
#  include <sys/timeb.h>
#else
#  include <time.h>
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Time_time_in_seconds : unit -> Int32.int
 *
 * Return the time of day in seconds.
 * NOTE: time() is POSIX and also ISO C, so it should be supported on all
 * target platforms.
 */
ml_val_t _ml_Time_time_in_seconds (ml_state_t *msp, ml_val_t arg)
{
    return INT32_CtoML(msp, time (0));

} /* end of _ml_Time_timeofday */
