/* timeofday.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#  include "ml-osdep.h"
#if defined(OPSYS_WIN32)
#  include <windows.h>
#elif defined(HAS_GETTIMEOFDAY)
#  include <sys/time.h>
#else
#  error no timeofday mechanism
#endif
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"

/* _ml_Time_timeofday : unit -> Int64.int
 *
 * Return the time of day.
 * NOTE: gettimeofday() is not POSIX (time() returns seconds, and is POSIX
 * and ISO C).
 */
ml_val_t _ml_Time_timeofday (ml_state_t *msp, ml_val_t arg)
{
#if defined(OPSYS_UNIX)
    struct timeval	t;

    gettimeofday (&t, NIL(struct timezone *));
    c_sec = t.tv_sec;
    c_usec = t.tv_usec;

    return ML_AllocNanoseconds(msp, c_sec, c_usec);
#elif defined(OPSYS_WIN32)
    FILETIME t;
    Unsigned64_t ns;

    GetSystemTime (&t);
  /* convert to nanoseconds; FILETIME is in units of 100ns */
    ns = 100 * (((Unsigned64_t)t.dwHighDateTime << 32) + (Unsigned64_t)t.dwLowDateTime);

    return ML_AllocWord64(msp, ns);
#else
#  error no timeofday mechanism
#endif

} /* end of _ml_Time_timeofday */
