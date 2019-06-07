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

    return ML_AllocNanoseconds(msp, t.tv_sec, t.tv_usec);
#elif defined(OPSYS_WIN32)
  /* we could use Win32 GetSystemTime/SystemTimetoFileTime here,
   * but the conversion routines for 64-bit 100-ns values
   * (in the mapi dll) are non-Win32s
   *
   * we'll use time routines from the C runtime for now.
   */
    {
	struct _timeb t;

	_ftime(&t);
	c_sec = t.time;
	c_usec = t.millitm*1000;
    }
#else
#  error no timeofday mechanism
#endif

} /* end of _ml_Time_timeofday */
