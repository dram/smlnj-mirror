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
 * NOTE: gettimeofday() is not POSIX (time() returns seconds, and is POSIX
 * and ISO C).
 */
ml_val_t _ml_Time_time_in_seconds (ml_state_t *msp, ml_val_t arg)
{
    int			c_sec;

#if defined(OPSYS_WIN32)
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
    }
#else
    c_sec = time (0);
#endif

    return INT32_CtoML(msp, c_sec);

} /* end of _ml_Time_timeofday */
