/*! \file unix-date.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common definitions for UNIX versions of the Date functions.
 */

#ifndef _UNIX_DATE_H_
#define _UNIX_DATE_H_

#include <time.h>

/* convert time_t value (in seconds) to 64-bit unsigned nanoseconds */
STATIC_INLINE Unsigned64_t time_to_ns (time_t t)
{
    return 1000000000 * (Unsigned64_t)t;
}

/* convert 64-bit unsigned nanoseconds  to a time_t value (in seconds) */
STATIC_INLINE time_t ns_to_time (Unsigned64_t ns)
{
    return (time_t)(ns / 1000000000);
}

#endif /* _UNIX_DATE_H_ */
