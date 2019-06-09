/*! \file win32-date.h
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common definitions for Windows versions of the Date functions.
 */

#ifndef _WIN32_DATE_H_
#define _WIN32_DATE_H_

#include "ml-base.h"
#include <windows.h>

/* convert an unsigned 32-bit seconds value to a FILETIME value. */
STATIC_INLINE void secs_to_filetime (Unsigned32_t sec, FILETIME *ft)
{
    ULARGE_INTEGER uli;

    uli.u.LowPart = sec;
    uli.u.HighPart = 0;
    uli.QuadPart = 10000000 * uli.QuadPart;	/* convert to 100ns units */

    ft->dwLowDateTime = uli.u.LowPart;
    ft->dwHighDateTime = uli.u.HighPart;
}

/* convert a FILETIME in 100ns units to unsigned seconds */
STATIC_INLINE Unsigned32_t filetime_to_secs (const FILETIME *ft)
{
    ULARGE_INTEGER uli;

    uli.u.LowPart = ft->dwLowDateTime;
    uli.u.HighPart = ft->dwHighDateTime;

    uli.QuadPart = uli.QuadPart / 10000000;	/* convert to seconds */

    return uli.u.LowPart;

}

/* compute the day of the year from a SYSTEMTIME struct */
int _ml_year_day (const SYSTEMTIME *st);
ml_val_t _ml_alloc_tm (ml_state_t *msp, const SYSTEMTIME *st, BOOL isDST);

#endif /* !_WIN32_DATE_H_ */
