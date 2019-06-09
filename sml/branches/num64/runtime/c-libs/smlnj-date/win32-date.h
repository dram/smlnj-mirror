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

#include <windows.h>

/* convert an unsigned 32-bit seconds value to a FILETIME value. */
STATIC_INLINE void secs_to_filetime (Unsigned32_t sec, FILETIME &ft)
{
    ULARGE_INTEGER uli;
    FILETIME ft;

    uli.LowPart = sec;
    uli.HiPart = 0;
    uli.QuadPart = 10000000 * uli.QuadPart;	/* convert to 100ns units */

    ft->dwLowDateTime = uli.LowPart;
    ft->dwHighDateTime = uli.HighPart;
}

/* convert a FILETIME in 100ns units to unsigned seconds */
STATIC_INLINE Unsigned32_t filetime_to_secs (FILETIME &ft)
{
    ULARGE_INTEGER uli;

    uli.LowPart = ft->dwLowDateTime;
    uli.HighPart = ft->dwHighDateTime;

    uli.QuadPart = uli.QuadPart / 10000000;	/* convert to seconds */

    return uli.LowPart;

}

/* compute the day of the year from a SYSTEMTIME struct */
int _ml_year_day (SYSTEMTIME &st);
ml_val_t _ml_alloc_tm (ml_state_t *msp, SYSTEMTIME &st, BOOL isDST);

#endif /* !_WIN32_DATE_H_ */
