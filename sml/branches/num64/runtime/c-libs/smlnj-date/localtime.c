/*! \file localtime.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "ml-c.h"

#if !defined(OPSYS_WIN32)

#include <time.h>

/* _ml_Date_localtime : Word32.word -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a UTC time value (in seconds), and converts it to local time represented
 * as a 9-tuple with the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year,
 * tm_wday, tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_localtime (ml_state_t *msp, ml_val_t arg)
{
    time_t	t = (time_t)WORD32_MLtoC(arg);
    struct tm	*tm;

    tm = localtime (&t);

    if (tm == NULL) RAISE_SYSERR(msp,0);

    ML_AllocWrite(msp, 0, MAKE_DESC(DTAG_record, 9));
    ML_AllocWrite(msp, 1, INT_CtoML(tm->tm_sec));
    ML_AllocWrite(msp, 2, INT_CtoML(tm->tm_min));
    ML_AllocWrite(msp, 3, INT_CtoML(tm->tm_hour));
    ML_AllocWrite(msp, 4, INT_CtoML(tm->tm_mday));
    ML_AllocWrite(msp, 5, INT_CtoML(tm->tm_mon));
    ML_AllocWrite(msp, 6, INT_CtoML(tm->tm_year));
    ML_AllocWrite(msp, 7, INT_CtoML(tm->tm_wday));
    ML_AllocWrite(msp, 8, INT_CtoML(tm->tm_yday));
    ML_AllocWrite(msp, 9, INT_CtoML(tm->tm_isdst));

    return ML_Alloc(msp, 9);

} /* end of _ml_Date_localtime */

#else /* OPSYS_WIN32 */

#include "win32-date.h"

/* _ml_Date_localtime : Word32.word -> (int * int * int * int * int * int * int * int * int)
 *
 * Takes a UTC time value (in seconds), and converts it to local time represented
 * as a 9-tuple with the fields:  tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year,
 * tm_wday, tm_yday, and tm_isdst.
 */
ml_val_t _ml_Date_localtime (ml_state_t *msp, ml_val_t arg)
{
    FILETIME utcFT;
    SYSTEMTIME localST, utcST;
    TIME_ZONE_INFORMATION tzInfo;
    BOOL isDST;

    secs_to_filetime (WORD32_MLtoC(arg), &utcFT);

  /* convert to system time */
    if (! FileTimeToSystemTime(&utcFT, &utcST)) {
	return RAISE_SYSERR(msp, 0);
    }

  /* adjust back to local time using the local time zone */
    if (! SystemTimeToTzSpecificLocalTime(NULL, &utcST, &localST)) {
	return RAISE_SYSERR(msp, 0);
    }

  /* need to figure out if localST is in DST; we do this by getting the local
   * timezone info for the given year and then check the range of dates
   */
    if (! GetTimeZoneInformationForYear(localST.wYear, NULL, &tzInfo)) {
	return RAISE_SYSERR(msp, 0);
    }
    if (tzInfo.StandardDate.wMonth == 0) {
      /* timezone does not support DST */
	isDST = FALSE;
    }
    else {
	/* TODO: test localST against tzInfo.StandardState and tzInfo.DaylightDate */
    }

    return _ml_alloc_tm (msp, &localST, isDST);

} /* end of _ml_Date_localtime */

#endif
