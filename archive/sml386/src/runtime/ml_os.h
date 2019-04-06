/* ml_os.h     (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * OS dependent definitions (to make things look like 4.3bsd)
 *
 * Altered 20 Dec. 1991 by :    Yngvi S. Guttesen
 *                              Department of Computer Science
 *                              The Technical University of Denmark
 *                              DK-2800 Lyngby
 */

#ifndef _ML_OS_
#define _ML_OS_

struct timeval {
    unsigned long   tv_sec;         /* seconds */
    long            tv_usec;        /* and microseconds */
};

#include <time.h>

/* the return type for signal handlers */
typedef void SIGH_RET_TYPE;

#define SETSIG(sig, h, mask)    signal(sig, h);

/* Signal name and code for initiation of garbage-collection */
#define     GC_SIG       SIGUSR1
#define     GC_CODE      SIGUSR1


/* cache-flushing stuff */

#define FlushICache(addr, size)


/** Reading directories **/
#include "direct.h"
#define READDIR(fd,buf,sz)


#endif !_ML_OS_
