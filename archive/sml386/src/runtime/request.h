/* request.h    (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories
 *
 * Altered 20 Dec. 1991 by:     Yngvi S. Guttesen
 *                              Department of Computer Science
 *                              The Technical University of Denmark
 *                              DK-2800 Lyngby
 */

#ifndef _REQUEST_

#define _REQUEST_

#define REQ_RETURN	0
#define REQ_EXN		1
#define REQ_FAULT	2
#define REQ_GC1         3
#define REQ_CALLC	4
#define REQ_SIGNAL	5
#define REQ_SIG_RETURN	6
#define REQ_SIG_RESUME	7
#define REQ_SIG_RAISE	8
#define REQ_GC2         9
#define REQ_GC3         10

extern int      request;

#endif
