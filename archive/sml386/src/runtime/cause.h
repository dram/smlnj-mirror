/* cause.h     (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Reasons for doing garbage collection.
 *
 * Altered 20 Dec. 1991 by:     Yngvi S. Guttesen
 *                              Department of Computer Science
 *                              The Technical University of Denmark
 *                              DK-2800 Lyngby
 */

#ifndef _CAUSE_
#define _CAUSE_

#define CAUSE_GC	0	/* exhausted free space */
#define CAUSE_MINOR	1	/* requested minor collection */
#define CAUSE_MAJOR	2	/* requested major collection */
#define CAUSE_EXPORT	3
#define CAUSE_BLAST	4
#define CAUSE_STORE	5

#endif !_CAUSE_
