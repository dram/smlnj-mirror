/* timers.c     (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * Altered 20 Dec. 1991 by:     Yngvi S. Guttesen
 *                              Department of Computer Science
 *                              The Technical University of Denmark
 *                              DK-2800 Lyngby
 */

#include "sml.h"
#include "ml_os.h"
#include "ml_types.h"
#include "time.h"

/* The global clocks (as ML ints) */
ML_val_t	t_sec, t_usec,	/* total usr time */
		s_sec, s_usec,	/* total sys time */
		g_sec, g_usec;	/* garbage collection usr time */

struct timeval gt0,	    /* the startime of the most recent garbage collection */
	       garbagetime; /* the cumulative garbage collection time */

/* get_cpu_time:
 * Get the user and/or system cpu times in a system independent way.
 */
static void get_cpu_time (user_t, sys_t)
    struct timeval  *user_t, *sys_t;
{
    DWORD    ms;
    ms = GetCurrentTime();
    if (user_t) {
	user_t->tv_usec = (ms % 1000) * 1000;
	user_t->tv_sec	= (ms / 1000);
    }
    if (sys_t) {
	sys_t->tv_usec	= (ms % 1000) * 1000;
	sys_t->tv_sec	= (ms / 1000);
    }
} /* end of get_cpu_time. */


/* start_gc_timer:
 * Start timing a garbage collection.
 */
void start_gc_timer ()
{
    get_cpu_time (&gt0, 0);
}

/* check_gc_timer:
 * Return the time (in ms.) spent since the start of the most recent
 * garbage collection.
 */
long check_gc_timer ()
{
    struct timeval t1;
    long sec, usec;

    get_cpu_time (&t1, 0);
    sec = t1.tv_sec - gt0.tv_sec;
    usec = t1.tv_usec - gt0.tv_usec;
    if (usec<0) {
	--sec;
	usec += 1000000;
    }
    return(usec/1000 + sec*1000);
}

/* stop_gc_timer:
 * Stop the garbage collection timer and update the cumulative garbage collection
 * time.
 */
void stop_gc_timer ()
{
    struct timeval t1;
    long sec, usec;

    get_cpu_time (&t1, 0);
    sec = (t1.tv_sec - gt0.tv_sec) + garbagetime.tv_sec;
    usec = (t1.tv_usec - gt0.tv_usec) + garbagetime.tv_usec;
    if (usec<0) {
	--sec;
	usec += 1000000;
    }
    else if (usec>1000000) {
	++sec;
	usec -= 1000000;
    }
    garbagetime.tv_sec = sec;
    garbagetime.tv_usec = usec;
}

/* resettimers:
 * Clear the timers.
 */
resettimers()
{
    garbagetime.tv_sec = 0;
    garbagetime.tv_usec = 0;
}

/* timer:
 * Set the ML version of the global clock.
 */
timer()
{
    struct timeval	t, s;

    get_cpu_time (&t, &s);
    t_usec	= INT_CtoML(t.tv_usec);
    t_sec	= INT_CtoML(t.tv_sec);
    s_usec	= INT_CtoML(s.tv_usec);
    s_sec	= INT_CtoML(s.tv_sec);
    g_usec	= INT_CtoML(garbagetime.tv_usec);
    g_sec	= INT_CtoML(garbagetime.tv_sec);
}
