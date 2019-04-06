/* timers.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */

#include "ml_os.h"

#if defined (V9) || defined(HPUX)
#include <sys/times.h>
#else
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "ml_types.h"

/* The global clocks (as ML ints) */
ML_val_t	g_usec, g_sec, t_usec, t_sec;

static struct timeval t1, t2, s1, s2, garbagetime;

/* get_cpu_time:
 * Get the user and system cpu times in a system independent way.
 */
static void get_cpu_time (user_t, sys_t)
    struct timeval  *user_t, *sys_t;
{
#if defined(V9) || defined(HPUX)
    struct tms	    ts;
    times (&ts);
    if (user_t) {
	user_t->tv_usec	= ((ts.tms_utime % 60) * 1000000) / 60;
	user_t->tv_sec	= (ts.tms_utime / 60);
    }
    if (sys_t) {
	sys_t->tv_usec	= ((ts.tms_stime % 60) * 1000000) / 60;
	sys_t->tv_sec	= (ts.tms_stime / 60);
    }
#else
    struct rusage   r;
    getrusage (RUSAGE_SELF, &r);
    if (user_t) { *user_t = r.ru_utime; }
    if (sys_t) { *sys_t = r.ru_stime; }
#endif
} /* end of get_cpu_time. */

/* starttime:
 */
void starttime ()
{
    get_cpu_time (&t1, 0);
}

/* endtime:
 */
int endtime ()
{
    int		sec, usec;

    get_cpu_time (&t2, 0);
    sec = t2.tv_sec - t1.tv_sec;
    usec = t2.tv_usec - t1.tv_usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    return (usec/1000 + sec*1000);
}

/* suspendtimers:
 */
suspendtimers()
{
    get_cpu_time (&s1, 0);
}

/* restarttimers:
 */
void restarttimers()
{
    int		sec, usec;
    get_cpu_time (&s2, 0);
    sec = (s2.tv_sec - s1.tv_sec) + garbagetime.tv_sec;
    usec = (s2.tv_usec - s1.tv_usec) + garbagetime.tv_usec;
    if (usec < 0) {
	sec--; usec += 1000000;
    }
    else if (usec > 1000000) {
	sec++; usec -= 1000000;
    }
    garbagetime.tv_sec = sec;
    garbagetime.tv_usec = usec;
}

/* resettimers:
 */
resettimers()
{
    garbagetime.tv_sec = 0;
    garbagetime.tv_usec = 0;
    s2.tv_sec = s1.tv_sec = 0;
    s2.tv_usec = s1.tv_usec = 0;
    t2.tv_sec = t1.tv_sec = 0;
    t2.tv_usec = t1.tv_usec = 0;
}

/* timer:
 */
timer()
{
    struct timeval	t;

    get_cpu_time (&t, 0);
    g_usec	= INT_CtoML(garbagetime.tv_usec);
    g_sec	= INT_CtoML(garbagetime.tv_sec);
    t_usec	= INT_CtoML(t.tv_usec);
    t_sec	= INT_CtoML(t.tv_sec);
}
