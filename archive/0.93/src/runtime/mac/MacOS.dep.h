/* MacOS.dep.h */
/* 31Dec91  e  */
/* 07Dec92  e  for 92 */

/* A MacOS version of Signals and Sbrk for ML */

#pragma once

struct timeval { int tv_sec; int tv_usec; };

#include <signal.h>
#if  (__NSIG != 6)
"this is an unknown version of THINK_C"
#endif

int brk(int);
int sbrk(int);
int get_edata(void);
int get_etext(void);
int getpagesize(void);
int eopen(char *, int);

#define SIG_DFL_e	((__sig_func_e) 0)
#define SIG_ERR_e	((__sig_func_e) -1)
#define SIG_IGN_e	((__sig_func_e) 1)

#undef	SIG_DFL
#define SIG_DFL		((__sig_func_e) 0)
#undef	SIG_ERR
#define SIG_ERR		((__sig_func_e) -1)
#undef	SIG_IGN
#define SIG_IGN		((__sig_func_e) 1)

/*      SIGABRT   	 1	*/	/* abort()	*/
/*      SIGFPE   	 2	*/	/* unused 	*/
/*      SIGILL   	 3	*/	/* unused 	*/
/*      SIGINT   	 4	*/	/* console 	*/
/*      SIGSEGV  	 5	*/	/* unused 	*/
/*      SIGTERM  	 6	*/	/* unused 	*/
/*      unused  	 7	*/
#define SIGHUP   	 8
#define SIGALRM  	 9
#define SIGUSR1  	10
#define SIGUSR2  	11
#define SIGQUIT  	12
#define SIGTSTP  	13
#define SIGCONT  	14
#define SIGURG   	15
#define SIGCHLD  	16
#define SIGIO    	17
#define SIGWINCH	18
#define SIGPIPE		19
#define SIGSYS		20
#define SIGVTALRM	21
#define SIGPROF		22
/*      unused  	23..30	*/
#define SIGTRAP		31
#define __NSIG_e	31


struct sigcontext	{
	int		sc_regs[15];
	int 	sig;
	short	sc_sr;
	int 	sc_pc;
	short 	code;
};

void e_raise(int);
typedef void (*__sig_func_e)(int, int, struct sigcontext *);
void e_signal(int, __sig_func_e);
void e_restart_handler(int);

/* end of MacOS.dep.h */
