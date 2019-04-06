/* ml_os.h
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * OS dependent definitions (to make things look like 4.3bsd)
 */

#ifndef _ML_OS_
#define _ML_OS_

#include <sys/types.h>

#ifdef V9
struct timeval {
    unsigned long   tv_sec;         /* seconds */
    long            tv_usec;        /* and microseconds */
};
#endif

#ifdef HPUX
#include <time.h>
#endif

/* the return type for signal handlers */
#if defined(VAX) || defined(RISCos) || defined(MACH)
typedef int SIGH_RET_TYPE;
#else
typedef void SIGH_RET_TYPE;
#endif

#ifdef HPUX
#define SETSIG(sig, h, mask)	{	\
    struct sigvec svec;			\
    svec.sv_mask = (mask);		\
    svec.sv_onstack = 0;		\
    svec.sv_handler = (h);		\
    sigvector ((sig), &svec, 0);	\
  }
#else !HPUX
#if defined(V9) || defined(SGI)
#define SETSIG(sig, h, mask)	{	\
    signal(sig, h);			\
  }
#else !HPUX && !V9 && !SGI
#define SETSIG(sig, h, mask)	{	\
    struct sigvec svec;			\
    svec.sv_mask = (mask);		\
    svec.sv_onstack = 0;		\
    svec.sv_handler = (h);		\
    sigvec ((sig), &svec, 0);		\
  }
#endif V9 || SGI
#endif HPUX

/* Signal name and code for initiation of garbage-collection */
#ifdef M68
#ifdef HPUX
#define     GC_SIG       SIGILL
#define     GC_CODE      7
#endif
#if defined(FPE_TRAPV_TRAP)
#define     GC_SIG       SIGFPE
#define     GC_CODE      FPE_TRAPV_TRAP
#endif
#if defined(NEXT)
#define     GC_SIG       SIGFPE
#define     GC_CODE      0x1c    /* TRAPV [cpTRAPcc TRAPcc] instr */
#endif
#if defined(MORE)
#define     GC_SIG       SIGFPE
#define     GC_CODE      FPE_INTOVF_TRAP
#endif
#endif

#if defined(VAX) && defined(V9)
#define FPE_INTOVF_TRAP K_INTOVF
#endif

/* miscellaneous */
#ifdef HPUX
#define bcopy(src, dst, len)	(memcpy((dst), (src), (len)))
#define bzero(dst, len)		(memset((dst), 0, (len)))
#endif

/* file-descriptor-sets for select() system call */
#ifndef FD_SET
#define	FD_SET(n, p)	((p)->fds_bits[(n)/32] |= (1 << ((n) % 32)))
#define	FD_CLR(n, p)	((p)->fds_bits[(n)/32] &= ~(1 << ((n) % 32)))
#define	FD_ISSET(n, p)	((p)->fds_bits[(n)/32] & (1 << ((n) % 32)))
#define	FD_ZERO(p)	(bzero((char *)(p), sizeof(*(p))))
#endif

/* cache-flushing stuff */
#ifdef MIPS
#include <sys/syscall.h>
#include <sys/sysmips.h>
#include <mips/cachectl.h>
#include <errno.h>
extern int errno;		/* some header files are missing this decl */
#  ifdef SGI
#  define  FlushICache(addr, size)	(sysmips(FLUSH_CACHE))
#  else
#  define FlushICache(addr, size)	\
      (syscall(SYS_sysmips, MIPS_CACHEFLUSH, (addr), (size), ICACHE, 0))
#  endif
#else
#define FlushICache(addr, size)

/* where to find syscall.h, used for getting the #define SYS_open */
#ifdef VAX
#include <syscall.h>
#else
#include <sys/syscall.h>
#endif
#endif

/** Reading directories **/
#ifdef HPUX
#include <sys/param.h>
/* the following are for <ndir.h> */
#define LONGFILENAMES
#define DIRSIZ_MACRO
#include <ndir.h>
#else !HPUX
#include <sys/dir.h>
#endif !HPUX
#ifndef DIRBLKSIZ
#define DIRBLKSIZ 512
#endif !DIRBLKSIZ
#ifdef SUNOS
#define READDIR(fd,buf,sz)	getdents((fd), (buf), (sz))
#else !sun3 && !sun4
static long dummy;
#define READDIR(fd,buf,sz)	getdirentries((fd), (buf), (sz), &dummy)
#endif


#if defined(BSD) || defined(RISCos) || defined(HPUX)
#define HAS_WRITEV
#include <sys/uio.h>
#define HAS_NONBLOCKING_IO
#endif




#endif !_ML_OS_
