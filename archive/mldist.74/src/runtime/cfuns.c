/* cfuns.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * These are the C functions that are callable from ML (via REQ_CALLC).
 */

#include "ml_os.h"
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <sys/stat.h>
#include <sys/errno.h>
#ifdef V9
#include <sys/filio.h>
#include <sys/ttyio.h>
#else
#include <sys/ioctl.h>
#include <sys/file.h>
#ifndef HPUX
#include <sys/time.h>
#endif
#endif
#ifndef HPUX
#include <sys/param.h>
#endif
#ifdef SPARC
#include <vfork.h>	/* tells sparc optimizer about vfork */
#endif

#include "ml_state.h"
#include "ml_types.h"
#include "cause.h"
#include "prim.h"

/* Imported from mp.c */
extern void ml_release_proc ();
extern void ml_acquire_proc ();
extern void ml_spin_lock ();
extern void ml_max_procs ();

/* do a system call, restarting it if interrupted */
#define DO_SYSCALL(CALL,sts) {					\
	while (((sts = CALL) == -1) && (errno == EINTR))	\
            continue;						\
    }

/* return a value to the calling ML code */
#define RETURN(msp,r)	{		\
    msp->ml_arg = (r);			\
    return;}

/* return sts to the calling ML code, but raise an exception if an error occured */
#define CHK_RETURN(msp,sts)	{		\
    if (sts == -1) raise_syserror(msp, 0);	\
    else RETURN(msp, INT_CtoML(sts)) }

extern int	    errno;

/* backup_kont:
 * When a signal interrupts a blocking select call, this is called to
 * back-up the continuation to re-try the select.
 */
static void backup_kont (msp)
    MLState_ptr		msp;
{
    msp->ioWaitFlag	= 0;
    msp->ml_closure	= PTR_CtoML(callc_v+1);
    msp->ml_pc		= CODE_ADDR(PTR_CtoML(callc_v+1));
}

#ifdef V9

#define MAX_TIMEOUT	0x7fffffff

/* v9_select:
 * A 4.2bsd interface to V9 select.
 */
static int v9_select (width, rfds, wfds, efds, *timeout)
    int		    width;
    fd_set	    *rfds, *wfds, *efds;
    struct timeval  *timeout;
{
    int		    t, sts;

    if (timeout == 0)
	t = MAX_TIMEOUT;
    else {
	t = timeout.tv_usec / 1000;
	if (timeout.tv_sec > (MAX_TIMEOUT/1000)-t)
	    t = MAX_TIMEOUT;
	else
	    t += (t.tv_sec * 1000);
    }

    if (efds != NULL)
	return -1;  /* exceptional conditions not supported on V9 */

    DO_SYSCALL(select(width, rfds, wfds, t), sts);
    return sts;
}

#define select v9_select
#endif V9


/* raise_syserror:
 * Raise the ML exception SysError with the errno and error string as the argument
 * pair.  If alt_msg is non-zero, then use it as the error string and use -1 as the
 * errno.
 */
void raise_syserror (msp, alt_msg)
    MLState_ptr		msp;
    char	    *alt_msg;
{
    extern int	    sys_nerr, syserror_id0[];
    extern char	    *sys_errlist[];
    ML_val_t	    s, arg, exn;

    if (alt_msg != 0) {
	s = ML_alloc_string (msp, alt_msg);
	errno = -1;
    }
    else if ((0 <= errno) && (errno < sys_nerr))
	s = ML_alloc_string (msp, sys_errlist[errno]);
    else {
	char		buf[32];
	sprintf(buf, "<unknown error %d>", errno);
	s = ML_alloc_string (msp, buf);
    }

    REC_ALLOC2 (msp, arg, INT_CtoML(errno), s);
    REC_ALLOC2 (msp, exn, arg, PTR_CtoML(syserror_id0+1));
    msp->fault_exn = exn;

    raise_ml_exn (msp);
}


#define MAX_SYSCALL_ARGS	6

/* ml_syscall : (int * string list) -> int
 * Perform the requested system call with the given arguments.  Unboxed
 * values are converted to C ints.
 */
void ml_syscall (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    int		    code = REC_SELINT(arg, 0);
    register ML_val_t p = REC_SEL(arg, 1);
    int		    av[MAX_SYSCALL_ARGS];
    register int    i, r;
    register ML_val_t v;

#if (MAX_PROCS > 1)
    if (code == 1) /* exit */ {
      mp_shutdown(msp, INT_MLtoC(ML_hd(p)));
    }
#endif
#if  defined(RISCos) || defined(SGI)
    code += 1000;  /* adjust syscall code for RISCos */
#endif
    for (i = 0; p != ML_nil; p = ML_tl(p), i++) {
	if (OBJ_isBOXED(v = ML_hd(p)))
	    av[i] = (int)PTR_MLtoC(v);
	else
	    av[i] = INT_MLtoC(v);
    }
    if (! _setjmp(msp->SysCallEnv))
	switch (i) {
	  case 0:
	    r = syscall(code); break;
	  case 1:
	    r = syscall(code, av[0]); break;
	  case 2:
	    r = syscall(code, av[0], av[1]); break;
	  case 3:
	    r = syscall(code, av[0], av[1], av[2]); break;
	  case 4:
	    r = syscall(code, av[0], av[1], av[2], av[3]); break;
	  case 5:
	    r = syscall(code, av[0], av[1], av[2], av[3], av[4]); break;
	  case 6:
	    r = syscall(code, av[0], av[1], av[2], av[3], av[4], av[5]); break;
	  default:
	    raise_syserror (msp, "ml_syscall: too many args");
	    return;
	}
    else {
      /* a SIGSYS occurred (because of a bad syscall) */
	sigsetmask (0);  /* re-enable signals */
	raise_syserror (msp, "bad syscall");
	return;
    }

    if (r == -1) {
	if (errno == EINTR)
	    backup_kont(msp);
	else
	    raise_syserror(msp, 0);
	return;
    }
    else
	RETURN(msp, INT_CtoML(r));

} /* end of ml_syscall. */


/* ml_open : (string * int) -> int
 * Open a file and return the file descriptor.
 */
void ml_open (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    char	    *path = (char *)REC_SELPTR(arg, 0);
    int		    mode = REC_SELINT(arg, 1);
    int		    fd, flags;

#if defined(V9) || defined(HPUX)
    switch (mode) {
      case 0: /* READ */
	DO_SYSCALL(open(path, 0), fd);
	break;
      case 1: /* WRITE */
	DO_SYSCALL(creat(path, 0666), fd);
	break;
      case 2: /* APPEND */
	DO_SYSCALL(open(path, 1), fd);
	if (fd == -1)
	    DO_SYSCALL(creat(path, 0666), fd)
	else if (lseek(fd, 0, L_INCR) == -1)
	    fd = -1;
	break;
    }
#else
    switch (mode) {
      case 0: flags = (O_RDONLY); break;
      case 1: flags = (O_WRONLY|O_TRUNC|O_CREAT); break;
      case 2: flags = (O_WRONLY|O_APPEND|O_CREAT); break;
    }
    DO_SYSCALL(open (path, flags, 0666), fd);
#endif

    CHK_RETURN(msp, fd);

} /* end of ml_open */


/* ml_connect_unix : string -> fd
 * Open a client-side UNIX domain STREAM socket connection on the given pathname.
 */
void ml_connect_unix (msp, arg)
    MLState_ptr	    msp;
    ML_val_t	    arg;
{
    char	    *path = (char *)PTR_MLtoC(arg);
    struct sockaddr_un sock;
    int		    fd, len, sts;

    DO_SYSCALL (socket(PF_UNIX, SOCK_STREAM, 0), fd);
    if (fd != -1) {
	sock.sun_family = AF_UNIX;
	strcpy (sock.sun_path, path);
	len = strlen(path)+sizeof(sock.sun_family);
	DO_SYSCALL (connect(fd, (struct sockaddr *)&sock, len), sts);
	if (sts != -1) {
	    RETURN(msp, INT_CtoML(fd));
	}
	else {
	    int		olderrno = errno;
	    close (fd);
	    errno = olderrno;
	}
    }
    raise_syserror(msp, 0);

} /* end of ml_connect_unix */

/* ml_connect_inet : (string * string) -> fd
 * Open a client-side INET domain STREAM socket connection to the given host/port.
 * Currently the host must be specified as a string of the form "d.d.d.d" where
 * the d's are decimal numbers from 0 to 255 separated by "."s.  The port is specified
 * as a string representation of a decimal port number.
 * Note: eventually this interface will be extended to use symbolic names.
 */
void ml_connect_inet (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    char	    *hostname = (char *)REC_SELPTR(arg, 0);
    char	    *port = (char *)REC_SELPTR(arg, 1);
    struct sockaddr_in saddr;
    int		    fd, s, i, sts;

#if defined(SUNOS) || (defined(BSD) && defined(MIPS))
    DO_SYSCALL (socket(PF_INET, SOCK_STREAM, 0), fd);
    if (fd != -1) {
	saddr.sin_family = AF_INET;
	saddr.sin_port = htons(atoi(port));
	bzero(saddr.sin_zero, sizeof(saddr.sin_zero));
	s = i = 0;
	do {
	    s = (s << 8) | atoi(hostname);
	    while (*hostname && (*hostname != '.'))
		hostname++;
	} while (*hostname++ != '\0');
	saddr.sin_addr.s_addr = htonl(s);
	DO_SYSCALL (connect(fd, (struct sockaddr *)&saddr, sizeof(saddr)), sts);
	if (sts == 0) {
	    RETURN(msp, INT_CtoML(fd));
	}
	else {
	    int		olderrno = errno;
	    close (fd);
	    errno = olderrno;
	}
    }
    raise_syserror(msp, 0);
#else
    raise_syserror(msp, "unimplemented");
#endif

} /* end of ml_connect_unix */


/* ml_link : (bool * string * string) -> unit
 * Create a link (or symbolic link).
 */
void ml_link (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    ML_val_t	    is_sym = REC_SEL(arg, 0);
    char	    *name = (char *)REC_SELPTR(arg, 1);
    char	    *lname = (char *)REC_SELPTR(arg, 2);
    int		    sts;

    if (is_sym == ML_true)
	sts = symlink (name, lname);
    else
	sts = link (name, lname);

    CHK_RETURN(msp, sts);

} /* end of ml_link */


/* ml_wait_for_in : fd -> unit
 * Wait for input on the given file descriptor.
 */
void ml_wait_for_in (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = INT_MLtoC(arg), sts;
    fd_set	    rfds;

  restart:; /* on EINTR */
    if (msp->inSigHandler
    || ((! _setjmp (msp->SysCallEnv)) && 
	(((msp->ioWaitFlag = 1), (msp->NumPendingSigs == 0))))) {
#ifdef RISCos
	/* problem with select and pipes */
	sts = 0;
#else
	FD_ZERO(&rfds);
	FD_SET(fd, &rfds);
	sts = select(fd+1, &rfds, 0, 0, 0);
#endif
	msp->ioWaitFlag = 0;
    }
    else {
	backup_kont(msp);
	sigsetmask (0);  /* re-enable signals */
	return;
    }

    if (sts == -1) {
	if (errno == EINTR)
	    backup_kont(msp);
	else
	    raise_syserror(msp, 0);
	return;
    }

    RETURN(msp, ML_unit);

} /* end of ml_wait_for_in. */

/* ml_wait_for_out : fd -> unit
 * Wait for output on the given file descriptor.
 */
void ml_wait_for_out (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = INT_MLtoC(arg), sts;
    fd_set	    wfds;

  restart:; /* on EINTR */
    if (msp->inSigHandler
    || ((! _setjmp (msp->SysCallEnv)) && 
	(((msp->ioWaitFlag = 1), (msp->NumPendingSigs == 0))))) {
#ifdef RISCos
	/* problem with select and pipes */
	sts = 0;
#else
	FD_ZERO(&wfds);
	FD_SET(fd, &wfds);
	sts = select(fd+1, 0, &wfds, 0, 0);
#endif
	msp->ioWaitFlag = 0;
    }
    else {
	backup_kont (msp);
	sigsetmask (0);  /* re-enable signals */
	return;
    }

    if (sts == -1) {
	if (errno == EINTR)
	    backup_kont(msp);
	else
	    raise_syserror(msp, 0);
	return;
    }

    RETURN(msp, ML_unit);

} /* end of ml_wait_for_out. */


/* ml_read : (int * bytearray * int) -> int
 * Read data from the specified file into the given bytearray.  Return the
 * number of bytes read.
 */
void ml_read (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    char	    *buf = (char *)REC_SELPTR(arg, 1);
    int		    nbytes = REC_SELINT(arg, 2);
    int		    n;

    DO_SYSCALL (read (fd, buf, nbytes), n);
    CHK_RETURN(msp, n);

} /* end of ml_read */

/* ml_readi : (int * bytearray * int * int) -> int
 * Read data from the specified file into the given bytearray, starting at
 * offset.  Return the number of bytes read.
 */
void ml_readi (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    char	    *buf = (char *)REC_SELPTR(arg, 1);
    char	    *start = buf + REC_SELINT(arg, 2);
    int		    nbytes = REC_SELINT(arg, 3);
    int		    n;

    DO_SYSCALL (read (fd, buf, nbytes), n);
    CHK_RETURN(msp, n);

} /* end of ml_readi */


/* write_all:
 * Write the requested number of bytes from the buffer.  Return 0 on success,
 * and -1 on errors.
 */
static int write_all (fd, buf, nbytes)
    int		    fd;
    char	    *buf;
    int		    nbytes;
{
    register int    n;

    while (nbytes > 0) {
	DO_SYSCALL (write (fd, buf, nbytes), n);
	if (n > 0) {
	    nbytes -= n;
	    buf += n;
	}
	else
	    return -1;
    }
    return 0;

} /* end of write_all. */

/* ml_write : (int * bytearray * int) -> unit
 * Write data from the given bytearray to the specified file.  Return the
 * number of bytes written.
 */
void ml_write (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    sts;

    sts = write_all (
	    REC_SELINT(arg, 0),
	    (char *)REC_SELPTR(arg, 1),
	    REC_SELINT(arg, 2));

    if (sts == -1)
	raise_syserror(msp, 0);
    else
	RETURN(msp, ML_unit);

} /* end of ml_write */

/* ml_writei : (int * bytearray * int * int) -> unit
 * Write data from the given bytearray to the specified file, starting at the
 * given offset.  This routine is guaranteed to write all the bytes.
 */
void ml_writei (msp, arg)
    MLState_ptr     msp;
    register ML_val_t arg;
{
    int		    sts;

    sts = write_all (
	    REC_SELINT(arg, 0),
	    (char *)REC_SELPTR(arg, 1) + REC_SELINT(arg, 2),
	    REC_SELINT(arg, 3));

    if (sts == -1)
	raise_syserror(msp, 0);
    else
	RETURN(msp, ML_unit);

} /* end of ml_writei */


#ifndef HAS_WRITEV
struct iovec {
    char	    *iov_base;
    int		    iov_len;
};
#endif !HAS_WRITEV

/* write_multiple:
 * Write a vector of blocks and return the number of blocks written.  Normally,
 * this will be iovcnt, but if a signal occurs during the write, then it can
 * be less.  Return -1 on error.
 */
static int write_multiple (msp, fd, iov, iovcnt, nbytes)
    MLState_ptr     msp;
    int		    fd;
    struct iovec    *iov;
    int		    iovcnt, nbytes;
{
#ifdef HAS_WRITEV
    int		    skip = 0, i = iovcnt;

    while (nbytes > 0) {
      advance:;
	while (skip > 0) {
	    if (iov->iov_len <= skip)
		skip -= iov->iov_len;
	    else {
	      /* write the incomplete buffer */
		int	sts;
		do {
		    sts = write_all(fd, iov->iov_base+skip, iov->iov_len-skip);
		    if (sts < 0) {
			raise_syserror(msp, 0);
			return -1;
		    }
		} while (sts != 0);
		if ((nbytes -= (iov->iov_len - skip)) == 0)
		    return iovcnt;
		else if (msp->NumPendingSigs > 0)
		    return ((iovcnt - i) + 1);
	    }
	    i--;  iov++;
	}
	DO_SYSCALL (writev(fd, iov, i), skip);
	if (skip < 0) {
	    raise_syserror(msp, 0);
	    return -1;
	}
	nbytes -= skip;
    }
#else !HAS_WRITEV
    int		    i, sts;

    for (i = 0;  i < iovcnt;  i++) {
	if ((sts = write_all (fd, vec[i].iov_base, vec[i].iov_len)) == -1)
	    return -1;
	else
	    i++;
	if (msp->NumPendingSigs > 0)
	    return i;
    }
#endif HAS_WRITEV

    return iovcnt;

} /* end of write_multiple */

#define WRITEVEC_SZ	8

/* ml_writev : (int * (bytearray * int) list) -> unit
 * For each (data, len) element in the list, write the len number of bytes to the
 * file descriptor.
 */
void ml_writev (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    ML_val_t	    p = REC_SEL(arg, 1), q = p;
    int		    nbytes = 0, i, n;
    struct iovec    vec[WRITEVEC_SZ];

    nbytes = 0;
    for (i = 0; p != ML_nil;  ) {
	ML_val_t    pair = ML_hd(p);

	p = ML_tl(p);
	vec[i].iov_base = (char *)REC_SELPTR(pair, 0);
	vec[i].iov_len  = REC_SELINT(pair, 1);
	nbytes += vec[i].iov_len;
	if ((++i == WRITEVEC_SZ) || (p == ML_nil)) {
	    if ((n = write_multiple(msp, fd, vec, i, nbytes)) < 0)
		return; /* error */
	    else if (n < i) {
	      /* a signal occurred, so set things up so that the resume continuation
	       * will complete the write operation.
	       */
		while (n > 0) {
		    q = ML_tl(q);  n--;
		}
		REC_ALLOC2 (msp, arg, INT_CtoML(fd), q);
		REC_ALLOC2 (msp, msp->ml_arg, PTR_CtoML(ml_writev), arg);
		msp->ml_closure = PTR_CtoML(callc_v+1);
		msp->ml_pc	    = CODE_ADDR(PTR_CtoML(callc_v+1));
		return;
	    }
	    nbytes = 0;
	    i = 0;
	    q = p;
	}
    } /* end of for */

} /* end of ml_writev */

/* ml_send_obd : (fd * bytearray * int) -> unit
 * Send out-of-band data on the specified socket file descriptor.
 */
void ml_send_obd (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    char	    *buf = (char *)REC_SELPTR(arg, 1);
    register int    nbytes = REC_SELINT(arg, 2);
    register int    n;

    while (nbytes > 0) {
	DO_SYSCALL (send (fd, buf, nbytes, MSG_OOB), n);
	if (n > 0) {
	    nbytes -= n;
	    buf += n;
	}
	else
	    raise_syserror (msp, 0);
    }

} /* end of ml_send_obd */


/* ml_getdirent : int -> string list
 * Get directory entries from the directory referenced by fdesc.  If there are
 * no more entries, then return nil.
 */
static void ml_getdirent (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = INT_MLtoC(arg);
    char	    buf[DIRBLKSIZ];
    register int    nbytes, i;
    ML_val_t	    l = ML_nil;

    do {
	DO_SYSCALL (READDIR(fd, buf, DIRBLKSIZ), nbytes);
	if (nbytes == -1) {
	    raise_syserror (msp, 0);
	    return;
	}
	else {
	    ML_val_t	    s;
	    struct direct       *dp;

	    for (i = 0;  i < nbytes;  i += dp->d_reclen) {
		dp = (struct direct *)&(buf[i]);
		if (dp->d_name[0] != 0) {
		    s = ML_alloc_string (msp, dp->d_name);
		    l = ML_cons (msp, s, l);
		}
	    } /* end of for */
	}
    } while ((nbytes > 0) && (l == ML_nil));
    RETURN(msp, l);

} /* end of ml_getdirent */


/* ml_readlink : string -> string
 * Read the contents of the specified symbolic link.
 */
void ml_readlink (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    char	    *lname = (char *)PTR_MLtoC(arg);
    int		    n;
    char	    buf[MAXPATHLEN];

    if ((n = readlink(lname, buf, MAXPATHLEN)) == -1)
	raise_syserror (msp, 0);
    else {
	ML_val_t	path;
	buf[n] = '\0';
	path = ML_alloc_string (msp, buf);
	RETURN(msp, path);
    }

} /* end of ml_readlink */


/* ml_truncate : (fd or string * int) -> unit
 * Truncate the specified file to the specified length.
 */
void ml_truncate (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    register ML_val_t f = REC_SEL(arg, 0);
    int		    len = REC_SELINT(arg, 1);
    int		    sts;

    if (OBJ_isBOXED(f))
	sts = truncate(PTR_MLtoC(f), len);
    else
	sts = ftruncate(INT_MLtoC(f), len);

    CHK_RETURN(msp, sts);

} /* end of ml_truncate */


/* ml_chmod : (fd or string * int) -> unit
 * Change the protection mode of the specified file.
 */
void ml_chmod (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    ML_val_t	    f = REC_SEL(arg, 0);
    int		    mode = REC_SELINT(arg, 1);
    int		    sts;

    if (OBJ_isBOXED(f))
	sts = chmod(PTR_MLtoC(f), mode);
    else
	sts = fchmod(INT_MLtoC(f), mode);

    CHK_RETURN(msp, sts);

} /* end of ml_chmod */


/* ml_access : (string * int list) -> bool
 * Check to see if the user has the specified access to the specified file.
 */
void ml_access (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    char	    *path = (char *)REC_SELPTR(arg, 0);
    register ML_val_t p = REC_SEL(arg, 1);
    int		    mode = F_OK;

    for (;  p != ML_nil;  p = ML_tl(p)) {
        switch (INT_MLtoC(ML_hd(p))) {
	  case 0: mode |= R_OK; break;
	  case 1: mode |= W_OK; break;
	  case 2: mode |= X_OK; break;
	  default:
	    raise_syserror (msp, "unknown access mode");
	    return;
	} /* end of switch */
    } /* end of for */

    if (access(path, mode) == 0)
	RETURN(msp, ML_true)
    else if (errno == EACCES)
	RETURN(msp, ML_false)
    else
	raise_syserror (msp, 0);

} /* end of ml_access. */

/* stat_file:
 * Get the file status of f.  The file can either be specified as a path, in which
 * case f will be a boxed ML string, otherwise f will be an unboxed file descriptor.
 */
static int stat_file (msp, f, buf)
    MLState_ptr     msp;
    ML_val_t	    f;
    struct stat	    *buf;
{
    int		    sts;

    if (OBJ_isBOXED(f))
	sts = lstat((char *)PTR_MLtoC(f), buf);
    else
	sts = fstat(INT_MLtoC(f), buf);

    if (sts == -1)
	raise_syserror (msp, 0);

    return sts;

} /* end of stat_file */

/* ml_getfid : (fd or string) -> fileid
 * Return the unique file id (a string created from the device and inode of the
 * file) of the specified file.
 */
void ml_getfid (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;
    struct { dev_t dev; ino_t ino; } id_buf;
    ML_val_t	    p;

    if (stat_file(msp, f, &buf) == 0) {
	register int	sz = (sizeof(id_buf)+3) & ~3;

	ML_alloc_write (msp, 0, MAKE_DESC(sz, tag_string));
	p = ML_alloc (msp, sz >> 2);

	bzero ((char *)&id_buf, sz);
	id_buf.dev = buf.st_dev;
	id_buf.ino = buf.st_ino;
	bcopy ((char *)&id_buf, (char *)PTR_MLtoC(p), sz);

	RETURN(msp, p);
    }

} /* end of ml_getfid */

/* ml_getmod : (fd or string) -> int
 * Return the file protection mode of the file specified by f.
 */
void ml_getmod (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;

    if (stat_file(msp, f, &buf) == 0) {
	RETURN(msp, INT_CtoML(buf.st_mode & 0777));
    }

} /* end of ml_getmod */

/* ml_ftype : (fd or string) -> int
 * Return the file type of the file specified by f.  The return values must
 * track those in System.Unsafe.FileIO (see "boot/perv.sml").
 */
void ml_ftype (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;
    register ML_val_t typ;

    if (stat_file(msp, f, &buf) == 0) {
	switch (buf.st_mode & S_IFMT) {
	  case S_IFREG: typ = INT_CtoML(0); break;
	  case S_IFDIR: typ = INT_CtoML(1); break;
	  case S_IFLNK: typ = INT_CtoML(2); break;
	  case S_IFSOCK: typ = INT_CtoML(3); break;
	  case S_IFCHR: typ = INT_CtoML(4); break;
	  case S_IFBLK: typ = INT_CtoML(5); break;
	  default:
	    raise_syserror(msp, "unknown file type");
	    return;
	}
	RETURN(msp, typ);
    }
} /* end of ml_ftype */

/* ml_getownid : (fd or string) -> (int * int)
 * Return the user and group ids of the specified file.
 */
void ml_getownid (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;
    ML_val_t	    obj;

    if (stat_file(msp, f, &buf) == 0) {
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_uid), INT_CtoML(buf.st_gid));
	RETURN(msp, obj);
    }

} /* end of ml_getownid */

/* ml_fsize : (fd or string) -> int
 * Return the size in bytes of the specified file.
 */
void ml_fsize (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;
    extern int	    overflow_e0[];

    if (stat_file(msp, f, &buf) == 0) {
	if ((buf.st_size & 0xC0000000) != 0)
	    raise_ml_exn (PTR_CtoML(overflow_e0+1));
	else
	    RETURN(msp, INT_CtoML(buf.st_size))
    }

} /* end of ml_fsize */

/* ml_atime : (fd or string) -> (int * int)
 * Get the most recent access time of the specified file.
 */
void ml_atime (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;

    if (stat_file(msp, f, &buf) == 0) {
	ML_val_t	obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_atime), INT_CtoML(0));
	RETURN(msp, obj);
    }

} /* end of ml_atime */

/* ml_ctime : (fd or string) -> (int * int)
 * Get the creation time of the specified file.
 */
void ml_ctime (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;
    extern int	    overflow_e0[];

    if (stat_file(msp, f, &buf) == 0) {
	ML_val_t	obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_ctime), INT_CtoML(0));
	RETURN(msp, obj);
    }

} /* end of ml_ctime */

/* ml_mtime : (fd or string) -> (int * int)
 * Get the most recent modification time of the specified file.
 */
void ml_mtime (msp, f)
    MLState_ptr     msp;
    ML_val_t	    f;
{
    struct stat	    buf;
    extern int	    overflow_e0[];

    if (stat_file(msp, f, &buf) == 0) {
	ML_val_t	obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(buf.st_mtime), INT_CtoML(0));
	RETURN(msp, obj);
    }

} /* end of ml_mtime */


/* ml_isatty : int -> bool
 * Return true if the file descriptor fd refers to a tty device.
 */
void ml_isatty (msp, fd)
    MLState_ptr     msp;
    ML_val_t	    fd;
{
    RETURN(msp, isatty(INT_MLtoC(fd)) ? ML_true : ML_false);

} /* end of ml_isatty */


/* fd_list2set:
 * Map a ML list of file descriptors to a fd_set.
 */
static fd_set *fd_list2set (fdl, fds, width)
    ML_val_t	    fdl;
    fd_set	    *fds;
    int		    *width;
{
    register int    fd, maxfd = -1;

    FD_ZERO(fds);
    while (fdl != ML_nil) {
	fd = INT_MLtoC(ML_hd(fdl));
	if (fd > maxfd)
	    maxfd = fd;
	FD_SET (fd, fds);
	fdl = ML_tl(fdl);
    }

    if (maxfd >= 0) {
	if (maxfd >= *width)
	    *width = maxfd+1;
	return fds;
    }
    else
	return (fd_set *)0;
}

/* fd_set2list:
 * Map a fd_set to a ML list of ready file descriptors.
 */
static ML_val_t fd_set2list (msp, fds, width)
    MLState_ptr     msp;
    register fd_set *fds;
    register int    width;
{
    register ML_val_t p;
    register int    i;

    if (fds == 0)
	return ML_nil;

    for (i = 0, p = ML_nil;  i < width;  i++) {
	if (FD_ISSET(i, fds))
	    p = ML_cons (msp, INT_CtoML(i), p);
    }

    return p;
}

/* ml_select : (int list * int list * int list * (int * int))
 *                 -> (int list * int list * int list)
 * Check file descriptors for the readiness of I/O operations.
 */
void ml_select (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    ML_val_t	    rl = REC_SEL(arg, 0);
    ML_val_t	    wl = REC_SEL(arg, 1);
    ML_val_t	    el = REC_SEL(arg, 2);
    ML_val_t	    timeout = REC_SEL(arg, 3);
    fd_set	    rset, wset, eset;
    fd_set	    *rfds, *wfds, *efds;
    int		    width = 0, sts;
    struct timeval  t, *tp;

    rfds = fd_list2set (rl, &rset, &width);
    wfds = fd_list2set (wl, &wset, &width);
    efds = fd_list2set (el, &eset, &width);

    if (OBJ_isBOXED(timeout)) {
	t.tv_sec = REC_SELINT(timeout, 0);
	t.tv_usec = REC_SELINT(timeout, 1);
	tp = &t;
    }
    else
	tp = 0;

    if (msp->inSigHandler
    || ((! _setjmp (msp->SysCallEnv)) && 
	(((msp->ioWaitFlag = 1), (msp->NumPendingSigs == 0))))) {
#ifdef RISCos
	/* problem with select and pipes */
	sts = 0;
#else
	DO_SYSCALL (select (width, rfds, wfds, efds, tp), sts);
#endif
	msp->ioWaitFlag = 0;
    }
    else {
	backup_kont(msp);
	sigsetmask (0);  /* re-enable signals */
	return;
    }

    if (sts == -1)
	raise_syserror (msp, 0);
    else {
	ML_val_t	    rfdl, wfdl, efdl, res;

	if (sts == 0)
	    rfdl = wfdl = efdl = ML_nil;
	else {
	    rfdl = fd_set2list (msp, rfds, width);
	    wfdl = fd_set2list (msp, wfds, width);
	    efdl = fd_set2list (msp, efds, width);
	}
	REC_ALLOC3 (msp, res, rfdl, wfdl, efdl);
	RETURN(msp, res);
    }

} /* end of ml_select */


/* ml_pipe : unit -> (int * int)
 * Create a pipe and return its input and output descriptors.
 */
void ml_pipe (msp)
    MLState_ptr     msp;
{
    int		fds[2];

    if (pipe(fds) == -1)
	raise_syserror (msp, 0);
    else {
	ML_val_t obj;
	REC_ALLOC2 (msp, obj, INT_CtoML(fds[0]), INT_CtoML(fds[1]));
	RETURN(msp, obj);
    }

} /* end of ml_pipe. */


/* ml_fionread : int -> int
 * Return the number of bytes available for reading in the given file.
 */
void ml_fionread (msp, arg)
     MLState_ptr     msp;
    int		arg;
{
    int		fd = INT_MLtoC(arg);
    struct stat	buf;
    int		pos;
    int		count[2], r;

#  ifdef HPUX
    if (isatty(fd)) {		/* FIONREAD unsupported on tty's on 6.5 */
	fd_set		readfds;
	int		nfound;
	struct timeval	timeout;

	FD_SET(fd, &readfds);
	timeout.tv_sec = 0;  timeout.tv_usec = 0;
	nfound = select(fd+1, &readfds, 0, 0, &timeout);
	CHK_RETURN(msp, nfound);
    } /* if */
#  else
    if (ioctl(fd, FIONREAD, count) >= 0)
	RETURN(msp, INT_CtoML(count[0]));
#  endif HPUX

    if ((fstat(fd, &buf) < 0) || ((pos = lseek(fd, 0, L_INCR)) < 0))
	raise_syserror (msp, 0);
    else
	RETURN(msp, INT_CtoML(buf.st_size - pos));

} /* end of ml_fionread */


/* ml_system : string -> int
 * Issue the given shell command and return the exit status.
 */
void ml_system (msp, arg)
    MLState_ptr     msp;
    ML_val_t	arg;
{
    int		sts;

    sts = system ((char *)PTR_MLtoC(arg));
    CHK_RETURN (msp, sts);

} /* end of ml_system */


/* ml_exec : (string * string list * string list) -> (int * int)
 * Fork a process to execute the given command, with the given command-line
 * arguments and environment.  Return the file descriptors for pipes
 * connected to the process' stdin and stdout. The arg list should be
 * non-empty with the command name as the first element.
 */
void ml_exec (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    char	    *cmd = (char *)REC_SELPTR(arg, 0);
    ML_val_t	    arglst = REC_SEL(arg, 1);
    ML_val_t	    envlst = REC_SEL(arg, 2);
    int		    p1[2], p2[2];
    ML_val_t	    res;

    if ((pipe(p1) < 0) || (pipe(p2) < 0))
	raise_syserror (msp, 0);
#ifdef V9
    else if (fork()) {
#else
    else if (vfork()) {
#endif
	close(p1[0]); close(p2[1]);
	REC_ALLOC2 (msp, res, INT_CtoML(p2[0]), INT_CtoML(p1[1]));
	RETURN(msp, res);
    }
    else {
	char		**argv, **envp;
	register ML_val_t p;
	register char   **cp;

      /* use the heap for temp space for the argv[] and envp[] vectors */
	argv = cp = (char **)(msp->ml_allocptr);
	for (p = arglst;  p != ML_nil;  p = ML_tl(p))
	    *cp++ = (char *)PTR_MLtoC(ML_hd(p));
	*cp++ = 0;  /* terminate the argv[] */
	envp = cp;
	for (p = envlst;  p != ML_nil;  p = ML_tl(p))
	    *cp++ = (char *)PTR_MLtoC(ML_hd(p));
	*cp++ = 0;  /* terminate the envp[] */

	close (p1[1]); close (p2[0]);
	dup2 (p1[0], 0); dup2 (p2[1], 1);
	execve (cmd, argv, envp);
	_exit(1);
    }

} /* end of ml_exec */


extern ML_val_t make_str_list();

/* ml_argv : unit -> string list
 * Return the command-line argument list.
 */
void ml_argv (msp)
    MLState_ptr     msp;
{
    extern char **global_argv;

    RETURN(msp, make_str_list (msp, global_argv));

} /* end of ml_argv */

/* ml_envrion : unit -> string list
 * Return the environment list.
 */
void ml_environ (msp)
    MLState_ptr     msp;
{
    extern char **environ;

    RETURN(msp, make_str_list (msp, environ));

} /* end of ml_environ */

/* ml_gethostname : unit -> string
 * Return the name of our host.
 */
void ml_gethostname (msp)
    MLState_ptr     msp;
{
    char	buf[64];

    if (gethostname(buf, 64) == 0) {
	ML_val_t name;
	buf[63] = '\0';  /* insure null termination */
	name = ML_alloc_string (msp, buf);
	RETURN(msp, name);
    }
    else
	raise_syserror (msp, 0);

} /* end of ml_gethostname */


static int	blast_fd;	/* the file descriptor to blast to */

/* ml_blast_out : (int * 'a) -> 'a
 */
void ml_blast_out (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    blast_fd	    = REC_SELINT(arg, 0);
    msp->ml_arg = REC_SEL(arg, 1);
    callgc0 (msp, CAUSE_BLAST, 0, CONT_ARGS_MASK);

} /* end of ml_blast_out */

/* blast_write:
 */
void blast_write (msp, start, end, ptr)
    MLState_ptr msp;
    int		start, end, ptr;
{
    int		hdr[3];

    hdr[0] = start;
    hdr[1] = end;
    hdr[2] = ptr - start;
    if (bulletproofWrite0 (blast_fd, hdr, sizeof(hdr)) 
       || bulletproofWrite0 (blast_fd, start, end-start))
         raise_syserror (msp, 0);
    else RETURN(msp, ML_unit);
} /* end of blast_write */


/* ml_blast_in : string -> 'a
 * Build an object from the string.  The string has a special header (produced
 * by blast_write).
 */
void ml_blast_in (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    *obj = PTR_MLtoC(arg);
    int		    start = obj[0];
    int		    end = obj[1];
    int		    offset = obj[2];
    int		    *words = (obj + 3);

    (PTR_MLtoC(arg))[-1] = MAKE_DESC(4, tag_string);
    relocate (start, end, words);
    RETURN(msp, PTR_CtoML((int)words + offset));

} /* end of blast_in */


/* ml_export : int -> bool
 * Export the world to the given file and return false (the exported version
 * returns true).
 */
void ml_export (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		    fd = INT_MLtoC(arg);
    register int    i;
    extern int	    isExported;
    extern MLState_ptr Exporters_State;
#if (MAX_PROCS > 1)
    extern void     check_suspended();

    check_suspended(msp);
#endif

    msp->ml_pc = ML_unit;
    msp->ml_closure = ML_unit;

  /* shed the unecessary stuff */
    callgc0(msp, CAUSE_EXPORT, 0, CONT_ARGS_MASK);
    callgc0(msp, CAUSE_EXPORT, 0, CONT_ARGS_MASK);

  /* export */
#if !defined(CALLEESAVE)
#define CALLEESAVE 0
#endif

#if (CALLEESAVE > 0)
    msp->ml_pc = msp->ml_cont;
#else 
    msp->ml_pc = CODE_ADDR(msp->ml_cont);
#endif
    isExported = 1;
    Exporters_State = msp;
    i = export (fd);
    isExported = 0;
    Exporters_State = (MLState_ptr)0;
    if (i) raise_syserror (msp, 0);
    else RETURN(msp, ML_false);


} /* end of ml_export */


/* ml_gettime : unit -> (int * int * int * int * int * int)
 * Return the total CPU time, system time and garbage collection time used by this
 * process so far.
 */
void ml_gettime (msp)
    MLState_ptr     msp;
{
    ML_val_t	res;
    extern ML_val_t	t_sec, t_usec, s_sec, s_usec, g_sec, g_usec;

    timer();
    REC_ALLOC6 (msp, res, t_sec, t_usec, s_sec, s_usec, g_sec, g_usec);
    RETURN(msp, res);

} /* end of ml_gettime */


/* ml_timeofday : unit -> (int * int)
 * Return the time of day.
 */
void ml_timeofday (msp)    
     MLState_ptr     msp;
{
    struct timeval	t;
    ML_val_t		res;

    gettimeofday (&t, 0);
    REC_ALLOC2 (msp, res, INT_CtoML(t.tv_sec), INT_CtoML(t.tv_usec));
    RETURN(msp, res);

} /* end of ml_timeofday. */


/* ml_setitimer : (int * int * int * int * int) -> int
 * Set an interval timer; the first argument specifies which timer.
 */
void ml_setitimer (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    struct itimerval itv;
    register int    which;

    itv.it_interval.tv_sec  = REC_SELINT(arg, 1);
    itv.it_interval.tv_usec = REC_SELINT(arg, 2);
    itv.it_value.tv_sec     = REC_SELINT(arg, 3);
    itv.it_value.tv_usec    = REC_SELINT(arg, 4);

    switch (REC_SELINT(arg, 0)) {
      case 0: which = ITIMER_REAL; break;
      case 1: which = ITIMER_VIRTUAL; break;
      case 2: which = ITIMER_PROF; break;
    }

    if (setitimer (which, &itv, 0) == -1) {
        raise_syserror(msp, 0);
        return;
    }
    else
        RETURN(msp, ML_unit);

} /* end of ml_setitimer */


/* ml_setglobal : int array -> unit
 */
void ml_setglobal (msp, p)
    MLState_ptr     msp;
    ML_val_t	    p;
{
#ifdef GLOBAL_INDX
    msp->ml_globalptr = p;
#endif
    RETURN(msp, ML_unit);

} /* end of ml_setglobal */

/* ml_flush_icache : string -> unit
 * Flush the instruction cache for the range covered by s.  This is a no-op
 * on most machines.
 */
void ml_flush_icache (msp, s)
    MLState_ptr     msp;
    ML_val_t		s;
{
    int		begin = (int)PTR_MLtoC(s);
    int		len = OBJ_LEN(s) + 4;

    FlushICache (begin, len);

    RETURN(msp, ML_unit);
}


/* ml_gc : int -> unit
 * Force a garbage collection of the specified level (0 == minor, 1 == major).
 */
void ml_gc (msp, level)
    MLState_ptr     msp;
    int		    level;
{
    msp->ml_arg = ML_unit;

    switch (INT_MLtoC(level)) {
      case 0: callgc0 (msp, CAUSE_MINOR, 0, CONT_ARGS_MASK); break;
      default: callgc0 (msp, CAUSE_MAJOR, 0, CONT_ARGS_MASK); break;
    }

} /* end of ml_gc */

/* ml_enablesig : (int * bool) -> unit
 * This function is called by ML code to enable/disable a given signal.  If the
 * second argument is true, the the signal is enabled, otherwise disabled.
 */
void ml_enablesig (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    enable_sig (REC_SELINT(arg, 0), (REC_SEL(arg, 1) == ML_true));
    RETURN(msp, ML_unit);

} /* end of ml_enablesig. */

/* ml_masksigs : bool -> unit
 * Turn the masking of signals on and off.
 */
void ml_masksigs (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    msp->maskSignals = (arg == ML_true);
    RETURN (msp, ML_unit);

} /* end of ml_masksigs */

/* ml_sigpause : unit -> unit
 * Pause until the next signal.  Note, this must not restart the system call
 * on EINTR.
 */
void ml_sigpause (msp)
    MLState_ptr     msp;
{
    sigpause (0);
    RETURN(msp, ML_unit);

} /* end of ml_sigpause. */

#ifdef GETSTORELIST
/* ml_getstorelist : bool -> storelist
 */
void ml_getstorelist (msp, arg)
    MLState_ptr     msp;
    ML_val_t	    arg;
{
    int		   i;
    ML_val_t	    res;
    extern int	    preserving, store_preserve;
    extern ML_val_t uniq();

    callgc0 (msp, CAUSE_STORE, 0, CONT_ARGS_MASK);
    preserving = (arg != ML_false);
    res = uniq(store_preserve);
    store_preserve = (int)STORLST_nil;
    RETURN(msp, res);

} /* end of ml_getstorelist */
#endif


int icountM;

void ml_geticount (msp)
    MLState_ptr     msp;
{   int x;
    ML_val_t		res;
#ifdef ICOUNT
    x=(int)(msp->ml_icount);
    msp->ml_icount=0;
#else
    x=0;
#endif
    REC_ALLOC2 (msp, res,INT_CtoML(icountM), INT_CtoML(x));
    icountM=0;
    RETURN(msp, res);

}

/** The C function table **/

struct table_t {
    int		    tag;
    ML_val_t	    func;
    ML_val_t	    name;
    ML_val_t	    next;
    int		    stag;
    char	    str[16];
};

#define FUNCTION(ff,nn)				\
     {MAKE_DESC(3,tag_record),			\
     PTR_CtoML(ff),				\
     0, /* fill in later */			\
     0, /* fill in later */			\
     MAKE_DESC(sizeof(nn)-1,tag_string),	\
     nn}

struct table_t externlist0[] =
    {
/*                                  "xxxxxxxxxxxxxxxx" MAX NAME LENGTH (16) */
	FUNCTION (ml_syscall,	    "syscall"),
	FUNCTION (ml_open,	    "open"),
	FUNCTION (ml_connect_unix,  "connect_unix"),
	FUNCTION (ml_connect_inet,  "connect_inet"),
	FUNCTION (ml_link,	    "link"),
	FUNCTION (ml_wait_for_in,   "wait_for_in"),
	FUNCTION (ml_wait_for_out,  "wait_for_out"),
	FUNCTION (ml_read,	    "read"),
	FUNCTION (ml_readi,	    "readi"),
	FUNCTION (ml_write,	    "write"),
	FUNCTION (ml_writei,	    "writei"),
	FUNCTION (ml_writev,	    "writev"),
	FUNCTION (ml_send_obd,	    "send_obd"),
	FUNCTION (ml_getdirent,	    "getdirent"),
	FUNCTION (ml_readlink,	    "readlink"),
	FUNCTION (ml_truncate,	    "truncate"),
	FUNCTION (ml_chmod,	    "chmod"),
	FUNCTION (ml_access,	    "access"),
	FUNCTION (ml_getfid,	    "getfid"),
	FUNCTION (ml_getmod,	    "getmod"),
	FUNCTION (ml_ftype,	    "ftype"),
	FUNCTION (ml_getownid,	    "getownid"),
	FUNCTION (ml_fsize,	    "fsize"),
	FUNCTION (ml_atime,	    "atime"),
	FUNCTION (ml_ctime,	    "ctime"),
	FUNCTION (ml_mtime,	    "mtime"),
	FUNCTION (ml_isatty,	    "isatty"),
	FUNCTION (ml_select,	    "select"),
	FUNCTION (ml_pipe,	    "pipe"),
	FUNCTION (ml_fionread,	    "fionread"),
	FUNCTION (ml_system,	    "system"),
	FUNCTION (ml_exec,	    "exec"),
	FUNCTION (ml_argv,	    "argv"),
	FUNCTION (ml_environ,	    "environ"),
	FUNCTION (ml_gethostname,   "gethostname"),
	FUNCTION (ml_blast_out,	    "blas"),
	FUNCTION (ml_blast_in,	    "salb"),
	FUNCTION (ml_export,	    "export"),
	FUNCTION (ml_gettime,	    "gettime"),
	FUNCTION (ml_timeofday,	    "timeofday"),
	FUNCTION (ml_setitimer,	    "setitimer"),
	FUNCTION (ml_setglobal,	    "setg"),
	FUNCTION (ml_flush_icache,  "cach"),
	FUNCTION (ml_gc,	    "gc"),
	FUNCTION (ml_enablesig,	    "enablesig"),
	FUNCTION (ml_masksigs,	    "masksigs"),
	FUNCTION (ml_sigpause,	    "sigpause"),
	FUNCTION (ml_geticount,     "geticount"),
	FUNCTION (ml_acquire_proc,  "acquire_proc"),
	FUNCTION (ml_release_proc,  "release_proc"),
	FUNCTION (ml_spin_lock,     "spin_lock"),
        FUNCTION (ml_max_procs,     "max_procs"),
#ifdef GETSTORELIST
	FUNCTION (ml_getstorelist,  "getstorelist"),
#endif
/*                                  "xxxxxxxxxxxxxxxx" MAX NAME LENGTH (16) */
    };
#define NEXTERNS	(sizeof(externlist0)/sizeof(struct table_t))

/* init_externlist:
 * Initialize the extern list.
 */
void init_externlist ()
{
    int		    i;
    struct table_t  *p = (struct table_t *)INT_CtoML(0);

    for (i = NEXTERNS;  --i >= 0; ) {
	externlist0[i].next = PTR_CtoML(p);
	externlist0[i].name = PTR_CtoML(externlist0[i].str);
	p = (struct table_t *)&(externlist0[i].func);
    }

} /* end of init_externlist */
