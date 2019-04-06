/* cfuns.c   (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories
 *
 * These are the C functions that are callable from ML (via REQ_CALLC).
 * 
 * Altered 20 Dec. 1991 by:  Yngvi S. Guttesen  
 *                           Department of Computer Science
 *                           The Technical University of Denmark
 *                           DK-2800 Lyngby
 *
 */

#include "sml.h"
#include "ml_os.h"
#include "errno.h"
#include "ml_state.h"
#include "ml_types.h"
#include "cause.h"
#include "stdlib.h"

/* return a value to the calling ML code */
#define RETURN(r)	{		\
    MLState->ml_arg = (r);		\
    return;}

/* return sts to the calling ML code, but raise an exception if an error occured */
#define CHK_RETURN(sts)	{		\
    if (sts == -1) raise_syserror(0);	\
    else RETURN(INT_CtoML(sts)) }

extern int console_io(int, LPSTR, int) ;

int isatty(int fd)
{   if (fd>=0 && fd<=2)
        return 1;
    else
        return 0 ;
}

/* raise_syserror:
 * Raise the ML exception SysError with the errno and error string as the argument
 * pair.  If alt_msg is non-zero, then use it as the error string and use -1 as the
 * errno.
 */
void raise_syserror (alt_msg)
    char *          alt_msg;
{
    EXTASM(syserror_id0)
    ML_val_t	    s, arg, exn;

    if (alt_msg != 0) {
	s = ML_alloc_string (alt_msg);
	errno = -1;
    }
    else if ((0 <= errno) && (errno < sys_nerr))
	s = ML_alloc_string (sys_errlist[errno]);
    else {
	char		buf[32];
	sprintf(buf, "<unknown error %d>", errno);
	s = ML_alloc_string (buf);
    }

    arg = REC_ALLOC2 (INT_CtoML(errno), s);
    exn = REC_ALLOC2 (arg, PTR_CtoML(LOWORD(syserror_id0)+4));

    raise_ml_exn (exn);
}


#define MAX_SYSCALL_ARGS	6

/* ml_syscall : (int * string list) -> int
 * Perform the requested system call with the given arguments.  Unboxed
 * values are converted to C ints.
 */
void ml_syscall (arg)
    ML_val_t        arg;
{
    long code = REC_SELINT(arg,0) ;
    ML_val_t p =REC_SEL(arg,1) ;
    int fd;
    long r;

    if (code=6) {
        fd = INT_MLtoC(ML_hd(p));
        r = _lclose(fd);
    }
    else {
        raise_syserror ("bad syscall !");
        return;
    }

    CHK_RETURN(r);

} /* end of ml_syscall. */

void unixtodos(LPSTR s)
{
    for( ; *s != '\0' ; s++)
        if (*s == '/')
            *s = '\\';
}

/* ml_open : (string * int) -> int
 * Open a file and return the file descriptor.
 */
void ml_open (arg)
    ML_val_t	    arg;
{
    LPSTR           fnm ;
    char            s[128];
    LPSTR           newfnm = s;
    ML_val_t        path = REC_SELPTR(arg, 0) ;
    ML_val_t        mode = REC_SELINT(arg, 1);
    int             fd, flags;
    OFSTRUCT        dummy ;

    Global16PointerAlloc(wsUse32Data, path,(LPDWORD) &fnm, 1024, 0) ;

    lstrcpy(newfnm, fnm);
    unixtodos(newfnm);

    switch (mode) {
      case 0: flags = (OF_READ); break;
      case 1: flags = (OF_WRITE | OF_CREATE); break;
      case 2: flags = (OF_READWRITE); break;
    }
    fd = OpenFile(newfnm, &dummy, flags);
    Global16PointerFree(wsUse32Data, (DWORD) fnm, 0) ;

    CHK_RETURN (fd);

} /* end of ml_open */

void ml_connect_unix(arg)
    ML_val_t arg ;
{
    raise_syserror(" connect_unix not implemented under WINDOWS !") ;
    return;
}

void ml_connect_inet(arg)
    ML_val_t arg ;
{
    raise_syserror(" connect_inet not implemented under WINDOWS !") ;
    return;
}


void ml_link(arg)
    ML_val_t arg ;
{
    raise_syserror(" link not implemented under WINDOWS !") ;
    return;
}



/* ml_wait_for_in : fd -> unit
 * Wait for input on the given file descriptor.
 */
void ml_wait_for_in (arg)
    ML_val_t	    arg;
{
    RETURN(ML_unit);
} /* end of ml_wait_for_in. */

/* ml_wait_for_out : fd -> unit
 * Wait for output on the given file descriptor.
 */
void ml_wait_for_out (arg)
    ML_val_t	    arg;
{
    RETURN(ML_unit);
} /* end of ml_wait_for_out. */

#define BLK_SZ 512


/* ml_read : (int * bytearray * int) -> int
 * Read data from the specified file into the given bytearray.  Return the
 * number of bytes read.
 */
void ml_read (arg)
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    ML_val_t        buf = REC_SELPTR(arg, 1);
    long            nbytes = REC_SELINT(arg, 2) ;
    long            n, a=0;
    LPSTR           arr ;
    int             console = FALSE;

    if ((fd>=0) && (fd<=2))
        console = TRUE;

    while (nbytes >= BLK_SZ) {
        Global16PointerAlloc(wsUse32Data, buf,(LPDWORD) &arr, BLK_SZ, 0) ;
        if (console)
            n = console_io(fd, arr, BLK_SZ);
        else
            n = _lread(fd, arr, BLK_SZ);
        Global16PointerFree(wsUse32Data, (DWORD)arr, 0) ;
        if (n<0)            /* error */
            CHK_RETURN(n) ;
        if (n < BLK_SZ)     /* EOF */
            CHK_RETURN( n+a ) ;
        a += BLK_SZ ;
        buf += BLK_SZ ;
        nbytes -= BLK_SZ ;
    }
    Global16PointerAlloc(wsUse32Data, buf, (LPDWORD)&arr, BLK_SZ, 0) ;
    if (console)
        n = console_io(fd, arr, (int) nbytes);
    else
        n = _lread(fd, arr, (WORD)nbytes);
    Global16PointerFree(wsUse32Data, (DWORD)arr, 0) ;
    if (n<0)            /* error */
        CHK_RETURN(n) ;
    CHK_RETURN( a+n );

} /* end of ml_read */

/* ml_readi : (int * bytearray * int * int) -> int
 * Read data from the specified file into the given bytearray, starting at
 * offset.  Return the number of bytes read.
 */
void ml_readi (arg)
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    ML_val_t        start = REC_SELPTR(arg, 1);
    ML_val_t        buf = start + REC_SELPTR(arg,2) ;
    long            nbytes = REC_SELINT(arg, 3) ;
    long            n, a=0;
    LPSTR           arr ;
    int             console= FALSE;

    if ((fd>=0) && (fd<=2))
        console = TRUE;

    while (nbytes >= BLK_SZ) {
        Global16PointerAlloc(wsUse32Data, buf, (LPDWORD)&arr, BLK_SZ, 0) ;
        if (console)
            n = console_io(fd, arr, BLK_SZ);
        else
            n = _lread(fd, arr, BLK_SZ);
        Global16PointerFree(wsUse32Data, (DWORD)arr, 0) ;
        if (n<0)            /* error */
            CHK_RETURN(n) ;
        if (n < BLK_SZ)     /* EOF */
            CHK_RETURN( n+a ) ;
        a += BLK_SZ ;
        buf += BLK_SZ ;
        nbytes -= BLK_SZ ;
    }
    Global16PointerAlloc(wsUse32Data, buf, (LPDWORD)&arr, BLK_SZ, 0) ;
    if (console)
        n = console_io(fd, arr, (int) nbytes);
    else
        n = _lread(fd, arr, (WORD)nbytes);
    Global16PointerFree(wsUse32Data, (DWORD)arr, 0) ;
    if (n<0)            /* error */
        CHK_RETURN(n) ;
    CHK_RETURN( a+n );

} /* end of ml_readi */


/* write_all:
 * Write the requested number of bytes from the buffer.  Return 0 on success,
 * and -1 on errors.
 */
static int write_all (int fd, ML_val_t buf, long nbytes)
{
    long            n, a=0;
    LPSTR           arr ;
    int             console = FALSE;

    if ((fd>=0) && (fd<=2))
        console = TRUE;
    while (nbytes >= BLK_SZ) {
        Global16PointerAlloc(wsUse32Data, buf, (LPDWORD)&arr, BLK_SZ, 0) ;
        if (console)
            n = console_io(fd, arr, BLK_SZ);
        else
           n = _lwrite(fd, arr, BLK_SZ);
        Global16PointerFree(wsUse32Data, (DWORD)arr, 0) ;
        if (n<0)            /* error */
            return(-1) ;
        a += n ;
        buf += n ;
        nbytes -= n ;
    }
    while (nbytes > 0) {
        Global16PointerAlloc(wsUse32Data, buf, (LPDWORD)&arr, BLK_SZ, 0) ;
        if (console)
            n = console_io(fd, arr, (int) nbytes);
        else
            n = _lwrite(fd, arr, (WORD)nbytes);
        Global16PointerFree(wsUse32Data, (DWORD)arr, 0) ;
        if (n<0)            /* error */
            return(-1) ;
        a += n ;
        buf += n ;
        nbytes -= n ;
    }
    return(0) ;


} /* end of write_all. */

/* ml_write : (int * bytearray * int) -> unit
 * Write data from the given bytearray to the specified file.  Return the
 * number of bytes written.
 */
void ml_write (arg)
    ML_val_t	    arg;
{
    int		    sts;

    sts = write_all (
            (int) REC_SELINT(arg, 0),
            REC_SELPTR(arg, 1),
	    REC_SELINT(arg, 2));

    if (sts == -1)
	raise_syserror(0);
    else
	RETURN(ML_unit);

} /* end of ml_write */

/* ml_writei : (int * bytearray * int * int) -> unit
 * Write data from the given bytearray to the specified file, starting at the
 * given offset.  This routine is guaranteed to write all the bytes.
 */
void ml_writei (arg)
    register ML_val_t arg;
{
    int		    sts;

    sts = write_all (
            (int) REC_SELINT(arg, 0),
            REC_SELPTR(arg, 1) + REC_SELINT(arg, 2),
	    REC_SELINT(arg, 3));

    if (sts == -1)
	raise_syserror(0);
    else
	RETURN(ML_unit);

} /* end of ml_writei */


struct iovec {
    ML_val_t        iov_base;
    long            iov_len;
};

/* write_multiple:
 * Write a vector of blocks and return the number of blocks written.  Normally,
 * this will be iovcnt, but if a signal occurs during the write, then it can
 * be less.  Return -1 on error.
 */
static long write_multiple (fd, iov, iovcnt, nbytes)
    int		    fd;
    struct iovec    *iov;
    long            iovcnt, nbytes;
{
    int             sts;
    long            i;

    for (i = 0;  i < iovcnt;  i++) {
        if ((sts = write_all (fd, iov[i].iov_base, iov[i].iov_len)) == -1)
	    return -1;
	else
            i++;                /* ######## WRONG ######### */
    }

    return iovcnt;

} /* end of write_multiple */

#define WRITEVEC_SZ	8

/* ml_writev : (int * (bytearray * int) list) -> unit
 * For each (data, len) element in the list, write the len number of bytes to the
 * file descriptor.
 */
void ml_writev (arg)
    ML_val_t	    arg;
{
    int		    fd = REC_SELINT(arg, 0);
    ML_val_t	    p = REC_SEL(arg, 1), q = p;
    long            nbytes = 0, i, n;
    struct iovec    vec[WRITEVEC_SZ];

    nbytes = 0;
    for (i = 0; p != ML_nil;  ) {
	ML_val_t    pair = ML_hd(p);

	p = ML_tl(p);
        vec[i].iov_base = REC_SELPTR(pair, 0);
	vec[i].iov_len  = REC_SELINT(pair, 1);
	nbytes += vec[i].iov_len;
	if ((++i == WRITEVEC_SZ) || (p == ML_nil)) {
	    if ((n = write_multiple(fd, vec, i, nbytes)) < 0)
		return; /* error */
	    else if (n < i) {
	      /* a signal occurred, so set things up so that the resume continuation
	       * will complete the write operation.
               */
                BRSP(callc_v) ;

                while (n>0) {
                    q = ML_tl(q) ; n-- ;
                }
                arg = REC_ALLOC2(INT_CtoML(fd), q) ;
                MLState->ml_arg = REC_ALLOC2(PTR_CtoML(LOWORD(ml_writev)), arg);
                MLState->ml_closure = PTR_CtoML(LOWORD(callc_v)) ;
                MLState->ml_pc      = CODE_ADDR(PTR_CtoML(LOWORD(callc_v)));
                return ;
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
void ml_send_obd (arg)
    ML_val_t	    arg;
{
    raise_syserror ("ml_send_obd not implemented under WINDOWS !");
    return ;

} /* end of ml_send_obd */


/* ml_getdirent : int -> string list
 * Get directory entries from the directory referenced by fdesc.  If there are
 * no more entries, then return nil.
 */
void ml_getdirent (arg)
    ML_val_t	    arg;
{
    /* We cannot implement this function under WINDOWS, unless we make it a
     * function of FileName -> string list
     */

    raise_syserror("ml_getdirent not implemented under WINDOWS !");
    return ;

} /* end of ml_getdirent */


/* ml_readlink : string -> string
 * Read the contents of the specified symbolic link.
 */
void ml_readlink (arg)
    ML_val_t	    arg;
{
    raise_syserror("readlink not implemented under WINDOWS !");
    return ;

} /* end of ml_readlink */


/* ml_truncate : (fd or string * int) -> unit
 * Truncate the specified file to the specified length.
 */
void ml_truncate (arg)
    ML_val_t	    arg;
{
    ML_val_t    fd  = REC_SELINT(arg, 0);
    ML_val_t    len = REC_SELINT(arg, 1);
    int         sts;

    if (OBJ_isBOXED(fd)) {
        raise_syserror("ml_truncate need a file handle !");
        return;
    }
    else
        sts = chmod(fd, len);

    CHK_RETURN(sts);

} /* end of ml_truncate */


/* ml_chmod : (fd or string * int) -> unit
 * Change the protection mode of the specified file.
 */
void ml_chmod (arg)
    ML_val_t	    arg;
{
    ML_val_t        fn   = REC_SELPTR(arg, 0);
    LPSTR           fp ;
    int		    mode = REC_SELINT(arg, 1);
    int		    sts;

    if (OBJ_isBOXED(fn)) {
        Global16PointerAlloc(wsUse32Data, fn, (LPDWORD)&fp, 1024, 0) ;
        sts = chmod(PTR_MLtoC(fp), mode);
        Global16PointerFree(wsUse32Data, (DWORD)fp, 0) ;
    }
    else {
        raise_syserror("chmod need a filename !") ;
        return ;
    }

    CHK_RETURN(sts);

} /* end of ml_chmod */


/* ml_access : (string * int list) -> bool
 * Check to see if the user has the specified access to the specified file.
 */
void ml_access (arg)
    ML_val_t	    arg;
{
    raise_syserror ("access not implemented under WINDOWS !");
    return ;

} /* end of ml_access. */

/* stat_file:
 * Get the file status of f.  The file can either be specified as a path, in which
 * case f will be a boxed ML string, otherwise f will be an unboxed file descriptor.
 */
static int stat_file (f, buf)
    ML_val_t	    f;
    struct stat	    *buf;
{
    raise_syserror ("stat_file not implemented under WINDOWS !");
    return (0);

} /* end of stat_file */

/* ml_getfid : (fd or string) -> fileid
 * Return the unique file id (a string created from the device and inode of the
 * file) of the specified file.
 */
void ml_getfid (f)
    ML_val_t	    f;
{
    raise_syserror ("getfid not implemented under WINDOWS !");
    return ;

} /* end of ml_getfid */

/* ml_getmod : (fd or string) -> int
 * Return the file protection mode of the file specified by f.
 */
void ml_getmod (f)
    ML_val_t	    f;
{
    raise_syserror ("getmod not implemented under WINDOWS !");
    return ;

} /* end of ml_getmod */

/* ml_ftype : (fd or string) -> int
 * Return the file type of the file specified by f.  The return values must
 * track those in System.Unsafe.FileIO (see "boot/perv.sml").
 */
void ml_ftype (f)
    ML_val_t	    f;
{
    raise_syserror ("ftype not implemented under WINDOWS !");
    return ;

} /* end of ml_ftype */

/* ml_getownid : (fd or string) -> (int * int)
 * Return the user and group ids of the specified file.
 */
void ml_getownid (f)
    ML_val_t	    f;
{
    raise_syserror ("getownid not implemented under WINDOWS !");
    return ;

} /* end of ml_getownid */

/* ml_fsize : (fd or string) -> int
 * Return the size in bytes of the specified file.
 */
void ml_fsize (f)
    ML_val_t	    f;
{
    raise_syserror ("fsize not implemented under WINDOWS !");
    return ;

} /* end of ml_fsize */

/* ml_atime : (fd or string) -> (int * int)
 * Get the most recent access time of the specified file.
 */
void ml_atime (f)
    ML_val_t	    f;
{
    raise_syserror ("atime not implemented under WINDOWS !");
    return ;

} /* end of ml_atime */

/* ml_ctime : (fd or string) -> (int * int)
 * Get the creation time of the specified file.
 */
void ml_ctime (f)
    ML_val_t	    f;
{
    raise_syserror ("ctime not implemented under WINDOWS !");
    return ;

} /* end of ml_ctime */

/* ml_mtime : (fd or string) -> (int * int)
 * Get the most recent modification time of the specified file.
 */
void ml_mtime (f)
    ML_val_t	    f;
{
    raise_syserror ("mtime not implemented under WINDOWS !");
    return ;

} /* end of ml_mtime */


/* ml_isatty : int -> bool
 * Return true if the file descriptor fd refers to a tty device.
 */
void ml_isatty (fd)
    ML_val_t	    fd;
{
    RETURN (isatty(INT_MLtoC(fd)) ? ML_true : ML_false);

} /* end of ml_isatty */



/* ml_select : (int list * int list * int list * (int * int))
 *                 -> (int list * int list * int list)
 * Check file descriptors for the readiness of I/O operations.
 */
void ml_select (arg)
    ML_val_t	    arg;
{
    raise_syserror ("select not implemented under WINDOWS !");
    return ;

} /* end of ml_select */


/* ml_pipe : unit -> (int * int)
 * Create a pipe and return its input and output descriptors.
 */
void ml_pipe ()
{
    raise_syserror ("pipe not implemented under WINDOWS !");
    return ;

} /* end of ml_pipe. */


/* ml_fionread : int -> int
 * Return the number of bytes available for reading in the given file.
 */
void ml_fionread (arg)
    int		arg;
{
    int fd = INT_MLtoC(arg);
    long pos, size ;

    if (((pos = _llseek(fd,0,1)) < 0) || ((size = _llseek(fd,0,2)) < 0))
        raise_syserror(0);
    else {
        _llseek(fd,pos,0);
        RETURN(INT_CtoML(size-pos));
    }

} /* end of ml_fionread */


/* ml_system : string -> int
 * Issue the given shell command and return the exit status.
 */
void ml_system (arg)
    ML_val_t	arg;
{
    raise_syserror ("system not implemented under WINDOWS !");
    return ;

} /* end of ml_system */


/* ml_exec : (string * string list * string list) -> (int * int)
 * Fork a process to execute the given command, with the given command-line
 * arguments and environment.  Return the file descriptors for pipes
 * connected to the process' stdin and stdout. 
 */
void ml_exec (arg)
    ML_val_t	    arg;
{
    raise_syserror ("exec not implemented under WINDOWS !");
    return ;

} /* end of ml_exec */


extern ML_val_t make_str_list();

/* ml_argv : unit -> string list
 * Return the command-line argument list.
 */
void ml_argv ()
{
    raise_syserror ("argv not implemented under WINDOWS !");
    return ;

} /* end of ml_argv */

/* ml_envrion : unit -> string list
 * Return the environment list.
 */
void ml_environ ()
{
    raise_syserror ("environ not implemented under WINDOWS !");
    return ;

} /* end of ml_environ */

/* ml_gethostname : unit -> string
 * Return the name of our host.
 */
void ml_gethostname ()
{
    raise_syserror ("gethostname not implemented under WINDOWS !");
    return ;

} /* end of ml_gethostname */


static int	blast_fd;	/* the file descriptor to blast to */

/* ml_blast_out : (int * 'a) -> 'a
 */
void ml_blast_out (arg)
    ML_val_t	    arg;
{
    blast_fd	    = REC_SELINT(arg, 0);
    MLState->ml_arg = REC_SEL(arg, 1);
    callgc0 (CAUSE_BLAST, STD_ARGS_MASK);

} /* end of ml_blast_out */

/* blast_write:
 */
void blast_write (start, end, ptr)
    ML_val_t      start, end, ptr;
{
    raise_syserror ("blast_write not implemented under WINDOWS !");
    return ;

} /* end of blast_write */

/* ml_blast_in : string -> 'a
 * Build an object from the string.  The string has a special header (produced
 * by blast_write).
 */
void ml_blast_in (arg)
    ML_val_t	    arg;
{
    raise_syserror ("blast_in not implemented under WINDOWS !");
    return ;

} /* end of blast_in */


/* ml_export : int -> bool
 * Export the world to the given file and return false (the exported version
 * returns true).
 */
void ml_export (arg)
    ML_val_t	    arg;
{
    raise_syserror ("export not implemented under WINDOWS !");
    return ;

} /* end of ml_export */


/* ml_gettime : unit -> (int * int * int * int * int * int)
 * Return the total CPU time, system time and garbage collection time used by this
 * process so far.
 */
void ml_gettime ()
{
    ML_val_t res ;
    res = REC_ALLOC6(1,1,1,1,1,1) ;
    RETURN(res) ;
} /* end of ml_gettime */


/* ml_timeofday : unit -> (int * int)
 * Return the time of day.
 */
void ml_timeofday ()
{
    raise_syserror ("timeofday not implemented under WINDOWS !");
    return ;

} /* end of ml_timeofday. */


/* ml_setitimer : (int * int * int * int * int) -> int
 * Set an interval timer; the first argument specifies which timer.
 */
void ml_setitimer (arg)
    ML_val_t	    arg;
{
    raise_syserror ("setitimer not implemented under WINDOWS !");
    return ;

} /* end of ml_setitimer */


/* ml_setglobal : int array -> unit
 */
void ml_setglobal (p)
    ML_val_t	    p;
{
    raise_syserror ("setglobal not implemented under WINDOWS !");
    return ;

} /* end of ml_setglobal */

/* ml_flush_icache : string -> unit
 * Flush the instruction cache for the range covered by s.  This is a no-op
 * on most machines.
 */
void ml_flush_icache (s)
    ML_val_t		s;
{
    return ;
}


/* ml_gc : int -> unit
 * Force a garbage collection of the specified level (0 == minor, 1 == major).
 */
void ml_gc (level)
    int		    level;
{
    register int    i;

    MLState->ml_arg = ML_unit;

    switch (INT_MLtoC(level)) {
      case 0: callgc0 (CAUSE_MINOR, STD_ARGS_MASK); break;
      default: callgc0 (CAUSE_MAJOR, STD_ARGS_MASK); break;
    }

} /* end of ml_gc */

/* ml_enablesig : (int * bool) -> unit
 * This function is called by ML code to enable/disable a given signal.  If the
 * second argument is true, the the signal is enabled, otherwise disabled.
 */
void ml_enablesig (arg)
    ML_val_t	    arg;
{
//    raise_syserror ("enablesig not implemented under WINDOWS !");
    RETURN (ML_unit);

} /* end of ml_enablesig. */

/* ml_masksigs : bool -> unit
 * Turn the masking of signals on and off.
 */
void ml_masksigs (arg)
    ML_val_t	    arg;
{
//    raise_syserror ("masksigs not implemented under WINDOWS !");
    RETURN (ML_unit);

} /* end of ml_masksigs */

/* ml_getstorelist : bool -> storelist
 */
void ml_getstorelist (arg)
    ML_val_t	    arg;
{
    raise_syserror ("getstorelist not implemented under WINDOWS !");
    return ;

} /* end of ml_getstorelist */



/* init_externlist:
 * Initialize the extern list.
 */
void init_externlist ()
{
} /* end of init_externlist */
