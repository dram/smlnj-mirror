/* Copyright 1989 by AT&T Bell Laboratories */
/* linkdata.c -- group files into the text segment of a .o file */

#include <stdio.h>
#include <a.out.h>
#include <assert.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "tags.h"

#define SYMBOL "_mo_share"

#define LONG	    (sizeof(long))
#define align(x)    (((x) + (LONG - 1)) & ~(LONG - 1))

struct nlist nconst;

struct str_table {  /* The string table buffer. */
    int	    len;    /* length including this word */
    char    strs[16];
} str_tbl = {20,SYMBOL};

void die(s) char *s;
{   perror(s);
    exit(1);
}

extern long *malloc();	/* wrong but convenient */

/* copy:
 * append the code from file to output, preceded by name.
 */
int copy (file)
    char *file;
{
    int fd;
    long i, x, len, n, nbytes;
    char *s;
    struct stat st;
    static int bufsize = 0;
    static long *buf;

    if ((fd = open(file, 0)) == -1) {
	char	buf[64];
	sprintf(buf, "open: %s", file);
	die(buf);
    }
    if (fstat(fd, &st) == -1)
	die("stat");

    n = st.st_size;
    if (n==0) die("mo file is empty");
    len = strlen(file);
    nbytes = LONG + align(n) + LONG + align(len);

    if (nbytes > bufsize) {
	bufsize = nbytes + 4096;
	if (buf != NULL)
	    free(buf);
	buf = malloc(bufsize);
	if (buf == NULL)
	    die("malloc(buf)");
    }

    buf[0] = mak_desc(len, tag_string);	   /* tag for file name */
    strcpy(&buf[1], file);
    s = ((char *) &buf[1]) + len;
    for (i = align(len); i > len; i--)
	*s++ = 0;

    *((long *) s) = mak_desc(n, tag_string);  /* tag for code */
    s += LONG;

    for(x=n;x>0;)
      {i=read(fd,s,x);
       if (i<=0) die("read");
       s+=i; x-=i;
     }

    for (i = align(n); i > n; i--) *s++ = 0;

    if (close(fd) == -1) die("close");

    assert(s - nbytes == (char *) buf);
    
    s= (char*)buf;
    for(x=nbytes;x>0;)
      {i=write(1, s, x);
       if (i<=0) die ("write");
       x-=i; s+=i;
     }
    return nbytes;
}

int main(argc, argv)
    int argc;
    char *argv[];
{
    long pc = 0;
    struct exec exec;
    int i, sz;

  /* build the (one-entry) name list */
    nconst.n_other	= 0;
    nconst.n_desc	= 0;
    nconst.n_type	= N_EXT | N_TEXT;
    nconst.n_value = 0;
    nconst.n_un.n_strx = 4; /* datalist, first string table entry */

  /* initialize the header, we fill in the text size info later */
#ifdef M68
    exec.a_machtype = M_68020;
#endif
#ifdef SPARC
    exec.a_dynamic	= 0;
    exec.a_machtype	= M_SPARC;
    exec.a_toolversion	= 1;
#endif
    exec.a_magic    = NMAGIC;
    exec.a_data	    = 0;
    exec.a_bss	    = 0;
    exec.a_syms	    = sizeof(nconst);
    exec.a_entry    = 0;
    exec.a_drsize   = 0;
    exec.a_trsize   = 0;
    if (write(1, &exec, sizeof exec) == -1) die("write");

    for (i=1; i<argc; i++)
	pc += copy(argv[i], pc, i==argc-1);

    {int desc = mak_desc(0,tag_string);
     if (write(1, &desc, LONG) == -1) die("write(\"\")");
     pc += 4;
    }

    exec.a_text	    = pc;

  /* write out the symbol table. */
    if (write(1, &nconst, exec.a_syms) == -1) die("write(nl)");

  /* write out the string table */
    if (write(1, &str_tbl, str_tbl.len) == -1) die("write(str_tbl)");
    
  /* update the header to reflect the true text size */
    if (lseek(1, 0, 0L) == -1) die("lseek");
    if (write(1, &exec, sizeof exec) == -1) die("write(exec)");

    return 0;
}
