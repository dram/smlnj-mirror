/* Copyright 1989 by AT&T Bell Laboratories */
/* linkdata.c -- group files into the text segment of a .o file */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include "tags.h"

#ifdef MIPS
#define SYMBOL "mo_share"
#define MAKELONGWORD ".word"
#else
#define SYMBOL "_mo_share"
#define MAKELONGWORD ".long"
#endif

#define LONG	    (sizeof(long))
#define align(x)    (((x) + (LONG - 1)) & ~(LONG - 1))

void die(s) char *s;
{   perror(s);
    exit(1);
}

extern long *malloc();	/* wrong but convenient */

int getsize(file) char *file;
{ struct stat st;
  int n, len;
  if (stat(file, &st) == -1) die("stat");
  n = st.st_size;
  if (n==0) die("mo file is empty");
  len = strlen(file);
  return LONG + align(n) + LONG + align(len);
}

/* copy:
 * append the code from file to output, preceded by name.
 */
int copy (file,ofd)
    char *file; int ofd;
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

    s= (char*)buf;
    for(x=nbytes;x>0;)
      {i=write(ofd, s, x);
       if (i<=0) die ("write");
       x-=i; s+=i;
     }
}

int main(argc, argv)
    int argc;
    char *argv[];
{
    int i, j, pos, sz;
    int magicnum = 0x18273645;
    FILE *asfile, *ofile; int ofd;


    sz=0;
    for (i=1; i<argc; i++)
	sz += getsize(argv[i]);
    sz+=4;
    asfile=fopen("runtime/allmo.s","w");
    if (!asfile) die("fopen runtime/allmo.s");
    fprintf(asfile,".text\n.globl %s\n%s: %s %d\n.space %d\n",
	       SYMBOL,SYMBOL,MAKELONGWORD,magicnum,sz-4);
    fclose(asfile);
    system("as runtime/allmo.s -o runtime/allmo.o");
    ofile=fopen("runtime/allmo.o","r");
    if (!ofile) die("fopen runtime/allmo.o");

    i=0; pos= -LONG;
    while(i!=magicnum)
      {j=fread(&i,sizeof(i),1,ofile);
       pos+=LONG;
       if (!j) die("can't find magic num");
     }
    fclose(ofile);
    ofd=open("runtime/allmo.o",O_RDWR,0);
    if (ofd<0) die("open runtime/allmo.o");

    lseek(ofd,pos,0);

    for (i=1; i<argc; i++)
	copy(argv[i], ofd);

    {int desc = mak_desc(0,tag_string);
     if (write(ofd, &desc, LONG) == -1) die("write(\"\")");
    }

    return 0;
}
