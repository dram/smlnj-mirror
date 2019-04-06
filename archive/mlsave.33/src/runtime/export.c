#include "ml.h"
#include <a.out.h>

#define CEIL(x,quantum) ((((int)(x))+(quantum)-1)&~((quantum)-1))

#ifndef N_DATADDR
#define N_DATADDR(x)  CEIL((x).a_text,getpagesize())
#endif

#ifndef N_TXTADDR
#ifdef NeXT
#include <machine/vm_param.h>
#define N_TXTADDR(x)  USRTEXT
#endif
#ifdef VAX
#define N_TXTADDR(x)  0
#endif
#ifdef M68
#define N_TXTADDR(x)  getpagesize()
#endif
#endif


 
/* Garbage collection is already done.
 * Data to be saved is:
 *  0 -> ceil(etext)             text
 *  ceil(etext) -> arenabase     data
 *  arenabase -> old_high        heap
 *	
 *  > set a_entry as address of start procedure
 */

extern int etext;   /* &etext is just beyond the end of the text segment */
extern int old_high;

static int textstart,datastart;


extern int startptr;
#ifdef V9
getpagesize(){return 1024;}
#endif
export(filid) int filid;
{static struct exec E;  /* make it static so all fields=0 */
#ifdef SUN3
 E.a_magic = NMAGIC;
#else
 E.a_magic = ZMAGIC;
#endif
#ifdef M68
 E.a_machtype = 2;	/* M_68020 */
#else
#endif
 textstart = N_TXTADDR(E);
 E.a_text = (int) CEIL(((int)&etext),getpagesize())-textstart;
 datastart = N_DATADDR(E);
 E.a_bss = 0;
 E.a_syms = 0;
 E.a_entry = startptr;
 E.a_trsize = 0;
 E.a_drsize = 0;
 E.a_data = CEIL(old_high-datastart, getpagesize());

 filid >>= 1;
 fchmod(filid,0755);
 bulletproofWrite(filid,&E,sizeof(E));
#ifdef VAX
 {int i, nzeros = getpagesize()-sizeof(E);
  char zeros[1024];
  for(i=0;i<nzeros;i++) zeros[i]=0;
  bulletproofWrite(filid,zeros,nzeros);
 }
#endif
 bulletproofWrite(filid,textstart,E.a_text);
 bulletproofWrite(filid,datastart,E.a_data);
}


/* NICK: A Bullet-proof write to retry on NFS systems */
/* JHR: Added retry for NFS timeout errors. */
#include <errno.h>
bulletproofWrite(fid, buf, total)
int fid;
char *buf;
int total;
{
   int bytesWritten = 0;
#ifdef M68
   int retries = 0;
#endif
   int i;
   do
      {  i = write(fid, buf, total-bytesWritten);
#ifdef M68
     if (i < 0) {
        if (errno == ETIMEDOUT) {
           /* NFS timeout error, so try again. */
           if (retries++ > 5)
              die("export, NFS timeout");
           chatting("[Write timeout, retrying]\n");
           continue;
        }
        else die("export");
     }
     else retries = 0;
#else
	 if (i < 0) die("export");
#endif
	 bytesWritten += i;
	 buf += i;
	 if (bytesWritten < total)
	    chatting("[Write incomplete (%d/%d), retrying]\n",
		     bytesWritten, total);
      }
   while (bytesWritten < total);
}
