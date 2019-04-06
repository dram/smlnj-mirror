#include "ml.h"
#include <a.out.h>
#ifdef VAX
#ifdef BSD
#ifdef ULTRIX
#include <machine/param.h>
#else
#include <machine/machparam.h>
#endif
#endif
#ifdef V9
#include <sys/param.h>
#endif
#endif
#ifdef M68
#include <machine/param.h>
#endif
#ifdef V9
#include <sys/vmparam.h>
#define USRTEXT 0
#else
#include <machine/vmparam.h>
#endif

#define CEIL(x,quantum) ((((int)(x))+(quantum)-1)&~((quantum)-1))

extern int etext;   /* &etext is just beyond the end of the text segment */
extern int arenabase, arenasize, old_high, pagesize;
extern float gamma;
extern int lastbreak;
int isexport;       /* holds return value for Assembly.exportML */
extern int restart; /* points to the restart function */
int exportfile;     /* file descriptor to export to */

int *stackp;
int usrstack=USRSTACK; /* needed by prim */

#ifdef VAX
#define HEADERSIZE 1024
#define TEXTSTART USRTEXT
#define DATASTART CEIL(&etext,pagesize)
#endif
#ifdef M68
#define HEADERSIZE (sizeof(struct exec))
#define TEXTSTART (USRTEXT+HEADERSIZE)
#define DATASTART CEIL(&etext,SEGSIZ)
#endif

static int edat, etex, filid, i;

export()
{/* warning: don't use local variables */
int localvar; /* except this one */

union {struct exec E;
#ifdef VAX
       char zeros[1024];
#endif
	} B;

  filid = exportfile >> 1;
/* Garbage collection is already done.
 * Data to be saved is:
 *  0 -> ceil(etext)             text
 *  ceil(etext) -> arenabase     data
 *  arenabase -> old_high        heap
 *  "sp" -> USRSTACK             stack
 *	
 *  > copy stack on top of old_high
 *  > set a_entry as address of restart procedure
 */

 stackp = &localvar; /* stack up to where export was called */

 edat = (old_high + (USRSTACK - (int)stackp) + 1500);
 edat = CEIL(edat,pagesize);

#ifdef VAX
 for(i=0;i<1024;i++) B.zeros[i]=0;
#endif

 /* mysetjmp copies the stack and returns false.
  * restart restores the stack and returns here with true.
  */
 if (mysetjmp()) {lastbreak=edat;
		  arenasize = CEIL((int)((old_high-arenabase)*gamma),pagesize);
		  arenasize= findbreak(arenabase+arenasize) - arenabase;
		  isexport=ML_TRUE; setupsignals(); return;}
 isexport=ML_FALSE;

 etex = CEIL(&etext,pagesize);

 B.E.a_magic = ZMAGIC;
 B.E.a_text = etex - USRTEXT;
 B.E.a_bss = 0;
 B.E.a_syms = 0;
 B.E.a_entry = restart;
 B.E.a_trsize = 0;
 B.E.a_drsize = 0;
#ifdef M68
 B.E.a_machtype = M_68020;
 B.E.a_data = edat - DATASTART;
#endif
#ifdef VAX
 B.E.a_data = edat - B.E.a_text;
#endif

 fchmod(filid,0755);
 bulletproofWrite(filid,&B,HEADERSIZE);
 bulletproofWrite(filid,TEXTSTART,etex-TEXTSTART);
 bulletproofWrite(filid,DATASTART,B.E.a_data);
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
      {
	 i = write(fid, buf, total-bytesWritten);
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
