#include "ml.h"
#include <a.out.h>
#ifdef VAX
#ifdef BSD
#include <machine/machparam.h>
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

extern int etext;   /* &etext is just beyond the end of the text segment */
extern int arenabase, arenasize, old_high, pagesize;
int isexport;       /* holds return value for Assembly.exportML */
extern int restart; /* points to the restart function */
int exportfile;     /* file descriptor to export to */

int *stackp;
int usrstack=USRSTACK; /* needed by prim */

#ifdef VAX
#define HEADERSIZE 1024
#define TEXTSTART USRTEXT
#define DATASTART (((int)&etext+pagesize-1)&~(pagesize-1))
#endif
#ifdef M68
#define HEADERSIZE (sizeof(struct exec))
#define TEXTSTART (USRTEXT+HEADERSIZE)
#define DATASTART (((int)&etext+SEGSIZ-1)&~(SEGSIZ-1))
#endif

export()
{/* warning: don't use register variables */
int localvar, edat, etex,filid;
#ifdef VAX
int i;
#endif

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

 /* make sure there is enough space above the heap for the stack;
    2000 is a random padding */
 while ( USRSTACK - (int)stackp + 2000 > arenabase+2*arenasize - old_high)
	getmore_increase();

 edat = (old_high + (USRSTACK - (int)stackp) + 2000);
 edat = (edat + pagesize - 1) & ~(pagesize-1);

#ifdef VAX
 for(i=0;i<1024;i++) B.zeros[i]=0;
#endif

 /* mysetjmp copies the stack and returns false.
  * restart restores the stack and returns here with true.
  */
 if (mysetjmp()) {isexport=ML_TRUE; setupsignals(); return;}
 isexport=ML_FALSE;

 etex = (((int)&etext)+pagesize-1) & ~(pagesize-1);

 B.E.a_magic = ZMAGIC;
 B.E.a_text = etex - USRTEXT;
 B.E.a_bss = arenabase+2*arenasize - edat;
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
 write(filid,&B,HEADERSIZE);
 write(filid,TEXTSTART,etex-TEXTSTART);
 write(filid,DATASTART,B.E.a_data);
}

