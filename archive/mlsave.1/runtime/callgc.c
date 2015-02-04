#include <signal.h>
#include <sys/types.h>

int specialgc;

int pagesize;
int arenabase;
int arenasize = 1024 * 300;
extern int freestart;
extern int (*ghandle)();
extern int bottom;

init_gc()
{
#ifdef V9
pagesize = 1024;
#endif
#ifdef BSD
pagesize = getpagesize();
#endif
#ifdef NOTDEF
 arenasize = ((arenasize + pagesize-1) / pagesize ) * pagesize;
 {int k;
  k = (int)sbrk(0);
  k = ((k + pagesize-1) / pagesize) * pagesize;
  if(brk(k) < 0) abort("brk failed\n");
  arenabase=k;
 }
 brk(arenabase + 2 * arenasize);
#endif
 arenabase=findbreak(sbrk(0));
 arenasize = (findbreak(arenabase+2*arenasize)-arenabase)/2;
 freestart = arenabase+arenasize;
}



#ifdef M68
#ifdef BSD
extern char before1[], after1[], before2[], after2[];

/* Segmentation bug on the m68:  an instruction can cause a segmentation
   violation but leave the pc pointing at the next instruction.  The
   offending instruction is never executed since Unix doesn't catch the
   error.  Because of the design of the compiler, this only happens in
   predictable places.  Two places in the runtime code, labelled
   before/after1 and before/after2 can cause it, and they are handled
   in callgc_m68bsd.  The bug is caused in compiled code only when consing
   a cell, and it occurs as the last element (first stored) is put into
   the cell.  The instruction is always a move from d0 indirectly through a6,
   possibly with a displacement, or a move from d6 (the special case of
   adding to the store list).

   The opcode for "movl d0,a6@" is binary 0010110010000000.
   The opcode for "movl d6,a6@" is binary 0010110010000110.
   The opcode for "movl d0,a6@(disp)" is binary 0010110101000000,
					followed by the word displacement.
   In addition, the 68020 (but not the 68000) supports longer than word
   displacements.  However, this is not used the current compiler.

   If the opcode pointed to by the pc is not an opcode like this, there has
   been a segmentation fault which has left the pc pointing at the next
   instruction.  In this case callgc_m68bsd backs up the pc until it points
   at the (hopefully) correct instruction.
   Possible problems:
	You could have two of these instructions in a row, and a fault on the
	    first.  This never happens in the current compiler.
	You could have a displacement which looks like a "movl d0,a6@"
	    instruction.  This displacement would be at least 8192, or 2000
	    elements in the cell.  There will be a special case to handle
	    large conses like this, so this particular case should never
	    turn up in code spit out by the current compiler.
*/
/* Wrong unless instruction is
 *  movl d0,a6@		(alloc fault)
 *  movl d0,a6@(disp)	(alloc fault)
 *  movl d6,a6@		(ref store)
 */
wrongopcode(opcode)
unsigned short opcode;
{
 return (opcode != 026200 && opcode != 026500 && opcode != 026206);
}


callgc_m68bsd(B,A)
	struct {int d0,d1,a0,a1,x0,x1,x2,x3,x4,sp,pc,psl} *A;
	struct {int d2[6],a2[5];} B;
{int *regptrs[17]; int i;
 regptrs[0] = &A->d0;
 regptrs[1] = &A->d1;
 regptrs[8] = &A->a0;
 regptrs[9] = &A->a1;
 for (i=0;i<6;i++) regptrs[i+2] = B.d2+i;
 for (i=0;i<5;i++) regptrs[i+10]= B.a2+i;
 regptrs[15] = &A->sp;
 regptrs[16] = &A->pc;
 if (!specialgc)	/* don't care if exporting */
 if (A->pc == (int)after2) A->pc= (int)before2;
 else  if (A->pc == (int)after1) A->pc= (int)before1;
 else  {if(wrongopcode(*(unsigned short *)(A->pc))) A->pc -= 2;
	if(wrongopcode(*(unsigned short *)(A->pc))) A->pc -= 2;
	if(wrongopcode(*(unsigned short *)(A->pc)))
	    die("Memory fault not related to garbage collection.\n");}
 callgc_m68(regptrs);
}
#endif

callgc_m68(regptrs) int **regptrs;
{unsigned short **pc = (unsigned short**)(regptrs[16]);
 int *roots[20]; int **rootsptr = roots;
 unsigned short opcode = (*pc)[0];
 int *storeptrs = (int*) (*regptrs[6]);

 *regptrs[6]=0;

 /* fix up in case faulted at a ref-store */
 if (opcode == 026206)  /* movl d6,a6@ */
    {static int a[2];
     a[1] = (int)storeptrs;
     a[0] = *regptrs[8];
     storeptrs = a+1; (*pc)+=6;  /* 6 words, that is */
    }
 else if ( opcode==026500   /* movl d0,a6@(n) */
        || opcode==026200 ) /* movl d0,a6@ */
			    /* are there other possibilities? */
	*rootsptr++ = regptrs[0];
 *rootsptr=0;
 callgc(*regptrs[15],pc,roots,regptrs[14],storeptrs);
}
#endif

#ifdef VAX
callgc_vaxbsd(regs0to5, regs6to11,regs12to15)
	int *regs0to5, *regs6to11, *regs12to15;
{int *regptrs[16]; int i;
 for(i=0;i<6;i++) regptrs[i] = regs0to5+i;
 for(i=0;i<6;i++) regptrs[i+6] = regs6to11+i;
  /* these next four are in a funny order */
  regptrs[12]=regs12to15+2;
  regptrs[13]=regs12to15+1;
  regptrs[14]=regs12to15+0;
  regptrs[15]=regs12to15+3;
 callgc_vax(regptrs);
}

callgc_vaxv9(fp) int *fp;
{int *regptrs[16]; int sp;
 sp = (int)(fp+28);
 regptrs[0] = fp+15;
 regptrs[1] = fp+16;
 regptrs[2] = fp+17;
 regptrs[3] = fp+18;
 regptrs[4] = fp+19;
 regptrs[5] = fp+20;
 regptrs[6] = fp+21;
 regptrs[7] = fp+5;
 regptrs[8] = fp+6;
 regptrs[9] = fp+7;
 regptrs[10]= fp+8;
 regptrs[11]= fp+9;
 regptrs[12]= fp+12;
 regptrs[13]= fp+13;
 regptrs[14]= &sp;
 regptrs[15]= fp+26;

 callgc_vax(regptrs);
}

callgc_vax(regptrs) int **regptrs;
{unsigned char **pc = (unsigned char**)(regptrs[15]);
 int *roots[20]; int **rootsptr = roots;
 unsigned char opcode = (*pc)[0], mode = (*pc)[1];
 int *storeptrs = (int*) (*regptrs[11]);

 *regptrs[11]=0;

 /* fix up in case faulted at a ref-store */
 if (opcode == 0xd0 && mode == 0x5b)
    {static int a[2];
     a[1] = (int)storeptrs;
     a[0] = *regptrs[1];
     storeptrs = a+1; (*pc)+=13;
    }
 else if ( (opcode == 0xd0 || opcode == 0xde)
	   && (mode>>4)>4 && (mode & 0xf) < 12 )
	*rootsptr++ = regptrs[(mode & 15)];
 *rootsptr=0;
 callgc(*regptrs[14],pc,roots,regptrs[12],storeptrs);
}
#endif

int getmore_die()
{die("bug: insufficient to_space\n");
}

int getmore_increase()
{arenasize += arenasize;
 chatting("doubling...");
 arenasize = (findbreak(arenabase+2*arenasize)-arenabase)/2;
 return arenabase+2*arenasize;
}

int old_high=0;

#define FANCY 1
#ifdef FANCY

int arstart=0;

callgc(sp,	    /* stack pointer at point of fault */
       pcptr,	    /* pc at point of fault (by ref) */
       misc_roots,  /* vector of ptrs to extra root words */
       arptr,	    /* place to put new freespace pointer */
       store_list   /* list of refs stored into */
      )
    int sp; int *pcptr; int ***misc_roots; int *arptr; int **store_list;
{int arend = arenabase+2*arenasize; int new_high;
 int amount_desired = arend-((*arptr) - 4);
 if (!old_high) old_high=arenabase;
 if (!arstart) arstart=arenabase+arenasize;
 chatting("\n[Minor collection...");
 starttime();
 gc(sp,bottom,
    arstart,arend,
    old_high,arstart,
    old_high,
    &new_high,
    misc_roots,pcptr,store_list,
    getmore_die);
 chatting("  %d%% used  (%d/%d), %d msec]\n",
	  (int)(100.0*((double)(new_high-old_high))/((double)(arend-arstart))),
	    (new_high-old_high),(arend-arstart), endtime() );
/*checkup(arstart,arend,old_high,new_high);*/
 if (specialgc || new_high > arenabase+arenasize)
    {int new_new_high;
     chatting("[Major collection...");
     starttime();
     gc(sp,bottom,
	arenabase,old_high,
	old_high,arenabase+2*arenasize,
	new_high,
	&new_new_high,
        misc_roots,pcptr,0,
        getmore_increase);
	 chatting("  %d%% used  (%d/%d), %d msec ]\n",
	  (int)(100.0*((double)(new_new_high-old_high))/((double)(new_high-arenabase))),
	    (new_new_high-old_high),(new_high-arenabase), endtime() );
    moveback(sp,bottom,
	     old_high,new_new_high,
	     arenabase,
              misc_roots,pcptr);
      old_high=arenabase+(new_new_high-old_high);
    }
  else {
	old_high=new_high;
 	if (amount_desired > (arend-new_high)/4) getmore_increase();
       }
  arend=arenabase+2*arenasize;
  arstart=(((arend+old_high)/2)+3)&(~3);
 (*arptr)= arstart+4;
  if (specialgc) {*pcptr = specialgc; specialgc=0;
		  export();}
#ifdef V9
 signal(SIGSEGV, ghandle);
#endif
}
#else

callgc(sp,	    /* stack pointer at point of fault */
       pcptr,	    /* pc at point of fault (by ref) */
       misc_roots,  /* vector of ptrs to extra root words */
       arptr,	    /* place to put new freespace pointer */
       store_list   /* list of refs stored into */
      )
    int sp; int *pcptr; int ***misc_roots; int *arptr; int **store_list;
{int arend = arenabase+2*arenasize; int new_high;
 int arstart = arenabase+arenasize;
 chatting("\n[Garbage collection...");
 gc(sp,bottom,
    arstart,arend,
    arenabase,arstart,
    arenabase,
    &new_high,
    misc_roots,pcptr,store_list,
    getmore_die);
 chatting("  %d%% used  (%d/%d) ]\n",
	  (int)(100.0*((double)(new_high-arenabase))/((double)(arend-arstart))),
	    (new_high-arenabase),(arend-arstart) );
 moveback(sp,bottom,
	     arenabase,new_high,
	     arstart,
              misc_roots,pcptr);
 (*arptr) = new_high+arenasize+4;
#ifdef V9
 signal(SIGSEGV, ghandle);
#endif
}

#endif


int *descriptor;

checkup(from_low,from_high,to_low,to_high)
int *from_low,*from_high,*to_low,*to_high;
{
 int *i,*j;

 chatting("Checking to_space\n");

 i = to_low;
 while (i < to_high) {
  descriptor = i;
 /* check descriptors */
  switch (*i & 7) {
   case 0: case 2: case 4: case 6:
   chatting("Unexpected even flag %d at %x in to_space\n",*i & 7,i);
   exit(0);
   break;
   case 1:
    j = i + (*i >> 3) + 1;
    while(++i < j) {
     if (*i & 1) continue;
     else if ((int *)*i > from_low && (int *)*i < from_high) {
	     chatting("Uncool pointer %x at %x into from_space\n",*i,i);
	     chatting("Descriptor is at %x\n",descriptor);
	  }
    }
   break;
   case 3:
   chatting("Unexpected flag 3 at %x in to_space\n",i);
   exit(0);
   break;
   case 5: /* skip past byte data */
    i += (*i >> 3) + 1;	/* works if no huge records; then high bit carries */
   break;
   case 7:
   chatting("Unexpected flag 7 at %x in to_space\n",i);
   exit(0);
   break;
  }
 }
}


#include <setjmp.h>

static int *breakloc;

jmp_buf jb;

foundbreak()
{longjmp(jb,1);
}

extern int (*ghandle)();

#ifndef sigmask
#define sigmask(m)  (1<<((m)-1))
#endif

findbreak(p) int p;
{
 p = (p + 0x1fff ) & ~0x1fff;
 brk(p-4);
 return p;
}
#ifdef NOTDEF
findbreak(p) int *p;
{int (*f)(); int oldmask;
 f=ghandle;
 ghandle=foundbreak;
 setupsignals();
 oldmask=sigsetmask(~sigmask(SIGSEGV));
 brk(p);
 breakloc=p;
 if (!setjmp(jb)) while (1) {breakloc++; *breakloc=0;}
 ghandle=f;
 setupsignals();
 sigsetmask(oldmask);
 return (int) breakloc;
}
#endif


#include <sys/time.h>
#include <sys/resource.h>

struct rusage r1, r2;

starttime()
{getrusage(0,&r1);}

endtime()
{getrusage(0,&r2);
 return ((r2.ru_utime.tv_usec-r1.ru_utime.tv_usec)/1000
	+ (r2.ru_utime.tv_sec-r1.ru_utime.tv_sec)*1000);
}
