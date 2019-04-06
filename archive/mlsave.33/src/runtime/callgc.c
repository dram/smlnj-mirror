#include <signal.h>
#include <sys/types.h>

#include "tags.h"
#include "descriptor.h"
#include "ml.h"

#define refcell(z) int z[2] = {mak_desc(1,tag_array), ML_INT(0)};

refcell(collected0)
refcell(collectedfrom0)
refcell(current0)
refcell(gcmessages0)
refcell(majorcollections0)
refcell(minorcollections0)
refcell(pstruct0)
refcell(ratio0)
refcell(softmax0)

#define collected (collected0[1])
#define collectedfrom (collectedfrom0[1])
#define current (current0[1])
#define gcmessages (gcmessages0[1])
#define majorcollections (majorcollections0[1])
#define minorcollections (minorcollections0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])

int cause, fault_code;
int bottom;                 /* saved stack-pointer */
int fpsave;		    /* saved frame pointer */
int saved_pc,saved_dataptr,saved_exnptr,saved_storeptr,saved_limit,
    saved_ptrs[32], saved_nonptrs[32];

extern char saveregs;
extern char handle_c, return_c;
apply(func,arg) int func, arg;
{int i;
    saved_exnptr = (int) &handle_c;
    saved_ptrs[0] = arg;
    saved_ptrs[1] = (int) &return_c;
    saved_ptrs[2] = func;
    saved_pc = *(int*)func;
    for(i=3;i<32;i++) saved_ptrs[i]=0;
    runML();
    return saved_ptrs[0];
}

int exportFilid;

runML()
{int i;
 for(;;)
      {cause=0;
       restoreregs();
       switch(cause)
	{case CAUSE_EXN: return uncaught(saved_ptrs[0]);
	 case CAUSE_GC:  callgc0(); break;
	 case CAUSE_RET: return;
	 case CAUSE_EXPORT: saved_pc=0;
			    for(i=2;i<32;i++) saved_ptrs[i]=0;
			    callgc0(); callgc0();
			    saved_pc = *(int*)saved_ptrs[1];
			    export(exportFilid);
			    saved_ptrs[0]=1;
			    break;
	 case CAUSE_BLAST: saved_pc=0;
	                   callgc0();
	                   saved_pc = *(int*)saved_ptrs[1];
	                   break;
	 case CAUSE_FAULT:   
			   for(i=0;i<32;i++) saved_ptrs[i]=0;
			   saved_ptrs[0]=fault_code;
			   saved_ptrs[1]=saved_exnptr;
	                   saved_pc = *(int*)saved_ptrs[1];
			   break;
	}
      }
}

ghandle(sig,code,scp,addr) int sig,code,addr; struct sigcontext *scp;
{
#ifdef V9
 saved_pc = addr;  addr = (int)&saveregs; 
#else
 saved_pc = scp->sc_pc; scp->sc_pc = (int)&saveregs; 
#endif
#ifdef M68
 if (sig==SIGFPE && code==FPE_TRAPV_TRAP && ((short*)saved_pc)[-1] == 23548)
    {saved_pc-=4; cause=CAUSE_GC;}
#else
 if(sig==SIGSEGV) cause=CAUSE_GC;
#endif
 else {fault_code=exnCode(sig,code); cause=CAUSE_FAULT;}
}

int arenabase;               /* bottom of the heap */
int arenasize = 0;           /* heap starts empty */
int new_size = 1024 * 1024;  /* default heap size of 1 megabyte */
int arstart;                 /* beginning of main arena */
int arend;                   /* end of main arena, and the heap */
int old_high;                /* marks end of persistent heap */
int new_high;
int new_new_high;
int lastbreak;

extern int ghandle();
int store_save;              /* holds the ref list between applies; see prim */

#define pagesize 8192

init_gc()
{arenabase=(sbrk(0) + pagesize-1) & ~(pagesize-1);
 lastbreak=arenabase;
 increase_heapsize();
 old_high = arenabase;
 arstart = arenabase+arenasize/2;
 saved_dataptr = (int)arstart;
 saved_limit = arenabase+arenasize-4096;
}

restarter()
{lastbreak = sbrk(0);
 saved_dataptr = (int)arstart;
 saved_limit = lastbreak-4096;
 setupsignals();
 saved_ptrs[0]=3;
 runML();
 _exit(0);
}

callgc0()
{int i; int *roots[40]; int **rootsptr = roots;
#ifdef	V9
 int (*inthandle)();
 inthandle = signal(SIGINT,SIG_IGN);
#endif
 suspendtimers();
 *rootsptr++ = &pstruct;
 *rootsptr++ = &saved_exnptr;
 for(i=0;i<32;i++) *rootsptr++ = saved_ptrs+i;
  {extern int gcprof[];
   int *currentsave = (int *) current;
   current = (int) (gcprof+1);
   *rootsptr++ = (int *) &currentsave;
   *rootsptr=0;
   callgc(&saved_pc,roots,&saved_dataptr,saved_storeptr);
   saved_storeptr=0;
   current = (int) currentsave;
  }
 restarttimers();
#ifdef V9
 signal(SIGINT,inthandle);
#endif
}

int getmore_die(){die("bug: insufficient to_space\n");}

int amount_desired;

int decrease_heapsize()
{int p = arenabase+new_size;
 p = (p + pagesize-1 ) & ~(pagesize-1);
 if (p<lastbreak)
    {brk(p-4);
     arenasize = p-arenabase;
     if (gcmessages >= ML_INT(2))
        chatting("\n[Decreasing heap to %dk]\n",arenasize/1024);
     lastbreak=p;}
 return lastbreak;}

int increase_heapsize() /* new_size > arenasize */
{int p = arenabase+new_size;
 RESTART:
 p = (p + pagesize-1 ) & ~(pagesize-1);
 if (p==lastbreak)
    {if (gcmessages >= ML_INT(2)) chatting("\nWarning: can't increase heap\n");
     return p;}
 else if (brk(p-4))
         {if (gcmessages >= ML_INT(3))
	     chatting("\nWarning: must reduce heap request\n");
          p=(lastbreak+(p-pagesize))/2; goto RESTART;}
      else {lastbreak=p;
            arenasize = p-arenabase;
	    if (gcmessages >= ML_INT(2))
               chatting("\n[Increasing heap to %dk]\n",arenasize/1024);
            return p;}
 }

int getmore_must()
{int oldsize=arenasize;
 int gamma = (ratio<7?7:ratio)>>1;
 int live_size = amount_desired+arenasize+arenabase-old_high;
 int max = softmax>>1;
 new_size=live_size*gamma;
 if (max < new_size)
    {int new = 3*live_size;
     new_size = (new>max?new:max);}
 {int r=increase_heapsize();
  if (oldsize==arenasize) die("\nRan out of memory");
  return r;}}

callgc(pcptr,	    /* pc at point of fault (by ref) */
       misc_roots,  /* vector of ptrs to extra root words */
       arptr,	    /* place to put new freespace pointer */
       store_list   /* list of refs stored into */
      )
    int *pcptr; int ***misc_roots; int *arptr; int **store_list;
{int gamma = (ratio<7?7:ratio)>>1;
 int amount_desired;
 arend = arenabase+arenasize;
 if (cause==CAUSE_EXPORT) amount_desired = 0;
           else amount_desired = 4+arend-(*arptr);
 if (arstart== *arptr) new_high = old_high; /* no minor needed */
 else 
 {if (gcmessages >= ML_INT(3)) chatting("\n[Minor collection...");
  starttime();
  gc(arstart,arend,
    old_high,arstart,
    old_high,
    &new_high,
    misc_roots,pcptr,store_list,
    getmore_die, 0);
    {int a = new_high-old_high, b =(*arptr)-arstart, msec=endtime();
     if (gcmessages >= ML_INT(3))
	chatting(" %d%% used (%d/%d), %d msec]\n", ((100*a)/b), a, b, msec);
     collected += 2*((a+512)/1024); /* round to nearest k */
     collectedfrom += 2*((b+512)/1024);
     minorcollections+=2;
    }
#ifdef GCDEBUG
 checkup(arstart,new_high);
 clear(new_high,arenabase+arenasize);
#endif
 }
{int need_major = 0, cut = 2; /* 2 <= cut <= gamma */
 if (cause==CAUSE_EXPORT || cause==CAUSE_BLAST ||
     (new_high+amount_desired) > arenabase+arenasize-arenasize/cut)
    need_major = 1;
 else {int live_size = amount_desired+new_high-old_high;
       new_size=live_size*gamma;
       if (new_size > arenasize)
          {int max = softmax>>1;
	   if (max < new_size)
	      {int new = 3*live_size;
	       new_size = (new>max?new:max);
	       if (new_size > arenasize)
                  {int end = increase_heapsize();
	           if ((end-new_high)/2 <= amount_desired)
	              {need_major = 1;}}}
	   else {int end = increase_heapsize();
	         if ((end-new_high)/2 <= amount_desired)
	            {need_major = 1;}}}}
 if (cause==CAUSE_BLAST) old_high=new_high;
 if (need_major)
    {if (gcmessages >= ML_INT(1)) chatting("\n[Major collection...");
     starttime();
     if (gc(arenabase,old_high,
	old_high,arenabase+arenasize,
	new_high,
	&new_new_high,
        misc_roots,pcptr,0,
        getmore_must, cause==CAUSE_BLAST? &saved_ptrs[0]: 0))
       {moveback(old_high,new_new_high,
	      arenabase,
	      misc_roots,pcptr);
        {int a = new_new_high-new_high, b = old_high-arenabase, msec=endtime();
         if (gcmessages >= ML_INT(1))
        	chatting(" %d%% used (%d/%d), %d msec]\n",(100*a)/b, a, b, msec);
         collected += 2*((a+512)/1024);
         collectedfrom += 2*((b+512)/1024);
         majorcollections+=2;}
	{int live_size = amount_desired+new_new_high-old_high;
	 int max = softmax>>1;
         new_size = live_size*gamma;
         old_high=arenabase+new_new_high-old_high;
	 if (max < new_size)
	    {int new = 3*live_size;
	     new_size = (new>max?new:max);}
	 if (new_size > arenasize)
	    {int end = increase_heapsize();
	     if ((end-old_high)/2 <= amount_desired)
	        die("\nRan out of memory\n");}
         else if (new_size*4 < arenasize*3) decrease_heapsize();}}
       else {endtime(); if (gcmessages >= ML_INT(1)) chatting("abandoned]\n");}}
 else old_high=new_high;}
 store_save = 0;
 arend = arenabase+arenasize;
 arstart = (((arend+old_high)/2)+3)&(~3);
 (*arptr) = arstart;
 saved_limit = arend-4096;
#ifdef V9
 signal(SIGSEGV, ghandle);
#endif	/* V9 */
}

#ifdef GCDEBUG
clear(low,high) int *low, *high;
{int *i;
 for(i=low; i<high; i++) *i=0;
}

int *descriptor;
checkup(low,high)
int *low,*high;
{int *i,*j;
 chatting("checking to_space...  ");
 i = low;
 while (i < high) {
   descriptor = i;
   switch(get_tag(i)) {
        case tag_backptr:
		chatting("Uncool backpointer at %x in to_space\n",i);
		exit(0);
		break;
	case tag_embedded:
		chatting("Uncool embedded tag at %x in to_space\n",i);
		exit(0);
		break;
	case tag_string:
	case tag_bytearray:
		i += (get_len(i)+7)>>2;
		break;
	case tag_record:
	case tag_array:
	case tag_closure:
		j = i + get_len(i) + 1;
		while(++i < j) {
		 if (*i & 1) continue;
		 else if ((int*)*i > high) {
			 chatting("Uncool pointer %x at %x\n",
				  *i,i);
			 chatting("Descriptor is at %x\n",descriptor);
		      }
		}
		break;
	case tag_forwarded:
		chatting("Uncool forwarding tag at %x in to_space\n",i);
		exit(0);
		break;
	default: /* function pointers */
		chatting("Unexpected even tag %d at %x in to_space\n",
			 get_tag(i),i);
		exit(0);
		break;
	      }
 }
 chatting("done\n");
}
#endif	/* GCDEBUG */

int g_sec,g_usec,t_sec,t_usec;
#ifdef BSD
#include <sys/time.h>
#include <sys/resource.h>
struct rusage r1, r2;

starttime()
{getrusage(0,&r1);}

endtime()
{
 int sec,usec;
 getrusage(0,&r2);
 sec = r2.ru_utime.tv_sec-r1.ru_utime.tv_sec;
 usec = r2.ru_utime.tv_usec-r1.ru_utime.tv_usec;
 if (usec < 0) {sec--; usec += 1000000;}
 return (usec/1000 + sec*1000);
}

struct rusage garbagetime,timestamp,s1,s2;

suspendtimers()
{
 getrusage(0,&s1);
}

restarttimers()
{
 int sec,usec;
 getrusage(0,&s2);
 sec = s2.ru_utime.tv_sec-s1.ru_utime.tv_sec+garbagetime.ru_utime.tv_sec;
 usec = s2.ru_utime.tv_usec-s1.ru_utime.tv_usec+garbagetime.ru_utime.tv_usec;
 if (usec < 0) {sec--; usec += 1000000;}
 else if (usec > 1000000) {sec++; usec -= 1000000;}
 garbagetime.ru_utime.tv_sec = sec;
 garbagetime.ru_utime.tv_usec = usec;
}

resettimers()
{
 garbagetime.ru_utime.tv_sec = 0;
 garbagetime.ru_utime.tv_usec = 0;
 s2.ru_utime.tv_sec = s1.ru_utime.tv_sec = 0;
 s2.ru_utime.tv_usec = s1.ru_utime.tv_usec = 0;
 r2.ru_utime.tv_sec = r1.ru_utime.tv_sec = 0;
 r2.ru_utime.tv_usec = r1.ru_utime.tv_usec = 0;
 collected = collectedfrom = minorcollections = majorcollections = ML_INT(0);
}

timer()
{
 getrusage(0,&timestamp);
 g_usec = garbagetime.ru_utime.tv_usec * 2 + 1;
 g_sec = garbagetime.ru_utime.tv_sec * 2 + 1;
 t_usec = timestamp.ru_utime.tv_usec * 2 + 1;
 t_sec = timestamp.ru_utime.tv_sec * 2 + 1;
}

#endif	/* BSD */

#ifdef V9
#include <sys/times.h>
struct tms t1,t2;

starttime()
{times(&t1);}

endtime()
{
 int hzsec;
 times(&t2);
 hzsec = t2.tms_utime-t1.tms_utime;
 return(hzsec*1000/60);
}

struct tms timestamp,s1,s2;
int garbagetime;

suspendtimers()
{
 times(&s1);
}

restarttimers()
{
 times(&s2);
 garbagetime += s2.tms_utime-s1.tms_utime;
}

resettimers()
{
 garbagetime = 0;
 s2.tms_utime = s1.tms_utime = 0;
 t2.tms_utime = t1.tms_utime = 0;
 collected = collectedfrom = minorcollections = majorcollections = ML_INT(0);
}


timer()
{
 times(&timestamp);
 g_usec = garbagetime % 60 * 1000000 / 60 * 2 + 1;
 g_sec = garbagetime / 60 * 2 + 1;
 t_usec = timestamp.tms_utime % 60 * 1000000 / 60 * 2 + 1;
 t_sec = timestamp.tms_utime / 60 * 2 + 1;
}

#endif	/* V9 */
