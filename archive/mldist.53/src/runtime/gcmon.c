#include "tags.h"
#include "descriptor.h"

static int boundaries[1000];
static int boundtimes[1000];
static int bounddead[1000];
static int boundlim=0;
static int lasttime=0;

static int histd[32];
static int histl[32];

static int scan(lo,hi,age) int lo,hi,age;
{register int x = lo, deadcount;
 while (x<hi)
    {register int dead=0, size, log;
     register int descr = *(int *)(x), fp = *(int*)(x+4);
      if (descr==tag_forwarded)
	 descr = *(int*)(fp-4);
      else dead=1;
      if (contains_no_ptrs(descr)) 
	       size = ((descr>>width_tags)+7)&~3;
	    else size = (((descr&(power_tags-1))==tag_suspension)?1:
		       (descr>>width_tags)) * 4 + 4;
      if (!age) age = (hi-x)>>2;
	   {log=0;
	    while (age) {log++; age>>=1;}
	    if (dead) {histd[log]+= size>>2; deadcount+=size>>2;}
	    else histl[log]+= size>>2;
	   }
      x+=size;
    }
 return deadcount;
}

gcmonMinor(fromlo,fromhi,tolo,tohi)
     int fromlo,fromhi,tolo,tohi;
{
 bounddead[boundlim]= scan(fromlo,fromhi,0);
 boundtimes[boundlim] = lasttime;
 boundaries[boundlim++] = tohi;
 lasttime+=(fromhi-fromlo)>>2;
}

gcmonMajor(fromlo,fromhi,tolo,tohi) int fromlo, fromhi, tolo,tohi;
{int b=fromlo, i; int log,age; int dead=0;
 for(i=0;i<boundlim-1;i++)
  {age=lasttime-boundtimes[i];
   dead+=bounddead[i]+scan(b,boundaries[i],age);
   log=0;
   while (age) {log++; age>>=1;}
   histd[log]+= bounddead[i];
   b=boundaries[i];}
 boundlim=1;
 boundaries[0]=fromlo+tohi-tolo;
 boundtimes[0]=0;
 bounddead[0]=dead+bounddead[boundlim-1];
}

dumpMon()
{char buf[100]; int i;
 for(i=0;i<32;i++)
   chatting("%3d %10d %10d\n",i,histl[i],histd[i]);
}

