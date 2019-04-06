/* gc.c
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 */

#include "tags.h"

/** This stuff used to be in "descriptor.h."  It should probably be merged with
 ** the definitions in "ml_types.h."
 **/
#define is_ptr(x) (!((int)(x)&1))
#define mask_tags (power_tags-1)
#define get_len(x) (*(int *)(x)>>width_tags)
#define get_lenz(x) ((((*(int*)(x))&(power_tags-1))==tag_suspension)?1:get_len(x))
#define get_strlen(x) (((*(int *)(x)>>width_tags)+3)/4)
#define get_tag(x) (*(int *)(x)&mask_tags)


/* registers:
 inside arenas: allocation is on word boundaries and in units a multiple
    of a word (4 bytes) so words with odd contents are not pointers.
    Conversely, if a word is pointed to by a pointer p (i.e., the word
    is p[0], then p[-1] contains a descriptor of the record the word is in:
	struct {
		unsigned int flg:width_tags;	least sig bits
		int len:32-width_tags;
	} mem;
    flag is even:  look in previous word for descriptor
    flag is odd: this is the descriptor.
	len gives the number of 4-byte words. (not incl. descriptor)
	For any record in a collectable area, len>0
	when the gc isn't running:
		       flag=1    record containing pointers & integers
		       flag=5    record containing no pointers
		       flag=7    look in p[-len-1] for descriptor
	when gc is running, descriptor in the TO space:
			as above, but flag=3 not possible
	when the gc is running, descriptor in the FROM space:
		       flag=1    unmoved record containing pointers & integers
		       flag=3    record has already been moved, in which case,
				 p[0] is the forwarding pointer.
		       flag=5    unmoved record containing no pointers
		       flag=7    look in p[-len-1] for descriptor

	In a record containing pointers & integers,
	  any even number is a pointer, any odd number is not a pointer.

	There are occasional pointers to places outside the GC arena;
	 these get copied intact.

    Format of linked list of stored-into ref cells:
      p[0] = pointer to a ref or array cell that's been stored into.
      p[1] = index within object of modified slot
      p[2] = next object on list (1 for nil)

*/

int ** (*gmore)();
static int **to_ptr, **to_lim, **to_lim0;
static int **lowest, **highest;
static int repair;
static int any_weak;

extern int store_preserve, preserving;

/*static
xgc(refloc)
register int *refloc;*/
#define xgc(refloc)\
{register int *m = *((int**)(refloc));\
  /* if refloc is not a pointer,\
		 or is not in the allocated area, just leave it alone */\
 if(is_ptr(m) && (m >= (int*)lowest && m < (int*)highest))\
 { m--;\
   for(;;)\
      {\
	switch(get_tag(m)) {\
	case tag_backptr:\
		m -= get_len(m);\
		continue;\
	case tag_embedded:\
		m--; continue;\
	case tag_string:\
	case tag_bytearray:\
	    {register int **i=(int**)m, **j=to_ptr, len1 = (get_len(m)+7)>>2;\
		 if (j+len1 > to_lim) do {if (repair) \
						{repair=0; to_lim=to_lim0;} \
					  else to_lim=gmore();}\
				      while (j+len1 > to_lim);\
		 do {*j++ = *i++;} while (--len1 > 0);\
	         if (repair)  \
		   {if (to_ptr+5 < to_lim) \
		    {* -- to_lim = ((int**)m)[1]; \
		     * -- to_lim = m+1; \
		    } \
		    else {repair=0; to_lim=to_lim0;} \
		   } \
		 ((int**)m)[1]= 1+(int*)to_ptr;\
		 to_ptr = j;\
	    }\
	    (*m) = tag_forwarded;\
	    *(int*)(refloc) += ((int*)m)[1] - ((int)(m+1));\
	    break;\
	case tag_array:\
	  if (preserving)	    \
		{*to_ptr++ = (int*)(16*3+1);	    \
	         *to_ptr++ = m+1;	\
		 *to_ptr++ = (int*)-1;	\
		 *to_ptr++ = (int*)store_preserve;	\
		 store_preserve = (int) (to_ptr-3);	\
		}	\
	case tag_record:\
	    {register int **i=(int**)m, **j=to_ptr, len1 = get_len(m)+1;\
		 if (j+len1 > to_lim) do {if (repair) \
						{repair=0; to_lim=to_lim0;} \
					  else to_lim=gmore();}\
				      while (j+len1 > to_lim);\
		 do {*j++ = *i++;} while (--len1 > 0);\
	         if (repair)  \
		   {if (to_ptr+5 < to_lim) \
		    {* -- to_lim = ((int**)m)[1]; \
		     * -- to_lim = m+1; \
		    } \
		    else {repair=0; to_lim=to_lim0;} \
		   } \
		 ((int**)m)[1]= 1+(int*)to_ptr;\
		 to_ptr = j;\
	        }\
		(*m) = tag_forwarded;\
		/* fall through */\
	case tag_forwarded: *(int*)(refloc) += ((int*)m)[1] - ((int)(m+1));\
			    break;\
	case tag_suspension:\
	    {register int **i=(int**)m, **j=to_ptr, len1 = 2;\
		 if (j+len1 > to_lim) do {if (repair) \
						{repair=0; to_lim=to_lim0;} \
					  else to_lim=gmore();}\
				      while (j+len1 > to_lim);\
		 do {*j++ = *i++;} while (--len1 > 0);\
	         if (repair)  \
		   {if (to_ptr+5 < to_lim) \
		    {* -- to_lim = ((int**)m)[1]; \
		     * -- to_lim = m+1; \
		    } \
		    else {repair=0; to_lim=to_lim0;} \
		   } \
		 ((int**)m)[1]= 1+(int*)to_ptr;\
		 to_ptr = j;\
	        }\
		(*m) = tag_forwarded;\
	       *(int*)(refloc) += ((int*)m)[1] - ((int)(m+1));\
			    break;\
	default: /* function pointers */\
		m--; continue;\
     }\
     break;\
    }\
   }\
}

gc(from_low,	    /* lowest address in space to be collected from */
   from_high,	    /* higher than any ... */
   to_low,	    /* lowest address in space to copy into */
   to_high,	    /* limit address to copy into */
   to_done,	    /* to-space is already copied into up to here */
   to_where,        /* (by-ref) just past highest address copied into */
   misc_roots,	    /* vector (0-terminated) of ptrs to possible root words */
   store_list,	    /* head of linked list of store-pointers */
   get_more,	    /* procedure to call to increase to_lim */
   first_root       /* (optional) address of interesting root to trace;
		       if present, then to_done must equal to_low */
)
  int **from_low, **from_high, ***misc_roots,
      **to_low, **to_high, **to_done,
      ***to_where, **store_list;
  int *first_root;
  int ** (*get_more)();
{
#ifdef HEAP_TRACE
	target = 0;
#endif
	any_weak = 0;
	gmore=get_more;
	to_ptr = to_done;
	to_lim0 = to_lim = to_high;
	lowest=from_low;
	highest=from_high;

        repair=0;
        if (first_root)
	  {register int x;
           int **blast_begin = to_low;
	   repair=1;
	   xgc(first_root);
	   x = (int) to_done;
           while (x<(int)to_ptr)
	    {register int p = x+4;
	     {register int descr = *(int *)(x);
	      if (contains_no_ptrs(descr)) {x += ((get_len(x)+7)&~3);
					 continue;}
	      x += get_lenz(x) * 4 + 4;
	     }
	     do{xgc(p); p+=4;} while (p<x);
	    }
	   blast_write(blast_begin, x, *first_root);
	   if (repair)
	    {while(to_lim<to_lim0)
	      {int *loc = *to_lim++;
	       int *old = *to_lim++;
	       loc[-1] = ((int*)(loc[0]))[-1];
	       loc[0] = (int)old;
	      }
	     return 0;
	    }
	  }


	/* do the refs */
#ifdef GCDEBUG
        chatting("\nto_ptr at %x...  ",to_ptr);
        chatting("beginning refs... ");
#endif
	{register int **px;
#ifdef GCDEBUG
	 int count=0;
#endif
	 for(px=store_list; ((int)px)!=1; px= (int**) (px[2]))
	    {register int **r;
#ifdef GCDEBUG
	     count++;
#endif
	     r = (int**)(px[0])+(((int)(px[1]))>>1);
	     if (r>=from_low && r < from_high) continue;
 	     if (preserving)
		{*to_ptr++ = (int*)(16*3+1);
	         *to_ptr++ = px[0];
		 *to_ptr++ = px[1];
		 *to_ptr++ = (int*)store_preserve;
		 store_preserve = (int) (to_ptr-3);
		}
	     xgc(r);
	    }
#ifdef GCDEBUG
	chatting("(%d refs)\n",count);
#endif
	}

	/* do misc. roots */
#ifdef GCDEBUG
        chatting("to_ptr at %x...  ",to_ptr);
        chatting("beginning misc roots\n");
#endif
	{ register int ***p;
	  for(p=misc_roots; *p; p++) xgc(*p);
	}

	/* finish the new space */
#ifdef GCDEBUG
        chatting("to_ptr at %x...  ",to_ptr);
        chatting("finishing new space\n");
#endif
	{register int x = (int)to_low;
         while (x<(int)to_ptr)
	    {register int p = x+4;
	     {register int descr = *(int *)(x);
	      if (contains_no_ptrs(descr)) 
		  {x += ((get_len(x)+7)&~3);
		   continue;}
	      x += get_lenz(x) * 4 + 4;
#ifdef HEAP_TRACE
	      if (descr == tag_suspension + 4*power_tags)
		  target=p;
#endif
	      if (descr == tag_suspension + 2*power_tags)
                  {any_weak=1; continue;}
	     }
             do{xgc(p); p+=4;} while (p<x);
	    }
	}
#ifdef GCDEBUG
        chatting("to_ptr at %x...  ",to_ptr);
#endif
        if (any_weak)
	{register int x = (int)to_low;
#ifdef GCDEBUG
	 chatting("doing weak pointers\n");
#endif
         while (x<(int)to_ptr)
	    {int *p = ((int*)x)+1;
	     int descr = *(int *)(x);
	     if (contains_no_ptrs(descr)) 
		  {x += ((get_len(x)+7)&~3);
		   continue;}
	     x += get_lenz(x) * 4 + 4;
	     if (descr == tag_suspension + 2*power_tags)
                 {int *m = ((int*)*p)-1;
                  if ((!(((int)m)&1)) && m >= (int*)from_low && m <= (int*)from_high)
                      for(;;)
                        {switch(get_tag(m))
                          {case tag_string: case tag_bytearray:
                           case tag_array: case tag_record: 
                           case tag_suspension:
                                     *p = 1; 
				     p[-1]=tag_suspension+3*power_tags;
				     break;
                           case tag_forwarded:
                                     *p += m[1] - (int) (m+1);
				     break;
                           case tag_backptr: m -= get_len(m); 
                                    continue;
                           case tag_embedded: 
			   default:
				    m--; 
                                    continue;
			  }
                         break;
			}
		 }
	    }
	}
#ifdef GCDEBUG
        chatting("to_ptr at %x...  ",to_ptr);
        chatting("gc done\n");
#endif
#ifdef HEAP_TRACE
        if (target) trace(to_low,target,target+4);
#endif
        *to_where = to_ptr;
        return 1;
}

blockmove(from,to,words) register int * from, *to; register int words;
{
 if (!words) return;
 if (from<to && from+words >to)
    {from+=words; to+=words;	
     do {*--to = *--from;} while (--words > 0);
    }
 else do {*to++ = *from++;} while (--words > 0);
}

moveback
  (from_low,	    /* lowest address in space to be collected from */
   from_high,	    /* higher than any ... */
   to_low,	    /* lowest address in space to copy into */
   misc_roots	    /* vector (0-terminated) of ptrs to possible root words */
)
  int *from_low, *from_high, **misc_roots,
      *to_low;
{	register int *x, offset = sizeof(int)*(to_low-from_low);

#define INRANGE(x)  (((int)(x) >= (int)from_low) &&  \
		     ((int)(x) < (int)from_high) )
#define ADJUST1(x)   (INRANGE(x)?(x)+=offset:0)
#define ADJUST(x) (is_ptr(x)?ADJUST1(x):0)

	/* do misc. roots */
#ifdef GCDEBUG
	chatting("misc roots... ");
#endif
	{ register int **p;
	  for(p=misc_roots; *p; p++) ADJUST(**p);
	}

	/* finish the new space */
#ifdef GCDEBUG
	chatting("finishing... ");
#endif
	x=from_low;
	while (x<from_high)
	    if (contains_no_ptrs(*x))
		    x += (get_len(x)+7)>>2;
	    else    {register int i = get_lenz(x);
		     ++x;
	             do {ADJUST(*x); x++;} while (--i > 0);
		    }
	blockmove(from_low,to_low,from_high-from_low);
#ifdef GCDEBUG
	chatting("done\n");
#endif
}

relocate(start,end,stuff)
     int start, end; int *stuff;

{int *x=stuff, *done= stuff + (end-start)/4;
 int adjust = ((int)stuff) - start;
	while (x<done)
	    if (contains_no_ptrs(*x))
		    x += (get_len(x)+7)>>2;
	    else    {register int i = get_lenz(x);
		     ++x;
	             do {if ( (*x & 1) == 0
			      && *x >= start && *x < end)
			     *x += adjust;
			 x++;}
		       while (--i > 0);
		    }
}

#ifdef HEAP_TRACE
/* this is from the old "trace.c" */

trace(to_low, target_lo, target_hi) int *to_low, *target_lo, *target_hi;
{int *x=to_low;
 chatting("tracing %8x\n", target_lo);
  while(x<target_lo)
   {int *p= x+1, *x0= x;
    if (contains_no_ptrs(*x)) 
	x+=((get_len(x)+7)>>2);
    else 
      {x += get_lenz(x)+1;
       do if ((((*p)&1)==0) && *p >= (int)target_lo && *p < (int)target_hi)
            {trace(to_low,x0,x);
	     printrec(x0);
	     return;
	    }
         while (++p < x);
      }
   }
}

printrec(x) int *x;
{int len = get_len(x), i;
    chatting("\nAddress %8x:    tag=%8x\n",x+1,*x);
    for(i=0;i<len;i++)
     chatting("[%2x] %8x\n",i,x[i+1]);
}

#endif TRACE_HEAP


#ifdef GCMON

static int boundaries[1000];
static int boundtimes[1000];
static int bounddead[1000];
static int boundlim=0;
static int lasttime=0;

static struct {
    int live, dead;
} hist[32];
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
	    if (dead) {hist[log].dead+= size>>2; deadcount+=size>>2;}
	    else hist[log].live+= size>>2;
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
   hist[log].dead+= bounddead[i];
   b=boundaries[i];}
 boundlim=1;
 boundaries[0]=fromlo+tohi-tolo;
 boundtimes[0]=0;
 bounddead[0]=dead+bounddead[boundlim-1];
}

dumpMon()
{char buf[100]; int i;
 for (i=0; i<32; i++) {
   chatting("%3d %10d %10d %5.2f%%\n",
      i, hist[i].live, hist[i].dead);
 }
}

#endif GCMON
