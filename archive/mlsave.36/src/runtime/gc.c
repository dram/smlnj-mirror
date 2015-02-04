#include "tags.h"
#include "descriptor.h"
#ifndef GCDEBUG
#define chatting(x)
#define chatarg(x,y)
#else
#define chatarg chatting
#endif
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
static int *trap_pc;
static int trap_pc_done;
static int repair;

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
		if (!trap_pc_done &&\
		     m < trap_pc && m+((get_len(m)+7)>>2) > trap_pc)\
		     {trap_pc_done=1;\
		      {extern int saved_pc_relative; \
		       saved_pc_relative += to_ptr - (int**)m;}\
		      trap_pc += to_ptr - (int**)m;\
		     }\
		/* fall through */\
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

int target;

gc(from_low,	    /* lowest address in space to be collected from */
   from_high,	    /* higher than any ... */
   to_low,	    /* lowest address in space to copy into */
   to_high,	    /* limit address to copy into */
   to_done,	    /* to-space is already copied into up to here */
   to_where,        /* (by-ref) just past highest address copied into */
   misc_roots,	    /* vector (0-terminated) of ptrs to possible root words */
   trap_pcx,	    /* (by-ref) program counter at trap (needs moving) */
   store_list,	    /* head of linked list of store-pointers */
   get_more,	    /* procedure to call to increase to_lim */
   first_root       /* (optional) address of interesting root to trace;
		       if present, then to_done must equal to_low */
)
  int **from_low, **from_high, ***misc_roots,
      **to_low, **to_high, **to_done,
      ***to_where, **store_list;
  int **trap_pcx; int *first_root;
  int ** (*get_more)();
{
       gmore=get_more;
       trap_pc = *trap_pcx;
       trap_pc_done = !(trap_pc>=(int*)from_low && trap_pc<(int*)from_high);
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
	      x += get_len(x)*4+4;
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
        chatarg("\nto_ptr at %x...  ",to_ptr);
        chatting("beginning refs... ");
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
        chatarg("to_ptr at %x...  ",to_ptr);
        chatting("beginning misc roots\n");
	{ register int ***p;
	  for(p=misc_roots; *p; p++) xgc(*p);
	}

	/* finish the new space */
        chatarg("to_ptr at %x...  ",to_ptr);
        chatting("finishing new space\n");
	{register int x = (int)to_low;
         while (x<(int)to_ptr)
	    {register int p = x+4;
	     {register int descr = *(int *)(x);
	      if (contains_no_ptrs(descr)) {x += ((get_len(x)+7)&~3);
					 continue;}
	      x += get_len(x)*4+4;
	     }
	     do{xgc(p); p+=4;} while (p<x);
	    }
	}
        chatarg("to_ptr at %x...  ",to_ptr);
        chatting("gc done\n");
        if (target) trace(to_low,target,target+4);
        *to_where = to_ptr;
	if (!trap_pc_done) die("Trap_pc not found!\n");
	*trap_pcx = trap_pc;
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
   misc_roots,	    /* vector (0-terminated) of ptrs to possible root words */
   trap_pcx	    /* (by-ref) program counter at trap (needs moving) */
)
  int *from_low, *from_high, **misc_roots,
      *to_low;
  int *trap_pcx;
{	register int *x, offset = sizeof(int)*(to_low-from_low);

#define INRANGE(x)  (((int)(x) >= (int)from_low) &&  \
		     ((int)(x) < (int)from_high) )
#define ADJUST1(x)   (INRANGE(x)?(x)+=offset:0)
#define ADJUST(x) (is_ptr(x)?ADJUST1(x):0)
         ADJUST1(*trap_pcx);

	/* do misc. roots */
	chatting("misc roots... ");
	{ register int **p;
	  for(p=misc_roots; *p; p++) ADJUST(**p);
	}

	/* finish the new space */
	chatting("finishing... ");
	x=from_low;
	while (x<from_high)
	    if (contains_no_ptrs(*x))
		    x += (get_len(x)+7)>>2;
	    else    {register int i = get_len(x);
		     ++x;
	             do {ADJUST(*x); x++;} while (--i > 0);
		    }
	blockmove(from_low,to_low,from_high-from_low);
	chatting("done\n");
}

relocate(start,end,stuff)
     int start, end; int *stuff;

{int *x=stuff, *done= stuff + (end-start)/4;
 int adjust = ((int)stuff) - start;
	while (x<done)
	    if (contains_no_ptrs(*x))
		    x += (get_len(x)+7)>>2;
	    else    {register int i = get_len(x);
		     ++x;
	             do {if ( (*x & 1) == 0
			      && *x >= start && *x < end)
			     *x += adjust;
			 x++;}
		       while (--i > 0);
		    }
}
