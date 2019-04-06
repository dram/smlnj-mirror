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
      p[0] = next element of list  (zero value means end of list)
      p[-1] = pointer to a ref or array cell that's been stored into.

*/

int ** (*gmore)();
static int **to_ptr, **to_lim;
static int **lowest, **highest;
static int *trap_pc;
static int trap_pc_done;

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
		      trap_pc += to_ptr - (int**)m;\
		     }\
		/* fall through */\
	case tag_bytearray:\
	    {register int **i=(int**)m, **j=to_ptr, len1 = (get_len(m)+7)>>2;\
		 if (j+len1 > to_lim) do {to_lim=gmore();}\
				      while (j+len1 > to_lim);\
		 do {*j++ = *i++;} while (--len1 > 0);\
		 ((int**)m)[1]= 1+(int*)to_ptr;\
		 to_ptr = j;\
	    }\
	    (*m) = tag_forwarded;\
	    *(int*)(refloc) += ((int*)m)[1] - ((int)(m+1));\
	    break;\
	case tag_record:\
	case tag_array:\
	case tag_closure:\
	    {register int **i=(int**)m, **j=to_ptr, len1 = get_len(m)+1;\
		 if (j+len1 > to_lim) do {to_lim=gmore();}\
				      while (j+len1 > to_lim);\
		 do {*j++ = *i++;} while (--len1 > 0);\
		 ((int**)m)[1]= 1+(int*)to_ptr;\
		 to_ptr = j;\
	        }\
		(*m) = tag_forwarded;\
		/* fall through */\
	case tag_forwarded: *(int*)(refloc) += ((int*)m)[1] - ((int)(m+1));\
			    break;\
	default: /* function pointers */\
		m--; continue;\
     }\
     break;\
    }\
   }\
}

gc(stack_low,	    /* lowest address of possible roots in stack */
   stack_high,	    /* higher than any possible root in stack */
   from_low,	    /* lowest address in space to be collected from */
   from_high,	    /* higher than any ... */
   to_low,	    /* lowest address in space to copy into */
   to_high,	    /* limit address to copy into */
   to_done,	    /* to-space is already copied into up to here */
   to_where,        /* (by-ref) just past highest address copied into */
   misc_roots,	    /* vector (0-terminated) of ptrs to possible root words */
   trap_pcx,	    /* (by-ref) program counter at trap (needs moving) */
   store_list,	    /* head of linked list of store-pointers */
   get_more	    /* procedure to call to increase to_lim */
)
  int **stack_low, **stack_high, **from_low, **from_high, ***misc_roots,
      **to_low, **to_high, **to_done,
      ***to_where, **store_list;
  int **trap_pcx;
  int ** (*get_more)();
{
       gmore=get_more;
       trap_pc = *trap_pcx;
       trap_pc_done = !(trap_pc>=(int*)from_low && trap_pc<(int*)from_high);
	to_ptr = to_done;
	to_lim = to_high;
	lowest=from_low;
	highest=from_high;

	/* do the refs */
        chatarg("\nto_ptr at %x...  ",to_ptr);
        chatting("beginning refs... ");
	{register int **px;
#ifdef GCDEBUG
	 int count=0;
#endif
	 for(px=store_list; px; px= (int**) (px[0]))
	    {register int **r;
#ifdef GCDEBUG
	     count++;
#endif
	     r = (int**)(px[-1]);
	     if (r>=from_low && r < from_high) continue;
	     xgc(r);
	    }
#ifdef GCDEBUG
	chatting("(%d refs)\n",count);
#endif
	}
	/* do the stack */
        chatarg("to_ptr at %x...  ",to_ptr);
        chatting("beginning stack... ");
	{register int **p;
	 for(p = stack_low; p < (int **) stack_high; p++) xgc(p);
	}
        chatarg("(%d elements)\n",stack_high-stack_low);

        chatarg("to_ptr at %x...  ",to_ptr);
        chatting("beginning pstruct\n");
	{extern int *pstruct_v; xgc(&pstruct_v);}

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
	      if (contains_ptrs(descr)) {x += ((get_len(x)+7)>>2)<<2;
					 continue;}
	      x += get_len(x)*4+4;
	     }
	     do{xgc(p); p+=4;} while (p<x);
	    }
	}
        chatarg("to_ptr at %x...  ",to_ptr);
        chatting("gc done\n");
        *to_where = to_ptr;
	if (!trap_pc_done) die("Trap_pc not found!\n");
	*trap_pcx = trap_pc;
}

blockmove(from,to,words) register int * from, *to; register int words;
{
 if (!words) return;
 if (from<to && from+words >to)
    {from+=words; to+=words;	
     do {*--to = *--from;} while (--words);
    }
 else do {*to++ = *from++;} while (--words);
}

moveback
  (stack_low,	    /* lowest address of possible roots in stack */
   stack_high,	    /* higher than any possible root in stack */
   from_low,	    /* lowest address in space to be collected from */
   from_high,	    /* higher than any ... */
   to_low,	    /* lowest address in space to copy into */
   misc_roots,	    /* vector (0-terminated) of ptrs to possible root words */
   trap_pcx	    /* (by-ref) program counter at trap (needs moving) */
)
  int **stack_low, **stack_high, **from_low, **from_high, ***misc_roots,
      **to_low;
  int **trap_pcx;
{	register int **x; int offset = to_low-from_low;

#define INRANGE(x)  (((int)(x) >= (int)from_low) &&  \
		     ((int)(x) < (int)from_high) )
#define ADJUST1(x)   (INRANGE(x)?(x)+=offset:0)
#define ADJUST(x) (is_ptr(x)?ADJUST1(x):0)
         ADJUST1(*trap_pcx);

	/* do the stack */
	chatting("adjusting stack... ");
	{register int **p;
	 for(p = stack_low; p < (int **) stack_high; p++) ADJUST(*p);
	}

	chatting("pstruct... ");
	{extern int *pstruct_v; ADJUST(pstruct_v);}

	/* do misc. roots */
	chatting("misc roots... ");
	{ register int ***p;
	  for(p=misc_roots; *p; p++) ADJUST(**p);
	}

	/* finish the new space */
	chatting("finishing... ");
	x=from_low;
	while (x<from_high)
	  {register int tag = get_tag(x), len1 = get_len(x)+1;
	    switch (tag) {
	    case tag_record:
	    case tag_array:
	    case tag_closure:
		    {register int i;
		     for(i=1; i<len1; i++) ADJUST(x[i]);
		      x += len1;
		    };
		    break;
	    case tag_bytearray:
	    case tag_string:
		    x += (len1+6)>>2;
		    break;
	    default: die("bogus flag in gc (to space)\n");
	  }
	}
	blockmove(from_low,to_low,from_high-from_low);
	chatting("done\n");
}

