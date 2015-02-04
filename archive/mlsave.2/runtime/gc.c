#define chatting(x)
/* registers:
 inside arenas: allocation is on word boundaries and in units a multiple
	of a word (4 bytes) so words with odd contents are not pointers.
	Conversely, if a word is pointed to by a pointer p (i.e., the word
	is p[0], then p[-1] contains a descriptor of the record the word is in:
	struct {
		unsigned int flg:3;	least sig bits
		int len:29;
	} mem;
	  flag is even:  look in previous word for descriptor
          flag is odd: this is the descriptor.
		       len gives the number of 4-byte words. (not incl. descriptor)
		       For any record in a collectable area, len>0
	when the gc isn't running:
		       flag=1    record containing pointers & integers
		       flag=3    record with stored pointer value
		       flag=5    record containing no pointers
		       flag=7    look in p[-len-1] for descriptor
	when gc is running, descriptor in the TO space:
			as above, but flag=3 not possible
	when the gc is running, descriptor in the FROM space:
		       flag=1    unmoved record containing pointers & integers
		       flag=3    record has already been moved
				 in which case, p[0] is the forwarding pointer.
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

typedef struct mem {
#ifdef VAX
	unsigned int flg:3;
	int len:29;
#endif
#ifdef M68
	int len:29;
	unsigned int flg:3;
#endif
} mem;


#define EVEN(x)  (1^(((int)(x))&1))

int ** (*gmore)();
static int **to_ptr, **to_lim;
static int **lowest, **highest;
static int *trap_pc;
static int trap_pc_done;

#define LEN(m) ((*m)>>3)

/* static
xgc(refloc)
register int *refloc; */
#define xgc(refloc)		\
{register int *m = *((int**)(refloc));	\
  /* if refloc is not a pointer,	\
		 or is not in the allocated area, just leave it alone */  \
 if( (!((int)m & 1)) && (m >= (int*)lowest && m < (int*)highest))	\
 { m--;	\
   for(;;)	\
      {	\
	switch((*m)&7) {	\
	case 0: case 2: case 4: case 6:	\
		m--; continue;	\
	case 7: m -= LEN(m); continue;	\
	case 5: if (!trap_pc_done &&	\
		     m < trap_pc && m+LEN(m)+1 > trap_pc)	\
		     {trap_pc_done=1;	\
		      trap_pc += to_ptr - (int**)m;	\
		     }	\
		/* fall through */	\
	case 1: {register int **i=(int**)m, **j=to_ptr, len1 = LEN(m)+1;  \
		 if (j+len1 > to_lim) do {to_lim=gmore();}	\
				      while (j+len1 > to_lim);	\
		 do {*j++ = *i++;} while (--len1 > 0);	\
		 ((int**)m)[1]= 1+(int*)to_ptr;	\
		 to_ptr = j;	\
	        }	\
		(*m) = (((*m)&~7)|3);	\
		/* fall through */	\
	case 3:	*(int*)(refloc) += ((int*)m)[1] - ((int)(m+1));	\
     }	\
     break;	\
    }	\
   }	\
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
       trap_pc_done = !(trap_pc >= (int*)from_low && trap_pc < (int*)from_high);
	to_ptr = to_done;
	to_lim = to_high;
	lowest=from_low;
	highest=from_high;

	/* do the refs */
	chatting ("beginning refs\n");
	{register int **p;
#ifndef chatting
 int count=0;
#endif
/*	 for(p=store_list; p; p= (int**) (p[0]))
	        ((mem*)(p[-1]-1))->flg = 1; */
	 for(p=store_list; p; p= (int**) (p[0]))
	    {register int **r;
#ifndef chatting
	     count++;
#endif
	     r = (int**)(p[-1]);
	     if (r>=from_low && r < from_high) continue;
	     xgc(r);
/*	     {register int len;
	      len = ((mem*)(r-1))->len;
	      do {xgc(r); r++;} while (--len > 0);
	     }
*/
	    }
/*	 chatting("(%d refs)\n",count); */
	}
	/* do the stack */
	chatting ("beginning stack\n");
	{register int **p;
	 for(p = stack_low; p < (int **) stack_high; p++) xgc(p);
	}

	{extern int *pstruct_v; xgc(&pstruct_v);}

	/* do misc. roots */
	chatting ("beginning misc roots\n");
	{ register int ***p;
	  for(p=misc_roots; *p; p++) xgc(*p);
	}

	/* finish the new space */
	chatting ("finishing new space\n");
	{register int x = (int)to_low;
         while (x<(int)to_ptr)
	    {register int p = x+4;
	     {register int descr = (*(int*)x)>>1;
	      x+=(descr&~3)+4;
	      if (descr&2) continue;
	     }
	     do{xgc(p); p+=4;} while (p<x);
	    }
	}

/*	{register int **p;
	 for(p=store_list; p; p= (int**) (p[0]))
		chatting("reference %X forwarded to %X\n",
			    p[-1], p[-1][0]);
	}
*/
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
#define ISPTR(x)   (!((int)(x)&1))
#define ADJUST1(x)   (INRANGE(x)?(x)+=offset:0)
#define ADJUST(x) (ISPTR(x)?ADJUST1(x):0)
         ADJUST1(*trap_pcx);

	/* do the stack */
	chatting ("beginning stack\n");
	{register int **p;
	 for(p = stack_low; p < (int **) stack_high; p++) ADJUST(*p);
	}

	{extern int *pstruct_v; ADJUST(pstruct_v);}

	/* do misc. roots */
	chatting ("beginning misc roots\n");
	{ register int ***p;
	  for(p=misc_roots; *p; p++) ADJUST(**p);
	}

	/* finish the new space */
	chatting ("finishing new space\n");
	x=from_low;
	while (x<from_high)
	  {register int flg = ((mem*)x)->flg, len1 = ((mem*)x)->len + 1;
	    if (flg==1)
		    {register int i;
		     for(i=1; i<len1; i++) ADJUST(x[i]);
		      x += len1;
		    }
	    else if (flg==5)  x += len1;
	    else die("bogus flag in gc (to space)\n");
	  }

	blockmove(from_low,to_low,from_high-from_low);

}

