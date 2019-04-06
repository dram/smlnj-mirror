/* callgc.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */

#include "ml_os.h"
#include "ml_state.h"
#include "ml_types.h"
#include "tags.h"
#include "cause.h"
#include "request.h"

#define refcell(z)	\
    ML_val_t z[2] = {(ML_val_t)MAKE_DESC(1,tag_array), INT_CtoML(0)};

refcell(collected0)
refcell(collectedfrom0)
refcell(current0)
refcell(gcmessages0)
refcell(majorcollections0)
refcell(minorcollections0)
refcell(pstruct0)
refcell(ratio0)
refcell(sighandler0)
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

int		arenabase;               /* bottom of the heap */
int		arenasize = 0;           /* heap starts empty */
int		new_size = 1024 * 1024;  /* default heap size of 1 megabyte */
int		arstart;                 /* beginning of allocation arena */
int		arend;                   /* end of main arena, and the heap */
int		old_high;                /* marks end of persistent heap */
int		new_high;
int		new_new_high;
int		lastbreak;

static int	pagesize;

ML_val_t	store_preserve = INT_CtoML(0);
int		preserving = 0;

void callgc0 ();
static void callgc ();
static int getmore_die ();
static int getmore_must ();
#ifdef NeXT
static int brk(), sbrk();
#endif

/* init_gc:
 */
void init_gc ()
{
    pagesize		 = getpagesize();
    arenabase		 = sbrk(0);
    lastbreak		 = arenabase;
    increase_heapsize();
    old_high		 = arenabase;
#ifdef CACHE_SIZE
    arstart              = arenabase+arenasize-CACHE_SIZE;
#else
    arstart              = ((arenabase+arenasize/2)+3)&(~3);
#endif
    collected		 = INT_CtoML(0);
    collectedfrom	 = INT_CtoML(0);
    minorcollections	 = INT_CtoML(0);
    majorcollections	 = INT_CtoML(0);
    MLState->ml_allocptr = arstart;
    MLState->ml_limitptr = arenabase+arenasize-4096;
}

/* restart_gc:
 */
void restart_gc()
{
    int		live_size = old_high - arenabase;
    int		a = 0;
    ML_val_t	x = gcmessages;
    extern int	edata;

    resettimers();
    lastbreak = (int)&edata;
    gcmessages = INT_CtoML(0);
    new_size = compute_new_size(live_size);
    do {
	increase_heapsize();
	if (arenasize == a)
	    die("Can't get enough memory to start ML\n");
	a = arenasize;
    } while (arenasize < 3*live_size);
    gcmessages = x;
#ifdef ADVICE
    ostime=zero; otime=zero; ogtime=zero;
    getting_advice=1;
    initadvice();
#endif
    MLState->ml_allocptr = (int)arstart;
    MLState->ml_limitptr = lastbreak-4096;

} /* end of restart_gc */


/* check_heap:
 * Check the heap to insure that there is a sufficient amount of available
 * memory in the allocation arena.  If not, then do a garbage collection and
 * return 1, otherwise return 0.
 * NOTE: if a garbage collection is done, then any roots in C variables (other
 * than the ML state vector) are obsolete.
 */
int check_heap (amount)
    int		amount;
{
    register int    top = (arenabase + arenasize) - 4096;

    if ((MLState->ml_allocptr + amount) >= top) {
	register int	i;
	if (gcmessages >= INT_CtoML(3))
	    chatting("[check_heap: %d bytes available, %d required]\n",
		(top + 4096) - MLState->ml_allocptr, amount+4096);

	callgc0 (CAUSE_GC, STD_ARGS_MASK);
	return 1;
    }
    else
	return 0;

} /* end of check_heap */


/* callgc0:
 */
void callgc0 (cause, mask)
    int		cause, mask;
{
    int		i;
    int		*roots[NROOTS+5];
    int		**rootsptr = roots;
    ML_val_t	currentsave = current;
    extern int	gcprof[];

    current = PTR_CtoML(gcprof+1);

    suspendtimers();
/* chatting("mask = %#x\n",mask); */

    *rootsptr++ = (int *) &pstruct;
    *rootsptr++ = (int *) &currentsave;
    *rootsptr++ = (int *) &store_preserve;
    *rootsptr++ = (int *) &(sighandler0[1]);
    *rootsptr++ = (int *) &(MLState->ml_pc);
    *rootsptr++ = (int *) &(MLState->ml_exncont);
#ifdef BASE_INDX
    *rootsptr++ = (int *) &(MLState->ml_baseptr);
#endif
#ifdef GLOBAL_INDX
    *rootsptr++ = (int *) &(MLState->ml_globalptr);
#endif
    for (i = 0;  mask != 0;  i++, mask >>= 1) {
	if ((mask & 1) != 0)
	    *rootsptr++ = (int *)&(MLState->ml_roots[ArgRegMap[i]]);
    }
    *rootsptr = 0;

    callgc (cause, roots, &(MLState->ml_allocptr), MLState->ml_storeptr);

    MLState->ml_limitptr = (arend - 4096);
    MLState->ml_storeptr = (int)STORLST_nil;
    current = currentsave;

    restarttimers();

} /* end of callgc0 */


/* callgc:
 */
static void callgc (cause, misc_roots, arptr, store_list)
    int		cause;		/* the reason for doing GC */
    int		***misc_roots;	/* vector of ptrs to extra root words */
    int		*arptr;		/* place to put new freespace pointer */
    int		**store_list;	/* list of refs stored into */
{
    int amount_desired;

    arend = arenabase+arenasize;
    if ((cause == CAUSE_GC) || (cause == CAUSE_BLAST))
 	amount_desired = 4 + arend - (*arptr);
    else
	amount_desired = 0;
    if (arstart == *arptr)
	new_high = old_high; /* no minor needed */
    else  {
	if (gcmessages >= INT_CtoML(3))
	    chatting("\n[Minor collection...");
	starttime();
	gc (arstart, arend,
	    old_high,arstart,
	    old_high,
	    &new_high,
	    misc_roots,store_list,
	    getmore_die, 0);
	{
	    int a = new_high-old_high, b =(*arptr)-arstart, msec=endtime();
	    if (gcmessages >= INT_CtoML(3))
		chatting(" %d%% used (%d/%d), %d msec]\n", (100*a)/b, a, b, msec);
	    collected = INT_incr(collected, (a+512)/1024); /* round to nearest K */
	    collectedfrom = INT_incr(collectedfrom, (b+512)/1024);
	    minorcollections = INT_incr(minorcollections, 2);
	}

#ifdef GCMON
    gcmonMinor(arstart,*arptr,old_high,new_high);
#endif

  /* flush i-cache from old_high to new_high */
    FlushICache(old_high, new_high-old_high);

#ifdef GCDEBUG
	checkup (arstart, new_high);
	clear (new_high, arenabase+arenasize);
#endif
    }

    {
	int need_major = 0;
	int was_preserving;
	int gamma = INT_MLtoC(ratio);

	if (gamma < 3) gamma = 3;

	if ((cause == CAUSE_EXPORT) || (cause == CAUSE_BLAST) || (cause == CAUSE_MAJOR))
	    need_major = 1;
	else {
	    int cut = arenasize-arenasize/gamma;
	    int max = INT_MLtoC(softmax);
	    int halfmax = max/2;
	    int halfsize = arenasize/2;
	    cut = (cut<halfmax ? cut : halfmax);
	    cut = (cut>halfsize ? cut : halfsize);
	    if (new_high+amount_desired > arenabase+cut)
		need_major = 1;
	    else {
		int live_size = amount_desired+new_high-old_high;
#ifdef ADVICE
		if (((arenabase+arenasize-new_high)/2 <= amount_desired*3+100)
		|| (minorcollections > old_minorcount+200))
		    need_major = 1;
#else
		new_size = compute_new_size(live_size);
		if (new_size > arenasize
		&& (increase_heapsize()-new_high)/2 <= amount_desired)
		    need_major = 1;
#endif ADVICE
	   }
	}
	if (cause == CAUSE_BLAST)
	    old_high = new_high;
	if (need_major) {
	    if (gcmessages >= INT_CtoML(1))
		chatting("\n[Major collection...");
	    starttime();
	    was_preserving=preserving; preserving=0;
	    if (gc(arenabase, old_high,
		   old_high, arenabase+arenasize,
		   new_high,
		   &new_new_high,
		   misc_roots, 1,
		   getmore_must, (cause == CAUSE_BLAST) ? &(MLState->ml_arg) : 0))
	    {
#ifdef GCMON
		gcmonMajor (arenabase, old_high, old_high, new_new_high);
#endif
		moveback (old_high, new_new_high, arenabase, misc_roots);
	      /* flush i-cache from arenabase to arenabase+new_new_high-old_high */
		FlushICache (arenabase, new_new_high-old_high);
		{
		    int a = new_new_high-new_high, 
		    b = new_high-arenabase, msec=endtime();
		    if (gcmessages >= INT_CtoML(1))
			chatting(" %d%% used (%d/%d), %d msec]\n",(100*a)/b, a, b, msec);
		    collected += 2*((a+512)/1024);
		    collectedfrom += 2*((b+512)/1024);
		    majorcollections += 2;
		}
		{
		    int live_size = amount_desired+new_new_high-old_high;
		    old_high = arenabase+new_new_high-old_high;
#ifdef ADVICE
		    new_size = ask_new_size(live_size);
#else
		    new_size = compute_new_size(live_size);
#endif
		    if (new_size > arenasize) {
			int end = increase_heapsize();
			if ((end-old_high)/2 <= amount_desired)
			    die("\nRan out of memory\n");
		    }
#ifdef ADVICE
		    else if (new_size < arenasize)
			decrease_heapsize();
		    old_size = arenasize;
		    old_minorcount = minorcollections;
#else
		    else if (new_size < (arenasize/4)*3)
			decrease_heapsize();
#endif
		}
	    }
	    else {
		endtime();
		if (gcmessages >= INT_CtoML(1))
		    chatting("abandoned]\n");
	    }
 	    preserving=was_preserving;
	}
	else
	    old_high=new_high;
    }
    arend = arenabase+arenasize;
    arstart = (((arend+old_high)/2)+3)&(~3);
    (*arptr) = arstart;

} /* end of callgc */


/* getmore_die:
 */
static int getmore_die ()
{
    die("bug: insufficient to_space\n");
}

int amount_desired;

/* decrease_heapsize:
 */
int decrease_heapsize ()
{
    int		p = arenabase+new_size;
    p = (p + pagesize-1 ) & ~(pagesize-1);
    if (p < lastbreak) {
	brk(p);
	arenasize = p-arenabase;
	if (gcmessages >= INT_CtoML(2))
	    chatting ("\n[Decreasing heap to %dk]\n",arenasize/1024);
	lastbreak = p;
    }
    return lastbreak;
}

/* increase_heapsize:
 * Assume that new_size > arenasize.
 */
int increase_heapsize ()
{
    int		p = arenabase+new_size;

  RESTART:;
    p = (p + pagesize-1 ) & ~(pagesize-1);
    if (p == lastbreak) {
	if (gcmessages >= INT_CtoML(2))
	    chatting("\nWarning: can't increase heap\n");
	return p;
    }
    else if (brk(p)) {
	if (gcmessages >= INT_CtoML(3))
	    chatting("\nWarning: must reduce heap request\n");
	p = (lastbreak+(p-pagesize))/2;
	goto RESTART;
    }
    else {
	lastbreak=p;
	arenasize = p-arenabase;
	if (gcmessages >= INT_CtoML(2))
            chatting("\n[Increasing heap to %dk]\n",arenasize/1024);
        return p;
    }
}

int compute_new_size (live_size) 
    int		live_size;
{
    int		new_size;
    int		gamma = INT_MLtoC(ratio);
    int		max = INT_MLtoC(softmax);

    if (gamma < 3)
	gamma = 3;
    if (2000000000 / gamma < live_size)
	new_size = 2000000000;
    else
	new_size = live_size*gamma;
    if (max < new_size) {
	int new = 3*live_size;
	new_size = ((new > max) ? new : max);
    }
    return new_size;
}

/* getmore_must:
 */
static int getmore_must ()
{
    int		oldsize = arenasize;
    int		live_size = amount_desired+arenasize+arenabase-old_high;
    int		r;

    new_size = compute_new_size(live_size);
    r = increase_heapsize();
#ifdef ADVICE
    while (oldsize == arenasize) {
	chatting ("\nCan't get more memory; waiting\n");
	sleep (10);
	chatting("Trying again\n");
	r = increase_heapsize();
    }
#else
    if (oldsize == arenasize)
	die("\nRan out of memory");
#endif
    return r;
} /* end of getmore_must */


#ifdef GETSTORELIST
/* uniq:
 * THIS COULD BE PUT IN ml_getstorelist (in cfuns.c).
 */
ML_val_t uniq (arg)
    ML_val_t	arg;
{
    register ML_val_t *p, q;

    for (q = arg;  q != STORLST_nil;  q = STORLST_next(q)) {
	if (STORLST_index(q) == -1) {
	    (PTR_MLtoC(q))[-1] = STORLST_objdesc(q);
	    (PTR_MLtoC(STORLST_obj(q)))[-1] = 0;
	}
    }

    for (q = arg;  q != INT_CtoML(0);  q = REC_SEL(q, 2)) {
	if ((STORLST_objdesc(q) != 0)
	&& (STORLST_index(q) >= 0)
	&& (STORLST_index(STORLST_obj(q)) != 0)) {
	    (PTR_MLtoC(q))[-1] = STORLST_index(STORLST_obj(q));
	    (PTR_MLtoC(STORLST_obj(q)))[1] = 0;
	}
    }

    for (p = &arg;  *p != STORLST_nil;  p = &(STORLST_next(*p))) {
	if ((PTR_MLtoC(STORLST_obj(*p)))[1] == 0) {
	    (PTR_MLtoC(STORLST_obj(*p)))[STORLST_index(*p)] = OBJ_DESC(*p);
	    (PTR_MLtoC(*p))[-1] = MAKE_DESC(3, tag_record);
	}
    }

    return arg;

} /* end of uniq */
#endif GETSTORELIST


#ifdef NeXT
/**
 ** This implements sbrk/brk using vm_allocate/vm_deallocate.
 ** Arguments are assumed to be page multiples, and the argument
 ** to brk is assumed to be just after the desired breakpoint.
 **
 ** No relationship between the mapped region and the rest of the
 ** process image is guaranteed, but it is expected that the region
 ** will follow the end of data/bss.
 **
 ** Works with NeXT Mach (software release 0.9).  5/15/89
 **
 ** James William O'Toole Jr.
 **
 **/

#include <c.h>			/* TRUE and FALSE */
#include <sys/kern_return.h>	/* KERN_whatever */
#include <mach.h>

extern vm_task_t task_self_;

int mach_sbrk_needsinit = TRUE;
int mach_maplimit = 0;
int mach_brkpt = 0;
int mach_quant = 0x800000;
int big_heap = 0x2000000;

static int sbrk(incr)
    int incr;
{
    if (incr)
	die("sbrk called with nonzero value");
    if (mach_sbrk_needsinit != FALSE) {
	if (vm_allocate(task_self_, &mach_brkpt, big_heap, TRUE) != KERN_SUCCESS)
	    die("vm_allocate failed");
	mach_maplimit = mach_brkpt + big_heap;
	mach_sbrk_needsinit = FALSE;
    }
    return(mach_brkpt);
}

static int brk(pos)
    int pos;
{
    if (pos > mach_maplimit)
	return KERN_FAILURE;
    else
	return KERN_SUCCESS;
}

#endif NeXT


/** GCDEBUG routines **/

#ifdef GCDEBUG
clear(low,high) int *low, *high;
{int *i;
 chatting("clearing new space...  ");
 for(i=low; i<high; i++) *i=0;
 chatting("done\n");
}

int *descriptor;
checkup(low,high)
int *low,*high;
{int *i,*j;
 chatting("checking to_space...  ");
 i = low;
 while (i < high) {
   descriptor = i;
   switch(ML_gettag(i)) {
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
		i += (ML_getlen(i)+7)>>2;
		break;
	case tag_record:
	case tag_array:
		j = i + ML_getlen(i) + 1;
		while(++i < j) {
		 if (*i & 1) continue;
		 else if ((int*)*i > high) {
			 chatting("Uncool pointer %x at %x\n", *i,i);
			 chatting("Descriptor is at %x\n",descriptor);
		      }
		 else if ((int*)*i < low) {
			 chatting("Uncool pointer %#x at %#x\n", *i,i);
			 chatting("Descriptor is at %#x\n",descriptor);
		      }
		}
		break;
	case tag_forwarded:
		chatting("Uncool forwarding tag at %x in to_space\n",i);
		exit(0);
		break;
	default: /* function pointers */
		chatting("Unexpected even tag %d at %x in to_space\n",
			 ML_gettag(i),i);
		exit(0);
		break;
	      }
 }
 chatting("done\n");
}
#endif	/* GCDEBUG */
