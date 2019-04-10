/* MacExpImp.c */
/*
 * 07Jan92  e   export() and corresponding import() for SMLNJ for the Mac.
 *
 * uses stuff from:
 *
/* gc.c
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 */

#define DBG_WTF 1

#include "tags.h"
#include "ml_state.h"
#include "ml_types.h"

#include "os_mac.h"

#include "eventchk.h"		/* 25Jan93  e */

/** This stuff used to be in "descriptor.h."  It should probably be merged with
 ** the definitions in "ml_types.h."
 **/

#define is_ptr(x)	(((int)(x)&0x3) == 0)
#define mask_tags	(power_tags-1)
#define get_len(x)	(*(int *)(x)>>width_tags)
#define get_lenz(x)	((((*(int*)(x)) & mask_tags) == TAG_special) ? 1 : get_len(x))
#define get_strlen(x)	(((*(int *)(x)>>width_tags)+3) >> 2)
#define get_realarraylenw(x)	(get_len(x) << 1)  /* word len */
#define get_realarraylenb(x)	(get_len(x) << 3)  /* byte len */
#define tag_from_desc(d)	((d) & mask_tags)
#define get_tag(x)		tag_from_desc(*(int *)(x))

/* return true if "m" points to the descriptor word of a code string. */
#define isCodeString(m)		\
	(((get_len(m) & 1) == 0) && ((*((m)+2)) == MAKE_DESC(1,TAG_backptr)))

/* Given a 6-bit tag, return true if is the tag of an object that cannot
 * contain pointers.
 */
static char hasNoPtrs[16] = {
	0, 1, 1, 0, 	/* pair, reald, emb_reald, unused */
	0, 1, 0, 0,	/* special, backptr, unused, forwarded */
	0, 0, 1, 1,	/* record, array, string, emb_string */
	1, 1, 0, 0	/* bytearray, realdarray, unused, unused */
    };
#define ContainsNoPtrs(x)	(hasNoPtrs[((x)>>2)&0xF])

/* end of stuff from gc.c */

/* refcells from callgc.c */

extern int active_procs0[];
extern int collected0[];
extern int collectedfrom0[];
extern int times0[];		/* new */
extern int current0[];
extern int gcmessages0[];
extern int majorcollections0[];
extern int minorcollections0[];
extern int pstruct0[];
extern int ratio0[];
extern int softmax0[];
extern int lastratio0[];
extern int sighandler0[];
extern int errstrings[];
/* see below extern int externlist0[]; */

#define collected (collected0[1])
#define collectedfrom (collectedfrom0[1])
#define current (current0[1])
#define gcmessages (gcmessages0[1])
#define majorcollections (majorcollections0[1])
#define minorcollections (minorcollections0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])
#define lastratio (lastratio0[1])
/* refcells from mp.c */
#define active_procs (active_procs0[1])

extern void restart_gc(MLState_ptr);
extern MLState_ptr Exporters_State;

extern int		arenabase;		/* bottom of the heap */
extern int		arenasize;		/* heap starts empty */
extern int		lastbreak;
extern int		new_size;
extern int		arstart;		/* beginning of allocation arena */
extern int		arend;			/* end of main arena, and the heap */
extern int		old_high;		/* marks end of persistent heap */
extern ML_val_t		store_preserve;		/* s.b. ML_val_t */

/* end of stuff from callgc.c */

/* from cfuns.c */

struct table_t {
    int		    tag;
    ML_val_t	    func;
    ML_val_t	    name;
    ML_val_t	    next;
    int		    stag;
    char	    str[16];
};

extern struct table_t externlist0[];

/* won't work! #define NEXTERNS	(sizeof(externlist0)/sizeof(struct table_t)) */
extern int nexterns;

/* end of stuff from cfuns.c */

extern MLState_t	*MLproc;
extern int		datalist[];			/* s.b. ML_val_t */

#include <unix.h>
#include <fcntl.h>
#include <errno.h>
#include <LoMem.h>

#define DUMP(p,sz) if(write(fd,((char *)p),(sz))<0) return(-1)
#define LOAD(p,sz) if( read(fd,((char *)p),(sz))<0) return(-1)

static int eMAGIC = 0x357;
static int eVERSION = 103;

extern ML_val_t cstruct[];
extern ML_val_t mathvec[];
extern ML_val_t runvec[];
extern int array0_v[];
extern int bytearray0_v[];
extern int realarray0_v[];
extern int vector0_v[];

/* from run_ml.c */

extern int profile_array[];

/* magic constants from cstruct.c */

#define CSTRUCT_SZ	28
#define MATHVEC_SZ	9
#define RUNVEC_SZ	12

extern int handle_v[];

extern int return_v[];
extern ML_val_t *return_c;		/* == return_v+1 */
extern int sigh_return_v[];
extern ML_val_t *sigh_return_c;		/* == sigh_return_v+1 */

/* from ml_objects.c */

extern int string0[];

/* e's stuff */

#define OLDVEC_SZ 8

#if DBG_WTF > 1

#define DBGprintf(x,y) chatting(x,y,z)

#else

void DBGprintf( char *x, int y, int z )
{
  if (gcmessages >= (int )INT_CtoML(4)) chatting(x,y,z);
}

#endif

static int old_cstruct[CSTRUCT_SZ+1];
static int old_mathvec[MATHVEC_SZ+1];
static int old_runvec[RUNVEC_SZ+1];
/* ugh static int old_extern[NEXTERNS]; */
extern int old_extern[];
static int old_vecvec[OLDVEC_SZ];

void init_old_vecs()
{
    int		    i;
    struct table_t  *p = (struct table_t *)INT_CtoML(0);

    blockmove( cstruct, old_cstruct, CSTRUCT_SZ+1 );
    blockmove( mathvec, old_mathvec, MATHVEC_SZ+1 );
    blockmove( runvec, old_runvec, RUNVEC_SZ+1 );
    old_vecvec[0] = (int )&cstruct[1];
    old_vecvec[1] = (int )&mathvec[1];
    old_vecvec[2] = (int )&runvec[1];
    old_vecvec[3] = (int )&handle_v[1];
    old_vecvec[4] = (int )&profile_array[1];
    old_vecvec[5] = (int )&string0[1];
    old_vecvec[6] = (int )return_c;
    old_vecvec[7] = (int )sigh_return_c;

    for (i = nexterns;  --i >= 0; ) {
	old_extern[i] = (int )externlist0[i].func;
    }
}

static int ewtf( int z )
{
    int i;
    int *p;

    for ( p = (int *)&old_cstruct[1], i = 1; i <= CSTRUCT_SZ; p++, i++ )	{
	if ( z == *p )	{
	    DBGprintf( "\n wtf: 0x%x found, cstruct: %d", z, i );
	    return (int )cstruct[i];
	}
    }
    if ( z == old_vecvec[0] )	{
	    DBGprintf( "\n wtf: 0x%x found, cstruct", z, 0 );
	    return (int )&cstruct[1];    	
    }
    if ( z == old_vecvec[1] )	{
	    DBGprintf( "\n wtf: 0x%x found, mathvec", z, 0 );
	    return (int )&mathvec[1];    	
    }
    if ( z == old_vecvec[2] )	{
	    DBGprintf( "\n wtf: 0x%x found, runvec", z, 0 );
	    return (int )&runvec[1];    	
    }
    if ( z == old_vecvec[3] )	{
	    DBGprintf( "\n wtf: 0x%x found, handle_v", z, 0 );
	    return (int )&handle_v[1];    	
    }
    if ( z == old_vecvec[4] )	{
	    DBGprintf( "\n wtf: 0x%x found, profile_array", z, 0 );
	    return (int )&profile_array[1];    	
    }
    if ( z == old_vecvec[5] )	{
	    DBGprintf( "\n wtf: 0x%x found, string0", z, 0 );
	    return (int )&string0[1];    	
    }
    if ( z == old_vecvec[6] )	{
	    DBGprintf( "\n wtf: 0x%x found, return_c", z, 0 );
	    return (int )return_c;    	
    }
    if ( z == old_vecvec[7] )	{
	    DBGprintf( "\n wtf: 0x%x found, sigh_return_c", z, 0 );
	    return (int )sigh_return_c;    	
    }
    for ( p = (int *)&old_mathvec[1], i = 1; i <= MATHVEC_SZ; p++, i++ )	{
	if ( z == *p )	{
	    DBGprintf( "\n wtf: 0x%x found, mathvec: %d", z, i );
	    return (int )mathvec[i];
	}
    }
    for ( p = (int *)&old_runvec[1], i = 1; i <= RUNVEC_SZ; p++, i++ )	{
	if ( z == *p )	{
	    DBGprintf( "\n wtf: 0x%x found, runvec: %d", z, i );
	    return (int )runvec[i];
	}
    }
    for (i = nexterns;  --i >= 0; ) {
	if ( z == old_extern[i] )	{
	    DBGprintf( "\n wtf: 0x%x found, extern: %d", z, i );
	    return (int )externlist0[i].func;
	}
    }
    chatting( "\n[ warning! wtf: 0x%x not found! ]", z );
    return (int )INT_CtoML(0);
}

static int verify_arena()
{
    int *x = (int *)arenabase, *done = (int *)old_high;

#define ewtfMAC(x) (is_ptr(x)?(((x)>=arenabase&&(x)<old_high)?(x):(ewtf(x))):(x))

    if ( store_preserve != INT_CtoML(0) )
    	printf( "\n store_preserve non-NIL!" );

    {	MLState_t   *p;
	int i, mask;
	
	p = &(MLproc[0]);

	ewtfMAC((int )p->ml_pc);
	ewtfMAC((int )p->ml_exncont);
	ewtfMAC((int )p->ml_varptr);

	mask = p->mask;
	for (i = 0;  mask != 0;  i++, mask >>= 1) {
	  if ((mask & 1) != 0) ewtfMAC((int )p->ml_roots[ArgRegMap[i]]);
	}
    }

    ewtfMAC(pstruct);
    ewtfMAC(current);
    ewtfMAC(times0[1]);
    ewtfMAC(sighandler0[1]);

    while (x < done) {
	int tag = get_tag(x);
	if (ContainsNoPtrs(tag)) {
	    if (tag == TAG_reald) x += 3;
	    else if (tag == TAG_realdarray) x += (get_realarraylenw(x) + 1);
	    else x += (get_strlen(x) + 1);
	}
	else {
	    register int i = get_lenz(x);
	    register int z;
	    ++x;
	    do {
		z = *x;
#if 0
		if (is_ptr(z=*x))
		    if (z >= arenabase && z < old_high)
			/* *x += adjust */ ;
		    else
			/* *x = */ ewtf(z);
#else
		if ( ( z < arenabase || z >= old_high ) && (! ( z & 1 )) )
		    ewtf(z);
#endif
		x++;
	    } while (--i > 0);
	}
	MAYBE_EVENTCHK();
    }
    DBGprintf( "\n", 0, 0 );
}

int export_guts(fd) /* nonzero return means error, check errno */
int fd;
	{
	/* dump file header for verification */
	DUMP(&eMAGIC, 4);
	DUMP(&eVERSION, 4);

	/* dump addresses for import relocation */
	DUMP(&arenabase, 4);
	DUMP(&old_high, 4);
	DUMP(&arstart, 4);

	/* dump ml_state */
	/* DUMP(&(MLproc[0]), sizeof(MLState_t)); */ /* ????????? */
	DUMP(&(MLproc->mask), 4);		/* real roots */
	DUMP(MLproc, 12+NROOTS*4);		/* = 11 ML registers */

	/* dump A5 world */
	DUMP(old_cstruct, (CSTRUCT_SZ+1)<<2 );
	DUMP(old_mathvec, (MATHVEC_SZ+1)<<2 );
	DUMP(old_runvec, (RUNVEC_SZ+1)<<2 );
	DUMP(old_extern, nexterns<<2 );
	DUMP(old_vecvec, (OLDVEC_SZ)<<2 );

	/* dump refcells */  	
	DUMP(&pstruct, 4);
	DUMP(&current, 4);
	DUMP(&(sighandler0[1]), 4);		/* proc */

	DUMP(&active_procs, 4);			/* int */

	DUMP(&gcmessages, 4);			/* int */
	
	DUMP(&(times0[1]), 4);			/* new */
	DUMP(&collected, 4);			/* reload the following cells? */
	DUMP(&collectedfrom, 4);
	DUMP(&majorcollections, 4);
	DUMP(&minorcollections, 4);
	DUMP(&ratio, 4);
	DUMP(&softmax, 4);
	DUMP(&lastratio, 4);

	/* DUMP(&(datalist[1]), 12);		/* no */

	/* dump heap */  	
	DUMP((void*)arenabase, (old_high - arenabase));
  	
	return(0);
}

int export(fd) /* nonzero return means error, check errno */
int fd;
{
    init_old_vecs();
#if DBG_WTF > 0
    verify_arena();
#endif
    export_guts(fd);
    return(0);
}

/* like relocate() in gc.c */

e_relocate (start, end, stuff)
    int start, end;
    int *stuff;
{
    int *x = stuff, *done = stuff + (end-start)/4;
    int adjust = ((int)stuff) - start;

    while (x < done) {
	int tag = get_tag(x);
	if (ContainsNoPtrs(tag)) {
	    if (tag == TAG_reald) x += 3;
	    else if (tag == TAG_realdarray) x += (get_realarraylenw(x) + 1);
	    else x += (get_strlen(x) + 1);
	}
	else {
	    register int i = get_lenz(x);
	    register int z;
	    ++x;
	    do {
#if 0
		/* is_ptr() returns false for pointers into Mac space = 2mod4 */
		if (is_ptr(z=*x))
		    if (z >= start && z < end)
			*x += adjust;
		    else
			*x = ewtf(z);
#else
		z = *x;
		if ( z >= start && z < end ) { if (is_ptr(z)) *x += adjust; }
		else			     { if (! (z & 1)) *x = ewtf(z); }
#endif
		x++;
	    } while (--i > 0);
	}
	MAYBE_EVENTCHK();
    }
}

int import(fd) /* nonzero return means error, check errno */
	int fd;
{
	int hp_offset, mask, size, i, bit_bucket[2];
	int dumped_arenabase, dumped_old_high;
	/* register int *p; */
	register int  x;
	MLState_t   *p;

	/* load file header for verification */
	LOAD(bit_bucket, 8);
	if(bit_bucket[0] != eMAGIC || bit_bucket[1] != eVERSION) {
		errno = EDOM;
		return(1);
	}

	/* load addresses for import relocation */
	LOAD(&dumped_arenabase, 4);
	LOAD(&dumped_old_high, 4);
	LOAD(&arstart, 4);
	
	hp_offset = arenabase - dumped_arenabase;
	size = dumped_old_high - dumped_arenabase;
	if(brk((int)arenabase + size))
		die("loader: not enough memory");
	arstart += hp_offset;						/* ??? */
	old_high = dumped_old_high + hp_offset;
	
	/* load ml_state */
	/* LOAD(&(MLproc[0]), sizeof(MLState_t)); */ /* ????????? */
	LOAD(&(MLproc[0].mask), 4);		/* real roots */
	LOAD(&(MLproc[0]), 12+NROOTS*4);	/* = 11 ML registers */

	/* load A5 world */
	LOAD(old_cstruct, (CSTRUCT_SZ+1)<<2 );
	LOAD(old_mathvec, (MATHVEC_SZ+1)<<2 );
	LOAD(old_runvec, (RUNVEC_SZ+1)<<2 );
	LOAD(old_extern, nexterns<<2 );
	LOAD(old_vecvec, (OLDVEC_SZ)<<2 );

	/* load refcells */  	
	LOAD(&pstruct, 4);
	LOAD(&current, 4);
	LOAD(&(sighandler0[1]), 4);		/* proc */

	LOAD(&active_procs, 4);			/* int */

	LOAD(&gcmessages, 4);			/* int */
	
	LOAD(&(times0[1]), 4);			/* new */
	LOAD(&collected, 4);			/* reload the following cells? */
	LOAD(&collectedfrom, 4);
	LOAD(&majorcollections, 4);
	LOAD(&minorcollections, 4);
	LOAD(&ratio, 4);
	LOAD(&softmax, 4);
	LOAD(&lastratio, 4);

	/* LOAD(&(datalist[1]), 12);		/* no */

	/* load heap */  	

	LOAD((void*)arenabase, size);
  	
	/* adjust */
	
	/* collect_roots from callgc.c was used as the basis for identifying roots */
	/*  each root that it would have collected is dumped to the file "as is"   */
	/*  and is adjusted when imported as follows: */

#define ADJ(z) (is_ptr(x=(z))?(((x)>=dumped_arenabase&&(x)<dumped_old_high)\
                  ?((x)+hp_offset):(/*(x)+A5_offset*/ewtf(x))):(x))

	/* I used to just try adjusting all the registers...
	for(p = (int *)&(MLproc[0]),i=0; i < (NROOTS+3); i++)
		*p++ = ADJ(*p);
	but this is what collect_roots does... */
	
	p = &(MLproc[0]);

	p->ml_pc = 	(ML_val_t )ADJ((int )p->ml_pc);
	p->ml_exncont =	(ML_val_t )ADJ((int )p->ml_exncont);
	p->ml_varptr =	(ML_val_t )ADJ((int )p->ml_varptr);

	mask = p->mask;
	for (i = 0;  mask != 0;  i++, mask >>= 1) {
	  if ((mask & 1) != 0)
	    p->ml_roots[ArgRegMap[i]] = (ML_val_t )ADJ((int )p->ml_roots[ArgRegMap[i]]);
	}

	pstruct = ADJ(pstruct);
	current = ADJ(current);
	times0[1] = ADJ(times0[1]);
	sighandler0[1] = ADJ(sighandler0[1]);

	/* no - we verify that it's NIL at export time...
	store_preserve = (ML_val_t)ADJ((int)store_preserve); */
	/* no - we have no datalist on Mac yet...
	datalist[1] = ADJ(datalist[1]);
	datalist[2] = ADJ(datalist[2]);
	datalist[3] = ADJ(datalist[3]); */

	e_relocate( dumped_arenabase, dumped_old_high, arenabase );

    return(0);
}

#include <Dialogs.h>
#include <Quickdraw.h>
#include <ToolUtils.h>

void restarter(char *imageName)
	{
	DialogPtr	dlg;
	int fd;
 
	dlg=GetNewDialog(loading_dlogID,NULL,(WindowPtr)(-1L));
	DrawDialog(dlg);
	SetCursor(*GetCursor(watchCursor));

	/* arenabase = lastbreak = sbrk(0);	*/
	/* pagesize = ...; */

	mp_init(0);
	init_gc(MLproc);
	init_externlist(MLproc);
	Exporters_State = MLproc;

	if ((fd = eopen(imageName, O_RDONLY|O_BINARY)) < 3)
		die("restarter: Cannot open %s.\n",imageName);
	if(import(fd))
		die("restarter: Cannot restart %s, %d.\n",imageName,errno);
	close(fd);

	DisposDialog(dlg);
	/* SetCursor(&qd.arrow); */
	asm {
		movea.l	(a5),a0
		pea		-108(a0)			;  arrow
		_SetCursor
	}
	
	Exporters_State = MLproc;
	restart_ml(MLproc);

	/* not reached */
 }

/* end of MacExpImp.c */
