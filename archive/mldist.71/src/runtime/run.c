/* run.c
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories.
 */

#include <stdio.h>
#include "ml_os.h"
#include <sys/stat.h>
#include <sys/file.h>
#include <signal.h>

#include "ml_state.h"
#include "ml_types.h"
#include "tags.h"
#include "prim.h"

extern ML_val_t cstruct[];

static ML_val_t load();
static void enroll();

extern int new_size;
extern int resettimers();
extern MLState_ptr mp_init();
extern ML_val_t apply_ml_fn();

#if defined(M68) || defined(C)
extern ML_val_t mathvec[];
#endif

extern ML_val_t gcmessages0[], ratio0[], softmax0[], pstruct0[];
#define gcmessages (gcmessages0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])

int		isExported = 0;
char		**global_argv;

main (argc, argv)
    int		argc; 
    char	*argv[];
{
    ML_val_t	    argname, perv, core, math, loader, obj;
    MLState_ptr     MLState;
    char	    **p = argv+1;
    int		    xflag = 0;

    global_argv = argv;

    if (isExported)
	restart_ml();

    gcmessages	= INT_CtoML(2);
    ratio	= INT_CtoML(5);
    softmax	= INT_CtoML(1024*1024*100);
    while (*p && **p == '-') {
	switch (p[0][1]) {
	  case 'h':
	    if (p[1]) {
		new_size = 1024*atoi(p[1]);
		p+=2;
	    }
	    else
		quit("no -h value");
	    break;
	  case 'r':
	    if (p[1]) {
		int r = atoi(p[1]);
		p += 2;
		if (r < 3)
		    quit ("bad -r value");
		ratio = INT_CtoML(r);
	    }
	    else
		quit("no -r value");
	    break;
	  case 'm':
	    if (p[1]) {
		softmax = INT_CtoML(1024*atoi(p[1]));
		p+=2;
	    }
	    else
		quit("no -m value");
	    break;
	  case 'g':
	    if (p[1]) {
	 	gcmessages0[1] = INT_CtoML(atoi(p[1]));
		p+=2;
	    }
	    else
		quit("no -g value");
	    break;
	  case 'x':
	    xflag = 1;  p++;
	    break;
	  case 'y':
	    xflag = 2;  p++;
	    break;
	  case 'z':
	    xflag = 3;  p++;
	    break;
	} /* end of switch */
    } /* end of while */

#if (!defined(C))
    if ((*p == NULL) && (xflag == 0))
	quit("no file to execute\n");
#endif
    MLState = mp_init(FALSE);
    setup_signals (MLState, TRUE);
    resettimers (MLState);
    init_gc (MLState);
    init_externlist (MLState);

    perv = load(MLState, ML_alloc_string(MLState,"CoreFunc"));
    enroll (ML_alloc_string(MLState,"Core"), 
	    core = apply_ml_fn(MLState, perv, PTR_CtoML(cstruct+1)));
#if defined(M68) || defined(C)
    math = PTR_CtoML(mathvec+1);
    enroll (ML_alloc_string(MLState,"Math"), math);
#else
    math = load(MLState, ML_alloc_string(MLState,"Math"));
#endif
    perv = pstruct = load(MLState,ML_alloc_string(MLState,"Initial"));
    if (xflag==1) {
	chatting("Result is %#x\n", REC_SELINT(perv, 0));
	_exit(0);
    }

    loader = load(MLState,ML_alloc_string(MLState,"Loader"));
    if (xflag==3) {
	chatting("Result is %#x\n", REC_SELINT(loader, 0));
	_exit(0);
    }

#if (!defined(C))
    argname = ML_alloc_string(MLState,*p);
#else
    argname = ML_alloc_string(MLState,"bogus");
#endif
    REC_ALLOC4(obj, core, perv, math, argname);
    apply_ml_fn (MLState, loader, obj);

#ifdef GCPROFILE
    print_gcprof(MLState);
#endif
    shutdown(0);
}


/** The table of objects we need to boot. **/

static struct {
    ML_val_t	    name;	/* an ML string */
    ML_val_t	    obj;
} objtbl[10];
int objcount;

/* enroll:
 * Add the (name, obj) pair to the object table.
 */
static void enroll (name, obj)
    ML_val_t	    name, obj;
{
    objtbl[objcount].name = name;
    objtbl[objcount].obj  = obj;
    objcount++;
}

/* lookup:
 * Search for name in the object table, return the corresponding object or 0.
 */
static ML_val_t lookup (name)
    ML_val_t	    name;
{
    int		    i;

    for (i = 0;  i < objcount;  i++) {
	if (ML_eqstr(objtbl[i].name, name))
	    return objtbl[i].obj;
    }
    return 0;
}


/* openread:
 * Return a pointer to the code for the specified structure.  If the structure
 * is not in the data list, then read it into the heap from its ".mo" file.
 */
static ML_val_t openread (MLState,s)
    MLState_ptr     MLState;
    char	    *s;
{
    int		    fd = -1, i;
    register char   *p, *q;
    ML_val_t	    ss = ML_alloc_string(MLState,s);
    ML_val_t	    d;

  /* search the datalist for the file */
    for (d = PTR_CtoML(datalist+1);  d != MOLST_nil;  d = MOLST_next(d)) {
	if (ML_eqstr(ss, MOLST_name(d)))
	    return MOLST_code(d);
    }

  /* not in the datalist, so open the file */
#if defined(V9) || defined(HPUX)
    fd = open(s, 0);
#else
    fd = open(s, O_RDONLY, 0666);
#endif
    if (fd < 0)
	quit("cannot open %s\n",s);

  /* allocate and initialize the code string in the heap */
    p = (char *)(MLState->ml_allocptr);
    MLState->ml_allocptr += sizeof(int);
    while (i = read(fd, MLState->ml_allocptr, 4096))
	MLState->ml_allocptr += i;
    q = (char *)MLState->ml_allocptr;
    MLState->ml_allocptr = ((((int)q) + 3) & ~3);
    *(int*)p = MAKE_DESC(q - (p + 4), tag_string);

    return PTR_CtoML(((int)p) + 4);

} /* end of openread */


/* loadlist:
 */
static ML_val_t loadlist (MLState,names)
    MLState_ptr     MLState;
    ML_val_t	    names;
{
    if (names == ML_nil)
	return ML_nil;
    else {
	ML_val_t	obj  = load(MLState,ML_hd(names));
	ML_val_t	rest = loadlist(MLState,ML_tl(names));
	return ML_cons(obj, rest);
    }
} /* end of loadlist */

/* load:
 */
static ML_val_t load (MLState,name)
    MLState_ptr     MLState;
    ML_val_t	    name;
{
    ML_val_t	    p, args;
    char	    buf[64];

    if (p = lookup(name))
	return p;
    else {
	strcpy (buf, "mo/");
	strncpy (buf+3, (char *)PTR_MLtoC(name), OBJ_LEN(name));
	strcpy (buf+3+OBJ_LEN(name), ".mo");

#if (!defined(C))
	chatting("[Loading %s]\n", buf);
	p = openread(MLState,buf);
	ml_flush_icache(MLState, p);
	REC_ALLOC1(p, PTR_CtoML(PTR_MLtoC(p)+2));
	p = apply_ml_fn (MLState, p, ML_unit);
	chatting("[Executing %s]\n",buf);
#else
	p = openread(MLState,buf);
	REC_ALLOC1(p, PTR_CtoML(p))
	p = apply_ml_fn (MLState, p, ML_unit);
#endif
	args = loadlist (MLState,((int*)p)[1]);
	p = REC_SEL(apply_ml_fn(MLState, REC_SEL(p, 0), args), 0);
	enroll (name, p);

	return p;
    }
} /* end of load */


int quit (s, a, b, c, d, e, f)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf, s, a, b, c, d, e, f);
    write(2, dbuf, strlen(dbuf));
    shutdown (2);
}

int die (s, a, b, c, d, e, f)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf, s, a, b, c, d, e, f);
    write(2, dbuf, strlen(dbuf));
    /* abort(); */
    shutdown (3);
}

int chatting (s, a, b, c, d, e, f, g)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf, s, a, b, c, d, e, f, g);
    write(2, dbuf, strlen(dbuf));
}

#ifdef MP_DEBUG
int pchatting (MLState, s, a, b, c, d, e, f, g)
     MLState_ptr MLState;
     char *s;
{
    char dbuf[1024];
    int offset;
    extern ML_val_t gcmessages;

    if (gcmessages >= INT_CtoML(4)) {
      offset = sprintf(dbuf, "%d:", MLState->self);
      sprintf(dbuf+offset, s, a, b, c, d, e, f, g);
      write(2, dbuf, strlen(dbuf));
    }
}
#endif MP_DEBUG
