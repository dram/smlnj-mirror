/* run.c     (MS-Windows version)
 *
 * COPYRIGHT (c) 1989 by AT&T Bell Laboratories
 *
 * Altered 20 Dec 1991 by:   Yngvi S. Guttesen 
 *                           Department of Computer Science
 *                           The Technical University of Denmark
 *                           DK-2800 Lyngby
 */

#include "sml.h"
#include "ml_state.h"
#include "ml_types.h"
#include "tags.h"

EXTASM(cstruct)

BRSP gcmessages0;
BRSP ratio0;
BRSP softmax0;
BRSP pstruct0;

long load(long);
void enroll(long,long);

extern long new_size;
extern long resettimers();
extern ML_val_t apply_ml_fn(ML_val_t,ML_val_t);
extern int console_io(int, LPSTR, int);

#define gcmessages (gcmessages0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])

extern CATCHBUF CatchBuf;

int             isExported = 0;

run (LPSTR p[])
{
    ML_val_t    argname, perv, core, math, loader, obj;
    int         xflag = 0;

    if (isExported)
	restart_ml();

    gcmessages	= INT_CtoML(5);
    ratio	= INT_CtoML(3);
    softmax     = INT_CtoML(1024L*1024L);
    while (*p && **p == '-') {
	switch (p[0][1]) {
	  case 'h':
	    if (p[1]) {
                new_size = 1024*atol(p[1]);
		p+=2;
	    }
	    else
		quit("no -h value");
	    break;
	  case 'r':
	    if (p[1]) {
                long r = atol(p[1]);
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
                softmax = INT_CtoML(1024*atol(p[1]));
		p+=2;
	    }
	    else
		quit("no -m value");
	    break;
	  case 'g':
	    if (p[1]) {
                gcmessages0[1] = INT_CtoML(atol(p[1]));
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

    if ((*p == NULL) && (xflag == 0))
	quit("no file to execute\n");

    resettimers ();
    init_mlstate ();
    init_gc ();
    init_externlist ();

    perv = load(ML_alloc_string("CoreFunc"));
    enroll (ML_alloc_string("Core"),
            core = apply_ml_fn(perv,(long)LOWORD((cstruct+1))));
    math = load(ML_alloc_string("Math"));
    perv = pstruct = load(ML_alloc_string("Initial"));
    if (xflag==1) {
        chatting("Result is %#x\n", REC_SEL(perv,0));
	_exit(0);
    }

    loader = load(ML_alloc_string("Loader"));
    if (xflag==3) {
        chatting("Result is %#x\n", REC_SEL(loader,0));
	_exit(0);
    }

    argname = ML_alloc_string(*p);
    obj = REC_ALLOC4(core, perv, math, argname);
    apply_ml_fn (loader, obj);

    Throw((LPCATCHBUF) &CatchBuf, 1) ;

}


/** The table of objects we need to boot. **/

struct {
                long     name;       /* an ML string */
                long     obj;
              } objtbl[10];
int objcount;

/* enroll:
 * Add the (name, obj) pair to the object table.
 */
void enroll (name, obj)
    long name, obj;
{
    objtbl[objcount].name = name;
    objtbl[objcount].obj  = obj;
    objcount++;
}

/* lookup:
 * Search for name in the object table, return the corresponding object or 0.
 */
static long lookup (name)
    long        name;
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
long openread (s)
    char	    *s;
{
    int                 fd = -1, i;
    long                p, q;
    long                ss = ML_alloc_string(s);
    long                d;
    extern long _far    datalist_a;
    OFSTRUCT            ofstruct;
    long _far *__p;
    long _far *__q ;
    DWORD      __psaved ;

  /* search the datalist for the file */
    for (d = PTR_CtoML(LOWORD(&datalist_a));  d != MOLST_nil; d = MOLST_next(d)) {
        if (ML_eqstr(ss, MOLST_name(d)))
            return MOLST_code(d) ;
    }

  /* not in the datalist, so open the file */
    fd = OpenFile(s, (LPOFSTRUCT) &ofstruct, OF_READ);
    if (fd < 0)
	quit("cannot open %s\n",s);

  /* allocate and initialize the code string in the heap */

    Global16PointerAlloc(wsUse32Data,(DWORD)MLState->ml_allocptr,
                         (LPDWORD)&__q, (DWORD)4,0);

    p = MLState->ml_allocptr;
    MLState->ml_allocptr += sizeof(long);
    do {

        Global16PointerAlloc(wsUse32Data,(DWORD)MLState->ml_allocptr,
                             (LPDWORD)&__p, (DWORD)4*1024,0);
        __psaved = (DWORD)__p ;
        if ((i = _lread(fd, (LPSTR)__p, 4*1024)) >0)
            MLState->ml_allocptr += i;
        Global16PointerFree(wsUse32Data, __psaved, 0);
     } while (i>0) ;
    _lclose(fd);


    q = MLState->ml_allocptr;
    MLState->ml_allocptr = ((q + 3) & ~3);
    *__q = MAKE_DESC(q - (p + 4), tag_string);

    Global16PointerFree(wsUse32Data, (DWORD)__q, 0);


    return PTR_CtoML(p + 4);

} /* end of openread */


/* loadlist:
 */
static long loadlist (names)
    long        names;
{
    if (names == ML_nil)
	return ML_nil;
    else {
        long        obj  = load(ML_hd(names));
        long        rest = loadlist(ML_tl(names));
	return ML_cons(obj, rest);
    }
} /* end of loadlist */

/* load:
 */
static long load (name)
    long        name;
{
    long        p, args;
    char        buf[64];

    if (p = lookup(name))
	return p;
    else {
        long i,j ;
        char _far *__p ;
        long _far *__q ;
        DWORD __psaved ;
        DWORD __qsaved ;

        lstrcpy (buf, "mo\\");

        Global16PointerAlloc(wsUse32Data,(DWORD)name,
                             (LPDWORD)&__p, (DWORD)64,0);
        __psaved = (DWORD)__p ;

        for (i=0, j=OBJ_LEN(name) ; i<j && *__p ; i++, __p++)
            buf[i+3] = *__p ;

        Global16PointerFree(wsUse32Data, __psaved, 0);

        lstrcpy (buf+3+OBJ_LEN(name), ".mo");

	chatting("[Loading %s]\n", buf);
	p = openread(buf);
        p = REC_ALLOC1(PTR_CtoML(PTR_MLtoC(p)+8));
	p = apply_ml_fn (p, ML_unit);

        chatting("[Executing %s]\n",buf);

        Global16PointerAlloc(wsUse32Data,(DWORD)p+4,
                             (LPDWORD)&__q, (DWORD)8,0);
        __qsaved = (DWORD)__q ;

        args = loadlist (*__q);

        Global16PointerFree(wsUse32Data, __qsaved, 0);

        p = REC_SEL(apply_ml_fn(REC_SEL(p,0),args),0) ;
	enroll (name, p);

	return p;
    }
} /* end of load */


int quit (s,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n);
    console_io(2, dbuf, strlen(dbuf));
    Throw((LPCATCHBUF) &CatchBuf, 1) ;
}

int die (s,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n);
    console_io(2, dbuf, strlen(dbuf));
    Throw((LPCATCHBUF) &CatchBuf, 1) ;
}

int chatting (s,a,b,c,d,e,f,g,h,i,j,k,l,m,n)
    char *s;
{
    char dbuf[1024];
    sprintf(dbuf,s,a,b,c,d,e,f,g,h,i,j,k,l,m,n);
    console_io(2, dbuf, strlen(dbuf));
}
