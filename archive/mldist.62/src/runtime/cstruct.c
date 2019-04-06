/* cstruct.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 */

#include <sys/param.h>  /* for NOFILE */
#include "tags.h"
#include "ml_types.h"

#ifdef __STDC__
#define CONCAT(ex,suffix)	ex ## suffix
#else
#define CONCAT(ex,suffix)	ex/**/suffix
#endif

/* Exceptions are identified by (string ref) values */
#define ML_EXNID(ex,name)				\
    struct {						\
	int  tag;					\
	char s[(sizeof(name)+2) & ~3];			\
    } CONCAT(ex,_s) =					\
    {MAKE_DESC(sizeof(name)-1, tag_string), name};	\
    int CONCAT(ex,_id0)[2] = {				\
	MAKE_DESC(1, tag_array),			\
	(int)PTR_CtoML(CONCAT(ex,_s).s)};

/* A nullary exception is represented by an exn packet */
#define ML_EXN(ex,name)					\
    ML_EXNID(ex,name)					\
    int CONCAT(ex,_e0)[3] = {				\
	MAKE_DESC(2, tag_record),			\
	(int)ML_unit,					\
	(int)PTR_CtoML(CONCAT(ex,_id0)+1)};

int array0_v[]	    = {MAKE_DESC(0,tag_array)};
int bytearray0_v[]  = {MAKE_DESC(0,tag_bytearray)};

struct {int tag; ML_val_t calls, interrupts; char c[4];} gcprof =
    {MAKE_DESC(12,tag_bytearray), INT_CtoML(0), INT_CtoML(0), "(gc)"};

extern int array_v[];
extern int callc_v[];
extern int create_b_v[];
extern int create_s_v[];
extern int floor_v[];
extern int logb_v[];
extern int scalb_v[];

#define RUNVEC_SZ	8
ML_val_t runvec[RUNVEC_SZ+1] = {
    (ML_val_t)MAKE_DESC(RUNVEC_SZ, tag_record),
    PTR_CtoML(array_v),
    PTR_CtoML(callc_v),
    PTR_CtoML(create_b_v),
    PTR_CtoML(create_s_v),
    PTR_CtoML(floor_v),
    PTR_CtoML(logb_v),
    PTR_CtoML(scalb_v),
};


ML_EXN(div,"Div");
ML_EXN(overflow,"Overflow");
ML_EXN(unboundTable,"UnboundTable");
ML_EXNID(syserror,"SysError");

extern int collected0[];
extern int collectedfrom0[];
extern int current0[];
extern int gcmessages0[];
extern struct machineid machine_id;
extern int majorcollections0[];
extern int minorcollections0[];
extern int pstruct0[];
extern int ratio0[];
extern int softmax0[];
extern int sighandler0[];
extern int errstrings[];
extern int externlist0[];
extern int datalist[];

#define CSTRUCT_SZ	24
ML_val_t cstruct[CSTRUCT_SZ+1] = {
    (ML_val_t)MAKE_DESC(CSTRUCT_SZ, tag_record),
    PTR_CtoML(runvec+1),
    PTR_CtoML(div_e0+1),
    PTR_CtoML(overflow_e0+1),   		
    PTR_CtoML(syserror_id0+1),
    PTR_CtoML(unboundTable_e0+1),
    PTR_CtoML(array0_v+1),
    PTR_CtoML(bytearray0_v+1),
    PTR_CtoML(collected0+1),
    PTR_CtoML(collectedfrom0+1),
    PTR_CtoML(current0+1),
    PTR_CtoML(datalist),
    INT_CtoML(NOFILE),
    PTR_CtoML(externlist0+1),
    PTR_CtoML(gcmessages0+1),
    PTR_CtoML(&gcprof.calls),
    PTR_CtoML(machine_id.s),
    PTR_CtoML(majorcollections0+1),
    PTR_CtoML(minorcollections0+1),
#if defined(V9) || defined(HPUX) || defined(RISCos)
    INT_CtoML(3),
#else
#  ifdef VAX
    INT_CtoML(1),
#  else
    INT_CtoML(2),
#  endif
#endif
    PTR_CtoML(pstruct0+1),
    PTR_CtoML(ratio0+1),
    PTR_CtoML(sighandler0+1),
    PTR_CtoML(softmax0+1),
};


#ifdef M68
ML_EXN(ln,"Ln");
ML_EXN(sqrt,"Sqrt");

extern int arctan_v[], cos_v[], exp_v[], ln_v[], sin_v[], sqrt_v[];

#define MATHVEC_SZ	9
ML_val_t mathvec[MATHVEC_SZ+1] = {
    (ML_val_t)MAKE_DESC(MATHVEC_SZ, tag_record),
    PTR_CtoML(ln_e0+1),
    PTR_CtoML(sqrt_e0+1),
    PTR_CtoML(arctan_v),
    PTR_CtoML(cos_v),
    PTR_CtoML(exp_v),
    PTR_CtoML(ln_v),
    PTR_CtoML(sin_v),
    PTR_CtoML(sqrt_v),
};
#endif
