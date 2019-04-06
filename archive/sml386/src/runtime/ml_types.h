/* ml_types.h   (MS-Windows version)
 *
 * COPYRIGHT (c) 1990 by AT&T Laboratories
 *
 * Altered 20 Dec. 1991 by:   Yngvi S. Guttesen
 *                            Department of Computer Science
 *                            The Technical University of Denmark
 *                            DK-2800 Lyngby
 *
 *
 */


#ifndef _ML_TYPES_
#define _ML_TYPES_

#include "sml.h"

#ifndef _ML_STATE_
#include "ml_state.h"
#endif
#ifndef _TAGS_
#include "tags.h"
#endif

/* All ML values are represented by a (32-bit) word.  A value is either a tagged
 * integer (unboxed), or a pointer to a heap object (boxed).
 */

/* The following macros are used to convert ML and C values.
 */
#define OBJ_isBOXED(p)          (((long)(p) & 0x1) == 0)
#define INT_MLtoC(n)            (((long)(n)) >> 1)
#define INT_CtoML(n)            ((ML_val_t)(((n) << 1) + 1))
#define PTR_MLtoC(p)            ((ML_val_t)(p))             /* carefull */
#define PTR_CtoML(p)            ((ML_val_t)(p))             /* carefull */

/* ML unit */
#define ML_unit			INT_CtoML(0)

/* ML booleans */
#define ML_false		INT_CtoML(0)
#define ML_true			INT_CtoML(1)

/* ML integers */
#define INT_incr(n, i)          (ML_val_t)((long)(n) + ((i) << 1))

/* ML record field selection */
/* #define REC_SEL(p,i) ((ML_val_t)(PTR_MLtoC(p)[(i)])) */
ML_val_t REC_SEL(long, long) ;  /* see util.asm */

#define REC_SELPTR(p, i)    PTR_MLtoC(REC_SEL(p, i))
#define REC_SELINT(p, i)    INT_MLtoC(REC_SEL(p, i))

/* Record allocation functions (see util.asm) */
ML_val_t REC_ALLOC1(ML_val_t) ;
ML_val_t REC_ALLOC2(ML_val_t, ML_val_t);
ML_val_t REC_ALLOC3(ML_val_t, ML_val_t, ML_val_t);
ML_val_t REC_ALLOC4(ML_val_t, ML_val_t, ML_val_t, ML_val_t);
ML_val_t REC_ALLOC6(ML_val_t, ML_val_t, ML_val_t,
                    ML_val_t, ML_val_t, ML_val_t);

/* ML strings. */
ML_val_t ML_eqstr(long,long);
ML_val_t ML_alloc_string(char *);

struct machineid {long d; char s[16];};

/* machine identification strings */
#define MACHINEID(id)	\
    struct machineid machine_id = { MAKE_DESC(sizeof(id)-1, tag_string), id }

/* heap allocation functions (see util.asm) */
/* #define ML_alloc_write(i, x)    (((long *)(MLState->ml_allocptr))[(i)] = (long)(x)) */

ML_val_t ML_alloc_write(long, long);
ML_val_t ML_alloc(long);

/* ML lists */
#define ML_hd(l)		REC_SEL(l, 0)
#define ML_tl(l)		REC_SEL(l, 1)
#define ML_nil                  INT_CtoML(0)
#define ML_cons(a,b)    (                                   \
            ML_alloc_write(0, MAKE_DESC(2, tag_record)),    \
            ML_alloc_write(1, (a)),                         \
            ML_alloc_write(2, (b)),                         \
            ML_alloc(2))

/* ML closures */
#define CODE_ADDR(c)            ((long)REC_SELPTR(c, 0))

/* get type and length info (boxed objects only) */
#define OBJ_DESC(obj)           ((long)REC_SEL(obj, -1))
#define OBJ_TAG(obj)		(OBJ_DESC(obj) & (power_tags-1))
#define OBJ_LEN(obj)		(OBJ_DESC(obj) >> width_tags)

/* the store list */
#define STORLST_obj(p)		REC_SEL(p, 0)
#define STORLST_objdesc(p)	OBJ_DESC(STORLST_obj(p))
#define STORLST_index(p)	REC_SELINT(p, 1)
#define STORLST_next(p)		REC_SEL(p, 2)
#define STORLST_nil		INT_CtoML(0)

/* the layout of datalist (mo objects) items */
#define MOLST_name(p)   REC_SEL(p, 0)
#define MOLST_code(p)   REC_SEL(p, 1)
#define MOLST_next(p)   REC_SEL(p, 2)
#define MOLST_nil       INT_CtoML(0)

#endif !_ML_TYPES_
