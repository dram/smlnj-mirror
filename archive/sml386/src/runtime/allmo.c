/* allmo.c:
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * This is a stub allmo file for the noshare version.
 */

#include "tags.h"
#include "ml_types.h"

static struct { int tag; char str[16]; } never0 =
    { MAKE_DESC(13, tag_string), "%never match%\0\0\0" };

struct molist_t {
    ML_val_t	    name;
    ML_val_t	    code;
    ML_val_t	    next;
} datalist = {
    PTR_CtoML(never0.str), PTR_CtoML(0), MOLST_nil
};
