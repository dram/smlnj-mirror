/* ml_objects.c   (MS-Windows version)
 *
 * COPYRIGHT 1990 by AT&T Bell Laboratories.
 *
 * Altered 20 Dec. 1991 by:    Yngvi S. Guttesen
 *                             Department of Computer Science
 *                             The Technical University of Denmark
 *                             DK-2800 Lyngby
 */

#include "sml.h"
#include "ml_state.h"
#include "ml_types.h"

/* the null string */
EXTASM(string0)

/* ML_alloc_string:
 * Allocate and initialize an ML string.
 */
ML_val_t ML_alloc_string (s)
    char *s;
{
    int      len, n;
    long     res;

    len = lstrlen(s);
    if (len == 0)
        return PTR_CtoML(string0+1);
    else if (len == 1)
        return ((long)INT_CtoML(*s));
    else {
        char _far *__p ;
        DWORD      __psaved ;
	n = (len + 3) >> 2;
	ML_alloc_write (0, MAKE_DESC(len, tag_string));
	res = ML_alloc (n);
        Global16PointerAlloc(wsUse32Data,(DWORD)res,
                             (LPDWORD)&__p, (DWORD)len,0);
        __psaved = (DWORD)__p ;
        for ( ; len>0 ; len--)
            *__p++ = *s++;
        Global16PointerFree(wsUse32Data, __psaved, 0);
	return res;
    }
} /* end of ML_alloc_string. */

/* make_str_list:
 * Make a ML list of ML strings from a NULL terminated (char *) vector.
 */
ML_val_t make_str_list (vec)
    char	**vec;
{
/*    register int i;
    ML_val_t	l;

    for (i = 0;  vec[i] != 0;  i++)
	continue;
    for (l = ML_nil;  --i >= 0; ) {
	ML_val_t s = ML_alloc_string(vec[i]);
	l = ML_cons(s, l);
    }

    return l;
*/
}

extern stringcompare(long, long, long);

/* ML_eqstr:
 * ML string equality.
 */
ML_val_t ML_eqstr (s1, s2)
    long       s1, s2;
{
    long l;

    if (s1 == s2)
        return (long) 1;
    else if ((l = OBJ_LEN(s1)) != OBJ_LEN(s2))
        return (long) 0;
    else
        return stringcompare(s1,s2,l);

}
