#include "tags.h"
#include "descriptor.h"
#include "ml.h"
extern int *c_alloc();

/* NOTE: all these functions use c_alloc, and modify freestart.  This means
   that freestart must be properly set from the data pointer before calls
   to these functions, and reset afterwards (apply does this, but beware
   calls from other functions, like raise_systemcall). */

/* Make a C string into an ML string.  Used by restartFn (through
   mak_str_lst()), _raise_systemcall, and when system_v raises SystemCall.
   These last two are probably unsafe.  It definitely creams r1 on the vax. */
mak_str(s)
char *s;
{int len = strlen(s);
 if (len == 1) return(mak_int(*(char *)s));
 else {int i = (len+3)>>2;
       int *p = 1 + c_alloc(i+1);
       p[i-1] = 0; /* allocate */
       p[-1] = (len<<width_tags) + tag_string;
       strncpy(p,s,len);
       return((int)p);
 }
}

/* Make an ML string into a C string */
char *
get_str(x)
int x;
{
 if (is_ptr(x)) return((char *)x);
 else {char *s = (char *)c_alloc(1); *(int *)s = 0; *s = x; return s;}
}

/* No good for strings/bytearrays (allocates by word, not byte).
   Also a dangerous, as the last word should be allocated first for
   garbage collector safety.  Only used in bootup (run.c) and restartFn
   (through mak_str_lst()). */
mak_obj(l,t)
int l,t;
{int *p = 1 + c_alloc(l+1);
 p[-1] = mak_desc(l,t);
 return((int)p);
}

/* Take a vector of strings (like argv) and turn it into an ML string list.
   Uses mak_obj, so it is unsafe and should be used with caution.  Only
   restartFn uses it for now. */
int *
mak_str_lst(sl)
char **sl;
{
 int *list;
 if (!(*sl)) return ((int *)ML_NIL);
 list = (int *)mak_obj(2,tag_record);
 list[1] = (int)mak_str_lst(sl+1);
 list[0] = mak_str(*sl);
 return list;
}


