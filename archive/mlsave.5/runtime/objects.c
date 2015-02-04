#include "tags.h"
#include "descriptor.h"
extern int *c_alloc();

mak_str(s)
char *s;
{int len = strlen(s);
 if (len == 1) return(mak_int(*(char *)s));
 else {int i = (len+3)>>2;
       int *p = 1 + c_alloc(i+1);
       p[-1] = (len<<width_tags) + tag_string;
       p[i-1] = 0; /* allocate */
       strncpy(p,s,len);
       return((int)p);
 }
}

char *
get_str(x)
int x;
{
 if (is_ptr(x)) return((char *)x);
 else {char *s = (char *)c_alloc(1); *s = x; return s;}
}

/* no good for strings/bytearrays */
mak_obj(l,t)
int l,t;
{int *p = 1 + c_alloc(l+1);
 p[-1] = mak_desc(l,t);
 return((int)p);
}

