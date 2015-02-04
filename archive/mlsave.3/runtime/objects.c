#include "tags.h"
#include "descriptor.h"
extern int *c_alloc();

mak_str(s)
char *s;
{int len = strlen(s);
 if (len == 1) return(mak_int(*(char *)s));
 else {int i = (strlen(s)+7)/4;
       int *p = (int *)mak_obj(i,tag_string);
       p[0] = strlen(s);
       p[i-1] = 0;
       strncpy(p+1,s,p[0]);
       return((int)p);
 }
}

char *
get_str(x)
int x;
{
 if (is_ptr(x)) return(((struct string *)x)->s);
 else {char *s = (char *)c_alloc(1); *s = x; return s;}
}

mak_obj(l,t)
int l,t;
{int *p = 1 + c_alloc(l+1);
 p[-1] = mak_desc(l,t);
 return((int)p);
}

