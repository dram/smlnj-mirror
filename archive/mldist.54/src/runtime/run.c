/* Copyright 1989 by AT&T Bell Laboratories */
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <signal.h>
#include "tags.h"
#include "descriptor.h"
#include "prof.h"
extern double atof();

extern int cstruct[];

extern int profvec[];

extern int saved_dataptr;

struct list {mlstring name; struct list *next;};

struct list *datalist;

extern int mo_share[];

int openread(s) char *s;
{int fd= -1; char *p, *q; int i;
 {struct list *d; mlstring ss=(mlstring)mak_str(s);
  for(d= datalist; (int)d!=1; d=d->next)
    if (ml_eqstr(ss,d->name)) return (int)(d->next->name-4);
 }

#if defined(V9) || defined(HPUX)
	fd = open(s,0);
#endif
#ifdef BSD
	fd = open(s,O_RDONLY,0666);
#endif
	if (fd<0) quit("cannot open %s\n",s);
  p=(char *)saved_dataptr;
  saved_dataptr += sizeof(int);
  while (i=read(fd,saved_dataptr,4096)) saved_dataptr+=i;
  q=(char*)saved_dataptr;
  while (saved_dataptr & 3) saved_dataptr++;
  *(int*)p = mak_desc(q-p-sizeof(int),tag_string);
  return (int)p;
}

char *load();

closure(x) int x;
{int *p = (int *)mak_obj(1,tag_record);
 p[0] = x;
 return (int)p;
}

extern int new_size;
extern int resettimers();
#ifdef M68
extern int math_functor, mathvec;
#endif

char *pervfunc = "mo/PervFunc.mo";
char *loadername = "mo/Loader.mo";
char *mathfunc = "mo/Math.mo";
int xflag=0;
extern int gcmessages0[], ratio0[], softmax0[], pstruct0[];
#define gcmessages (gcmessages0[1])
#define pstruct (pstruct0[1])
#define ratio (ratio0[1])
#define softmax (softmax0[1])
extern int cause;
char **global_argv;
main(argc,argv) int argc; 
char *argv[];
{
	int perv, core, math, loader, *argrec;
	char *argname;
	char **p = argv+1;

        global_argv=argv;
        if (cause==CAUSE_EXPORT) restarter();
	gcmessages = ML_INT(2);
        ratio=11;
	softmax=1024*1024*100*2 + 1;
	while (*p && **p == '-')
	  switch (p[0][1])
	    {case 'h': if (p[1])
			 {new_size = 1024*atoi(p[1]); p+=2;}
		       else quit("no -h value");
		       break;
	     case 'r': if (p[1]) {ratio = atoi(p[1])*2+1; p+=2;
				  if (ratio<7) ratio=7;
				 }
		       else quit("no -r value");
		       break;
	     case 'm': if (p[1]) {softmax = 1024*atoi(p[1])*2+1; p+=2;}
		       else die("no -m value");
		       break;
	     case 'g': if (p[1])
			 {gcmessages0[1] = ML_INT(atoi(p[1])); p+=2;}
		       else quit("no -g value");
		       break;
	     case 'x': if (p[1])
			 {xflag=1; pervfunc = p[1]; p+=2;}
		       else quit("no -x value");
		       break;
	     case 'y': if (p[1])
			 {xflag=2; pervfunc = p[1]; p+=2;}
		       else quit("no -y value");
		       break;
	     case 'z': if (p[1])
			 {xflag=3; loadername = p[1]; p+=2;}
		       else quit("no -z value");
		       break;
             case 't': if (p[1])
			 {mathfunc = p[1]; p+=2;}
		       else quit("no -t value");
		       break;
	    }

	    if (*p) argname= *p;
	    else if (xflag==0) quit("no file to execute\n");

	setupsignals();
	resettimers();
	init_gc();
	construct_datalist();

#ifdef PROFILE
{int i;
 for(i=0; i<PROFSIZE; i++) profvec[i] = ML_INT(0);
}
#endif

	perv = (int)load(mak_str("CoreFunc"));
	enroll(mak_str("Core"),core=apply(perv,cstruct+1));
#ifdef M68
	math = (int)&mathvec;
	enroll(mak_str("Math"),math);
#else
	math = (int)load(mak_str("Math"));
#endif
	perv = pstruct = (int)load(mak_str("Initial"));
	if (xflag==1) {chatting("Result is %d\n",(*(int*) perv)>>1); _exit(0);}

	loader = (int)load(mak_str("Loader"));
	if (xflag==3) {chatting("Result is %d\n", (*(int*) loader)>>1); _exit(0);}
	argrec = (int *)mak_obj(4,tag_record);
	argrec[0] = core;
	argrec[1] = perv;
	argrec[2] = math;
	argrec[3] = mak_str(argname);
	apply(loader,argrec);
#ifdef PROFILE
	print_profile_info();
#endif
#ifdef GCPROFILE
	print_gcprof();
#endif
	exit(0);
}

struct exception {
	mlstring val; 
	mlstring *name;
};

uncaught(e) struct exception *e;
{int descr;
 if ((int)e->val & 1)
 chatting("Uncaught exception %.*s with %d\n",
	  (((int *)*e->name)[-1])>>width_tags,
	  e->name[0],
	  (int)e->val);
 else if (((descr=((int*)e->val)[-1])&mask_tags) == tag_string
	  || (descr&mask_tags) == tag_embedded) /* possible bug on reals */
      chatting("uncaught exception %.*s with \"%.*s\"\n",
	       (((int *)*e->name)[-1])>>width_tags,
	       e->name[0],
	       descr>>width_tags,
	       e->val);
 else chatting("uncaught exception %.*s with <unknown>\n",
	       (((int *)*e->name)[-1])>>width_tags,
	       e->name[0]);
 exit(1);
}


int quit(s, a, b, c, d, e, f)
char *s;
{char dbuf[1024];
	sprintf(dbuf, s, a, b, c, d, e, f);
	write(2, dbuf, strlen(dbuf));
	exit (2);
}

die(s, a, b, c, d, e, f)
char *s;
{char dbuf[1024];
	sprintf(dbuf, s, a, b, c, d, e, f);
	write(2, dbuf, strlen(dbuf));
	/* abort(); */
        exit(3);
}

chatting(s, a, b, c, d, e, f, g)
char *s;
{char dbuf[1024];
	sprintf(dbuf, s, a, b, c, d, e, f, g);
	write(2, dbuf, strlen(dbuf));
}


mlstring names[10];
char * objs[10];
int objcount;

enroll(name,obj) mlstring name; char *obj;
{
 names[objcount]=name; objs[objcount]=obj;
 objcount++;
}

#define get_slen(x)  (  ((int*)(x))[-1]>>width_tags )
char *lookup(name) mlstring name;
{int i;
 for(i=0;i<objcount;i++)
   {mlstring s = names[i];
    if (get_slen(name)==get_slen(s) && !strncmp(name,s,get_slen(s)))
	return objs[i];
   }
 return 0;
}

int *endmo;

construct_datalist()
{struct list *g, *h, *i; int *s = mo_share; int *g0;
 static int space[600]; /* enough for 200 entries */
 datalist = (struct list *)1;
 g0=space;
 while (*s != mak_desc(0,tag_string))
   {*g0++ = mak_desc(2,tag_record);
    g = (struct list *)g0; g0+=2;
    g->name = (mlstring) (s+1);
    s = s + 1 + ((((*s)>>4)+3)>>2);
    g->next = datalist;
    datalist=g;
   }
 endmo=s;
 h= datalist; g= (struct list *)1; 
 while (h!=(struct list *)1)
   {i=h->next; h->next=g; g=h; h=i;}
 datalist=g;
 /* the 13 is the position of the datalist in cstruct[], see cstruct.c */
 cstruct[13]= (int)g;
}
 
struct list *loadlist(names) struct list *names;
{struct list * g;
 if ((int)names==1) return (struct list *)1;
 g = (struct list *) mak_obj(2,tag_record);
 g->name = (mlstring) load(names->name);
 g->next = loadlist(names->next);
 return g;
}

char *load(name) mlstring name;
{mlstring p; char buf[50];
 if (p=lookup(name)) return p;
 strcpy(buf,"mo/");
 strncpy(buf+3,name,get_slen(name));
 strcpy(buf+3+get_slen(name),".mo");
 chatting("[Loading %s]\n",buf);
 p = (char *)closure(openread(buf)+12);
 p = (char *)apply(p,ML_UNIT);
 chatting("[Executing %s]\n",buf);
 p = (char *)(((int*)(apply(((int*)p)[0], loadlist(((int*)p)[1]))))[0]);  
/* p = (char *)apply(((int*)p)[0], loadlist(((int*)p)[1])); */
 enroll(name,p);
 return p;
}


/* profiling stuff */
extern int *current0[];

handleprof()
{
 current0[1][1] += 2;
}
