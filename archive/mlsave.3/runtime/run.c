#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <signal.h>
#include "tags.h"
#include "descriptor.h"

extern ghandle_bsd();
extern ghandle_v9();

int (*ghandle)();

extern int runvec[];

char *freestart;

int *c_alloc(siz) int siz;
{char *s=freestart;
 freestart+= 4*siz;
 return (int *)s;
}

int openread(s) char *s;
{int fd= -1; char *p, *q; int i;
#ifdef V9
	fd = open(s,0);
#endif
#ifdef BSD
	fd = open(s,O_RDONLY,0666);
#endif
	if (fd<0) die("cannot open %s\n",s);
  read(fd,freestart,4);  /* skip past length */
  p=freestart;
  freestart+= 2 * sizeof(int);
  while (i=read(fd,freestart,4096)) freestart+=i;
  q=freestart;
  while ((int)freestart & 3) freestart++;
/* STRING dependency */
  *(int*)p = mak_desc((freestart-p)/sizeof(int) - 1,tag_string);
  p+=sizeof(int);
  *(int*)p = q-p-sizeof(int);
/* end STRING dependency */
  return (int)p;
}

closure(x) int x;
{int *p = (int *)mak_obj(1,tag_closure);
 p[0] = x;
 return (int)p;
}

extern int arenasize;

char *pervfunc = "mo/PervFunc.mo";
char *loadername = "mo/Loader.mo";
char *overloadsname = "mo/Overloads.mo";
int xflag=0;

int pstruct_v,ovstruct_v;

main(argc,argv)
int argc; 
char *argv[];
{	extern handleinterrupt(), handlefpe();
	int perv, overloads, loader, *pervl, *overl, *argrec;
	char *argname;
	char **p = argv+1;

	while (*p && **p == '-')
	  switch (p[0][1])
	    {case 'h': if (p[1])
			 {arenasize = 1024*atoi(p[1]); p+=2;}
		       else die("no -h value");
		       break;
	     case 'x': if (p[1])
			 {xflag=1; pervfunc = p[1]; p+=2;}
		       else die("no -x value");
		       break;
	     case 'y': if (p[1])
			 {xflag=2; pervfunc = p[1]; p+=2;}
		       else die("no -y value");
		       break;
	     case 'z': if (p[1])
			 {xflag=3; loadername = p[1]; p+=2;}
		       else die("no -z value");
		       break;
	    }

	    if (*p) argname= *p;
	    else if (xflag==0) die("no file to execute\n");

#ifdef V9
	 ghandle = ghandle_v9;
#endif
#ifdef BSD
	 ghandle = ghandle_bsd;
#endif
	init_gc();
	setupsignals();
	perv = closure(openread(pervfunc)+8);
	perv = apply(perv,ML_UNIT);
	perv = *(int*)perv;
	perv = apply(perv,ML_NIL);
	if (xflag==1) {chatting("Result is %d\n", (*(int*) perv)>>1); _exit(0);}
	perv = pstruct_v = apply(perv,runvec);
	if (xflag==2) {chatting("Result is %d\n", (*(int*) perv)>>1); _exit(0);}

	overloads = closure(openread(overloadsname)+8);
	overloads = apply(overloads,1);
	overloads = *(int*)overloads;
	pervl = (int *)mak_obj(2,tag_record);
	pervl[0]=perv; pervl[1]=ML_NIL;
	overloads = ovstruct_v = apply(overloads,pervl);
	overl = (int *)mak_obj(2,tag_record);
	overl[0]=overloads; overl[1]=(int)pervl;

	loader = closure(openread(loadername)+8);
	loader = apply(loader,ML_UNIT);
	loader = *(int*)loader;
	loader = apply(loader,overl);
	if (xflag==3) {chatting("Result is %d\n", (*(int*) loader)>>1); _exit(0);}
	argrec = (int *)mak_obj(1,tag_record);
	argrec[0] = mak_str(argname);
	apply(loader,argrec);
#ifdef PROFILE
	print_profile_info();
#endif
	exit(0);
}

struct exception {
	struct string *val; 
	struct string **name;
};

uncaught(e) struct exception *e;
/* STRING dependency */
{
 chatting("uncaught exception %*s with %s\n",e->name[0]->n,e->name[0]->s,
						e->val->s);
	exit(1);
}



/* temporary gc scaffolding

char *sbrk();

ghandle_simple(p) 
    struct sigcontext *p;
{
 write(1,"\nExtended arena\n",15);
 sbrk(65536);
 signal(SIGSEGV, ghandle);
}

 */

char dbuf[1024];

die(s, a, b, c, d, e, f)
char *s;
{
	sprintf(dbuf, s, a, b, c, d, e, f);
	write(2, dbuf, strlen(dbuf));
	abort();
}

chatting(s, a, b, c, d, e, f, g)
char *s;
{
	sprintf(dbuf, s, a, b, c, d, e, f, g);
	write(2, dbuf, strlen(dbuf));
}

#ifndef sigmask
#define sigmask(m)  (1<<((m)-1))
#endif

#ifdef V9
sigsetmask(){return 0;}
#endif

setupsignals()
{
#ifdef BSD
  struct sigvec a;
  a.sv_handler = ghandle;
  a.sv_mask = sigmask(SIGINT);
  a.sv_onstack=0;
  sigvec(SIGSEGV,&a,0);
  a.sv_handler = handleinterrupt;
  a.sv_mask = 0;
  sigvec(SIGINT,&a,0);
  a.sv_handler = handlefpe;
  sigvec(SIGFPE,&a,0);
#endif
#ifdef V9
  signal(SIGSEGV,ghandle);
  signal(SIGINT,handleinterrupt);
  signal(SIGFPE,handlefpe);
#endif
}
