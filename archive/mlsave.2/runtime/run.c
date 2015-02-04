#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <signal.h>

extern ghandle_bsd();
extern ghandle_v9();

int signalmask;

int (*ghandle)();


extern int runvec[], openf_v9[], *openslot, *isattyslot, isatty_v9[];

char *freestart, *alloc_s_cnt, *freelimit, *except, *origfreestart;

int *c_alloc(siz) int siz;
{char *s=freestart;
 freestart+= 4*siz;
 return (int *)s;
}

int ptrtag(i) int i;
{return i*8+1;}

int chartag(i) int i;
{return i*8+5;}

struct cl {
	int desc; 
	int *text;
} *init;

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
  *(int*)p = chartag((freestart-p)/sizeof(int) - 1);
  p+=sizeof(int);
  *(int*)p = q-p-sizeof(int);
  return (int)p;
}

closure(x) int x;
{int *p = 1+c_alloc(2);
 p[-1] = ptrtag(1);
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
	int perv, overloads, *overl, *runl, loader, *pervl, *argrec;
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
	 openslot = openf_v9;
	 isattyslot = isatty_v9;
	 ghandle = ghandle_v9;
#endif
#ifdef BSD
	 ghandle = ghandle_bsd;
#endif
	init_gc();
	setupsignals();
	perv = closure(openread(pervfunc)+8);
	perv = apply(perv,1);
	perv = *(int*)perv;
	perv = apply(perv,1);
	if (xflag==1) {chatting("Result is %d\n", (*(int*) perv)>>1); _exit(0);}
	perv = pstruct_v = apply(perv,runvec);
	if (xflag==2) {chatting("Result is %d\n", (*(int*) perv)>>1); _exit(0);}
	overloads = closure(openread(overloadsname)+8);
	overloads = apply(overloads,1);
	overloads = *(int*)overloads;
	pervl = 1+c_alloc(3);
	pervl[-1] = ptrtag(2);
	pervl[0]=perv; pervl[1]=1;
	overloads = ovstruct_v = apply(overloads,pervl);
	overl = 1+c_alloc(3);
	overl[-1] = ptrtag(2);
	overl[0]=overloads;
	overl[1]=(int)pervl;

	loader = closure(openread(loadername)+8);
	loader = apply(loader,1);
	loader = *(int*)loader;
	loader = apply(loader,overl);
	if (xflag==3) {chatting("Result is %d\n", (*(int*) loader)>>1); _exit(0);}
	argrec = 1+(int*)c_alloc(2);
	argrec[-1] = ptrtag(1);
	if (strlen(argname)==1) argrec[0] = argname[0]*2+1;
        else	{int i = (strlen(argname)+7)/4;
		 int *p = 1+c_alloc(i+1);
		 p[-1] = chartag(i);
		 p[0] = strlen(argname);
		 p[i-1]=0;
		 strncpy(p+1,argname,p[0]);
		 argrec[0] = (int)(p);
	        }
	apply(loader,argrec);
	print_profile_info();
	exit(0);
}

struct string {
	int n; 
	char s[4];
};
struct exception {
	struct string *val; 
	struct string **name;
};
uncaught(e) struct exception *e;
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
