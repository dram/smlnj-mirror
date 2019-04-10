/* MacOS.dep.c */
/* 31Dec91  e  */
/* 10Dec92  e  for SML/NJ 92 */
/* 30Dec92  e  fixed "no apname" bug */

/* A MacOS version of Signals and Sbrk for ML */

#include "MacOS.dep.h"
#include "os_mac.h"

#include <errno.h>
#include <stdlib.h>
#include <unix.h>
#include <fcntl.h>
#include <console.h>
#include <string.h>
#include <console.h>

/* portions derived from: M68.dep.c
 *
 * COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
 *
 * M68 dependent code for the SML/NJ runtime kernel.
 */

#include "ml_os.h"
#include "ml_state.h"
#include "request.h"

#include "tags.h"
#include "ml_types.h"

#include <sysEqu.h>		/* for CurrentA5 */
#include <Dialogs.h>
#include <Memory.h>
#include <Events.h>
#include <Files.h>
#include <StandardFile.h>
#undef  SystemSixOrLater
#define SystemSixOrLater 1
#include <OSUtils.h>

extern void cecho2file(char *, int, FILE *);
extern long _ftype, _fcreator;
extern int	arenasize;
extern int	isExported;

int mac_sbrk_done = FALSE;
int mac_brkpt = 0;
int mac_blimt = 0;

char usual_prefix[] = ":";
char *prefix = usual_prefix;

jmp_buf restartJmpBuf;

/** Define trap numbers **/

#ifndef _Unimplemented
#define _Unimplemented	0xA89F	/* Unimplemented trap	*/
#endif

#ifndef _SysEnvirons
#define _SysEnvirons	0xA090	/* SysEnvirons Inquiry	*/
#endif

static char *eargv[4];
static char apname[64];

#if 0

short homeRefNum;

short getHome(void)	{
	IOParam pb;
    char buf[256];
	
	pb.ioNamePtr = (StringPtr)buf;
	pb.ioVRefNum = 0;
	PBGetVolSync((ParmBlkPtr)&pb);
	return(homeRefNum = pb.ioVRefNum);
}

void setHome(short refnum)	{
	IOParam pb;
	
	pb.ioNamePtr = 0;
	pb.ioVRefNum = refnum;
	PBSetVolSync((ParmBlkPtr)&pb);
}

#else /* 08Jan93  e  -- for chdir */

static short dfltWDVRefNum;
static short homeWDVRefNum;
static long  dfltWDDirID;
static long  homeWDDirID;

static void getHome(void)	{
	WDPBRec pb;
	
	pb.ioNamePtr = 0;
	pb.ioCompletion = 0;
	PBHGetVolSync(&pb);
	dfltWDVRefNum = homeWDVRefNum = pb.ioWDVRefNum;
	dfltWDDirID = homeWDDirID = pb.ioWDDirID;
}

static void setupDfltDir(void)	{
	WDPBRec pb;
	
	pb.ioNamePtr = 0;
	pb.ioCompletion = 0;
	pb.ioVRefNum = dfltWDVRefNum;
	pb.ioWDDirID = dfltWDDirID;
	PBHSetVolSync(&pb);
}

static void setupHomeDir(void)	{
	WDPBRec pb;
	
	pb.ioNamePtr = 0;
	pb.ioCompletion = 0;
	pb.ioVRefNum = homeWDVRefNum;
	pb.ioWDDirID = homeWDDirID;
	PBHSetVolSync(&pb);
}

#endif

int mac_init( char ***p_argv )
{
    SysEnvRec sysenv;
    KeyMap kMap;
    long len;
    short err, err2;
    short refNum;
    
    /* make sure System 6 or later is running */
    if ( GetTrapAddress(_SysEnvirons) == GetTrapAddress(_Unimplemented) )
    	die("SMLeNJ requires at least System 6");
    /* make sure it's a M68020+ and has an FPU */
    SysEnvirons( curSysEnvVers, &sysenv );
    if ( (sysenv.processor < env68020) || (!sysenv.hasFPU) )
    	die("SMLeNJ requires at least a M68020 and FPU");
    /* added this line for Soren/Quadra...   10Feb92  e  */
    SetApplLimit((void *)(((long )&sysenv)-32768L-2000L));
    /* find the home directory */
    getHome();
    setjmp(restartJmpBuf);
    arenasize = 0;
    isExported = 0;
    prefix = usual_prefix;
    _ftype = 'SMLi';
    _fcreator = 'NJML';
	sprintf(apname, "¥%#s", CurApName);				/* magic bullet ¥ to prevent prefix */
    thinkc_workaround();   /* see cstruct.c */
    /* err = HOpenDF( homeRefNum, 0, (unsigned char *)CurApName, fsRdPerm, &refNum ); 08Jan93 e */
    err = HOpenDF( dfltWDVRefNum, dfltWDDirID, (unsigned char *)CurApName, fsRdPerm, &refNum );
    if ( err == noErr )
    { err2 = GetEOF( refNum, &len );
      FSClose( refNum );
      if ( err2 == noErr && len > 500000 )
	  { GetKeys( kMap );
		if ( ! ( kMap[1] & 4 ) )
		{ /* option key not down */
		  chatting( "[Loading SML core image]\n");	/* note: NEEDED for console_init() */
		  eargv[0] = apname;
		  eargv[1] = "-i";
		  eargv[2] = apname;
		  eargv[3] = 0;
		  *p_argv = eargv;
		  return 3;
		}
	  }
    }
    return ccommand(p_argv);
}

static char *
strcatbut(/* char *s1, const char *s2 */)
{
	asm {
		movea.l	4(sp),a0		;  A0 = s1
		movea.l	8(sp),a1		;  A1 = s2
		clr.l	d0				;  oops, otherwise d0 might never be set - 7Jan92 
@1		tst.b	(a0)+
		bne.s	@1
		subq.l	#1,a0
@2		move.b	(a1)+,d1		;  D1.B = char
		cmpi.b	#'.',d1
		bne.s	@3
		move.l	a1,d0			;  D0.L = addr of last '.'
		subq.l	#1,d0
		cmpi.b	#'.',(a1)
		bne.s	@4
		addq.l	#1,a1
		bra.s	@2
@3		cmpi.b	#'/',d1
		bne.s	@4
		moveq	#':',d1
@4		move.b	d1,(a0)+
		bne.s	@2
	}
}

extern unsigned char * __c2p(register const char *s, register char *t);

int chdir( char *dn ) {
	WDPBRec pb;
	char cbuf[256];
	char pbuf[256];
	short err;

	if((strlen(prefix)+strlen(dn)) > 255) {
		errno = EDOM;
		return -1;
	}
	/* keep this translation consistent with that done in eopen below */
	if ( strchr( dn, ':' ) ) {
		__c2p( dn, pbuf );
	}
	else {
		if ( *dn == '¥' ) {
			dfltWDVRefNum = homeWDVRefNum;
			dfltWDDirID = homeWDDirID;
      		dn++;
		}
		if ( *dn == '/' ) { *cbuf = 0; dn++; }
		else strcpy( cbuf, prefix );
    	strcatbut( cbuf, dn );
		__c2p( cbuf, pbuf );
    }
	pb.ioNamePtr = (StringPtr )pbuf;
	pb.ioCompletion = 0;
	pb.ioVRefNum = dfltWDVRefNum;
	pb.ioWDDirID = dfltWDDirID;
	err = PBHSetVolSync(&pb);
	if ( err == noErr )
	{	pb.ioNamePtr = 0;
		pb.ioCompletion = 0;
		PBHGetVolSync(&pb);
		{	if ( err == noErr )
			dfltWDVRefNum = pb.ioWDVRefNum;
			dfltWDDirID = pb.ioWDDirID;
			return 0;
		}
	}
	return -1;
}

/* 4Feb93  e  */
OSType e_getfiletype( char *fn )
{	FInfo finf;
	Str255 pn;
	
	__c2p( fn, (char *)pn );
	if ( HGetFInfo( 0, 0L, pn, &finf ) == noErr )
		return finf.fdType;
	else
		return (OSType )'?!?!';
}

char *texttypes[] = { ".sig", ".sml", ".s", ".txt", ".lex", ".grm", "" };

int eopen(char *fn, int mode)
    {
    char buf[256];
    char *p, **q;
    int res;
    
	/* 08Jan93  e  -- new pathname translation */
	/* keep this translation consistent with that done in chdir above */
	if((strlen(prefix)+strlen(fn)) > 255) {
		errno = EDOM;
		return -1;
	}
	setupDfltDir();
	if ( strchr(fn,':') ) {
		strcpy(buf, fn);
    	p=strrchr(buf,'.');
	}
	else {
		if ( *fn == '¥' ) {
			setupHomeDir();
			if(!strcmp(fn,apname)) _ftype = 'APPL';
      		fn++;
		}
		if ( *fn == '/' ) { *buf = 0; fn++; }
		else strcpy(buf, prefix);
    	p = strcatbut(buf,fn);
    }
#if 0
    if (p) {
    	if     (!strcmp(p,".sig")) mode |= O_TEXT;
    	else if(!strcmp(p,".sml")) mode |= O_TEXT;
    	else if(!strcmp(p,".s"))   mode |= O_TEXT;
    	else if(!strcmp(p,".txt")) mode |= O_TEXT;
    	else if(!strcmp(p,".lex")) mode |= O_TEXT;
    	else if(!strcmp(p,".grm")) mode |= O_TEXT;
    }
#else
	/* 4Feb93  e  */
    if ( ( ( ! p ) || strcmp( p, ".mo" ) ) && e_getfiletype( buf ) == 'TEXT' )
    	 mode |= O_TEXT;
    else if (p) {
    	for ( q = &texttypes[0]; **q != '\0'; q++ )
    		if ( ! strcmp( p, *q ) ) mode |= O_TEXT;
    }
#endif
    if (mode & O_TEXT) _ftype = 'TEXT';
    res = open(buf, mode);
    _ftype = 'SMLi';
    return res;
}

#if 0

static Point putWhere = { 106, 104 };

/* dribble was... [didn't work!?]
			{
			char **av;
			int ac;
			ac = ccommand(&av);
			}
			break;
*/

void doDribble(void)
	{
	SFReply reply;
	char buf[256];
	IOParam pb;
	
	/*  present dialog  */
	SFPutFile(putWhere, (ConstStr255Param)"", (ConstStr255Param)"", (DlgHookProcPtr)NULL, &reply);
	if (reply.good) {
		/*  redirect stdout  */
		/* setfile((char *) scratch.fName, &scratch); */
		setHome(reply.vRefNum);
		/* sprintf(buf, "%#s", reply.fName); */
		cecho2file((char *) reply.fName, 0, stdout);
		setHome(homeRefNum);
	}
}

#endif

void nalert(char *s)
{
	register int i;
	char p0[255];
	p0[0] = (char) strlen(s);
	strcpy(p0+1,s);
	for(i=1;i<255;i++)
		if (p0[i] == '\n') p0[i] = '\r';
		
	InitCursor();		/* get standard arrow cursor */
	ParamText((ConstStr255Param)p0,"\p","\p","\p");
	NoteAlert((short )ok_alertID,0L);
}

void eDoDialog(int sel)
{
	
	switch(sel) {
	  case 0: {
		DialogPtr dlg;
		short i;
		/* do About */
		/* SetDAFont(helvetica); */
		dlg=GetNewDialog(about_dlogID,NULL,(WindowPtr)(-1L));
		ModalDialog((ModalFilterProcPtr)NULL,&i);
		DisposDialog(dlg);
		/* SetDAFont(systemFont); */
		break;
		}
	  case 1: {
		char s[64];
		sprintf(s,"Heap size is %ldk.\n\nPartition size is %ldk.",
		          arenasize/1024L, (mac_blimt - mac_brkpt)/1024L);
		nalert(s);
		break;
		}
#if 0
	  case 2: {
		doDribble();
		break;
		}
#endif
	}
}

/*  Sbrk */
/**
 ** This implements sbrk/brk using MacOS.
 ** it is based on the mach implementation in callgc.c
 ** 01Jan92  e
 **/

int sbrk(incr)
    int incr;
{
    Size largestSz, cangrowSz;
    Ptr mptr;

    if (incr)
		die("sbrk called with nonzero value");
    if (mac_sbrk_done == FALSE) {
		MaxApplZone();	/* expand the heap */
		largestSz = MaxMem(&cangrowSz);
		cangrowSz = largestSz + cangrowSz - 393216L;
		/*384L*1024L=393216=384k for runtime system*/
		if(!(mptr = NewPtr(cangrowSz)))
			die("sbrk cannot allocate: %dK", cangrowSz/1024);
		mac_brkpt = ((int)(mptr + 8191) & -8192); /* make it a "page" boundary */
		mac_blimt =  (int)(mptr + cangrowSz);
		mac_sbrk_done = TRUE;
    }
    return(mac_brkpt);
}

int brk(pos)
    int pos;
{
    if (pos > mac_blimt)
	return TRUE;
    else
	return FALSE;
}

int getpagesize()	{
    return(8192);
}

int get_edata()	{
    return((mac_blimt + 8191) & -8192);
}

int get_etext()	{
    die("get_etext: not done yet!");
    return(0);
}

/* signals */

static __sig_func_e e_sig[__NSIG_e+1];

void
e_raise_guts(struct sigcontext *scp)
{
	__sig_func_e f;
	int i = scp->sig;
	
	if (i <= 0 || i > __NSIG_e) {
		errno = EINVAL;
		return;
	}
	f = e_sig[i];
	if (f != SIG_IGN_e) {
		/* e_sig[i] = SIG_DFL_e; */
		if (f == SIG_DFL_e)
			_exit(0);
		(*f)(i, (scp->code & 0x0fff), scp);
	}
}

void
init_TRAPS();
void
handleTRAP()
	{
	asm {
@handleTRAP:						; rewritten to handle Virtual Memory -- 10Feb92  e
		MOVE.L	#SIGTRAP, -(SP)		; push sig
		MOVEM.L	D0-D7/A0-A6, -(SP)	;  push 15 regs
		MOVE.L	SP, -(SP)			;   push sig context ptr
		MOVE.L	CurrentA5, A5		;    for C variable access
		JSR		e_raise_guts		;    called in supervisor mode!!!!!
		ADDQ.L	#4, SP				;   drop sig context ptr
		MOVEM.L	(SP)+, D0-D7/A0-A6	;  pop  15 regs
		ADDQ.L	#4, SP				; drop sig
		RTE							; return
		;
@handleFPU:							; added to handle M68882 FPU EXC_PEND -- 12Feb92  e
		MOVE.L	#SIGTRAP, -(SP)		; push sig
		MOVEM.L	D0-D7/A0-A6, -(SP)	;  push 15 regs
		MOVE.L	SP, -(SP)			;   push sig context ptr
		MOVE.L	CurrentA5, A5		;    for C variable access
		JSR		e_raise_guts		;    called in supervisor mode!!!!!
		ADDQ.L	#4, SP				;   drop sig context ptr
		FSAVE	-(SP)				; dump the FPU
		; M68881 check not necessary
		; MOVEQ	#$18, D0			; frame size for M68881
		; CMP.B	D0, 1(SP)			; FPU frame size
		; BEQ.S	@rtn				; if M68881, we're done
		MOVE.B	(SP), D0			; null frame?
		BEQ.S	@rtn				; if so, done
		CLR.L	D0					; get frame size
		MOVE.B	1(SP), D0			; into D0
		BSET	#3, (SP, D0)		; set EXC_PEND bit of BIU
@rtn:	FRESTORE (SP)+				; load the FPU
		MOVEM.L	(SP)+, D0-D7/A0-A6	;  pop  15 regs
		ADDQ.L	#4, SP				; drop sig
		RTE							; return
		;
extern init_TRAPS:
		LEA.L	@handleTRAP, A0
		MOVE.L	A0, 0x14			; dbz
		MOVE.L	A0, 0x1C			; TRAPcc
		LEA.L	@handleFPU, A0		;				12Feb92  e
		MOVE.L	A0, 0xC8			; FPdbz
		MOVE.L	A0, 0xD4			; FPovf
		MOVE.L	A0, 0xCC			; FPund
		; for Virtual Memory Mode					10Feb92  e
		; this is not strictly necessary, 
		;   but speeds things up a bit
		MOVEQ	#0, D0				; in VM Mode?
		DC.W	0xA08D				; _DebugUtil
		SUBQ.L	#8, D0				; VM?
		BLT.S	@punt				; skip if not
		MOVEQ	#8, D0				; enter supervisor mode
		DC.W	0xA08D				; _DebugUtil
		LEA.L	@handleTRAP, A0
		MOVEC	VBR, A1
		MOVE.L	A0, 0x14(A1)		; dbz
		MOVE.L	A0, 0x1C(A1)		; TRAPcc
		LEA.L	@handleFPU, A0		;				12Feb92  e
		MOVE.L	A0, 0xC8(A1)		; FPdbz
		MOVE.L	A0, 0xD4(A1)		; FPovf
		MOVE.L	A0, 0xCC(A1)		; FPund
		MOVE	D0, SR				;  exit supervisor mode
@punt:
	}
}

void
e_raise(int sig)
	{
	asm {
		/* fix the stack frame to look like an exception frame */
		MOVE.W	#0, -(SP)			; dummy SR
		MOVE.L	6(SP), -(SP)		; sig
		MOVEM.L	D0-D7/A0-A6, -(SP)	; 15 regs
		MOVE.L	SP, -(SP)			;  sig context ptr
		MOVE.L	CurrentA5, A5		; for C variable access
		JSR		e_raise_guts
		ADDQ.L	#4, SP				; drop context ptr
		MOVEM.L	(SP)+, D0-D7/A0-A6	; 15 regs
		ADDQ.L	#6, SP				; drop dummy SR & sig
	}
	if (sig <= __NSIG)
		signal(sig, e_raise);
}

void
e_signal(int i, __sig_func_e f)
{
	
	if (i <= 0 || i > __NSIG_e) {
		errno = EINVAL;
		return;
	}
	e_sig[i] = f;
	if (i <= __NSIG)
		signal(i, e_raise);
}

void
e_restart_handler(int sig)
	{
	signal(sig, e_restart_handler);
	longjmp(restartJmpBuf, 2);
}

/* end of MacOS.dep.c */
