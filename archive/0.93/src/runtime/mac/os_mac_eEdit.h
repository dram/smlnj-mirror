/* eEdit.h     */
/* 14Apr92  e  */
/* 28Sep92  e  added print stuff */

#include <Types.h>
#include <THINK.h>
#include <Memory.h>
#include <Quickdraw.h>
#include <Fonts.h>
#include <Controls.h>
#include <Windows.h>
#include <Events.h>
#include <Scrap.h>
#include <TextEdit.h>
#include <ToolUtils.h>
#include <Errors.h>
#include <Printing.h>
#include <Menus.h>

#define CONTEXT_LINES 		3			/* lines to show around caret for eTeShowCaret() */

#define	PAGE_DELAY			1

#define MIN_RUNS			(10)		/* # runs to extend the run starts block */
#define MIN_LINES			(100)		/* # lines to extend the line starts block */
/* 28May92  e  was:
#define MAX_SCROLL_CHARS	(1024)		/* max # of chars to scroll horizontally */

/*
 * ASCII character definitions
 */
#define ENTER			0x03			/* keypad ENTER */
#define DELETE			0x08			/* delete/backspace key */
#define TAB				0x09			/* normal TAB    - stored in text */
#define RETURN			0x0D			/* normal RETURN - stored in text */
#define SPACE			0x20

/*
 * ARROW key definitions - uses ASCII codes, not keycodes.
 */
#define LEFT_ARROW		0x1C
#define RIGHT_ARROW		0x1D
#define UP_ARROW		0x1E
#define DOWN_ARROW		0x1F

/*
 * KEYCODE definitions for function keys on extended keyboard
 */
#define KeyF1			0x7A
#define KeyF2			0x78
#define KeyF3			0x63
#define KeyF4			0x76
#define KeyF5			0x60
#define KeyF6			0x61
#define KeyF7			0x62
#define KeyF8			0x64
#define KeyF9			0x65
#define KeyF10			0x6D
#define KeyF11			0x67
#define KeyF12			0x6F
#define KeyF13			0x69
#define KeyF14			0x6B
#define KeyF15			0x71

#define KeyHelp			0x72
#define KeyHome			0x73
#define KeyPageUp		0x74
#define KeyDel			0x75
#define KeyEnd			0x77
#define KeyPageDown		0x79

/*  8Jul92  e   for: curStyle  */
#define NOSTYLE (-1)

typedef long LineRec;

typedef long RunRec;

struct ChPos {
    short v;
    short h;
};
typedef struct ChPos ChPos;

typedef struct eRec
{	Rect 		viewRect;
	Rect 		caretRect;
	ChPos 		bounds;
	ChPos 		position;
	ChPos 		caretChPos;
	ChPos 		selStart;
	ChPos 		selEnd;
	ChPos 		writeChPos;		/* chpos for writer (vs. human editor) 8Jul92  e  */
	Boolean 	selActive;
	Boolean 	caretState;
	Boolean 	active;
	Boolean 	dirty;
	short 		leftMargin;
	short 		topMargin;
	short 		hContext;		/* #chars/#lines visible around caret in eTeShowCaret() */
	short 		vContext;
	short 		hOverlap;		/* #chars/#lines visible between one page scroll & the next */
	short 		vOverlap;
	short 		hStep;			/* #chars/#lines to step for scroll arrow */
	short 		vStep;
	short 		wrap;			/*              5Jul92  for line wrap, max chars/line */
	short		tabStops;		/*              8Jul92  measured in spaces */
	short		autoInd;		/* 			   25Jan93  auto indent flag   */
	short		spare;
	short		hSpan;			/* computed */
	short		vSpan;
	short 		hScale;			/* computed - pixels per scroll unit */
	short 		vScale;
	long 		hOrigin;		/* computed */
	long 		vOrigin;
	short 		width;			/* computed */
	short 		height;
	short 		caretHeight;	/* computed */
	short 		fontAscent;
	short 		tabWidth;		/* computed     8Jul92  renamed */
	short 		spaceWidth;
	short 		maxRight;		/* computed */
	short 		qRuns;
	short 		linesAllocated;	/* computed */
	short 		runsAllocated;
	short 		curStyle;		/* computed     8Jul92  to speed up drawing */
	short		fustStyle;
	TextStyle	style[2];		/* use eTeSetStyles() */
	ProcPtr 	eTeWordBreak;	/* use eTeWordBreak() */
	WindowPtr 	macPort;		/* use eTeNew() */
	ControlHandle hSBar;		/* use eTeNew() */
	ControlHandle vSBar;		/* use eTeNew() */
	Handle 		hText;			/* use eTeNew() or eTeSetTextHandle() */
	LineRec 	**hLines;		/* computed */
	long 		**hRuns;		/* computed */
	THPrint		hPrint;			/* Handle to print record for file  	 28Sep92  e  */
} eRec, **eTeHandle;

OSErr	eTePrint( eRec **hE, Boolean jobDlg, Boolean firstJob, Str255 flnm);	/*   28Sep92  e  */

ChPos 	eTeOffsetToChPos( eRec **hE, register long anOffset );
long 	eTeChPosToOffset( eRec **hE, ChPos aPos );
long 	eTeTextLength( eRec **hE );

void 	eTeGetRun( eRec **hE, long *sta, long *end );

void 	eTeScroll( eRec **hE, short hDelta, short vDelta, Boolean redraw );
void 	eTeScrollTo( eRec **hE, ChPos aPosition, Boolean redraw );
void 	eTeShowCaret( eRec **hE );

void 	eTeSetWrap( eRec **hE, short wrap );
void 	eTeSetTabStop( eRec **hE, short aTabStop );
void 	eTeSetWordBreak( eRec **hE, ProcPtr aFunc );

void 	eTePutScrap();          						/*  13Aug92  e  */
void 	eTeGetScrap();          						/*  13Aug92  e  */
void	eTeUndo( eRec **hE );   						/*  13Aug92  e  */
void	eTeEditMenuUpdate( eRec **hE, MenuHandle hM );	/*  13Aug92  e  */

void 	eTeIdle( eRec **hE );
void 	eTeActivate( eRec **hE );
void 	eTeDeactivate( eRec **hE );
void 	eTeDraw( eRec **hE, Rect *area );
void 	eTeUpdate( eRec **hE );
void 	eTeClick( eRec **hE, Point hitPt, short modifierKeys, long when );
void 	eTeCut( eRec **hE );
void 	eTeCopy( eRec **hE );
void 	eTeDelete( eRec **hE );
void 	eTeKillTo( eRec **hE, ChPos chPos );  /* 12Jul92  e */
void 	eTePaste( eRec **hE, short style );
void 	eTeInsert( eRec **hE, Ptr textPtr, long numChars, short style );
void 	eTeWrite(  eRec **hE, Ptr textPtr, long numChars, short style ); 	 /*  9Jul92  e */
void 	eTeTranspose( eRec **hE, long beg, long midl, long midr, long end ); /* 22Jul92  e */
void 	eTeKey( eRec **hE, char theChar, Byte keyCode, short modifiers, short style );
void 	eTeSetSelect( eRec **hE, long aStart, long anEnd );

short 	eTeSetTextPtr( eRec** hE, Ptr pT, long numChars );
short 	eTeSetTextHandle( eRec** hE, Handle hT );
short 	eTeSetTextHandleDetabify( eRec **hE, Handle hT, short tabstops );

void 	eTeNewView( eRec **hE, Rect *view );
void 	eTeSetStyles( eRec **hE, TextStyle *ts0, TextStyle *ts1 );

void 	eTeInit( void );
eRec  **eTeNew( WindowPtr macPort, Rect viewRect, short tabStops, short wrap,
				 short autoInd, ControlHandle aHSizing, ControlHandle aVSizing );
void 	eTeDispose( eRec **hE );

extern EventRecord		gLastMouseUp;
extern EventRecord		gLastMouseDown;
extern long				gMaxSleep;

/* end of eEdit.h */
