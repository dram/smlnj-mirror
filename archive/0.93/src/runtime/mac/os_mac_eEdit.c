/* os_mac_eEdit.c 
 * 8Apr92  e
 */

#include <MacHeaders>

#include "os_mac_eEdit.h"

/* an editor for text files by e
   questions/comments via Internet <e@Flavors.COM> */
/* Copyright © e, 1992. All rights reserved.
	Developed using THINK C 5.0.1 for use with Gambit Scheme.
	This code may be freely distributed as long as this notice remains.
   based upon CEditor.C...
	Copyright © BRH Toolsmith, 1992. All rights reserved.
	Developed using THINK C 4.0.2 and Symantec's OOP libraries.
	Portions of this code courtesy Symantec, Inc.
	This code may be freely distributed as long as this notice remains. If you wish
	to distribute modifications to these classes, please make a copy of the class
	and implement your changes there. That's the beauty of OOP!
since:
a) I was not using Symantec's OOP, and
b) CEditor.C had (due to the dependence on the Symantec libraries) a serious limitation,
 to wit a limit of 32,767 pixels of document height (about 3000 lines of text),
eEdit.c is a recasting of CEditor.C in vanilla C with very significant enhancements and fixes.
*/

/* 10Dec92  e  -- to do:
- better out-of-memory error handling
*/

/* for arrow keys which ~work like ThinkC editor instead of Apple standard
#define THINK_ARROWS (1)
*/
/* for RETURN which auto-tabs like ThinkC editor - not good with Gambit
16Dec92  e  -- dont turn this on; it doesn't work anymore!
#define THINK_RETURN (1)
*/
/* to allow chars > 127 in the text... */
#define CHAR8_OK (1)

/* eEdit documentation...

	Limitations
		text size is limited by...
			memory space
			  = ~200 + text + (#lines + #style-runs + MIN_LINES + MIN_RUNS) * 4
			32K lines max
			32K style runs max
		  eEdit's been tested with files several hundred K in size
		  there is no pixel height limit (which constrains many editors to ~3000 lines)
		exactly two styles per text

	eTeKey()
		EXTENDED KEYBOARD:
			F1 - F4 - Undo, Cut, Copy, Paste
			F5 - F15 - function keys (not implemented)
			KeyDel - delete forward, erase character after cursor position
			[these next four do not move the insert point, just scroll]
			KeyHome - scrolls text to the top of the screen
			KeyEnd - scrolls text to the bottom of the screen
			KeyPageUp - scrolls text up a page
			KeyPageDown - scrolls text down a page

		ALL KEYBOARDS
			LEFT_ARROW - move cursor to the left one character
			RIGHT_ARROW - move cursor to the right one character
			UP_ARROW - move cursor up one line
			DOWN_ARROW - move cursor down one line
			DELETE - erase character before cursor position
			RETURN/ENTER - insert new line and move to left margin (insert a RETURN)
			TAB - insert TAB character; displayed as N spaces
			0x20 - 0xFF - range of valid text characters

	  Arrow keys can be modified with shift, option, and command
		shift - extend selection per Apple standard (optionally ThinkC mode)
		option - left/right by word, may be used with shift key to extend by word
		option - up/down by page, may be used with shift key to extend by page
		option+shift up/down arrows make the start/end of the selection the active end
		command - left/right: start/end of line, up/down: start/end of text
					may be used with shift key to extend selection

	*** NOTE: 
	*** command-arrow keys will work ONLY IF the main event loop
	*** lets command keys not associated with menu items pass thru to eEdit

	eTeClick()
		Mouse clicks
			single - position insert point
			double - select word
			triple - select line
		  dragging after one of these extends selection
		    by character, word, line repectively; anchor point is original click,
		    active end is the other end of the selection
		  after drag, active end can be moved with shift clicks or shift arrow keys
		  anchor and active ends of selection can be swapped
		    with option+shift+click, or option+shift+up/down arrow keys
		    
	scrolling: holding down command, shift, option, and control keys
	            speeds up scrolling when mousing in the arrows of the scroll bar
	            speed is 2^^count lines per iteration,
	             where count is a count of the pressed keys
*/


#define OPTION_SPACE	'Ê'				/* typed as OPTION <SPACE> - used for tabs */
#define EMPTY_PTR		((Ptr )2L)		/* valid but nonsense ptr used for Munger */
#define PTL(a)		   *((long *)&(a))	/* coerce Point or ChPos type to long */

#define		Abs(x)			((x) < 0 ? -(x) : (x))
#define		Max(x, y)		((x) > (y) ? (x) : (y))
#define		Min(x, y)		((x) < (y) ? (x) : (y))

#define CARET_ON FALSE
#define CARET_OFF TRUE

EventRecord			gLastMouseUp;
EventRecord			gLastMouseDown;
long				gMaxSleep;

static short 		gClicks;
static RgnHandle 	gUtilRgn;
static eRec		  **gLastViewHit;
static ChPos		zeroPos = { 0, 0 };
/*
#define	MAXINT		32767
static Rect			gMobyRect = { -MAXINT, -MAXINT, MAXINT, MAXINT };
*/
TextStyle dfltStylNormal =
{ monaco, normal,          0, 9, { 65536, 65536, 65536 } /* RGBColor Black */ };
TextStyle dfltStylHilite =
{ monaco, bold+condense,   0, 9, { 65536, 65536, 65536 } /* RGBColor Black */ };


static void eTeEdGuts( eRec **hE, Ptr textPtr, long numChars,
						 Boolean hasReturn, Boolean show, short style );
static void eTeFontChanged( eRec **hE );
static void eTeHiliteRange( eRec **hE, register ChPos start, register ChPos stop );
static void eTeAdjustScrollMax( eRec **hE );
static void eTeCalibrate( eRec **hE );
static short eTeOffsetToRun( eRec **hE, long anOffset );
static short eTeRunToStyle( eRec **hE, short r );
static ChPos eTePointHtoChPosH( eRec **hE, register Point aPt, register ChPos aPos );
static ChPos eTePointToChPos( eRec **hE, register Point aPt );
static Point eTeChPosToPoint( eRec **hE, register ChPos aPos );
static void eTeDoHscroll( eRec **hE, short whichPart );
static void eTeDoVscroll( eRec **hE, short whichPart );
static Boolean eTeAutoScroll( eRec **hE, Point mouseLoc );
static pascal void hSBarActionProc( ControlHandle macControl, short whichPart );
static pascal void vSBarActionProc( ControlHandle macControl, short whichPart );
/* obsolete...
void 	eTeSetFontNumber( eRec **hE, short aFontNumber );
void 	eTeSetFontName( eRec **hE, Str255 aFontName );
void 	eTeSetFontSize( eRec **hE, short aFontSize );
*/
static short eTeUpdateLineStarts( eRec **hE, short firstLine );
static void eTeKeyIns( eRec **hE, char c, short style );

/* the scrap  13Aug92  e  */
static Handle eTeScrap;
static long   eTeScrapLen;
static long   eTeScrapCnt;

/* undo       13Aug92  e  */

typedef struct undoStuff
{	unsigned char *undoTitle;			/* string for Edit menu */
	void  (*undoProc)( eRec  **hE );	/* proc to do undo */
	Handle	undoText;					/* text that was wiped by last operation */
	long	undoTxLen;					/* size of undoText */
	long	undoInLen;					/* number of chars inserted since last delete */
	eRec  **undoTeRec;					/* eRec       of last operation */
	long	undoStart;					/* selStart   before last operation */
	long	undoEnd;					/* selEnd     before last operation */
	Boolean undoDirty;					/* dirty      before last operation */
	Boolean undoKeyAccum;				/* can accumulate keystrokes */
	Boolean undoDeleting;				/* some version of delete was last operation */
	Boolean undoCarStaP;				/* TRUE if CaretChPos == selStart */
} undoStuff;

static undoStuff eTeUndoStuff;

unsigned char *utUndo   = "\pUndo";
unsigned char *utCopy   = "\pUndo Copy";
unsigned char *utCut    = "\pUndo Cut";
unsigned char *utPaste  = "\pUndo Paste";
unsigned char *utClear  = "\pUndo Clear";
unsigned char *utTyping = "\pUndo Typing";
unsigned char *utInsert = "\pUndo Insert";
unsigned char *utKill   = "\pUndo Kill";

unsigned char *rtCopy   = "\pRedo Copy";
unsigned char *rtCut    = "\pRedo Cut";
unsigned char *rtPaste  = "\pRedo Paste";
unsigned char *rtClear  = "\pRedo Clear";
unsigned char *rtTyping = "\pRedo Typing";
unsigned char *rtInsert = "\pRedo Insert";
unsigned char *rtKill   = "\pRedo Kill";

/* DeTabifyHandle takes a text handle,
	and a size (which it assumes is less than or equal to the HandleSize)
	 removes all characters in the range 0..0x1f except RETURN (0x0d)
	 replacing TAB characters (0x09) with spaces according to tabstop
	 it makes two passes over the text,
	  and only one resize call to the memory manager
	 it returns the number of lines in count (passed by reference)
	 and an OsErr result  ( 0 == noErr )
*/
/* static */
short DeTabifyHandle( Handle h, long *size, long *count, short tabstop )
{ long old_size = *size;
  long new_size = *size;
  long line_count = 0;
  register unsigned char *start = *(unsigned char **)h;
  register unsigned char *end = &start[old_size-1];
  register unsigned char *p = start;
  register unsigned char *q = start;
  register unsigned char c;
  short error;
  char lastch = *end;
  
  *end++ = '\0';
  while ( p < end )
  { c = *p++;
    if ( c < ' ' )
    { if ( c == '\r' )
      { line_count++;
        *q++ = c;
        start = q;
      }
      else if ( c == '\t' )
      { c = tabstop - ( ( q - start ) % tabstop);
        *q++ = c;
        c -= 1;
        start -= c;
        new_size += c;
      }
      else if ( p == end )
      {  *q++ = lastch; /* the last char may be a control char */
      }
      else
      { old_size -= 1;
        new_size -= 1;
      }
    }
    else
      *q++ = c;
  }
  *count = line_count;
  *size = new_size;
  SetHandleSize( h, new_size );
  asm { move.w D0, error } 		/* error = MemError(); */
  if ( error == noErr )
  { end = *(unsigned char **)h;
    p = &end[new_size];
    *--p = lastch;
    start = &end[old_size-1];
    while ( p > end )
    { c = *--start;
      if ( c < ' ' && c != '\r' )
        while ( c-- ) *--p = ' ';
      else
        *--p = c;
    }
  }
  else
  { /* cleanup the buffer? */
  }
  return error;
}

static void undoBugNi( eRec **hE )
{	SysBeep(3); SysBeep(10); SysBeep(3);
}

void eTeInit( void )
{
	gUtilRgn = NewRgn();
	gClicks = 0;
	/* 13Aug92  e  */
	eTeScrap = NewHandle(0);
	eTeScrapLen = 0;
	eTeScrapCnt = (InfoScrap())->scrapCount - 1;
	eTeGetScrap();
	/* */
	eTeUndoStuff.undoProc = undoBugNi;
	eTeUndoStuff.undoTitle = utUndo;
	eTeUndoStuff.undoText = NewHandle(0);
	eTeUndoStuff.undoTeRec = NULL;
}

/* replacement can be installed via eTeSetWordBreak() */

static long WordLimits( char *text, long offset, Boolean reverse )
{
	register char *ptr, c;

	ptr = text + offset;
	if ( reverse ) {
		/* Scan backwards until we find beginning of word */
		while ( ptr > text ) {
			c = *( ptr - 1 );
			if ( ( c >= 'a' && c <= 'z' ) ||
				 ( c >= 'A' && c <= 'Z' ) || 
				 ( c >= '0' && c <= '9' ) || 
				 ( c == '_' ) )
				--ptr;
			else
				break;
		}
	}
	else {
		/* Scan forwards until we find end of word */
		while ( 1 ) {
			c = *ptr;
			if ( ( c >= 'a' && c <= 'z' ) ||
				 ( c >= 'A' && c <= 'Z' ) || 
				 ( c >= '0' && c <= '9' ) || 
				 ( c == '_' ) )
				++ptr;
			else
				break;
		}
	}
	return( ptr - text );
}

static short eTeNewRuns( eRec **hE, long len )
{	register long *runPtr;
	register eRec *pE;
	short error;

	if ( (**hE).hRuns )
		DisposHandle( (Handle )(**hE).hRuns );
	(**hE).hRuns = (long **)NewHandle( MIN_RUNS * sizeof( long ) );
  	asm { move.w D0, error } 		/* error = MemError(); */
  	if( error == noErr )
  	{	pE = *hE;
		runPtr = *((*pE).hRuns);
		*runPtr++ = 0L;
		*runPtr = len;
		(*pE).runsAllocated = MIN_RUNS;
		(*pE).qRuns = 1;
		(*pE).fustStyle = 0;
	}
	return error;
}

long eTeTextLength( eRec **hE )
{
	return (*(**hE).hRuns)[(**hE).qRuns];
}

eRec **eTeNew( WindowPtr macPort, Rect viewRect, short tabStops, short wrap,
				 short autoInd, ControlHandle aHSizing, ControlHandle aVSizing )
{	
	register eRec *pE;
	LineRec *linePtr;
	eRec   **hE = (eRec **)NewHandle( sizeof( eRec ) );

	if( hE != 0 )
	{	HLock( (Handle )hE );
		pE = *hE;
		(*pE).active = FALSE;
		(*pE).macPort = macPort;
		(*pE).viewRect = viewRect;
		(*pE).width = viewRect.right - viewRect.left;
		(*pE).height = viewRect.bottom - viewRect.top;
		(*pE).hOrigin = -viewRect.left;
		(*pE).vOrigin = -viewRect.top;
		(*pE).position.h = (*pE).position.v = 0;
		(*pE).hScale = (*pE).vScale = 1;
		(*pE).bounds.v = 1;
		(*pE).bounds.h = 1;     /* arbitrary size for now */
		(*pE).leftMargin = 1;	/* room for cursor will blink at the far left */
		(*pE).topMargin = 0;
		(*pE).selStart.h = (*pE).selStart.v = 0;
		(*pE).selEnd = (*pE).selStart;
		(*pE).selActive = FALSE;
		(*pE).caretChPos.h = (*pE).caretChPos.v = 0;
		(*pE).writeChPos.h = (*pE).writeChPos.v = 0;
		(*pE).caretState = CARET_OFF;
		(*pE).maxRight = 0;
		(*pE).eTeWordBreak = WordLimits;
		SetPort( macPort );
		(*pE).tabStops = tabStops;
		(*pE).spaceWidth = 1;
		(*pE).wrap = wrap;							/*  5Jul92  e  */
		(*pE).autoInd = autoInd;
		(*pE).hText = NewHandle( 1L );
		**((*pE).hText) = '\0';
		(*pE).hLines = (LineRec **)NewHandle( MIN_LINES * sizeof( LineRec ) );
		linePtr = *((*pE).hLines);
		*linePtr++ = 0L;
		*linePtr = 0L;
		(*pE).linesAllocated = MIN_LINES;
		/* 2May92  e  */
		(*pE).hRuns = NULL;
		eTeNewRuns( hE, 0L);
		(*pE).qRuns = 0;
		(*pE).hPrint = NULL;						/* 28Sep92  e  */
		/* scrollPane */
		(*pE).hStep = (*pE).vStep = 1;
		(*pE).hOverlap = (*pE).vOverlap = 1;
		(*pE).hContext = (*pE).vContext = CONTEXT_LINES;
		if ( ( (*pE).hSBar = aHSizing ) != NULL )
		{	SetCRefCon( aHSizing, (long )hE );
			SetCtlAction( aHSizing, hSBarActionProc );
			/* SetThumbFunc( aHSizing, SBarThumbFunc ); */
		}
		if ( ( (*pE).vSBar = aVSizing ) != NULL )
		{	SetCRefCon( aVSizing, (long )hE );
			SetCtlAction( aVSizing, vSBarActionProc );
			/* SetThumbFunc( aVSizing, SBarThumbFunc ); */
		}
		HUnlock( (Handle )hE );
		eTeSetStyles( hE, &dfltStylNormal, &dfltStylHilite);
		eTeAdjustScrollMax( hE );
		eTeCalibrate( hE );
		(**hE).dirty = FALSE;  /* was (*pE). 14Jul92  e  */
		(**hE).active = TRUE;  /* was (*pE). 14Jul92  e  */
	}
	return hE;
}

void eTeDispose( eRec **hE )
{
	if ( (**hE).hText )
		DisposHandle( (**hE).hText );
	(**hE).hText = NULL;
	if ( (**hE).hLines )
		DisposHandle( (Handle )(**hE).hLines );
	(**hE).hLines = NULL;
	if ( (**hE).hRuns )
		DisposHandle( (Handle )(**hE).hRuns );
	(**hE).hRuns = NULL;
	if ( (**hE).hPrint )
		DisposHandle( (Handle)(**hE).hPrint );	/*  28Sep92  e  */
	DisposHandle( (Handle )hE );
}

static void eTePrepare( eRec **hE )
{
	Rect		tempRect;				/* ClipRect may move memory	*/

	SetPort( (**hE).macPort );
	tempRect = (**hE).viewRect;
	ClipRect(&tempRect);
	/* obsolete...
	TextFont( (**hE).fontNumber );
	TextSize( (**hE).fontSize );
	TextFace( 0 ); */
	/* Paranoid... */
	TextMode( srcOr );
}

static void eTePrepareStyle( eRec **hE, short style )
{
  if( (**hE).curStyle != style )
  { (**hE).curStyle = style;
    TextFont( (**hE).style[style].tsFont );
    TextFace( (**hE).style[style].tsFace );
    TextSize( (**hE).style[style].tsSize );
    /* RGBColor tsColor; */
  }
}

static void eTePrepareRun( eRec **hE, short run )
{
	eTePrepareStyle( hE, eTeRunToStyle( hE, run ) );
}

static void eTeRefresh( eRec **hE )
{
	SetPort( (**hE).macPort );
	InvalRect( &(**hE).viewRect );
}

static void eTeUpdateCaretRect( eRec **hE )
{
	Point	tempPt;
	register eRec	*pE;

	tempPt = eTeChPosToPoint( hE, (**hE).caretChPos );
	pE = *hE;
	(*pE).caretRect.top    = tempPt.v;
	(*pE).caretRect.right  = tempPt.h;
	(*pE).caretRect.left   = tempPt.h - 1;
	(*pE).caretRect.bottom = tempPt.v + (*pE).caretHeight;
}

void eTeSetWrap( eRec **hE, short wrap )
{
	/* 22Jul92  e  -- added limits */
	if( wrap < 1 ) wrap = 1;
	else if( wrap > 999 ) wrap = 999;
	/* (**hE).wrap = wrap; */
	/* 10Aug92  e   need to fix all ChPos for new wrap! */
	if( (**hE).wrap != wrap )
	{ long car = eTeChPosToOffset( hE, (**hE).caretChPos );
	  long sta = eTeChPosToOffset( hE, (**hE).selStart );
	  long end = eTeChPosToOffset( hE, (**hE).selEnd );
	  long wri = eTeChPosToOffset( hE, (**hE).writeChPos );
	  (**hE).wrap = wrap;
	  eTeUpdateLineStarts( hE, 0 );
	  (**hE).caretChPos = eTeOffsetToChPos( hE, car );
	  (**hE).selStart   = eTeOffsetToChPos( hE, sta );
	  (**hE).selEnd     = eTeOffsetToChPos( hE, end );
	  (**hE).writeChPos = eTeOffsetToChPos( hE, wri );
	  eTeUpdateCaretRect( hE );
	  eTeRefresh( hE );
	}
}

void eTeSetTabStop( eRec **hE, short aTabStop )
{
	/* Undo? */
	(**hE).tabStops = aTabStop;
	(**hE).tabWidth = aTabStop * (**hE).spaceWidth;
	eTeRefresh( hE );
}

static short eTeTabStop( eRec **hE, register short curPosition )
{
	register short tabWidth = (**hE).tabWidth;
	return (curPosition +
			 (tabWidth -
			   ((curPosition - (**hE).leftMargin + (**hE).hOrigin) % tabWidth)));
}

void eTeSetWordBreak( eRec **hE, ProcPtr aFunc )
{
	if ( aFunc == NULL )
		(**hE).eTeWordBreak = WordLimits;
	else
		(**hE).eTeWordBreak = aFunc;
}

/* 23Jul92  e  */

/* Inverts the text between the start and stop positions. */

static void eTeHiliteRange( eRec **hE, register ChPos start, register ChPos stop )
{
	Rect tmpRect;
	Point stopPt;
	register short vScale;
	
	if ( ! (**hE).active )
		return;

	topLeft( tmpRect ) = eTeChPosToPoint( hE, start );
	tmpRect.left -= 1;

	vScale = (**hE).vScale;

	/* Just need to hilite within same line */
	if ( start.v == stop.v ) {
		botRight( tmpRect ) = eTeChPosToPoint( hE, stop );
		tmpRect.right -= 1;
		tmpRect.bottom = tmpRect.top + vScale;
		if ( tmpRect.left != tmpRect.right ) {
			eTePrepare( hE );
			asm { bclr #hiliteBit, HiliteMode }
			InvertRect( &tmpRect );
		}
	}
	/* Hilite spans more than one line */
	else
	{	stopPt = eTeChPosToPoint( hE, stop );
		if( tmpRect.top < (**hE).viewRect.bottom && stopPt.v >= (**hE).viewRect.top )
		{	eTePrepare( hE );
			tmpRect.right = (**hE).viewRect.right;
			tmpRect.bottom = tmpRect.top + vScale;
			if( tmpRect.bottom > (**hE).viewRect.top )
			{	asm { bclr #hiliteBit, HiliteMode }
				InvertRect( &tmpRect );
				tmpRect.top += vScale;
			}
			else
				tmpRect.top = (**hE).viewRect.top;
			tmpRect.left = (**hE).viewRect.left;
			tmpRect.bottom = Min( stopPt.v, (**hE).viewRect.bottom );
			asm { bclr #hiliteBit, HiliteMode }
			InvertRect( &tmpRect );
			if( stopPt.v < (**hE).viewRect.bottom )
			{	tmpRect.bottom += vScale;
				tmpRect.top = stopPt.v;
				tmpRect.right = stopPt.h - 1;
				asm { bclr #hiliteBit, HiliteMode }
				InvertRect( &tmpRect );
			}
		}
	}
}

/* 23Jul92  e  */

static void eTeSetCaretState( eRec **hE, Boolean state )
{
	Rect	tmpRect;
	register eRec	*pE = *hE;

	if (   ! (*pE).selActive
		  && (*pE).active
		  && (*pE).caretState != state )
	{
		(*pE).caretState = state;
		tmpRect = (*pE).caretRect;
		eTePrepare( hE );
		PenMode(patXor);
		FrameRect( &tmpRect );
		PenNormal();
	}
}

void eTeActivate( eRec **hE )
{
	if( (**hE).active == FALSE )
	{	(**hE).active = TRUE;
		if( (**hE).selActive )
			eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
	}
}

void eTeDeactivate( eRec **hE )
{
	if( (**hE).active == TRUE )
	{	if( (**hE).selActive )
			eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
		else
			eTeSetCaretState( hE, CARET_OFF );
		(**hE).active = FALSE;
	}
}

void eTeIdle( eRec **hE )
{
	static long lastCaretToggle = 0L;
	register long	now;
	Rect	tmpRect;

	gMaxSleep = GetCaretTime();	/* user may change it using desk accessory */
	now = TickCount();
	if ( now - lastCaretToggle >= gMaxSleep ) {
		lastCaretToggle = now;
		eTeSetCaretState( hE, (**hE).caretState == CARET_OFF ? CARET_ON : CARET_OFF );
	}
	else {
		gMaxSleep -= ( now - lastCaretToggle );
	}
}

static void eTeResize( eRec **hE, register Rect *delta )
{
	register eRec *pE = *hE;

	(*pE).width      += delta->right - delta->left;
	(*pE).height     += delta->bottom - delta->top;
	(*pE).leftMargin += delta->left;
	(*pE).topMargin  += delta->top;

	(*pE).hOrigin -= delta->left;
	(*pE).vOrigin -= delta->top;

	eTeUpdateCaretRect( hE );
	eTeAdjustScrollMax( hE );
	eTeCalibrate( hE );
}

void eTeNewView( eRec **hE, Rect *viewRect )
{
	register eRec *pE = *hE;
	Rect	delta;
	
	delta = *viewRect;
	delta.top -= (*pE).viewRect.top;
	delta.left -= (*pE).viewRect.left;
	delta.bottom -= (*pE).viewRect.bottom;
	delta.right -= (*pE).viewRect.right;
	(*pE).viewRect   = *viewRect;
	
	eTeResize( hE, &delta );
}

/* eTeSetStyles also causes the display to be redrawn */

void eTeSetStyles( eRec **hE, TextStyle *ts0, TextStyle *ts1 )
{
	short		height[2];
	FontInfo	fontInfo[2];
	ChPos		tmpPt;
	short		bigger;

	(**hE).style[0] = *ts0;
	(**hE).style[1] = *ts1;
	(**hE).curStyle = NOSTYLE;

	/* Turn off cursor */
	eTePrepare( hE );
	eTeSetCaretState( hE, CARET_OFF );

	eTePrepareStyle( hE, 1 );
	GetFontInfo( &fontInfo[1] );
	height[1] = CharWidth( OPTION_SPACE );

	eTePrepareStyle( hE, 0 );
	GetFontInfo( &fontInfo[0] );
	height[0] = CharWidth( OPTION_SPACE );
	
	(**hE).spaceWidth = Max ( height[0], height[1] );
	/* calculate new tabstop based on new font/size */
	(**hE).tabWidth = (**hE).tabStops * (**hE).spaceWidth;

	height[0] = fontInfo[0].ascent + fontInfo[0].descent;
	height[1] = fontInfo[1].ascent + fontInfo[1].descent;
	
	bigger = height[1] > height[0] ? 1 : 0;
	(**hE).fontAscent = fontInfo[bigger].ascent;
	(**hE).caretHeight = height[bigger];

	/* Update caret values for new font */
	eTeUpdateCaretRect( hE );
	(**hE).maxRight = (**hE).caretRect.right;

	/*
	 * Set the scale values for our scrollbars. Vertical scrolling is done by 
	 * lines; horizontal by widest character. First, move to top of panorama (but
	 * don't redraw), then change the scale values. Finally, move back to original
	 * position and redraw screen.
	 */
	tmpPt = (**hE).position;
	eTeScroll( hE, -(**hE).position.h, -(**hE).position.v, FALSE );
	(**hE).hScale = Max( fontInfo[0].widMax, fontInfo[1].widMax );
	(**hE).vScale = (**hE).caretHeight + fontInfo[bigger].leading;
	eTeAdjustScrollMax( hE );
	eTeScrollTo( hE, tmpPt, FALSE );
	eTeRefresh( hE );
}

#ifdef include_obsolete_code

void eTeSetFontNumber( eRec **hE, short aFontNumber )
{
	Str255		fontName;

	GetFontName( aFontNumber, fontName );
	if ( fontName[0] == 0 )
		aFontNumber = 0;	/* If font does not exist, then use the system font */

	/* Undo? */
	(**hE).fontNumber = aFontNumber;
	eTeFontChanged( hE );
}

void eTeSetFontName( eRec **hE, Str255 aFontName )
{
	short	fontNumber;

	GetFNum( aFontName, &fontNumber );
	eTeSetFontNumber( hE, ( fontNumber > 0 ) ? fontNumber : 0 );
}

void eTeSetFontSize( eRec **hE, short aFontSize )
{
	/* Undo? */
	(**hE).fontSize = aFontSize;
	eTeFontChanged( hE );
}

#endif

/* static --  25Sep92  e  for os_mac_Print.c */
void eTeDrawLine( eRec **hE, ChPos beginPos, Point location )
{
	register char	c, *charPtr, *firstChar;
	register short	count;
	long 	offset, instyle, inline, eol;
	short 	run, style;
	Point	pt;

	HLock( (**hE).hText );
	MoveTo( location.h, location.v + (**hE).fontAscent );
	
	offset = *(*(**hE).hLines + beginPos.v) + beginPos.h;
	charPtr = *(**hE).hText + offset;
	eol    = *(*(**hE).hLines + beginPos.v + 1);
	inline = eol - offset;
	run = eTeOffsetToRun( hE, offset );
	while( inline )
	{	eTePrepareRun( hE, run );
		instyle = (*(**hE).hRuns)[++run] - offset;
		if( instyle > inline )
			instyle = inline;
		inline -= instyle;
		offset += instyle;
		firstChar = charPtr;
		count = 0;
		while ( instyle && ( c = *charPtr++ ) != RETURN )	/* 11Aug92  e   was: instyle-- */
		{	if ( c == TAB ) 
			{	if ( count > 0 ) DrawText( firstChar, 0, count );
				GetPen( &pt );
				pt.h = eTeTabStop( hE, pt.h );
				MoveTo( pt.h, pt.v );
				firstChar = charPtr;
				count = 0;
			}
			else ++count;
			instyle -= 1;									/* 11Aug92  e  */
		}
		if ( count  > 0 ) DrawText( firstChar, 0, count );
	}
	if( c != RETURN && *charPtr != '\0' ) DrawChar( 0xd7 );
	/* if( instyle == 0 && *charPtr != '\0' ) DrawChar( 0xd7 ); /* NG!? 11Aug92  e  */
	HUnlock( (**hE).hText );
}

void eTeDraw( eRec **hE, Rect *area )
{
	eRec *pE;
	short	vFirst, vLast;
	ChPos	startPos;
	Point	location;
	Rect	tRect;
	Boolean doCaret;	/* 23Jul92  e */
	
	HLock( (Handle )hE );
	pE = *hE;
	
	if ( SectRect( area, &(*pE).viewRect, &tRect ) )
	{
		eTePrepare( hE );
		
		doCaret = ( ( ! (*pE).selActive ) && ( (*pE).caretState == CARET_ON ) );
		if( doCaret ) eTeSetCaretState( hE, CARET_OFF );

		vFirst = ( tRect.top - (*pE).topMargin + (*pE).vOrigin ) / (*pE).vScale;
		vLast = ( tRect.bottom + (*pE).vScale - (*pE).topMargin + (*pE).vOrigin - 1 )
			    / (*pE).vScale;

		if ( vFirst < 0 )
			vFirst = 0;
		if ( vLast >= (*pE).bounds.v ) 
			vLast = (*pE).bounds.v - 1;

		EraseRect( &tRect );

		if ( vFirst < (*pE).bounds.v && vLast >= 0)

		{	location.h = (*pE).leftMargin - (*pE).hOrigin;
			location.v = vFirst * (*pE).vScale + (*pE).topMargin - (*pE).vOrigin;
			startPos.h = 0;

			for ( startPos.v = vFirst;
				  startPos.v <= vLast;
				  location.v += (*pE).vScale, ++startPos.v )
			{	eTeDrawLine( hE, startPos, location );
			}
			if ( (*pE).selActive )
			 	eTeHiliteRange( hE, (*pE).selStart, (*pE).selEnd );	/* may have munged it */
			else if( doCaret )
				eTeSetCaretState( hE, CARET_ON );
		}
	}
	HUnlock( (Handle )hE );
}

void eTeUpdate( eRec **hE )
{
	Rect tempRect;
	
	eTePrepare( hE );
	tempRect = (**((**hE).macPort)->visRgn).rgnBBox;
	eTeDraw( hE, &tempRect );
}
		
#ifndef THINK_ARROWS

static void eTeExtSelGuts( eRec **hE, ChPos chPos )
{
	if( PTL( chPos ) < PTL( (**hE).caretChPos ) )
	{	eTeHiliteRange( hE, chPos, (**hE).caretChPos );
		if( PTL( (**hE).caretChPos ) == PTL( (**hE).selStart ) )
			(**hE).selStart = chPos;
		else if( PTL( chPos ) >= PTL( (**hE).selStart ) )
			(**hE).selEnd = chPos;
		else
		{	(**hE).selEnd = (**hE).selStart;
			(**hE).selStart = chPos;
		}
	}
	else if( PTL( chPos ) > PTL( (**hE).caretChPos ) )
	{	eTeHiliteRange( hE, (**hE).caretChPos, chPos );
		if( PTL( (**hE).caretChPos ) == PTL( (**hE).selEnd ) )
			(**hE).selEnd = chPos;
		else if( PTL( chPos ) <= PTL( (**hE).selEnd ) )
			(**hE).selStart = chPos;
		else
		{	(**hE).selStart = (**hE).selEnd;
			(**hE).selEnd = chPos;
		}
	}
}

#endif

static void eTeEnsureChPos( eRec **hE, ChPos chPos )
{
	if( PTL( chPos ) != PTL( (**hE).caretChPos ) )
	{	(**hE).caretChPos = chPos;
		eTeUpdateCaretRect( hE );
		(**hE).maxRight = (**hE).caretRect.right;
	}
}

void eTeKey( eRec **hE, char theChar, Byte keyCode, short modifiers, short style )
{
	char		*charPtr;
	LineRec		*linePtr;
	long		offset;
	short			eoln;
	ChPos		chPos;
	char		chars[ 255 ];
	Boolean		shifted, optioned, commanded;
	Rect 		invalRect;
	long		new;
	Point 		aPt;

	ObscureCursor();
	eTeSetCaretState( hE, CARET_OFF ); /* move up here 22Jul92  e  */

	shifted = ( modifiers & shiftKey ) ? 1 : 0;
	optioned = ( modifiers & optionKey ) ? 1 : 0;
	commanded = ( modifiers & cmdKey ) ? 1 : 0;

	switch ( keyCode ) {
		case KeyF1:
			/* Undo */
			break;
		case KeyF2:
			eTeCut( hE );
			break;
		case KeyF3:
			eTeCopy( hE );
			break;
		case KeyF4:
			/* eTePaste( hE, style );			/* correct        3May92  e */
			eTePaste( hE, style ^ optioned );	/* for debugging  3May92  e */
			break;
		case KeyF5:
		case KeyF6:
		case KeyF7:
		case KeyF8:
		case KeyF9:
		case KeyF10:
		case KeyF11:
		case KeyF12:
		case KeyF13:
		case KeyF14:
		case KeyF15:
			/* DoFunctionKey( theChar, keyCode, modifiers ); */
			break;

		case KeyHome:											/*** HOME ***/
			eTeScrollTo( hE, zeroPos, TRUE );
			break;

		case KeyEnd:											/*** END ***/
			chPos.v = (**hE).bounds.v - (**hE).vSpan;
			if ( chPos.v < 0 )
				chPos.v = 0;
			chPos.h = (**hE).position.h;
			eTeScrollTo( hE, chPos, TRUE );
			break;

		case KeyPageUp:											/*** PAGE UP ***/
			eTeDoVscroll( hE, inPageUp );
			eTeAdjustScrollMax( hE );
			break;

		case KeyPageDown:										/*** PAGE DOWN ***/
			eTeDoVscroll( hE, inPageDown );
			eTeAdjustScrollMax( hE );
			break;

		case KeyDel:											/*** DEL FWD ***/
			offset = eTeChPosToOffset( hE, (**hE).caretChPos );
			if ( (**hE).selActive )
				eTeDelete( hE );
			else if ( optioned ) { /* delete to end of line */
				/* 5Jul92  e */
				chPos = (**hE).caretChPos;
				linePtr = *(**hE).hLines;
			  labelTryAgainK:
				chPos.h = linePtr[ chPos.v + 1 ] - linePtr[ chPos.v ];
				if ( chPos.v < (**hE).bounds.v - 1 )
				{ --chPos.h;
				  new = eTeChPosToOffset( hE, chPos );
				  if( *(*(**hE).hText + new) != RETURN )
				  { chPos.v++;
					goto labelTryAgainK;
				  }
				  if( new == offset )
				  	chPos.h++;	/* delete the RETURN if its all there is */
				}
				eTeKillTo( hE, chPos );
			}
			else if ( *(*(**hE).hText + offset) != '\0' ) {
				/* 14Aug92  e   for undo...
				eTeEdGuts( hE, EMPTY_PTR, 0L, *(*(**hE).hText + offset) == RETURN, TRUE, style );
				*/
				eTeKillTo( hE, eTeOffsetToChPos( hE, offset + 1 ) ); /* optimize for Undo Typing */
			}
			break;

		default:
			MoveHHi( (Handle )(**hE).hLines );
			HLock( (Handle )(**hE).hLines );
			/* eTeSetCaretState( hE, CARET_OFF );  --  move above  22Jul92  e  */
			linePtr = *(**hE).hLines;
			switch( theChar ) {
				case LEFT_ARROW:								/*** LEFT ARROW ***/
					if ( (**hE).selActive && ! shifted )
					{	/* if no shift key and there is a selection, remove it */
						eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
						chPos = (**hE).selEnd = (**hE).selStart;
					}
#ifndef THINK_ARROWS
					else if( (**hE).selActive && optioned && commanded )
					{	/* move chPos to start of selection if not there */
						chPos = (**hE).selStart;
						eTeEnsureChPos( hE, chPos );
					}
#endif
					else
					{	if ( shifted )
					  	{	if ( ! (**hE).selActive )
								(**hE).selStart = (**hE).selEnd = (**hE).caretChPos;
#ifdef THINK_ARROWS
							(**hE).caretChPos = (**hE).selStart;
#endif
						}
						chPos = (**hE).caretChPos;
						if ( commanded )
						{	chPos.h = 0;	/* move to beginning of line */
							labelTryAgainL:	/* added the rest 5Jul92  e  */
							if ( chPos.v > 0 )
							{	offset = eTeChPosToOffset( hE, chPos );
								if( *(*(**hE).hText + offset - 1) != RETURN )
								{	chPos.v--;
									goto labelTryAgainL;
								}
							}
						}
						else if ( optioned )
						{ 					/* move to start/end of previous word */
					  		offset = eTeChPosToOffset( hE, chPos );
							new = WordLimits( *(**hE).hText, offset, TRUE );
							if ( new == offset && offset != 0L )
								--new;
							chPos = eTeOffsetToChPos( hE, new );
						}
					  	else 				/* move to previous character position */
							chPos = eTeOffsetToChPos( hE, eTeChPosToOffset( hE, chPos ) - 1 );
						if ( shifted )
						{
#ifdef THINK_ARROWS
							eTeHiliteRange( hE, chPos, (**hE).selStart );
							(**hE).selStart = chPos;
#else
							eTeExtSelGuts( hE, chPos );
#endif
							/* 30Apr92  e  -- needed if newly active AND scrolling... */
							(**hE).selActive = ( PTL( (**hE).selStart ) != PTL( (**hE).selEnd ) );
						}
					}
					(**hE).caretChPos = chPos;
					eTeUpdateCaretRect( hE );
					(**hE).maxRight = (**hE).caretRect.right;
					eTeShowCaret( hE );
					break;

				case RIGHT_ARROW:								/*** RIGHT ARROW ***/
					if ( (**hE).selActive && ! shifted )
					{	/* if no shift key and there is a selection, remove it */
						eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
						chPos = (**hE).selStart = (**hE).selEnd;
					}
#ifndef THINK_ARROWS
					else if( (**hE).selActive && optioned && commanded )
					{	/* move chPos to end of selection if not there */
						chPos = (**hE).selEnd;
						eTeEnsureChPos( hE, chPos );
					}
#endif
					else
					{	if ( shifted )
					  	{	if ( ! (**hE).selActive )
								(**hE).selStart = (**hE).selEnd = (**hE).caretChPos;
#ifdef THINK_ARROWS
							(**hE).caretChPos = (**hE).selEnd;
#endif
						}
						chPos = (**hE).caretChPos;
						if ( commanded )
						{					/* move to end of line */
							labelTryAgainR:
							chPos.h = linePtr[ chPos.v + 1 ] - linePtr[ chPos.v ];
							if ( chPos.v < (**hE).bounds.v - 1 )
							{	/* 5Jul92  e  was: --chPos.h; */
								--chPos.h;
								offset = eTeChPosToOffset( hE, chPos );
								if( *(*(**hE).hText + offset) != RETURN )
								{	chPos.v++;
									goto labelTryAgainR;
								}
							}
						}
						else if ( optioned )
						{ 					/* move to start/end of next word */
					  		offset = eTeChPosToOffset( hE, chPos );
							new = WordLimits( *(**hE).hText, offset, FALSE );
							if ( new == offset && *(*(**hE).hText + offset) != '\0' )
								++new;
							chPos = eTeOffsetToChPos( hE, new );
						}
					  	else 				/* move to previous character position */
							chPos = eTeOffsetToChPos( hE, eTeChPosToOffset( hE, chPos ) + 1 );
						if ( shifted )
						{
#ifdef THINK_ARROWS
							eTeHiliteRange( hE, (**hE).selEnd, chPos );
							(**hE).selEnd = chPos;
#else
							eTeExtSelGuts( hE, chPos );
#endif
							/* 30Apr92  e  -- needed if newly active AND scrolling... */
							(**hE).selActive = ( PTL( (**hE).selStart ) != PTL( (**hE).selEnd ) );
						}
					}
					(**hE).caretChPos = chPos;
					eTeUpdateCaretRect( hE );
					(**hE).maxRight = (**hE).caretRect.right;
					eTeShowCaret( hE );
					break;

				case UP_ARROW:								/*** UP ARROW ***/
					if ( (**hE).selActive && ! shifted )
					{	/* if no shift key and there is a selection, remove it */
						eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
						chPos = (**hE).selEnd = (**hE).selStart;
					}
#ifndef THINK_ARROWS
					else if( (**hE).selActive && optioned && commanded )
					{	/* move chPos to start of selection if not there */
						chPos = (**hE).selStart;
						eTeEnsureChPos( hE, chPos );
					}
#endif
					else
					{	if ( shifted )
					  	{	if ( ! (**hE).selActive )
								(**hE).selStart = (**hE).selEnd = (**hE).caretChPos;
#ifdef THINK_ARROWS
							else
								eTeEnsureChPos( hE, (**hE).selStart );
#endif
						}
						chPos = (**hE).caretChPos;
						if ( commanded )
							chPos.h = chPos.v = 0;	/* move to beginning of text */
						else if( optioned )
						{	aPt.h = (**hE).maxRight;
							chPos.v -= (**hE).vSpan - (**hE).vOverlap;	/* move up one page */
							if( chPos.v < 0 ) chPos.v = 0;
							chPos = eTePointHtoChPosH( hE, aPt, chPos );
						}
					  	else if ( chPos.v > 0 )
						{	/* NG...   [caretRect.top can be bogus!]
							aPt.h = (**hE).maxRight;
							aPt.v = (**hE).caretRect.top - (**hE).vScale;
							chPos = eTePointToChPos( hE, aPt );
							*/
							aPt.h = (**hE).maxRight;
							chPos.v -= 1;	/* move up one line */
							chPos = eTePointHtoChPosH( hE, aPt, chPos );
						}
						if ( shifted )
						{
#ifdef THINK_ARROWS
							eTeHiliteRange( hE, chPos, (**hE).selStart );
							(**hE).selStart = chPos;
#else
							eTeExtSelGuts( hE, chPos );
#endif
							/* 30Apr92  e  -- needed if newly active AND scrolling... */
							(**hE).selActive = ( PTL( (**hE).selStart ) != PTL( (**hE).selEnd ) );
						}
					}
					(**hE).caretChPos = chPos;
					eTeUpdateCaretRect( hE );
					eTeShowCaret( hE );
					break;

				case DOWN_ARROW:							/*** DOWN ARROW ***/
					if ( (**hE).selActive && ! shifted )
					{	/* if no shift key and there is a selection, remove it */
						eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
						chPos = (**hE).selStart = (**hE).selEnd;
					}
#ifndef THINK_ARROWS
					else if( (**hE).selActive && optioned && commanded )
					{	/* move chPos to end of selection if not there */
						chPos = (**hE).selEnd;
						eTeEnsureChPos( hE, chPos );
					}
#endif
					else
					{	if ( shifted )
					  	{	if ( ! (**hE).selActive )
								(**hE).selStart = (**hE).selEnd = (**hE).caretChPos;
#ifdef THINK_ARROWS
							else
								eTeEnsureChPos( hE, (**hE).selEnd );
#endif
						}
						chPos = (**hE).caretChPos;
						if ( commanded )
						{					/* move to end of text */
							chPos.v = (**hE).bounds.v - 1;
							chPos.h = linePtr[ chPos.v + 1 ] - linePtr[ chPos.v ];
						}
						else if( optioned )
						{	aPt.h = (**hE).maxRight;
							if( chPos.v > (**hE).bounds.v - (**hE).vSpan + (**hE).vOverlap )
								chPos.v = (**hE).bounds.v - 1;
							else
								chPos.v += (**hE).vSpan - (**hE).vOverlap;	/* move down one page */
							chPos = eTePointHtoChPosH( hE, aPt, chPos );
						}
					  	else if ( chPos.v < (**hE).bounds.v - 1 )
						{	/* NG...   [caretRect.top can be bogus!]
							aPt.h = (**hE).maxRight;
							aPt.v = (**hE).caretRect.top + (**hE).vScale;
							chPos = eTePointToChPos( hE, aPt );
							*/
							aPt.h = (**hE).maxRight;
							chPos.v += 1;	/* move down one line */
							chPos = eTePointHtoChPosH( hE, aPt, chPos );
						}
						if ( shifted )
						{
#ifdef THINK_ARROWS
							eTeHiliteRange( hE, (**hE).selEnd, chPos );
							(**hE).selEnd = chPos;
#else
							eTeExtSelGuts( hE, chPos );
#endif
							/* 30Apr92  e  -- needed if newly active AND scrolling... */
							(**hE).selActive = ( PTL( (**hE).selStart ) != PTL( (**hE).selEnd ) );
						}
					}
					(**hE).caretChPos = chPos;
					eTeUpdateCaretRect( hE );
					eTeShowCaret( hE );
					break;

				case DELETE:								/*** DELETE ***/
					eTeDelete( hE );
					break;

				default:									/*** TEXT ***/
					/* Undo? */
					if ( theChar == ENTER || theChar == RETURN ) {
						chars[ 0 ] = RETURN;
						offset = 1;
#ifdef THINK_RETURN
						if ( (**hE).caretChPos.v > 0 ) {
							charPtr = *(**hE).hText + linePtr[ (**hE).caretChPos.v - 1 ];
							while ( *charPtr == TAB || *charPtr == SPACE ) {
								chars[ offset++ ] = *charPtr++;
							}
						}
#endif
					}
#ifdef CHAR8_OK
					else if ( theChar == TAB || (unsigned char)theChar >= SPACE )
#else
					else if ( theChar == TAB ||                theChar >= SPACE )
#endif
					{
						chars[0] = theChar;
						offset = 1;
					}
					else {
						break;
					}
					/* Insert character to right of cursor position */
					/*  14Aug92  e  for undo...
					eTeEdGuts( hE, chars, (long )offset,
								 ( chars[0] == RETURN ? TRUE : FALSE ), TRUE, style );
					*/
					eTeKeyIns( hE, chars[0], style );
					/* (**hE).maxRight = (**hE).caretRect.right;
					   -- done in eTeEdGuts --                   02Oct92  e  */
					break;
			} /* END SWITCH ASCII CHARACTER */

			/* Update selection flag and turn cursor back on */
			(**hE).selActive = ( PTL( (**hE).selStart ) != PTL( (**hE).selEnd ) );
			eTeSetCaretState( hE, CARET_ON );
			HUnlock( (Handle )(**hE).hLines );
			break;
	}
}

void eTeClick( eRec **hE, Point hitPt, short modifierKeys, long when )
{
	ChPos	pos;
	Point	newPt;
	Boolean didScroll = TRUE;
	long	low, high, offset, start, end, anchorStart, anchorEnd;
	eRec *pE;
	
	HLock( (Handle )hE );
	pE = *hE;

	if ( ( hE == gLastViewHit )  &&
		 ( ( when - gLastMouseUp.when ) < GetDblTime() )  &&
		 ( Abs ( gLastMouseDown.where.h - hitPt.h ) < 3 ) &&
		 ( Abs ( gLastMouseDown.where.v - hitPt.v ) < 3 ) )
		gClicks++;
	else
		gClicks = 1;
	gLastViewHit = hE;

	eTePrepare( hE );

	eTeSetCaretState( hE, CARET_OFF );

	if ( gClicks > 3 )
		gClicks = 3;

	if ( modifierKeys & shiftKey ) {
		gClicks = 1;
		if ( ! (*pE).selActive ) {
			(*pE).selStart = (*pE).caretChPos;
			(*pE).selEnd = (*pE).selStart;
			(*pE).selActive = TRUE;
		}
		start = eTeChPosToOffset( hE, (*pE).selStart );
		end = eTeChPosToOffset( hE, (*pE).selEnd );
		/*  24Jul92  e  was...
		if( modifierKeys & optionKey )
			anchorStart = anchorEnd = end;
		else
			anchorStart = anchorEnd = start;
		s.b.... */
		if( eTeChPosToOffset( hE, (*pE).caretChPos ) == start )
			anchorStart = anchorEnd = (long )( ( modifierKeys & optionKey ) ? start : end );
		else
			anchorStart = anchorEnd = (long )( ( modifierKeys & optionKey ) ? end : start );
		/* since caretChPos is the active end    24Jul92  e  */
	}
	else if ( (*pE).selActive ) {
		eTeHiliteRange( hE, (*pE).selStart, (*pE).selEnd );
		(*pE).selActive = FALSE;
	}

	/*
	 * The DO-WHILE loop monitors the cursor while the mouse button is held down.
	 * The selection is modified with changes in the cursor. NewPt is the new
	 * mouse location, while hitPt contains the last position.
	 */
	newPt = hitPt;
	do {
		if ( didScroll || PTL( newPt ) != PTL( hitPt ) ) {
			/* first time OR did scroll last loop OR mouse has moved */
			/* (didScroll == TRUE) causes this to execute first time */
			switch ( gClicks ) {
				case 3:
					pos = eTePointToChPos( hE, newPt );
					pos.h = 0;
					low = eTeChPosToOffset( hE, pos );
					if ( pos.v < (*pE).bounds.v - 1 ) {
						++pos.v;
						pos.h = 0;
					}
					else {
						pos.h = (*(*pE).hLines)[ pos.v + 1 ] - (*(*pE).hLines)[ pos.v ];
					}
					high = eTeChPosToOffset( hE, pos );
					break;

				case 2:
					offset = eTeChPosToOffset( hE, eTePointToChPos( hE, newPt ) );
					low = WordLimits( *(*pE).hText, offset, TRUE );
					high = WordLimits( *(*pE).hText, offset, FALSE );
					break;

				default:
					low = eTeChPosToOffset( hE, eTePointToChPos( hE, newPt ) );
					high = low;
					break;
			}
			/*
			 * Use selActive variable to tell us if this is the first time thru.
			 * If so, then set our anchor points.
			 * Otherwise we just update the selection around the anchors.
			 */
			if ( ! (*pE).selActive ) {
				start = anchorStart = low;
				end = anchorEnd = high;
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, low ), eTeOffsetToChPos( hE, high ) );
				(*pE).selActive = TRUE;
			}
			if ( low < start ) {
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, low ), eTeOffsetToChPos( hE, start ) );
				start = low;
			}
			else if ( low < anchorStart ) {
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, start ), eTeOffsetToChPos( hE, low ) );
				start = low;
			}
			else {
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, start ), eTeOffsetToChPos( hE, anchorStart ) );
				start = anchorStart;
			}
			if ( high > end ) {
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, end ), eTeOffsetToChPos( hE, high ) );
				end = high;
			}
			else if ( high > anchorEnd ) {
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, high ), eTeOffsetToChPos( hE, end ) );
				end = high;
			}
			else {
				eTeHiliteRange( hE, eTeOffsetToChPos( hE, anchorEnd ), eTeOffsetToChPos( hE, end ) );
				end = anchorEnd;
			}
			(*pE).selStart = eTeOffsetToChPos( hE, start );
			(*pE).selEnd = eTeOffsetToChPos( hE, end );
		}
		hitPt = newPt;
		GetMouse( &newPt );
		didScroll = eTeAutoScroll( hE, newPt );
	} while ( StillDown() );

	if ( start == end ) {
		/*
		(*pE).caretChPos = eTeOffsetToChPos( hE, start );
		(*pE).selActive = FALSE;
		(*pE).selStart = (*pE).caretChPos;
		(*pE).selEnd = (*pE).caretChPos;
		*/
		(*pE).selActive = FALSE;
		(*pE).caretChPos = (*pE).selStart;
	}
	else {
		if( start == anchorStart )
			(*pE).caretChPos = (*pE).selEnd;
		else
			(*pE).caretChPos = (*pE).selStart;
	}
	eTeUpdateCaretRect( hE );
	(*pE).maxRight = (*pE).caretRect.right;
	eTeSetCaretState( hE, CARET_ON );
	HUnlock( (Handle )hE );
}

/* 2May92  e  -- Style */

static short eTeOffsetToRun( eRec **hE, long anOffset )
{
	register long a;
	register long *runPtr;
	register long qRuns, count, j;

	if ( anOffset < 0 )  return -1;	/* special case for j == 0 below */
	if ( anOffset == 0 )  return 0;	/* special case for empty text */

	runPtr = *(**hE).hRuns;
	count = 0;
	qRuns = (**hE).qRuns;
	while ( count < qRuns )	{
		j = (count + qRuns - 1) >> 1;
		if( anOffset < runPtr[ j ] )
			qRuns = j;
		else if( anOffset >= runPtr[ j + 1 ] )
			count = j + 1;
		else	{
			count = j;
			break;
		}
	}
	return count;
}

static short eTeRunToStyle( eRec **hE, short r )
{
	return ( ( r & 1 ) ^ (**hE).fustStyle );
}

/* 12Sep92  e  */
void eTeGetRun( eRec **hE, long *sta, long *end )
{
	long car = eTeChPosToOffset( hE, (**hE).caretChPos );
	short run = eTeOffsetToRun( hE, car-1 );
	short sty = eTeRunToStyle( hE, run );
	if( run < 0 || sty == 0 )
	  *sta = *end = car;
	else
	{ long *runVec = *(**hE).hRuns;
	  *sta = runVec[run];
	  *end = runVec[run+1];
	}
}

static void eTeUpdateRuns( eRec **hE, long j, long k, long n, short ns )
{	/* removing chars j..k, inserting n chars of style ns */
	short jr = eTeOffsetToRun( hE, j - 1 );
	short kr = eTeOffsetToRun( hE, k );
	short js = eTeRunToStyle( hE, jr );
	short ks = eTeRunToStyle( hE, kr );
	long adj = n - ( k - j );
	long *runVec = *(**hE).hRuns;
	short qRuns = (**hE).qRuns;
	
	if( n == 0 ) ns = ks;
	
	if( ( ns != js ) || ( j == 0 ) )
	{	/* can't merge n with j */
		if( j == 0 ) (**hE).fustStyle = ns;
		/* 10Aug92  e  was: 
		if( (( n != 0 ) && ( kr == qRuns )) || (( ns != ks ) && ( kr <= jr + 1 )) ) */
		if( ( n != 0 ) && ( kr <= jr + 1 ) && ( kr == qRuns || ns != ks ) )
		{	/* grow */
			short indx, incr = ( ( j == 0 ) || ( kr == qRuns ) ) ? 1 : 2;
			qRuns += incr;
			if ( qRuns >= (**hE).runsAllocated )
			{	(**hE).runsAllocated += MIN_RUNS;
				SetHandleSize( (Handle )(**hE).hRuns, (**hE).runsAllocated * sizeof( long ) );
				runVec = *(**hE).hRuns;
			}
			kr = jr + incr;
			for( indx = qRuns; indx > kr; indx-- )
				runVec[indx] = runVec[indx-incr] + adj;
			runVec[++jr] = j;
			if( jr != kr ) runVec[++jr] = j + n;
			(**hE).qRuns = qRuns;
			return;
		}
		runVec[++jr] = j;
	}
#ifdef use_old_buggy_code
	/* 16Sep92  e  was... */
	else if( ( kr == qRuns ) && ( ns == ks ) /* && ( ns == js ) */ )
		kr--;
	if( ns == ks )
#endif
	if( ns == ks && ( kr != qRuns || ns != js ) )	/* 16Sep92  e */
	{	/* merge k with n (1&3) */
		while( kr < qRuns ) runVec[++jr] = runVec[++kr] + adj;
	}
	else
	{	/* can't merge k with n (2&4) */
		if( k > runVec[kr] ) { runVec[++jr] = j + n; kr++; }	/*  13Oct92  e  */
		while( kr <= qRuns ) runVec[++jr] = runVec[kr++] + adj;
	}
	(**hE).qRuns = jr;
	/* see if we should shrink the runs vector */
	if ( jr + MIN_RUNS + MIN_RUNS < (**hE).runsAllocated )
	{	(**hE).runsAllocated = ( jr / MIN_RUNS + 1 ) * MIN_RUNS;
		SetHandleSize( (Handle )(**hE).hRuns, (**hE).runsAllocated * sizeof( long ) );
	}
}

/* ******** */

static short eTeUpdateLineStarts( eRec **hE, short firstLine )
{
	char hdlState;
	short error = noErr;
	register LineRec 	*linePtr;
	register char		*charPtr, c;
	register short		numLines;
	register long		maxLine;	/* 28May92  e */
	register long		offsLine;	/* 28May92  e */
	register short		wrap;		/*  5Jul92  e */
	eRec	*pE;
	char    *pT;

	MoveHHi( (Handle )hE );
	HLock( (Handle )hE );
	pE = *hE;
	MoveHHi( (*pE).hText );
	HLock( (*pE).hText );
	pT  = *(*pE).hText;
	HLock( (Handle )(*pE).hLines );

	linePtr = *(*pE).hLines;
	numLines = firstLine + 1;
	
	maxLine = (firstLine == 0) ? 0 : (*pE).bounds.h;	/* 28May92  e  */
	wrap = (*pE).wrap;									/*  5Jul92  e  */

	/* Scan entire block of text, start with line firstLine */
	for ( charPtr = linePtr[ firstLine ] + pT; ( c = *charPtr ) != '\0';  ++charPtr )
	{	/* Found end of line. Store next line's starting position */
		if ( c == RETURN
			 || ( --wrap <= 0 && (c = charPtr[1]) != RETURN && c != '\0' ) )
			 /* 10Aug92  e  added RETURN test  |  11Aug92  e  added '\0' test  */
		{	wrap = (*pE).wrap;									/*  5Jul92  e  */
			offsLine = charPtr - pT + 1;
			if( ( offsLine - linePtr[firstLine] ) >= maxLine )	/* 10Aug92  e  was > */
				maxLine = offsLine - linePtr[firstLine] + 1;	/* 10Aug92  e  +1 */
			linePtr[ ++firstLine ] = offsLine;
			/* */
			++numLines;
			/* See if we need to allocate more space for our line starts */
			if ( numLines >= (*pE).linesAllocated )
			{	(*pE).linesAllocated += MIN_LINES;
				HUnlock( (Handle )(*pE).hLines );
				SetHandleSize( (Handle )(*pE).hLines, (*pE).linesAllocated * sizeof( LineRec ) );
  				asm { move.w D0, error } 		/* error = MemError(); */
  				if( error != noErr )
  				{	HUnlock( (*pE).hText );
  					HUnlock( (Handle )hE );
  					return error;
  				}
				MoveHHi( (Handle )(*pE).hLines );
				HLock( (Handle )(*pE).hLines );
				linePtr = *(*pE).hLines;
			}
		}
	}
	/* Last entry contains length of entire text block */
	offsLine = charPtr - pT;
	if( ( offsLine - linePtr[firstLine] ) >= maxLine )			/* 10Aug92  e  was > */
		maxLine = offsLine - linePtr[firstLine] + 1;			/* 10Aug92  e  +1 */
	linePtr[ firstLine + 1 ] = offsLine;

	HUnlock( (*pE).hText );
	HUnlock( (Handle )(*pE).hLines );

	/* See if we should shrink our line starts block */
	if ( numLines + MIN_LINES < (*pE).linesAllocated ) {
		(*pE).linesAllocated = ( numLines / MIN_LINES + 1 ) * MIN_LINES;
		SetHandleSize( (Handle )(*pE).hLines, (*pE).linesAllocated * sizeof( LineRec ) );
  		asm { move.w D0, error } 		/* error = MemError(); */
	}

	(*pE).bounds.h = Min( maxLine, 32767 );  /* 28May92  e  */
	(*pE).bounds.v = numLines;
	HUnlock( (Handle )hE );
	eTeAdjustScrollMax( hE );
	return error;
}

static short eTeSetTextGuts( eRec** hE, Handle hT, long numChars )
{	short error = noErr;
	if ( (**hE).hText )
		DisposHandle( (**hE).hText );
	if ( (*hT)[ numChars -1 ] != 0 ) {
		numChars += 1;
		SetHandleSize( hT, numChars );
  		asm { move.w D0, error } 		/* error = MemError(); */
  		if( error != noErr ) return error;
	}
	(*hT)[ numChars - 1 ] = '\0';
	(**hE).hText = hT;
	error = eTeUpdateLineStarts( hE, 0 );
  	if( error != noErr ) return error;
	error = eTeNewRuns( hE, numChars - 1 );
  	if( error != noErr ) return error;
	(**hE).caretChPos.h = (**hE).caretChPos.v = 0L;
	(**hE).selStart = (**hE).selEnd = (**hE).caretChPos;
	(**hE).maxRight = 0;
	eTeUpdateCaretRect( hE );
	(**hE).dirty = FALSE;
	eTeRefresh( hE );							/* 17Dec92  e  */
	return error;
}


short eTeSetTextHandleDetabify( eRec **hE, Handle hT, short tabstops )
{	long count;
	long size = GetHandleSize( hT );
	short error = DeTabifyHandle( hT, &size, &count, tabstops >= 0 ? tabstops : (**hE).tabStops );
	if ( error != noErr ) return error;
	/* see if count lines fit in lineStarts array */
	if( count > ( 32767 - ( MIN_LINES >> 1) ) )
		return -357;
	if ( count >= (**hE).linesAllocated )
	{	(**hE).linesAllocated = count + ( MIN_LINES >> 1);
		SetHandleSize( (Handle )(**hE).hLines, (**hE).linesAllocated * sizeof( LineRec ) );
  		asm { move.w D0, error } 		/* error = MemError(); */
  		if( error != noErr )
  		{	(**hE).linesAllocated = 0;
  			return error;
  		}
	}
	return eTeSetTextGuts( hE, hT, size );
}

short eTeSetTextHandle( eRec** hE, Handle hT )
{
	return eTeSetTextGuts( hE, hT, GetHandleSize( hT ) );
}

short eTeSetTextPtr( eRec** hE, Ptr pT, long numChars )
{
	Handle hT;
	
	if ( pT[ numChars -1 ] != 0 )
		numChars += 1;
	PtrToHand( pT, &hT, numChars );
	(*hT)[ numChars - 1 ] = '\0';
	return eTeSetTextGuts( hE, hT, numChars );
}

void eTeSetSelect( eRec **hE, long aStart, long anEnd )
{
	ChPos	first, last;
	/* Unhilite existing selection */
	if ( (**hE).selActive ) {
		first = (**hE).selStart;
		last = (**hE).selEnd;
		eTeHiliteRange( hE, first, last );
		(**hE).selActive = FALSE;
	}
	else
		eTeSetCaretState( hE, CARET_OFF );

	if ( aStart < 0 ) aStart = 0;
	if ( anEnd < 0 ) anEnd = 0;
	if ( aStart >= anEnd ) {
		(**hE).selStart = (**hE).selEnd = eTeOffsetToChPos( hE, aStart );
	}
	else {
		(**hE).selActive = TRUE;
		first = eTeOffsetToChPos( hE, aStart );
		last = eTeOffsetToChPos( hE, anEnd );
		(**hE).selStart = first;
		(**hE).selEnd = last;
		eTeHiliteRange( hE, first, last );
	}
	(**hE).caretChPos = (**hE).selEnd;
	eTeUpdateCaretRect( hE );
	(**hE).maxRight = (**hE).caretRect.right;
}

static Boolean eTeHasReturns( register char *data, register long len )
{
	while( len-- )
		if ( *data++ == RETURN )
			return TRUE;
	return FALSE;
}

/* the scrap  13Aug92  e  */

void 	eTePutScrap()
{	OSErr		err;

	HLock( eTeScrap );
	/* Make a copy of the scrap and give to the clipboard */
	if( ( err = ZeroScrap() ) == noErr )
	{	err = PutScrap( eTeScrapLen, 'TEXT', *eTeScrap );
	}
	if( err == noErr )
	{	
	}
	else SysBeep( 3 );
	eTeScrapCnt = (InfoScrap())->scrapCount;
	HUnlock( eTeScrap );
}

static void puntUndoStuff()
{
	eTeUndoStuff.undoTeRec = NULL;
	eTeUndoStuff.undoTitle = utUndo;
	eTeUndoStuff.undoProc  = undoBugNi;
}

void 	eTeGetScrap()
{	long offset, length;

	if( eTeScrapCnt != (InfoScrap())->scrapCount )
	{	length = GetScrap( NULL, 'TEXT', &offset );
		if ( length >= 0 )
		{ eTeScrapLen = GetScrap( eTeScrap, 'TEXT', &offset );
		  puntUndoStuff();
		}
		else if( length != noTypeErr )
		{ SysBeep( 3 );
		  SetHandleSize( eTeScrap, 0 );
		  eTeScrapLen = 0;
		  puntUndoStuff();
		}
		/* else punt: noTypeErr => nothing to copy */
	}
}

/* I don't feel bad using a large buffer on the stack,
   even on a MacPlus Quickdraw uses several K of stack space to draw text! */
#define TR_BUF_SZ 4000

/* ####################################################################

14Aug92  e

Undo notes...

try to alloc a temp handle in dodoTyping()

concatenate successive kills (use clip or undo ?) ?

add to eTeKeyIns() mechanism for delete and del>

  if undoTyping or undoClear already active && there's no selection

    if normal char
      if caret >= undoStart && caret <= undoStart + insertLen
      increment insertLen
      insert char
    else
      start new undo & keyAccum

    if Delete
      if caret > undoStart && caret <= undoStart + insertLen
        decr insertLen
        do the delete
      else if caret == undoStart
        put character at caret-1 onto front of undoText
        do the delete
      else
        start new undo & keyAccum

    if Del>
      if caret >= undoStart && caret < undoStart + insertLen
        decr insertLen
        do the delete
      else if caret == undoStart + insertLen
        put character at caret onto end of undoText
        do the delete
      else
        start new undo & keyAccum

  else
    what's done now

ensureSelOK() is only necessary
 because caretChPos is not kept syncronized with selStart & selEnd and vice versa
 should this be fixed so ensureSelOK() can be eliminated?  for other reasons?

make eTeTranspose() work better with undo -- at least adjust offsets?

subroutinize undo routines
  -- eTeEdGuts( hE, EMPTY_PTR, 0L, FALSE, TRUE, 0 ); ??
  -- hoseUndoText();
  -- rsSelectionPts();

/* ##################### */

static void ensureSelOK( eRec **hE )				/* can this be eliminated !? */
{	if( ! (**hE).selActive )
		(**hE).selStart = (**hE).selEnd = (**hE).caretChPos;
}

void eTeUndo( eRec **hE )
{
	if( hE == eTeUndoStuff.undoTeRec )
		eTeUndoStuff.undoProc( hE );
	else SysBeep( 3 );
}

void eTeEditMenuUpdate( eRec **hE, MenuHandle hM )
{
	SetItem( hM, 1, eTeUndoStuff.undoTitle );		/* memoize this ?! */
	if( hE != NULL )
	{	if( (**hE).selActive )
		{	EnableItem( hM, 3 ); /* cut */
			EnableItem( hM, 4 ); /* copy */
		}
		else
		{	DisableItem( hM, 3 ); /* cut */
			DisableItem( hM, 4 ); /* copy */
		}
		EnableItem( hM, 5 ); /* paste */
		EnableItem( hM, 6 ); /* clear */
		if( hE == eTeUndoStuff.undoTeRec )
			EnableItem( hM, 1 );
		else
			DisableItem( hM, 1 ); /* undo */
	}
	else
	{	EnableItem( hM, 3 ); /* cut */
		EnableItem( hM, 4 ); /* copy */
		EnableItem( hM, 5 ); /* paste */
		EnableItem( hM, 6 ); /* clear */
		DisableItem( hM, 1 ); /* undo */
	}
}

static void ssSelectionPts( eRec **hE )
{	ensureSelOK( hE );
	eTeUndoStuff.undoInLen = 0;
	eTeUndoStuff.undoKeyAccum = FALSE;
	eTeUndoStuff.undoStart = eTeChPosToOffset( hE, (**hE).selStart );
	eTeUndoStuff.undoEnd   = eTeChPosToOffset( hE, (**hE).selEnd   );
	eTeUndoStuff.undoCarStaP = PTL( (**hE).caretChPos ) == PTL( (**hE).selStart );
	eTeUndoStuff.undoDirty = (**hE).dirty;
	eTeUndoStuff.undoTeRec = hE;
}

static void ssSwapScrap()
{	Handle h;
	long l;
	h = eTeScrap;
	l = eTeScrapLen;
	eTeScrap = eTeUndoStuff.undoText;
	eTeScrapLen = eTeUndoStuff.undoTxLen;
	eTeUndoStuff.undoText = h;
	eTeUndoStuff.undoTxLen = l;
}

static void eTeSetSelCar( eRec **hE, long aStart, long anEnd, Boolean stap )
{
	eTeSetSelect( hE, aStart, anEnd );
	if( stap ) (**hE).caretChPos = (**hE).selStart;
}

static void eTePasteNu( eRec **hE, short style )
{
	MoveHHi( eTeScrap );
	HLock( eTeScrap );
	eTeEdGuts( hE, *eTeScrap, eTeScrapLen, eTeHasReturns( *eTeScrap, eTeScrapLen ), TRUE, style );
	HUnlock( eTeScrap );
}

static void eTeCopyNu( eRec **hE )
{
	long		length, offset;

	/* Get starting position and length of text to copy */
	offset = eTeChPosToOffset( hE, (**hE).selStart );
	length = eTeChPosToOffset( hE, (**hE).selEnd ) - offset;
	MoveHHi( (**hE).hText );
	HLock( (**hE).hText );
	/* Make a copy of the text and put in scrap */
	offset = Munger( eTeScrap, 0L, NULL, eTeScrapLen, *(**hE).hText + offset, length );
	if( offset == length )
		eTeScrapLen = length; /* success */
	else SysBeep( 3 );
	HUnlock( (**hE).hText );
}

static void undoCut( eRec  **hE )				/* Undo Cut */
{												/* restore selection from scrap */
	eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart );
	eTePasteNu( hE, 0 );
	ssSwapScrap();         						/* restore scrap */
	SetHandleSize( eTeUndoStuff.undoText, 0 );
	eTeUndoStuff.undoTxLen = 0;
	eTeSetSelCar( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoEnd, eTeUndoStuff.undoCarStaP );
	(**hE).dirty = eTeUndoStuff.undoDirty;		/* restore selection points & flags */
	eTeUndoStuff.undoTitle = rtCut;
	eTeUndoStuff.undoProc  = eTeCut;
}
  
void eTeCut( eRec **hE )						/*  Cut == RedoCut  */
{	if( ! (**hE).selActive ) return;
	ssSelectionPts( hE );  /* snapshot selection points */
	ssSwapScrap();         /* snapshot scrap */
	eTeUndoStuff.undoTitle = utCut;
	eTeUndoStuff.undoProc  = undoCut;
	eTeCopyNu( hE );							/* do cut */
	eTeEdGuts( hE, EMPTY_PTR, 0L, FALSE, TRUE, 0 );
}

static void undoCopy( eRec  **hE )				/* Undo Copy */
{	
	ssSwapScrap();         						/* restore scrap */
	SetHandleSize( eTeUndoStuff.undoText, 0 );
	eTeUndoStuff.undoTxLen = 0;
	eTeSetSelCar( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoEnd, eTeUndoStuff.undoCarStaP );
	(**hE).dirty = eTeUndoStuff.undoDirty;		/* restore selection points & flags */
	eTeUndoStuff.undoTitle = rtCopy;
	eTeUndoStuff.undoProc  = eTeCopy;
}
  
void eTeCopy( eRec **hE )						/* Copy == Redo Copy */
{	if( ! (**hE).selActive ) return;
	ssSelectionPts( hE );  						/* snapshot selection points */
	ssSwapScrap();         						/* snapshot scrap */
	eTeUndoStuff.undoTitle = utCopy;
	eTeUndoStuff.undoProc  = undoCopy;
	eTeCopyNu( hE );							/* do copy */
}

static void redoPaste( eRec  **hE )				/* Redo Paste */
{	eTePaste( hE, 0 );
}

static void undoPaste( eRec  **hE )				/* Undo Paste */
{												/* restore selection from scrap */
	eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart + eTeScrapLen );
	ssSwapScrap();         						/* swap scrap & undo */
	eTePasteNu( hE, 0 );						/* restore text from undo */
	ssSwapScrap();         						/* swap scrap & undo */
	SetHandleSize( eTeUndoStuff.undoText, 0 );
	eTeUndoStuff.undoTxLen = 0;
	eTeSetSelCar( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoEnd, eTeUndoStuff.undoCarStaP );
	(**hE).dirty = eTeUndoStuff.undoDirty;		/* restore selection points & flags */
	eTeUndoStuff.undoTitle = rtPaste;
	eTeUndoStuff.undoProc  = redoPaste;
}

void eTePaste( eRec **hE, short style )			/* Paste != Redo Paste */
{	ssSelectionPts( hE );  						/* snapshot selection points */
	ssSwapScrap();         						/* snapshot selection */
	eTeCopyNu( hE );
	ssSwapScrap();         						/* restore scrap */
	eTeUndoStuff.undoTitle = utPaste;
	eTeUndoStuff.undoProc  = undoPaste;
	eTePasteNu( hE, style );					/* do paste [14Sep92  e  -- added style back] */
}

static void undoDelete( eRec **hE )				/* Undo Clear */
{												/* restore selection from scrap */
	eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart );
	ssSwapScrap();         						/* swap scrap & undo */
	eTePasteNu( hE, 0 );						/* restore text from undo */
	ssSwapScrap();         						/* swap scrap & undo */
	SetHandleSize( eTeUndoStuff.undoText, 0 );
	eTeUndoStuff.undoTxLen = 0;
	eTeSetSelCar( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoEnd, eTeUndoStuff.undoCarStaP );
	(**hE).dirty = eTeUndoStuff.undoDirty;		/* restore selection points & flags */
	eTeUndoStuff.undoTitle = rtClear;
	eTeUndoStuff.undoProc  = eTeDelete;
}

void eTeDelete( eRec **hE )						/* Clear == Redo Clear */
{	long offset;
	if ( ! (**hE).selActive )
	{	/* Just delete previous character */
		offset = eTeChPosToOffset( hE, (**hE).caretChPos );
		if ( offset <= 0 ) return;
		ssSelectionPts( hE );  						/* snapshot selection points */
		eTeUndoStuff.undoStart--;
		(**hE).selEnd = (**hE).caretChPos;
		(**hE).selStart = eTeOffsetToChPos( hE, --offset );
	}
	else
		ssSelectionPts( hE );  						/* snapshot selection points */
	ssSwapScrap();         						/* snapshot selection */
	eTeCopyNu( hE );
	ssSwapScrap();         						/* restore scrap */
	eTeUndoStuff.undoTitle = utClear;
	eTeUndoStuff.undoProc  = undoDelete;
	if ( (**hE).selActive )
		eTeEdGuts( hE, EMPTY_PTR, 0L, FALSE, TRUE, 0 );
	else
	{	(**hE).selEnd = (**hE).caretChPos = (**hE).selStart;
		eTeSetCaretState( hE, CARET_OFF ); /* 23Jul92  e */
		eTeUpdateCaretRect( hE );
		eTeEdGuts( hE, EMPTY_PTR, -1L, *(*(**hE).hText + offset) == RETURN, TRUE, 0 );
	}
}

static void redoKill( eRec **hE )				/* Redo Kill */
{
	eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart );
	eTeKillTo( hE, eTeOffsetToChPos( hE, eTeUndoStuff.undoEnd ) );
}

static void undoKill( eRec  **hE )				/* Undo Kill */
{												/* restore selection from scrap */
	eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart );
	eTePasteNu( hE, 0 );
	ssSwapScrap();         						/* restore scrap */
	SetHandleSize( eTeUndoStuff.undoText, 0 );
	eTeUndoStuff.undoTxLen = 0;
	eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart );
	(**hE).dirty = eTeUndoStuff.undoDirty;		/* restore selection points & flags */
	eTeUndoStuff.undoTitle = rtKill;
	eTeUndoStuff.undoProc  = redoKill;
}
  
void eTeKillTo( eRec **hE, ChPos chPos )		/* Kill != Redo Kill */
{
	if( PTL( chPos ) == PTL( (**hE).caretChPos ) ) return;
	(**hE).selStart = (**hE).caretChPos;		/* mung selection points first! */
	(**hE).selEnd = chPos;
	(**hE).selActive = TRUE;
	ssSelectionPts( hE );  						/* snapshot selection points */
	ssSwapScrap();         						/* snapshot scrap */
	eTeCopyNu( hE );
	eTeUndoStuff.undoTitle = utKill;
	eTeUndoStuff.undoProc  = undoKill;
	/* since we'll redraw the line anyway, eTeHiliteRange isn't necessary!?    */
	/*  unfortunately, for wrapped lines, it is - otherwise cursor turds; ugh. */
	/* eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );                   */
	/* instead we'll fake it out by making the window look inactive instead    */
	(**hE).active = FALSE;
	eTeEdGuts( hE, EMPTY_PTR, 0L, FALSE, TRUE, 0 );
	(**hE).active = TRUE;
}

static Boolean dodoTyping( eRec **hE )
{	char buf[TR_BUF_SZ];
	long sz;
	register char *p, *q;
	register long iter = sz = eTeUndoStuff.undoTxLen;;
	
	if( iter > 0 || eTeUndoStuff.undoInLen > 0 )
	{
		if( eTeUndoStuff.undoInLen > TR_BUF_SZ )
		{	SysBeep( 6 );							/* try to alloc a temp handle ? */
			return FALSE;
		}
		q = buf;
		p = *eTeUndoStuff.undoText;
		while( iter-- ) *q++ = *p++;
		eTeSetSelect( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoStart + eTeUndoStuff.undoInLen );
		ssSwapScrap();         						/* snapshot insertion */
		eTeCopyNu( hE );
		ssSwapScrap();         						/* restore scrap */
		eTeEdGuts( hE, buf, sz, eTeHasReturns( buf, sz ), TRUE, 0 );
		eTeUndoStuff.undoInLen = sz;
	}
	return TRUE;
}

static void redoTyping( eRec **hE );

static void undoTyping( eRec **hE )
{	if( dodoTyping( hE ) )
	{	eTeUndoStuff.undoKeyAccum = FALSE;
		eTeSetSelCar( hE, eTeUndoStuff.undoStart, eTeUndoStuff.undoEnd, eTeUndoStuff.undoCarStaP );
		(**hE).dirty = eTeUndoStuff.undoDirty;		/* restore selection points & flags */
		eTeUndoStuff.undoTitle = rtTyping;
		eTeUndoStuff.undoProc  = redoTyping;
	}
}
	
static void redoTyping( eRec **hE )
{	eTeUndoStuff.undoKeyAccum = TRUE;
	eTeUndoStuff.undoDirty = (**hE).dirty;
	if( dodoTyping( hE ) )
	{	eTeUndoStuff.undoTitle = utTyping;
		eTeUndoStuff.undoProc  = undoTyping;
	}
	else
	{	eTeUndoStuff.undoKeyAccum = FALSE;
	}
}

static void eTeKeyIns( eRec **hE, char c, short style )
{
	if( eTeUndoStuff.undoKeyAccum
		&& hE == eTeUndoStuff.undoTeRec
		&& (**hE).selActive == FALSE
		&& eTeChPosToOffset( hE, (**hE).caretChPos )
			 == eTeUndoStuff.undoStart + eTeUndoStuff.undoInLen
	  )
	{	eTeUndoStuff.undoInLen += 1;			/* incr numChars inserted */
	}
	else
	{	ssSelectionPts( hE );  					/* snapshot selection points */
		eTeUndoStuff.undoKeyAccum = TRUE;
		ssSwapScrap();         					/* snapshot selection */
		eTeCopyNu( hE );
		ssSwapScrap();         					/* restore scrap */
		eTeUndoStuff.undoInLen = 1;				/* remember numChars inserted */
		eTeUndoStuff.undoTitle = utTyping;
		eTeUndoStuff.undoProc  = undoTyping;
	}
	eTeEdGuts( hE, &c, 1, c == RETURN, TRUE, style );
}

void eTeInsert( eRec **hE, Ptr textPtr, long numChars, short style )
{	if( numChars <= 0 ) { eTeDelete( hE ); return; } /* 08Jan93  e  */
	ssSelectionPts( hE );  						/* snapshot selection points */
	eTeUndoStuff.undoKeyAccum = TRUE;
	ssSwapScrap();         						/* snapshot selection */
	eTeCopyNu( hE );
	ssSwapScrap();         						/* restore scrap */
	eTeUndoStuff.undoInLen = numChars;			/* remember numChars inserted */
	eTeUndoStuff.undoTitle = utInsert;
	eTeUndoStuff.undoProc  = undoTyping;
	eTeEdGuts( hE, textPtr, numChars, eTeHasReturns( textPtr, numChars ), TRUE, style );
}

/* #################################################################### */

/* eTeTranspose  22Jul92  e  */

static void eTeTransposeGuts1( eRec **hE, long beg, long mid, long end )
{ char buf[TR_BUF_SZ];
  long shift = mid - beg;
  register char *p, *q;
  register long incr, iter;
  
  while( shift > 0 )
  { incr = Min( shift, TR_BUF_SZ );
    shift -= incr;
    /* */
    p = *(**hE).hText + beg;
    q = buf;
    iter = incr;
    while( iter-- ) *q++ = *p++;
    /* */
    p = *(**hE).hText + beg + incr;
    q = *(**hE).hText + beg;
    iter = end - beg - incr;
    while( iter-- ) *q++ = *p++;
    /* */
    p = buf;
    q = *(**hE).hText + end - incr;
    iter = incr;
    while( iter-- ) *q++ = *p++;
  }
}

static void eTeTransposeGuts0( eRec **hE, long beg, long midl, long midr, long end )
{ char buf[TR_BUF_SZ];
  register char *p, *q;
  register long iter;
  
  q = buf;
  p = *(**hE).hText + midl;
  iter = midr - midl;
  while( iter-- ) *q++ = *p++;
  p = *(**hE).hText + beg;
  iter = midl - beg;
  while( iter-- ) *q++ = *p++;
  q = *(**hE).hText + beg;
  p = *(**hE).hText + midr;
  iter = end - midr;
  while( iter-- ) *q++ = *p++;
  p = buf;
  iter = midr - beg;
  while( iter-- ) *q++ = *p++;
}

static void eTeTransposeGuts2( eRec **hE, long beg, long midl, long midr, long end )
{ 
  if( midr - beg <= TR_BUF_SZ )
  { eTeTransposeGuts0( hE, beg, midl, midr, end );
  }
  else
  { eTeTransposeGuts1( hE, beg, midl, midr );
    eTeTransposeGuts1( hE, beg, midr, end );
  }
}

void eTeTranspose( eRec **hE, long beg, long midl, long midr, long end )
{ long offs = beg + end - midr;
  long numChars = end - beg;
  char *textPtr;
  
  if ( (**hE).selActive
  	  || beg  <  0
  	  || beg  >= midl
  	  || midl >  midr
  	  || midr >= end
  	  || end  >  eTeTextLength( hE ) )
 { SysBeep(3);
   return;
  }
  eTeSetCaretState( hE, CARET_OFF );
  if( midl == midr )
    eTeTransposeGuts1( hE, beg, midr, end );
  else
    eTeTransposeGuts2( hE, beg, midl, midr, end );
  (**hE).selStart = eTeOffsetToChPos( hE, beg );
  (**hE).selEnd   = eTeOffsetToChPos( hE, end );
  (**hE).selActive = TRUE;
  textPtr = *(**hE).hText + beg;
  /* fake it out by making the window look inactive */
  (**hE).active = FALSE;
  eTeEdGuts( hE, textPtr, numChars, eTeHasReturns( textPtr, numChars ), FALSE, 0 );
  (**hE).active = TRUE;
  eTeSetSelect( hE, offs, offs );
  eTeShowCaret( hE );
  puntUndoStuff(); 				/* all bets off for now    --  17Aug92  e */
}

/* eTeWrite  9Jul92  e  */

void eTeWrite( eRec **hE, Ptr textPtr, long numChars, short style )
{	/* save selection, etc. */
	long sta = eTeChPosToOffset( hE, (**hE).selStart );
	long end = eTeChPosToOffset( hE, (**hE).selEnd );
	long car = eTeChPosToOffset( hE, (**hE).caretChPos );
	long pos = eTeChPosToOffset( hE, (**hE).writeChPos );
	short mxr = (**hE).maxRight;							/* 10Aug92  e  */
	Boolean mxrp = mxr != (**hE).caretRect.right;			/* 10Aug92  e  */
	Boolean selp = (**hE).selActive;
	Boolean stap = selp && car == sta;
	if( selp ) eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
	else       eTeSetCaretState( hE, CARET_OFF );           /*  14Jul92  e  */
	/* swap caretChPos & writeChPos */
	(**hE).selActive = FALSE;
	(**hE).caretChPos = (**hE).writeChPos;
	eTeUpdateCaretRect( hE );
	/* show was always FALSE; changed to ( pos == car )     --  14Jul92  e  */
	eTeEdGuts( hE, textPtr, numChars, eTeHasReturns( textPtr, numChars ), ( pos == car ), style );
	/* set writeChPos to caretChPos */
	(**hE).writeChPos = (**hE).caretChPos;
	/* restore selection, etc. */
	if( selp )
	{ if( pos < end )
	  { end += numChars;
	    if( pos <= sta ) sta += numChars;
	  }
	}
	else
	{ if( pos <= car ) car += numChars;
	  sta = end = car;
	}
	eTeSetSelCar( hE, sta, end, stap );
	if( mxrp ) (**hE).maxRight = mxr;						/* 10Aug92  e  */
	if( hE == eTeUndoStuff.undoTeRec )						/* 17Aug92  e  */	
	{	if( pos <= eTeUndoStuff.undoStart )
		{	eTeUndoStuff.undoEnd   += numChars;
			eTeUndoStuff.undoStart += numChars;
		}
		else if( pos < eTeUndoStuff.undoEnd )
			puntUndoStuff(); /* all bets off */
		/* else OK, a write after undo stuff */
	}
}

/* 11Aug92  e   rewritten with eol and sol and new eTeUpdateLineStarts case  */

static Boolean eTeAdjustLineStarts( eRec **hE, register short start, short amount )
{
	register short bot = (**hE).bounds.v;
	register LineRec *linePtr;
	register long incr;
	long llen;
	long sol, eol;
	char *pT, c;

	if ( ( incr = (long )amount ) != 0 ) {
		/* 28May92  e  was: linePtr = *(**hE).hLines + start + 1; */
		pT = *(**hE).hText;
		linePtr = *(**hE).hLines + start;
		sol = *linePtr++;
		eol = *linePtr + incr;
		llen = eol - sol;
		if( llen > (**hE).bounds.h )
		{ (**hE).bounds.h = Min( llen, ( (**hE).wrap + 1 ) );
		  HLock( (Handle )(**hE).hLines );
		  eTeAdjustScrollMax( hE );
		  HUnlock( (Handle )(**hE).hLines );
		}
#ifdef use_old_code
		/* 12Aug92  e  moved to eTeEdGuts() since it's also applicable when hasReturn is TRUE */
		if( start > 0
			&& pT[sol-1] != RETURN
			&& ( (c = pT[sol]) == RETURN || c == '\0' ) )
		{ /* this line starts off with a RETURN or NUL and prev line has no RETURN */
		  eTeUpdateLineStarts( hE, start - 1 );
		  return TRUE; /* -1 ? */
		}
		else
#endif
		if( llen > (**hE).wrap || ( (c = pT[eol-1]) != RETURN && c != '\0' ) )
		{ /* this line is longer than wrap or already doesn't end with a RETURN or NUL */
		  eTeUpdateLineStarts( hE, start );
		  return TRUE; /* +1 ? */
		}
		while ( start++ < bot )
			*linePtr++ += incr;
	}
	return FALSE;
}

/* eTeEdGuts
	handles insertion, deletion, and substitution
	If selection is active, then the text of the selection is deleted, then...
	If numChars is greater than 0, then the given text is inserted;
	 else if numChars is equal to 0, then no text is inserted;
	 otherwise (numChars is less than 0) AND THIS ONLY WORKS IF NO SELECTION...
	   minus numChars characters to the RIGHT of the caret position are deleted.
*/

static void eTeEdGuts( eRec **hE, Ptr textPtr, long numChars,
							 Boolean hasReturn, Boolean show, short style )
{
	register long	oldLen, offset;
	Rect			invalidRect;
	long newWrite = -1; /* 11Aug92  e   was: 0 */
	char *pT;			/* 12Aug92  e  */

	if ( (**hE).selActive ) {
		eTeHiliteRange( hE, (**hE).selStart, (**hE).selEnd );
		(**hE).selActive = FALSE;
		offset = eTeChPosToOffset( hE, (**hE).selStart );
		oldLen = eTeChPosToOffset( hE, (**hE).selEnd ) - offset;
		if( numChars < 0 ) numChars = 0;	/*  17Aug92  e   */
		if( (**hE).selEnd.v != (**hE).selStart.v )
			hasReturn = TRUE; /* 18May92  e   Oops! */
		else                  /*  5Jul92  2   Oops! */
			hasReturn |= eTeHasReturns( *(**hE).hText + offset, oldLen );
		/* 9Jul92  e */
		if( PTL( (**hE).writeChPos ) > PTL( (**hE).selStart ) )
		{ if( PTL( (**hE).writeChPos ) < PTL( (**hE).selEnd ) )
			(**hE).writeChPos = (**hE).selStart;
		  else
		    newWrite = eTeChPosToOffset( hE, (**hE).writeChPos ) + numChars - oldLen;
		}
		/* */
		(**hE).caretChPos = (**hE).selEnd = (**hE).selStart;
		/* 3May92  e  broken by AppleArrows
		topLeft( invalidRect ) = eTeChPosToPoint( hE, (**hE).caretChPos );
		invalidRect.left -= 1;
		*/
		eTeUpdateCaretRect( hE );
		topLeft( invalidRect ) = topLeft( (**hE).caretRect );
	}
	else {
		eTeSetCaretState( hE, CARET_OFF ); /* 23Jul92  e */
		offset = eTeChPosToOffset( hE, (**hE).caretChPos );
		/*  17Aug92  e  was: oldLen = numChars ? 0 : 1; */
		if( numChars < 0 )
		{	oldLen = - numChars;
			numChars = 0;
		}
		else oldLen = 0;
		/* 9Jul92  e */
		if( PTL( (**hE).writeChPos ) > PTL( (**hE).caretChPos ) )
		  newWrite = eTeChPosToOffset( hE, (**hE).writeChPos ) + numChars - oldLen;
		/* */
		topLeft( invalidRect ) = topLeft( (**hE).caretRect );
	}

	/* Do substitution/insertion */
	Munger( (**hE).hText, offset, NULL, oldLen, textPtr, numChars );
	(**hE).dirty = TRUE;
	
	/* 12Aug92  e    as a result of adding line wrap, need to check if
	                 preceeding line could be 'unwrapped' by this edit...
	*/
	if( (**hE).caretChPos.h == 0						  /* edit is at the start of a line */
		&& offset > 0									  /* and it's not the first line */
		&& ((pT=*(**hE).hText))[offset-1] != RETURN 	  /* and the prev line is a wrapped line */
		&& ( pT[offset] == RETURN || pT[offset] == '\0' ) /* and this line starts with CR or NUL */
	  )
	{	hasReturn = TRUE;
		eTeUpdateLineStarts( hE, (**hE).caretChPos.v - 1 );
		(**hE).caretChPos = eTeOffsetToChPos( hE, offset );
		eTeUpdateCaretRect( hE );
		topLeft( invalidRect ) = topLeft( (**hE).caretRect );
	}
	else if( hasReturn )
		eTeUpdateLineStarts( hE, (**hE).caretChPos.v );
	else
		hasReturn |= eTeAdjustLineStarts( hE, (**hE).caretChPos.v, numChars - oldLen );
	
	eTeUpdateRuns( hE, offset, offset + oldLen, numChars, style );

	invalidRect.right = (**hE).viewRect.right;		/* 21Jul92  e  moved outside of cond */
	
	/* Redraw the current line, starting at the caret */
	if ( invalidRect.left < (**hE).viewRect.right ) {
		invalidRect.bottom = invalidRect.top + (**hE).vScale;
		eTePrepare( hE );
		++invalidRect.left;  /* move up here 23Jul92  e  */
		EraseRect( &invalidRect );
		/* ++invalidRect.left; !?  22Jul92  e  */
		eTeDrawLine( hE, (**hE).caretChPos, topLeft( invalidRect ) );
	}

	/* if RETURN added, redraw from the next line to the end of the screen */
	if ( hasReturn ) {
		invalidRect.top += (**hE).vScale;
		/* this left ghost on left edge when xor caret used...
		invalidRect.left = (**hE).leftMargin - (**hE).hOrigin;
		23Jul92  e  -- instead... */
		invalidRect.left =  (**hE).viewRect.left;
		invalidRect.bottom = (**hE).viewRect.bottom;
		/* eTePrepare( hE );  -- done by eTeDraw  21Jul92  e  */
		eTeDraw( hE, &invalidRect );
		(**hE).caretChPos = eTeOffsetToChPos( hE, offset + numChars );
		eTeUpdateCaretRect( hE );
	}
	/* Otherwise, just adjust the caret point by moving it over the inserted text */
	else {
	 	eTePrepare( hE );
	 	eTePrepareStyle( hE, style );
	 	(**hE).caretChPos.h += numChars;
		for ( ; numChars > 0 ; ++textPtr, --numChars ) {
			if ( *textPtr == TAB ) {
				(**hE).caretRect.right = eTeTabStop( hE, (**hE).caretRect.right );
			}
			else
				(**hE).caretRect.right += CharWidth( *textPtr );
		}
		(**hE).caretRect.left = (**hE).caretRect.right - 1;
	}
	/* 9Jul92  e */
	if( newWrite >= 0 ) /* 11Aug92  e   was: != 0 */
	  (**hE).writeChPos = eTeOffsetToChPos( hE, newWrite );
	/* */
	if( show ) eTeShowCaret( hE );
	(**hE).maxRight = (**hE).caretRect.right;	/*  02Oct92  e  */
}

static short eTeChPosVToPointV( eRec **hE, short v )
{
	register long tmp;

	tmp = (long )v * (**hE).vScale + (**hE).topMargin - (**hE).vOrigin;
	if( tmp < -32000 )
		return -32000;
	if( tmp > 32000 )
		return 32000;
	return (short )tmp;
}

Point eTeChPosToPoint( eRec **hE, register ChPos aPos )
{
	register char *charPtr, c;
	register short left;
	long 	offset, instyle;
	short 	run;
	Point	pt;

	SignedByte hdlState;

	hdlState = HGetState( (**hE).hText );
	HLock( (**hE).hText );

	eTePrepare( hE );
	offset = *( *(**hE).hLines + aPos.v );
	charPtr = *(**hE).hText + offset;
	run = eTeOffsetToRun( hE, offset );
	left = (**hE).leftMargin - (**hE).hOrigin;
	do
	{	eTePrepareRun( hE, run );
		instyle = (*(**hE).hRuns)[++run] - offset;
		offset += instyle;
		while ( instyle-- && aPos.h-- )
		{	c = *charPtr++;
			if ( c == TAB )
				left = eTeTabStop( hE, left );
			else
				left += CharWidth( c );
		}
	} while( aPos.h > 0 );

	HSetState( (**hE).hText, hdlState );

	pt.h = left;

	/* may exceed limits of sixteen bits...
	pt.v = aPos.v * (**hE).vScale + (**hE).topMargin - (**hE).vOrigin;
	*/
	pt.v = eTeChPosVToPointV( hE, aPos.v);

	return( pt );
}

static ChPos eTePointHtoChPosH( eRec **hE, register Point aPt, register ChPos aPos )
{
	register char *charPtr;
	register short left, c, limit;
	SignedByte hdlState;
	long 	offset, instyle;
	short 	run;
	Point	pt;

	hdlState = HGetState( (**hE).hText );
	HLock( (**hE).hText );

	eTePrepare( hE );
	/* Stop when we reach the point.h given to us */
	left = (**hE).leftMargin - (**hE).hOrigin;
	offset = *( *(**hE).hLines + aPos.v );
	charPtr = *(**hE).hText + offset;
	limit = *(*(**hE).hLines + aPos.v + 1) - *(*(**hE).hLines + aPos.v);
	/* 5Jul92  e  if ( aPos.v < (**hE).bounds.v - 1 ) --limit; */
	if ( charPtr[limit-1] == RETURN ) --limit;	/* 5Jul92  e   */
	run = eTeOffsetToRun( hE, offset );
	aPos.h = 0;
	do
	{	eTePrepareRun( hE, run );
		instyle = (*(**hE).hRuns)[++run] - offset;
		offset += instyle;
		while ( instyle-- && ( aPos.h < limit ) )
		{	c = *charPtr++;
			if ( c == TAB )
				c = eTeTabStop( hE, left );
			else
				c = CharWidth( c ) + left;
			/* see if we have passed the point */
			if ( c >= aPt.h )
			{	/* see if the point is in the second half of the character,
				   if so the returned value points to the next character. */
				if ( c - aPt.h <= aPt.h - left ) aPos.h++;
				limit = -1; /* force outer loop escape */
				break;
			}
			left = c;
			aPos.h++;
		}
	} while( aPos.h < limit );

	HSetState( (**hE).hText, hdlState );
	return( aPos );
}

ChPos eTePointToChPos( eRec **hE, register Point aPt )
{
	register ChPos aPos;

	/* Restrict point to selectable text */
	if ( aPt.v < (**hE).topMargin - (**hE).vOrigin ) {
		aPos.v = 0;
		aPos.h = 0;
		return( aPos );
	}
	else {
		aPos.v = ( aPt.v - (**hE).topMargin + (**hE).vOrigin ) / (**hE).vScale;
		/* Past bottom of text? */
		if ( aPos.v > (**hE).bounds.v - 1 ) {
			aPos.v = (**hE).bounds.v - 1;
			aPos.h = *(*(**hE).hLines + aPos.v + 1) - *(*(**hE).hLines + aPos.v);
			return( aPos );
		}
	}
	return eTePointHtoChPosH( hE, aPt, aPos );
}

ChPos eTeOffsetToChPos( eRec **hE, register long anOffset )
{
	register ChPos aPos;
	register LineRec *linePtr;
	register long numLines, count, j;

	if ( anOffset < 0 ) {
		aPos.h = 0;
		aPos.v = 0;
		return( aPos );
	}

	linePtr = *(**hE).hLines;
	count = 0;
	numLines = (**hE).bounds.v - 1;
#ifdef use_original_slow_version
	while ( anOffset >= linePtr[ count + 1 ] && count < numLines )
		++count;
	aPos.v = count;
#else
	while ( count < numLines )	{
		j = (count + numLines - 1) >> 1;
		if( anOffset < linePtr[ j ] )
			numLines = j;
		else if( anOffset >= linePtr[ j + 1 ] )
			count = j + 1;
		else	{
			count = j;
			break;
		}
	}
	aPos.v = count;
#endif

	if ( anOffset > linePtr[ count + 1 ] )
		aPos.h = linePtr[ count + 1 ] - linePtr[ count ];
	else
		aPos.h = anOffset - linePtr[ count ];
	return( aPos );
}

long eTeChPosToOffset( eRec **hE, ChPos aPos )
{
	return( *(*(**hE).hLines + aPos.v) + aPos.h );
}

void eTeShowCaret( eRec **hE )
{
	short		deltaH, deltaV;
	register eRec *pE = *hE;

	deltaH = 0;
	deltaV = 0;

	/* 30Apr92  e  -- was...
	if ( (*pE).caretRect.bottom > (*pE).viewRect.bottom )
		deltaV = ( (*pE).caretRect.bottom - (*pE).viewRect.bottom + (*pE).vScale - 1 ) / (*pE).vScale;
	else if ( (*pE).caretRect.top < (*pE).viewRect.top ) {
		deltaV = ( (*pE).caretRect.top - (*pE).viewRect.top ) / (*pE).vScale;
	*/
	if ( (*pE).caretChPos.v >= (*pE).position.v + (*pE).vSpan )
		deltaV = (*pE).caretChPos.v - (*pE).position.v - (*pE).vSpan
				+ Min( (*pE).vContext, (*pE).vSpan >> 1 );
	else if ( (*pE).caretChPos.v < (*pE).position.v )
		deltaV = (*pE).caretChPos.v - (*pE).position.v
				 - Min( Min( (*pE).vContext, (*pE).vSpan >> 1 ), (*pE).caretChPos.v );
	/* NG for tabs and variable width fonts...
	if ( (*pE).caretChPos.h >= (*pE).position.h + (*pE).hSpan )
		deltaH = (*pE).caretChPos.h - (*pE).position.h - (*pE).hSpan + 1;
	else if ( (*pE).caretChPos.h < (*pE).position.h )
		deltaH = (*pE).caretChPos.h - (*pE).position.h;
	*/
	if ( (*pE).caretRect.right > (*pE).viewRect.right )
		deltaH = ( (*pE).caretRect.right - (*pE).viewRect.right + (*pE).hScale - 1 ) / (*pE).hScale;
	else if ( (*pE).caretRect.left <= (*pE).viewRect.left ) /* was: (*pE).caretRect.right  22Jul92  e  */
	{	if( (*pE).caretRect.right + (*pE).hOrigin < (*pE).width )						/* 22Jul92  e  */
			deltaH = - (*pE).hOrigin;													/* 22Jul92  e  */
		else																			/* 22Jul92  e  */
			deltaH = (*pE).caretRect.right;
		deltaH = ( deltaH - (*pE).viewRect.left - (*pE).hScale + 1 ) / (*pE).hScale;
	}
	if ( deltaH || deltaV )
	{	eTeScroll( hE, deltaH, deltaV, TRUE );
		eTeAdjustScrollMax( hE );
		eTeCalibrate( hE );
	}
}

/* scrolling (Panorama) */

/* 30Apr92  e  -- eTeScroll rewritten for >32K pixel document height */

void eTeScroll( eRec **hE, short hDelta, short vDelta, Boolean redraw )
{
	long			hPixels;
	long			vPixels;
	Rect			tempRect;
	register eRec	*pE;

	hPixels = (long )hDelta * (**hE).hScale;
	vPixels = (long )vDelta * (**hE).vScale;

#if 0					/* 25Jan93  e  - to get rid of annoying inappropriate validation */
	if (redraw) {
		eTePrepare( hE );
		tempRect = (**hE).viewRect;
		if( vPixels < 32767 && vPixels > -32767 )
		{	ScrollRect( &tempRect, (short )-hPixels, (short )-vPixels, gUtilRgn );
			InvalRgn( gUtilRgn );
		}
		else InvalRect( &tempRect );
	}
#else
	if (redraw)
	{	eTePrepare( hE );
		tempRect = (**hE).viewRect;
		if( vPixels < (**hE).height && vPixels > -(**hE).height )
		{	if ( ! EmptyRgn( ((WindowPeek)(**hE).macPort)->updateRgn ) )
			{	BeginUpdate( (**hE).macPort );
				eTeUpdate( hE );
				EndUpdate( (**hE).macPort );
			}
			ScrollRect( &tempRect, (short )-hPixels, (short )-vPixels, gUtilRgn );
			InvalRgn( gUtilRgn );
		}
		else InvalRect( &tempRect );
	}
#endif

	pE = *hE;
	
	(*pE).position.h += hDelta;
	(*pE).position.v += vDelta;

	(*pE).hOrigin += hPixels;
	(*pE).vOrigin += vPixels;
	
	(*pE).maxRight	-= hPixels;
	(*pE).caretRect.left -= hPixels;
	(*pE).caretRect.right -= hPixels;
	(*pE).caretRect.top = eTeChPosVToPointV( hE, (*pE).caretChPos.v );
	(*pE).caretRect.bottom = (*pE).caretRect.top + (*pE).caretHeight;

	if (redraw) {
		BeginUpdate( (*pE).macPort );
		eTeUpdate( hE );
		EndUpdate( (**hE).macPort );
	}
}

void eTeScrollTo( eRec **hE, ChPos aPosition, Boolean redraw )
{
	eTeScroll( hE, aPosition.h - (**hE).position.h, aPosition.v - (**hE).position.v, redraw );
	eTeAdjustScrollMax( hE );
	eTeCalibrate( hE );
}

static Boolean eTeAutoScroll( eRec **hE, Point mouseLoc )
{
	short		hDelta = 0;
	short		vDelta = 0;

	if ( mouseLoc.h < (**hE).viewRect.left ) {
		hDelta = Max( -(**hE).hStep, -(**hE).position.h );
		if (hDelta > 0) {
			hDelta = 0;
		}
	} else if ( mouseLoc.h > (**hE).viewRect.right ) {
		hDelta = Min( (**hE).hStep, (**hE).bounds.h - (**hE).position.h - (**hE).hSpan );
		if (hDelta < 0) {
			hDelta = 0;
		}
	}
	
	if ( mouseLoc.v < (**hE).viewRect.top ) {
		vDelta = Max( -(**hE).vStep, -(**hE).position.v );
		if (vDelta > 0) {
			vDelta = 0;
		}
	} else if ( mouseLoc.v >=
					/* (**hE).viewRect.bottom */
					(**hE).viewRect.top + (**hE).vSpan * (**hE).vScale
					 ) {
		vDelta = Min( (**hE).vStep, (**hE).bounds.v - (**hE).position.v - (**hE).vSpan );
		if (vDelta < 0) {
			vDelta = 0;
		}
	}
	
	if ( (hDelta != 0) || (vDelta != 0) ) {
		eTeScroll( hE, hDelta, vDelta, TRUE );
		eTeCalibrate( hE );
		eTePrepare( hE );
		return(TRUE);
	} else {
		return(FALSE);
	}
}

static void eSbPrepare( eRec **hE )
{
	SetPort( (**hE).macPort );
	ClipRect( &(**hE).macPort->portRect );
}

static void eTeAdjustScrollMax( eRec **hE )
{
	short			hSpan;
	short			vSpan;
	
	eSbPrepare( hE );

	(**hE).hSpan = hSpan = (**hE).width / (**hE).hScale;
	(**hE).vSpan = vSpan = (**hE).height / (**hE).vScale;
	
	if ( (**hE).hSBar != NULL ) {
		SetCtlMax( (**hE).hSBar, Max( ( (**hE).bounds.h - hSpan ), (**hE).position.h ) );
	}

	if ( (**hE).vSBar != NULL ) {
		SetCtlMax( (**hE).vSBar, Max( ( (**hE).bounds.v - vSpan ), (**hE).position.v ) );
	}
}

static void eTeCalibrate( eRec **hE )
{
	eSbPrepare( hE );

	if ( (**hE).hSBar != NULL ) {
		SetCtlValue( (**hE).hSBar, (**hE).position.h );
	}
	if ( (**hE).vSBar != NULL ) {
		SetCtlValue( (**hE).vSBar, (**hE).position.v );
	}
}

static short countKeys()
{	KeyMap km;
	register short count = 1;
	register short i = 4;
	register long kmi;
	
	GetKeys( km );
	while( i-- )
	{	kmi = km[i];
		while( kmi )
		{	count <<= 1;
			kmi &= (kmi-1);
		}
	}
	return count;
}

static void eTeDoHscroll( eRec **hE, short whichPart )
{
	register short		delta;			/* Number of pixels to scroll		*/
	short				oldValue;		/* Current scroll bar setting		*/
	register short		minmax;			/* Minimum or Maximum delta			*/
	long				ticks;			/* Tick count at end of Delay		*/

	switch (whichPart) {
		case inUpButton:
			delta = -( (**hE).hStep * countKeys() );
			break;
		case inDownButton:
			delta =  ( (**hE).hStep * countKeys() );
			break;
		case inPageUp:
			Delay(PAGE_DELAY, &ticks);
			delta = (**hE).hOverlap - (**hE).hSpan;
			break;
		case inPageDown:
			Delay(PAGE_DELAY, &ticks);
			delta = (**hE).hSpan - (**hE).hOverlap;
			break;
	}
	oldValue = GetCtlValue( (**hE).hSBar );
	if (delta < 0) {
		minmax = GetCtlMin( (**hE).hSBar ) - oldValue;
		if (delta < minmax)
			delta = minmax;
	} else {
		minmax = GetCtlMax( (**hE).hSBar ) - oldValue;
		if (delta > minmax)
			delta = minmax;
	}
	if (delta != 0) {
		eSbPrepare( hE );
		SetCtlValue( (**hE).hSBar, oldValue + delta );
		eTeScroll( hE, delta, 0, TRUE );
		eSbPrepare( hE );
	}
}
	
static void eTeDoVscroll( eRec **hE, short whichPart )
{
	register short		delta;			/* Number of pixels to scroll		*/
	short				oldValue;		/* Current scroll bar setting		*/
	register short		minmax;			/* Minimum or Maximum delta			*/
	long				ticks;			/* Tick count at end of Delay		*/

	switch (whichPart) {
		case inUpButton:
			delta = -( (**hE).vStep * countKeys() );
			break;
		case inDownButton:
			delta =  ( (**hE).vStep * countKeys() );
			break;
		case inPageUp:
			Delay(PAGE_DELAY, &ticks);
			delta = (**hE).vOverlap - (**hE).vSpan;
			break;
		case inPageDown:
			Delay(PAGE_DELAY, &ticks);
			delta = (**hE).vSpan - (**hE).vOverlap;
			break;
	}
	oldValue = GetCtlValue( (**hE).vSBar );
	if (delta < 0) {
		minmax = GetCtlMin( (**hE).vSBar ) - oldValue;
		if (delta < minmax)
			delta = minmax;
	} else {
		minmax = GetCtlMax( (**hE).vSBar ) - oldValue;
		if (delta > minmax)
			delta = minmax;
	}
	if (delta != 0) {
		eSbPrepare( hE );
		SetCtlValue( (**hE).vSBar, oldValue + delta );
		eTeScroll( hE, 0, delta, TRUE );
		eSbPrepare( hE );
	}
}
	
/* called continuously while mouse down within a fixed part of a scroll bar.
   Called by the Toolbox during the TrackControl trap. */

static pascal void hSBarActionProc( ControlHandle macControl, short whichPart )
{
	eRec	**hE; 
	
	if (whichPart != 0) {
		hE = (eRec **)GetCRefCon( macControl );
		eTeDoHscroll( hE, whichPart );
	}
}

static pascal void vSBarActionProc( ControlHandle macControl, short whichPart )
{
	eRec	**hE; 
	
	if (whichPart != 0) {
		hE = (eRec **) GetCRefCon( macControl );
		eTeDoVscroll( hE, whichPart );
	}
}

/* end of os_mac_eEdit.c */