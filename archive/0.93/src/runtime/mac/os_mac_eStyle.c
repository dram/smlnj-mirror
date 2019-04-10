/* os_mac_eStyle.c
 * 2Jun92  e
 */

#include <MacHeaders>

/*
#include <Dialogs.h>
#include <Controls.h>
#include <QuickDraw.h>
#include <Windows.h>
#include <ToolUtils.h>
#include <OSUtils.h>
#include <Menus.h>
#include <Fonts.h>
#include <resources.h>
*/

#include "os_mac.h"

/* extensions to an editor for text files by e
   questions/comments via Internet <e@Flavors.COM>
*/
/* Copyright ╘ e 1992. All rights reserved.
	Developed using THINK C 5.0.1 for use with Gambit Scheme.
	This code may be freely distributed as long as this notice remains.
*/

/* to do:
06Jun92  e
consider checkbox to modify OK to:
- Set Style for All Windows
consider showing text in style
- (let ((the 3) (quick 5) (brown 7)) (fox jumps over the lazy dog))
- ((lambda (the quick brown) (fox jumps over (the lazy) dog)) 3 5 7)
consider a way to save Window's style in the file
consider a way to save Creation style (preferences)
add a Use Outline Fonts checkbox
*/

#define MAXFONTSIZE 1023

/* resources */

#define esdID		357

/* dialog items */

#define esdOK		 1
#define esdCancel	 2
#define esdTabsEt	 3
#define esdTabsSt	 4
#define esdFont1St	 5
#define esdFont1Pu	 6
#define esdSize1St	 7
#define esdSize1Et	 8
#define esdBold1Cb	 9
#define esdItal1Cb	10
#define esdCond1Cb	11
#define esdUndr1Cb	12
#define esdInvr1Cb	13
#define esdExpd1Cb	14
#define esdFont2St	15
#define esdFont2Pu	16
#define esdSize2St	17
#define esdSize2Et	18
#define esdBold2Cb	19
#define esdItal2Cb	20
#define esdCond2Cb	21
#define esdUndr2Cb	22
#define esdInvr2Cb	23
#define esdExpd2Cb	24
#define esdPS1St	25
#define esdPS2St	26
#define esdSize1Pu	27
#define esdSize2Pu	28
#define esdOutline1	29
#define esdOutline2	30
#define esdOKring	31
#define esdThisBt	32
#define esdPrefBt	33
#define esdDfltBt	34
#define esdGetSt	35
#define esdOutline3	36
#define esdApplSt	37
#define esdThisCb	38
#define esdPrefCb	39
#define esdWrapEt	40
#define esdAutoInd	42

/* the pop-up font item */

/* See: Apple Macintosh Developer Technical Support
        Pop-up Menu Example Application
        PopMenus.p
*/

static TextStyle tsNewPri, tsNewAlt;
static short tabsNew;
static short wrapNew;
static short autoIndNew;

#define popMenuID1 128
static MenuHandle popMenu1;
static Rect  popUpBox1;		/* boundsrect of font popUp1 */
static Rect  promptBox1;	/* boundsrect of its title */
static short fontChoice1;	/* its last chosen item */

#define popMenuID2 129
static MenuHandle popMenu2;
static Rect  popUpBox2;		/* boundsrect of font popUp2 */
static Rect  promptBox2;	/* boundsrect of its title */
static short fontChoice2;	/* its last chosen item */

#define popMenuID3 130
static MenuHandle popMenu3;
static Rect  popUpBox3;		/* boundsrect of font popUp2 */
static short sizeChoice3;	/* its last chosen item */

#define popMenuID4 131
static MenuHandle popMenu4;
static Rect  popUpBox4;		/* boundsrect of font popUp2 */
static short sizeChoice4;	/* its last chosen item */

/* some user item action procs -- used in update events mostly */

#define leftSlop 15
#define rightSlop 17
#define botSlop 5

static void drawPopArrow( Rect r )
{ 
  FrameRect( &r );
  MoveTo( r.right, r.top+2 );
  LineTo( r.right, r.bottom );
  LineTo( r.left+2, r.bottom );
  MoveTo( r.right-17, r.top+6 );
  Line( 11, 0 );
  Move( -10, 1 );
  Line( 9, 0 );
  Move( -8, 1);
  Line( 7, 0 );
  Move( -6, 1);
  Line( 5, 0 );
  Move( -4, 1 );
  Line( 3, 0 );
  Move( -2, 1 );
  Line( 1, 0 );
}

static pascal void DrawPopUpSz( DialogPtr dwind, short dinum )
{ short itemtype;
  Handle itemhandle;
  Rect r;
  GetDItem( dwind, dinum, &itemtype, &itemhandle, &r );
  InsetRect( &r, -1, -1 );
  drawPopArrow( r );
}

static void DrawPopUpX( MenuHandle popMenu, Rect* popUpBox, short lastChoice )
{ Rect r;
  Str255 curFont;
  short newWid, newLen, wid;

  GetItem( popMenu, lastChoice, curFont ); /* get currently-selected item */
  r = *popUpBox;
  EraseRect( &r );
  InsetRect( &r, -1, -1 );
  /* Make sure the title fits. Truncate it and add an ellipses (рис) otherwise */
  wid = (r.right - r.left) - (leftSlop + rightSlop);
  newWid = StringWidth( curFont );
  if( newWid > wid )
  { newLen = curFont[0]; /* current length in characters */
	wid = wid - CharWidth('и');
	do
	{ newWid = newWid - CharWidth( curFont[newLen--] );
	} while( ( newWid > wid ) && ( newLen > 0 ) );
	curFont[++newLen] = 'и';
	curFont[0] = newLen;
  }
  drawPopArrow( r );
  MoveTo( r.left+leftSlop, r.bottom-botSlop);
  DrawString( curFont );
}

static pascal void DrawPopUp1( DialogPtr theDialog, short theItem )
{ DrawPopUpX( popMenu1, &popUpBox1, fontChoice1 );
}

static pascal void DrawPopUp2( DialogPtr theDialog, short theItem )
{ DrawPopUpX( popMenu2, &popUpBox2, fontChoice2 );
}

/* BorderDefault draws a heavy border around the default ( OK ) button */
static pascal void BorderDefault( WindowPtr dwind, short dinum )
{ short itemtype;
  Handle itemhandle;
  Rect borderRect;
  GetDItem( dwind, esdOK, &itemtype, &itemhandle, &borderRect );
  InsetRect( &borderRect, -4, -4 );
  PenSize( 3, 3 );
  FrameRoundRect( &borderRect, 16, 16 );
  PenSize( 1, 1 );
}

/* Outliner draws a light border around the the user item */
static pascal void Outliner( WindowPtr dwind, short dinum )
{ short itemtype;
  Handle itemhandle;
  Rect borderRect;
  GetDItem( dwind, dinum, &itemtype, &itemhandle, &borderRect );
  /* InsetRect( &borderRect, -4, -4 ); */
  PenSize( 1, 1 );
  FrameRoundRect( &borderRect, 16, 16 );
}

/* util fcns. */

/* cursor to IBeam if it's over an active edit line, arrow if it's not */
void IBeamIt( WindowPtr dwind )
{ Point mouseAt;
  short itemtype;
  Handle itemhandle;
  Rect borderRect;
  short itemNum;
  /* first get the current edit line out of the dialog record */
  itemNum = ((DialogPeek )dwind)->editField + 1; /* always stored 1 less */
  GetDItem( dwind, itemNum, &itemtype, &itemhandle, &borderRect );
  GetMouse( &mouseAt );
  if( PtInRect( mouseAt, &borderRect ) )
    SetCursor(&ibeam_cursor);
  else
    SetCursor(&arrow);
}

static void pseudoClick( DialogPtr dbox, short item )
{ short itemtype;
  Rect itemrect;
  Handle hC;
  long tilticks;
  GetDItem( dbox, item, &itemtype, &hC, &itemrect );
  HiliteControl( (ControlHandle)hC, inButton );
  Delay( 8, &tilticks );
  HiliteControl( (ControlHandle)hC, false );
}

static short apndMenu( MenuHandle hM, Str255 title, short itemno, Boolean check, Boolean realfont )
{
  AppendMenu( hM, title );
  if( realfont ) SetItemStyle( hM, itemno, outline );
  if( check )
  { SetItemMark( hM, itemno, checkMark );
    return itemno;
  }
  return 0;
}

static MenuHandle sizeMenu( short id, short *ckitem, short fNum, short curSize )
{ MenuHandle hM;
  Str255 oddSize;
  short match = 0;
  hM = NewMenu( id, "" );
  match |= apndMenu( hM,  "\p9", 1, (curSize ==  9), RealFont( fNum,  9 ) );
  match |= apndMenu( hM, "\p10", 2, (curSize == 10), RealFont( fNum, 10 ) );
  match |= apndMenu( hM, "\p12", 3, (curSize == 12), RealFont( fNum, 12 ) );
  match |= apndMenu( hM, "\p14", 4, (curSize == 14), RealFont( fNum, 14 ) );
  match |= apndMenu( hM, "\p18", 5, (curSize == 18), RealFont( fNum, 18 ) );
  match |= apndMenu( hM, "\p24", 6, (curSize == 24), RealFont( fNum, 24 ) );
  if( match == 0 )
  { NumToString( (long )curSize, oddSize );
    InsMenuItem( hM, oddSize, 0 );
    InsMenuItem( hM, "\p(-", 1 );
    SetItemMark( hM, 1, checkMark );
    if( RealFont( fNum, curSize ) ) SetItemStyle( hM, 1, outline );
    *ckitem = 1;
  }
  else *ckitem = match;
  return( hM );
}

static short doPop( DialogPtr esd, MenuHandle popMenu, Rect popUpBox, short id, short oldChoice )
{ long chosen;
  Point popLoc;
  short newChoice;
  InsertMenu( popMenu, -1 );
  popLoc = topLeft(popUpBox);
  LocalToGlobal( &popLoc );
  CalcMenuSize( popMenu ); /* Work around Menu Mgr bug */
  chosen = PopUpMenuSelect( popMenu, popLoc.v, popLoc.h, oldChoice );
  DeleteMenu( id );
  if( chosen != 0 )
  { newChoice = LoWord(chosen);
	if( newChoice != oldChoice )
	{ SetItemMark( popMenu, oldChoice, ' ' );
	  SetItemMark( popMenu, newChoice, checkMark );
	  EraseRect( &popUpBox );
	  return( newChoice );
	}
  }
  return 0;
}

/* filter proc */

#define kReturnKey 0x0D
#define kEnterKey 0x03
#define kEscKey 0x1B
#define kTabKey 9
enum  {
    kBackSpace = 8,
    kLeftArrow = 0x1C, kRightArrow, kUpArrow, kDownArrow,
    kDeleteKey = 0x7F
};

/* size menu selection => edit text box */
static void doSize1( DialogPtr esd )
{ Rect tempRect;
  short tempItem;
  Handle tempHandle;
  long tmp;
  Str255 tStr;
  GetDItem( esd, esdSize1Et, &tempItem, &tempHandle, &tempRect );
  GetItem( popMenu3, sizeChoice3, tStr );
  SetIText( tempHandle, tStr );
  SelIText( esd, esdSize1Et, 0, 32767 );
  StringToNum( tStr, &tmp);
  tsNewPri.tsSize = tmp;
}

/* size menu selection => edit text box */
static void doSize2( DialogPtr esd )
{ Rect tempRect;
  short tempItem;
  Handle tempHandle;
  long tmp;
  Str255 tStr;
  GetDItem( esd, esdSize2Et, &tempItem, &tempHandle, &tempRect );
  GetItem( popMenu4, sizeChoice4, tStr );
  SetIText( tempHandle, tStr );
  SelIText( esd, esdSize2Et, 0, 32767 );
  StringToNum( tStr, &tmp);
  tsNewAlt.tsSize = tmp;
}

/* font menu selection triggers update of size menu */
static void doFont1( void )
{ Str255 tStr;
  GetItem( popMenu1, fontChoice1, tStr );
  GetFNum( tStr, &tsNewPri.tsFont );
  if( popMenu3 ) DisposHandle( popMenu3 );
  popMenu3 = sizeMenu( popMenuID3, &sizeChoice3, tsNewPri.tsFont, tsNewPri.tsSize );
}

/* font menu selection triggers update of size menu */
static void doFont2( void )
{ Str255 tStr;
  GetItem( popMenu2, fontChoice2, tStr );
  GetFNum( tStr, &tsNewAlt.tsFont );
  if( popMenu4 ) DisposHandle( popMenu4 );
  popMenu4 = sizeMenu( popMenuID4, &sizeChoice4, tsNewAlt.tsFont, tsNewAlt.tsSize );
}

/* this function always returns a value between 1 and MAXFONTSIZE inclusive */
/* an out of range number returns 8;
   this is so font sizes and tab stops are reasonable */
static long etToNum( DialogPtr dbox, short item )
{ short tempItem;
  Rect tempRect;
  Handle tempHandle;
  long tmp;
  Str255 tStr;
  GetDItem( dbox, item, &tempItem, &tempHandle, &tempRect );
  GetIText( tempHandle, tStr );
  StringToNum( tStr, &tmp);
  if( tmp > MAXFONTSIZE || tmp < 1 ) tmp = 8;
  return tmp;
}

/* update size menu if size edit text box changed */
static void preSize1( DialogPtr esd )
{ long tmp;
  tmp = etToNum( esd, esdSize1Et );
  if( tsNewPri.tsSize != tmp )
  { tsNewPri.tsSize = tmp;
    if( popMenu3 ) DisposHandle( popMenu3 );
    popMenu3 = sizeMenu( popMenuID3, &sizeChoice3, tsNewPri.tsFont, tsNewPri.tsSize );
  }
}

/* update size menu if size edit text box changed */
static void preSize2( DialogPtr esd )
{ long tmp;
  tmp = etToNum( esd, esdSize2Et );
  if( tsNewAlt.tsSize != tmp )
  { tsNewAlt.tsSize = tmp;
    if( popMenu4 ) DisposHandle( popMenu4 );
    popMenu4 = sizeMenu( popMenuID4, &sizeChoice4, tsNewAlt.tsFont, tsNewAlt.tsSize );
  }
}

pascal Boolean filterIt( DialogPtr esd, EventRecord *myDialogEvent, short *theDialogItem)
{ WindowPtr temp;
  char theKey;
  Rect tempRect;
  short tempItem;
  Handle tempHandle;
  Boolean result = false;

  GetPort( &temp );
  SetPort( esd );
    
  IBeamIt( esd ); /* make IBeam cursor when over the active edit line */
    
  /* do standard filtering for escape and return as OK and Cancel aliases */
  /* invert the button in the dialog, so the user get's visual feedback */

  if ((myDialogEvent->what == keyDown) || (myDialogEvent->what == autoKey))
  { theKey = myDialogEvent->message & charCodeMask;
    if( (myDialogEvent->modifiers & (cmdKey|controlKey|optionKey)) == 0 )
    { switch( theKey )
      { case kReturnKey:
        case kEnterKey:
  		  *theDialogItem = esdOK;
  		  pseudoClick( esd, esdOK );
          SetPort(temp);
          result = true;
          break;
        case kEscKey:
          *theDialogItem = esdCancel;
  		pseudoClick( esd, esdCancel );
          SetPort(temp);
          result = true;
          break;
        case kTabKey:
        case kBackSpace:
        case kLeftArrow:
        case kRightArrow:
        case kUpArrow:
        case kDownArrow:
        case kDeleteKey:
  		  break;
        default:
          /* filter out non-numeric keys */
          if( theKey < 0x30 || theKey > 0x39 )
          { SysBeep( 1 );
            SetPort( temp );
            result = true;
          }
          break;
      }
    }
  }
  else if( myDialogEvent->what == mouseDown )
  { Point mouseLoc;
    short newChoice;
    mouseLoc = ( myDialogEvent->where );
    GlobalToLocal( &mouseLoc );
	switch( newChoice = FindDItem( esd, mouseLoc ) + 1 )
	{ case esdFont1St:
      case esdFont1Pu:
    	InvertRect( &promptBox1 );
    	newChoice = doPop( esd, popMenu1, popUpBox1, popMenuID1, fontChoice1 );
		InvertRect( &promptBox1 );
		if( newChoice != 0 )
		{ fontChoice1 = newChoice;
		  DrawPopUp1( esd, esdFont1Pu );
		  *theDialogItem = esdFont1Pu;
          SetPort( temp );
          result = true;
	    }
	    break;
	  case esdFont2St:
      case esdFont2Pu:
    	InvertRect( &promptBox2 );
    	newChoice = doPop( esd, popMenu2, popUpBox2, popMenuID2, fontChoice2 );
		InvertRect( &promptBox2 );
		if( newChoice != 0 )
		{ fontChoice2 = newChoice;
		  DrawPopUp2( esd, esdFont2Pu );
		  *theDialogItem = esdFont2Pu;
          SetPort( temp );
          result = true;
	    }
	    break;
	  /* case esdSize2St:  not for size menu!? */
      case esdSize1Pu:
  		SelIText( esd, esdSize1Et, 0, 32767 );
  		preSize1( esd );
    	newChoice = doPop( esd, popMenu3, popUpBox3, popMenuID3, sizeChoice3 );
		if( newChoice != 0 )
		{ sizeChoice3 = newChoice;
		  DrawPopUpSz( esd, esdSize1Pu );
		  *theDialogItem = esdSize1Pu;
          SetPort( temp );
          result = true;
	    }
	    break;
	  /* case esdSize2St:  not for size menu!? */
      case esdSize2Pu:
  		SelIText( esd, esdSize2Et, 0, 32767 );
  		preSize2( esd );
    	newChoice = doPop( esd, popMenu4, popUpBox4, popMenuID4, sizeChoice4 );
		if( newChoice != 0 )
		{ sizeChoice4 = newChoice;
		  DrawPopUpSz( esd, esdSize2Pu );
		  *theDialogItem = esdSize2Pu;
          SetPort( temp );
          result = true;
	    }
	    break;
	}
  }
  return( result );
}

static void check( DialogPtr dbox, short item, Boolean mark )
{ ControlHandle hC;
  short itemtype;
  Rect itemrect;    
  GetDItem( dbox, item, &itemtype, (Handle)&hC, &itemrect);
  SetCtlValue( hC, mark );
}

static void checks( DialogPtr dbox, short style, short offs )
{
  check( dbox, esdBold1Cb + offs, ( style & bold ) ? 1 : 0 );
  check( dbox, esdItal1Cb + offs, ( style & italic ) ? 1 : 0 );
  check( dbox, esdUndr1Cb + offs, ( style & underline ) ? 1 : 0 );
  check( dbox, esdCond1Cb + offs, ( style & condense ) ? 1 : 0 );
  check( dbox, esdExpd1Cb + offs, ( style & extend ) ? 1 : 0 );
  /*
  check( dbox, esdInvr1Cb + offs, ( style & bold ) ? 1 : 0 );
  */
}

short checklist[] = { bold, italic, condense, underline, 0x80, extend };

static short findFontItem( MenuHandle hM, short fontNum )
{ short result = 1;
  short i, fn;
  Str255 tStr;
  for( i = CountMItems( hM ); i > 0; i-- )
  { GetItem( hM, i, tStr );
    GetFNum( tStr, &fn );
    if( fn == fontNum ) return i;
    if( fn == 0 ) result = i;
  }
  return result;
}

/* get the item's rect & set the procPtr */
static void initUserItem( DialogPtr esd, short item, Rect *pRect, ProcPtr pProc )
{ short tempItem;
  Handle tempHandle;
  GetDItem( esd, item, &tempItem, &tempHandle, pRect );
  SetDItem( esd, item, tempItem, (Handle)pProc, pRect );
}

static void hosePops( void )
{
  if( popMenu1 ) DisposHandle( popMenu1 );
  popMenu1 = 0;
  if( popMenu2 ) DisposHandle( popMenu2 );
  popMenu2 = 0;
  if( popMenu3 ) DisposHandle( popMenu3 );
  popMenu3 = 0;
  if( popMenu4 ) DisposHandle( popMenu4 );
  popMenu4 = 0;
}

static void initPops( DialogPtr esd )
{ Str255 tStr;
  short tempItem;
  Rect tempRect;
  Handle tempHandle;

  hosePops();
  
  SetPort( esd );
  
  popMenu1 = NewMenu( popMenuID1, "" );
  AddResMenu( popMenu1, 'FONT' );
  fontChoice1 = findFontItem( popMenu1, tsNewPri.tsFont );
  SetItemMark( popMenu1, fontChoice1, checkMark );
  InvalRect( &popUpBox1 );
	
  popMenu2 = NewMenu( popMenuID2, "" );
  AddResMenu( popMenu2, 'FONT' );
  fontChoice2 = findFontItem( popMenu2, tsNewAlt.tsFont );
  SetItemMark( popMenu2, fontChoice2, checkMark );
  InvalRect( &popUpBox2 );
  
  popMenu3 = sizeMenu( popMenuID3, &sizeChoice3, tsNewPri.tsFont, tsNewPri.tsSize );
  doSize1( esd );

  popMenu4 = sizeMenu( popMenuID4, &sizeChoice4, tsNewAlt.tsFont, tsNewAlt.tsSize );
  doSize2( esd );
  
  checks( esd, tsNewPri.tsFace,  0 );
  checks( esd, tsNewAlt.tsFace, 10 );
  
  NumToString( (long )tabsNew, tStr );
  GetDItem( esd, esdTabsEt, &tempItem, &tempHandle, &tempRect );
  SetIText( tempHandle, tStr );
  SelIText( esd, esdTabsEt, 0, 32767 );
  
  NumToString( (long )wrapNew, tStr );
  GetDItem( esd, esdWrapEt, &tempItem, &tempHandle, &tempRect );
  SetIText( tempHandle, tStr );
  SelIText( esd, esdWrapEt, 0, 32767 );

  check( esd, esdAutoInd, autoIndNew );
}  

void eStyleDlg( eRec **hE /* , TextStyle *pri, TextStyle *alt */ )
{ DialogPtr esd = NULL;
  short hitItem = 0;
  short tempItem;
  Rect tempRect;
  Handle tempHandle;
  Boolean tBool;
  long tmp1, tmp2, tmp3;
  
  tsNewPri = (**hE).style[0];
  tsNewAlt = (**hE).style[1];
  tabsNew = (**hE).tabStops;
  wrapNew = (**hE).wrap;
  autoIndNew = (**hE).autoInd;

  /* Get a dialog box, and set up our useritems */
  esd = GetNewDialog( esdID, NULL, (WindowPtr)(-1) );
		
  /* Find out where our useritems are, and set their item handles to be
	 a pointer to our popup-drawing or other special procedure */
  GetDItem( esd, esdFont1St, &tempItem, &tempHandle, &promptBox1 ); /* get the title rect */
  initUserItem( esd, esdFont1Pu, &popUpBox1, DrawPopUp1 );
  GetDItem( esd, esdFont2St, &tempItem, &tempHandle, &promptBox2 ); /* get the title rect */
  initUserItem( esd, esdFont2Pu, &popUpBox2, DrawPopUp2 );
  initUserItem( esd, esdSize1Pu, &popUpBox3, DrawPopUpSz );
  initUserItem( esd, esdSize2Pu, &popUpBox4, DrawPopUpSz );
  initUserItem( esd, esdOutline1, &tempRect, Outliner );
  initUserItem( esd, esdOutline2, &tempRect, Outliner );
  initUserItem( esd, esdOutline3, &tempRect, Outliner );

  GetDItem( esd, esdOK,&tempItem, &tempHandle, &tempRect ); /* get the OK button's rect */
  InsetRect( &tempRect, -4, -4);
  SetDItem( esd, esdOKring, userItem+itemDisable, (Handle)BorderDefault, &tempRect);

  /* Get two menus containing the current set of fonts */
  /* and two menus containing their corresponding sizes */
  initPops( esd );
  
  check( esd, esdThisCb, 1 );
  check( esd, esdPrefCb, 0 );	/* defaulted to off  --  16Jul92  e  */
	
  ShowWindow( (WindowPtr)esd );
  DrawDialog( esd );
  do
  { ModalDialog( (ModalFilterProcPtr)filterIt, &hitItem );
    switch ( hitItem )
    { case esdFont1Pu:
    	doFont1( );
        break;
      case esdFont2Pu:
    	doFont2( );
        break;
      case esdSize1Pu:
  		doSize1( esd );
        break;
      case esdSize2Pu:
  		doSize2( esd );
        break;
      case esdBold1Cb:
      case esdItal1Cb:
      case esdCond1Cb:
      case esdUndr1Cb:
      case esdInvr1Cb:
      case esdExpd1Cb:
		GetDItem( esd, hitItem, &tempItem, &tempHandle, &tempRect);
		tempItem = GetCtlValue( (ControlHandle )tempHandle ) ^ 1;
		if( tempItem == 0 )
		  tsNewPri.tsFace &= ~checklist[hitItem-esdBold1Cb];
		else
		  tsNewPri.tsFace |=  checklist[hitItem-esdBold1Cb];
		SetCtlValue( (ControlHandle )tempHandle, tempItem);
        break;
      case esdBold2Cb:
      case esdItal2Cb:
      case esdCond2Cb:
      case esdUndr2Cb:
      case esdInvr2Cb:
      case esdExpd2Cb:
		GetDItem( esd, hitItem, &tempItem, &tempHandle, &tempRect);
		tempItem = GetCtlValue( (ControlHandle )tempHandle ) ^ 1;
		if( tempItem == 0 )
		  tsNewAlt.tsFace &= ~checklist[hitItem-esdBold2Cb];
		else
		  tsNewAlt.tsFace |=  checklist[hitItem-esdBold2Cb];
		SetCtlValue( (ControlHandle )tempHandle, tempItem);
        break;
      case esdThisCb:
      case esdPrefCb:
      case esdAutoInd:
		GetDItem( esd, hitItem, &tempItem, &tempHandle, &tempRect);
		tempItem = GetCtlValue( (ControlHandle )tempHandle ) ^ 1;
		SetCtlValue( (ControlHandle )tempHandle, tempItem);
        break;
      case esdThisBt:
		tsNewPri = (**hE).style[0];
		tsNewAlt = (**hE).style[1];
		tabsNew =  (**hE).tabStops;
		wrapNew =  (**hE).wrap;
		autoIndNew = (**hE).autoInd;
		initPops( esd );
        break;
      case esdPrefBt:
		tsNewPri = prefStylNormal;
		tsNewAlt = prefStylHilite;
		tabsNew  = prefTabs;
		wrapNew  = prefWrap;
		autoIndNew = prefAutoInd;
		initPops( esd );
        break;
      case esdDfltBt:
		tsNewPri = dfltStylNormal;
		tsNewAlt = dfltStylHilite;
		tabsNew  = dfltTabs;
		wrapNew  = dfltWrap;
		autoIndNew = dfltAutoInd;
		initPops( esd );
        break;
    }
  } while ( hitItem != esdOK && hitItem != esdCancel );
  
  if( hitItem == esdOK )
  { tsNewPri.tsSize = etToNum( esd, esdSize1Et );
    tsNewAlt.tsSize = etToNum( esd, esdSize2Et );
    tmp1 = etToNum( esd, esdTabsEt );
    tmp2 = etToNum( esd, esdWrapEt );
	GetDItem( esd, esdAutoInd, &tempItem, &tempHandle, &tempRect);
    tmp3 = GetCtlValue( (ControlHandle )tempHandle );
	GetDItem( esd, esdThisCb, &tempItem, &tempHandle, &tempRect);
    if( GetCtlValue( (ControlHandle )tempHandle ) )
    { if( (**hE).tabStops != tmp1 )
	  { eTeSetTabStop( hE, (short )tmp1 );
	  }
	  if( (**hE).wrap != tmp2 )
	  { eTeSetWrap( hE, (short )tmp2 );
	  }
	  (**hE).autoInd = tmp3;
	  if(    tsNewPri.tsFont != (**hE).style[0].tsFont
		  || tsNewPri.tsSize != (**hE).style[0].tsSize
		  || tsNewPri.tsFace != (**hE).style[0].tsFace
		  || tsNewAlt.tsFont != (**hE).style[1].tsFont
		  || tsNewAlt.tsSize != (**hE).style[1].tsSize
		  || tsNewAlt.tsFace != (**hE).style[1].tsFace
		)
	  { eTeSetStyles( hE, &tsNewPri, &tsNewAlt );
	  }
    }
	GetDItem( esd, esdPrefCb, &tempItem, &tempHandle, &tempRect);
    if( GetCtlValue( (ControlHandle )tempHandle ) )
    { prefStylNormal = tsNewPri;
      prefStylHilite = tsNewAlt;
      prefTabs    = (short )tmp1;
      prefWrap    = (short )tmp2;
      prefAutoInd = (short )tmp3;
      savePrefs( PREFS_FILENAME );  /*  16Jul92  e  */
    }
  }
  hosePops();
  if( esd ) DisposDialog( esd );
}

/* end of os_mac_eStyle.c */
