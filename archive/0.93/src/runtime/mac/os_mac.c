/* os_mac.c
 * 12Sep92  e  adapted from the file of the same name in the Gambit Project
 * Macintosh specific stuff (for THINK C 5.0.1 compiler).
 * portions Copyright © 1992 Marc Feeley and Douglas H. Currie, Jr.
 */

#include <MacHeaders>

#include "os_mac.h"
#include <stdio.h>
#include <errno.h>
#include <signal.h>

#define use_MacTraps2 (1)

/*---------------------------------------------------------------------------*/

/* new stuff  29Dec92  e  */

#include <GestaltEqu.h>
#include <AppleEvents.h>

/* 1Feb93  e */
#ifdef THINK_C
#undef kAERestart
#undef kAEShutDown
#endif
#include <AERegistry.h>

static void init_ae();

Boolean		gDoQuit = false;
Boolean		gHasAppleEvents = false;
Boolean		gPrintPage = false;
Boolean		spareB1;

#include "eventchk.h"		/* 25Jan93  e */
long next_eventchk_ticks;	/* 25Jan93  e */

/*---------------------------------------------------------------------------*/

TextStyle prefStylNormal =
{ monaco, 0, 0, FontSize, { 65536, 65536, 65536 } /* RGBColor Black */ };
TextStyle prefStylHilite =
{ monaco, 0, 0, FontSize, { 65536, 65536, 65536 } /* RGBColor Black */ };

short prefTabs = dfltTabs;
short prefWrap = dfltWrap;
short prefAutoInd = dfltAutoInd;

short current_menus;

MenuHandle menus[7];		/* 10Jan93  e */

Cursor watch_cursor;
Cursor gc_cursor;
Cursor ibeam_cursor;
Cursor *current_cursor;

unsigned char *find_string, *replace_string;

Boolean find_ci   = FALSE;
Boolean find_wrap = FALSE;

static Boolean smKeyBdP;
static Boolean showPosnP;	/* 10Jan93  e */

short interaction_id = 0;
short abnormal_exit = 0;
short interrupted = 0;
short current_volume = 0;

static struct {
  short in_use;
  short bold_input;
  WindowPtr wptr;
  ControlHandle vscroll;
  ControlHandle hscroll;
  TeHANDLE hTE;
  short height;
  short dirty;
  short pos, len;
  char *buf;
  Handle out_buf;
  short out_len;
  short is_file;
  unsigned char *filename;
  long needs_inval_ticks;			/* 10Jan93  e --  display of caret position */
  } wind_table[MAX_NB_WINDOWS];

static void io_err( short io );

#if 0

static struct
{ char   *a5;
  VBLTask task;
} et;

static void intr_task()
{ asm
  { move.l a5,-(sp)
    move.l -4(a0),a5
    
    cmpa.l 0x904,a5
    beq.s @goon
    _Debugger
goon:
  }
  if (intr_task_interval > 0)
  { timer_action( 0L, 0L, 1L );
    et.task.vblCount = intr_task_interval;
  }
  else
    et.task.vblCount = 1;
  asm
  { move.l (sp)+,a5
  }
}

static void start_intr_task()
{ asm
  { move.l a5,et.a5
  }
  et.task.qType    = vType;
  et.task.vblAddr  = (ProcPtr)intr_task;
  et.task.vblCount = 1;
  et.task.vblPhase = 0;
  intr_task_installed = (VInstall( (QElemPtr )&et.task ) == noErr);
}

static void stop_intr_task()
{ if (intr_task_installed)
  { VRemove( (QElemPtr )&et.task ); intr_task_installed = 0; }
}

#endif

RgnHandle gCursorRgn;
Boolean gHasWNE = true;
Boolean gHasHWPriv = true;
Boolean gCursRgnOK = false;
Boolean gInBackground = false;
long gWaitTicksBG = 5; /* in ticks,  should be 1..15 */
long gWaitTicksFG = 1; /* in ticks,  should be 1..15 */
/** Define trap numbers **/
#ifndef _Unimplemented
#define _Unimplemented 0xA89F /* Unimplemented trap */
#endif
#ifndef _WaitNextEvent
#define _WaitNextEvent 0xA860 /* WaitNextEvent trap */
#endif
#ifndef _HWPriv
#define _HWPriv 0xA098 /* HWPriv trap */
#endif

TrapType GetTrapType(short theTrap)
{ /* OS traps start with A0, Tool with A8 or AA. */
  if ((theTrap & 0x0800) == 0)
    return (OSTrap);
  else
    return (ToolTrap);
}
short NumToolboxTraps(void)
{ /* InitGraf (trap $A86E) is always implemented. */
  if (NGetTrapAddress(0xA86E, ToolTrap) == NGetTrapAddress(0xAA6E, ToolTrap))
    return (0x200);
  else
    return (0x400);
}
Boolean TrapExists(short theTrap)
{ TrapType theTrapType;
  theTrapType = GetTrapType(theTrap);
  if ((theTrapType == ToolTrap) && ((theTrap &= 0x07FF) >= NumToolboxTraps()))
    theTrap = _Unimplemented;
  return (NGetTrapAddress(_Unimplemented, ToolTrap) != NGetTrapAddress(theTrap, theTrapType));
}

static void setup_cursors()
{ CursHandle hCurs;
  hCurs = GetCursor(watchCursor);
  watch_cursor = **hCurs;
  /* 28Jan93  e
  hCurs = GetCursor(gc_cursorID);
  gc_cursor = **hCurs;
  */
  hCurs = GetCursor(iBeamCursor);
  ibeam_cursor = **hCurs;

  current_cursor = &watch_cursor;
}

/* 02Oct92  e  added ParamText1 */

static void ParamText1( Str255 text )
{
	ParamText( text, "\p", "\p", "\p" );
}

static void c_to_p( c_str, p_str )
char *c_str;
Str255 p_str;
{ short i = 0;
  while (c_str[i] != '\0') { p_str[i+1] = c_str[i]; i++; }
  p_str[0] = i;
}

static void p_to_p( p1, p2 )
Str255 p1, p2;
{ short len = (unsigned char)*p1++;
  *p2++ = len;
  while (len>0) { *p2++ = *p1++; len--; }
}

static void buf_to_p( unsigned short len, char *p1, Str255 p2 )
{ if( len > 255 ) len = 255;
  *p2++ = len;
  while (len>0) { *p2++ = *p1++; len--; }
}

static void p_to_c( p_str, c_str )
Str255 p_str;
char *c_str;
{ short len = (unsigned char)*p_str++, i = 0;
  while (i<len) { *c_str++ = *p_str++; i++; }
  *c_str++ = '\0';
}

static short compare_p_to_p( p1, p2 )
Str255 p1, p2;
{ short len1 = (unsigned char)*p1++, len2 = (unsigned char)*p2++;
  if (len1 < len2) return -1;
  if (len1 > len2) return 1;
  while (len1>0)
  { short c1 = (unsigned char)*p1++, c2 = (unsigned char)*p2++;
    if (c1 < c2) return -1;
    if (c1 > c2) return 1;
    len1--;
  }
  return 0;
}

static void pathstr_to_filename( register Str255 p_str, Str255 f_str )
{	register short plen = p_str[0];
	while( plen && p_str[plen] != ':' ) plen--;
	buf_to_p( p_str[0] - plen, (char *)&p_str[plen+1], f_str );
}

/* *** preferences saved to disk *** */

#ifdef use_MacTraps2

static OSErr findPrefFolder( short *foundVRefNum, long *foundDirID )
{    
  *foundVRefNum = 0;
  *foundDirID = 0;
  /* MacTraps2 has FindFolder() glue */
  return FindFolder( kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
                       foundVRefNum, foundDirID );
}

#else

#include <GestaltEqu.h>

#define BTstQ(arg, bitnbr)        (arg & (1 << bitnbr))

static OSErr findPrefFolder( short *foundVRefNum, long *foundDirID )
{
  long          gesResponse;
  SysEnvRec     envRec;
  WDPBRec       myWDPB;
  unsigned char volName[34];
  OSErr         err;
    
  *foundVRefNum = 0;
  *foundDirID = 0;
  if ( !Gestalt( gestaltFindFolderAttr, &gesResponse ) 
  		&& BTstQ( gesResponse, gestaltFindFolderPresent ) )
  { /* Folder Manager exists */
    err = FindFolder( kOnSystemDisk, kPreferencesFolderType, kCreateFolder,
                       foundVRefNum, foundDirID );
  }
  else
  { /* Gestalt can't give us the answer, so we resort to SysEnvirons */
    if ( !(err = SysEnvirons( curSysEnvVers, &envRec )) )
    {  myWDPB.ioVRefNum = envRec.sysVRefNum;
       volName[0] = '\000';                   /* Zero volume name */
       myWDPB.ioNamePtr = volName;
       myWDPB.ioWDIndex = 0;
       myWDPB.ioWDProcID = 0;
       if ( !(err = PBGetWDInfo( &myWDPB, 0 )) )
       { *foundVRefNum = myWDPB.ioWDVRefNum;
         *foundDirID = myWDPB.ioWDDirID;
       }
    }
  }
  return (err);
}

#endif

struct prefs
{ short version;
  short tabs;
  short wrap;
  Boolean find_ci;
  Boolean find_wrap;
  Boolean spare0;		/* smKeyBdP in Gambit */
  Boolean showPosnP;	/* 10Jan93  e  */
  short spare2;
  short spare3;
  short autoInd;
  TextStyle stylNormal;
  TextStyle stylHilite;
  unsigned char fontnmNormal[64];
  unsigned char fontnmHilite[64];
};

void savePrefs( ConstStr255Param fn )
{
	short rRef;
  	OSErr err;
	struct prefs **hRes;
	struct prefs  *pRes;
	short foundVRefNum;
	long  foundDirID;
	Str255 fontnmNormal, fontnmHilite;
	
	err = findPrefFolder( &foundVRefNum, &foundDirID );
	if( err != noErr ) return;
	
	if( ( rRef = HOpenResFile( foundVRefNum, foundDirID, fn, fsRdWrPerm ) ) < 0)
	{	HCreateResFile( foundVRefNum, foundDirID, fn );
		{ HFileInfo pb;
		  pb.ioCompletion = (void *)0;
		  pb.ioNamePtr = (StringPtr )fn;
		  pb.ioVRefNum = foundVRefNum;
		  pb.ioFDirIndex = 0;
		  pb.ioDirID = foundDirID;
		  err = PBHGetFInfo( &pb, FALSE );
		  pb.ioCompletion = (void *)0;
		  pb.ioNamePtr = (StringPtr )fn;
		  pb.ioVRefNum = foundVRefNum;
		  pb.ioFDirIndex = 0;
		  pb.ioDirID = foundDirID;
		  pb.ioFlFndrInfo.fdCreator = 'NJML'; /* 14Dec92  e  */
		  pb.ioFlFndrInfo.fdType    = 'gamP';
		  if( err == noErr )
		    PBHSetFInfo( &pb, FALSE );
		}
		rRef = HOpenResFile( foundVRefNum, foundDirID, fn, fsRdWrPerm );
	}
	if( rRef < 0 )
		return;
	hRes = (struct prefs **)GetResource('ePrf', 357);
	if(	hRes == 0 || rRef != HomeResFile( (Handle )hRes ) )
	{	hRes = (struct prefs **)NewHandle( sizeof( struct prefs ) );
		if( hRes == 0 )
		{	CloseResFile( rRef );
			return;
		}
		AddResource( (Handle )hRes, (ResType )'ePrf', 357, (ConstStr255Param)"\p" );
		if( ResErr != noErr )
		{	DisposHandle( (Handle )hRes );
			CloseResFile( rRef );
			return;
		}
	}
	pRes = *hRes;
	(*pRes).version    = 1;
	(*pRes).tabs       = prefTabs;
	(*pRes).wrap       = prefWrap;
	(*pRes).find_ci    = find_ci;
	(*pRes).find_wrap  = find_wrap;
	(*pRes).spare0     = 0;
	(*pRes).showPosnP  = showPosnP;		/* 10Jan93  e  */
	(*pRes).spare2     = 0;
	(*pRes).spare3     = 0;
	(*pRes).autoInd    = prefAutoInd;
	(*pRes).stylNormal = prefStylNormal;
	(*pRes).stylHilite = prefStylHilite;
	GetFontName( prefStylNormal.tsFont, fontnmNormal );
	if( fontnmNormal[0] > 63 ) fontnmNormal[0] = 63;
	p_to_p( fontnmNormal, (**hRes).fontnmNormal );
	GetFontName( prefStylHilite.tsFont, fontnmHilite );
	if( fontnmHilite[0] > 63 ) fontnmHilite[0] = 63;
	p_to_p( fontnmHilite, (**hRes).fontnmHilite );
	
	ChangedResource( (Handle )hRes );
	CloseResFile( rRef );
}

static void readPrefs( ConstStr255Param fn )
{
	short rRef;
  	OSErr err;
	struct prefs **hRes;
	struct prefs  *pRes;
	short foundVRefNum;
	long  foundDirID;
	short	fontNumber;

	err = findPrefFolder( &foundVRefNum, &foundDirID );
	if( err != noErr ) return;
	
	rRef = HOpenResFile( foundVRefNum, foundDirID, fn, fsRdWrPerm );
	/* don't test rRef here since resource may come from elsewhere, e.g., from appl. */
	hRes = (struct prefs **)GetResource('ePrf', 357);
	if(	hRes != 0 )
	{	pRes = *hRes;
		if( (*pRes).version == 1 )
		{	prefTabs   = (*pRes).tabs;
			prefWrap   = (*pRes).wrap;
			find_ci    = (*pRes).find_ci;
			find_wrap  = (*pRes).find_wrap;
			prefAutoInd = (*pRes).autoInd;
			showPosnP  = (*pRes).showPosnP;	/* 10Jan93  e  */
			prefStylNormal = (*pRes).stylNormal;
			prefStylHilite = (*pRes).stylHilite;
			GetFNum( (**hRes).fontnmNormal, &prefStylNormal.tsFont );
			if( prefStylNormal.tsFont < 0 ) prefStylNormal.tsFont = 0;
			GetFNum( (**hRes).fontnmHilite, &prefStylHilite.tsFont );
			if( prefStylHilite.tsFont < 0 ) prefStylHilite.tsFont = 0;
		}
	}
	if( rRef >= 0 ) CloseResFile( rRef );
}

/* *** */

#define fustItem 3
static short itemtowindid[MAX_NB_WINDOWS];
static short windidtodigit[MAX_NB_WINDOWS];
static short qWindItems = 0;

typedef struct { short nbmenus6, right, dummy; struct { MenuHandle mh; short left; } menu[100]; } menu_list;

static short cmd_key_exists( c )
char c;
{
  menu_list **ml = (menu_list **)MenuList;
  short n = (*ml)->nbmenus6/6;
  short i, j, m, cmd;
  for (i=0; i<n; i++)
  { MenuHandle mh = (*ml)->menu[i].mh;
    m = CountMItems( mh );
    for (j=1; j <= m; j++)
    {
      GetItemCmd( mh, j, &cmd );
      if (cmd == c) return 1;
    }
  }
  return 0;
}

static void addWindMenuItem( short id )
{	short d, i, j;
	Str255 title, key = "\p / ";
	WindowPtr wind = wind_table[id].wptr;
	if (qWindItems >= MAX_NB_WINDOWS) return;
	for (d=1; d<10; d++) if (!cmd_key_exists('0'+d)) break;
	if (d < 10) key[3] = '0'+d; else key[2] = ' ';
	windidtodigit[id] = d;
	for (i=0; i < qWindItems; i++)
	  if (windidtodigit[itemtowindid[i]] > d) break;
	for (j=qWindItems-1; j > i; j--)
	  itemtowindid[j] = itemtowindid[j-1];
	itemtowindid[i] = id;
	GetWTitle( wind, title );
	InsMenuItem( menus[windowsM], key, i+fustItem-1 );
	SetItem( menus[windowsM], i+fustItem, title );
	qWindItems++;
}

static void delWindMenuItem( short id )
{	register short i;
	for(i = 0; i < qWindItems; i++)
	{	if( id == itemtowindid[i] )
		{	DelMenuItem( menus[windowsM], i+fustItem );
			for( i++; i < qWindItems; i++ )
				itemtowindid[i-1] = itemtowindid[i];
			qWindItems--;
			break;
		}
	}
}

void eSmudgeWindow( register short w )
{	register short i;
	if( ! wind_table[w].dirty )
	{	wind_table[w].dirty = 1;
		for( i = 0; i < qWindItems; i++ )
		{	if( w == itemtowindid[i] )
			{	SetItemMark( menus[windowsM], i+fustItem, '×');
				break;
			}
		}
	}
}

void eUnSmudgeWindow( register short w, TeHANDLE hTE )
{	register short i;
	(**hTE).dirty = 0;
	if( wind_table[w].dirty )
	{	wind_table[w].dirty = 0;
		for( i = 0; i < qWindItems; i++ )
		{	if( w == itemtowindid[i] )
			{	SetItemMark( menus[windowsM], i+fustItem, 0);
				break;
			}
		}
	}
}

void eMaybeSmudgeWindow( short w, TeHANDLE hTE )
{
	if( (**hTE).dirty )
		eSmudgeWindow( w );
	else
		eUnSmudgeWindow( w, hTE );	/* for Undo  14Aug92  e  */
}

void select_and_show( wptr )
WindowPtr wptr;
{
  SelectWindow( wptr );
  ShowWindow( wptr );
}

/* ************* */

/* 10Jan93  e  -- separated out Search menu */

static void wind_begin()
{ short i;
  for (i=0; i<MAX_NB_WINDOWS; i++) wind_table[i].in_use = 0;
  InitGraf( &thePort );
  InitFonts();
  FlushEvents( everyEvent, 0 );
  InitWindows();
  InitMenus();
  TEInit();
  eTeInit();
  InitDialogs( 0L );
  InitCursor();

  prefStylHilite.tsFace = bold+condense;

  menus[appleM] = GetMenu( appleID );
  AddResMenu( menus[appleM], 'DRVR' );
  menus[fileM]    = GetMenu( fileID );
  menus[editM]    = GetMenu( editID );
  menus[findM]    = GetMenu( findID );
  menus[commandM] = GetMenu( commandID );
  menus[windowsM] = GetMenu( windowsID );

  ClearMenuBar();
  for ( (i=appleM); (i<=windowsM); i++ ) InsertMenu(menus[i], 0);
  DrawMenuBar();
  current_menus = 0;

  setup_cursors();

  find_string =    (unsigned char *)NewPtr( 256 );
  replace_string = (unsigned char *)NewPtr( 256 );
  if( find_string == NULL || replace_string == NULL )
  {	SysBeep(10);
  	os_quit();
  }
  find_string[0] = '\0';
  replace_string[0] = '\0';

  gCursorRgn = NewRgn();
  SetRectRgn(gCursorRgn, -32768, -32768, 32766, 32766);
  gHasWNE = TrapExists(_WaitNextEvent);

  gHasHWPriv = TrapExists(_HWPriv);

  readPrefs( PREFS_FILENAME );

  init_ae();	/* 29Dec92  e  */
}

static void wind_close( id )
short id;
{ TeHANDLE hTE = wind_table[id].hTE;
  if (id == interaction_id)
  { HideWindow( wind_table[id].wptr );
    eTeSetSelect( hTE, 0, eTeTextLength( hTE ));
    eTeDelete( hTE );
  }
  else if (wind_table[id].in_use)
  { wind_table[id].in_use = 0;
    HideWindow( wind_table[id].wptr );
    eTeDispose( hTE );
	delWindMenuItem( id );
    DisposeControl( wind_table[id].vscroll );
    DisposeControl( wind_table[id].hscroll );
    DisposeWindow( wind_table[id].wptr );
    if (wind_table[id].buf != NULL)
    { DisposPtr( wind_table[id].buf ); wind_table[id].buf = NULL; }
    if( wind_table[id].out_buf != NULL )
    { DisposHandle( wind_table[id].out_buf ); wind_table[id].out_buf = NULL; }
    if( wind_table[id].filename != NULL )
    { DisposPtr( wind_table[id].filename ); wind_table[id].filename = NULL; }
  }
}

static void wind_end()
{ short id;
  for (id=MAX_NB_WINDOWS-1; id>=0; id--) wind_close( id );
  eTePutScrap();	/* 27Jan93  e */
}

static pascal click_scroll();

static short wind_open( name, visible, goaway, bold_input )
char *name;
short visible, goaway, bold_input;
{ Rect viewRect, vScrollRect, bounds;
  short width = 80 /*(screenBits.bounds.right-SBarWidth-2*Border-11)/FontW*/;
  short height;
  short id;
  Str255 wname;

  for (id=0; id<MAX_NB_WINDOWS; id++) if (!wind_table[id].in_use) break;
  if (id == MAX_NB_WINDOWS) return -1;

  height = (screenBits.bounds.bottom-Border-48-SBarWidth)/FontH - id;

  bounds.left   = 3 + id*5;
  bounds.right  = bounds.left + width*FontW + SBarWidth + Border;
  bounds.top    = 41 + id*(FontH+3);
  bounds.bottom = bounds.top + height*FontH + SBarWidth + Border;

  c_to_p( name, wname );

  wind_table[id].wptr = (WindowPtr )
    NewWindow( NULL, &bounds, wname, visible, documentProc, (WindowPtr)-1L, goaway, 0L );
  if (wind_table[id].wptr == NULL) return -1;

  SetPort( wind_table[id].wptr );
  TextFont( prefStylNormal.tsFont );
  TextSize( prefStylNormal.tsSize );

  vScrollRect = (*wind_table[id].wptr).portRect;
  vScrollRect.left = vScrollRect.right-SBarWidth;
  vScrollRect.right += 1;
  vScrollRect.bottom -= SBarWidth-1;
  vScrollRect.top -= 1;

  wind_table[id].vscroll =
    NewControl( wind_table[id].wptr, &vScrollRect, "\p", 1, 0, 0, 0,
                scrollBarProc, 0L);

  if (wind_table[id].vscroll != NULL)
  { static FontInfo fInfo;

    vScrollRect = (*wind_table[id].wptr).portRect;
    vScrollRect.top = vScrollRect.bottom-SBarWidth;
    vScrollRect.bottom += 1;
    vScrollRect.left += (vScrollRect.right-vScrollRect.left)>>1;
    vScrollRect.right -= SBarWidth-1;
    wind_table[id].hscroll =
      NewControl( wind_table[id].wptr, &vScrollRect, "\p", 1, 0, 0, 0,
                scrollBarProc, 0L);
    if (wind_table[id].hscroll != NULL)
    {

      viewRect = (*wind_table[id].wptr).portRect;
	  viewRect.top    += Border;
	  viewRect.left   += Border;
	  viewRect.bottom -= SBarWidth;
	  viewRect.right  -= SBarWidth;

      wind_table[id].hTE = eTeNew( wind_table[id].wptr, viewRect, prefTabs, prefWrap,
    								  prefAutoInd, wind_table[id].hscroll, wind_table[id].vscroll );

      if (wind_table[id].hTE != NULL)
      {
        eTeSetStyles( wind_table[id].hTE, &prefStylNormal, &prefStylHilite );
        wind_table[id].height     = height;
        wind_table[id].bold_input = bold_input;
        wind_table[id].dirty      = 0;
        wind_table[id].pos        = 0;
        wind_table[id].len        = 0;
        wind_table[id].buf        = NULL;
        wind_table[id].out_buf    = NULL;
        wind_table[id].out_len    = 0;
        wind_table[id].is_file    = 0;
        wind_table[id].filename   = (unsigned char *)NewPtr( FILENAME_LEN );
        if( wind_table[id].filename != NULL )
        { wind_table[id].filename[0] = '\0';
          wind_table[id].in_use   = 1;
	      addWindMenuItem( id );
          return id;
        }
    	eTeDispose( wind_table[id].hTE );
      }
	  DisposeControl( wind_table[id].hscroll );
    }
    DisposeControl( wind_table[id].vscroll );
  }
  DisposeWindow( wind_table[id].wptr );
  return -1;
}

static short discard_changes( id )
short id;
{ Str255 title;
  GetWTitle( wind_table[id].wptr, title );
  ParamText( "\pDiscard changes to \"", title, "\p\"?", "\p" );
  SysBeep( 10 );
  switch (CautionAlert( ok_cancel_alertID, 0L ))
  { case 3: return 1;
    default: return 0;
  }
}

static short mem_full = 0;
static short supress_mem_full_dialog = 0;		/* 27Jan93  e  */

pascal long mem_full_err( size )
long size;
{ if (mem_full)
  { ParamText1( "\pOut of memory.  The application will exit." );
    SysBeep( 10 );
    StopAlert( ok_alertID, 0L );
    ExitToShell();
  }
  mem_full = 1;
  if ( ! supress_mem_full_dialog )			/* 27Jan93  e  */
  { ParamText1( "\pOut of memory." );
    SysBeep( 10 );
    StopAlert( ok_alertID, 0L );
  }
  return 0;
}

static void already_open_err( name )
Str255 name;
{ ParamText( "\p\"", name, "\p\"", "\pis already open.  Close it first." );
  SysBeep( 10 );
  StopAlert( ok_alertID, 0L );
}

static void path_too_long_err( name )
Str255 name;
{ ParamText( "\pPath to file \"", name, "\p\" is too long.", "\p" );
  SysBeep( 10 );
  StopAlert( ok_alertID, 0L );
}

static void wind_err()
{ ParamText1( "\pCan't open window" );
  SysBeep( 10 );
  StopAlert( ok_alertID, 0L );
}

/* 02Oct92  e  added Printer Not Found check, error number displayed for other errors */

static void io_err( short io )
{ unsigned char errnostr[16];
  switch (io)
  { case wrPermErr    : ParamText1( "\pCan't write file!" ); break;
    case dupFNErr     : ParamText1( "\pDuplicate file name!" ); break;
    case fBsyErr      : ParamText1( "\pFile is busy!" ); break;
    case vLckdErr     : ParamText1( "\pVolume is locked!" ); break;
    case fLckdErr     : ParamText1( "\pFile is locked!" ); break;
    case fnfErr       : ParamText1( "\pFile not found!" ); break;
    case bdNamErr     : ParamText1( "\pBad filename!" ); break;
    case ioErr        : ParamText1( "\pIO transfer error!" ); break;
    case dskFulErr    : ParamText1( "\pDisk full!" ); break;
    case dirFulErr    : ParamText1( "\pDirectory full!" ); break;
    case mFulErr	  : ParamText1( "\pFile is too large for memory!" ); break;
    					
    case -4101		  : ParamText1( "\pNo printer selected. Use the Chooser." ); break;
    default           : NumToString( io, errnostr );
    					ParamText( "\pIO Error! Mac error number: ", errnostr, "\p", "\p" );
    					break;
  }
  SysBeep( 10 );
  StopAlert( ok_alertID, 0L );
}

static void te_limit_err()
{ ParamText1( "\pFile unopened due to size limit on buffer (32000 lines)." );
  SysBeep( 10 );
  StopAlert( ok_alertID, 0L );
}

static short check_TEWrite( ptr, len, hTE, bold )
char *ptr;
long len;
eTeHandle hTE;
short bold;
{	eTeWrite( hTE, ptr, len, bold );
	if (mem_full) { mem_full = 0; return 1; }
	return 0;
}

/* 23Jul92  e  exported for eKeys */
short check_TEInsert( ptr, len, hTE, bold )
char *ptr;
long len;
eTeHandle hTE;
short bold;
{	eTeInsert( hTE, ptr, len, bold );
	if (mem_full) { mem_full = 0; return 1; }
	return 0;
}

static short check_TECut( hTE )
eTeHandle hTE;
{ eTeCut( hTE );
  if (mem_full) { mem_full = 0; return 1; }
  return 0;
}

static short check_TECopy( hTE )
eTeHandle hTE;
{ eTeCopy( hTE );
  if (mem_full) { mem_full = 0; return 1; }
  return 0;
}

static short check_TEPaste( hTE, bold )
eTeHandle hTE;
short bold;
{ 	eTePaste( hTE, bold );
	if (mem_full) { mem_full = 0; return 1; }
	return 0;
}

static short check_TEKey( c, hTE, bold, event )
char c;
eTeHandle hTE;
short bold;
EventRecord *event;
{	eTeKey( hTE, c, (event->message & keyCodeMask) >> 8, event->modifiers, bold );
	if (mem_full) { mem_full = 0; return 1; }
	return 0;
}

short getfullpath( vRefNum, dirID, fName, Path, MaxLength, warn )
short vRefNum;
long dirID;
Str255 fName;
char *Path;
short MaxLength;
short warn;
{
  char *p, *q;
  long len;
  CInfoPBRec block;
  Str255 directoryName;

  p = &Path[MaxLength];
  *--p = '\0';

  len = (unsigned char)fName[0];
  p -= len;
  if (p < Path) goto too_long;
  BlockMove( &fName[1], p, len );

  block.dirInfo.ioNamePtr = directoryName;
  block.dirInfo.ioDrParID = dirID;

  do
  { block.dirInfo.ioVRefNum = vRefNum;
    block.dirInfo.ioFDirIndex = -1;
    block.dirInfo.ioDrDirID = block.dirInfo.ioDrParID;

    if (PBGetCatInfo( &block, FALSE ) != noErr) return 0;
    *--p = ':';
    len = (unsigned char)directoryName[0];
    p -= len;
    if (p < Path) goto too_long;
    BlockMove( &directoryName[1], p, len );
  } while ( block.dirInfo.ioDrDirID != fsRtDirID );

  q = Path;
  while (*p != '\0') *q++ = *p++;
  *q = '\0';

  return 1;

  too_long:
  if (warn) path_too_long_err( fName );
  return 0;
}

/* 3Jul92  e  so I could get rid of MacTraps2 */
#ifdef use_MacTraps2
short getfullpathfromwd( wdRefNum, fName, Path, MaxLength, warn )
long wdRefNum;
Str255 fName;
char *Path;
short MaxLength;
short warn;
{
  short vRefNum;
  long dirID, procID;

  if (GetWDInfo( wdRefNum, &vRefNum, &dirID, &procID ) != noErr) return 0;

  return getfullpath( vRefNum, dirID, fName, Path, MaxLength, warn );
}
#else
/* pascal OSErr GetWDInfo(short wdRefNum,short *vRefNum,long *dirID,long *procID) */
short getfullpathfromwd( wdRefNum, fName, Path, MaxLength, warn )
long wdRefNum;
Str255 fName;
char *Path;
short MaxLength;
short warn;
{
  WDPBRec myBlock;

  myBlock.ioNamePtr = nil;
  myBlock.ioVRefNum = wdRefNum;
  myBlock.ioWDIndex = 0;
  myBlock.ioWDProcID = 0;
  PBGetWDInfo(&myBlock,false);

  if ( myBlock.ioResult != noErr ) return 0;

  return getfullpath( myBlock.ioWDVRefNum, myBlock.ioWDDirID, fName, Path, MaxLength, warn );
}
#endif

short getfullpathfromcurrentvolume( fName, Path, MaxLength, warn )
Str255 fName;
char *Path;
short MaxLength;
short warn;
{ return getfullpathfromwd( (long)current_volume, fName, Path, MaxLength, warn );
}

static Point SFGwhere = { 90, 82 };
static Point SFPwhere = { 106, 104 };

short get_file( prompt, nbtypes, ftypes, path, maxlength )
Str255 prompt;
short nbtypes;
char *ftypes;
char *path;
short maxlength;
{
  SFReply reply;

  if (nbtypes == 0) nbtypes = -1;

  eTePutScrap();	/* 10Jan93  e */
  SFGetFile( SFGwhere, prompt, 0L, nbtypes, (SFTypeList *)ftypes, 0L, &reply );
  eTeGetScrap();	/* 10Jan93  e */
  if (!reply.good) return 0;

  current_volume = reply.vRefNum;
  return getfullpathfromwd( (long)current_volume, (StringPtr)reply.fName, path, maxlength, 1 );
}

short put_file( prompt, deflt, path, maxlength )
Str255 prompt;
Str255 deflt;
char *path;
short maxlength;
{
  SFReply reply;

  eTePutScrap();	/* 10Jan93  e */
  SFPutFile( SFPwhere, prompt, deflt, 0L, &reply );
  eTeGetScrap();	/* 10Jan93  e */
  if (!reply.good) return 0;

  current_volume = reply.vRefNum;
  return getfullpathfromwd( (long)current_volume, (StringPtr)reply.fName, path, maxlength, 1 );
}

static short wptr_to_id( w )
WindowPtr w;
{ short id;
  for (id=0; id<MAX_NB_WINDOWS; id++)
    if ((wind_table[id].in_use) && (wind_table[id].wptr == (WindowPtr )w)) return id;
  return -1;
}

static void setup_view( id )
short id;
{ WindowPtr w = wind_table[id].wptr;
  TeHANDLE hTE = wind_table[id].hTE;
  Rect view = w->portRect;
  /*
  view.right -= SBarWidth;
  view.bottom -= SBarWidth;
  InsetRect(&view, Border, Border);				/*  21Jul92  e  */
  view.top    += Border;
  view.left   += Border;
  view.bottom -= SBarWidth;
  view.right  -= SBarWidth;
  eTeNewView( hTE, &view );
}

/* 10Jan93  e -- added display of caret position */
#if 0
static void content( short id, EventRecord *event )
{ WindowPtr w = wind_table[id].wptr;
  eTeHandle hTE = wind_table[id].hTE;
  short cntlCode;
  ControlHandle theControl;

  SetPort( w );
  ClipRect( &w->portRect );
  GlobalToLocal( &event->where );
  if ((cntlCode = FindControl( event->where, w, &theControl )) == 0 )
  { if (PtInRect( event->where, &(**hTE).viewRect ))
      eTeClick( hTE, event->where, event->modifiers, event->when );
  }
  else if (cntlCode == inThumb)
  { cntlCode = GetCtlValue( theControl );
    TrackControl( theControl, event->where, 0L );
	if ( ( cntlCode = GetCtlValue( theControl ) - cntlCode ) != 0 )
		if( (**theControl).contrlRect.left < (**theControl).contrlRect.top ) /* horizontal? */
			eTeScroll( hTE, cntlCode, 0, TRUE );
		else
			eTeScroll( hTE, 0, cntlCode, TRUE );
  }
  else
    TrackControl( theControl, event->where, (ProcPtr )-1L );
}
#else
static void really_inval_msgrect( short w )
{ GrafPtr port = wind_table[w].wptr;
  Rect r = port->portRect;
  r.right -= (( r.right - r.left ) >> 1 ) + SBarWidth + 1;
  r.top = r.bottom - SBarWidth + 1;
  SetPort( port );
  InvalRect( &r );
}
static void inval_msgrect( short w )
{ if ( showPosnP ) wind_table[w].needs_inval_ticks = TickCount();
}
static void content( short id, EventRecord *event )
{ WindowPtr w = wind_table[id].wptr;
  eTeHandle hTE = wind_table[id].hTE;
  short cntlCode;
  ControlHandle theControl;
  ChPos oldCaret;

  SetPort( w );
  ClipRect( &w->portRect );
  GlobalToLocal( &event->where );
  if ((cntlCode = FindControl( event->where, w, &theControl )) == 0 )
  { if (PtInRect( event->where, &(**hTE).viewRect ))
    { oldCaret = (**hTE).caretChPos;
      eTeClick( hTE, event->where, event->modifiers, event->when );
      if ( *(long *)&oldCaret != *(long *)&(**hTE).caretChPos ) inval_msgrect( id );
    }
  }
  else if (cntlCode == inThumb)
  { cntlCode = GetCtlValue( theControl );
    TrackControl( theControl, event->where, 0L );
	if ( ( cntlCode = GetCtlValue( theControl ) - cntlCode ) != 0 )
		if( (**theControl).contrlRect.left < (**theControl).contrlRect.top ) /* horizontal? */
			eTeScroll( hTE, cntlCode, 0, TRUE );
		else
			eTeScroll( hTE, 0, cntlCode, TRUE );
  }
  else
    TrackControl( theControl, event->where, (ProcPtr )-1L );
}
#endif
/* 10Jan93  e -- added display of caret position */
#if 0
static void update_window( short w )
{ GrafPtr port = wind_table[w].wptr;
  Rect r = port->portRect;
#ifdef USE_eTe_debug
  Str255 tStr;
#endif
  r.right -= (( r.right - r.left ) >> 1 ) + SBarWidth + 1;
  r.top = r.bottom - SBarWidth;
  SetPort( port );
  ClipRect( &port->portRect );	/* added for eTe  16Apr92  e  */
  BeginUpdate( port );
  EraseRect( &port->portRect );
  DrawControls( port );
  DrawGrowIcon( port );
  EraseRect( &r );
  MoveTo( r.left, r.top );
  LineTo( r.right, r.top );
#ifdef USE_eTe_debug
  NumToString( eTeTextLength( wind_table[w].hTE ), tStr);
  MoveTo( r.left + 4, r.bottom - 4 );
  ClipRect( &r );
  DrawString( (ConstStr255Param )tStr );
#endif
  eTeUpdate( wind_table[w].hTE );
  EndUpdate( port );
}
#else
static void update_window( short w )
{ GrafPtr port = wind_table[w].wptr;
  Rect r = port->portRect;
  Str255 tStr;
  eTeHandle hTE;
  r.right -= (( r.right - r.left ) >> 1 ) + SBarWidth + 1;
  r.top = r.bottom - SBarWidth;
  SetPort( port );
  ClipRect( &port->portRect );	/* added for eTe  16Apr92  e  */
  BeginUpdate( port );
  EraseRect( &port->portRect );
  DrawControls( port );
  DrawGrowIcon( port );
  EraseRect( &r );
  MoveTo( r.left, r.top );
  LineTo( r.right, r.top );
#ifdef USE_eTe_debug
  NumToString( eTeTextLength( wind_table[w].hTE ), tStr);
  MoveTo( r.left + 4, r.bottom - 4 );
  ClipRect( &r );
  DrawString( (ConstStr255Param )tStr );
#else
  if ( showPosnP )
  { hTE = wind_table[w].hTE;
	MoveTo( r.left + 4, r.bottom - 4 );
	ClipRect( &r );
	NumToString( (long )(**hTE).caretChPos.v, tStr);
	DrawString( (ConstStr255Param )tStr );
	DrawString( "\p:" );
	NumToString( (long )(**hTE).caretChPos.h, tStr);
	DrawString( (ConstStr255Param )tStr );
  }
#endif
  eTeUpdate( wind_table[w].hTE );
  EndUpdate( port );
}
#endif

static void grow_window( id, p )
short id;
Point p;
{ WindowPtr w = wind_table[id].wptr;
  long result;
  short oScroll;
  Rect r, oView;
  
  SetPort( w );

  /* 16Sep92  e  increase min size for H scroll bar
  SetRect(&r, 80, 80, screenBits.bounds.right, screenBits.bounds.bottom); */
  /* 13Oct92  e  per: bernard@sigi.cs.colorado.edu
  SetRect(&r, 150, 80, screenBits.bounds.right, screenBits.bounds.bottom); */
  r = (**GetGrayRgn()).rgnBBox;
  r.top = 80;
  r.left = 150;
  /*  13Oct92  e  */
  result = GrowWindow( w, p, &r );
  if (result == 0) return;
  SizeWindow( w, LoWord(result), HiWord(result), 1);

  InvalRect(&w->portRect);
  setup_view( id );
  HideControl( wind_table[id].vscroll );
  MoveControl( wind_table[id].vscroll, w->portRect.right - SBarWidth, w->portRect.top-1);
  SizeControl( wind_table[id].vscroll, SBarWidth+1, w->portRect.bottom - w->portRect.top-(SBarWidth-2));
  HideControl( wind_table[id].hscroll );
  MoveControl( wind_table[id].hscroll,
  				w->portRect.right-((w->portRect.right-w->portRect.left)>>1),
  				w->portRect.bottom - SBarWidth);
  SizeControl( wind_table[id].hscroll,
  				((w->portRect.right-w->portRect.left)>>1)-(SBarWidth-1),
  				SBarWidth+1);
  ShowControl( wind_table[id].hscroll );
  ValidRect(&(*wind_table[id].hscroll)->contrlRect );
  ShowControl( wind_table[id].vscroll );
  ValidRect(&(*wind_table[id].vscroll)->contrlRect );
  update_window( id );
  /* dunno (sometimes appropriate, sometimes not)... eTeShowCaret( wind_table[id].hTE ); */
}

static void put_input( id, ptr, len, cr, flush, freshline )
short id;
char *ptr;
short len;
short cr, flush, freshline;
{ if (wind_table[id].in_use)
  { long len1;
    register long len2 = (wind_table[id].len - wind_table[id].pos);
    register char *buf, *p1, *p2;
    if ((len2 < 0) || flush) len2 = 0;
    len1 = ((cr)?1:0) + len2 + (long)len;
    buf = NewPtr( len1 );
    if (buf == NULL) { SysBeep(10); return; }
    p1 = buf;
    p2 = wind_table[id].buf + wind_table[id].pos;
    while (len2 > 0) { *p1++ = *p2++; len2--; }
    p2 = ptr;
    while (len > 0) { *p1++ = *p2++; len--; }
    if (cr) *p1++ = '\r';
    if (wind_table[id].buf != NULL) DisposPtr( wind_table[id].buf );
    wind_table[id].buf = buf;
    wind_table[id].pos = 0;
    wind_table[id].len = len1;
    if (freshline)
    { eTeHandle hTE = wind_table[id].hTE;
      if ( (**hTE).caretChPos.h > 0 )
        check_TEInsert( "\r", 1L, hTE, 0 );
      (**hTE).writeChPos = (**hTE).caretChPos; /*  9Jul92  e  */
      eTeShowCaret( hTE );
    }
  }
}

static void handle_interrupt()
{ interrupted = 1;
  /* FlushEvents(keyDownMask,0); */
  /* raise(SIGINT); */
  /* errno = EINTR; */
}

static short open_file( filename, for_output, txt, io )
Str255 filename;
short for_output;
short txt;
short *io;
{ ioParam pb;
  fileParam fp;
  short refnum;
  short len = (unsigned char)filename[0];
  short file_existed = 1;

  pb.ioNamePtr = filename;
  pb.ioVRefNum = current_volume;
  pb.ioVersNum = 0;
  pb.ioPermssn = ((for_output) ? fsRdWrPerm : fsRdPerm);
  pb.ioMisc = 0;

  if (for_output)
  { asm
    { lea    pb,a0
      _PBCreate
    }
    file_existed = (pb.ioResult == dupFNErr);
    if ((pb.ioResult != noErr) && !file_existed)
    { *io = pb.ioResult;  return -1; }
  }
  
  asm
  { lea    pb,a0
    _PBOpen
  }
  if (pb.ioResult != noErr)
  { if (for_output)
      asm
      { lea    pb,a0
        _PBDelete
      }
    *io = pb.ioResult;
    return -1;
  }
  refnum = pb.ioRefNum;
  
  if (!for_output) { *io = noErr; return refnum; }

  asm
  { lea    pb,a0
    _PBSetEOF
  }

  if (!file_existed)
  { fp.ioNamePtr = filename;
    fp.ioVRefNum = current_volume;
    fp.ioFVersNum = 0;
    fp.ioFDirIndex = 0;
    asm
    { lea    fp,a0
      _PBGetFInfo
      bmi.s  @1
    }
    if (txt)
      fp.ioFlFndrInfo.fdType = 'TEXT';
    else
      fp.ioFlFndrInfo.fdType = 'DATA';
    fp.ioFlFndrInfo.fdCreator = 'NJML';
    asm
    { lea    fp,a0
      _PBSetFInfo
      @1
    }
  }

  *io = noErr;
  return refnum;
}

static short close_file( refnum, io )
short refnum;
short *io;
{ *io = FSClose( refnum );
  return (*io != noErr);
}

static short read_file( id, line, chr )
short id;
long line;
long chr;
{ TeHANDLE hTE = wind_table[id].hTE;
  short refnum;
  short io, io2;
  long pos = 0;
  short result;
  Rect r;
  short beep = 0;
  short text = 0;
  long size, count;
  ChPos chPos;
  Handle hData;
  
  if ( line > 32768 )
  { te_limit_err();
    return 0;
  }
  chPos.v = (line==0) ? 0 : line-1;
  chPos.h = (line==0) ? 0 : chr;

  SelectWindow( wind_table[id].wptr );

  if (wind_table[id].dirty)
    if (!discard_changes( id )) return 0;

  refnum = open_file( wind_table[id].filename, 0, 1, &io );
  if (refnum == -1)
  { io_err( io );
    return 0;
  }

  eTeSetTextPtr( hTE, (Ptr )&text, 1L );
  io = GetEOF(refnum, &size);
  if( io == noErr )	{
	if( hData = NewHandle( size + 1 ) )
	{	HLock( hData );
		count = size;
		io = FSRead( refnum, &count, *hData );
		HUnlock( hData );
		if( ( io != noErr ) || ( count != size ) )
		{	DisposHandle( hData );
			if ( io == noErr ) io = ioErr;
			/* io_err( io ); will happen */
			/* added these next lines 7Jul92  e */
			mem_full = 0;
  			eTeSetTextPtr( hTE, (Ptr )&text, 1L );
		}
		else
		{	((char *)*hData)[size] = '\0';
			io = eTeSetTextHandleDetabify( hTE, hData, prefTabs );
			if ( io == noErr )
			{   count = eTeChPosToOffset( hTE, chPos );
				if ( count > 0 )
					eTeSetSelect( hTE, count-1, count );
				else
					eTeSetSelect( hTE, count, count );
				eTeShowCaret( hTE );
			}
			else
			{	DisposHandle( hData );
				/* io_err( io ); will happen */
				/* added these next lines 7Jul92  e */
				mem_full = 0;
  				eTeSetTextPtr( hTE, (Ptr )&text, 1L );
			}
	  	}
  	}
  	else
  	{ mem_full = 0; io = mFulErr; }	 /* mem_full = 0; added  7Jul92  e */
  }

  if (close_file( refnum, &io2 ) || ((io != eofErr) && (io != noErr)))
  { if ((io != eofErr) && (io != noErr)) io_err( io ); else io_err( io2 );
	  eSmudgeWindow( id );
      return 0;
  }
  if (io != noErr)
	  eSmudgeWindow( id );
  else
	  eUnSmudgeWindow( id, hTE );
  return (io == noErr);
}

static void write_file( id )
short id;
{ TeHANDLE hTE = wind_table[id].hTE;
  short refnum;
  short io, io2;
  long count;

  refnum = open_file( wind_table[id].filename, 1, 1, &io );
  if (refnum == -1)
  { io_err( io );
    return;
  }

  count = eTeTextLength( hTE );
  io = FSWrite( refnum, &count, *((**hTE).hText) );

  if (close_file( refnum, &io2 ) || (io != noErr))
  { if (io != noErr) io_err( io ); else io_err( io2 );
    return;
  }

  eUnSmudgeWindow( id, hTE );
}

short edit( name, line, chr )
char *name;
long line;
long chr;
{ short id;
  Str255 pname;
  c_to_p( name, pname );
  if (pname[0] != '\0')
  { for (id=0; id<MAX_NB_WINDOWS; id++)
      if (wind_table[id].in_use && wind_table[id].is_file &&
          (compare_p_to_p( wind_table[id].filename, pname ) == 0)) break;
  }
  else
    id = MAX_NB_WINDOWS;

  if (id == MAX_NB_WINDOWS) id = wind_open( name, /* 27Jan93 e -- TRUE */ FALSE, TRUE, FALSE );
  if (id < 0)
    wind_err();
  else
  { wind_table[id].is_file = 1;
    p_to_p( pname, wind_table[id].filename );
    supress_mem_full_dialog = 1;		/* 27Jan93  e  */
	if ( read_file( id, line, chr ) )
		select_and_show( wind_table[id].wptr );
	else
		wind_close( id );
    supress_mem_full_dialog = 0;		/* 27Jan93  e  */
  }
  return id;
}

void save_as( id, path )
short id;
char *path;
{ Str255 ppath;
  short i;
  c_to_p( path, ppath );
  for (i=0; i<MAX_NB_WINDOWS; i++)
    if (wind_table[i].in_use && wind_table[i].is_file &&
        (compare_p_to_p( wind_table[i].filename, ppath ) == 0))
    { already_open_err( ppath );
      return;
    }
  p_to_p( ppath, wind_table[id].filename );
  if (wind_table[id].is_file) SetWTitle( wind_table[id].wptr, ppath );
  if ( id != interaction_id ) /* 28Jan93  e  */
  { delWindMenuItem( id );
    addWindMenuItem( id );
  }
  write_file( id );
}

static void wind_quit()
{ short id;
  for (id=MAX_NB_WINDOWS-1; id>=0; id--)
  { if (wind_table[id].in_use && wind_table[id].is_file && wind_table[id].dirty)
    { if (!discard_changes( id )) return;
      wind_close( id );
    }
  }
  abnormal_exit = 0;
  /* os_quit();    29Dec92  e  */
  gDoQuit = 1;	/* 29Dec92  e  */
}

static void handle_close( id )
short id;
{ if (wind_table[id].is_file)
  { if ((wind_table[id].dirty) && (!discard_changes( id ))) return;
    wind_close( id );
  }
}

static char upcase( char c )
{ if( c >= 'a' &&  c <= 'z' ) return c - 0x20;
  else return c;
}
/* 08Jan93  e  -- changed to return Boolean; added test for empty find string */
Boolean find_next( id )
short id;
{ TeHANDLE hTE = wind_table[id].hTE;
  char *txt = *((**hTE).hText);
  long pos = eTeChPosToOffset( hTE, (**hTE).selActive ? (**hTE).selEnd : (**hTE).caretChPos );
  register char *start = txt + pos - 1;
  register short len = (unsigned char)find_string[0];
  register char *end = txt + eTeTextLength( hTE ) - len;
  register short i;
  Boolean again = TRUE;
  Str255 ci_string;
  if ( len == 0 ) goto not_found;
  if( find_ci )
  { for (i=len; i>0; i--)
      ci_string[i] = upcase( find_string[i] );
    /* ci_string[0] = len; -- not used */
  }
  find_begin:
  while (start < end)
  { if( find_ci )
    { for (i=len; i>0; i--)
        if( upcase( start[i] ) != (char )ci_string[i] ) goto next_pos;
    }
    else
    { for (i=len; i>0; i--)
        if (start[i] != (char )find_string[i]) goto next_pos;
    }
    eTeSetSelect( hTE, (long)(start-txt+1), (long)(start-txt+1+len) );
    return 1;
    next_pos:
    start++;
  }
  if( find_wrap && again ) 
  { start = txt;
    end = start + pos - len;
    again = FALSE;
    goto find_begin;
  }
not_found:
  SysBeep(10);
  return 0;
}

void replace_next( id )
short id;
{ TeHANDLE hTE = wind_table[id].hTE;
  if ( ! (**hTE).selActive )
    SysBeep(10);
  else
  { long len = (unsigned char)replace_string[0];
    check_TEInsert( ((char *)replace_string)+1, len, hTE, wind_table[id].bold_input );
    find_next( id );
    eTeShowCaret( hTE );
  }
}

/* 08Jan93  e  */
void replace_all( id )
short id;
{ TeHANDLE hTE = wind_table[id].hTE;
  long len = (unsigned char)replace_string[0];
  
  if ( (**hTE).selActive )
  { long pos = eTeChPosToOffset( hTE, (**hTE).selStart );
	eTeSetSelect( hTE, pos, pos );
  }
  while ( find_next( id ) )
    check_TEInsert( ((char *)replace_string)+1, len, hTE, wind_table[id].bold_input );
  eTeShowCaret( hTE );
}

static void find_dialog( short id )
{ DialogPtr dp;
  short i, s;
  Handle h;
  Rect r;
  eTePutScrap();          								/*  13Oct92  e  */
  ParamText( "\pFind:", "\pReplace with:", "\p", "\p" );
  dp = GetNewDialog( find_dlogID, 0L, (WindowPtr)-1L );
  if (dp == NULL) return;
  GetDItem( dp, 3, &i, &h, &r );
  SetIText( h, find_string );
  GetDItem( dp, 5, &i, &h, &r );
  SetIText( h, replace_string );
  GetDItem( dp, 7, &i, &h, &r);
  SetCtlValue( h, find_wrap );
  GetDItem( dp, 8, &i, &h, &r);
  SetCtlValue( h, find_ci );
  SelIText( dp, 3, 0, find_string[0] );
  SetCursor( &arrow );
  do
  { ModalDialog( 0L, &s );
    switch( s )
    { case 1:
        GetDItem( dp, 3, &i, &h, &r );
        GetIText( h, find_string );
        GetDItem( dp, 5, &i, &h, &r );
        GetIText( h, replace_string );
	    GetDItem( dp, 7, &i, &h, &r);
	    find_wrap = GetCtlValue( h );
	    GetDItem( dp, 8, &i, &h, &r);
	    find_ci   = GetCtlValue( h );
	    break;
      case 7:
      case 8:
		GetDItem( dp, s, &i, &h, &r );
		SetCtlValue( (ControlHandle )h, GetCtlValue( (ControlHandle )h ) ^ 1 );
        break;
    }
  } while( s != 1 && s != 2 );
  CloseDialog( dp );
  if( s == 1 ) find_next( id );
  eTeGetScrap();          								/*  13Oct92  e  */
}

/* 25Sep92  e  added print menu items */

static void handle_pgsetup( short id )
{ 
  TeHANDLE hTE = wind_table[id].hTE;
  SetCursor(&qd.arrow);
  PageSetupDialog( hTE );
}

/* 02Oct92  e  added iPrAbort check */

static void handle_print( short id )
{ 
  OSErr err;
  TeHANDLE hTE = wind_table[id].hTE;
  Str255 flnm;
  
  SetCursor(&qd.arrow);
  err = noErr;
  if ( ! (**hTE).hPrint )
	err = PageSetupDialog( hTE );
  if( ! err )
  {	pathstr_to_filename( wind_table[id].filename, flnm );
    err = eTePrint( hTE, true, true, flnm );
	eTePrint( nil, false, false, flnm );
  }
  if( err && err != userCanceledErr && err != iPrAbort ) io_err( err );
}

static short handle_menu( result, id )
long result;
short id;
{ short theItem, theMenu, m, w;
  MenuHandle mh;
  WindowPeek wPtr;
  Str255 name;

  theItem = LoWord( result );
  theMenu = HiWord( result );
  if (theMenu == 0) return 1;
  mh = GetMHandle( theMenu );
  for (m=appleM; m<=windowsM; m++) if (menus[m] == mh) break;
  switch (m)
  { case appleM:
      if (theItem == 1)
	  { eDoDialog(0);	/* about */
	  }
      else if (theItem == 2)
        ;
      else
      { GetItem(menus[appleM], theItem, &name);
        OpenDeskAcc( &name );
      }
      break;
    case fileM: 
      switch (theItem)
      { case newCommand:
        { short w = wind_open( "Untitled", TRUE, TRUE, FALSE );
          if (w < 0)
            wind_err();
          else
            wind_table[w].is_file = 1;
          break;
        }
        case openCommand:
        { short i;
          for (i=0; i<MAX_NB_WINDOWS; i++) if (!wind_table[i].in_use) break;
          if (i == MAX_NB_WINDOWS)
            wind_err();
          else
          { char path[FILENAME_LEN];
            if (get_file( "\p", 1, "TEXT", path, FILENAME_LEN ))
              edit( path, 0L, 0L );
          }
          break;
        }
        case closeCommand:
          if (id >= 0)
            handle_close( id );
          break;
        case saveCommand:
          if (id >= 0)
            if (wind_table[id].is_file && (wind_table[id].filename[0] != '\0'))
            { write_file( id );
              break;
            }
        case saveasCommand:
          if (id >= 0)
          { char path[FILENAME_LEN];
            Str255 flnm;
            pathstr_to_filename( wind_table[id].filename, flnm );
            if (put_file( "\pSave as:", flnm, path, FILENAME_LEN ))
              save_as( id, path );
          }
          break;
        case revertCommand:
          if (id >= 0)
            read_file( id, 0L, 0L );
          break;
        /* 25Sep92  e  added print menu items */
        case pgsetupCommand:
          if (id >= 0)
            handle_pgsetup( id );
          break;
        case printCommand:
          if (id >= 0)
            handle_print( id );
          break;
        case quitCommand:
          wind_quit();
          if (gDoQuit) os_quit(); /* 29Dec92  e  */
          break;
      }
      break;
	/* 10Jan93  e  -- separated out Search menu */
    case editM: 
      if (id < 0)
        SystemEdit(theItem-1);
      else
      { TeHANDLE hTE = wind_table[id].hTE;
        switch (theItem)
        { case undoCommand:
        	eTeUndo( hTE );
            eMaybeSmudgeWindow( id, hTE );
            break;
          case cutCommand:
            check_TECut( hTE );
            eMaybeSmudgeWindow( id, hTE );
            break;
          case copyCommand:
            check_TECopy( hTE );
            break;
          case pasteCommand:
            check_TEPaste( hTE, wind_table[id].bold_input );
            eMaybeSmudgeWindow( id, hTE );
            break;
          case clearCommand:
            eTeDelete( hTE );
            eMaybeSmudgeWindow( id, hTE );
            break;
          case selAllCommand:
            eTeSetSelect( hTE, 0, eTeTextLength( hTE ));
            break;
		  case stylesCommand:
			eStyleDlg( hTE );
          	break;
          case showPosnCommand:	/* 10Jan93  e  */
          	showPosnP ^= 1;
			CheckItem( menus[editM], showPosnCommand, showPosnP );
			for (w=0; w<MAX_NB_WINDOWS; w++)
    			if (wind_table[w].in_use) really_inval_msgrect( w );
          	break;
          default: ;
        }
        eTeShowCaret( hTE );
      }
      break;
    case findM:
      if (id >= 0)
      { TeHANDLE hTE = wind_table[id].hTE;
        switch (theItem)
        { case findCommand:
            find_dialog( id );
            break;
          case againCommand:
            find_next( id );
            break;
          case replaceCommand:
            replace_next( id );
            eMaybeSmudgeWindow( id, hTE );
            break;
          case fEnterCommand:
          { long sel_sta, sel_end, sel_len = 0;
            if( (**hTE).selActive ) {
              sel_sta = eTeChPosToOffset( hTE, (**hTE).selStart );
              sel_end = eTeChPosToOffset( hTE, (**hTE).selEnd );
              sel_len = sel_end - sel_sta;
            }
            if (sel_len > 0)
            { if( sel_len > 255 ) sel_len = 255;
              buf_to_p( (unsigned short)sel_len, *((**hTE).hText) + sel_sta, find_string );
            }
            else
              SysBeep( 1 );
            break;
          }
          case replaceAllCommand:	/* 08Jan93  e  */
            replace_all( id );
            eMaybeSmudgeWindow( id, hTE );
            break;
	    }
        eTeShowCaret( hTE );
	  }
	  break;
    case commandM: 
      switch (theItem)
      { case interruptCommand:
          HiliteMenu(0);
          handle_interrupt();
          return 0;
        case helpCommand:
          eDoDialog(1);	/* add help stuff */
          break;
        default:
          break;
        }
      break;
    case windowsM:
      switch(theItem)
	  { case 1:
		  select_and_show( wind_table[interaction_id].wptr );
		  break;
		case 2: /* blank line */
		  break;
		default:
		  theItem -= fustItem;
		  if(theItem < qWindItems)
			SelectWindow( wind_table[itemtowindid[theItem]].wptr );
		  break;
	  }
      break;
   }
  HiliteMenu(0);
  return 1;
}

static short do_tasks()
{ short w;
  register short i;
  short men;
  EventRecord event;
  Rect r;
  RgnHandle rgn;
  long ticks = Ticks;

  for (i=0; i<MAX_NB_WINDOWS; i++)
    if (wind_table[i].in_use)
    { if (wind_table[i].out_len > 0)
      { HLock( wind_table[i].out_buf );
        check_TEWrite( *wind_table[i].out_buf, (long)wind_table[i].out_len, wind_table[i].hTE, 0 );
        HUnlock( wind_table[i].out_buf );
        wind_table[i].out_len = 0;
      }
      /* 10Jan93  e -- added display of caret position */
      if ( wind_table[i].needs_inval_ticks != 0
           && ( wind_table[i].needs_inval_ticks + 15 ) < ticks )
      { really_inval_msgrect( i );
        wind_table[i].needs_inval_ticks = 0;
      }
    }
  w = wptr_to_id( FrontWindow() );
  if ( (!gInBackground) && (w >= 0) )
  { Point pt;
    Cursor *curs;
    OSEventAvail(0L, &event); /* This returns a NULL event with the global mouse location */
    if( gCursRgnOK && PtInRgn( event.where, gCursorRgn ) )
    { /* punt... SetCursor( current_cursor ); */
    }
    else
    { SetPort( (GrafPtr)wind_table[w].wptr );
      r = (**wind_table[w].hTE).viewRect;
      LocalToGlobal((Point *)&(r.top));      /* topLeft  */
      LocalToGlobal((Point *)&(r.bottom));   /* botRight */
      if (PtInRect( event.where, &r ))
      { curs = &ibeam_cursor;
        RectRgn(gCursorRgn, &r);
      }
      else
      { curs = &arrow;
        SetRectRgn(gCursorRgn, -32768, -32768, 32766, 32766);
        rgn = NewRgn();
        RectRgn(rgn, &r);
        DiffRgn(gCursorRgn, rgn, gCursorRgn);
        DisposeRgn(rgn);
      }
      if (!gCursRgnOK || current_cursor != curs)
      { current_cursor = curs;
        SetCursor( current_cursor );
      }
      gCursRgnOK = true;
    }
    eTeIdle( wind_table[w].hTE );
    men = (wind_table[w].is_file && (wind_table[w].filename[0] != '\0')) ? 1 : 0;
  }
  else
  { SetRectRgn(gCursorRgn, -32768, -32768, 32766, 32766);
    gCursRgnOK = false;
    SetCursor(&qd.arrow);	/*  13Oct92  e  */
    men = 2;
  }

  if ( (!gInBackground) && (men != current_menus) )
  { current_menus = men;
    switch (men)
    { case 0:
      case 1:
        /*
        EnableItem( menus[fileM], newCommand );
        EnableItem( menus[fileM], openCommand );
        EnableItem( menus[fileM], closeCommand );
        */
        EnableItem( menus[fileM], saveCommand );
        EnableItem( menus[fileM], saveasCommand );
        if (men == 0)
          DisableItem( menus[fileM], revertCommand );
        else
          EnableItem( menus[fileM], revertCommand );
        /*
        EnableItem( menus[fileM], quitCommand );
        */
        EnableItem( menus[fileM], pgsetupCommand );
        EnableItem( menus[fileM], printCommand );
        EnableItem( menus[editM], selAllCommand );
        EnableItem( menus[editM], stylesCommand );
        EnableItem( menus[editM], showPosnCommand );
        /*
        EnableItem( menus[findM], findCommand );
        EnableItem( menus[findM], againCommand );
        EnableItem( menus[findM], replaceCommand );
        EnableItem( menus[findM], replaceAllCommand );
        EnableItem( menus[findM], fEnterCommand ); */
        EnableItem( menus[findM], 0 );
        /*
        EnableItem( menus[commandM], 0 );
        */
        DrawMenuBar();
        break;
      case 2:
        /*
        DisableItem( menus[fileM], newCommand );
        DisableItem( menus[fileM], openCommand );
        DisableItem( menus[fileM], closeCommand );
        */
        DisableItem( menus[fileM], saveCommand );
        DisableItem( menus[fileM], saveasCommand );
        DisableItem( menus[fileM], revertCommand );
        /*
        DisableItem( menus[fileM], quitCommand );
        */
        DisableItem( menus[fileM], pgsetupCommand );
        DisableItem( menus[fileM], printCommand );
        DisableItem( menus[editM], selAllCommand );
        DisableItem( menus[editM], stylesCommand );
        DisableItem( menus[editM], showPosnCommand );
        /*
        DisableItem( menus[findM], findCommand );
        DisableItem( menus[findM], againCommand );
        DisableItem( menus[findM], replaceCommand );
        DisableItem( menus[findM], replaceAllCommand );
        DisableItem( menus[findM], fEnterCommand ); */
        DisableItem( menus[findM], 0 );
        /*
        DisableItem( menus[commandM], 0 );
        */
        DrawMenuBar();
        break;
    }
  }

  if ( (!gInBackground) && (w>=0) )
  { register EvQElPtr q;
    for (q = (EvQElPtr)EventQueue.qHead; q != NULL; q = (EvQElPtr)q->qLink)
      if ((q->evtQWhat == keyDown) && ((char)q->evtQMessage == '.') &&
          (q->evtQModifiers & cmdKey))
      { while (GetNextEvent(keyDownMask+keyUpMask+autoKeyMask,&event) &&
               !((event.what == keyDown) && ((event.message & charCodeMask) == '.') &&
                 ((event.modifiers & cmdKey) != 0) )) ;
        handle_interrupt();
        return 0;
      }
  }
  return 1;
}

/* from MacDTS */
#define kDITop					0x0050		/* kTopLeft - for positioning the Disk */
#define kDILeft					0x0070		/*   Initialization dialogs. */

void handle_activate( WindowPtr wptr, Boolean activep )
{ GrafPtr port;
  Rect r;
  short w = wptr_to_id( wptr );
  if( w >= 0 )
  { port = wind_table[w].wptr;
    r = port->portRect;
    r.left = r.right - (SBarWidth+1);
    SetPort( port );
    InvalRect( &r );
    r = port->portRect;
    r.top = r.bottom - (SBarWidth+1);
    InvalRect( &r );
    if ( activep )
    { eTeActivate( wind_table[w].hTE );
      ShowControl( wind_table[w].vscroll );
      ShowControl( wind_table[w].hscroll );
    }
    else
    { eTeDeactivate( wind_table[w].hTE );
      HideControl( wind_table[w].vscroll );
      HideControl( wind_table[w].hscroll );
    }
  }
}

static short handle_event( event )
EventRecord *event;
{ register short i;
  short w;
  WindowPtr event_window;

  if (IsDialogEvent( event ))
  { DialogPtr dp;
    short item;
    DialogSelect( event, &dp, &item );
    return 1;
  }

  w = wptr_to_id( FrontWindow() );
  { switch (event->what)
    { case mouseDown:
        gCursRgnOK = false;
        switch (FindWindow(event->where,&event_window))
        { case inDesk: 
            SysBeep( 10 );
            break;
          case inGoAway:
            if ((w = wptr_to_id( event_window )) >= 0)
              if (TrackGoAway(event_window,event->where)) handle_close( w );
            break;
          case inMenuBar:
          { TeHANDLE hTE = NULL;
            if( w >= 0) hTE = wind_table[w].hTE;
			eTeEditMenuUpdate( hTE, menus[editM] );
            return handle_menu( MenuSelect(event->where), w );
          }
          case inSysWindow:
            SystemClick(event,event_window);
            break;
          case inDrag:
            if ((w = wptr_to_id( event_window )) >= 0)
              DragWindow(event_window,event->where,&screenBits.bounds);
            break;
          case inGrow:
            if ((w = wptr_to_id( event_window )) >= 0)
              grow_window( w, event->where );
            break;
          case inContent:
            if (event_window != FrontWindow())
              SelectWindow(event_window);
            else 
              if ((w = wptr_to_id( event_window )) >= 0)
                content( w, event );
            break;
          default: ;
        }
        gLastMouseDown = *event;
        break;
      case mouseUp:
        gLastMouseUp = *event;
        break;
      case keyDown:
      case autoKey:
        { unsigned char c = event->message & charCodeMask;
		  TeHANDLE hTE = (w>=0)?wind_table[w].hTE:NULL;
          if ((event->modifiers & cmdKey) != 0)
          { long res;
            if (c == '=') c = '+'; else if (c == '/') c = '?';
            if (hTE != NULL) eTeEditMenuUpdate( hTE, menus[editM] );
			res = MenuKey( c );
			if( HiWord( res ) != 0 )
            	return handle_menu( res, w );
            if( (hTE != NULL) && (c >= 0) && (c <= 31) )
            {	check_TEKey( c, hTE, 0, event );
            	inval_msgrect( w ); /* 10Jan93  e -- added display of caret position */
            }
            break;
          }
          if (hTE == NULL) break;
          /*
          if( ( event->modifiers & controlKey ) != 0 )
          { eEditCommand( hTE, c, event->modifiers, wind_table[w].bold_input );
          	eMaybeSmudgeWindow( w, hTE );
          	inval_msgrect( w );
          }
          */
          else if( ( c == 127 /* del fwd */ )  || ( c <= 31 ) )
          { if( ( c == 3 /*enter*/ ) || (( c == '\r' /*cr*/ ) && ((event->modifiers & optionKey) != 0 )))
            { short id = (wind_table[w].is_file) ? interaction_id : w;
              long sel_sta;
              long sel_end;
              long sel_len;
              if( (**hTE).selActive ) {
              	sel_sta = eTeChPosToOffset( hTE, (**hTE).selStart );
              	sel_end = eTeChPosToOffset( hTE, (**hTE).selEnd );
              }
              else
              { extern void eTeGetRun( eRec **hE, long *sta, long *end );
                if ( w == interaction_id )
                  eTeGetRun( hTE, &sel_sta, &sel_end );
                else
                  sel_end = sel_sta = eTeChPosToOffset( hTE, (**hTE).caretChPos );
              }
              eTeSetSelect( hTE, sel_end, sel_end );
              sel_len = sel_end - sel_sta;
              HLock((**hTE).hText);
              if (sel_len > 0)
                put_input( id, *((**hTE).hText) + sel_sta, sel_len, 1, 0, 1 );
              else
                put_input( id, NULL, 0, 1, 0, 1 ); /*  not done yet!?  12Sep92  e  */
              HUnlock((**hTE).hText);
            }
            else if (c == 9) /*tab*/
            { eTabCommand( hTE, event->modifiers, wind_table[w].bold_input );
              eMaybeSmudgeWindow( w, hTE );
              inval_msgrect( w ); /* 10Jan93  e -- added display of caret position */
            }
            else
            { check_TEKey( c, hTE, wind_table[w].bold_input, event );
              if ((c == '\r') && (**hTE).autoInd)
                eTabCommand( hTE, optionKey, wind_table[w].bold_input );
              eMaybeSmudgeWindow( w, hTE );
              inval_msgrect( w ); /* 10Jan93  e -- added display of caret position */
              if ( (c=='\r' /*cr*/) || (c=='\b' /*bs*/) ) eTeShowCaret( hTE );
            }
          }
          else
          { check_TEKey( c, hTE, wind_table[w].bold_input, event );
            eMaybeSmudgeWindow( w, hTE );
            inval_msgrect( w ); /* 10Jan93  e -- added display of caret position */
            eTeShowCaret( hTE );
          }
        }
        break;
      case activateEvt:
        gCursRgnOK = false;
      	handle_activate( (WindowPtr)event->message, ((event->modifiers & activeFlag) != 0) );
        break;
      case updateEvt: 
        if ((w = wptr_to_id( (WindowPtr)event->message )) >= 0)
        { update_window(w);
        }
        else
        { GrafPtr port = (WindowPtr)event->message;
          SetPort( port );
          BeginUpdate( port );
          EndUpdate( port );
        }
        break;
      case osEvt:
        if( *(char *)(&event->message) == suspendResumeMessage )
        { gCursRgnOK = false; /* Suspend/resume is also an activate/deactivate. */
          gInBackground = !(event->message & resumeFlag);
          if( gInBackground ) eTePutScrap();          		/*  13Aug92  e  */
          else				  eTeGetScrap();          		/*  13Aug92  e  */
          handle_activate( FrontWindow(), !gInBackground );
        }
        /* punt mouseMovedMessage */
        break;
	  case diskEvt:
        gCursRgnOK = false;		             /* It is not a bad idea to at least call */
		if (HiWord(event->message) != noErr) /* DIBadMount in response to a diskEvt,  */
		{ Point pt;							 /* so that the user can format a floppy. */
		  SetPt( &pt, kDILeft, kDITop );
		  DIBadMount( pt, event->message );
		}
		break;
      /* 29Dec92  e  */
      case kHighLevelEvent:
        AEProcessAppleEvent(event);
        if (gDoQuit) os_quit();
        break;
      default: ;
    }
  }
  return 1;
}

long os_get_next_event( EventRecord *event )
{ long result = 0;
  GrafPtr save;
  GetPort( &save );
  if (do_tasks())
  { if (gHasWNE)
  	{ result = (long)( gInBackground ? gWaitTicksBG	/* 22Jul92  e   -- added gMaxSleep */
  	  								 : (long )( (gWaitTicksFG > gMaxSleep) ? gMaxSleep
  	  								 									   : gWaitTicksFG) );
      result = WaitNextEvent( everyEvent, event, result, gCursorRgn );
    }
    else
    { SystemTask();
      result = GetNextEvent( everyEvent, event );
    }
  }
  SetPort( save );
  return result;
}

long os_handle_event( EventRecord *event )
{ long result;
  GrafPtr save;
  GetPort( &save );
  result = handle_event( event );
  SetPort( save );
  return result;
}

void os_event_check( void )			/* 25Jan93  e  */
{ EventRecord event;
  os_get_next_event( &event );
  os_handle_event( &event );
  next_eventchk_ticks = Ticks + TicksBetweenEventChecks;
}

void os_quit()
{ short i;
  long ticks;
#if 0
  stop_intr_task();
  for (i=MAX_NB_OPEN_FILES-1; i>0; i--) os_file_close( (OS_FILE)i );
  if (abnormal_exit)
  { for (i=0; i<3; i++) { SysBeep(3); Delay( 3, &ticks ); }
    Delay( 60, &ticks );
  }
#endif
  wind_end();
  /* was: ExitToShell();  for Tricia: */
  exit(0);
}

/* ******* console io ******* */

#if 0
void os_flush_caches()
{ if (gHasHWPriv)
  asm
  { moveq #1,d0
    dc.w _HWPriv
  }
}
#endif

static short main_internal( )
{ short i;
  MaxApplZone();
  for (i = 0; i < 10; i++)
		MoreMasters();
  /* CouldAlert( ok_alertID ); */
  SetGrowZone( mem_full_err );

  wind_begin();

#if 0
  GetVol( new_args, &current_volume );
  if (!getfullpathfromcurrentvolume( CurApName, new_args, 256, 0 ))
    p_to_c( CurApName, new_args );
#endif

  SetCursor( current_cursor );

  ParamText1( "\p" );

#if 0
  loading_dp = GetNewDialog( loading_dlogID, 0L, (WindowPtr)-1L );
  if (loading_dp != NULL) DrawDialog( loading_dp );
#endif

  return 0;
}

static short console_initialized = 0;

WindowPeek os_console_new( unsigned char *name )
{ char cname[256];
  short id;
  WindowPeek wp;
  if( !console_initialized )
  { main_internal();
    console_initialized = 1;
  }
  p_to_c( name, cname );
  id = wind_open( cname, FALSE, FALSE, TRUE );
  if( id < 0 ) return (WindowPeek )0;
  interaction_id = id;
  delWindMenuItem( interaction_id );
  wp = (WindowPeek )wind_table[id].wptr;
  wp->refCon = (long )id;
  return(wp);
}

long os_console_read( WindowPeek wp, unsigned char *ptr, long cnt )
{ long result = 0;
  GrafPtr save;
  GetPort( &save );
  if( cnt > 0 )
  { register unsigned char *p1 = ptr;
    short id = wp->refCon;
    EventRecord event;
    do
    { os_get_next_event( &event );
      os_handle_event( &event );
      if( interrupted )
        goto ocr_done;
      else
      { register unsigned char *p2 = (unsigned char *)wind_table[id].buf + wind_table[id].pos;
        register long len = wind_table[id].len - wind_table[id].pos;
    	register short i = 0;
        if (len > cnt) len = cnt;
        while ( i < len )
        { register unsigned char c = p2[i++];
          if (c == '\r')
          { c = '\n';
            len = i; /* to exit inner loop */
            cnt = i; /* to exit outer loop */
          }
          *p1++ = c;
        }
        wind_table[id].pos += i;
        cnt -= i;
        result += i;
      }
    } while( cnt > 0 );
  }
ocr_done:
  SetPort( save );
  return result;
}

long os_console_write( WindowPeek wp, unsigned char *ptr, long cnt)
{ long result = 0;
  GrafPtr save;
  short id = (short )wp->refCon;
  register unsigned char *p1 = ptr, *p2 = ptr+cnt;
  register short len = wind_table[id].out_len;
  register unsigned char *p3;
  if( wind_table[id].out_buf == NULL )
  { wind_table[id].out_buf = NewHandle( OUT_BUF_LEN );
    if( wind_table[id].out_buf == NULL )
    { SysBeep(10);
      return result;
    }
  }
  HLock( wind_table[id].out_buf );
  p3 = (unsigned char *)*wind_table[id].out_buf;

  if (!(wp->visible))
    select_and_show( wp );

  GetPort( &save );
  while( p1 < p2 )
  { register unsigned char c = *p1++;
    if (c == '\n') { p3[len++] = '\r'; goto output; }
    p3[len++] = c;
    if (len == OUT_BUF_LEN)
    { output:
      if( check_TEWrite( (char *)p3, (long)len, wind_table[id].hTE, 0 ) ) goto werror;
      wind_table[id].out_len = 0;
      len = 0;
    }
  }
  wind_table[id].out_len = len;
  result = cnt;
werror:
  SetPort( save );
  HUnlock( wind_table[id].out_buf );
  return result;
}

long os_console_close( WindowPeek wp )
{ long result = 0;
  GrafPtr save;
  short id = (short )wp->refCon;
  GetPort( &save );
  if( id < 0 )
    result = -1;
  else
    wind_close( id );
  SetPort( save );
  return result;
}

/*--------------------------------------------------------------------------*/

/* new stuff  29Dec92  e  */

/* Used to check for any unread required parameters. Returns true if we
** missed at least one. */

static Boolean	MissedAnyParameters(AppleEvent *message)
{
	OSErr		err;
	DescType	ignoredActualType;
	AEKeyword	missedKeyword;
	Size		ignoredActualSize;
	EventRecord	event;

	err = AEGetAttributePtr(	/* SEE IF PARAMETERS ARE ALL USED UP.		  */
		message,				/* AppleEvent to check.						  */
		keyMissedKeywordAttr,	/* Look for unread parameters.				  */
		typeWildCard,			/* So we can see what type we missed, if any. */
		&ignoredActualType,		/* What it would have been if not coerced.	  */
		(Ptr)&missedKeyword,	/* Data area.  (Keyword not handled.)		  */
		sizeof(missedKeyword),	/* Size of data area.						  */
		&ignoredActualSize		/* Actual data size.						  */
	);

/* No error means that we found some unused parameters. */

	if (err == noErr) {
		event.message = *(long *) &ignoredActualType;
		event.where = *(Point *) &missedKeyword;
		err = errAEEventNotHandled;
	}

/* errAEDescNotFound means that there are no more parameters.  If we get
** an error code other than that, flag it. */

	return(err != errAEDescNotFound);
}

static OSErr	OpenDocEventHandler(AppleEvent *message, AppleEvent *reply, short mode)
{
	OSErr		err;
	OSErr		err2;
	AEDesc		theDesc;
	FSSpec		theFSS;
	short		loop;
	long		numFilesToOpen;
	AEKeyword	ignoredKeyWord;
	DescType	ignoredType;
	Size		ignoredSize;
	char path[512];
	short w;
	Str255 flnm;
	FInfo theFInfo;

	theDesc.dataHandle = nil;
		/* Make sure disposing of the descriptors is okay in all cases.
		** This will not be necessary after 7.0b3, since the calls that
		** attempt to create the descriptors will nil automatically
		** upon failure. */

	if ( err = AEGetParamDesc( message, keyDirectObject, typeAEList, &theDesc ) )
		return(err);

	if ( ! MissedAnyParameters( message ) ) {

/* Got all the parameters we need.  Now, go through the direct object,
** see what type it is, and parse it up. */

		err = AECountItems(&theDesc, &numFilesToOpen);
		if (!err)
		{	/* We have numFilesToOpen that need opening, as either a window
			** or to be printed.  Go to it... */

			for (loop = 1; ((loop <= numFilesToOpen) && (!err)); ++loop)
			{	err = AEGetNthPtr(		/* GET NEXT IN THE LIST...		 */
					&theDesc,			/* List of file names.			 */
					loop,				/* Item # in the list.			 */
					typeFSS,			/* Item is of type FSSpec.		 */
					&ignoredKeyWord,	/* Returned keyword -- we know.  */
					&ignoredType,		/* Returned type -- we know.	 */
					(Ptr)&theFSS,		/* Where to put the FSSpec info. */
					sizeof(theFSS),		/* Size of the FSSpec info.		 */
					&ignoredSize		/* Actual size -- we know.		 */
				);
				if (err) break;

				FSpGetFInfo( &theFSS, &theFInfo );
				if ( theFInfo.fdType != 'TEXT' )
				{ SysBeep( 3 );
				  continue;
				}
				gPrintPage = mode;
					/* Open the window off-screen if we are printing.
					** We use the gPrintPage global to flag this.  Normally, the
					** gPrintPage global is to tell ImageDocument if we are imaging
					** to the window or to paper.  We don't need it for this yet,
					** as we can't image the document until it is opened.  DoNewWindow()
					** uses gPrintPage as a flag to open the window off-screen, but
					** visible, so that PrintMonitor can use the title of the window
					** as the document name that is being printed. */
				
				if ( getfullpath( theFSS.vRefNum, theFSS.parID, theFSS.name, path, 511, 1 ) )
				{	
					w = edit( path, 0L, 0L );

					if ( gPrintPage && w >= 0 )
					{	pathstr_to_filename( wind_table[w].filename, flnm );
    					err  = eTePrint( wind_table[w].hTE, (mode == 2), (loop == 1), flnm );
						mode = 1;	/* No interaction mode (mode == 2) only valid
									** for the first printed document. */
						wind_close( w );
					}
				}
				else err = errAEEventNotHandled;	 /* file pathname too long */
			}
			if (gPrintPage)
			{	eTePrint( nil, false, false, flnm ); /* cleanup */
				gPrintPage = 0; 					 /* back to normal */
			}
		}
	}
	err2 = AEDisposeDesc(&theDesc);
	return(err ? err : err2);
}

static Boolean install_aehandler( AEEventClass cl, AEEventID id, ProcPtr p )
{ OSErr	err;
  err = AEInstallEventHandler( cl, id, p, 0L, false );
  if (err)
  { io_err(err);
    return false;
  }
  return true;
}

static pascal OSErr	DoAEOpenApplication( AppleEvent *message, AppleEvent *reply, long refcon )
{
	return(noErr);
}

static pascal OSErr	DoAEOpenDocuments( AppleEvent *message, AppleEvent *reply, long refcon )
{
    gCursRgnOK = false;
	return( OpenDocEventHandler( message, reply, 0 ) ); /* 0 means regular open document */
}

#define kTimeOutInTicks (60 * 30)	/* 30 second timeout. */

static pascal OSErr	DoAEPrintDocuments(AppleEvent *message, AppleEvent *reply, long refcon)
{
	short				openMode;
	ProcessSerialNumber	cpsn, fpsn;
	Boolean				procsSame;

    gCursRgnOK = false;
	openMode = 1;
	if (!AEInteractWithUser(kTimeOutInTicks, nil, nil))
		++openMode;

	GetCurrentProcess(&cpsn);		/* We may have been moved to the front. */
	GetFrontProcess(&fpsn);
	SameProcess(&cpsn, &fpsn, &procsSame);
	gInBackground = !procsSame;

	return( OpenDocEventHandler( message, reply, openMode ) );
		/* openMode is either 1 or 2, depending if user interaction is okay. */
}

static pascal OSErr	DoAEQuitApplication( AppleEvent *message, AppleEvent *reply, long refcon )
{
    gCursRgnOK = false;
	wind_quit();
	return( gDoQuit ? noErr : errAEEventNotHandled );
}

/* 1Feb93  e  --  DoScript added */

static OSErr DoScriptAsFile( FSSpec *theFSS )
{
	OSErr err = noErr;
	char  path[512];
	
	if( getfullpath( theFSS->vRefNum, theFSS->parID, theFSS->name, path, 511, 1 ) )
	{
		put_input( interaction_id, " use \"", 6, 0, 0, 1 );
		put_input( interaction_id, path, strlen(path), 0, 0, 0 );
		put_input( interaction_id, "\";", 2, 1, 0, 0 );
	}
	else err = errAEEventNotHandled;
	return err;
}

static pascal OSErr DoAEDoScript( AppleEvent *theAEEvent, AppleEvent *theAEReply, long *theRefCon)
{
	OSErr		theErr;
	DescType	typeCode;
	Size		sizeOfParam, actualSize;
	char		ourScriptText[256];
	FSSpec		ourScriptFSpec;

	/* Get the script to run */ 

	theErr = AESizeOfParam( theAEEvent, keyDirectObject, &typeCode, &sizeOfParam);
	if (theErr != noErr)
	{	/*
			If we fail here just return the error. We don't need to do any clean up since 
			we've allocated nothing on the heap yet.  The Apple Event Manager automatically 
			adds the error number to the reply as keyErrorNumber for non zero handler returns.
		*/
		return theErr;
	}
	else if ((typeCode == typeChar) || (typeCode == typeStyledText) || (typeCode == typeIntlText))
	{
		theErr = AEGetParamPtr( theAEEvent, keyDirectObject, typeChar, &typeCode,
								 (Ptr)&ourScriptText, sizeof(ourScriptText), &actualSize);
		if (theErr == noErr)
			if ( MissedAnyParameters( theAEEvent ) )
				theErr = errAEEventNotHandled;
			else put_input( interaction_id, ourScriptText, actualSize, 1, 0, 1 );
	}
	else if (typeCode == typeAlias)
	{
		theErr = AEGetParamPtr( theAEEvent, keyDirectObject, typeFSS, &typeCode,
                                (Ptr)&ourScriptFSpec, sizeof(ourScriptFSpec), &actualSize);
		if ( theErr == noErr )
			if ( MissedAnyParameters( theAEEvent ) )
				theErr = errAEEventNotHandled;
			else theErr = DoScriptAsFile( &ourScriptFSpec );
	}
	else theErr = errAEEventNotHandled;
	return theErr;
}


static void init_ae()
{
	long	result;

	gHasAppleEvents = (Gestalt(gestaltAppleEventsAttr, &result) ? false : result != 0);

	if (gHasAppleEvents)
	     install_aehandler( kCoreEventClass, kAEOpenApplication, (ProcPtr)DoAEOpenApplication )
	  && install_aehandler( kCoreEventClass, kAEOpenDocuments, (ProcPtr)DoAEOpenDocuments )
	  && install_aehandler( kCoreEventClass, kAEPrintDocuments, (ProcPtr)DoAEPrintDocuments )
	  && install_aehandler( kCoreEventClass, kAEQuitApplication, (ProcPtr)DoAEQuitApplication )
	  && install_aehandler( kAEMiscStandards, kAEDoScript, (ProcPtr)DoAEDoScript );
}

/*--------------------------------------------------------------------------*/

