/* os_mac.h */

/* extracted from os_mac.c, eEdit.c, ext_mac.c, eStyle.c  30Jun92  e */
/* edited for Tricia  --  12Sep92  e  */

/* Macintosh specific stuff (for THINK C 5.0 compiler). */

#include <Retrace.h>
#include <Folders.h>

/*
#define USE_eTe_debug (1) */

#include "os_mac_eEdit.h"
#define TeHANDLE eTeHandle

#define dfltTabs (8)
#define dfltWrap (357)
#define dfltAutoInd (0)

/* prototypes for eStyle.c and eKeys.c */
void eStyleDlg( eRec **hE );
void eEditCommand( eRec **hE, char ch, short modifiers, short style );
void eTabCommand( eRec **hE, short modifiers, short style );

#define PREFS_FILENAME "\pSMLeNJ Preferences"
void savePrefs( ConstStr255Param fn );

#define FontSize 9
#define FontH 12
#define FontW 6
#define Border 3
#define SBarWidth 15

#define ok_alertID        128
#define ok_cancel_alertID 129

#define loading_dlogID 130
#define about_dlogID   131
#define find_dlogID    358

#define appleID   128
#define fileID    129
#define editID    130
#define commandID 131
#define windowsID 132
#define specialID 133
#define findID    134

#define newCommand       1
#define openCommand      2
#define closeCommand     3
#define saveCommand      5
#define saveasCommand    6
#define revertCommand    7
#define pgsetupCommand   9
#define printCommand     10
#define quitCommand      12

#define undoCommand       1
#define cutCommand        3
#define copyCommand       4
#define pasteCommand      5
#define clearCommand      6
#define selAllCommand     7
#define stylesCommand	  9
#define showPosnCommand	  10

#define findCommand       1
#define againCommand      2
#define replaceCommand    3
#define fEnterCommand     4
#define replaceAllCommand 5


#define interruptCommand  1
#define helpCommand       3

#define appleM   0
#define fileM    1
#define editM    2
#define findM    3
#define commandM 4
#define windowsM 5
#define specialM 6

#define gc_cursorID 128

#define MAX_NB_WINDOWS 12
#define FILENAME_LEN 256
#define OUT_BUF_LEN 256

extern MenuHandle menus[7];
extern TextStyle prefStylNormal;
extern TextStyle prefStylHilite;
extern TextStyle dfltStylNormal;
extern TextStyle dfltStylHilite;

extern short prefTabs;
extern short prefWrap;
extern short prefAutoInd;

extern Cursor watch_cursor;
extern Cursor gc_cursor;
extern Cursor ibeam_cursor;

/*  added for Tricia  --  12Sep92  e  */

extern short interrupted;

void os_quit(void);
WindowPeek os_console_new( unsigned char *name );
long os_console_read(  WindowPeek wp, unsigned char *ptr, long cnt );
long os_console_write( WindowPeek wp, unsigned char *ptr, long cnt );
long os_console_close( WindowPeek wp );

/* end of os_mac.h */
