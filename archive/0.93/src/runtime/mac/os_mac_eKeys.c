/* os_mac_eKeys.c
 * 3May92  e
 */

#include "os_mac_eEdit.h"

/* extensions to an editor for text files by e
   questions/comments via Internet <e@Flavors.COM> */
/* Copyright © e, 1989,1992. All rights reserved.
	Developed using THINK C 5.0.1 for use with Gambit Scheme.
	This code may be freely distributed as long as this notice remains.
   thanks to Marc Feeley.
*/

#define whsp_char(c) (((c)>=0) && ((c)<=32))

/* 16Dec92  e  -- miniature version of os_mac_eKeys.c for ML */

/* <tab> documentation
 * C1 is the position of the first graphic character on the current line
 * P1 is the position of the first graphic character on the previous line
 * <option-tab>
 *   inserts or removes space on the current line to align C1 with P1
 * <tab>
 *   if C1 < P1 then same as <option-tab>
 *              else insert sufficient space on the current line
 *                   to move C1 right to next tab stop
 * <shift-tab>
 *   if C1 > P1 then same as <option-tab>
 *              else remove sufficient space on the current line
 *                   to move C1 left to previous tab stop
 * Notes: 
 *  <tab> does not move the insert point or selection relative to the text
 *  <tab> characters are never inserted into the text
 */

extern short check_TEInsert( char *ptr, long len, eRec **hE, short bold );

static char *eighty = "                                                                                ";

void eTabCommand( eRec **hE, short modifiers, short style )
{ long open_paren;
  long selStart, selEnd, selLen;
  ChPos plcp, tlcp, nlcp;
  char *p;
  long pl, tl, nl;
  long adj = 0;
  Boolean selActive;

  if( ( selActive = (**hE).selActive ) )
  {	selStart = eTeChPosToOffset( hE, (**hE).selStart );
	selEnd   = eTeChPosToOffset( hE, (**hE).selEnd );
    selLen   = selEnd - selStart;
	eTeSetSelect( hE, selStart, selStart );
    tlcp = (**hE).selStart;
  }
  else
  {	selStart = eTeChPosToOffset( hE, (**hE).caretChPos );
    selLen = 0;
    tlcp = (**hE).caretChPos;
  }
  plcp.h = tlcp.h = nlcp.h = 0;	/* start of {prev,this,next} line */
  plcp.v = tlcp.v - 1;
  nlcp.v = tlcp.v + 1;
  nl = eTeChPosToOffset( hE, nlcp );	/* next line */
  tl = eTeChPosToOffset( hE, tlcp );	/* this line */
  if (plcp.v < 0)
  {	pl = 0;
  	plcp.v = 0;
  }
  else
  { pl = eTeChPosToOffset( hE, plcp );
	p = *((**hE).hText) + pl;
	while( whsp_char( *p ) && pl < tl && *p != RETURN ) { p++; pl++; plcp.h++; }
  }
  p = *((**hE).hText) + tl;
  while( whsp_char( *p ) && tl < nl && *p != RETURN ) { p++; tl++; tlcp.h++; }

  if( tl != selStart ) eTeSetSelect( hE, tl, tl );
  if( tl < selStart )  adj = selStart - tl;
  
  if ( ! ( modifiers & optionKey ) )
  {	short tabStops = (**hE).tabStops;
    if( modifiers & shiftKey )
	{ if( tlcp.h <= plcp.h )
		if( tlcp.h > 0 && tabStops > 0 )
			plcp.h = ( (tlcp.h - 1) / tabStops ) * tabStops;
		else
			plcp.h = 0;
    }
    else
    { if( tlcp.h >= plcp.h && tabStops > 0 )
		plcp.h = tlcp.h + tabStops - tlcp.h % tabStops;
	}
  }
  if( tlcp.h > plcp.h )
  { plcp.v = tlcp.v;
	pl = eTeChPosToOffset( hE, plcp );
	eTeSetSelect( hE, pl, pl );
	eTeKillTo( hE, tlcp );
  }
  else while (tlcp.h < plcp.h)
  { long len = (plcp.h - tlcp.h < 80) ? plcp.h - tlcp.h : 80;
    if (check_TEInsert( eighty, len, hE, style )) goto err;
    tlcp.h += len;
  }
  err:
  if( tl < selStart || selActive )
  { selStart = eTeChPosToOffset( hE, (**hE).caretChPos ) + adj;
    eTeSetSelect( hE, selStart, selStart + selLen );
  }
  eTeShowCaret( hE );
}

/* end of os_mac_eKeys.c */
