/* os_mac_Print.c */
/* 28Sep92  e     */
/* adapted from ...
/*
** Apple Macintosh Developer Technical Support
**
** Program:     MacShell
** File:        print.c
** Written by:  Eric Soldan
** Based on:    Code from Pete "Luke" Alexander.
**
** Copyright © 1989-1991 Apple Computer, Inc.
** All rights reserved.
*/

/* portions Copyright © 1992 e.  All rights reserved. */

#include <MacHeaders>

/*****************************************************************************/

/* 10Dec92  e  edited for Tricia */

#ifdef THINK_C
#if THINK_C < 5
#define THINK_PRE_5
#endif
#endif

#include "os_mac_eEdit.h"

#include <Packages.h>

#define rPrStatusDlg (270)

#ifndef __ERRORS__
#include <Errors.h>
#endif

#ifndef __RESOURCES__
#include <Resources.h>
#endif

/*****************************************************************************/

static pascal void	PrintIdleProc( void );
static DialogPtr	PrintingStatusDialog;

/*****************************************************************************/

/* eEdit specific stuff... */

extern void eTeDrawLine( eRec **hE, ChPos beginPos, Point location );

static void eTePrintGuts( eRec **hE, short *vNext, Rect *area, short lpp )
{
	eRec *pE;
	short vLast, vFirst;
	ChPos	startPos;
	Point	location;
	/* Rect	tRect; */
	
	HLock( (Handle )hE );
	pE = *hE;
	
	if ( ( vFirst = *vNext ) < 0 )
		vFirst = 0;

	vLast = vFirst - 1 + lpp;	/* lpp was: ( (*area).bottom - (*area).top ) / (*pE).vScale; */

	if ( vLast >= (*pE).bounds.v ) 
	{	vLast = (*pE).bounds.v - 1;
		*vNext = -1;
	}
	else
		*vNext = vLast + 1;	/* for next time */

	/* EraseRect( &tRect ); */

	if ( vFirst < (*pE).bounds.v && vLast >= 0)

	{	location.h = (*area).left /* (*pE).leftMargin */ ;
		location.v = (*area).top  /* (*pE).topMargin */  ;
		startPos.h = 0;

		for ( startPos.v = vFirst;
			  startPos.v <= vLast;
			  location.v += (*pE).vScale, ++startPos.v )
		{	eTeDrawLine( hE, startPos, location );
		}
		(**hE).curStyle = NOSTYLE;	/* gotta do to force first font/size/style in window */
	}
	HUnlock( (Handle )hE );
}

static void headerPrepare( eRec **hE, short style )
{
	(**hE).curStyle = style;
    TextFont( (**hE).style[style].tsFont );
    TextFace( (**hE).style[style].tsFace );
    TextSize( (**hE).style[style].tsSize );
    /* RGBColor tsColor; */
	PenMode(patCopy);PenSize(1,1);PenPat(&black);
}

#define leftMargin(port) ((port)->portRect.left + 4)

static void dpyTab( short tabwidth )
{
	GrafPort *gPort;
	short targetX;

	GetPort( &gPort );
	targetX = leftMargin(gPort) + 
		( ( gPort->pnLoc.h - leftMargin(gPort) ) / tabwidth + 1 ) * tabwidth;
	MoveTo( targetX, gPort->pnLoc.v );
}

#define WAGspace (3)

static void	eTePrintPages( eRec **hE, TPPrPort printPort, Str255 nameString, short lastPage )
{
	Rect			area, frame;
	OSErr			err;
	short			tabwidth;
	short			vNext = 0;
	short 			ePrintPage = 1;
	short			lpp, lh;
	long			time;
	/* Str255		timeString, dateString, tStr; */
	unsigned char   timeString[32];
	unsigned char   dateString[64];
	unsigned char   tStr[32];

	headerPrepare( hE, 1 );
	tabwidth = CharWidth(' ') * (12);
	GetDateTime( &time );
	IUTimeString( time, 0, timeString );
	IUDateString( time, abbrevDate, dateString );
			
	area = ((WindowPtr )printPort)->portRect;
	ClipRect( &area );
	InsetRect( &area, 4, 4 );	/* Just so no characters get clipped. */
	lh = (**hE).vScale;
	lpp = ( area.bottom - area.top ) / lh;
	area.bottom = area.top + WAGspace + lpp * lh;
	
	frame = area;
	InsetRect( &frame, -4, 0 );
	area.top += lh + WAGspace;

	while( ePrintPage <= lastPage && ! (err = PrError()) )
	{
		PrOpenPage( printPort, nil );
		if( ! (err = PrError()) )
		{
			headerPrepare( hE, 1 );
			MoveTo( frame.left, frame.top + lh );			/* WAG */
			Line(   frame.right - frame.left, 0 );
			MoveTo( area.left, frame.top + (**hE).fontAscent );
			DrawString( dateString );
			dpyTab(tabwidth);
			DrawString( timeString );
			dpyTab(tabwidth);
			DrawString( nameString );
			dpyTab(tabwidth);
			DrawString( "\ppage " );
			NumToString( ePrintPage, &tStr );
			DrawString( &tStr );

			eTePrintGuts( hE, &vNext, &area, lpp - 1 );
			
			headerPrepare( hE, 1 );
			MoveTo( frame.left, frame.bottom );
			LineTo( frame.right, frame.bottom);
		}
		PrClosePage( printPort );
		if( vNext == -1 ) break;	/* Text didn't go to bottom of page so we're done */
		++ePrintPage;
	}
}

/*****************************************************************************/
/* mostly generic stuff... */

/* This print-loop function is designed to be called under various situations.
** The big issue that it handles is finder printing.  If multiple documents
** are to be printed from the finder, the user should only see one job dialog
** for all the files.  (If a job dialog is shown for each file, how does the
** user know for which file the dialog is for?)  So, for situations where
** there is more than one file to be printed, call this code the first time
** with the firstJob boolean true.  Normally, the jobDlg boolean will also
** be true, except that under 7.0, you may be printing in the background.
** If this is the case, you don't want a job dialog for even the first file,
** and you should pass in false for the jobDlg boolean in this case.  For
** files 2-N, you should pass false for both booleans.  For regular application
** printing, you should pass true for both booleans, since the file is the
** first (only) file, and you are not in the background.
**
** After calling this function to print a document, you need to call it
** again with a nil document handle.  The print record for the first (or only)
** document printed is preserved in a static variable.  This is so that the
** job dialog information can be passed on to documents 2-N in the print job.
** Calling this function with the document handle nil tells this function
** that you are done printing documents, and that the print record for the
** first job can be disposed of.
*/

OSErr	eTePrint( eRec **hE, Boolean jobDlg, Boolean firstJob, Str255 flnm)
{
	OSErr			err;
	short			i, copies, keepResFile, fstPage, lstPage;
	Boolean 		do_init;
	TPrStatus		status;
	THPrint			prRecHndl;
	TPPrPort		printPort;
	GrafPtr			oldPort;
	ControlHandle	proceedButton;
	Rect 			rct;

	static THPrint	prMergeHndl;

	if ( ! hE )
	{	if ( prMergeHndl )
		{	DisposHandle( (Handle )prMergeHndl );
			prMergeHndl = nil;
		}
		return( noErr );
	}

	PrintingStatusDialog = nil;

	if( ! (prRecHndl = (THPrint )NewHandle( sizeof( TPrint )) ) )
	{	/* If we can't generate a print record handle, we are out of here. */
		return(memFullErr);
	}
	if( (**hE).hPrint )
	{	/* Get the document's print info into the print record handle. */
		BlockMove( (Ptr )(*(**hE).hPrint), (Ptr )(*prRecHndl), sizeof( TPrint ) );
		do_init = FALSE;
	}
	else
	{	if (!((**hE).hPrint = (THPrint)NewHandle(sizeof(TPrint))))
		{	/* If we can't generate a print record handle, we are out of here. */
			DisposHandle( (Handle )prRecHndl );
			return( memFullErr );
		}
		do_init = TRUE;
	}

	GetPort(&oldPort);

	SetCursor(&qd.arrow);

	PrOpen();
	err = PrError();

	if (!err) {
		keepResFile = CurResFile();	/* some printers change CurResFile & we are being safe! */

		if( do_init )
		{	PrintDefault(prRecHndl);		/* The document print record was never 
			err = PrError();				** initialized.  Now is is. */
		}			
		if( ! err )
		{	PrValidate( prRecHndl );		/* Do this just 'cause Apple says so. */
			err = PrError();
		}
		if( ! err )
		{	if( jobDlg )					/* User gets to click some buttons. */
			{	if (!(PrJobDialog(prRecHndl))) err = userCanceledErr;
				else						   err = PrError();
			}
		}
		if( ! err )
		{	if( ! firstJob )
			{	fstPage = (*prMergeHndl)->prJob.iFstPage;
				lstPage = (*prMergeHndl)->prJob.iLstPage;
				PrJobMerge(prMergeHndl, prRecHndl);
				(*prMergeHndl)->prJob.iFstPage = (*prRecHndl)->prJob.iFstPage = fstPage;
				(*prMergeHndl)->prJob.iLstPage = (*prRecHndl)->prJob.iLstPage = lstPage;
				err = PrError();
			}
		}

		if( ! err )
		{	/* Put the defaulted/validated/jobDlg'ed print record in the doc. */
			fstPage = (*prRecHndl)->prJob.iFstPage;
			lstPage = (*prRecHndl)->prJob.iLstPage;
			copies  = (*prRecHndl)->prJob.iCopies;
			BlockMove((Ptr)(*prRecHndl), (Ptr)(*(**hE).hPrint), sizeof(TPrint));
			/* Setup the proceed/pause/cancel dialog with the document name. */
			ParamText( flnm, nil, nil, nil);
			PrintingStatusDialog = GetNewDialog( rPrStatusDlg, nil, (WindowPtr )-1 );
			if (PrintingStatusDialog)
			{
#ifndef THINK_PRE_5
				GetDItem(PrintingStatusDialog, 1, &i, (Handle *)&proceedButton, &rct);
#else
				GetDItem(PrintingStatusDialog, 1, &i, &proceedButton, &rct);
#endif
				HiliteControl(proceedButton, 255);
				/* Hook in the proceed/pause/cancel dialog. */
				(*prRecHndl)->prJob.pIdleProc = PrintIdleProc;
			}
			UseResFile( keepResFile );	/* some printers change CurResFile & we are being safe! */

			for( i = 1; i <= copies && ! err; ++i )
			{
				printPort = PrOpenDoc(prRecHndl, nil, nil);
				if ( ! ( err = PrError() ) ) eTePrintPages( hE, printPort, flnm, lstPage );
				PrCloseDoc( printPort );
			}
		}
		else if( do_init )
		{	DisposHandle( (Handle)(**hE).hPrint );
			(**hE).hPrint = NULL;
		}

		if ((!err) && ((*prRecHndl)->prJob.bJDocLoop == bSpoolLoop) && (!(err = PrError())) )
		{
			PrPicFile(prRecHndl, nil, nil, nil, &status);
			err = PrError();
		}
	}

	if (firstJob) prMergeHndl = prRecHndl;
	else		  DisposHandle( (Handle )prRecHndl );

	if( PrintingStatusDialog ) DisposDialog( PrintingStatusDialog );

	PrClose();
	SetPort(oldPort);

	return(err);
}

/*****************************************************************************/

/* PrintIdleProc will handle events in the 'Printing Status Dialog' which
** gives the user the option to 'Proceed', 'Pause', or 'Cancel' the current
** printing job during print time.
**
** The buttons:
**		1: Proceed
**		2: Pause
**		3: Cancel 
*/

pascal void		PrintIdleProc(void)
{
	Boolean				button, paused;
	ControlHandle		pauseButton, proceedButton;
    DialogPtr			aDialog;
	EventRecord			anEvent;
    GrafPtr				oldPort;
	Rect 				rct;
    short				item, itemType;

	if( ! PrintingStatusDialog ) return;
	
	GetPort( &oldPort );

#ifndef THINK_PRE_5
	GetDItem( PrintingStatusDialog, 1, &itemType, (Handle *)&proceedButton, &rct );
	HiliteControl( proceedButton, 255 );
	GetDItem( PrintingStatusDialog, 2, &itemType, (Handle *)&pauseButton, &rct );
#else
	GetDItem( PrintingStatusDialog, 1, &itemType, &proceedButton, &rct );
	HiliteControl( proceedButton, 255 );
	GetDItem( PrintingStatusDialog, 2, &itemType, &pauseButton, &rct );
#endif

	paused = false;
	do {
		if( GetNextEvent( (mDownMask + mUpMask + updateMask), &anEvent ) ) {
			if( PrintingStatusDialog != FrontWindow () )
				SelectWindow( PrintingStatusDialog );

			if ( IsDialogEvent( &anEvent ) ) {
				button = DialogSelect( &anEvent, &aDialog, &item );

				if ( (button) && (aDialog == PrintingStatusDialog) ) {
					switch (item) {
						case 1:
							HiliteControl( pauseButton, 0 );		/* Enable PAUSE    */
							HiliteControl( proceedButton, 255 );	/* Disable PROCEED */
							paused = false;
							break;
						case 2:
							HiliteControl( pauseButton, 255 );	/* Disable PAUSE  */
							HiliteControl( proceedButton, 0 );	/* Enable PROCEED */
							paused = true;
							break;
						case 3:
							PrSetError( iPrAbort );               /* CANCEL printing */
							paused = false;
							break;
					}
				}
			}
		}
	} while( paused != false ); 

	SetPort( oldPort );
}

/*****************************************************************************/

OSErr	PageSetupDialog( eRec **hE )
{
	OSErr		err;
	Boolean 	do_init;
	THPrint		prRecHndl;

	SetCursor( &qd.arrow );
	
	if( ! ( prRecHndl = (THPrint )NewHandle( sizeof( TPrint )) ) )
	{	/* If we can't generate a print record handle, we are out of here. */
		return( memFullErr );
	}
	if( (**hE).hPrint )
	{	/* Get the document's print info into the print record handle. */
		BlockMove( (Ptr )(*(**hE).hPrint), (Ptr )(*prRecHndl), sizeof( TPrint ) );
		do_init = FALSE;
	}
	else
	{	if( ! ( (**hE).hPrint = (THPrint )NewHandle( sizeof( TPrint )) ) )
		{	/* If we can't generate a print record handle, we are out of here. */
			 err = memFullErr;
			 goto pguperr;
		}
		do_init = TRUE;
	}

	PrOpen();

	if( ! (err = PrError()) )

	{	if( do_init ) PrintDefault( prRecHndl );
		else          PrValidate( prRecHndl );
		if( ! (err = PrError()) )
		{	if( PrStlDialog(prRecHndl) )
			{	BlockMove((Ptr)*prRecHndl, (Ptr)(*(**hE).hPrint), sizeof(TPrint));
				/* (*frHndl)->doc.printRecValid  = true; */
				/* (*frHndl)->fileState.docDirty = true; */
			}
			else
			{	err = userCanceledErr;
				if( do_init )
				{	DisposHandle((Handle)(**hE).hPrint);
					(**hE).hPrint = NULL;
				}
			}
		}
	}
	PrClose();

pguperr:
	DisposHandle( (Handle)prRecHndl );
	return( err );
}

/* end of os_mac_Print.c */
