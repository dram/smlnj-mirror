/* os_mac_console.c
 * 15Aug92  e
 *
 * The following functions are needed for console support...
 *  __console_options (boot.c)
 *  __cecho2file (ANSI Tricia ¹)
 *  __cecho2printer (ANSI Tricia ¹)
 *  __open_std (ANSI Tricia ¹)
 *  __cshow (ANSI Tricia ¹)
 * 
 * and were shamelessly stolen from Symantec's console.c
 * so that the eEdit code could be interfaced with the ANSI library
 * so...
 * portions Copyright © e 1992. All rights reserved.
 * portions Copyright (c) 1991 Symantec Corporation.  All rights reserved.
 */

#include <MacHeaders>
#include <console.h>
#include <stddef.h>
#include <stdlib.h>
#include <signal.h>
#include <errno.h>
#include <ansi_private.h>

#include "os_mac.h"

extern short interrupted;	/* from os_mac.c */

struct __copt console_options = { 50, 10, "\pconsole", 8, 4, 9, 0, 25, 80, 1 };

/* closeecho - close echo file (if any) */
/* unimplemented; was...
static void closeecho(void)
{
	if (c.echo2fp)
	{	if (c.spool)
			print_console();
		fclose(c.echo2fp);
	}
}
*/

/* cecho2file - echo console display to file */

void cecho2file(char *s, int append, FILE *fp)
{
	/* unimplemented; was...
	struct save save;
	setup(cflush(fp), &save);
	closeecho();
	c.echo2fp = fopen(s, append ? "a" : "w");
	c.spool = 0;
	restore(&save);
	*/
}

/* cecho2printer - echo console display to printer */

void cecho2printer(FILE *fp)
{
	/* unimplemented; was...
	struct save save;
	setup(cflush(fp), &save);
	closeecho();
	c.echo2fp = tmpfile();
	c.spool = 1;
	restore(&save);
	*/
}

/* cflush - flush all pending output to a console */

static WindowPeek cflush(FILE *fp)
{
	WindowPeek wp = __checkfile(fp)->window;
	int n;
	for (fp = &__file[0], n = FOPEN_MAX; n--; fp++) {
		if (fp->dirty && fp->window == wp)
			fflush(fp);
	}
	return(wp);
}

/* cshow - show a console window
 *
 *  All pending output to the window is forced to appear. */

void cshow(FILE *fp)
{
	WindowPeek wp = cflush(fp);
	if (wp != (WindowPeek) FrontWindow())
		SelectWindow(wp);
	ShowWindow(wp);
}

/*  __open_std - open the std streams
 *
 *  This is called automatically (by "__checkfile") whenever an
 *  unopened std stream is referenced. */

void
__open_std(void)
{
	FILE *fp = NULL;
	char buf[40];
	
	if (stdin->std)
		fp = freopenc(fp, stdin);
	if (stdout->std)
		fp = freopenc(fp, stdout);
	if (stderr->std)
		fp = freopenc(fp, stderr);
	/* unimplemented; was...
	if (__log_stdout)
	{	sprintf(buf, "%#s.log", CurApName);
		cecho2file(buf, 1, stdout);
		console_options.pause_atexit = 0;
	}
	*/
}

/* consoleio - I/O handler proc for console windows */

static int consoleio(FILE *fp, int i)
{
	int result = 0;
	long  old_cnt = fp->cnt; /* 15Sep92  e */
	void *old_ptr = fp->ptr; /* 15Sep92  e */
	if (_abnormal_exit)
		return(0);
	switch (i)
	{	case 0:				/*  read  */
consoleio_read_again:
			cshow(fp);
			fp->eof = 0;
			if ((fp->cnt = os_console_read(fp->window, fp->ptr, fp->cnt)) == 0)
			{	fp->eof = 1;
				result = EOF; /* why EOF !?? */
			}
			break;
		case 1:				/*  write  */
			cshow(fp);
			os_console_write(fp->window, fp->ptr, fp->cnt);
			break;
		case 2:				/*  close  */
			os_console_close(fp->window);
			break;
	}
	if (interrupted)
	{	interrupted = 0;
		FlushEvents(keyDownMask, 0);
		fp->cnt = 0;
		raise(SIGINT);
		/* 15Sep92  e */
		if( i == 0 )
		{ fp->cnt = old_cnt;
		  fp->ptr = old_ptr;
		  result = 0;
		  goto consoleio_read_again;
		}
		else
		{ errno = EINTR;
		  result = EOF; /* why EOF !?? */
		}
		/* */
	}
	return(result);
}

/* console_exit - console shutdown routine */

static void console_exit(void)
{	register FILE *fp;
	int n;
	/*  complete pending output  */
	for (fp = &__file[0], n = FOPEN_MAX; n--; fp++)
	{	if (fp->dirty && fp->window)
			fflush(fp);
	}
	/*  pause for user acknowledgement  */
	/* not implemented...
	if (console_environment && console_options.pause_atexit)
	{	for (fp = &__file[0], n = FOPEN_MAX; n--; fp++)
		{	if (fp->window)
			{	SetWTitle(fp->window, "\ppress ÇreturnÈ to exit");
				c.raw = c.cbreak = c.edit = 0;
				setbuf(fp, NULL);
				fgetc(fp);
				break;
			}
		}
	}
	*/
	/*  close consoles  */
	for (fp = &__file[0], n = FOPEN_MAX; n--; fp++)
	{	if (fp->window)
			fclose(fp);
	}
}

/*  freopenc - reopen a stream on a new or existing console
 *
 *  "fp" is closed, if necessary, then opened as a console.  If "fp2"
 *  is NULL, a new console is created; otherwise "fp2" must refer to
 *  a console, and "fp" is made to refer to the same console. */

FILE *freopenc(FILE *fp2, FILE *fp)
{
	if (fp == NULL)
		return(NULL);
	/* if (WWExist)  ???
		InitConsole(); */
	fclose(fp);
	fp->refnum = -1;
	fp->window = fp2 ? fp2->window : os_console_new( console_options.title );
	setvbuf(fp, NULL, _IOLBF, BUFSIZ);
	fp->proc = consoleio;
	__atexit_console(console_exit);
	return(fp);
}

/* end of os_mac_console.c */
