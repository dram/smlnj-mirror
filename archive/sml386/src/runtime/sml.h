/*************************************************************************
* Copyright (c) 1991 by:     Department of Computer Science
*                            The Technical University of Denmark
*                            DK-2800 Lyngby
*
* 20 Dec. 1991     Yngvi Skaalum Guttesen
*/

#ifndef _SML_

#define _SML_

#include <windows.h>                /* required for all Windows applications */
#include <winmem32.h>
#include <dos.h>

#define IDM_ABOUT 100

int PASCAL WinMain(HANDLE, HANDLE, LPSTR, int);
BOOL InitApplication(HANDLE);
BOOL InitInstance(HANDLE, int);
long FAR PASCAL MainWndProc(HWND, unsigned, WORD, LONG);
BOOL FAR PASCAL About(HWND, unsigned, WORD, LONG);
int InitSegments(void);
void FreeSegments(void);

extern WORD    wsUse32Data;
extern WORD    wsUse32Code;

extern DWORD   dwDataAlias;

extern _segment runtime_seg;

#define BRSP ML_val_t _based(runtime_seg) *

#define EXTASM(x)                                \
    extern ML_val_t _far x##_a ;              \
    ML_val_t _based(runtime_seg) *x = &x##_a ;

#endif
