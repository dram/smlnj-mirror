/****************************************************************************
* Copyright (c) 1991 by:     Department of Computer Science
*                            The Technical University of Denmark
*                            DK-2800 Lyngby
*
* 20 Dec. 1991     Yngvi Skaalum Guttesen
*/

#include "sml.h"
#include "ml_types.h"

HANDLE hInst;
MSG msg;
HWND		hWnd;
PAINTSTRUCT     ps;
TEXTMETRIC      tm;
CATCHBUF    CatchBuf;
int	    exit_sml = FALSE;

#define nrArgs 10
char argvBuf[128];
LPSTR argv[nrArgs];

int console_io(int, LPSTR, int);
void init_buf(void);

int PASCAL WinMain(hInstance, hPrevInstance, lpCmdLine, nCmdShow)
HANDLE hInstance;
HANDLE hPrevInstance;
LPSTR lpCmdLine;
int nCmdShow;
{
    int i,j;

    copy_params(lpCmdLine);
    init_buf();

    if (!hPrevInstance)			 /* Other instances of app running? */
        if (!InitApplication(hInstance)) /* Initialize shared things        */
            return (FALSE);              /* Exits if unable to initialize   */

    /* Perform initializations that apply to a specific instance */

    if (!InitInstance(hInstance, nCmdShow))
        return (FALSE);


    if (!InitSegments())
        return (FALSE) ;

    if (Catch((LPCATCHBUF) &CatchBuf) == 0)
        run(argv) ;
    else {
        FreeSegments();
        return (msg.wParam);           /* Returns the value from PostQuitMessage */
    }
}



/****************************************************************************

    FUNCTION: InitApplication(HANDLE)

    PURPOSE: Initializes window data and registers window class

    COMMENTS:

        This function is called at initialization time only if no other 
        instances of the application are running.  This function performs 
        initialization tasks that can be done once for any number of running 
        instances.  

        In this case, we initialize a window class by filling out a data 
        structure of type WNDCLASS and calling the Windows RegisterClass() 
        function.  Since all instances of this application use the same window 
        class, we only need to do this when the first instance is initialized.  


****************************************************************************/

BOOL InitApplication(hInstance)
HANDLE hInstance;			       /* current instance	     */
{
    WNDCLASS  wc;

    /* Fill in window class structure with parameters that describe the       */
    /* main window.                                                           */

    wc.style = NULL;                    /* Class style(s).                    */
    wc.lpfnWndProc = MainWndProc;       /* Function to retrieve messages for  */
                                        /* windows of this class.             */
    wc.cbClsExtra = 0;                  /* No per-class extra data.           */
    wc.cbWndExtra = 0;                  /* No per-window extra data.          */
    wc.hInstance = hInstance;           /* Application that owns the class.   */
    wc.hIcon = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor = LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground = GetStockObject(WHITE_BRUSH); 
    wc.lpszMenuName =  "SmlMenu";   /* Name of menu resource in .RC file. */
    wc.lpszClassName = "SmlWClass"; /* Name used in call to CreateWindow. */

    /* Register the window class and return success/failure code. */

    return (RegisterClass(&wc));

}


/****************************************************************************

    FUNCTION:  InitInstance(HANDLE, int)

    PURPOSE:  Saves instance handle and creates main window

    COMMENTS:

        This function is called at initialization time for every instance of 
        this application.  This function performs initialization tasks that 
        cannot be shared by multiple instances.  

        In this case, we save the instance handle in a static variable and 
        create and display the main program window.  
        
****************************************************************************/

BOOL InitInstance(hInstance, nCmdShow)
    HANDLE          hInstance;          /* Current instance identifier.       */
    int             nCmdShow;           /* Param for first ShowWindow() call. */
{

    /* Save the instance handle in static variable, which will be used in  */
    /* many subsequence calls from this application to Windows.            */

    hInst = hInstance;

    /* Create a main window for this application instance.  */

    hWnd = CreateWindow(
        "SmlWClass",                    /* See RegisterClass() call.          */
        "Standard ML (DTH)",            /* Text for window title bar.         */
        WS_OVERLAPPEDWINDOW,            /* Window style.                      */
        CW_USEDEFAULT,                  /* Default horizontal position.       */
        CW_USEDEFAULT,                  /* Default vertical position.         */
        CW_USEDEFAULT,                  /* Default width.                     */
        CW_USEDEFAULT,                  /* Default height.                    */
        NULL,                           /* Overlapped windows have no parent. */
        NULL,                           /* Use the window class menu.         */
        hInstance,                      /* This instance owns this window.    */
        NULL                            /* Pointer not needed.                */
    );

    /* If window could not be created, return "failure" */

    if (!hWnd)
        return (FALSE);

    /* Make the window visible; update its client area; and return "success" */

    ShowWindow(hWnd, nCmdShow);  /* Show the window                        */
    UpdateWindow(hWnd);          /* Sends WM_PAINT message                 */
    return (TRUE);               /* Returns the value from PostQuitMessage */

}


WORD      wsUse32Data;  /* the selector of the USE32 data segment (ML-heap)  */
WORD      wsUse32Code;  /* the selector of the USE32 code segment (ML-code)  */
DWORD     dwDataAlias;  /* the USE16 alias of wsUse32Data                    */
_segment  runtime_seg;  /* the segment of dwDataAlias.                       */
                        /* runtime_seg is used with based pointers           */

extern long _far RUNCODE_START ;
extern long _far RUNCODE_END   ;

long edata ;
long Use32HeapSize ;

                         /* simulates the UNIX &edata for the end of  */
                         /* the initialized data region               */

int InitSegments(void)
{
    LPWORD  lpUse32Data  = &wsUse32Data,
            lpUse32Code  = &wsUse32Code ;

    LPDWORD lpDataAlias  = &dwDataAlias;

    long _based(runtime_seg)  *lpp ;
    long _based(runtime_seg)  *lps;
    long _far *lpq;

    /* Hard limit        = 0x1000000 = 16M   */
    /* Initial heap size = Use32HeapSize     */

    edata = (LOWORD(&RUNCODE_END) + 3) & (~3) ;
    Use32HeapSize = edata ;


    if (Global32Alloc(0x1000000L,&wsUse32Data,0x1000000L,0) != 0)
        return (FALSE) ;

    if (Global32CodeAlias(wsUse32Data,&wsUse32Code,0) != 0)
        return(FALSE) ;

    /* create a 16k alias for the runtime sytem */

    if (Global16PointerAlloc(wsUse32Data,0,&dwDataAlias,edata,0) != 0)
        return (FALSE) ;

    runtime_seg = HIWORD(dwDataAlias) ;

    /* Move the RUNTIME segment to the USE32 heap. Must be less than 16k */


    lpp = &RUNCODE_START ;
    lps = &RUNCODE_END ;
    lpq = &RUNCODE_START ;

    for ( ; lpp < lps ; *lpp++ = *lpq++ )
        ;


    Use32HeapSize = edata = 64L*1024L ;

    return (TRUE) ;

}

void FreeSegments(void)
{
    Global16PointerFree(wsUse32Data,dwDataAlias,0);
    Global32CodeAliasFree(wsUse32Data, wsUse32Code, 0);
    Global32Free(wsUse32Data, 0);
}
void msg_loop(void)
{
    while (PeekMessage(&msg, NULL, NULL, NULL, PM_REMOVE)) {
	TranslateMessage(&msg);
	DispatchMessage(&msg);
    }
}

#define XBUFSIZE 80	 /* size of the io-window    Cols */
#define YBUFSIZE 32	 /*			     Rows */
#define BUF(x,y) winBuffer[y][x]



char	winBuffer[YBUFSIZE][XBUFSIZE+1] ; /* the io-buffer */
int	curline = 0;	 /* the current inputline in the buffer */

int cxChar, cyChar,     /* size of the chars */
    cxClient, cyClient, /* size of the window */
    cxBuffer, cyBuffer, /* vizible portion of the buffer */
    xCaret, yCaret;     /* the cursor position           */

int reading = FALSE;
int ch_read;

void init_buf(void)
{   int i;
    for (i=0 ; i<YBUFSIZE; i++)
	BUF(0,i) = '\n' ;
}

int console_write(LPSTR buf, int nbytes)
{
    HDC         hdc;
    int         i,j;

    HideCaret(hWnd);
    hdc = GetDC(hWnd);
    SelectObject(hdc, GetStockObject(SYSTEM_FIXED_FONT));

    for(i=0 ; i< nbytes ; i++) {
        switch (buf[i]) {
            case '\n' :
		    BUF(xCaret,curline) = '\n';
                    xCaret = 0;
		    if (++curline >= YBUFSIZE)
			curline = 0;
		    BUF(xCaret,curline) = '\n';
		    ScrollWindow(hWnd, 0, -cyChar, NULL, NULL);
		    for(j=0 ; j< cxBuffer; j++)
			TextOut(hdc,j*cxChar,yCaret*cyChar," ",1);
		    continue;

	    case '\t' :
		    do {
			BUF(xCaret,yCaret) = ' ';
			TextOut(hdc, xCaret*cxChar, yCaret*cyChar," ",1);
			++xCaret;
		    } while (((xCaret % 4) !=0) && xCaret<cxBuffer) ;
		    BUF(xCaret,yCaret) = '\n';
                    continue;

            default:
		    if (xCaret>=cxBuffer) {
			BUF(xCaret,curline) = '\n' ;
			if (++curline == YBUFSIZE) {
			    curline = 0;
			}
			ScrollWindow(hWnd, 0, -cyChar, NULL, NULL);
			for(j=0 ; j< cxBuffer; j++)
			    TextOut(hdc,j*cxChar,yCaret*cyChar," ",1);
			xCaret = 0;
                    }
		    BUF(xCaret, curline) = buf[i];
		    TextOut(hdc, xCaret++*cxChar, yCaret*cyChar, &buf[i], 1);
		    BUF(xCaret, curline) = '\n';
                    continue;
        }
    }
    ReleaseDC(hWnd, hdc);
    ShowCaret(hWnd);
    SetCaretPos(xCaret*cxChar,yCaret*cyChar);
    ValidateRect(hWnd,NULL);
    return nbytes;
}

int console_read(LPSTR buf, int nbytes)
{
    int i;
    int lastpos = xCaret;
    int lastline = curline;

    reading = TRUE;
    ch_read = 0;
    while (reading && GetMessage(&msg, NULL, NULL, NULL)) {
	TranslateMessage(&msg);
	DispatchMessage(&msg);
    }

    if (reading)
	Throw((LPCATCHBUF) &CatchBuf, 1) ;

    for (i=0 ; (i<ch_read) && (i<nbytes) ; i++)
	buf[i] = BUF(i+lastpos,lastline);

    return i;
}

int console_io(int fd, LPSTR buf, int nbytes)
{
    if (fd == 0)
        return console_read(buf, nbytes) ;
    else
        return console_write(buf, nbytes) ;
}

long FAR PASCAL MainWndProc(hWnd, message, wParam, lParam)
HWND hWnd;
unsigned message;
WORD wParam;
LONG lParam;
{
    FARPROC lpProcAbout;
    HDC     hdc;
    int i,x,y,z;

    switch (message) {
	case WM_COMMAND:
	    if (wParam == IDM_ABOUT) {
		lpProcAbout = MakeProcInstance(About, hInst);

		DialogBox(hInst,		 /* current instance	     */
		    "AboutBox",			 /* resource to use	     */
		    hWnd,			 /* parent handle	     */
		    lpProcAbout);		 /* About() instance address */

		FreeProcInstance(lpProcAbout);
                return 0;
	    }
            else                            /* Lets Windows process it       */
		return (DefWindowProc(hWnd, message, wParam, lParam));

	case WM_DESTROY:		  /* message: window being destroyed */
	    PostQuitMessage(0);
	    exit_sml = TRUE;
            return 0;

        case WM_SETFOCUS:
            CreateCaret(hWnd, NULL, cxChar, cyChar);
            SetCaretPos(xCaret*cxChar, yCaret*cyChar);
            ShowCaret(hWnd);
            return 0;

        case WM_KILLFOCUS:
            HideCaret(hWnd);
            DestroyCaret();
            return 0;

        case WM_CREATE:
            hdc = GetDC(hWnd);
            SelectObject(hdc, GetStockObject(SYSTEM_FIXED_FONT));
            GetTextMetrics(hdc, &tm);
            cxChar = tm.tmAveCharWidth;
            cyChar = tm.tmHeight + tm.tmExternalLeading;
            ReleaseDC(hWnd, hdc);
            xCaret = 0;
            return 0;

        case WM_SIZE :
            cxClient = LOWORD(lParam);
            cyClient = HIWORD(lParam);
	    cxBuffer = min(XBUFSIZE, max(1, cxClient/cxChar));
	    cyBuffer = min(YBUFSIZE, max(1, cyClient/cyChar));
            yCaret = cyBuffer - 1 ;
            if (hWnd = GetFocus())
                SetCaretPos(xCaret*cxChar, yCaret*cyChar);
            InvalidateRect(hWnd, NULL, TRUE);
            return 0 ;

        case WM_PAINT :
            hdc = BeginPaint(hWnd, &ps);
	    SelectObject(hdc, GetStockObject(SYSTEM_FIXED_FONT));
	    if ((y = curline-cyBuffer+1)<0)
		y += YBUFSIZE;
	    for (i=0; i<cyBuffer ; i++) {
		for (x=0; (x<cxBuffer) && (BUF(x,y) != '\n'); ++x)
		    TextOut(hdc, x*cxChar, i*cyChar, &BUF(x,y), 1);
		if (++y>= YBUFSIZE)
		    y=0;
            }
            EndPaint(hWnd, &ps);
            return 0;

        case WM_CHAR:
            if (!reading)
                return 0;
            switch (wParam)
                {
                case '\b' :
		    if (ch_read>0)
                        {
			--xCaret;
			--ch_read;
			BUF(xCaret,curline) = '\n';
                        HideCaret(hWnd);
                        hdc = GetDC(hWnd);
                        SelectObject(hdc, GetStockObject(SYSTEM_FIXED_FONT));
                        TextOut(hdc, xCaret*cxChar, yCaret*cyChar, " ", 1);
                        ShowCaret(hWnd);
                        ReleaseDC(hWnd,hdc);
                        }
		    break;

                case '\t' :
                    do
                        SendMessage(hWnd, WM_CHAR, ' ', 1L);
                    while (xCaret % 4 != 0);
                    break;
                case '\n' :
                    break;

		case '\r' :
		    reading = FALSE;
		    ++ch_read;
		    BUF(xCaret,curline) = '\n' ;
                    xCaret = 0;
		    if (++curline >= YBUFSIZE)
			curline = 0;
		    BUF(xCaret,curline) = '\n' ;
		    HideCaret(hWnd);
		    hdc = GetDC(hWnd);
		    SelectObject(hdc, GetStockObject(SYSTEM_FIXED_FONT));
		    ScrollWindow(hWnd, 0 , -cyChar, NULL, NULL) ;
		    for(i=0 ; i< cxBuffer; i++)
			TextOut(hdc,i*cxChar,yCaret*cyChar," ",1);
		    ShowCaret(hWnd);
		    ReleaseDC(hWnd,hdc);
		    ValidateRect(hWnd,NULL);
		    break;
		default :
		    if (xCaret >= cxBuffer)
			return 0;
		    ++ch_read;
		    BUF(xCaret, curline) = (char) wParam;
                    HideCaret (hWnd);
                    hdc = GetDC(hWnd);
                    SelectObject(hdc, GetStockObject(SYSTEM_FIXED_FONT));
                    TextOut(hdc, xCaret*cxChar, yCaret*cyChar,
			    &BUF(xCaret, curline), 1);
		    ++xCaret;
                    ShowCaret(hWnd);
                    ReleaseDC(hWnd,hdc);
                    break;
                }
            SetCaretPos(xCaret*cxChar, yCaret*cyChar);
	    return 0;

	default:
	    return (DefWindowProc(hWnd, message, wParam, lParam));
    }
}

BOOL FAR PASCAL About(hDlg, message, wParam, lParam)
HWND hDlg;
unsigned message;
WORD wParam;
LONG lParam;
{
    switch (message) {
	case WM_INITDIALOG:
	    return (TRUE);

	case WM_COMMAND:
	    if (wParam == IDOK
		|| wParam == IDCANCEL) {
		EndDialog(hDlg, TRUE);
		return (TRUE);
	    }
	    break;
    }
    return (FALSE);
}


copy_params(LPSTR lp)
{
    int i = 0;

    lstrcpy(argvBuf, lp);
    lp = argvBuf;

    while (*lp != 0) {
        while(isspace(*lp))
            lp++;
        if (isprint(*lp) && !isspace(*lp))
            argv[i++] = lp;
        while (isprint(*lp) && !isspace(*lp))
            lp++;
        if (*lp != 0)
            *lp++ = 0;
    }
    while (i<nrArgs)
        argv[i++] = NULL;
}
