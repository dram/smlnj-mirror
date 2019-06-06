/*! \file win32-filesys.c
 *
 * Interface to win32 filesys functions
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include <windows.h>
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"

#define TMP_PREFIX "TMP-SMLNJ"

#define IS_DOTDIR(c) ((c)[0] == '.' && (!(c)[1] || ((c)[1] == '.' && !(c)[2])))

static WIN32_FIND_DATA wfd;

static ml_val_t find_next_file (ml_state_t *msp, HANDLE h)
{
    ml_val_t fname_opt,fname;

  loop:
    if (FindNextFile(h,&wfd)) {
	if (IS_DOTDIR(wfd.cFileName))
	  /* skip "." and ".." */
	    goto loop;
	fname = ML_CString(msp,wfd.cFileName);
	OPTION_SOME(msp,fname_opt,fname);
    }
    else {
        fname_opt = OPTION_NONE;
    }
    return fname_opt;
}

/* _ml_win32_FS_find_next_file : c_pointer -> (string option)
 */
ml_val_t _ml_win32_FS_find_next_file (ml_state_t *msp, ml_val_t arg)
{
    return find_next_file(msp, HANDLE_MLtoC(arg));
}

/* _ml_win32_FS_find_first_file : string -> (c_pointer * string option)
 */
ml_val_t _ml_win32_FS_find_first_file (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h = FindFirstFile(STR_MLtoC(arg), &wfd);
    ml_val_t fname_opt, fname, ml_h, res;

    if (h != INVALID_HANDLE_VALUE) {
	if (IS_DOTDIR(wfd.cFileName)) {
	    fname_opt = find_next_file(msp, h);
	}
	else {
	    fname = ML_CString(msp, wfd.cFileName);
	    OPTION_SOME(msp, fname_opt, fname);
	}
    }
    else {
	fname_opt = OPTION_NONE;
    }

    ml_h = HANDLE_CtoML(msp,h);
    REC_ALLOC2(msp, res, ml_h, fname_opt);

    return res;
}

/* _ml_win32_FS_find_close : c_pointer -> bool
 */
ml_val_t _ml_win32_FS_find_close (ml_state_t *msp, ml_val_t arg)
{
    return FindClose(HANDLE_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_set_current_directory : string -> bool
 */
ml_val_t _ml_win32_FS_set_current_directory (ml_state_t *msp, ml_val_t arg)
{
    return SetCurrentDirectory(STR_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_get_current_directory : unit -> string
 */
ml_val_t _ml_win32_FS_get_current_directory (ml_state_t *msp, ml_val_t arg)
{
    char buf[MAX_PATH];
    DWORD r = GetCurrentDirectory(MAX_PATH, buf);

    if (r == 0 || r > MAX_PATH) {
        return RAISE_SYSERR(msp, -1);
    }
    else {
        return ML_CString(msp,buf);
    }
}


/* _ml_win32_FS_create_directory : string -> bool
 */
ml_val_t _ml_win32_FS_create_directory (ml_state_t *msp, ml_val_t arg)
{
    return CreateDirectory(STR_MLtoC(arg),NULL) ? ML_true : ML_false;
}

/* _ml_win32_FS_remove_directory : string -> bool
 */
ml_val_t _ml_win32_FS_remove_directory (ml_state_t *msp, ml_val_t arg)
{
    return RemoveDirectory(STR_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_get_file_attributes : string -> (word32 option)
 */
ml_val_t _ml_win32_FS_get_file_attributes (ml_state_t *msp, ml_val_t arg)
{
    DWORD w = GetFileAttributes(STR_MLtoC(arg));
    ml_val_t res, ml_w;

    if (w != INVALID_FILE_ATTRIBUTES) {
#ifdef DEBUG_WIN32
        SayDebug("get_file_attributes: returning SOME %#x as attrs for <%s>\n",
	    w, STR_MLtoC(arg));
#endif
	ml_w = INT32_CtoML(msp, w);
	OPTION_SOME(msp,res,ml_w);
    }
    else {
#ifdef DEBUG_WIN32
        SayDebug("get_file_attributes: returning NONE as attrs for <%s>; error = %d\n",
	    STR_MLtoC(arg), GetLastError());
#endif
        res = OPTION_NONE;
    }
    return res;
}

/* _ml_win32_FS_get_file_attributes_by_handle : c_pointer -> (word32 option)
 */
ml_val_t _ml_win32_FS_get_file_attributes_by_handle (ml_state_t *msp, ml_val_t arg)
{
    BY_HANDLE_FILE_INFORMATION bhfi;
    ml_val_t ml_w, res;

    if (GetFileInformationByHandle(HANDLE_MLtoC(arg), &bhfi)) {
	ml_w = INT32_CtoML(msp, bhfi.dwFileAttributes);
	OPTION_SOME(msp,res,ml_w);
    }
    else {
	res = OPTION_NONE;
    }
    return res;
}

/* _ml_win32_FS_get_full_path_name : string -> string
 */
ml_val_t _ml_win32_FS_get_full_path_name (ml_state_t *msp, ml_val_t arg)
{
    char buf[MAX_PATH], *dummy;
    DWORD r;
    ml_val_t res;

    r = GetFullPathName(STR_MLtoC(arg), MAX_PATH, buf, &dummy);
    if ((r == 0) || (r > MAX_PATH)) {
	return RAISE_SYSERR(msp, -1);
    }
    res = ML_CString(msp, buf);
    return res;
}

/* _ml_win32_FS_get_file_size : c_pointer -> word64
 */
ml_val_t _ml_win32_FS_get_file_size (ml_state_t *msp, ml_val_t arg)
{
    LARGE_INTEGER sz;

    if (GetFileSizeEx(HANDLE_MLtoC(arg), &sz)) {
	return ML_AllocInt64(msp, sz.QuadPart);
    }
    else {
	return RAISE_SYSERR(msp, -1);
    }
}

/* _ml_win32_FS_get_file_size_by_name : string -> (word64 option)
 */
ml_val_t _ml_win32_FS_get_file_size_by_name (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h;

    h = CreateFile (
	STR_MLtoC(arg), 0, 0, NULL,
	OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, INVALID_HANDLE_VALUE);

    if (h != INVALID_HANDLE_VALUE) {
	ml_val_t res = OPTION_NONE;
	LARGE_INTEGER sz;

	if (GetFileSizeEx(h, &sz)) {
	    ml_val_t ml_sz = ML_AllocInt64(msp, sz.QuadPart);
	    OPTION_SOME(msp, res, ml_sz);
	}

	CloseHandle(h);

	return res;
    }
    else {
	return OPTION_NONE;
    }

}

/* _ml_win32_FS_get_file_time : string -> Int64.int option
 */
ml_val_t _ml_win32_FS_get_file_time (ml_state_t *msp, ml_val_t arg)
{
    HANDLE h;
    ml_val_t ml_ns, res;

    h = CreateFile(
	    STR_MLtoC(arg), 0, 0, NULL,
	    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, INVALID_HANDLE_VALUE);

    if (h != INVALID_HANDLE_VALUE) {
	FILETIME ft;
	if (GetFileTime(h, NULL, NULL, &ft)) {  /* request time of "last write" */
	  /* convert to nanoseconds; FILETIME is in units of 100ns */
	    Int64_t ns = 100 * (((Int64_t)ft.dwHighDateTime << 32) + (Int64_t)ft.dwLowDateTime);
	    ml_ns = ML_AllocInt64(msp, ns);
	    OPTION_SOME(msp, res, ml_ns);
	}
    } else {
	res = OPTION_NONE;
    }
    return res;
}

/* _ml_win32_FS_set_file_time : (string * Word64.int option) -> bool
 */
ml_val_t _ml_win32_FS_set_file_time (ml_state_t *msp, ml_val_t arg)
{
    HANDLE		h;
    ml_val_t		res = ML_false;
    ml_val_t		fname = REC_SEL(arg,0);
    Unsigned64_t	ns = WORD64_MLtoC(REC_SEL(arg,1));

    h = CreateFile (
	    STR_MLtoC(fname), GENERIC_WRITE, 0 ,NULL,
	    OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, INVALID_HANDLE_VALUE);

    if (h != INVALID_HANDLE_VALUE) {
	FILETIME ft;

	ns /= 100;  /* FILETIME is in units of 100ns */
	ft.dwHighDateTime = (DWORD)(ns >> 32);
	ft.dwLowDateTime = (DWORD)ns;

	if (SetFileTime(h, NULL, NULL, &ft)) {
	    res = ML_true;
	}

	CloseHandle (h);
    }

    return res;
}

/* _ml_win32_FS_delete_file : string->bool
 */
ml_val_t _ml_win32_FS_delete_file (ml_state_t *msp, ml_val_t arg)
{
    return DeleteFile (STR_MLtoC(arg)) ? ML_true : ML_false;
}

/* _ml_win32_FS_move_file : (string * string)->bool
 */
ml_val_t _ml_win32_FS_move_file (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	f1 = REC_SEL(arg, 0);
    ml_val_t	f2 = REC_SEL(arg, 1);

    if (MoveFile (STR_MLtoC(f1), STR_MLtoC(f2)))
	return ML_true;
    else
	return ML_false;
}

/* _ml_win32_FS_get_temp_file_name : unit -> string option
 */
ml_val_t _ml_win32_FS_get_temp_file_name (ml_state_t *msp, ml_val_t arg)
{
    ml_val_t	res = OPTION_NONE;
    char	name_buf[MAX_PATH];
    char	path_buf[MAX_PATH];
    DWORD	pblen;

    pblen = GetTempPath(MAX_PATH, path_buf);
    if ((pblen <= MAX_PATH) && (GetTempFileName(path_buf, TMP_PREFIX, 0, name_buf) != 0)) {
	ml_val_t tfn = ML_CString(msp, name_buf);

	OPTION_SOME(msp, res, tfn);
    }
    return res;
}

/* end of win32-filesys.c */
