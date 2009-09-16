/* win32-process.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 *
 * interface to win32 process functions
 */

#include <windows.h>
#include <process.h>
#include <stdlib.h>
#include <shlwapi.h>

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"


/* _ml_win32_PS_create_process : string -> word32
 * 
 * Note: This function returns the handle to the created process
 *       This handle will need to be freed before the system releases
 *       the memory associated to the process.
 *       We will take care of this in the wait_for_single_object
 *       call. This is for the time being only used by CML.
 *       It could also cause problems later on.
 */
ml_val_t _ml_win32_PS_create_process_internal(ml_state_t *msp, ml_val_t arg, STARTUPINFO *pStartup)
{
  char *str = STR_MLtoC(arg);
  PROCESS_INFORMATION pi;
  STARTUPINFO si;
  ml_val_t res;
  BOOL fSuccess;
  ZeroMemory (&si,sizeof(si));
  si.cb = sizeof(si);

  if (pStartup == NULL) {
    pStartup = &si;
  }
  fSuccess = CreateProcess (NULL,str,NULL,NULL,TRUE,CREATE_NEW_CONSOLE,NULL,NULL,pStartup,&pi);
  if (fSuccess) {
    HANDLE hProcess = pi.hProcess;
    CloseHandle (pi.hThread);
    WORD_ALLOC (msp,res,(Word_t)hProcess);
    return res;
  }
  WORD_ALLOC (msp,res,(Word_t)0);
  return res;
}

ml_val_t _ml_win32_PS_create_process(ml_state_t *msp, ml_val_t arg)
{
    return _ml_win32_PS_create_process_internal(msp, arg, NULL);
}

ml_val_t _ml_win32_PS_create_process_redirect_handles(ml_state_t *msp, ml_val_t arg)
{
    SECURITY_ATTRIBUTES sa;
    SECURITY_DESCRIPTOR sd;               //security information for pipes
    STARTUPINFO si;
    HANDLE hStdoutRd, hStdoutWr, hStdinRd, hStdinWr = NULL;
    ml_val_t res,procHandle,in,out;

    InitializeSecurityDescriptor(&sd,SECURITY_DESCRIPTOR_REVISION);
    SetSecurityDescriptorDacl(&sd, TRUE, NULL, FALSE);
    sa.lpSecurityDescriptor = &sd;
    sa.nLength = sizeof(SECURITY_ATTRIBUTES);
    sa.bInheritHandle = TRUE;

    ZeroMemory(&si, sizeof(si));
    si.cb = sizeof(si);

    // Create a pipe for the child process's STDOUT. 
    if (!CreatePipe(&hStdoutRd, &hStdoutWr, &sa, 0)) 
        return RAISE_SYSERR(msp,-1);

    // Create a pipe for the child process's STDIN. 
    if (!CreatePipe(&hStdinRd, &hStdinWr, &sa, 0)) 
        return RAISE_SYSERR(msp,-1);

    si.dwFlags = STARTF_USESTDHANDLES|STARTF_USESHOWWINDOW;
    si.wShowWindow = SW_HIDE;
    si.hStdInput = hStdinRd; // The child process READS from this
    si.hStdOutput = si.hStdError = hStdoutWr; // And it WRITES to this one

    procHandle = _ml_win32_PS_create_process_internal(msp, arg, &si);
    WORD_ALLOC(msp, in, (Word_t)hStdoutRd);
    WORD_ALLOC(msp, out, (Word_t)hStdinWr);
    REC_ALLOC3(msp, res, procHandle, in, out);
    return res;
}

ml_val_t _ml_win32_PS_wait_for_single_object(ml_state_t *msp, ml_val_t arg)
{
  HANDLE hProcess = (HANDLE) WORD_MLtoC (arg);
  DWORD exit_code;
  int res;
  ml_val_t p,obj;
  res = WaitForSingleObject (hProcess,0);
  if (res==WAIT_TIMEOUT || res==WAIT_FAILED) {
    /* information is not ready, or error */
    obj = OPTION_NONE;
  }
  else { 
    /* WAIT_OBJECT_0 ... done, finished */
    /* get info and return SOME(exit_status) */
    GetExitCodeProcess (hProcess,&exit_code);
    CloseHandle (hProcess);   /* decrease ref count */
    WORD_ALLOC (msp,p,(Word_t)exit_code);
    OPTION_SOME(msp,obj,p);
  }
  return obj;
}  
    

/* _ml_win32_PS_system : string -> word32
 *                       command
 *
 */
ml_val_t _ml_win32_PS_system(ml_state_t *msp, ml_val_t arg)
{
  int ret = system(STR_MLtoC(arg));
  ml_val_t res;

  WORD_ALLOC(msp, res, (Word_t)ret);
  return res;
}

/* _ml_win32_PS_exit_process : word32 -> 'a
 *                             exit code
 *
 */
void _ml_win32_PS_exit_process(ml_state_t *msp, ml_val_t arg)
{
  ExitProcess((UINT)WORD_MLtoC(arg));
}

/* _ml_win32_PS_get_environment_variable : string -> string option
 *                                         var
 *
 */
ml_val_t _ml_win32_PS_get_environment_variable(ml_state_t *msp, ml_val_t arg)
{
#define GEV_BUF_SZ 4096
  char buf[GEV_BUF_SZ];
  int ret = GetEnvironmentVariable(STR_MLtoC(arg),buf,GEV_BUF_SZ);
  ml_val_t ml_s,res = OPTION_NONE;

  if (ret > GEV_BUF_SZ) {
    return RAISE_SYSERR(msp,-1);
  }
  if (ret > 0) {
    ml_s = ML_CString(msp,buf);
    OPTION_SOME(msp,res,ml_s);
  }
  return res;
#undef GEV_BUF_SZ
}

/* _ml_win32_PS_sleep : word32 -> unit
 *
 * Suspend execution for interval in MILLIseconds.
 */
ml_val_t _ml_win32_PS_sleep (ml_state_t *msp, ml_val_t arg)
{
  Sleep ((DWORD) WORD_MLtoC(arg));
  return ML_unit;
}


ml_val_t _ml_win32_PS_find_executable(ml_state_t *msp, ml_val_t arg)
{
  Byte_t *fileName = STR_MLtoC(arg);
  TCHAR szResultPath[MAX_PATH];
  int length;
  ml_val_t res, vec, obj;
  BOOL found = FALSE;

  strcpy_s(szResultPath, max(strlen(fileName), MAX_PATH-1), fileName);
  found = PathFindOnPath(szResultPath, NULL);

  if (!found) {
    return OPTION_NONE;
  }

  length = strlen(szResultPath);
  vec = ML_AllocRaw32 (msp, BYTES_TO_WORDS (length + 1));
  strcpy_s(PTR_MLtoC(void, vec), length+1, szResultPath);
  SEQHDR_ALLOC (msp, obj, DESC_string, vec, length);
  OPTION_SOME(msp, res, obj);
  return res;
}

ml_val_t _ml_win32_PS_launch_application(ml_state_t *msp, ml_val_t arg)
{
  Byte_t *fileName = STR_MLtoC(REC_SEL(arg,0));
  Byte_t *argument = STR_MLtoC(REC_SEL(arg,1));

  int result = (int)ShellExecute(NULL, NULL, fileName, argument, NULL, SW_SHOWNORMAL);

  if (result < 32) {
    return RAISE_SYSERR(msp,-1);
  }

  return ML_unit;
}

ml_val_t _ml_win32_PS_open_document(ml_state_t *msp, ml_val_t arg)
{
  Byte_t *document = STR_MLtoC(arg);

  int result = (int)ShellExecute(NULL, NULL, document, NULL, NULL, SW_SHOWNORMAL);

  if (result < 32) {
    return RAISE_SYSERR(msp,-1);
  }

  return ML_unit;
}


/* end of win32-process.c */
