/* win32-io-lib.c
 *
 * COPYRIGHT (c) 1996 Bell Laboratories, Lucent Technologies
 */

#include "ml-base.h"
#include "c-library.h"
#include "cfun-proto-list.h"


/* the table of C functions and ML names */
#define CFUNC(NAME, FUNC, MLTYPE)	CFUNC_BIND(NAME, FUNC, MLTYPE)
PVT cfunc_binding_t CFunTable[] = {
#include "cfun-list.h"
	CFUNC_NULL_BIND
    };
#undef CFUNC


/* the Signals library */
c_library_t	    WIN32_IO_Library = {
	CLIB_NAME,
	CLIB_VERSION,
	CLIB_DATE,
	NIL(clib_init_fn_t),
	CFunTable
    };

