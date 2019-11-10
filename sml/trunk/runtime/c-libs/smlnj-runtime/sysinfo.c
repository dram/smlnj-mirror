/*! \file sysinfo.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * General interface to query system properties.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "cfun-proto-list.h"
#include "machine-id.h"

#if defined(OPSYS_UNIX)
#  include "ml-unixdep.h"  /* for OS_NAME */
#elif defined(OPSYS_WIN32)
#  define OS_NAME "Win32"
#endif

#define STREQ(s1, s2)	(strcmp((s1), (s2)) == 0)


#define FALSE_VALUE	"NO"
#define TRUE_VALUE	"YES"


/* _ml_RunT_sysinfo : string -> string option
 *
 * Current queries:
 *   "OS_NAME"
 *   "OS_VERSION"
 *   "HOST_ARCH"
 *   "TARGET_ARCH"
 *   "HAS_SOFT_POLL"
 *   "HAS_MP"
 *   "HEAP_SUFFIX"      -- added by Blume (7/2000)
 */
ml_val_t _ml_RunT_sysinfo (ml_state_t *msp, ml_val_t arg)
{
    char	*name = STR_MLtoC(arg);
    ml_val_t	res;

    if (STREQ("OS_NAME", name))
	res = ML_CString(msp, OS_NAME);
    else if (STREQ("OS_VERSION", name))
	res = ML_CString(msp, "<unknown>");
    else if (STREQ("HEAP_SUFFIX", name))
        res = ML_CString(msp, MACHINE_ID "-" OPSYS_ID);
    else if (STREQ("HOST_ARCH", name))
#if   defined(HOST_AMD64)
	res = ML_CString(msp, "AMD64");
#elif defined(HOST_PPC)
	res = ML_CString(msp, "PPC");
#elif defined(HOST_SPARC)
	res = ML_CString(msp, "SPARC");
#elif defined(HOST_X86)
	res = ML_CString(msp, "X86");
#else
	res = ML_CString(msp, "<unknown>");
#endif
    else if (STREQ("TARGET_ARCH", name))
#if   defined(TARGET_AMD64)
	res = ML_CString(msp, "AMD64");
#elif defined(TARGET_PPC)
	res = ML_CString(msp, "PPC");
#elif defined(TARGET_SPARC)
	res = ML_CString(msp, "SPARC");
#elif defined(TARGET_X86)
	res = ML_CString(msp, "X86");
#else
	res = ML_CString(msp, "<unknown>");
#endif
    else if (STREQ("HAS_SOFT_POLL", name))
#ifdef SOFT_POLL
	res = ML_CString(msp, TRUE_VALUE);
#else
	res = ML_CString(msp, FALSE_VALUE);
#endif
    else if (STREQ("HAS_MP", name))
#ifdef MP_SUPPORT
	res = ML_CString(msp, TRUE_VALUE);
#else
	res = ML_CString(msp, FALSE_VALUE);
#endif
    else
	return OPTION_NONE;

    OPTION_SOME(msp, res, res);

    return res;

} /* end of _ml_RunT_sysinfo */
