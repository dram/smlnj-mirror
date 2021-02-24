/*! \file AARCH64.prim.asm
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Assembly code for the AARCH64 (aka ARM64) target.
 */

#include "ml-base.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "mlstate-offsets.h"	/** this file is generated **/
#include "ml-limits.h"

/* AArch64 register usage and C function calling conventions:
 *
 *	r0-r7		-- argument/result registers
 *	r8		-- indirect result location register
 *	r19-r28		-- callee-save registers
 *	r29		-- frame pointer
 *	r30		-- link register
 *	sp		-- stack pointer (r31)
 *
 *	r9-r15		-- temporary registers
 *	r16,r17		-- intra-procedure-call temporaries (IP0,IP1)
 *	r18		-- reserved for platform-specific use
 *
 * The stack pointer must be 16-byte aligned.
 */

/* SML Register conventions for AArch64:
 *
 *	alloc ptr
 *	limit ptr
 *	store ptr
 *	exn ptr
 *	var ptr
 *	GC link
 *	base ptr
 *	std link
 *	std clos
 *	std arg
 *	std cont
 *	misc0
 */

#define		allocptr	x24
#define 	limitptr	x25
#define 	storeptr	x26
#define 	exnptr		x27
#define 	varptr		x28
#define		stdlink		x3
#define 	stdclos		x2
#define 	stdarg		x0
#define 	stdcont		x1
#define		miscreg0	x4
#define		miscreg1	x5
#define 	miscreg2	x6

#define		gclink		lr

