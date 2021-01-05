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

/*
 * AARCH64 register usage and C function calling conventions:
 *
 *	r0-r7		-- argument/result registers
 *	r8		-- indirect result location register
 *	r19-r28		-- callee-save registers
 *	r29		-- frame pointer
 *	r30		-- link register
 *	sp		-- stack pointer (r31)
 *
 *	r9,r15		-- temporary registers
 *	r16,r17		-- intra-procedure-call temporaries (IP0,IP1)
 *	r18		-- reserved for platform-specific use
 *
 * The stack pointer must be 16-byte aligned.
 */
