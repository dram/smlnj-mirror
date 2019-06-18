/*! \file asm-base.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common definitions for assembly files in the SML/NJ system.
 */

#ifndef _ASM_BASE_
#define _ASM_BASE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/* bool_t values for assembly code */
#define FALSE	0
#define TRUE	1

#if (!defined(GLOBALS_HAVE_UNDERSCORE)) && (((defined(OPSYS_FREEBSD) || defined(OPSYS_NETBSD2) || defined(OPSYS_OPENBSD)) && !defined(__ELF__)) || defined(OPSYS_WIN32) || defined(OPSYS_DARWIN) || defined(OPSYS_CYGWIN))
#  define GLOBALS_HAVE_UNDERSCORE
#endif

/* we should probably consider factoring this out into ml-unixdep.h -- JHR */
#ifdef GLOBALS_HAVE_UNDERSCORE
#  define CSYM(ID)	CONCAT(_,ID)
#else
#  define CSYM(ID)	ID
#endif

#if defined(HOST_SPARC)
#  if defined(OPSYS_SOLARIS)
#    define _ASM
#    include <sys/stack.h>
#    include <sys/trap.h>
#  endif
#  define GLOBAL(ID)	.global	ID
#  define LABEL(ID)	ID:
#  define ALIGN4        .align 4
#  define WORD(W)       .word W
#  define TEXT		.seg "text"
#  define DATA		.seg "data"
#  define BEGIN_PROC(P)
#  define END_PROC(P)

#elif defined(HOST_PPC)
#  if defined(OPSYS_AIX)
#    define CFUNSYM(ID)	CONCAT(.,ID)
#    define USE_TOC
#    define GLOBAL(ID)	.globl ID
#    define TEXT	.csect [PR]
#    define DATA	.csect [RW]
#    define RO_DATA	.csect [RO]
#    define ALIGN4	.align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)   ID:

#  elif (defined(OPSYS_LINUX) && defined(TARGET_PPC))
#    define CFUNSYM(ID)	ID
#    define GLOBAL(ID)	.globl ID
#    define TEXT	.section ".text"
#    define DATA	.section ".data"
#    define RO_DATA	.section ".rodata"
#    define ALIGN4	.align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:

#  elif (defined(OPSYS_DARWIN) && defined(TARGET_PPC))
#    define CFUNSYM(ID) CSYM(ID)
#    define GLOBAL(ID)  .globl  ID
#    define TEXT        .text
#    define DATA        .data
#    define RO_DATA     .data
#    define ALIGN4      .align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:
#    define __SC__      @

#  elif (defined(OPSYS_OPENBSD) && defined(TARGET_PPC))
#    define CFUNSYM(ID) CSYM(ID)
#    define GLOBAL(ID)  .globl  ID
#    define TEXT        .text
#    define DATA        .data
#    define RO_DATA     .data
#    define ALIGN4      .align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:
#  endif

#  define CENTRY(ID)		\
    .globl CFUNSYM(ID) __SC__	\
    LABEL(CFUNSYM(ID))

#elif defined(HOST_X86)
#  if defined(OPSYS_WIN32)
#    define GLOBAL(ID)		PUBLIC ID
#    define LABEL(ID)		CONCAT(ID,:)
#    define ALIGN4        	EVEN
#    define WORD16(n,w)   	n WORD w
#    define WORD32(n,w)   	n DWORD w
#    define TEXT          	.CODE
#    define DATA          	.DATA
#    define BEGIN_PROC(P)	.ent P
#    define END_PROC(P)		.end P
#    define WORD(W)		WORD32(W)
#  else
#    define GLOBAL(ID)		.global ID
#    define LABEL(ID)		CONCAT(ID,:)
#    define ALIGN4		.align 2
#    define WORD(W)		.word W
#    define TEXT		.text
#    define DATA		.data
#    define BEGIN_PROC(P)	.ent P
#    define END_PROC(P)		.end P

#  endif

#elif defined(HOST_AMD64)
#  if defined(OPSYS_WIN32)
#    define GLOBAL(ID)		PUBLIC ID
#    define LABEL(ID)		CONCAT(ID,:)
#    define ALIGN4        	EVEN
#    define WORD16(n,w)   	n WORD w
#    define WORD32(n,w)   	n DWORD w
#    define TEXT          	.CODE
#    define DATA          	.DATA
#    define BEGIN_PROC(P)	.ent P
#    define END_PROC(P)		.end P
#    define WORD(W)		WORD32(W)
#  else
#    define GLOBAL(ID)		.global ID
#    define LABEL(ID)		CONCAT(ID,:)
#    define ALIGN4		.align 2
#    define WORD(W)		.word W
#    define TEXT		.text
#    define DATA		.data
#    define BEGIN_PROC(P)	.ent P
#    define END_PROC(P)		.end P

#  endif

#else

#  error missing asm definitions

#endif

#ifndef __SC__
#  define __SC__ 	;
#endif

#  define CGLOBAL(ID)	GLOBAL(CSYM(ID))

#if !(defined(TARGET_X86) || defined(TARGET_AMD64))
#define ENTRY(ID)				\
    CGLOBAL(ID) __SC__				\
    LABEL(CSYM(ID))

#define ML_CODE_HDR(name)			\
	    CGLOBAL(name) __SC__		\
	    ALIGN4 __SC__			\
    LABEL(CSYM(name))
#endif /* not x86 or amd64 */

#endif /* !_ASM_BASE_ */

