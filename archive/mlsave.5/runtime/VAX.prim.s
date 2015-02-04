#include "tags.h"
#include "prof.h"
#define String(handle,len,str) .align 2;\
			       .long len*power_tags+tag_string;\
			       handle: .ascii str
#define Closure(name,handle) .align 2; .long mak_desc(1,tag_closure);\
			     name: .long handle

	.text

	.globl	_runvec
	.align	2
#ifdef VAX	/* VAX assembler is fubar */
        .long	tag_record
#else
	.long	mak_desc((end_runvec - _runvec)/4,tag_record)
#endif
_runvec:
	.long	_div_e
	.long	_float_e
	.long	_interrupt_e
	.long	_io_failure_e
	.long	_overflow_e
	.long	_array_v
	.long	_array0_v
	.long	_boot_v
	.long	_byte_array0_v
	.long	_close_v
	.long	_create_b_v
	.long	_create_s_v
	.long	_export_v
	.long	_fionread_v
	.long	_floor_v
#ifdef BSD
	.long	_isatty_v
#endif
#ifdef V9
	.long	_isatty_v9
#endif
	.long	_logb_v
#ifdef BSD
	.long	_openf_v
#endif
#ifdef V9
	.long	_openf_v9
#endif
	.globl	_pstruct_v
	.long	_pstruct_v
	.long	_read_v
	.long	_scalb_v
	.long	_seql_v
	.long	_write_v
end_runvec:

	.align	2
	.long mak_desc(1,tag_array)
_div_e: .long 1f
String(1,3,"Div\0")

	.align	2
	.long mak_desc(1,tag_array)
_overflow_e: .long 1f
String(1,8,"Overflow")

	.align	2
	.long mak_desc(1,tag_array)
_interrupt_e: .long 1f
String(1,9,"Interrupt\0\0\0")

	.align	2
	.long mak_desc(1,tag_array)
_io_failure_e: .long 1f
String(1,10,"Io_failure\0\0")

	.align	2
	.long	mak_desc(0,tag_array)
_array0_v:

Closure(_array_v,1f)
1:	movl 4(sp),r2		/* r2 = (int * int) */
	ashl $-1,(r2),r1	/* r1 = length */
	movl 4(r2),r2		/* r2 = default value */
#ifdef PROFILE
addl2 $1,(ARRAYS*4)(r10)
addl2 r1,(ARRAYSIZE*4)(r10)
#endif
	ashl $width_tags,r1,r0
	bisl2 $tag_array,r0	/* r0 = new tag */
	clrl -4(r12)[r1]	/* allocate */
	movl r0,-4(r12)
	movl r12,r0
	jbr 2f
1:	movl r2,(r12)+		/* store default */
2:	sobgeq	r1,1b
	addl2 $4,r12
	rsb

	.align	2
	.long	mak_desc(0,tag_bytearray)
_byte_array0_v:

Closure(_create_b_v,1f)
1:	movl	$tag_bytearray,r4
	jbr	2f
Closure(_create_s_v,1f)
1:	movl	$tag_string,r4
2:	movl	4(sp),r0
	ashl	$-1,r0,r1	/* r1 = length */
	addl2	$13,r0
	ashl	$-3,r0,r0
	ashl	$2,r0,r3	/* r3 = bytes in string including tag */
	decl	r0		/* r0 = words in string, not including tag */
#ifdef PROFILE
addl2 $1,(STRINGS*4)(r10)
addl2 r0,(STRINGSIZE*4)(r10)
#endif
	clrl	-4(r12)[r0]	/* allocate */
	ashl	$width_tags,r1,r1
	bisl3	r4,r1,-4(r12)	/* new tag */
	movl	r12,r0
	addl2	r3,r12
	rsb

Closure(_boot_v,1f)
1:	addl3 4(sp),$8,r0	/* r0 = first function of structure */
	movl  r0,(r12)		/* make a closure of it */
	movl  $mak_desc(1,tag_closure),-4(r12)
	movl  r12,r0
	addl2 $8,r12
	rsb

Closure(_seql_v,1f)
1:	movl	4(sp),r0		/* r0 = (string * string) */
	movl	4(r0),r1		/* r1 = second string */
	movl	(r0),r0			/* r0 = first string */
	cmpl	r0,r1			/* identical objects? */
	jeql	2f
	bisb3	r0,r1,r2		/* character (int format)? */
	blbs	r2,3f
	ashl	$-width_tags,-4(r0),r2	/* check length */
	ashl	$-width_tags,-4(r1),r3
	cmpl	r2,r3
	jneq	3f
	addl2	$3,r2
	ashl	$-2,r2,r2
	jeql	2f
1:	cmpl	(r0)+,(r1)+		/* all should match */
	jneq	3f
	sobgtr	r2,1b
2:	movl	$ML_TRUE,r0
	rsb
3:	movl	$ML_FALSE,r0
	rsb

_raise_io_failure:
	moval _io_failure_e,4(r12)
	movl  4(sp),(r12)
	movl  $mak_desc(2,tag_record),-4(r12)
	movl  r12,r0
	addl2 $12,r12
	movl r13,sp		/* raise */
	movl (sp)+,r13
	rsb

/* datatype flags = APPEND | READ | WRITE  (on BSD: 01011 | 0 | 03001) */
_flags:	.long	0	# dummy
	.long	01011	# APPEND
	.long	0	# dummy
	.long	0	# READ
	.long	0	# dummy
	.long	03001	# WRITE

/* the string argument of openf must be zero padded */
Closure(_openf_v,1f)
/* remember no regs across allocate */
1:	movl	$0666,12(ap)	/* permissions = rw-rw-rw- */
	movl	4(sp),r0	/* r0 = (string * flags) */
	movl	4(r0),r1	/* r1 = flag */
	movl	_flags[r1],8(ap)
	movl	(r0),4(ap)	/* file name */
	movl	$3,(ap)		/* arg count */
	chmk	$5		/* call open */
	jcs	_cannot
	addl2	r0,r0
	incl	r0		/* return filid */
	rsb
_cannot: pushal	1f
	jsb	_raise_io_failure
String(1,4,"open")

Closure(_openf_v9,1f)
1:	movl	4(sp),r0	/* r0 = (string * flags) */
	movl	4(r0),r1	/* r1 = flag */
	cmpl	r1,$5		/* write? */
	jeql	_open_write
	cmpl	r1,$3		/* read? */
	jeql	_open_read
_open_append:			/* append not implemented */
	pushal	1f
	jsb	_raise_io_failure
String(1,22,"append not implemented\0\0")
_open_read:
	movl	$0,8(ap)	/* set up for read */
	movl	(r0),4(ap)	/* file name */
	movl	$2,(ap)		/* arg count */
2:	chmk	$5		/* call open */
	jcs	_cannot
	addl2	r0,r0		/* return filid */
	incl	r0
	rsb
_open_write:
	movl	$0666,8(ap)	/* permissions = rw-rw-rw- */
	movl	(r0),4(ap)	/* file name */
	movl	$2,(ap)		/* arg count */
	chmk	$8		/* call create */
	jcs	_cannot
	movl	$1,8(ap)	/* set up for write */
	jbr	2b		/* open */


Closure(_close_v,1f)
1:	ashl	$-1,4(sp),4(ap)	/* filid */
	movl	$1,(ap)		/* arg count */
	chmk	$6		/* call close */
	jcs	1f
	movl	$ML_UNIT,r0	/* return UNIT */
	rsb
1:	pushal	1f
	jsb	_raise_io_failure
String(1,5,"close\0\0\0")
	
Closure(_read_v,1f)
1:	movl	4(sp),r0	/* r0 = (int * string) */
	movl	4(r0),r0
	ashl	$-width_tags,-4(r0),r0
	movl	r0,12(ap)	/* length */
	movl	4(sp),r0	/* r0 = (int * string);
				   no ptrs across allocate */
	movl	4(r0),8(ap)	/* string */
	ashl	$-1,(r0),4(ap)	/* filid */
	movl	$3,(ap)		/* arg count */
	chmk	$3		/* call read */
	jcs	1f
	addl2	r0,r0		/* return characters read */
	incl	r0
	rsb
1:	pushal	1f
	jsb	_raise_io_failure
String(1,4,"read")

Closure(_write_v,1f)
1:	movl	4(sp),r0	/* r0 = (int * string * int) */
	ashl	$-1,8(r0),r0
	movl	r0,12(ap)	/* characters to write */
	movl	4(sp),r0	/* r0 = (int * string * int);
				   no ptrs across allocate */
	movl	4(r0),8(ap)	/* string */
	ashl	$-1,(r0),4(ap)	/* filid */
	movl	$3,(ap)		/* arg count */
	chmk	$4		/* call write */
	jcs	1f
	addl2	r0,r0		/* return characters written */
	incl	r0
	rsb
1:	pushal	1f
	jsb	_raise_io_failure
String(1,5,"write\0\0\0")

.data
6:	.long	0
	.long	0
.text

/*  WARNING!   not re-entrant    WARNING! */
Closure(_fionread_v,1f)
1:	moval	6b,12(r12)		/* scratch location */
	movl	$0x4004667f,8(r12)	/* fionread */
	ashl	$-1,4(sp),4(r12)	/* filid */
	movl	$3,(r12)		/* arg count */
	chmk	$54			/* call ioctl */
	jcs	1f
	ashl	$1,6b,r0		/* return characters available */
	incl	r0
	rsb
1:	pushal	1f
	jsb	_raise_io_failure
String(1,9,"can_input\0\0\0")

Closure(_isatty_v9,1f)
1:	pushal	6b			/* bogus location */
	pushl	$0x7408			/* isatty */
	ashl	$-1,12(sp),-(sp)	/* filid */
	calls	$3,_ioctl		/* call ioctl */
	tstl	r0
	bgeq	1f
	movl	$ML_FALSE,r0
	brb	2f
1:	movl	$ML_TRUE,r0
2:	rsb

Closure(_isatty_v,1f)
1:	moval	6b,12(r12)		/* bogus location */
	movl	$0x40067408,8(r12)	/* isatty */
	ashl	$-1,4(sp),4(r12)	/* filid */
	movl	$3,(r12)		/* arg count */
	chmk	$54			/* call ioctl */
	jcs	1f
	movl	$ML_TRUE,r0
	rsb
1:	movl	$ML_FALSE,r0
	rsb

.align 2
.globl	_sigsetmask
.globl _handleinterrupt
_handleinterrupt:
	.word 0x4000		/* enable overflow */
	movl	28(sp),ap	/* restore r12 (r12 - r15 scrambled) */
	movl	32(sp),r13
	pushl	$0
	calls	$1,_sigsetmask
	movl	r13,sp
	movl	(sp)+,r13
	moval	1f,r0
	rsb

	.align 2
	.long mak_desc(2,tag_record)
1:	.long 1
	.long _interrupt_e

/* NOTE -> os/machine dependent */
.set	FPE_INTOVF_TRAP,    0x1
.set	FPE_INTDIV_TRAP,    0x2
.set	FPE_FLTOVF_TRAP,    0x3
.set	FPE_FLTDIV_TRAP,    0x4
.set	FPE_FLTUND_TRAP,    0x5
.data
code: .long 0
.text
.align 2
.globl _handlefpe
_handlefpe:
	.word 0x4000		/* enable overflow */
	movl 8(ap),code		/* grab code */
	movl 28(sp),ap		/* restore r12 */
	movl 32(sp),r13
	pushl $0
	calls $1,_sigsetmask
	cmpl $FPE_INTDIV_TRAP,code
	jeql _handleintdiv
	cmpl $FPE_INTOVF_TRAP,code
	jeql _handleintovfl
	cmpl $FPE_FLTOVF_TRAP,code
	jeql _handlefloatovfl
	cmpl $FPE_FLTDIV_TRAP,code
	jeql _handlefloatdiv
    	cmpl $FPE_FLTUND_TRAP,code
	jeql _handlefloatunfl
	pushal	1f
	jsb	_raise_float
String(1,28,"strange floating point error")

_handleintdiv:
	movl r13,sp
	movl (sp)+,r13
	moval 1f,r0
	rsb

	.align 2
	.long mak_desc(2,tag_record)
1:	.long 1
	.long _div_e

_handleintovfl:
	movl r13,sp
	movl (sp)+,r13
	moval 1f,r0
	rsb

	.align 2
	.long mak_desc(2,tag_record)
1:	.long 1
	.long _overflow_e

_handlefloatovfl:
	pushal	1f
	jsb	_raise_float
String(1,8,"overflow")
_handlefloatunfl:
	pushal	1f
	jsb	_raise_float
String(1,9,"underflow\0\0\0")
_handlefloatdiv:
	pushal	1f
	jsb	_raise_float
String(1,14,"divide by zero\0\0")


.data
	.align	2
	.globl	_bottom
_bottom: .long 0
.text	

	.align	2
	.globl	_apply
	.globl	_profvec
_apply:
	.word	0x4ffe	/* save all regs but r0; allow integer overflow */
	pushl	fp
	moval	_profvec,r10
	moval	0,r11
	pushal	_handle	/* establish exception handler */
	pushl	$0
	movl	sp,r13
	movl	sp,_bottom
	pushl	4(ap)
	pushl	8(ap)
	addl3	_freestart,$4,r12
	clrl	r1
	clrl	r2
	clrl	r3
	clrl	r4
	clrl	r5
	clrl	r6
	clrl	r7
	clrl	r8
	clrl	r9
	movl	4(sp),r0
_go:	jsb	*0(r0)
	addl2	$8,sp
	addl2	$8,sp
_return:
	subl3	$4,r12,_freestart
	movl	(sp)+,fp
	ret
	
	.globl	_uncaught
_handle:
	pushl	r0
	calls	$1,_uncaught
	addl2	$4,sp
	clrl	r0
	jbr	_return

	.globl	_ghandle_bsd
	.align	2
_ghandle_bsd:
	.word	0xfc0
	movl	12(ap),r0
	pushal	8(r0)
	pushal	20(fp)
	pushal  -24(ap)
	calls	$3,_callgc_vaxbsd
	ret

	.globl	_ghandle_v9
	.align	2
_ghandle_v9:
	.word	0xf80
	pushl	fp
	calls	$3,_callgc_vaxv9
 	ret

	.globl	_specialgc
	.globl	_isexport
	.globl	_exportfile
Closure(_export_v,1f)
1:	movl	4(sp),_exportfile
	moval	2f,_specialgc
	clrl	0x3fff0000	/* invoke the garbage collector */
2:	movl	_isexport,r0
	rsb


.data
_spsave: .long	0
.text
	.globl _restart
        .globl _old_high
	.globl _usrstack
_restart:
	.word	0x4ffe	/* save all regs but r0; allow integer overflow */
	movl	_spsave,r0
	movl	_old_high,r1
	movl	_usrstack,r2
1:	movl	(r1)+,(r0)+
	cmpl	r0,r2
	jlss	1b
	movl	_spsave,sp
	movl	sp,fp
	movl	$1,r0
	ret

	.globl	_mysetjmp
_mysetjmp: .word 0
	movl	sp,_spsave
	movl	sp,r0
	movl	_old_high,r1
	movl	_usrstack,r2
1:	movl	(r0)+,(r1)+
	cmpl	r0,r2
	jlss	1b
	movl	$0,r0
	ret

/*
 * The vaxcode for ML reals assumes that no function is ever passed a reserved
 * operand (ROP) and no ROP is ever produced by the ML system.  This is
 * true of the assembly code here.
 *
 * Floating exceptions raised (assuming ROP's are never passed to functions):
 *	DIVIDE BY ZERO - (div)
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 */

/* floor raises integer overflow if the float is out of 32-bit range,
 * so the float is tested before conversion, to make sure it is in (31-bit)
 * range
 */

Closure(_floor_v,1f)
1:
#ifdef V9 /* hack because gfloats aren't implemented in v9's assembler */
.long 0x04be50fd, 0x5051fd50, 0x0041f08f, 0x00000000
.long 0xfd2c1800, 0xf08f5051, 0x400000c1, 0x15000000
.long 0x504afd1e, 0xbe53fd50, 0xfd0d1804, 0xfd51504e
.long 0x5104be51
#endif
/* bytes should be identical to the following instructions. */
#ifdef BSD
	movg *4(sp),r0
	cmpg r0,$0g1073741824.0		# higher than highest 31-bit integer
	bgeq out_of_range
	cmpg r0,$0g-1073741825.0	# lower than lowest 31-bit integer
	bleq out_of_range
	cvtgl r0,r0
	tstg *4(sp)
	bgeq 1f
	cvtlg r0,r1			# handle negative
	cmpg *4(sp),r1
#endif
	beql 1f
	decl r0
1:	ashl $1,r0,r0
	incl r0
	rsb
out_of_range:
	pushal 1f
	jsb _raise_float
String(1,5,"floor\0\0\0")

Closure(_logb_v,1f)
1:	bicl3	$0xffff800f,*4(sp),r0	# grab exponent
	beql    1f			# if zero, return same
	ashl	$-4,r0,r0
	subl2	$1025,r0		# unbias
1:	addl2	r0,r0
	incl	r0
	rsb

Closure(_scalb_v,1f)
1:	movl	4(sp),r0		# r0 = real*integer
	bicl3	$1,4(r0),r2		# grab add value and shift to exponent
	ashl	$3,r2,r2		#  field
	movq	*(r0),r0		# r0/r1 = old float
	bicl3	$0xffff800f,r0,r3	# grab exponent
	beql	1f			# 0?
	addl2	r2,r3			# check out the new exponent
	bleq	under			# too small?
	cmpl	r3,$0x8000
	bgeq	over			# too large?
	addl2	r2,r0			# r0 = new float
	movq	r0,(r12)		# allocate and return new ML real
	movl 	$mak_desc(8,tag_string),-4(r12)
	movl	r12,r0
	addl2	$12,r12
	rsb
1:	movl	*4(sp),r0		# r0 = 0; return same
	rsb
over:	pushal	1f
	jsb	_raise_float
under:	pushal	1f
	jsb	_raise_float
String(1,9,"underflow\0\0\0")

.align 2
_raise_float:
	moval _float_e,4(r12)
	movl  4(sp),(r12)
	movl  $mak_desc(2,tag_record),-4(r12)
	movl  r12,r0
	addl2 $12,r12
	movl r13,sp
	movl (sp)+,r13
	rsb

	.align 2
	.long mak_desc(1,tag_array)
_float_e: .long 1f
String(1,5,"Float\0\0\0")
