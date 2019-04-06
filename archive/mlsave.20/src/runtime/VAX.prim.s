/* VAX runtime code for ML
   r13 : exception handler
   r12 : freespace pointer
   r11 : store pointer
   r10 : profile pointer
*/
#include "tags.h"
#include "prof.h"
#include "ml.h"
#define String(handle,len,str) .align 2;\
			       .long len*power_tags+tag_string;\
			       handle: .ascii str
#ifdef CPS
/* args come in as
   r2 = closure; can be ignored because contains no free vars
   r0 = arg
   r1 = continuation
*/
#define Closure(name) .align	2;\
		      .long	mak_desc(1,tag_record);\
		      name:	.long 9f;\
		      .long	1;\
		      .long	tag_backptr;\
		      9:\
		      pushl	r1;\
		      pushl	r0;\
		      jsb	7f;\
		      addl2	$4,sp;\
		      movl	(sp)+,r1;\
		      clrq	r2;\
		      clrq	r4;\
		      clrq	r6;\
		      clrl	r8;\
		      jmp	*(r1);\
		      7:
#else
#define Closure(name) .align	2;\
		      .long	mak_desc(1,tag_closure);\
		      name:	.long 7f;\
		      .long	1;\
		      .long	tag_backptr;\
		      7:
#endif

#ifdef CPS
#define RAISE \
	movl	r13,r1 ; \
	jmp	*(r13)
#else
#define RAISE \
	movl r13,sp ; \
	movl (sp)+,r13 ; \
	rsb
#endif

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
	.long	_overflow_e
	.long   _systemcall_e
	.long	_array_v
	.long	_array0_v
	.long	_boot_v
	.long	_byte_array0_v
	.long	_chdir_v
	.long	_close_v
	.globl _control_v
	.long	_control_v+4
	.long	_create_b_v
	.long	_create_s_v
	.long	_dup2_v
	.long	_execute_v
	.long	_exportFn_v
	.long	_exportML_v
	.long	_fionread_v
	.long	_floor_v
	.long	_fork_v
	.long	_isatty_v
	.long	_logb_v
	.long	_openf_v
	.long	_pipe_v
	.long   _profvec_v
	.globl	_pstruct_v
	.long	_pstruct_v
	.long	_read_v
	.long	_scalb_v
	.long	_seql_v
	.long	_system_v
	.long	_timer_v
	.long	_write_v
end_runvec:

.text
	.align 2
	.long mak_desc(2,tag_record)
_div_e:	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,3,"Div\0")

	.align	2
	.long mak_desc(2,tag_record)
_overflow_e:
	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,8,"Overflow")

	.align	2
	.long mak_desc(2,tag_record)
	.long mak_desc(1,tag_array)
_interrupt_e:
	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,9,"Interrupt\0\0\0")

	.align	2
	.long mak_desc(1,tag_array)
_systemcall_e: .long 1f
String(1,10,"SystemCall\0\0")

	.align	2
	.long	mak_desc(0,tag_array)
_array0_v:

	.globl	_bottom
Closure(_profvec_v)
	movl 4(sp),r1
	movl 4(r1),r2		/* new value */
	ashl $-1,(r1),r1	/* index */
	movl _bottom,r3
	movl (r3)[r1],r0
	movl r2,(r3)[r1]
	rsb

Closure(_array_v)
	movl 4(sp),r2		/* r2 = (int * 'a) */
	ashl $-1,(r2),r9	/* r9 = length */
#ifdef PROFILE
movl _bottom,r10
addl2 $2,(ARRAYS*4)(r10)
addl2 r9,(ARRAYSIZE*4)(r10)
addl2 r9,(ARRAYSIZE*4)(r10)
#endif
	ashl $width_tags,r9,r0
	bisl2 $tag_array,r0	/* r0 = new tag */
	clrl -4(r12)[r9]	/* allocate */
	movl r0,-4(r12)
	movl r12,r0
	movl 4(sp),r2
	movl 4(r2),r2		/* r2 = default value */
	jbr 2f
1:	movl r2,(r12)+		/* store default */
2:	sobgeq	r9,1b
	addl2 $4,r12
	rsb

	.align	2
	.long	mak_desc(0,tag_bytearray)
_byte_array0_v:

Closure(_create_b_v)
	movl	$tag_bytearray,r4
	jbr	2f
Closure(_create_s_v)
	movl	$tag_string,r4
2:	movl	4(sp),r10
#ifdef GCDEBUG
jneq 3f
pushal 4f
.globl _die
calls $1,_die
4: .asciz "create called with 0!\n"
3:
#endif
	addl2	$13,r10
	ashl	$-3,r10,r10
	ashl	$2,r10,r9	/* r9 = bytes in string including tag */
	decl	r10		/* r10 = words in string, not including tag */
#ifdef PROFILE
movl _bottom,r5
addl2 $2,(STRINGS*4)(r5)
addl2 r10,(STRINGSIZE*4)(r5)
addl2 r10,(STRINGSIZE*4)(r5)
#endif
	clrl	-4(r12)[r10]	/* allocate */
	movl	4(sp),r1
	ashl	$-1,r1,r1	/* r1 = length */
	ashl	$width_tags,r1,r1
	bisl3	r4,r1,-4(r12)	/* new tag */
	movl	r12,r0
	addl2	r9,r12
	rsb

Closure(_boot_v)
	addl3 4(sp),$8,r0	/* r0 = first function of structure */
	movl  r0,(r12)		/* make a closure of it */
	movl  $mak_desc(1,tag_closure),-4(r12)
	movl  r12,r0
	addl2 $8,r12
	rsb

Closure(_seql_v)
	movl	4(sp),r0		/* r0 = (string * string) */
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

	.globl _errno
	.globl _make_errstring
_raise_systemcall:
	movl r0,_errno
	calls $0,_make_errstring
_raise_systemcall_with:
	moval _systemcall_e,4(r12)
	movl r0,(r12)
	movl $mak_desc(2,tag_record),-4(r12)
	movl r12,r0
	addl2 $12,r12
	RAISE

/* the string argument of openf must be zero padded */
/* datatype flags = APPEND | READ | WRITE  (on BSD: 01011 | 0 | 03001) */

#ifdef BSD
flags:	.long	0	# dummy
	.long	01011	# APPEND
	.long	0	# dummy
	.long	0	# READ
	.long	0	# dummy
	.long	03001	# WRITE
Closure(_openf_v)
/* remember no regs across allocate */
1:	movl	$0666,12(ap)	/* permissions = rw-rw-rw- */
	movl	4(sp),r0	/* r0 = (string * flags) */
	movl	4(r0),r1	/* r1 = flag */
	movl	flags[r1],8(ap)
	movl	(r0),4(ap)	/* file name */
	movl	$3,(ap)		/* arg count */
	chmk	$5		/* call open */
	jcs _raise_systemcall
	addl2	r0,r0
	incl	r0		/* return fildes */
	rsb
#endif BSD

#ifdef V9
Closure(_openf_v)
	movl	4(sp),r0	/* r0 = (string * flags) */
	movl	4(r0),r1	/* r1 = flag */
	cmpl	r1,$5		/* write? */
	jeql	_open_write
	cmpl	r1,$3		/* read? */
	jeql	_open_read
_open_append:
	movl	$1,8(ap)	/* set up for write */
	movl	(r0),4(ap)	/* file name */
	movl	$2,(ap)		/* arg count */
	chmk	$5		/* call open */
	jcs	_open_write
	movl	$2,12(ap)	/* lseek at eof */
	movl	$0,8(ap)	/* offset from eof */
	movl	r0,4(ap)	/* file descriptor */
	movl	$3,(ap)		/* arg count */
	chmk	$19		/* call lseek */
	jcs	_raise_systemcall
	movl	4(ap),r0
	addl2	r0,r0		/* return fildes */
	incl	r0
	rsb
_open_read:
	movl	$0,8(ap)	/* set up for read */
	movl	(r0),4(ap)	/* file name */
	movl	$2,(ap)		/* arg count */
	chmk	$5		/* call open */
	jcs	_raise_systemcall
	addl2	r0,r0		/* return fildes */
	incl	r0
	rsb
_open_write:
	movl	$0666,8(ap)	/* permissions = rw-rw-rw- */
	movl	(r0),4(ap)	/* file name */
	movl	$2,(ap)		/* arg count */
	chmk	$8		/* call create */
	jcs	_raise_systemcall
	addl2	r0,r0		/* return fildes */
	incl	r0
	rsb
#endif V9

Closure(_close_v)
	ashl $-1,4(sp),4(ap)	/* fildes */
	movl $1,(ap)		/* arg count */
	chmk $6			/* call close */
	jcs _raise_systemcall
	movl $ML_UNIT,r0	/* return UNIT */
	rsb
	
Closure(_read_v)
	clrl	12(ap)
	movl	4(sp),r0	/* r0 = (int * string) */
	movl	4(r0),r0
	ashl	$-width_tags,-4(r0),r0
	movl	r0,12(ap)	/* length */
	movl	4(sp),r0	/* r0 = (int * string);
				   no ptrs across allocate */
	movl	4(r0),8(ap)	/* string */
	ashl	$-1,(r0),4(ap)	/* fildes */
	movl	$3,(ap)		/* arg count */
	chmk	$3		/* call read */
	jcs _raise_systemcall
	addl2	r0,r0		/* return characters read */
	incl	r0
	rsb

Closure(_write_v)
	clrl	12(ap)
	movl	4(sp),r0	/* r0 = (int * string * int) */
	ashl	$-1,8(r0),r0
	movl	r0,12(ap)	/* characters to write */
	movl	4(sp),r0	/* r0 = (int * string * int);
				   no ptrs across allocate */
	movl	4(r0),8(ap)	/* string */
	ashl	$-1,(r0),4(ap)	/* fildes */
	movl	$3,(ap)		/* arg count */
	chmk	$4		/* call write */
	jcs _raise_systemcall
	addl2	r0,r0		/* return characters written */
	incl	r0
	rsb

.data
6:	.long	0
	.long	0
.text

#ifdef BSD
#define FIONREAD 0x4004667f
#define TIOCGETP 0x40067408
#endif
#ifdef V9
#define FIONREAD 0x667f
#define TIOCGETP 0x7408
#endif
Closure(_fionread_v)
	moval 6b,12(ap)		/* scratch location */
	movl $FIONREAD,8(ap)		/* fionread */
	ashl $-1,4(sp),4(ap)		/* fildes */
	movl $3,(ap)			/* arg count */
	chmk $54			/* call ioctl */
	jcs _raise_systemcall
	ashl $1,6b,r0			/* return characters available */
	incl r0
	rsb

Closure(_isatty_v)
	moval 6b,12(ap)		/* bogus location */
	movl $TIOCGETP,8(ap)		/* isatty */
	ashl $-1,4(sp),4(ap)		/* fildes */
	movl $3,(ap)			/* arg count */
	chmk $54			/* call ioctl */
	jcs 1f
	movl $ML_TRUE,r0
	rsb
1:	movl $ML_FALSE,r0
	rsb

.align 2
#ifdef BSD
.globl _sigsetmask
#endif
#ifdef V9
.globl _setupsignals
#endif
.globl _handleinterrupt
_handleinterrupt:
	.word 0x4000		/* enable overflow */
	movl 28(sp),ap		/* restore r12 (r12 - r15 scrambled) */
	movl 32(sp),r13
#ifdef CPS
	movl _bottom,sp
#endif
#ifdef BSD
	pushl $0
	calls $1,_sigsetmask
#endif
#ifdef V9
	calls $0,_setupsignals
#endif
	moval _interrupt_e,r0
	RAISE

/* NOTE -> os/machine dependent */
#define FPE_INTOVF_TRAP 0x1
#define FPE_INTDIV_TRAP 0x2
#define FPE_FLTOVF_TRAP 0x3
#define FPE_FLTDIV_TRAP 0x4
#define FPE_FLTUND_TRAP 0x5
#define FPE_FLTOVF_FAULT 0x8
#define FPE_FLTDIV_FAULT 0x9
#define FPE_FLTUND_FAULT 0xa

.align 2
.globl _handlefpe
_handlefpe:
	.word 0x4000		/* enable overflow */
	movl 8(ap),r2		/* grab code */
	movl 28(sp),ap		/* restore r12 */
	movl 32(sp),r13
#ifdef CPS
	movl _bottom,sp
#endif
#ifdef BSD
	pushl $0
	calls $1,_sigsetmask
#endif
#ifdef V9
	calls $0,_setupsignals
#endif
	cmpl $FPE_INTDIV_TRAP,r2
	jeql _handleintdiv
	cmpl $FPE_INTOVF_TRAP,r2
	jeql _handleintovfl
	cmpl $FPE_FLTOVF_TRAP,r2
	jeql _handlefloatovfl
	cmpl $FPE_FLTDIV_TRAP,r2
	jeql _handlefloatdiv
    	cmpl $FPE_FLTUND_TRAP,r2
	jeql _handlefloatunfl
	cmpl $FPE_FLTOVF_FAULT,r2
	jeql _handlefloatovfl
	cmpl $FPE_FLTDIV_FAULT,r2
	jeql _handlefloatdiv
    	cmpl $FPE_FLTUND_FAULT,r2
	jeql _handlefloatunfl
	pushal	1f
	jbr	_raise_float
String(1,28,"strange floating point error")

_handleintdiv:
	moval _div_e,r0
	RAISE

_handleintovfl:
	moval _overflow_e,r0
	RAISE

_handlefloatovfl:
	pushal 1f
	jbr _raise_float
String(1,8,"overflow")
_handlefloatunfl:
	pushal 1f
	jbr _raise_float
String(1,9,"underflow\0\0\0")
_handlefloatdiv:
	pushal 1f
	jbr _raise_float
String(1,14,"divide by zero\0\0")

	.align	2
	.globl	_apply
	.globl	_bottom
	.globl	_store_save
_apply:
	.word	0x4ffe	/* save all regs but r0; allow integer overflow */
	pushl	fp
	movl	_store_save,r11		/* keeps store list between applies */
#ifdef CPS
#ifdef PROFILE
.globl _profvec
	movl $PROFSIZE-1,r0
	moval _profvec,r1
8:	movl (r1)[r0],-(sp)
	sobgeq r0,8b
#endif
	moval	handle_c,r13		/* global exception handler */
	movl	sp,_bottom		/* marks end of collectible stack data */
	movl	4(ap),r2		/* function */
	movl	8(ap),r0		/* argument */
	moval	return_c,r1		/* continuation */
	addl3	_freestart,$4,r12	/* initialize heap;
					   make space for the first tag */
_go:	jmp	*(r2)
_return:
#ifdef PROFILE
	moval _profvec,r1
	movl $PROFSIZE,r2
8:	movl (sp)+,(r1)+
	sobgtr r2,8b
#endif
#else
	pushal	_handle			/* global exception handler */
	pushl	$0			/* no previous handler */
	movl	sp,r13			/* establish handler */
	movl	sp,_bottom		/* marks end of collectible stack data */
	pushl	4(ap)			/* function */
	pushl	8(ap)			/* argument */
	addl3	_freestart,$4,r12	/* initialize heap;
					   make space for the first tag */
	movl	4(sp),r0
_go:	jsb	*0(r0)
	addl2	$8,sp			/* pop closure and argument */
	addl2	$8,sp			/* pop global handler */
_return:
#endif
	subl3	$4,r12,_freestart	/* update heap pointer */
	movl	r11,_store_save
	movl	(sp)+,fp
	ret

/* only needed for CPS */
	.align 2
	.long mak_desc(1,tag_record)
return_c: .long _return
	.long mak_desc(1,tag_record)
handle_c: .long	_handle

	.globl	_uncaught
_handle:
	pushl	r0			/* push exception */
	calls	$1,_uncaught		/* print it */
	addl2	$4,sp
	clrl	r0			/* return 0 */
	jbr	_return

	.globl	_callgc0
	.globl	_ghandle
	.align	2
_ghandle:
	.word	0xfff
	calls	$0,_callgc0
	ret

	.globl	_pctest
	.globl	_sptest
	.globl	_afterg
	.globl  _gtest
_gtest: .word    0xfff
	pushl	ap
	pushl	fp
	movl	$0x99000,r0
	movl	$0x99001,r1
	movl	$0x99002,r2
	movl	$0x99003,r3
	movl	$0x99004,r4
	movl	$0x99005,r5
	movl	$0x99006,r6
	movl	$0x99007,r7
	movl	$0x99008,r8
	movl	$0x99009,r9
	movl	$0x9900a,r10
	movl	$0x9900b,r11
	movl	$0x9900c,r12
	movl	$0x9900d,r13
	movl	sp,_sptest
	moval	1f,_pctest
1:	movl	$0,0x1ffffff0
_afterg:movl	(sp)+,fp
	movl	(sp)+,ap
	ret

.data
_spsave: .long	0
.text
	.globl _restart
	.globl _specialgc
	.globl _isexport
	.globl _exportfile
        .globl _old_high
	.globl _usrstack
Closure(_exportML_v)
	movl	4(sp),_exportfile
	moval	2f,_specialgc
	moval	restartML,_restart
	clrl	0x3fff0000	/* invoke the garbage collector */
2:	movl	_isexport,r0
	rsb

	.align 2
	.globl _resettimers
restartML:
	.word	0x4ffe		/*  allow integer overflow */
	calls	$0,_resettimers
	movl	_spsave,r2
	movl	_old_high,r1
	movl	_usrstack,r0
1:	movl	(r1)+,-(r0)
	cmpl	r0,r2
	jneq	1b
	movl	_spsave,sp
	movl	sp,fp
	movl	$1,r0
	ret

	.align 2
	.globl	_mysetjmp
_mysetjmp: .word 0
	movl	sp,_spsave
	movl	sp,r2
	movl	_old_high,r1
	movl	_usrstack,r0
1:	movl	-(r0),(r1)+
	cmpl	r0,r2
	jneq	1b
	movl	$0,r0
	ret

	.globl _chatting
Closure(_exportFn_v)
	movl	4(sp),r0	/* r0 = fildes * function */
	movl	(r0),_exportfile
	moval	2f,_specialgc
	clrl	_restart	/* don't export this time;
				   just clean up refs */
	movl	_usrstack,sp	/* discard stack */
	movl	sp,_bottom
	pushl	4(r0)		/* make sure function is collected */
	clrl	0x3fff0000	/* invoke the garbage collector */
2:	moval	3f,_specialgc
	moval	entry,_restart
	clrl	0x3fff0000	/* invoke the garbage collector */
3:	pushal	exportmsg
	calls	$1,_chatting
	chmk	$1
	.align 2
exportmsg: .asciz "exportFn exiting\n"

	.globl _restartFn
	.globl _exit
	.globl _func
	.align 2
entry:	.word 0
	movl	*_old_high,_func
	subl2	$8,sp
	movl	8(sp),(sp)
	movab	12(sp),r0
	movl	r0,4(sp)
1:	tstl	(r0)+
	jneq	1b
	cmpl	r0,*4(sp)
	jlss	2f
	tstl	-(r0)
2:	movl	r0,8(sp)
	calls	$3,_restartFn
	pushl	r0
	calls	$1,_exit
	chmk	$1


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

Closure(_floor_v)

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
	pushal	1f
	jbr	_raise_float
String(1,5,"floor\0\0\0")

Closure(_logb_v)
	bicl3	$0xffff800f,*4(sp),r0	# grab exponent
	beql    1f			# if zero, return same
	ashl	$-4,r0,r0
	subl2	$1025,r0		# unbias
1:	addl2	r0,r0
	incl	r0
	rsb

Closure(_scalb_v)
	clrl	4(r12)
	movl	4(sp),r0		# r0 = real*integer
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
	jbr	_raise_float
under:	pushal	1f
	jbr	_raise_float
String(1,9,"underflow\0\0\0")

.align 2
_raise_float:
	moval _float_e,4(r12)
	movl  (sp),(r12)
	movl  $mak_desc(2,tag_record),-4(r12)
	movl  r12,r0
	addl2 $12,r12
	RAISE


	.align 2
	.long mak_desc(1,tag_array)
_float_e: .long 1f
String(1,5,"Float\0\0\0")

/* timing functions */
.globl _timer
.globl _g_sec
.globl _g_usec
.globl _t_sec
.globl _t_usec
Closure(_timer_v)
	calls $0,_timer
	movl _g_usec,4(r12)
	movl _g_sec,(r12)
	movl  $mak_desc(2,tag_record),-4(r12)
	movl  r12,r0
	addl2 $12,r12
	movl _t_usec,4(r12)
	movl _t_sec,(r12)
	movl  $mak_desc(2,tag_record),-4(r12)
	movl  r12,r1
	addl2 $12,r12
	movl  r0,4(r12)
	movl  r1,(r12)
	movl  $mak_desc(2,tag_record),-4(r12)
	movl  r12,r0
	addl2 $12,r12
	rsb

/* the string argument of _chdir_v must be zero padded */
Closure(_chdir_v)
	movl 4(sp),4(ap)	/* directory name */
	movl $1,(ap)		/* arg count */
	chmk $12		/* call chdir */
	jcs _raise_systemcall
	movl $ML_UNIT,r0	/* return unit */
	rsb

	.globl _system
	.globl _make_system_errstring
/* the string argument of _system_v must be zero padded */
Closure(_system_v)
	pushl 4(sp)		/* command */
	calls $1,_system
	tstl r0			/* error? */
	jneq 2f
	movl $ML_UNIT,r0	/* return unit */
	rsb
2:	pushl r0
	calls $1,_make_system_errstring
	jbr _raise_systemcall_with

	.globl _execl
Closure(_execute_v)
	pushl $0		/* null to terminate vector */
	pushl 8(sp)		/* command string */
	pushal 1f
	pushal 2f
	pushal 3f
	calls $5,_execl		/* should never return */
	pushl $127
	calls $1,__exit		/* will never return */
.align 2
3: .asciz "/bin/sh"
.align 2
2: .asciz "sh"
.align 2
1: .asciz "-c"

#ifdef V9
Closure(_fork_v)
	movl $0,(ap)		/* arg count; necessary? */
	chmk $2			/* call fork */
	jcs 2f			/* don't raise SystemCall here, for */
	blbc r1,2f		/*  compatibility with systems using */
	clrl r0			/*  vfork (SystemCall can't be handled */
2:	addl2 r0,r0		/*  in that case, see perv.sml). */
	incl r0
	rsb
#endif
#ifdef BSD
Closure(_fork_v)
	movl (sp)+,r2		/* save return address */
	chmk $0x42		/* call vfork */
	jcs 2f			/* don't raise SystemCall; see perv.sml */
	blbc r1,2f
	clrl r0
2:	addl2 r0,r0
	incl r0
	jmp (r2)
#endif

Closure(_dup2_v)
	movl 4(sp),r0		/* r0 = int * int */
	ashl $-1,4(r0),8(ap)	/* fildes2 */
	ashl $-1,(r0),4(ap)	/* fildes */
	bisb2 $64,4(ap)		/* tell kernel dup2 instead of dup */
	movl $2,(ap)		/* arg count */
	chmk $0x29		/* call dup */
	jcs _raise_systemcall
	movl $ML_UNIT,r0
	rsb

Closure(_pipe_v)
	movl $0,4(ap)		/* bogus argument */
	movl $1,(ap)		/* arg count */
	chmk $0x2a		/* call pipe */
	jcs _raise_systemcall
	addl2 r1,r1
	addl3 $1,r1,4(r12)	/* return fildes */
	addl2 r0,r0
	addl3 $1,r0,(r12)
	movl $mak_desc(2,tag_record),-4(r12)
	movl r12,r0
	addl2 $12,r12
	rsb
