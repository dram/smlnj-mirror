#include "tags.h"
#define as_str(handle,len,str) handle:	.long len; .ascii str
#include "prof.h"
	.data
	.globl	f68881_used
	.align 2
	.globl	_runvec
_runvec:
	.long	_div_e
	.long	_float_e
	.long	_interrupt_e
	.long	_io_failure_e
	.long	_overflow_e
	.long	_array_v
	.long	_array0_v
	.long	_boot_v
	.long	_close_v
#ifdef NEWTAGS
	.long	_create_b_v
#endif
	.long	_create_s_v
	.long	_export_v
	.long	_fionread_v
	.long	_floor_v
	.long	_isatty_v
	.long	_logb_v
    	.long	_openf_v
	.globl	_ovstruct_v
	.long	_ovstruct_v
	.globl	_pstruct_v
	.long	_pstruct_v
	.long	_read_v
	.long	_scalb_v
	.long	_seql_v
	.long	_write_v


	.text
	.align 2
_div_e:
	.long	1f
as_str(1,3,"div\0")
	.align 2
_overflow_e:
	.long	1f
as_str(1,8,"overflow")
	.align 2
_interrupt_e:	.long	1f
as_str(1,9,"interrupt\0\0\0")
	.align 2
_io_failure_e:	.long	1f
as_str(1,10,"io_failure\0\0")

	.text
	.align 2
	.long	(mak_desc(0,tag_array))
_array0_v:

	.align 2
_array_v:
	.long 1f
1:	movl sp@(4),a1		| a1 = args
	movl a1@,d1		| d1 = length
	asrl #1,d1
#ifdef PROFILE
addql #1,a5@(ARRAYS*4)
addl d1,a5@(ARRAYSIZE*4)
#endif
	movl d1,d2
	asll #width_tags,d2
	orl #tag_array,d2
	asll #2,d1
.globl _before2
_before2:
	clrl a6@(-4,d1:L)	| allocate
.globl _after2
_after2:
	movl d2,a6@(-4)
	movl a6,d0
	movl sp@(4),a0
	movl a0@(4),d2
	jra  2f
1:	movl d2,a6@+
2:	subql #4,d1
	jge  1b
	addql #4,a6
	rts

	.align 2
_create_b_v:
	.long	1f
1:	movl	#tag_bytearray,d3
	jra	2f
_create_s_v:
	.long	1f
1:	movl	#tag_string,d3
2:	movl	sp@(4),d0
	asrl	#1,d0
	movl	d0,d2
	addql	#7,d0
	asrl	#2,d0
	movl	d0,d1
	addql	#1,d0
	asll	#2,d0
#ifdef PROFILE
addql #1,a5@(STRINGS*4)
addl d1,a5@(STRINGSIZE*4)
#endif
	asll	#width_tags,d1
	addl	d3,d1
.globl _before1
_before1:
	clrl	a6@(-8,d0:L)	| allocate
.globl _after1
_after1:
	movl	d2,a6@
	movl	d1,a6@(-4)
	addl	a6,d0
	exg	d0,a6
	rts


	.align 2
_boot_v:	.long 1f
1:	movl sp@(4),d0
	addql #8,d0
	movl  d0,a6@
	movl  #mak_desc(1,tag_closure),a6@(-4)
	movl	a6,d0
	lea	a6@(8),a6
	rts

	.align 2
_seql_v: .long 1f
1:	movl	sp@(4),a0
	movl	a0@(4),a1
	movl	a0@,a0
	movl	a0,d0
	cmpl	a1,d0
	jeq	2f
	movl	a1,d1
	orl	d1,d0
	andb	#1,d0
	jne	3f
	movl	a0@,d1
	addql	#3,d1
	asrl	#2,d1
1:	cmpml	a0@+,a1@+
	jne	3f
	subql	#1,d1
	jge	1b
2:	movl	#3,d0
	rts
3:	movl	#1,d0
	rts


_raise_io_failure:
	lea	_io_failure_e,a0
	movl	a0,d0		|  do this because can only allocate from d0
	movl	d0,a6@(4)
	movl	sp@(4),a6@
	movl  #mak_desc(2,tag_record),a6@(-4)
	movl  a6,d0
	lea	a6@(12),a6
	movl d7,sp
	movl sp@+,d7
	rts

/* datatype flags = APPEND | READ | WRITE  (on BSD:   01011 | 0 | 03001) */
_flags:	.long	01011	| APPEND
	.long	0	| READ
	.long	03001	| WRITE

	.align 2
_openf_v:
	.long	1f
1:	movl	sp@(4),a0	| args
	movl	a0@(4),d0	| flags
	asrl	#1,d0
	asll	#2,d0
	movl	d0,a1
	pea	0666
	movl	a1@(_flags),sp@-
	movl	a0@,a0
	pea	a0@(4)
	pea	0		| bogus return address
	pea	5		| number for open
	trap	#0
	lea	sp@(16),sp
	jcs	_cannot
	addl	d0,d0
	addql	#1,d0
	rts
_cannot: pea	1f
	jsr	_raise_io_failure
	.align 2
as_str(1,4,"open")

	.align 2
	.globl	_openf_v9
_openf_v9:
	.long	1f
1:	movl	sp@(4),a0
	movl	a0@(4),d0
	cmpl	#5,d0		| write
	jeq	_open_write
	cmpl	#3,d0		| read
	jeq	_open_read
_open_append:
	pea	1f
	jsr	_raise_io_failure
	.align 2
as_str(1,22,"append not implemented\0\0")
_open_read:
	movl	sp@(4),a0
	movl	a0@,a0
	pea	0
	pea	a0@(4)
2:	pea	0
	pea	5
	trap	#0	
	lea	sp@(12),sp
	jcs	_cannot
	addl	d0,d0
	addql	#1,d0
	rts
_open_write:
	movl	sp@(4),a0
	movl	a0@,a0
	pea	0666
	pea	a0@(4)
	pea	0
	pea	8
	trap	#0
	lea	sp@(12),sp
	jcs	_cannot
	movl	sp@(4),a0
	movl	a0@,a0
	pea	1
	pea	a0@(4)
	jra	2b


	.align 2
_close_v: .long	1f
1:	movl	sp@(4),d0
	asrl	#1,d0
	movl	d0,sp@-
	pea	0	    | bogus return address
	pea	6
	trap	#0
	lea	sp@(8),sp
	jcs	1f
	moveq	#1,d0
	rts
1:	pea	1f
	jsr	_raise_io_failure
	.align 2
as_str(1,5,"close\0\0\0")

	.align 2
_read_v: .long	1f
1:	movl	sp@(4),a0       | args
	movl	a0@(4),a1	| byte array
	movl	a1@,sp@-	| length
	pea	a1@(4)		| bytes
	movl	a0@,d0
	asrl	#1,d0
	movl	d0,sp@-
	pea	0		| bogus return address
	pea	3
	trap	#0
	lea	sp@(16),sp
	jcs	1f
	addl	d0,d0
	addql	#1,d0
	rts
1:	pea	1f
	jsr	_raise_io_failure
	.align 2
as_str(1,4,"read")

	.align 2
_write_v: .long	1f
1:	movl	sp@(4),a0
	movl	a0@(8),d0
	asrl	#1,d0
	movl	d0,sp@-		| length
	movl	a0@(4),a1
	pea	a1@(4)		| bytes
	movl	a0@,d0
	asrl	#1,d0
	movl	d0,sp@-		| file id
	pea	0		| bogus return address
	pea	4
	trap	#0
	lea	sp@(16),sp
	jcs	1f
	addl	d0,d0
	addql	#1,d0
	rts
1:	pea	1f
	jsr	_raise_io_failure
	.align 2
as_str(1,5,"write\0\0\0")

.align 2
.globl _sigsetmask
.globl _handleinterrupt
_handleinterrupt:
	movl #0,sp@-
	jsr _sigsetmask
	movl d7,sp
	movl sp@+,d7
	lea 1f,a0
	movl	a0,d0
	rts
.align 2
1:	.long 1
	.long _interrupt_e


/* NOTE -> os/machine dependent */
FPE_INTOVF_TRAP=    0x1c	| TRAPV - won't be raised unless a trapv
FPE_INTDIV_TRAP=    0x14	|  instruction is executed after each arithmetic
FPE_FLTOVF_TRAP=    0xd4	|  operation
FPE_FLTDIV_TRAP=    0xc8
FPE_FLTUND_TRAP=    0xcc
FPE_FLTOPERR_TRAP=  0xd0	| raised by 0.0/0.0
.data
code: .long 0
.text
.align 2
.globl _handlefpe
_handlefpe:
	.word 0
	movl sp@(8),code	| grab code
	movl #0,sp@-
	jsr _sigsetmask
	cmpl #FPE_INTDIV_TRAP,code
	beql _handleintdiv
	cmpl #FPE_INTOVF_TRAP,code
	beql _handleintovfl
	cmpl #FPE_FLTOVF_TRAP,code
	beql _handlefloatovfl
	cmpl #FPE_FLTUND_TRAP,code
	beql _handlefloatunfl
	cmpl #FPE_FLTDIV_TRAP,code
	beql _handlefloatdiv
	cmpl #FPE_FLTOPERR_TRAP,code
	beql _handleintdiv
	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,28,"strange floating point error")

_handleintdiv:
	movl d7,sp
	movl sp@+,d7
	lea 1f,a0
	movl	a0,d0
	rts
.align 2
1:	.long 1
	.long _div_e
/* integer overflow should have some other exception. */
_handleintovfl:
	movl d7,sp
	movl sp@+,d7
	lea 1f,a0
	movl	a0,d0
	rts
.align 2
1:	.long 1
	.long _overflow_e
_handlefloatovfl:
	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,8,"overflow")
_handlefloatunfl:
	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,9,"underflow\0\0\0")
_handlefloatdiv:
	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,14,"divide by zero\0\0")

.data
	.globl	_bottom
	.align 2
_bottom: .long	0
.text
	.globl	_apply
	.globl	_profvec
_apply:
	link a6,#0
	moveml d2-d7/a0-a6,sp@-
	lea _profvec,a5
	fmovel #0x3400,fpcr	| enable float operand error, overflow, and div
	movl #0,d6
	pea _handle
	movl #0,sp@-
	movl sp,d7
	movl sp,_bottom
	movl a6@(8),sp@-
	movl a6@(12),sp@-
	movl _freestart,a6
	addql #4,a6
	movl  #0,d0
	movl  #0,d1
	movl  #0,d2
	movl  #0,d3
	movl  #0,d4
	movl  #0,d5
	movl  #0,a1
	movl  #0,a2
	movl  #0,a3
	movl  #0,a4
	movl sp@(4),a0
	movl a0@,a0
_go:	jbsr a0@
	addql #8,sp
	addql #8,sp
_return:
	subql #4,a6
	movl a6,_freestart
	fmovel #0x0000,fpcr	| disable float operand error, overflow, and div
	moveml sp@+,d2-d7/a0-a6
	unlk  a6
	rts
	
	.globl	_uncaught
_handle:
	movl d0,sp@-		| pass arg to uncaught
	jbsr _uncaught
	addql #4,sp		| restore stack
	clrl d0			| return 0
	jra  _return

	.globl	_ghandle_bsd
	.align 2
_ghandle_bsd:
	movl	sp@(12),a0
	pea	a0@(-28)
	moveml	d2-d7/a2-a6,sp@-
	jsr	_callgc_m68bsd
	moveml	sp@+,d2-d7/a2-a6
	lea	sp@(4),sp
	rts

        .globl  _specialgc
        .globl  _export
        .globl  _isexport
        .globl  _exportfile
/* dummy versions of export,fionread, and isatty for now. */
_export_v:
	.long 1f
1:	movl sp@(4),_exportfile
	lea 2f,a0
	movl a0,_specialgc
	clrl 0x3fff0000		|invoke the garbage collector
2:	movl _isexport,d0
	rts

.data
6:      .long   0
.text

/*  WARNING!   not re-entrant    WARNING! */
_fionread_v:
	.long 1f
1:	movl sp@(4),d0
	asrl #1,d0
	pea 6b
	movl #0x4004667f,sp@-
	movl d0,sp@-
	pea 0			| bogus return address
	pea 54			| ioctl
	trap #0
	lea sp@(16),sp
	jcs 1f
	asll #1,d0
	addql #1,d0
	rts
1:      pea 1f
	jsr _raise_io_failure
	.align 2
as_str(1,9,"can_input\0\0\0")

_isatty_v:
	.long 1f
1:	movl sp@(4),d0
	asrl #1,d0
	pea 6b
	movl #0x40067408,sp@-
	movl d0,sp@-
	pea 0			| bogus return address
	pea 54			| ioctl
	trap #0
	lea sp@(16),sp
	jcs 1f
	movl #3,d0
	rts
1:	movl #1,d0
	rts

.data
_spsave: .long  0
.text
	.globl _restart
        .globl _old_high
        .globl _usrstack
_restart:
	movl _spsave,a0
	movl _old_high,a1
	movl _usrstack,a2
1:	movl a1@+,a0@+
	cmpl a2,a0
	jlt 1b
	movl _spsave,sp
	moveml sp@+,d2-d7/a0-a6
	fmovel #0x3400,fpcr	| enable float operand error, overflow, and div
	movl #1,d0
	rts
 
	.globl _mysetjmp
_mysetjmp:
	moveml d2-d7/a0-a6,sp@-
	movl	sp,_spsave
	movl	sp,a0
	movl	_old_high,a1
	movl	_usrstack,a2
1:	movl	a0@+,a1@+
	cmpl	a2,a0
	jlt	1b
	moveml sp@+,d2-d7/a0-a6
	movl	#0,d0
	rts
/*
 * All the m68code for ML reals assumes that NaN's and INF's are never
 * generated and therefore do not need to be handled.
 * This code does not produce NaN's or INF's as long as none are passed to it
 * and overflow, underflow, and operr are handled.
 *
 * Floating exceptions raised (assuming NaN's and INF's are never passed to
 * functions):
 *	OPERR - (div) for 0.0/0.0 (does NOT also cause DZ)
 *	DZ - (div) for (in range) / 0.0
 *	OVERFLOW/UNDERFLOW - (add,div,sub,mul) as appropriate
 */
	.globl	f68881_used
	.globl _floor_v
	.globl _scalb_v
	.globl _logb_v
	.align 2
/* checks that the float will fit in a 31-bit integer before converting;
 * otherwise an operr will result
 */
_floor_v:
	.long 1f
1:	movl sp@(4),a0
	fmoved a0@,fp0		| check that the float is in integer range
	fcmpl #0x40000000,fp0	| higher than highest 31-bit integer
	fbge out_of_range
	fcmpl #0xbfffffff,fp0	| lower than lowest 31-bit integer
	fble out_of_range
	ftstx fp0		| handle positive and negative cases separately
	fblt 1f
/* positive numbers */
	fintrzx fp0,fp0		| round towards zero (down)
	fmovel fp0,d0
	asll #1,d0
	addql #1,d0
	rts
/* negative numbers */
1:	fintrzx fp0,fp1		| round towards zero (up)
	fmovel fp0,d0
	fcmpx fp0,fp1
	fjeq 1f
	subql #1,d0		| push 1 lower
1:	asll #1,d0
	addql #1,d0
	rts
out_of_range:
	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,5,"floor\0\0\0")

	.align 2
/* returns 0 on 0. */
_logb_v:
	.long 1f
1:	movl sp@(4),a0
	fgetexpd a0@,fp0
	fmovel fp0,d0
	rts

_scalb_v:
	.long 1f
1:	movl	sp@(4),a0		| a0 = real*int
	movl	a0@(4),d2		| grab add value and shift to exponent
	andl	#0xfffffffe,d2		|  field
	cmpl	#0x00001000,d2		| prevent possible integer overflow
	bge	over			|  exception from being raised
	cmpl	#0xffffe000,d2		|  on the asll
	ble	under
	movl	a0@,a0
	moveml	a0@,d0/d1		| d0/d1 = old float
	movl	d0,d3
	andl	#0x800fffff,d3		| grab exponent
	beql	1f			| 0?
	asll	#19,d2
	addl	d2,d3			| check out the new exponent
	ble	under			| too small?
	cmpl	#0x80000000,d3
	bge	over			| too large?
	addl	d0,d2			| d0/d1 = new float
	movl	d1,d0
	movl	d0,a6@(4)		| allocate and return new ML real
	movl	d2,a6@
	movl 	#mak_desc(2,tag_string),a6@(-4)
	movl	a6,d0
	addl	#12,a6
	rts
1:	movl	a0,d0			| d0/d1 = 0; return same
	rts
over:	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,8,"overflow")
under:	pea	1f
	jsr	_raise_float
	.align 2
as_str(1,9,"underflow\0\0\0")


.globl _raise_float
_raise_float:
	lea	_float_e,a0
	movl	a0,d0		|  do this because can only allocate from d0
	movl	d0,a6@(4)
	movl	sp@(4),a6@
	movl	#mak_desc(2,tag_record),a6@(-4)
	movl	a6,d0
	lea	a6@(12),a6
	movl	d7,sp
	movl	sp@+,d7
	rts
	.data
	.globl _float_e
	.align 2
_float_e:
	.long	1f
as_str(1,5,"float\0\0\0")
	.text
