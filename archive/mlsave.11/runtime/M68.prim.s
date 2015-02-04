#include "tags.h"
#include "prof.h"
#define String(handle,len,str) .align 2;\
			       .long len*power_tags+tag_string;\
			       handle: .ascii str
#define Closure(name,handle) .align 2; .long mak_desc(1,tag_closure);\
			     name: .long handle

	.text
	.globl	f68881_used

	.globl	_runvec
	.align	2
	.long	mak_desc((end_runvec - _runvec)/4,tag_record)
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
	.long	_chdir_v
	.long	_close_v
	.long	_control_v
	.long	_create_b_v
	.long	_create_s_v
	.long	0 /*    val execute : unit -> unit (* bogus type for now *) */
	.long	_export_v
	.long	_export1_v
	.long	_fionread_v
	.long	_floor_v
	.long	_isatty_v
	.long	_logb_v
    	.long	_openf_v
	.globl	_pstruct_v
	.long	_pstruct_v
	.long	_read_v
	.long	_scalb_v
	.long	_seql_v
	.long	_system_v
	.long	_timer_v
	.long	_write_v
end_runvec:

	.globl _gcmessages
	.align 2
	.long mak_desc(1,tag_record)
_control_v:
	.long _gcmessages
.data
	.align 2
	.long mak_desc(1,tag_array)
_gcmessages:
	.long ML_TRUE
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
_io_failure_e: .long 1f
String(1,10,"Io_failure\0\0")

	.align 2
	.long mak_desc(0,tag_array)
_array0_v:

Closure(_array_v,1f)
1:	movl sp@(4),a1		/* a1 = (int * int) */
	movl a1@,d1
	asrl #1,d1		/* d1 = length */
#ifdef PROFILE
addql #1,a5@(ARRAYS*4)
addl d1,a5@(ARRAYSIZE*4)
#endif
	movl d1,d2
	asll #width_tags,d2
	orl #tag_array,d2	/* d2 = new tag */
	asll #2,d1
.globl _before2
_before2:
	clrl a6@(-4,d1:L)	/* allocate */
.globl _after2
_after2:
	movl d2,a6@(-4)
	movl a6,d0
	movl a1@(4),d2		/* d2 = default */
	jra  2f
1:	movl d2,a6@+		/* store default */
2:	subql #4,d1
	jge  1b
	addql #4,a6
	rts

	.align 2
	.long mak_desc(0,tag_bytearray)
_byte_array0_v:

Closure(_create_b_v,1f)
1:	movl	#tag_bytearray,d3
	jra	2f
Closure(_create_s_v,1f)
1:	movl	#tag_string,d3
2:	movl	sp@(4),d0
	asrl	#1,d0		/* d0 = length */
	movl	d0,d2
	asll	#width_tags,d2
	addl	d3,d2		/* d2 = new tag */
	addql	#3,d0
	asrl	#2,d0		/* d0 = words in string, not including tag */
#ifdef PROFILE
addql #1,a5@(STRINGS*4)
addl d0,a5@(STRINGSIZE*4)
#endif
	addql	#1,d0
	asll	#2,d0
.globl _before1
_before1:
	clrl	a6@(-8,d0:L)	/* allocate */
.globl _after1
_after1:
	movl	d2,a6@(-4)
	addl	a6,d0
	exg	d0,a6
	rts

Closure(_boot_v,1f)
1:	movl	sp@(4),d0	/* d0 = string (structure) */
	addql	#8,d0		/* d0 = first function of structure */
	movl	d0,a6@		/* make a closure of it */
	movl	#mak_desc(1,tag_closure),a6@(-4)
	movl	a6,d0
	lea	a6@(8),a6
	rts

Closure(_seql_v,1f)
1:	movl	sp@(4),a0	/* a0 = (string * string) */
	movl	a0@(4),a1	/* a1 = second string */
	movl	a0@,a0		/* a0 = first string */
	movl	a0,d0
	cmpl	a1,d0		/* identical objects? */
	jeq	2f
	movl	a1,d1
	orl	d1,d0		/* character (int format)?*/
	andb	#1,d0
	jne	3f
	movl	a0@(-4),d0
	asrl	#width_tags,d0	/* check length */
	movl	a1@(-4),d1
	asrl	#width_tags,d1
	cmpl	d0,d1
	jne	3f
	addql	#3,d0
	asrl	#2,d0
	jeq	2f
1:	cmpml	a0@+,a1@+	/* all should match */
	jne	3f
	subql	#1,d0
	jgt	1b
2:	movl	#ML_TRUE,d0
	rts
3:	movl	#ML_FALSE,d0
	rts


_raise_io_failure:
	lea	_io_failure_e,a0
	movl	a0,d0		/* can only allocate from d0 */
	movl	d0,a6@(4)
	movl	sp@(4),a6@
	movl  	#mak_desc(2,tag_record),a6@(-4)
	movl	a6,d0
	lea	a6@(12),a6
	movl	d7,sp		/* raise */
	movl	sp@+,d7
	rts

/* datatype flags = APPEND | READ | WRITE  (on BSD:   01011 | 0 | 03001) */
_flags:	.long	01011	| APPEND
	.long	0	| READ
	.long	03001	| WRITE

/* the string argument of openf must be zero padded */
Closure(_openf_v,1f)
1:	movl	sp@(4),a0	/* a0 = (string * flags) */
	movl	a0@(4),d0	/* d0 = flag */
	asrl	#1,d0
	asll	#2,d0
	movl	d0,a1
	pea	0666		/* permissions = rw-rw-rw- */
	movl	a1@(_flags),sp@-   /* read, write or append */
	movl	a0@,a0
	pea	a0@		/* file name */
	pea	0		/* bogus return address */
	pea	5		/* number for open */
	trap	#0
	lea	sp@(16),sp
	jcs	_cannot
	addl	d0,d0
	addql	#1,d0		/* return fildes */
	rts
_cannot: pea	1f
	jsr	_raise_io_failure
String(1,4,"open")

Closure(_close_v,1f)
1:	movl	sp@(4),d0
	asrl	#1,d0
	movl	d0,sp@-		/* fildes */
	pea	0		/* bogus return address */
	pea	6		/* number for close */
	trap	#0
	lea	sp@(8),sp
	jcs	1f
	moveq	#ML_UNIT,d0	/* return unit */
	rts
1:	pea	1f
	jsr	_raise_io_failure
String(1,5,"close\0\0\0")

Closure(_read_v,1f)
1:	movl	sp@(4),a0       /* a0 = (int * string) */
	movl	a0@(4),a1	/* a1 = string */
	movl	a1@(-4),d0
	asrl	#width_tags,d0
	movl	d0,sp@-		/* length */
	pea	a1@		/* string */
	movl	a0@,d0
	asrl	#1,d0		/* fildes */
	movl	d0,sp@-
	pea	0		/* bogus return address */
	pea	3		/* number for read */
	trap	#0
	lea	sp@(16),sp
	jcs	1f
	addl	d0,d0		/* return characters read */
	addql	#1,d0
	rts
1:	pea	1f
	jsr	_raise_io_failure
String(1,4,"read")

Closure(_write_v,1f)
1:	movl	sp@(4),a0	/* a0 = (int * string * int) */
	movl	a0@(8),d0	
	asrl	#1,d0
	movl	d0,sp@-		/* characters to write */
	movl	a0@(4),a1
	pea	a1@		/* string */
	movl	a0@,d0
	asrl	#1,d0
	movl	d0,sp@-		/* fildes */
	pea	0		/* bogus return address */
	pea	4		/* number for write */
	trap	#0
	lea	sp@(16),sp
	jcs	1f
	addl	d0,d0		/* return characters written */
	addql	#1,d0
	rts
1:	pea	1f
	jsr	_raise_io_failure
String(1,5,"write\0\0\0")

.data
6:      .long   0,0
.text

/*  WARNING!   not re-entrant    WARNING! */
Closure(_fionread_v,1f)
1:	movl sp@(4),d0
	asrl #1,d0
	pea 6b			/* bogus location */
	movl #0x4004667f,sp@-	/* fionread */
	movl d0,sp@-		/* fildes */
	pea 0			/* bogus return address */
	pea 54			/* number for ioctl */
	trap #0
	lea sp@(16),sp
	jcs 1f
	asll #1,d0		/* return characters available */
	addql #1,d0
	rts
1:      pea 1f
	jsr _raise_io_failure
String(1,9,"can_input\0\0\0")

Closure(_isatty_v,1f)
1:	movl sp@(4),d0
	asrl #1,d0
	pea 6b			/* bogus location */
	movl #0x40067408,sp@-	/* isatty */
	movl d0,sp@-		/* fildes */
	pea 0			/* bogus return address */
	pea 54			/* number for ioctl */
	trap #0
	lea sp@(16),sp
	jcs 1f
	movl #ML_TRUE,d0
	rts
1:	movl #ML_FALSE,d0
	rts

.align 2
.globl _sigsetmask
.globl _handleinterrupt
_handleinterrupt:
	movl	#0,sp@-
	jsr	_sigsetmask
	movl	d7,sp
	movl	sp@+,d7
	lea	_interrupt_e,a0
	movl	a0,d0
	rts

/* NOTE -> os/machine dependent */
#define FPE_INTOVF_TRAP 0x1c	/* TRAPV - won't be raised unless a trapv */
#define FPE_INTDIV_TRAP 0x14	/*  instruction is executed after each    */
#define FPE_FLTOVF_TRAP 0xd4	/*  operation.				  */
#define FPE_FLTDIV_TRAP 0xc8
#define FPE_FLTUND_TRAP 0xcc
#define FPE_FLTOPERR_TRAP 0xd0	/* raised by 0.0/0.0 */
.data
code: .long 0
.text
.align 2
.globl _handlefpe
_handlefpe:
	movl sp@(8),code	/* grab code */
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
String(1,28,"strange floating point error")

_handleintdiv:
	movl d7,sp
	movl sp@+,d7
	lea _div_e,a0
	movl a0,d0
	rts

_handleintovfl:
	movl d7,sp
	movl sp@+,d7
	lea _overflow_e,a0
	movl a0,d0
	rts

_handlefloatovfl:
	pea	1f
	jsr	_raise_float
String(1,8,"overflow")
_handlefloatunfl:
	pea	1f
	jsr	_raise_float
String(1,9,"underflow\0\0\0")
_handlefloatdiv:
	pea	1f
	jsr	_raise_float
String(1,14,"divide by zero\0\0")

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
	fmovel #0x3400,fpcr	/* enable float operand error,
				   overflow, and div */
	movl #0,d6
	pea _handle			/* global exception handler */
	movl #0,sp@-			/* no previous handler */
	movl sp,d7			/* establish handler */
	movl sp,_bottom			/* marks end of collectible stack data */
	movl a6@(8),sp@-		/* function */
	movl a6@(12),sp@-		/* argument */
	movl _freestart,a6		/* initialize heap; */
	addql #4,a6			/* make space for the first tag */
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
appst:	movl sp@(4),a0
	movl a0@,a0
_go:	jbsr a0@
	addql #8,sp			/* pop closure and argument */
	addql #8,sp			/* pop global handler */
_return:
	subql #4,a6			/* update heap pointer */
	movl a6,_freestart
	fmovel #0x0000,fpcr	/* disable float operand error,
				   overflow, and div */
	moveml sp@+,d2-d7/a0-a6
	unlk  a6
	rts
	
	.globl	_uncaught
_handle:
	movl d0,sp@-		/* pass arg to uncaught */
	jbsr _uncaught
	addql #4,sp		/* restore stack */
	clrl d0			/* return 0 */
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

.data
_spsave: .long  0
.text
	.globl _restart
	.globl _do_export
	.globl _mak_str_lst
	.globl _specialgc
	.globl _isexport
	.globl _exportfile
        .globl _old_high
	.globl _usrstack
Closure(_export_v,1f)
1:	movl sp@(4),_exportfile
	lea 2f,a0
	movl a0,_specialgc
	movl #1,_do_export
	clrl 0x3fff0000		/* invoke the garbage collector */
2:	movl _isexport,d0
	rts

_restart:
	movl _spsave,a0
	movl _old_high,a1
	movl _usrstack,a2
1:	movl a1@+,a0@+
	cmpl a2,a0
	jlt 1b
	movl _spsave,sp
	moveml sp@+,d2-d7/a0-a6
	fmovel #0x3400,fpcr	/* enable float operand error,
				   overflow, and div */
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

	.globl _die
Closure(_export1_v,1f)
1:	movl sp@(4),a1		/* a1 = fildes * function */
	movl a1@,_exportfile
	lea 2f,a0
	movl a0,_specialgc
	clrl _do_export		/* don't export this time;
				   just clean up refs */
	movl _bottom,d7		/* restore exception handler */
	movl _bottom,sp
	movl a1@(4),sp@-	/* function */
	movl #ML_NIL,sp@-	/* nil list for function */
	clrl 0x3fff0000		/* invoke the garbage collector */
2:	lea 3f,a0
	movl a0,_specialgc
	movl #1,_do_export
	clrl 0x3fff0000		/* invoke the garbage collector */
3:	movl _isexport,d0
	cmpl	#ML_FALSE,d0
	jne	appst
	lea	exportmsg,a0
	movl	a0,sp@-
	jsr	_die
exportmsg: .asciz "export1 exiting\n"

restart1:
	movl _spsave,a0
	movl _old_high,a1
	movl _usrstack,a2
1:	movl a1@+,a0@+
	cmpl a2,a0
	jlt 1b
	movl _spsave,sp
	moveml sp@+,d2-d7/a0-a6
	fmovel #0x3400,fpcr	/* enable float operand error,
				   overflow, and div */
	movl #1,d0
	rts

/*
 * Floating point primitives
 *
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
 *
 * Floor:
 * checks that the float will fit in a 31-bit integer before converting;
 * otherwise an operr will result
 *
 */
Closure(_floor_v,1f)
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
String(1,5,"floor\0\0\0")

/* returns 0 on 0. */
Closure(_logb_v,1f)
1:	movl sp@(4),a0
	fgetexpd a0@,fp0
	fmovel fp0,d0
	rts

Closure(_scalb_v,1f)
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
	movl 	#mak_desc(8,tag_string),a6@(-4)
	movl	a6,d0
	addl	#12,a6
	rts
1:	movl	a0,d0			| d0/d1 = 0; return same
	rts
over:	pea	1f
	jsr	_raise_float
String(1,8,"overflow")
under:	pea	1f
	jsr	_raise_float
String(1,9,"underflow\0\0\0")


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
Closure(_timer_v,1f)
1:	jsr  _timer
	movl _g_usec,d0
	movl d0,a6@(4)	/* only allocate from d0 */
	movl _g_sec,a6@
	movl #mak_desc(2,tag_record),a6@(-4)
	movl a6,d2
	addl #12,a6
	movl _t_usec,d0
	movl d0,a6@(4)
	movl _t_sec,a6@
	movl #mak_desc(2,tag_record),a6@(-4)
	movl a6,d1
	addl #12,a6
	movl d2,d0
	movl d0,a6@(4)
	movl d1,a6@
	movl #mak_desc(2,tag_record),a6@(-4)
	movl a6,d0
	addl #12,a6
	rts

/* the string argument of _chdir_v must be zero padded */
Closure(_chdir_v,1f)
1:	movl sp@(4),sp@-	/* directory name */
	pea 0			/* bogus return address */
	pea 12			/* number for chdir */
	trap #0
	lea sp@(8),sp
/* what should happen on errors?
	jcs	???
*/
	movl #ML_UNIT,d0	/* return unit */
	rts

	.globl _system
/* the string argument of _system_v must be zero padded */
Closure(_system_v,1f)
1:	movl sp@(4),sp@-	/* command */
	jsr _system
/* what should happen on errors?*/
	lea sp@(4),sp
	movl #ML_UNIT,d0	/* return unit */
	rts
