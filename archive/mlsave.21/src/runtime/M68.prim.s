/* M68 runtime code for ML
   d7 : exception handler
   a6 : freespace pointer
   d6 : store pointer
   a5 : profile pointer
*/
#include "tags.h"
#include "prof.h"
#define String(handle,len,str) .align 2;\
			       .long len*power_tags+tag_string;\
			       handle: .ascii str
#define Closure(name) .align	2;\
		      .long	mak_desc(1,tag_closure);\
		      name:	.long 7f;\
		      .long	1;\
		      .long	tag_backptr;\
		      7:

	.text
	.globl	_runvec
	.align	2
	.long	mak_desc((end_runvec - _runvec)/4,tag_record)
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
	.globl	_control_v
	.long	_control_v+4
	.long	_create_b_v
	.long	_create_s_v
	.long	_datalist
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
	.long	_profvec_v
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

	.align 2
	.long mak_desc(0,tag_array)
_array0_v:

	.globl	_bottom
Closure(_profvec_v)
	movl sp@(4),a0
	movl a0@(4),d2	/* new value */
	movl a0@,d1
	asrl #1,d1	/* index */
	asll #2,d1
	movl _bottom,a0
	movl a0@(0,d1:L),d0
	movl d2,a0@(0,d1:L)
	rts


Closure(_array_v)
	movl sp@(4),a1		/* a1 = (int * int) */
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

Closure(_create_b_v)
	movl	#tag_bytearray,d3
	jra	2f
Closure(_create_s_v)
	movl	#tag_string,d3
2:	movl	sp@(4),d0
	asrl	#1,d0		/* d0 = length */
#ifdef GCDEBUG
jne 3f
movl #4f,sp@-
.globl _die
jsr _die
.align 2
4: .asciz "create called with 0!\n"
.align 2
3:
#endif
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

Closure(_boot_v)
	movl	sp@(4),d0	/* d0 = string (structure) */
	addql	#8,d0		/* d0 = first function of structure */
	movl	d0,a6@		/* make a closure of it */
	movl	#mak_desc(1,tag_closure),a6@(-4)
	movl	a6,d0
	addql #8,a6
	rts

Closure(_seql_v)
	movl	sp@(4),a0	/* a0 = (string * string) */
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


	.globl _errno
	.globl _make_errstring
_raise_systemcall:
	movl d0,_errno		/* errno is an index to the error string */
	jsr _make_errstring
_raise_systemcall_with:
	movl d0,d1		/* save string arg */
	movl #_systemcall_e,d0	/* can only allocate from d0 */
	movl d0,a6@(4)
	movl d1,a6@		/* string arg */
	movl #mak_desc(2,tag_record),a6@(-4)
	movl a6,d0
	addl #12,a6
	movl d7,sp		/* raise */
	movl sp@+,d7
	rts

/* datatype flags = APPEND | READ | WRITE  (on BSD:   01011 | 0 | 03001) */
_flags:	.long 01011	| APPEND
	.long 0		| READ
	.long 03001	| WRITE

/* the string argument of openf must be zero padded */
Closure(_openf_v)
	movl sp@(4),a0		/* a0 = (string * flags) */
	movl a0@(4),d0		/* d0 = flag */
	asrl #1,d0
	asll #2,d0
	movl d0,a1
	pea 0666		/* permissions = rw-rw-rw- */
	movl a1@(_flags),sp@-   /* read, write or append */
	movl a0@,a0
	pea a0@			/* file name */
	pea 0			/* bogus return address */
	pea 5			/* number for open */
	trap #0
	addl #16,sp
	jcs _raise_systemcall
	addl d0,d0
	addql #1,d0		/* return fildes */
	rts

Closure(_close_v)
	movl	sp@(4),d0
	asrl	#1,d0
	movl	d0,sp@-		/* fildes */
	pea	0		/* bogus return address */
	pea	6		/* number for close */
	trap	#0
	addql #8,sp
	jcs _raise_systemcall
	moveq	#ML_UNIT,d0	/* return unit */
	rts

Closure(_read_v)
	movl	sp@(4),a0       /* a0 = (int * string) */
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
	addl #16,sp
	jcs _raise_systemcall
	addl	d0,d0		/* return characters read */
	addql	#1,d0
	rts

Closure(_write_v)
	movl	sp@(4),a0	/* a0 = (int * string * int) */
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
	addl #16,sp
	jcs _raise_systemcall
	addl	d0,d0		/* return characters written */
	addql	#1,d0
	rts

.data
6:      .long   0,0
.text

#define FIONREAD 0x4004667f
#define TIOCGETP 0x40067408

Closure(_fionread_v)
	movl sp@(4),d0
	asrl #1,d0
	pea 6b			/* bogus location */
	movl #FIONREAD,sp@-	/* fionread */
	movl d0,sp@-		/* fildes */
	pea 0			/* bogus return address */
	pea 54			/* number for ioctl */
	trap #0
	addl #16,sp
	jcs _raise_systemcall
	movl 6b,d0
	asll #1,d0		/* return characters available */
	addql #1,d0
	rts

Closure(_isatty_v)
	movl sp@(4),d0
	asrl #1,d0
	pea 6b			/* bogus location */
	movl #TIOCGETP,sp@-	/* isatty */
	movl d0,sp@-		/* fildes */
	pea 0			/* bogus return address */
	pea 54			/* number for ioctl */
	trap #0
	addl #16,sp
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
	movl #_interrupt_e,d0
	rts

/* SIGEMT is raised if a 68881 instruction is executed with
   no 68881 present. */
.align 2
.globl _handleemt
_handleemt:
	movl	#0,sp@-
	jsr	_sigsetmask
	movl	d7,sp
	movl	sp@+,d7
	pea	1f
	jra	_raise_float
String(1,19,"68881 not installed")

/* NOTE -> os/machine dependent */
#define FPE_INTOVF_TRAP 0x1c	/* TRAPV - won't be raised unless a trapv */
#define FPE_INTDIV_TRAP 0x14	/*  instruction is executed after each    */
#define FPE_FLTOVF_TRAP 0xd4	/*  operation.				  */
#define FPE_FLTDIV_TRAP 0xc8
#define FPE_FLTUND_TRAP 0xcc
#define FPE_FLTOPERR_TRAP 0xd0	/* raised by 0.0/0.0 */
.align 2
.globl _handlefpe
/* jumps indirectly to a floating exception handler.  The
   indirection is necessary because the 68881 math functor
   needs to handle floating exceptions specially. */
/* CPS NOTE: might have to fix stack here, and in other handlers. */
_handlefpe:
	movl #0,sp@-
	jsr _sigsetmask
	movl fpehandler,a0
	jmp a0@
.data
.align 2
.globl fpehandler
fpehandler: .long standardfpe
.text

/* the standard floating exception handler, which raises
   Float with an appropriate string. */
	.globl standardfpe
	.align 2
standardfpe:
	movl sp@(12),d0			/* grab code */
	cmpl #FPE_INTDIV_TRAP,d0
	jeq _handleintdiv
	cmpl #FPE_INTOVF_TRAP,d0
	jeq _handleintovfl
	cmpl #FPE_FLTOVF_TRAP,d0
	jeq _handlefloatovfl
	cmpl #FPE_FLTUND_TRAP,d0
	jeq _handlefloatunfl
	cmpl #FPE_FLTDIV_TRAP,d0
	jeq _handlefloatdiv
	cmpl #FPE_FLTOPERR_TRAP,d0
	jeq _handleintdiv
	pea 1f
	jra _raise_float
String(1,28,"strange floating point error")

_handleintdiv:
	movl d7,sp
	movl sp@+,d7
	movl #_div_e,d0
	rts
_handleintovfl:
	movl d7,sp
	movl sp@+,d7
	movl #_overflow_e,d0
	rts
_handlefloatovfl:
	pea	1f
	jra	_raise_float
String(1,8,"overflow")
_handlefloatunfl:
	pea	1f
	jra	_raise_float
String(1,9,"underflow\0\0\0")
_handlefloatdiv:
	pea	1f
	jra	_raise_float
String(1,14,"divide by zero\0\0")

	.align 2
#ifdef PROFILE
	.globl	_profvec
#endif
	.globl	_apply
	.globl	_bottom
	.globl	_store_save
_apply:
	link a6,#0
	moveml d2-d7/a0-a6,sp@-
#ifdef PROFILE
	lea _profvec,a5			/* init profile vector */
#endif
	jsr fpenable			/* enable floating point exceptions */
	movl _store_save,d6		/* keeps store list between applies */
	pea _handle			/* global exception handler */
	movl #0,sp@-			/* no previous handler */
	movl sp,d7			/* establish handler */
	movl sp,_bottom			/* marks end of collectible stack data */
	movl a6@(8),sp@-		/* function */
	movl a6@(12),sp@-		/* argument */
	movl _freestart,a6		/* initialize heap; */
	addql #4,a6			/* make space for the first tag */
	movl sp@(4),a0
	movl a0@,a0
_go:	jbsr a0@
	addql #8,sp			/* pop closure and argument */
	addql #8,sp			/* pop global handler */
_return:
	subql #4,a6			/* update heap pointer */
	movl a6,_freestart
	movl d6,_store_save
	jsr fpdisable			/* disable floating point exceptions */
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

	.globl	_callgc0
	.globl	_ghandle
	.align 2
_ghandle:
	movl	sp@(12),a0
	pea	a0@(-28)
	moveml	d2-d7/a2-a6,sp@-
	jsr	_callgc0
	moveml	sp@+,d2-d7/a2-a6
	addql #4,sp
	rts

.data
_spsave: .long  0
.text
	.globl _restart
	.globl _specialgc
	.globl _isexport
	.globl _exportfile
        .globl _old_high
	.globl _usrstack
Closure(_exportML_v)
	movl sp@(4),_exportfile
	movl #2f,_specialgc
	movl #restartML,_restart
	clrl 0x3fff0000		/* invoke the garbage collector */
2:	movl _isexport,d0
	rts

restartML:
	jsr _resettimers
	movl _spsave,sp
	movl _old_high,a1
	movl _usrstack,a0
1:	movl a1@+,a0@-			/* restore stack from heap */
	cmpl sp,a0
	jne 1b
	jsr fpenable			/* enable floating point exceptions */
	moveml sp@+,d2-d7/a0-a6		/* restore registers */
	movl #1,d0			/* return C true */
	rts
 
	.globl _mysetjmp
_mysetjmp:
	moveml	d2-d7/a0-a6,sp@-	/* save registers on stack */
	movl	sp,_spsave
	movl	sp,a2
	movl	_old_high,a1
	movl	_usrstack,a0
1:	movl	a0@-,a1@+		/* save stack on heap */
	cmpl	a2,a0
	jne	1b
	moveml sp@+,d2-d7/a0-a6		/* restore stack and registers */
	movl	#0,d0			/* return C false */
	rts

	.globl _chatting
	.globl _exit
	.globl __exit
Closure(_exportFn_v)
	movl sp@(4),a1		/* a1 = fildes * function */
	movl a1@,_exportfile
	movl #2f,_specialgc
	clrl _restart		/* don't export this time;
				   just clean up refs */
	movl _usrstack,sp	/* discard stack */
	movl sp,_bottom
	movl a1@(4),sp@-	/* make sure function is collected */
	clrl 0x3fff0000		/* invoke the garbage collector */
2:	movl #3f,_specialgc
	movl #entry,_restart
	clrl 0x3fff0000		/* invoke the garbage collector */
3:	subl #24,sp		/* add bogus padding to stack for chatting's
				   nonexistent arguments */
	movl #exportmsg,sp@-
	jsr _chatting		/* print exiting message */
	addql #4,sp		/* pop arg to chatting */
	movl #0,sp@-		/* exit sequence */
	jsr _exit
	jsr __exit
	.align 2
exportmsg: .asciz "exportFn exiting\012"

	.globl _restartFn
	.globl _func
	.align 2
entry:	
	movl	_old_high,a3
	movl	a3@,_func
	movl	sp@,d2
	lea	sp@(4),a3
	movl	d2,d1
	asll	#2,d1
	lea	a3@(4,d1:l),a4
	pea	a4@		/* environment */
	pea	a3@		/* argv */
	movl	d2,sp@-		/* argc */
	lea	0:w,a6
	jsr	fpenable	/* enable floating point exceptions */
	jsr	_restartFn
	addw	#12,sp		/* pop args to restartFn */
	movl	#0,sp@-		/* exit sequence */
	jsr	_exit
	addql	#4,sp
	movl	d0,sp@-
	jsr	__exit

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
Closure(_floor_v)
	movl sp@(4),a0
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
	jra	_raise_float
String(1,5,"floor\0\0\0")

/* returns 0 on 0. */
Closure(_logb_v)
	movl sp@(4),a0
	fgetexpd a0@,fp0
	fmovel fp0,d0
	rts

Closure(_scalb_v)
	movl	sp@(4),a0		| a0 = real*int
	movl	a0@(4),d2		| grab add value and shift to exponent
	andl	#0xfffffffe,d2		|  field
	cmpl	#0x00001000,d2		| prevent possible integer overflow
	jge	over			|  exception from being raised
	cmpl	#0xffffe000,d2		|  on the asll
	jle	under
	movl	a0@,a0
	moveml	a0@,d0/d1		| d0/d1 = old float
	movl	d0,d3
	andl	#0x800fffff,d3		| grab exponent
	jeq	1f			| 0?
	asll	#19,d2
	addl	d2,d3			| check out the new exponent
	jle	under			| too small?
	cmpl	#0x80000000,d3
	jge	over			| too large?
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
	jra	_raise_float
String(1,8,"overflow")
under:	pea	1f
	jra	_raise_float
String(1,9,"underflow\0\0\0")


.globl _raise_float
_raise_float:
	movl #_float_e,d0	|  do this because can only allocate from d0
	movl d0,a6@(4)
	movl sp@,a6@
	movl #mak_desc(2,tag_record),a6@(-4)
	movl a6,d0
	addl #12,a6
	movl d7,sp
	movl sp@+,d7
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
Closure(_timer_v)
	jsr  _timer
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
Closure(_chdir_v)
	movl sp@(4),sp@-	/* directory name */
	pea 0			/* bogus return address */
	pea 12			/* number for chdir */
	trap #0
	addql #8,sp
	jcs _raise_systemcall
	movl #ML_UNIT,d0	/* return unit */
	rts

	.globl _system
	.globl _make_system_errstring
/* the string argument of _system_v must be zero padded */
Closure(_system_v)
	movl sp@(4),sp@-	/* command */
	jsr _system
	addql #4,sp
	tstl d0
	jne 2f
	movl #ML_UNIT,d0	/* return unit */
	rts
2:	movl d0,sp@-
	jsr _make_system_errstring
	addql #4,sp
	jra _raise_systemcall_with

	.globl __exit
	.globl _execl
Closure(_execute_v)
	pea 0			/* null to terminate vector */
	movl sp@(8),sp@-	/* command string */
	pea 1f
	pea 2f
	pea 3f
	jsr _execl		/* should never return */
	pea 127
	jsr __exit		/* will never return */
.align 2
3: .asciz "/bin/sh"
.align 2
2: .asciz "sh"
.align 2
1: .asciz "-c"

Closure(_fork_v)
	movl sp@+,a0		| bogus stack hack seems necessary
	pea 0x42		| number for vfork
	trap #0
	jcs _raise_systemcall
	tstl d1
	jeq 2f
	clrl d0
2:	addl d0,d0
	addql #1,d0
	jmp a0@

/* Old version which calls fork instead of vfork.
Closure(_fork_v)
	pea 0x2			| number for fork
	trap #0
	jcs _raise_systemcall
	tstl d1
	jeq 2f
	clrl d0
2:	addl d0,d0
	addql #1,d0
	rts
*/

Closure(_dup2_v)
	movl sp@(4),a0		/* a0 = int * int */
	movl a0@(4),d0
	asrl #1,d0
	movl d0,sp@-		/* fildes2 */
	movl a0@,d0
	asrl #1,d0
	movl d0,sp@-		/* fildes */
	pea 0			/* bogus return address */
	pea 0x5a		/* number for dup2 */
	trap #0
	addl #12,sp
	jcs _raise_systemcall
	movl #ML_UNIT,d0
	rts

Closure(_pipe_v)
	movl #0,sp@-		/* bogus argument */
	movl #0,sp@-		/* bogus return address */
	pea 0x2a		/* number for pipe */
	trap #0
	addql #8,sp
	jcs _raise_systemcall
	addl d1,d1
	addql #1,d1
	movl d1,a6@(4)	/* return fildes */
	addl d0,d0
	addql #1,d0
	movl d0,a6@
	movl #mak_desc(2,tag_record),a6@(-4)
	movl a6,d0
	addl #12,a6
	rts

.globl _minitfp_		/* checks for 68881 and sets flags */
.globl _fp_state_mc68881	/* a flag that gets set */
#define fp_enabled 2 /* from /usr/src/lib/libc/sun/crt/fpcrttypes.h */
.align 2
/* Enable/disable float operand error, overflow, and div.  If no 68881
   is present, nothing happens. */
fpenable:
	jsr _minitfp_			/* checks for 68881 and sets flags.
					   normally executed on startup,
					   but won't be if compiled without
					   -f68881 (for possible sun/50
					   compatibility).  This is just
					   to make sure. */
	cmpl #fp_enabled,_fp_state_mc68881
	jne 1f
	fmovel #0x3400,fpcr
1:	rts
fpdisable:
	cmpl #fp_enabled,_fp_state_mc68881
	jne 1f
	fmovel #0,fpcr
1:	rts
