#include "tags.h"
#include "prof.h"
#define String(handle,len,str) .align 2;\
			       .long len*power_tags+tag_string;\
			       handle: .ascii str
#define Closure(name,handle) .align 2;\
			     .long mak_desc(1,tag_closure);\
			     name: .long handle;\
			     .long 1; .long tag_backptr

	.text
	.align	2
	.long	mak_desc((end_mathvec - mathvec)/4,tag_record)
mathvec:
	.long	_exp_e
	.long	_ln_e
	.long	_sqrt_e
	.long	_atan_v
	.long	_cos_v
	.long	_exp_v
	.long	_ln_v
	.long	_sin_v
	.long   _sqrt_v
end_mathvec:

	.align 2
	.globl _math_functor
Closure(_math_functor,1f)
1:	movl #mathvec,d0
	rts

	.align 2
	.long mak_desc(2,tag_record)
_exp_e:	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,3,"Exp\0")

	.align 2
	.long mak_desc(2,tag_record)
_ln_e:	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,2,"Ln\0\0")

	.align 2
	.long mak_desc(2,tag_record)
_sqrt_e:
	.long 1
	.long 1f
	.long mak_desc(1,tag_array)
1:	.long 1f
String(1,4,"Sqrt")

Closure(_sin_v,1f)
1:	movl sp@(4),a0
	fsind a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	addl #12,a6
	rts

Closure(_cos_v,1f)
1:	movl sp@(4),a0
	fcosd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	addl #12,a6
	rts

.globl fpehandler
.globl standardfpe

/* handle operr and dz */
Closure(_ln_v,1f)
1:	movl sp@(4),a0
	movl #2f,fpehandler
	flognd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	addl #12,a6
	movl #standardfpe,fpehandler
	rts
2:	movl #standardfpe,fpehandler
	movl d7,sp
	movl sp@+,d7
	movl #_ln_e,d0
	rts

/* handle overflow */
Closure(_exp_v,1f)
1:	movl sp@(4),a0
	movl #2f,fpehandler
	fetoxd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	addl #12,a6
	movl #standardfpe,fpehandler
	rts
2:	movl #standardfpe,fpehandler
	movl d7,sp
	movl sp@+,d7
	movl #_exp_e,d0
	rts

/* handle operr */
Closure(_sqrt_v,1f)
1:	movl sp@(4),a0
	movl #2f,fpehandler
	fsqrtd a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	addl #12,a6
	movl #standardfpe,fpehandler
	rts
2:	movl #standardfpe,fpehandler
	movl d7,sp
	movl sp@+,d7
	movl #_sqrt_e,d0
	rts

Closure(_atan_v,1f)
1:	movl sp@(4),a0
	fatand a0@,fp0
	fmoved fp0,a6@
	movl #mak_desc(tag_string,8),a6@(-4)
	movl a6,d0
	addl #12,a6
	rts

