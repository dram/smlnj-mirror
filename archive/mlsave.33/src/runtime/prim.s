# 1 "M68.prim.s" 
# 1 "./tags.h" 1
































# 2 "M68.prim.s" 2
# 1 "./prof.h" 1




































# 3 "M68.prim.s" 2
# 1 "./prim.h" 1










# 14 "./prim.h" 

# 4 "M68.prim.s" 2

























	.text
	.globl	_runvec
	.align	2
	.long	((8)*16+(1))
_runvec:
	.long	_array_v
        .long	_callc_v
	.long	_create_b_v
	.long	_create_s_v
	.long	_floor_v
	.long	_logb_v
	.long	_scalb_v
	.long	_syscall_v

.align 2; .long ((1)*16+(1)); _array_v: .long 7f; .long 1; .long 5; 7:
	movl a0@,d1		
	movl a0@(4),d3		
	asrl #1,d1		
	movl d1,d2
	asll #4,d2
	orl #9,d2	
	asll #2,d1
3:	movl a6,d4
	addl d1,d4
        cmpl d4,d5
        jmi  6f
	movl d2,a6@+
	movl a6,a0
	jra  2f
1:	movl d3,a6@+		
2:	subql #4,d1
	jge  1b
	movl a1@,a2
	jmp a2@

	clrl d0
6:	trapmi
	jra 3b

.align 2; .long ((1)*16+(1)); _create_b_v: .long 7f; .long 1; .long 5; 7:
	movl	#11,d3
	jra	2f
.align 2; .long ((1)*16+(1)); _create_s_v: .long 7f; .long 1; .long 5; 7:
	movl	#15,d3
2:	movl	a0,d1
	asrl	#1,d1		
	movl	d1,d2
	asll	#4,d2
	addl	d3,d2		
	andl	#~3,d1		
3:	movl	a6,d4
	addl	d1,d4
	cmpl	d4,d5
	jmi	1f
	movl	d2,a6@+
	movl	a6,a0
	addl	d1,a6
	clrl	a6@+		
	movl a1@,a2
	jmp a2@

	clrl	d0
1:	trapmi
	jra 3b






.align 2
.globl _handleinterrupt
_handleinterrupt:
	movl	#0,sp@-
	jsr	_sigsetmask
	movl	_bottom, sp
	clrl	d3
	movl	d3,a0
	movl	d3,a1
	movl	d3,a2
	movl	d3,a3
	movl	d3,a4
	movl	d3,a5
	lea	(_interrupt_e0+4),a0
	movl d7,a1 ; movl a1@,a2 ; jra a2@

	.globl _saveregs
	.globl _handle_c
	.globl _return_c
	.globl _restoreregs
.align 2; .long ((1)*16+(1)); _handle_c: .long 7f; .long 1; .long 5; 7:
	movl	#3,_cause;
	jra	_saveregs

.align 2; .long ((1)*16+(1)); _return_c: .long 7f; .long 1; .long 5; 7:
	movl	#2,_cause;
_saveregs:
	movl	a6,_saved_dataptr
	movl	d7,_saved_exnptr
	movl	d6,_saved_storeptr
        movl	d5,_saved_limit
	movl	a0,_saved_ptrs
	movl	a1,_saved_ptrs+4
	movl	a2,_saved_ptrs+8
	movl	a3,_saved_ptrs+12
	movl	a4,_saved_ptrs+16
	movl	a5,_saved_ptrs+20
	movl	d3,_saved_ptrs+24
	movl	d1,_saved_nonptrs
	movl	d2,_saved_nonptrs+4
	movl	d4,_saved_nonptrs+8
	movl	_bottom,sp
	movl	_fpsave,a6
	rts

_restoreregs:
	movl	a6,_fpsave
	movl	sp,_bottom
	movl	_saved_dataptr,a6
	movl	_saved_exnptr,d7
	movl	_saved_storeptr,d6
        movl	_saved_limit,d5
	movl	_saved_ptrs,a0
	movl	_saved_ptrs+4,a1
	movl	_saved_ptrs+8,a2
	movl	_saved_ptrs+12,a3
	movl	_saved_ptrs+16,a4
	movl	_saved_ptrs+20,a5
	movl	_saved_ptrs+24,d3
	movl	_saved_nonptrs,d1
	movl	_saved_nonptrs+4,d2
	movl	_saved_nonptrs+8,d4
go:	movl	_saved_pc,sp@-	
	rts














.align 2; .long ((1)*16+(1)); _floor_v: .long 7f; .long 1; .long 5; 7:
	fmoved a0@,fp0
	ftstx fp0		| handle positive and negative cases separately
	fblt 1f

	fintrzx fp0,fp0		| round towards zero (down)
	fmovel fp0,d0
	asll #1,d0
	trapv
	addql #1,d0
	movl d0,a0
	movl a1@,a2
	jmp a2@

1:	fintrzx fp0,fp1		| round towards zero (up)
	fmovel fp1,d0
        asll #1,d0
	trapv
	fcmpx fp0,fp1
	fjeq 1f
	subql #1,d0
	trapv
	movl d0,a0
	movl a1@,a2
	jmp a2@
1:	addql #1,d0
	movl d0,a0
	movl a1@,a2
	jmp a2@


.align 2; .long ((1)*16+(1)); _logb_v: .long 7f; .long 1; .long 5; 7:
	fgetexpd a0@,fp0
	fmovel fp0,d0
	asll	#1,d0
	addql #1,d0
	movl d0,a0
	movl a1@,a2
	jmp a2@

.align 2; .long ((1)*16+(1)); _scalb_v: .long 7f; .long 1; .long 5; 7:
	lea 2f,a0
	movl d7,a1 ; movl a1@,a2 ; jra a2@
	.align 2
	.long	((2)*16+(1));
2:	.long	1f
	.long	(_real_e0+4)
.align 2; .long 19*16+15; 1: .ascii "scalb unimplemented\0"

.align 2; .long ((1)*16+(1)); _callc_v: .long 7f; .long 1; .long 5; 7:
	movl a1,sp@-
	movl a0@,a1
	movl a0@(4),sp@-
	jsr a1@
        movl d0,a0
        addql #4,sp
	movl sp@+,a1
	tstl _cause
	jne  _saveregs
	movl a1@,a2
	jmp a2@

.align 2; .long ((1)*16+(1)); _syscall_v: .long 7f; .long 1; .long 5; 7:        
	movl	a0@(8),d0  
	asrl	#1,d0
	movl	d0,d1
	asll	#2,d1
	subl	d1,sp
	movl	sp,a2
	movl	a0@(4),a3
1:	subl	#1,d0
	blt	2f
	movl	a3@,d1
	movl	a3@(4),a3
	btst	#0,d1
	jeq	3f
	asrl	#1,d1
3:	movl	d1,a2@+
	jra	1b
2:	pea	0
	movl	a0@,d0
	asrl	#1,d0
	movl	d0,sp@-
	trap	#0
	jcs	1f
	movl	a2,sp
	addl	d0,d0
	addql	#1,d0
	movl	d0,a0
	movl a1@,a2
	jmp a2@

1:	movl	a2,sp
	movl	#-1,d0
	movl	d0,a0
	movl a1@,a2
	jmp a2@



.globl _minitfp_		
.globl _fp_state_mc68881	


.align 2


.globl _fpenable
.globl _fpdisable
_fpenable:

	jsr _minitfp_			





	cmpl #2,_fp_state_mc68881
	jne 1f

	fmovel #0x3400,fpcr
1:	rts
_fpdisable:

	cmpl #2,_fp_state_mc68881
	jne 1f

	fmovel #0,fpcr
1:	rts


	.text
	.align	2
	.globl	_mathvec
	.long	((9)*16+(1))
_mathvec:
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
.align 2; .long ((1)*16+(1)); _math_functor: .long 7f; .long 1; .long 5; 7:
	lea _mathvec,a0
	movl a1@,a2
	jmp a2@

	.align 2
	.long ((2)*16+(1))
_exp_e:	.long 1
	.long 1f
	.long ((1)*16+(9))
1:	.long 1f
.align 2; .long 3*16+15; 1: .ascii "Exp\0"

	.align 2
	.long ((2)*16+(1))
_ln_e:	.long 1
	.long 1f
	.long ((1)*16+(9))
1:	.long 1f
.align 2; .long 2*16+15; 1: .ascii "Ln\0\0"

	.align 2
	.long ((2)*16+(1))
_sqrt_e:
	.long 1
	.long 1f
	.long ((1)*16+(9))
1:	.long 1f
.align 2; .long 4*16+15; 1: .ascii "Sqrt"

.align 2; .long ((1)*16+(1)); _sin_v: .long 7f; .long 1; .long 5; 7:
	fsind a0@,fp0
finishfloat:
        cmpl a6,d5
        trapmi
	movl #((8)*16+(15)),a6@+
	movl a6,a0
	fmoved fp0,a6@+
	movl a1@,a2
	jmp a2@

.align 2; .long ((1)*16+(1)); _cos_v: .long 7f; .long 1; .long 5; 7:
	fcosd a0@,fp0
        jra finishfloat
.align 2; .long ((1)*16+(1)); _ln_v: .long 7f; .long 1; .long 5; 7:
	flognd a0@,fp0
        jra finishfloat
.align 2; .long ((1)*16+(1)); _exp_v: .long 7f; .long 1; .long 5; 7:
	fetoxd a0@,fp0
        jra finishfloat
.align 2; .long ((1)*16+(1)); _sqrt_v: .long 7f; .long 1; .long 5; 7:
	fsqrtd a0@,fp0
        jra finishfloat
.align 2; .long ((1)*16+(1)); _atan_v: .long 7f; .long 1; .long 5; 7:
	fatand a0@,fp0
        jra finishfloat


	.globl	_startptr
_startptr: .long    start
