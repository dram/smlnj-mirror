/* Mac.prim.c  */
/* created 4Dec92  e  from the file M68.prim.s of 17Nov92 */

/* how to make this file:
  0) run M68.prim.s through ThinkC preprocessor
  1) grep: sp@(\([0-9]*\)) => \1(sp)
  2) grep: sp@(\([+0-9]*\)) => (\1)(sp)
  3) grep: sp@(\(-[0-9]*\)) => \1(sp)
  4) frep: sp@- => -(sp)
  5) frep: sp@ => (sp)
  6) grep: a\([0-9]*\)@(\([0-9]*\)) => \2(a\1)
  7) grep: a\([0-9]*\)@(\([+0-9]*\)) => (\2)(a\1)
  8) grep: a\([0-9]*\)@(\(-[0-9]*\)) => \2(a\1)
  9) grep: a\([0-9]*\)@- => -(a\1)
 10) grep: a\([0-9]*\)@ => (a\1)
 11) frep: " 0(a" => " (a" [ note the leading space!]
 12) frep: .word => dc.w
 13) frep: jra => bra
 14) frep: jgt => bgt
 15) frep: jne => bne
 16) frep: jpl => bpl
 17) frep: jge => bge
 18) frep: jeq => beq
 19) frep: movl => move.l
 1a) frep: cmpl => cmp.l
 1b) frep: addl => add.l
 1c) frep: andl => and.l [whole tokens only!]
 1d) frep: orl => or.l
 1e) frep: asll => asl.l
 1f) frep: asrl => asr.l
 1g) frep: fmovel => fmove.l
 1h) frep: addql => addq.l
 1i) frep: subql => subq.l
 1j) frep: tstl => tst.l
 1k) frep: clrl => clr.l
 1l) frep: moveml => movem.l
 1m) frep: fmoved => fmove.d
 1n) frep: movw => move.w
 1o) frep: ftstx => ftst.x
 1p) frep: fgetexpd => fgetexp.d
 1q) frep: fatand => fatan.d
 1r) frep: fcosd => fcos.d
 1s) frep: fetoxd => fetox.d
 1t) frep: flognx => flogn.x
 1u) frep: fsind => fsin.d
 1v) frep: fsqrtx => fsqrt.x
 1w) frep: fintrzx => fintrz.x
 1x) frep: fcmpx => fcmp.x

 20) replace "; " with \n
 21) remove .align 2 delarations
 22) replace .globl declarations with C declarations
 23) remove leading underscores on labels

 2a) grep: \([0-9]\): => @\1:
 2b) manually patch branch labels
     - remove extraneous labels
     - change 1f & 1b to @1 as appropriate
     - change 1f & 1b to @9 as appropriate
     - change 2f & 2b to @2
     - change 3f & 3b to @3
     - change 4f & 4b to @4
 2c) manually replace @<digit> by <function-name>_<digit>
       merging multiple labeles to the same address
 2d) change quicksave => @quicksave
 2e) change finishfloat => @finishfloat
 2f) prefix appropriate labels with extern

 3a) manually format
 3b) wrap the code with a dummy c function
 3c) add the next few lines...
*/

#include "ml_state.h"
extern MLState_t *MLproc;
extern int overflow_e0[];
extern int ln_e0[];
extern int sqrt_e0[];

int sigh_resume();
int saveregs();
int restoreregs();
int sigh_return_a();
int handle_a();
int return_a();
int request_fault();
int callc_a();
int savefpregs();
int restorefpregs();
int adjust_limit();
int saved_pc();
int array_a();
int create_r_a();
int create_b_a();
int create_s_a();
int create_v_a();
int try_lock_a();
int unlock_a();
int floor_a();
int logb_a();
int scalb_a();
int fpenable();
int fpdisable();
int arctan_a();
int cos_a();
int exp_a();
int ln_a();
int sin_a();
int sqrt_a();

void dummy_c_fun()
{
asm	{

	dc.w ((4*(5))+2)
extern sigh_return_a:
	move.l (sp)+,a5
	move.l 48(sp),a5
	move.l #6,48(a5)
	bra @quicksave

extern sigh_resume:
	move.l (sp)+,a5
	move.l 48(sp),a5
	move.l #7,48(a5)
	bra @quicksave
	
	dc.w ((4*(5))+2)
extern handle_a:
	move.l (sp)+,a5
	move.l 48(sp),a5
	move.l #1,48(a5)
	bra @quicksave
	
	dc.w ((4*(5))+2)
extern return_a:
	move.l (sp)+,a5
	move.l 48(sp),a5
	move.l #0,48(a5)
	bra @quicksave
	
extern request_fault:
	move.l (sp)+,a5
	move.l 48(sp),a5
	move.l #2,48(a5)
	bra @quicksave
	
	dc.w ((4*(5))+2)
extern callc_a:
	bgt @callc_a_2
	lea @callc_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@callc_a_2:
	move.l (sp)+,a5
	move.l 48(sp),a5
	move.l #4,48(a5)

@quicksave:
	clr.l 44(a5)
	move.l a6,(a5)
	movem.l d6-d7/a0-a2,8(a5)
	move.l d3,36(a5)
	movem.l (sp)+,d2-d7/a2-a6
	rts

extern saveregs:
	pea (a5)
	move.l (48+4)(sp),a5
	move.l (sp)+,40(a5)
	clr.l 44(a5)
	tst.l d5
	bne @saveregs_1
	move.l #5,48(a5)
@saveregs_1:
	move.l d4,96(a5)
	move.l a6,(a5)
	movem.l d6-d7/a0-a4,8(a5)
	move.l d3,36(a5)
	movem.l (sp)+,d2-d7/a2-a6
	rts

extern restoreregs:
	movem.l d2-d7/a2-a6,-(sp)
	move.l 48(sp),a5
	move.l (a5),a6
	movem.l 4(a5),d5-d7/a0-a4
	move.l 36(a5),d3
	addq.l #1,44(a5)
	tst.l 72(a5)
	bne @restoreregs_3
	tst.l 64(a5)
	bne @restoreregs_2
@restoreregs_1:
	pea @saveregs
	move.l 40(a5),a5
	cmp.l a6,d5
	jmp (a5)
@restoreregs_2:
	tst.l 60(a5)
	bne @restoreregs_1
	tst.l 56(a5)
	bne @restoreregs_1
	addq.l #1,52(a5)
@restoreregs_3:
	clr.l d5
	bra @restoreregs_1
	
extern savefpregs:
	link a6, #-4
	move.l a0, -4(a6)
	move.l MLproc, a0
	move.l (a0), a0
	move.l #((6*8)*64+(((4*(0x8+2))+2))), (a0)+
	fmove.d fp2, (a0)+
	fmove.d fp3, (a0)+
	fmove.d fp4, (a0)+
	fmove.d fp5, (a0)+
	fmove.d fp6, (a0)+
	fmove.d fp7, (a0)+
	move.l -4(a6), a0
	unlk a6
	rts
	
extern restorefpregs:
	link a6, #-4
	move.l a0, -4(a6)
	move.l 8(a6), a0
	fmove.d (a0)+, fp2
	fmove.d (a0)+, fp3
	fmove.d (a0)+, fp4
	fmove.d (a0)+, fp5
	fmove.d (a0)+, fp6
	fmove.d (a0)+, fp7
	move.l -4(a6), a0
	unlk a6
	rts
/*
extern adjust_limit:
	move.w cc,d5
	move.l saved_pc,-(sp)
	move.w d5,-(sp)
	clr.l d5
	rtr
*/
	dc.w ((4*(5))+2)
extern array_a:
	move.l (a0),d1
	asr.l #1,d1
	move.l d1,d2
	asl.l #6,d2
	or.l #((4*(0x8+1))+2),d2
	asl.l #2,d1
	move.l a6,d4
	add.l d1,d4
	cmp.l d4,d5
	bpl @array_a_4
	lea @array_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@array_a_4:
	move.l 4(a0),d0
	move.l d2,(a6)+
	move.l a6,a0
	bra @array_a_3
@array_a_2:
	move.l d0,(a6)+
@array_a_3:
	subq.l #4,d1
	bge @array_a_2
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern create_r_a:
	move.l a0,d1
	asr.l #1,d1
	move.l d1,d2
	asl.l #6,d2
	asl.l #3,d1
	add.l #((4*(0x8+5))+2),d2
	move.l a6,d4
	add.l d1,d4
	cmp.l d4,d5
	bpl @create_r_a_4
	lea @create_r_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@create_r_a_4:
	move.l d2,(a6)+
	move.l a6,a0
	add.l d1,a6
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern create_b_a:
	move.l a0,d1
	asr.l #1,d1
	move.l d1,d2
	asl.l #6,d2
	add.l #3,d1
	add.l #((4*(0x8+4))+2),d2
	and.l #~3,d1
	move.l a6,d4
	add.l d1,d4
	cmp.l d4,d5
	bpl @create_b_a_4
	lea @create_b_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@create_b_a_4:
	move.l d2,(a6)+
	move.l a6,a0
	add.l d1,a6
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern create_s_a:
	move.l a0,d1
	asr.l #1,d1
	move.l d1,d2
	asl.l #6,d2
	add.l #3,d1
	add.l #((4*(0x8+2))+2),d2
	and.l #~3,d1
	move.l a6,d4
	add.l d1,d4
	cmp.l d4,d5
	bpl @create_s_a_4
	lea @create_s_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@create_s_a_4:
	move.l d2,(a6)+
	move.l a6,a0
	add.l d1,a6
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern create_v_a:
	move.l (a0),d1
	asr.l #1,d1
	move.l d1,d2
	asl.l #6,d2
	or.l #((4*(0x8+0))+2),d2
	asl.l #2,d1
	move.l a6,d4
	add.l d1,d4
	cmp.l d4,d5
	bpl @create_v_a_4
	lea @create_v_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@create_v_a_4:
	move.l d2,(a6)+
	move.l 4(a0),a5
	move.l a6,a0
	move.l #1,d1
@create_v_a_3:
	move.l (a5),(a6)+
	move.l 4(a5),a5
	cmp.l a5,d1
	bne @create_v_a_3
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern try_lock_a:
	move.l (a0),d0
	move.l #1,(a0)
	move.l d0,a0
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern unlock_a:
	move.l #3,(a0)
	move.l #1,a0
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern floor_a:
	fmove.d (a0),fp0
	ftst.x fp0
	fblt @floor_a_1
	fintrz.x fp0,fp0
	fmove.l fp0,d0
	asl.l #1,d0
	trapv
	addq.l #1,d0
	move.l d0,a0
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
@floor_a_1:
	fintrz.x fp0,fp1
	fmove.l fp1,d0
	asl.l #1,d0
	trapv
	fcmp.x fp0,fp1
	fbeq @floor_a_9
	subq.l #1,d0
	trapv
	move.l d0,a0
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
@floor_a_9:
	addq.l #1,d0
	move.l d0,a0
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern logb_a:
	fgetexp.d (a0),fp0
	fmove.l fp0,d0
	asl.l #1,d0
	addq.l #1,d0
	move.l d0,a0
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern scalb_a:
	lea overflow_e0+4,a0
	move.l d7,a1
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
extern fpenable:
	fmove.l #0x3400,fpcr
	rts

extern fpdisable:
	fmove.l #0,fpcr
	rts
	
	dc.w ((4*(5))+2)
extern arctan_a:
	bgt @arctan_a_2
	lea @arctan_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@arctan_a_2:
	fatan.d (a0),fp0
	bra @finishfloat
	
	dc.w ((4*(5))+2)
extern cos_a:
	bgt @cos_a_2
	lea @cos_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@cos_a_2:
	fcos.d (a0),fp0
	bra @finishfloat
	
	dc.w ((4*(5))+2)
extern exp_a:
	bgt @exp_a_2
	lea @exp_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@exp_a_2:
	fetox.d (a0),fp0
	bra @finishfloat
	
	dc.w ((4*(5))+2)
extern ln_a:
	bgt @ln_a_2
	lea @ln_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@ln_a_2:
	fmove.d (a0),fp0
	ftst.x fp0
	fble @ln_a_9
	flogn.x fp0,fp0
	bra @finishfloat
@ln_a_9:
	lea ln_e0+4,a0
	move.l d7,a1
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	dc.w ((4*(5))+2)
extern sin_a:
	bgt @sin_a_2
	lea @sin_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@sin_a_2:
	fsin.d (a0),fp0
	bra @finishfloat
	
	dc.w ((4*(5))+2)
extern sqrt_a:
	bgt @sqrt_a_2
	lea @sqrt_a,a5
	move.l #((1 * 16) - 1),d4
	rts
@sqrt_a_2:
	fmove.d (a0),fp0
	ftst.x fp0
	fblt @sqrt_a_9
	fsqrt.x fp0,fp0
	bra @finishfloat
@sqrt_a_9:
	lea sqrt_e0+4,a0
	move.l d7,a1
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)

@finishfloat:
	move.l #((2)*64+(((4*(1))+2))),(a6)+
	move.l a6,a0
	fmove.d fp0,(a6)+
	move.l (a1),a3
	cmp.l a6,d5
	jmp (a3)
	
	}
}
