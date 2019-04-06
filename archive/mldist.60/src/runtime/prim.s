













































































































































	.text

	.globl	request
	.globl	MLState
	.globl	saveregs
	.globl	handle_c
	.globl	return_c
	.globl	restoreregs








	.ent	saveregs



							    .globl	sigh_return_c;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    sigh_return_c:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
	li	$24,6
	b	set_request





	.globl	sigh_resume
sigh_resume:
	li	$24,7
	b	set_request

							    .globl	handle_c;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    handle_c:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9: 
	li	$24,	1
	b	set_request

							    .globl	return_c;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    return_c:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9: 
	li	$24,0
	b	set_request

							    .globl	callc_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    callc_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
	add	$0,$23,$19
	li	$24,4
	

set_request:
	sw	$24,request
	sw	$0,inML			
	lw	$21,MLState		
	sw	$23,0($21)
	sw	$22,8($21)
	sw	$2,12($21)
	sw	$3,16($21)
	sw	$30,24($21)
restore_c_regs:
	lw	$31,16+36($sp)	
	lw	$30,16+32($sp)
        lw      $23,16+28($sp)
        lw      $22,16+24($sp)
        lw      $21,16+20($sp)
        lw      $20,16+16($sp)
        lw      $19,16+12($sp)
        lw      $18,16+8($sp)
        lw      $17,16+4($sp)
        lw      $16,16($sp)
	addu	$sp,(40+4+16) 		
	j	$31			

saveregs:
	sw	$0,inML			
	lw	$21,MLState		
	sw	$23,0($21)
	sw	$22,8($21)
	sw	$2,12($21)
	sw	$3,16($21)
	sw	$4,20($21)
	sw	$30,24($21)
	sw	$5,32($21)		  
	sw	$6,36($21)
	sw	$7,40($21)
	sw	$8,44($21)
	sw	$9,48($21)
	sw	$10,52($21)
	sw	$11,56($21)
	sw	$12,60($21)
	sw	$13,64($21)
	sw	$14,68($21)
	sw	$15,72($21)
	sw	$16,76($21)
	sw	$17,80($21)
	sw	$18,84($21)
	b	restore_c_regs

	.end	saveregs

	.ent	restoreregs
restoreregs:
	subu	$sp,(40+4+16) 		
					
.mask 0xd0ff0000,0-4
	sw	$31,16+36($sp)
	sw	$30,16+32($sp)
        sw      $23,16+28($sp)
        sw      $22,16+24($sp)
        sw      $21,16+20($sp)
        sw      $20,16+16($sp)
        sw      $19,16+12($sp)
        sw      $18,16+8($sp)
        sw      $17,16+4($sp)
        sw      $16,16($sp)
					
	lw	$21,MLState
	lw	$23,0($21)
	lw	$19,4($21)
	li	$24,0x7fffffff	
	sub	$19,$24,$19
	lw	$22,8($21)
	lw	$2,12($21)
	lw	$3,16($21)
	lw	$4,20($21)
	lw	$30,24($21)
	lw	$24,28($21)	
	lw	$5,32($21)
	lw	$6,36($21)
	lw	$7,40($21)
	lw	$8,44($21)
	lw	$9,48($21)
	lw	$10,52($21)
	lw	$11,56($21)
	lw	$12,60($21)
	lw	$13,64($21)
	lw	$14,68($21)
	lw	$15,72($21)
	lw	$16,76($21)
	lw	$17,80($21)
	lw	$18,84($21)
	li	$25,1
.set	noreorder			
	sw	$25,inML		
	lw	$20,NumPendingSigs	
	nop				
	bnez	$20,1f
	nop				
	j	$24			
	nop
1:				      
	lw	$20,maskSignals	
	nop				
	bnez	$20,2f
	nop				
	lw	$20,inSigHandler	
	nop				
	bnez	$20,2f
	nop				
	sw	$25,handlerPending	
	li	$19,0x7fffffff	
2:
	j	$24			
	nop				
.set	reorder
	.end	restoreregs





							    .globl	array_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    array_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
1:					
	lw	$24,0($2)	
	lw	$10,4($2)		
	sra	$24,1		
	sll	$25,$24,4 
	ori	$25,9     
	sll	$24,2		
.set noreorder
	li	$20,0x7fffffff
	sub	$20,$20,$19	
	sub	$20,$20,$23	
	sub	$20,$20,$24	
	blez	$20,3f		
	nop
.set reorder
	sw	$25,0($23)	
	add	$23,4		
	add	$20,$24,$23	
	move	$2,$23	
					
2:					
	sw	$10,0($23)	  
        addi	$23,4		  
	bne	$23,$20,2b	  
					
        						    lw		$10,0($3);				    j		$10

3:					
.set noreorder
	li	$19,0x7fffffff	
	add	$0,$23,$19	
	b	1b
	nop
.set reorder






							    .globl	create_b_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    create_b_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
	li	$11,11    	
	b	1f

							    .globl	create_s_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    create_s_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
	li	$11,15    		
1:					
	addi	$24,$2,13	
	sra	$24,3		
	sll	$24,2 		
.set noreorder
	li	$20,0x7fffffff
	sub	$20,$20,$19	
	sub	$20,$20,$23	
	sub	$20,$20,$24	
	blez	$20,2f		
	nop
.set reorder
	sra	$25,$2,1	
	sll	$25,4
	or	$25,$11
	sw	$25,0($23)	
	addi	$2,$23,4	
	add	$23,$24	
							    lw		$10,0($3);				    j		$10

2:					
.set noreorder
	li	$19,0x7fffffff	
	add	$0,$23,$19	
	b	1b
	nop
.set reorder















maxint:	.double	1073741824.0

							    .globl	floor_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    floor_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
	lwc1	$f4,(4-0)($2)	
	lwc1	$f5,0($2)	
	lwc1	$f2,maxint+(4-0)
	lwc1	$f3,maxint+0
	abs.d	$f6,$f4
	c.le.d	$f6,$f2
	cfc1	$20,$31			
	bc1f	over
	ori	$24,$20,0x03		
	ctc1	$24,$31			
	cvt.w.d $f6,$f4			
	ctc1	$20,$31			
	mfc1	$2,$f6		
	add	$2,$2		
	add	$2,1		
							    lw		$10,0($3);				    j		$10

							    .globl	logb_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    logb_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
	lw 	$2,0($2)
	srl 	$2,20		
	andi	$2,0x07ff		
	sub 	$2,1023		
	sll 	$2,1		
	add	$2,1		
							    lw		$10,0($3);				    j		$10

							    .globl	scalb_v;						    .align	2; 		    .set noreorder;						    .word	 ((1)*16+( 1     ));		    scalb_v:   .word	9f; 			    .word	1; 		    .word	5     ;					    .set reorder;					    9:
.set noreorder
	add	$0,$23,$19	
.set reorder
	lw 	$24,4($2)	
	sra	$24,1		
	beqz	$24,9f		
	lw	$21,0($2)	
	lw 	$25,0($21)	
	srl 	$25,20		
	andi	$25,0x07ff		
	add	$20,$25,$24	
	blt	$20,1,under		
	bgt	$20,2046,over	
	xor	$20,$25		
	sll	$20,20		
	lw	$25,0($21)	
	xor	$25,$20		
	sw	$25,0+4($23)	
	lw 	$25,(4-0)($21) 
	sw	$25,(4-0)+4($23)	
8:	li	$10, ((8)*16+(15    )) 
	sw	$10,0($23)	
	add	$2,$23,4	
	add	$23,12		
	lw 	$10,0($3)		
	j 	$10			

9:	lw	$2,0($2)	
							    lw		$10,0($3);				    j		$10

over:	li	$20,0x7fffffff
	add	$20,$20		

under:	sw	$0,4($23)		
	sw	$0,8($23)
	b	8b




	.globl	set_fsr
	.ent	set_fsr
set_fsr:
	cfc1	$24,$31		
	ori 	$24,$24,0x600	
	ctc1	$24,$31		
	j	$31
	.end	set_fsr


	.globl	startptr
startptr: .word    __start


