










































































































































































































































	.text

	.globl	saveregs
	.globl	handle_c
	.globl	return_c
	.globl	restoreregs
	.globl	savefpregs
	.globl	restorefpregs









	.globl	saveregs
	.ent	saveregs



							    .globl	sigh_return_c;						    .align	2; 		    .set noreorder;						    .word	((1 << (3 + 3)) - 2); 	    sigh_return_c:   .word	5     ;					    .set reorder;
	li	$18   ,6
	b	set_request





	.globl	sigh_resume
sigh_resume:
	li	$18   ,7
	b	set_request

							    .globl	handle_c;						    .align	2; 		    .set noreorder;					    handle_c:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9: 
	li	$18   ,	1
	b	set_request

							    .globl	return_c;						    .align	2; 		    .set noreorder;						    .word	((1 << (3 + 3)) - 2); 	    return_c:   .word	5     ;					    .set reorder; 
	li	$18   ,0
	b	set_request

							    .globl	callc_v;						    .align	2; 		    .set noreorder;					    callc_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
	add	$0,$23,$19
	li	$18   ,4
	

	.globl	set_request
	.ent	set_request
set_request:
	lw	$21, (16+40)($sp)	
	sw	$18   ,               100($21)
	sw	$0,                   96($21)	
	sw	$23,0($21)
	sw	$22,8($21)
	sw	$2,12($21)
	sw	$3,16($21)
	sw	$4,20($21)
	sw	$30,24($21)
        sw      $5,32($21)
        sw      $6,36($21)
        sw      $7,40($21)
        sw      $8,44($21)
        sw      $9,48($21)
        sw      $10,52($21)
        sw      $11,56($21)
        sw      $12,60($21)
        sw      $13,64($21)
	sw	$20,88($21)
	sw	$24,92($21)
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
	addu	$sp,(44+4+16) 		
	j	$31			

saveregs:
	lw	$21, (16+40)($sp)	
	sw	$0,                   96($21)	
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
	sw	$20,88($21)
	sw	$24,92($21)
	b	restore_c_regs

	.end	saveregs

	.ent	restoreregs
restoreregs:
	subu	$sp,(44+4+16) 		
					
.frame $sp,(44+4+16) ,$0
.mask 0xc0ff0000,0-4
	sw	$4, (16+40)($sp)	
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
					
	move    $21,$4             
                                       
	lw	$23,0($21)
	lw	$19,4($21)
	li	$18   ,0x7fffffff	
	sub	$19,$18   ,$19
	lw	$22,8($21)
	li	$18   ,1
.set	noreorder			
	sw	$18   ,                   96($21)	
	lw	$18   ,             124($21)	
	nop
	beqz	$18   ,6f
	nop
	li	$19,0x7fffffff	
6:
	lw	$18   ,        116($21)	
	nop				
	bnez	$18   ,1f
	nop				
8:	lw	$2,12($21)
	lw	$3,16($21)
	lw	$4,20($21)
	lw	$30,24($21)
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
	lw	$20,88($21)
	lw	$24,92($21)
	lw	$21,28($21)
	nop
	.globl	go
	.ent	go
go:	j	$21			
	nop
1:				      
	lw	$18   ,           112($21)	
	nop				
	bnez	$18   ,8b
	nop				
	lw	$18   ,          108($21)	
	nop				
	bnez	$18   ,8b              
        li	$18   ,1		
	sw	$18   ,        104($21)	
	li	$19,0x7fffffff	
	beqz 	$0,8b
	nop
.set	reorder
	.end	restoreregs

	.text
	.ent savefpregs			
	.set reorder
savefpregs:
	move 	$18   , $4             
					
	li      $25  ,  ((5*8)*16+( 15    ))
	lw 	$18   , 	0	($18   )
        sw      $25  , 0($18   )       
	swc1	$f20,	4($18   )	
	swc1	$f21,   8($18   )
	swc1	$f22,   12($18   )	
	swc1	$f23,   16($18   )
	swc1	$f24,   20($18   )	
	swc1	$f25,   24($18   )
	swc1	$f26,   28($18   )	
	swc1	$f27,   32($18   )
	swc1	$f28,   36($18   )	
	swc1	$f29,   40($18   )
	j 	$31			
	.end	savefpregs

	.ent	restorefpregs		
	.set 	reorder
restorefpregs:
	add	$18   , $4, 0		
	lwc1	$f20,	0($18   )	
	lwc1	$f21,	4($18   )
	lwc1	$f22,	8($18   )
	lwc1	$f23,	12($18   )
	lwc1	$f24,	16($18   )
	lwc1	$f25,	20($18   )
	lwc1	$f26,	24($18   )
	lwc1	$f27,	28($18   )
	lwc1	$f28,	32($18   )
	lwc1	$f29,	36($18   )
	j	$31
	.end 	restorefpregs






							    .globl	try_lock_v;						    .align	2; 		    .set noreorder;					    try_lock_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:

	lw	$18   ,0($2)
	li	$25  ,1		
	sw	$25  ,0($2)
	move	$2,$18   
						            j		$3


							    .globl	unlock_v;						    .align	2; 		    .set noreorder;					    unlock_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:

	li	$18   ,3		
	sw	$18   ,0($2)
	li	$2,1		
						            j		$3






							    .globl	masksigs_v;						    .align	2; 		    .set noreorder;					    masksigs_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
.set	noreorder
	lw	$21, (16+40)($sp)
	sra	$2,1
	lw	$18   ,           112($21)
	beqz	$2,1f			
	li	$2,1			

	beqz	$0,2f
	add	$18   ,$2,$18   		
						

1:	sub	$18   ,$18   ,$2		
2:	sw	$18   ,           112($21)	
.set	reorder
						            j		$3




							    .globl	array_v;						    .align	2; 		    .set noreorder;					    array_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
1:					
	lw	$18   ,0($2)	
	lw	$15,4($2)		
	sra	$18   ,1		
	sll	$25  ,$18   ,4 
	ori	$25  ,9     
	sll	$18   ,2		
.set noreorder
	li	$17,0x7fffffff
	sub	$17,$17,$19	
	sub	$17,$17,$23	
	sub	$17,$17,$18   	
	blez	$17,3f		
	nop
.set reorder
	sw	$25  ,0($23)	
	add	$23,4		
	add	$17,$18   ,$23	
	move	$2,$23	
					
2:					
	sw	$15,0($23)	  
        addi	$23,4		  
	bne	$23,$17,2b	  
					
        					            j		$3

3:					
.set noreorder
	li	$19,0x7fffffff	
	b	4f
	nop
	.word	((1 << (3 + 3)) - 1) 
	.word	0	
4:	add	$0,$23,$19	
	b	1b
	nop
.set reorder






							    .globl	create_b_v;						    .align	2; 		    .set noreorder;					    create_b_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
	li	$11,11    	
	b	1f

							    .globl	create_s_v;						    .align	2; 		    .set noreorder;					    create_s_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
	li	$11,15    		
1:					
	addi	$18   ,$2,13	
	sra	$18   ,3		
	sll	$18   ,2 		
.set noreorder
	li	$17,0x7fffffff
	sub	$17,$17,$19	
	sub	$17,$17,$23	
	sub	$17,$17,$18   	
	blez	$17,2f		
	nop
.set reorder
	sra	$25  ,$2,1	
	sll	$25  ,4
	or	$25  ,$11
	sw	$25  ,0($23)	
	addi	$2,$23,4	
	add	$23,$18   	
						            j		$3

2:					
.set noreorder
	li	$19,0x7fffffff	
	b	4f
	nop
	.word	((1 << (3 + 3)) - 1)|0x200 
	.word	0	
4:	add	$0,$23,$19	
	b	1b
	nop
.set reorder





							    .globl	create_v_v;						    .align	2; 		    .set noreorder;					    create_v_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:



1:					
	lw	$18   ,0($2)	
	sra	$18   ,1		
	sll	$25  ,$18   ,4
	ori	$25  ,1     	
	sll	$18   ,2		
.set noreorder
	li	$17,0x7fffffff
	sub	$17,$17,$19	
	sub	$17,$17,$23	
	sub	$17,$17,$18   	
	blez	$17,5f		
	nop
.set reorder
	sw	$25  ,0($23)	
	addi	$23,4		
        lw      $25  , 4($2)      
	move	$2,$23	
        li      $17, 		1          
3:					
        lw      $15, 	0($25  ) 
        sw      $15, 0($23)     
        lw      $25  ,  	4($25  )
        addi    $23, 4            
        bne     $25  , $17, 3b      
4:
        					            j		$3

5:					
.set noreorder
	li	$19,0x7fffffff       
	b	6f
	nop
	.word	((1 << (3 + 3)) - 1) 
	.word	0	
6:	add	$0,$23,$19	
	b	1b
	nop
.set reorder














.set noreorder
maxint:	.double	1073741824.0
.set reorder
							    .globl	floor_v;						    .align	2; 		    .set noreorder;					    floor_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
	lwc1	$f4,(4-0)($2)	
	lwc1	$f5,0($2)	
	mtc1	$0,$f2			
 	lui	$17,0x41d0
	mtc1	$17,$f3
	abs.d	$f6,$f4
	c.le.d	$f6,$f2
	cfc1	$17,$31			
	bc1f	over
	ori	$25  ,$17,0x03		
	ctc1	$25  ,$31			
	cvt.w.d $f6,$f4			
	ctc1	$17,$31			
	mfc1	$2,$f6		
	add	$2,$2		
	add	$2,1		
						            j		$3


							    .globl	logb_v;						    .align	2; 		    .set noreorder;					    logb_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
	lw 	$2,0($2)
	srl 	$2,20		
	andi	$2,0x07ff		
	sub 	$2,1023		
	sll 	$2,1		
	add	$2,1		
						            j		$3

							    .globl	scalb_v;						    .align	2; 		    .set noreorder;					    scalb_v:   .word	 ((1)*16+( 1     ));		            .word	9f; 			    .word	((1 << (3 + 3)) - 1); 		    .word	5     ;					    .set reorder;					    9:
.set noreorder
	add	$0,$23,$19	
.set reorder
	lw 	$18   ,4($2)	
	sra	$18   ,1		
	beqz	$18   ,9f		
	lw	$21,0($2)	
	lw 	$25  ,0($21)	
	srl 	$25  ,20		
	andi	$25  ,0x07ff		
	add	$17,$25  ,$18   	
	blt	$17,1,under		
	bgt	$17,2046,over	
	xor	$17,$25  		
	sll	$17,20		
	lw	$25  ,0($21)	
	xor	$25  ,$17		
	sw	$25  ,0+4($23)	
	lw 	$25  ,(4-0)($21) 
	sw	$25  ,(4-0)+4($23)	
8:	li	$15, ((8)*16+(15    )) 
	sw	$15,0($23)	
	add	$2,$23,4	
	add	$23,12		
        					            j		$3

9:	lw	$2,0($2)	
						            j		$3

over:	li	$17,0x7fffffff
	add	$17,$17		

under:	sw	$0,4($23)		
	sw	$0,8($23)
	b	8b




	.globl	set_fsr
	.ent	set_fsr
set_fsr:
	cfc1	$18   ,$31		
	ori 	$18   ,$18   ,0xe00	
	ctc1	$18   ,$31		
	j	$31
	.end	set_fsr


	.globl	startptr
startptr: .word    __start
