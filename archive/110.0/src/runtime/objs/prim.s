# 1 "../mach-dep/MIPS.prim.asm"
 






 






































 

# 1 "../include/ml-base.h"
 








 






 














# 1 "../objs/ml-sizes.h"
 


















# 30 "../objs/ml-sizes.h"



# 32 "../include/ml-base.h"



 

 

 

 


 


 


 





# 197 "../include/ml-base.h"




# 49 "../mach-dep/MIPS.prim.asm"

# 1 "../include/asm-base.h"
 













 







 






# 54 "../include/asm-base.h"





# 1 "/usr/include/regdef.h"





 
















 
 
 
 
 
 
 

 

 




# 1 "/usr/include/sys/regdef.h"
 










 







# 1 "/usr/include/sgidefs.h"
 



















 















 






 









 






		 


 









 



































 











# 246 "/usr/include/sgidefs.h"







# 20 "/usr/include/sys/regdef.h"


# 61 "/usr/include/sys/regdef.h"


# 102 "/usr/include/sys/regdef.h"



# 38 "/usr/include/regdef.h"

# 1 "/usr/include/sys/fpregdef.h"

 










 







# 1 "/usr/include/sgidefs.h"
 










# 252 "/usr/include/sgidefs.h"

# 21 "/usr/include/sys/fpregdef.h"


 
























# 81 "/usr/include/sys/fpregdef.h"


# 116 "/usr/include/sys/fpregdef.h"


# 161 "/usr/include/sys/fpregdef.h"





# 39 "/usr/include/regdef.h"







# 59 "../include/asm-base.h"













# 139 "../include/asm-base.h"

















# 50 "../mach-dep/MIPS.prim.asm"

# 1 "../include/ml-values.h"
 


















 	 

# 33 "../include/ml-values.h"


# 52 "../include/ml-values.h"






# 71 "../include/ml-values.h"



 







# 51 "../mach-dep/MIPS.prim.asm"

# 1 "../include/tags.h"
 





















# 37 "../include/tags.h"

					 



					 
					 



 





 










 


 
 


 





					 


 



 















 












 








 







# 52 "../mach-dep/MIPS.prim.asm"

# 1 "../include/ml-request.h"
 




























# 53 "../mach-dep/MIPS.prim.asm"

# 1 "../objs/reg-mask.h"
 



















# 54 "../mach-dep/MIPS.prim.asm"

# 1 "../include/ml-limits.h"
 









# 1 "../include/ml-base.h"
 





# 199 "../include/ml-base.h"


# 11 "../include/ml-limits.h"







 




 




 









 




					     















 


 





 






 


 


 







# 97 "../include/ml-limits.h"













# 55 "../mach-dep/MIPS.prim.asm"

# 1 "../objs/mlstate-offsets.h"
 

































# 56 "../mach-dep/MIPS.prim.asm"


 


























 
























 
 
 
 








 








 

























 








































































	.text

 


.globl	sigh_return_a   	; 	       .align 2  	; 	sigh_return_a :  
	li	$25 	,	124	
	li	$15  ,	11 
	b	set_request

 



.globl	sigh_resume   	; 	sigh_resume :  
	li	$25 	,	124	
	li	$15  ,	12 
	b	set_request

 


.globl	pollh_return_a   	; 	       .align 2  	; 	pollh_return_a :  
	li	$25 	,	124	
	li	$15  ,        13 
	b	set_request

 


.globl	pollh_resume   	; 	pollh_resume :  
	li	$25 	,	124	
	li	$15  ,        14 
	b	set_request

.globl	handle_a   	; 	       .align 2  	; 	handle_a :  
	li	$25 	,	127	
	li	$15  ,		2 
	b	set_request

.globl	return_a   	; 	       .align 2  	; 	return_a :  
	li	$25 	,	124	
	li	$15  ,	1 
	b	set_request

.globl	request_fault   	; 	request_fault :  
	li	$25 	,	127	
	li	$15  ,	3 
	b	set_request

 

.globl	bind_cfun_a   	; 	       .align 2  	; 	bind_cfun_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	li	$25 	,	127	
	li	$15  ,	4 
	b	set_request

.globl	build_literals_a   	; 	       .align 2  	; 	build_literals_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	li	$25 	,	127	
	li	$15  ,15 
	b	set_request

.globl	callc_a   	; 	       .align 2  	; 	callc_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	li	$25 	,	127	
	li	$15  ,	5 
	 

set_request:				 
					 
					 

	move	$21 	,$25 			 
	lw	$25 	,0 ($29	)	 
	sw	$21 	,112 ($25 	)
	lw	$21 	,4 ($25 	)	 
	sw	$0	,8 ($21 	)	 
	sw	$23 	,8 ($25 	)
	sw	$22 	,108 ($25 	)
	sw	$5	,(16 +20) ($25 	)
	sw	$5	,(16 +16) ($25 	)	 
	sw	$2 	,(16 +0) ($25 	)
	sw	$4 	,(16 +8) ($25 	)
	sw	$3 	,(16 +4) ($25 	)
	sw	$20 	,(16 +76) ($25 	)
	sw	$30 	,(16 +12) ($25 	)
	move	$2 ,$15  			 

	sw	$6   ,(40+(4*(0))) ($25 	)

	sw	$7 ,(40+(4*(1))) ($25 	)

	sw	$8 ,(40+(4*(2))) ($25 	)
# 342 "../mach-dep/MIPS.prim.asm"




					 
restore_c_regs:
	lw	$15  ,16 ($29	)
	lw	$16  ,16 +4($29	)
	sw	$15  ,100 ($25 	)
	sw	$16  ,104 ($25 	)
	lw	$31,24 +36($29	)
	lw	$30,24 +32($29	)
        lw      $23,24 +28($29	)
        lw      $22,24 +24($29	)
        lw      $21,24 +20($29	)
        lw      $20,24 +16($29	)
        lw      $19,24 +12($29	)
        lw      $18,24 +8($29	)
        lw      $17,24 +4($29	)
        lw      $16,24 ($29	)
	addu	$29	,4096 			 
	j	$31				 

	.ent saveregs  
.globl	saveregs   	; 	saveregs :  
	move	$21 	,$25 			 
	lw	$25 	,0 ($29	)	 
	sw	$21 	,112 ($25 	)

# 436 "../mach-dep/MIPS.prim.asm"


	lw	$21 	,4 ($25 	)
	sw	$0	,8 ($21 	)	 
	sub     $24 	,32764			 
	sw	$23 	,8 ($25 	)
	sw	$22 	,108 ($25 	)
	sw	$2 	,(16 +0) ($25 	)
	sw	$3 	,(16 +4) ($25 	)
	sw	$4 	,(16 +8) ($25 	)
	sw	$31 	,(16 +16) ($25 	)
	sw	$30 	,(16 +12) ($25 	)
						 

	sw	$6   ,(40+(4*(0))) ($25 	)
	sw	$7 ,(40+(4*(1))) ($25 	)

	sw	$8 ,(40+(4*(2))) ($25 	)
	sw	$9 ,(40+(4*(3))) ($25 	)
	sw	$10 ,(40+(4*(4))) ($25 	)
	sw	$11 ,(40+(4*(5))) ($25 	)
	sw	$12 ,(40+(4*(6))) ($25 	)
	sw	$13 ,(40+(4*(7))) ($25 	)
	sw	$14 ,(40+(4*(8))) ($25 	)
	sw	$15 ,(40+(4*(9))) ($25 	)
	sw	$16 ,(40+(4*(10))) ($25 	)
	sw	$17 ,(40+(4*(11))) ($25 	)
	sw	$18 ,(40+(4*(12))) ($25 	)
	sw	$5	,(16 +20) ($25 	)
	sw	$24 	,(16 +80) ($25 	)		 
	sw	$20 	,(16 +76) ($25 	)
	li	$2 ,		0 
	b	restore_c_regs
	.end	saveregs


	.ent restoreregs  
.globl	restoreregs   	; 	restoreregs :  
	subu	$29	,4096 			 
	.frame	$29	,4096 ,$0	
	.mask	0xc0ff0000,0
						 
	la	$5,saveregs
	sw	$4 ,0 ($29	)	 
	sw	$5,4 ($29	)		 
	sw	$31,24 +36($29	)
	sw	$30,24 +32($29	)
        sw      $23,24 +28($29	)
        sw      $22,24 +24($29	)
        sw      $21,24 +20($29	)
        sw      $20,24 +16($29	)
        sw      $19,24 +12($29	)
        sw      $18,24 +8($29	)
        sw      $17,24 +4($29	)
        sw      $16,24 ($29	)
	move    $25 	,$4 			 

	lw	$15  ,100 ($25 	)  
	lw	$16  ,104 ($25 	)
	sw	$15  ,16 ($29	)
	sw	$16  ,16 +4($29	)

	lw	$23 	,8 ($25 	)
	lw	$19 	,12 ($25 	)
	lw	$22 	,108 ($25 	)
	li	$15  ,1
	lw	$21 	,4 ($25 	)
.set	noreorder			  
	sw	$15  ,8 ($21 	)  
	lw	$2 	,(16 +0) ($25 	)
	lw	$3 	,(16 +4) ($25 	)
	lw	$4 	,(16 +8) ($25 	)
	lw	$30 	,(16 +12) ($25 	)
	lw	$6   ,(40+(4*(0))) ($25 	)
	lw	$7 ,(40+(4*(1))) ($25 	)
	lw	$8 ,(40+(4*(2))) ($25 	)
	lw	$9 ,(40+(4*(3))) ($25 	)
	lw	$10 ,(40+(4*(4))) ($25 	)
	lw	$11 ,(40+(4*(5))) ($25 	)
	lw	$12 ,(40+(4*(6))) ($25 	)
	lw	$13 ,(40+(4*(7))) ($25 	)
	lw	$14 ,(40+(4*(8))) ($25 	)
	lw	$15 ,(40+(4*(9))) ($25 	)
	lw	$16 ,(40+(4*(10))) ($25 	)
	lw	$17 ,(40+(4*(11))) ($25 	)
	lw	$18 ,(40+(4*(12))) ($25 	)
	lw	$5	,(16 +20) ($25 	)
	lw	$20 	,(16 +76) ($25 	)
	lw 	$24 	,(16 +80) ($25 	)
	lw	$31 	,(16 +16) ($25 	)
	add     $24 	,32764			 
						 
	lw	$25 	,20 ($21 	)
.set	noat
	lw	$1,24 ($21 	)
	nop 
	add	$25 	,$25 	,$1
.set	at
	bnez	$25 	,pending_sigs
	nop					 
	.end	restoreregs
	.ent	ml_go
.globl	ml_go   	; 	ml_go :  
	j	$31 					 
	sltu	$25 	,$23 	,$19 		 
	.end	ml_go

pending_sigs:	 
					 
	lw	$25 	,16 ($21 	)
	nop 
	bnez	$25 	,ml_go	
					 
	li	$25 	,1
	sw	$25 	,12 ($21 	)
	b	ml_go
	move	$19 	,$23 			 
.set	reorder


 





	         .text 
	.ent SaveFPRegs  
.globl	SaveFPRegs   	; 	SaveFPRegs :  
	swc1	$f20,4($4	)		 
	swc1	$f21,8($4	)
	swc1	$f22,12($4	)		 
	swc1	$f23,16($4	)
	swc1	$f24,20($4	)		 
	swc1	$f25,24($4	)
	swc1	$f26,28($4	)		 
	swc1	$f27,32($4	)
	swc1	$f28,36($4	)		 
	swc1	$f29,40($4	)
	j 	$31				 
	.end SaveFPRegs  

 





	.ent RestoreFPRegs  
.globl	RestoreFPRegs   	; 	RestoreFPRegs :  			 
	lwc1	$f20,0($4	)		 
	lwc1	$f21,4($4	)
	lwc1	$f22,8($4	)
	lwc1	$f23,12($4	)
	lwc1	$f24,16($4	)
	lwc1	$f25,20($4	)
	lwc1	$f26,24($4	)
	lwc1	$f27,28($4	)
	lwc1	$f28,32($4	)
	lwc1	$f29,36($4	)
	j	$31	
	.end RestoreFPRegs  


 

 


.globl	array_a   	; 	       .align 2  	; 	array_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	lw	$15  ,0($2 	)		     
	sra	$15  ,1			     
	bgt	$15  ,512 ,1f	     
	sll	$17  ,$15  ,(2 +4 )        
	ori	$17  ,((((	         0x8  	 |           0x1  ) )*4) +          0x2  	) 
	sw	$17  ,0($23 	)	     
	addi	$23 	,4		     
	lw	$16  ,4($2 	)		     
	move	$2 	,$23 	
	sll	$15  ,2			     
	add	$15  ,$23 			     
2:					     
	sw	$16  ,0($23 	)		 
	addi	$23 	,4			 
	bne	$23 	,$15  ,2b		 
					     
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 

1:	 
	li	$25 	,	127	
	li	$15  ,	9 
	b	set_request

 


.globl	create_r_a   	; 	       .align 2  	; 	create_r_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	sra	$15  ,$2 	,1		     
	sll	$16  ,$15  ,1		     
	bgt	$16  ,512 ,1f	     
	sll	$17  ,$15  ,(2 +4 )        
	ori	$17  ,$17  ,((((	         0x8  	 |           0x5  ) )*4) +          0x2  	) 

	ori	$23 	,4		     
					     

	sw	$17  ,0($23 	)
	addi	$2 	,$23 	,4	     
	sll	$16  ,2			     
	addi	$16  ,4			     
	add	$23 	,$16  		     
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 

1:	 
	li	$25 	,	127	
	li	$15  ,8 
	b	set_request

 


.globl	create_b_a   	; 	       .align 2  	; 	create_b_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	sra	$15  ,$2 	,1		     
	addi	$16  ,$15  ,3		     
	sra	$16  ,2
	bgt	$16  ,512 ,1f	     
	sll	$17  ,$15  ,(2 +4 )       
	ori	$17  ,$17  ,((((	         0x8  	 |           0x4  ) )*4) +          0x2  	) 
	sw	$17  ,0($23 	)
	addi	$2 	,$23 	,4	     
	sll	$16  ,2			     
	addi	$16  ,4			     
	add	$23 	,$16  		     
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 
1:					 
	li	$25 	,	127	
	li	$15  ,7 
	b	set_request

 




.globl	create_s_a   	; 	       .align 2  	; 	create_s_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	sra	$15  ,$2 	,1		     
	addi	$16  ,$15  ,4		     
	sra	$16  ,2
	bgt	$16  ,512 ,1f	     
	sll	$17  ,$15  ,(2 +4 )       
	ori	$17  ,$17  ,((((	         0x8  	 |           0x2  ) )*4) +          0x2  	) 
	sw	$17  ,0($23 	)
	addi	$2 	,$23 	,4	     
	sll	$16  ,2			     
	addi	$16  ,4			     
	add	$23 	,$16  		     
	sw	$0	,-4($23 	)	     
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 
1:					 
	li	$25 	,	127	
	li	$15  ,6 
	b	set_request

 



.globl	create_v_a   	; 	       .align 2  	; 	create_v_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	lw	$15  ,0($2 	)		     
	sra	$15  ,1			     
	bgt	$15  ,512 ,1f	     
	sll	$16  ,$15  ,(2 +4 )       
	ori	$16  ,((((	         0x8  	 |           0x0  )  )*4) +          0x2  	) 
	sw	$16  ,0($23 	)	     
	addi	$23 	,4		     
	lw	$16  ,4($2 	)		     
	move	$2 	,$23 			     
	li	$17  ,			(((0)*2)+1)  
2:					     
	lw	$15  ,0($16  )		         
	lw	$16  ,4($16  )		         
	sw	$15  ,0($23 	)	         
	addi	$23 	,4		         
	bne	$16  ,$17  ,2b			 
					     
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 

1:	 
	li	$25 	,	127	
	li	$15  ,10 
	b	set_request








 





.globl	floor_a   	; 	       .align 2  	; 	floor_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	lwc1	$f4,(4-0 ) ($2 	)	 
	lwc1	$f5,0 ($2 	)	 
	cfc1	$17  ,$31		 
	ori	$16  ,$17  ,0x03	 
	ctc1	$16  ,$31		 
	cvt.w.d $f6,$f4			 
	ctc1	$17  ,$31		 
	mfc1	$2 	,$f6		 
	add	$2 	,$2 			 
	add	$2 	,1		 
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 


.globl	logb_a   	; 	       .align 2  	; 	logb_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	lw 	$2 	,0 ($2 	)	 
	srl 	$2 	,20		 
	andi	$2 	,0x07ff		 
	sub 	$2 	,1023		 
	sll 	$2 	,1		 
	add	$2 	,1		 
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 

.globl	scalb_a   	; 	       .align 2  	; 	scalb_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	lw 	$15  ,4($2 	)		 
	sra	$15  ,1			 
	beqz	$15  ,9f		 
	lw	$25 	,0($2 	)	 
	lw 	$16  ,0 ($25 	)	 
	srl 	$16  ,20		 
	andi	$16  ,0x07ff		 
	add	$17  ,$16  ,$15  	 
	blt	$17  ,1,under		 
	bgt	$17  ,2046,over		 
	xor	$17  ,$16  		 
	sll	$17  ,20		 
	lw	$16  ,0 ($25 	)	 
	xor	$16  ,$17  		 
	sw	$16  ,0 +4($23 	)	 
	lw 	$16  ,(4-0 ) ($25 	)  
	sw	$16  ,(4-0 ) +4($23 	)	 
8:	li	$18  ,(((2)*64) + ((( (	         0x0  	 |           0x1  ) )*4) +          0x2  	) )           
	sw	$18  ,0($23 	)	 
	add	$2 	,$23 	,4	 
	add	$23 	,12		 
        sltu	$21 	,$23 	,$19 	;	j		$3 	; 

9:	lw	$2 	,0($2 	)	 
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 

over:	li	$17  ,0x7fffffff
	add	$17  ,$17  		 

under:	sw	$0	,4($23 	)		 
	sw	$0	,8($23 	)
	b	8b

 



.globl	try_lock_a   	; 	       .align 2  	; 	try_lock_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
# 829 "../mach-dep/MIPS.prim.asm"

	lw	$15  ,0($2 	)
	li	$16  ,			(((1)*2)+1)  
	sw	$16  ,0($2 	)
	move	$2 	,$16  
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 


 


.globl	unlock_a   	; 	       .align 2  	; 	unlock_a :  
	bnez	$21 	,3f;	li		$25 	,	127	;	move	$31 	,$5	;	b		saveregs;	3: 
	li	$15  ,		(((0)*2)+1)  
	sw	$15  ,0($2 	)
	li	$2 	,			(((0)*2)+1)  
	sltu	$21 	,$23 	,$19 	;	j		$3 	; 

 
# 882 "../mach-dep/MIPS.prim.asm"


 


	.ent SetFSR  
.globl	SetFSR   	; 	SetFSR :  
	cfc1	$8	,$31		 
	ori 	$8	,$8	,0x00	 
	ctc1	$8	,$31		 
	j	$31	
	.end SetFSR  


# 908 "../mach-dep/MIPS.prim.asm"

