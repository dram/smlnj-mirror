
	.DATA

L6
	.WORD 2
	.STRING	" O"

	.DATA

L7
	.WORD 2
	.STRING	" ."

	.DATA

L13
	.WORD 1
	.STRING	"\n"

	.DATA

L16
	.WORD 1
	.STRING	"\n"

	.CODE
	.EXPORT	printboard,ENTRY
	.PROC
	.CALLINFO
printboard
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 12(%r18)	/*  */
	STW	%r16, 16(%r18)	/*  */
	STW	%r15, 20(%r18)	/*  */
		OR	%r2, %r0, %r15

	STW	%r26, 0(%r18)	/*  */
L33
	LDO	0(%r0), %r16
	LDW	0(%r18), %r17	/*  */
	LDW	4(%r17), %r1	/*  */
	ADDIO	-1, %r1, %r22
	STW	%r22, 8(%r18)	/*  */
L14
	LDW	8(%r18), %r23	/*  */
	COMBT,<=,n	%r16, %r23, L15
	NOP
LL13
L4
	LDW	0(%r18), %r19	/*  */
	LDW	0(%r19), %r26	/*  */
	ADDIL	LR'L16-$global$, %r27, %r1
	LDO	RR'L16-$global$(%r1), %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r18

	B,n	L32
	NOP
L15
	LDO	0(%r0), %r17
	LDW	0(%r18), %r20	/*  */
	LDW	4(%r20), %r19	/*  */
	ADDIO	-1, %r19, %r20
	STW	%r20, 4(%r18)	/*  */
L11
	LDW	4(%r18), %r21	/*  */
	COMBT,<=,n	%r17, %r21, L12
	NOP
LL20
L5
	LDW	0(%r18), %r21	/*  */
	LDW	0(%r21), %r26	/*  */
	ADDIL	LR'L13-$global$, %r27, %r1
	LDO	RR'L13-$global$(%r1), %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r23
	SUB	%r30, %r23, %r18
	ADDI	1, %r16, %r16
	B,n	L14
	NOP
L12
	LDW	0(%r18), %r24	/*  */
	LDW	0(%r24), %r26	/*  */
	LDW	0(%r18), %r29	/*  */
	LDW	12(%r29), %r28	/*  */
	ZDEP	%r16, 29, 30, %r31
	LDWX	%r31(%r28), %r25	/*  */
	COMBT,=,n	%r25, %r17, L8
	NOP
LL24
L9
	ADDIL	LR'L7-$global$, %r27, %r1
	LDO	RR'L7-$global$(%r1), %r25
L10



	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r2
	SUB	%r30, %r2, %r18
	ADDI	1, %r17, %r17
	B,n	L11
	NOP
L8
	ADDIL	LR'L6-$global$, %r27, %r1
	LDO	RR'L6-$global$(%r1), %r25
	B,n	L10
	NOP
L32
		OR	%r15, %r0, %r2

	LDW	20(%r18), %r15	/*  */
	LDW	16(%r18), %r16	/*  */
	LDW	12(%r18), %r17	/*  */
	LDO	128(%r0), %r19
	SUB	%r30, %r19, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	$global$,DATA
	.IMPORT	print,CODE

	.CODE
	.EXPORT	try,ENTRY
	.PROC
	.CALLINFO
try
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 8(%r18)	/*  */
	STW	%r16, 12(%r18)	/*  */
	STW	%r15, 16(%r18)	/*  */
		OR	%r2, %r0, %r15

	STW	%r26, 0(%r18)	/*  */
		OR	%r25, %r0, %r16

L35
	LDW	0(%r18), %r17	/*  */
	LDW	4(%r17), %r24	/*  */
	COMBT,=,n	%r16, %r24, L29
	NOP
LL32
L30
	LDO	0(%r0), %r17
	LDW	0(%r18), %r20	/*  */
	LDW	4(%r20), %r19	/*  */
	ADDIO	-1, %r19, %r1
	STW	%r1, 4(%r18)	/*  */
L27
	LDW	4(%r18), %r2	/*  */
	COMBT,<=,n	%r17, %r2, L28
	NOP
LL36
L17
L31
	LDO	0(%r0), %r28
	B,n	L34
	NOP
L29
	LDW	0(%r18), %r26	/*  */

	BL,n	printboard, %r2
	NOP
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r18
	B,n	L31
	NOP
L28
	LDW	0(%r18), %r24	/*  */
	LDW	8(%r24), %r23	/*  */
	ZDEP	%r17, 29, 30, %r25
	LDWX	%r25(%r23), %r22	/*  */
	COMIBT,=,n	0, %r22, L18
	NOP
LL42
L19
L22
L25
L26
	ADDI	1, %r17, %r17
	B,n	L27
	NOP
L18
	LDW	0(%r18), %r29	/*  */
	LDW	16(%r29), %r28	/*  */
	ADD	%r17, %r16, %r1
	ZDEP	%r1, 29, 30, %r31
	LDWX	%r31(%r28), %r26	/*  */
	COMIBF,=,n	0, %r26, L22
	NOP
LL47
L21
	LDW	0(%r18), %r20	/*  */
	LDW	20(%r20), %r19	/*  */
	ADDI	7, %r17, %r23
	SUB	%r23, %r16, %r22
	ZDEP	%r22, 29, 30, %r21
	LDWX	%r21(%r19), %r2	/*  */
	COMIBF,=,n	0, %r2, L25
	NOP
LL49
L24
	LDW	0(%r18), %r25	/*  */
	LDW	8(%r25), %r24	/*  */
	ZDEP	%r17, 29, 30, %r26
	ADD	%r24, %r26, %r28
	LDO	1(%r0), %r29
	STW	%r29, 0(%r28)	/*  */
	LDW	0(%r18), %r1	/*  */
	LDW	16(%r1), %r31	/*  */
	ADD	%r17, %r16, %r19
	ZDEP	%r19, 29, 30, %r2
	ADD	%r31, %r2, %r20
	LDO	1(%r0), %r21
	STW	%r21, 0(%r20)	/*  */
	LDW	0(%r18), %r23	/*  */
	LDW	20(%r23), %r22	/*  */
	ADDI	7, %r17, %r26
	SUB	%r26, %r16, %r25
	ZDEP	%r25, 29, 30, %r24
	ADD	%r22, %r24, %r28
	LDO	1(%r0), %r29
	STW	%r29, 0(%r28)	/*  */
	LDW	0(%r18), %r1	/*  */
	LDW	12(%r1), %r31	/*  */
	ZDEP	%r16, 29, 30, %r2
	ADD	%r31, %r2, %r19
	STW	%r17, 0(%r19)	/*  */
	LDW	0(%r18), %r26	/*  */
	ADDI	1, %r16, %r25

	BL,n	try, %r2
	NOP
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r18
	LDW	0(%r18), %r23	/*  */
	LDW	8(%r23), %r22	/*  */
	ZDEP	%r17, 29, 30, %r24
	ADD	%r22, %r24, %r25
	LDO	0(%r0), %r26
	STW	%r26, 0(%r25)	/*  */
	LDW	0(%r18), %r29	/*  */
	LDW	16(%r29), %r28	/*  */
	ADD	%r17, %r16, %r1
	ZDEP	%r1, 29, 30, %r31
	ADD	%r28, %r31, %r2
	LDO	0(%r0), %r19
	STW	%r19, 0(%r2)	/*  */
	LDW	0(%r18), %r21	/*  */
	LDW	20(%r21), %r20	/*  */
	ADDI	7, %r17, %r24
	SUB	%r24, %r16, %r23
	ZDEP	%r23, 29, 30, %r22
	ADD	%r20, %r22, %r25
	LDO	0(%r0), %r26
	STW	%r26, 0(%r25)	/*  */
	B,n	L26
	NOP
L34
		OR	%r15, %r0, %r2

	LDW	16(%r18), %r15	/*  */
	LDW	12(%r18), %r16	/*  */
	LDW	8(%r18), %r17	/*  */
	LDO	128(%r0), %r31
	SUB	%r30, %r31, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	printboard,CODE
	.IMPORT	try,CODE

	.CODE
	.EXPORT	toymain,ENTRY
	.PROC
	.CALLINFO
toymain
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 24(%r18)	/*  */
	STW	%r16, 28(%r18)	/*  */
		OR	%r2, %r0, %r16

	STW	%r26, 0(%r18)	/*  */
L37
	LDO	8(%r0), %r17
	STW	%r17, 4(%r18)	/*  */
	ADDI	8, %r18, %r17
	LDW	4(%r18), %r26	/*  */
	LDO	0(%r0), %r25

	BL,n	initArray, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	STW	%r28, 0(%r17)	/*  */
	ADDI	12, %r18, %r17
	LDW	4(%r18), %r26	/*  */
	LDO	0(%r0), %r25

	BL,n	initArray, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	STW	%r28, 0(%r17)	/*  */
	ADDI	16, %r18, %r17
	LDW	4(%r18), %r20	/*  */
	LDW	4(%r18), %r21	/*  */
	ADD	%r20, %r21, %r19
	ADDIO	-1, %r19, %r26
	LDO	0(%r0), %r25

	BL,n	initArray, %r2
	NOP
	LDO	128(%r0), %r23
	SUB	%r30, %r23, %r18

	STW	%r28, 0(%r17)	/*  */
	ADDI	20, %r18, %r17
	LDW	4(%r18), %r20	/*  */
	LDW	4(%r18), %r21	/*  */
	ADD	%r20, %r21, %r19
	ADDIO	-1, %r19, %r26
	LDO	0(%r0), %r25

	BL,n	initArray, %r2
	NOP
	LDO	128(%r0), %r23
	SUB	%r30, %r23, %r18

	STW	%r28, 0(%r17)	/*  */
	OR	%r18, %r0, %r26
	LDO	0(%r0), %r25

	BL,n	try, %r2
	NOP
	LDO	128(%r0), %r25
	SUB	%r30, %r25, %r18

	B,n	L36
	NOP
L36
		OR	%r16, %r0, %r2

	LDW	28(%r18), %r16	/*  */
	LDW	24(%r18), %r17	/*  */
	LDO	128(%r0), %r28
	SUB	%r30, %r28, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	initArray,CODE
	.IMPORT	try,CODE
