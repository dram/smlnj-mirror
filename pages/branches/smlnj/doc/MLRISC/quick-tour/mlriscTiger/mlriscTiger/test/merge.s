
	.DATA

L4
	.WORD 1
	.STRING	"0"

	.DATA

L5
	.WORD 1
	.STRING	"9"

	.CODE
	.EXPORT	isdigit,ENTRY
	.PROC
	.CALLINFO
isdigit
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
	STW	%r15, 12(%r18)	/*  */
		OR	%r2, %r0, %r15

	STW	%r26, 0(%r18)	/*  */

L53
	LDO	1(%r0), %r16
	LDW	0(%r18), %r17	/*  */
	LDW	0(%r17), %r2	/*  */
	LDW	0(%r2), %r26	/*  */
	LDW	0(%r18), %r20	/*  */
	LDW	0(%r20), %r19	/*  */
	LDW	4(%r19), %r25	/*  */

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r18

		OR	%r28, %r0, %r17

	LDW	0(%r18), %r20	/*  */
	LDW	0(%r20), %r19	/*  */
	LDW	0(%r19), %r26	/*  */
	ADDIL	LR'L4-$global$, %r27, %r1
	LDO	RR'L4-$global$(%r1), %r25

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r18

	COMBT,<=,n	%r28, %r17, L6
	NOP
LL12
L7
L10
	LDO	0(%r0), %r16
L9
		OR	%r16, %r0, %r28

	B,n	L52
	NOP
L6
	LDW	0(%r18), %r24	/*  */
	LDW	0(%r24), %r23	/*  */
	LDW	0(%r23), %r26	/*  */
	LDW	0(%r18), %r28	/*  */
	LDW	0(%r28), %r25	/*  */
	LDW	4(%r25), %r25	/*  */

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r31
	SUB	%r30, %r31, %r18

		OR	%r28, %r0, %r17

	LDW	0(%r18), %r20	/*  */
	LDW	0(%r20), %r19	/*  */
	LDW	0(%r19), %r26	/*  */
	ADDIL	LR'L5-$global$, %r27, %r1
	LDO	RR'L5-$global$(%r1), %r25

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r18

	COMBT,<=,n	%r17, %r28, L9
	NOP
LL18
L54
	B,n	L10
	NOP
L52
		OR	%r15, %r0, %r2

	LDW	12(%r18), %r15	/*  */
	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r24
	SUB	%r30, %r24, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	$global$,DATA
	.IMPORT	ord,CODE

	.DATA

L12
	.WORD 1
	.STRING	" "

	.DATA

L13
	.WORD 1
	.STRING	"\n"

	.CODE
	.EXPORT	skipto,ENTRY
	.PROC
	.CALLINFO
skipto
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 8(%r18)	/*  */
		OR	%r2, %r0, %r17

	STW	%r26, 0(%r18)	/*  */
L56
L17
	LDW	0(%r18), %r21	/*  */
	LDW	0(%r21), %r20	/*  */
	LDW	4(%r20), %r26	/*  */
	ADDIL	LR'L12-$global$, %r27, %r1
	LDO	RR'L12-$global$(%r1), %r25

	BL,n	stringEqual, %r2
	NOP
	LDO	128(%r0), %r23
	SUB	%r30, %r23, %r18

	COMIBF,=,n	0, %r28, L14
	NOP
LL28
L15
	LDW	0(%r18), %r25	/*  */
	LDW	0(%r25), %r24	/*  */
	LDW	4(%r24), %r26	/*  */
	ADDIL	LR'L13-$global$, %r27, %r1
	LDO	RR'L13-$global$(%r1), %r25

	BL,n	stringEqual, %r2
	NOP
	LDO	128(%r0), %r29
	SUB	%r30, %r29, %r18

L16
	COMIBF,=,n	0, %r28, L18
	NOP
LL33
L11
	LDO	0(%r0), %r28
	B,n	L55
	NOP
L18
	LDW	0(%r18), %r1	/*  */
	LDW	0(%r1), %r31	/*  */
	ADDI	4, %r31, %r23
	STW	%r23, 4(%r18)	/*  */
	LDW	0(%r18), %r19	/*  */
	LDW	0(%r19), %r2	/*  */
	LDW	0(%r2), %r26	/*  */

	BL,n	getChar, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	LDW	4(%r18), %r23	/*  */
	STW	%r28, 0(%r23)	/*  */
	B,n	L17
	NOP
L14
	LDO	1(%r0), %r28
	B,n	L16
	NOP
L55
		OR	%r17, %r0, %r2

	LDW	8(%r18), %r17	/*  */
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	getChar,CODE
	.IMPORT	$global$,DATA
	.IMPORT	stringEqual,CODE

	.DATA

L20
	.WORD 1
	.STRING	"0"

	.CODE
	.EXPORT	readint,ENTRY
	.PROC
	.CALLINFO
readint
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
	STW	%r15, 12(%r18)	/*  */
		OR	%r2, %r0, %r16

	STW	%r26, 0(%r18)	/*  */
		OR	%r25, %r0, %r17

L58
	LDO	0(%r0), %r15
	OR	%r18, %r0, %r26

	BL,n	skipto, %r2
	NOP
	LDO	128(%r0), %r19
	SUB	%r30, %r19, %r18
	ADDI	0, %r17, %r17
	OR	%r18, %r0, %r26
	LDW	0(%r18), %r19	/*  */
	LDW	4(%r19), %r25	/*  */

	BL,n	isdigit, %r2
	NOP
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r18

	STW	%r28, 0(%r17)	/*  */
L21
	OR	%r18, %r0, %r26
	LDW	0(%r18), %r22	/*  */
	LDW	4(%r22), %r25	/*  */

	BL,n	isdigit, %r2
	NOP
	LDO	128(%r0), %r24
	SUB	%r30, %r24, %r18

	COMIBF,=,n	0, %r28, L22
	NOP
LL44
L19
		OR	%r15, %r0, %r28

	B,n	L57
	NOP
L22
	LDO	10(%r0), %r25
		OR	%r15, %r0, %r26

	BL,n	$$muloI, %r31
	NOP
		OR	%r29, %r0, %r15

	LDW	0(%r18), %r29	/*  */
	LDW	0(%r29), %r26	/*  */
	LDW	0(%r18), %r17	/*  */
	LDW	4(%r17), %r25	/*  */

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	ADD	%r15, %r28, %r15
	LDW	0(%r18), %r21	/*  */
	LDW	0(%r21), %r26	/*  */
	ADDIL	LR'L20-$global$, %r27, %r1
	LDO	RR'L20-$global$(%r1), %r25

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r19
	SUB	%r30, %r19, %r18

	SUB	%r15, %r28, %r15
	LDW	0(%r18), %r20	/*  */
	ADDI	4, %r20, %r17
	LDW	0(%r18), %r19	/*  */
	LDW	0(%r19), %r26	/*  */

	BL,n	getChar, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	STW	%r28, 0(%r17)	/*  */
	B,n	L21
	NOP
L57
		OR	%r16, %r0, %r2

	LDW	12(%r18), %r15	/*  */
	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	$$muloI,MILLICODE
	.IMPORT	$global$,DATA
	.IMPORT	skipto,CODE
	.IMPORT	isdigit,CODE
	.IMPORT	ord,CODE
	.IMPORT	getChar,CODE

	.CODE
	.EXPORT	readlist,ENTRY
	.PROC
	.CALLINFO
readlist
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
	STW	%r15, 12(%r18)	/*  */
		OR	%r2, %r0, %r17

	STW	%r26, 0(%r18)	/*  */
L60
	LDO	4(%r0), %r26

	BL,n	allocRecord, %r2
	NOP
	LDO	128(%r0), %r16
	SUB	%r30, %r16, %r18

	LDO	0(%r0), %r19
	STW	%r19, 0(%r28)	/*  */
		OR	%r28, %r0, %r15

	LDW	0(%r18), %r26	/*  */
		OR	%r15, %r0, %r25


	BL,n	readint, %r2
	NOP
	LDO	128(%r0), %r16
	SUB	%r30, %r16, %r18
		OR	%r28, %r0, %r16

	LDW	0(%r15), %r19	/*  */
	COMIBF,=,n	0, %r19, L23
	NOP
LL55
L24
	LDO	0(%r0), %r28
L25

	B,n	L59
	NOP
L23
	LDO	8(%r0), %r26

	BL,n	allocRecord, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18
		OR	%r28, %r0, %r15

	STW	%r16, 0(%r15)	/*  */
	ADDI	4, %r15, %r16
	LDW	0(%r18), %r26	/*  */

	BL,n	readlist, %r2
	NOP
	LDO	128(%r0), %r19
	SUB	%r30, %r19, %r18

	STW	%r28, 0(%r16)	/*  */
		OR	%r15, %r0, %r28

	B,n	L25
	NOP
L59
		OR	%r17, %r0, %r2

	LDW	12(%r18), %r15	/*  */
	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	allocRecord,CODE
	.IMPORT	readint,CODE
	.IMPORT	readlist,CODE

	.CODE
	.EXPORT	merge,ENTRY
	.PROC
	.CALLINFO
merge
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
	STW	%r15, 12(%r18)	/*  */
	STW	%r14, 16(%r18)	/*  */
	STW	%r13, 20(%r18)	/*  */
		OR	%r2, %r0, %r14

	STW	%r26, 0(%r18)	/*  */
		OR	%r24, %r0, %r15
		OR	%r25, %r0, %r13

L62
	COMIBT,=,n	0, %r13, L32
	NOP
LL63
L33
	COMIBT,=,n	0, %r15, L29
	NOP
LL66
L30
	LDW	0(%r13), %r31	/*  */
	LDW	0(%r15), %r16	/*  */
	COMBT,<,n	%r31, %r16, L26
	NOP
LL69
L27
	LDO	8(%r0), %r26

	BL,n	allocRecord, %r2
	NOP
	LDO	128(%r0), %r17
	SUB	%r30, %r17, %r18
		OR	%r28, %r0, %r16

	LDW	0(%r15), %r19	/*  */
	STW	%r19, 0(%r16)	/*  */
	ADDI	4, %r16, %r17
	LDW	0(%r18), %r26	/*  */
		OR	%r13, %r0, %r25

	LDW	4(%r15), %r24	/*  */

	BL,n	merge, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	STW	%r28, 0(%r17)	/*  */
		OR	%r16, %r0, %r28

L28

L31

L34

	B,n	L61
	NOP
L32
		OR	%r15, %r0, %r28

	B,n	L34
	NOP
L29
		OR	%r13, %r0, %r28

	B,n	L31
	NOP
L26
	LDO	8(%r0), %r26

	BL,n	allocRecord, %r2
	NOP
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r18
		OR	%r28, %r0, %r16

	LDW	0(%r13), %r22	/*  */
	STW	%r22, 0(%r16)	/*  */
	ADDI	4, %r16, %r17
	LDW	0(%r18), %r26	/*  */
	LDW	4(%r13), %r25	/*  */
		OR	%r15, %r0, %r24


	BL,n	merge, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	STW	%r28, 0(%r17)	/*  */
		OR	%r16, %r0, %r28

	B,n	L28
	NOP
L61
		OR	%r14, %r0, %r2

	LDW	20(%r18), %r13	/*  */
	LDW	16(%r18), %r14	/*  */
	LDW	12(%r18), %r15	/*  */
	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	allocRecord,CODE
	.IMPORT	merge,CODE

	.DATA

L35
	.WORD 1
	.STRING	"0"

	.CODE
	.EXPORT	f,ENTRY
	.PROC
	.CALLINFO
f
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
		OR	%r2, %r0, %r16

	STW	%r26, 0(%r18)	/*  */
		OR	%r25, %r0, %r17

L64
	COMIBT,<,n	0, %r17, L36
	NOP
LL81
L37
L38
	LDO	0(%r0), %r28
	B,n	L63
	NOP
L36
	LDW	0(%r18), %r28	/*  */
	LDO	10(%r0), %r25
		OR	%r17, %r0, %r26

	BL,n	$$divI, %r31
	NOP

		OR	%r28, %r0, %r26
		OR	%r29, %r0, %r25

	BL,n	f, %r2
	NOP
	LDO	128(%r0), %r1
	SUB	%r30, %r1, %r18
	LDW	0(%r18), %r20	/*  */
	LDW	0(%r20), %r19	/*  */
	LDW	0(%r19), %r2	/*  */
	STW	%r2, 16(%r18)	/*  */
	LDW	0(%r18), %r23	/*  */
	LDW	0(%r23), %r22	/*  */
	LDW	0(%r22), %r21	/*  */
	STW	%r21, 12(%r18)	/*  */
	LDO	10(%r0), %r25
		OR	%r17, %r0, %r26

	BL,n	$$divI, %r31
	NOP

	LDO	10(%r0), %r25
		OR	%r29, %r0, %r26

	BL,n	$$muloI, %r31
	NOP

	SUB	%r17, %r29, %r17
	LDW	0(%r18), %r20	/*  */
	LDW	0(%r20), %r19	/*  */
	LDW	0(%r19), %r26	/*  */
	ADDIL	LR'L35-$global$, %r27, %r1
	LDO	RR'L35-$global$(%r1), %r25

	BL,n	ord, %r2
	NOP
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r18

	LDW	12(%r18), %r26	/*  */
	ADD	%r17, %r28, %r25

	BL,n	chr, %r2
	NOP
	LDO	128(%r0), %r24
	SUB	%r30, %r24, %r18

	LDW	16(%r18), %r26	/*  */

		OR	%r28, %r0, %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r29
	SUB	%r30, %r29, %r18
	B,n	L38
	NOP
L63
		OR	%r16, %r0, %r2

	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r1
	SUB	%r30, %r1, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	$$divI,MILLICODE
	.IMPORT	$$muloI,MILLICODE
	.IMPORT	$global$,DATA
	.IMPORT	f,CODE
	.IMPORT	ord,CODE
	.IMPORT	chr,CODE
	.IMPORT	print,CODE

	.DATA

L39
	.WORD 1
	.STRING	"-"

	.DATA

L40
	.WORD 1
	.STRING	"0"

	.CODE
	.EXPORT	printint,ENTRY
	.PROC
	.CALLINFO
printint
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
		OR	%r2, %r0, %r16

	STW	%r26, 0(%r18)	/*  */
		OR	%r25, %r0, %r17

L66
	COMIBF,<=,n	0, %r17, L44
	NOP
LL95
L45
	COMIBT,<,n	0, %r17, L41
	NOP
LL98
L42
	LDW	0(%r18), %r17	/*  */
	LDW	0(%r17), %r26	/*  */
	ADDIL	LR'L40-$global$, %r27, %r1
	LDO	RR'L40-$global$(%r1), %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

L43

L46

	B,n	L65
	NOP
L44
	LDW	0(%r18), %r19	/*  */
	LDW	0(%r19), %r26	/*  */
	ADDIL	LR'L39-$global$, %r27, %r1
	LDO	RR'L39-$global$(%r1), %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r21
	SUB	%r30, %r21, %r18
	OR	%r18, %r0, %r26
	SUBI	0, %r17, %r25

	BL,n	f, %r2
	NOP
	LDO	128(%r0), %r23
	SUB	%r30, %r23, %r18

	B,n	L46
	NOP
L41
	OR	%r18, %r0, %r26
		OR	%r17, %r0, %r25


	BL,n	f, %r2
	NOP
	LDO	128(%r0), %r25
	SUB	%r30, %r25, %r18

	B,n	L43
	NOP
L65
		OR	%r16, %r0, %r2

	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r28
	SUB	%r30, %r28, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	print,CODE
	.IMPORT	$global$,DATA
	.IMPORT	f,CODE

	.DATA

L47
	.WORD 1
	.STRING	"\n"

	.DATA

L48
	.WORD 1
	.STRING	" "

	.CODE
	.EXPORT	printlist,ENTRY
	.PROC
	.CALLINFO
printlist
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 4(%r18)	/*  */
	STW	%r16, 8(%r18)	/*  */
		OR	%r2, %r0, %r16

	STW	%r26, 0(%r18)	/*  */
		OR	%r25, %r0, %r17

L68
	COMIBT,=,n	0, %r17, L49
	NOP
LL112
L50
	LDW	0(%r18), %r26	/*  */
	LDW	0(%r17), %r25	/*  */

	BL,n	printint, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18
	LDW	0(%r18), %r21	/*  */
	LDW	0(%r21), %r26	/*  */
	ADDIL	LR'L48-$global$, %r27, %r1
	LDO	RR'L48-$global$(%r1), %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r23
	SUB	%r30, %r23, %r18
	LDW	0(%r18), %r26	/*  */
	LDW	4(%r17), %r25	/*  */

	BL,n	printlist, %r2
	NOP
	LDO	128(%r0), %r25
	SUB	%r30, %r25, %r18

L51

	B,n	L67
	NOP
L49
	LDW	0(%r18), %r26	/*  */
	LDW	0(%r26), %r26	/*  */
	ADDIL	LR'L47-$global$, %r27, %r1
	LDO	RR'L47-$global$(%r1), %r25

	BL,n	print, %r2
	NOP
	LDO	128(%r0), %r29
	SUB	%r30, %r29, %r18

	B,n	L51
	NOP
L67
		OR	%r16, %r0, %r2

	LDW	8(%r18), %r16	/*  */
	LDW	4(%r18), %r17	/*  */
	LDO	128(%r0), %r1
	SUB	%r30, %r1, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	$global$,DATA
	.IMPORT	printint,CODE
	.IMPORT	print,CODE
	.IMPORT	printlist,CODE

	.CODE
	.EXPORT	toymain,ENTRY
	.PROC
	.CALLINFO
toymain
	OR	%r30, %r0, %r18
	ADDI	128, %r30, %r30
	STW	%r17, 8(%r18)	/*  */
	STW	%r16, 12(%r18)	/*  */
		OR	%r2, %r0, %r16

	STW	%r26, 0(%r18)	/*  */
L70
	ADDI	4, %r18, %r17
	LDW	0(%r18), %r26	/*  */

	BL,n	getChar, %r2
	NOP
	LDO	128(%r0), %r19
	SUB	%r30, %r19, %r18

	STW	%r28, 0(%r17)	/*  */
	OR	%r18, %r0, %r26

	BL,n	readlist, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18
	STW	%r28, 16(%r18)	/*  */
	ADDI	4, %r18, %r17
	LDW	0(%r18), %r26	/*  */

	BL,n	getChar, %r2
	NOP
	LDO	128(%r0), %r19
	SUB	%r30, %r19, %r18

	STW	%r28, 0(%r17)	/*  */
	OR	%r18, %r0, %r26

	BL,n	readlist, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

	OR	%r18, %r0, %r17
	OR	%r18, %r0, %r26
	LDW	16(%r18), %r25	/*  */

		OR	%r28, %r0, %r24

	BL,n	merge, %r2
	NOP
	LDO	128(%r0), %r20
	SUB	%r30, %r20, %r18

		OR	%r17, %r0, %r26


		OR	%r28, %r0, %r25

	BL,n	printlist, %r2
	NOP
	LDO	128(%r0), %r22
	SUB	%r30, %r22, %r18

	B,n	L69
	NOP
L69
		OR	%r16, %r0, %r2

	LDW	12(%r18), %r16	/*  */
	LDW	8(%r18), %r17	/*  */
	LDO	128(%r0), %r24
	SUB	%r30, %r24, %r30
	BV,n	%r0(%r2)
	NOP
	.PROCEND
	.IMPORT	getChar,CODE
	.IMPORT	readlist,CODE
	.IMPORT	merge,CODE
	.IMPORT	printlist,CODE
