/*! \file x86-syntax.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This file abstracts over the syntax of x86 (and x86-64) assembly
 * instructions.  It supports two syntactic conventions:
 *
 *    1. "GNU Assember syntax", where arguments are `src,dst`.  This
 *      syntax is used by BSD, Linux, macOS, etc.
 *
 *    2. "Microsoft Assember syntax", where arguments are `dst,src`.
 *      This syntax is used in the Intel reference manuals.
 *
 * 32-bit Registers
 *	EAX		X86 only
 *	EBX		X86 only
 *	ECX		X86 only
 *	EDX		X86 only
 *	EBP		X86 only
 *	ESI		X86 only
 *	EDI		X86 only
 *	ESP		X86 only
 *
 * 64-bit Registers
 *	RDI		AMD64 only
 *	RSI		AMD64 only
 *	RSP		AMD64 only
 *	RAX		AMD64 only
 *	RBX		AMD64 only
 *	RCX		AMD64 only
 *	RDX		AMD64 only
 *	RBP		AMD64 only
 *	R8		AMD64 only
 *	R9		AMD64 only
 *	R10		AMD64 only
 *	R11		AMD64 only
 *	R12		AMD64 only
 *	R13		AMD64 only
 *	R14		AMD64 only
 *	R15		AMD64 only
 *
 * 128-bit SSE Registers
 *	XMM0		AMD64 only
 *	XMM1		AMD64 only
 *	XMM2		AMD64 only
 *	XMM3		AMD64 only
 *	XMM4		AMD64 only
 *	XMM5		AMD64 only
 *	XMM6		AMD64 only
 *	XMM7		AMD64 only
 *	XMM8		AMD64 only
 *	XMM9		AMD64 only
 *	XMM10		AMD64 only
 *	XMM11		AMD64 only
 *	XMM12		AMD64 only
 *	XMM13		AMD64 only
 *	XMM14		AMD64 only
 *	XMM15		AMD64 only
 *
 * Operands
 *	IM(x)		immediate value
 *	REGIND(r)	register indirect
 *	REGOFF(d,r)	register indirect with offset
 *	CODEPTR(r)	register contents as jump target
 *
 * Instructions; operand  are ordered src,dst and are the native machine size
 *	ADD(src,dst)		`dst := dst + src`
 *	AND(src,dst)		`dst := dst AND src`
 *	CALL(lab)		procedure call
 *	CMP(src,dst)		test of `dst - src`
 *	FILD(src)		load 32-bit integer into ST(0) (X86 only)
 *	FINIT			initialize x87 FPU (X86 only)
 *	FISTP			store floating-point value as integer (X86 only)
 *	FLD_D(dst)		load 64-bit floating-point value into ST(0) (X86 only)
 *	FLDCW			load x87 FPU control word (X86 only)
 *	FSCALE			scale ST(0) by ST(1) (X86 only)
 *	FSTCW			store x87 FPU control word (X86 only)
 *	FSTP_D(dst)		store ST(0) as 64-bit float (X86 only)
 *	INC(dst)		`dst := dst + 1`
 *	LEA(src,dst)		load effective address
 *	JB(lab)			jump if below
 *	JGE(lab)		jump if greater or equal (dst >= src)
 *	JMP(lab)		jump
 *	JNE(lab)		jump if not equal
 *	MOV(src,dst)		move from src to dst
 *	MOVS_D(src,dst)		move scalar 64-bit floating-point value (AMD64 only)
 *	OR(src,dst)		`dst := src OR dst`
 *	POP(dst)		pop value off stack into `dst`
 *	PUSH(src)		push `src` onto stack
 *	RET			return
 *	SAL(src,dst)		`dst := dst << src`
 *	SAR(src,dst)		`dst := dst ~>> src`
 *	SUB(src,dst)		`dst := dst - src`
 */

#ifndef _X86_SYNTAX_H_
#define _X86_SYNTAX_H_

#if !defined(GNU_ASSEMBLER) && !defined(MASM_ASSEMBLER)
#  error must specify either GNU_ASSEMBLER or MASM_ASSEMBLER
#endif

#if defined(__STDC__)
#define CONCAT(x, y)	x ## y
#else
#define CONCAT(x, y)	x/**/y
#endif

#ifdef GNU_ASSEMBLER

/* arguments are in src,dst order for the GNU assembler */
#define ARGS2(src,dst)		src,dst

#define CHOICE(gnu, masm)	gnu

/* operands */
#define IM(x)		CONCAT($,a)
#define REG(r)		CONCAT(%,r)
#define REGIND(r)	(r)
#define REGOFF(d,r)	d(r)
#define CODEPTR(r)	*r

#else /* MASM_ASSEMBLER */

/* arguments are in dst,src order for the MASM assembler */
#define ARGS2(src,dst)		dst,src

#define CHOICE(gnu, masm)	masm

/* operands */
#define IM(x)		x
#define REG(r)		r
#ifdef ARCH_X86
#define REGIND(r)	dword ptr [r]
#define REGOFF(d,r)	dword ptr [r + d]
#else /* ARCH_AMD64 */
#define REGIND(r)	qword ptr [r]
#define REGOFF(d,r)	qword ptr [r + d]
#endf
#define CODEPTR(r)	via r

#endif /* GNU_ASSEMBLER */

#define CALL(lab)		call lab
#define JB(lab)			jb lab
#define JGE(lab)		jge lab
#define JMP(lab)		jmp lab
#define JNE(lab)		jne lab
#define RET			ret

#ifdef ARCH_X86
/* 32-bit sized operations */
#define ADD(src,dst)		CHOICE(addl ARGS2(src,dst), add ARGS2(src,dst))
#define AND(src,dst)		CHOICE(andl ARGS2(src,dst), and ARGS2(src,dst))
#define CMP(src,dst)		CHOICE(cmpl ARGS2(src,dst), cmp ARGS2(src,dst))
#define INC(dst)		CHOICE(incl dst, inc dst)
#define LEA(src,dst)		CHOICE(leal ARGS2(src,dst), lea ARGS2(src,dst))
#define MOV(src,dst)		CHOICE(movl ARGS2(src,dst), mov ARGS2(src,dst))
#define OR(src,dst)		CHOICE(orl ARGS2(src,dst), or ARGS2(src,dst))
#define POP(dst)		CHOICE(popl dst, pop dst)
#define PUSH(src)		CHOICE(pushl src, push src)
#define SAL(src,dst)		CHOICE(sall ARGS2(src,dst), sal ARGS2(src,dst))
#define SAR(src,dst)		CHOICE(sarl ARGS2(src,dst), sar ARGS2(src,dst))
#define SUB(src,dst)		CHOICE(subl ARGS2(src,dst), sub ARGS2(src,dst))
/* x87 floating-point instructions */
#define FILD_L(src)		CHOICE(fildl src, fild src)
#define FINIT			finit
#define FISTP_L			CHOICE(fistpl, fistp)
#define FLD_D(dst)		CHOICE(fldd dst, fld dst)
#define FLDCW			fldcw
#define FSCALE			fscale
#define FSTCW			fstcw
#define FSTP_D(dst)		CHOICE(fstpd dst, fstp dst)
/* 32-bit registers */
#define EAX		REG(eax)
#define EBX		REG(ebx)
#define ECX		REG(ecx)
#define EDX		REG(edx)
#define EBP		REG(ebp)
#define ESI		REG(esi)
#define EDI		REG(edi)
#define ESP		REG(esp)
#endif /* ARCH_X86 */

#ifdef ARCH_AMD64
/* 64-bit sized operations */
#define ADD(src,dst)		CHOICE(addq ARGS2(src,dst), add ARGS2(src,dst))
#define AND(src,dst)		CHOICE(andq ARGS2(src,dst), and ARGS2(src,dst))
#define CMP(src,dst)		CHOICE(cmpl ARGS2(src,dst), cmp ARGS2(src,dst))
#define INC(dst)		CHOICE(incq dst, inc dst)
#define LEA(src,dst)		CHOICE(leaq ARGS2(src,dst), lea ARGS2(src,dst))
#define MOV(src,dst)		CHOICE(movq ARGS2(src,dst), mov ARGS2(src,dst))
#define MOVS_D(src,dst)		CHOICE(movsd ARGS2(src,dst), movs ARGS2(src,dst))
#define MULS_D(src,dst)		CHOICE(mulsd ARGS2(src,dst), mulsd ARGS2(src,dst))
#define OR(src,dst)		CHOICE(orq ARGS2(src,dst), or ARGS2(src,dst))
#define POP(dst)		CHOICE(popq dst, pop dst)
#define PUSH(src)		CHOICE(pushq src, push src)
#define SAL(src,dst)		CHOICE(salq ARGS2(src,dst), sal ARGS2(src,dst))
#define SAR(src,dst)		CHOICE(sarq ARGS2(src,dst), sar ARGS2(src,dst))
#define SUB(src,dst)		CHOICE(subq ARGS2(src,dst), sub ARGS2(src,dst))
/* 64-bit registers */
#define RDI		REG(rdi)
#define RSI		REG(rsi)
#define RSP		REG(rsp)
#define RAX		REG(rax)
#define RBX		REG(rbx)
#define RCX		REG(rcx)
#define RDX		REG(rdx)
#define RBP		REG(rbp)
#define R8		REG(r8)
#define R9		REG(r9)
#define R10		REG(r10)
#define R11		REG(r11)
#define R12		REG(r12)
#define R13		REG(r13)
#define R14		REG(r14)
#define R15		REG(r15)
/* 128-bit SSE Registers */
#define XMM0		REG(xmm0)
#define XMM1		REG(xmm1)
#define XMM2		REG(xmm2)
#define XMM3		REG(xmm3)
#define XMM4		REG(xmm4)
#define XMM5		REG(xmm5)
#define XMM6		REG(xmm6)
#define XMM7		REG(xmm7)
#define XMM8		REG(xmm8)
#define XMM9		REG(xmm9)
#define XMM10		REG(xmm10)
#define XMM11		REG(xmm11)
#define XMM12		REG(xmm12)
#define XMM13		REG(xmm13)
#define XMM14		REG(xmm14)
#define XMM15		REG(xmm15)
#endif /* ARCH_AMD64 */

/* MOVE(src,tmp,dst) copies one memory location `src` to `dst``, using register `tmp`. */
#ifdef GNU_ASSEMBLER
#define MOVE(src,tmp,dst)	\
	MOV(src, tmp);	\
	MOV(tmp, dst)
#else /* MASM_ASSEMBLER */
MOVE_M MACRO src,tmp,dst
	MOV	(src, tmp)
	MOV	(tmp, dst)
ENDM
#define MOVE(a,b,c) MOVE_M a, b, c
#endif

#endif /* _X86_SYNTAX_H_ */
