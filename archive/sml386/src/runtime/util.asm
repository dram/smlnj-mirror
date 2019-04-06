;*************************************************************************
; gc.asm
;
; Copyright (c) 1991 by:     Department of Computerscience
;			     The Technical University of Denmark
;			     DK-2800 Lyngby
;
; 19 Dec. 1991	    Yngvi Skaalum Guttesen
;
; This file contains utility functions to handle the mixed USE16/USE32
; memory model. The functions are equivalent to the macros in ML_TYPES.H
; in the UNIX version.
;

    TITLE   Utility functions

    PAGE    90,132

    .386p
    .387

    .XLIST

include tags.inc

DWP     EQU <dword ptr>
tmp     EQU <eax>

    .LIST

_RUNCODE segment para use32 public 'CODE'

    EXTRN   _MLState_a        : DWORD

_RUNCODE ends

_DATA   segment word public use16 'DATA'

    EXTRN   _wsUse32Data    : WORD
    EXTRN   _wsUse32Code    : WORD
    EXTRN   _runtime_seg    : WORD

_DATA   ends

_TEXT   segment word public use16 'CODE'

    assume cs:_TEXT, ds:_DATA, es:_DATA, gs:_DATA, fs:_DATA, ss:_DATA


public _REC_SEL

; #define REC_SEL(p,i)

_REC_SEL    PROC NEAR


param1  EQU <[bp+4]>
param2  EQU <[bp+8]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    assume  gs:_RUNCODE

    mov     eax, param2
    mov     ebx, param1
    lea     eax, [ebx+eax*4]

    mov     eax, gs:[eax]

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_REC_SEL ENDP

public _REC_ALLOC1

_REC_ALLOC1 PROC NEAR

    assume cs:_TEXT, ds:_DATA, es:_DATA, gs:_DATA, fs:_DATA, ss:_DATA


param1  EQU <[bp+4]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    assume  gs:_RUNCODE

    mov     ebx, OFFSET _MLState_a
    mov     eax, gs:[ebx]
    add     DWP gs:[ebx], 8


    mov     DWORD PTR gs:[eax], 1*power_tags + tag_record

    mov     ebx, param1
    mov     gs:[eax+4], ebx

    add     eax, 4

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_REC_ALLOC1 ENDP

public _REC_ALLOC2

_REC_ALLOC2 PROC NEAR

    assume  gs:_RUNCODE

param1  EQU <[bp+4]>
param2  EQU <[bp+8]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    mov     ebx, OFFSET _MLState_a
    mov     eax, gs:[ebx]
    add     DWP gs:[ebx], 12


    mov     DWORD PTR gs:[eax], 1*power_tags + tag_record

    mov     ebx, param1
    mov     gs:[eax+4], ebx

    mov     ebx, param2
    mov     gs:[eax+8], ebx

    add     eax, 4

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_REC_ALLOC2 ENDP

public _REC_ALLOC3

_REC_ALLOC3 PROC NEAR

    assume  gs:_RUNCODE

param1  EQU <[bp+4]>
param2  EQU <[bp+8]>
param3  EQU <[bp+12]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    mov     ebx, OFFSET _MLState_a
    mov     eax, gs:[ebx]
    add     DWP gs:[ebx], 16


    mov     DWORD PTR gs:[eax], 1*power_tags + tag_record

    mov     ebx, param1
    mov     gs:[eax+4], ebx

    mov     ebx, param2
    mov     gs:[eax+8], ebx

    mov     ebx, param3
    mov     gs:[eax+12], ebx

    add     eax, 4

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_REC_ALLOC3 ENDP

public _REC_ALLOC4

_REC_ALLOC4 PROC NEAR

    assume  gs:_RUNCODE

param1  EQU <[bp+4]>
param2  EQU <[bp+8]>
param3  EQU <[bp+12]>
param4  EQU <[bp+16]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    mov     ebx, OFFSET _MLState_a
    mov     eax, gs:[ebx]
    add     DWP gs:[ebx], 20


    mov     DWORD PTR gs:[eax], 1*power_tags + tag_record

    mov     ebx, param1
    mov     gs:[eax+4], ebx

    mov     ebx, param2
    mov     gs:[eax+8], ebx

    mov     ebx, param3
    mov     gs:[eax+12], ebx

    mov     ebx, param4
    mov     gs:[eax+16], ebx

    add     eax, 4

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_REC_ALLOC4 ENDP


public _REC_ALLOC6

_REC_ALLOC6 PROC NEAR

    assume  gs:_RUNCODE

param1  EQU <[bp+4]>
param2  EQU <[bp+8]>
param3  EQU <[bp+12]>
param4  EQU <[bp+16]>
param5  EQU <[bp+20]>
param6  EQU <[bp+24]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    mov     ebx, OFFSET _MLState_a
    mov     eax, gs:[ebx]
    add     DWP gs:[ebx], 28


    mov     DWORD PTR gs:[eax], 1*power_tags + tag_record

    mov     ebx, param1
    mov     gs:[eax+4], ebx

    mov     ebx, param2
    mov     gs:[eax+8], ebx

    mov     ebx, param3
    mov     gs:[eax+12], ebx

    mov     ebx, param4
    mov     gs:[eax+16], ebx

    mov     ebx, param5
    mov     gs:[eax+20], ebx

    mov     ebx, param6
    mov     gs:[eax+24], ebx

    add     eax, 4

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_REC_ALLOC6 ENDP

public _ML_alloc_write


_ML_alloc_write PROC NEAR

    assume  gs:_RUNCODE

param1  EQU <[bp+4]>
param2  EQU <[bp+8]>

    push    bp
    mov     bp, sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    mov     ebx, OFFSET _MLState_a
    mov     ebx, gs:[ebx]
    mov     eax, param1
    lea     ebx, [ebx+4*eax]

    mov     eax, param2
    mov     gs:[ebx], eax

    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_ML_alloc_write ENDP


public _ML_alloc

_ML_alloc   PROC NEAR

    assume  gs:_RUNCODE

param1  EQU <[bp+4]>

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     gs, ax

    mov     ecx, OFFSET _MLState_a
    mov     eax, gs:[ecx]

    mov     ebx, param1
    inc     ebx
    sal     ebx, 2
    add     gs:[ecx], ebx

    add     eax, 4
    mov     edx, eax
    sar     edx, 16

    pop     bp
    ret

_ML_alloc   ENDP

public _stringcompare

_stringcompare   PROC NEAR

param1  EQU <[bp+4]>
param2  EQU <[bp+8]>
param3  EQU <[bp+12]>

    assume  es:_RUNCODE

    push    bp
    mov     bp,sp

    mov     ax, _wsUse32Data
    mov     es, ax

    mov     ebx, param1
    mov     edx, param2
    mov     ecx, param3

Loopstringcompare:

    jecxz   short Lstringcompare_eq
    mov     al, es:[ebx-1*ecx-1]
    cmp     es:[edx-1*ecx-1], al
    jne     short Lstringcompare_neq
    dec     ecx
    jmp     short Loopstringcompare

Lstringcompare_eq:

    mov     ax,1
    cwd

    pop     bp
    ret

Lstringcompare_neq:
    sub     ax,ax
    cwd

    pop     bp
    ret

_stringcompare ENDP

_TEXT ends

end
