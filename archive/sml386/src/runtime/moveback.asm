;*************************************************************************
; moveback.asm
;
; Copyright (c) 1991 by:     Department of Computer Science
;                            The Technical University of Denmark
;                            DK-2800 Lyngby
;
; 10/4/91   Yngvi Skaalum Guttesen
;
; This file contains the moveback function that adjusts the ML-heap
; after a G.C.
;

    TITLE   MoveBack

    PAGE    90,132

    .386p
    .387

    .XLIST

include tags.inc

DWP     EQU <dword ptr>
tmp     EQU <eax>

    .LIST

_DATA   segment word public use16 'DATA'

    EXTRN   _wsUse32Data    : WORD

_DATA   ends

_TEXT   segment word public use16 'CODE'

    assume cs:_TEXT, ds:_DATA, es:_DATA, gs:_DATA, fs:nothing, ss:_DATA

;********************************************************************
; Blockmove moves the "words" double words from "from" to "to".
;
; blockmove(int *from, int *to, int words)
;     if (!words) return ;
;     if (from<to && from+words>to)
;        {from += words ; to += words ;
;         do { *--to = *--from } while (--words>0) ;
;        }
;     else
;        do { *to++ = *from++ } while (--words>0) ;
;
;
; Frame on entry:   [sp+10] -> words
;                   [sp+06] -> to
;                   [sp+02] -> from
;                   [sp+00] -> return address
;

words   EQU <ecx>
from    EQU <esi>
to      EQU <edi>

param3  EQU <[bp+12]>
param2  EQU <[bp+08]>
param1  EQU <[bp+04]>


blockmove PROC NEAR

    push    bp
    mov     bp,sp

    push    words

    mov     words,param3
    or      words,words
    jz      SHORT Lblockmove_return       ; if (!words) return

    push    from
    push    to

    cld                     ; count up

    mov     from,param1
    mov     to,param2
    cmp     from,to
    jae     Lblockmove

    mov     tmp,words
    shl     tmp,2
    add     tmp,from        ; tmp = from + words

    cmp     tmp,to
    jbe     Lblockmove

    mov     tmp,words
    dec     tmp
    shl     tmp,2
    add     from,tmp
    add     from,tmp
    std                     ; count down

Lblockmove:

    db  67h
    rep movsd

    pop     to
    pop     from

Lblockmove_return:

    pop     words
    pop     bp
    ret

blockmove endp

;*************************************************************************
;
;     moveback(from_low, from_high, to_low, misc_roots)
;     long *from_low;
;     long *from_high;
;     long *to_low;
;     long **misc_roots;
;
;      #define INRANGE(x) (((int)(x) >= (int)from_low) && \
;			   ((int)(x) <	(int)from_high) )
;      #define ADJUST1(x) (INRANGE(x) ? (x) += offset : 0 )
;      #define ADJUST(x)  (is_ptr(x) ? ADJUST1(x) : 0)
;
;     {
;	  int *x, offset = sizeof(int)*(to_low-from_low);
;
;	  { int **p;
;	    for (p=misc_roots; *p; p++)
;		ADJUST(**p);
;	  }
;
;	  x=from_low;
;	  while (x<from_high)
;	      if (contains_no_ptrs(*x))
;		  x+= (get_len(x)+7)>>2;
;	      else {int i= get_lenz(x);
;		    ++x;
;		    do {ADJUST(*x); x++;} while (--i>0);
;		   }
;	  blockmove(from_low,to_low,from_high - from_low);
;     }
;
;
;Frame on entry:    sp + 00 -> return address
;                   sp + 02 -> from_low
;                   sp + 06 -> from_high
;                   sp + 10 -> to_low
;                   sp + 14 -> misc_roots
;
PUBLIC _moveback

_moveback    proc    NEAR

    push    bp
    mov     bp,sp

    push    si
    push    di
    push    ds

    mov     ax, _wsUse32Data
    mov     ds, ax
    mov     es, ax

flow    EQU <DWORD PTR [bp +  4]>
fhigh   EQU <DWORD PTR [bp +  8]>
tlow    EQU <DWORD PTR [bp + 12]>
misc    EQU <DWORD PTR [bp + 16]>

x       EQU <ebx>   ; int *x
offs    EQU <edi>   ; int offset
p       EQU <esi>   ; int **p
i       EQU <ecx>   ; int i
tmp2    EQU <edx>

ADJUST     macro   x
    local   Skip

    is_ptr(x)
    jnz     SHORT Skip

    cmp     x,flow
    jnae    SHORT Skip
    cmp     x,fhigh
    jnb     SHORT Skip

    add     x,offs

Skip:
    endm

    mov     offs,tlow
    sub     offs,flow           ; offset = sizeof(int)*(to_low-from_low)

;   Do misc. roots
;   for (p=misc_roots ; *p ; p++) ADJUST(**p)

    mov     p,misc              ; p = misc_roots

Lmb_loop1_test:

    mov     tmp2,[p]
    cmp     tmp2,0              ; *p <> NULL
    je      SHORT Lmb_loop1_done

    mov     tmp,[tmp2]
    ADJUST  tmp
    mov     [tmp2],tmp          ; ADJUST(**p)

    add     p,4                 ; p++

    jmp     Lmb_loop1_test

Lmb_loop1_done:

;   Finish the new space

    mov     x,flow              ; x = from_low

Lmb_loop2_test:

    cmp     x,fhigh
    jnb     SHORT Lmb_loop2_end ; while (x<from_high) {

    mov     tmp,[x]
    contains_no_ptrs tmp
    jz      SHORT Lmb1          ;   if (contains_no_ptrs)
    get_len tmp,x
    add     tmp,7
    and     tmp,NOT 3
    add     x,tmp               ;       x += (get_len(x)+7) >> 2
    jmp     Lmb_loop2_test

Lmb1:                           ;   else {

    get_lenz i,x                ;       i = get_lenz(x)
    add     x,4                 ;       ++x

Lmb_loop3:                      ;       do {

    mov     tmp,[x]
    ADJUST  tmp
    mov     [x],tmp             ;           ADJUST(*x)

    add     x,4                 ;           x++

    loop    Lmb_loop3           ;       while (--i>0)
                                ;   }
    jmp     Lmb_loop2_test      ; }

Lmb_loop2_end:

    mov     tmp,fhigh
    sub     tmp,flow
    sar     tmp,2
    push    tmp                 ; push from_high-from_low
    push    tlow
    push    flow
    call    blockmove           ; blockmove(from_low,to_low,from_high-from_low)
    add     sp,3*4

    pop     ds
    pop     di
    pop     si
    pop     bp

    ret

_moveback    endp


_TEXT ends

end
