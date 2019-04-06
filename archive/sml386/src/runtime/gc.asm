;*************************************************************************
; gc.asm
;
; Copyright (c) 1991 by:    Department of Computer Science
;			    The Technical University of Denmark
;			    DK-2800 Lyngby
;
; 19 Dec. 1991	  Yngvi Skaalum Guttesen    
;
; This file contains the heart of the garbage collector, and is equivalent
; to the gc.c file in the UNIX version.
;
;                                                          Available regs.
;                          (tmp = eax is always available     bx = ebx ... )
;                                                      <-- bx cx dx si di bp>

    TITLE   Garbage Collection

    PAGE    90,132

    .386p
    .387

    .XLIST

include tags.inc

DWP     EQU <dword ptr>
tmp     EQU <eax>

    .LIST

_RUNCODE segment para use32 public 'CODE'

    EXTRN   _store_preserve_a : DWORD
    EXTRN   _MLState_a        : DWORD
    EXTRN   _enterUse32     : FAR

_RUNCODE ends

_DATA   segment word public use16 'DATA'

    EXTRN   _preserving     : WORD
    EXTRN   _wsUse32Data    : WORD
    EXTRN   _wsUse32Code    : WORD
    EXTRN   _runtime_seg    : WORD

    gmore       dd  ?           ; int ** (*gmore)()
    to_lim      dd  ?           ; int **to_lim
    to_lim0     dd  ?           ; int **to_lim0
    lowest      dd  ?           ; int **lowest
    highest     dd  ?           ; int **highest
    repair      dd  ?           ; int repair
    any_weak    dd  ?           ; int any_weak

    from_low    dd  ?
    from_high   dd  ?
    to_low      dd  ?
    to_high     dd  ?
    to_done     dd  ?
    to_where    dd  ?
    misc_roots  dd  ?
    store_list  dd  ?
    get_more    dd  ?
    first_root  dd  ?

_DATA   ends

_TEXT   segment word public use16 'CODE'

    assume cs:_TEXT, ds:_DATA, es:_DATA, gs:_DATA, fs:nothing, ss:_DATA

to_ptr      EQU <ebp>       ; int **to_ptr             ;<-- bx cx dx si di -->

; NOTE: remember that EBP has SS as default segment register
; so we have to use segment override DS:... when used as a pointer

extrn   _blast_write     : NEAR


;*************************************************************************
; xgc(refloc)
; int *refloc
;
;   Frame on entry:     [sp+0] -> return address
;                       refloc in ebx
;   On exit:
;           all regs are destroyed

xgc     proc NEAR

    assume cs:_TEXT, ds:_RUNCODE, es:_RUNCODE, gs:_DATA, fs:nothing, ss:_DATA

refloc  EQU <ebx>   ; int * refloc
m       EQU <edx>   ; int *m
                                                       ;<-- -- cx -- si di -->
    mov     m,[refloc]              ; m = * refloc

    ; if (is_ptr(m) && (m>=lowest) && (m<highest)) {

    is_ptr  m                       ; if (is_ptr(m)) &&
    jz      @f
    ret
@@: cmp     m,lowest
    jae     @f                      ;    (m >= lowest) &&
    ret
@@: cmp     m,highest
    jb      @f                      ;    (m < highest)
    ret
@@:
    sub     m,4                     ; m--
; m is pointing at the tag now


Lxgc_loop1:                         ; for (;;)
                                    ;   switch (get_tag(m)) { ...

    get_tag tmp,m
    jmp     DWP cs:Lxgc_switch[tmp*4]

Lxgc_tag_backptr:

    get_len tmp,m
    sal     tmp,2
    sub     m,tmp                   ; m -= get_len(m)
    jmp     Lxgc_loop1              ; continue

Lxgc_tag_embedded:
Lxgc_default:

    sub     m,4                     ; m--
    jmp     Lxgc_loop1              ; continue

Lxgc_tag_string:
Lxgc_tag_bytearray:

j       EQU <edi>   ; int **j
len1    EQU <ecx>   ; int len1
tmp2    EQU <esi>   ; j+len1   (j+4*len1)
                                                       ;<-- -- -- -- -- -- -->
    mov     j, to_ptr               ; j = to_ptr
    get_len len1,m
    add     len1,7
    shr     len1,2                  ; len1 = (get_len(m)+7) >> 2
                                    ; ( count in dwords )
    mov     tmp2,len1
    sal     tmp2,2
    add     tmp2,j                  ; tmp2 = j+len1

Lxgc_loop2:

    cmp     tmp2,to_lim             ; while (j+len1>to_lim) {
    jna     SHORT Lxgc2
    cmp     repair,0                ;     if (repair)
    jz      Lxgc1
    mov     repair,0                ;         repair = 0
    mov     tmp,to_lim0
    mov     to_lim,tmp              ;         to_lim = to_lim0
    jmp     Lxgc_loop2

Lxgc1:
    pushad
    push    ds
    push    es
    push    gs
    mov     ax,gs
    mov     ds,ax
    call    word ptr gmore          ;     else
    pop     gs
    pop     es
    pop     ds
    sal     edx, 16
    mov     dx,ax
    mov     to_lim,edx              ;         to_lim = gmore()
    popad
    jmp     Lxgc_loop2              ; }
                                                       ;<-- -- -- -- si -- -->
Lxgc2:


i   EQU <esi>   ; int **i                              ;<-- -- -- -- -- -- -->

    mov     i, m                    ; i = m
    cld
    db  67h
    rep movsd                       ; do {*j++ = *i++} while (--len1>0)

; len1 is free now
; i    is free now

tmp2    EQU <esi>                                      ;<-- -- cx -- -- -- -->

    cmp     repair,0                ; if (repair)
    jz      SHORT Lxgc4
    mov     tmp,to_ptr
    add     tmp,5*4
    cmp     tmp,to_lim              ;   if (to_ptr+5<to_lim) {
    jae     SHORT Lxgc3
    mov     tmp,to_lim
    mov     tmp2,[m+4]
    mov     [tmp-4],tmp2            ;       *--to_lim = m[1]
    mov     [tmp-2*4],m
    add     DWP [tmp-2*4],4         ;       *--to_lim = m+1
    sub     to_lim,2*4              ;   }
    jmp     SHORT Lxgc4

Lxgc3:
                                    ;   else {
    mov     repair,0                ;       repair = 0
    mov     tmp,to_lim0             ;
    mov     to_lim,tmp              ;       to_lim = to_lim0 }

Lxgc4:

    mov     tmp,to_ptr
    add     tmp,4
    mov     DWP [m + 4], tmp        ; m[1] = 1 + to_ptr
    mov     to_ptr,j                ; to_ptr = j
    mov     DWP [m], tag_forwarded  ; *m = tag_forwarded



    mov     tmp,[refloc]
    sub     tmp,m
    sub     tmp,4
    add     tmp,[m+4]
    mov     [refloc],tmp            ; *refloc += m[1] -(m+1)
    ret

Lxgc_tag_array:                                        ;<-- -- cx -- si di -->

    cmp     _preserving,0
    je      SHORT Lxgc_tag_record           ; if (preserving)
    mov     DWP ds:[to_ptr],16*3+1          ; *to_ptr++ = 16*3+1
    mov     DWP ds:[to_ptr+4],m
    add     DWP ds:[to_ptr+4],4             ; *to_ptr++ = m+1
    mov     DWP ds:[to_ptr+8],-1            ; *to_ptr++ = -1
    mov     tmp,_store_preserve_a
    mov     ds:[to_ptr+3*4],tmp             ; *to_ptr++ = store_preserve
    mov     DWP [_store_preserve_a],to_ptr
    add     DWP [_store_preserve_a],4       ; store_preserve = to_ptr-3
    add     to_ptr, 4*4

; fall trough to tag_record

Lxgc_tag_record:

j       EQU <edi>       ; int **j
len1    EQU <ecx>       ; int len1
tmp2    EQU <esi>       ; j+len1   (j+4*len1)          ;<-- -- -- -- -- -- -->

    mov     j, to_ptr               ; j = to_ptr
    get_len len1,m
    inc     len1                    ; len1 = get_len(m)+1
                                    ; ( count in dwords )
    mov     tmp2,len1
    sal     tmp2,2
    add     tmp2,j                  ; tmp2 = j+len1

Lxgc_loop3:

    cmp     tmp2,to_lim             ; while (j+len1>to_lim) {
    jna     SHORT Lxgc6
    cmp     repair,0                ;     if (repair)
    jz      Lxgc5
    mov     repair,0                ;         repair = 0
    mov     tmp,to_lim0
    mov     to_lim,tmp              ;         to_lim = to_lim0
    jmp     Lxgc_loop3

Lxgc5:
    pushad
    push    ds
    push    es
    push    gs
    mov     ax,gs
    mov     ds,ax
    call    word ptr gmore          ;     else
    pop     gs
    pop     es
    pop     ds
    sal     edx, 16
    mov     dx,ax
    mov     to_lim,edx              ;         to_lim = gmore()
    popad
    jmp     Lxgc_loop3              ;    }
                                                       ;<-- -- -- -- si -- -->
Lxgc6:

i   EQU <esi>   ; int **i                              ;<-- -- -- -- -- -- -->

    mov     i, m                    ; i = m
    cld
    db  67h
    rep movsd                       ; do {*j++ = *i++} while (--len1>0)

; len1 is free now
; i    is free now

tmp2    EQU <esi>                                      ;<-- -- cx -- -- -- -->

    cmp     repair,0                ; if (repair)
    jz      SHORT Lxgc8
    mov     tmp,to_ptr
    add     tmp,5*4
    cmp     tmp,to_lim              ;   if (to_ptr+5<to_lim) {
    jae     SHORT Lxgc7
    mov     tmp,to_lim
    mov     tmp2,[m+4]
    mov     [tmp-4],tmp2            ;       *--to_lim = m[1]
    mov     [tmp-2*4],m
    add     DWP [tmp-2*4],4         ;       *--to_lim = m+1
    sub     to_lim,2*4              ;   }
    jmp     SHORT Lxgc8

Lxgc7:
                                    ;   else {
    mov     repair,0                ;       repair = 0
    mov     tmp,to_lim0             ;
    mov     to_lim,tmp              ;       to_lim = to_lim0 }

Lxgc8:

    mov     tmp,to_ptr
    add     tmp,4
    mov     DWP [m + 4], tmp        ; m[1] = 1 + to_ptr
    mov     to_ptr,j                ; to_ptr = j
    mov     DWP [m], tag_forwarded  ; *m = tag_forwarded

    ; fall trough to tag_forwarded

Lxgc_tag_forwarded:

    mov     tmp,[refloc]
    sub     tmp,m
    sub     tmp,4
    add     tmp,[m+4]
    mov     [refloc],tmp            ; *refloc += m[1] -(m+1)
    ret



Lxgc_tag_suspension:                                   ;<-- -- cx -- si di -->

j       EQU <edi>   ; int **j
len1    EQU <ecx>   ; int len1
tmp2    EQU <esi>   ; j+len1   (j+4*len1)              ;<-- -- -- -- -- -- -->

    mov     j, to_ptr               ; j = to_ptr
    mov     len1,2                  ; len1 = 2
                                    ; ( count in dwords )
    mov     tmp2,len1
    sal     tmp2,2
    add     tmp2,j                  ; tmp2 = j+len1

Lxgc_loop4:

    cmp     tmp2,to_lim             ; while (j+len1>to_lim) {
    jna     SHORT Lxgc10
    cmp     repair,0                ;     if (repair)
    jz      Lxgc9
    mov     repair,0                ;         repair = 0
    mov     tmp,to_lim0
    mov     to_lim,tmp              ;         to_lim = to_lim0
    jmp     Lxgc_loop4

Lxgc9:
    pushad
    push    ds
    push    es
    push    gs
    mov     ax,gs
    mov     ds,ax
    call    word ptr gmore          ;     else
    pop     gs
    pop     es
    pop     ds
    sal     edx, 16
    mov     dx,ax
    mov     to_lim,edx              ;         to_lim = gmore()
    popad
    jmp     Lxgc_loop4              ;    }
                                                       ;<-- -- -- -- si -- -->
Lxgc10:

i   EQU <esi>   ; int **i                              ;<-- -- -- -- -- -- -->

    mov     i, m                    ; i = m
    cld
    db  67h
    rep movsd                       ; do {*j++ = *i++} while (--len1>0)
@@:                                                    ;<-- -- cx -- si -- -->

; len1 is free now
; i    is free now

tmp2    EQU <esi>                                      ;<-- -- cx -- -- -- -->

    cmp     repair,0                ; if (repair)
    jz      SHORT Lxgc12
    mov     tmp,to_ptr
    add     tmp,5*4
    cmp     tmp,to_lim              ;   if (to_ptr+5<to_lim) {
    jae     SHORT Lxgc11
    mov     tmp,to_lim
    mov     tmp2,[m+4]
    mov     [tmp-4],tmp2            ;       *--to_lim = m[1]
    mov     [tmp-2*4],m
    add     DWP [tmp-2*4],4         ;       *--to_lim = m+1
    sub     to_lim,2*4              ;   }
    jmp     SHORT Lxgc12

Lxgc11:
                                    ;   else {
    mov     repair,0                ;       repair = 0
    mov     tmp,to_lim0             ;
    mov     to_lim,tmp              ;       to_lim = to_lim0 }

Lxgc12:

    mov     tmp,to_ptr
    add     tmp,4
    mov     DWP [m + 4], tmp        ; m[1] = 1 + to_ptr
    mov     to_ptr,j                ; to_ptr = j
    mov     DWP [m], tag_forwarded  ; *m = tag_forwarded

    mov     tmp,[refloc]
    sub     tmp,m
    sub     tmp,4
    add     tmp,[m+4]
    mov     [refloc],tmp            ; *refloc += m[1] -(m+1)
    ret

ALIGN 4

Lxgc_switch:    ; switch table

    dd  Lxgc_default, Lxgc_tag_record
    dd  Lxgc_default, Lxgc_tag_forwarded
    dd  Lxgc_default, Lxgc_tag_backptr
    dd  Lxgc_default, Lxgc_tag_embedded
    dd  Lxgc_default, Lxgc_tag_array
    dd  Lxgc_default, Lxgc_tag_bytearray
    dd  Lxgc_default, Lxgc_tag_suspension
    dd  Lxgc_default, Lxgc_tag_string

xgc endp

;***************************************************************************
; Frame on entry:
;   esp   38 -> first_root      int *
;   esp + 34 -> get_more        int ** (*get_more)()
;   esp + 30 -> store_list      int **
;   esp + 26 -> misc_roots      int ***
;   esp + 22 -> to_where        int ***
;   esp + 18 -> to_done         int **
;   esp + 14 -> to_high         int **
;   esp + 10 -> to_low          int **
;   esp + 06 -> from_high       int **
;   esp + 02 -> from_low        int **
;   esp + 00 -> return adress


                                                       ;<-- bx cx dx si di -->
PUBLIC _gc

_gc  proc    NEAR

    assume cs:_TEXT, ds:_DATA, es:_DATA, gs:_DATA, fs:nothing, ss:_DATA

    push    bp
    mov     bp, sp

    push    si
    push    di
    push    ds


    mov     ax, ds
    mov     es, ax
    mov     di, offset from_low
    mov     si, bp
    add     si, 4
    cld
    mov     cx, 10
    rep movsd                       ; copy the parameters so we can use the
                                    ; ebp register
    mov     ax, _wsUse32Data
    mov     ds,ax
    mov     es,ax
    mov     ax, _DATA
    mov     gs,ax                   ; setup the segs.

    assume cs:_TEXT, ds:_RUNCODE, es:_RUNCODE, gs:_DATA, fs:nothing, ss:_DATA

    mov     any_weak,0              ; any_weak = 0
    mov     tmp,get_more
    mov     gmore,tmp               ; gmore = get_more
    mov     to_ptr,to_done          ; to_ptr = to_done
    mov     tmp,to_high
    mov     to_lim0,tmp             ; to_lim0 = to_high
    mov     to_lim,tmp              ; to_lim = to_high
    mov     tmp,from_low
    mov     lowest,tmp              ; lowest = from_low
    mov     tmp,from_high
    mov     highest,tmp             ; highest = from_high

    mov     repair,0
    cmp     first_root,0
    jz      Lgc4                    ; if (first_root) {

    mov     repair,1                ; repair = 1

    mov     refloc,first_root
    call    xgc                     ;   xgc(first_root)

x           EQU <edx>               ;   int x
blast_begin EQU <edi>               ;   int **blast_begin
                                                       ;<-- bx cx -- si -- -->

    mov     blast_begin,to_low      ;   blast_begin = to_low
    mov     x,to_done               ;   x = to_done

Lgc_loop1:

    cmp     x,to_ptr                ;   while (x<to_ptr) {
    jae     SHORT Lgc2

p       EQU <ebx>   ; int p
descr   EQU <ecx>   ; int descr
                                                       ;<-- -- -- -- si -- -->
    mov     p,x
    add     p,4                     ;       int p = x+4
    mov     descr,[x]               ;       int descr = *x

    contains_no_ptrs descr          ;       if contain_no_ptrs(descr) {
    jz      SHORT Lgc1
    get_len tmp,x
    add     tmp,7
    and     tmp,NOT 3
    add     x,tmp                   ;           x += ((get_len(x)+7)&~3)
    jmp     Lgc_loop1               ;           continue
                                    ;       }
Lgc1:

    get_lenz tmp,x
    sal     tmp,2
    add     tmp,4
    add     x,tmp                   ;       x += get_lenz(x) * 4 + 4

Lgc_loop2:


    mov     refloc,p                ;       do {
    push    p
    push    x
    push    blast_begin
    call    xgc                     ;           xgc(p)
    pop     blast_begin
    pop     x
    pop     p

    add     p,4                     ;           p+=4
    cmp     p,x                     ;        }
    jb      Lgc_loop2               ;       while (p<x)
                                    ;
    jmp     Lgc_loop1               ;   }

Lgc2:                                                  ;<-- bx cx -- si di -->

    mov     tmp,first_root
    push    dword ptr [tmp]
    push    x
    push    blast_begin
    call    _blast_write            ;   blast_write(blast_begin,x,*first_root)
    add     sp,12

    cmp     repair,0                ;   if (repair) {
    jz      Lgc4

Lgc_loop9:

tolim       EQU <esi>   ; to_lim alias ### REMEMBER TO UPDATE!! ###
tolim0      EQU <edi>   ; to_lim0 alias
loc         EQU <ebx>   ;
old         EQU <ecx>   ;
                                                       ;<-- -- -- -- -- -- -->

    mov     tolim,to_lim
    mov     tolim0,to_lim0
@@:
    cmp     tolim,tolim0            ;       while (to_lim<to_lim0) {
    jnb     Lgc3
    mov     loc,[tolim]             ;           int *loc = *to_lim++
    mov     old,[tolim+4]           ;           int *old = *to_lim++
    add     tolim,2*4

    mov     tmp,[loc]
    mov     tmp,[tmp-4]
    mov     [loc-4],tmp             ;           loc[-1] = (loc[0])[-1]

    mov     [loc],old               ;           loc[0] = old
    jmp     @b
                                    ;       }
Lgc3:

    mov     to_lim,tolim            ; update to_lim



    pop     ds
    pop     di
    pop     si
    pop     bp

    sub     eax,eax
    ret                             ;       return 0
                                    ;   }
                                    ; }
                                                       ;<-- bx cx -- si di -->
Lgc4:

    ; do the refs

px      EQU <esi>   ; int **px
r       EQU <edi>   ; int **r
                                                       ;<-- bx cx -- -- ---->

    mov     px,store_list           ; for (px=store_list; px!=1 ; px=px[2])
    jmp     Lgc_loop3_test          ; {

Lgc_loop3:

    mov     r,[px+4]                ;   r = px[1]
    sar     r,1                     ;   r = px[1]>>1
    sal     r,2                     ;   r = (int*) px[1]>>1
    add     r,[px]                  ;   r = px[0] + (px[1]>>1)

    cmp     r,from_low              ;   if (r>=from_low)
    jnae    @f
    cmp     r,from_high             ;     && (r<from_high)
    jnb     @f
    jmp     Lgc_loop3_continue      ;     continue

@@:

    cmp     _preserving,0            ;   if (preserving) {
    jz      Lgc5

    mov     DWP ds:[to_ptr],16*3+1     ;       *to_ptr++ = 16*3+1

    mov     tmp,[px]
    mov     ds:[to_ptr+4],tmp          ;       *to_ptr++ = px[0]

    mov     tmp,[px+4]
    mov     ds:[to_ptr+2*4],tmp        ;       *to_ptr++ = px[1]

    mov     tmp,_store_preserve_a
    mov     ds:[to_ptr+3*4],tmp        ;       *to_ptr++ = store_preserve

    add     to_ptr,4*4              ;       adjust to_ptr

    mov     _store_preserve_a,to_ptr
    sub     _store_preserve_a,3*4      ;       store_preserve = to_ptr-3
                                    ;   }
Lgc5:
    push    px

    mov     refloc,r                ;   xgc(r)
    call    xgc

    pop     px                      ; }


Lgc_loop3_continue:

    mov     px,[px+2*4]             ;   px = px[2]

Lgc_loop3_test:

    cmp     px,1
    jne     Lgc_loop3               ;   px != 1 ?
                                                       ;<-- bx cx -- si di -->
; end of for loop


    ; do misc. roots


p       EQU <esi>   ; int ***p
                                                       ;<-- bx cx -- -- di -->
    mov     p,misc_roots            ; p = misc_roots
    jmp     Lgc_loop4_test

Lgc_loop4:                          ; for (p = misc_roots ; *p ; p++) {

    push    p
    mov     refloc,[p]
    call    xgc                     ;   xgc(*p)
    pop     p                       ; }

    add     p,4                     ;   p++

Lgc_loop4_test:
    cmp     DWP [p],0               ;   *p <> 0
    jne     Lgc_loop4
                                                       ;<-- bx cx -- si di -->

    ; Finish the new space


x       EQU <esi>   ; int x
p       EQU <edi>   ; int p
descr   EQU <ecx>   ; int descr
                                                       ;<-- bx -- -- -- -- -->

    mov   x,to_low                  ; x = to_low

Lgc_loop5:

    cmp     x,to_ptr                ; while (x<to_ptr) {
    jnb     Lgc8

    mov     p,x                     ;   p = x+4
    add     p,4

    mov     descr,[x]               ;   descr = *x

    contains_no_ptrs descr          ;    if (contains_no_ptrs(descr)) {
    jz      SHORT Lgc6

    get_len tmp,x
    add     tmp,7
    and     tmp, NOT 3
    add     x,tmp                   ;        x += ((get_len(x) + 7) & ~3)
    jmp     Lgc_loop5               ;        continue }

Lgc6:

    get_lenz tmp,x
    sal     tmp,2
    add     tmp,4
    add     x,tmp                   ;   x += get_len(x) * 4 + 4

    mov     tmp,tag_suspension
    add     tmp,2*power_tags
    cmp     tmp,descr
    jne     Lgc7                    ;   if (descr==tag_suspension+2*power_tags)
    mov     any_weak,1              ;       { any_weak = 1 ;
    jmp     Lgc_loop5               ;         continue }

Lgc7:

Lgc_loop6:

    push    x                       ;   do {
    push    p
    mov     refloc,p
    call    xgc                     ;       xgc(p)
    pop     p
    pop     x

    add     p,4                     ;       p += 4
    cmp   p,x                       ;   }
    jb      Lgc_loop6               ;   while (p<x)

    jmp     Lgc_loop5               ; }
                                                       ;<-- bx cx -- si di -->


Lgc8:

    cmp     any_weak,0              ; if (any_weak)
    jz      Lgc_out                 ; {


x       EQU <esi>   ; int x
p       EQU <edi>   ; int *p
descr   EQU <ecx>   ; int descr
m       EQU <ebx>   ; int *m
                                                       ;<-- -- -- -- -- -- -->

    mov     x,to_low                ;   x = to_low

Lgc_loop7:

    cmp     x,to_ptr
    jnb     Lgc_out                 ;   while (x<to_ptr) {

    mov     p,x
    add     p,4                     ;       int p=x+4

    mov     descr,[x]               ;       int descr=*x

    contains_no_ptrs(descr)         ;       if (contain_no_ptrs(descr)) {
    jz      Lgc9

    get_len tmp,x
    add     tmp,7
    and     tmp, NOT 3
    add     x,tmp                   ;           x += (get_len(x)+7) & ~3)
    jmp     Lgc_loop7               ;           continue }

Lgc9:

    get_lenz tmp,x
    sal     tmp,2
    add     tmp,4
    add     x,tmp                   ;   x += get_lenz(x)*4+4

    mov     tmp,tag_suspension
    add     tmp,2*power_tags
    cmp     descr,tmp               ;   if (descr == tag_suspension + 2*power_tags)
    jne     Lgc_loop7

    mov     m,[p]
    sub     m,4                     ;       int *m = *p - 1

    test    m,1                     ;       if (!(m&1) &&
    jnz     Lgc_loop7
    cmp     m,from_low              ;           ( m >= from_low) &&
    jb      Lgc_loop7
    cmp     m,from_high             ;           ( m <= from_high) {
    ja      Lgc_loop7

Lgc_loop8:                          ;           for (;;) {

    get_tag tmp,m
    jmp     DWP cs:Lgc_switch[tmp*4]   ;       switch(get_tag(m)) {

Lgc_tag_string:
Lgc_tag_bytearray:
Lgc_tag_array:
Lgc_tag_record:
Lgc_tag_suspension:

    mov     DWP [p],1               ;                   *p = 1
    mov     DWP [p-4],tag_suspension+3*power_tags
    jmp     Lgc_loop7               ;                   p[-1] = tag_suspension+3*power_tags
                                    ;                   break
Lgc_tag_forwarded:

    mov     tmp,[m+4]
    add     [p],tmp
    sub     [p],m
    sub     DWP [p],4               ;                   *p += m[1] - (m+1)
    jmp     Lgc_loop7               ;                   break

Lgc_tag_backptr:

    get_len tmp,m
    sal     tmp,2
    sub     m,tmp                   ;                   m -= get_len(m)
    jmp     Lgc_loop8               ;                   continue

Lgc_tag_embedded:
Lgc_default:

    sub     m,4                     ;                   m--
    jmp     Lgc_loop8               ;                   continue
                                    ;               } switch
                                    ;           } for
                                    ;       } if
                                    ;   } if
                                    ; } while
                                                       ;<-- bx cx -- si di -->
Lgc_out:


    mov     tmp,to_where
    mov     gs:[tmp],to_ptr            ; *to_where = to_ptr

    mov     eax,1

    pop     ds
    pop     di
    pop     si
    pop     bp
    ret                             ; return 1


Lgc_switch:
    dd  Lgc_default, Lgc_tag_record
    dd  Lgc_default, Lgc_tag_forwarded
    dd  Lgc_default, Lgc_tag_backptr
    dd  Lgc_default, Lgc_tag_embedded
    dd  Lgc_default, Lgc_tag_array
    dd  Lgc_default, Lgc_tag_bytearray
    dd  Lgc_default, Lgc_tag_suspension
    dd  Lgc_default, Lgc_tag_string

_gc  endp

_TEXT ends

end
