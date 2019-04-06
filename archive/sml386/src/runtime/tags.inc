;*************************************************************************
; tags.inc
;
; 10/4/91   Yngvi Skaalum Guttesen
;
; This file has a corresponding ML structure tags embedded in structure
; Boot in the file boot\perv.sml

IFNDEF _TAGS_

_TAGS   equ 1

include globdefn.inc

width_tags      equ     4
power_tags      equ    16
tag_record      equ     1   ; 0001
tag_array       equ     9   ; 1001
tag_bytearray   equ    11   ; 1011
tag_string      equ    15   ; 1111
tag_embedde     equ     7   ; 0111
tag_suspension  equ    13   ; 1101
tag_backptr     equ     5   ; 0101
tag_forwarded   equ     3   ; 0011

mask_tags       equ     power_tags-1

;-------------------------------------------------------------------------
; #define contains_no_ptrs(x) ((x)&2)

contains_no_ptrs    MACRO x         ; Zero if x contains pointers
    test        x,2
    ENDM

;-------------------------------------------------------------------------
; #define is_ptr(x)   (!((int)(x)&1))

is_ptr          MACRO x             ; Zero if x is a pointer
    test        x,1
    ENDM

;-------------------------------------------------------------------------
; #define get_len(x)    ( * (int *) (x) >> width_tags)

get_len     MACRO x,y               ;; Returns the length of record in x
    mov     x,[y]                   ;; pointet to by y. y points to the
    shr     x,width_tags            ;; tag of the record
    ENDM                            ;; x and y must be registers !

;-------------------------------------------------------------------------
; #define get_lenz(x)
;       ((((* (int*) (x)) & mask_tags)==tag_suspension) ? 1 : get_len(x))

get_lenz    MACRO x,y               ;; Returns the length of record in x
    local   L1, L2                  ;; pointet to by y. y points to the
    mov     tmp,[y]                 ;; tag of the record. If the tag is
    and     tmp,mask_tags           ;; tag_suspension x=1.
    cmp     tmp,tag_suspension      ;; x and y must be registers !
    je      L1
    get_len x,y
    jmp     L2
L1:
    mov     x,1
L2:
    ENDM

;-------------------------------------------------------------------------
; #define get_strlen(x) ((( * (int *) (x) >> width_tags) + 3)/4)

get_strlen  MACRO x,y       ;; x = stringlength in dwords rounded up to 4
    mov     x,[y]           ;; x and y must be registers
    sar     x,width_tags
    add     x,3
    sar     x,2
    ENDM

;-------------------------------------------------------------------------
; #define get_tag(x) ( * (int *) (x) & mask_tags)

get_tag     MACRO x,y       ;; set x to the tags of the descriptor pointed
    mov     x,[y]           ;; to by y
    and     x,mask_tags     ;; x and y must be registers
    ENDM



;-------------------------------------------------------------------------
; #define MAKE_DESC(l,t) ((l)*power_tags+(t))

MAKE_DESC   MACRO   l,t
    l*power_tags+t
    ENDM


ENDIF
