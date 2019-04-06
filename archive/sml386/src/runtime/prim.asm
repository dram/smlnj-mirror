;***********************************************************************
; prim.asm
;
; Copyright (c) 1991 by:    Department of Computer Science
;			    The Technical University of Denmark
;			    DK-2800 Lyngby
;
; 19 Dec. 1991	    Yngvi Skaalum Guttesen
;
; This file contains the parts of the runtime system which has to
; be located in the same address space as the compiled ML code. Also
; some utility functions which are called directly from ML are located here.

    .386p
    .387

_DATA       segment word public use16 'DATA'
; this is the C data segment
_DATA       ends

_TEXT       segment word public use16 'CODE'
; this is the C code segment
_TEXT       ends


_RUNCODE    segment word public use32 'CODE'
; this is the SML use32 code/dada segment
_RUNCODE    ends


include tags.inc
include request.inc


_DATA   segment

    EXTRN _request   : WORD
    EXTRN _fault_exn : WORD

_DATA   ends

_TEXT	segment word public use16 'CODE'

; the c library functions

        EXTRN   _ml_syscall:FAR
        EXTRN   _ml_open:FAR
        EXTRN   _ml_connect_unix:FAR
        EXTRN   _ml_connect_inet:FAR
        EXTRN   _ml_link:FAR
        EXTRN   _ml_wait_for_in:FAR
        EXTRN   _ml_wait_for_out:FAR
        EXTRN   _ml_read:FAR
        EXTRN   _ml_readi:FAR
        EXTRN   _ml_write:FAR
        EXTRN   _ml_writei:FAR
        EXTRN   _ml_writev:FAR
        EXTRN   _ml_send_obd:FAR
        EXTRN   _ml_getdirent:FAR
        EXTRN   _ml_readlink:FAR
        EXTRN   _ml_truncate:FAR
        EXTRN   _ml_chmod:FAR
        EXTRN   _ml_access:FAR
        EXTRN   _ml_getfid:FAR
        EXTRN   _ml_getmod:FAR
        EXTRN   _ml_ftype:FAR
        EXTRN   _ml_getownid:FAR
        EXTRN   _ml_fsize:FAR
        EXTRN   _ml_atime:FAR
        EXTRN   _ml_ctime:FAR
        EXTRN   _ml_mtime:FAR
        EXTRN   _ml_isatty:FAR
        EXTRN   _ml_select:FAR
        EXTRN   _ml_pipe:FAR
        EXTRN   _ml_fionread:FAR
        EXTRN   _ml_system:FAR
        EXTRN   _ml_exec:FAR
        EXTRN   _ml_argv:FAR
        EXTRN   _ml_environ:FAR
        EXTRN   _ml_gethostname:FAR
        EXTRN   _ml_blast_out:FAR
        EXTRN   _ml_blast_in:FAR
        EXTRN   _ml_export:FAR
        EXTRN   _ml_gettime:FAR
        EXTRN   _ml_timeofday:FAR
        EXTRN   _ml_setitimer:FAR
        EXTRN   _ml_setglobal:FAR
        EXTRN   _ml_flush_icache:FAR
        EXTRN   _ml_gc:FAR
        EXTRN   _ml_enablesig:FAR
        EXTRN   _ml_masksigs:FAR
        EXTRN   _ml_getstorelist:FAR

_TEXT   ends

_RUNCODE    segment

    assume  CS:_RUNCODE, DS:_RUNCODE, ES:_RUNCODE, SS:_RUNCODE
    assume  GS:_DATA, FS:_TEXT

    public  _RUNCODE_START, _RUNCODE_END

    ORG 8192

_RUNCODE_START  LABEL   DWORD

; register usage
; eax = temp1
; ecx = temp2
; ebx = general purpose regs    (ml_roots[1])
; edx = general purpose regs    (ml_roots[2])
; esi = general purpose regs    (ml_roots[4])
; ebp = general purpose regs    (ml_roots[5])
; edi = allocptr

misc1_reg   EQU <ebx>
misc2_reg   EQU <edx>
misc3_reg   EQU <esi>
misc4_reg   EQU <ebp>
alloc_reg   EQU <edi>


; When entering the USE32 code the limitptr will be copyed onto
; the stack so we can save a byte in the  "cmp datalimit, allocptr"
; operation in the beginning of every ML-function.
; So the stack layout will be
;       esp + 00  --> limitptr
;       esp + 04  --> return address
;       ...

    public Use32Stack

Use32Stack  LABEL DWORD         ; Windows needs a stak to work with
stack_temp  DD ?                ; esp + 08 local temporary variable
            DD offset do_gc1    ; esp + 12
            DD offset do_gc2    ; esp + 16
            DD offset trapv     ; esp + 20

; The MLState vector

public _MLState_a

_MLState_a LABEL DWORD
allocptr    DD ?            ; esp + 24
limitptr    DD ?            ; esp + 28
storeptr    DD ?            ; esp + 32
exncont     DD ?            ; esp + 36
stdarg      DD ?            ; esp + 40
stdcont     DD ?            ; esp + 44
stdclos     DD ?            ; esp + 48
misc1       DD ?            ; esp + 52
misc2       DD ?            ; esp + 56
misc3       DD ?            ; esp + 60
misc4       DD ?            ; esp + 64
ml_pc       DD ?            ; esp + 68

; Machine ID

public _machine_id_a

ALIGN 4

_machine_id_a   DD  5*power_tags + tag_string
                DB  "80386",0,0,0,0,0,0,0,0,0,0,0

; this stuff comes from ml_objec.c

public _string0_a

ALIGN 4

_string0_a      DD  0*power_tags + tag_string
                DD  0

; this stuff comes from allmo.c

public _never0_a, _datalist_a

ALIGN 4

_never0_a   DD  13*power_tags + tag_string
            DB  "%never match%",0,0,0

ALIGN 4

_datalist_a DD  _never0_a+4
            DD  0               ; PTR_CtoML(0)
            DD  0*2+1           ; MOLST_nil

; this stuff comes from callgc.c

; #define refcell(z)      \
;     ML_val_t z[2] = {(ML_val_t)MAKE_DESC(1,tag_array), INT_CtoML(0)};

refcell     MACRO   x
            PUBLIC  x
            x   DD  1*power_tags+tag_array, 0*2+1
            ENDM

ALIGN 4

refcell     _collected0_a
refcell     _collectedfrom0_a
refcell     _current0_a
refcell     _gcmessages0_a
refcell     _majorcollections0_a
refcell     _minorcollections0_a
refcell     _pstruct0_a
refcell     _ratio0_a
refcell     _sighandler0_a
refcell     _softmax0_a

public  _store_preserve_a, _currentsave_a,_roots_a

_store_preserve_a    DD 0*2+1    ; INT_CtoML(0)
_currentsave_a       DD ?
_roots_a             DD  NROOTS+64 DUP (?)

; this stuff comes from cstruct.c

PUBLIC _cstruct_a, _syserror_id0_a, _gcprof_a

ALIGN 4

_array0_v        DD  0*power_tags + tag_array
_bytearray0_v    DD  0*power_tags + tag_bytearray

_gcprof_a       DD  12*power_tags + tag_bytearray
                DD  1
                DD  1
                DB  "(gc)"

ALIGN 4

RUNVEC_SZ   = 7

_runvec         DD  RUNVEC_SZ*power_tags + tag_record
                DD  _array_v
                DD  _callc_v
                DD  _create_b_v
                DD  _create_s_v
                DD  _floor_v
                DD  _logb_v
                DD  _scalb_v

_div_s               DD  3*power_tags + tag_string
                     DB  "Div",0
ALIGN 4

_div_id0             DD  1*power_tags + tag_array
                     DD  _div_s + 4
_div_e0              DD  2*power_tags + tag_record
                     DD  1
                     DD  _div_id0 + 4

_overflow_s         DD  8*power_tags + tag_string
                    DB  "Overflow"
ALIGN 4

_overflow_id0       DD  1*power_tags + tag_array
                    DD  _overflow_s + 4
_overflow_e0        DD  2*power_tags + tag_record
                    DD  1
                    DD  _overflow_id0 + 4

_unboundTable_s     DD  12*power_tags + tag_string
                    DB  "UnboundTable"
ALIGN 4

_unboundTable_id0   DD  1*power_tags + tag_array
                    DD  _unboundTable_s + 4
_unboundTable_e0    DD  2*power_tags + tag_record
                    DD  1
                    DD  _unboundTable_id0 + 4

_syserror_s         DD  8*power_tags + tag_string
                    DB  "Syserror"
ALIGN 2

_syserror_id0_a     DD  1*power_tags + tag_array
                    DD  _syserror_s + 4

CSTRUCT_SZ  = 23

_cstruct_a  DD  CSTRUCT_SZ * power_tags + tag_record
            DD  _runvec + 4
            DD  _div_e0 + 4
            DD  _overflow_e0 + 4
            DD  _syserror_id0_a + 4
            DD  _unboundTable_e0 + 4
            DD  _array0_v + 4
            DD  _bytearray0_v + 4
            DD  _collected0_a + 4
            DD  _collectedfrom0_a + 4
            DD  _current0_a + 4
            DD  _datalist_a
            DD   21                            ; NOFILE
            DD  _externlist0_a + 4
            DD  _gcmessages0_a + 4
            DD  _gcprof_a + 4
            DD  _machine_id_a + 4
            DD  _majorcollections0_a + 4
            DD  _minorcollections0_a + 4
            DD   2*2+1
            DD  _pstruct0_a + 4
            DD  _ratio0_a + 4
            DD  _sighandler0_a + 4
            DD  _softmax0_a + 4

; this stuff comes from cfuns.c

FUNCTION    MACRO   ff,nn,ll
	    DD	    3*power_tags+tag_record
            DD      OFFSET ff
            DD      $+12
            DD      $+28
            DD      ll * power_tags+tag_string
            DB      nn
            DB      16-ll DUP (0)
            ENDM

LASTFUNC    MACRO   ff,nn,ll
            local   LAB,NEXT
	    DD	    3*power_tags+tag_record
            DD      OFFSET ff
            DD      $+12
            DD      1
            DD      ll * power_tags+tag_string
            DB      nn
            DB      16-ll DUP (0)
	    ENDM



_externlist0_a LABEL DWORD
    FUNCTION    _ml_syscall,        "syscall"        ,7
    FUNCTION    _ml_open        ,   "open"           ,4
    FUNCTION    _ml_connect_unix,   "connect_unix"   ,12
    FUNCTION    _ml_connect_inet,   "connect_inet"   ,12
    FUNCTION    _ml_link,           "link"           ,4
    FUNCTION    _ml_wait_for_in,    "wait_for_in"    ,11
    FUNCTION    _ml_wait_for_out,   "wait_for_out"   ,12
    FUNCTION    _ml_read,           "read"           ,4
    FUNCTION    _ml_readi,          "readi"          ,5
    FUNCTION    _ml_write,          "write"          ,5
    FUNCTION    _ml_writei,         "writei"         ,6
    FUNCTION    _ml_writev,         "writev"         ,6
    FUNCTION    _ml_send_obd,       "send_obd"       ,8
    FUNCTION    _ml_getdirent,      "getdirent"      ,9
    FUNCTION    _ml_readlink,       "readlink"       ,8
    FUNCTION    _ml_truncate,       "truncate"       ,8
    FUNCTION    _ml_chmod,          "chmod"          ,5
    FUNCTION    _ml_access,         "access"         ,6
    FUNCTION    _ml_getfid,         "getfid"         ,6
    FUNCTION    _ml_getmod,         "getmod"         ,6
    FUNCTION    _ml_ftype,          "ftype"          ,5
    FUNCTION    _ml_getownid,       "getownid"       ,8
    FUNCTION    _ml_fsize,          "fsize"          ,5
    FUNCTION    _ml_atime,          "atime"          ,5
    FUNCTION    _ml_ctime,          "ctime"          ,5
    FUNCTION    _ml_mtime,          "mtime"          ,5
    FUNCTION    _ml_isatty,         "isatty"         ,6
    FUNCTION    _ml_select,         "select"         ,6
    FUNCTION    _ml_pipe,           "pipe"           ,4
    FUNCTION    _ml_fionread,       "fionread"       ,8
    FUNCTION    _ml_system,         "system"         ,6
    FUNCTION    _ml_exec,           "exec"           ,4
    FUNCTION    _ml_argv,           "argv"           ,4
    FUNCTION    _ml_environ,        "environ"        ,7
    FUNCTION    _ml_gethostname,    "gethostname"    ,11
    FUNCTION    _ml_blast_out,      "blas"           ,4
    FUNCTION    _ml_blast_in,       "salb"           ,4
    FUNCTION    _ml_export,         "export"         ,6
    FUNCTION    _ml_gettime,        "gettime"        ,7
    FUNCTION    _ml_timeofday,      "timeofday"      ,9
    FUNCTION    _ml_setitimer,      "setitimer"      ,9
    FUNCTION    _ml_setglobal,      "setg"           ,4
    FUNCTION    _ml_flush_icache,   "cach"           ,4
    FUNCTION    _ml_gc,             "gc"             ,2
    FUNCTION    _ml_enablesig,      "enablesig"      ,9
    FUNCTION    _ml_masksigs,       "masksigs"       ,8
    LASTFUNC    _ml_getstorelist,   "getstorelist"   ,12



; there are three ways to initiate GC (see 386.sml)

do_gc1: pop ml_pc
        sub ml_pc, 9
        mov _request, REQ_GC1
        jmp short _saveregs

do_gc2: pop ml_pc
        sub ml_pc, 17
        mov _request, REQ_GC2
        jmp short _saveregs

trapv:  pop ml_pc
        mov _request, REQ_FAULT
        lea eax, [_overflow_e0+4]
        mov _fault_exn, ax
        jmp short _saveregs


CLOSURE         MACRO   name
                local   lab
                public  name
                align   4
                DD      1*power_tags + tag_record
        name:   DD      lab
                DD      7
                DD      tag_backptr
        lab:
                ENDM

RAISE           MACRO
                mov     eax, exncont
                mov     stdcont, eax
                mov     eax, [eax]
                mov     stdclos, eax
                jmp     eax
                ENDM


DO_GC3          MACRO   x
                mov     ml_pc, OFFSET x
                mov     _request, REQ_GC3
                jmp     _saveregs
                ENDM

;*********************************************************

PUBLIC _enterUse32

_enterUse32:	; called from restoreregs in intrface.asm
            mov     alloc_reg, allocptr
            mov     misc1_reg, misc1
            mov     misc2_reg, misc2
            mov     misc3_reg, misc3
            mov     misc4_reg, misc4
            push    limitptr
            jmp     DWORD PTR [ml_pc]

_saveregs:	; return to the runtime system (restoreregs in intrface.asm)
            mov     allocptr, alloc_reg
            mov     misc1, misc1_reg
            mov     misc2, misc2_reg
            mov     misc3, misc3_reg
            mov     misc4, misc4_reg
            pop     limitptr
            db      66h
            retf

CLOSURE _handle_c
            mov     _request, REQ_EXN
            jmp     _saveregs

CLOSURE _return_c
            mov     _request, REQ_RETURN
            jmp     _saveregs

CLOSURE _callc_v
L_callc_v:  cmp     limitptr, alloc_reg
            jns     @f
            DO_GC3  L_callc_v
@@:         mov     _request, REQ_CALLC
            jmp     _saveregs


; allocate an array:
;           stdarg + 0 -> length
;           stdarg + 4 -> initial value (stdarg is a memory variable)

CLOSURE _array_v

L_arr:      mov     ecx, stdarg
            mov     ecx, [ecx]
            sar     ecx, 1                  ; ecx = length (untagged)

            mov     stack_temp, ecx
            sal     stack_temp, width_tags
            or      stack_temp, tag_array   ; stack_temp = new tag

            mov     eax, ecx                ; check the heap limit
            sal     eax, 2
            add     eax, alloc_reg
            cmp     limitptr, eax
            jns     @f
            DO_GC3  L_arr
@@:
            mov     eax, stack_temp
            stosd                       ; write the tag
            mov     eax, stdarg
            mov     eax, [eax+4]        ; eax = initial value
            mov     stdarg, edi         ; return the adr. of the array

            rep stosd                   ; we know that ecx>0

            mov     eax, stdcont
            mov     eax,[eax]
            mov     stdclos, eax
            jmp     eax


CLOSURE _create_b_v

L_byte:     mov     ecx, stdarg
            sar     ecx, 1              ; ecx = length

            mov     stack_temp, ecx
            sal     stack_temp, width_tags
            or      stack_temp, tag_bytearray  ; stack_temp = new tag

            and     ecx, NOT 3
            mov     eax, alloc_reg
            add     eax, ecx
            cmp     limitptr, eax
            jns     @f
            DO_GC3  L_byte
@@:
            mov     eax, stack_temp
            stosd                       ; write the tag
            mov     stdarg, alloc_reg
            add     alloc_reg, ecx
            sub     eax,eax
            stosd   ; must clear the last 4 bytes

            mov     eax, stdcont
            mov     eax, [eax]
            mov     stdclos, eax
            jmp     eax

CLOSURE _create_s_v

L_str:      mov     ecx, stdarg
            sar     ecx, 1                     ; ecx = length

            mov     stack_temp, ecx
            sal     stack_temp, width_tags
            or      stack_temp, tag_string     ; stack_temp = new tag

            and     ecx, NOT 3
            mov     eax, alloc_reg
            add     eax, ecx
            cmp     limitptr, eax
            jns     @f
            DO_GC3  L_str
@@:
            mov     eax, stack_temp
            stosd                       ; write the tag
            mov     stdarg, alloc_reg
            add     alloc_reg, ecx
            sub     eax,eax
            stosd                       ; must clear the last 4 bytes

            mov     eax, stdcont
            mov     eax,[eax]
            mov     stdclos, eax
            jmp     eax

co_cw_new   dw ?
co_cw_save  dw ?

CLOSURE _floor_v

            fstcw   co_cw_save
            mov     ax, co_cw_save
            and     ax, 0F3FFh         ; rounding down towards -infinity
            or      ax, 0400h
            mov     co_cw_new, ax
            fldcw   co_cw_new

            mov     eax, stdarg
            fld     qword ptr [eax]
            frndint
            fistp   stdarg
            sal     stdarg,1
            inc     stdarg

            fldcw   co_cw_save

            mov     eax, stdcont
            mov     eax,[eax]
            mov     stdclos, eax
            jmp     eax

CLOSURE _logb_v
            mov     eax, stdcont
            mov     eax,[eax]
            mov     stdclos, eax
            jmp     eax

CLOSURE _scalb_v
            lea     eax, _overflow_e0[4]
            mov     stdcont, eax
            RAISE

_RUNCODE_END:

_RUNCODE    ends

            end
