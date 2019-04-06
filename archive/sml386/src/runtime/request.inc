;request.inc
;
;COPYRIGHT (c) 1990 by AT&T Bell Laboratories.
;
;These are the request codes for controlling the run_ml() loop.

REQ_RETURN      EQU 0
REQ_EXN         EQU 1
REQ_FAULT       EQU 2
REQ_GC1         EQU 3
REQ_CALLC       EQU 4
REQ_SIGNAL      EQU 5
REQ_SIG_RETURN  EQU 6
REQ_SIG_RESUME  EQU 7
REQ_SIG_RAISE   EQU 8
REQ_GC2         EQU 9
REQ_GC3         EQU 10
