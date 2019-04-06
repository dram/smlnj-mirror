(* Copyright 1989 by AT&T Bell Laboratories *)

signature RUNVEC =
  sig
    val array : int * 'a -> 'a array
    val callc : 'b (* func*)  * 'a -> 'c
    eqtype bytearray
    val create_b : int -> bytearray
    val create_s : int -> string
    val floor : real -> int
    val logb : real -> int
    val scalb : real * int -> real
  end (* RUNVEC *)

signature ASSEMBLY =
  sig
    datatype datalist = DATANIL | DATACONS of (string * string * datalist)
    type func
    datatype funclist = FUNCNIL | FUNC of (func * string * funclist)
    type object
    structure A : RUNVEC
    exception Div
    exception Overflow
    exception SysError of (int * string)
    exception UnboundTable
    val array0 : 'a array
    val bytearray0 : A.bytearray
    val calleesaves : int
    val collected : int ref
    val collectedfrom : int ref
    val current : string ref
    val datalist : datalist
    val dtablesize : int
    val external : funclist
    val gcmessages : int ref
    val gcprof : string ref
    val lastratio : int ref
    val machine : string  (* m68, mipsb, mipsl, ns32, sparc or vax *)
    val majorcollections : int ref
    val minorcollections : int ref
    val opsys : int   (* 1 = vax bsd ultrix, 4.2, 4.3
			 2 = sunos 3.0, 4.0 
			 3 = vax v9 (bell labs), hpux or RISCos *)
    val pstruct : object ref
    val ratio : int ref
    val sighandler : ((int * int * unit cont) -> unit cont) ref
    val softmax : int ref
end
