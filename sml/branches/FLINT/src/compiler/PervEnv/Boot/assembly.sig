(* Copyright 1996 by AT&T Bell Laboratories *)
(* assembly.sig *)

(* 
 * This file provides the interface to the structures provied by the runtime 
 * system. The BOXED version is supposed to correspond to the assembly and
 * the C code that implement the functions using the boxed calling 
 * conventions. Right now, we tried hard to eliminate the polymorphic type
 * in the BOXED version because they are interpreted differently across 
 * different versions of the compilers. In "core.sml", we use the magic 
 * (and "dirty") cast to force them into the right ML types. (ZHONG)
 *)

signature ASSEMBLY =
  sig
    type object
    datatype 'a option = NONE | SOME of 'a

    structure A : 
      sig
        type c_function
        eqtype word8array
        eqtype real64array
        type spin_lock

        val array : int * 'a -> 'a array
        val bind_cfun : (string * string) -> c_function
        val callc : (c_function * 'a) -> 'c
        val create_b : int -> word8array
        val create_r : int -> real64array
        val create_s : int -> string
        val create_v : int * 'a list -> 'a vector
        val floor : real -> int
        val logb : real -> int
        val scalb : real * int -> real
        val try_lock : spin_lock -> bool
        val unlock : spin_lock -> unit
      end 

    exception Div
    exception Overflow
    exception SysErr of (string * int option)

    val array0 : 'a array
    val word8array0 : A.word8array
    val profCurrent : int ref
    val pollEvent : bool ref
    val pollFreq : int ref
    val pollHandler : (unit cont -> unit cont) ref
    val activeProcs : int ref
    val pstruct : object ref
    val real64array0 : A.real64array
    val sighandler : ((int * int * unit cont) -> unit cont) ref
    val vector0 : 'a vector
end


signature ASSEMBLYBOXED =
  sig
    type object
    datatype 'a option = NONE | SOME of 'a

    structure A : 
      sig
        type c_function
        eqtype word8array
        eqtype real64array
        type spin_lock

        val array : object -> object
        val bind_cfun : object -> object
        val callc : object -> object
        val create_b : object -> word8array
        val create_r : object -> real64array
        val create_s : object -> string
        val create_v : object -> object 
        val floor : object -> object
        val logb : object -> object
        val scalb : object -> object
        val try_lock : spin_lock -> object
        val unlock : spin_lock -> object
      end 

    exception Div
    exception Overflow
    exception SysErr of (string * int option)

    val array0 : object array
    val word8array0 : A.word8array
    val profCurrent : int ref
    val pollEvent : bool ref
    val pollFreq : int ref
    val pollHandler : (unit cont -> unit cont) ref
    val activeProcs : int ref
    val pstruct : object ref
    val real64array0 : A.real64array
    val sighandler : ((int * int * unit cont) -> unit cont) ref
    val vector0 : object vector
end

(*
 * $Log: assembly.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:40:05  george
 * Version 110.5
 *
 *)
