(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compbasic.sig *)

signature COMPBASIC = 
sig

(** important intermediate formats used during the compilations *)
type source                         (* the input file *)
type ast                            (* concrete syntax *)
type absyn                          (* abstract syntax *)
type flint                          (* intermediate code *)
type csegments = CodeObj.csegments  (* binary code segments *)
type executable = CodeObj.executable(* machine executables *)
type object                         (* resulting runtime object *)

datatype importTree = ITNODE of (int * importTree) list
                                    (* import selection specification *)

(** important context, environment, and utility functions *)
type compInfo
val mkCompInfo  : source * (absyn -> absyn) * (unit -> Stamps.generator)
                  -> compInfo
val anyErrors   : compInfo -> bool

end (* signature COMPBASIC *)


