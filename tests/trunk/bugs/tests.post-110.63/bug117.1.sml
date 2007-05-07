(* bug117.1.sml *)

(***************************************************************************)
(* This is illegal in version 3 of the ML standard                         *)
(* s may only be elaborated to a non-equality type (+ extra bits)          *)
(* t may only be elaborated to an equality type (for consistency with its  *)
(* constructor environment)                                                *)
(* Hence s and t can't share                                               *)
(*                                                                         *)
(* 5/7/07 Error recovery change for duplicate constructor names no longer  *)
(*   continues to instantiate and thus to check sharing type constraints   *)
(*   in the presence of duplicate constructor names. This test will now be *)
(*   divided into two. One to ensure that duplicate constructors signatures*)
(*   do not instantiate (117.1), and the other to ensure that sharing type *)
(*   checking for equality type properly (117.2).                          *)
(***************************************************************************)

signature BADSIG =
sig
  datatype s = Dummy of bool -> bool
  datatype t = Dummy of int
  sharing type s = t;
end;

