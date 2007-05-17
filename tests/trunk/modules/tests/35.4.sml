(* test35.4.sml *)
(* 5/17/07 If sigdecs are instantiated even if there are errors during
   elaboration, then this testcase gives an additional error

   35.4.sml:5.1-12.4 Error: type definition spec inside of sharing at: v
 *)
type s0 = int -> int;  (* not an equality type *)

signature S =
sig
  eqtype v
  type t
  type u = t * t
  sharing type t = s0  (* should cause an error *)
  sharing type u = v
end;
