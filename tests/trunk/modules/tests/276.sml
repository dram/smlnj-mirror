(* 276.sml *)
(* sharing and where type *)

signature S =
sig
  type s
  type t
  sharing type s = t
end;

(* S1 should fail *)
signature S1 =
sig
  type u
  structure A : S where type t = int (* effectively defines A.s as well as A.t *)
  sharing type u = A.s  (* A.s is made rigid by previous line *)
end;

