(* from bug1038.sml *)

signature S =
sig
  type v
  type t = v
  type u = t
end 

functor f(structure s : S) = struct end;
