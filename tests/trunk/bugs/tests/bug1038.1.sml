(* bug1038.sml *)

signature S =
sig
  type a
  datatype 'a internal_address = Address of 'a
  type T = a internal_address
  type U = T
end 

functor f(structure s : S) = struct end;
