(* bug1013.sml *)

(* This bug case was revised to conform to SML97.
   Consequently, where type is used instead of sharing
   type definitional constraints.
 *)
signature A =
sig
  type x
  datatype d = D of {extension: x}
end;

signature B =
sig
  type t
  type b_x = {a: t, b: int}
  include A where type x = b_x
end; 

signature C =
sig
  include B
end;

functor F (structure In: C) =
struct
  fun f arg =
       let val In.D {extension} = arg
           val {a, ...}: In.b_x = extension
       in a
       end
end;

