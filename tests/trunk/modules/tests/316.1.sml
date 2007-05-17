(* 316.1.sml *)
(* pretty printing of functor includes "strange tycon [E]" *)

signature FOO = sig type t end;

functor F(A : FOO) = struct type 'a s = 'a * A.t end;
