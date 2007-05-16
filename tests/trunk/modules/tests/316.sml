(* 310.sml *)
(* pretty printing of functor includes "strange tycon [E]" *)

signature FOO = sig type t end;

functor F(A : FOO) = struct type s = A.t end;
