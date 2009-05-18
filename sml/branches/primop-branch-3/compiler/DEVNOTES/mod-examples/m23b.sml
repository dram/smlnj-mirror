(* m23b.sml *)
(* entityEnv in functor instantiations *)
(* does closure entEnv of formally instantiated F contain correct binding of ev_t *)

signature S =
sig
  type t
  functor F(X: sig end) : sig type s = t val a : s end
end;

functor G(Y: S) =
struct
  structure A = Y.F(struct end)
  val b = (fn (x: Y.t) => ()) A.a
end


