(* test.sml --
  Derived from Twelf 1.5RC1
  nonstrict type operator sigmatch
 *)

signature S =
sig
  type 'a t

  val f : unit -> 'a t
end;

functor F (X : S) =
struct
    val x : int X.t = X.f () 
end;

structure A : S =
struct
  type 'a t = unit

  fun f () = ()
end;

structure B = F(A);
