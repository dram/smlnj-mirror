(* 257.sml *)

signature S =
sig
  type p
  structure A :
    sig
      type t
    end
  val f : unit -> A.t list
  (* option type error not even discovered here *)
end;

functor F(type r):
  sig
    val f : (unit -> r) -> r
  end = 
struct
  fun f x = x()
end;

functor G(structure X : S) :
  sig
    type u = (X.A.t list)
    val g : X.p -> u option
  end = 
struct
  type u = X.A.t list
  structure B = F(type r = u)
  fun g(x,y) = B.f X.f 
end;
