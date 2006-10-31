(* bug1308.4.sml *)

signature STOCOMP =
sig
  type store
  val update : (store -> 'a) -> 'a
  type 'a lift
  val rap : 'a -> 'a lift
end;

functor F (structure SC : STOCOMP)
: sig
    val x : SC.store SC.lift
  end =
struct
  val x = SC.update (SC.rap: SC.store -> SC.store SC.lift)
end;

structure StoComp : STOCOMP =
struct
  type store = string list * int
  fun update f = f (["x"],0)
  type 'a lift = ('a -> store) -> store
  fun rap (v: 'a) (k: 'a -> store) = k v
end;

structure R =
  F (structure SC = StoComp);

val s2' = R.x (fn x => x);
