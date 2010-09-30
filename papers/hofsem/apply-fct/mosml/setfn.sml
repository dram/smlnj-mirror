functor SetFn X:sig type t end =
struct
  type u = X.t list
  val v = []
  fun f x = ()
end :>
 sig
   type u
   val v : u
   val f : u -> unit
 end
