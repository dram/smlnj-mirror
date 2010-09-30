signature setfn = 
sig
functor F X:sig type t end : 
  sig
    type u
    val v : u
    val f : u -> unit
  end
end
