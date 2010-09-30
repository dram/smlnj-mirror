module SetFn(X: sig type t end) 
 :
  sig 
    type u 
    val v : u
    val f : u -> bool
  end
= 
struct
  type u = X.t list
  let v = []
  let f x = true
end  

module K = struct type t = int end
