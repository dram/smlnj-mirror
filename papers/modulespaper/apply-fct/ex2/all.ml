module K = struct type t = int end
module K' = struct type t = int end

module S(X:sig type t end) : sig type u val v : u val f : u -> bool end = 
struct
  type u = X.t list
  let v = []
  let f x = true
end 
module I0 = S(K)
module I1 = S(K')
let _ = I0.f(I1.v)
