
module S(X: sig type t end) =
struct
  type u = X.t list
  let v = []
  let f x = ()
end

module K = struct type t = int end
