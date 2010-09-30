module F1 = functor (X:SIG) -> struct type t = X.t val x = X.x end
