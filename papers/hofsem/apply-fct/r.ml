module R =
struct
  module R1 = APPS(F1)
  module R2 = APPS(F2)
  val res = (R1.x = R2.x)
end

