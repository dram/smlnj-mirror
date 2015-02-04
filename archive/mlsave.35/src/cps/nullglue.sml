structure IntNull = IntShare(
struct
  val _ = System.Control.interp := true;
  fun generate lexp = ErrorMsg.impossible "no code generator!"
end);
