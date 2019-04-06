signature DEBUGKERNEL =
sig
  val times: int array
  val break: (System.Unsafe.object array -> unit) ref
end

structure DbgKern: DEBUGKERNEL =
struct
  val times = array (2,0)	(* sub 0 = currentTime *)
				(* sub 1 = targetTime *)
  val break = ref(fn (a:System.Unsafe.object array) => ())
end
