(* mc/lib/lambdavar.sml *)

structure LambdaVar =
struct

  type lvar = int
  fun inc r = r := !r + 1

  val varcount = ref 0
  fun mkLvar () = (inc varcount; !varcount)

end
    
