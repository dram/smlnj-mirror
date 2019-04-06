structure BitOps = struct

infix 5 << >>
infix 4 bit_and
infix 3 bit_or

fun pow2 0 = 1 | pow2 1 = 2 | pow2 2 = 4 | pow2 3 = 8 | pow2 4 = 16
  | pow2 5 = 32 | pow2 6 = 64 | pow2 7 = 128 | pow2 n = 2*pow2(n-1)

fun num >> bits = if bits < 0 then num << ~bits else num div (pow2 bits)
and num << bits = if bits < 0 then num >> ~bits else num * (pow2 bits)

(* This should go in assembly one day
fun num >> 0 = num
  | num >> bits = (num div 2) >> (bits-1)
fun num << 0 = num
  | num << bits = (2*num) << (bits-1)
*)

fun 0 bit_and _ = 0
  | _ bit_and 0 = 0
  | 1 bit_and a = a mod 2
  | a bit_and 1 = a mod 2
  | a bit_and b = (a mod 2)*(b mod 2) + 2*((a div 2) bit_and (b div 2))

fun 0 bit_or a = a
  | a bit_or 0 = a
  | a bit_or b =
      let val rest = ((a div 2) bit_or (b div 2)) * 2
      in
          case (a mod 2,b mod 2) of
	        (1,_) => 1 + rest
	      | (_,1) => 1 + rest
	      | _ => rest
       end

end (* structure BitOps *)
