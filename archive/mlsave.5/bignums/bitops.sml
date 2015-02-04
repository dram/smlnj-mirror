structure BitOps = struct

infix 5 << >>
infix 4 AND
infix 3 OR

fun pow2 0 = 1 | pow2 1 = 2 | pow2 2 = 4 | pow2 3 = 8
  | pow2 4 = 16 | pow2 5 = 32 | pow2 6 = 64 | pow2 7 = 128
  | pow2 n = 2*pow2(n-1)

fun num >> bits = if bits < 0 then num << ~bits else num div (pow2 bits)
and num << bits = if bits < 0 then num >> ~bits else num * (pow2 bits)

(* This should go in assembly one day
fun num >> 0 = num
  | num >> bits = (num div 2) >> (bits-1)
fun num << 0 = num
  | num << bits = (2*num) << (bits-1)
*)

fun 0 AND _ = 0
  | _ AND 0 = 0
  | 1 AND a = a mod 2
  | a AND 1 = a mod 2
  | a AND b = (a mod 2)*(b mod 2) + 2*(a>>1 AND b>>1)

fun 0 OR a = a
  | a OR 0 = a
  | a OR b =
      let val rest = (a>>1 OR b>>1)<<1
      in
          case (a mod 2,b mod 2) of
	        (1,_) => 1 + rest
	      | (_,1) => 1 + rest
	      | _ => rest
       end

end (* structure BitOps *)
