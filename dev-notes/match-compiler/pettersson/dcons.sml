(* pet/dcons.sml *)

(* data constructors *)

(* All we need initially is dcon -> dconfamily, and using that to test whether
 * a set of decons is exhaustive *)

structure Dcons =
struct

(* deconFamilies : dcon list list *)

type dcon = string

val boolDcons = ["TRUE", "FALSE"]
val listDcons = ["CONS", "NIL"]

val dconFamilies = [boolDcons, listDcons]


fun member (dcon: dcon, family: dcon list) =
    List.exists (fn dcon' => (dcon' = dcon)) family

fun subset (xs: dcon list, ys: dcon list) =
    List.all (fn x => List.exists (fn y => (y = x)) ys) xs

(* dconFamily : dcon -> dcon list
 * return the complete "family" of constructors containing the dcon.
 * e.g. "NIL" ==> ["NIL", "CONS"]. *)
fun dconFamily dcon =
    let fun loop nil = nil
	  | loop (fam::rest) =
	      if member (dcon, fam) then fam else loop rest
    in loop dconFamilies
    end

(* isExhaustive : dcon list -> bool *)
fun isExhaustive (dcons as dcon :: _) =
      let val family = dconFamily dcon
       in subset (family, dcons)
      end
  | isExhaustive nil = raise Fail "isExhaustive nil"

(* isConstant : dcon -> bool *)
fun isConstant dcon =
    (case dcon
      of "TRUE" => true
       | "FALSE" => true
       | "NIL" => true
       | _ => false)

end (* structure Dcons *)
