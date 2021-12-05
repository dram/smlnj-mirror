(* match-compiler/pettersson/tests.sml *)

structure Tests =
struct

local 
  open Absyn Dcons
in

val pnil = PCON ("NIL", PTUP nil);
fun pcons (phd,ptl) = PCON ("CONS", PTUP [phd, ptl]);
val pwild = PVAR "_";
val ptrue = PCON ("TRUE", PTUP nil);
val pfalse = PCON ("FALSE", PTUP nil);
val etrue = CON ("TRUE", TUP nil);
val efalse = CON ("FALSE", TUP nil);

(* example m1 *)
(* x => x *)
val match1 : match = [(PVAR "x", VAR "x")];

(* example m2 *)
(* (x,y) => x *)
val match2 : match = [(PTUP[PVAR "x", PVAR "y"], VAR "x")];

(* example 3: null *)
(* nil => true
   _ => false *)
val match3 : match =
    [(pnil, etrue), (pwild, efalse)];

(* example 3a: null *)
(* cons(x,xs) => false
   _ => true *)
val match3a : match =
    [(pcons(PVAR "x", PVAR "xs"), efalse), (pwild, etrue)];

(* example 3b: not *)
(* true => false
   false => true *)
val match3b =
    [(ptrue, efalse), (pfalse, etrue)];

(* example 4: demo (P-J/W: p 82) *)
(* demo (f, nil, ys) = A (f, ys)
   demo (f, (x::xs), nil) = B (f, x, xs)
   demo (f, (x::xs), (y::ys)) = C (f, x, xs, y, ys)
*)
val match4 : match =
     [(PTUP [PVAR "f", pnil, PVAR "ys"],
       FUNapp(VAR "A", TUP [VAR "f", VAR "ys"])),
      (PTUP [PVAR "f", pcons (PVAR "x", PVAR "xs"), pnil],
       FUNapp(VAR "B", TUP [VAR "f", VAR "x", VAR "xs"])),
      (PTUP [PVAR "f", pcons (PVAR "x", PVAR "xs"), pcons (PVAR "y", PVAR "ys")],
       FUNapp(VAR "C", TUP [VAR "f", VAR "x", VAR" xs", VAR "y", VAR "ys"]))];

(* example 5: unwieldy *)
(* unwieldy (nil, nil) = A
   unwieldy (xs, ys) = B (xs, ys) 
*)
val match5: match =
     [(PTUP [pnil, pnil], VAR "A"),
      (PTUP [PVAR "xs", PVAR "ys"], FUNapp (VAR "B", TUP [VAR "xs", VAR "ys"]))];

end (* top local *)
end (* structure Tests *)
