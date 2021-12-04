(* tests.sml *)

(* example m1 *)
(* x => x *)
val m1 = (["u0"], [([PVAR "v0"], VAR "v0")], TUP nil);

(* example m2 *)
(* (x,y) => x *)
val m2 = (["u0"], [([PTUP[PVAR "v0", PVAR "v1"]], VAR "v0")], TUP nil);

val pnil = PCON ("NIL", PTUP nil);
fun pcons (phd,ptl) = PCON ("CONS", PTUP [phd, ptl])
val etrue = CON ("TRUE", TUP nil);
val efalse = CON ("FALSE", TUP nil);

(* example 3: null *)
(* nil => true
 * _ => false *)
val m3 = (["u0"],
	  [([pnil], etrue)],
	  ERROR);

(* example 4: demo (P-J/W: p 82) *)
(* demo (f, nil, ys) = A (f, ys)
   demo (f, (x::xs), nil) = B (f, x, xs)
   demo (f, (x::xs), (y::ys)) = C (f, x, xs, y, ys)
*)
val m4 =
    (["u0"],
     [([PTUP [PVAR "f", pnil, PVAR "ys"]],
       FUNapp(VAR "A", TUP [VAR "f", VAR "ys"])),
      ([PTUP [PVAR "f", pcons (PVAR "x", PVAR "xs"), pnil]],
       FUNapp(VAR "B", TUP [VAR "f", VAR "x", VAR "xs"])),
      ([PTUP [PVAR "f", pcons (PVAR "x", PVAR "xs"), pcons (PVAR "y", PVAR "ys")]],
       FUNapp(VAR "C", TUP [VAR "f", VAR "x", VAR" xs", VAR "y", VAR "ys"]))
     ],
     ERROR);

(* example 5: unweildy *)

val m5 =
    (["u0"],
     [([PTUP [pnil, pnil]],
	VAR "A"),
      ([PTUP [PVAR "xs", PVAR "ys"]],
       FUNapp (VAR "B", TUP [VAR "xs", VAR "ys"]))],
     ERROR)
