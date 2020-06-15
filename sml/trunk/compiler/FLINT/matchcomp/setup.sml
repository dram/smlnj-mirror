(* setup.sml *)

(* test rig for testing and debugging the match compiler *)
(* some test cases *)

structure Setup =
struct

local
    structure S = Symbol
    structure T = Types
    structure BT = BasicTypes
    structure LV = LambdaVar
    structure VC = VarCon
    open Absyn
in
val _ = LV.reset ()

(* bool datacons *)
val T = {name = S.make "T", width = 2}
val F = {name = S.make "F", width = 2}

(* list datacons *)
val Nil = {name = S.make "Nil", width = 2}
val Cons = {name = S.make "Cons", width = 2}

(* datatype t = A | B | C *)
val A = {name = S.make "A", width = 3}
val B = {name = S.make "A", width = 3}
val C = {name = S.make "A", width = 3}

(* datatype tree = L | N of tree * tree *)
val L = {name = S.make "L", width = 2}  (* short for Leaf *)
val N = {name = S.make "N", width = 2}  (* short for Node *)

val truePat = CONpat(T, [])
val falsePat = CONpat(F, [])

(* mkTuplePat : pat list -> pat *)
fun mkTuplePat pats =
    let fun mkFields(pat::pats, n, fields) =
            mkFields(pats, n+1, (T.mkLabel(Int.toString n),pat)::fields)
          | mkFields (nil,_,fields) = rev fields
     in RECORDpat{fields=mkFields(pats,1,nil)}
    end

val pat11 = truePat
val pat12 = falsePat

val pats1 = [pat11, pat12]

val pat21 = mkTuplePat [truePat, falsePat]
val pat22 = mkTuplePat [falsePat, truePat]
				   
val example0 = [pat21, pat22]

val xpat = VARpat(VC.mkVALvar(S.make "x", LV.mkLvar()))
val ypat = VARpat(VC.mkVALvar(S.make "y", LV.mkLvar()))
val zpat = VARpat(VC.mkVALvar(S.make "z", LV.mkLvar()))

val pats0 = [xpat]

(* Example 1 *)
(*
   datatype tree = L | N of tree * tree
   (1) N(x,L)
   (2) N(L,y)
   (3) z
*)
val Lpat = CONpat(L,[])
val p1 = APPpat(N, [], mkTuplePat[xpat, Lpat])
val p2 = APPpat(N, [], mkTuplePat[Lpat, ypat])
val p3 = zpat
val example1 = [p1,p2,p3]

end (* local *)
end (* structure Stuff *)
