(* setup.sml *)

(* test rig for testing and debugging the match compiler *)
(* some test cases *)

structure Setup =
struct

local
    structure S = Symbol
    structure T = Types
    structure TU = TypesUtil
    structure BT = BasicTypes
    structure LV = LambdaVar
    structure V = Var
    open Types Absyn
in

val reset = LV.reset

(* bool datacons *)
val T = BT.trueDcon
val F = BT.falseDcon

(* list datacons *)
val Nil = BT.nilDcon
val Cons = BT.consDcon

(* datatype t = A | B | C *)
val (tTycon,tDconsRef) = TU.mkDataTycon("t",0)
val tTy = POLY{arity = 0, body = CONty(tTycon, nil)}
val A = DCON{name = "A", stamp=Stamp.new(), owner=tTycon,polyty = tTy}
val B = DCON{name = "B", stamp=Stamp.new(), owner=tTycon,polyty = tTy}
val C = DCON{name = "C", stamp=Stamp.new(), owner=tTycon,polyty = tTy}
val _ = tDconsRef := [A,B,C]
val tTy = CONty(tTycon,[])

(* datatype tree = L | N of tree * tree *)
val (treeTycon,treeDconsRef) = TU.mkDataTycon("tree",0)
val Lty = POLY{arity = 0, body = CONty(treeTycon, nil)}
val Nty = POLY{arity = 0, body = CONty(TU.funTycon,
				       [CONty(TU.tupleTycon 2,
					      [CONty(treeTycon, nil),
					       CONty(treeTycon, nil)]),
					CONty(treeTycon, nil)])}
val L = DCON{name = "L", stamp=Stamp.new(), owner=tTycon, polyty = Lty}
val N = DCON{name = "N", stamp=Stamp.new(), owner=tTycon, polyty = Nty}
val _ = treeDconsRef := [L,N]

val dcons = [T,F,Nil,Cons,A,B,C,L,N]

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

val xpat = VARpat(V.mkVALvar(S.make "x", LV.mkLvar()))
val ypat = VARpat(V.mkVALvar(S.make "y", LV.mkLvar()))
val zpat = VARpat(V.mkVALvar(S.make "z", LV.mkLvar()))

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
val example0 = [p1,p2,p3]

val example1 =
"N(x,L); N(L,y); z"    
val polyty1 = POLY{arity=0, body=CONty(treeTycon, [])}

val example2 =
"(T, F, T);\
\(T, x, F);\
\(F, T, y)"
val polyty2 = POLY{arity=0, body=TU.mkTupleTy [BT.boolTy, BT.boolTy, BT.boolTy]}

val example3 =
"(A, F, T);\
\(B, x, F);\
\(z, T, y)"
val polyty3 = POLY{arity=0, body=TU.mkTupleTy [tTy, BT.boolTy, BT.boolTy]}

val example4 =
"(A, F, T);\
\(B, x, F);\
\(z, T, F)"

end (* local *)
end (* structure Stuff *)
