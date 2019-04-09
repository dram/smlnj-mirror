(* Copyright 1996 by Bell Laboratories *)
(* lambdaopt.sml *)

signature LAMBDAOPT = 
sig
  val lambdaopt: Lambda.lexp -> Lambda.lexp 
end 

structure LambdaOpt : LAMBDAOPT = 
struct

local open Access Lambda
in

fun click a = if !Control.CG.debugcps then Control.Print.say a else ()

val sameName = LambdaVar.sameName
fun bug s = ErrorMsg.impossible ("LambdaOpt: "^s)
val ident = fn le => le
    		
fun endpath(PATH(p,_)) = endpath p
  | endpath(LVAR v) = v
  | endpath _ = bug "unexpected path in endpath"

(*** temporarily I am letting it call the lcontract code instead (ZHONG) ***)

fun lambdaopt lexp = LContract.lcontract lexp

end (* toplevel local *)
end (* structure LambdaOpt *)

