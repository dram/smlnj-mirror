(* print.sml *)

signature PRINT =
sig
  val printType : Syntax.ty -> unit
  val printVal : Eval.value -> unit
  val printStmt : Syntax.ty * Eval.stmtValue -> unit
end (* signature PRINT *)


structure Print : PRINT =
struct

  structure S = Syntax
  structure V = Eval

  (* printType : S.ty -> unit *)
  fun printType ty =
      case ty
	of S.UNITty => print "Unit"
	 | S.INTty => print "Int"
	 | S.BOOLty => print "Bool"
         | S.PRODty(t1,t2) => 
	   (print "("; printType t1; print " * "; printType t2; print ")")
         | S.SUMty(t1,t2) => 
	   (print "("; printType t1; print " + "; printType t2; print ")")
         | S.FUNty(t1,t2) => 
	   (print "("; printType t1; print " -> "; printType t2; print ")")
         | S.VARty v => print v
         | S.RECty(tv,ty) =>
	   (print "Rec["; print tv; print "]"; printType ty)
         | S.POLYty(tv,ty) =>
	   (print "All["; print tv; print "]"; printType ty)
         | S.ERRORty => print "ERRORty"

  (* printVal : V.value -> unit *)
  fun printVal (V.UNIT) = print "Unit"
    | printVal (V.NUM n) =
      print (Int.toString n)
    | printVal (V.BOOL b) =
      print (Bool.toString b)
    | printVal (V.FUN _) =
      print ("<function>")
    | printVal (V.FUNrec _) =
      print ("<function>")
    | printVal (V.TFUN _) =
      print ("<polyfunction>")
    | printVal (V.PRIM _) =
      print ("<prim function>")
    | printVal (V.PAIR(v1,v2)) =
      (print "("; printVal v1; print ","; printVal v2; print ")")
    | printVal (V.INL(v)) =
      (print "INL("; printVal v; print ")")
    | printVal (V.INR(v)) =
      (print "INR("; printVal v; print ")")
    | printVal (V.FOLD v) =
      (print "FOLD("; printVal v; print ")")
    | printVal (V.ERROR) = print "Error"

  (* printStmt : S.ty * V.stmtValue -> unit *)
  fun printStmt (ty, V.ExprVal v) =
      (print ">> "; printVal v; print " : "; printType ty; print "\n")
    | printStmt (ty, V.DeclVal(var,v)) =
      (print (">> "^var^" = ");
       printVal v; print " : "; printType ty; print "\n")

end (* structure Print *)
