(* key.sml *)

structure Key =
struct

(* keys: most keys (D,V,I,W,C,S) are used to discriminate choices in the "variants"
 *   field of OR nodes and the decVariants of decision trees. These key values
 *   (appearing in variants) determine the different flavors of OR nodes
 *   (data, vector length, and 4 varieties of constants).
 *   There is an extra R (for record) key representing record/product selection.
 *   R keys appear only in paths to indicate product projections. *)

local
  structure T = Types
  structure TU = TypesUtil
in

  datatype key
    = D of T.datacon * T.tyvar list
       (* datacon key, possibly constant, with instantiation tyvars *)
    | V of int              (* vector length; ASSERT int >= 0 *)

    (* following constant keys supercede previous constCon constructors *)
    | I of T.ty IntConst.t  (* int constant, as determined by ty *)
    | W of T.ty IntConst.t  (* word constant, as determined by ty *)
    | C of char             (* char constant (span depends on charwidth) *)
    | S of string           (* string constant *)

    (* record selection: not a choice discriminator, but a selection key for products,
     * Will only appear in paths, never in variants. ASSERT: int >= 0 *)
    | R of int

  (* eqKey : key * key -> bool
   * type info disregarded when comparing Dkeys and Vkeys *)
  fun eqKey (D (dcon1,_), D (dcon2,_)) = TU.eqDatacon(dcon1,dcon2)
	(* we can ignore type instantiation tyvars, which are guaranteed to be 
	 * equivalent at the same pattern node. *)
    | eqKey (V l1, V l2) = l1 = l2
    | eqKey (I c1, I c2) = IntConst.same(c1,c2)
    | eqKey (W c1, W c2) = IntConst.same(c1,c2)
    | eqKey (C c1, C c2) = c1 = c2  (* character keys *)
    | eqKey (S s1, S s2) = s1 = s2
    | eqKey (R i1, R i2) = i1 = i2
    | eqKey _ = false  (* mismatching key constructors *)

  fun keyToString (D (dcon,_)) = Symbol.name(TU.dataconName dcon)
    | keyToString (V n) = "V"^(Int.toString n)
    | keyToString (I{ival,ty}) = "I"^(IntInf.toString ival)
    | keyToString (W{ival,ty}) = "W"^(IntInf.toString ival)
    | keyToString (C c) = "C"^(Char.toString c)
    | keyToString (S s) = "S["^ s ^ "]"
    | keyToString (R i) = Int.toString i

end (* local *)
end (* structure Key *)				     
