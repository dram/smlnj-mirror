(* Copyright 1997 Bell Laboratories *)
(* unify.sml *)

signature UNIFY =
sig

  datatype unifyFail
    = CIRC (* circularity *)
    | EQ (* equality type required *)
    | TYC of Types.tycon * Types.tycon (* tycon mismatch *)
    | TYP of Types.ty * Types.ty (* type mismatch *)
    | LIT of Types.tvKind (* literal *)
    | UBVE of Types.tvKind (* UBOUND, equality mismatch *)
    | UBV of Types.tvKind (* UBOUND match *)
    | SCH (* SCHEME, equality mismatch  *)
    | REC (* record labels *)

  exception Unify of unifyFail
  val failMessage: unifyFail -> string

  val unifyTy : Types.ty * Types.ty -> unit

  val debugging : bool ref

end (* signature UNIFY *)


structure Unify: UNIFY =
struct

(*** type unification ***)
val debugging = ref false

local
  structure T = Types
  structure TU = TypesUtil
  structure OLL = OverloadLit
  structure ED = ElabDebug
  open Types 

  (* debugging *)
  val say = Control.Print.say
  fun debugmsg (msg: string) = if !debugging then (say msg; say "\n") else ()

  fun bug msg = ErrorMsg.impossible("Unify: "^msg)

  val ppType = PPType.ppType StaticEnv.empty
  fun debugPPType (msg,ty) =
      ED.debugPrint debugging (msg, ppType, ty)

in

datatype unifyFail
  = CIRC (* circularity *)
  | EQ (* equality type required *)
  | TYC of Types.tycon * Types.tycon (* tycon mismatch *)
  | TYP of Types.ty * Types.ty (* type mismatch *)
  | LIT of Types.tvKind (* literal *)
  | UBVE of Types.tvKind (* UBOUND, equality mismatch *)
  | UBV of Types.tvKind (* UBOUND match *)
  | SCH (* SCHEME, equality mismatch  *)
  | REC (* record labels *)

fun failMessage failure =
    case failure
      of CIRC => "circularity"
       | EQ => "equality type required"
       | TYC(tyc1,tyc2) => "tycon mismatch"
       | TYP(ty1,ty2) => "type mismatch"
       | LIT(info) => "literal"
       | UBVE(info) => "UBOUND, equality mismatch"
       | UBV(info) => "UBOUND match"
       | SCH => "SCHEME, equality mismatch"
       | REC => "record labels"


exception Unify of unifyFail


(*************** misc functions *****************************************)

val eqLabel = Symbol.eq

fun eqLitKind (lk : T.litKind) =
    case lk of (INT | WORD | CHAR | STRING) => true | REAL => false

(*
 * tyconEqprop tycon:
 *
 *    This function returns the eqprop of tycon for use in determining
 * when a CONty is an equality type.
 *
 * Note: Calling this function on ERRORtyc produces an impossible
 * because an ERRORtyc should never occur in a CONty and hence an eqprop
 * of one of them should never be needed.
 *
 * Calling this function on a DEFtyc also produces an impossible because
 * the current eqprop scheme is insufficiently expressive to describe
 * the possibilities.  (Ex: first argument must be an eq type but not
 * necessarily the second)  Because of this, it is currently necessary to
 * expand DEFtyc's before checking for equality types.
 *)
fun tyconEqprop (GENtyc{eq,...}) =
      (case !eq of ABS => NO | ep => ep)
  | tyconEqprop (RECORDtyc _)  = YES
  | tyconEqprop (DEFtyc _) = bug "tyconEqprop: DEFtyc"
  | tyconEqprop (ERRORtyc) = bug "tyconEqprop: ERRORtyc"
  | tyconEqprop _ = bug "unexpected tycon in tyconEqprop"

(*
 * fieldwise(just1,just2,combine,fields1,fields2):
 *
 *    This function merges two sorted lists of (label, type) pairs
 * (sorted by label) into a single sorted list of (label, type) pairs.
 * If (l1,t1) occurs in fields1 but l1 doesn't occur in fields2 then
 * (l1, just1 t1) occurs in the output.  Similarly with just2.
 * If (l, t1) occurs in fields1 and (l,t2) in fields2, then 
 * (l, combine t1 t2) occurs in the output.
 *)
fun fieldwise(_,just2,_,[],fields2) = map (fn (n,t) => (n,just2 t)) fields2
  | fieldwise(just1,_,_,fields1,[]) = map (fn (n,t) => (n,just1 t)) fields1
  | fieldwise(just1,just2,combine,((n1,t1)::r1),((n2,t2)::r2)) =
      if eqLabel(n1,n2) then
	(n1,combine(t1,t2))::(fieldwise(just1,just2,combine,r1,r2))
      else if TU.gtLabel(n2,n1) then
	(n1,just1 t1)::(fieldwise(just1,just2,combine,r1,((n2,t2)::r2)))
      else
	(n2,just2 t2)::(fieldwise(just1,just2,combine,((n1,t1)::r1),r2))


(*************** adjust function *****************************************)

(* propagate depth and eq while checking for circularities in the
 * type ty that is going to unify with tyvar var *)
fun adjustType (var,depth,eq,ty) =
    let val _ = debugPPType(">>adjustType: ",ty)
	fun iter _ WILDCARDty = ()
	  | iter eq (VARty(var' as ref(info))) =
	      (case info
		 of INSTANTIATED ty => iter eq ty
		  | OPEN{kind=k,depth=d,eq=e} =>
		      (* check for circularity, propagage eq and depth *)
		      if TU.eqTyvar(var,var')
		      then raise Unify CIRC
		      else (case k
			      of FLEX fields =>
				  (* recurse into FLEX field types *)
				  app (fn (l,t) => adjustType(var,depth,e,t))
				      fields
			       | _ => ();
			    var' := OPEN{depth=Int.min(depth,d),
					 eq=eq orelse e, kind=k})
		  | UBOUND{depth=d,eq=e,name} =>
		      (* check if eq is compatible and propagate depth *)
		      if eq andalso not e
		      then raise Unify EQ
		      else if depth < d
		      then var' := UBOUND{depth=depth,eq=e,name=name}
		      else ()
		  | SCHEME eq' =>
		      if TU.eqTyvar(var,var')
		      then raise Unify CIRC
		      else if eq andalso not eq'
		      then var' := SCHEME eq
		      else ()
		  | LITERAL{kind=k,...} =>
		      (* check if eq is compatible *)
		      if eq andalso not(eqLitKind k)
		      then raise Unify EQ
		      else ())
	  | iter eq (ty as CONty(DEFtyc _, args)) =
	      iter eq (TU.headReduceType ty)
 	  | iter eq (CONty(tycon,args)) =
	      (case tyconEqprop tycon
		 of OBJ => app (iter false) args
		  | YES => app (iter eq) args
		  | _ =>
		    if eq then raise Unify EQ
		    else app (iter false) args)
 (* BUG? why don't these cases blow up (in tyconEqprop) when iter is applied
    to arguments that are unreduced applications of DEFtycs? *)
          | iter _ (POLYty _) = bug "adjustType 1"
          | iter _ (IBOUND _) = bug "adjustType 2"
	  | iter _ _ = bug "adjustType 3"
     in iter eq ty
    end

(*************** unify functions *****************************************)

(* LITERAL can be instantiated to a compatible LITERAL or a monotype of
 *   its LITERAL class
 * UBOUND cannot be instantiated, but it's depth property can be reduced
 * FLEX can merge with another FLEX or instantiate a META
 * META can be instantiated to anything
 *)

(* reorder two tyvars in descending order according to the ordering
 * LITERAL > UBOUND > SCHEME > OPEN/FLEX > OPEN/META *)
fun sortVars(v1 as ref i1, v2 as ref i2) =
    case (i1,i2)
      of (LITERAL _, _) => (v1,v2)
       | (_, LITERAL _) => (v2,v1)
       | (UBOUND _, _) => (v1,v2)
       | (_, UBOUND _)=> (v2,v1)
       | (SCHEME _, _) => (v1,v2)
       | (_, SCHEME _)=> (v2,v1)
       | (OPEN{kind=FLEX _,...}, _) => (v1,v2)
       | (_, OPEN{kind=FLEX _,...})=> (v2,v1)
       | _ => (v1,v2) (* both OPEN/META *)

fun unifyTy(type1,type2) =
    let val type1 = TU.prune type1
	val type2 = TU.prune type2
	val _ = debugPPType(">>unifyTy: type1: ",type1)
	val _ = debugPPType(">>unifyTy: type2: ",type2)
     in case (TU.headReduceType type1, TU.headReduceType type2)
	  of (VARty var1,VARty var2) =>
	       unifyTyvars(var1,var2)  (* used to take type1 and type2 as args *)
	   | (VARty var1,etype2) => (* etype2 may be WILDCARDty *)
	       instTyvar(var1,type2,etype2)
	   | (etype1,VARty var2) => (* etype1 may be WILDCARDty *)
	       instTyvar(var2,type1,etype1)
	   | (CONty(tycon1,args1),CONty(tycon2,args2)) =>
	       if TU.eqTycon(tycon1,tycon2) then
		   ListPair.app unifyTy (args1,args2)
	       else raise Unify (TYC(tycon1,tycon2))
	  (* if one of the types is WILDCARDty, propagate it down into the
	   * other type to eliminate tyvars that might otherwise cause
	   * generalizeTy to complain. *)
	   | (WILDCARDty, CONty(_, args2)) => 
               (app (fn x => unifyTy(x, WILDCARDty)) args2)
           | (CONty(_, args1), WILDCARDty) =>
               (app (fn x => unifyTy(x, WILDCARDty)) args1)
	   | (WILDCARDty,_) => ()
	   | (_,WILDCARDty) => ()
	   | tys => raise Unify (TYP tys)
    end

and unifyTyvars (var1, var2) =
    let fun unify(var1 as ref i1, var2 as ref i2) =
	    (* ASSERT: var1 <> var2 *)
	    case i1
	      of LITERAL{kind,region} =>
		  (case i2
		     of LITERAL{kind=kind',...} =>
			 if kind = kind'
			 then var2 := INSTANTIATED (VARty var1)
			 else raise Unify (LIT i1)
		      | (OPEN{kind=META,eq=e2,...} | SCHEME e2)=>
			 (* check eq compatibility *)
			 if not e2 orelse eqLitKind kind
			 then var2 := INSTANTIATED (VARty var1)
			 else raise Unify (LIT i1)
		      | _ => raise Unify (LIT i1))

	       | UBOUND {depth=d1,eq=e1,name} =>
		  (case i2
		     of OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then (if d2 < d1
				   then var1 := UBOUND{depth=d2,eq=e1,name=name}
				   else ();
			       var2 := INSTANTIATED (VARty var1))
			 else raise Unify (UBV i1)
		      | _ => raise Unify (UBV i1))

	       | SCHEME e1 =>
		  (case i2
		     of SCHEME e2 =>
			 if e1 orelse not e2 then var2 := INSTANTIATED (VARty var1)
			 else var1 := INSTANTIATED(VARty var2)
		      | OPEN{kind=META,eq=e2,depth=d2} =>
			 if e1 orelse (not e2)
			 then var2 := INSTANTIATED (VARty var1)
		         else (var1 := SCHEME e2;
			       var2 := INSTANTIATED (VARty var1))
		      | _ => raise Unify SCH)

	       | OPEN{kind=k1 as FLEX f1,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=k2,eq=e2,depth=d2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in case k2
			       of FLEX f2 =>
				   (app (fn (l,t) => adjustType(var1,d,e,t)) f2;
				    app (fn (l,t) => adjustType(var2,d,e,t)) f1;
				    var1 :=
				      OPEN{depth=d, eq=e,
					   kind=FLEX(merge_fields(true,true,f1,f2))};
				    var2 := INSTANTIATED(VARty var1))
			        | META =>
				   (app (fn (l,t) => adjustType(var2,d,e,t)) f1;
				    var1 := OPEN{kind=k1,depth=d,eq=e};
				    var2 := INSTANTIATED(VARty var1))
			 end
		      | _ => bug "unifyTyvars 2")
			 
	       | OPEN{kind=META,depth=d1,eq=e1} =>
		  (case i2
		     of OPEN{kind=META,depth=d2,eq=e2} =>
			 let val d = Int.min(d1,d2)
			     val e = e1 orelse e2
			  in var1 := OPEN{kind=META,depth=d,eq=e};
			     var2 := INSTANTIATED(VARty var1)
			 end
		      | _ => bug "unifyTyvars 3")

	       | _ => bug "unifyTyvars 4"
val _ = debugmsg ">>unifyTyvars"			 
     in if TU.eqTyvar(var1,var2) then ()
        else unify(sortVars(var1,var2))
    end

and instTyvar (var as ref(OPEN{kind=META,depth,eq}),ty,ety) =
      (case ety
         of WILDCARDty => ()
	  | _ => adjustType(var,depth,eq,ety);
       var := INSTANTIATED ty)

  | instTyvar (var as ref(OPEN{kind=FLEX fields,depth,eq}),ty,ety) =
      (case ety
	 of CONty(RECORDtyc field_names, field_types) =>
	      let val record_fields = ListPair.zip (field_names,field_types)
	       in app (fn t => adjustType(var,depth,eq,t)) field_types;
		  merge_fields(false,true,fields,record_fields);
		  var := INSTANTIATED ty
	      end
          | WILDCARDty => (* propagate WILDCARDty to the fields *)
	      (app (fn (lab,ty) => unifyTy(WILDCARDty,ty)) fields)
          | _ => raise Unify (TYP(VARty(var), ety)))

  | instTyvar (var as ref(i as SCHEME eq),ty,ety) =
      (adjustType(var,infinity,eq,ety);
       var := INSTANTIATED ty)

  | instTyvar (var as ref(i as LITERAL{kind,...}),ty,ety) =
      (case ety
	 of WILDCARDty => ()
	  | _ => 
	     if OLL.isLiteralTy(kind,ety)
	     then var := INSTANTIATED ty
	     else raise Unify (LIT i))   (* could return the ty for error msg*)

  | instTyvar (ref(i as UBOUND _),_,ety) =
      (case ety
         of WILDCARDty => ()
          | _ =>  raise Unify (UBV i))   (* could return the ty for error msg*)

  | instTyvar (ref(INSTANTIATED _),_,_) = bug "instTyvar: INSTANTIATED"
  | instTyvar (ref(LBOUND _),_,_) = bug "instTyvar: LBOUND"

(*
 * merge_fields(extra1,extra2,fields1,fields2):
 *
 *    This function merges the 2 sorted field lists.  Fields occuring
 * in both lists have their types unified.  If a field occurs in only
 * one list, say fields{i} then if extra{i} is true, an Unify error
 * is raised.
 *)
and merge_fields(extra1,extra2,fields1,fields2) =
    let fun extra allowed t =
	if not allowed
	then raise Unify REC
	else t
     in fieldwise(extra extra1, extra extra2, 
                  (fn (t1,t2) => (unifyTy(t1,t2); t1)),
		  fields1, fields2)
    end

end (* local *)
end (* structure Unify *)

(*
 * $Log: unify.sml,v $
 * Revision 1.10  1997/12/02 05:29:34  dbm
 *   Fix for bug 1316.  Added an occurrence check when instantiating a
 *   SCHEME type variable.
 *
 * Revision 1.9  1997/09/15  15:56:08  dbm
 *   Changed two instances of "ety" to "ty" in instTyvar to fix problem
 *   of "?." appearing in non-exhaustive match warning messages.
 *
 * Revision 1.8  1997/04/18  15:48:16  george
 *   Cosmetic changes on some constructor names. Changed the shape for
 *   FIX type to potentially support shared dtsig. -- zsh
 *
 * Revision 1.7  1997/04/14  21:27:28  dbm
 *   Eliminated redundant rule in instTyvar.
 *
 * Revision 1.6  1997/04/10  14:33:00  dbm
 *   Fix for bug 1187.  Modified instTyvar FLEX case to propagate depth and
 *   eq.
 *
 * Revision 1.5  1997/04/02  03:53:23  dbm
 * Further simplification and fix for bug 1160.
 *
 * Revision 1.4  1997/03/22  18:02:38  dbm
 * Major rewrite for better handling of literal overloading and to
 * fix bug 905/952.  Varieties of type variables have multiplied for
 * more exact treatment of instantiation and generalization.
 *
 * Revision 1.3  1997/02/26  21:47:47  george
 *    Improve the type-checking error messages; fix BUG 1149, 1123
 *    reported by Allen Stoughton.
 *
 *)



