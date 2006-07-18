(* primopid.sml
 *
 * (C) 2001 Lucent Technologies, Bell Labs
 *)

(* [dbm, 6/19/06]
     Folded ii.sml into this structure, eliminating exn hack.
     Changed name of pureInfo to isPrimCast.
     Eliminated redundant INL_PRIM, INL_STR, INL_NO. *)

structure PrimOpId : PRIMOPID = 
struct

  (* in the front end, primops are identified by a primop number *)
  datatype primId = Prim of string | NonPrim

  datatype strPrimElem = PrimE of primId
                       | StrE of strPrimInfo

  withtype strPrimInfo = strPrimElem list

  fun bug s = ErrorMsg.impossible ("PrimOpId: " ^ s)

  fun isPrimop (Prim _) = true
    | isPrimop NonPrim  = false

  (* Used in TopLevel/main/compile.sml *)
  fun isPrimCallcc (Prim("callcc" | "capture")) = true
    | isPrimCallcc _ = false

  (* Used in ElabData/modules/moduleutil.sml *)
  fun isPrimCast (Prim "cast") = true
    | isPrimCast _ = false

  (* Select the prim ids for a substructure *)
  fun selStrPrimId(elems, slot) = 
      (case List.nth(elems, slot) 
	of StrE elems' => elems'
	 | PrimE _ => bug "PrimOpId.selStrPrimId: unexpected PrimE")
      handle Subscript => (bug "PrimOpId.selStrPrimId Subscript")
	(* This bug happens if we got a primid for a value 
	   component when we expected a strPrimElem for a 
	   structure *)

  (* Select the prim id for a value component *)
  fun selValPrimFromStrPrim(elems, slot) =
      (case List.nth(elems, slot)
	of PrimE(id) => id
	 | _ => 
	   bug "PrimOpId.selValPrimFromStrPrim: unexpected StrE"
      ) handle Subscript => bug "PrimOpId.selValPrimFromStrPrim Subscript"
           (* This bug occurs if we got a substructure's
	      strPrimElem instead of an expected value component's
	      primId *)

  fun ppPrim NonPrim = print "<NonPrim>"
    | ppPrim (Prim p) = print ("<PrimE "^p^">")

  fun ppStrInfo strelems = 
      let fun ppElem [] = ()
	    | ppElem ((PrimE p)::xs) = (ppPrim p; ppElem xs)
	    | ppElem ((StrE s)::xs) = (ppStrInfo s; ppElem xs)
      in (print "[ "; ppElem strelems; print " ]\n")
      end
(* 
    fun selStrInfo (StrE l, i) =
	(List.nth (l, i) handle Subscript => bug "Wrong field in List")
      | selStrInfo (Null, _) = Null
      | selStrInfo (Info _, i) = bug "Unexpected selection from Info"
   

    fun match i { inl_prim, inl_str, inl_no } =
	case i
	  of Info x => inl_prim x
	   | List l => inl_str l
	   | Null => inl_no ()

    fun prInfo i = let
	fun loop (i, acc) =
	    case i
              of Info (p,_) => PrimOp.prPrimop p :: acc
	       | Null => "<InlNo>" :: acc
	       | List m => 
                 (case m
                   of [] => "{}" :: acc
		    | h::t =>
		      "{" :: loop (h,foldr (fn (x, a) => "," :: loop (x, a))
                                           ("}" :: acc)
					   t))
    in
	concat (loop (i, []))
    end

    fun isPrimCallcc (Info ((PrimOp.CALLCC | PrimOp.CAPTURE), _)) = true
      | isPrimCallcc _ = false

    fun isPrimCast (Info (PrimOp.CAST, _)) = true
      | isPrimCast _ = false

    val mkPrimInfo = Info
    val mkStrInfo = List
    val nullInfo = Null

    fun primopTy (Info (_, ty)) = SOME ty
      | primopTy _ = NONE
 *)
end (* structure InlInfo *)
