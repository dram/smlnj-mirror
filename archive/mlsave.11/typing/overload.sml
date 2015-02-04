(* overload.sml *)

structure Overload : OVERLOAD = struct

structure Basics = Basics

local open Basics TypesUtil PrintUtil ErrorMsg BasicTypes
 in

type subst = (tyvar * tvstatus) list

exception SoftUnify

fun freshTyList (tys : ty list) =
    let val copyenv = ref nil : (tyvar * tyvar) list ref
	fun freshTyvar(tv: tyvar) : tyvar = 
	    let fun search(nil) : tyvar = 
		    let val newTv = newTyvar(METAARG)
		     in copyenv := (tv, newTv)::(!copyenv);
			newTv
		    end
		  | search((oldTv,newTv)::rest) =
		    if eqTyvar(tv, oldTv)
		       then newTv
		       else search(rest)
	     in search(!copyenv)
	    end
        fun fresh(ty) =  (* simple copy everything version *)
	    let val ty = prune ty
	     in case ty
		  of VARty(tv as TYVAR{status = ref BOUND,...}) =>
		       VARty(freshTyvar(tv))
		   | VARty _ => ty
		   | CONty(tyconref,args) =>
		       CONty(tyconref, map fresh args)
		   | UNKNOWNty => UNKNOWNty  (* propagate error *)
		   | _ => impossible "Overload:frestTyList"
	    end
     in map fresh tys
    end

fun copyType (ty : ty) : ty * ty list =
    let val copyenv = ref nil : (tyvar * tyvar) list ref
	fun copyTyvar(tv: tyvar) : tyvar = 
	    let fun search(nil) : tyvar = 
		    let val newTv = newTyvar(METAARG)
		     in copyenv := (tv, newTv)::(!copyenv);
			newTv
		    end
		  | search((oldTv,newTv)::rest) =
		    if eqTyvar(tv, oldTv)
		       then newTv
		       else search(rest)
	     in search(!copyenv)
	    end
        fun copy(ty) =  (* simple copy everything version *)
	    let val ty = prune ty
	     in case ty
		  of VARty(tv as TYVAR{status = ref BOUND,...}) =>
		       VARty(copyTyvar(tv))
		   | VARty _ => ty
		   | CONty(tyconref,args) =>
		       CONty(tyconref, map copy args)
		   | UNKNOWNty => UNKNOWNty  (* propagate error *)
		   | _ => impossible "Overload-copyType"
	    end
     in (copy ty, rev(map (fn (_,tv) => VARty tv) (!copyenv)))
    end

fun rollBack subst =
    let fun loop (nil,trace) = trace
	  | loop (((tv as TYVAR{status = refstatus,...}),oldstatus)::subst,trace) =
	      let val savedstatus = !refstatus
	       in refstatus := oldstatus;
		  loop(subst,(tv,savedstatus)::trace)
	      end
     in loop(subst,nil)
    end

fun redoSubst nil = ()
  | redoSubst ((tv,INSTANTIATED ty)::rest) =
      (instantiate(tv, ty); redoSubst rest)
  | redoSubst (_) = impossible "Overload--redoSubst"

fun softUnifyList(tys1: ty list, tys2: ty list): subst =
    let val subst: subst ref = ref nil
	fun softInst(tv as TYVAR{status = refstatus,...}: tyvar, ty: ty) : unit =
	    let fun scan(ty: ty) : unit =  (* simple occurrence check *)
		   case ty
		     of VARty(tv') => 
		          if eqTyvar(tv, tv') then raise SoftUnify else ()
		      | CONty(_, args) => app scan args
		      | UNKNOWNty => ()  (* propagate error *)
		      | FLEXRECORDty{fields,...} =>
		          app (fn (_,ty') => scan ty') fields
	     in case !refstatus
		  of METAARG => 
		       (scan ty;
			subst := (tv, !refstatus)::(!subst);
			refstatus := INSTANTIATED ty)
		   | METALAM _ =>
		       (scan ty;
			subst := (tv, !refstatus)::(!subst);
			refstatus := INSTANTIATED ty)
		   | _ => raise SoftUnify
	    end
	
	fun unify(ty1: ty, ty2: ty): unit =
	    let val ty1 = prune ty1
		and ty2 = prune ty2
	     in case (ty1,ty2)
		  of (VARty(tv1),VARty(tv2)) =>
		       if eqTyvar(tv1,tv2) then () else softInst(tv1,ty2)
		   | (VARty(tv1),_) => softInst(tv1,ty2)
		   | (_,VARty(tv2)) => softInst(tv2,ty1)
		   | (CONty(ref tycon1, args1), CONty(ref tycon2, args2)) =>
		       if eqTycon(tycon1, tycon2)
		       then unifyLists(args1, args2)
		       else (case (tycon1,tycon2)
			      of (TYPEtyc _, _) => unify(expandTy ty1, ty2)
			       | (_, TYPEtyc _) => unify(ty1, expandTy ty2)
			       | _ => raise SoftUnify)
		   | (UNKNOWNty, _) => ()  (* propagate error *)
		   | (_, UNKNOWNty) => ()  (* propagate error *)
		   | _ => raise SoftUnify
	    end
	
	and unifyLists([],[]) = ()
	  | unifyLists(ty1::rest1, ty2::rest2) = 
	      (unify(ty1,ty2); unifyLists(rest1,rest2))
	  | unifyLists(_) = raise SoftUnify

     in unifyLists(tys1,tys2)
	  handle SoftUnify => (rollBack(!subst); raise SoftUnify);
	!subst
    end


exception Overld

datatype stack
  = EMPTY
  | PUSH of var ref * ty list * stack
  | MARK of stack

val overloaded = ref EMPTY  (* stack of currently unresolved overloaded vars *)

fun resetOverloaded () = overloaded := EMPTY

fun markOverloaded () = overloaded := MARK(!overloaded)

fun pushOverloaded (refvar as ref(OVLDvar{options,scheme,...}) : var ref) = 
	   let val (scheme',tvs) = copyType(scheme)
	    in overloaded := PUSH(refvar,tvs,!overloaded);
	       scheme'
	   end
  | pushOverloaded _ = impossible "overload.1"

fun resolveOVLDvar(rv as ref(OVLDvar{name,options,...}),context) =
   (let fun findFirst({indicators, variant}::rest) =
	    ((softUnifyList(freshTyList indicators, context), variant, rest)
	      handle SoftUnify => findFirst(rest))
	  | findFirst(nil) = 
	    (complain "type error: no match for overloaded variable:";
	     printSym name; newline();
	     raise Overld)
	fun findSecond({indicators, variant}::rest) =
	    ((softUnifyList(freshTyList indicators, context);
	      complain "type error: multiple matches for overloaded variable:";
	      printSym name; newline(); raise Overld)
	     handle SoftUnify => findSecond(rest))
	  | findSecond(nil) = ()
        val (subst,var,restOptions) = findFirst(!options)
	val subst = rollBack subst
     in findSecond(restOptions);
	redoSubst subst;
	rv := var
    end handle Overld => ())
  | resolveOVLDvar _ = impossible "overload.2"

fun resolveOverloaded () =
    let fun loop EMPTY = ()
	  | loop(MARK(s)) = (overloaded := s)
	  | loop(PUSH(refvar,context,s)) = 
	      (resolveOVLDvar(refvar,context); loop(s))
     in loop(!overloaded)
    end

end (* local *)

end (* structure Overload *)
