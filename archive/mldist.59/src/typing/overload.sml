(* Copyright 1989 by AT&T Bell Laboratories *)
(* overload.sml *)
structure Overload : OVERLOAD = struct

open Basics TypesUtil Unify PrintUtil ErrorMsg BasicTypes PrintType

type subst = (tyvar * tvkind) list

exception SoftUnify

local val defaultMETA = META{depth=0,weakness=infinity,eq=false}
      fun typeArgs n = 
	    if n>0
	    then VARty(mkTyvar defaultMETA) :: typeArgs(n-1)
	    else []
 in fun copyScheme (tyfun as TYFUN{arity,...}) : ty * ty =
    let val tvs = typeArgs arity
     in (applyTyfun(tyfun,tvs),
	 if arity>1 then tupleTy tvs else hd tvs)
    end
end

fun rollBack subst =
    let fun loop (nil,trace) = trace
	  | loop (((tv as ref kind),oldkind)::subst,trace) =
	       (tv := oldkind;
		loop(subst,(tv,kind)::trace))
     in loop(subst,nil)
    end

fun redoSubst nil = ()
  | redoSubst ((tv,INSTANTIATED ty)::rest) =
      (instantiate(tv, ty); redoSubst rest)
  | redoSubst (_) = impossible "Overload--redoSubst"

fun softUnify(ty1: ty, ty2: ty): subst =
    let val subst: subst ref = ref nil
	fun softInst(tv as ref kind: tyvar, ty: ty) : unit =
	    let fun scan(ty: ty) : unit =  (* simple occurrence check *)
		   case ty
		     of VARty(tv') => 
		          if eqTyvar(tv, tv') then raise SoftUnify else ()
		      | CONty(_, args) => app scan args
		      | FLEXRECORDty(ref(OPEN fields)) =>
		          app (fn (_,ty') => scan ty') fields
		      | ty => ()  (* propagate error *)
	     in case kind
		  of META _ => ()
		   | _ => raise SoftUnify;
 	        scan ty;
		subst := (tv, kind)::(!subst);
		tv := INSTANTIATED ty
	    end
	
	fun unify(ty1: ty, ty2: ty): unit =
	    let val ty1 = prune ty1
		and ty2 = prune ty2
	     in case (ty1,ty2)
		  of (VARty(tv1),VARty(tv2)) =>
		       if eqTyvar(tv1,tv2) then () else softInst(tv1,ty2)
		   | (VARty(tv1),_) => softInst(tv1,ty2)
		   | (_,VARty(tv2)) => softInst(tv2,ty1)
		   | (CONty(tycon1, args1), CONty(tycon2, args2)) =>
		       if eqTycon(tycon1, tycon2)
		       then unifyLists(args1, args2)
		       else (unify(reduceType ty1, ty2)
			     handle ReduceType => 
			       unify(ty1, reduceType ty2)
			       handle ReduceType => raise SoftUnify)
		   | (ERRORty, _) => ()  (* propagate error *)
		   | (_, ERRORty) => ()  (* propagate error *)
		   | _ => raise SoftUnify
	    end
	
	and unifyLists([],[]) = ()
	  | unifyLists(ty1::rest1, ty2::rest2) = 
	      (unify(ty1,ty2); unifyLists(rest1,rest2))
	  | unifyLists(_) = raise SoftUnify

     in unify(ty1,ty2)
	  handle SoftUnify => (rollBack(!subst); raise SoftUnify);
	!subst
    end

exception Overld

val overloaded = ref (nil: (var ref * ErrorMsg.complainer * ty) list)

fun resetOverloaded () = overloaded := nil

fun pushOverloaded (refvar as ref(OVLDvar{options,scheme,...}), err) = 
	   let val (scheme',ty) = copyScheme(scheme)
	    in overloaded := (refvar,err,ty) :: !overloaded;
	       scheme'
	   end
  | pushOverloaded _ = impossible "overload.1"

fun resolveOverloaded() =
 let fun resolveOVLDvar(rv as ref(OVLDvar{name,options,...}),err,context) =
	(let fun findFirst({indicator, variant}::rest) =
		 ((softUnify(applyPoly(indicator,0,infinity), context), variant, rest)
		   handle SoftUnify => findFirst(rest))
	       | findFirst(nil) = 
		   (err COMPLAIN("overloaded variable \"" ^ Symbol.name(name) ^
			     "\" not defined at type:");
		    print "   ";
		    resetPrintType(); printType(context); newline();
		    raise Overld)
	     fun findSecond({indicator, variant}::rest) =
		 ((rollBack(softUnify(applyPoly(indicator,0,infinity), context));
		   err COMPLAIN("overloaded variable \"" ^ Symbol.name(name) ^
			     "\" cannot be resolved");
		   raise Overld)
		  handle SoftUnify => findSecond(rest))
	       | findSecond(nil) = ()
	     val (subst,var,restOptions) = findFirst(!options)
	     val subst = rollBack subst
	  in findSecond(restOptions);
	     redoSubst subst;
	     rv := var
	 end handle Overld => ())
       | resolveOVLDvar _ = impossible "overload.2"

  in app resolveOVLDvar (!overloaded); 
     overloaded := nil
 end

end (* structure Overload *)
