(* Copyright 1990 by AT&T Bell Laboratories *)

structure Unify: UNIFY =
struct

(*** type unification ***)

open Types ErrorMsg TypesUtil

type labeledTy = label * ty

val eqLabel = Symbol.eq
fun ltLabel(l1,l2) = Symbol.name l1 < Symbol.name l2

exception Unify of string

fun instantiate(tv as ref(META{depth=d,weakness=w,eq=e}): tyvar, ty: ty) : unit =
    let fun scan (e:bool) (ty: ty) : unit =
	    case ty  (* "pruning" done in-line *)
	      of VARty(ref(INSTANTIATED ty')) => scan e ty'
	       | FLEXRECORDty(ref(CLOSED ty')) => scan e ty'
	       | VARty(tv') => 
		 if eqTyvar(tv, tv')
		 then raise Unify("circularity")
		 else (case !tv'
			of META{depth=d',weakness=w',eq=e'} =>
			     tv' := META{depth = min(d,d'),
					 weakness = min(w,w'),	
					 eq = e orelse e'}
			 | UBOUND{name,depth=d',weakness=w',eq=e'} =>
			     if w < w'
			     then raise Unify("weakness violation")
			     else if e andalso not e'
			     then raise Unify "equality type required"
			     else if d >= d'
			     then ()
			     else tv' := UBOUND{name=name,
						depth=d,
						weakness=w',
						eq=e'}
			 | _ => ())
	       | CONty(DEFtyc{tyfun,...}, args) =>
		   scan e (applyTyfun(tyfun,args))
	       | CONty(GENtyc{eq,path,...}, args) =>
		   if e then
		     (case !eq
		       of OBJ => app (scan false) args
			| YES => app (scan e) args
			| NO =>  raise Unify "equality type required"
			| IND => raise Unify "equality type required"
			| DATA => (PrintUtil.prSymPath path; print "\n";
				   impossible "instantiate -- DATA")
			| UNDEF => (PrintUtil.prSymPath path; print "\n";
				    impossible "instantiate -- UNDEF"))
		   else app (scan e) args
	       | CONty(_, args) => app (scan e) args
	       | FLEXRECORDty(r as ref(OPEN(fields,w'))) =>
		   if e then raise Unify "equality type required"
		   else (app (fn (_,ty') => scan e ty') fields;
			 if w < w' then r := OPEN(fields,w) else ())
	       | _ => ()   (* propagate error *)
     in scan e ty;
	tv := INSTANTIATED ty
    end
  | instantiate _ = impossible "instantiate -- nonMETA type variable"

datatype weakening
  = FST of int
  | SND of int
  | NEITHER

fun weaken(ty,s) =
    let val tv = ref(META{depth=infinity,weakness=s,eq=false})
     in instantiate(tv,ty);
        case !tv
	  of INSTANTIATED ty' => ty'
	   | _ => impossible "weaken"
    end

fun unifyFields(labtys1: labeledTy list, labtys2: labeledTy list,
		weakening: weakening): labeledTy list =
    case (labtys1, labtys2)
      of ([],_) => 
	   (case weakening
	      of SND s => 
		  map (fn (l,t) => (l, weaken(t,s))) labtys2
	       | _ => labtys2)
       | (_,[]) =>
	   (case weakening
	      of FST s => 
		  map (fn (l,t) => (l, weaken(t,s))) labtys1
	       | _ => labtys1)
       | ((labty1 as (lab1,ty1))::labtys1',
	  (labty2 as (lab2,ty2))::labtys2') =>
	    if eqLabel(lab1,lab2)
	    then (unifyTy(ty1,ty2);  (* type error possible *)
		  labty1 :: unifyFields(labtys1',labtys2',weakening))
	    else if ltLabel(lab1, lab2)
	    then (case weakening
		    of FST s => 
			let val (lab1,ty1) = labty1 
			 in (lab1,weaken(ty1,s))
			end
                     | _ => labty1)
		 :: unifyFields(labtys1', labtys2, weakening)
	    else (case weakening
		    of SND s => 
			let val (lab2,ty2) = labty2
			 in (lab2,weaken(ty2,s))
			end
                     | _ => labty2)
		 :: unifyFields(labtys1, labtys2', weakening)

and matchFields(fields: labeledTy list, labels: label list,
		argTys: ty list, weakness: int) =
    case (fields, labels, argTys)
      of ([],_,_) =>
	   if weakness < infinity
	   then app (fn ty => weaken(ty,weakness)) argTys
	   else ()
       | (_,[],_) => raise Unify("record length")
       | ((lab1,ty1)::fields', lab2::labels', ty2::argTys') =>
	   if eqLabel(lab1,lab2)
	   then (unifyTy(ty1,ty2);
		 matchFields(fields',labels',argTys',weakness))
	   else if ltLabel(lab2,lab1)
	   then (if weakness < infinity 
		 then (weaken(ty2,weakness); ()) else ();
		 matchFields(fields, labels', argTys',weakness))
	   else raise Unify("record labels")
       | _ => impossible "matchFields"

and unifyTy(ty1: ty, ty2: ty): unit =
    let val ty1 = prune ty1
	and ty2 = prune ty2
     in case (ty1,ty2)
	  of (VARty(tv1),VARty(tv2)) =>
	       if eqTyvar(tv1,tv2)
	       then ()
	       else (case tv1
		       of ref(META _) => instantiate(tv1,ty2)
			| _ => (case tv2
				  of ref(META _) => instantiate(tv2,ty1)
				   | _ => raise Unify("bound type var")))
	   | (VARty(tv1 as ref(META _)),_) => instantiate(tv1,ty2)
	   | (_,VARty(tv2 as ref(META _))) => instantiate(tv2,ty1)
	   | (CONty(RECORDtyc labels, argTys),
	      FLEXRECORDty(r as ref(OPEN(fields,s)))) =>
	        (matchFields(fields, labels, argTys, s);
		 r := CLOSED(ty1))

	   | (FLEXRECORDty(r as ref(OPEN(fields,s))),
	      CONty(RECORDtyc labels, argTys)) =>
	        (matchFields(fields, labels, argTys, s);
		 r := CLOSED(ty2))

	   | (FLEXRECORDty(r1 as ref(OPEN(fields1,s1))),
	      FLEXRECORDty(r2 as ref(OPEN(fields2,s2)))) =>
	        let val (weakening,snew) =
		      if s1 < s2 then (SND s1, s1)
		      else if s2 < s1 then (FST s2, s2)
		      else (NEITHER, s1)
		 in r1 := CLOSED(FLEXRECORDty
			 (ref(OPEN(unifyFields(fields1,fields2,weakening),snew))));
		    r2 := !r1
		end

	   | (CONty(tycon1, args1), CONty(tycon2, args2)) =>
	       if eqTycon(tycon1, tycon2)
	       then case tycon1
		    of ERRORtyc => ()
		     | _ => case tycon2
			    of ERRORtyc => ()
			     | _ => unifyArgs(args1, args2)
	       else (unifyTy(reduceType ty1, ty2)
		     handle ReduceType =>
		       unifyTy(ty1, reduceType ty2)
		       handle ReduceType => raise Unify("tycon mismatch"))
	   | (CONty(DEFtyc _, _),_) => unifyTy(reduceType ty1, ty2)
	   | (_,CONty(DEFtyc _, _)) => unifyTy(ty1, reduceType ty2)
	   | (ERRORty, _) => ()   (* propagate error *)
	   | (_, ERRORty) => ()   (* propagate error *)
           | _ => raise Unify("type mismatch")
    end

and unifyArgs([],[]) = ()
  | unifyArgs(ty1::rest1, ty2::rest2) = (unifyTy(ty1,ty2); unifyArgs(rest1,rest2))
  | unifyArgs(_) = raise Unify("argument numbers");

fun unifyTypes([]: ty list) : ty = raise Unify("empty set")
  | unifyTypes(ty :: rest) = (app (fn ty' => unifyTy(ty, ty')) rest; ty);

end (* structure Unify *)
