(* typesutil.sml *)

structure TypesUtil : TYPESUTIL = struct

structure Basics = Basics

open PrintUtil Basics List2 ErrorMsg PrintType BasicTypes

type label = Symbol.symbol
type labeledTy = label * ty

val eqLabel = Symbol.eq
fun ltLabel(l1,l2) = Symbol.name l1 < Symbol.name l2

fun bindTyvars tyvars =
    let fun loop([],_) = ()
	  | loop(tv::rest,n) = (tv := IBOUND n; loop(rest,n+1))
     in loop(tyvars,0)
    end

exception SHARE

(* assume that f fails on identity, i.e. f x raises SHARE instead of 
   returning x *)
fun shareMap f nil = raise SHARE
  | shareMap f (x::l) =
      (f x) :: ((shareMap f l) handle SHARE => l)
      handle SHARE => x :: (shareMap f l)

fun applyTyfun(TYFUN{arity,body},args) =
    let fun subst(VARty(ref(IBOUND n))) = nth(args,n)
	  | subst(CONty(tycref,args)) = CONty(tycref, shareMap subst args)
	  | subst(VARty(ref(INSTANTIATED ty))) = subst ty
	  | subst(FLEXRECORDty(ref(CLOSED ty))) = subst ty
	  | subst _ = raise SHARE
     in if arity > 0
	then subst body
	     handle SHARE => body
	else body
    end

exception ReduceType

fun reduceType(CONty(ref(TYCON{kind=DEFtyc tyfun,...}), args)) =
      applyTyfun(tyfun,args)
  | reduceType _ = raise ReduceType

fun equalTycon(tycs) =
    (* needed to deal with abbreviations *)
    eqTycon(tycs) orelse
    case tycs
      of (TYCON{kind=DEFtyc(TYFUN{arity,body}),...},
	  TYCON{kind=DEFtyc(TYFUN{arity=arity',body=body'}),...}) =>
	    arity = arity'
	    andalso equalType(body,body')
       | _ => false

and equalType(ty,ty') =
    let fun eq(VARty(tv),VARty(tv')) =
	      eqTyvar(tv,tv') orelse
	      (case (!tv,!tv')
	         of (IBOUND i, IBOUND j) => i=j
		  | _ => false)
	  | eq(ty as CONty(ref tycon, args), ty' as CONty(ref tycon', args')) =
	      if eqTycon(tycon, tycon') then List2.all2 equalType(args,args') 
	      else (equalType(reduceType ty, ty')
		    handle ReduceType =>
		      (equalType(ty,reduceType ty')
		       handle ReduceType => false))
	  | eq _ = false
     in eq(prune ty, prune ty')
    end

(* matching a scheme against a target type -- used declaring overloadings *)
fun matchScheme(TYFUN{arity,body}: tyfun, target: ty) : ty =
    let val tyenv = array(arity,UNDEFty)
	fun listofarray a =
	    let fun loop i = (a sub i)::loop(i+1) handle Subscript => []
	     in loop 0
	    end
	fun matchTyvar(i:int, ty: ty) : unit = 
	    case tyenv sub i
	      of UNDEFty => update(tyenv,i,ty)
	       | ty' => if equalType(ty,ty')
			then () 
			else impossible("matchScheme: bad tyvar "^makestring i)
        fun match(scheme:ty, target:ty) =
	    case (scheme,prune(target))
	      of (VARty(ref(IBOUND i)),ty) => matchTyvar(i,ty)
	       | (CONty(ref tycon1,args1), pt as CONty(ref tycon2,args2)) =>
		   if eqTycon(tycon1,tycon2)
		   then app2 match (args1, args2)
		   else (match(reduceType scheme, target)
			 handle ReduceType =>
			   (match(scheme, reduceType pt)
			    handle ReduceType =>
			      impossible "matchScheme: match -- tycons "))
	       | _ => impossible "matchScheme: match"
     in case prune target
	  of POLYty(TYFUN{arity=arity',body=body'}) =>
	       (match(body,body');
	        POLYty(TYFUN{arity = arity',
			     body = if arity>1
				    then tupleTy(listofarray tyenv)
				    else tyenv sub 0}))
	   | ty => 
	       (match(body,ty);
	        if arity>1
		then tupleTy(listofarray tyenv)
		else tyenv sub 0)
    end

fun typeArgs n = 
    if n>0
    then VARty(mkTyvar(METAARG)) :: typeArgs(n-1)
    else []

(* this should be merged with typeInContext and used in varApplied, etc. *)
fun applyPoly(POLYty(tyfun as TYFUN{arity,...})) : ty =
      applyTyfun(tyfun, typeArgs arity)
  | applyPoly ty = ty

(* type unification *)

exception Unify
      and Instantiate

fun instantiate(tv as ref kind: tyvar, ty: ty) : unit =
    case kind
      of METAARG => 
	   let fun scan(ty: ty) : unit =
	           case ty  (* "pruning" done in-line *)
		     of VARty(ref(INSTANTIATED ty')) => scan ty'
		      | VARty(tv') => 
			  if eqTyvar(tv, tv')
			  then (complain "circular type or self-application";
				raise Unify)
			  else ()
		      | CONty(_, args) => app scan args
		      | FLEXRECORDty(ref(OPEN fields)) =>
			  app (fn (_,ty') => scan ty') fields
		      | FLEXRECORDty(ref(CLOSED ty)) => scan ty
		      | _ => ()   (* propagate error *)
            in scan ty;
	       tv := INSTANTIATED ty
	   end
       | METALAM n =>
	   let fun scan(ty: ty) : unit =
	           case ty  (* "pruning" done in-line *)
		     of VARty(ref(INSTANTIATED ty')) => scan ty'
		      | VARty(tv') => 
			if eqTyvar(tv, tv')
			then (complain "circular type or self-application";
			      raise Unify)
			else (case !tv'
			       of METAARG => 
				    tv' := kind
				| METALAM m =>
				    if n < m
				    then tv' := kind
				    else ()
				| _ => ())
		      | CONty(_, args) => app scan args
		      | FLEXRECORDty(ref(OPEN fields)) =>
			  app (fn (_,ty') => scan ty') fields
		      | FLEXRECORDty(ref(CLOSED ty)) => scan ty
		      | _ => ()   (* propagate error *)
            in scan ty;
	       tv := INSTANTIATED ty
	   end
       | UBOUND _ => raise Instantiate
       | IBOUND n => impossible("instantiate -- IBOUND: "^makestring n)
       | INSTANTIATED _ => impossible "instantiate -- INSTANTIATED"

fun unifyFields(labtys1: labeledTy list, labtys2: labeledTy list): labeledTy list =
    case (labtys1, labtys2)
      of ([],_) => labtys2
       | (_,[]) => labtys1
       | ((labty1 as (lab1,ty1))::labtys1',
	  (labty2 as (lab2,ty2))::labtys2') =>
	    if eqLabel(lab1,lab2)
	    then (unifyTy(ty1,ty2);  (* type error possible *)
		  labty1 :: unifyFields(labtys1',labtys2'))
	    else if ltLabel(lab1, lab2)
	    then labty1 :: unifyFields(labtys1', labtys2)
	    else labty2 :: unifyFields(labtys1, labtys2')


and matchFields(fields: labeledTy list, labels: label list,
		argTys: ty list) =
    case (fields, labels)
      of ([],_) => ()
       | (_,[]) => 
	   (print "matchFields--too few labels\n";
	    raise Unify  (* RECORD mismatch *))
       | ((lab1,ty1)::fields', lab2::labels') =>
	   if eqLabel(lab1,lab2)
	   then let val ty2::argTys' = argTys
		 in unifyTy(ty1,ty2);
		    matchFields(fields',labels',argTys')
		end
           else if ltLabel(lab2,lab1)
	   then matchFields(fields, labels', tl argTys)
	   else (print "labels do not match:\n";
		 printSym lab1; print " "; printSym lab2; print "\n";
		 raise Unify)

and unifyTy(ty1: ty, ty2: ty): unit =
    let val ty1 = prune ty1
	and ty2 = prune ty2
     in case (ty1,ty2)
	  of (VARty(tv1),VARty(tv2)) =>
	       if eqTyvar(tv1,tv2)
	       then ()
	       else (instantiate(tv1,ty2)
		     handle Instantiate =>  (* tv1 is UBOUND *)
		       instantiate(tv2,ty1)
		       handle Instantiate => (* tv2 also UBOUND *)
		       raise Unify)
	   | (VARty(tv1),_) => instantiate(tv1,ty2)
	   | (_,VARty(tv2)) => instantiate(tv2,ty1)

	   | (CONty(ref(TYCON{kind=RECORDtyc labels,...}), argTys),
	      FLEXRECORDty(r as ref(OPEN fields))) =>
	        (matchFields(fields, labels, argTys);
		 r := CLOSED(ty1))

	   | (FLEXRECORDty(r as ref(OPEN fields)),
	      CONty(ref(TYCON{kind=RECORDtyc labels,...}), argTys)) =>
	        (matchFields(fields, labels, argTys);
		 r := CLOSED(ty2))

	   | (FLEXRECORDty(r1 as ref(OPEN fields1)),
	      FLEXRECORDty(r2 as ref(OPEN fields2))) =>
		(r1 := CLOSED(FLEXRECORDty(ref(OPEN(unifyFields(fields1,fields2)))));
                 r2 := !r1)

	   | (CONty(ref tycon1, args1), CONty(ref tycon2, args2)) =>
	       if eqTycon(tycon1, tycon2)
	       then unifyArgs(args1, args2)
	       else (unifyTy(reduceType ty1, ty2)
		     handle ReduceType =>
		       unifyTy(ty1, reduceType ty2)
		       handle ReduceType => raise Unify)
	   | (CONty(ref(TYCON{kind=DEFtyc _,...}),_),_) =>
	       unifyTy(reduceType ty1, ty2)
	   | (_,CONty(ref(TYCON{kind=DEFtyc _,...}),_)) =>
	       unifyTy(ty1, reduceType ty2)
	   | (ERRORty, _) => ()   (* propagate error *)
	   | (_, ERRORty) => ()   (* propagate error *)
           | _ => impossible "unifyTy"
    end

and unifyArgs([],[]) = ()
  | unifyArgs(ty1::rest1, ty2::rest2) = (unifyTy(ty1,ty2); unifyArgs(rest1,rest2))
  | unifyArgs(_) = raise Unify;


fun unifyTypes([]: ty list) : ty = raise Unify
  | unifyTypes(ty :: rest) = (app (fn ty' => unifyTy(ty, ty')) rest; ty);

end (* structure TypesUtil *)
