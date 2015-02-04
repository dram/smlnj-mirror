(* typesutil.sml *)

structure TypesUtil : TYPESUTIL = struct

structure Basics = Basics

local
  open PrintUtil Basics List2 ErrorMsg PrintType BasicTypes
in

type label = Symbol.symbol
type labeledTy = label * ty

val eqLabel = Symbol.Eq;
fun ltLabel(l1,l2) = Symbol.Name l1 < Symbol.Name l2;

(* expanding type abbreviations *)
fun expandTy (ty as CONty(ref(TYPEtyc{params,def,...}), args)) =
    let val rec expand =
         fn v as VARty(tv) =>
		 let fun look ([],[]) = v
		       | look (tv'::params', arg::args') =
			  if eqTyvar(tv,tv') then arg else look(params',args')
		       | look _ = (PrintType.printType ty; print "\n";
				   Complain "wrong number of arguments\
					    \ to type constructor";
				   v)
		  in look (params,args)
	         end
	   | CONty(reftycon, args') =>
	       CONty(reftycon, map expand args')  (* ??? *)
	   | _ => Impossible "231 in typesutil"
     in expand def
    end
  | expandTy _ = Impossible "expandTy in typesutil"

(* matching a scheme against a target type -- used declaring overloadings *)
fun matchType(scheme: ty, target: ty) : ty list =
    let val copyenv = ref nil : (tyvar * ty) list ref
	fun matchTyvar(tv: tyvar, ty: ty) : unit = 
	    let fun search(nil) : unit = 
		      copyenv := (tv, ty)::(!copyenv)
		  | search((tv',ty')::rest) =
		    if eqTyvar(tv, tv')
		       then if eqTy(ty,ty')
			      then () 
			      else (Complain "matchType: bad tyvar";
				    printTyvar tv; newline())
		       else search(rest)
	     in search(!copyenv)
	    end
        fun match(scheme:ty, target:ty) =
	    case (scheme,prune(target))
	      of (VARty(tv),ty) => matchTyvar(tv,ty)
	       | (CONty(ref tycon1,args1), pt as CONty(ref tycon2,args2)) =>
		   if eqTycon(tycon1,tycon2)
		   then app2 match (args1, args2)
		   else (case (tycon1,tycon2)
		     of (TYPEtyc _, _) => match(expandTy scheme, target)
		      | (_, TYPEtyc _) => match(scheme, expandTy pt)
		      | _ => (Complain "matchType:match -- tycons: ";
			  printTycon tycon1; prstr " "; printTycon tycon2;
			  newline()))
	       | (UNKNOWNty, _) => ()   (* propagate error *)
	       | (_, UNKNOWNty) => ()   (* propagate error *)
	       | _ => (Complain "matchType:match"; prstr"\n scheme: ";
		       printType scheme; prstr "\n target: ";
		       printType target; newline())
     in (match(scheme,target); rev(map (fn(_,ty)=>ty) (!copyenv)))
    end;

exceptionx SHARE

(* assume that f fails on identity, i.e. f x raises SHARE instead of 
   returning x *)

fun share_map f nil = raisex SHARE
  | share_map f (x::l) =
      (f x) :: ((share_map f l) handlex SHARE => l)
      handlex SHARE => x :: (share_map f l)

fun freshTy (ty : ty) =
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
		   | VARty _ => raisex SHARE
		   | CONty(tyconref,args) =>
		       CONty(tyconref, share_map fresh args)
		   | UNKNOWNty => raisex SHARE  (* propagate error *)
		   | _ => Impossible "fresh in typesutil.freshTy"
	    end
     in fresh(ty) handlex SHARE => ty
    end;


(* type unification *)

exceptionx unify: unit

fun instantiate(tv as TYVAR{status = refstatus,...}: tyvar, ty: ty) : unit =
    case !refstatus
      of METAARG => 
	   let fun scan(ty: ty) : unit =
	           case ty  (* "pruning" done in-line *)
		     of VARty(TYVAR{status=ref(INSTANTIATED ty'),...}) =>
			  scan ty'
		      | VARty(tv') => 
			  if eqTyvar(tv, tv')
			    then Complain "circular type or self-application"
			    else ()
		      | CONty(_, args) => app scan args
		      | FLEXRECORDty{fields,completion=ref UNKNOWNty} =>
			  app (fn (_,ty') => scan ty') fields
		      | FLEXRECORDty{completion,...} => scan(!completion)
		      | UNKNOWNty => ()   (* propagate error *)
		      | _ => Impossible "instantiate--scan1"
            in scan ty;
	       refstatus := INSTANTIATED ty
	   end
       | METALAM n =>
	   let fun scan(ty: ty) : unit =
	           case ty  (* does "pruning" in-line *)
		     of VARty(tv') => 
			if eqTyvar(tv, tv')
			then raisex unify
			else let val TYVAR{status = refstatus',...} = tv'
			      in case !refstatus'
				   of METAARG => 
				        refstatus' := !refstatus
				    | METALAM m =>
				        if n < m
					then refstatus' := !refstatus
					else ()
				    | INSTANTIATED ty' => scan ty'  (* new case *)
				    | _ => () (* should BOUND be an error? *)
			     end
		      | CONty(_, args) => app scan args
		      | FLEXRECORDty{fields,completion=ref UNKNOWNty} =>
			  app (fn (_,ty') => scan ty') fields
		      | FLEXRECORDty{completion,...} => scan(!completion)
		      | UNKNOWNty => ()   (* propagate error *)
		      | _ => Impossible "typesutil.instantiate -- scan2"
            in scan ty;
	       refstatus := INSTANTIATED ty
	   end
       | _ => raisex unify

fun mergeFields(labtys1: labeledTy list, labtys2: labeledTy list): labeledTy list =
    case (labtys1, labtys2)
      of ([],_) => labtys2
       | (_,[]) => labtys1
       | ((labty1 as (lab1,ty1))::labtys1',
	  (labty2 as (lab2,ty2))::labtys2') =>
	    if eqLabel(lab1,lab2)
	    then (unifyTy(ty1,ty2);  (* type error possible *)
		  labty1 :: mergeFields(labtys1',labtys2'))
	    else if ltLabel(lab1, lab2)
	    then labty1 :: mergeFields(labtys1', labtys2)
	    else labty2 :: mergeFields(labtys1, labtys2')


and matchFields(fields: (label * ty) list, labels: label list,
		argTys: ty list) =
    case (fields, labels)
      of ([],_) => ()
       | (_,[]) => 
	   (print "matchFields--too few labels\n";
	    raisex unify  (* RECORD mismatch *))
       | ((lab1,ty1)::fields', lab2::labels') =>
	   if eqLabel(lab1,lab2)
	   then let val ty2::argTys' = argTys
		 in unifyTy(ty1,ty2);
		    matchFields(fields',labels',argTys')
		end
           else if ltLabel(lab2,lab1)
	   then matchFields(fields, labels', tl argTys)
	   else (print "matchFields--label mismatch\n";
		 printSym lab1; print " "; printSym lab2; print "\n";
		 raisex unify)

and unifyTy(ty1: ty, ty2: ty): unit =
    let val ty1 = prune ty1
	and ty2 = prune ty2
     in case (ty1,ty2)
	  of (VARty(tv1),VARty(tv2)) =>
	       if eqTyvar(tv1,tv2)
	       then ()
	       else instantiate(tv1,ty2)
		    handlex unify => instantiate(tv2,ty1) (* tv1 is FIXED *)
	   | (VARty(tv1),_) => instantiate(tv1,ty2)
	   | (_,VARty(tv2)) => instantiate(tv2,ty1)

	   | (ty as CONty(ref(RECORDtyc{labels,...}), argTys),
	      FLEXRECORDty{fields, completion}) =>
	        (matchFields(fields, labels, argTys);
		 completion := ty)

	   | (FLEXRECORDty{fields, completion},
	      ty as CONty(ref(RECORDtyc{labels,...}), argTys)) =>
	        (matchFields(fields, labels, argTys);
		 completion := ty)

	   | (FLEXRECORDty{fields = labtys1, completion = refty1},
	      FLEXRECORDty{fields = labtys2, completion = refty2}) =>
		(refty1 := FLEXRECORDty{fields = mergeFields(labtys1, labtys2),
				        completion = ref UNKNOWNty};
                 refty2 := !refty1)

	   | (CONty(ref tycon1, args1), CONty(ref tycon2, args2)) =>
	       if eqTycon(tycon1, tycon2)
	       then unifyArgs(args1, args2)
	       else (case (tycon1,tycon2)
		  of (TYPEtyc _, _) => unifyTy(expandTy ty1, ty2)
		   | (_, TYPEtyc _) => unifyTy(ty1, expandTy ty2)
		   | _ => raisex unify)
	   | (CONty(ref(TYPEtyc _),_),_) => unifyTy(expandTy ty1, ty2)
	   | (_,CONty(ref(TYPEtyc _),_)) => unifyTy(ty1,expandTy ty2)
	   | (UNKNOWNty, _) => ()   (* propagate error *)
	   | (_, UNKNOWNty) => ()   (* propagate error ???*)
           | _ => (print "unifyTy--unknown case\n";
		   printType ty1; print "\n";
		   printType ty2; print "\n";
		   raisex unify)
    end

and unifyArgs([],[]) = ()
  | unifyArgs(ty1::rest1, ty2::rest2) = (unifyTy(ty1,ty2); unifyArgs(rest1,rest2))
  | unifyArgs(_) = raisex unify;


fun unifyTypes([]: ty list) : ty = raisex unify
  | unifyTypes(ty :: rest) = (app (fn ty' => unifyTy(ty, ty')) rest; ty);

end (* local open T ... *)

end; (* structure TypesUtil *)
