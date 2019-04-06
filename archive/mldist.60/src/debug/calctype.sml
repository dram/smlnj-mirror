(* d r tarditi aug 89 *)

signature CALCTYPE = sig
  datatype enclosure = 
      ENCLOSED of BareAbsyn.pat * (unit -> enclosure) * (unit -> args)
    | NOTENCLOSED
  and args = 
      ARGS of BareAbsyn.exp * (unit -> enclosure)
    | NOTAVAIL
  val getType : BareAbsyn.exp * (unit -> enclosure) -> Basics.ty 
end  (* signature CALCTYPE *)

structure CalcType : CALCTYPE = struct

val DEBUG = false

open Basics BasicTypes Unify TypesUtil BareAbsyn Overload ErrorMsg
     PrintUtil PrintType PrintAbsyn 

datatype enclosure = 
    ENCLOSED of pat * (unit -> enclosure) * (unit -> args)
  | NOTENCLOSED
and args = 
    ARGS of exp * (unit -> enclosure)
  | NOTAVAIL

(* sorting numbered fields, used in typing record expressions *)
local
  val maxFieldNum = 100
  val buffer = array(maxFieldNum,
	             (Symbol.symbol("bogus"), UNDEFty))
in
  fun sortNumbered(numberedFields) =
      (app (fn (n,idty) => update(buffer,n,idty)) numberedFields;
       let fun collect(i,l) = 
	       if i<0 then l else collect(i-1,(buffer sub i)::l)
	in collect(length(numberedFields)-1,nil)
       end)
end (* local *)

fun applyType(ratorTy: ty, randTy: ty) : ty =
    let val resultType = VARty(mkTyvar defaultMETA)
     in unifyTy(ratorTy, (randTy --> resultType));
	resultType
    end

val patType = fn mapTy =>
let

   (* loop invariant: mapTy will be applied once to each saved type.  A saved
      type is a type that was left after unification. *)
       
   fun patType(pat: pat, kind: tvkind) : ty =
    case pat
      of WILDpat => VARty(mkTyvar(kind))
       | VARpat(VALvar{typ,...}) => mapTy(!typ)
       | INTpat _ => intTy
       | REALpat _ => realTy
       | STRINGpat _ => stringTy
       | CONpat(DATACON{typ,...}) => applyPoly(typ,0,infinity)
       | RECORDpat{fields,flex,typ,...} =>
	   (* fields assumed already sorted by label *)
	   let val labtys =
		   map (fn (lab,pat') => (lab,patType(pat',kind))) fields
	    in if flex
	       then mapTy(!typ)

	(* recordTy consists of types calculated using patType.  This 
	   imples that mapType has already been applied *)

	       else recordTy(labtys)
	   end
       | APPpat(DATACON{typ,rep,...},arg) =>  (* danger, exception case *)

	 (* invariant is true here also *)

	   let val argty = patType(arg,kind)
	    in applyType(applyPoly((case rep of REF  => refPatType | _ => typ),
				   0,infinity),
			 argty)
	   end
       | CONSTRAINTpat(pat',ty) => mapTy ty
       | LAYEREDpat(VARpat(VALvar{typ,...}),pat) => mapTy(!typ)
       | p => impossible "patType -- unexpected pattern"
in patType
end

(* doesn't matter here *)

val lambdaDepth = ref 0

val expType = fn mapTy =>
let val patType = patType mapTy
 fun expType(exp: exp, abs: int, wmax: int) : ty =
    case exp
      of VARexp(ref(VALvar{typ,...})) => mapTy(applyPoly(!typ,abs,wmax))
       | VARexp _ => impossible "expType -- bad VARexp"
       | CONexp(DATACON{typ,...}) => applyPoly(typ,abs,wmax)
       | INTexp _ => intTy
       | REALexp _ => realTy
       | STRINGexp _ => stringTy
       | RECORDexp fields =>
	   let val tyfields = map (fn (LABEL{name = id, number = n},exp') => 
				      (n, (id, expType(exp',abs,wmax))))
				  fields
	    in recordTy(sortNumbered tyfields)
	   end
       | SEQexp exps => 
	   let fun scan nil = unitTy
	         | scan [e] = expType(e,abs,wmax)
		 | scan (e::rest) = (scan rest)
	    in scan exps
	   end
       | APPexp(rator, rand) =>
	   let val wmax_rand = min(abs,wmax)
	       fun appType(exp as APPexp(rator,rand),abs_rator) =
		   let val ratorTy = 
			    case rator
			     of APPexp _ => appType(rator,abs_rator-1)
			      | _ => expType(rator,abs_rator-1,wmax)
		       val randTy = expType(rand,abs,wmax_rand)
		    in applyType(ratorTy,randTy)
		    end
	   in appType(exp,abs)
	   end
       | CONSTRAINTexp(e,ty) => mapTy ty
       | HANDLEexp(e,HANDLER h) =>
	   let val ety = expType(e,abs,wmax)
	       and hty = expType(h,abs,wmax)
	    in (unifyTy(hty, exnTy --> ety); ety)
	   end
       | RAISEexp(e) =>
	   let val ety = expType(e,abs,wmax)
	   in unifyTy(ety,exnTy); ety
	   end
       | LETexp(d,e) => expType(e,abs,wmax)
       | CASEexp(e,rules) =>
	   let val ety = expType(e,abs,wmax)
	       and rty = matchType(rules,abs-1,wmax)
	    in applyType(rty,ety)
	   end
		 (* this causes case to behave differently from let, i.e.
		    bound variables do not have generic types *)
       | FNexp rules => matchType(rules,abs,wmax)
       | MARKexp(e,_,_) => expType(e,abs,wmax)

and ruleType(RULE(pat,exp),abs,wmax) =  
    patType(pat,META{depth=(!lambdaDepth),weakness=infinity,eq=false})
      --> expType(exp,abs+1,wmax)

and matchType(l,abs,wmax) =
    let val d = !lambdaDepth
     in (inc lambdaDepth;
         (case l
	    of [] => impossible "empty rule list in typecheck.matchType"
	     | [rule] => ruleType(rule,abs,wmax)
	     | rule::rest =>
		 let val rty = ruleType(rule,abs,wmax)
		     fun checkrule rule' =
			 let val rty' = ruleType(rule',abs,wmax)
			  in unifyTy(rty, rty')
			 end
		  in app checkrule rest; rty
		 end)
	 before lambdaDepth := d)
    end
in expType
end



fun makeMap () =
let val l = ref (nil : (tyvar * tyvar) list)
    val find =
      fn (a as ref (b as META _)) =>
	 let fun f nil = NONE
	       | f ((key,data)::r) = if key=a then SOME data else f r
	 in case f (!l)
	    of NONE => let val newRef = ref b
		       in l := ((a,newRef) :: (!l));
			  newRef
		       end
	     | SOME data => data
	  end
	| a => a
     fun mapTy ty =
	case ty
	of VARty (ref (INSTANTIATED ty)) => mapTy ty
	 | VARty tyvar => VARty(find tyvar)
         | CONty (tyc,args) => CONty(tyc,map mapTy args)
         | FLEXRECORDty(ref(CLOSED ty)) => mapTy ty
	 | UNDEFty => ty
	 | ERRORty => ty
	 | _ => impossible "makeMap/mapTy--- impossible type in mapTy"
in mapTy
end 

val getMetaVars = fn ty =>
   let fun f (ty,l) =
     case ty
	of VARty (ref (INSTANTIATED ty)) => f(ty,l)
	 | VARty (r as ref (META _)) =>
	     if List.exists (fn a=>a=r) l then l else r :: l
	 | VARty _ => l
         | CONty (tyc,args) => fold f args l
         | FLEXRECORDty(ref(CLOSED ty)) => f (ty,l)
	 | UNDEFty => l
	 | ERRORty => l
	 | _ => impossible "getMetaVars/mapTy--- impossible type in mapTy"
   in f (ty,nil)
   end

fun disjoint(nil,_) = true
  | disjoint(_,nil) = true
  | disjoint(h::t,l) =
	if List.exists (fn a=>h=a) l then false else disjoint(t,l)

fun isConcrete (ref (META _)) = false
  | isConcrete (ref (INSTANTIATED ty)) =
	if length (getMetaVars ty)>0 then false else true
  | isConcrete _ = true

val notConcrete = fn l =>
	fold (fn (x,r) => if isConcrete x then r else x::r) l nil

fun makeConcrete mapTy =
   let val expType' = expType mapTy
       val patType' = patType mapTy
   in fn (exp,enclosing) =>
	let val resultTy = expType'(exp,0,infinity)
	    val _ = if DEBUG then (print "resultTy = ";
			           printType resultTy;
				   print "\n")
		    else ()
	    val metaVarList = getMetaVars resultTy
	    fun loop (_,nil) = resultTy
              | loop (ef,l) =
		 (case (ef()) of
		    NOTENCLOSED => resultTy
	          | ENCLOSED(pat,next,argFunc) =>
		     let val patTy = patType' (pat, META{depth=(!lambdaDepth),
						   weakness=infinity,eq=false})
		         val patMetaVarList = getMetaVars patTy
		     in if disjoint(l,patMetaVarList)
		 	then loop(next,l)
			else case (argFunc()) of
			       ARGS(exp,f) =>
			         let val argType = getType(exp,f)
			         in unifyTy(patTy,argType);
			            loop(next,notConcrete l)
			         end
			     | NOTAVAIL => resultTy  (* best can do -- a.t. *)
		     end)
	     val result = loop(enclosing,metaVarList)
	     val _ = if DEBUG then (print "end type = ";
				    printType result;
				    print"\n")
		     else ()
	  in result
	  end
    end
				 
and getType (args as (exp,enclosing)) =
   let val newMap = makeMap ()
       val _ = if DEBUG then print "entering getType\n"
	       else ()
       val result = makeConcrete newMap args
   in if DEBUG then (print "exiting getType\n";
		     printType result;
		     print "\n")
      else ();
      result
   end

end
