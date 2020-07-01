(* mcopt.sml *)
structure MCopt = 
struct

structure Absyn : BAREABSYN = Absyn
structure Lambda : LAMBDA = Lambda
structure Access : ACCESS = Access
open Basics Absyn Lambda
open PrintUtil PrintBasics PrintAbsyn MCprint ErrorMsg

fun cons2 (hd::hds,tl::tls) =
		(hd::tl)::cons2(hds,tls)
  | cons2 (hd::hds,nil) = [hd]::cons2(hds,nil)
  | cons2 (nil,nil) = nil
  | cons2 _ =  impossible "cons2 in mcopt"

infixr cons2

(* take a list of record patterns and return the list
   of the tail fields of each record pattern *)
fun tl2 ([_]::_) = nil
  | tl2 ((hd::tl)::pats) = tl::(tl2 pats)
  | tl2 nil = nil
  | tl2 _ = impossible "tl2 in mcopt"

(* take a list of record patterns and return the list
   of the first field of each record pattern *)
fun hd2 ((hd::tl)::pats) = hd::(hd2 pats)
  | hd2 nil = nil
  | hd2 _ = impossible "hd2 in mcopt"

fun branch_factor fs =
 let fun existsPat f =
      let val rec ePat =
	   fn nil => false
            | VARpat _::more => ePat more
            | WILDpat::more => ePat more
            | LAYEREDpat (_,p)::more => ePat (p::more)
            | CONSTRAINTpat (p,_)::more => ePat (p::more)
            | p::more => (f p orelse ePat more
			 handle Match => impossible "ePat in mcopt")
      in  ePat
      end
     fun within(p,plist) =
       case p
         of APPpat(DATACON{name=r1,...},_) =>
		existsPat (fn APPpat(DATACON{name=r2,...},_) => Symbol.eq(r1,r2)
			    | CONpat _ => false) plist
	  | CONpat(DATACON{name=r1,...}) =>
		existsPat (fn CONpat(DATACON{name=r2,...}) => Symbol.eq(r1,r2)
			    | APPpat _ => false) plist
	  | INTpat i => existsPat (fn INTpat j => i=j) plist
	  | REALpat r => existsPat (fn REALpat s => r=s) plist
	  | STRINGpat s => existsPat (fn STRINGpat t => s=t) plist
	  | VARpat _ => true
	  | WILDpat => true
	  | LAYEREDpat (_,p) => within (p,plist)
	  | CONSTRAINTpat (p,_) => within (p,plist)
	  |  _ => impossible "within in mcopt"
 in  length (fold (fn(a,b) => if within(a,b) then b else a::b)
		  (hd2 fs)
		  nil)
 end

fun arity ((hd::_)::_) = 
      let val rec ar =
	   fn INTpat _ => 1
	    | REALpat _ => 1
	    | STRINGpat _ => 1
	    | VARpat _ => 0
	    | WILDpat => 0
	    | RECORDpat{pats,...} => length (!pats)
	    | APPpat (_,p) => 1 + ar p
	    | CONpat _ => 1
	    | LAYEREDpat (_,p) => ar p
	    | CONSTRAINTpat (p,_) => ar p
      in  ar hd
      end
  | arity _ = impossible "arity in mcopt"

exception Record
val rec relevant =
 fn VARpat _ => false
  | WILDpat => false (* any var always matches so never relevant *)
  | RECORDpat{pats=ref nil,...} => false (* unit isDCB never relevant *)
  | RECORDpat _ => raise Record (* otherwise, immediately expand records *)
  | LAYEREDpat (_,p) => relevant p
  | CONSTRAINTpat (p,_) => relevant p
    (* if only one data constructor, no need to test *)
  | CONpat(DATACON{sign = [_],...}) => false
  | APPpat(DATACON{sign = [_],...},p) => relevant p
  | _ => true (* everything else is relevant *)
	
(* a record should be immediately expanded by mcand so that
   the nested fields can be considered as well;
   don't bother to look at the rest of the fields,
   and leave the record at the end of relf.
   otherwise, just check the relevant.
*)
fun rel_order (nil,nil) = (nil,nil,nil,nil)
  | rel_order (arg as (hd::_)::_,xl as x::xs) =
	let val (relf,relx,irrelf,irrelx) = rel_order(tl2 arg,xs)
	in  (case(relevant hd,relf,irrelf) of
		(true,_,nil) => (arg,xl,nil,nil)
	      | (false,nil,_) => (nil,nil,arg,xl)
	      | (true,_,_) => ((hd2 arg) cons2 relf, x::relx,irrelf,irrelx)
	      | (false,_,_) => (relf,relx,(hd2 arg) cons2 irrelf,x::irrelx))
	    handle Record => ((hd2 arg) cons2 nil,[x],tl2 arg,xs)
	end
  | rel_order _ = impossible "rel_order in mcopt"

fun branch_order (argp as [_]::_,argx as [_]) = (argp,argx,nil,nil)
  | branch_order (argp as ((_::_)::_),x::xs) =
	let val head = hd2 argp
	    val tail = tl2 argp
	    val (bestf,bestx,otherfs,otherxs) = branch_order(tail,xs)
	    val bfnew = branch_factor argp
	    val bfold = branch_factor bestf
	in  if bfnew < bfold then (head cons2 nil,[x],tail,xs)
	    else if bfnew > bfold
		 then (bestf,bestx,head cons2 otherfs,x::otherxs)
	    else (head cons2 bestf,x::bestx,otherfs,otherxs)
	end
  | branch_order (nil,nil) = (nil,nil,nil,nil)
  | branch_order _ = impossible "branch_order in mcopt"


fun arity_order (argp as [_]::_,argx as [_]) = (argp,argx,nil,nil)
  | arity_order (argp as ((_::_)::_),x::xs) =
	let val head = hd2 argp
	    val tail = tl2 argp
	    val (bestf,bestx,otherfs,otherxs) = arity_order(tail,xs)
	    val anew = arity argp
	    val aold = arity bestf
	in  if anew < aold then (head cons2 nil,[x],tail,xs)
	    else if anew > aold
		 then (bestf,bestx,head cons2 otherfs,x::otherxs)
	    else (head cons2 bestf,x::bestx,otherfs,otherxs)
	end
  | arity_order (nil,nil) = (nil,nil,nil,nil)
  | arity_order _ = impossible "arity_order in mcopt"

local
    fun combine (relf::relfs) (irrelf::irrelfs) =
			(relf@irrelf)::(combine relfs irrelfs)
      | combine nil nil = nil
      | combine nil irrel = irrel
      | combine rel nil = rel
    fun addtail (fields::pats,rhs::tl) =
			(fields,rhs)::addtail(pats,tl)
      | addtail (nil,nil) = nil
      | addtail _ = impossible "addtail in mcopt"
    fun strip ((fields,rhs)::pats) =
		let val (fl,tl) = strip pats
		in  (fields::fl,rhs::tl)
		end
      | strip nil = (nil,nil)
    fun root [r] = r
      | root (hd::tl) = root tl
      | root nil = impossible "root in mcopt"
in

fun isRec (RECORDpat{pats = ref nil,...}) = false
  | isRec (RECORDpat _) = true
  | isRec (LAYEREDpat(_,p)) = isRec p
  | isRec (CONSTRAINTpat(p,_)) = isRec p
  | isRec _ = false

(* OPT:  rearrange the fields of a tuple into a better order to evaluate.
	 use the relevant test.  if there are no relevant fields,
	 then the first pattern will match - don't bother returning
	 the rest.  if one of the fields is a record, return it first
	 so it is expanded.  if the relevant test does not isolate
	 one field, use the branch factor test, then the arity test.
*)
fun opt (arg as (nil,_)) = arg
  | opt (pl,xl) =
      let val (pats,tl) = strip pl
	  val (relf,relx,irrelf,irrelx) = rel_order(pats,xl)
      in  case (relf)
            of nil => ([hd pl],xl)
	     | [_]::_  => (addtail(combine relf irrelf,tl),relx@irrelx)
             | hd::_  =>
		if isRec(root hd)
		then (addtail(combine(map rev relf)irrelf,tl),rev relx @ irrelx)
		else let val (branchf,branchx,branchfs,branchxs) =
			         branch_order(relf,relx)
		     in  (case branchf
		           of [_]::_  =>
			      (addtail(combine(combine branchf branchfs)irrelf,tl),
				     branchx@branchxs@irrelx)
			    | _ =>
		              let val (arityf,arityx,arityfs,arityxs) =
				   arity_order(branchf,branchx)
			      in  (addtail(combine(combine(combine arityf arityfs)
				                           branchfs)irrelf,tl),
				   arityx@arityxs@branchxs@irrelx)
			      end)
		     end
      end

end (* local *)

end (* structure MCopt *)
