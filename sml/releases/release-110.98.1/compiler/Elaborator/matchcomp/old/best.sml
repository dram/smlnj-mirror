(* best.sml *)

(* match optimization heuristics -- choosing the best choice node *)

(* ================================================================================ *)
(* mlsave.33/src/tranlate/mcopt.sml *)

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


(* ================================================================================ *)
(* mldist.44/src/translate/mcopt.sml *)
    
signature MCOPT =
  sig
    structure Absyn : BAREABSYN
    structure Access : ACCESS
    type rhs
    val opt : (Absyn.pat list * rhs) list * Access.lvar list ->
              (Absyn.pat list * rhs) list * Access.lvar list
  end

structure MCopt : MCOPT = struct

structure Absyn : BAREABSYN = Absyn
structure Lambda : LAMBDA = Lambda
structure Access : ACCESS = Access
open Basics Absyn Lambda
open PrintUtil PrintBasics PrintAbsyn MCprint ErrorMsg

type rhs = (Access.lvar * Access.lvar) list * int (* bindings and tag *)

fun cons2 (hd::hds,tl::tls) = (hd::tl)::cons2(hds,tls)
  | cons2 (hd::hds,[]) = [hd]::cons2(hds,[])
  | cons2 ([],[]) = []
  | cons2 _ =  impossible "cons2 in mcopt"

infixr cons2

(* take a list of record patterns and return the list
   of the tail fields of each record pattern *)
fun tl2 ([_]::_) = []
  | tl2 ((_::tl)::pats) = tl::(tl2 pats)
  | tl2 [] = []
  | tl2 _ = impossible "tl2 in mcopt"

(* take a list of record patterns and return the list
   of the first field of each record pattern *)
fun hd2 ((hd::_)::pats) = hd::(hd2 pats)
  | hd2 [] = []
  | hd2 _ = impossible "hd2 in mcopt"

fun combine(relf::relfs,irrelf::irrelfs) = (relf@irrelf)::(combine(relfs,irrelfs))
  | combine([],[]) = []
  | combine([],irrel) = irrel
  | combine(rel,[]) = rel
fun addtail (fields::pats,rhs::tl) = (fields,rhs)::addtail(pats,tl)
  | addtail ([],[]) = []
  | addtail _ = impossible "addtail in mcopt"
fun strip ((fields,rhs)::pats) =
    let val (fl,tl) = strip pats in  (fields::fl,rhs::tl) end
  | strip [] = ([],[])

fun branch_factor fs =
 let fun existsPat f =
      let val rec ePat =
	   fn [] => false
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
 in  length (fold (fn(a::_,b) => if within(a,b) then b else a::b) fs [])
 end

fun arity ((hd::_)::_) = 
      let val rec ar =
	   fn INTpat _ => 1
	    | REALpat _ => 1
	    | STRINGpat _ => 1
	    | VARpat _ => 0
	    | WILDpat => 0
	    | RECORDpat{pats=ref pats,...} => length pats
	    | APPpat (_,p) => 1 + ar p
	    | CONpat _ => 1
	    | LAYEREDpat (_,p) => ar p
	    | CONSTRAINTpat (p,_) => ar p
      in  ar hd
      end
  | arity _ = impossible "arity in mcopt"

exception Record

(* relevant : pat -> bool *)
fun relevant pat =
    case pat
     of VARpat _ => false
      | WILDpat => false (* any var always matches so never relevant *)
      | RECORDpat{pats=ref [],...} => false (* unit isDCB never relevant *)
      | RECORDpat _ => raise Record (* otherwise, immediately expand records *)
      | LAYEREDpat (_,p) => relevant p
      | CONSTRAINTpat (p,_) => relevant p
      (* if only one data constructor, no need to test *)
      | CONpat(DATACON{sign = [_],...}) => false
      | APPpat(DATACON{sign = [_],...},p) => relevant p
      | _ => true (* everything else is relevant *)

fun rel fs = fold (fn (a::_,b) => if relevant a then b else b+1) fs 0
	
(* a record should be immediately expanded by mcand so that
   the nested fields can be considered as well;
   don't bother to look at the rest of the fields,
   and leave the record at the end of relf.
   otherwise, just check the relevant.
*)
local
fun r_o([],[],relf,relx,irrelf,irrelx) = (relf,relx,irrelf,irrelx)
  | r_o(arg as (hd::_)::_,x::xs,relf,relx,irrelf,irrelx) =
    ((if relevant hd
      then r_o(tl2 arg,xs,(hd2 arg) cons2 relf,x::relx,irrelf,irrelx)
      else r_o(tl2 arg,xs,relf,relx,(hd2 arg) cons2 irrelf,x::irrelx))
     handle Record =>
      ((hd2 arg) cons2 [],[x],combine(relf,combine(tl2 arg,irrelf)),relx@xs@irrelx))
  | r_o _ = impossible "r_o in mcopt"
in fun rel_order(a,x) = r_o(a,x,[],[],[],[])
end

local
fun gen_order f =
    let fun order([],[],_,bestf,bestx,otherfs,otherxs) =
	    (bestf,bestx,otherfs,otherxs)
          | order(argp,x::xs,old,bestf,bestx,otherfs,otherxs) =
	    let val head = hd2 argp
		val tail = tl2 argp
		val new = f argp
	    in if Integer.<(new,old)
	       then order(tail,xs,new,head cons2 [],[x],
		          combine(bestf,otherfs),bestx@otherxs)
	       else if new > old
	       then order(tail,xs,old,bestf,bestx,head cons2 otherfs,x::otherxs)
	       else order(tail,xs,old,head cons2 bestf,x::bestx,otherfs,otherxs)
	    end
	  | order _ = impossible "order in mcopt"
    in fn(a,x::xs) =>
       let val hd = hd2 a
	   val tl = tl2 a
	   val bestf = hd cons2 []
       in order(tl,xs,f bestf,bestf,[x],[],[])
       end
    end
in
val relevance_order = gen_order rel
val branch_order = gen_order branch_factor
val arity_order = gen_order arity
end

(* OPT:  rearrange the fields of a tuple into a better order to evaluate.
	 use the relevant test.  if there are no relevant fields,
	 then the first pattern will match - don't bother returning
	 the rest.  if one of the fields is a record, return it first
	 so it is expanded.  if the relevant test does not isolate
	 one field, use the branch factor test, then the arity test. *)

fun opt (arg as ([],_)) = arg
  | opt (pl as hd::_,xl) =
    let val (pats,tl) = strip pl
	val (relf,relx,irrelf,irrelx) = rel_order(pats,xl)
    in case relf of
         [] => ([hd],xl)
       | [_]::_ => (addtail(combine(relf,irrelf),tl),relx@irrelx)
       | _ =>
	 let val (rf,rx,irf,irx) = relevance_order(relf,relx)
	     val rrest = combine(irf,irrelf)
	     val rrestx = irx@irrelx
	 in case rf of
	      [_]::_ => (addtail(combine(rf,rrest),tl),rx@rrestx)
	    | _ =>
	      let val (branchf,branchx,branchfs,branchxs) = branch_order(rf,rx)
		  val brest = combine(branchfs,rrest)
		  val brestx = branchxs@rrestx
	      in case branchf of
		   [_]::_ => (addtail(combine(branchf,brest),tl),branchx@brestx)
		 | _ => let val (arityf,arityx,arityfs,arityxs) =
				arity_order(branchf,branchx)
		        in (addtail(combine(arityf,combine(arityfs,brest)),tl),
			    arityx@arityxs@brestx)
			end
	      end
	 end
    end

end (* structure MCopt *)

(* ================================================================================ *)
(* 110.97/../matchcomp.sml *)

fun relevant (CASEDEC{defaults,...}, rulenum) =
      not (R.member(rulenum, defaults))
  | relevant (BINDDEC _, _) =
      bug "relevant - unexpected BINDDEC arg"

fun metric (CASEDEC{cases, defaults, ...}) = (length defaults, length cases)
  | metric (BINDDEC _) = bug "metric - unexpected BINDDEC arg"

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

(* extractNth : int * 'a list -> 'a * 'a list *)
fun extractNth(0, a::b) = (a, b)
  | extractNth(n, a::b) =
      let val (c,d) = extractNth(n - 1, b) in (c, a::d) end
  | extractNth _ = bug "extractNth - n too big"

exception PickBest
(* pickBest : decision list * ruleset -> decision * decision list *)
fun pickBest (decisions, active) = 
    let fun pick(nil, _, _, _, NONE) = raise PickBest
	  | pick(nil, _, _, _, SOME n) = n
	  | pick((BINDDEC _)::rest, _, n, _, _) = n
	  | pick((CASEDEC{sign = DA.CSIG(1,0), ...})::rest, _, n, _, _) = n (* single datacon *)
	  | pick((CASEDEC{sign = DA.CSIG(0,1), ...})::rest, _, n, _, _) = n (* single datacon *)
	  | pick(aCase::rest, active as act1::_, n, NONE, NONE) =
	    if relevant (aCase, act1)
	    then pick(rest, active, n + 1, SOME(metric aCase), SOME n)
	    else pick(rest, active, n + 1, NONE, NONE)
	  | pick(aCase::rest, active as act1::_, n, SOME m, SOME i) =
	    if relevant (aCase, act1)
	    then let val myMetric = metric aCase
		 in if metricBetter(myMetric, m) then
			pick(rest, active, n + 1, SOME(myMetric), SOME n)
		    else pick(rest, active, n + 1, SOME m, SOME i)
		 end
	    else pick(rest, active, n + 1, SOME m, SOME i)
	  | pick _ = bug "pick - unexpected arg"
	val best = pick(decisions, active, 0, NONE, NONE)
    in  extractNth(best, decisions)
    end

			     
(* ================================================================================ *)
(* FLINT/matchcomp/decision-tree.sml *)
	
exception PickBest

fun relevant (CHOICE{defaults,...}, rulenum) =
      not (R.member(rulenum, defaults))
  | relevant (BND _, _) =
      bug "relevant - unexpected BINDDEC arg"

(* numberChoices : choiceKind -> int *)
fun numberChoices (DATAchoices l) = length l
  | numberChoices (VECchoices l = length l
  | numberChoices (CONSTchoices l) = length l

(* metric : choice -> int * int *)
fun metric (CHOICE{choices, defaults)) = (length defaults, numberChoices choices)
  | metric (BND _) = bug "metric - unexpected BINDDEC arg"

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)

fun singleDcon (CHOICE{choices=DATAchoices((dcon, _, _)::_) =
    TU.dataconCount dcon = 1   (* dataconCount in TypesUtil *)
  | singleDcon _ = false

(* CSIG(0,1) means 0 dcons tagged, 1 dcon untagged => single dcon in datatype
   CSIG(1,0) is the reverse: 1 dcon tagged, 0 dcon untaged => single dcon in datatype
   -- assuming in CSIG(n,m) the total number of dcons is n+m, i.e. all dcons are either
      tagged or untagged *)
		       
(* pickBest0 : choice list * ruleset * int * (int * int) option * int option -> int *) 
(* the result is the index (into the choice list) for the "best" choice *)
fun pickBest0(nil, _, _, _, NONE) = raise PickBest
  | pickBest0(nil, _, _, _, SOME n) = n
  | pickBest0((BND _)::rest, _, n, _, _) = n
    (* if you encounter a BND, return its position in the choice list -- this
     * will trigger fireConstraint in genDecisionTree, starting over with new args *)
  | pickBest0(choice::rest, live, n,  caseONE, NONE) =
    if singleDcon choice then n
       (* singleton datatype choice considered _best_ ?
        * singleton constructors are discarded and their "children" choices ("guarded")
        * are added to the choices list and restart *)
    else if relevant (choice, R.minItem live)
    then pickBest0(rest, live, n + 1, SOME(metric choice), SOME n)
    else pickBest0(rest, live, n + 1, NONE, NONE) (* drop choice if not "relevant" *)
  | pickBest0(choice::rest, live, n, SOME m, SOME i) =
    if singleDcon choice then n
    else if relevant (choice, R.minItem live)
    then let val myMetric = metric choice
	  in if metricBetter(myMetric, m)
	     then pickBest0(rest, live, n + 1, SOME(myMetric), SOME n) (* best choice so far *)
	     else pickBest0(rest, live, n + 1, SOME m, SOME i)
	 end
    else pickBest0(rest, live, n + 1, SOME m, SOME i) (* drop choice if not "relevant" *)
  | pickBest0 _ = bug "pickBest0 - unexpected arg"

(* pickBest : choice list * rules -> ruleno *)
fun pickBest (choices, live) = pickBest0(choices, live, 0, NONE, NONE)

(* extractNth : int * 'a list -> 'a * 'a list *)
fun extractNth(0, a::b) = (a, b)
  | extractNth(n, a::b) =
      let val (c,d) = extractNth(n - 1, b) in (c, a::d) end

(* what does fireConstraint do?  constraints have disappeared, so the name is inappropriate *)
(* fireConstraint : path
 *                  * (path list * decision list) list     -- (needPaths, decisions) list
 *                  * decision list                        -- ready list
 *                  * (path list * decision list) list     -- delayed list
 *                  -> decision list * (path list * decision list) list
 * -- returns new "ready" list and new "delayed" list 
 *)
fun fireConstraint (path, (needPaths, decisions)::rest, ready, delayed) =
      (case removePath(path, needPaths)
         of nil => fireConstraint(path, rest, decisions@ready, delayed)
          | x => fireConstraint(path, rest, ready, (x,decisions)::delayed))
  | fireConstraint (path, nil, ready, delayed) =
      (ready, delayed)

(* genDecisionTree : (choice list * (path list * choice list) list) * ruleset
 *                   -> dectree *)
fun genDecisionTree((choices, delayed), live) =
      ((case extractNth(pickBest(choices, live), choices)
         of (BND(path, _), rest) =>
	      genDecisionTree(fireConstraint(path,delayed,rest,nil),live)
          | (CHOICE{path, defaults, choices}, rest) =>
	     (* case choices
                  of DATAchoices [(dcon,_,guarded)] =>
                       if singleDcon dcon
                       then genDecisionTree(rest@guarded, delayed), live)
		       else		       
             *)
            let fun isLive(CHOICE{ruleset,...}) = not(R.isEmpty(R.intersect(ruleset, live)))
		  | isLive _ = false (* ??? *)
                 val activeChoices = List.filter isLive choices
                 val caseTrees =
                   gencases(activeChoices, rest, delayed, defaults, live)
                 val defActive = R.intersect(live, defaults)
		 val branching =
		     case choices
		      of  DATAchoices((dcon,_,_)::_) =>
			  dataconBranching dcon
		       |  _  =>  0
		 val defTreeOp =
                     if length activeChoices = branching then NONE
                     else SOME (genDecisionTree((rest, delayed), defActive))
              in DCHOICE{path=path, caseTrees, default=defTreeOp)
             end
       handle PickBest => (RHS (R.minItem live))
  | genDecisionTree (_,nil) = bug "genDecisionTree - nil active"

and gencases (nil, decs, delayed, defaults, active) = nil
  | gencases ((pcon,rules,guarded)::rest,decs,delayed,defaults,active)=
      let val rActive = R.intersect(R.union(defaults, rules), active)
       in (pcon, genDecisionTree((decs@guarded, delayed),rActive))
          :: (gencases(rest,decs,delayed,defaults,active))
      end

(* ================================================================================ *)
(* matchcomp/decision-tree.sml *)

exception PickBest

fun relevant (CHOICE{defaults,...}, rulenum) =
      not (R.member(rulenum, defaults))
  | relevant (BND _, _) =
      impossible "relevant - unexpected BINDDEC arg"

(* numberChoices : choiceKind -> int *)
fun numberChoices (DATAchoices l) = length l
  | numberChoices (VECchoices l = length l
  | numberChoices (CONSTchoices l) = length l

(* metric : choice -> int * int *)
fun metric (CHOICE{choices, defaults)) = (length defaults, numberChoices choices)
  | metric (BND _) = impossible (metric only applied to CHOICE nodes)

fun metricBetter((a:int,b:int),(c,d)) = a < c orelse (a = c andalso b < d)
(* (1) having fewer defaults (variables at the node) is better
   (2) having fewer children at the node is better
*)

fun singleDcon (CHOICE{choices=DATAchoices((dcon, _, _)::_) =
    TU.dataconCount dcon = 1   (* dataconCount in TypesUtil *)
  | singleDcon _ = false

(* pickBest0 : choice list * ruleset * int * (int * int) option * int option
               -> int *) 
fun pickBest0(nil, _, _, _, NONE) = raise PickBest
  | pickBest0(nil, _, _, _, SOME n) = n
  | pickBest0((BND _)::rest, _, n, _, _) = n
  | pickBest0(choice::rest, live, n, NONE, NONE) =
    if singleDcon choice then n  (* ignore these degenerate OR nodes *)
    else if relevant (choice, R.minItem live)  (* discard non-relevant OR nodes *)
    then pickBest0(rest, live, n + 1, SOME(metric choice), SOME n)
    else pickBest0(rest, live, n + 1, NONE, NONE)
  | pickBest0(choice::rest, live, n, SOME m, SOME i) =
    if singleDcon choice then n
    else if relevant (choice, R.minItem live)
    then let val myMetric = metric choice
	  in if metricBetter(myMetric, m)
	     then pickBest0(rest, live, n + 1, SOME(myMetric), SOME n)
	     else pickBest0(rest, live, n + 1, SOME m, SOME i)
	 end
    else pickBest0(rest, live, n + 1, SOME m, SOME i)
  | pickBest0 _ = bug "pickBest0 - unexpected arg"

(* pickBest : choice list * ruleset -> int *)
(* returns the index of the "best" node in the choice list argument *)
fun pickBest (choices, live) = pickBest0(choices, live, 0, NONE, NONE)

(* 
Could it be that _none_ of the choices (OR nodes) are "relevant" to the first
remaining live rule?  If so, example?
In this case, exception PickBest is raised, and then handled in genDecisionTree,
which then generates a terminal node (RHS active1) for the decision tree, where
active1 is the first live rules (minimal member of live argument).

Thus if there is no releavant node, then this branch of decision tree is finished?

Does the "live" ruleset need to be provided, or can it be the live set(s)
of the OR nodes being compared?  Comes from the context of previous choices
while building the decision tree.

Can two OR nodes be compare based only on their internal properties (live ruleset,
default ruleset, breadth of choice), or is there an "external factor" in the form
of a (live) ruleset influence the comparison.

Is "relevance" an intrinsic property of an OR node, or does it only make sense 
with respect to an external ruleset?

Examples?

Make a choice
the next choice will be made in environments (live sets) created by the branches
of the first choice.  The different children of the first choice have different
live sets and the best choices for each child could therefore be different.

There are new best choice selections made for each child of the first choice (OR node).

The children may expose new possible choice points that will be added to the
list of possible choice points.

The first choice is "used up", so it will not be used again in the decision tree.

Other choices may be used multiple times in different branches of the decision tree
as it is being constructed.

A choice (OR node) will not be used multiple times on a given branch of the 
decision tree.  Multiple uses will be on different branches.

So as choices are made in different contexts

Want to minimiate total number of "uses" of OR nodes in the decision tree, so 
need to limit repeated uses where possible.  

Live set and "relevance" calculation changes as one goes down a branch of the
decision tree.

The behavior of live sets
-------------------------

1. the live set at an andor node contains the rule numbers of rules/patterns that 
could be matched given the choices along the path to the node.

2. the live sets for children of an OR node are subsets of the live set of the OR
node. They are proper subsets in the case of multiple children (e.g. true and false,
nil and cons).  The rule that requires true at a bool OR node will not be live in 
the falso child.

3. an OR node for a singleton datatype (one constructor) will have 0 or 1 children
(depending on whether the constructor is a constant). If it has a single child, then
that child inherits the same live set as the OR node.  No choice is involved, the
OR node is degenerate, and should be treated as a "no-op" wrt pattern matching (and
should not be included in candidate OR nodes when computing a decision tree).

4. variables at a node introduce "default" rules (rulenos).  The node can match values
that make any choice at the node.

4a. variables can occur at OR nodes, AND nodes, and VARS nodes.  They introduce defaults
that apply throughout the andor tree below that node (if any).

4b. a rule can be in the live set of _all_ (multiple?) children of an OR node if it is
compatible with all the keys of the children, which means it must be a variable rule
for that node (and therefore appears in the defaults set).  What is the defn of 
"a variable rule for a node"?  The rule can have a variable at the node's path,
or it can be "dominated" by a variable occurrence in a "higher" position, i.e. it
can have a variable at some prefix of the path. Example:

(1)     x,            true
(2) 	cons(y,nil),  true

Here node [R0] is an OR node for list, and it is a var node (variable x). So rule 1 is
in the default set for the node.  The live set is {1,2} (no choices above).

The andor tree is

[] AND {live = {1,2}, defaults = {}}
   [R0] OR (list) {live = {1,2}, defaults = {1}. vars = [(x,1)]}
XXX     [D(nil)] VARS(x,1) {live = {1}, defaults = {1}}  (x matches nil) (1 in _both_ live and defaults?)
        [D(cons)] AND {live = {2}, defauls = {1}}
                  [R0] VARS(y,2) {live = {2}, defaults = {1}}
                  [R1] OR(list) 
                       [D(nil)] # {live = {2}, defaults = {1}}
   [R1] OR(bool)
        [D(true) # {live = {1,2}, defaults = {}}

XXX is a "phantom" child, not really there because there is no nil pattern at that position [R0]
in the pattern space. There was a VARS node created during the construction of the andor tree
(by initAndor on the first row pattern), but it was merged into the OR(list) node when merging
the second row pattern.  I.e after initAndor on rule 1 the andor was:

[] AND {live = {1,2}, defaults = {}}
   [R0] VARS {live = {1,2}, defaults = {1}, vars = [(x,1)]}
   [R1] OR(bool)
        [D(true)] # {live = {1,2}, defaults = {}}

It would be possible to add phantom children of OR nodes covering choices not explicitly
appearing in the patterns (default choices). These default children would be justified
by existence of defaults (i.e. simple variables covering the node in the pattern space.

Live rulesets
-------------
What is the role of live rulesets.

(1) exhaustion of live rules can be used to determine that a rule is redundant?
    If a rule is not a member of the live set (or defaults?) at any terminal node
    of the decision tree, then that rule is redundant?

Example:
   (1) x
   (2) true     -- this should be redundant.

(2) Terminal node in a decision tree with empty live?
    Does that mean that the match is not exhaustive? redundant?

(2) If the live set at a terminal node in the decision tree has multiple members,
    then the rule chosen at that node is the minimal rule in the live set.

(3) How to live rules interact with defaults.
    (a) one possibility is that defaults should be included as a subset of live rules
    (b) another possibility is that they are independent. Is it the case that 
        live rules and defaults should be disjoint?
    (c) for a given node, rules in both live rules and defaults are "compatible",
        i.e. there exist values that match at that node an have the potential to
        fire that rule (except what about constraints from "independent" or "parallel"
        OR nodes.

(3) Are live rulesets actually necessary?

(4) Any rule that is "consistent" with all the alternatives (children) under an OR node
must be in the defaults for that OR node.

Example: datatype t = A | B | C

  (1) A
  (2) B
  (3) x

Rule (1) is inconsistent with child B (not live in the B child), rule (2) is inconsistent with A
(not live in the A child).  Rule 3 is consistent (default) with A and B, and it is in the
defaults.

For a rule n to be live at a node N, it must be live for all nodes along the path from root
to N. If it is not live at a node N, it is not live at any descendent of N.  

Defn: (n: rule, N: andor) Live(n,N) iff Pat(n,N) consistent with all choices along path(N).

Example of a rule not being live (not(Live(n,N))

(1) nil
(2) cons(x,y)

Node at [D(nil)] is inconsistent with pattern (2). Thus not(Live(2, [D(nil)]).


Defn: N < N' means that node N is on the path from root to N' and N <> N'.

Prop: Live(n, N) => All (N' < N). Live(n, N').
Corollary: not(Live(n,N)) => All (N < N').not(Live(n,N'))

Decision Trees

(1). To deal with nonexhaustive matches, we add a "default" variable pattern as an
extra rule at the end. This makes the match exhaustive.

If the extra default rule turns out to be redundant, then the original match was 
exhaustive.

Example

datatype t = A | B | C

(1) A
(2) B

This is a nonexhaustive match.  We augment it with:

(1) A
(2) B
(3) x  (or wildcard, which is just a variable with a default name)

In this case, (3) is not redundant, showing that the original match was not exhaustive.




 *)
