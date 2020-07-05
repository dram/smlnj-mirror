(* match.sml *)

(* sort rules
AppPat and ConPat w. dcon up to irref.
Maintain order, but group by head dcon

1. sort rules
2. if no head dcons then all bindpats (var, wild, const, tuple)
    test bindpats in succession
3. if head dcons in patterns,
   within dcon group (left to right)
  a. ConPat -- just test, with fail transfering to next group
  b. AppPat -- test dcon, then test secondary (arg bindpat) if refutable
		on failure, test next seconday
	        on success, commit and "bind" variables (insertVar), if any
		if secondaries exhausted, go to next group
4. no irref. pats (var,wild), add one as default, with match failure exp
*)

(* rbindpat : a bindpat analyzed with respect to refutable/irrefutable properties
   of its components *)
datatype rbindpat
  = AtomRefut of atompat  (* refutable atomic: int, string, nonsingleton dcon *)
  | AtomIrref of atompat  (* irrefutable atomic: VarPat or Wild, or singleton dcon *)
  | RTuplePat of
     {refut: (atompat * int) list,
      irref: (atompat * int) list}

datatype matchTree
  = HeadDcon of dcon * (rbindpat * int) list  (* int is rule number *)
  | ConstDcon of dcon * int
  | Bind of (rbindpat * int) list

(* testing pattern refutability *)
fun irrefAtom(VarPat _) = true
  | irrefAtom(WildPat) = true
  | irrefAtom(ConPat dcon) = DC.singleton dcon
  | irrefAtom _ = false

fun irrefBind (AtomPat apat) = irrefAtom apat
  | irrefBind (TuplePat apats) =
    List.all (fn p => irrefAtom p) apats

(* partBind : bindpat -> rbindpat
partition a bindpat: var, wild, const (int, string), tuple
TuplePat ==>  (refuts, irrefs)
1. refuts: refutable elements, left to right, with position
2. irrefs: irrefutable elements (vars, wild), left to right, with position
3. no refutables => pattern is irref.
Not a tuple ==>
  ([c,0],[]) if p a constant c
  ([],[v,0]) if p a var v (or wildcard)
*)
fun partBind (AtomPat(apat as VarPat v)) = AtomRefut apat
  | partBind (AtomPat(apat as WildPat)) = AtomRefut apat
  | partBind (TuplePat apats) =
    let fun part (nil, n, refuts, irrefs) =
	    RTuplePat{refut = rev refuts, irref = rev irrefs}
          | part (apat::apats, n, refuts, irrefs) = 
	    if irrefAtom apat then part(apats,n+1,refuts,(apat,n)::irrefs)
	    else part(apats,n+1,(apat,n)::refuts,irrefs)
     in part (apats, 0, nil, nil)
    end

fun insertarg(dcon, rbindpat, (h as HeadDcon(dcon', rbindpats))::sorted) =
    if DC.same (dcon,dcon') then HeadDcon(dcon', (rbindpat,n)::rbindpats)::sorted
    else h :: insert(dcon, rbindpat, n sorted)
  | insertarg(dcon, rbindpat, sorted) = HeadDcon(dcon, [rbindpat])::sorted

fun insertcon(dcon, n, (h as HeadDcon _)::sorted) =
    h :: insertcon(dcon,n,sorted)
  | insertcon(dcon, n, sorted as (h as ConsDcon(dcon',_))::rest) =
    if DC.same(dcon,dcon') then (warn "redundant patterns"; s)
    else insert(dcon,n,rest)
  | insertcon(dcon, n, sorted) = ConstDcon(dcon,n)::sorted

fun insertbind(rbindpat, (h as HeadDcon _) :: sorted) =
    h :: insertbind(rbindpat, sorted)
  | insertbind(rbindpat, (h as ConstDcon _) :: sorted) =
    h :: insertbind(rbindpat, sorted)
  | insertbind(rbindpat, [Bind rbindpats]) =
    [Bind(rbindpat::rbindpats)]
  | insertbind(rbindpat, nil) =
    [Bind[rbindpat]]

(* sort: (rulepat * int) list -> matchTree list *)
(* sorting the toplevel patterns.
 * if there is an AppPat, there should be at most one BindPat that is not
 * a ConPat, and that should be a VarPat/WildPat (irrefutable) *)
fun sort ((pat,n)::pats, sorted) =
    case pat
      of AppPat(dcon, argpat) =>
	 sort(pats, insertarg(dcon, (part(argpat), n), sorted))
       | BindPat(AtomPat(ConPat dcon)) =>
	 sort(pats, insertcon(dcon, n, sorted))
       | BindPat pat =>
	 if refutable pat then sort(pats, insertbind((part(pat), n), sorted)
	 else (case pats
		 of nil => ()
		  | _ => warning "redundant patterns";
	       insertbind(part(pat), n sorted))

(* (1) only AppPat/ConPat patterns -- check all dcons represented
 * (2) BindPat(TuplePat pats)) -- check each field exhaustive
 * (3) or check for irrefutable BindPat (VarPat/WildPat) *)
fun checkExhaustive matchTrees =
    

fun transMatch rules =
    let val (pats, exps) = ListPair.unzip rules
	val (numberedpats,_) = 
	    foldl (fn (pat, (npats,n)) => ((pat,n)::npats, n+1)) (nil, 0) pats
	val matchTrees = sort(numberedpats, nil)
	val exhaustive = checkExhaustive matchTrees
    in (rev numberedpats, exps)
    end

end (* structure Match *)
