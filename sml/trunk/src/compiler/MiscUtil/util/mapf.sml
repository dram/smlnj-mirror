(* mapf.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature ORDSET = 
sig
    type elem
    val < : elem * elem -> bool
end

signature MAPF =
sig
  structure S : ORDSET
  type 'a map
  val empty : 'a map
  val singleton : S.elem * 'a  -> 'a map
  val overlay : 'a map * 'a map -> 'a map
  val merge : ('a * 'a -> 'a) -> 'a map * 'a map -> 'a map
  val add : 'a map * S.elem * 'a  -> 'a map
  exception MapF
  val lookup : 'a map -> S.elem -> 'a
  val members : 'a map -> (S.elem * 'a) list
  val cardinality : 'a map -> int
  val difference : 'a map * 'b map -> 'a map
  val delete : S.elem * 'a map -> 'a map
end

(*
    Copyright 1992 Stephen Adams.

    ALTERED FROM THE ORIGINAL by Andrew Appel

    This software may be used freely provided that:
      1. This copyright notice is attached to any copy, derived work,
         or work including all or part of this software.
      2. Any derived work must contain a prominent notice stating that
         it has been altered from the original.

*)

(* Name(s): Stephen Adams.
   Department, Institution: Electronics & Computer Science,
      University of Southampton
   Address:  Electronics & Computer Science
             University of Southampton
	     Southampton  SO9 5NH
	     Great Britain
   E-mail:   sra@ecs.soton.ac.uk

   Comments:

     1.  The implementation is based on Binary search trees of Bounded
         Balance, similar to Nievergelt & Reingold, SIAM J. Computing
         2(1), March 1973.  The main advantage of these trees is that
         they keep the size of the tree in the node, giving a constant
         time size operation.

     2.  The bounded balance criterion is simpler than N&R's alpha.
         Simply, one subtree must not have more than `weight' times as
         many elements as the opposite subtree.  Rebalancing is
         guaranteed to reinstate the criterion for weight>2.23, but
         the occasional incorrect behaviour for weight=2 is not
         detrimental to performance.

*)

functor MapF(S : ORDSET) : MAPF = 
struct
    structure S=S    

    val weight = 3

    datatype 'a map = E | T of S.elem * 'a * int * 'a map * 'a map

    fun size E = 0
      | size (T(_,_,n,_,_)) = n
    
    (*fun N(v,a,l,r) = T(v,a,1+size(l)+size(r),l,r)*)
    fun N(v,a,E,              E)               = T(v,a,1,E,E)
      | N(v,a,E,              r as T(_,_,n,_,_)) = T(v,a,n+1,E,r)
      | N(v,a,l as T(_,_,n,_,_),E)               = T(v,a,n+1,l,E)
      | N(v,a,l as T(_,_,n,_,_),r as T(_,_,m,_,_)) = T(v,a,n+m+1,l,r)

    fun single_L (a,a',x,T(b,b',_,y,z)) = N(b,b',N(a,a',x,y),z)
      | single_L _ = raise Match
    fun single_R (b,b',T(a,a',_,x,y),z) = N(a,a',x,N(b,b',y,z))
      | single_R _ = raise Match
    fun double_L (a,a',w,T(c,c',_,T(b,b',_,x,y),z)) = 
	         N(b,b',N(a,a',w,x),N(c,c',y,z))
      | double_L _ = raise Match
    fun double_R (c,c',T(a,a',_,w,T(b,b',_,x,y)),z) = N(b,b',N(a,a',w,x),N(c,c',y,z))
      | double_R _ = raise Match

    fun T' (v,v',E,E) = T(v,v',1,E,E)
      | T' (v,v',E,r as T(_,_,_,E,E))     = T(v,v',2,E,r)
      | T' (v,v',l as T(_,_,_,E,E),E)     = T(v,v',2,l,E)

      | T' (p as (_,_,E,T(_,_,_,T(_,_,_,_,_),E))) = double_L p
      | T' (p as (_,_,T(_,_,_,E,T(_,_,_,_,_)),E)) = double_R p

      (* these cases almost never happen with small weight*)
      | T' (p as (_,_,E,T(_,_,_,T(_,_,ln,_,_),T(_,_,rn,_,_)))) =
	if ln<rn then single_L p else double_L p
      | T' (p as (_,_,T(_,_,_,T(_,_,ln,_,_),T(_,_,rn,_,_)),E)) =
	if ln>rn then single_R p else double_R p

      | T' (p as (_,_,E,T(_,_,_,E,_)))  = single_L p
      | T' (p as (_,_,T(_,_,_,_,E),E))  = single_R p

      | T' (p as (v,v',l as T(lv,lv',ln,ll,lr),r as T(rv,rv',rn,rl,rr))) =
	if rn>=weight*ln then (*right is too big*)
	    let val rln = size rl
		val rrn = size rr
	    in
		if rln < rrn then  single_L p  else  double_L p
	    end
	    
	else if ln>=weight*rn then  (*left is too big*)
	    let val lln = size ll
		val lrn = size lr
	    in
		if lrn < lln then  single_R p  else  double_R p
	    end

	else
             T(v,v',ln+rn+1,l,r)

    fun add (E,x,x') = T(x,x',1,E,E)
      | add (T(v,v',w,l,r),x,x') =
        if S.<(x,v) then T'(v,v',add(l,x,x'),r)
	else if S.<(v,x) then T'(v,v',l,add(r,x,x'))
	     (* replace v,v' with x,x'! (blume/4/96) *)
	     else T(x,x',w,l,r)

    fun concat3 (E,v,v',r) = add(r,v,v')
      | concat3 (l,v,v',E) = add(l,v,v')
      | concat3 (l as T(v1,v1',n1,l1,r1), v, v', r as T(v2,v2',n2,l2,r2)) =
	if weight*n1 < n2 then T'(v2,v2',concat3(l,v,v',l2),r2)
	else if weight*n2 < n1 then T'(v1,v1',l1,concat3(r1,v,v',r))
	     else N(v,v',l,r)

    fun split (E, x) = (E, NONE, E)
      | split (T(v,v',_,l,r), x) = 
	if S.<(x,v) then let val (ll,z,lr) = split(l,x)
	                  in (ll,z,concat3(lr,v,v',r))
			 end
	else if S.<(v,x)
	            then let val (rl,z,rr) = split(r,x)
			  in (concat3(l,v,v',rl),z,rr)
			 end
        else (l,SOME v',r)

    fun split_lt (E,x) = E
      | split_lt (t as T(v,v',_,l,r),x) =
	if S.<(x,v) then split_lt(l,x)
	else if S.<(v,x) then concat3(l,v,v',split_lt(r,x))
	     else l

    fun split_gt (E,x) = E
      | split_gt (t as T(v,v',_,l,r),x) =
	if S.<(v,x) then split_gt(r,x)
	else if S.<(x,v) then concat3(split_gt(l,x),v,v',r)
	     else r

    and delmin (T(v,v',_,E,r)) = (v,v',r)
      | delmin (T(v,v',_,l,r)) = let val (x,x',l') = delmin l
	                          in (x,x',T'(v,v',l',r))
				 end
      | delmin _ = raise Match

    and cat2 (E,r) = r
      | cat2 (l,E) = l
      | cat2 (l,r) = let val (x,x',r') = delmin r
	                 in T'(x,x',l,r')
                        end

    fun concat (E,  s2) = s2
      | concat (s1, E)  = s1
      | concat (t1 as T(v1,v1',n1,l1,r1), t2 as T(v2,v2',n2,l2,r2)) =
	if weight*n1 < n2 then T'(v2,v2',concat(t1,l2),r2)
	else if weight*n2 < n1 then T'(v1,v1',l1,concat(r1,t2))
	     else cat2(t1,t2)

    fun fold(f,base,set) =
	let fun fold'(base,E) = base
	      | fold'(base,T(v,v',_,l,r)) = fold'(f((v,v'),fold'(base,r)),l)
	in 
	    fold'(base,set)
	end


    val empty = E
	
    fun singleton (x,x') = T(x,x',1,E,E)

    fun merge combine =
     let fun f (E,s2)  = s2
      | f (s1,E)  = s1
      | f (s1 as T(v,v',_,l,r),s2) = 
	case split(s2,v)
	 of (l2,SOME vv', r2) => concat3(f(l,l2), v, combine(v',vv'), f(r,r2))
          | (l2, NONE, r2) => concat3(f(l,l2),v,v',f(r,r2))
     in f
    end

    fun overlay x = merge #1 x

    fun difference (E,s)  = E
      | difference (s,E)  = s
      | difference (s, T(v,_,_,l,r)) =
	let val l2 = split_lt(s,v)
	    val r2 = split_gt(s,v)
	in
	    concat(difference(l2,l),difference(r2,r))
	end

    exception MapF

    fun lookup set x =
	let fun mem E = raise MapF
	      | mem (T(v,v',_,l,r)) = 
		if S.<(x,v) then mem l else if S.<(v,x) then mem r else v'
	in mem set end

    fun members set = fold(op::,[],set)

    fun cardinality E = 0
      | cardinality (T(_,_,n,_,_)) = n
    
    fun delete (x,E) = E
      | delete (x,set as T(v,v',_,l,r)) =
	if S.<(x,v) then T'(v,v',delete(x,l),r)
	else if S.<(v,x) then T'(v,v',l,delete(x,r))
	     else cat2(l,r)

end

(*
 * $Log: mapf.sml,v $
 * Revision 1.1.1.1  1997/01/14  01:38:49  george
 *   Version 109.24
 *
 *)
