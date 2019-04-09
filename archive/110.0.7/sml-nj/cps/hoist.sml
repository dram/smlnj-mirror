(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *)
structure Hoist : sig val hoist : (string->unit)-> CPS.cexp 
                                  -> CPS.cexp 
                  end =
struct
 open Access CPS SortedList
structure CG = Control.CG
structure P = CPS.P (* to avoid confusing SourceGroup *)

type fv = lvar list

type rebind = (lvar * lvar) list

datatype cexp'
  = RECORD' of record_kind * (value * accesspath) list * lvar * cexp' * fv
  | SELECT' of int * value * lvar * cty * cexp' * fv
  | OFFSET' of int * value * lvar * cexp' * fv
  | APP' of value * value list
  | FIX' of function' list * fv * cexp' * fv
  | SWITCH' of value * lvar * (cexp' * fv) list
  | SETTER' of P.setter * value list * cexp' * fv
  | LOOKER' of P.looker * value list * lvar * cty * cexp' * fv
  | PURE' of P.pure * value list * lvar * cty * cexp' * fv
  | ARITH' of P.arith * value list * lvar * cty * cexp' * fv
  | BRANCH' of P.branch * value list * lvar * cexp' * fv * cexp' * fv
withtype function' = fun_kind * lvar * lvar list * cty list * cexp' * fv

 fun sum f = let fun h [] = 0 
		   | h (a::r) = f a + h r
	      in h
	     end

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
  in  subl
  end

fun split pred =
	let fun f nil = (nil,nil)
	      | f (a::r) = let val (x,y) = f r
			    in if pred a then (a::x, y) else (x, a::y)
		           end
        in f
       end

 fun escapers cexp =
  let val s = Intset.new()
      val escape' = Intset.add s
      fun escape(VAR x) = escape' x
        | escape _ = ()
      val rec pass1 = 
      fn RECORD(_,vl,w,e) =>  (app (escape o #1) vl; pass1 e)
       | SELECT (i,v,w,_,e) => pass1 e
       | APP(f,vl) => app escape vl
       | FIX(l, e) => (app (pass1 o #5) l; pass1 e)
       | SWITCH(v,_,el) => app pass1 el
       | LOOKER(_,vl,_,_,e) => pass1 e
       | ARITH(_,vl,_,_,e) => pass1 e
       | PURE(_,vl,_,_,e) => (app escape vl; pass1 e)
       | SETTER(_,vl,e) => (app escape vl; pass1 e)
       | BRANCH(_,_,_,e1,e2) => (pass1 e1; pass1 e2)
       | OFFSET _ => ErrorMsg.impossible "OFFSET in hoist"
  in pass1 cexp; Intset.mem s
 end

 local fun vars(l, VAR x :: rest) = vars(x::l, rest)
       | vars(l, _::rest) = vars(l,rest)
       | vars(l, nil) = uniq l
   in fun uniqv l = vars(nil, l)
  end

 fun hoist click cexp =
  let (* val _ = CPSprint.show Control.Print.say (Intmap.map ctab) cexp *)
      val clicked = ref false
      val click = fn x => (clicked := true; click x)
      infix 6 \/ val op \/ = merge
      infix 7 /\ val op /\ = intersect
      infix 6 -- val op -- = fn(a,b) => remove(b,a)
      val escapes = escapers cexp
      exception Pushdown
      (* the variable x always stands for a function value *)
      fun pushdown(x,x1,e,v0,make) =
        let fun g(v1) = (v1\/x1)--[x]
	    val rec push =
	    fn RECORD'(k,vl,w,e,v2) =>
	         if member (uniqv (map #1 vl)) x then raise Pushdown
		     else RECORD'(k,vl,w, push e, g v2)
             | SELECT'(i,v as VAR x',w,t,e,v2) => if x=x' then raise Pushdown
		                     else SELECT'(i,v,w,t,push e,g v2)
             | SELECT'(i,v,w,t,e,v2) => SELECT'(i,v,w,t,push e,g v2)
	     | LOOKER'(i,vl,w,t,e,v2) => 
		   if member(uniqv vl) x then raise Pushdown
		       else LOOKER'(i,vl,w,t,push e,g v2)
	     | ARITH'(i,vl,w,t,e,v2) => 
		   if member(uniqv vl) x then raise Pushdown
		       else ARITH'(i,vl,w,t,push e,g v2)
	     | PURE'(i,vl,w,t,e,v2) => 
		   if member(uniqv vl) x then raise Pushdown
		       else PURE'(i,vl,w,t,push e,g v2)
	     | SETTER'(i,vl,e,v2) => 
		   if member(uniqv vl) x then raise Pushdown
		       else SETTER'(i,vl,push e,g v2)
             | FIX'(l,v1,e,v2) =>
		   if member v1 x then raise Pushdown
		       else FIX'(l,v1, push e, g v2)
             | e as BRANCH'(i,vl,c,e1,v1,e2,v2) => 
		  (if member (uniqv vl) x then raise Pushdown
		     else case (member v1 x, member v2 x)
			   of (false,false) => e
			    | (true,false) =>
			       let val (e1',v1') = push1(e1,v1)
			        in BRANCH'(i,vl,c,e1', v1', e2,v2)
			       end
			    | (false,true) =>
			       let val (e2',v2') = push1(e2,v2)
			        in BRANCH'(i,vl,c,e1, v1, e2',v2')
			       end
			    | (true,true) => raise Pushdown)
             | _ => raise Pushdown
            and push1 = fn(e1,v1) => 
		(click "%";
		 (push e1, g v1) handle Pushdown => (make(e1,v1), g v1))
	 in ((push e handle Pushdown => make(e,v0)), g v0)
	end
		       
      val rec hoist = 
	fn RECORD(k,vl, w, e) =>
	  let fun makerecord(e,v) = pushdown(w,uniqv(map #1 vl),e,v,
					     fn(e,v)=>RECORD'(k,vl,w,e,v))
           in case hoist e
	    of ev as (FIX'(l,v1,e2,v2), _) => 
		    if member v1 w orelse not(!CG.hoistup)
		      then makerecord ev
		      else let val (e5,v5) = makerecord(e2,v2)
			    in (FIX'(l,v1,e5,v5), v1\/(v5--uniq(map #2 l)))
			   end
	    | ev => makerecord ev
          end
	 | SELECT(i,v,w,t,e) =>
	   let fun makeselect(e,v0) = pushdown(w,uniqv[v],e,v0,
					      fn(e,v0)=>SELECT'(i,v,w,t,e,v0))
	    in case hoist e of
	      ev as (FIX'(l,v1,e2,v2), _) => 
		  if member v1 w orelse not(!CG.hoistup)
		      then makeselect ev
		      else let val (e5,v5) = makeselect(e2,v2)
			    in (FIX'(l,v1,e5,v5),v1\/(v5--uniq(map #2 l)))
			   end
	    | ev => makeselect ev
	   end
         | LOOKER(i,vl,w,t,e) =>
	   let fun makeprim(e,v) = 
	                  (LOOKER'(i,vl,w,t,e,v), v--[w]\/uniqv vl)
	    in case hoist e
	     of ev as (FIX'(l,v1,e2,v2),_) =>
	       (case ([w] /\ v1, !CG.hoistup)
	         of ([],true) => let val (e5,v5) = makeprim(e2,v2)
			         in (FIX'(l,v1,e5,v5),v1\/(v5--uniq(map #2 l)))
			        end
	         | _  =>  makeprim ev)
	      | ev => makeprim ev
	    end
         | ARITH(i,vl,w,t,e) =>
	   let fun makeprim(e,v) = 
	                  (ARITH'(i,vl,w,t,e,v), v--[w]\/uniqv vl)
	    in case hoist e
	     of ev as (FIX'(l,v1,e2,v2),_) =>
	       (case ([w] /\ v1, !CG.hoistup)
	         of ([],true) => let val (e5,v5) = makeprim(e2,v2)
			         in (FIX'(l,v1,e5,v5),v1\/(v5--uniq(map #2 l)))
			        end
	         | _  =>  makeprim ev)
	      | ev => makeprim ev
	    end
         | PURE(i,vl,w,t,e) =>
	   let fun makeprim(e,v) = 
	            pushdown(w,uniqv vl,e,v,
			     fn (e,v) => PURE'(i,vl,w,t,e,v))
	    in case hoist e
	     of ev as (FIX'(l,v1,e2,v2),_) =>
	       (case ([w] /\ v1, !CG.hoistup)
	         of ([],true) => let val (e5,v5) = makeprim(e2,v2)
			         in (FIX'(l,v1,e5,v5),v1\/(v5--uniq(map #2 l)))
			        end
	         | _  =>  makeprim ev)
	      | ev => makeprim ev
	    end
         | SETTER(i,vl,e) =>
	   let fun makeprim(e,v) = 
	                  (SETTER'(i,vl,e,v), v \/ uniqv vl)
	    in case hoist e
	     of ev as (FIX'(l,v1,e2,v2),_) =>
	        if !CG.hoistup
	         then let val (e5,v5) = makeprim(e2,v2)
		       in (FIX'(l,v1,e5,v5),v1\/(v5--uniq(map #2 l)))
		      end
	         else makeprim ev
	      | ev => makeprim ev
	    end
	       
	 | BRANCH(i,vl,c,e1,e2) =>
	       let val (e1',v1') = hoist e1
		   val (e2',v2') = hoist e2
		in (BRANCH'(i,vl,c,e1',v1',e2',v2'),
		    uniqv vl \/ v1' \/ v2')
	       end

	 | APP(f,vl) => (APP'(f,vl), uniqv(f::vl))
	 | SWITCH(v,c,el) => 
		    let val el' = map hoist el
		     in (SWITCH'(v, c, el'), foldmerge(map #2 el')\/uniqv[v])
		    end
	 | FIX(l,e) =>
	   let fun h((fk,f,vl,cl,(e as FIX'(l',v1,e',v2),v3))::r) =
			  (case (uniq vl /\ v1, !CG.hoistup)
			    of ([],true) => (click "*"; 
					     (fk,f,vl,cl,e',v2):: l' @ h r)
			     | _ => (fk,f,vl,cl,e,v3) :: h r)
		 | h((fk,f,vl,cl,(a,va))::r) = (fk,f,vl,cl,a,va) :: h r
		 | h [] = []
	       val l = h (map (fn(fk,f,vl,cl,a)=>(fk,f,vl,cl,hoist a)) l)
	       fun gather(a,nil,dontadd) = (a,dontadd)
                 | gather(a,add,dontadd) = 
			let val a' = a @ add
			    val va = uniq(map #2 a')
			    fun test(_,_,_,_,_,v1) = (v1/\va<>nil)
			    val (add',dontadd') = split test dontadd
			 in gather(a',add',dontadd')
			end
	       val (esc,nonesc) = split (escapes o #2) l
	       val (downbunch,upbunch) = gather(nil,esc,nonesc)
	       val downdef = uniq(map #2 downbunch)
	       val updef = uniq(map #2 upbunch)
	       val vd = foldmerge(map (#6) downbunch) -- downdef
	       val vu = foldmerge(map (#6) upbunch) -- updef
	       val (e,v2) = hoist e
	       exception Down
	       fun check vl = if !CG.hoistdown 
				  then case downdef /\ uniqv vl of [] => () 
						    | _ => raise Down
				  else raise Down
	       fun present (_,vx) = case downdef/\vx
				      of []=>0 | _ => 1
	       val rec down' = fn (cexp,vx) => 
			    case downdef /\ vx
			     of [] => (cexp,vx)
			      | _ => down cexp 
				     handle Down => (FIX'(downbunch,vd,cexp,vx),
						     vx--downdef\/vd)
	       and down =
		fn RECORD'(k,vl,w,e,v3) => (check(map #1 vl); 
			    let val (e',v4) = down e
			     in (RECORD'(k,vl,w,e',v4),v4--[w]\/uniqv(map #1 vl))
			    end)
		 | SELECT'(i,v,w,t,e,v3) => 
			    let val (e',v4) = (check nil; down e)
			     in (SELECT'(i,v,w,t,e',v4), v4--[w]\/uniqv[v])
			    end
		 | LOOKER'(i,vl,w,t,e,_) =>
		       (check vl;
			let val (e',v4) = down e
			 in (LOOKER'(i,vl,w,t,e',v4), v4--[w] \/ uniqv vl)
			end)
		 | ARITH'(i,vl,w,t,e,_) =>
		       (check vl;
			let val (e',v4) = down e
			 in (ARITH'(i,vl,w,t,e',v4), v4--[w] \/ uniqv vl)
			end)
		 | PURE'(i,vl,w,t,e,_) =>
		       (check vl;
			let val (e',v4) = down e
			 in (PURE'(i,vl,w,t,e',v4), v4--[w] \/ uniqv vl)
			end)
		 | SETTER'(i,vl,e,_) =>
		       (check vl;
			let val (e',v4) = down e
			 in (SETTER'(i,vl,e',v4), v4 \/ uniqv vl)
			end)
		 | BRANCH'(i,vl,c,e1,v1,e2,v2) =>
		     (check vl;
		      if present(e1,v1)+present(e2,v2) < 2 
			    then let val (e1',v1') = down'(e1,v1)
				     val (e2',v2') = down'(e2,v2)
				  in click "&";
				     (BRANCH'(i,vl,c,e1',v1',e2',v2'), 
				      v1' \/ v2' \/ uniqv vl)
				 end
			    else raise Down)
		 | SWITCH'(v,c,el) => (* can't switch on a function *)
		      (check nil;
		       if sum present el < 2
			  then let val el' = map down' el
				in (SWITCH'(v,c,el'), 
				    foldmerge(map #2 el')\/uniqv[v])
			       end
			  else raise Down)
		 | e as APP'(f,vl) => (check(f::vl); click ")"; 
				       (e, uniqv(f::vl)))
		 | FIX'(m,v3,e',v4) => 
		    (*  (!CG.hoistdown orelse !CG.hoistup) required here *)
			let val v5 = vd\/(v3--downdef)
			 in click "_"; 
			    (FIX'(downbunch@m,v5,e',v4),
			     v5\/(v4--(downdef\/uniq(map #2 m))))
			end
	    in (case (upbunch, 
                      case downbunch of nil => raise Down | _ => down e)
		 of (nil,e_v) => e_v
		  | (_,(e',v7)) => (FIX'(upbunch,vu,e',v7),v7--updef\/vu))
	       handle Down => let val v1 = (vd\/vu)--(updef\/downdef)
			       in (FIX'(l,v1,e,v2),v1--(updef\/downdef)\/v2)
			      end
	   end
      val rec clean =
	fn RECORD'(k,vl,w,e,_) => RECORD(k,vl,w,clean e)
	 | SELECT'(i,v,w,t,e,_) => SELECT(i,v,w,t,clean e)
	 | SETTER'(i,vl,e,_) => SETTER(i,vl,clean e)
	 | LOOKER'(i,vl,w,t,e,_) => LOOKER(i,vl,w,t,clean e)
	 | ARITH'(i,vl,w,t,e,_) => ARITH(i,vl,w,t,clean e)
	 | PURE'(i,vl,w,t,e,_) => PURE(i,vl,w,t,clean e)
	 | BRANCH'(i,vl,c,e1,_,e2,_) => BRANCH(i,vl,c,clean e1, clean e2)
         | SWITCH'(v,c,el) => SWITCH(v, c, map (clean o #1) el)
	 | APP'(f,vl) => APP(f,vl)
	 | FIX'(l,_,e,_) => FIX(map (fn (k,f,vl,cl,e,_)=>
                                  (k,f,vl,cl,clean e)) l, clean e)
      val cexp' = #1(hoist cexp)
    fun ssss cexp = if !CG.debugcps (*debug*)
		    then (Control.Print.say "\nAfter hoist: \n"; 
			  (* if !CG.misc4=16 then
			      CPSprint.show Control.Print.say cexp
			      else (); *)
			  cexp)
	            else cexp


   in if !clicked then ssss(clean cexp') else cexp
  end

end

(*
 * $Log: hoist.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:44  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:38:31  george
 *   Version 109.24
 *
 *)
