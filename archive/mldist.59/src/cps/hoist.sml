(* Copyright 1989 by AT&T Bell Laboratories *)
structure Hoist : sig val hoist : (string->unit)->CPS.cexp->CPS.cexp end =
struct
 open Access CPS SortedList
structure CG = System.Control.CG

type fv = lvar list

datatype cexp'
  = RECORD' of (lvar * accesspath) list * lvar * cexp' * fv
  | SELECT' of int * lvar * lvar * cexp' * fv
  | OFFSET' of int * lvar * lvar * cexp' * fv
  | APP' of lvar * lvar list
  | FIX' of function' list * fv * cexp' * fv
  | SWITCH' of lvar * (cexp' * fv) list
  | PRIMOP' of Access.primop * lvar list * lvar list * (cexp' * fv) list
withtype function' = lvar * lvar list * cexp' * fv

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
      val escape = Intset.add s
      val rec pass1 = 
      fn RECORD(vl,w,e) =>  (app (escape o #1) vl; pass1 e)
       | SELECT (i,v,w,e) => pass1 e
       | APP(f,vl) => app escape vl
       | FIX(l, e) => (app (pass1 o #3) l; pass1 e)
       | SWITCH(v,el) => app pass1 el
       | PRIMOP(_,vl,_,el) => (app escape vl; app pass1 el)
       | OFFSET _ => ErrorMsg.impossible "OFFSET in hoist"
  in pass1 cexp; Intset.mem s
 end

 fun hoist click cexp =
  let (* val _ = CPSprint.show (outputc std_out) (Intmap.map ctab) cexp *)
      val clicked = ref false
      val click = fn x => (clicked := true; click x)
      infix 6 \/ val op \/ = merge
      infix 7 /\ val op /\ = intersect
      infix 6 -- val op -- = fn(a,b) => remove(b,a)
      val escapes = escapers cexp
      val rec hoist = 
	fn RECORD(vl, w, e) =>
	   (case hoist e
	    of (e as FIX'(l,v1,e',v2), v3) => 
		    if member v1 w orelse not(!CG.hoistup)
		      then (RECORD'(vl, w, e, v3), v3--[w]\/uniq(map #1 vl))
		      else let val defined = uniq(map #1 l)
			       val v4 = v2--[w]\/uniq(map #1 vl)
			    in (FIX'(l,v1,RECORD'(vl,w,e',v2),v4), v1\/(v4--defined))
			   end
	    | (e,v1) => (RECORD'(vl, w, e, v1), v1--[w]\/uniq(map #1 vl)))
	 | SELECT(i,v,w,e) =>
	   (case hoist e of
	      (e as FIX'(l,v1,e',v2), v3) => if member v1 w orelse not(!CG.hoistup)
		      then (SELECT'(i, v, w, e, v3), v3--[w]\/[v])
		      else let val defined = uniq(map #1 l)
			       val v4 = v2--[w]\/[v]
			    in (FIX'(l,v1,SELECT'(i,v,w,e',v2),v4),v1\/(v4--defined))
			   end
	    | (e,v1) => (SELECT'(i, v, w, e, v1),v1--[w]\/[v]))
	 | PRIMOP(i,vl,wl,[e]) =>
	   (case hoist e
	     of(e as FIX'(l,v1,e',v2),v3) =>
	      (case (uniq wl /\ v1, !CG.hoistup)
	        of ([],true) => let val v4 = v2--uniq wl\/uniq vl
		             val defined = uniq(map #1 l)
			  in (FIX'(l,v1,PRIMOP'(i,vl,wl,[(e',v2)]),v4),v1\/(v4--defined))
			 end
	         | _  =>  (PRIMOP'(i, vl, wl, [(e,v3)]),v3--uniq wl\/uniq vl))
	    | (e,v1) => (PRIMOP'(i, vl, wl, [(e,v1)]),v1--uniq wl\/uniq vl))
	 | PRIMOP(i,vl,wl,el) => 
		    let val el' = map hoist el
		     in (PRIMOP'(i,vl,wl,el'), foldmerge(map #2 el')--uniq wl\/uniq vl)
		    end
	 | APP(f,vl) => (APP'(f,vl), uniq(f::vl))
	 | SWITCH(v,el) => 
		    let val el' = map hoist el
		     in (SWITCH'(v, el'), foldmerge(map #2 el')\/[v])
		    end
	 | FIX(l,e) =>
	   let fun h((f,vl,(e as FIX'(l',v1,e',v2),v3))::r) =
			  (case (uniq vl /\ v1, !CG.hoistup)
			    of ([],true) => (click "p"; 
					     (f,vl,e',v2):: l' @ h r)
			     | _ => (f,vl,e,v3) :: h r)
		 | h((f,vl,(a,va))::r) = (f,vl,a,va) :: h r
		 | h [] = []
	       val l = h (map (fn(f,vl,a)=>(f,vl,hoist a)) l)
	       fun gather(a,nil,dontadd) = (a,dontadd)
                 | gather(a,add,dontadd) = 
			let val a' = a @ add
			    val va = uniq(map #1 a')
			    fun test(_,_,_,v1) = (v1/\va<>nil)
			    val (add',dontadd') = split test dontadd
			 in gather(a',add',dontadd')
			end
	       val (esc,nonesc) = split (escapes o #1) l
	       val (downbunch,upbunch) = gather(nil,esc,nonesc)
	       val downdef = uniq(map #1 downbunch)
	       val updef = uniq(map #1 upbunch)
	       val vd = foldmerge(map (#4) downbunch) -- downdef
	       val vu = foldmerge(map (#4) upbunch) -- updef
	       val (e,v2) = hoist e
	       exception Down
	       fun check vl = if !CG.hoistdown 
				  then case downdef /\ uniq vl of [] => () 
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
		fn RECORD'(vl,w,e,v3) => (check(map #1 vl); 
			    let val (e',v4) = down e
			     in (RECORD'(vl,w,e',v4),v4--[w]\/uniq(map #1 vl))
			    end)
		 | SELECT'(i,v,w,e,v3) => 
			    let val (e',v4) = (check nil; down e)
			     in (SELECT'(i,v,w,e',v4), v4--[w]\/[v])
			    end
	         | PRIMOP'(i,vl,wl,[(e,_)]) => (check vl;
			    let val (e',v4) = down e
			     in (PRIMOP'(i,vl,wl,[(e',v4)]), 
				 v4--uniq wl\/uniq vl)
			    end)
		 | PRIMOP'(i,vl,wl,el) => 
		     (check vl;
		      if sum present el < 2 
			    then let val el' = map down' el
				  in click "t";
				     (PRIMOP'(i,vl,wl,el'), 
				      foldmerge(map #2 el')--uniq wl\/uniq vl)
				 end
			    else raise Down)
		 | SWITCH'(v,el) => (* can't switch on a function *)
		      (check nil;
		       if sum present el < 2
			  then let val el' = map down' el
				in (SWITCH'(v,el'), foldmerge(map #2 el')\/[v])
			       end
			  else raise Down)
		 | e as APP'(f,vl) => (check(f::vl); click "s"; 
				       (e, uniq(f::vl)))
		 | FIX'(m,v3,e',v4) => 
		    (*  (!CG.hoistdown orelse !CG.hoistup) required here *)
			let val v5 = vd\/(v3--downdef)
			 in click "r"; 
			    (FIX'(downbunch@m,v5,e',v4),
			     v5\/(v4--(downdef\/uniq(map #1 m))))
			end
	    in (case (upbunch, if downbunch=nil then raise Down else down e)
		 of (nil,e_v) => e_v
		  | (_,(e',v7)) => (FIX'(upbunch,vu,e',v7),v7--updef\/vu))
	       handle Down => let val v1 = (vd\/vu)--(updef\/downdef)
			       in (FIX'(l,v1,e,v2),v1--(updef\/downdef)\/v2)
			      end
	   end
      val rec clean =
	fn RECORD'(vl,w,e,_) => RECORD(vl,w,clean e)
	 | SELECT'(i,v,w,e,_) => SELECT(i,v,w, clean e)
	 | PRIMOP'(i,vl,wl,el) => PRIMOP(i,vl,wl,map (clean o #1) el)
         | SWITCH'(v,el) => SWITCH(v, map (clean o #1) el)
	 | APP'(f,vl) => APP(f,vl)
	 | FIX'(l,_,e,_) => FIX(map (fn (f,vl,e,_)=>(f,vl,clean e)) l, clean e)
      val cexp' = #1(hoist cexp)
   in if !clicked then clean cexp' else cexp
  end

end
