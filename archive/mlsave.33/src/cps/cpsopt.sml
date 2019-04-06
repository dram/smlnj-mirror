functor CPSopt(val maxfree : int) :
	sig val reduce : (CPS.const Intmap.intmap) -> CPS.cexp -> CPS.cexp
        end =
struct

structure Recursive : sig val recursive : CPS.cexp -> Intset.intset end
 = struct

open CPS

     fun uses use =
     let val rec g =
      fn RECORD (vl,_,e) => (app (use o #1) vl; g e)
       | SELECT (_,v,_,e) => (use v; g e)
       | OFFSET (_,v,_,e) => (use v; g e)
       | APP(f,vl) => (use f; app use vl)
       | FIX(l, e) => (app (g o #3) l; g e)
       | SWITCH(v,el) => (use v; app g el)
       | PRIMOP(_,vl,_,el) => (app use vl; app g el)
      in g
     end
     fun fundefs do_it =
     let val rec g =
      fn RECORD (vl,_,e) => g e
       | SELECT (_,v,_,e) => g e
       | OFFSET (_,v,_,e) => g e
       | APP(f,vl) => ()
       | FIX(l, e) => (app do_it l; app (g o #3) l; g e)
       | SWITCH(v,el) => app g el
       | PRIMOP(_,vl,_,el) => app g el
      in g
     end
fun recursive e = 
 let type info = (int ref * cexp)
     exception IDBODYmap
     val idbodymap : info Intmap.intmap = Intmap.new(32, IDBODYmap) (* 32 *)
     val getidbody = Intmap.map idbodymap
     val setidbody = Intmap.add idbodymap
     val remidbody = Intmap.rem idbodymap
     val m3 = Intset.new()
     val markrecur = Intset.add m3
     val id = ref 0
     fun visit (v, path) =
      (case getidbody v of
        (idnum as ref 0, body)
	  => (inc id; idnum := !id; uses (fn w => visit(w,v::path)) body)
      | (ref name, _)
	  => let fun loop [] = ()
		   | loop (w::r) = if w=v
				   then (markrecur v; remidbody v)
				   else let val (ref wname, _) = getidbody w
					in if wname<name then () else loop r
					end
	     in loop path
	     end)
      handle IDBODYmap => ()	
 in fundefs (fn (v,_,b) => setidbody (v,(ref 0,b))) e;
    fundefs (fn (v,_,_) => visit (v,[])) e;
    m3
 end
end (* structure Recursive *)

open CPS SortedList Access

val foldconst = System.Control.CG.foldconst
val bodysize = System.Control.CG.bodysize
val etasplit = System.Control.CG.etasplit
fun member(i : int, a::b) = i=a orelse member(i,b)
  | member(i,[]) = false
fun choose([],[]) = []
  | choose(a::r, true::b) = a::choose(r,b)
  | choose(a::r, false::b) = choose(r,b)
fun map1 f (a,b) = (f a, b)

datatype arity = BOT | COUNT of int | TOP
datatype replacement = USUAL
		     | BETACONST of (int * (lvar * lvar list * cexp))
		     | BETA of ((lvar * lvar list * cexp) * bool ref)
		     | FLATTENARGS of int
		     | DROPARGS of bool list

val prrep = fn
   USUAL => "USUAL"
 | BETACONST _ => "BETACONST"
 | BETA _ => "BETA"
 | FLATTENARGS i => "FLATTENARGS " ^ makestring i 
 | DROPARGS l => "DROPARGS " ^ fold (fn(b,s) => if b then "T"^s else "F"^s) l ""

datatype cv = CO of const | VA of lvar

fun reduce ctab cexp =
let 
 val rec_set = Recursive.recursive cexp
 val isrec = Intset.mem rec_set
 val mkconst = Intmap.add ctab
 fun ctable v = CO(Intmap.map ctab v) handle Ctable => VA v
 fun isconst v = case ctable v of CO _ => true | VA _ => false
 val clicked = ref 0
 fun click s = ((*output std_out "*"; output std_out s;*) inc clicked)
 (* We're up to click "U" *)

     fun eta cexp =
     let exception M2
         val m2 : lvar Intmap.intmap = Intmap.new(32, M2) (* 32 *)
	 val name = Intmap.map m2
	 fun rename v = rename(name v) handle M2 => v
	 val newname = Intmap.add m2
	 val rec eta = 
	   fn RECORD(vl,w,e) => RECORD(map (map1 rename) vl, w, eta e)
	    | SELECT(i,v,w,e) => SELECT(i, v, w, eta e)
	    | APP(f,vl) => APP(rename f, map rename vl)
	    | SWITCH(v,el) => SWITCH(v, map eta el)
	    | PRIMOP(i,vl,wl,el) => PRIMOP(i, map rename vl, wl, map eta el)
	    | FIX(l,e) =>
		let fun h((ff as (f,vl,APP(v,wl)))::r) = 
			if wl=vl andalso not (member(rename v, f::vl))
			then let val v' = rename v
			     in if isrec f then Intset.add rec_set v' else ();
			        newname(f,v'); h r
			     end
			else ff :: h r
		      | h(ff :: r) = ff :: h r
		      | h [] = []
		    val l' = h l
		in case l' of
		     [] => eta e
		   | l' => FIX(map (fn(f,vl,a)=>(f,vl,eta a)) l', eta e)
		end
     in eta cexp
     end
     fun hoist cexp =
     let val freemap = FreeMap.freemapClose(cexp, fn x => false)
	 fun fixfree fl = foldmerge(map (fn (y,_,_) => freemap y) fl)
         val rec hoist = 
	   fn RECORD(vl, w, e) =>
	      (case hoist e of
	         e as FIX(l,e') => if member(w,fixfree l)
		 		   then RECORD(vl, w, e)
				   else FIX(l,RECORD(vl,w,e'))
	       | e => RECORD(vl, w, e))
	    | SELECT(i,v,w,e) =>
	      (case hoist e of
	         e as FIX(l,e') => if member(w,fixfree l)
				   then SELECT(i, v, w, e)
				   else FIX(l,SELECT(i,v,w,e'))
	       | e => SELECT(i, v, w, e))
	    | PRIMOP(i,vl,wl,[e]) =>
	      (case hoist e of
	         e as FIX(l,e') =>
		 (case intersect(uniq wl,fixfree l) of
		    [] => FIX(l,PRIMOP(i,vl,wl,[e']))
		  | _  =>  PRIMOP(i, vl, wl, [e]))
	       | e => PRIMOP(i, vl, wl, [e]))
	    | PRIMOP(i,vl,wl,el) => PRIMOP(i, vl, wl, map hoist el)
	    | e as APP(f,vl) => e
	    | SWITCH(v,el) => SWITCH(v, map hoist el)
	    | FIX(l,e) =>
	      let val (l,e) = (case hoist e of FIX(m,e) => (l@m,e) | e => (l,e))
	          val l = map (fn(f,vl,a)=>(f,vl,hoist a)) l
		  fun h((ff as (f,vl,FIX(l',e')))::r) =
		      (case intersect(uniq vl, fixfree l') of
			 [] => (f,vl,e')::l'@(h r)
		       | _ => ff :: h r)
		    | h(ff :: r) = ff :: h r
		    | h [] = []
	      in FIX(h l,e)
	      end
     in hoist cexp
     end

fun reducepass cexp =
 let val _ = clicked := 0
     datatype info = FNinfo of {calls : int ref, arity: arity ref, size: int ref,
			        copy : bool ref, replace : replacement ref}
		   | RECinfo of (lvar * accesspath) list
		   | SELinfo of (int * lvar)
		   | MISCinfo

     exception Escapemap
     val m : {info: info, used : int ref, escape : bool ref} Intmap.intmap =
		     Intmap.new(128, Escapemap)
     val get = Intmap.map m
     val enter = Intmap.add m
     fun use v = inc(#used(get v)) handle Escapemap => ()
     fun used v = !(#used(get v)) > 0 handle Escapemap => true
     fun escape v = let val {escape,used,...} = get v
		    in escape := true; inc used
		    end
		    handle Escapemap => ()
     fun call v =  (case get v
		    of {info=FNinfo{calls,...},used,...} => (inc calls; inc used)
		     | {used,...} => inc used)
		    handle Escapemap => ()
     fun argcnt(v,c) =
	 (case get v
	  of {info=FNinfo{arity as ref BOT,...},...} => arity := c
	   | {info=FNinfo{arity,...},...} => if c = !arity then () else arity := TOP
	  | _ => ())
	  handle Escapemap => ()
     fun onearg v = argcnt(v, TOP)
     fun selectonly r = not (!(#escape(get r))) handle Escapemap => false
     fun enterREC(w,vl) = enter(w,{info=RECinfo vl, escape=ref false, used = ref 0})
     fun enterSEL(w,x) = enter(w,{info=SELinfo x, escape=ref false, used = ref 0})
     fun enterMISC w = enter(w,{info=MISCinfo, escape=ref false, used = ref 0})
     fun enterFN f = enter(f,{info=FNinfo{calls=ref 0,arity=ref BOT,size=ref 0,
					  copy=ref false,replace=ref USUAL},
			      escape=ref false,used=ref 0})
     fun setsize(f,n) =
	 let val {info=FNinfo{size,...},...} = get f in size := n; n end
     fun sum f = let fun h [] = 0 | h (a::r) = f a + h r in h end

     val rec pass1 = 
      fn RECORD(vl,w,e) =>
         (enterREC(w,vl); app (escape o #1) vl; 2 + length vl + pass1 e)
       | SELECT (i,v,w,e) => (enterSEL(w,(i,v)); use v; 1 + pass1 e) (**)
       | APP(f,vl) => 
	 ((case get(hd vl) of
	     {info=RECinfo wl, ...} => argcnt(f,COUNT(length wl))
	   | _ => onearg f) handle Escapemap => onearg f
			         | Hd => onearg f;
	  call f; app escape vl; 1 + length vl)
       | FIX(l, e) => (app (fn (f, vl, e) => (enterFN f; app enterMISC vl)) l;
		       sum (fn (f,vl,e) => setsize(f, pass1 e)) l +
		       length l + pass1 e)
       | SWITCH(v,el) => (use v; 4 + (2 * length el) + sum pass1 el) (**)
       | PRIMOP(_,vl,wl,el) =>
	 (app escape vl; app enterMISC wl; length vl + length wl + sum pass1 el)
       | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"

     fun alphaConv(wl,e) =
      let open Intmap
	  exception Alpha
          val vm : lvar intmap = new(16, Alpha) (* 16 *)
          val map = map vm and add = add vm
          fun use v = map v handle Alpha => v
          fun def v = let val w = Access.dupLvar v 
		      in if isrec v then Intset.add rec_set w else (); add(v,w); w
		      end
          val rec g =
         fn RECORD(vl,w,ce) =>
	    let val {used, escape, ...} = get w
		val vl' = List.map (map1 use) vl
		val w' = def w
	    in enter(w',{info=RECinfo vl', escape=escape, used=used});
	       RECORD(vl', w', g ce)
	    end
          | SELECT(i,v,w,ce) =>
	    let val {used, escape, ...} = get w
		val v' = use v
		val w' = def w
	    in enter(w',{info=SELinfo (i,v'), escape=escape, used=used});
	       SELECT(i, v', w', g ce)
	    end
          | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
          | APP(v,vl) => APP(use v, List.map use vl)
          | FIX(l,ce) => 
	    let fun enterMISC v =
		    let val {used, escape, ...} = get v
			val v' = def v
		    in enter(v',{info=MISCinfo, used=used, escape=escape}); v'
		    end
	        fun h(f,vl,e) = (f, def f, List.map enterMISC vl, e)
	        fun h'(f,f',vl,e) =
		     let val {info=FNinfo{calls, arity, size, copy=ref c, ...},
		              used, escape} = get f
		     in enter(f',{info=FNinfo{calls=calls, arity=arity,
					      size=size, copy=ref c,
					      replace=ref USUAL},
				  used=used, escape=escape});
			(f', vl, g e)
		     end
	    in FIX(List.map h' (List.map h l), g ce)
	    end
          | SWITCH(v,l) => SWITCH(use v, List.map g l)
          | PRIMOP(i,vl,wl,ce) =>
	    let fun enterMISC w = 
		let val {used, escape, ...} = get w
		    val w' = def w
		in enter(w',{info=MISCinfo, used=used, escape=escape}); w'
		end
	    in PRIMOP(i, List.map use vl, List.map enterMISC wl, List.map g ce)
	    end
       in (List.map def wl, g e)
      end

     exception Beta
     val m2 : lvar Intmap.intmap = Intmap.new(16, Beta)
     fun ren v = ren(Intmap.map m2 v) handle Beta => v
     val newname = Intmap.add m2
     fun newnames(v::vl, w::wl) = (newname (v,w); newnames(vl,wl))
       | newnames([],[]) = ()
     fun extract ((v,OFFp 0),(f,l)) = (f,v::l)
       | extract ((v,SELp(i,p)),x) = let val w = mkLvar()
					 val (f',l') = extract((w,p),x)
				     in ((fn e=>SELECT(i,v,w,f' e)),l')
				     end
     fun selectop (i,v) =
	 let val v' = ren v
	 in (case get v' of
	       {info=RECinfo vl,...} =>
	            let val (x,p) = nth(vl,i) in click "a"; (ren x, p) end
	     | _ => raise Escapemap)
	    handle Escapemap => (v', SELp(i, OFFp 0))
	 end

     fun pathopt (v,p) = 
	 let val v' = ren v
	 in (case get v' of
	       {info=SELinfo iw,used=cnt as ref 1,...} =>
	            let val (x,p0) = selectop iw
		    in click "b"; (x,combinepaths(p0,p))
		    end
	     | _ => (v',p))
	    handle Escapemap => (v',p)
	 end

     val one = let val x = mkLvar() in mkconst(x, INTconst 1); x end
     
     val rec reduce = fn cexp => g NONE cexp
     and g = fn hdlr =>
     let val rec g' =
       fn RECORD (vl,w,e) => if selectonly w
			     then (click "c"; g' e)
			     else RECORD(map pathopt vl, w, g' e)
        | SELECT(i,v,w,e) =>
	  if not(used w)
          then (click "d"; g' e)
	  else let fun f(v',OFFp 0) = (newname(w,v'); g' e)
		     | f(v',SELp(i,OFFp 0)) = SELECT(i,v',w, g' e)
		     | f(v',SELp(i,p)) = 
		       let val w' = mkLvar() in SELECT(i,v',w', f(w',p)) end
	       in f(selectop(i,v))
	       end
	| OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
	| APP(f,vl) =>
	  ((case get(ren f) of
	      {info=FNinfo{replace=ref rep, ...}, ...} =>
	  (case rep of
	     BETA((_,wl,e),copy) => 
	     if !copy
	     then let val (wl',e') = alphaConv(wl,e)
		  in click "e"; newnames(wl', map ren vl); g' e'
		  end
	     else (copy := true; click "f"; newnames(wl, map ren vl); g' e)
	   | BETACONST(size,(_,wl,e)) =>
	     ((if (isconst(ren(hd vl)) andalso size <= 20) orelse
		  (case get(ren(hd vl)) of
		     {info=RECinfo _, ...} => size <= 2
		   | {info=FNinfo _, ...} => size <= 2
		   | {info=SELinfo _, ...} => size <= 2
		   | _ => false)
	       then let val (wl',e') = alphaConv(wl,e)
		    in click "g"; newnames(wl', map ren vl); g' e'
		    end
	       else raise Hd) handle Hd => APP(ren f, map ren vl)
				   | Escapemap => APP(ren f, map ren vl))
	   | DROPARGS bl => APP(ren f, map ren(choose(vl,bl)))
	   | FLATTENARGS _ =>
	     let val {info=RECinfo wl, ...} = get(hd vl)
		 val wl' = map (map1 ren) wl
		 val (ff,l) = fold extract wl' (fn x=>x, map ren (tl vl))
	     in ff(APP(ren f, l))
	     end
	   | USUAL => APP(ren f, map ren vl))
	   | {info=MISCinfo, ...} => APP(ren f, map ren vl)
	   | {info=SELinfo(i,w), ...} => APP(ren f, map ren vl)
	   | _ => ErrorMsg.impossible "cpsopt reduce")
	   handle Escapemap => APP(ren f, map ren vl))
	| FIX(l,e) =>
	  let fun h((ff as (f,vl,a))::r) = 
		  (case get f of
		     {info = FNinfo{size=ref b, arity, calls, copy, replace},
		      escape=ref false, ...} =>
		     if isrec f orelse (!calls > 1 andalso b > !bodysize)
		     then let val bl = map used vl
			  in if exists not bl
			     then (replace := DROPARGS bl; click "h";
				   (f,choose(vl,bl),a) :: h r)
			     else (case !arity of
			             COUNT j =>
				     if j>1 andalso j+length vl < maxfree-1
					andalso selectonly(hd vl)
				     then let fun vars (0,a) = a
					        | vars (i,a) =
						  vars(i-1,Access.mkLvar()::a)
					      val newargs = vars (j,[])
				          in replace := FLATTENARGS j;
					     click "i";
					     enterREC(hd vl, map (fn x =>(x,OFFp 0))
								 newargs);
					     use(hd vl);
					     (f, newargs @ tl vl, a) :: h r
					  end
				     else if isrec f
					  then ff::h r
					  else (replace := BETACONST(b,ff); ff::h r)
				   | _ => if isrec f
					  then ff::h r
					  else (replace := BETACONST(b,ff);ff::h r))
			  end
		     else (replace := BETA(ff,copy); click "j"; h r)
		   | {info = FNinfo{size=ref b, replace, ...}, ...} =>
		     if isrec f
		     then ff :: h r
		     else if b > !bodysize
			  then if used(hd vl)
			       then (replace := BETACONST(b,ff); ff :: h r)
			       else ff :: h r
			  else (replace := BETA(ff,ref true); ff :: h r))
		| h [] = []
	  in case h l of
	       [] => (click "k"; g' e)
	     | l' => FIX(map (fn(f,vl,a)=>(f,vl,reduce a)) l', g' e)
	  end
        | SWITCH(v,el) => 
		(case ctable (ren v)
		  of CO(INTconst i) => (click "l"; g' (nth(el,i)))
		   | VA v' => SWITCH(v', map g' el))
	| PRIMOP(P.gethdlr,vl,wl as [w],[e]) =>
	  (case hdlr of
	     NONE => PRIMOP(P.gethdlr,vl,wl,[g (SOME w) e])
	   | SOME w' => (click "m"; newname(w,w'); g' e))
	| PRIMOP(P.sethdlr,[v],wl,[e]) =>
	  let val v' = ren v
	      val e' = g (SOME v') e
	  in case hdlr of
	       NONE => PRIMOP(P.sethdlr,[v'],wl,[e'])
	     | SOME v'' => if v'=v'' then (click "n"; e')
			   else PRIMOP(P.sethdlr,[v'],wl,[e'])
	  end
        | PRIMOP(i,vl,wl as [w],el as [e]) =>
	  if not(used w) andalso Prim.pure i
	  then (click "o"; g' e)
	  else let val vl' = map ren vl
	       in (if !foldconst
		   then g' (primops i (map ctable vl', wl, el))
		   else raise Match)
	          handle Match => PRIMOP(i, vl', wl, [g' e])
	       end
        | PRIMOP(i,vl,wl,el) =>
	     let val vl' = map ren vl
	     in (if !foldconst
	         then g' (primops i (map ctable vl', wl, el))
	         else raise Match)
	        handle Match => PRIMOP(i, vl', wl, map g' el)
             end
     in g'
     end

     and primops =
	fn P.boxed => (fn ([CO(INTconst _)],_,[_,b]) => (click "A"; b)
		        | ([CO(STRINGconst s)],_,[a,b]) =>
			    (click "A"; if size s = 1 then b else a))
         | P.< => (fn ([CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
	              (click "B"; if Integer.<(i,j) then a else b))
         | P.<= => (fn ([CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
		   (click "C"; if Integer.<=(i,j) then a else b))
	 | P.>  => (fn ([CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
	           (click "D"; if Integer.>(i,j) then a else b))
         | P.>= => (fn ([CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
	           (click "E"; if Integer.>=(i,j) then a else b))
         | P.ieql => (fn ([CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
		     (click "F"; if i=j then a else b))
         | P.ineq => (fn ([CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
		     (click "G"; if i=j then b else a))
         | P.* => (fn ([CO(INTconst 1), VA(v)],[w],[c]) =>
		      (click "H"; newname(w,v); c)
		 | ([VA(v), CO(INTconst 1)],[w],[c]) =>
		      (click "H"; newname(w,v); c)
		 | ([CO(INTconst 0), _],[w],[c]) =>
		   (click "H"; mkconst(w,INTconst 0); c)
		 | ([_, CO(INTconst 0)],[w],[c]) =>
		      (click "H"; mkconst(w,INTconst 0); c)
		 | ([CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   let val x = i*j
		   in x+x; mkconst(w,INTconst x); click "H"; c
		   end handle Overflow => raise Match)
	 | P.div => (fn ([VA(v), CO(INTconst 1)],[w],[c]) =>
		      (click "I"; newname(w,v); c)
		 | ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
		   let val x = i div j
		   in click "I"; mkconst(w,INTconst x); c
		   end handle Div => raise Match)
         | P.+ => (fn ([CO(INTconst 0), VA(v)],[w],[c]) =>
		   (click "J"; newname(w,v); c)
		 | ([VA(v), CO(INTconst 0)],[w],[c]) =>
		   (click "J"; newname(w,v); c)
		 | ([CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   let val x = i+j
		   in x+x; mkconst(w,INTconst x); click "J"; c
		   end handle Overflow => raise Match)
         | P.- => (fn ([VA(v), CO(INTconst 0)],[w],[c]) =>
		      (click "K";newname(w,v); c)
		 | ([CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   let val x = i-j
		   in x+x; mkconst(w,INTconst x); click "K"; c
		   end handle Overflow => raise Match)
	 | P.rshift => (fn ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			   (click "L"; mkconst(w,INTconst(Bits.rshift(i,j))); c)
			 | ([CO(INTconst 0), VA v],[w],[c]) =>
			   (click "L"; mkconst(w,INTconst 0); c)
			 | ([VA v, CO(INTconst 0)],[w],[c]) =>
			   (click "L"; newname(w,v); c))
         | P.slength => (fn ([CO(INTconst _)],[w],[c]) =>
			 (click "M"; mkconst(w, INTconst 1); c)
		       | ([CO(STRINGconst s)], [w],[c]) =>
			 (click "M"; mkconst(w, INTconst(size s)); c))
         | P.ordof => (fn ([CO(STRINGconst s), CO(INTconst i)],[w],[c]) =>
			 (click "N"; mkconst(w, INTconst (ordof(s,i))); c))
         | P.~ => (fn ([CO(INTconst i)], [w], [c]) =>
		      let val x = ~i
		      in x+x; mkconst(w,INTconst x); click "O"; c
		      end handle Overflow => raise Match)
	 | P.lshift => (fn ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			   (let val x = Bits.lshift(i,j)
			    in x+x; mkconst(w,INTconst x); click "P"; c
			    end handle Overflow => raise Match)
			 | ([CO(INTconst 0), VA v],[w],[c]) =>
			   (click "P"; mkconst(w,INTconst 0); c)
			 | ([VA v, CO(INTconst 0)],[w],[c]) =>
			   (click "P"; newname(w,v); c))
	 | P.orb => (fn ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			(click "Q"; mkconst(w,INTconst(Bits.orb(i,j))); c)
		      | ([CO(INTconst 0),VA v],[w],[c]) =>
			(click "Q"; newname(w,v); c)
		      | ([VA v, CO(INTconst 0)],[w],[c]) =>
			(click "Q"; newname(w,v); c))
	 | P.xorb => (fn ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			 (click "R"; mkconst(w,INTconst(Bits.xorb(i,j))); c)
		      | ([CO(INTconst 0),VA v],[w],[c]) =>
			(click "R"; newname(w,v); c)
		      | ([VA v, CO(INTconst 0)],[w],[c]) =>
			(click "R"; newname(w,v); c))
	 | P.notb => (fn ([CO(INTconst i)], [w], [c]) =>
			 (mkconst(w,INTconst(Bits.notb i)); click "S"; c))
	 | P.andb => (fn ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			 (click "T"; mkconst(w,INTconst(Bits.andb(i,j))); c)
		      | ([CO(INTconst 0),VA v],[w],[c]) =>
			(click "T"; mkconst(w,INTconst 0); c)
		      | ([VA v, CO(INTconst 0)],[w],[c]) =>
			(click "T"; mkconst(w,INTconst 0); c))
         | _ => raise Match

   fun inverseEta cexp =
     let exception Etasplit
	 val m2 : lvar Intmap.intmap = Intmap.new(4, Etasplit)
	 fun ren v = Intmap.map m2 v handle Etasplit => v
	 val newname = Intmap.add m2
	 val rec g = 
	   fn RECORD(vl,w,e) => RECORD(vl,w, g e)
	    | SELECT(i,v,w,e) => SELECT(i, v, w, g e)
	    | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
	    | APP(f,vl) => APP(ren f, vl)
	    | FIX(l,e) =>
		let fun h((ff as (f,vl,e))::r) = 
		        (case get f of
		           {info=FNinfo{calls=ref 0,...},...} => ff::(h r)
			 | {escape = ref false,...} => ff::(h r)
			 | _ => let val vl' = map Access.dupLvar vl
				    val f' = Access.dupLvar f
				in newname(f,f');
				   if isrec f
				   then (Intset.add rec_set f';
					 Intset.rem rec_set f)
				   else ();
				   (f,vl',APP(f',vl'))::(f',vl,e)::(h r)
				end)
		      | h [] = []
		in FIX(map (fn(f,vl,a)=>(f,vl,g a)) (h l),  g e)
		end
	    | SWITCH(v,el) => SWITCH(v, map g el)
	    | PRIMOP(i,vl,wl,el) => PRIMOP(i, vl, wl, map g el)
      in g cexp
     end
     val h = (pass1 cexp; reduce cexp)
 in if !clicked<>0 andalso !clicked > !System.Control.CG.reducemore
    then reducepass h
    else let val h = eta(hoist h)
	 in if !etasplit
	    then let val h = inverseEta h in pass1 h; reduce h end
	    else h
	 end
 end
in reducepass(eta(hoist cexp))
end

end (* structure CPSopt *)
