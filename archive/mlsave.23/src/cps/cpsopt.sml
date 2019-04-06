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
     val idbodymap : info Intmap.intmap = Intmap.new IDBODYmap
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
	  => let fun loop nil = ()
		   | loop (w::r) = if w=v
				   then (markrecur v; remidbody v)
				   else let val (ref wname, _) = getidbody w
					in if wname<name then () else loop r
					end
	     in loop path
	     end)
      handle IDBODYmap => ()	

 in fundefs (fn (v,_,b) => setidbody (v,(ref 0,b))) e;
    fundefs (fn (v,_,_) => visit (v,nil)) e;
    print "done with recursiveness testing\n";
    m3
 end

end (* structure Recursive *)

open CPS SortedList Access

val foldconst = System.Control.CG.foldconst
val bodysize = System.Control.CG.bodysize
val etasplit = System.Control.CG.etasplit

fun member(i : int, a::b) = i=a orelse member(i,b)
  | member(i,nil) = false

fun choose(nil,nil) = nil
  | choose(a::r, true::b) = a::choose(r,b)
  | choose(a::r, false::b) = choose(r,b)

fun map1 f (a,b) = (f a, b)


datatype argcount = BOT | COUNT of int | TOP
datatype replacement = USUAL
		     | BETA of (lvar list * cexp * bool ref * bool)
		     | FLATTENARGS of int
		     | DROPARGS of bool list

val prrep = fn
  USUAL => "USUAL" | BETA _ => "BETA"
 | FLATTENARGS i => "FLATTENARGS " ^ makestring i 
 | DROPARGS l => "DROPARGS " ^ fold (op ^) 
		    (map (fn true => "T" | false => "F") l) ""

datatype cv = CO of const | VA of lvar

fun reduce ctab cexp =
let 
 val rec_set = Recursive.recursive cexp
 val isrecursive = Intset.mem rec_set
 val bindconst = Intmap.add ctab
 fun ctable v = CO(Intmap.map ctab v) handle Ctable => VA v
 val clicked = ref 0
 fun click s = (output std_out s; inc clicked)
 (* We're up to click "F" *)

fun reducepass (cexp,donePass) =
 let val _ = clicked := 0
     fun eta cexp =
     let exception M2
         val m2 : lvar Intmap.intmap = Intmap.new M2
	 fun rename v = rename(Intmap.map m2 v) handle M2 => v
	 val newname = Intmap.add m2
         val freemap = FreeMap.freemapClose(cexp, fn x => false)
	 val fixfree = fn fl => fold (fn((y,_,_),x)=> merge(x, freemap y)) fl nil
	 val rec g = 
	   fn RECORD(vl,w, e as FIX(l,e')) =>
		if member(w,fixfree l)
		    then  RECORD(map (map1 rename) vl, w, g e)
		    else (click "a"; g(FIX(l,RECORD(vl,w,e'))))
	    | SELECT(i,v,w,e as FIX(l,e')) =>
		if member(w,fixfree l)
		    then SELECT(i,rename v, w, g e)
		    else (click "b"; g(FIX(l,SELECT(i,v,w,e'))))
	    | PRIMOP(i,vl,wl,[e as FIX(l,e')]) =>
		(case intersect(uniq wl,fixfree l)
		  of nil => (click "c"; g(FIX(l,PRIMOP(i,vl,wl,[e']))))
		   | _ =>  PRIMOP(i, map rename vl, wl, [g e]))
	    | RECORD(vl,w,e) => 
		    RECORD(map (map1 rename) vl, w, g e)
	    | SELECT(i,v,w,e) => SELECT(i,rename v, w, g e)
	    | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
	    | APP(f,vl) => APP(rename f, map rename vl)
	    | FIX(l,FIX(m,e)) => (click "d"; g(FIX(l@m,e)))
	    | FIX(l,e) =>
		let val found = ref false
		    fun h((ff as (f,vl,APP(v,wl)))::r) = 
			       if wl=vl andalso not (member(rename v, f::vl))
				  then (click "e";  newname(f,rename v); h r) 
				  else ff :: h r

		      | h((ff as (f,vl,FIX(l',e')))::r) =
			 (case intersect(uniq vl, fixfree l')
			   of nil => (click "f"; found := true; (f,vl,e')::l'@(h r))
			    | _ => ff :: h r)

		      | h(ff :: r) = ff :: h r
		      | h nil = nil
		    val l' = h l
		 in case l'
		     of nil => g e
		      | l' => if !found then g(FIX(l',e))
			    else FIX(map (fn(f,vl,a)=>(f,vl,g a)) l',  g e)
		end
	    | SWITCH(v,el) => SWITCH(rename v, map g el)
	    | PRIMOP(i,vl,wl,el) => PRIMOP(i, map rename vl, wl, map g el)
      in g cexp
     end

     datatype info = FNinfo of {body: lvar list * cexp, calls : int ref,
			      argcount: argcount ref, size: int ref,
			      copy : bool ref}
		   | RECinfo of (lvar * accesspath) list
		   | MISCinfo

     exception Escapemap
     val m : {info: info, used : bool ref, escape : bool ref} Intmap.intmap =
		     Intmap.new Escapemap
     val get = Intmap.map m
     val enter = Intmap.add m
     fun use v = #used(get v) := true handle Escapemap => ()
     fun used v = !(#used(get v)) handle Escapemap => true
     fun escape v = let val {escape,used,...} = get v
		     in escape := true; used := true
		    end
		    handle Escapemap => ()
     fun call v =  (case get v
		     of {info=FNinfo{calls,...},used,...} =>
				(inc calls; used := true)
		      | {used,...} => used := true)
		    handle Escapemap => ()
     fun argcnt(v,c) = 
		   (case get v
		     of {info=FNinfo{argcount as ref BOT,...},...} =>
							argcount := c
		      | {info=FNinfo{argcount as ref c',...},...} =>
				     if c=c' then () else argcount := TOP
		      | _ => ())
		     handle Escapemap => ()
     fun onearg v = argcnt(v, TOP)
     fun selectonly r = not (!(#escape(get r))) handle Escapemap => false
     fun enterREC(w,vl) = enter(w,{info=RECinfo vl,
				   escape=ref false, used = ref false})
     fun enterMISC w = enter(w,{info=MISCinfo, 
				   escape=ref false, used = ref false})
     fun enterFN(f,body) =
	enter (f, {info=FNinfo{body=body, calls=ref 0, argcount=ref BOT,
				    size=ref ~1, copy = ref false},
				          escape=ref false,used=ref false})
     fun setsize(f,n) =
	let val {info=FNinfo{size,...},...} = get f
	 in size := n; n
	end

     fun sum f =
	let fun h nil = 0 | h (a::r) = f a + h r
	 in h
	end

     val rec pass1 = 
      fn RECORD(vl,w,APP(f,args as w'::a)) =>
	   (enterREC(w, vl);
	    call f; app(escape o #1) vl; app escape args;
	    if w=w' then argcnt(f,COUNT(length vl)) else (onearg f);
	    length vl + length args + 2)
       | RECORD (vl,w,e) => (enterREC(w, vl);
			     app (escape o #1) vl; 
			     length vl + 1 + pass1 e)
       | SELECT (_,v,w,e) => (enterMISC w; use v; 1 + pass1 e)
       | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
       | APP(f,vl) => (call f; onearg f; app escape vl; 1 + length vl)
       | FIX(l, e) => (app (fn (f, vl, e) => (enterFN(f,(vl,e));
					      app enterMISC vl))
			    l;
		       sum (fn (f,vl,e) => setsize(f, pass1 e)) l
		       + pass1 e)
       | SWITCH(v,el) => (use v; length el + sum pass1 el)
       | PRIMOP(_,vl,wl,el) => (app escape vl; 
			        app enterMISC wl;
				length vl + length wl + sum pass1 el)

     fun replace f = (case get f
		       of {info = FNinfo{body=(vl,e),argcount=ref args,calls,
					    size=ref b, copy},
			    escape=ref false,...}
			     => if not (isrecursive f)
				    andalso (b < !bodysize orelse !calls < 2)
				     then BETA(vl,e,copy,false)
				else (case (args, vl)
				      of (COUNT j, v::_) =>
					   if j>1 andalso selectonly v
						andalso j+length(vl)-1 < maxfree-2
					    then FLATTENARGS j
					    else DROPARGS(map used vl)
				       | _ => DROPARGS(map used vl))
		        | {info = FNinfo{body=(vl,e),size=ref b, ...},...}
			     => if not (isrecursive f) andalso  b < !bodysize
				     then BETA(vl,e,ref true,true)
			        else USUAL
		        | _ => USUAL)
		     handle Escapemap => USUAL

     fun alphaConv(wl,e) =
      let open Intmap
	  exception Alpha
          val vm : lvar intmap = new Alpha
          val map = map vm and add = add vm
          fun use v = map v handle Alpha => v
          fun def v = let val v' = Access.dupLvar v 
		       in if isrecursive v then Intset.add rec_set v' else ();
			  add(v,v'); v'
		      end
          val rec g =
         fn RECORD(vl,w,ce) => RECORD(List.map (map1 use) vl, def w, g ce)
          | SELECT(i,v,w,ce) => SELECT(i, use v, def w, g ce)
          | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
          | APP(v,vl) => APP(use v, List.map use vl)
          | FIX(l,ce) => 
		     let fun h(f,vl,e) = (def f, List.map def vl, e)
		         fun h'(f,vl,e) = (f, vl, g e)
		      in FIX(List.map h' (List.map h l), g ce)
		     end
          | SWITCH(v,l) => SWITCH(use v, List.map g l)
          | PRIMOP(i,vl,wl,ce) => 
	         PRIMOP(i, List.map use vl, List.map def wl, List.map g ce)
       in (List.map def wl, g e)
      end

     exception Beta
     val m2 : lvar Intmap.intmap = Intmap.new Beta
     fun rename v = rename(Intmap.map m2 v) handle Beta => v
     fun newnames(v::vl, w::wl) = (Intmap.add m2 (v,w); newnames(vl,wl))
       | newnames(nil,nil) = ()
     
     val rec record = fn (vl,w,e) =>
         if selectonly w then (click "g"; g e)
	    else RECORD(map (map1 rename) vl, w, g e)

     and g = 
	    (* all paths = nil in vl *)
       fn RECORD(r as (vl,w, e as APP(f,args as w'::a))) =>
	    if w=w' andalso replace(rename f)=FLATTENARGS(length vl)
		then APP(rename f, map (rename o #1) vl @ map rename a)
		else record r
	| RECORD r =>  record r
	| SELECT(i,v,w,e) => 
	    if used w then
		    let val v' = rename v
		     in (case get v'
			 of {info=RECinfo vl,...} =>
			       (click "h"; 
				newnames([w],[rename(#1(nth(vl,i)))]);
				g e)
		          | _ => raise Escapemap)
			handle Escapemap => SELECT(i,v',w,g e)
		    end
		else (click "i"; g e)
	| OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
	| APP(f,vl) => (case replace(rename f)
		        of BETA(wl,e,copy,_) => 
			     if !copy
				then let val (wl',e') = alphaConv(wl,e)
				      in click "j";
					 newnames(wl', map rename vl); g e'
				     end
				else (copy := true; click "k";
				      newnames(wl, map rename vl); g e)
			 | DROPARGS bl => 
		                   APP(rename f, map rename(choose(vl,bl)))
			 | _ => APP(rename f, map rename vl))
	| FIX(l,e) =>
	    let fun h((f,vl,a)::r) = 
		  (case replace f
		    of BETA(_,_,_,false) => (click "l"; h r)
		     | DROPARGS bl => 
			    (if exists not bl then click "m" else ();
			     (f,choose(vl,bl),a) :: h r)
		     | FLATTENARGS k => 
			   let fun vars 0 = nil
				 | vars i = (Access.mkLvar()::vars(i-1))
			       val newargs = vars k
			    in click "n";
			      enterREC(hd vl, map (fn x =>(x,OFFp 0)) newargs);
			       (f, newargs @ tl vl, a) :: h r
			   end
		     | _ => (f,vl,a) :: h r)
		  | h nil = nil
	     in case h l
		 of nil => g e
		  | l' => FIX(map (fn(f,vl,a)=>(f,vl,g a)) l',  g e)
	    end
        | SWITCH(v,el) => 
		(case ctable (rename v)
		  of CO(INTconst i) => (click "o"; g(nth(el,i)))
		   | VA v' => SWITCH(v', map g el))
        | PRIMOP(i,vl,wl,el) => 
	  let val vl' = map rename vl
	   in (if !foldconst
	       then g((primops i) (map ctable vl', wl, el))
	       else raise Match)
	      handle Match => PRIMOP(i, vl', wl, map g el)
          end

    and primops =
	fn P.boxed => (fn ([CO(INTconst _)],[],[a,b]) => (click "p"; b)
		        | ([CO(STRINGconst s)],[],[a,b]) =>
			    (click "p"; if length s = 1 then b else a))
         | P.< => (fn ([CO(INTconst i),CO(INTconst j)],[],[a,b]) =>
	              (click "q"; if Integer.<(i,j) then a else b))
         | P.<= => (fn ([CO(INTconst i),CO(INTconst j)],[],[a,b]) =>
		   (click "r"; if Integer.<=(i,j) then a else b))
	 | P.>  => (fn ([CO(INTconst i),CO(INTconst j)],[],[a,b]) =>
	           (click "s"; if Integer.>(i,j) then a else b))
         | P.>= => (fn ([CO(INTconst i),CO(INTconst j)],[],[a,b]) =>
	           (click "t"; if Integer.>=(i,j) then a else b))
         | P.ieql => (fn ([CO(INTconst i),CO(INTconst j)],[],[a,b]) =>
		     (click "u"; if (fn (i,j:int) => i=j)(i,j) then a else b))
         | P.ineq => (fn ([CO(INTconst i),CO(INTconst j)],[],[a,b]) =>
		     (click "v"; if (fn (i,j:int) => i<>j)(i,j) then a else b))
         | P.sceql => (fn([CO(STRINGconst x),CO(STRINGconst y),_],[],[a,b])=>
		      (click "w"; if x=y then a else b))
         | P.* => (fn ([CO(INTconst 1), VA(v)],[w],[c]) =>
		      (click "x"; newnames([w],[v]); c)
		 | ([VA(v), CO(INTconst 1)],[w],[c]) =>
		      (click "x"; newnames([w],[v]); c)
		 | ([CO(INTconst 0), _],[w],[c]) =>
		      (click "x"; bindconst(w,INTconst 0); c)
		 | ([_, CO(INTconst 0)],[w],[c]) =>
		      (click "x"; bindconst(w,INTconst 0); c)
		 | ([CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   if (abs(i*j)<500000000) handle Overflow => false
		      then (click "x"; bindconst(w,INTconst(i*j)); c)
		      else raise Match)
	 | P.div => (fn ([VA(v), CO(INTconst 1)],[w],[c]) =>
		      (click "y"; newnames([w],[v]); c)
		 | ([CO(INTconst i),CO(INTconst j)],[w],[c]) =>
		   if (abs(i div j)< 500000000) handle Div => false
		   then (click "y"; bindconst(w,INTconst(i div j)); c)
		   else raise Match)
         | P.+ => (fn ([CO(INTconst 0), VA(v)],[w],[c]) =>
		      (click "z"; newnames([w],[v]); c)
		 | ([VA(v), CO(INTconst 0)],[w],[c]) =>
		      (click "z"; newnames([w],[v]); c)
		 | ([CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   if (abs(i+j)<500000000) handle Overflow => false
		      then (click "z"; bindconst(w,INTconst(i+j)); c)
		      else raise Match)
         | P.- => (fn ([VA(v), CO(INTconst 0)],[w],[c]) =>
		      (click "A";newnames([w],[v]); c)
		 | ([CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   if (abs(i-j)<500000000) handle Overflow => false
		      then (click "A"; bindconst(w,INTconst(i-j)); c)
		      else raise Match)
         | P.cast => (fn ([VA(v)],[w],[c]) => (click "B"; newnames([w],[v]); c)
		    | ([CO(k)],[w],[c]) => (click "B"; bindconst(w,k); c))
         | P.slength => (fn ([CO(INTconst _)],[w],[c]) =>
			 (click "C"; bindconst(w, INTconst 1); c)
		       | ([CO(STRINGconst s)], [w],[c]) =>
			 (click "C"; bindconst(w, INTconst(length s)); c))
         | P.ordof => (fn ([CO(STRINGconst s), CO(INTconst i)],[w],[c]) =>
			 (click "D"; bindconst(w, INTconst (ordof(s,i))); c))
         | P.~ => (fn ([CO(INTconst i)], [w], [c]) =>
		   if (abs i < 500000000) handle Overflow => false
		      then (click "E"; bindconst(w,INTconst(~i)); c)
		      else raise Match)
         | _ => raise Match

   fun inverseEta cexp =
     let exception Etasplit
	 val m2 : lvar Intmap.intmap = Intmap.new Etasplit
	 fun rename v = Intmap.map m2 v handle Etasplit => v
	 val newname = Intmap.add m2
	 val rec g = 
	   fn RECORD(vl,w,e) => RECORD(vl,w, g e)
	    | SELECT(i,v,w,e) => SELECT(i, v, w, g e)
	    | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
	    | APP(f,vl) => APP(rename f, vl)
	    | FIX(l,e) =>
		let fun h((ff as (f,vl,e))::r) = 
		       ((case get f
		         of {escape=ref false, info=FNinfo{calls=ref 0,...},
				    ...} => (click "F"; h r)
			  | {info=FNinfo{calls=ref 0,...},...} => ff::(h r)
			  | {escape = ref false,...} => ff::(h r)
			  | {info=FNinfo{calls,...},...} =>
			         let val vl' = map Access.dupLvar vl
				     val f' = Access.dupLvar f
				 in newname(f,f');
				     (f,vl',APP(f',vl'))::(f',vl,e)::(h r)
				 end
		       ) handle Escapemap => ff::(h r))
		      | h nil = nil
		    val l' = h l
		 in FIX(map (fn(f,vl,a)=>(f,vl,g a)) l',  g e)
		end
	    | SWITCH(v,el) => SWITCH(v, map g el)
	    | PRIMOP(i,vl,wl,el) => PRIMOP(i, vl, wl, map g el)
      in g cexp
     end

     val cexp' = (print "eta: "; eta cexp before print "\n")
     val done = !clicked
     val h = if donePass then cexp'
	     else (print(pass1 cexp'); print ": "; g cexp' before print "\n")
     val donewithpass = done = !clicked
 in
if !clicked<>0 andalso !clicked > !System.Control.CG.reducemore
    then reducepass (h,donewithpass)
    else if !etasplit
	 then (print "inverseEta: "; (inverseEta h) before print "\n")
	 else h
 end
in reducepass (cexp,false)
end

end (* structure CPSopt *)
