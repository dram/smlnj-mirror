(* Copyright 1989 by AT&T Bell Laboratories *)
(* All record paths must terminate in "OFFp 0" in cpsopt.sml *)

functor CPSopt(val maxfree : int) :
	sig val reduce : (CPS.const Intmap.intmap) -> CPS.cexp -> CPS.cexp
        end =
struct

 open Access CPS SortedList
 structure CG = System.Control.CG
 fun map1 f (a,b) = (f a, b)

 fun member(i : int, a::b) = i=a orelse member(i,b)
   | member(i,[]) = false

 fun choose(a::b,true::d) = a::choose(b,d)
   | choose(a::b,false::d) = choose(b,d)
   | choose _ = []

 fun sum f = let fun h [] = 0 
		   | h (a::r) = f a + h r
	     in h
	     end

 val debug = false (*System.Control.CG.printit*)
 fun debugprint s = if (*!*)debug then print(s:string) else ()
 fun debugflush() = if (*!*)debug then flush_out std_out else ()

fun reduce ctab cexp =
let
 val clicked = ref 0
 fun click (s:string) = (debugprint s; inc clicked)

 fun eta cexp =
  let exception M2
      val m : lvar Intmap.intmap = Intmap.new(32, M2)
      val name = Intmap.map m
      fun rename v = rename(name v) handle M2 => v
      fun newname x = (Access.sameName x; Intmap.add m x)
      val rec eta = 
	fn RECORD(vl,w,e) => RECORD(map (map1 rename) vl, w, eta e)
	 | SELECT(i,v,w,e) => SELECT(i, v, w, eta e)
	 | APP(f,vl) => APP(rename f, map rename vl)
	 | SWITCH(v,el) => SWITCH(v, map eta el)
	 | PRIMOP(i,vl,wl,el) => PRIMOP(i, map rename vl, wl, map eta el)
	 | FIX(l,e) =>
	     let fun split(f,vl,body, rest) =
			if !CG.etasplit
			 then let val vl' = map dupLvar vl 
			          and f' = dupLvar f
		               in (f,vl',APP(f',vl'))::(f',vl,body) :: rest
			      end
			 else (f,vl,body) :: rest
		  fun h((f,vl,body as APP(g,wl))::r) =
		     if wl=vl andalso not (member(g, f::vl)) andalso !CG.eta
		     	then (newname(f,rename g); h r)
		     	else split(f,vl,body, h r)
		   | h((f,vl,body)::r) = split(f,vl,body, h r)
		   | h [] = []
	      in case h l of
		  [] => eta e
		| l' => FIX(map (fn(f,vl,e)=>(f,vl,eta e)) l', eta e)
	     end
  in eta cexp
  end

 fun hoist x = if !CG.hoistup orelse !CG.hoistdown then Hoist.hoist click x
			else x
fun contract last cexp =
 let val mkconst = Intmap.add ctab
     datatype cv = CO of const | VA of lvar
     fun ctable v = CO(Intmap.map ctab v) handle Ctable => VA v
     fun isconst v = case ctable v of CO _ => true | VA _ => false
     datatype arity = BOT | COUNT of int | TOP | NOTUSED
     val botlist = if !CG.flattenargs then map (fn _ => BOT)
                                      else map (fn _ => TOP)
     datatype info = FNinfo of {arity: arity list ref,
			        args: lvar list,
			        body : cexp,
				reduce_ok : bool ref}
		   | RECinfo of (lvar * accesspath) list * (lvar * int) list ref
		   | SELinfo of int * lvar
		   | MISCinfo

     exception Escapemap
     val m : {info: info, used : int ref, escape : int ref} Intmap.intmap =
		     Intmap.new(128, Escapemap)
     val get = Intmap.map m
     val enter = Intmap.add m
     fun use v = inc(#used(get v)) handle Escapemap => ()
     fun used v = !(#used(get v)) > 0 handle Escapemap => true
     fun escape v = let val {escape,used,...} = get v
		    in inc escape; inc used
		    end handle Escapemap => ()
     fun escapes r = !(#escape(get r)) handle Escapemap => 0
     fun flatfun(f,n) =
	 (case get f of
	    {info=FNinfo{arity=ref al,reduce_ok=ref false,...},escape=ref 0,...} =>
	    (case nth(al,n) of
               COUNT i => 1
	     | _ => 0)
	  | _ => 0) handle Escapemap => 0
     fun selectonly r = 0 = !(#escape(get r)) handle Escapemap => false
     fun enterREC(w,vl) = enter(w,{info=RECinfo(vl,ref[]),escape=ref 0,used=ref 0})
     fun enterMISC w = enter(w,{info=MISCinfo, escape=ref 0, used = ref 0})
     fun enterSEL(v,i,w) = enter(v,{info=SELinfo(i,w), 
				    escape=ref 0, used = ref 0})
     fun enterFN (f,vl,cexp) =
		(enter(f,{escape=ref 0,used=ref 0,
		 	 info=FNinfo{arity=ref(botlist vl), args=vl, body=cexp,
			 reduce_ok=ref(!CG.betacontract)}});
		 app enterMISC vl)

     fun checkreduce(f,vl,body) =
	 case get f of
	   {escape=ref 0,used=ref i,
	    info=FNinfo{reduce_ok,arity as ref al,...},...} =>
		if i>1
		then
		  let fun loop(v::vl,a::al) =
			  if used v
			  then if selectonly v
			       then a::loop(vl,al)
			       else TOP::loop(vl,al)
			  else if !CG.dropargs then NOTUSED::loop(vl,al)
				else a::loop(vl,al)
		        | loop _ = []
		  in reduce_ok := false;
		     arity := loop(vl,al)
		  end
		else ()
	      | {info=FNinfo{reduce_ok,...},...} =>
		    (reduce_ok := false;
		     if last
		     then ()
		     else (case body of
		             APP(g,_) =>
			     (case get g of
				{info=FNinfo{reduce_ok,...},...} => 
				reduce_ok := false
			      | _ => ())
			   | _ => ()))

     exception ConstFold

     val rec pass1 = 
      fn RECORD(vl,w,e) => (enterREC(w,vl); app (escape o #1) vl; pass1 e)
       | SELECT (i,v,w,e) => (enterSEL(w,i,v); 
                              if !CG.selectopt then use v else escape v;
 			      pass1 e)
       | APP(f,vl) =>
	 ((case get f of
	     {info=FNinfo{arity as ref al,...},...} =>
	     let fun loop(TOP::r,v::vl,n) = TOP::loop(r,vl,n+1)
		   | loop(BOT::r,vl0 as v::vl, n) =
		     ((case get v
		        of {info=RECinfo(wl,_),...} =>
					loop(COUNT(length wl)::r,vl0,n)
		         | _ => raise Escapemap)
		      handle Escapemap => TOP::loop(r,vl,n+1))
		   | loop((cnt as COUNT a)::r,v::vl,n) = 
		     ((case get v of
		         {info=RECinfo(wl,flr as ref fl), ...} =>
			 if a = length wl
			 then (flr := (f,n)::fl; cnt::loop(r,vl,n+1))
			 else TOP::loop(r,vl,n+1)
		       | _ => raise Escapemap)
		      handle Escapemap => TOP::loop(r,vl,n+1))
		   | loop _ = []
	     in arity := loop(al,vl,0)
	     end
	   | _ => ())
	  handle Escapemap => ();
	  use f; app escape vl)
       | FIX(l, e) => (app enterFN l;
		       app (fn(* (f,vl,APP(g,wl)) => (use g; app escape wl)
			     |*) (f,vl,body) => pass1 body) l;
		       pass1 e;
		       app checkreduce l)
       | SWITCH(v,el) => (use v; app pass1 el)
       | PRIMOP(i,vl,wl,el) => (app escape vl; app enterMISC wl; app pass1 el)
       | OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"

     exception Beta
     val m2 : lvar Intmap.intmap = Intmap.new(32, Beta)
     fun ren v = ren(Intmap.map m2 v) handle Beta => v
     fun newname x = (Access.sameName x; Intmap.add m2 x)
     fun newnames(v::vl, w::wl) = (newname(v,w); newnames(vl,wl))
       | newnames _ = ()
     val one = let val x = mkLvar() in mkconst(x, INTconst 1); x end
     
     val rec reduce = fn cexp => g NONE cexp
     and g = fn hdlr =>
     let val rec g' =
       fn RECORD (vl,w,e) =>
	  let val {info=RECinfo(_,ref fl),escape=ref esc,...} = get w
	      fun elem(x,p) = (case (get(ren x), !CG.recordpath)
				of ({info=SELinfo(i,v),used=ref 1,...},true) =>
					(click "rp "; (ren v, SELp(i,p)))
				 | _ => raise Escapemap)
			      handle Escapemap => (ren x, p)
	  in if esc = sum flatfun fl andalso !CG.deadvars
	     then (click "rec "; g' e)
	     else RECORD(map elem vl, w, g' e)
	  end
        | SELECT(i,v,w,e) =>
	  if not(used w) andalso !CG.deadvars
          then (click "Sel "; g' e)
	  else let val v' = ren v
	       in (if not(!CG.selectopt) then raise Escapemap else ();
		  case get v'
		   of {info=RECinfo(vl,_),...} =>
		     let fun chase(x, OFFp 0) = (newname(w,ren x); g' e)
			   | chase(x, SELp(j,p)) =
				let val z = mkLvar()
				 in SELECT(j,ren x,z,chase(z,p))
				end
		      in click "sel "; chase(nth(vl,i))
		     end
	           | _ => raise Escapemap)
	          handle Escapemap => SELECT(i,v',w,g' e)
	       end
	| OFFSET _ => ErrorMsg.impossible "OFFSET in cpsopt"
	| APP(f,vl) =>
	  ((case get(ren f) of
	       {info=FNinfo{args,body,reduce_ok=ref true,...},...} =>
		(newnames(args, map ren vl); g' body)
             | {info=FNinfo{arity=ref al,...},escape=ref 0,...} =>
	       let fun loop(COUNT _ :: r,v::vl,args) =
		       let val {info=RECinfo(wl,_),...} = get(ren v)
			   fun f((x,OFFp 0)::wl',args) = f(wl',ren x :: args)
			     | f((x,SELp(i,p))::wl',args) =
				 let val z = mkLvar()
				  in SELECT(i,ren x,z,f((z,p)::wl',args))
				 end
			     | f(nil,args) = loop(r,vl,args)
		       in f(wl,args)
		       end
		     | loop(NOTUSED::r,v::vl,args) = loop(r,vl,args)
		     | loop(_::r,v::vl,args) = loop(r,vl, ren v :: args)
		     | loop (_,_,args) = APP(ren f, rev args)
	       in loop(al,vl,nil)
	       end
	     | _ => raise Escapemap)
	   handle Escapemap => APP(ren f, map ren vl))
	| FIX(l,e) =>
	  let fun h((f,vl,body)::r) = 
		 (case get f
		  of {info=FNinfo{reduce_ok=ref true,...},...} =>
		     (click "fn  "; h r)
		   | {info=FNinfo{arity=ref al,...},escape=ref 0,...} =>
	       	          let fun vars 0 = []
				| vars i = mkLvar()::vars(i-1)
			      fun newargs(COUNT j :: r,v::vl) =
				  let val new = vars j
				  in enterREC(v, map (fn x =>(x,OFFp 0)) new);
				     click "flt ";
				     new @ newargs(r,vl)
				  end
				| newargs(NOTUSED::r,v::vl) =
				     (click "drp "; newargs(r,vl))
				| newargs(_::r,v::vl) = v::newargs(r,vl)
				| newargs _ = []
			  in (f, newargs(al,vl), reduce body) :: h r
			  end
		   | _ => (f, vl, reduce body) :: h r)
		| h [] = []
	   in case h l of [] => g' e | l' => FIX(l', g' e)
	  end
        | SWITCH(v,el) => 
		(case ctable (ren v)
		  of CO(INTconst i) => 
			if !CG.switchopt then (click "swt "; g' (nth(el,i)))
				else SWITCH(ren v, map g' el)
		   | VA v' => SWITCH(v', map g' el)
		   | _ => ErrorMsg.impossible "3121 in cpsopt")
	| PRIMOP(P.gethdlr,vl,wl as [w],[e]) =>
	  (if !CG.handlerfold
	    then case hdlr of
	     NONE => if used w then PRIMOP(P.gethdlr,vl,wl,[g (SOME w) e])
		     else (click "gth "; g' e)
	   | SOME w' => (click "gth "; newname(w,w'); g' e)
	    else PRIMOP(P.gethdlr,vl,wl,[g (SOME w) e]))
	| PRIMOP(P.sethdlr,[v],wl,[e]) =>
	  let val v' = ren v
	      val e' = g (SOME v') e
	  in if !CG.handlerfold
	    then case hdlr of
	       NONE => PRIMOP(P.sethdlr,[v'],wl,[e'])
	     | SOME v'' => if v'=v'' then (click "sth "; e')
			   else PRIMOP(P.sethdlr,[v'],wl,[e'])
	    else PRIMOP(P.sethdlr,[v'],wl,[e'])
	  end
	| PRIMOP(i, vl, wl, el as [e1,e2]) => 
	      if e1 = e2 andalso !CG.branchfold
	      then (click "tst "; g' e1)
	      else let val vl' = map ren vl
	           in g' (if !CG.arithopt
			  then primops(i,map ctable vl', wl, el)
			  else raise ConstFold)
	              handle ConstFold => PRIMOP(i, vl', wl, map g' el)
	           end
        | PRIMOP(i, vl, wl as [w], el as [e]) =>
	  if not(used w) andalso Prim.pure i andalso !CG.deadvars
	    then (click "prm "; g' e)
	    else let val vl' = map ren vl
	          in g' (if !CG.arithopt
			 then primops(i,map ctable vl', wl, el)
			 else raise ConstFold)
	             handle ConstFold => PRIMOP(i, vl', wl, map g' el)
	         end
        | PRIMOP(i,vl,wl,el) =>
		 let val vl' = map ren vl
	          in g' (if !CG.arithopt
			 then primops(i,map ctable vl', wl, el)
			 else raise ConstFold)
	             handle ConstFold => PRIMOP(i, vl', wl, map g' el)
	         end
      in g'
     end

     and primops =
	fn (P.boxed, CO(INTconst _)::_,_,_::b::_) => (click "A"; b)
	 | (P.boxed, CO(STRINGconst s)::_,_,a::b::_) =>
			    (click "A"; if size s = 1 then b else a)
	 | (P.boxed, VA v :: _,_,a::_) => 
            ((case get v of
		  {info=RECinfo _, ...} => (click "A"; a)
	      | _ => raise ConstFold)
	     handle Escapemap => raise ConstFold)
         | (P.<, [CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
	              (click "B"; if Integer.<(i,j) then a else b)
         | (P.<=, [CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
		   (click "C"; if Integer.<=(i,j) then a else b)
	 | (P.> , [CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
	           (click "D"; if Integer.>(i,j) then a else b)
         | (P.>=, [CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
	           (click "E"; if Integer.>=(i,j) then a else b)
	 | (P.rangechk, [CO(INTconst i), CO(INTconst j)],_,[a,b]) =>
		   (click "V"; if 0 <= i andalso i < j then a else b)
	 | (P.rangechk, [CO(INTconst i), VA _],_,[a,b]) =>
		   if i < 0 then (click "V"; b) else raise ConstFold
	 | (P.rangechk, [VA _, CO(INTconst j)],_,[a,b]) =>
		   if j <= 0 then (click "V"; b) else raise ConstFold
         | (P.ieql, [CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
		     (click "F"; if i=j then a else b)
         | (P.ineq, [CO(INTconst i),CO(INTconst j)],_,[a,b]) =>
		     (click "G"; if i=j then b else a)
         | (P.<, [VA v,VA w],_,[a,b]) =>
                    if v=w then (click "U"; b) else raise ConstFold
         | (P.<=, [VA v,VA w],_,[a,b]) =>
                    if v=w then (click "U"; a) else raise ConstFold
         | (P.> , [VA v,VA w],_,[a,b]) =>
                    if v=w then (click "U"; b) else raise ConstFold
         | (P.>=, [VA v,VA w],_,[a,b]) =>
                    if v=w then (click "U"; a) else raise ConstFold
         | (P.ieql, [VA v,VA w],_,[a,b]) =>
                    if v=w then (click "U"; a) else raise ConstFold
         | (P.ineq, [VA v,VA w],_,[a,b]) =>
                    if v=w then (click "U"; b) else raise ConstFold
         | (P.*, [CO(INTconst 1), VA(v)],[w],[c]) =>
		      (click "H"; newname(w,v); c)
	 | (P.*, [VA(v), CO(INTconst 1)],[w],[c]) =>
		      (click "H"; newname(w,v); c)
	 | (P.*, [CO(INTconst 0), _],[w],[c]) =>
		   (click "H"; mkconst(w,INTconst 0); c)
	 | (P.*, [_, CO(INTconst 0)],[w],[c]) =>
		      (click "H"; mkconst(w,INTconst 0); c)
	 | (P.*, [CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   (let val x = i*j
		    in x+x; mkconst(w,INTconst x); click "H"; c
		    end handle Overflow => raise ConstFold)
	 | (P.div, [VA(v), CO(INTconst 1)],[w],[c]) =>
		      (click "I"; newname(w,v); c)
	 | (P.div, [CO(INTconst i),CO(INTconst j)],[w],[c]) =>
		   (let val x = i quot j
		    in click "I"; mkconst(w,INTconst x); c
		    end handle Div => raise ConstFold)
         | (P.+, [CO(INTconst 0), VA(v)],[w],[c]) =>
		   (click "J"; newname(w,v); c)
	 | (P.+, [VA(v), CO(INTconst 0)],[w],[c]) =>
		   (click "J"; newname(w,v); c)
	 | (P.+, [CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		   (let val x = i+j
		    in x+x; mkconst(w,INTconst x); click "J"; c
		    end handle Overflow => raise ConstFold)
         | (P.-, [VA(v), CO(INTconst 0)],[w],[c]) =>
		      (click "K";newname(w,v); c)
	 | (P.-, [CO(INTconst i),CO(INTconst j)], [w], [c]) =>
		  (let val x = i-j
		   in x+x; mkconst(w,INTconst x); click "K"; c
		   end handle Overflow => raise ConstFold)
	 | (P.rshift, [CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			   (click "L"; mkconst(w,INTconst(Bits.rshift(i,j))); c)
	 | (P.rshift, [CO(INTconst 0), VA v],[w],[c]) =>
			   (click "L"; mkconst(w,INTconst 0); c)
	 | (P.rshift, [VA v, CO(INTconst 0)],[w],[c]) =>
			   (click "L"; newname(w,v); c)
         | (P.slength, [CO(INTconst _)],[w],[c]) =>
			 (click "M"; mkconst(w, INTconst 1); c)
	 | (P.slength, [CO(STRINGconst s)], [w],[c]) =>
			 (click "M"; mkconst(w, INTconst(size s)); c)
         | (P.ordof, [CO(STRINGconst s), CO(INTconst i)],[w],[c]) =>
			 (click "N"; mkconst(w, INTconst (ordof(s,i))); c)
         | (P.~, [CO(INTconst i)], [w], [c]) =>
		     (let val x = ~i
		      in x+x; mkconst(w,INTconst x); click "O"; c
		      end handle Overflow => raise ConstFold)
	 | (P.lshift, [CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			   (let val x = Bits.lshift(i,j)
			    in x+x; mkconst(w,INTconst x); click "P"; c
			    end handle Overflow => raise ConstFold)
	 | (P.lshift, [CO(INTconst 0), VA v],[w],[c]) =>
			   (click "P"; mkconst(w,INTconst 0); c)
	 | (P.lshift, [VA v, CO(INTconst 0)],[w],[c]) =>
			   (click "P"; newname(w,v); c)
	 | (P.orb, [CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			(click "Q"; mkconst(w,INTconst(Bits.orb(i,j))); c)
	 | (P.orb, [CO(INTconst 0),VA v],[w],[c]) =>
			(click "Q"; newname(w,v); c)
	 | (P.orb, [VA v, CO(INTconst 0)],[w],[c]) =>
			(click "Q"; newname(w,v); c)
	 | (P.xorb, [CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			 (click "R"; mkconst(w,INTconst(Bits.xorb(i,j))); c)
	 | (P.xorb, [CO(INTconst 0),VA v],[w],[c]) =>
			(click "R"; newname(w,v); c)
	 | (P.xorb, [VA v, CO(INTconst 0)],[w],[c]) =>
			(click "R"; newname(w,v); c)
	 | (P.notb, [CO(INTconst i)], [w], [c]) =>
			 (mkconst(w,INTconst(Bits.notb i)); click "S"; c)
	 | (P.andb, [CO(INTconst i),CO(INTconst j)],[w],[c]) =>
			 (click "T"; mkconst(w,INTconst(Bits.andb(i,j))); c)
	 | (P.andb, [CO(INTconst 0),VA v],[w],[c]) =>
			(click "T"; mkconst(w,INTconst 0); c)
	 | (P.andb, [VA v, CO(INTconst 0)],[w],[c]) =>
			(click "T"; mkconst(w,INTconst 0); c)
         | _ => raise ConstFold

    val _ = (debugprint "\nContract: "; debugflush())
  in (pass1 cexp; reduce cexp)
  end

fun expand(cexp,bodysize) =
   let
     datatype info = Fun of {escape: int ref, call: int ref, size: int ref,
		         args: lvar list, body: cexp}
	           | Arg of {escape: int ref, savings: int ref,
		             record: (int * lvar) list ref}
	           | Sel of {savings: int ref}
		   | Rec of {escape: int ref, size: int,
			     vars: (lvar * accesspath) list}

     exception Expand
     val m : info Intmap.intmap = Intmap.new(128,Expand)
     val get = Intmap.map m
     fun call(v,args) = (case get v
	            of Fun{call,...} => inc call
		     | Arg{savings,...} => savings := !savings+1
		     | Sel{savings} => savings := !savings+1
		     | Rec _ => ()  (* impossible *)
	 	  ) handle Expand => ()
     fun escape v = (case get v
	            of Fun{escape,...} => inc escape
		     | Arg{escape,...} => inc escape
		     | Sel _ => ()
		     | Rec{escape,...} => inc escape
		  ) handle Expand => ()
     fun escapeargs v = (case get v
	                 of Fun{escape,...} => inc escape
		       | Arg{escape,savings, ...} =>
			     (inc escape; savings := !savings + 1)
		       | Sel{savings} => savings := !savings + 1
		       | Rec{escape,...} => inc escape)
			 handle Expand => ()
     fun unescapeargs v = (case get v
	                 of Fun{escape,...} => dec escape
		       | Arg{escape,savings, ...} =>
			     (dec escape; savings := !savings - 1)
		       | Sel{savings} => savings := !savings - 1
		       | Rec{escape,...} => dec escape)
			 handle Expand => ()
     fun setsize(f,n) = case get f of Fun{size,...} => (size := n; n)
     fun enter (f,vl,e) = (Intmap.add m(f,Fun{escape=ref 0, call=ref 0, size=ref 0,
					      args=vl, body=e});
			   app (fn v => Intmap.add m (v,
					Arg{escape=ref 0,savings=ref 0,
					    record=ref []})) vl)
     fun noterec(w, vl, size) = Intmap.add m (w,Rec{size=size,escape=ref 0,vars=vl})
     fun notesel(i,v,w) = (Intmap.add m (w, Sel{savings=ref 0});
		     (case get v of
                        Arg{savings,record,...} => (inc savings;
						    record := (i,w)::(!record))
                      | _ => ()) handle Expand => ())
     fun save(v,k) = (case get v
		       of Arg{savings,...} => savings := !savings + k
		        | Sel{savings} => savings := !savings + k
		        | _ => ()
		     ) handle Expand => ()
     fun nsave(v,k) = (case get v
		       of Arg{savings,...} => savings := k
		        | Sel{savings} => savings := k
		        | _ => ()
		     ) handle Expand => ()
     fun savesofar v = (case get v 
		       of Arg{savings,...} => !savings
		        | Sel{savings} => !savings
		        | _ => 0
		     ) handle Expand => 0
     val rec prim = fn (_,vl,wl,el) =>
	 let fun vbl v = (Intmap.map ctab v; 0)
			  handle Ctable =>
			    ((case get v of
                                Rec _ => 0
			      | _ => 1) handle Expand => 1)
	     val nonconst = sum vbl vl
	     val len = length el
	     val sl = map savesofar vl
	     val branches = sum pass1 el
	     val zl = map savesofar vl
	     val overhead = length vl + length wl
	     val potential = overhead + (branches*(len-1)) quot len
	     val savings = case nonconst of
		             1 => potential
			   | 2 => potential div 4
			   | _ => 0
	     fun app3 f = let fun loop (a::b,c::d,e::r) = (f(a,c,e); loop(b,d,r))
				| loop _ = ()
			  in loop
			  end
	 in app3(fn (v,s,z)=> nsave(v,s + savings + (z-s) quot len)) (vl,sl,zl);
	    overhead+branches
	 end

     and pass1 = 
      fn RECORD(vl,w,e) =>
	  (app (escape o #1) vl;
	   noterec(w,vl,length vl);
	   2 + length vl + pass1 e)
       | SELECT (i,v,w,e) => (notesel(i,v,w); 1 + pass1 e)
       | APP(f,vl) => (call(f,length vl); app escapeargs vl; 1 + length vl)
       | FIX(l, e) => 
	    (app enter l; 
             sum (fn (f,_,e) => setsize(f, pass1 e)) l + length l + pass1 e)
       | SWITCH(v,el) => let val len = length el
			     val jumps = 4 + len
		             val branches = sum pass1 el
			  in save(v, (branches*(len-1)) quot len + jumps);
			     jumps+branches
			 end
       | PRIMOP(args as (P.boxed,_,_,_)) => prim args
       | PRIMOP(args as (P.<,_,_,_)) => prim args
       | PRIMOP(args as (P.<=,_,_,_)) => prim args
       | PRIMOP(args as (P.>,_,_,_)) => prim args
       | PRIMOP(args as (P.>=,_,_,_)) => prim args
       | PRIMOP(args as (P.ieql,_,_,_)) => prim args
       | PRIMOP(args as (P.ineq,_,_,_)) => prim args
       | PRIMOP(args as (P.*,_,_,_)) => prim args
       | PRIMOP(args as (P.div,_,_,_)) => prim args
       | PRIMOP(args as (P.+,_,_,_)) => prim args
       | PRIMOP(args as (P.-,_,_,_)) => prim args
       | PRIMOP(args as (P.rshift,_,_,_)) => prim args
       | PRIMOP(args as (P.slength,_,_,_)) => prim args
       | PRIMOP(args as (P.ordof,_,_,_)) => prim args
       | PRIMOP(args as (P.~,_,_,_)) => prim args
       | PRIMOP(args as (P.lshift,_,_,_)) => prim args
       | PRIMOP(args as (P.orb,_,_,_)) => prim args
       | PRIMOP(args as (P.xorb,_,_,_)) => prim args
       | PRIMOP(args as (P.notb,_,_,_)) => prim args
       | PRIMOP(args as (P.andb,_,_,_)) => prim args
       | PRIMOP(_,vl,wl,el) =>
	 (app escape vl; length vl + length wl + sum pass1 el)

     fun substitute(args,wl,e) =
      let exception Alpha
          val vm : lvar Intmap.intmap = Intmap.new(16, Alpha)
          fun use v = Intmap.map vm v handle Alpha => v
          fun def v = let val w = dupLvar v 
		      in Intmap.add vm (v,w); w
		      end
	  fun bind(a::args,w::wl) = (Intmap.add vm (w,a); bind(args,wl))
	    | bind _ = ()
          val rec g =
         fn RECORD(vl,w,ce) => RECORD(map (map1 use) vl, def w, g ce)
          | SELECT(i,v,w,ce) => SELECT(i, use v, def w, g ce)
          | APP(v,vl) => APP(use v, map use vl)
          | FIX(l,ce) => 
	    let fun h1(f,vl,e) = (f,def f, vl, e)
		fun h2(f,f',vl,e) =
		    let val vl' = map def vl
			val e'= g e
		    in (f', vl', e')
		    end
	     in FIX(map h2(map h1 l), g ce)
	    end
          | SWITCH(v,l) => SWITCH(use v, map g l)
          | PRIMOP(i,vl,wl,ce) => PRIMOP(i, map use vl, map def wl, map g ce)
      val cexp = (bind(args,wl); g e)
      in debugprint(makestring(pass1 cexp)); debugprint " "; cexp
      end
		
     fun beta(n, d, e) = case e
      of RECORD(vl,w,ce) => RECORD(vl, w, beta(n,d+2+length vl, ce))
       | SELECT(i,v,w,ce) => SELECT(i, v, w, beta(n,d+1, ce))
       | APP(v,vl) => 
	   ((case get v
	     of Fun{escape,call as ref calls,size,args,body} =>
		let val size = !size
		    fun chasepath(v,OFFp 0) = v
		      | chasepath(v,SELp(i,p)) =
			   case get v
			    of Rec{vars,...} =>
				   chasepath(chasepath(nth(vars,i)),p)
			     | _ => raise Expand
		    fun whatsave(acc, v::vl, a::al) =
			if acc>=size
			then acc
			else
			(case get a of
			   Arg{escape=ref esc,savings=ref save,record=ref rl} =>
                           let val (this,nvl,nal) =
			       if (Intmap.map ctab v; true) handle Ctable => false
			       then (save,vl,al)
			       else (case get v of
			               Fun{escape=ref 1,...} =>
                                         (if esc>0 then save else 6+save,vl,al)
				     | Fun _ => (save,vl,al)
				     | Rec{escape=ref ex,vars,size} =>
				       let fun loop([],nvl,nal) = 
					       (if ex>1 orelse esc>0
					        then save
						else save+size+2,nvl,nal)
					     | loop((i,w)::rl,nvl,nal) =
						loop(rl,
						   chasepath(nth(vars,i))::nvl,
						     w::nal)
				       in loop(rl,vl,al)
				       end
                                     | _ => (0,vl,al)) handle Expand => (0,vl,al)
			   in whatsave(acc + this - (acc*this) quot size, nvl,nal)
			   end
			 | Sel{savings=ref save} =>
                           let val this =
			       if (Intmap.map ctab v; true) handle Ctable => false
			       then save
			       else (case get v of
				       Fun _ => save
				     | Rec _ => save
                                     | _ => 0) handle Expand => 0
			   in whatsave(acc + this - (acc*this) quot size, vl,al)
			   end)
		      | whatsave(acc,_,_) = acc
		  val predicted = calls*((size-whatsave(0,vl,args))-(1+length vl))
                  val depth = 2
		  val max = 5
		  val increase = (bodysize*(depth - n)) quot depth
	     in if (predicted <= increase
		    orelse (!escape=0 andalso
			    (calls = 1
		    orelse (case vl of
			      [_] => calls = 2 andalso
				     predicted - (size+1) <= increase
			    | _ => false))))
		    andalso (n <= max orelse (debugprint "n>";
			   		      debugprint(makestring max);
					      debugprint "\n"; false))
		then let val new = beta(n+1, d+1, substitute(vl,args,body))
		     in click "";
		        call := calls-1;
		        app unescapeargs vl;
		        new
		     end
		else e
	    end
	    | _ => e) handle Expand => e)
       | FIX(l,ce) => let fun h(f,vl,e) = 
			     case get f
			      of Fun{escape=ref 0,...} => (f,vl, beta(n,0,e))
			       | _ => (f,vl,e)
		       in FIX(if n<1 then map h l else l, 
			      beta(n,d+length l, ce))
		      end
       | SWITCH(v,l) => SWITCH(v, map (fn e => beta(n,d+2,e)) l)
       | PRIMOP(i,vl,wl,ce) => PRIMOP(i, vl, wl, map (fn e => beta(n,d+2,e)) ce)

    in debugprint("\nExpand ("); debugprint(makestring(pass1 cexp));
       debugprint("): "); debugflush();
       beta(0,0,cexp)
   end

  val bodysize = !System.Control.CG.bodysize
  val rounds = !System.Control.CG.rounds
  val reducemore = !System.Control.CG.reducemore

  fun contracter last cexp =
	 let val cexp = (clicked := 0; contract false cexp)
	  in if !clicked <= reducemore
	     then if last
		  then contract last cexp
		  else cexp
	     else contracter last cexp
	 end

  fun cycle(0,cexp) = contract true cexp
    | cycle(1,cexp) = 
	let val _ = debugprint "\nHoist: "
	    val cexp = hoist cexp
	    val _ = clicked := 0
	    val cexp = if !CG.betaexpand then expand(cexp,bodysize quot rounds)
			else cexp
	    val cl = !clicked before clicked := 0
        in if cl <= reducemore
	   then contract true cexp
	   else contracter true cexp
	end
    | cycle(k,cexp) = 
	let val _ = debugprint "\nHoist: "
	    val cexp = hoist cexp
	    val _ = clicked := 0
	    val cexp = if !CG.betaexpand then expand(cexp,(bodysize * k) quot rounds)
			else cexp
	    val cl = !clicked before clicked := 0
        in if cl <= reducemore
	   then contract true cexp
	   else cycle(k-1, contract false cexp)
	end
in (if rounds < 0 then 
      cexp
    else cycle(rounds,contracter false (eta cexp)))
   before (debugprint "\n"; debugflush())
end
end
