(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPSopt(val maxfree : int) :
	sig val reduce : CPS.function * System.Unsafe.object option * bool
					-> CPS.function
        end =
struct


structure Fastlib = struct

structure Ref = 
  struct
    open Ref
    fun inc r = r := !r + 1
    fun dec r = r := !r + 1
  end

structure List : LIST =
  struct
    open List
    fun hd (a::r) = a | hd nil = raise Hd
    fun tl (a::r) = r | tl nil = raise Tl    
    fun null nil = true | null _ = false
    fun length l = 
	let fun j(k,nil) = k
	      | j(k, a::x) = j(k+1,x)
	 in j(0,l)
	end
    fun op @ (nil,l) = l
      | op @ (a::r, l) = a :: (r@l)
    fun rev l =
	let fun f (nil, h) = h
	      | f (a::r, h) = f(r, a::h)
	in  f(l,nil)
	end
    fun map f =
	let fun m nil = nil
	      | m (a::r) = f a :: m r
	in  m
	end
    fun fold f [] = (fn b => b)
      | fold f (a::r) = (fn b => let fun f2(e,[]) = f(e,b)
				       | f2(e,a::r) = f(e,f2(a,r))
				 in f2(a,r)
				 end)
    fun revfold f [] = (fn b => b)
      | revfold f (a::r) = (fn b => let fun f2(e,[],b) = f(e,b)
					  | f2(e,a::r,b) = f2(a,r,f(e,b))
				    in f2(a,r,b)
				    end)	
    fun app f = let fun a2 (e::r) = (f e; a2 r) | a2 nil = () in a2 end
    fun revapp f = let fun a2 (e::r) = (a2 r; f e; ()) | a2 nil = () in a2 end
    fun nthtail(e,0) = e 
      | nthtail(e::r,n) = nthtail(r,n-1)
      | nthtail _ = raise NthTail
    fun nth x = hd(nthtail x) handle NthTail => raise Nth | Hd => raise Nth
    fun exists pred =
	let fun f nil = false
	      | f (hd::tl) = pred hd orelse f tl
	in  f
	end
  end


structure General =
  struct
    open General
    fun f o g = fn x => f(g x)
    fun a before b = a
  end (* structure General *)


 val inc = Ref.inc
 val dec = Ref.dec
 val hd = List.hd and tl = List.tl
 val null = List.null and length = List.length
 val op @ = List.@ and rev = List.rev
 val map = List.map and fold = List.fold and revfold=List.revfold
 val app = List.app and revapp = List.revapp
 val nthtail = List.nthtail and nth = List.nth and exists = List.exists
 val op o = General.o and op before = General.before
 val min = Integer.min and max = Integer.max 

end
 open Fastlib

 open Access CPS
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


 fun last0[x]=x | last0(a::b)=last0 b | last0 _ = 0
			  
 fun sameName(x,VAR y) = Access.sameName(x,y) 
   | sameName(x,LABEL y) = Access.sameName(x,y) 
   | sameName _ = ()

 val equalUptoAlpha =
 let fun equ pairs =
        let fun same(VAR a, VAR b) = 
	          let fun look((x,y)::rest) = a=x andalso b=y orelse look rest
		        | look nil = false
		   in a=b orelse look pairs
		  end
	      | same(a,b) = a=b
            fun samefields((a,ap)::ar,(b,bp)::br) = ap=bp andalso same(a,b)
		                                     andalso samefields(ar,br)
              | samefields(nil,nil) = true
              | samefields _ = false
	    fun samewith p = equ (p::pairs)
            fun all2 f (e::r,e'::r') = f(e,e') andalso all2 f (r,r')
              | all2 f (nil,nil) = true
              | all2 f _ = false
            val rec sameexp = 
	     fn (SELECT(i,v,w,e),SELECT(i',v',w',e')) =>
		     i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (RECORD(vl,w,e),RECORD(vl',w',e')) =>
		     samefields(vl,vl') andalso samewith (w,w') (e,e')
              | (OFFSET(i,v,w,e),OFFSET(i',v',w',e')) =>
		     i=i' andalso same(v,v') andalso samewith(w,w') (e,e')
              | (SWITCH(v,el),SWITCH(v',el')) =>
		    same(v,v') andalso all2 sameexp (el,el')
	      | (APP(f,vl),APP(f',vl')) => same(f,f') andalso all2 same (vl,vl')
              | (FIX(l,e),FIX(l',e')) => (* punt! *) false
              | (PRIMOP(i,vl,nil,el),PRIMOP(i',vl',nil,el')) =>
		    i=i' andalso all2 same (vl,vl')
		    andalso all2 sameexp(el,el')
              | (PRIMOP(i,vl,[w],el),PRIMOP(i',vl',[w'],el')) =>
		    i=i' andalso all2 same (vl,vl')
		    andalso all2 (samewith(w,w')) (el,el')
	      | _ => false
         in sameexp
        end
   in equ nil
  end

fun reduce (function, _, afterClosure) =
let
 val debug = false
 fun debugprint s = if debug then print(s:string) else ()
 fun debugflush() = if debug then flush_out std_out else ()
 val clicked = ref 0
 fun click (s:string) = (debugprint s; inc clicked)

     val say = debugprint
     fun sayv(VAR v) = say(Access.lvarName v)
        | sayv(LABEL v) = say("(L)" ^ Access.lvarName v)
	| sayv(INT i) = say(makestring i)
	| sayv(REAL r) = say r
	| sayv(STRING s) = (say "\""; say s; say "\"")

 val label = if afterClosure then LABEL else VAR

 fun eta(fvar, fargs, cexp) =
  let exception M2
      val m : value Intmap.intmap = Intmap.new(32, M2)
      val name = Intmap.map m
      fun rename(v0 as VAR v) = (rename(name v) handle M2 => v0)
        | rename(v0 as LABEL v) = (rename(name v) handle M2 => v0)
        | rename x = x
      fun newname x = (sameName x; Intmap.add m x)
      val rec eta = 
	fn RECORD(vl,w,e) => RECORD(map (map1 rename) vl, w, eta e)
	 | SELECT(i,v,w,e) => SELECT(i, v, w, eta e)
	 | OFFSET(i,v,w,e) => OFFSET(i, v, w, eta e)
	 | APP(f,vl) => APP(rename f, map rename vl)
	 | SWITCH(v,el) => SWITCH(v, map eta el)
	 | PRIMOP(i,vl,wl,el) => PRIMOP(i, map rename vl, wl, map eta el)
	 | FIX(l,e) =>
	     let fun split1(f,vl,body,rest) =
			if !CG.etasplit
			 then let val vl' = map dupLvar vl 
			          and f' = dupLvar f
		               in (f,vl',APP(label f', map VAR vl'))
				  ::(f',vl,body)::rest
			      end
			 else (f,vl,body) :: rest
		 fun split(f,vl,body as FIX([(g,[b,k],body2)],
					    APP(VAR c,[VAR g'])),
			   rest) =
		       if last0 vl = c andalso g=g' 
			     andalso !System.Control.CG.uncurry
			then let val b'=dupLvar b
			         and vl' = map dupLvar vl
				 and k'=dupLvar k
				 and g'=dupLvar g
				 and f'=dupLvar f
			     in (f,vl',FIX([(g',[b',k'],
                                               APP(VAR f',
					           map VAR vl' @ 
						   [VAR g',VAR b',VAR k']))],
					        APP(VAR(last0 vl'),[VAR g'])))
			         ::split(f',vl@[g,b,k],body2,rest)
			     end
			 else split1(f,vl,body,rest)
		   | split(f,vl,body,rest) = split1(f,vl,body,rest)

		  fun h((f,vl,body as APP(VAR g, wl))::r) =
		     if wl= map VAR vl andalso not (member(g, f::vl)) andalso !CG.eta
		     	then (newname(f,rename(VAR g)); h r)
		     	else (f,vl,body):: h r
		   | h((f,vl,body as APP(LABEL g, wl))::r) =
		     if wl= map VAR vl andalso not (member(g, f::vl)) andalso !CG.eta
		     	then (newname(f,rename(LABEL g)); h r)
		     	else (f,vl,body):: h r
		   | h((f,vl,body)::r) = split(f,vl,body, h r)
		   | h [] = []
	      in case h l of
		  [] => eta e
		| l' => FIX(map (fn(f,vl,e)=>(f,vl,eta e)) l', eta e)
	     end
  in (fvar, fargs, eta cexp)
  end

     datatype arity = BOT 
                    | UNK  (* an arg seen that isn't a known record *)
                    | COUNT of int * bool (* int is # of record fields;
			   bool is whether any arguments were unknown records*)
                    | TOP

     datatype info = FNinfo of {arity: arity list ref,
			        args: lvar list,
			        body : cexp,
				reduce_ok : bool ref,
				specialuse: int ref}
		   | RECinfo of (value * accesspath) list * (value * int) list ref
		   | SELinfo of int * value
		   | ARGinfo of {biggestSEL: int ref}
		   | INTinfo of int * int
		   | MISCinfo

 local fun double x = double(x+x) handle Overflow => x
   in val minint = double(~1)
      val maxint = ~(minint+1)
      val defaultINTinfo = INTinfo(minint,maxint)
      val nonnegINTinfo = INTinfo(0,maxint)
  end

 fun hoist(fvar,fargs,x) =
    if !CG.hoistup orelse !CG.hoistdown then (fvar,fargs, Hoist.hoist click x)
	else (fvar,fargs,x)

fun contract last (fvar, fargs, cexp) =
 let val botlist = if !CG.flattenargs then map (fn _ => BOT)
                                      else map (fn _ => TOP)
     exception Escapemap
     val m : {info: info, used : int ref, escape : int ref} Intmap.intmap =
		     Intmap.new(128, Escapemap)
     val get = Intmap.map m
     val get = fn i => get i handle Escapemap => ErrorMsg.impossible
	                                  ("Escapemap on " ^ makestring i)
     val enter = Intmap.add m
     fun use(VAR v) = inc(#used(get v))
       | use(LABEL v) = inc(#used(get v))
       | use _ = ()
     fun used v = !(#used(get v)) > 0
	
     fun escape(VAR v) = let val {escape,used,...} = get v
		   	  in inc escape; inc used
		    	 end
       | escape(LABEL v) = escape(VAR v)
       | escape _ = ()
     fun flatfun(VAR f,n) =
	  (case get f of
	    {info=FNinfo{arity=ref al,reduce_ok=ref false,...},escape=ref 0,...} =>
	    (case nth(al,n) of
               COUNT _ => 1
	     | _ => 0)
	  | _ => 0)
       | flatfun(LABEL f, n) = flatfun(VAR f, n)
       | flatfun(_,_) = 0

     fun selectFrom(VAR v,i) =
	   let val {info,used,escape,...} = get v
	   in if !CG.selectopt then inc used else inc escape;
	      case info
	       of ARGinfo{biggestSEL as ref j} => biggestSEL := max(i,j)
                | _ => ()
           end
       | selectFrom(LABEL v, i) = selectFrom(VAR v, i)
       | selectFrom _ = ()
					 
     fun enterField(v, SELp(i,_)) = selectFrom(v,i)
       | enterField(v, _) = escape(v)

     fun enterREC(w,vl) = enter(w,{info=RECinfo(vl,ref[]),escape=ref 0,used=ref 0})
     fun enterARG w = enter(w,{info=ARGinfo{biggestSEL=ref ~1},
			       escape=ref 0, used = ref 0})
     fun enterMISC w = enter(w,{info=MISCinfo, escape=ref 0, used = ref 0})
				
     fun enterINT(w,lo,hi) = enter(w,{info=INTinfo(lo,hi),
				      escape=ref 0, used = ref 0})
				
     fun enterFN (f,vl,cexp) =
		(enter(f,{escape=ref 0,used=ref 0,
		 	 info=FNinfo{arity=ref(botlist vl), args=vl, body=cexp,
			 specialuse=ref 0,reduce_ok=ref(!CG.betacontract)}});
		 app enterARG vl)

    local exception Found
    in fun findFetch(v,k) body =
	 (* find whether field k of variable v is guaranteed to exist *)
	 let fun f(RECORD(fields,_,e)) = (app g fields; f e)
	    | f(SELECT(i,VAR v',w,e)) = if v=v' andalso i=k then raise Found
	                                   else f e
	    | f(SELECT(_,_,_,e)) = f e
	    | f(OFFSET(_,_,_,e)) = f e
            | f(FIX(_,e)) = f e
	    | f(PRIMOP(_,_,_,[e])) = f e
            | f(PRIMOP(_,_,_,el)) = not(exists (not o findFetch(v,k)) el)
            | f(SWITCH(_,el)) = not(exists (not o findFetch(v,k)) el)
	    | f _ = false
          and g(VAR v',SELp(i,_)) = 
                         if v=v' andalso i=k then raise Found else ()
	    | g _  = ()
	 in f body 
            handle Found => true
         end
    end

     fun don'tReduce(VAR g) = (case get g
				of {info=FNinfo{reduce_ok,...},...} => 
				       reduce_ok := false
			         | _ => ())
       | don'tReduce(LABEL g) = don'tReduce(VAR g)
       | don'tReduce _ = ()

     fun SELECTandAPP(APP(g,_)) = SOME g
       | SELECTandAPP(SELECT(_,_,_,e)) = SELECTandAPP e
       | SELECTandAPP _ = NONE

     fun checkreduce(f,vl,body) =
	   (if last then () 
	    else case (vl,body)
             of ([a,c],FIX([(h,[b,k],body)],APP(VAR c',[VAR h']))) =>
		    (case (c=c' andalso h=h', SELECTandAPP(body))
		      of (true,SOME g) => don'tReduce g
                       | _ => ())
              | _ => ();
  	   case get f of
             {escape=ref 0,used=ref 2,
	      info=FNinfo{specialuse=ref 2,reduce_ok,...},...} =>
		 reduce_ok := !CG.ifidiom
	   | {escape=ref 0,used=ref i,
	    info=FNinfo{reduce_ok,arity as ref al,...},...} =>
		  let fun loop(v::vl,a::al) =
		       (case (get v, a)
			 of ({used=ref 0,...},_) =>
			        if !CG.dropargs then COUNT(0,true)
				     else a
			  | ({escape=ref 0,...}, COUNT(_,false)) => a
                          | ({escape=ref 0,info=ARGinfo{biggestSEL=ref j},...},
			     COUNT(c,true)) => if j=c-1 andalso
			                               findFetch(v,c-1) body
			                           then a else TOP
			  | _ => TOP)
			  ::loop(vl,al)
		        | loop _ = []
		  in if i>1 then reduce_ok := false else ();
		     arity := loop(vl,al)
		  end
	      | {info=FNinfo{reduce_ok,...},...} =>
		    (reduce_ok := false;
		     if last
		     then ()
		     else (case body of
		            APP(g, _) => don'tReduce g
			   | _ => ())))

     val defaultrange = (minint,maxint)
     fun getrange(VAR v) = if !CG.rangeopt 
	                    then (case get v
				   of {info=INTinfo lohi,...} => lohi
				    | _ => defaultrange)
			    else defaultrange
       | getrange(INT i) = (i,i)
       | getrange _ = defaultrange

     exception ConstFold

     val rec pass1 = 
      fn RECORD(vl,w,e) => (enterREC(w,vl); app enterField vl; pass1 e)
       | SELECT (i,v,w,e) => 
	        (enter(w,{info=SELinfo(i,v), escape=ref 0, used = ref 0});
		 selectFrom(v,i);
 	         pass1 e)
       | OFFSET (i,v,w,e) => (enterMISC w; escape v; pass1 e)
       | APP(LABEL f, vl) => pass1(APP(VAR f, vl))
       | APP(VAR f, vl) =>
	 ((case get f
	    of {info=FNinfo{arity as ref al,...},...} =>
	     let fun loop(BOT::r,vl0 as VAR v :: vl, n) =
		      (case get v
		        of {info=RECinfo(wl,_),...} =>
		(* We have maxfree registers; one might be used for a closure
			argument; one might be needed for shuffling;
		        so the most arguments we can give a function here is 
			maxfree-2  *)
				if length al - 1 + length wl > maxfree-2
				   then TOP::loop(r,vl,n+1)
				   else loop(COUNT(length wl,false)::r,vl0,n)
		         | _ => UNK::loop(r,vl,n+1))
                   | loop(UNK::r,vl0 as VAR v :: vl, n) =
		      (case get v
		        of {info=RECinfo(wl,_),...} =>
				if length al - 1 + length wl > maxfree-2
				   then TOP::loop(r,vl,n+1)
				   else loop(COUNT(length wl,true)::r,vl0,n)
		         | _ => UNK::loop(r,vl,n+1))
		   | loop((cnt as COUNT(a,unk))::r, VAR v::vl,n) = 
		      (case get v of
		         {info=RECinfo(wl,flr as ref fl), ...} =>
			 if a = length wl
			 then (flr := (VAR f, n)::fl; cnt::loop(r,vl,n+1))
			 else TOP::loop(r,vl,n+1)
		       | _ => COUNT(a,true)::loop(r,vl,n+1))
		   | loop(_::r, _::vl,n) = TOP::loop(r,vl,n+1)
		   | loop _ = []
	     in arity := loop(al,vl,0)
	     end
	    | _ => ());
	  use(VAR f); app escape vl)
       | APP(f, vl) => (use f; app escape vl)
       | FIX(l, e) => (app enterFN l;
		       app (fn (f,vl,body) => pass1 body) l;
		       pass1 e;
		       app checkreduce l)
       | SWITCH(v,el) => (use v; app pass1 el)
       | PRIMOP(P.alength,[a],[w],[e]) => (use a; enterINT(w,0,maxint);
					  pass1 e)
       | PRIMOP(P.slength,[a],[w],[e]) => (use a; enterINT(w,0,maxint);
					   pass1 e)
       | PRIMOP(i,vl,wl,el as [APP(VAR f1, [INT 1]), APP(VAR f2, [INT 0])]) =>
	     (case get f1
	       of {info=FNinfo{body=PRIMOP(p,[INT 0, VAR w2],_,_),
			       args=[w1],specialuse,...},...} =>
		   if f1=f2 andalso w1=w2 
		       andalso (p=P.ineq orelse p=P.ieql)
		       then inc specialuse else ()
	        | _ => ();
              app escape vl; app enterMISC wl; app pass1 el)
       | PRIMOP(i,vl,wl,el) => (app escape vl; app enterMISC wl; app pass1 el)

     exception Beta
     val m2 : value Intmap.intmap = Intmap.new(32, Beta)
     fun ren(v0 as VAR v) = (ren(Intmap.map m2 v) handle Beta => v0)
       | ren(v0 as LABEL v) = (ren(Intmap.map m2 v) handle Beta => v0)
       | ren x = x
     fun newname vw = (sameName vw; Intmap.add m2 vw)
     fun newnames(v::vl, w::wl) = (newname(v,w); newnames(vl,wl))
       | newnames _ = ()
     
     fun evalSELECT(v,p) =
         case ren v
          of v' as VAR y =>
	      (case (get y,p,!CG.selectopt,!CG.recordopt)
                of ({info=RECinfo(vl,_),...},SELp(i,p'),true,_) =>
		      let val (x,p'') = nth(vl,i)
		       in click "d";
			     (* say"["; sayv v; say",";
			      sayv v'; say","; sayv x;
			      say","; say(makestring i); say"]";*)
			      evalSELECT(x,combinepaths(p'',p'))
		      end
		 | ({info=SELinfo(i,w),used=ref 1,...},_,_,true) =>
		       (click "a";
			     (* say"["; sayv v; say",";
			      sayv v'; say","; sayv w;
			      say","; say(makestring i); say"]";*)
			evalSELECT(w,SELp(i,p)))
		 | _ => (v',p))
           | v' => (v',p)
     
     fun makeSELECT(i,v,w,e) =
       let fun chase(x,OFFp 0) = (newname(w,x); e())
             | chase(x,OFFp i) = OFFSET(i,x,w,e())
             | chase(x,SELp(j,OFFp 0)) = SELECT(j,x,w,e())
             | chase(x,SELp(j,p')) = let val z = mkLvar()
	                              in SELECT(j,x,z,chase(VAR z,p'))
				     end
        in chase(evalSELECT(v,SELp(i,OFFp 0)))
       end

     fun findrecord(VAR v) =
	   (case get v
             of {info=RECinfo _,...} => true
              | {info=SELinfo(i,w),...} => findrecord(ren w)
              | _ => false)
       | findrecord _ = false

     val rec reduce = fn cexp => g NONE cexp
     and g = fn hdlr =>
     let val rec g' =
       fn RECORD (vl,w,e) =>
	  let val {info=RECinfo(_,ref fl),escape=ref esc,used=ref use,...} = get w
	  in if use=0(*esc = sum flatfun fl*) andalso !CG.deadvars
	     then (click "b"; g' e)
	     else RECORD(map evalSELECT vl, w, g' e)
	  end
        | SELECT(i,v,w,e) =>
	      if not(used w) andalso !CG.deadvars then (click "c"; g' e)
	      else if findrecord(ren v) then makeSELECT(i,v,w, fn()=> g' e)
		  else SELECT(i,ren v, w, g' e)
	| OFFSET(i,v,w,e) => OFFSET(i,ren v,w,g' e)
	| APP(f, vl) =>
	  let fun trybeta(f',fv) = 
	      (case get fv
	       of {info=FNinfo{args,body,reduce_ok=ref true,...},...} =>
		(newnames(args, map ren vl); g' body)
                | {info=FNinfo{arity=ref al,...},escape=ref 0,...} =>
		  let fun loop(COUNT(cnt,_) :: r, v::vl,args) =
			  let fun g(i,args) = 
				  if i=cnt then loop(r,vl,args)
				      else let val z = mkLvar()
					    in makeSELECT(i,v,z,
					      fn()=>g(i+1, ren(VAR z) :: args))
					   end
			   in g(0,args)
			  end
			| loop(_::r,v::vl,args) = loop(r,vl, ren v :: args)
			| loop (_,_,args) = APP(f', rev args)
		  in loop(al,vl,nil)
		  end
		   | _ => APP(f', map ren vl))
          in case ren f
	    of f' as VAR fv => trybeta(f',fv)
	     | f' as LABEL fv => trybeta(f',fv)
	     | f' => APP(f', map ren vl)
	  end
	| FIX(l,e) =>
	  let fun process_args(f,vl,body) = 
	           case get f
		    of {info=FNinfo{arity=ref al,...},escape=ref 0,...} =>
	       	          let fun vars 0 = []
				| vars i = mkLvar()::vars(i-1)
			      fun newargs(COUNT(j,_) :: r,v::vl) =
				  let val new = vars j
				  in app enterMISC new;
				     enterREC(v, map (fn x =>(VAR x, OFFp 0)) new);
				     click "f";
				     new @ newargs(r,vl)
				  end
				| newargs(_::r,v::vl) = v::newargs(r,vl)
				| newargs _ = []
			  in (f, newargs(al,vl), body)
			  end
		     | _ => (f, vl, body)
  	      fun reduce_body ((f,vl,body)::rest) = 
		  (case get f 
		    of {info=FNinfo{reduce_ok=ref true,...},
			used=ref uses,...} => 
			    (click (if uses=2 then "E" else "e");
			     reduce_body rest)
		     | _ => (f,vl, reduce body) :: reduce_body rest)
                | reduce_body nil = nil
              val e' = g' e
	   in case reduce_body (map process_args l)
	       of nil => e'
		| l' => FIX(l',e')
	  end
        | SWITCH(v,el) => 
		(case ren v
		  of INT i => 
			if !CG.switchopt then (click "h"; g' (nth(el,i)))
				else SWITCH(ren v, map g' el)
		   | VAR v' => SWITCH(VAR v', map g' el)
		   | _ => ErrorMsg.impossible "3121 in cpsopt")
	| PRIMOP(P.gethdlr,vl,wl as [w],[e]) =>
	  (if !CG.handlerfold
	    then case hdlr of
	     NONE => if used w then PRIMOP(P.gethdlr,vl,wl,[g (SOME(VAR w)) e])
		     else (click "i"; g' e)
	   | SOME w' => (click "j"; newname(w,w'); g' e)
	    else PRIMOP(P.gethdlr,vl,wl,[g (SOME(VAR w)) e]))
	| PRIMOP(P.sethdlr,[v],wl,[e]) =>
	  let val v' = ren v
	      val e' = g (SOME v') e
	  in if !CG.handlerfold
	    then case hdlr of
	       NONE => PRIMOP(P.sethdlr,[v'],wl,[e'])
	     | SOME v'' => if v'=v'' then (click "k"; e')
			   else PRIMOP(P.sethdlr,[v'],wl,[e'])
	    else PRIMOP(P.sethdlr,[v'],wl,[e'])
	  end
        | PRIMOP(i, vl, wl as [w], el as [e]) =>
	  if not(used w) andalso Prim.pure i andalso !CG.deadvars
	    then (click "m"; g' e)
	    else let val vl' = map ren vl
	          in g' (if !CG.arithopt
			 then primops(i, vl', wl, el)
			 else raise ConstFold)
	             handle ConstFold => PRIMOP(i, vl', wl, map g' el)
	         end
        | PRIMOP(i,vl,wl,el) =>
		 let val vl' = map ren vl
	          in g' (if !CG.arithopt
			 then primops(i,vl', wl, el)
			 else raise ConstFold)
	             handle ConstFold => PRIMOP(i, vl', wl, map g' el)
	         end
      in g'
     end

     and primops =
	fn (P.boxed, [INT _],_,_::b::_) => (click "n"; b)
	 | (P.boxed, [STRING s],_,a::b::_) =>
			    (click "o"; if size s = 1 then b else a)
	 | (P.boxed, [VAR v],_,a::_) => 
             (case get v
	       of  {info=RECinfo _, ...} => (click "p"; a)
	        | _ => raise ConstFold)
         | (P.<, [v,w],_,[a,b]) =>
	 let val (vlo,vhi) = getrange v
	     val (wlo,whi) = getrange w
          in if vhi < wlo andalso !CG.comparefold then (click "w"; a)
             else if vlo >= whi andalso !CG.comparefold then (click "w"; b)
	     else if v=w andalso !CG.comparefold then (click "v"; b)
	     else if !CG.branchfold andalso equalUptoAlpha(a,b)
		     then (click "z"; a)
             else case (a,b)
		   of (_,PRIMOP(P.<,[v',z],_,[c,a'])) =>
		        if v=v' andalso equalUptoAlpha(a,a')
			    then let val x = mkLvar() and y = mkLvar()
				 in click "C";
				    enterMISC x; enterMISC y;
				    PRIMOP(P.-,[v,w],[x],
				      [PRIMOP(P.-,[z,w],[y],
					[PRIMOP(P.<,[z,w],[],
					 [a,PRIMOP(P.rangechk,[VAR x, VAR y],[],[c,a])])])])
				 end
			    else raise ConstFold
		    | (PRIMOP(P.<,[v',z],_,[b',c]),_) =>
		        if v=v' andalso equalUptoAlpha(b,b')
			    then let val x = mkLvar() and y = mkLvar()
				  in click "C";
				    enterMISC x; enterMISC y;
				     PRIMOP(P.-,[v,z],[x],
				      [PRIMOP(P.-,[w,z],[y],
					[PRIMOP(P.<,[w,z],[],
					 [b,PRIMOP(P.rangechk,[VAR x,VAR y],[],[c,b])])])])
				 end
			    else raise ConstFold
		    | _ => raise ConstFold
         end
         | (P.>, [w,v],_,el) => PRIMOP(P.<,[v,w],[],el)
         | (P.<=, [w,v],_,el) => PRIMOP(P.>=,[v,w],[],el)
         | (P.>=, vl,_,[a,b]) => PRIMOP(P.<,vl,[],[b,a])
	 | (P.rangechk, [v,w],_,[a,b]) =>
	 let val (vlo,vhi) = getrange v
	     val (wlo,whi) = getrange w
          in if vhi < wlo andalso vlo >= 0 andalso !CG.comparefold 
		 then (click "w"; a)
             else if vlo >= whi orelse vhi < 0 andalso !CG.comparefold 
		      then (click "w"; b)
             else if v=w andalso !CG.comparefold then (click "v"; b)
	     else if !CG.branchfold andalso equalUptoAlpha(a,b)
		      then (click "z"; a)
             else raise ConstFold
	 end
         | (P.ieql, [v,w],_,[a,b]) =>
	 let val (vlo,vhi) = getrange v
	     val (wlo,whi) = getrange w
          in if vhi < wlo orelse whi < vlo andalso !CG.comparefold 
		 then (click "x"; b)
             else if vlo = vhi andalso wlo=whi andalso vlo=wlo andalso !CG.comparefold 
		                           then (click "x"; a)
             else if v=w andalso !CG.comparefold  then (click "v"; a)
	     else if !CG.branchfold andalso equalUptoAlpha(a,b) 
		      then (click "z"; a)
             else raise ConstFold
	 end
         | (P.ineq, [v,w],_,[a,b]) => PRIMOP(P.ieql,[v,w],[],[b,a])
         | (P.*, [INT 1, v],[w],[c]) =>
		      (click "F"; newname(w,v); c)
	 | (P.*, [v, INT 1],[w],[c]) =>
		      (click "G"; newname(w,v); c)
	 | (P.*, [INT 0, _],[w],[c]) =>
		   (click "H"; newname(w,INT 0); c)
	 | (P.*, [_, INT 0],[w],[c]) =>
		      (click "I"; newname(w,INT 0); c)
	 | (P.*, [INT i, INT j], [w], [c]) =>
		   (let val x = i*j
		    in x+x; newname(w,INT x); click "J"; c
		    end handle Overflow => raise ConstFold)
	 | (P.div, [v, INT 1],[w],[c]) =>
		      (click "K"; newname(w,v); c)
	 | (P.div, [INT i, INT j],[w],[c]) =>
		   (let val x = i quot j
		    in click "L"; newname(w,INT x); c
		    end handle Div => raise ConstFold)
         | (P.+, [INT 0, v],[w],[c]) =>
		   (click "M"; newname(w,v); c)
	 | (P.+, [v, INT 0],[w],[c]) =>
		   (click "N"; newname(w,v); c)
	 | (P.+, [INT i, INT j], [w], [c]) =>
		   (let val x = i+j
       		    in x+x; newname(w,INT x); click "O"; c
		    end handle Overflow => raise ConstFold)
         | (P.-, [v, INT 0],[w],[c]) =>
		      (click "P";newname(w,v); c)
	 | (P.-, [INT i, INT j], [w], [c]) =>
		  (let val x = i-j
		   in x+x; newname(w,INT x); click "Q"; c
		   end handle Overflow => raise ConstFold)
	 | (P.rshift, [INT i, INT j],[w],[c]) =>
			   (click "R"; newname(w,INT(Bits.rshift(i,j))); c)
	 | (P.rshift, [INT 0, _],[w],[c]) =>
			   (click "S"; newname(w,INT 0); c)
	 | (P.rshift, [v, INT 0],[w],[c]) =>
			   (click "T"; newname(w,v); c)
         | (P.slength, [INT _],[w],[c]) =>
			 (click "U"; newname(w, INT 1); c)
	 | (P.slength, [STRING s], [w],[c]) =>
			 (click "V"; newname(w, INT(size s)); c)
         | (P.ordof, [STRING s, INT i],[w],[c]) =>
			 (click "W"; newname(w, INT(ordof(s,i))); c)
         | (P.~, [INT i], [w], [c]) =>
		     (let val x = ~i
		      in x+x; newname(w,INT x); click "X"; c
		      end handle Overflow => raise ConstFold)
	 | (P.lshift, [INT i, INT j],[w],[c]) =>
			   (let val x = Bits.lshift(i,j)
			    in x+x; newname(w,INT x); click "Y"; c
			    end handle Overflow => raise ConstFold)
	 | (P.lshift, [INT 0, _],[w],[c]) =>
			   (click "Z"; newname(w,INT 0); c)
	 | (P.lshift, [v, INT 0],[w],[c]) =>
			   (click "1"; newname(w,v); c)
	 | (P.orb, [INT i, INT j],[w],[c]) =>
			(click "2"; newname(w,INT(Bits.orb(i,j))); c)
	 | (P.orb, [INT 0, v],[w],[c]) =>
			(click "3"; newname(w,v); c)
	 | (P.orb, [v, INT 0],[w],[c]) =>
			(click "4"; newname(w,v); c)
	 | (P.xorb, [INT i, INT j],[w],[c]) =>
			 (click "5"; newname(w,INT(Bits.xorb(i,j))); c)
	 | (P.xorb, [INT 0, v],[w],[c]) =>
			(click "6"; newname(w,v); c)
	 | (P.xorb, [v, INT 0],[w],[c]) =>
			(click "7"; newname(w,v); c)
	 | (P.notb, [INT i], [w], [c]) =>
			 (newname(w,INT(Bits.notb i)); click "8"; c)
	 | (P.andb, [INT i, INT j],[w],[c]) =>
			 (click "9"; newname(w,INT(Bits.andb(i,j))); c)
	 | (P.andb, [INT 0, _],[w],[c]) =>
			(click "0"; newname(w,INT 0); c)
	 | (P.andb, [_, INT 0],[w],[c]) =>
			(click "T"; newname(w,INT 0); c)
         | _ => raise ConstFold

    val _ = (debugprint "\nContract: "; debugflush())
    fun ssss cexp = (print "\nAfter contract: \n"; CPSprint.show (outputc std_out) cexp; cexp)

  in enterMISC fvar; app enterMISC fargs;
      pass1 cexp;
     (fvar, fargs, (*ssss*)(reduce cexp))
  end

fun expand((fvar,fargs,cexp),bodysize) =
   let
     datatype info = Fun of {escape: int ref, call: int ref, size: int ref,
		         args: lvar list, body: cexp}
	           | Arg of {escape: int ref, savings: int ref,
		             record: (int * lvar) list ref}
	           | Sel of {savings: int ref}
		   | Rec of {escape: int ref, size: int,
			     vars: (value * accesspath) list}
		   | Real
		   | Const
		   | Other

     exception Expand
     val m : info Intmap.intmap = Intmap.new(128,Expand)
     val note = Intmap.add m
     val get = Intmap.map m
     fun getval(VAR v) = get v
       | getval(LABEL v) = get v
       | getval(REAL _) = Other (*Real*)
       | getval(INT _) = Const
       | getval _ = Other
     fun call(v, args) = case getval v
	  	          of Fun{call,...} => inc call
		  	   | Arg{savings,...} => savings := !savings+1
		     	   | Sel{savings} => savings := !savings+1
		     	   | _ => ()
     fun escape v = case getval v
		     of Fun{escape,...} => inc escape
		      | Arg{escape,...} => inc escape
		      | Rec{escape,...} => inc escape
		      | _ => ()
     fun escapeargs v = case getval v
	                 of Fun{escape,...} => inc escape
		          | Arg{escape,savings, ...} =>
				     (inc escape; savings := !savings + 1)
		          | Sel{savings} => savings := !savings + 1
		     	  | Rec{escape,...} => inc escape
			  | _ => ()
     fun unescapeargs v = case getval v
		           of Fun{escape,...} => dec escape
		     	    | Arg{escape,savings, ...} =>
			     		(dec escape; savings := !savings - 1)
		       	    | Sel{savings} => savings := !savings - 1
		       	    | Rec{escape,...} => dec escape
			    | _ => ()
     fun setsize(f,n) = case get f of Fun{size,...} => (size := n; n)
     fun notearg v = (note (v,Arg{escape=ref 0,savings=ref 0, record=ref []}))
     fun notereal v = note (v,Other(*Real*))
     fun noteother v = note (v,Other)
     fun enter (f,vl,e) = (note(f,Fun{escape=ref 0, call=ref 0, size=ref 0,
					      args=vl, body=e});
			   app notearg vl)
     fun noterec(w, vl, size) = note (w,Rec{size=size,escape=ref 0,vars=vl})
     fun notesel(i,v,w) = (note (w, Sel{savings=ref 0});
		           case getval v
                            of Arg{savings,record,...} => (inc savings;
						    record := (i,w)::(!record))
                      	     | _ => ())
     fun save(v,k) = case getval v
		      of Arg{savings,...} => savings := !savings + k
		       | Sel{savings} => savings := !savings + k
		       | _ => ()
     fun nsave(v,k) = case getval v
		       of Arg{savings,...} => savings := k
		        | Sel{savings} => savings := k
		        | _ => ()
     fun savesofar v = case getval v 
		        of Arg{savings,...} => !savings
			 | Sel{savings} => !savings
		         | _ => 0
     val rec prim = fn (_,vl,wl,el) =>
	 let fun vbl(VAR v) = (case get v of Rec _ => 0 | _ => 1)
	       | vbl _ = 0
	     val _ = app noteother wl
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

     and primreal = fn (_,vl,wl,el) =>
	 (app notereal wl;
	  app (fn v => save(v,1)) vl;
	  2*(length vl + length wl) + sum pass1 el)

     and pass1 = 
      fn RECORD(vl,w,e) =>
	  (app (escape o #1) vl;
	   noterec(w,vl,length vl);
	   2 + length vl + pass1 e)
       | SELECT (i,v,w,e) => (notesel(i,v,w); 1 + pass1 e)
       | OFFSET (i,v,w,e) => (noteother w; 1 + pass1 e)
       | APP(f,vl) => (call(f,length vl); app escapeargs vl; 
		       1 + ((length vl + 1) quot 2))
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
       | PRIMOP(args as (P.rangechk,_,_,_)) => prim args
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
       | PRIMOP(args as (P.fadd,_,_,_)) => primreal args
       | PRIMOP(args as (P.fdiv,_,_,_)) => primreal args
       | PRIMOP(args as (P.feql,_,_,_)) => primreal args
       | PRIMOP(args as (P.fge,_,_,_)) => primreal args
       | PRIMOP(args as (P.fgt,_,_,_)) => primreal args
       | PRIMOP(args as (P.flt,_,_,_)) => primreal args
       | PRIMOP(args as (P.fle,_,_,_)) => primreal args
       | PRIMOP(args as (P.fmul,_,_,_)) => primreal args
       | PRIMOP(args as (P.fneq,_,_,_)) => primreal args
       | PRIMOP(args as (P.fsub,_,_,_)) => primreal args
       | PRIMOP(_,vl,wl,el) =>
	 (app escape vl; app noteother wl;length vl + length wl + sum pass1 el)

     fun substitute(args,wl,e) =
      let exception Alpha
          val vm : value Intmap.intmap = Intmap.new(16, Alpha)
          fun use(v0 as VAR v) = (Intmap.map vm v handle Alpha => v0)
	    | use(v0 as LABEL v) = (Intmap.map vm v handle Alpha => v0)
	    | use x = x
          fun def v = let val w = dupLvar v 
		      in Intmap.add vm (v, VAR w); w
		      end
          fun defl v = let val w = dupLvar v 
		      in Intmap.add vm (v, label w); w
		      end
	  fun bind(a::args,w::wl) = 
	         (sameName(w,a); Intmap.add vm (w,a); bind(args,wl))
	    | bind _ = ()
          val rec g =
         fn RECORD(vl,w,ce) => RECORD(map (map1 use) vl, def w, g ce)
          | SELECT(i,v,w,ce) => SELECT(i, use v, def w, g ce)
          | OFFSET(i,v,w,ce) => OFFSET(i, use v, def w, g ce)
          | APP(v,vl) => APP(use v, map use vl)
          | FIX(l,ce) => 
	    let fun h1(f,vl,e) = (f,defl f, vl, e)
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
      in (* debugprint("\nSize=" ^ makestring(pass1 cexp)); debugprint " "; *)
	  pass1 cexp;
	  cexp
      end
		
     fun beta(n, d, e) = case e
      of RECORD(vl,w,ce) => RECORD(vl, w, beta(n,d+2+length vl, ce))
       | SELECT(i,v,w,ce) => SELECT(i, v, w, beta(n,d+1, ce))
       | OFFSET(i,v,w,ce) => OFFSET(i, v, w, beta(n,d+1, ce))
       | APP(v,vl) => 
	   (case getval v
	     of Fun{escape,call as ref calls,size=ref size,args,body} =>
		let fun whatsave(acc, (v:value)::vl, a::al) =
			if acc>=size
			then acc
			else
			(case get a of
			   Arg{escape=ref esc,savings=ref save,record=ref rl} =>
                           let val (this, nvl: value list, nal) =
				case getval v
				 of Fun{escape=ref 1,...} =>
                                         (if esc>0 then save else 6+save,vl,al)
				  | Fun _ => (save,vl,al)
				  | Rec{escape=ref ex,vars,size} =>
				       let exception Chase
					   fun chasepath(v,OFFp 0) = v
		      			     | chasepath(v, SELp(i,p)) =
			  			(case getval v
			   			  of Rec{vars,...} =>
							 chasepath(chasepath(nth(vars,i)),p)
			    			   | _ => raise Chase)
					     | chasepath _ = raise Chase
				    	   fun loop([],nvl,nal) = 
					       (if ex>1 orelse esc>0
					        then save
						else save+size+2,nvl,nal)
					     | loop((i,w)::rl,nvl,nal) =
						loop(rl,
						   chasepath(nth(vars,i))::nvl,
						     w::nal)
				        in loop(rl,vl,al)
					   handle Chase => (0,vl,al)
					        | Nth => (0,vl,al)
				       end 
				 (* | Real => (save,vl,al)*)
				  | Const => (save,vl,al)
                                  | _ => (0,vl,al)
			   in whatsave(acc+this - (acc*this) quot size, nvl,nal)
			   end
			 | Sel{savings=ref save} =>
                           let val this =
			       case v
				of VAR v' => (case get v' of
					       Fun _ => save
					     | Rec _ => save
	                                     | _ => 0)
				 | _ => save
			   in whatsave(acc + this - (acc*this) quot size, vl,al)
			   end)
		      | whatsave(acc,_,_) = acc
		  val predicted = calls*((size-whatsave(0,vl,args))-(1+length vl))
                  val depth = 2
		  val max = 5
		  val increase = (bodysize*(depth - n)) quot depth
	     in if false andalso debug
		    then (CPSprint.show (outputc std_out) e;
			  print predicted; print "   ";print increase;
			  print"   ";
			  print n;
			  print "\n")
		   else ();
		if (predicted <= increase
		    orelse (!escape=0 andalso
			    (calls = 1
		    orelse (case vl of
			      [_] => calls = 2 andalso
				     predicted - (size+1) <= increase
			    | _ => false))))
		    andalso n <= max
		then let val new = beta(n+1, d+1, substitute(vl,args,body))
		     in click "^";
		        call := calls-1;
		        app unescapeargs vl;
		        new
		     end
		else e
	    end
	    | _ => e)
       | FIX(l,ce) => let fun h(f,vl,e) = 
			     case (get f,e)
			      of (_,FIX([(g,[b,k],APP _)],
					APP(VAR c,[VAR g']))) =>
				   if c=last0 vl andalso g=g' then (f,vl,e)
				        else (f,vl, beta(n,0,e))
			       | (Fun{escape=ref 0,...},_) => 
				         (f,vl, beta(n,0,e))
			       | (_,APP _) => (f,vl,e)
			       | _ => (f,vl, beta(n,0,e))
		       in FIX(if n<1 then map h l else l, 
			      beta(n,d+length l, ce))
		      end
       | SWITCH(v,l) => SWITCH(v, map (fn e => beta(n,d+2,e)) l)
       | PRIMOP(i,vl,wl,ce) => PRIMOP(i, vl, wl, map (fn e => beta(n,d+2,e)) ce)

    in notearg fvar; app notearg fargs;
       debugprint("\nExpand   "); debugprint(makestring(pass1 cexp));
       debugprint("\n"); debugflush();
       (fvar, fargs, beta(0,0,cexp))
   end

  val bodysize = !System.Control.CG.bodysize
  val rounds = !System.Control.CG.rounds
  val reducemore = !System.Control.CG.reducemore

  fun contracter last function =
	 let val function = (clicked := 0; contract false function)
	  in if !clicked <= reducemore
	     then if last
		  then contract last function
		  else function
	     else contracter last function
	 end

  fun cycle(0,function) = contract true function
    | cycle(1,function) = 
	let val _ = debugprint "\nHoist: "
	    val function = hoist function
	    val _ = clicked := 0
	    val function = if !CG.betaexpand then expand(function,bodysize quot rounds)
			else function
	    val cl = !clicked before clicked := 0
        in if cl <= reducemore
	   then contract true function
	   else contracter true function
	end
    | cycle(k,function) = 
	let val _ = debugprint "\nHoist: "
	    val function = hoist function
	    val _ = clicked := 0
	    val function = if !CG.betaexpand then expand(function,(bodysize * k) quot rounds)
			else function
	    val cl = !clicked before clicked := 0
        in if cl <= reducemore
	   then contract true function
	   else cycle(k-1, contracter false function)
	end
in (if rounds < 0 then 
      function
    else cycle(rounds,contracter false (eta 
			      (contract true (contract true function)))))
   before (debugprint "\n"; debugflush())
end
end
