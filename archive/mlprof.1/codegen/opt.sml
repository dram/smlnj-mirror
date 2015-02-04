signature OPT =
sig
  structure L : LAMBDA sharing L=Lambda
  structure A : ACCESS sharing A=Access
  val reduce : L.lexp -> L.lexp
  val hoist : L.lexp -> L.lexp
  val closestr : (int -> string) * L.lexp -> L.lexp
  val closetop : L.lexp -> L.lexp
  val free : L.lexp -> A.lvar -> A.lvar list
  val alphaConvert : L.lexp -> L.lexp
  val mapfree : (A.lvar -> L.lexp) -> (L.lexp -> L.lexp)
  val pure : L.lexp -> bool
end

structure Opt : OPT =
struct

structure A : ACCESS = Access
structure L : LAMBDA = Lambda

open A Basics L

fun root [v] = v | root (_::p) = root p
  | root _ = ErrorMsg.impossible "root nil in codegen/opt";

fun mapfree lookfree =
   let val m = Intmap.new()
      val add = Intmap.add m
      fun look v = Intmap.map m v
		   handle Intmap.Intmap =>
                     let val x = lookfree v in add(v,x); x end
      fun copycon (DATAcon(DATACON{rep=VARIABLE(Access.PATH p),const,name,typ,sign})) =
	    let fun f [v] = let val VAR w = look v in [w] end
	          | f (i::r) = i::(f r) 
	     in DATAcon(DATACON{rep=VARIABLE (Access.PATH(f p)),const=const,
			    name=name,typ=typ,sign=sign})
	    end
        | copycon c = c
      fun newvar v = let val w = mkLvar() in add(v,VAR w); w end	    
      val rec f =
	 fn VAR v => look v
          | FN(v,b) => FN(newvar v, f b)
	  | FIX(vl,el,b) => FIX(map newvar vl, map f el, f b)
	  | APP(a,b) => APP(f a, f b)
	  | SELECT(i,a) => SELECT(i, f a)
	  | RECORD l => RECORD(map f l)
	  | SWITCH(a, l, SOME d) => 
			SWITCH(f a, map(fn(c,x)=>(copycon c, f x))l, SOME(f d))
	  | SWITCH(a, l, NONE) => 
			SWITCH(f a,map(fn(c,x)=>(copycon c, f x))l,NONE)
	  | HANDLE(a,b) => HANDLE(f a, f b)
	  | RAISE x => RAISE(f x)
	  | e as INT _ => e
	  | e as STRING _ => e
	  | e as REAL _ => e
    in f
   end

val alphaConvert = mapfree VAR

val simple = fn VAR _ => true
	      | RECORD nil => true
	      | INT _ => true
	      | STRING s => length s = 1
	      | _ => false

fun all f = not o (exists (not o f))

val rec pure =
	   fn VAR _ => true
	    | APP(FN(_,a),b) => pure a andalso pure b
	    | FIX(vl,el,b) => all pure el andalso pure b
	    | APP(SELECT (i, VAR 0),z) => Prim.pure i andalso pure z
	    | FN _ => true
	    | INT _ => true
	    | REAL _ => true
	    | STRING _ => true
	    | SELECT(_, x) => pure x
	    | RECORD l => all pure l
	    | SWITCH(a, l, NONE) => pure a andalso all (pure o #2) l
	    | SWITCH(a, l, SOME d) => pure a andalso pure d andalso
				  all (pure o #2) l
	    | _ => false

fun reduce exp =
      let val t = Intmap.new() : lexp Intmap.intmap
	  val set = Intmap.add t
	  val unset = Intmap.rem t
	  val imap = Intmap.map t
          val s = Intset.new()
	  val mark = Intset.add s
	  val marked = Intset.mem s
	  fun mapvar v = (imap v handle Intmap.Intmap => VAR v)
	  fun makp [v] = ((case imap v of VAR w => makp[w]
				        | _ => (mark v; [v]))
			  handle Intmap.Intmap => (mark v; [v]))
	    | makp (i::p) = i :: makp p
	  val rec makcon =
		fn (DATAcon(DATACON{rep=VARIABLE(PATH p),
					    name,const,typ,sign}), e) =>
		          (DATAcon(DATACON{rep=VARIABLE(PATH(makp p)),
			       name=name,const=const,typ=typ,sign=sign}),
			   mak e)
		  | (c,e) => (c, mak e)
	  and mak =
	     fn FN(v,b as APP(l,VAR w)) =>
		 if v=w andalso pure l
		  then let val body = mak l
		       in  if marked v then FN(v,mak(APP(body,VAR v)))
			   else body
		       end
		  else FN(v, mak b)
	      | APP(FN(v, e' as SWITCH(VAR v',l,d)), e) =>
		if v=v' then
		let val arg = mak e
		    val _ = set(v,arg)
		    val l' = map makcon l
		    val d' = case d of NONE => NONE | SOME a => SOME(mak a)
		    val _ = unset v
		 in if marked v then 
			APP(FN (v, SWITCH(VAR v, l', d')), arg)
		    else SWITCH(arg, l', d')
	        end
		else    let val arg = mak e
			    val body = (set(v, arg); mak e' before unset v)
			in  if marked v orelse not(pure arg)
				then APP(FN(v,body),arg)
				else body
			end
	      | APP(FN(v,e'),e) =>
			let val arg = mak e
			    val body = (set(v, arg); mak e' before unset v)
			in  if marked v orelse not(pure arg)
				then APP(FN(v,body),arg)
				else body
			end
	      | FIX(nil,nil,b) => mak b
	      | FIX(vl,el,b) => FIX(vl,map (fn FN(v,e) => FN(v,mak e)) el,mak b)
	      | e as VAR v => (let val e' = imap v
			       in  if simple e' then mak e' else (mark v; e)
			       end handle Intmap.Intmap => (mark v; e))
	      | e as SELECT(i, VAR v) =>
			      ((case imap v
			        of RECORD l =>
				   let val e' = nth(l,i)
				   in  if simple e' then e' else (mark v; e)
				   end
				 | ew as VAR w => mak(SELECT(i,ew))
				 | _ => (mark v; e))
			        handle Intmap.Intmap => (mark v; e))
	      | FN (w,b) => FN(w,mak b)
	      | APP (f,a) => APP(mak f, mak a)
	      | SWITCH(e,l,d) => 
		  (SWITCH(mak e, map makcon l,
				  case d of NONE => NONE 
					  | SOME a => SOME(mak a)))
	      | RECORD l => RECORD(map mak l)
	      | SELECT (i,e) => SELECT(i,mak e)
	      | HANDLE (a,h) => HANDLE(mak a, mak h)
	      | RAISE e => RAISE(mak e)
	      | e as INT _ => e
	      | e as REAL _ => e
	      | e as STRING _ => e
      in  mak exp
      end

(* minimal hoist function:  does not move bindings around, evaluation order
   is unchanged. *)
fun hoist (FN(v,b)) = FN(v,hoist b)
  | hoist (APP(FN(v,b),f as FN _)) = hoist(FIX([v],[f],b))
  | hoist (APP(l,r)) = APP(hoist l,hoist r)
  | hoist (FIX(vl,bl,FIX(vs,bs,b))) = hoist(FIX(vl@vs,bl@bs,b))
  | hoist (FIX(vl,bl,APP(FN(v,b),f as FN _))) = hoist(FIX(v::vl,f::bl,b))
  | hoist (FIX(vl,bl,b)) = FIX(vl,map hoist bl, hoist b)
  | hoist (SWITCH(e,l,d)) = 
	SWITCH(hoist e, map (fn (c,e) => (c, hoist e)) l,
	       case d of NONE => NONE 
		       | SOME a => SOME(hoist a))
  | hoist (RECORD l) = RECORD(map hoist l)
  | hoist (SELECT (i,e)) = SELECT(i,hoist e)
  | hoist (RAISE e) = RAISE(hoist e)
  | hoist (HANDLE (a,h)) = HANDLE(hoist a, hoist h)
  | hoist x = x

fun freevars e =
    let val t = Intset.new()
	val set = Intset.add t
	val unset = Intset.rem t
	val done = Intset.mem t
	val free : int list ref = ref nil
	val rec mak =
	 fn VAR w => if done w then () else (set w; free := w :: !free)
	  | FN (w,b) => (set w; mak b; unset w)
	  | FIX (vl,el,b) => (app set vl; app mak (b::el); app unset vl)
	  | APP (f,a) => (mak f; mak a)
	  | SWITCH(e,l,d) => 
	      (mak e;
	       app (fn (DATAcon(DATACON{rep=VARIABLE(PATH p),...}),e) =>
			 (mak(VAR(root p)); mak e)
		     | (c,e) => mak e)
		   l;
	       case d of NONE => () | SOME a => mak a)
	  | RECORD l => app mak l
	  | SELECT (i,e) => mak e
	  | HANDLE (a,h) => (mak a; mak h)
	  | RAISE e => mak e
	  | INT _ => ()
	  | REAL _ => ()
	  | STRING _ => ()
    in  set 0; mak e; !free
    end

fun closestr(lookup: int->string, e:lexp) : lexp =
    let val fv = freevars e
	val names = map lookup fv
    in  if !ErrorMsg.debugging
	  then app (fn s => (print s; print " ")) names
	  else ();
	FN(mkLvar(),
	   RECORD
	     [fold (fn (v,f) =>
		      let val w = mkLvar()
		       in FN(w,APP(FN(v,APP(f,SELECT(1,(VAR w)))),
				   SELECT(0,(VAR w))))
		      end)
		   fv
		   (FN(mkLvar(),e)),
	      fold (fn (s,f) => RECORD[STRING s, f])
		   names
		   (RECORD nil)])
    end

fun closetop(e: lexp) : lexp =
    let val looker = Access.mkLvar()
	val fv = freevars e
    in  FN(looker,
	   fold (fn (v,f) => APP(FN(v,f),APP(VAR looker, INT v)))
		fv e)
    end

(* free variable analysis *)
fun merge(a,nil) = a
  | merge(nil,a) = a
  | merge(l as (i:int)::a, m as j::b) = 
	if i<j then i::merge(a,m)
	else if i>j then j::merge(l,b)
	else merge(l,b)

fun remove (x: int) =
    let fun f nil = nil | f (i::r) = if i=x then r else i::(f r)
     in f
    end

fun free e =
let val vars : lvar list Intmap.intmap = Intmap.new()
    val setvars = Intmap.add vars
    val rec freevars =
	 fn VAR 0 => nil
	  | VAR w => [w]
	  | APP(FN(w,b),a) => remove w (merge(freevars b, freevars a))
	  | APP(f,a) => merge(freevars f, freevars a)
	  | FN(w,b) => let val z = remove w (freevars b)
		       in  setvars(w,z); z
		       end
	  | FIX(nil,_,b) => freevars b
	  | FIX(vl as v::_,el,b) =>
		let val z = fold (fn (x,y) => remove x y)
				 vl 
				 (fold (fn(FN(v,a),b) =>
					 merge(remove v (freevars a), b)) el nil)
		in  setvars(v,z);
		    merge(fold (fn(x,y) => remove x y) vl (freevars b), z)
		end
	  | SWITCH(e,l,d) =>
		let fun freevcon(DATAcon(DATACON{rep=VARIABLE(Access.PATH p),
						 ...})) = [root p]
		      | freevcon _ = nil
		in  fold (fn ((c,e'),b) => merge(merge(freevcon c, freevars e'),b))
			 l
			 (merge(freevars e,case d of SOME x => freevars x
						   | NONE => nil))
		end 
	  | RECORD l => fold (fn (a,b) => merge(freevars a, b)) l nil
	  | SELECT(_,e) => freevars e
	  | HANDLE(a,h) => merge(freevars a, freevars h)
	  | RAISE e => freevars e
	  | INT _ => nil
	  | REAL _ => nil
	  | STRING _ => nil
in  freevars e;
    Intmap.map vars
end


end (* structure Opt *)
