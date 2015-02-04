structure Opt =
struct

local open Access Basics Lambda
      infix before
      fun a before b = a
in

fun root [v] = v | root (_::p) = root p
  | root _ = ErrorMsg.Impossible "34 in codegen/opt";

fun applyOnly (v,b) =
   let exceptionx found
       fun checkv vl =
        let fun inSet v = exists((fn u=> v=u),vl)
	    val rec any =
		  fn VAR w => if inSet w then raisex found else ()
	 	   | FN (w,b) => any b (* alpha-conversion done beforehand *)
		   | FIX (vl,el,b) => app any (b::el)
		   | APP (f,a) => (any f; any a)
		   | SWITCH(e,l,d) =>
			    (any e; app (fn (c,e) => any e) l;
			     case d of NONE => ()
				     | SOME a => any a)
		   | RECORD l => app any l
		   | SELECT (i,e) => any e
		   | HANDLE (a,h) => (any a; any h)
		   | RAISE e => any e
		   | INT _ => () | REAL _ => () | STRING _ => ()
	    val rec check =
		  fn VAR w => if inSet w then raisex found else ()
		   | APP(VAR _, b) => check b
		   | APP(FN(w,b),VAR u)
			    => if inSet u then checkv(w::vl)b else check b
		   | APP(FN(w,b),FN(u,c)) =>
			    if ((any c; false) handlex found => true)
			      then (check c; checkv(w::vl)b)
			      else check b
		   | APP(FN(w,b),a) => (check b; check a)
	 	   | FN (w,b) => any b (* alpha-conversion done beforehand *)
		   | FIX (vl,el,b) => app check (b::el)
		   | APP (f,a) => (check f; check a)
		   | SWITCH(e,l,d) =>
			    (check e; app (fn (c,e) => check e) l;
			     case d of NONE => ()
				     | SOME a => check a)
		   | RECORD l => app check l
		   | SELECT (i,e) => check e
		   | HANDLE (a,h) => (check a; check h)
		   | RAISE e => check e
		   | INT _ => () | REAL _ => () | STRING _ => ()
         in check
        end
    in (checkv [v] b; true) handlex found => false
   end


  (* a function called selectOpt used to be here, but it has been
     made obsolete by "reduce" *)

  (* this is the reduce phase before codegen *)

  val simple = fn VAR _ => true
		 | RECORD nil => true
		 | INT _ => true
		 | STRING x => length x = 1
		 | _ => false

   val rec droppable =
	fn VAR _ => true
	 | RECORD l => not(exists(not o droppable, l))
	 | SELECT(_, e) => droppable e
         | INT _ => true | REAL _ => true | STRING _ => true
	 | _ => false

   fun reduce exp =
      let val T = intmap.new() : lexp intmap.intmap
	  val set = intmap.add T
	  val unset = intmap.rem T
	  val imap = intmap.map T

          val S = intset.new()
	  val mark = intset.add S
	  val marked = intset.mem S

	  fun mapvar v = (imap v handlex intmap.intmap => VAR v)

	  fun makp [v] = (case imap v of VAR w => makp[w]
				         | _ => (mark v; [v]))
			   handlex intmap.intmap => (mark v; [v])
	    | makp (i::p) = i :: makp p
       
         val pure = 
	 fn VAR _ => true
          | FN _ => true
	  | _ => false

	  val rec mak =
	     fn FN(v,b as APP(l,VAR w)) =>
		 if v=w andalso pure l
		  then let val body = mak l
			in if marked v then FN(v,mak(APP(body,VAR v)))
			     else body
		       end
		  else FN(v, mak b)
	      | APP(FN(v,e'),e) => 
			let val arg = mak e
			    val body = (set(v, arg); mak e' before unset v)
			 in if marked v orelse not(droppable arg)
				then APP(FN(v,body),arg)
				else body
			end
(*	      | FIX (vl,el,b) => 
			let val el = map mak el
			    val b = mak b
			    fun take (v::vl, e::el) = 
				 if marked v
				    then let val (vl', el') = take (vl,el)
				          in (v::vl', e::el')
					 end
				    else take(vl,el)
			 in case take(vl,el)
		             of (nil,nil) => b 
			      | (vl,el) => FIX(vl,el,b)
			end
*)
	      | FIX(nil,nil,b) => mak b
	      | FIX(vl,el,b) => FIX(vl, map mak el, mak b)
	      | e as VAR v => (let val e' = imap v
			        in if simple e' then mak e' else (mark v; e)
			       end handlex intmap.intmap => (mark v; e))
	      | e as SELECT(i, VAR v) =>
			      ((case imap v
			        of RECORD l =>
				   let val e' = nth(l,i)
				    in if simple e' 
					then e'
					else (mark v; e)
				   end
				 | ew as VAR w => mak(SELECT(i,ew))
				 | _ => (mark v; e))
			        handlex intmap.intmap => (mark v; e))
	      | FN (w,b) => FN(w,mak b)
	      | APP (f,a) => APP(mak f, mak a)
	      | SWITCH(e,l,d) => 
		let fun f (DATAcon(DATACON{rep=ref(VARIABLE(PATH p)),
					    name,const,vtype,tycon}), e) =
		          (DATAcon(DATACON{rep=ref(VARIABLE(PATH(makp p))),
			       name=name,const=const,vtype=vtype,tycon=tycon}),
			   mak e)
		      | f (c,e) = (c, mak e)
		  in SWITCH(mak e, map f l,
				  case d of NONE => NONE 
					  | SOME a => SOME(mak a))
		 end
	      | RECORD l => RECORD(map mak l)
	      | SELECT (i,e) => SELECT(i,mak e)
	      | HANDLE (a,h) => HANDLE(mak a, mak h)
	      | RAISE e => RAISE(mak e)
	      | e as INT _ => e
	      | e as REAL _ => e
	      | e as STRING _ => e
       in mak exp
      end

   (* minimal hoist function:  does not move bindings around, order stays *)
   (* the same *)
   fun hoist (FN(v,b)) = FN(v,hoist b)
     | hoist (APP(FN(v,b),f as FN _)) = hoist(FIX([v],[f],b))
     | hoist (APP(l,r)) = APP(hoist l,hoist r)
     | hoist (FIX(vl,bl,FIX(vs,bs,b))) = hoist(FIX(vl@vs,bl@bs,b))
     | hoist (FIX(vl,bl,APP(FN(v,b),f as FN _))) =
		 hoist(FIX(vl@[v],bl@[f],b))
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

end
end
