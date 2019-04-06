(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPSgen(M: CMACHINE) :
  sig structure CPS : CPS
    val codegen : CPS.function list * ErrorMsg.complainer -> unit
  end =
struct

structure CPS = CPS
open CPS M System.Tags Access
structure CG = System.Control.CG

fun findescapes(fl as (firstfun,_,_)::_) =
 let val s = Intset.new()
     val add0 = Intset.add s and mem = Intset.mem s
     fun add(LABEL v) = add0 v | add _ = ()
     fun f(RECORD(l,_,e)) = (app (add o #1) l; f e)
       | f(SELECT(_,_,_,e)) = f e
       | f(OFFSET(_,_,_,e)) = f e
       | f(SWITCH(_,el)) = app f el
       | f(PRIMOP(_,vl,_,el)) = (app add vl; app f el)
       | f(APP(_,vl)) = app add vl
 in  add0 firstfun;
     app (f o #3) fl; 
     mem
 end

datatype frag
  = STANDARD of (lvar * lvar list * cexp) option ref
  | KNOWN of (lvar list * cexp) * int list option ref * addressing ref
  | STRINGfrag of string
  | REALfrag of string

val allregs = standardclosure::standardarg::standardcont::miscregs

fun standardformals [_,_]   = [2,1]
  | standardformals [_,_,_] = [0,1,2]
  | standardformals _ = ErrorMsg.impossible "110 in CPSgen"

fun tempreg(x,f) = case arithtemps of _::z::_ => f z | _ => f x

local val regarr = arrayoflist allregs
   in fun regt x = regarr sub x
  end

local val okreg = array(length allregs, true)
      fun mark b i = update(okreg,i,b)
      val last = ref 0
      val len = Array.length okreg
 in val arithtemp = case arithtemps of z::_ => z 
                                     | _ => (mark false 3; regt 3)
    exception Getscratch
    fun getscratch(preferred, prohibited) =
        let val next = !last
	    fun find2 i = if okreg sub i then i 
			  else if i=next then (app (mark true) prohibited;
					       raise Getscratch)
			                 else find2(i+1)
	    fun find i = if okreg sub i then i else find(i+1)
         in app (mark false) prohibited;
	    ((if (okreg sub preferred handle Subscript => false) 
		  then preferred else find next)
	      handle Subscript => find2 0)
	    before (app (mark true) prohibited;
		    last := (if next+1 >= len then 0 else next+1))
        end
end

fun split pred nil = (nil,nil)
  | split pred (a::r) = let val (x,y) = split pred r
			 in if pred a then (a::x, y) else (x, a::y)
		        end

fun codegen(funs : (lvar * lvar list * cexp) list, err) =
let 
    val escapes = findescapes funs

    exception Regbind and Labbind
    val labtable : EA Intmap.intmap = Intmap.new(32, Labbind)
    val addlabbinding = Intmap.add labtable
    val labmap = Intmap.map labtable
    val regbindtable : int Intmap.intmap = Intmap.new(32, Regbind)
    val addbinding = Intmap.add regbindtable
    val regmap = Intmap.map regbindtable

    exception Know
    val knowtable : frag Intmap.intmap = Intmap.new(32, Know)
    val addknow = Intmap.add knowtable
    val know = Intmap.map knowtable

    exception Freemap
    val freemaptable : lvar list Intmap.intmap = Intmap.new(32, Freemap)
    val freemap = Intmap.map freemaptable

    local val addr = ref(ref PositionIndependent)
          val knowngen = System.Control.CG.knowngen
          val stdgen = System.Control.CG.stdgen
     in fun makefrag (f,vl,e) = 
	let val lab = newlabel()
	    val knowledge = if not(escapes f)
	      then (inc knowngen; KNOWN((vl,e), ref NONE, ref PositionIndependent))
	      else (inc stdgen; STANDARD(ref(SOME(f,vl,e))))
	 in addknow(f, knowledge); addlabbinding(f,lab);
	    FreeMap.freemap (Intmap.add freemaptable) e;
	    (lab,knowledge)
	end
    end

    val frags = ref(map makefrag funs)
    fun addfrag f = frags := f :: !frags

    exception Strings
    local open IntStrMap
         val m : EA intstrmap = new(32,Strings)
    in fun enterString (s,lab) = add m (StrgHash.hashString(s),s,lab);
       fun lookString s = map m (StrgHash.hashString(s),s)
    end

    fun regbind(VAR v) = regt (regmap v)
      | regbind(LABEL v) = labmap v
      | regbind(INT i) = (immed(i+i+1) handle Overflow =>
		          ErrorMsg.impossible "Overflow in cps/generic.sml")
      | regbind(STRING s) = (lookString s handle Strings =>
	                     let val lab = newlabel()
			      in addfrag(lab, STRINGfrag s);
				 enterString(s,lab);
				 lab
			     end)
      | regbind(REAL s) = let val lab = newlabel()
			     in addfrag(lab, REALfrag s);
				lab
			    end

    fun regmask formals = 
	let fun f(i,mask) = Bits.orb(Bits.lshift(1,i),mask)
         in emitlong (fold f formals 0)
	end

    fun root(RECORD(_,_,e)) = root e
      | root(SELECT(_,_,_,e)) = root e
      | root(OFFSET(_,_,_,e)) = root e
      | root(SWITCH(_,e::_)) = root e
      | root(PRIMOP(_,_,_,e::_)) = root e
      | root(e as APP _) = e

    val root1 = ref(APP(VAR 0, []))

    val any = INT 0 (* default argument for alloc *)

    fun alloc(v,default,continue) =
      let val (f,formals,wl) = case !root1
                        of APP(VAR f, wl) => (f, standardformals wl, wl)
                         | APP(LABEL f, wl) => 
				     (f,(case know f
					  of KNOWN(_,ref(SOME fmls),_) => fmls
					   | KNOWN(_,ref NONE,_) => nil
					   | STANDARD _ => standardformals wl),
				      wl)
	    val proh = map regmap (freemap v)

	    fun delete (z,nil) = nil
	      | delete (z:int, a::r) = if a=z then r else a::delete(z,r)
	    val default = case default
		           of VAR i => regmap i
			    | _ => ~1
	    fun get(good,bad) =
		let val r = getscratch(good,bad@proh)
		            handle Getscratch => getscratch(default,proh)
	         in addbinding(v,r); continue(regt r)
		end
	    fun find fmls = 
               let fun g(VAR w ::wl, r::rl) = 
				if w=v then get(r, delete(r,fmls))
				       else g(wl,rl)
		     | g(_::wl,_::rl) = g(wl,rl)
		     | g _ = get(default, fmls)
		in g(wl,fmls)
	       end
         in if v=f then get(default,standardformals[0,0,0])
	    else find formals
        end

    local 
       fun shuffle1(args: value list, formals: int list) =
	let fun classify(VAR v::al, f::fl, match, nomatch, notinreg) =
	        let val v' = regmap v
		 in if v' = f
			then classify(al,fl,f::match,nomatch,notinreg)
			else classify(al,fl,match,(v',f)::nomatch,notinreg)
		end
	      | classify(a::al,f::fl, m,n,notinreg) = 
	            classify(al,fl,m,n,(regbind a, regt f)::notinreg)
	      | classify(_,_, m,n,nr) = (m,n,nr)
		 
	    val (matched, notmatched, notinreg) =
						classify(args,formals,[],[],[])

	    fun f(nil, used) = ()
	      | f (pairs,used) = 
	      let val u' = map #1 pairs @ used
		  fun movable (a, b) = not (exists (fn z=>z=b) u')
	       in case split movable pairs
		   of (nil,(a,b)::r) => 
			      let val x = getscratch(~1,u')
				in move(regt a, regt x); 
				   f((x,b)::r, used)
			      end
		    | (m,m') => (app (fn(a,b)=>move(regt a,regt b)) m; 
				 f(m', (map #2 m) @ used))
	      end
	in f(notmatched, matched);
	   app move notinreg
       end

	fun getregs (VAR v :: rest, l) = getregs(rest, regmap v :: l)
	  | getregs (_::rest, l) = getregs(rest,l)
	  | getregs (nil,l) = l

    in fun shuffle(func as VAR f, args: value list, formals) =
	    let val x = getscratch(regmap f, getregs(args,nil) @ formals)
			  handle Getscratch => getscratch(regmap f, formals)
	     in shuffle1(func::args,x::formals); jmp(regt x)
	    end
         | shuffle(func as LABEL f, args,formals) =
		(shuffle1(args,formals); jmp(labmap f))
   end

    fun allocparams(args,formals) =
       let fun getregs [] = [nil]
	     | getregs(VAR a :: ar) = let val ar' as a'::_ = getregs ar
				       in (regmap a :: a') :: ar'
				      end
	     | getregs(_::ar) = let val ar' as a'::_ = getregs ar
				 in a'::ar'
				end
	   fun f(already, VAR a :: ar, occupied::occupied', b::br) =
		let val i = regmap a
		    val z = getscratch(i, already @ occupied)
		 in addbinding(b,z); 
		    if i=z then () else move(regt i, regt z);
		    f(z::already,ar,occupied',br)
		end
	     | f(already, a::ar, occupied::occupied', b::br) =
		let val z = getscratch(~1, already@occupied)
		 in addbinding(b,z); 
		    move(regbind a, regt z);
		    f(z::already,ar,occupied',br)
		end
	     | f(l,nil,_,nil) = rev l
	in f(nil, args, tl(getregs args), formals)
       end

    fun stupidargs(f,args,vl) =
	let fun argregs(nil,_) = nil 
	      | argregs(v::rest,i) = (addbinding(v,i); i::argregs(rest,i+1))
	    val formals = argregs(vl,0)
	 in shuffle(f,args,formals);
	    formals
	end

  (* Compute the maximum amount of allocation done by this function (in bytes). *)
    fun sumAlloc exp = let
	  fun sum (RECORD (fields, _, exp'), max) = sum (exp', max+(length fields)+1)
	    | sum (SELECT (_, _, _, exp'), max) = sum (exp', max)
	    | sum (OFFSET (_, _, _, exp'), max) = sum (exp', max)
	    | sum (APP _, max) = max
	    | sum (SWITCH (_, lst), max) = max + lstMax(lst, 0)
	    | sum (PRIMOP (P.makeref, _, _, [exp']), max) = sum (exp', max+2)
	    | sum (PRIMOP (P.delay, _, _, [exp']), max) = sum (exp', max+2)
	    | sum (PRIMOP (P.update, _, _, [exp']), max) = sum (exp', max+4)
	    | sum (PRIMOP (P.:=, _, _, [exp']), max) = sum (exp', max+4)
	    | sum (PRIMOP (P.fadd, _, _, [exp']), max) = sum (exp', max+3)
	    | sum (PRIMOP (P.fsub, _, _, [exp']), max) = sum (exp', max+3)
	    | sum (PRIMOP (P.fmul, _, _, [exp']), max) = sum (exp', max+3)
	    | sum (PRIMOP (P.fdiv, _, _, [exp']), max) = sum (exp', max+3)
	    | sum (PRIMOP (_, _, _, [exp']), max) = sum (exp', max)
	    | sum (PRIMOP (_, _, _, lst), max) = max + lstMax(lst, 0)
	  and lstMax (nil, max) = max
	    |lstMax (e::rest, max) = let val m = sum (e, 0)
		in
		  if m > max then lstMax(rest, m) else lstMax(rest, max)
		end
	  in
	    (sum (exp, 0)) * 4
	  end

(**** This doesn't quite work yet
  (* return true if the expression needs to compute a PC relative value *)
    fun needsPC exp = let
	  fun chkVal (LABEL _) = true
	    | chkVal (REAL _) = true
	    | chkVal (STRING _) = true
	    | chkVal _ = false
	  fun chkVals [] = false
	    | chkVals (v::r) = (chkVal v) orelse (chkVals r)
	  fun chkFields [] = false
	    | chkFields ((v, _)::r) = (chkVal v) orelse (chkFields r)
	  fun chkExp (RECORD(fields, _, exp)) = (chkFields fields) orelse (chkExp exp)
	    | chkExp (SELECT(_, v, _, exp)) = (chkVal v) orelse (chkExp exp)
	    | chkExp (OFFSET(_, v, _, exp)) = (chkVal v) orelse (chkExp exp)
	    | chkExp (APP _) = false
	    | chkExp (SWITCH(v, exps)) = (chkVal v) orelse (chkExps exps)
	    | chkExp (PRIMOP(_, vl, _, exps)) = (chkVals vl) orelse (chkExps exps)
	  and chkExps [] = false
	    | chkExps (exp::r) = (chkExp exp) orelse (chkExps r)
	  in
	    chkExp exp
	  end
****)
    fun needsPC exp = true  (* assume PC relative addressing always *)

    val curFnAddr = ref(ref PositionIndependent)

    val (genStandardFn, genKnownFn) = (case beginStdFn
	 of NONE => let
	      fun begin gen (lab, cexp, _) = (
		    root1 := root cexp;
		    define lab;
		    checkLimit (sumAlloc cexp);
		    gen cexp)
	      in (begin, begin) end
	  | (SOME f) => let
	      fun beginStd gen (lab, cexp, closure) = (
		    root1 := root cexp;
		    define lab;
		    checkLimit (sumAlloc cexp);
		    curFnAddr := f (closure, lab);
		    if ((!(!curFnAddr) = PositionIndependent) andalso (needsPC cexp))
		      then ((!curFnAddr) := Relative)
		      else ();
		    gen cexp)
	      fun beginKnown gen (lab, cexp, addr) = let
		    val saveFnAddr = !curFnAddr
		    in
		      root1 := root cexp;
		      define lab;
		      checkLimit (sumAlloc cexp);
		      curFnAddr := addr;
		      if (needsPC cexp)
			then (addr := Relative; saveFnAddr := Relative)
			else ();
		      gen cexp;
		      case (!addr) of Relative => (saveFnAddr := Relative) | _ => ();
		      curFnAddr := saveFnAddr
		    end
	      in (beginStd, beginKnown) end
	  (* end case *))

    fun genfrag (_, STANDARD(ref NONE)) = ()
      | genfrag (lab, STANDARD(r as ref (SOME(fname,fmls,e)))) =
		 let val fmls' as closure::_ = standardformals fmls
		  in r := NONE;
		     List2.app2 addbinding (fmls, fmls');
		     align(); regmask fmls'; mark();
		     comment(Access.lvarName fname ^ ":\n");
		     genStandardFn gen (lab, e, regt closure)
		 end
      | genfrag (_, KNOWN _) = ()
      | genfrag (lab, REALfrag r) =
		    (align(); mark(); emitlong(desc_embedded_real);
		     define lab; comment("# real constant " ^ r ^ "\n");
		     realconst r
		     handle M.BadReal r => err ErrorMsg.COMPLAIN 
					("real constant out of range: " ^ r))
      | genfrag (lab, STRINGfrag s) =
		    (align(); mark();
		     emitlong(make_desc(size s, tag_embedded_string));
		     define lab; emitstring s; align())

  (* generate a new code label *)
    and genlab(lab, cexp) = (root1 := root cexp; define lab; gen cexp)

    and gen cexp =
	case cexp
	 of RECORD(vl,w,e) =>
		 alloc(w, any,  fn w' => 
			   (record((immed(make_desc(length vl, tag_record)),OFFp 0) ::
				      map (fn(x,p)=>(regbind x, p)) vl,
			           w');
			    gen e))
	  | SELECT(i,v,w,e) =>
	    alloc(w,any,  fn w' => (select(i,regbind v,w'); gen e))
	  | OFFSET(i,v,w,e) =>
	      alloc(w, v, fn w' => (offset(i, regbind v,w'); gen e))
	  | APP(func as VAR f, args) => 
			shuffle(func, args, standardformals args)
	  | APP(func as LABEL f, args) =>
	    (case know f
	      of KNOWN(_, ref(SOME formals), ref Relative) =>
			 ( !curFnAddr := Relative;
			   shuffle(func, args, formals))
	       | KNOWN(_, ref(SOME formals), _) =>
			  shuffle(func, args, formals)
	       | KNOWN((vl,cexp), r as (ref NONE), addr) =>
			let val formals = if (!CG.argrep)
						then allocparams(args, vl)
						else stupidargs(func, args, vl)
			 in r := SOME formals;
			    jmp (labmap f); align(); regmask formals; mark();
		    	    comment(Access.lvarName f ^ ":\n");
			    genKnownFn gen (labmap f, cexp, addr)
			end
	       | k as STANDARD _ =>
		      (shuffle(func, args, standardformals args);
		       genfrag(labmap f, k)))
	  | APP _ => ErrorMsg.impossible "constant func in CPSgen"
	  | SWITCH(v,l) => 
		let val lab = newlabel()
		    val labs = map (fn _ => newlabel()) l;
		    fun f(i, s::r) = (emitlab(i, s); f(i+4, r))
		      | f(_, nil) = ()
		    fun h(lab::labs, e::es) = (genlab(lab, e); h(labs,es))
		      | h(nil,nil) = ()
		 in fetchindexl(lab, arithtemp, regbind v);
		    jmpindexb(lab,arithtemp);
(*		    align();   temporarily removed so 68020 will work. *)
		    define lab;
		    f (0, labs);
		    h(labs,l)
		end
        | PRIMOP(P.+, [INT k, w],[x],[e]) =>
	      alloc(x, w, fn x' =>
		    (addl3t(immed(k+k), regbind w, x');
		     gen e))
        | PRIMOP(P.+, [w, v as INT _],x,e) => gen(PRIMOP(P.+,[v,w],x,e))
        | PRIMOP(P.+, [v,w],[x],[e]) =>
	      alloc(x, w, fn x' =>
		    (subl3(immed 1, regbind v, arithtemp);
		     addl3t(arithtemp, regbind w, x');
		     gen e))
        | PRIMOP(P.orb, [v,w],[x],[e]) =>
          alloc(x, w, fn x' => (orb(regbind v, regbind w, x'); gen e))
        | PRIMOP(P.andb, [v,w],[x],[e]) =>
          alloc(x, w, fn x' =>(andb(regbind v, regbind w, x'); gen e))
        | PRIMOP(P.xorb, [INT k, w],[x],[e]) =>
	      alloc(x, w, fn x' =>
		    (xorb(immed(k+k), regbind w, x');
		     gen e))
        | PRIMOP(P.xorb, [w,v as INT _],x,e) => gen(PRIMOP(P.xorb,[v,w],x,e))
        | PRIMOP(P.xorb, [v,w],[x],[e]) =>
	      alloc(x,any, fn x' => tempreg(x', fn x'' =>
		    (xorb(regbind v, regbind w, x'');
		     addl3(immed 1, x'', x');
		     gen e)))
       | PRIMOP(P.notb, [v],[x],[e]) =>
          alloc(x, any, fn x' =>
	            (subl3(regbind v, immed 0, x');
		     gen e))
       | PRIMOP(P.lshift, [INT k, w],[x],[e]) =>
	     alloc(x,w, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, regbind w, x'');
		    ashl(x'',immed(k+k),x'');
		    addl3(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.lshift, [v, INT k],[x],[e]) =>
	     alloc(x,v, fn x' => tempreg(x', fn x'' =>
		   (addl3(immed ~1, regbind v, x'');
		    ashl(immed k, x'', x'');
		    addl3(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.lshift, [v,w],[x],[e]) =>
	     alloc(x,w, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, regbind w, arithtemp);
		    addl3(immed ~1, regbind v, x'');
		    ashl(arithtemp, x'', x'');
		    addl3(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.rshift, [v, INT k],[x],[e]) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed k, regbind v, x'');
		    orb(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.rshift, [v,w],[x],[e]) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, regbind w, arithtemp);
		    ashr(arithtemp, regbind v, x'');
		    orb(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.-, [INT k,w],[x],[e]) =>
	     alloc(x, w, fn x' =>
		   (subl3t(regbind w, immed(k+k+2), x');
		    gen e))
       | PRIMOP(P.-, [v, INT k],[x],[e]) =>
	     alloc(x, v, fn x' =>
		   (subl3t(immed(k+k), regbind v, x');
		    gen e))
       | PRIMOP(P.-, [v,w],[x],[e]) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (subl3(regbind w, regbind v, x'');
		    addl3(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.*, [INT k, INT j],[x],[e]) =>
	    alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (move(immed k, x'');
	      mull2t(immed(j+j),x'');
	      addl3(immed 1, x'', x');
	      gen e)))
       | PRIMOP(P.*, [INT k, w],[x],[e]) =>
	     alloc(x,any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, regbind w, x'');
		    mull2t(immed(k+k), x'');
		    addl3(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.*, [v,w as INT _],x,e) => gen(PRIMOP(P.*,[w,v],x,e))
       | PRIMOP(P.*, [v,w],[x],[e]) =>
           alloc(x,any,fn x' => tempreg(x', fn x'' =>
	         (ashr(immed 1, regbind v, arithtemp);
		  subl3(immed 1, regbind w, x'');
		  mull2t(arithtemp,x'');
		  addl3(immed 1,x'',x');
		  gen e)))
       | PRIMOP(P.div, [INT k, INT j],[x],[e]) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (move(immed k, x'');
		    divl2(immed j, x'');
		    addl3t(x'',x'',x'');
		    addl3(immed 1, x'',x');
		    gen e)))
       | PRIMOP(P.div, [INT k,w],[x],[e]) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, regbind w, arithtemp);
		    move(immed k, x'');
		    divl2(arithtemp,x'');
		    addl3t(x'',x'',x'');
		    addl3(immed 1, x'',x');
		    gen e)))
       | PRIMOP(P.div, [v, INT k],[x],[e]) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, regbind v, x'');
		    divl2(immed k, x'');
		    addl3t(x'',x'',x'');
		    addl3(immed 1, x'',x');
		    gen e)))
       | PRIMOP(P.div, [v,w],[x],[e]) =>
            alloc(x,any, fn x' => tempreg(x', fn x'' =>
		  (ashr(immed 1, regbind w, arithtemp);
		   ashr(immed 1, regbind v, x'');
		   divl2(arithtemp,x'');
		   addl3t(x'',x'',x'');
		   addl3(immed 1, x'',x');
		   gen e)))
       | PRIMOP(P.!, [v],[w],[e]) => gen(SELECT(0,v,w,e))
       | PRIMOP(P.:=, [v,w],[],[e]) =>
	    let val v' = regbind v
	     in record([(immed(make_desc(3,tag_record)),OFFp 0), (v', OFFp 0),
		       (immed 1, OFFp 0), (storeptr, OFFp 0)], storeptr);
	        storeindexl(regbind w, v', immed 1);
	        gen e
	    end
       | PRIMOP(P.unboxedassign, [v,w],[],[e]) =>
              (storeindexl(regbind w, regbind v, immed 1); gen e)
       | PRIMOP(P.~, [v],[w],[e]) =>
	 alloc(w,any,fn w' => (subl3t(regbind v,immed 2,w'); gen e))
       | PRIMOP(P.makeref, [v],[w],[e]) =>
	    alloc(w,any, fn w' =>
		(record([(immed(make_desc(1,tag_array)),OFFp 0),
			 (regbind v, OFFp 0)], w');
		 gen e))
       | PRIMOP(P.delay, [i,v],[w],[e]) =>
	    alloc(w,any, fn w' =>
		(record([(regbind i, OFFp 0),(regbind v, OFFp 0)], w');
		 gen e))
       | PRIMOP(P.rangechk, [v,w],[],[d,e]) =>
	   let val false_lab = newlabel()
	    in rangeChk(regbind v, regbind w, false_lab); 
	       gen d;
	       genlab(false_lab, e)
	   end
       | PRIMOP(P.subscript, [v,w],[x],[e]) =>
			alloc(x,any, fn x' =>
			    (fetchindexl(regbind v, x', regbind w);
			     gen e))
       | PRIMOP(P.update, [a, i, v], [], [e]) =>
	    let val a' = regbind a and i' = regbind i
	     in record([(immed(make_desc(3,tag_record)),OFFp 0), (a',OFFp 0),
		        (i', OFFp 0), (storeptr, OFFp 0)], storeptr);
	        storeindexl(regbind v, a', i');
	        gen e
	    end
       | PRIMOP(P.unboxedupdate, [a, i, v], [], [e]) =>
		(storeindexl(regbind v, regbind a, regbind i);
		 gen e)
       | PRIMOP(P.alength, [a], [x], [e]) =>
	alloc(x,any,  fn x' => tempreg(x', fn x'' =>
	    (select(~1, regbind a, x'');
	     ashr(immed(width_tags-1),x'', x'');
	     orb(immed 1, x'', x');
	     gen e)))
       | PRIMOP(P.slength, [a as VAR _], [x], [e]) =>
	  alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (select(~1, regbind a, x'');
	      ashr(immed(width_tags-1), x'', x'');
	      orb(immed 1, x'', x');
	      gen e)))
       | PRIMOP(P.slength, [a], [x], [e]) =>
	  alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (move(regbind a, x');
	      select(~1,x',x'');
	      ashr(immed(width_tags-1), x'', x'');
	      orb(immed 1, x'', x');
	      gen e)))
       | PRIMOP(P.store, [s,INT i', INT v'], [], [e]) =>
	     (storeindexb(immed v', regbind s, immed i');
	      gen e)
       | PRIMOP(P.store, [s,INT i',v], [], [e]) =>
	     (ashr(immed 1, regbind v, arithtemp);
	      storeindexb(arithtemp, regbind s, immed i');
	      gen e)
       | PRIMOP(P.store, [s,i,INT v'], [], [e]) =>
	     (ashr(immed 1, regbind i, arithtemp);
	      storeindexb(immed v', regbind s, arithtemp);
	      gen e)
       | PRIMOP(P.store, [s,i,v], [], [e]) =>
	     let val v' = regbind v
	      in ashr(immed 1, regbind i, arithtemp);
		 ashr(immed 1, v', v');
	         storeindexb(v', regbind s, arithtemp);
		 addl3(v',v',v');
		 addl3(immed 1, v', v');
		 gen e
	     end
       | PRIMOP(P.ordof, [s as VAR _, INT k], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (fetchindexb(regbind s, v', immed k);
		    addl3(v',v',v');
		    addl3(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s, INT k], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (move(regbind s, v');
		    fetchindexb(v', v', immed k);
		    addl3(v',v',v');
		    addl3(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s as VAR _, i], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (ashr(immed 1, regbind i, arithtemp);
		    fetchindexb(regbind s, v', arithtemp);
		    addl3(v',v',v');
		    addl3(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s, i], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (ashr(immed 1, regbind i, arithtemp);
		    move(regbind s, v');
		    fetchindexb(v', v', arithtemp);
		    addl3(v',v',v');
		    addl3(immed 1, v',v');
		    gen e))
       | PRIMOP(P.profile, [index,incr],[],[c]) =>
			(case (globalvar,regbind index, regbind incr)
			  of (SOME r,i,j) => 
				(fetchindexl(r,arithtemp,i);
				 addl3(arithtemp,j,arithtemp);
				 subl3(immed 1,arithtemp,arithtemp);
			         storeindexl(arithtemp,r,i))
			  | _ => ();
			 gen c)
       | PRIMOP(P.boxed, [x],[],[a,b]) =>
		    let val lab = newlabel()
		     in bbs(immed 0, regbind x, lab); gen a; genlab(lab, b)
		    end
       | PRIMOP(P.gethdlr, [],[x],[e]) =>
		  alloc(x,any, fn x' => (move(exnptr,x'); gen e))
       | PRIMOP(P.sethdlr, [x],[],[e]) => (move(regbind x, exnptr); gen e)
       | PRIMOP(P.fmul, [x,y], [z], [e]) =>
		alloc(z,any, fn z' =>
		 (mulg3(regbind x, regbind y, z'); gen e))
       | PRIMOP(P.fdiv, [x,y], [z], [e]) =>
		alloc(z,any, fn z' =>
		 (divg3(regbind x, regbind y, z'); gen e))
       | PRIMOP(P.fadd, [x,y], [z], [e]) =>
		alloc(z,any, fn z' =>
		 (addg3(regbind x, regbind y, z'); gen e))
       | PRIMOP(P.fsub, [x,y], [z], [e]) =>
		alloc(z,any, fn z' =>
		 (subg3(regbind x, regbind y, z'); gen e))
       | PRIMOP(args as (P.ieql,_,_,_)) => compare(ibranch,NEQ,args)
       | PRIMOP(args as (P.ineq,_,_,_)) => compare(ibranch,EQL,args)
       | PRIMOP(args as (P.>   ,_,_,_)) => compare(ibranch,LEQ,args)
       | PRIMOP(args as (P.>=  ,_,_,_)) => compare(ibranch,LSS,args)
       | PRIMOP(args as (P.<   ,_,_,_)) => compare(ibranch,GEQ,args)
       | PRIMOP(args as (P.<=  ,_,_,_)) => compare(ibranch,GTR,args)
       | PRIMOP(args as (P.feql,_,_,_)) => compare(gbranch,NEQ,args)
       | PRIMOP(args as (P.fneq,_,_,_)) => compare(gbranch,EQL,args)
       | PRIMOP(args as (P.fgt ,_,_,_)) => compare(gbranch,LEQ,args)
       | PRIMOP(args as (P.flt ,_,_,_)) => compare(gbranch,GEQ,args)
       | PRIMOP(args as (P.fge ,_,_,_)) => compare(gbranch,LSS,args)
       | PRIMOP(args as (P.fle ,_,_,_)) => compare(gbranch,GTR,args)
       | _ => ErrorMsg.impossible "3312 in CPSgen"

    and compare(branch,test, (_,[v,w],[],[d,e])) =
		let val lab = newlabel()
		 in branch(test,regbind v, regbind w, lab); 
		    gen d; genlab(lab, e)
		end
       
in  (* not necessary with regmasks: emitlong 1; Bogus tag for spacing, boot_v. *)
    let fun loop nil = ()
          | loop (frag::r) = (frags := r; genfrag frag; loop(!frags))
    in loop(!frags)
    end
end

end
