(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPSgen(M: CMACHINE) :
  sig structure CPS : CPS
    val codegen : (((CPS.lvar * CPS.lvar list * CPS.cexp) * bool) list
		   * ErrorMsg.complainer)
		   -> unit
  end =
struct

structure CPS = CPS
open CPS M System.Tags Access
structure CG = System.Control.CG

datatype frag
  = STANDARD of (lvar * lvar list * cexp) option ref
  | KNOWN of (lvar list * cexp) * EA list option ref * addressing ref
  | STRINGfrag of string
  | REALfrag of string

val standardformals2 = [standardcont, standardarg]
val standardformals3 = [standardclosure,standardarg,standardcont]
val notastandardformal::_ = miscregs
val any = notastandardformal
val allregs = standardformals3 @ miscregs

fun isreg' r = case isreg r of NONE => false | _ => true

val maxConceivableRegs = 50
val knowngen = System.Control.CG.knowngen
val stdgen = System.Control.CG.stdgen

local
  val num2reg = array(maxConceivableRegs, hd allregs)
  val _ = app (fn r => case isreg r of SOME i => update(num2reg,i,r)) allregs
  val allregs' = map (fn r => case isreg r of SOME i => i) allregs
  val okreg = array(maxConceivableRegs, false)
  fun mark b = (fn r => case isreg r of (SOME i) => update(okreg,i,b) | _ => ())
  val _ = app (mark true) allregs
  val reg_queue = ref allregs'
  fun getallregs'() = let val l as h::t = !reg_queue
		       in reg_queue := t@[h]; l
		      end
 in
    exception Getscratch
    fun getscratch(preferred, prohibited) =
        let fun f(x::a) = if okreg sub x then num2reg sub x else f a
              | f nil = raise Getscratch
         in app (mark false) prohibited;
	    (case isreg preferred
	     of SOME i => (if okreg sub i then preferred else f allregs')
	      | _ => f (getallregs'()))
	     before app (mark true) prohibited
	    handle e => (app (mark true) prohibited; raise e)
        end
end

fun sublist test =
  let fun subl(a::r) = if test a then a::(subl r) else subl r
        | subl [] = []
  in  subl
  end

fun split pred nil = (nil,nil)
  | split pred (a::r) = let val (x,y) = split pred r
			 in if pred a then (a::x, y) else (x, a::y)
		        end

fun codegen(funs : ((lvar * lvar list * cexp) * bool) list, err) =
let 
    exception Regbind
    val regbindtable : EA Intmap.intmap = Intmap.new(32, Regbind)
    val addbinding = Intmap.add regbindtable

    exception Know
    val knowtable : frag Intmap.intmap = Intmap.new(32, Know)
    val addknow = Intmap.add knowtable
    val know = Intmap.map knowtable

    fun notconstant v = (know v; false) handle Know => true

    exception Freemap
    val freemaptable : lvar list Intmap.intmap = Intmap.new(32, Freemap)
    val freemap = sublist notconstant o Intmap.map freemaptable

    local val addr = ref(ref PositionIndependent) in
    fun makefrag ((f,vl,e),known) = 
	let val lab = newlabel()
	    val knowledge = if known
	      then (inc knowngen; KNOWN((vl,e),ref NONE,ref PositionIndependent))
	      else (inc stdgen; STANDARD(ref(SOME(f,vl,e))))
	 in addknow(f, knowledge); addbinding(f,lab);
	    FreeMap.freemap (Intmap.add freemaptable) e;
	    (lab,knowledge)
	end
    end

    val frags = ref(map makefrag funs)
    fun addfrag f = frags := f :: !frags

    fun regbind(VAR v) = Intmap.map regbindtable v
      | regbind(INT i) = (immed(i+i+1) handle Overflow =>
		          ErrorMsg.impossible "Overflow in cps/generic.sml")
      | regbind(STRING s) = let val lab = newlabel()
			     in addfrag(lab, STRINGfrag s);
				lab
			    end
      | regbind(REAL s) = let val lab = newlabel()
			     in addfrag(lab, REALfrag s);
				lab
			    end

    fun root(RECORD(_,_,e)) = root e
      | root(SELECT(_,_,_,e)) = root e
      | root(OFFSET(_,_,_,e)) = root e
      | root(SWITCH(_,e::_)) = root e
      | root(PRIMOP(_,_,_,e::_)) = root e
      | root(e as APP _) = e

    val root1 = ref(APP(VAR 0, []))

    fun alloc(v,cexp,default,continue) =
	let val APP(VAR f, wl) = !root1
	    val proh = map (regbind o VAR) (freemap v)
	    fun delete (z,nil) = nil
	      | delete (z, a::r) = if eqreg a z then r else a::delete(z,r)
	    fun get(good,bad) =
		let val r = getscratch(good,bad@proh)
		            handle Getscratch => getscratch(default,proh)
	         in addbinding(v,r); continue r
		end
	    fun find fmls = 
               let fun g(VAR w ::wl, r::rl) = 
				if w=v then get(r, delete(r,fmls))
				       else g(wl,rl)
		     | g(_::wl,_::rl) = g(wl,rl)
		     | g _ = get(default, fmls)
		in g(wl,fmls)
	       end
         in if v=f then get(default,standardformals3)
	    else
	    case (know f handle Know => STANDARD(ref NONE))
	     of KNOWN(_,ref(SOME fmls),_) => find fmls
	      | KNOWN(_,ref NONE,_) => get(default,nil)
	      | STANDARD _ => case length wl
		 	       of 2 => find standardformals2
				| 3 => find standardformals3
			        | _ => ErrorMsg.impossible "cps vax 44"
        end

    fun shuffle(func as VAR f, args,formals) =
      let val (fv,used,args,formals) =
	    let val fv = regbind func
	     in if exists (eqreg fv) formals
		  then let val x = getscratch(any, args@formals)
			in move(fv,x); addbinding(f,x);
			   (x,[x],args,formals)
		       end
		    handle Getscratch =>
		     (addbinding(f,notastandardformal);
		      (notastandardformal, nil, fv::args, notastandardformal::formals))
		  else (fv,[fv],args,formals)
	    end
	  fun mate(a::al, b::bl)= (a,b)::mate(al,bl) 
	    | mate _ = nil
	  val (inreg,notinreg) = split (isreg' o #1) (mate(args,formals))
	  val (matched, notmatched) = split (fn(x,y)=>eqreg x y) inreg

	  fun f(nil, used) = ()
	    | f (pairs,used) = 
	    let val u' = map #1 pairs @ used
	        fun movable (a, b) = not (exists (eqreg b) u')
	     in case split movable pairs
	         of (nil,(a,b)::r) => 
			    let val x = getscratch(any,u')
			      in move(a,x); f((x,b)::r, used)
			    end
	          | (m,m') => (app move m; f(m', (map #2 m) @ used))
	    end
      in f(notmatched, (map #1 matched) @ used);
         app move notinreg;
	 jmp fv
     end

    fun allocparams(args,formals) =
       let fun f(already,a::ar,b::br) =
		let val z = getscratch(a, already@ar)
		 in addbinding(b,z); 
		    if eqreg a z then () else move(a,z);
		       f(z::already,ar,br)
		end
	     | f(l,nil,nil) = rev l
	in f(nil,args,formals)
       end

    fun stupidargs(f,args',vl) =
	let fun argregs(nil,_) = nil 
	      | argregs(a::rest,r::r') = (addbinding(a,r);
					  r::argregs(rest,r'))
	    val formals = argregs(vl,allregs)
	 in shuffle(f,args',formals);
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

    val (genStandardFn, genKnownFn) = (case beginStdFn
	 of NONE => let
	      fun begin gen (lab, cexp, _) = (
		    root1 := root cexp;
		    define lab;
		    checkLimit (sumAlloc cexp);
		    gen cexp)
	      in (begin, begin) end
	  | (SOME f) => let
	      fun beginStd gen (lab, cexp, closure) = let
		    val saveFnAddr = !curFnAddr
		    val newFnAddr = ref PositionIndependent
		    in
		      root1 := root cexp;
		      curFnAddr := newFnAddr;
		      define lab;
		      checkLimit (sumAlloc cexp);
		      f (closure, lab, newFnAddr);
		      gen cexp;
		      curFnAddr := saveFnAddr
		    end
	      fun beginKnown gen (lab, cexp, addr) = let
		    val saveFnAddr = !curFnAddr
		    in
		      root1 := root cexp;
		      curFnAddr := addr;
		      define lab;
		      checkLimit (sumAlloc cexp);
		      gen cexp;
		      case (!addr) of Relative => (saveFnAddr := Relative) | _ => ();
		      curFnAddr := saveFnAddr
		    end
	      in (beginStd, beginKnown) end
	  (* end case *))

    fun genfrag (_, STANDARD(ref NONE)) = ()
      | genfrag (lab, STANDARD(r as ref (SOME(fname,[f,a,c],e)))) =
		    (r := NONE;
		     List2.app2 addbinding ([f,a,c],standardformals3);
		     align(); mark();
		     comment(Access.lvarName fname ^ ":\n");
		     genFun(lab, e, standardclosure))
      | genfrag (lab, STANDARD(r as ref (SOME(fname,[f,a],e)))) =
		    (r := NONE;
		     List2.app2 addbinding ([f,a],standardformals2);
		     align(); mark();
		     comment(Access.lvarName fname ^ ":\n");
		     genFun(lab, e, standardcont))
      | genfrag (_, STANDARD _) = ErrorMsg.impossible "standard with wrong args"
      | genfrag (_, KNOWN _) = ()
      | genfrag (lab, REALfrag r) =
		    (align(); mark(); emitlong(8 * power_tags + tag_embedded);
		     define lab; comment("# real constant " ^ r ^ "\n");
		     realconst r
		     handle M.BadReal r => err ErrorMsg.COMPLAIN 
					("real constant out of range: " ^ r))
      | genfrag (lab, STRINGfrag s) =
		    (align(); mark();
		     emitlong(size s * power_tags + tag_embedded);
		     define lab; emitstring s; align())

  (* generate a new code label *)
    and genlab(lab, cexp) = (root1 := root cexp; define lab; gen cexp)

  (* generate a new function header *)
    and genFun arg = (genStandardFn gen arg)

    and gen cexp =
	case cexp
	 of RECORD(vl,w,e) =>
		 alloc(w, e,any,  fn w' => 
			   (record((immed(16*(length vl)+1),OFFp 0) ::
				      map (fn(x,p)=>(regbind x, p)) vl,
			           w');
			    gen e))
	  | SELECT(i,v,w,e) =>
	    alloc(w, e,any,  fn w' => (select(i,regbind v,w'); gen e))
	  | OFFSET(i,v,w,e) =>
	    let val v' = regbind v
	    in alloc(w, e,v', fn w' => (offset(i,v',w'); gen e))
	    end
	  | APP(func as VAR f,args) =>
	    (case (map regbind args,
		   know f handle Know => STANDARD(ref NONE))
	      of (args', KNOWN(_, ref(SOME formals), ref Relative)) =>
			 ((!curFnAddr) := Relative;
			  shuffle(func, args', formals))
	       | (args', KNOWN(_, ref(SOME formals), _)) => 
				shuffle(func, args', formals)
	       | (args', KNOWN((vl,cexp), r as (ref NONE), addr)) => let
			  val lab = newlabel()
			  in
		    	    r := SOME(if (!CG.argrep)
					then allocparams(args', vl)
					else stupidargs(func, args', vl));
			  (* replace fall-through with a jump to insure a mark
			   * at the beginning of every function.
			   *)
			    jmp lab; align(); mark(); define lab;
		    	    comment(Access.lvarName f ^ ":\n");
			    genKnownFn gen (regbind func, cexp, addr)
			  end
	       | (args' as [_,_], STANDARD (ref NONE)) =>
			  shuffle(func, args',standardformals2)
	       | (args' as [_,_,_], STANDARD (ref NONE)) =>
			  shuffle(func, args',standardformals3)
	       | (args' as [_,_], k as STANDARD(ref(SOME _))) =>
		          (shuffle(func, args',standardformals2); 
			   genfrag(regbind func, k))
	       | (args' as [_,_,_], k as STANDARD(ref(SOME _))) =>
		          (shuffle(func, args',standardformals3);
			   genfrag(regbind func, k)))
	  | APP _ => ErrorMsg.impossible "constant func in CPSgen"
	  | SWITCH(v,l) => 
		let val lab = newlabel()
		    val labs = map (fn _ => newlabel()) l;
		    fun f(i, s::r) = (emitlab(i, s); f(i+4, r))
		      | f(_, nil) = ()
		    fun h(lab::labs, e::es) = (genlab(lab, e); h(labs,es))
		      | h(nil,nil) = ()
		 in fetchindexl(lab, arithtemp, regbind v);
		    jmpindexb lab;
(*		    align();   temporarily removed so 68020 will work. *)
		    define lab;
		    f (0, labs);
		    h(labs,l)
		end

	  | PRIMOP (i,vl,wl,el) => primops i (vl,wl,el)

(* warning:  on three-address instructions, be careful about 
   non-pointers in registers.  On some machines,
    addl3(a,b,c) is translated to:   mov(b,c); add(a,c);
   and it's dangerous when b is a non-pointer.  In such a case,
   usually a is "safe", so that addl3(b,a,c) works better.
  The rule is, therefore:  if the destination is a pointer register,
    then b must also be a tagged value *)

    and compare(branch,test) ([v,w],[],[d,e]) =
		let val lab = newlabel()
		 in branch(test,regbind v, regbind w, lab); 
		    gen d; genlab(lab, e)
		end
    and primops p =
        case p of
          P.+ => (fn ([v,w],[x],[e]) =>
	  let val v' = regbind v and w' = regbind w
	  in case (isimmed v', isimmed w',!CG.arithopt) of
	       (SOME k, _,true) =>
                 (alloc(x,e,w', fn x' => addl3t(immed(k-1),w',x')))
(* the next case must be done (by all machines) with v and x in
   root registers (for offset computations in "boot") *)
	     | (_, SOME k,true) =>
                 (alloc(x,e,v', fn x' => addl3t(immed(k-1),v',x')))
	     | _ => alloc(x,e,w',fn x' => (subl3(immed 1,v',arithtemp);
					   addl3t(arithtemp, w', x')));
	     gen e
	  end)
       | P.orb => (fn ([v,w],[x],[e]) =>
        let val w' =  regbind w
         in alloc(x,e,w', fn x' => (orb(regbind v, w', x'); gen e))
        end)
       | P.andb => (fn ([v,w],[x],[e]) =>
        let val w' =  regbind w
         in alloc(x,e,w', fn x' => (andb(regbind v, w', x'); gen e))
        end)
       | P.xorb => (fn ([v,w],[x],[e]) =>
         let val v' = regbind v and w' = regbind w
	 in alloc(x,e,any,fn x' => (case (isimmed v', isimmed w',!CG.arithopt)
	    of(SOME k,_,true) => xorb(immed(k-1), w', x')
	    | (_,SOME k,true) => xorb(v', immed(k-1), x')
	    | _ => (xorb(v', w', arithtemp); orb(immed 1, arithtemp, x'));
				    gen e))
	 end)
       | P.notb => (fn ([v],[x],[e]) =>
          alloc(x,e,regbind v, fn x' =>
	     (notb(regbind v, x');
	      orb(immed 1, x', x');
	      gen e)))
       | P.lshift => (fn ([v,w],[x],[e]) =>
         let val v' = regbind v and w' = regbind w
	 in alloc(x,e,any, fn x' => (
		  case (isimmed v', isimmed w',!CG.arithopt)
		   of (SOME k,_,true) =>
			(ashr(immed 1,w',arithtemp); ashl(arithtemp,immed(k-1), x'))
		    | (_,SOME k,true) => (
			addl3(immed(~1),v',arithtemp);
			ashl(immed(Bits.rshift(k,1)), arithtemp, x'))
		    | _ => (
			ashr(immed 1, w',arithtemp);
			addl3(immed(~1),v',arithtemp2);
			ashl(arithtemp, arithtemp2, x'));
		  orb(immed 1, x', x');
		  gen e))
	 end)
       | P.rshift => (fn ([v,w],[x],[e]) =>
         let val v' = regbind v and w' = regbind w
         in alloc(x,e,v', fn x' => (
            case (isimmed w', !CG.arithopt)
	     of (SOME k,true) => ashr(immed(Bits.rshift(k,1)), v', x')
	      | _ => (ashr(immed 1, w',arithtemp); ashr(arithtemp, v', x'));
	    orb(immed 1, x', x');
	    gen e))
         end)
       | P.- => (fn ([v,w],[x],[e]) =>
	 let val v' = regbind v and w' = regbind w
	 in case (isimmed v', isimmed w',!CG.arithopt) of
	      (SOME k, _,true) => alloc(x,e,w', fn x' => 
					subl3t(w', immed(k+1), x'))
	    | (_, SOME k,true) => alloc(x,e,v', fn x' =>
				     subl3t(immed(k-1),v',x'))
	    | _ => alloc(x, e,v',fn x' => (subl3(immed 1,w',arithtemp);
					   subl3t(arithtemp, v', x')));
	    gen e
	 end)
       | P.* => (fn ([v,w],[x],[e]) =>
         let val v' = regbind v and w' = regbind w
         in alloc(x,e,any,fn x' =>
            (case (isimmed v', isimmed w',!CG.arithopt) of
               (SOME k,_,true) => (ashr(immed 1, w', arithtemp);
			      mull2t(immed(k-1),arithtemp))
	     | (_,SOME k,true) => (ashr(immed 1, v', arithtemp);
			      mull2t(immed(k-1),arithtemp))
	     | _ => (ashr(immed 1, v', arithtemp);
		     subl3(immed 1, w', arithtemp2);
		     mull2t(arithtemp2,arithtemp));
	     orb(immed 1,arithtemp,x');
	     gen e))
         end)				  
       | P.div => (fn ([v,w],[x],[e]) =>
         let val v' = regbind v and w' = regbind w
         in alloc(x, e,any, fn x' =>
		  (case (isimmed v', isimmed w', !CG.arithopt) of
		      (SOME k,_,true) =>
			(move(immed(Bits.rshift(k,1)),arithtemp);
			 ashr(immed 1, w', arithtemp2);
			 divl2(arithtemp2,arithtemp))
		    | (_,SOME k,true) => 
			(ashr(immed 1, v', arithtemp);
			 divl2(immed(Bits.rshift(k,1)),arithtemp))
		    | _ => 
			(ashr(immed 1, v', arithtemp);
			 ashr(immed 1, w', arithtemp2);
			 divl2(arithtemp2,arithtemp));
		    addl3(arithtemp, arithtemp, arithtemp);
		    orb(immed 1, arithtemp,x');
		    gen e))
         end)
       | P.! => (fn ([v],[w],[e]) => gen(SELECT(0,v,w,e)))
       | P.:= => (fn ([v,w],[],[e]) =>
	    let val v' = regbind v
	     in record([(immed(16*3+1),OFFp 0), (v', OFFp 0),
		       (immed 1, OFFp 0), (storeptr, OFFp 0)], storeptr);
	        storeindexl(regbind w, v', immed 1);
	        gen e
	    end)
       | P.unboxedassign => (fn ([v,w],[],[e]) =>
              (storeindexl(regbind w, regbind v, immed 1); gen e))
       | P.~ => (fn ([v],[w],[e]) =>
	 alloc(w,e,any,fn w' => (subl3t(regbind v,immed 2,w'); gen e)))
       | P.makeref =>
	 (fn ([v],[w],[e]) =>
	    alloc(w, e,any, fn w' =>
		(record([(immed(power_tags+tag_array),OFFp 0),
			 (regbind v, OFFp 0)], w');
		 gen e)))
       | P.delay =>
	 (fn ([i,v],[w],[e]) =>
	    alloc(w, e,any, fn w' =>
		(record([(regbind i, OFFp 0),(regbind v, OFFp 0)], w');
		 gen e)))
       | P.ieql => compare(ibranch,NEQ)
       | P.ineq => compare(ibranch,EQL)
       | P.> => compare(ibranch,LEQ)
       | P.>= => compare(ibranch,LSS)
       | P.< => compare(ibranch,GEQ)
       | P.<= => compare(ibranch,GTR)
       | P.rangechk => (fn ([v,w],[],[d,e]) => let
	   val false_lab = newlabel()
	   in
	      rangeChk(regbind v, regbind w, false_lab); 
	      gen d;
	      genlab(false_lab, e)
	   end)
       | P.subscript => (fn ([v,w],[x],[e]) =>
			alloc(x, e,any, fn x' =>
			    (fetchindexl(regbind v, x', regbind w);
			     gen e)))
       | P.update => (fn ([a, i, v], [], [e]) =>
	    let val a' = regbind a and i' = regbind i
	     in record([(immed(16*3+1),OFFp 0), (a',OFFp 0),
		        (i', OFFp 0), (storeptr, OFFp 0)], storeptr);
	        storeindexl(regbind v, a', i');
	        gen e
	    end)
       | P.unboxedupdate => (fn ([a, i, v], [], [e]) =>
	(storeindexl(regbind v, regbind a, regbind i);
	 gen e))
       | P.alength => (fn ([a], [w], [e]) =>
	alloc(w,  e,any,  fn w' =>
	    (select(~1, regbind a, arithtemp);
	     ashr(immed(width_tags-1),arithtemp, arithtemp);
	     orb(immed 1, arithtemp, w');
	     gen e)))
       | P.slength => (fn ([a], [w], [e]) =>
	  alloc(w, e,any, fn w' =>
	    let val a' = regbind a
	    in if isreg' a'
		   then select(~1,a',arithtemp)
		   else (move(a',w'); select(~1,w',arithtemp));
		ashr(immed(width_tags-1), arithtemp, arithtemp);
		orb(immed 1, arithtemp, w');
	        gen e
	    end))
       | P.store => (fn ([s,i,v], [], [e]) => let
		val i' = regbind i and v' = regbind v
		val indx = case (isimmed i', !CG.arithopt)
		     of (SOME k, true) => immed(Bits.rshift(k, 1))
		      | _ => (ashr(immed 1, i', arithtemp); arithtemp)
		val src = case (isimmed v', !CG.arithopt)
		     of (SOME k, true) => immed(Bits.rshift(k, 1))
		      | _ => (ashr(immed 1, v', arithtemp2); arithtemp2)
		in
		  storeindexb(src, regbind s, indx);
	          gen e
		end)
       | P.ordof => (fn ([s,i], [v], [e]) => (
	    alloc(v, e, any, fn v' => let
	      val s' = regbind s and i' = regbind i
	      val indx = case (isimmed i', !CG.arithopt)
		   of (SOME k, true) => immed(Bits.rshift(k, 1))
		    | _ => (ashr(immed 1, i', arithtemp); arithtemp)
	      in
		if (isreg' s')
		  then fetchindexb(s', arithtemp2, indx)
		  else (move(s', v'); fetchindexb(v', arithtemp2, indx));
		addl3(arithtemp2, arithtemp2, arithtemp2);
		orb(immed 1, arithtemp2, v');
		gen e
	      end)))
       | P.profile => (fn ([index,incr],[],[c]) =>
			(case (globalvar,regbind index, regbind incr)
			  of (SOME r,i,j) => 
				(fetchindexl(r,arithtemp,i);
				 addl3(arithtemp,j,arithtemp);
				 subl3(immed 1,arithtemp,arithtemp);
			         storeindexl(arithtemp,r,i))
			  | _ => ();
			 gen c))
       | P.boxed => (fn ([x],[],[a,b]) =>
		    let val lab = newlabel()
		     in bbs(immed 0, regbind x, lab); gen a; genlab(lab, b)
		    end)
       | P.gethdlr => (fn ([],[x],[e]) =>
		  alloc(x, e,any, fn x' => (move(exnptr,x'); gen e)))
       | P.sethdlr => (fn ([x],[],[e]) => (move(regbind x, exnptr); gen e))
       | P.fmul =>  (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (mulg3(regbind x, regbind y, z'); gen e)))
       | P.fdiv =>  (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (divg3(regbind x, regbind y, z'); gen e)))
       | P.fadd => (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (addg3(regbind x, regbind y, z'); gen e)))
       | P.fsub => (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (subg3(regbind x, regbind y, z'); gen e)))
       | P.feql => compare(gbranch,NEQ)
       | P.fneq => compare(gbranch,EQL)
       | P.fgt => compare(gbranch,LEQ)
       | P.flt => compare(gbranch,GEQ)
       | P.fge => compare(gbranch,LSS)
       | P.fle => compare(gbranch,GTR)
       
in  emitlong 1; (* Bogus tag for spacing, boot_v. *)
    let fun loop nil = ()
          | loop (frag::r) = (frags := r; genfrag frag; loop(!frags))
    in loop(!frags)
    end
end

end
