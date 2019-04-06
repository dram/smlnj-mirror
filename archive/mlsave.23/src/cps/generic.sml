functor CPSgen(M: CMACHINE) :
     sig structure CPS : CPS
	 val codegen : (((CPS.lvar * CPS.lvar list * CPS.cexp) * bool) list
		        * (CPS.lvar -> CPS.const)) -> unit
	 val knowngen : int ref
	 val stdgen : int ref
     end =
struct

structure CPS = CPS
open CPS M System.Tags Access

datatype frag = STANDARD of (lvar * lvar list * cexp) option ref
	      | KNOWN of (lvar list * cexp) * EA list option ref
	      | CONSTfrag of const

val standardformals2 = [standardcont, standardarg]
val standardformals3 = [standardclosure,standardarg,standardcont]
val notastandardformal::_ = miscregs
val any = notastandardformal

fun isreg' r = case isreg r of NONE => false | _ => true

val maxConceivableRegs = 50
val knowngen = System.Control.CG.knowngen
val stdgen = System.Control.CG.stdgen
local
  val allregs = standardformals3 @ miscregs
  val num2reg = array(maxConceivableRegs, hd allregs)
  val _ = app (fn r => case isreg r of SOME i => update(num2reg,i,r)) allregs
  val allregs' = map (fn r => case isreg r of SOME i => i) allregs
  val okreg = array(maxConceivableRegs, false)
  fun mark b = (fn r => case isreg r of (SOME i) => update(okreg,i,b) | _ => ())
  val _ = app (mark true) allregs
 in
    exception Getscratch
    fun getscratch(preferred, prohibited) =
        let fun f(x::a) = if okreg sub x then num2reg sub x else f a
              | f nil = raise Getscratch
         in app (mark false) prohibited;
	    (case isreg preferred
	     of SOME i => (if okreg sub i then preferred else f allregs')
	      | _ => f allregs')
	     before app (mark true) prohibited
	    handle e => (app (mark true) prohibited; raise e)
        end
end

fun split pred nil = (nil,nil)
  | split pred (a::r) = let val (x,y) = split pred r
			 in if pred a then (a::x, y) else (x, a::y)
		        end

fun codegen(funs : ((lvar * lvar list * cexp) * bool) list,
	    ctable : lvar -> const) =
let 
    exception Regbind
    val regbindtable : EA Intmap.intmap = Intmap.new Regbind
    val addbinding = Intmap.add regbindtable

    exception Know
    val knowtable : frag Intmap.intmap = Intmap.new Know
    val addknow = Intmap.add knowtable
    val know = Intmap.map knowtable

    exception Freemap
    val freemaptable : lvar list Intmap.intmap = Intmap.new Freemap
    val freemap = Intmap.map freemaptable

    fun makefrag ((f,vl,e),known) = 
	(addknow(f, if known then (inc knowngen; KNOWN((vl,e),ref NONE))
			     else (inc stdgen; STANDARD(ref(SOME(f,vl,e)))));
	 addbinding(f,newlabel());
	 FreeMap.freemap (Intmap.add freemaptable) e;
	 f)

    val frags = ref(map makefrag funs)
    fun addfrag f = frags := f :: !frags

    fun regbind v =
	Intmap.map regbindtable v
	handle Regbind =>
	(case ctable v of
	   INTconst i => (immed(i+i+1) handle Overflow =>
			  ErrorMsg.impossible "Overflow in cps/generic.sml")
	 | f => let val lab = newlabel()
		in addbinding(v,lab); addknow(v, CONSTfrag f); addfrag v; lab
		end)

    fun root(RECORD(_,_,e)) = root e
      | root(SELECT(_,_,_,e)) = root e
      | root(OFFSET(_,_,_,e)) = root e
      | root(SWITCH(_,e::_)) = root e
      | root(PRIMOP(_,_,_,e::_)) = root e
      | root(e as APP _) = e

    val root1 = ref(APP(0,[]))

    fun alloc(v,cexp,default,continue) =
	let val APP(f,wl) = !root1
	    val proh = map regbind (freemap v)
	    fun delete (z,nil) = nil
	      | delete (z, a::r) = if eqreg a z then r else a::delete(z,r)
	    fun get(good,bad) =
		let val r = getscratch(good,bad@proh)
		            handle Getscratch => getscratch(default,proh)
	         in addbinding(v,r); continue r
		end
	    fun find fmls = 
               let fun g(w::wl, r::rl) = if w=v then get(r, delete(r,fmls))
					        else g(wl,rl)
		     | g(nil,nil) = get(default, fmls)
		     | g _ = ErrorMsg.impossible "cps vax 33"
		in g(wl,fmls)
	       end
         in if v=f then get(default,standardformals3)
	    else
	    case (know f handle Know => STANDARD(ref NONE))
	     of KNOWN(_,ref(SOME fmls)) => find fmls
	      | KNOWN(_,ref NONE) => get(default,nil)
	      | STANDARD _ => case length wl
		 	       of 2 => find standardformals2
				| 3 => find standardformals3
			        | _ => ErrorMsg.impossible "cps vax 44"
        end

    fun shuffle(func, args,formals) =
      let val (used,args,formals) =
	    let val fv = regbind func
	     in if exists (eqreg fv) formals
		  then let val x = getscratch(any, args@formals)
			in move(fv,x); addbinding(func,x); ([x],args,formals)
		       end
		    handle Getscratch =>
		     (addbinding(func,notastandardformal);
		      (nil, fv::args, notastandardformal::formals))
		  else ([fv],args,formals)
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
      in f(notmatched, (map #1 matched) @ used); app move notinreg
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

    fun call(f,args, lab, KNOWN(_,ref(SOME formals))) =
	  (shuffle(f, map regbind args,formals); jmp lab)
      | call(f,args, lab, KNOWN((vl,cexp), r as ref(NONE))) =
	  (r := SOME(allocparams(map regbind args,vl));
	   comment(Access.lvarName f ^ ":\n"); genlab(lab,cexp))
      | call(f,args as [_,_], lab, STANDARD (ref NONE)) =
	  (shuffle(f, map regbind args,standardformals2); jmp lab)
      | call(f,args as [_,_,_], lab, STANDARD (ref NONE)) =
	  (shuffle(f, map regbind args,standardformals3); jmp lab)
      | call(f,args as [_,_], lab, STANDARD(ref(SOME _))) =
	  (shuffle(f, map regbind args,standardformals2); jmp lab; genfrag f)
      | call(f,args as [_,_,_], lab, STANDARD(ref(SOME _))) =
	  (shuffle(f, map regbind args,standardformals3); jmp lab; genfrag f)

    and genfrag f = case (regbind f, know f)
	    of (_, STANDARD(ref NONE)) => ()
	     | (lab, STANDARD(r as ref (SOME(fname,[f,a,c],e)))) =>
		    (r := NONE;
		     List2.app2 addbinding ([f,a,c],standardformals3);
		     align(); mark();
		     comment(Access.lvarName fname ^ ":\n");
		     genlab(lab,e))
	     | (lab, STANDARD(r as ref (SOME(fname,[f,a],e)))) =>
		    (r := NONE;
		     List2.app2 addbinding ([f,a],standardformals2);
		     align(); mark();
		     comment(Access.lvarName fname ^ ":\n");
		     genlab(lab,e))
	     | (_, STANDARD _) => ErrorMsg.impossible "standard with wrong args"
	     | (_, KNOWN _) => ()
	     | (lab, CONSTfrag(REALconst r)) =>
		    (align(); mark(); emitlong(8 * power_tags + tag_embedded);
		     define lab; comment("# real constant " ^ r ^ "\n");
		     realconst r)
	     | (lab, CONSTfrag(STRINGconst s)) =>
		    (align(); mark();
		     emitlong(length s * power_tags + tag_embedded);
		     define lab; emitstring s; align())

    and genlab(lab,cexp) = (root1 := root cexp; define lab; gen cexp)

    and gen cexp =
	case cexp
	 of RECORD(vl,w,e) =>
(* the only time that arithtemp is allowed to hold a pointer
   is during an allocate *)
		let val len = length vl
		    val vl' = map (fn (x,p) => (regbind x, p)) (rev vl)
		    fun f(i,(r,OFFp 0)::vl) = (storefield(i,r); f(i-1,vl))
		      | f(i,(r,OFFp j)::vl) =
			  (offset(j,r,arithtemp); f(i,(arithtemp,OFFp 0)::vl))
		      | f(i,(r,SELp(j,path))::vl) =
			  (select(j,r,arithtemp); f(i,(arithtemp,path)::vl))
		      | f(_,nil) = ()
		 in alloc(w, e,any,  fn w' => 
			(f(len - 1, vl');
			 storefield(~1,immed(16*(len)+1));
			 move(dataptr,w'); 
			 addtodataptr(len + 1);
			 gen e))
		end

	  | SELECT(i,v,w,e) =>
		 alloc(w, e,any,  fn w' => (select(i,regbind v,w'); gen e))
	  | OFFSET(i,v,w,e) =>
	    let val v' = regbind v
	    in alloc(w, e,v', fn w' => (offset(i,v',w'); gen e))
	    end
	  | APP(f,args) =>
	    call (f, args, regbind f, know f handle Know => STANDARD(ref NONE))
	  | SWITCH(v,l) => 
		let val lab = newlabel()
		    val labs = map (fn _ => newlabel()) l;
		    fun f(i, s::r) = (emitlab(i, s); f(i+4, r))
		      | f(_, nil) = ()
		    fun h(lab::labs, e::es) = (genlab(lab,e); h(labs,es))
		      | h(nil,nil) = ()
		 in ashl(immed ~1, regbind v, arithtemp);
		    fetchindexl(lab, arithtemp);
		    jmpindexb lab;
		    align(); define lab;
		    f (0, labs);
		    h(labs,l)
		end

	  | PRIMOP (i,vl,wl,el) => primops i (vl,wl,el)

    and markstore w =
	 (if !CGoptions.profile then profile(Profile.REFLISTS,2) else ();
	  storefield(0, storeptr);
	  storefield(~1, w);
	  move(dataptr, storeptr);
	  addtodataptr 2)

    and primops p =
        case p of
          P.+ => (fn ([v,w],[x],[e]) =>
	  let val v' = regbind v and w' = regbind w
	  in case (isimmed v', isimmed w') of
	       (SOME k, _) => alloc(x,e,w', fn x' => addl3(immed(k-1),w',x'))
(* the next case must be done by any machine with v and x in
   root registers (for offset computations in "boot") *)
	     | (_, SOME k) => alloc(x,e,v', fn x' => addl3(immed(k-1),v',x'))
	     | _ => alloc(x,e,w', fn x' => (subl3(immed 1,v',arithtemp);
					    addl3(arithtemp, w', x')));
	     gen e
	  end)
       | P.- => (fn ([v,w],[x],[e]) =>
	 let val v' = regbind v and w' = regbind w
	 in case (isimmed v', isimmed w') of
	      (SOME k, _) => alloc(x,e,w', fn x' => subl3(w',immed(k+1),x'))
	    | (_, SOME k) => alloc(x,e,v', fn x' => subl3(immed(k-1),v',x'))
	    | _ => alloc(x, e,v', fn x' => (subl3(immed 1,w',arithtemp);
			                    subl3(arithtemp, v', x')));
	    gen e
	 end)
       | P.* => (fn ([v,w],[x],[e]) =>
	  alloc(x, e,any, fn x' =>
			(ashl(immed(~1), regbind v, arithtemp);
			 subl3(immed 1, regbind w, arithtemp2);
			 mull2(arithtemp2,arithtemp);
			 addl3(immed 1,arithtemp,x');
			 gen e)))
       | P.div => (fn ([v,w],[x],[e]) =>
	  alloc(x, e,any, fn x' =>
			(ashl(immed(~1), regbind v, arithtemp);
			 ashl(immed(~1), regbind w, arithtemp2);
			 divl2(arithtemp2,arithtemp);
			 addl3(arithtemp,arithtemp,arithtemp);
			 addl3(immed 1,arithtemp,x');
			 gen e)))
       | P.! => (fn ([v],[w],[e]) => gen(SELECT(0,v,w,e)))
       | P.:= => (fn ([v,w],[],[e]) =>
               (store(regbind w, regbind v);
		markstore(regbind v);
		gen e))
       | P.unboxedassign => (fn ([v,w],[],[e]) =>
             (store(regbind w, regbind v);
	      gen e))
       | P.~ => (fn ([v],[w],[e]) =>
	    alloc(w, e,any, fn w' =>
		    (subl3(regbind v, immed 2, w');
		     gen e)))
       | P.makeref =>
	 (fn ([v],[w],[e]) =>
	    alloc(w, e,any, fn w' =>
		(if !CGoptions.profile then profile(Profile.REFCELLS,2) else ();
		 storefield(0, regbind v);
		 storefield(~1, immed(power_tags+tag_array));
		 move(dataptr, w');
		 addtodataptr 2;
		 gen e)))
       | P.ieql => (fn ([v,w],[],[d,e]) =>
		let val lab = newlabel()
		 in ibranch(NEQ,regbind v, regbind w, lab);
		    gen d;
		    genlab(lab,e)
		end )
       | P.ineq => (fn ([v,w],[],[d,e]) =>
		let val lab = newlabel()
		 in ibranch(EQL,regbind v, regbind w, lab);
		    gen d;
		    genlab(lab,e)
		end )
       | P.> => (fn ([v,w],[],[d,e]) =>
		let val lab = newlabel()
		 in ibranch(LEQ,regbind v, regbind w, lab);
		    gen d;
		    genlab(lab,e)
		end )
       | P.>= => (fn ([v,w],[],[d,e]) =>
		let val lab = newlabel()
		 in ibranch(LSS,regbind v, regbind w, lab);
		    gen d;
		    genlab(lab,e)
		end )
       | P.< => (fn ([v,w],[],[d,e]) =>
		let val lab = newlabel()
		 in ibranch(GEQ,regbind v, regbind w, lab);
		    gen d;
		    genlab(lab,e)
		end )
       | P.<= => (fn ([v,w],[],[d,e]) =>
		let val lab = newlabel()
		 in ibranch(GTR,regbind v, regbind w, lab);
		    gen d;
		    genlab(lab,e)
		end )


       | P.subscript => (fn ([v,w],[x],[e]) =>
			alloc(x, e,any, fn x' =>
			    (ashl(immed(~1),regbind w,arithtemp);
			     fetchindexl(regbind v, x');
			     gen e)))
       | P.update => (fn ([a, i, v], [], [e]) =>
	(addl3(immed ~1, regbind i, arithtemp);
	 addl3(arithtemp,arithtemp,arithtemp);
	 addl3(regbind a, arithtemp, arithtemp);
	      (* DANGER: pointer!
		    worry about this when you have concurrent g.c. *)
	 store(regbind v, arithtemp);
         markstore(arithtemp);
	 gen e))
       | P.unboxedupdate => (fn ([a, i, v], [], [e]) =>
	(ashl(immed(~1), regbind i, arithtemp);
	 storeindexl(regbind v, regbind a);
	 gen e))
       | P.alength => (fn ([a], [w], [e]) =>
	alloc(w,  e,any,  fn w' =>
	    (select(~1, regbind a, arithtemp);
	     ashl(immed(~(width_tags-1)),arithtemp, arithtemp);
(*	     bisl3(immed 1, arithtemp, w');
	this didn't work on the mc68020, dammit! *)
	     bisl3(immed 1, arithtemp, arithtemp);
	     move(arithtemp,w');
	     gen e)))
       | P.slength => (fn ([a], [w], [e]) =>
	  alloc(w, e,any, fn w' =>
	    let val a' = regbind a
	     in if isreg' a'
		   then select(~1,a',arithtemp)
		   else (move(a',w'); select(~1,w',arithtemp));
	        ashl(immed(~(width_tags-1)), arithtemp, arithtemp);
(*	     bisl3(immed 1, arithtemp, w');
	this didn't work on the mc68020, dammit! *)
	     bisl3(immed 1, arithtemp, arithtemp);
	     move(arithtemp,w');
	        gen e
	    end))
       | P.store => (fn ([s,i,v], [], [e])  =>
	       (ashl(immed(~1), regbind i, arithtemp);
		ashl(immed(~1), regbind v, arithtemp2);
		storeindexb(arithtemp2, regbind s);
	        gen e))
       | P.ordof => (fn ([s,i], [v], [e]) =>
	    alloc(v, e,any, fn v' =>
	    let val s' = regbind s
	     in ashl(immed ~1, regbind i, arithtemp);
	        if isreg' s' then fetchindexb(s', arithtemp2)
		 else (move(s',v'); fetchindexb(v',arithtemp2));
	        addl3(arithtemp2,arithtemp2,arithtemp2);
	        addl3(immed 1, arithtemp2, v');
	        gen e
	    end))
       | P.cast => (fn ([a], [b], [e]) =>
	    let val a' = regbind a
	     in alloc(b,e,a', fn b' => (move(a',b'); gen e))
	    end)
       | P.fneg => (fn ([x], [y], [e]) =>
	    alloc(y, e,any, fn y' =>
	      (mnegg(regbind x, dataptr);
	       storefield(~1, immed(8*power_tags + tag_string));
	       move(dataptr, y');
	       addtodataptr 3;
	       gen e)))
       | P.profile => (fn ([index,incr],[],[c]) =>
			(case (isimmed (regbind index), isimmed (regbind incr))
			  of (SOME i, SOME v) => profile(i div 2,(v div 2)*2);
			 gen c))
       | P.boxed => (fn ([x],[],[a,b]) =>
		    let val lab = newlabel()
		     in bbs(immed 0, regbind x, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       | P.sceql => (fn ([x,y,len],[],[a,b]) =>
		    let val lab = newlabel()
		     in sceql(regbind x, regbind y, regbind len, lab);	    
		        gen a;
		        genlab(lab,b)
		    end)
       | P.gethdlr => (fn ([],[x],[e]) =>
		  alloc(x, e,any, fn x' =>
		  (move(exnptr,x');
		   gen e)))
       | P.sethdlr => (fn ([x],[],[e]) =>
		  (move(regbind x, exnptr); gen e))
       | P.fmul =>  (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (mulg3(regbind x, regbind y, dataptr);
	          storefield(~1, immed(8*power_tags + tag_string));
		  move(dataptr, z');
		  addtodataptr 3;
		  gen e)))
       | P.fdiv =>  (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (divg3(regbind x, regbind y, dataptr);
	          storefield(~1, immed(8*power_tags + tag_string));
		  move(dataptr, z');
		  addtodataptr 3;
		  gen e)))
       | P.fadd => (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (addg3(regbind x, regbind y, dataptr);
	          storefield(~1, immed(8*power_tags + tag_string));
		  move(dataptr, z');
		  addtodataptr 3;
		  gen e)))
       | P.fsub => (fn ([x,y], [z], [e]) =>
		alloc(z,  e,any, fn z' =>
		 (subg3(regbind x, regbind y, dataptr);
	          storefield(~1, immed(8*power_tags + tag_string));
		  move(dataptr, z');
		  addtodataptr 3;
		  gen e)))
       | P.feql => (fn ([x,y],[],[a,b]) =>
		    let val lab = newlabel()
		     in gbranch(NEQ,regbind x, regbind y, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       | P.fneq => (fn ([x,y],[],[a,b]) =>
		    let val lab = newlabel()
		     in gbranch(EQL,regbind x, regbind y, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       | P.fgt => (fn ([x,y],[],[a,b]) =>
		    let val lab = newlabel()
		     in gbranch(LEQ,regbind x, regbind y, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       | P.flt => (fn ([x,y],[],[a,b]) =>
		    let val lab = newlabel()
		     in gbranch(GEQ,regbind x, regbind y, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       | P.fge => (fn ([x,y],[],[a,b]) =>
		    let val lab = newlabel()
		     in gbranch(LSS,regbind x, regbind y, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       | P.fle => (fn ([x,y],[],[a,b]) =>
		    let val lab = newlabel()
		     in gbranch(GTR,regbind x, regbind y, lab);
		        gen a;
		        genlab(lab,b)
		    end)
       

in  emitlong 1; (* Bogus tag for spacing, boot_v. *)
    let fun loop nil = ()
          | loop (frag::r) = (frags := r; genfrag frag; loop(!frags))
    in loop(!frags)
    end
end

end
