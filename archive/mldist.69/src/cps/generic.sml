(* Copyright 1989 by AT&T Bell Laboratories *)
functor CPSgen(M: CMACHINE) :
  sig structure CPS : CPS
    val codegen : CPS.function list * ErrorMsg.complainer -> unit
  end =
struct
val die = ErrorMsg.impossible

structure CPS = CPS
open CPS M System.Tags Access
val op sub = Array.sub
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

datatype RegType = FPReg | GPReg

(* FPR(fp,gp) => the variable is in the floating register fp _only_,
 *		 with an allocated general register gp
 * DPR(fp,gp) => the variable is in _both_ a floating register fp,
 *		 and the general register gp.
 * GPR gp     => the variable is n a general register gp only.
 *)          
datatype Reg = GPR of int		(* general purpose reg *)
	     | FPR of (int * int) 	(* floating reg * shadow gp reg *)
             | DPR of (int * int)	(* dual regs (fpr,gpr) *)
datatype frag
  = STANDARD of (lvar * lvar list * cexp) option ref
  | KNOWN of (lvar list * cexp) * Reg list option ref
  | STRINGfrag of string
  | REALfrag of string
fun regtype2string rty = case rty of FPReg => "FPReg " | GPReg => "GPReg "
fun reg2string reg = case reg 
    of FPR(fp,gp) => "FPR(" ^ makestring fp ^ "," ^ makestring gp ^ ")"
     | DPR(fp,gp) => "DPR(" ^ makestring fp ^ "," ^ makestring gp ^ ")"
     | GPR gp => "GPR(" ^ makestring gp ^ ")"
exception GpregNum and FpregNum and ShadowNum
fun gpregNum reg = case reg 
		     of GPR gp => gp
		      | DPR(_,gp) => gp
		      | FPR _ => raise GpregNum
fun fpregNum reg = case reg 
		     of FPR(fp,_) => fp
		      | DPR(fp,_) => fp
		      | GPR _ => raise FpregNum
fun shadowNum reg = case reg
		      of FPR(fp,gp) => gp
		       | DPR(fp,gp) => gp
		       | GPR _ => raise ShadowNum
fun standardformals [_,_]   = [GPR 2,GPR 1]
  | standardformals [_,_,_] = [GPR 0,GPR 1,GPR 2]
  | standardformals _ = ErrorMsg.impossible "110 in CPSgen"

val allregs = standardclosure::standardarg::standardcont::miscregs
val allfpregs = M.savedfpregs @ M.floatregs
local 
    exception FPRegEA and GPRegEA
    val gpregarr = arrayoflist allregs
    val fpregarr = arrayoflist allfpregs
in 
fun fpregEA reg = (fpregarr sub (fpregNum reg)) handle _ => raise FPRegEA
fun gpregEA reg = (gpregarr sub (gpregNum reg)) handle _ => raise GPRegEA
end
val max_fp_parameters = let val len = length M.savedfpregs
			in case M.floatregs 
			     of [] => if len=0 then 0 else len-1
			      | _ => len
			end
fun collect(_,[]) = []
  | collect(pred,x::xs) = 
    if pred x then x :: collect(pred,xs) else collect(pred,xs)

structure GetScratch :
    sig
	exception GetFpScratch
	exception GetGpScratch
	val getfpscratch: int * Reg list -> int
	val getgpscratch: int * Reg list -> int
	val arithtemp : EA
	val fpregtemp : EA
    end =
struct
val ok_gpreg = array(length allregs, true)
val ok_fpreg = array(length allfpregs, true)
val last_gp = ref 0
val last_fp = ref 0
val len_gp = Array.length ok_gpreg
val len_fp = Array.length ok_fpreg
fun mark b reg = 
    (*
     * ~1 may be passed as a don't care register number,
     * hence the handle Subscript ..
     *)
    let fun mboth(fp,gp) =
	(update(ok_fpreg,fp,b) handle Subscript => ();
	 update(ok_gpreg,gp,b) handle Subscript => ())
    in case reg
	 of GPR i => (update(ok_gpreg,i,b) handle Subscript => ())
	  | FPR(fp,gp) => mboth(fp,gp)
	  | DPR(fp,gp) => mboth(fp,gp)
    end
fun mark_prohibited proh = map (mark false) proh
fun cleanup regs = map (mark true) regs
exception FindReg
fun find_reg(okregs, next) =
    let fun find i = if okregs sub i then i else find (i+1)
	fun find2 i = if okregs sub i then i
		      else if i=next then raise FindReg else find2(i+1)
    in  find next handle Subscript => find2 0
    end

exception GetScratch
fun getregscratch(pref, proh, okregs, last, len) = 
    (mark_prohibited proh;
     (if ((okregs sub pref) handle Subscript => false) then pref
      else (find_reg(okregs, !last) 
	        handle FindReg => (cleanup proh; raise GetScratch)))
	  before
	  (cleanup proh;
	   last := (if !last+1=len then 0 else !last+1)))

exception GetFpScratch
fun getfpscratch(pref,proh) = 
    (getregscratch(pref, proh, ok_fpreg, last_fp, len_fp)
     	handle GetScratch => raise GetFpScratch)

exception GetGpScratch
fun getgpscratch(pref,proh) = 
    (getregscratch(pref, proh, ok_gpreg, last_gp, len_gp) 
       	handle GetScratch => raise GetGpScratch)

val arithtemp = case arithtemps 
		  of z::_ => z 
		   | _ => let val r = GPR(length(miscregs)+3-1)
                           in mark false r; gpregEA r
                          end
val fpregtemp = 
    (* see also max_fp_parameters *)
    case allfpregs 
      of [] => ErrorMsg.impossible "cps/generic: no floating point registers"
       | _ => let val tmp_reg = length allfpregs - 1
	       in  mark false (FPR (tmp_reg,~1)); 
		   fpregEA (FPR(tmp_reg, ~1))
	       end
end
open GetScratch

fun codegen(funs : (lvar * lvar list * cexp) list, err) =
let 
    val escapes = findescapes funs

    exception Labbind
    val labtable : EA Intmap.intmap = Intmap.new(32, Labbind)
    val addlabbinding = Intmap.add labtable
    val labmap = Intmap.map labtable

    exception Know
    val knowtable : frag Intmap.intmap = Intmap.new(32, Know)
    val addknow = Intmap.add knowtable
    val know = Intmap.map knowtable

    exception Freemap
    val freemaptable : lvar list Intmap.intmap = Intmap.new(32, Freemap)
    val freemap = Intmap.map freemaptable
    val cexp_freevars = FreeMap.cexp_freevars freemap
 
    exception Regbind
    val regbindtable : Reg Intmap.intmap = Intmap.new(32, Regbind)
    val addregbinding = Intmap.add regbindtable
    val regmap = Intmap.map regbindtable

    fun fpregbind_exists var = case regmap var of GPR _ => false | _ => true
    fun gpregbind_exists var = case regmap var of FPR _ => false | _ => true
	
    exception GpregMap and FpregMap
    fun gpregmap var = 
	if gpregbind_exists var then regmap var else raise GpregMap
    fun fpregmap var = 
	if fpregbind_exists var then regmap var else raise FpregMap
	
    fun clean (VAR x::r) = x :: clean r
      | clean (_::r) = clean r
      | clean [] = []
    fun live_regs(args:lvar list) = map regmap args
    local 
	val knowngen = System.Control.CG.knowngen
	val stdgen = System.Control.CG.stdgen
    in 
	fun makefrag (f,vl,e) =
	    let val lab = newlabel()
		val knowledge = 
		    if not(escapes f) 
			then (inc knowngen; 
			      KNOWN((vl,e), ref NONE))
		    else (inc stdgen; 
			  STANDARD(ref(SOME(f,vl,e))))
	    in 
		addknow(f, knowledge); 
		addlabbinding(f,lab);
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

    fun regbind(VAR v,regtype) = 
	(*
	 * Returns a register binding for a cps value
	 * A write back is generated when a integer register binding is
	 * required for a value in a floating register.
	 *)
	let val reg = regmap v
	in case (reg,regtype)
	     of (FPR(fp,gp),GPReg) => 
		 let val newreg = DPR(fp,gp)
		 in storefloat(fpregEA newreg, gpregEA newreg);
		     addregbinding(v, newreg);
		     gpregEA newreg
		 end
	      | (_,GPReg) => gpregEA reg
	      | (_,FPReg) => fpregEA reg
	end
      | regbind(value, GPReg) =
	(case value
	  of LABEL v => labmap v
	   | INT i => (immed(i+i+1) handle Overflow =>
		       ErrorMsg.impossible "Overflow in cps/generic.sml")
	   | STRING s => (lookString s handle Strings =>
			  let val lab = newlabel()
			  in addfrag(lab, STRINGfrag s);
			      enterString(s,lab);
			      lab
			  end)
	   | REAL s => let val lab = newlabel()
		       in  addfrag(lab, REALfrag s);
			   lab
		       end
	   | OBJECT _ => ErrorMsg.impossible "OBJECT in cps/generic/regbind")
      | regbind(_, FPReg) =
	ErrorMsg.impossible "value not loaded into floating register"

    val gpregbind : value -> EA = fn x => regbind(x,GPReg)
    val fpregbind : value -> EA = fn x => regbind(x,FPReg)

    exception RegMask
    fun regmask formals =
	let fun f (i,mask) =
	    case i
	      of GPR gp => Bits.orb(Bits.lshift(1,gp),mask)
	       | FPR _ => mask
	       | DPR _ => raise RegMask
	in emitlong(fold f formals 0)
	end
    fun root(RECORD(_,_,e)) = root e
      | root(SELECT(_,_,_,e)) = root e
      | root(OFFSET(_,_,_,e)) = root e
      | root(SWITCH(_,e::_)) = root e
      | root(PRIMOP(_,_,_,e::_)) = root e
      | root(e as APP _) = e
      | root _ = ErrorMsg.impossible "Unknown CPS object in cps/generic/root"
    val root1 = ref(APP(VAR 0, []))

    fun nextuse x =
	let fun xin[] = false 
	      | xin(VAR y::r) = x=y orelse xin r 
	      | xin(_::r) = xin r
	    fun g(level,a) =
		let val rec f =
		    fn ([],[]) => level
		     | ([],next) => g(level+1,next)
		     | (SWITCH(v,l)::r,next) => 
			   if xin[v] then level else f(r,l@next)
		     | (RECORD(l,w,c)::r,next) =>
			   if xin(map #1 l) then level 
			   else f(r,c::next)
		     | (SELECT(i,v,w,c)::r,next) => 
			   if xin[v] then level else f(r,c::next)
		     | (OFFSET(i,v,w,c)::r,next) => 
			   if xin[v] then level else f(r,c::next)
		     | (PRIMOP(i,a,w,cl)::r,next) => 
			   if xin a then level else f(r,cl@next)
		     | (APP(v,vl)::r,next) => 
			   if xin(v::vl) then level 
			   else f(r,next)
		in f(a,[])
		end
	    fun h y = g(0,[y])
	in h
	end

    fun next_fp_use(x,cexp) : int option = 
	let  val fp_ops = [P.fadd, P.fdiv, P.feql, P.fge, P.fgt, 
			   P.fle,  P.flt, P.fmul, P.fneq, P.fsub]
	    fun is_fp_op primop = exists (fn z => primop = z) fp_ops
	    fun f (cexp,level) =
	        case cexp
		  of RECORD(_,_,ce) => f(ce,level+1)
		   | SELECT(_,_,_,ce) => f(ce,level+1)
		   | OFFSET(_,_,_,ce) => f(ce,level+1)
		   | APP _ => NONE
		   | FIX _ => ErrorMsg.impossible "FIX in generic.sml"
		   | SWITCH(_,cl) => fpop_in_all_branches(cl,level)
		   | PRIMOP(f,vl,_,cl) =>
			 if (is_fp_op f) andalso
			     (exists (fn z => z= VAR(x)) vl) then  SOME level
			 else  fpop_in_all_branches(cl,level)
	    and
		fpop_in_all_branches (branches,level) =
		let val all_branches =  map (fn c => f(c,level)) branches
		in if exists (fn opt => opt = NONE) all_branches  
		       then NONE
		   else let val lvls = map (fn SOME l => l) all_branches
			in SOME (fold min lvls (hd lvls))
			end
		end
	in f(cexp,0)
	end

    fun preferred_register_asgn(formals,cexp) =
	if max_fp_parameters=0 then map (fn _ => GPReg) formals
	else 
          let val preferred_regs =  
	          map (fn SOME x => (FPReg,x) | NONE => (GPReg, 0))
		      (map (fn v => next_fp_use(v,cexp)) formals)
	      fun assign([],_) = []
		| assign(xs,0) = map (fn _ => GPReg) xs
		| assign((GPReg,_)::xs,acc) = GPReg::assign(xs,acc)
		| assign((FPReg,lvl)::xs,acc) =
		  let fun better_params([],c) = c
			| better_params((FPReg,lvl')::xs, c) =
			  if lvl' < lvl then better_params(xs,c+1)
			  else better_params(xs,c)
			| better_params(_::xs,c) = better_params(xs,c)
		  in if better_params(xs,0) >= acc then GPReg::assign(xs,acc)
		     else FPReg::assign(xs,acc-1)
		  end
	  in assign(preferred_regs, max_fp_parameters)
	  end

    val any = INT 0 (* default argument for alloc *)

    fun alloc(v,default,continue) =
	(*
	 * allocate a general purpose register for the new
	 * free variable v, and continue.
	 *)
	let val (f,formals,wl) = 
	    case !root1
	      of APP(VAR f, wl) => (f, standardformals wl, wl)
	       | APP(LABEL f, wl) => 
		     (f,(case know f
			   of KNOWN(_,ref(SOME fmls)) => fmls
			    | KNOWN(_,ref NONE) => nil
			    | STANDARD _ => standardformals wl),
		      wl)
	    val proh = live_regs (freemap v)
	    fun delete (z,nil) = nil
	      | delete (z:Reg, a::r) = if a=z then r else a::delete(z,r)
	    val default = case default
			    of VAR i => ((gpregmap i) handle GpregMap => GPR ~1)
			     | _ => GPR ~1
	    fun get(good,bad) =
		let val r = getgpscratch(gpregNum good,bad@proh) 
		            handle GetGpScratch => 
				          getgpscratch(gpregNum default,proh)
				 | GpregNum => 
				          getgpscratch(gpregNum default, proh)
		in addregbinding(v,GPR r); continue(gpregEA (GPR r))
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

    fun partition_args(args:value list, formals:Reg list) =
	(*
	 * Moves registers to the right register class.
	 * This makes it easier to shuffle later.
	 * If an actual argument is required in both a floating and
	 * general register then it will end up in a DPR register.
	 *
	 * The process is split into 3 phases.
	 * 1. move_GPR_args moves arguments into GPRegs that do not have
	 *   a GPReg binding.
	 * 2. flush_fpregs removes all unnecessary bindings in 
	 *   floating registers.
	 * 3. move_FPR_args moves arguments into FPRegs.
	 *)
	let fun move_GPR_args(VAR var::vs, GPR gp::fmls) = 
	        if gpregbind_exists var then  move_GPR_args(vs,fmls)
		else let val FPR(fp,gp) = regmap var
			 val newreg = DPR(fp,gp)
		     in (*
			 * Use shadow register to store floating value.
			 *)
			 storefloat(fpregEA newreg, gpregEA newreg);
			 addregbinding(var,newreg);
			 move_GPR_args(vs,fmls)
		     end
	      | move_GPR_args (_::a,_::f) = move_GPR_args(a,f)
	      | move_GPR_args ([],[]) = ()
	      | move_GPR_args _ =
		ErrorMsg.impossible "cps/generic/partition_args/move_GPR_args"
	    fun flush_fpregs () =
	        let open SortedList
		    fun GPRonly_args() =
		      let val pairs = List2.map2 (fn x => x) (args,formals)
			  val inFPRegs = 
			      collect(fn (VAR _,FPR _) => true | _ =>false,pairs)
			  val inGPRegs = 
			      collect(fn (VAR _,GPR _)=> true | _ =>false,pairs)
			  val h = fn (VAR x,_) => x
		      in difference (uniq(map h inGPRegs),uniq(map h inFPRegs))
		      end
		    fun f (d::ds) = 
			let val reg = regmap d 
			in case reg 
			     of DPR(fp,gp) => 
				 (* release floating point register *)
				 (addregbinding(d, GPR gp); f ds)
			      | GPR _ => f ds
			      | FPR _ => ErrorMsg.impossible
				    "cps/generic/partition_args/flush_fpregs"
			end
		      | f [] = ()
		in f (GPRonly_args())
		end
	    val formal_fp_regs = 
		let fun f (r::regs) = 
		    (case r 
		       of FPR(fp,_) => fp::f regs 
			| DPR(fp,_) => ErrorMsg.impossible
		                      "cps/generic/partition_args/formal_fp_regs"
			| _ => f regs)
		      | f [] = []
		in f formals
		end
	    fun move_FPR_args(VAR v::vs, (FPR(fp,_)::fmls)) =
		let fun getfpreg pref =
		        (* 
			 * The preferred floating register is the corresponding
			 * formal floating register, so this is deleted from
			 * the formals in a first attempt at getting a floating
			 * register.
			 *)
		        let fun delete (_,[]) = []
			      | delete (r,r'::rest) = if r=r' then rest 
						      else r'::delete(r,rest)
			    val liveregs = live_regs (clean args)
			    val avoid = map (fn r => FPR(r,~1)) 
				            (delete(pref,formal_fp_regs))
			in getfpscratch(pref, liveregs@avoid)
			       handle GetFpScratch => 
				         getfpscratch(pref,liveregs)
			end
		in if fpregbind_exists v then move_FPR_args(vs,fmls)
		   else let val z = getfpreg fp
			    val r = gpregNum (regmap v)
			    val newreg = DPR(z,r)
			in loadfloat(gpregEA newreg, fpregEA newreg);
			    addregbinding(v, newreg);
			    move_FPR_args(vs,fmls)
			end
		end
	      | move_FPR_args(_::a,_::f) = move_FPR_args(a,f)
	      | move_FPR_args([],[]) = ()
	      | move_FPR_args _ = 
		ErrorMsg.impossible "cps/generic/partition_args/move_FPR_args"
	in  move_GPR_args(args, formals);
	    if exists (fn FPR _ => true | _ => false) formals then
		(flush_fpregs (); move_FPR_args(args, formals))
	    else ()
	end

    fun shuffle_regtype(args:value list,formals:Reg list,regtype:RegType) =
	(*
	 *  Move actual arguments into registers for function call.
	 * Assumes that all the variable actuals have a binding in 
	 * the correct register class.
	 * If an actual is being passed in a floating and general 
	 * register, then its binding must be a DPR register.
	 * The function shuffles register of a specific type
	 * i.e. FPReg, GPReg.
	 *)
	let val (tempreg,EAfcn,regnum) =
		case regtype of GPReg => (arithtemp,gpregEA,gpregNum)
                              | FPReg => (fpregtemp,fpregEA,fpregNum)
	    fun classify(VAR v::al, f::fl, match, nomatch, notinreg) =
		let val v' = regmap v
		in if regnum v' = regnum f
		       then classify(al,fl,regnum f::match,nomatch,notinreg)
		   else classify(al,fl,match,(v',f)::nomatch,notinreg)
		end
	      | classify(a::al,f::fl, m,n,notinreg) = 
 		classify(al,fl,m,n,(a,f)::notinreg)
	      | classify(_,_, m,n,nr) = (m,n,nr)
	    fun f (pairs,used) = 
	      let val u' = (map (regnum o #1) pairs) @ used
		  fun movable (a, b) = not (exists (fn z => z = regnum b) u')
		  fun split pred nil = (nil,nil)
		    | split pred (a::r) =
		      let val (x,y) = split pred r
		      in if pred a then (a::x, y) else (x, a::y)
		      end
	      in case split movable pairs
		   of (nil,_) => (pairs,used)
		    | (m,m') => (app (fn(a,b)=>move(EAfcn a, EAfcn b)) m;
				 f(m', (map (regnum o #2) m) @ used))
	      end
            fun cycle(pairs, used) =
		case f(pairs,used)
		  of (nil,_) => ()
		   | ((a,b)::r, used) =>
			 cycle(move(EAfcn a, tempreg);
			       f(r,used) before move(tempreg, EAfcn b))
	    val (matched,notmatched,notinreg) = classify(args,formals,[],[],[])
	in
          cycle(notmatched,matched);
	  app (fn (a,b) => 
	          case regtype
		    of GPReg => move(gpregbind a, EAfcn b)
		     | FPReg => loadfloat(gpregbind a, EAfcn b))
	      notinreg
	end

    fun do_shuffle(args:value list,formals:Reg list) =
	(*
	 * - Partitions the actual arguments into sets
	 * based on their destination class.
	 * i.e. All agruments headed for general registers are
	 * in one partition, and all arguments headed for floating
	 * registers in another.
	 *)
	let fun register_sets(v::vs,f::fs,gv,gf,fv,ff) =
		(case f 
		    of GPR _ => register_sets(vs,fs,v::gv,f::gf,fv,ff)
		     | FPR _ => register_sets(vs,fs,gv,gf,v::fv,f::ff)
		     | DPR _ => ErrorMsg.impossible "cps/generic/do_shuffle")
	      | register_sets([],[],gv,gf,fv,ff) = ((gv,gf,GPReg),(fv,ff,FPReg))
	      | register_sets _ = ErrorMsg.impossible "register_sets/do_shuffle"
	
	    val _ = partition_args(args,formals)
	    val (gp_set,fp_set) = register_sets(args,formals,[],[],[],[])
	in shuffle_regtype gp_set;
	   shuffle_regtype fp_set
	end

    fun shuffle(func as VAR f, args: value list, formals:Reg list) =
	let val pref_fcn_reg = gpregNum (regmap f)
	    val reg = GPR(getgpscratch(pref_fcn_reg,
				       live_regs(clean args) @ formals)
	                  handle GetGpScratch => 
			                getgpscratch(pref_fcn_reg, formals))
	in  do_shuffle(func::args,reg::formals); 
	    jmp(gpregEA reg)
	end
      | shuffle(func as LABEL f, args,formals) = 
	(do_shuffle(args,formals); jmp(labmap f))

    fun allocparams(args:value list, formals:lvar list, prefer:RegType list) =
       (* 
	* Determines the parameter passing convention for a function.
        * This is complicated by the fact that an actual may not be in the
	* appropriate register class.
	* Even if an actual is in the correct register class it may not
	* be in a suitable register, since only a specific set of registers
	* can be used for parameter passing.
	* Precondition: 
	* 	|formals| <= maxfree && |live_regs(clean args)| <= maxfree
	* Invariant pass1:
	* 	|live_regs(clean(args)| + used_gp <= maxfree
	*) 
	let open SortedList
	    datatype PRegs = 
		OK of Reg 	(* allocated general registers *)
	      | NONE of int	(* allocated shadow register for a float *)
	    val liveregs = live_regs (clean args)
	    fun getgpr avoid = getgpscratch(~1, liveregs @ (map GPR avoid)) 
		                handle GetGpScratch => (print "allocparams\n";
							raise GetGpScratch)
	    fun okFPR_param fpr = fpr < max_fp_parameters
	    fun inuse (reg,already) = exists (fn r => r=reg) already

	    (* pass1 is guided by the preferred register class.
	     * If an actual is in the right register class and has not been 
	     * assigned then it is marked as being assigned.
	     * Otherwise an allocation is made.
	     * The shadow register is used where appropriate. 
	     *)
	    fun pass1 (VAR v::vl, p::pref, used_gp, used_fp) =
		(case p
		   of GPReg =>
		       let fun test_gp_reg z =
			       if inuse(z,used_gp) then getgpr used_gp
			       else z
			   fun pass1_with_gpreg z =
			       let val w = test_gp_reg z
			       in OK(GPR w)::pass1(vl,pref,w::used_gp,used_fp)
			       end
			   val reg = regmap v
		       in if gpregbind_exists v 
			  then pass1_with_gpreg(gpregNum reg)
			  else pass1_with_gpreg(shadowNum reg)
		       end
		    | FPReg =>
		      let fun bad_fpreg gp =
			      let val r = if not(inuse(gp,used_gp)) then gp
					  else getgpr used_gp
			      in NONE r:: pass1(vl,pref,r::used_gp,used_fp)
			      end
			  val reg = regmap v
		      in if fpregbind_exists v then
			    let val z = fpregNum reg
				val r = shadowNum reg
			    in if okFPR_param z andalso 
				  not (inuse(z,used_fp)) andalso
				  not (inuse(r,used_gp))
			       then OK(FPR(z,r))::
				    pass1(vl,pref,r::used_gp,z::used_fp)
			       else bad_fpreg r
			    end
			 else bad_fpreg (gpregNum reg)
		      end)
	      | pass1 (_::vl, p::pref, used_gp, used_fp) =
		let val z = getgpr used_gp
		in (case p of GPReg => OK(GPR z) | FPReg => NONE z) ::
		    pass1(vl,pref,z::used_gp,used_fp)
		end
	      | pass1 ([],[],_,_) = []
	      | pass1 _ =
		ErrorMsg.impossible "cps/generic/allocparams/pass1"
	    fun assigned_FPregs assgm =
		map (fn OK(FPR(fp,_)) => fp)
		    (collect(fn OK(FPR(fp,_))=> true | _ => false,assgm))
	    fun pass2 asgm =
		let val savedFPRegs = 
		        let fun from (n,m) = 
			        if n >= m then [] else n::from(n+1,m)
			in from (0, max_fp_parameters)
			end
		    val unusedfpr = difference (uniq savedFPRegs,
						uniq (assigned_FPregs asgm))
		    fun pass2(NONE gp::pregs,fp::fpregs)=
			FPR(fp,gp)::pass2(pregs,fpregs)
		      | pass2(NONE _ ::pregs, []) =
			ErrorMsg.impossible "cps/generic/allocparams/pass2"
		      | pass2(OK reg::pregs, fpregs) = reg :: pass2(pregs,fpregs)
		      | pass2 ([],_) = []
		in pass2(asgm, unusedfpr)
		end
	    val assign1 = pass1(args,prefer,[],[])
	    val final = if exists (fn rty => rty = FPReg) prefer
		        then pass2 assign1
			else map (fn (OK r) => r) assign1
	in
	    
	    List2.map2 addregbinding (formals,final);
	    do_shuffle(args, final);
	    final
	end

    fun stupidargs(f,args,vl,pref) = 
	(*
	 * - assign integer and floating registers in sequence
	 * starting from 0.
	 *)
	let fun argregs(v::rest,p::pref,gpreg,fpreg) =
	        (case p 
		   of GPReg => 
		       (addregbinding(v,GPR gpreg);
			GPR gpreg::argregs(rest,pref,gpreg+1,fpreg))
		    | FPReg => 
		       let val newreg = FPR(fpreg,gpreg)
		       in addregbinding(v,newreg);
			   newreg::argregs(rest,pref,gpreg+1,fpreg+1)
		       end)
	      | argregs ([],_,_,_) = []
	      | argregs _ = ErrorMsg.impossible "cps/generic/stupidargs"
	    val formals = argregs(vl,pref,0,0)
	in  shuffle(f,args,formals); formals
	end

    fun force_fpgetscratch (pref:int, proh:Reg list, cexp) =
	(*
	 * - allocate a floating point registers spilling if necessary.
	 * The floating registers in proh cannot be spilled.
	 * All free variables in cexp must have a register binding.
	 *)
      let val free = cexp_freevars cexp
	  exception Spill
	  fun spill():lvar =
	      let fun find_fp_spill_reg [] = raise Spill
		    | find_fp_spill_reg ((_,v)::uv) = 
		      if fpregbind_exists v
                      then let val r = fpregmap v
			   in if exists (fn reg => fpregNum reg = fpregNum r) 
			                proh
			      then find_fp_spill_reg uv
			      else v
			   end
		      else find_fp_spill_reg uv
		  val sortdecreasing = 
		      Sort.sort (fn ((i:int,_),(j:int,_)) => i < j)
		  val uses = map (fn v =>(nextuse v cexp, v)) free
	      in find_fp_spill_reg (sortdecreasing uses)
	      end
	  fun duplicates(vl:lvar list) =
	      let val avoid = 
		      (map fpregNum proh) handle RegNum => ErrorMsg.impossible
			  "cps/generic/force_getfpscratch/duplicates"
		  fun bad_dup v = exists (fn r => v = r) avoid
		  fun f (x::xs) = 
		      let val r = regmap x
		      in case r 
			   of DPR (fp,gp) => 
			       if bad_dup fp then f xs else (x,fp,gp)::f xs  
			    | _ => f xs
		      end
		    | f [] = []
	      in f vl
	      end
	  fun pref_dup [] = NONE
	    | pref_dup ((a as (v,fp,gp))::ds) =
	      if fp = pref then SOME a else pref_dup ds

	  exception FirstNoneUse of lvar
	  fun find_good_dup dups = 
	      let val sort = 
		  Sort.sort (fn ((_,lvl1),(_,lvl2)) => lvl1 <= lvl2)
		  val f = (fn (v,fp,gp) => case next_fp_use(v,cexp) 
					     of NONE => raise FirstNoneUse v
					      | SOME lvl => (v,lvl))
	      in #1 (hd (sort (map f dups)))
	      end

	  fun nofpr_handle () =
	      let val dups = duplicates free
	      in 
		  case pref_dup dups
		    of SOME(v,fp,gp) => 
			(addregbinding(v,GPR gp); fp)
		     | NONE => 
			if null dups then
			    let val z = (spill() handle Spill => 
					 		   raise GetFpScratch)
				val r as FPR(fp,gp) = fpregmap z
				val newreg = GPR gp
			    in storefloat(fpregEA r, gpregEA newreg);
				addregbinding(z, newreg);
				fp
			    end 
			else
			    (*
			     * Find the dup that is not going to be used
			     * in a floating context or one that is
			     * going to be used the furthest away.
			     *)
			    let val v = (find_good_dup dups) 
						handle FirstNoneUse x => x
				val DPR(fp,gp) = regmap v
			    in addregbinding(v, GPR gp); fp
			    end
	      end
      in  getfpscratch (pref, proh @ live_regs free)
	  handle GetFpScratch => (nofpr_handle ())
      end

    exception MoveToFPRs
    fun move_to_FPRs(vl, cexp) =
	(*
	 * move variables in vl to floating registers.
	 *)
	let fun f (VAR x::r,moved) =
		if fpregbind_exists x then f(r, regmap x::moved)
		else let val fp = force_fpgetscratch(~1,moved,cexp)
			 val gp = gpregNum(regmap x)
			 val newreg = DPR(fp,gp)
		     in loadfloat(gpregEA newreg,fpregEA newreg);
			 addregbinding(x, newreg);
			 f(r, newreg::moved)
		     end
	      | f (a::r,moved) =
		(*
		 * There is never a register allocated for constants.
		 * So when moving constants into floating point registers
		 * we _must_ not allocate the shadow register.
		 *)
		let val fp = force_fpgetscratch(~1,moved,cexp)
		    val newreg = FPR(fp, ~1)
		in loadfloat(gpregbind a,fpregEA newreg);
		    f(r, newreg::moved)
		end
	      | f ([],moved) = rev moved
	in f(vl,[])
	end



    fun do_fp_primop (cexp as PRIMOP(_,args,[w],[e]), continue) = 
	(* 
	 * ensure that the required arguments are in floating 
	 * registers and allocates a FPR for the result.
	 *)
	let
	    val moved  = move_to_FPRs(args,cexp)
	    val [x,y] = moved
	    val u = getgpscratch(~1, live_regs(freemap w))
	    (* 
	     * A lie to guarantee precondition for force_fpgetscratch
	     * which we promptly confess when creating newreg
	     *)
	    val _ = addregbinding(w, GPR u) 
	    val z = let 
			(* clean_fpregs:
			 * This function is required because of the M68k 
			 * that does not support 3 operand floating point 
			 * instructions. See definition of float in m68.sml
			 *
			 * Clean_fpregs removes the shadow registers in the
			 * moved set.
			 * Saying that they are prohibited is not strictly 
			 * correct. 
			 *)
			fun clean_fpregs [] = []
			  | clean_fpregs (x::xs) = 
			    (case x 
			       of FPR(fp,_) => FPR(fp, ~1) :: clean_fpregs xs
				| DPR(fp,_) => DPR(fp, ~1) :: clean_fpregs xs
				| GPR _ => ErrorMsg.impossible 
				            "cps/generic/do_fp_primop")
		    in force_fpgetscratch(~1, clean_fpregs moved, e)
		    end
	    val newreg = FPR(z,u)
	in  addregbinding(w,newreg);
	    continue (fpregEA x, fpregEA y, fpregEA newreg)
	end

    (* Compute the maximum amount of allocation done by this function 
     *  (in bytes). 
     *)
    fun sumAlloc exp = let
	  fun sum (RECORD (fields, _, exp'), max) = 
	      sum (exp', max+(length fields)+1)
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
	    | sum (PRIMOP (P.real, _, _, [exp']), max) = sum (exp', max+3)
	    | sum (PRIMOP (P.subscriptf, _, _, [exp']), max) = sum (exp', max+3)
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


    fun tempreg(x,f) = case arithtemps of _::z::_ => f z | _ => f x

    fun genfrag (_, STANDARD(ref NONE)) = ()
      | genfrag (lab, STANDARD(r as ref (SOME(fname,fmls,e)))) =
	let val fmls' as closure::_ = standardformals fmls
	in r := NONE;
	    List2.app2 addregbinding (fmls, fmls');
	    align(); regmask fmls'; mark();
	    comment(Access.lvarName fname ^ ":\n");
            define lab;
            checkLimit (sumAlloc e);
            beginStdFn ();
            root1 := root e;
            gen e
	end
      | genfrag (_, KNOWN _) = ()
      | genfrag (lab, REALfrag r) = 
	(align(); 
	 mark(); 
	 emitlong(desc_embedded_real);
	 define lab; 
	 comment("# real constant " ^ r ^ "\n");
	 realconst r
	 handle M.BadReal r => err ErrorMsg.COMPLAIN 
	     ("real constant out of range: " ^ r))
      | genfrag (lab, STRINGfrag s) = 
	(align(); 
	 mark();
	 emitlong(make_desc(size s, tag_embedded_string));
	 define lab; 
	 emitstring s; 
	 align())

    (* generate a new code label *)
    and genlab(lab, cexp) = (root1 := root cexp; define lab; gen cexp)

    and parallel_gen (shared_vars, f1, f2) =
	let val bindings = map regmap shared_vars
	in f1(); List2.app2 addregbinding (shared_vars,bindings); f2()
	end

    and gen cexp =
	case cexp
	  of RECORD(vl,w,e) =>
	      alloc(w, any,  fn w' =>
		    (record((immed(make_desc(length vl, tag_record)),
			     OFFp 0) :: map (fn(x,p)=>(gpregbind x, p)) vl,
			    w');
		     gen e))
	  | SELECT(i,v,w,e) =>
	    alloc(w,any,  fn w' => (select(i,gpregbind v,w'); gen e))
	  | OFFSET(i,v,w,e) =>
	      alloc(w, v, fn w' => (offset(i,gpregbind v,w'); gen e))
	  | APP(func as VAR f, args) => 
			shuffle(func, args, standardformals args)
	  | APP(func as LABEL f, args) =>
	    (case know f
	      of KNOWN(_, ref(SOME formals)) =>
			   shuffle(func, args, formals)
	       | KNOWN((vl,cexp), r as (ref NONE)) =>
		     let val pref = if !CG.floatreg_params then
			 		preferred_register_asgn(vl,cexp)
				    else map (fn _ => GPReg) vl
			 val formals = if !CG.argrep then
			     		   allocparams(args,vl,pref)
				       else stupidargs(func,args,vl,pref)
		     in r := SOME formals;
			 jmp (labmap f); align(); regmask formals; mark();
			 comment(Access.lvarName f ^ ":\n");
                         define(labmap f);
                         checkLimit (sumAlloc cexp);
                         root1 := root cexp;
			 gen cexp
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
		    fun h(lab::labs, e::es) = 
			parallel_gen(cexp_freevars e, 
				     fn () => genlab(lab,e),fn () => h(labs, es))
		      | h(nil,nil) = ()
		 in fetchindexl(lab, arithtemp, gpregbind v);
(*		    add(lab,arithtemp,arithtemp);
		    jmp(arithtemp); *)
		    jmpindexb(lab,arithtemp);
(*		    align();   temporarily removed so 68020 will work. *)
		    define lab;
		    f (0, labs);
		    h(labs,l)
		end
        | PRIMOP(P.+, [INT k, w],[x],[e]) =>
	      alloc(x, w, fn x' =>
		    (addt(immed(k+k), gpregbind w, x');
		     gen e))
        | PRIMOP(P.+, [w, v as INT _],x,e) => gen(PRIMOP(P.+,[v,w],x,e))
        | PRIMOP(P.+, [v,w],[x],[e]) =>
	      alloc(x, w, fn x' =>
		    (M.sub(immed 1, gpregbind v, arithtemp);
		     addt(arithtemp, gpregbind w, x');
		     gen e))
        | PRIMOP(P.orb, [v,w],[x],[e]) =>
          alloc(x, w, fn x' => (orb(gpregbind v, gpregbind w, x'); gen e))
        | PRIMOP(P.andb, [v,w],[x],[e]) =>
          alloc(x, w, fn x' =>(andb(gpregbind v, gpregbind w, x'); gen e))
        | PRIMOP(P.xorb, [INT k, w],[x],[e]) =>
	      alloc(x, w, fn x' =>
		    (xorb(immed(k+k), gpregbind w, x');
		     gen e))
        | PRIMOP(P.xorb, [w,v as INT _],x,e) => gen(PRIMOP(P.xorb,[v,w],x,e))
        | PRIMOP(P.xorb, [v,w],[x],[e]) =>
	      alloc(x,any, fn x' => tempreg(x', fn x'' =>
		    (xorb(gpregbind v, gpregbind w, x'');
		     add(immed 1, x'', x');
		     gen e)))
       | PRIMOP(P.notb, [v],[x],[e]) =>
          alloc(x, any, fn x' =>
	            (M.sub(gpregbind v, immed 0, x');
		     gen e))
       | PRIMOP(P.lshift, [INT k, w],[x],[e]) =>
	     alloc(x,w, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, x'');
		    ashl(x'',immed(k+k),x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.lshift, [v, INT k],[x],[e]) =>
	     alloc(x,v, fn x' => tempreg(x', fn x'' =>
		   (add(immed ~1, gpregbind v, x'');
		    ashl(immed k, x'', x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.lshift, [v,w],[x],[e]) =>
	     alloc(x,w, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, arithtemp);
		    add(immed ~1, gpregbind v, x'');
		    ashl(arithtemp, x'', x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.rshift, [v, INT k],[x],[e]) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed k, gpregbind v, x'');
		    orb(immed 1, x'', x');
 		    gen e)))
       | PRIMOP(P.rshift, [v,w],[x],[e]) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, arithtemp);
		    ashr(arithtemp, gpregbind v, x'');
		    orb(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.-, [INT k,w],[x],[e]) =>
	     alloc(x, w, fn x' =>
		   (M.subt(gpregbind w, immed(k+k+2), x');
		    gen e))
       | PRIMOP(P.-, [v, INT k],[x],[e]) =>
	     alloc(x, v, fn x' =>
		   (M.subt(immed(k+k), gpregbind v, x');
		    gen e))
       | PRIMOP(P.-, [v,w],[x],[e]) =>
	     alloc(x, v, fn x' => tempreg(x', fn x'' =>
		   (M.sub(gpregbind w, gpregbind v, x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.*, [INT k, INT j],[x],[e]) =>
	    alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (move(immed k, x'');
	      mult(immed(j+j),x'');
	      add(immed 1, x'', x');
	      gen e)))
       | PRIMOP(P.*, [INT k, w],[x],[e]) =>
	     alloc(x,any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, x'');
		    mult(immed(k+k), x'');
		    add(immed 1, x'', x');
		    gen e)))
       | PRIMOP(P.*, [v,w as INT _],x,e) => gen(PRIMOP(P.*,[w,v],x,e))
       | PRIMOP(P.*, [v,w],[x],[e]) =>
           alloc(x,any,fn x' => tempreg(x', fn x'' =>
	         (ashr(immed 1, gpregbind v, arithtemp);
		  M.sub(immed 1, gpregbind w, x'');
		  mult(arithtemp,x'');
		  add(immed 1,x'',x');
		  gen e)))
       | PRIMOP(P.div, [INT k, INT j],[x],[e]) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (move(immed k, x'');
		    divt(immed j, x'');
		    addt(x'',x'',x'');
		    add(immed 1, x'',x');
		    gen e)))
       | PRIMOP(P.div, [INT k,w],[x],[e]) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind w, arithtemp);
		    move(immed k, x'');
		    divt(arithtemp,x'');
		    addt(x'',x'',x'');
		    add(immed 1, x'',x');
		    gen e)))
       | PRIMOP(P.div, [v, INT k],[x],[e]) =>
	     alloc(x, any, fn x' => tempreg(x', fn x'' =>
		   (ashr(immed 1, gpregbind v, x'');
		    divt(immed k, x'');
		    addt(x'',x'',x'');
		    add(immed 1, x'',x');
		    gen e)))
       | PRIMOP(P.div, [v,w],[x],[e]) =>
            alloc(x,any, fn x' => tempreg(x', fn x'' =>
		  (ashr(immed 1, gpregbind w, arithtemp);
		   ashr(immed 1, gpregbind v, x'');
		   divt(arithtemp,x'');
		   addt(x'',x'',x'');
		   add(immed 1, x'',x');
		   gen e)))
       | PRIMOP(P.!, [v],[w],[e]) => gen(SELECT(0,v,w,e))
       | PRIMOP(P.:=, [v,w],[],[e]) =>
	    let val v' = gpregbind v
	     in record([(immed(make_desc(3,tag_record)),OFFp 0), (v', OFFp 0),
		       (immed 1, OFFp 0), (storeptr, OFFp 0)], storeptr);
	        storeindexl(gpregbind w, v', immed 1);
	        gen e
	    end
       | PRIMOP(P.unboxedassign, [v,w],[],[e]) =>
              (storeindexl(gpregbind w, gpregbind v, immed 1); gen e)
       | PRIMOP(P.~, [v],[w],[e]) =>
	 alloc(w,any,fn w' => (M.subt(gpregbind v,immed 2,w'); gen e))
       | PRIMOP(P.makeref, [v],[w],[e]) =>
	    alloc(w,any, fn w' =>
		(record([(immed(make_desc(1,tag_array)),OFFp 0),
			 (gpregbind v, OFFp 0)], w');
		 gen e))
       | PRIMOP(P.delay, [i,v],[w],[e]) =>
	    alloc(w,any, fn w' =>
		(record([(gpregbind i, OFFp 0),(gpregbind v, OFFp 0)], w');
		 gen e))
       | PRIMOP(P.lessu, [v,w],[],[d,e]) =>
	   let val false_lab = newlabel()
	    in rangeChk(gpregbind v, gpregbind w, false_lab);
		parallel_gen(cexp_freevars d,
			     fn () => gen d, fn () => genlab(false_lab, e))
	   end
       | PRIMOP(P.gequ, [v,w],[],[e,d]) =>
	   let val false_lab = newlabel()
	    in rangeChk(gpregbind v, gpregbind w, false_lab);
		parallel_gen(cexp_freevars d,
			     fn () => gen d, fn () => genlab(false_lab, e))
	   end
       | PRIMOP(P.subscript, [v,w],[x],[e]) =>
			alloc(x,any, fn x' =>
			    (fetchindexl(gpregbind v, x', gpregbind w);
			     gen e))
       | PRIMOP(P.update, [a, i, v], [], [e]) =>
	    let val a' = gpregbind a and i' = gpregbind i
	     in record([(immed(make_desc(3,tag_record)),OFFp 0), (a',OFFp 0),
		        (i', OFFp 0), (storeptr, OFFp 0)], storeptr);
	        storeindexl(gpregbind v, a', i');
	        gen e
	    end
       | PRIMOP(P.unboxedupdate, [a, i, v], [], [e]) =>
		(storeindexl(gpregbind v, gpregbind a, gpregbind i);
		 gen e)
       | PRIMOP(P.gethdlr,[],[x],[
           e0 as PRIMOP(P.update, [VAR x',i,v], [], [e])]) =>
             if x=x' andalso not (SortedList.member (cexp_freevars e) x)
               then let val i' = gpregbind i
		     in record([(immed(make_desc(3,tag_record)),OFFp 0), 
				(exnptr,OFFp 0),
				(i', OFFp 0), (storeptr, OFFp 0)], storeptr);
			 storeindexl(gpregbind v, exnptr, i');
			 gen e
		    end
               else alloc(x,any, fn x' => (move(exnptr,x'); gen e0))
       | PRIMOP(P.gethdlr,[],[x],[
           e0 as PRIMOP(P.unboxedupdate, [VAR x',i,v], [], [e])]) =>
            if x=x' andalso not (SortedList.member (cexp_freevars e) x)
               then (storeindexl(gpregbind v, exnptr, gpregbind i);
		     gen e)
               else alloc(x,any, fn x' => (move(exnptr,x'); gen e0))
       | PRIMOP(P.gethdlr,[],[x],[
           e0 as PRIMOP(P.subscript, [VAR x',y], [w], [e])]) =>
            if x=x' andalso not (SortedList.member (cexp_freevars e) x)
               then alloc(w,any, fn w' =>
			    (fetchindexl(exnptr, w', gpregbind y);
			     gen e))
               else alloc(x,any, fn x' => (move(exnptr,x'); gen e0))
       | PRIMOP(P.alength, [a], [x], [e]) =>
	alloc(x,any,  fn x' => tempreg(x', fn x'' =>
	    (select(~1, gpregbind a, x'');
	     ashr(immed(width_tags-1),x'', x'');
	     orb(immed 1, x'', x');
	     gen e)))
       | PRIMOP(P.slength, [a as VAR _], [x], [e]) =>
	  alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (select(~1, gpregbind a, x'');
	      ashr(immed(width_tags-1), x'', x'');
	      orb(immed 1, x'', x');
	      gen e)))
       | PRIMOP(P.slength, [a], [x], [e]) =>
	  alloc(x,any, fn x' => tempreg(x', fn x'' =>
	     (move(gpregbind a, x');
	      select(~1,x',x'');
	      ashr(immed(width_tags-1), x'', x'');
	      orb(immed 1, x'', x');
	      gen e)))
       | PRIMOP(P.store, [s,INT i', INT v'], [], [e]) =>
	     (storeindexb(immed v', gpregbind s, immed i');
	      gen e)
       | PRIMOP(P.store, [s,INT i',v], [], [e]) =>
	     (ashr(immed 1, gpregbind v, arithtemp);
	      storeindexb(arithtemp, gpregbind s, immed i');
	      gen e)
       | PRIMOP(P.store, [s,i,INT v'], [], [e]) =>
	     (ashr(immed 1, gpregbind i, arithtemp);
	      storeindexb(immed v', gpregbind s, arithtemp);
	      gen e)
       | PRIMOP(P.store, [s,i,v], [], [e]) =>
	     let val v' = gpregbind v
	      in ashr(immed 1, gpregbind i, arithtemp);
		 ashr(immed 1, v', v');
	         storeindexb(v', gpregbind s, arithtemp);
		 add(v',v',v');
		 add(immed 1, v', v');
		 gen e
	     end
       | PRIMOP(P.ordof, [s as VAR _, INT k], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (fetchindexb(gpregbind s, v', immed k);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s, INT k], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (move(gpregbind s, v');
		    fetchindexb(v', v', immed k);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s as VAR _, i], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (ashr(immed 1, gpregbind i, arithtemp);
		    fetchindexb(gpregbind s, v', arithtemp);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | PRIMOP(P.ordof, [s, i], [v], [e]) =>
	     alloc(v,any, fn v' =>
		   (ashr(immed 1, gpregbind i, arithtemp);
		    move(gpregbind s, v');
		    fetchindexb(v', v', arithtemp);
		    add(v',v',v');
		    add(immed 1, v',v');
		    gen e))
       | PRIMOP(P.profile, [index,incr],[],[c]) =>
			(case (globalvar,gpregbind index,gpregbind incr)
			  of (SOME r,i,j) => 
				(fetchindexl(r,arithtemp,i);
				 add(arithtemp,j,arithtemp);
				 M.sub(immed 1,arithtemp,arithtemp);
			         storeindexl(arithtemp,r,i))
			  | _ => ();
			 gen c)
       | PRIMOP(P.boxed, [x],[],[a,b]) =>
		    let val lab = newlabel()
		     in bbs(immed 0,gpregbind x,lab); 
			 parallel_gen(cexp_freevars a,
				      fn () => gen a, fn () => genlab(lab,b))
		    end
       | PRIMOP(P.unboxed, [x],[],[a,b]) => gen(PRIMOP(P.boxed,[x],[],[b,a]))
       | PRIMOP(P.gethdlr, [],[x],[e]) =>
		  alloc(x,any, fn x' => (move(exnptr,x'); gen e))
       | PRIMOP(P.sethdlr, [x],[],[e]) => (move(gpregbind x, exnptr); gen e)
       | PRIMOP(P.fmul, [x,y], [z], [e]) =>
	     do_fp_primop(cexp, 
			  (fn (x',y',z') => (mulf(x',y',z'); gen e)))

(* still to implement: 
      floor | round | real | subscriptf | updatef *)
       | PRIMOP(P.fdiv, [x,y], [z], [e]) =>
	     do_fp_primop(cexp, 
			  (fn (x',y',z') => (divf(x',y',z'); gen e)))
       | PRIMOP(P.fadd, [x,y], [z], [e]) =>
	     do_fp_primop(cexp, 
			  (fn (x',y',z') => (addf(x',y',z'); gen e)))
       | PRIMOP(P.fsub, [x,y], [z], [e]) =>
	     do_fp_primop(cexp, 
			  (fn (x',y',z') => (subf(x',y',z'); 
				     gen e)))
       | PRIMOP(args as (P.ieql,_,_,_)) => compare(ibranch,NEQ,args)
       | PRIMOP(args as (P.ineq,_,_,_)) => compare(ibranch,EQL,args)
       | PRIMOP(args as (P.>   ,_,_,_)) => compare(ibranch,LEQ,args)
       | PRIMOP(args as (P.>=  ,_,_,_)) => compare(ibranch,LSS,args)
       | PRIMOP(args as (P.<   ,_,_,_)) => compare(ibranch,GEQ,args)
       | PRIMOP(args as (P.<=  ,_,_,_)) => compare(ibranch,GTR,args)
       | PRIMOP(args as (P.feql,_,_,_)) => fpcompare(gbranch,NEQ,cexp)
       | PRIMOP(args as (P.fneq,_,_,_)) => fpcompare(gbranch,EQL,cexp)
       | PRIMOP(args as (P.fgt ,_,_,_)) => fpcompare(gbranch,LEQ,cexp)
       | PRIMOP(args as (P.flt ,_,_,_)) => fpcompare(gbranch,GEQ,cexp)
       | PRIMOP(args as (P.fge ,_,_,_)) => fpcompare(gbranch,LSS,cexp)
       | PRIMOP(args as (P.fle ,_,_,_)) => fpcompare(gbranch,GTR,cexp)
       | _ => ErrorMsg.impossible "3312 in CPSgen"

    and compare(branch,test, (_,[v,w],[],[d,e])) =
	let val lab = newlabel()
	in branch(test,gpregbind v, gpregbind w, lab); 
	    parallel_gen(cexp_freevars d,fn () => gen d,fn () => genlab(lab, e))
	end

    and
	fpcompare(branch, test, cexp as PRIMOP(_,args as [v,w],[],[d,e])) =
	let val lab = newlabel()
	    val reserved = move_to_FPRs([v,w], cexp)
	    val [v',w'] = reserved 
	in  branch(test, fpregEA v', fpregEA w', lab);
	    parallel_gen(cexp_freevars d,fn () => gen d,fn () => genlab(lab,e))
	end

in  (* not necessary with regmasks: emitlong 1; Bogus tag for spacing, boot_v. *)
    let fun loop nil = ()
          | loop (frag::r) = (frags := r; genfrag frag; loop(!frags))
    in loop(!frags)
    end
end (* codegen *)

end (* structure *)
