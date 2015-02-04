(* VAX code generator for ML
   r15 : program counter
   r14 : stack pointer
   r13 : exception handler
   r12 : freespace pointer
   r11 : store pointer
   r10 : profile pointer
*)

functor VaxGen (Coder : VAXCODER) : MACHINE =
struct

   structure VaxPrim = VaxPrim (Coder)
   open System.Tags Coder VaxPrim

   fun label lab = (pusha(address lab); inc offset)

   fun path1 [0] = ()
     | path1 [j] = (pop(direct r0); pusha(displace(4*j,r0)))
     | path1 (j::0::(more as _::_)) = 
		(pop(direct r0); push(deferred(4*j,r0)); path1 more)
     | path1 (j::(more as _::_)) = 
		(pop(direct r0); push(displace(4*j,r0)); path1 more)

   fun path [i] = (pusha(displace((!offset+i)*4,sp)); inc offset)
     | path (i::0::(more as _::_)) = (push(deferred((!offset+i)*4,sp));
			   path1 more; inc offset)
     | path (i::(more as _::_)) = (push(displace((!offset+i)*4,sp));
			   path1 more; inc offset)

   fun const i = (push(immed(i+i+1))
		  handle Overflow => 
		    if i mod 2 = 0
		      then (push(immed(i+1)); 
			    addl2(immed i, displace(0,sp)))
		      else (push(immed i); 
			    addl2(immed (i-1), displace(0,sp));
			    addl2(immed 2, displace(0,sp)));
		  inc offset)

   fun squeeze () = (pop(direct r0); addl2(immed(4), direct sp);
		     push(direct r0); dec offset)

   fun apply() =
	let val lab = newlabel()
	 in pusha(address lab);
	    path[2+ ~(!offset),0,0]; dec offset;
	    pop(direct r0);
	    jmp(displace(0,r0));
	    align(); mark(); define(lab);
	    addl2(immed(8), direct(sp));
	    push(direct r0);
	    dec offset
	end

   fun applyknown(func) =
	let val lab = newlabel()
	 in pusha(address lab);
	    jbr (address func);
	    align(); mark(); define(lab);
	    addl2(immed(8), direct(sp));
	    push(direct r0);
	    dec offset
	end

    fun tail() =
	(pop(displace((!offset-2)*4,sp));
	 pop(displace((!offset-2)*4,sp));
	 addl2(immed((!offset-4)*4), direct sp);
	 path[2+ ~(!offset),0,0]; dec offset;
	 pop(direct r0);
	 jmp(displace(0,r0));
	 dec offset)

    fun tailrecur(0) =
	(pop(displace((!offset-2)*4,sp));
	 addl2(immed((!offset-3)*4), direct sp);
	 path[2+ ~(!offset),0,0]; dec offset;
	 pop(direct r0);
	 jmp(displace(0,r0)))
      | tailrecur(you) =
	(pop(displace((!offset-2)*4,sp));
	 addl2(immed((!offset-3)*4), direct sp);
	 path[2+ ~(!offset),0]; dec offset;
	 pop(direct r0);
	 addl2(immed(4*you),direct r0);
	 movl(direct r0,displace(8,sp));
	 jmp(deferred(0,r0)))

    fun startswitch() = (pop(direct r1); dec offset)
    fun boxed (boxl,defl) =
	(jbr (address defl); define(boxl))
    fun testboxed lab =
	bbc (immed 0, direct r1, address lab)
    fun endboxed (defl) = define defl
    fun gettag() = movl(displace(4,r1),direct r1)
    fun testcase(skip) =
	(pop(direct r0);
	 cmpl(direct r0, direct r1);
	 jne (address skip);
	 dec offset)
    fun testcase_int(i,skip) =
	(cmpl(direct r1,immed(i+i+1))
	 handle Overflow => (const i; cmpl(direct r1, autoinc(sp)));
	 jne (address skip))
    fun testcase_real(lab,skip) =
	(moval(address lab,direct r0);
	 cmpg(displace(0,r1),displace(0,r0));
	 jne (address skip))
    fun testcase_string(lab,skip,len) =
	 let fun next i = if i<len
			  then (cmpl(displace(i,r0), displace(i,r1));
			        jne (address skip);
				next(i+4))
			  else ()
	  in moval(address lab,direct r0);
	     ashl(immed(~width_tags),displace(~4,r0),direct r2);
	     ashl(immed(~width_tags),displace(~4,r1),direct r3);
	     cmpl(direct r2,direct r3);
	     jne(address skip);
	     next 0
	 end
    fun endcase(join,skip) =
	(pop(direct r0); jbr (address join); define(skip); dec offset)
    fun endswitch(join) = (pop(direct r0); define(join); push(direct r0))

    val savewords = ref 0
    fun startalloc(words) = (savewords := words)
    fun nextelem (i) =
     if (i+1= !savewords)
      then (pop(direct r0); movl(direct r0,displace(4*i,r12)); dec offset)
      else (pop(displace(4*i,r12)); dec offset)
    fun endrecord () =
	(movl(immed(power_tags*(!savewords)+tag_record),
		displace(~4,r12));
	 push(direct(r12));
	 addl2(immed(4*(!savewords)+4), direct(r12));
	 inc offset)
    fun endclosure () =
	(movl(immed(power_tags*(!savewords)+tag_closure),
		displace(~4,r12));
	 push(direct(r12));
	 addl2(immed(4*(!savewords)+4), direct(r12));
	 inc offset)

    fun select(i) =
	(pop(direct r0);
	 push(displace(4*i,r0)))
    fun starthandle(handler,skip) =
	(pusha(address handler);
	 push(direct r13);
	 movl(direct sp,direct r13);
	 inc offset; inc offset)
    fun midhandle(handler,skip) =
	(pop(displace(4,sp));
	 pop(direct r13);
	 jbr (address skip); 
	 align(); mark(); define(handler);
         push(direct r0);
	 dec offset; dec offset)
    fun endhandle(handler,skip) = define(skip);
    fun raisexn() =
	(pop(direct r0);
	 movl(direct r13,direct sp);
	 pop(direct r13);
	 rsb())
    fun definelabel(lab) =
	(align(); mark(); define(lab))
    fun return() =
	(pop(direct r0); rsb())
    fun stringconst(s,lab) =
	(align(); mark();
	 emitlong(length s * power_tags + tag_embedded);
	 define(lab);
	 emitstring s)
    val realconst = fn(s,lab) =>
	(align(); mark();
	 emitlong(8*power_tags + tag_embedded);
	 define(lab);
	 realconst s)

    fun profile(i,a) = addl2(immed a, displace(4*i,r10))

end (* functor VaxGen *)
