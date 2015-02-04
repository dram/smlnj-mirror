(* M68 code generator for ML
   a7 : stack pointer
   d7 : exception handler
   a6 : freespace pointer
   d6 : store pointer
   a5 : profile pointer
*)

functor M68Gen (Coder : M68CODER) : machine = 
 struct

   structure M68Prim = M68prim(Coder)
   open Boot.tags Coder M68Prim

   fun label (L) = (pusha (Address L); inc offset)

   fun path1 [0] = ()
     | path1 [j] = (pop(Direct a1); pusha(Displace(a1,4*j)))
     | path1 (j::more) = (pop(Direct a1); push(Displace(a1,4*j)); path1 more)

   fun path [i] = (pusha(Displace(sp,(!offset+i)*4)); inc offset)
     | path (i::more) = (push(Displace(sp,(!offset+i)*4));
			 path1 more; inc offset)

   fun const i = (push(Immed(i+i+1))
		  handlex overflow => 
		    if i mod 2 = 0
		      then (push(Immed(i+1)); 
			    addl(Immed i, Displace(sp,0)))
		      else (push(Immed i); 
			    addl(Immed (i-1), Displace(sp,0));
			    addl(Immed 2, Displace(sp,0)));
		  inc offset)

   fun squeeze () = (pop (Direct d0);
		      addl (Immed 4,Direct sp);
		      push (Direct d0);
		      dec offset)

    fun apply() =
	let val L = newlabel ()
	in pusha (Address L);
	   path[2+ ~(!offset),0,0]; dec offset;
	   pop(Direct a1);
	   jra (Displace(a1,0));
	   align(); mark(); define(L);
	   addl (Immed 8,Direct sp);
	   push (Direct d0);
	   dec offset
	end

   fun applyknown(func) =
	let val L = newlabel()
	 in pusha(Address L);
	    jra(Address func);
	    align(); mark(); define(L);
	    addl(Immed 8,Direct sp);
	    push (Direct d0);
	    dec offset
	end

    fun tail() =
	  (pop (Displace(sp,(!offset-2)*4));
	   pop (Displace(sp,(!offset-2)*4));
	   addl (Immed ((!offset-4)*4),Direct sp);
	   path[2+ ~(!offset),0,0]; dec offset;
	   pop(Direct a1);
	   jra (Displace(a1,0));
	   dec offset)

    fun tailrecur(you) =
	(pop (Displace(sp,(!offset-2)*4));
	 addl (Immed((!offset-3)*4),Direct sp);
	 path[2 - !offset,you]; dec offset;
	 pop(Direct a1);
	 if you <> 0 then movl(Direct a1,Displace(sp,8))
		     else ();
	 movl(Displace(a1,0),Direct a1);
	 jra (Displace(a1,0)))

    fun startswitch() = (pop (Direct d0); dec offset)
    fun boxed (boxl,defl) =
	(jra(Address defl);
	 define (boxl))
    fun testboxed L =
	(btst (Immed 0,Direct d0);
	 jeq (Address L))
    fun endboxed defl = define defl
    fun gettag() =
	(movl (Direct d0,Direct a0);
	 movl (Displace(a0,4),Direct d0))
    fun testcase(skip) =
	  (pop(Direct d1);
	   cmpl (Direct d1, Direct d0);
	   jne (Address skip);
	   dec offset)
    fun testcase_int(i,skip) =
	  (cmpl (Immed (i+i+1),Direct d0)
	   handlex overflow => (const i; cmpl(Direct d0, PostInc(sp)));
	   jne (Address skip))
    fun testcase_real(lab,skip) =
	(lea(Address lab,Direct a0);
	 fmoved(Displace(a0,0),Direct(FloatReg 0));
	 movl(Direct d0,Direct a1);
	 fcmpd(Displace(a1,0),Direct(FloatReg 0));
	 fjne (Address skip))
    fun testcase_string(lab,skip,len) =
	 let fun next i = (cmpl(PostInc(a0), PostInc(a1));
			   jne (Address skip);
			   if i<len then next(i+4) else ())
	  in lea (Address lab,Direct a0);
	     movl (Direct d0, Direct a1);
	     next 0
	 end
    fun endcase(join,skip) =
	  (jra (Address join);
	   define skip;
	   dec offset)
    fun endswitch(join) = define join

    val savewords = ref 0
    fun startalloc (n) = (savewords := n)
    fun nextelem (i) = 
     if (i+1 = !savewords)
      then (pop(Direct d0); movl(Direct d0,Displace(a6,4*i)); dec offset)
      else (pop(Displace(a6,4*i)); dec offset)
    fun endrecord () =
	(movl(Immed(power_tags*(!savewords)+tag_record),
		Displace(a6,~4));
	 push(Direct(a6));
	 lea(Displace(a6,4*(!savewords)+4), Direct(a6));
	 inc offset)
    fun endclosure () =
	(movl(Immed(power_tags*(!savewords)+tag_closure),
		Displace(a6,~4));
	 push(Direct(a6));
	 lea(Displace(a6,4*(!savewords)+4), Direct(a6));
	 inc offset)

    fun select(i) =
	(pop (Direct a1);
	 push (Displace(a1,4*i)))
    fun starthandle(handler,skip) =
	(pusha (Address handler);
	 push (Direct d7);
	 movl (Direct sp,Direct d7);
	 inc offset; inc offset)
    fun midhandle(handler,skip) =
	(pop (Displace(sp,4));
	 pop (Direct d7);
	 jra (Address skip);
	 align(); mark(); define(handler);
	 push (Direct d0);
	 dec offset; dec offset)
    fun endhandle(handler,skip) = define skip
    fun raisexn() =
	(pop (Direct d0);
	 movl (Direct d7,Direct sp);
	 pop (Direct d7);
	 rts())
    fun definelabel(L) = (align(); mark(); define(L))
    fun return() =
	(pop (Direct d0);
	 rts())
    fun stringconst(s,L) =
	(align(); mark();
	 (* String change -- byte instead of word length *)
	 emitlong(((length s + 7) div 4) * power_tags + tag_embedded);
	 define(L);
	 emitstring s)
    val realconst = fn(s,L) =>
	(align(); mark();
	 (* String change -- byte instead of word length *)
	 emitlong(2*power_tags + tag_embedded);
	 define(L);
	 realconst s)

    fun profile(i,a) = addl(Immed a, Displace(a5,4*i))

end (* functor M68Gen *)
