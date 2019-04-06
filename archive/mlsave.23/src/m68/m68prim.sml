signature MACHINEPRIM =
sig
      type Label
      val canapp : Access.primop -> int
      val primapp : Access.primop -> unit
      val tester : Access.primop -> (Label -> unit) option
end

structure M68options = System.Control.CG.M68

functor M68prim (Coder : M68CODER) : MACHINEPRIM =
struct

   open System.Tags Coder Access

   val canapp =
     fn P.:= => 2
      | P.eql => 1
      | P.neq => 1
      | P.unboxedassign => 2
      | P.~ => 1
      | P.makeref => 1
      | P.! => 1
      | P.* => 2
      | P.div => 2
      | P.+ => 2
      | P.- => 2
      | P.boxed => 1
      | P.ieql => 2
      | P.ineq => 2
      | P.> => 2
      | P.>= => 2
      | P.< => 2
      | P.<= => 2
      | P.subscript => 2
      | P.update => 3
      | P.unboxedupdate => 3
      | P.alength => 1
      | P.slength => 1
      | P.store => 3
      | P.ordof => 2
      | P.cast => 1
      | P.fneg => 1
      | P.fmul => 2
      | P.fdiv => 2
      | P.fadd => 2
      | P.fsub => 2
      | P.feql => 2
      | P.fneq => 2
      | P.fgt => 2
      | P.flt => 2
      | P.fge => 2
      | P.fle => 2
      | _ => ErrorMsg.impossible "arity"

   (* from runtime/prof.h *)
   val refcells = 4
   val reflists = 5

   (* This sequence must be changed with care; callgc needs to know
      the length of code it produces. *)
   fun markstore() =
         (if !CGoptions.profile
	  then addl(Immed 1,Displace(a5,4*reflists))
	  else ();
	  movl(Direct d6, Displace(a6,0));
	  movl(Direct a0, Displace(a6,~4));
	  movl(Direct a6, Direct d6);
	  addl(Immed 8, Direct a6))

   val realdesc = Immed(8*power_tags + tag_string)

   val do_offset = (fn p => offset := !offset+1-canapp p)

   fun intcode (p,f) = let val lab = newlabel()
			 in movl(Immed 3,Direct d0);
			    cmpl(PostInc sp,PostInc sp);
			    f(Address lab);
			    movl(Immed 1,Direct d0);
			    define lab;
			    push(Direct d0);
			    do_offset p
			 end

   fun floatcode (p,f) = (pop(Direct a0);
			  pop(Direct a1);
			  fmoved(Displace(a1,0),Direct fp0);
			  f(Displace(a0,0),Direct fp0);
			  fmoved(Direct fp0,Displace(a6,0));
			  movl(realdesc,Displace(a6,~4));
			  push(Direct a6);
			  addl(Immed 12,Direct a6);
			  do_offset p)

   fun floatcmp (p,f) = let val lab = newlabel()
			  in movl(Immed 3,Direct d0);
			     pop(Direct a0);
			     fmoved(Displace(a0,0),Direct fp0);
			     pop(Direct a0);
			     fcmpd(Displace(a0,0),Direct fp0);
			     f(Address lab);
			     movl(Immed 1,Direct d0);
			     define lab;
			     push(Direct d0);
			     do_offset p
			  end

   fun testicode f = SOME (fn l => (cmpl(PostInc sp,PostInc sp);
				    f(Address l); 
				    offset := !offset-2))

   fun testfcode f = SOME (fn l => (pop(Direct a0);
				    fmoved(Displace(a0,0),Direct fp0);
				    pop(Direct a0);
				    fcmpd(Displace(a0,0),Direct fp0);
				    f(Address l);
				    offset := !offset-2))

   fun primapp p =
       case p of
         P.eql => do_offset p
       | P.neq => do_offset p
       | P.:= => (pop(Direct d0); pop(Direct a0);
		 movl(Direct d0, Displace(a0,0));
		 markstore();
		 push(Immed 1);
		 do_offset p)
       | P.unboxedassign => (pop(Direct d0); pop(Direct a0);
		            movl(Direct d0, Displace(a0,0));
			    push(Immed 1);
			    do_offset p)
       | P.~ => (pop(Direct d0);
		movl(Immed 2, Direct d1);
		subl(Direct d0, Direct d1);
		if !M68options.trapv then trapv() else ();
		push(Direct d1);
		do_offset p)
       | P.makeref => (pop(Direct d0);
		      if !CGoptions.profile
		      then addl(Immed 1,Displace(a5,4*refcells))
		      else ();
		      movl(Direct d0,Displace(a6,0));
		      movl(Immed(1*power_tags+tag_array),Displace(a6,~4));
		      movl(Direct a6,Direct d0);
		      addl(Immed 8,Direct a6);
		      push(Direct d0);
		      do_offset p)
       | P.! => (pop(Direct a0);
		push(Displace(a0,0));
		do_offset p)
       | P.* => (pop(Direct d0);
		 asrl(Immed 1, Direct d0);
		 pop(Direct d1);
		 subl(Immed 1, Direct d1);
		 mull(Direct d1,Direct d0);
		 if !M68options.trapv then trapv() else ();
		 addl(Immed 1,Direct d0);
		 push(Direct d0);
		do_offset p)
       | P.div => (pop(Direct d0);
		 asrl(Immed 1,Direct d0);
	         pop(Direct d1);
		 asrl(Immed 1,Direct d1);
		 divl(Direct d0,Direct d1);
		 addl(Direct d1,Direct d1);
		 addl(Immed 1,Direct d1);
		 push(Direct d1);
		  do_offset p)
       | P.+ => (pop(Direct d0);
		 subl(Immed 1,Direct d0);
		 addl(PostInc sp,Direct d0);
		 if !M68options.trapv then trapv() else ();
		 push(Direct d0);
		do_offset p)
       | P.- => (pop(Direct d0);
		 pop(Direct d1);
		 subl(Direct d0, Direct d1);
		 if !M68options.trapv then trapv() else ();
		 addl(Immed 1,Direct d1);
		 push(Direct d1);
		do_offset p)
       | P.boxed => let val lab = newlabel()
		    in pop(Direct d1);
		       movl(Immed 1, Direct d0);
		       btst(Immed 0, Direct d1);
		       jne(Address lab);
		       movl(Immed 3, Direct d0);
		       define lab;
		       push(Direct d0);
		       do_offset p
		    end
       | P.ieql => intcode (p,jeq)
       | P.ineq => intcode (p,jne)
       | P.> =>    intcode (p,jgt)
       | P.>= =>   intcode (p,jge)
       | P.< =>    intcode (p,jlt)
       | P.<= =>   intcode (p,jle)
       | P.fmul => floatcode (p,fmuld)
       | P.fdiv => floatcode (p,fdivd)
       | P.fadd => floatcode (p,faddd)
       | P.fsub => floatcode (p,fsubd)
       | P.feql => floatcmp (p,fjeq)
       | P.fneq => floatcmp (p,fjne)
       | P.fgt =>  floatcmp (p,fjlt)
       | P.flt =>  floatcmp (p,fjgt)
       | P.fge =>  floatcmp (p,fjle)
       | P.fle =>  floatcmp (p,fjge)
       | P.subscript => (pop(Direct d0);   (* index *)
			 pop(Direct a0);   (* array *)
			 push(Index(a0, ~2, d0, Word));
			 do_offset p)
       | P.update => (pop(Direct d0);    (* new value *)
		      pop(Direct d1);    (* index *)
		      pop(Direct a0);	    (* array *)
		      subl(Immed 1, Direct d1);
		      addl(Direct d1, Direct d1);
		      addl(Direct d1, Direct a0);
		      movl(Direct d0, Displace(a0,0));
		      markstore();
		      push(Immed 1);
		      do_offset p)
       | P.unboxedupdate => (pop(Direct d0);    (* new value *)
			     pop(Direct d1);    (* index *)
			     pop(Direct a0);	    (* array *)
			     movl(Direct d0, Index(a0,~2,d1,Word));
			     push(Immed 1);
			     do_offset p)
       | P.alength => (pop(Direct a0);
		       movl(Displace(a0,~4),Direct d0);
		       asrl(Immed width_tags,Direct d0);
		       addl(Direct d0,Direct d0);
		       addl(Immed 1, Direct d0);
		       push(Direct d0);
		       do_offset p)
       | P.slength => let val lab = newlabel()
		      in pop(Direct d1);
			 movl(Immed 3, Direct d0);
			 btst(Immed 0, Direct d1);
			 jne(Address lab);
			 movl(Direct d1,Direct a0);
			 movl(Displace(a0,~4),Direct d0);
			 asrl(Immed width_tags,Direct d0);
			 addl(Direct d0, Direct d0);
			 addl(Immed 1, Direct d0);
			 define lab;
			 push(Direct d0);
			 do_offset p
		      end		
       | P.store => (pop(Direct d0);    (* new value *)
		     pop(Direct d1);    (* index *)
		     pop(Direct a0);	    (* byte array *)
		     asrl(Immed 1, Direct d0);
		     asrl(Immed 1, Direct d1);
		     movb(Direct d0, Index(a0,0,d1,Byte));
		     push(Immed 1);
		     do_offset p)
       | P.ordof => (pop(Direct d1);    (* index *)
		     pop(Direct a0);    (* string *)
		     asrl(Immed 1,Direct d1);
		     movl(Immed 0, Direct d0);
		     movb(Index(a0,0,d1,Byte),Direct d0);
		     asll(Immed 1, Direct d0);
		     addl(Immed 1, Direct d0);
		     push(Direct d0);
		     do_offset p)
       | P.cast => do_offset p
       | P.fneg => (pop(Direct a0);
		    fnegd(Displace(a0,0),Direct fp0);
		    fmoved(Direct fp0,Displace(a6,0));
		    movl(realdesc,Displace(a6,~4));
		    push(Direct a6);
		    addl(Immed 12,Direct a6);
		    do_offset p)
       | _ => ErrorMsg.impossible "gen"

   val tester =
       fn P.ieql => testicode jeq
        | P.ineq => testicode jne
	| P.> =>    testicode jgt
	| P.>= =>   testicode jge
	| P.< =>    testicode jlt
	| P.<= =>   testicode jle
	| P.feql => testfcode fjeq
	| P.fneq => testfcode fjne
	| P.fgt =>  testfcode fjlt
	| P.flt =>  testfcode fjgt
	| P.fge =>  testfcode fjle
	| P.fle =>  testfcode fjge
	| _ => NONE

end (* functor vaxprim *)
