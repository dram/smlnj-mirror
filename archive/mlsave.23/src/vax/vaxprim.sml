signature MACHINEPRIM =
sig
      type Label
      val canapp : Access.primop -> int
      val primapp : Access.primop -> unit
      val tester : Access.primop -> (Label -> unit) option
end

functor VaxPrim (Coder : VAXCODER) : MACHINEPRIM =
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
	then addl2(immed 1,displace(4*reflists,r10))
	else ();
	movl(direct r11, displace(0,r12));
	movl(direct r1, displace(~4,r12));
	movl(direct r12,direct r11);
	addl2(immed 8, direct r12))

   val realdesc = immed(8*power_tags + tag_string)

   val do_offset = (fn p => offset := !offset+1-canapp p)

   fun intcode (p,cmp) = (movl(immed 3,direct r0);
		          cmpl(autoinc sp,autoinc sp);
			  cmp(displace(3,pc));
			  movl(immed 1,direct r0);
			  push(direct r0);
			  do_offset p)

   fun floatcode (p,f) = (pop(direct r0);
			  pop(direct r1);
			  f(displace(0,r0),displace(0,r1),displace(0,r12));
			  movl(realdesc,displace(~4,r12));
			  movl(direct r12,direct r0);
			  addl2(immed 12,direct r12);
			  push(direct r0);
			  do_offset p)

   fun floatcmp (p,cmp) = (movl(immed 3,direct r0);
			   movl(autoinc sp,direct r1);
			   movl(autoinc sp,direct r2);
			   cmpg(displace(0,r1),displace(0,r2));
			   cmp(displace(3,pc));
			   movl(immed 1,direct r0);
			   push(direct r0);
			   do_offset p)

   fun testicode f = SOME (fn l => (cmpl(autoinc sp,autoinc sp);
				     f(address l);
				     offset := !offset-2))

   fun testfcode f = SOME (fn l => (movl(autoinc sp,direct r1);
				     movl(autoinc sp,direct r2);
				     cmpg(displace(0,r1),displace(0,r2));
				     f(address l);
				     offset := !offset-2))

   fun primapp p =
       case p of
         P.eql => do_offset p
       | P.neq => do_offset p
       | P.:= => (pop(direct r0); pop(direct r1);
		 movl(direct r0, displace(0,r1));
		 markstore();
		 push(immed 1);
		 do_offset p)
       | P.unboxedassign => (pop(direct r0); pop(direct r1);
		            movl(direct r0, displace(0,r1));
		            push(immed 1);
			    do_offset p)
       | P.~ => (pop(direct r0);
		subl3(direct r0, immed 2, direct r0);
		push(direct r0);
		do_offset p)
       | P.makeref => (pop(direct r0);
		      if !CGoptions.profile
		      then addl2(immed 1,displace(4*refcells,r10))
		      else ();
		      movl(direct r0,displace(0,r12));
		      movl(immed(1*power_tags+tag_array),displace(~4,r12));
		      movl(direct r12,direct r0);
		      addl2(immed 8,direct r12);
		      push(direct r0);
		      do_offset p)
       | P.! => (pop(direct r0);
		push(displace(0,r0));
		do_offset p)
       | P.* => (pop(direct r0);
		ashl(immed(~1), direct r0, direct r0);
		subl3(immed 1,autoinc sp,direct r1);
		mull2(direct r1,direct r0);
		addl2(immed 1,direct r0);
		push(direct r0);
		do_offset p)
       | P.div => (pop(direct r0);
		  ashl(immed(~1),direct r0,direct r1);
		  ashl(immed(~1),autoinc sp,direct r0);
		  divl2(direct r1,direct r0);
		  addl2(direct r0,direct r0);
		  addl2(immed 1,direct r0);
		  push(direct r0);
		  do_offset p)
       | P.+ => (pop(direct r0);
		subl2(immed 1,direct r0);
		addl2(autoinc sp,direct r0);
		push(direct r0);
		do_offset p)
       | P.- => (pop(direct r0);
		subl3(direct r0, autoinc sp,direct r0);
		addl2(immed 1,direct r0);
		push(direct r0);
		do_offset p)
       | P.boxed => let val lab = newlabel()
		    in  pop(direct r1);
			movl(immed 1, direct r0);
			bbs(immed 0, direct r1, address lab);
			movl(immed 3, direct r0);
			define lab;
			push(direct r0);
			do_offset p
		    end		
       | P.ieql => intcode (p,beql)
       | P.ineq => intcode (p,bneq)
       | P.> =>    intcode (p,blss)
       | P.>= =>   intcode (p,bleq)
       | P.< =>    intcode (p,bgtr)
       | P.<= =>   intcode (p,bgeq)
       | P.fmul => floatcode (p,mulg3)
       | P.fdiv => floatcode (p,divg3)
       | P.fadd => floatcode (p,addg3)
       | P.fsub => floatcode (p,subg3)
       | P.feql => floatcmp (p,beql)
       | P.fneq => floatcmp (p,bneq)
       | P.fgt =>  floatcmp (p,blss)
       | P.flt =>  floatcmp (p,bgtr)
       | P.fge =>  floatcmp (p,bleq)
       | P.fle =>  floatcmp (p,bgeq)
       | P.subscript => (pop(direct r0);   (* index *)
			addl2(direct(r0),direct(r0));
			addl2(autoinc(sp),direct(r0));
			push(displace(~2,r0));
			do_offset p)
       | P.update => (pop(direct r0);    (* new value *)
		     pop(direct r1);    (* index *)
		     pop(direct r2);	   (* array *)
		     subl2(immed 1, direct r1);
		     addl2(direct r1, direct r1);
		     addl2(direct r2, direct r1);
		     movl(direct r0, displace(0,r1));
		     markstore();
		     push(immed 1);
		     do_offset p)
       | P.unboxedupdate => (pop(direct r0);    (* new value *)
			    pop(direct r2);    (* index *)
			    pop(direct r1);	    (* array *)
			    addl2(direct r2, direct r2);
			    addl2(direct r1, direct r2);
			    movl(direct r0, displace(~2,r2));
			    push(immed 1);
			    do_offset p)
       | P.alength => (pop(direct r0);
		      ashl(immed(~width_tags),displace(~4,r0), direct r0);
		      addl2(direct r0,direct r0);
		      addl2(immed 1, direct r0);
		      push(direct r0);
		      do_offset p)
       | P.slength => let val lab = newlabel()
		      in pop(direct r1);
			 movl(immed 3, direct r0);
			 bbs(immed 0, direct r1, address lab);
			 ashl(immed(~width_tags), displace(~4,r1), direct r0);
			 addl2(direct r0, direct r0);
			 addl2(immed 1, direct r0);
			 define lab;
			 push(direct r0);
			 do_offset p
		      end		
       | P.store => (pop(direct r0);    (* new value *)
		    ashl(immed(~1), direct r0, direct r0);
		    pop(direct r1);    (* index *)
		    ashl(immed(~1), direct r1, direct r1);
		    addl2(autoinc(sp),direct r1);
		    movb(direct r0, displace(0,r1));
		    push(immed 1);
		    do_offset p)
       | P.ordof => (pop(direct r0);    (* index *)
		    pop(direct r1);    (* string *)
		    ashl(immed ~1, direct r0, direct r0);
		    movzbl(index(displace(0,r1),r0), direct r0);
		    ashl(immed 1, direct r0, direct r0);
		    addl2(immed 1, direct r0);
		    push(direct r0);
		    do_offset p)
       | P.cast => do_offset p
       | P.fneg => (pop(direct r0);
		   mnegg(displace(0,r0),displace(0,r12));
		   movl(realdesc,displace(~4,r12));
		   movl(direct r12,direct r0);
		   addl2(immed 12,direct r12);
		   push(direct r0);
		   do_offset p)
       | _ => ErrorMsg.impossible "gen"

   val tester =
       fn P.ieql => testicode beql
        | P.ineq => testicode bneq
	| P.> =>    testicode blss
	| P.>= =>   testicode bleq
	| P.< =>    testicode bgtr
	| P.<= =>   testicode bgeq       
	| P.fneq => testfcode bneq
	| P.fgt =>  testfcode blss
	| P.flt =>  testfcode bgtr
	| P.fge =>  testfcode bleq
	| P.fle =>  testfcode bgeq
	| _ => NONE

end (* functor vaxprim *)
