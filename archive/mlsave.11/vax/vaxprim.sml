signature MACHINEPRIM =
sig
      exception Notprim
      val canapp : int -> int
      val primapp : int -> unit
end

functor VaxPrim (Coder : VAXCODER) : MACHINEPRIM =
struct

   open System.Tags Coder
   val primcount = 40

   (* from runtime/prof.h *)
   val refcells = 4
   val reflists = 5

   exception Notprim
   val apparray = array(primcount, (fn () => ((raise Notprim):unit)))
   val arity = array(primcount, ~1)

   fun canapp i = let val j = arity sub i handle Subscript => raise Notprim
		   in if j>=0 then j else raise Notprim
		  end
   fun primapp i = (apparray sub i handle Subscript => raise Notprim) ()

   nonfix ::=
   fun ::=((s,n),f) = 
       let open EnvAccess Basics Access
           val VARbind(VALvar{access=INLINE i,...}) =
            lookVARCONinStr(Prim.inLinePrim,SymbolTable.stringToSymbol(s),[])
	in update(arity,i,n);
	   update(apparray,i, (fn () => (f(); offset := !offset+1-n) ))
       end

   infix ::=

   fun markstore() =
	   (if !CGoptions.closurecount
	    then addl2(immed 1,displace(4*reflists,r10))
	    else ();
	    movl(direct r11, displace(0,r12));
	    movl(direct r1, displace(~4,r12));
	    movl(direct r12,direct r11);
	    addl2(immed 8, direct r12))

   val realdesc = immed(8*power_tags + tag_string)

   val () = (

    ("=",1) ::= (fn () => ());
    ("<>",1) ::= (fn () => ());

    (":=", 2)
	::= (fn () => (pop(direct r0); pop(direct r1);
		       movl(direct r0, displace(0,r1));
		       markstore();
		       push(immed 1)));

    ("unboxedassign", 2)
	::= (fn () => (pop(direct r0); pop(direct r1);
		       movl(direct r0, displace(0,r1));
		       push(immed 1)));

    ("~", 1)
       ::= (fn () => (pop(direct r0);
		      subl3(direct r0, immed 2, direct r0);
		      push(direct r0)));
   
    ("makeref", 1)
       ::= (fn () => (pop(direct r0);
		      if !CGoptions.closurecount
		      then addl2(immed 1,displace(4*refcells,r10))
		      else ();
		      movl(direct r0,displace(0,r12));
		      movl(immed(1*power_tags+tag_array),displace(~4,r12));
		      movl(direct r12,direct r0);
		      addl2(immed 8,direct r12);
		      push(direct r0)));

    ("!", 1)
       ::= (fn () => (pop(direct r0);
		      push(displace(0,r0))));

    ("*",2)
       ::= (fn () =>
		(pop(direct r0);
		 ashl(immed(~1), direct r0, direct r0);
		 subl3(immed 1,autoinc sp,direct r1);
		 mull2(direct r1,direct r0);
		 addl2(immed 1,direct r0);
		 push(direct r0) ));

    ("div", 2)
       ::= (fn () =>
		(pop(direct r0);
		 ashl(immed(~1),direct r0,direct r1);
		 ashl(immed(~1),autoinc sp,direct r0);
		 divl2(direct r1,direct r0);
		 addl2(direct r0,direct r0);
		 addl2(immed 1,direct r0);
		 push(direct r0) ));

    ("+", 2)
       ::= (fn () =>
		(pop(direct r0);
		 subl2(immed 1,direct r0);
		 addl2(autoinc sp,direct r0);
		 push(direct r0) ));

    ("-", 2)
       ::= (fn () =>
		(pop(direct r0);
		 subl3(direct r0, autoinc sp,direct r0);
		 addl2(immed 1,direct r0);
		 push(direct r0) ));

    ("ieql", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 beql(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("ineq", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 bneq(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    (">", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 blss(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    (">=", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 bleq(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("<", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 bgtr(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("<=", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 bgeq(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("subscript", 2)
	::= (fn () =>
		(pop(direct r0);   (* index *)
		 addl2(direct(r0),direct(r0));
	         addl2(autoinc(sp),direct(r0));
		 push(displace(~2,r0)) ));

    ("update", 3)
	::= (fn () =>
	       (pop(direct r0);    (* new value *)
		pop(direct r1);    (* index *)
		pop(direct r2);	    (* array *)
		subl2(immed 1, direct r1);
		addl2(direct r1, direct r1);
	        addl2(direct r2, direct r1);
		movl(direct r0, displace(0,r1));
		markstore();
		push(immed 1) ));

    ("unboxedupdate", 3)
	::= (fn () =>
	       (pop(direct r0);    (* new value *)
		pop(direct r2);    (* index *)
		pop(direct r1);	    (* array *)
		addl2(direct r2, direct r2);
	        addl2(direct r1, direct r2);
		movl(direct r0, displace(~2,r2));
		push(immed 1) ));

    ("alength", 1)
	::= (fn () =>
		(pop(direct r0);
		 ashl(immed(~width_tags),displace(~4,r0), direct r0);
		 addl2(direct r0,direct r0);
		 addl2(immed 1, direct r0);
		 push(direct r0) ));

    ("slength", 1)
	::= (fn () =>
		let val lab = newlabel()
		 in pop(direct r1);
		    movl(immed 3, direct r0);
		    bbs(immed 0, direct r1, address lab);
		    ashl(immed(~width_tags), displace(~4,r1), direct r0);
		    addl2(direct r0, direct r0);
		    addl2(immed 1, direct r0);
		    define lab;
		    push(direct r0)
		end );

    ("store", 3)
	::= (fn () =>
	       (pop(direct r0);    (* new value *)
		ashl(immed(~1), direct r0, direct r0);
		pop(direct r1);    (* index *)
		ashl(immed(~1), direct r1, direct r1);
		addl2(autoinc(sp),direct r1);
		movb(direct r0, displace(0,r1));
		push(immed 1) ));

		
    ("ordof", 2) ::= (fn () =>
       (pop(direct r0);    (* index *)
        pop(direct r1);    (* string *)
	ashl(immed ~1, direct r0, direct r0);
	movzbl(index(displace(0,r1),r0), direct r0);
	ashl(immed 1, direct r0, direct r0);
	addl2(immed 1, direct r0);
	push(direct r0)));


   ("cast", 1) ::= (fn () => ());

    ("fneg", 1)
       ::= (fn () =>
		(pop(direct r0);
		 mnegg(displace(0,r0),displace(0,r12));
		 movl(realdesc,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fmul", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 mulg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(realdesc,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fdiv", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 divg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(realdesc,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fadd", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 addg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(realdesc,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fsub", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 subg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(realdesc,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("feql", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 beql(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("fneq", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 bneq(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("fgt", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 blss(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("fge", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 bleq(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("flt", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 bgtr(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

    ("fle", 2)
       ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 bgeq(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));

   ())

    nonfix ::=
end (* functor vaxprim *)
