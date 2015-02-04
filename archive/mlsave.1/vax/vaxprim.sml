signature machineprim =
sig
      exceptionx notprim : unit
      val canapp : int -> int
      val primapp : int -> unit
end

functor VaxPrim (Coder : VAXCODER) : machineprim =
struct

   open Coder
   val primcount = 40

   exceptionx notprim
   val apparray = array(primcount, (fn () => ((raisex notprim):unit)))
   val arity = array(primcount, ~1)

   fun canapp i = let val j = arity sub i handlex subscript => raisex notprim
		   in if j>=0 then j else raisex notprim
		  end
   fun primapp i = (apparray sub i handlex subscript => raisex notprim) ()

   nonfix ::=
   fun ::=((s,n),f) = 
       let open EnvAccess Basics Access
           val VARbind(VALvar{access=INLINE i,...}) =
            lookVARCONinStr(Prim.InLinePrim,SymbolTable.StringToSymbol(s),[])
	in update(arity,i,n);
	   update(apparray,i, (fn () => (f(); offset := !offset+1-n) ))
       end

   infix ::=

   fun markstore() =
	   (movl(direct r11, displace(0,r12));
	    movl(direct r1, displace(~4,r12));
	    movl(direct r12,direct r11);
	    addl2(immed 8, direct r12))

(*	let val L = newlabel()
         in bbs(immed 1, displace(~4,r1), address L);
	    addl2(immed 2, displace(~4,r1));
	    movl(direct r11, displace(0,r12));
	    movl(direct r1, displace(~4,r12));
	    movl(direct r12,direct r11);
	    addl2(immed 8, direct r12);
	    define L
	end
*)

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
		 ashl(immed(~2),displace(~4,r0), direct r0);
		 addl2(immed 1, direct r0);
		 push(direct r0) ));

    ("slength", 1)
	::= (fn () =>
		let val L = newlabel()
		 in pop(direct r1);
		    movl(immed 3, direct r0);
		    bbs(immed 0, direct r1, address L);
		    ashl(immed 1, displace(0,r1), direct r0);
		    addl2(immed 1, direct r0);
		    define L;
		    push(direct r0)
		end );

    ("store", 3)
	::= (fn () =>
	       (pop(direct r0);    (* new value *)
		ashl(immed(~1), direct r0, direct r0);
		pop(direct r1);    (* index *)
		ashl(immed(~1), direct r1, direct r1);
		addl2(autoinc(sp),direct r1);
		movb(direct r0, displace(4,r1));
		push(immed 1) ));

(*
    ("create", 1)
	::= (fn () =>
	       (pop(direct r0);    (* length *)
		ashl(immed ~1, direct r0, direct r1);
		addl2(immed(22), direct r0);
	        ashl(immed(~3),direct(r0),direct(r0));
		ashl(immed 3, direct r0, direct r2);
		subl2(immed 3, direct r2);
		ashl(immed 2, direct r0, direct r3);
	        movl(immed 0,index(displace(~8,r12),r0));
		movl(direct r1, displace(0,r12));
		movl(direct r2, displace(~4,r12));
		push(direct r12);
		addl2(direct r3, direct r12) ));
*)
		
    ("ordof", 2) ::= (fn () =>
       (pop(direct r0);    (* index *)
        pop(direct r1);    (* string *)
	ashl(immed ~1, direct r0, direct r0);
	movzbl(index(displace(4,r1),r0), direct r0);
	ashl(immed 1, direct r0, direct r0);
	addl2(immed 1, direct r0);
	push(direct r0)));


   ("cast", 1) ::= (fn () => ());

    ("fneg", 1)
       ::= (fn () =>
		(pop(direct r0);
		 mnegg(displace(0,r0),displace(0,r12));
		 movl(immed 21,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fmul", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 mulg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(immed 21,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fdiv", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 divg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(immed 21,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fadd", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 addg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(immed 21,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) ));

    ("fsub", 2)
       ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 subg3(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(immed 21,displace(~4,r12));
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
