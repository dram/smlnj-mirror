signature MACHINEPRIM =
sig
      exception Notprim
      type Label
      val canapp : int -> int
      val primapp : int -> unit
      val tester : int -> (Label -> unit) option
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
   val testarray = array(primcount, NONE : (Label -> unit) option)

   fun canapp i = let val j = arity sub i handle Subscript => raise Notprim
		   in if j>=0 then j else raise Notprim
		  end
   fun primapp i = (apparray sub i handle Subscript => raise Notprim) ()

   fun tester i = (testarray sub i handle Subscript => NONE)

   nonfix ::=
   fun ::=((s,n),f) = 
       let open EnvAccess Basics Access
           val VARbind(VALvar{access=INLINE i,...}) =
            lookVARCONinStr(Prim.inLine,Symbols.stringToSymbol(s),[])
	in update(arity,i,n);
	   update(apparray,i, (fn () => (f(); offset := !offset+1-n) ))
       end

   infix ::=

   nonfix :=:=
   fun :=:=((s,2),f) = 
       let open EnvAccess Basics Access
           val VARbind(VALvar{access=INLINE i,...}) =
            lookVARCONinStr(Prim.inLine,Symbols.stringToSymbol(s),[])
	in update(testarray,i, SOME(fn l => (f l; offset := !offset-2) ))
       end

   infix :=:=

   fun markstore() =
	   (if !CGoptions.profile
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
		      if !CGoptions.profile
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

    let fun g(s,x) =
	 ((s,2) ::= (fn () =>
		(movl(immed 3,direct r0);
		 cmpl(autoinc sp,autoinc sp);
		 x(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));
          (s,2) :=:= (fn l =>
		(cmpl(autoinc sp,autoinc sp);
		 x(address l))))
     in g("ieql",beql);
	g("ineq",bneq);
	g(">",blss);
        g(">=",bleq);
        g("<",bgtr);
	g("<=", bgeq)
    end;

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

   let fun g(s,x) =
	( (s,2) ::= (fn () =>
		(pop(direct r0);
		 pop(direct r1);
		 x(displace(0,r0),displace(0,r1),displace(0,r12));
		 movl(realdesc,displace(~4,r12));
		 movl(direct r12,direct r0);
		 addl2(immed 12,direct r12);
		 push(direct r0) )))
    in g("fmul",mulg3);
       g("fdiv",divg3);
       g("fadd",addg3);
       g("fsub",subg3)
   end;

   let fun g(s,x) =
	( (s,2) ::= (fn () =>
		(movl(immed 3,direct r0);
		 movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 x(displace(3,pc));
		 movl(immed 1,direct r0);
		 push(direct r0) ));
        (s, 2) :=:= (fn l =>
		(movl(autoinc sp,direct r1);
		 movl(autoinc sp,direct r2);
		 cmpg(displace(0,r1),displace(0,r2));
		 x (address l))))
      in g("feql",bleq);
	 g("fneq",bneq);
	 g("fgt", blss);
	 g("flt", bgtr);
	 g("fge", bleq);
	 g("fle", bgeq)
     end;

   ())

    nonfix ::=
end (* functor vaxprim *)
