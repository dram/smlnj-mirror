signature MACHINEPRIM =
sig
      exception Notprim
      type Label
      val canapp : int -> int
      val primapp : int -> unit
      val tester : int -> (Label -> unit) option
end

structure M68options = System.Control.CG.M68

functor M68prim (Coder : M68CODER) : MACHINEPRIM =
struct

   open System.Tags Coder
   val primcount = 50

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

   val () = (

    ("=",1) ::= (fn () => ());
    ("<>",1) ::= (fn () => ());

    (":=", 2)
	::= (fn () => (pop(Direct d0);
		       pop(Direct a0);
		       movl(Direct d0, Displace(a0,0));
		       markstore();
		       push(Immed 1) ));

    ("unboxedassign", 2)
	::= (fn () => (pop(Direct d0);
		       pop(Direct a0);
		       movl(Direct d0, Displace(a0,0));
		       push(Immed 1) ));

    ("~", 1)
       ::= (fn () => (pop(Direct d0);
		      movl(Immed 2, Direct d1);
		      subl(Direct d0, Direct d1);
		      if !M68options.trapv then trapv() else ();
		      push(Direct d1)));
   
    ("!", 1)
       ::= (fn () => (pop(Direct a0);
		      push(Displace(a0,0))));

     ("makeref", 1)
       ::= (fn () => (pop(Direct d0);
		      if !CGoptions.profile
		      then addl(Immed 1,Displace(a5,4*refcells))
		      else ();
		      movl(Direct d0,Displace(a6,0));
		      movl(Immed(1*power_tags+tag_array),Displace(a6,~4));
		      movl(Direct a6,Direct d0);
		      addl(Immed 8,Direct a6);
		      push(Direct d0)));

   ("*",2)
       ::= (fn () =>
		(pop(Direct d0);
		 asrl(Immed 1, Direct d0);
		 pop(Direct d1);
		 subl(Immed 1, Direct d1);
		 mull(Direct d1,Direct d0);
		 if !M68options.trapv then trapv() else ();
		 addl(Immed 1,Direct d0);
		 push(Direct d0) ));

    ("div", 2)
       ::= (fn () =>
		(pop(Direct d0);
		 asrl(Immed 1,Direct d0);
	         pop(Direct d1);
		 asrl(Immed 1,Direct d1);
		 divl(Direct d0,Direct d1);
		 addl(Direct d1,Direct d1);
		 addl(Immed 1,Direct d1);
		 push(Direct d1) ));

    ("+", 2)
       ::= (fn () =>
		(pop(Direct d0);
		 subl(Immed 1,Direct d0);
		 addl(PostInc sp,Direct d0);
		 if !M68options.trapv then trapv() else ();
		 push(Direct d0) ));

    ("-", 2)
       ::= (fn () =>
		(pop(Direct d0);
		 pop(Direct d1);
		 subl(Direct d0, Direct d1);
		 if !M68options.trapv then trapv() else ();
		 addl(Immed 1,Direct d1);
		 push(Direct d1) ));

    ("boxed",1) ::= (fn () =>
	     let val lab = newlabel()
	      in pop(Direct d1);
		 movl(Immed 1, Direct d0);
		 btst(Immed 0, Direct d1);
		 jne(Address lab);
		 movl(Immed 3, Direct d0);
		 define lab;
		 push(Direct d0)
	      end);

    let fun g(s,x) =
	 ((s,2) ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 x(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);
	  (s,2) :=:= (fn lab =>
	        (cmpl(PostInc sp,PostInc sp);
		 x(Address lab))))
     in g("ieql",jeq);
	g("ineq",jne);
	g(">",jgt);
        g(">=",jge);
        g("<",jlt);
	g("<=", jle)
    end;
	
    ("subscript", 2)
	::= (fn () =>
		(pop(Direct d0);   (* index *)
		 pop(Direct a0);   (* array *)
		 addl(Direct(d0),Direct(d0));
		 push(Index(a0, ~2, d0, Long)) ));

    ("update", 3)
	::= (fn () =>
	       (pop(Direct d0);    (* new value *)
		pop(Direct d1);    (* index *)
		pop(Direct a0);	    (* array *)
		subl(Immed 1, Direct d1);
	        addl(Direct d1, Direct d1);
		addl(Direct d1, Direct a0);
		movl(Direct d0, Displace(a0,0));
		markstore();
		push(Immed 1) ));

    ("unboxedupdate", 3)
	::= (fn () =>
	       (pop(Direct d0);    (* new value *)
		pop(Direct d1);    (* index *)
		pop(Direct a0);	    (* array *)
	        addl(Direct d1, Direct d1);
			 (* change this someday to use a scale of 2 *)
		movl(Direct d0, Index(a0,~2,d1,Long));
		push(Immed 1) ));

    ("alength", 1)
	::= (fn () =>
		(pop(Direct a0);
		 movl(Displace(a0,~4),Direct d0);
		 asrl(Immed width_tags,Direct d0);
		 addl(Direct d0,Direct d0);
		 addl(Immed 1, Direct d0);
		 push(Direct d0) ));

    ("slength", 1)
	::= (fn () =>
		let val lab = newlabel()
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
		    push(Direct d0)
		end );

    ("store", 3)
	::= (fn () =>
	       (pop(Direct d0);    (* new value *)
		pop(Direct d1);    (* index *)
		pop(Direct a0);	    (* byte array *)
		asrl(Immed 1, Direct d0);
		asrl(Immed 1, Direct d1);
		movb(Direct d0, Index(a0,0,d1,Long));
		push(Immed 1) ));

    ("ordof", 2) ::= (fn () =>
       (pop(Direct d1);    (* index *)
        pop(Direct a0);    (* string *)
	asrl(Immed 1,Direct d1);
	movl(Immed 0, Direct d0);
	movb(Index(a0,0,d1,Long),Direct d0);
	asll(Immed 1, Direct d0);
	addl(Immed 1, Direct d0);
	push(Direct d0)));

   ("cast", 1) ::= (fn () => ());

    ("fneg", 1)
       ::= (fn () =>
		(pop(Direct a0);
		 fnegd(Displace(a0,0),Direct fp0);
		 fmoved(Direct fp0,Displace(a6,0));
		 movl(realdesc,Displace(a6,~4));
		 push(Direct a6);
		 addl(Immed 12,Direct a6) ));
   
   let fun g(s,x) =
	 (s,2) ::= (fn () =>
		(pop(Direct a0);
		 pop(Direct a1);
		 fmoved(Displace(a1,0),Direct fp0);
		 x(Displace(a0,0),Direct fp0);
		 fmoved(Direct fp0,Displace(a6,0));
		 movl(realdesc,Displace(a6,~4));
		 push(Direct a6);
		 addl(Immed 12,Direct a6) ))
    in g("fmul",fmuld);
       g("fdiv",fdivd);
       g("fadd",faddd);
       g("fsub",fsubd)
   end;

   let fun g(s,x) =
	( (s,2) ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 x(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);
        (s, 2) :=:= (fn l =>
		(pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 x (Address l))))
      in g("feql",fjeq);
	 g("fneq",fjne);
	 g("fgt", fjlt);
	 g("flt", fjgt);
	 g("fge", fjle);
	 g("fle", fjge)
     end;

   ())

    nonfix ::=
end (* functor m68prim *)
