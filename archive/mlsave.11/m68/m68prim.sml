signature MACHINEPRIM =
sig
      exception Notprim
      val canapp : int -> int
      val primapp : int -> unit
end

structure M68options = struct
    val trapv = ref true
end

functor M68prim (Coder : M68CODER) : MACHINEPRIM =
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
	  then addl(Immed 1,Displace(a5,4*reflists))
	  else ();
	  movl(Direct d6, Displace(a6,0));
	  movl(Direct a0, Displace(a6,~4));
	  movl(Direct a6, Direct d6);
	  lea(Displace(a6,8), Direct a6))

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
		      if !CGoptions.closurecount
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

    ("ieql", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 jeq(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("ineq", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 jne(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    (">", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 jgt(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    (">=", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 jge(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("<", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 jlt(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("<=", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 cmpl(PostInc sp,PostInc sp);
		 jle(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

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
		 lea(Displace(a6,12),Direct a6) ));
   
    ("fmul", 2)
       ::= (fn () =>
		(pop(Direct a0);
		 pop(Direct a1);
		 fmoved(Displace(a1,0),Direct fp0);
		 fmuld(Displace(a0,0),Direct fp0);
		 fmoved(Direct fp0,Displace(a6,0));
		 movl(realdesc,Displace(a6,~4));
		 push(Direct a6);
		 lea(Displace(a6,12),Direct a6) ));

    ("fdiv", 2)
       ::= (fn () =>
		(pop(Direct a0);
		 pop(Direct a1);
		 fmoved(Displace(a1,0),Direct fp0);
		 fdivd(Displace(a0,0),Direct fp0);
		 fmoved(Direct fp0,Displace(a6,0));
		 movl(realdesc,Displace(a6,~4));
		 push(Direct a6);
		 lea(Displace(a6,12),Direct a6) ));

    ("fadd", 2)
       ::= (fn () =>
		(pop(Direct a0);
		 pop(Direct a1);
		 fmoved(Displace(a1,0),Direct fp0);
		 faddd(Displace(a0,0),Direct fp0);
		 fmoved(Direct fp0,Displace(a6,0));
		 movl(realdesc,Displace(a6,~4));
		 push(Direct a6);
		 lea(Displace(a6,12),Direct a6) ));

    ("fsub", 2)
       ::= (fn () =>
		(pop(Direct a0);
		 pop(Direct a1);
		 fmoved(Displace(a1,0),Direct fp0);
		 fsubd(Displace(a0,0),Direct fp0);
		 fmoved(Direct fp0,Displace(a6,0));
		 movl(realdesc,Displace(a6,~4));
		 push(Direct a6);
		 lea(Displace(a6,12),Direct a6) ));

    ("feql", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 fjeq(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("fneq", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 fjne(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("fgt", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 fjlt(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("fge", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 fjle(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("flt", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 fjgt(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

    ("fle", 2)
       ::= (fn () =>
	     let val lab = newlabel()
	      in movl(Immed 3,Direct d0);
		 pop(Direct a0);
		 fmoved(Displace(a0,0),Direct fp0);
		 pop(Direct a0);
		 fcmpd(Displace(a0,0),Direct fp0);
		 fjge(Address lab);
		 movl(Immed 1,Direct d0);
		 define lab;
		 push(Direct d0)
	     end);

   ())

    nonfix ::=
end (* functor m68prim *)
