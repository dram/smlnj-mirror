(* t6b.sml  -- based on yacc.grm.sml *)
(* 1st 2 (+1) rules of t4.sml, 4 sv constructors. ruleCounts: [1,5,10] *)
(* dropped C0 constant constructor, which does not occur *)

datatype d = A of int | B of int

fun f x =
    case x  (* "state" (0,1) needed as first component of pair *)
      of (0, A u :: _ :: nil) => u
	 (* A needed, but cannot be C, 2nd :: needed *)
       | (1, B v :: nil) => v
	 (* B needed, but cannot be A *)

(*
andor:
<> AND 9 {0,1} {}
   <0> OR 0 {0,1} {}
      I0 LEAF {0} {}
      I1 LEAF {1} {}
   <1> OR 1 {0,1} {}
      ::
      <1.::> AND 8 {0,1} {}
         <1.::.0> OR 2 {0,1} {}
            A <1.::.0.A> VARS 3 (u,0) {0}
            B <1.::.0.B> VARS 10 (v,1) {1}
         <1.::.1> OR 4 {0,1} {}
            ::
            <1.::.1.::> AND 7 {0} {}
               <1.::.1.::.0> VARS 5 (%WILD%,0) {0}
               <1.::.1.::.1> OR 6 {0} {}
                  nil LEAF {0} {}
            nil LEAF {1} {}

dectree:
CHOICE 1 <1>
   :: CHOICE 4 <1.::.1>
      :: CHOICE 6 <1.::.1.::.1>
         nil CHOICE 0 <0>  -- length list = 2 
            I0 CHOICE 2 <1.::.0>
               A DLEAF 0  -- 1.hd is A u;  binds u
               B MATCH  -- 1.hd must be A *; does NOT bind v
            I1 MATCH  -- list length = 2 > 1
            * MATCH  -- 1 <> 0 or 1
         * MATCH  -- list length > 2
      nil DLEAF 1  -- 1.tl = nil; should bind v (no CHOICE on B here!)
   * MATCH

*)
