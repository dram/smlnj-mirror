(* Copyright 1989 by AT&T Bell Laboratories *)
(* primop.sig *)

signature PRIMOP =
sig
  datatype primop = 
      ! | * | + | - | := | < | <= | > | >= | lessu | gequ | alength | 
      boxed | unboxed | div | cast |
      eql | fadd |fdiv |feql |fge |fgt |fle |flt |fmul |fneq |fsub | gethdlr |
      ieql | ineq | neq | makeref | ordof | profile |
      sethdlr | slength | capture | callcc | throw | delay | force |
      store | subscript | unboxedassign | unboxedupdate | update | ~ |
      inlsubscript | inlupdate | inlbyteof | inlstore | inlordof |
      floor | round | real | subscriptf | updatef |
      rshift | lshift | orb | andb | xorb | notb |
      getvar | setvar | uselvar | deflvar

  val pr_primop: primop -> string

end
