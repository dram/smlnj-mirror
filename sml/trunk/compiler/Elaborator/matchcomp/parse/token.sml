(* token.sml *)

structure Token =
struct

  datatype token
    = ID of string
    | NAT of string
    | BAR                (* bar | *)
    | LPAR | RPAR        (* left and right parentheses *)
    | COMMA 
    | SEMI
	       
end  (* structure Token *)
