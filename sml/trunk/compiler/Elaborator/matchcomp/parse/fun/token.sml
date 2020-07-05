(* token.sml *)

structure Token =
struct

  datatype token
    = ID of string
    | NAT of int
    | NEG
    | PLUS | MINUS       (* additive operators (+, -) *)
    | TIMES | DIV | MOD  (* multiplicative operators ( *, /, %) *)
    | EQUAL | NOTEQUAL | LESS | GREATER | LESSEQ | GREATEREQ
        (* relational operators *)
    | TRUE | FALSE       (* boolean constants *)
    | AND | OR | NOT     (* boolean operators *)
    | FN | TFN | DARROW        (* function expression keywords *)
    | LET | LETREC | IN | EQ | VAL | FUN  (* declaration keywords and punctuation *)
    | IF | THEN | ELSE   (* conditional expr keywords *)
    | CASE | OF | BAR | INL | INR  (* case expressions *)
    | FOLD | UNFOLD      (* recursive type isomorphisms *)
    | INT | BOOL | TARROW | REC   (* type expressions *)
    | LPAR | RPAR        (* left and right parentheses *)
    | LBRACKET | RBRACKET     (* left and right brackets *)
    | COMMA | COLON | SEMI
    | QUIT

end  (* structure Token *)
