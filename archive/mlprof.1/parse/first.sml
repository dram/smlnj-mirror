(* first.sml  -- FIRST sets of selected nonterminals *)

structure FirstSets = struct

structure Token = Token

local
  open Token Lex Basics EnvAccess 
in

val firstApat =
    fn ID =>  lookFIX(!idValue) = NONfix
     | OP => true
     | IDDOT => true
     | INT => true 
     | REAL => true
     | STRING => true
     | WILD => true
     | LPAREN => true
     | LBRACE => true
     | LBRACKET => true
     | _    => false

val firstAexp =
    fn ID =>   lookFIX(!idValue) = NONfix
     | OP => true
     | IDDOT => true
     | INT => true
     | REAL => true
     | STRING => true
     | HASH => true
     | LPAREN => true
     | LBRACE => true
     | LBRACKET => true
     | LET => true
     | _    => false

val firstExp =
    fn IF => true
     | WHILE => true
     | CASE => true
     | RAISE => true
     | RAISEX => true
     | FN => true
     | tok => firstAexp(tok)
     
val firstLdec = 
    fn VAL => true
     | FUN => true
     | TYPE => true
     | DATATYPE => true
     | ABSTYPE => true
     | EXCEPTION => true
     | EXCEPTIONX => true
     | Token.OPEN => true
     | LOCAL => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | _    => false

val firstSdec = 
    fn VAL => true
     | FUN => true
     | TYPE => true
     | DATATYPE => true
     | ABSTYPE => true
     | EXCEPTION => true
     | EXCEPTIONX => true
     | Token.OPEN => true
     | LOCAL => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | STRUCTURE => true
     | ABSTRACTION => true
     | _    => false

val firstTdec = 
    fn SIGNATURE => true
     | STRUCTURE => true
     | ABSTRACTION => true
     | Token.FUNCTOR => true
     | _    => false

val firstSpec =
    fn STRUCTURE => true
     | TYPE => true
     | DATATYPE => true
     | SHARING => true
     | VAL => true
     | EXCEPTION => true
     | EXCEPTIONX => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | _    => false

end (* local *)

end (* FirstSets *)
