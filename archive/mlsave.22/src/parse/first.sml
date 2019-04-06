structure FirstSets = struct

structure Token = Token

local open Token Lex Basics EnvAccess in

val firstApat =
    fn ID s =>  lookFIX(s) = NONfix
     | OP => true
     | IDDOT _ => true
     | INT _ => true 
     | REAL _ => true
     | STRING _ => true
     | WILD => true
     | LPAREN => true
     | LBRACE => true
     | LBRACKET => true
     | _    => false

val firstAexp =
    fn ID s =>   lookFIX(s) = NONfix
     | OP => true
     | IDDOT _ => true
     | INT _ => true
     | REAL _ => true
     | STRING _ => true
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
     | FN => true
     | tok => firstAexp(tok)
     
val firstLdec = 
    fn VAL => true
     | FUN => true
     | TYPE => true
     | DATATYPE => true
     | ABSTYPE => true
     | EXCEPTION => true
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
     | Token.OPEN => true
     | LOCAL => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | STRUCTURE => true
     | ABSTRACTION => true
     | Token.FUNCTOR => true  (* monster structure hack *)
     | SIGNATURE => true      (* ditto *)
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
     | EQTYPE => true
     | DATATYPE => true
     | SHARING => true
     | VAL => true
     | EXCEPTION => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | INCLUDE => true
     | _    => false

end (* local *)

end (* FirstSets *)
