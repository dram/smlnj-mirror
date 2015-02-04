(* first.sml  -- FIRST sets of selected nonterminals *)

structure FirstSets = struct

structure Token = Token;

local
  open Token Lex Basics EnvAccess 
in

fun FirstApat() = case !NextToken
    of ID =>  lookFIX(!IdValue) = NONfix
     | OP => true
     | IDDOT => true
     | INT => true 
     | REAL => true
     | STRING => true
     | WILD => true
     | LPAREN => true
     | LBRACE => true
     | LBRACKET => true
     | _    => false;

fun FirstAexp() = case !NextToken
    of ID =>   lookFIX(!IdValue) = NONfix
     | OP => true
     | IDDOT => true
     | INT => true
     | REAL => true
     | STRING => true
     | LPAREN => true
     | LBRACE => true
     | LBRACKET => true
     | LET => true
     | _    => false;

fun FirstExp() = case !NextToken
    of IF => true
     | WHILE => true
     | CASE => true
     | RAISE => true
     | RAISEX => true
     | FN => true
     | _ => FirstAexp()
     
val FirstLdec = 
    fn VAL => true
     | FUN => true
     | TYPE => true
     | DATATYPE => true
     | EXCEPTION => true
     | EXCEPTIONX => true
     | OPEN => true
     | LOCAL => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | _    => false;

val FirstSdec = 
    fn VAL => true
     | FUN => true
     | TYPE => true
     | DATATYPE => true
     | STRUCTURE => true
     | EXCEPTION => true
     | EXCEPTIONX => true
     | OPEN => true
     | LOCAL => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | _    => false;

val FirstTdec = 
    fn SIGNATURE => true
     | STRUCTURE => true
     | Token.FUNCTOR => true
     | _    => false;

val FirstSpec =
    fn STRUCTURE => true
     | TYPE => true
     | DATATYPE => true
     | VAL => true
     | EXCEPTION => true
     | EXCEPTIONX => true
     | INFIX => true
     | INFIXR => true
     | NONFIX => true
     | OVERLOAD => true
     | _    => false;

end (* local *)

end; (* FirstSets *)

