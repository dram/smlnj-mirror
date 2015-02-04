(* token.sml *)

signature TOKEN = sig
    datatype token =
	   EOF | ID | IDDOT | TYVAR | INT | REAL | STRING
	 | AND | ARROW | AS | BAR | BARBAR | CASE | DATATYPE 
	 | DOTDOTDOT | ELSE | END | EQUAL | EXCEPTION | EXCEPTIONX
	 | DO | DARROW | FN | FUN | FUNCTOR | HANDLE | HANDLEX 
	 | IF | IN | INFIX | INFIXR | LET
	 | LOCAL | NONFIX | OF | OP | OPEN | OVERLOAD
         | QUERY | RAISE | RAISEX | REC
	 | SHARING | SIG | SIGNATURE | STRUCT | STRUCTURE
	 | THEN | TYPE | VAL | WHILE | WILD | WITH   
	 | ASTERISK | COLON | COMMA | LBRACE | LBRACKET | LPAREN
	 | RBRACE | RBRACKET | RPAREN | SEMICOLON
	 | ORELSE | ANDALSO

    val TokenName : token -> string
end;  (* TOKEN *)

structure Token : TOKEN = struct
    datatype token =
	   EOF | ID | IDDOT | TYVAR | INT | REAL | STRING
	 | AND | ARROW | AS | BAR | BARBAR | CASE | DATATYPE 
	 | DOTDOTDOT | ELSE | END | EQUAL | EXCEPTION | EXCEPTIONX
	 | DO | DARROW | FN | FUN | FUNCTOR | HANDLE | HANDLEX 
	 | IF | IN | INFIX | INFIXR | LET
	 | LOCAL | NONFIX | OF | OP | OPEN | OVERLOAD
         | QUERY | RAISE | RAISEX | REC
	 | SHARING | SIG | SIGNATURE | STRUCT | STRUCTURE
	 | THEN | TYPE | VAL | WHILE | WILD | WITH   
	 | ASTERISK | COLON | COMMA | LBRACE | LBRACKET | LPAREN
	 | RBRACE | RBRACKET | RPAREN | SEMICOLON
	 | ORELSE | ANDALSO;

   fun TokenName(tok: token) : string =
       case tok of
         EOF => "EOF"
       | ID => "ID"
       | IDDOT => "IDDOT"
       | TYVAR => "TYVAR"
       | INT => "INT"
       | REAL => "REAL"
       | STRING => "STRING"
       | AND => "AND"
       | ARROW => "ARROW"
       | AS => "AS"
       | BAR => "BAR"
       | BARBAR => "BARBAR"
       | CASE => "CASE"
       | DATATYPE => "DATATYPE"
       | DOTDOTDOT => "DOTDOTDOT"
       | ELSE => "ELSE"
       | END => "END"
       | EQUAL => "EQUAL"
       | EXCEPTION => "EXCEPTION"
       | EXCEPTIONX => "EXCEPTIONX"
       | DO => "DO"
       | DARROW => "DARROW"
       | FN => "FN"
       | FUN => "FUN"
       | FUNCTOR => "FUNCTOR"
       | HANDLE => "HANDLE"
       | HANDLEX => "HANDLEX"
       | IF => "IF"
       | IN => "IN"
       | INFIX => "INFIX"
       | INFIXR => "INFIXR"
       | LET => "LET"
       | LOCAL => "LOCAL"
       | NONFIX => "NONFIX"
       | OF => "OF"
       | OP => "OP"
       | OPEN => "OPEN"
       | OVERLOAD => "OVERLOAD"
       | QUERY => "QUERY"
       | RAISE => "RAISE"
       | RAISEX => "RAISEX"
       | REC => "REC"
       | SHARING => "SHARING"
       | SIG => "SIG"
       | SIGNATURE => "SIGNATURE"
       | STRUCT => "STRUCT"
       | STRUCTURE => "STRUCTURE"
       | THEN => "THEN"
       | TYPE => "TYPE"
       | VAL => "VAL"
       | WHILE => "WHILE"
       | WILD => "WILD"
       | WITH => "WITH"
       | ASTERISK => "ASTERISK"
       | COLON => "COLON"
       | COMMA => "COMMA"
       | LBRACE => "LBRACE"
       | LBRACKET => "LBRACKET"
       | LPAREN => "LPAREN"
       | RBRACE => "RBRACE"
       | RBRACKET => "RBRACKET"
       | RPAREN => "RPAREN"
       | SEMICOLON => "SEMICOLON"
       | ORELSE => "ORELSE"
       | ANDALSO => "ANDALSO"

end;  (* Token *)

