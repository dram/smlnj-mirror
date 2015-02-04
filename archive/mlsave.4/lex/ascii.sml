(* ascii.sml *)

structure Ascii = struct
    val	AMPERSAND	 = 38
    and ASTERISK	 = 42
    and ATSIGN		 = 64
    and BACKQUOTE	 = 96
    and BACKSLASH	 = 92
    and BANG		 = 33
    and BAR		 = 124
    and CARET		 = 94
    and COLON		 = 58
    and COMMA		 = 44
    and DEL		 = 127
    and DOLLAR		 = 36
    and DOT		 = 46
    and DQUOTE		 = 34
    and EQUAL		 = 61
    and FORMFEED	 = 12
    and GREATERTHAN	 = 62
    and LBRACE		 = 123
    and LBRACKET	 = 91
    and LC_A		 = 97
    and LC_N		 = 110
    and LC_T		 = 116
    and LC_Z		 = 122
    and LESSTHAN	 = 60
    and LPAREN		 = 40
    and MINUS		 = 45
    and NEWLINE		 = 10
    and NINE		 = 57
    and PERCENT		 = 37
    and PLUS		 = 43
    and QUERY		 = 63
    and RBRACE		 = 125
    and RBRACKET	 = 93
    and RETURN		 = 13
    and RPAREN		 = 41
    and SEMICOLON	 = 59
    and SHARP		 = 35
    and SLASH		 = 47
    and SPACE		 = 32
    and SQUOTE		 = 39
    and STAR		 = 42
    and TAB		 = 9
    and TILDE		 = 126
    and UC_A		 = 65
    and UC_Z		 = 90
    and UNDERSCORE	 = 95
    and ZERO		 = 48

    fun IsDigit (char) =
	char >= ZERO andalso char <= NINE;
    
    fun IsFormat (char) =
	char = NEWLINE orelse
	char = TAB orelse
	char = FORMFEED orelse
	char = SPACE;

   fun CharName (char) =
	if char < SPACE then "^"^chr(char+ATSIGN)
	else if char = DEL then "DEL"
	else if char = SPACE then "SPACE"
	else chr(char)

end;  (* Ascii *)
