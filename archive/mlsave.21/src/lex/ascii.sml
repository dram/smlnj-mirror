(* ascii.sml *)

structure Ascii = struct
    val	ampersand	 = 38
    and asterisk	 = 42
    and atsign		 = 64
    and backquote	 = 96
    and backslash	 = 92
    and bang		 = 33
    and bar		 = 124
    and caret		 = 94
    and colon		 = 58
    and comma		 = 44
    and del		 = 127
    and dollar		 = 36
    and dot		 = 46
    and dquote		 = 34
    and equal		 = 61
    and formfeed	 = 12
    and greaterthan	 = 62
    and lbrace		 = 123
    and lbracket	 = 91
    and lc_a		 = 97
    and lc_n		 = 110
    and lc_t		 = 116
    and lc_z		 = 122
    and lessthan	 = 60
    and lparen		 = 40
    and minus		 = 45
    and newline		 = 10
    and nine		 = 57
    and percent		 = 37
    and plus		 = 43
    and query		 = 63
    and rbrace		 = 125
    and rbracket	 = 93
    and return		 = 13
    and rparen		 = 41
    and SEMIcolon	 = 59
    and sharp		 = 35
    and slash		 = 47
    and space		 = 32
    and squote		 = 39
    and star		 = 42
    and tab		 = 9
    and tilde		 = 126
    and uc_a		 = 65
    and uc_z		 = 90
    and underscore	 = 95
    and zero		 = 48

    fun isDigit (char) =
	char >= zero andalso char <= nine;
    
    fun isFormat (char) =
	char = newline orelse
	char = tab orelse
	char = formfeed orelse
	char = space;

   fun charName (char) =
	if char < space then "^"^chr(char+atsign)
	else if char = del then "del"
	else if char = space then "space"
	else chr(char)

end  (* structure Ascii *)
