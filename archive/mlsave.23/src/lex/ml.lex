open ErrorMsg Symbols
type lexresult = Token.token
val eof = fn () => Token.EOF
val comLevel = ref 0
val charlist = ref (nil : string list)
fun addString (s:string) = charlist := s :: (!charlist)
fun makeInt (s : string) =
    (revfold (fn (c,a) => a*10 + ord c - Ascii.zero) (explode s) 0)
    handle Overflow => (complain "integer too large"; 0)

%% 
%s A S F;
idchars=[A-Za-z'_0-9];
id=[A-Za-z'_]{idchars}*;
qualid={id}".";
ws=[\t\ ]*;
sym=[!%&$+/:<=>?@~|#*`]|\\|\-|\^;
num=[0-9]+;
frac="."{num};
exp="E"(~?){num};
real=(~?)(({num}{frac}?{exp})|({num}{frac}{exp}?));
%%
<INITIAL>{ws}	=> (lex());
<INITIAL>\n	=> (inc lineNum; lex());
<INITIAL>"*"	=> (Token.ASTERISK);
<INITIAL>"|"	=> (Token.BAR);
<INITIAL>":"	=> (Token.COLON);
<INITIAL>"="	=> (Token.EQUAL);
<INITIAL>"_"	=> (Token.WILD);
<INITIAL>"?"	=> (Token.QUERY);
<INITIAL>"#"	=> (Token.HASH);
<INITIAL>","	=> (Token.COMMA);
<INITIAL>"{"	=> (Token.LBRACE);
<INITIAL>"}"	=> (Token.RBRACE);
<INITIAL>"["	=> (Token.LBRACKET);
<INITIAL>"]"	=> (Token.RBRACKET);
<INITIAL>";"	=> (Token.SEMICOLON);
<INITIAL>"("	=> (Token.LPAREN);
<INITIAL>")"	=> (Token.RPAREN);
<INITIAL>{qualid} =>
  (Token.IDDOT (stringToSymbol(substring(yytext,0,length(yytext)-1))));
<INITIAL>"..."	=> (Token.DOTDOTDOT);
<INITIAL>"'"{idchars}*	=> (Token.TYVAR(stringToSymbol yytext));
<INITIAL>({sym}+|{id})	=> (lexClass(stringToSymbol yytext));
<INITIAL>{real}	=> (Token.REAL yytext);
<INITIAL>{num}	=> (Token.INT(makeInt yytext));
<INITIAL>~{num}	=> (Token.INT(~(makeInt(substring(yytext,1,length(yytext)-1)))));
<INITIAL>\"	=> (charlist := nil; YYBEGIN S; lex());
<INITIAL>"(*"	=> (YYBEGIN A; inc comLevel; lex());
<INITIAL>.	=> (complain("illegal character"); lex());
<A>"(*"		=> (inc comLevel; lex());
<A>\n		=> (inc lineNum; lex());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); lex());
<A>.		=> (lex());
<S>"\""	        => (YYBEGIN INITIAL; Token.STRING(implode(rev(!charlist))));
<S>\n		=> (complain "unclosed string"; YYBEGIN INITIAL; Token.STRING "");
<S>[^"\\\n]*	=> (addString yytext; lex());
<S>\\\n	       	=> (inc lineNum; YYBEGIN F; lex());
<S>\\[\ \t]   	=> (YYBEGIN F; lex());
<F>\n		=> (inc lineNum; lex());
<F>{ws}		=> (lex());
<F>\\		=> (YYBEGIN S; lex());
<F>.		=> (complain "unclosed string"; YYBEGIN INITIAL; Token.STRING "");
<S>\\t		=> (addString "\t"; lex());
<S>\\n		=> (addString "\n"; lex());
<S>\\\\		=> (addString "\\"; lex());
<S>\\\"		=> (addString "\""; lex());
<S>\\\^[@-_]	=> (addString(chr(ordof(yytext,2)-ord("@"))); lex());
<S>\\[0-9]{3}	=>
 (let val x = ordof(yytext,1)*100
	     +ordof(yytext,2)*10
	     +ordof(yytext,3)
	     -(Ascii.zero*111)
  in (if x>255
      then complain ("illegal ascii escape '"^yytext^"'")
      else addString (chr x);
      lex())
  end);
<S>\\		=> (complain "illegal string escape"; lex());
