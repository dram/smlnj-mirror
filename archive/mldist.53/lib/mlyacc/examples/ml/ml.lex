structure Tokens = Tokens
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type pos = int
type lexresult = (svalue,pos) Tokens.token
type arg = unit	(* needed when using lexers created with %arg flag set in
		   ML-Lex *)

val lineNum = ref 0
val eof = fn () => Tokens.EOF(!lineNum,!lineNum)
open ErrorMsg
val comLevel = ref 0
val charlist = ref (nil : string list)
fun addString (s:string) = charlist := s :: (!charlist)
fun makeInt (s : string) =
    (revfold (fn (c,a) => a*10 + (ord c - Ascii.zero)) (explode s) 0)
    handle Overflow => (complain "integer too large"; 0)
val startToken = ref(!lineNum)
%% 
%arg (arg as _);
%header (functor MlLexFun(structure Tokens : Ml_TOKENS));
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
<INITIAL>{ws}	=> (lex() ());
<INITIAL>\n	=> (inc lineNum; lex() ());
<INITIAL>"*"	=> (Tokens.ASTERISK(!lineNum,!lineNum));
<INITIAL>"|"	=> (Tokens.BAR(!lineNum,!lineNum));
<INITIAL>":"	=> (Tokens.COLON(!lineNum,!lineNum));
<INITIAL>"="	=> (Tokens.EQUAL(!lineNum,!lineNum));
<INITIAL>"_"	=> (Tokens.WILD(!lineNum,!lineNum));
<INITIAL>"#"	=> (Tokens.HASH(!lineNum,!lineNum));
<INITIAL>","	=> (Tokens.COMMA(!lineNum,!lineNum));
<INITIAL>"{"	=> (Tokens.LBRACE(!lineNum,!lineNum));
<INITIAL>"}"	=> (Tokens.RBRACE(!lineNum,!lineNum));
<INITIAL>"["	=> (Tokens.LBRACKET(!lineNum,!lineNum));
<INITIAL>"]"	=> (Tokens.RBRACKET(!lineNum,!lineNum));
<INITIAL>";"	=> (Tokens.SEMICOLON(!lineNum,!lineNum));
<INITIAL>"("	=> (Tokens.LPAREN(!lineNum,!lineNum));
<INITIAL>")"	=> (Tokens.RPAREN(!lineNum,!lineNum));
<INITIAL>"and"	=> (Tokens.AND(!lineNum,!lineNum));
<INITIAL>"abstraction"	=> (Tokens.ABSTRACTION(!lineNum,!lineNum));
<INITIAL>"abstype"	=> (Tokens.ABSTYPE(!lineNum,!lineNum));
<INITIAL>"->"		=> (Tokens.ARROW(!lineNum,!lineNum));
<INITIAL>"as"		=> (Tokens.AS(!lineNum,!lineNum));
<INITIAL>"case"		=> (Tokens.CASE(!lineNum,!lineNum));
<INITIAL>"datatype"	=> (Tokens.DATATYPE(!lineNum,!lineNum));
<INITIAL>"..."		=> (Tokens.DOTDOTDOT(!lineNum,!lineNum));
<INITIAL>"else"		=> (Tokens.ELSE(!lineNum,!lineNum));
<INITIAL>"end"		=> (Tokens.END(!lineNum,!lineNum));
<INITIAL>"eqtype"	=> (Tokens.EQTYPE(!lineNum,!lineNum));
<INITIAL>"exception"	=> (Tokens.EXCEPTION(!lineNum,!lineNum));
<INITIAL>"do"		=> (Tokens.DO(!lineNum,!lineNum));
<INITIAL>"=>"		=> (Tokens.DARROW(!lineNum,!lineNum));
<INITIAL>"fn"		=> (Tokens.FN(!lineNum,!lineNum));
<INITIAL>"fun"		=> (Tokens.FUN(!lineNum,!lineNum));
<INITIAL>"functor"	=> (Tokens.FUNCTOR(!lineNum,!lineNum));
<INITIAL>"handle"	=> (Tokens.HANDLE(!lineNum,!lineNum));
<INITIAL>"if"		=> (Tokens.IF(!lineNum,!lineNum));
<INITIAL>"in"		=> (Tokens.IN(!lineNum,!lineNum));
<INITIAL>"include"	=> (Tokens.INCLUDE(!lineNum,!lineNum));
<INITIAL>"infix"	=> (Tokens.INFIX(!lineNum,!lineNum));
<INITIAL>"infixr"	=> (Tokens.INFIXR(!lineNum,!lineNum));
<INITIAL>"let"		=> (Tokens.LET(!lineNum,!lineNum));
<INITIAL>"local"	=> (Tokens.LOCAL(!lineNum,!lineNum));
<INITIAL>"nonfix"	=> (Tokens.NONFIX(!lineNum,!lineNum));
<INITIAL>"of"		=> (Tokens.OF(!lineNum,!lineNum));
<INITIAL>"op"		=> (Tokens.OP(!lineNum,!lineNum));
<INITIAL>"open"		=> (Tokens.OPEN(!lineNum,!lineNum));
<INITIAL>"overload"	=> (Tokens.OVERLOAD(!lineNum,!lineNum));
<INITIAL>"raise"	=> (Tokens.RAISE(!lineNum,!lineNum));
<INITIAL>"rec"		=> (Tokens.REC(!lineNum,!lineNum));
<INITIAL>"sharing"	=> (Tokens.SHARING(!lineNum,!lineNum));
<INITIAL>"sig"		=> (Tokens.SIG(!lineNum,!lineNum));
<INITIAL>"signature"	=> (Tokens.SIGNATURE(!lineNum,!lineNum));
<INITIAL>"struct"	=> (Tokens.STRUCT(!lineNum,!lineNum));
<INITIAL>"structure"	=> (Tokens.STRUCTURE(!lineNum,!lineNum));
<INITIAL>"then"		=> (Tokens.THEN(!lineNum,!lineNum));
<INITIAL>"type"		=> (Tokens.TYPE(!lineNum,!lineNum));
<INITIAL>"val"		=> (Tokens.VAL(!lineNum,!lineNum));
<INITIAL>"while"	=> (Tokens.WHILE(!lineNum,!lineNum));
<INITIAL>"with"		=> (Tokens.WITH(!lineNum,!lineNum));
<INITIAL>"withtype"	=> (Tokens.WITHTYPE(!lineNum,!lineNum));
<INITIAL>"orelse"	=> (Tokens.ORELSE(!lineNum,!lineNum));
<INITIAL>"andalso"	=> (Tokens.ANDALSO(!lineNum,!lineNum));
<INITIAL>"import"	=> (Tokens.IMPORT(!lineNum,!lineNum));
<INITIAL>{qualid} =>
  (Tokens.IDDOT (Symbol.symbol(substring(yytext,0,size(yytext)-1)),
		 !lineNum,!lineNum));
<INITIAL>"..."	=> (Tokens.DOTDOTDOT(!lineNum,!lineNum));
<INITIAL>"'"{idchars}*	=>(Tokens.TYVAR(Symbol.symbol yytext,!lineNum,!lineNum));
<INITIAL>({sym}+|{id})	=> (Tokens.ID(Symbol.symbol yytext,!lineNum,!lineNum));
<INITIAL>{real}	=> (Tokens.REAL (yytext,!lineNum,!lineNum));
<INITIAL>{num}	=> (Tokens.INT(makeInt yytext,!lineNum,!lineNum));
<INITIAL>~{num}	=> (Tokens.INT(~(makeInt(substring(yytext,1,size(yytext)-1))),
			      !lineNum,!lineNum));
<INITIAL>\"	=> (charlist := nil; YYBEGIN S;startToken := (!lineNum); lex() ());
<INITIAL>"(*"	=> (YYBEGIN A; inc comLevel; lex() ());
<INITIAL>.	=> (complain("illegal character"); lex() ());
<A>"(*"		=> (inc comLevel; lex() ());
<A>\n		=> (inc lineNum; lex() ());
<A>"*)" => (dec comLevel; if !comLevel=0 then YYBEGIN INITIAL else (); lex() ());
<A>.		=> (lex() ());
<S>"\""	        => (YYBEGIN INITIAL;
		    Tokens.STRING(implode(rev(!charlist)),!lineNum,!lineNum));
<S>\n		=> (complain "unclosed string"; YYBEGIN INITIAL;
		    Tokens.STRING("",!startToken,!lineNum));
<S>[^"\\\n]*	=> (addString yytext; lex() ());
<S>\\\n	       	=> (inc lineNum; YYBEGIN F; lex() ());
<S>\\[\ \t]   	=> (YYBEGIN F; lex() ());
<F>\n		=> (inc lineNum; lex() ());
<F>{ws}		=> (lex() ());
<F>\\		=> (YYBEGIN S; lex() ());
<F>.		=> (complain "unclosed string"; YYBEGIN INITIAL;
		   Tokens.STRING("",!startToken,!lineNum));
<S>\\t		=> (addString "\t"; lex() ());
<S>\\n		=> (addString "\n"; lex() ());
<S>\\\\		=> (addString "\\"; lex() ());
<S>\\\"		=> (addString "\""; lex() ());
<S>\\\^[@-_]	=> (addString(chr(ordof(yytext,2)-ord("@"))); lex() ());
<S>\\[0-9]{3}	=>
 (let val x = ordof(yytext,1)*100
	     +ordof(yytext,2)*10
	     +ordof(yytext,3)
	     -(Ascii.zero*111)
  in (if x>255
      then complain ("illegal ascii escape '"^yytext^"'")
      else addString (chr x);
      lex() ())
  end);
<S>\\		=> (complain "illegal string escape"; lex() ());
