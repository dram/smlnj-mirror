type pos = int
type svalue = Tok.svalue
type ('a,'b) token = ('a,'b) Tok.token
type lexresult= (svalue,pos) token

open Tok

val eof = fn () => EOF(~1,~1)
val error = (* fn (e,l : int,_) =>
      output(std_out,"line " ^ (makestring l) ^
	     ": " ^ e ^ "\n") *)
     fn _ => ()

local
val text = ref ([] : string list)
in
fun clrAction () = (text := ["("])
fun updAction (str) = (text := str :: (!text))
fun getAction () = (concat (rev (!text)))
end

(* what to do (i.e. switch start states) after recognizing an action *)
val afterAction = ref (fn () => ())

(* paren counting for actions *)
val pcount = ref 0
val inquote = ref false
fun inc r = if !inquote then () else r := !r + 1
fun dec r = if !inquote then () else r := !r - 1

structure SIS = RegExp.SymSet
val wildcard = SIS.complement (SIS.singleton 0w10) (* everything but \n *)
fun oneChar s = SIS.singleton 
      (Word32.fromInt (Char.ord (String.sub (s, 0))))
val highAscii = SIS.interval(0w128, 0w255)

%%

%header (functor MLLexLexFun(structure Tok: MLLex_TOKENS));
%s DEFS RE RECB CHARCLASS LEXSTATES ACTION STRING;
%count

ws	= [\ \n\t\013];
alpha	= [a-zA-Z];
num	= [0-9];
id	= {alpha}({alpha} | {num} | "_" | "'")*;

%%

<INITIAL> "%%"	=> (YYBEGIN DEFS; LEXMARK(yylineno, yylineno));
<INITIAL> ([^%] | [^%]* % [^%])*
		=> (DECLS(yytext, yylineno, yylineno));

<DEFS> {ws}	=> (lex());
<DEFS> "%%"	=> (YYBEGIN RE; LEXMARK(yylineno, yylineno));
<DEFS> "%s"	=> (YYBEGIN LEXSTATES; STATES(yylineno, yylineno));
<DEFS> "%header" {ws}* "("
		=> (clrAction(); pcount := 1; inquote := false; 
	            YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    HEADER(yylineno, yylineno));
<DEFS> "%structure"
		=> (STRUCT(yylineno, yylineno));
<DEFS> "%arg" {ws}* "("
		=> (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN DEFS);
		    ARG(yylineno, yylineno));
<DEFS> "%count" => (COUNT(yylineno, yylineno));
<DEFS> "%reject"=> (REJECTTOK(yylineno, yylineno));
<DEFS> {id}	=> (ID(yytext, yylineno, yylineno));
<DEFS> "="	=> (YYBEGIN RE; EQ(yylineno, yylineno));

<RE> {ws}	=> (lex());
<RE> "?"	=> (QMARK(yylineno, yylineno));
<RE> "*"	=> (STAR(yylineno, yylineno));
<RE> "+"	=> (PLUS(yylineno, yylineno));
<RE> "|"	=> (BAR(yylineno, yylineno));
<RE> "("	=> (LP(yylineno, yylineno));
<RE> ")"	=> (RP(yylineno, yylineno));
<RE> "$"	=> (DOLLAR(yylineno, yylineno));
<RE> "/"	=> (SLASH(yylineno, yylineno));
<RE> "."	=> (CHARS(wildcard, yylineno, yylineno));

<RE> "{"	=> (YYBEGIN RECB; lex());
<RE> "\""       => (YYBEGIN STRING; lex());
<RE> "["	=> (YYBEGIN CHARCLASS; LB(yylineno, yylineno));
<RE> "<"	=> (YYBEGIN LEXSTATES; LT(yylineno, yylineno));
<RE> ">"	=> (GT(yylineno, yylineno));
<RE> "=>" {ws}*	"("
		=> (clrAction(); pcount := 1; inquote := false;
                    YYBEGIN ACTION;
		    afterAction := (fn () => YYBEGIN RE);
		    ARROW(yylineno, yylineno));
<RE> ";"	=> (YYBEGIN DEFS; SEMI(yylineno, yylineno));

<RE,STRING>"\\\\"
		=> (CHARS(oneChar "\\", yylineno, yylineno));
<RE,STRING>"\\b"=> (CHARS(oneChar "\b", yylineno, yylineno));
<RE,STRING>"\\t"=> (CHARS(oneChar "\t", yylineno, yylineno));
<RE,STRING>"\\n"=> (CHARS(oneChar "\n", yylineno, yylineno));
<RE,STRING>"\\r"=> (CHARS(oneChar "\r", yylineno, yylineno));
<RE,STRING>"\\h"=> (CHARS(highAscii, yylineno, yylineno));
<RE,STRING> "\\\"" 
		=> (CHARS (oneChar "\"", yylineno, yylineno));
<RE,STRING>"\\"{num}{3}
		=> (CHARS(oneChar (valOf (String.fromString yytext)), 
		    yylineno, yylineno));
<RE,STRING>"\\".=> (CHARS(oneChar (String.substring (yytext, 1, 1)), 
		    yylineno, yylineno));

<RE> .          => (CHARS(oneChar yytext, yylineno, yylineno));

<RECB>{ws}	=> (lex());
<RECB>{id}	=> (ID(yytext, yylineno, yylineno));
<RECB>{num}+	=> (REPS(valOf (Int.fromString yytext), yylineno, yylineno));
<RECB>","	=> (COMMA(yylineno, yylineno));
<RECB>"}"	=> (YYBEGIN RE; RCB(yylineno, yylineno));

<CHARCLASS>"-]"	=> (YYBEGIN RE; RBD(yylineno, yylineno));
<CHARCLASS>"]"	=> (YYBEGIN RE; RB(yylineno, yylineno));
<CHARCLASS>"-"	=> (DASH(yylineno, yylineno));
<CHARCLASS>"^"	=> (CARAT(yylineno, yylineno));
<CHARCLASS>"\\b"=> (CHAR("\b", yylineno, yylineno));
<CHARCLASS>"\\t"=> (CHAR("\t", yylineno, yylineno));
<CHARCLASS>"\\n"=> (CHAR("\n", yylineno, yylineno));
<CHARCLASS>"\\r"=> (CHAR("\r", yylineno, yylineno));
<CHARCLASS>"\\"{num}{3}
		=> (CHAR(valOf (String.fromString yytext), yylineno, yylineno));
<CHARCLASS>"\\".=> (CHAR(String.substring (yytext, 1, 1), yylineno, yylineno));
<CHARCLASS>.	=> (CHAR(yytext, yylineno, yylineno));

<LEXSTATES>{id} => (LEXSTATE(yytext, yylineno, yylineno));
<LEXSTATES>{ws}	=> (lex());
<LEXSTATES> "," => (COMMA(yylineno, yylineno));
<LEXSTATES> ">" => (YYBEGIN RE; GT(yylineno, yylineno));
<LEXSTATES> ";" => (YYBEGIN DEFS; SEMI(yylineno, yylineno));

<STRING> "\""	=> (YYBEGIN RE; lex());
<STRING> .	=> (CHARS (oneChar yytext, yylineno, yylineno));

<ACTION> ";"	=> (if !pcount = 0
		    then ((!afterAction)();
			  ACT(getAction(), yylineno, yylineno))
		    else (updAction ";"; lex()));
<ACTION> "("	=> (updAction "("; inc pcount; lex());
<ACTION> ")"	=> (updAction ")"; dec pcount; lex());
<ACTION> "\\\"" => (updAction "\\\""; lex());
<ACTION> "\\\\"	=> (updAction "\\\\"; lex());
<ACTION> "\\"	=> (updAction "\\"; lex());
<ACTION> "\""   => (updAction "\""; inquote := not (!inquote); lex());
<ACTION> [^;()\"\\]*
		=> (updAction yytext; lex());
