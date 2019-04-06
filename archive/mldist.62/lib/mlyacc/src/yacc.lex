(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi

   yacc.lex: Lexer specification
 *)

structure Tokens = Tokens
val lineno = Header.lineno

type ('a,'b) token = ('a,'b) Tokens.token
type svalue = Tokens.svalue
type pos = int
type lexresult = (svalue,pos) token

open Tokens
val error = Header.error

val pcount = ref 0;
val commentLevel = ref 0
val actionstart = ref 0
val eof = fn () => (if (!pcount)>0 then
			 error " eof encountered in action beginning here !"
				(!actionstart)
		      else (); EOF(!lineno,!lineno))

val text = ref (nil : string list)
val Add = fn s => (text := s::(!text))

val lookup =
   let val dict = [("%prec",PREC_TAG),("%term",TERM),
		  ("%nonterm",NONTERM), ("%eop",PERCENT_EOP),("%start",START),
		  ("%prefer",PREFER),("%subst",SUBST),
		  ("%keyword",KEYWORD),("%name",NAME),
		  ("%verbose",VERBOSE), ("%nodefault",NODEFAULT),
		  ("%value",VALUE), ("%noshift",NOSHIFT),
		  ("%header",PERCENT_HEADER),("%pure",PERCENT_PURE),
		  ("%arg",PERCENT_ARG),
		  ("%pos",PERCENT_POS)]
   in fn (s,left,right) =>
	    let fun f ((a,d)::b) = if a=s then d(left,right) else f b
	 	  | f nil = UNKNOWN(s,left,right)
	    in f dict
    	    end
   end
%%
%header (functor LexMLYACC(structure Tokens : Mlyacc_TOKENS));
%s A CODE F COMMENT STRING EMPTYCOMMENT;
ws = [\t\ ]+;
idchars = [A-Za-z_'0-9];
id=[A-Za-z]{idchars}*;
tyvar="'"{idchars}*;
qualid ={id}".";
%%
<INITIAL>"(*"	=> (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    lex() before YYBEGIN INITIAL);
<A>"(*"		=> (YYBEGIN EMPTYCOMMENT; commentLevel := 1; lex());
<CODE>"(*"	=> (Add yytext; YYBEGIN COMMENT; commentLevel := 1;
		    lex() before YYBEGIN CODE);
<INITIAL>[^%\n]+ => (Add yytext; lex());
<INITIAL>"%%"	 => (YYBEGIN A; HEADER (implode (rev (!text)),!lineno,!lineno));
<INITIAL,CODE,COMMENT,F,EMPTYCOMMENT>\n  => (Add yytext; inc lineno; lex());
<INITIAL>.	 => (Add yytext; lex());

<A>\n		=> (inc lineno; lex ());
<A>{ws}+	=> (lex());
<A>of		=> (OF(!lineno,!lineno));
<A>for		=> (FOR(!lineno,!lineno));
<A>"="		=> (EQUAL(!lineno,!lineno));
<A>"{"		=> (LBRACE(!lineno,!lineno));
<A>"}"		=> (RBRACE(!lineno,!lineno));
<A>","		=> (COMMA(!lineno,!lineno));
<A>"*"		=> (ASTERISK(!lineno,!lineno));
<A>"->"		=> (ARROW(!lineno,!lineno));
<A>"%left"	=> (PREC(Header.LEFT,!lineno,!lineno));
<A>"%right"	=> (PREC(Header.RIGHT,!lineno,!lineno));
<A>"%nonassoc" 	=> (PREC(Header.NONASSOC,!lineno,!lineno));
<A>"%"[a-z_]+	=> (lookup(yytext,!lineno,!lineno));
<A>{tyvar}	=> (TYVAR(yytext,!lineno,!lineno));
<A>{qualid}	=> (IDDOT(yytext,!lineno,!lineno));
<A>[0-9]+	=> (INT (yytext,!lineno,!lineno));
<A>"%%"		=> (DELIMITER(!lineno,!lineno));
<A>":"		=> (COLON(!lineno,!lineno));
<A>"|"		=> (BAR(!lineno,!lineno));
<A>{id}		=> (ID ((yytext,!lineno),!lineno,!lineno));
<A>"("		=> (pcount := 1; actionstart := (!lineno);
		    text := nil; YYBEGIN CODE; lex() before YYBEGIN A);
<A>.		=> (UNKNOWN(yytext,!lineno,!lineno));
<CODE>"("	=> (inc pcount; Add yytext; lex());
<CODE>")"	=> (dec pcount;
		    if !pcount = 0 then
			 PROG (implode (rev (!text)),!lineno,!lineno)
		    else (Add yytext; lex()));
<CODE>"\""	=> (Add yytext; YYBEGIN STRING; lex());
<CODE>[^()"\n]+	=> (Add yytext; lex());

<COMMENT>[(*)]	=> (Add yytext; lex());
<COMMENT>"*)"	=> (Add yytext; dec commentLevel;
		    if !commentLevel=0
			 then BOGUS_VALUE(!lineno,!lineno)
			 else lex()
		   );
<COMMENT>"(*"	=> (Add yytext; inc commentLevel; lex());
<COMMENT>[^*()\n]+ => (Add yytext; lex());

<EMPTYCOMMENT>[(*)]  => (lex());
<EMPTYCOMMENT>"*)"   => (dec commentLevel;
		          if !commentLevel=0 then YYBEGIN A else ();
			  lex ());
<EMPTYCOMMENT>"(*"   => (inc commentLevel; lex());
<EMPTYCOMMENT>[^*()\n]+ => (lex());

<STRING>"\""	=> (Add yytext; YYBEGIN CODE; lex());
<STRING>\\	=> (Add yytext; lex());
<STRING>\n	=> (Add yytext; inc lineno; error "unclosed string" (!lineno);
		    YYBEGIN CODE; lex());
<STRING>[^"\\\n]+ => (Add yytext; lex());
<STRING>\\\"	=> (Add yytext; lex());
<STRING>\\[\ \t\n]   => (Add yytext;
			if substring(yytext,1,1)="\n" then inc lineno else ();
		     	YYBEGIN F; lex());

<F>{ws}		=> (Add yytext; lex());
<F>\\		=> (Add yytext; YYBEGIN STRING; lex());
<F>.		=> (Add yytext; error "unclosed string" (!lineno);
		    YYBEGIN CODE; lex());
%%
