(* -*- sml -*- *)

type svalue = Tokens.svalue
type pos = int

type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

type lexarg = {
	       enterC: unit -> unit,
	       leaveC: unit -> bool,
	       newS: pos -> unit,
	       addS: char -> unit,
	       addSC: string * int -> unit,
	       addSN: string * pos -> unit,
	       getS: pos -> lexresult,
	       handleEof: unit -> lexresult,
	       newline: pos -> unit,
	       error: pos -> string -> unit
	      }

type arg = lexarg
	       
fun eof (arg: lexarg) = (#handleEof arg ())

(*
local
    val depth = ref 0
    val curstring = ref ([]: char list)
    val startpos = ref 0
    val instring = ref false
in


    fun resetAll () = (depth := 0; startpos := 0; instring := false)

    (* comment stuff *)
    fun enterC () = depth := !depth + 1
    fun leaveC () = let
	val d = !depth - 1
	val _ = depth := d
    in
	d = 0
    end

    (* string stuff *)
    fun newS sp = (curstring := []; startpos := sp; instring := true)
    fun addS c = curstring := c :: (!curstring)
    fun addSC (t, p, b) = addS (chr (ord (String.sub (t, 2)) - b))
    fun addSN (t, p) = let
	val ns = substring (t, 1, 3)
	val n = Int.fromString ns
    in
	addS (chr (valOf n))
	handle _ =>
	    ErrorMsg.error p ("illegal decimal char spec " ^ ns)
    end
    fun getS endpos =
	(instring := false;
	 Tokens.STRING (implode (rev (!curstring)), !startpos, endpos + 1))

    (* handling EOF *)
    fun eof (arg: ) = let
	val pos = ErrorMsg.lastLinePos ()
    in
	if !depth > 0 then
	    ErrorMsg.error pos "unexpected EOF in COMMENT"
	else if !instring then
	    ErrorMsg.error pos "unexpected EOF in STRING"
	else ();
	resetAll ();
	Tokens.EOF(pos,pos)
    end
end
*)

local
    val idlist = [("Alias", Tokens.ALIAS),
		  ("Group", Tokens.GROUP),
		  ("Library", Tokens.LIBRARY),
		  ("is", Tokens.IS),
		  ("structure", Tokens.STRUCTURE),
		  ("signature", Tokens.SIGNATURE),
		  ("functor", Tokens.FUNCTOR),
		  ("funsig", Tokens.FUNSIG),
		  ("defined", Tokens.DEFINED),
		  ("div", Tokens.DIV),
		  ("mod", Tokens.MOD),
		  ("andalso", Tokens.ANDALSO),
		  ("orelse", Tokens.ORELSE),
		  ("not", Tokens.NOT)]
in
    fun idToken (t, p) =
	case List.find (fn (id, _) => id = t) idlist of
	    NONE => Tokens.FILE_STANDARD (t, p, p + size t)
	  | SOME (_, tok) => tok (p, p + size t)
end

(* states:

     INITIAL -> C
       |
       +------> P -> PC
       |        |
       |        +--> PM -> PMC
       |
       +------> M -> MC
       |
       +------> S -> SS

   "C"  -- COMMENT
   "P"  -- PREPROC
   "M"  -- MLSYMBOL
   "S"  -- STRING
   "SS" -- STRINGSKIP
*)

%%

%s C P PC PM PMC M MC S SS;

%header(functor CMLexFun (structure Tokens: CM_TOKENS));

%arg ({ enterC, leaveC,
        newS, addS, addSC, addSN, getS,
        handleEof,
        newline,
	error });

idchars=[A-Za-z'_0-9];
id=[A-Za-z]{idchars}*;
cmextrachars=[!%&$+/<=>?@~|#*]|\-|\^;
cmidchars={idchars}|{cmextrachars};
cmid={cmextrachars}+;
ws=("\012"|[\t\ ]);
eol=("\013\010"|"\013"|"\010");
sym=[!%&$+/:<=>?@~|#*]|\-|\^|"\\";
digit=[0-9];
sharp="#";
%%

<INITIAL>"(*"           => (enterC (); YYBEGIN C; continue ());
<P>"(*"                 => (enterC (); YYBEGIN PC; continue ());
<PM>"(*"                => (enterC (); YYBEGIN PMC; continue ());
<M>"(*"                 => (enterC (); YYBEGIN MC; continue ());

<C,PC,PMC,MC>"(*"       => (enterC (); continue ());

<C>"*)"                 => (if leaveC () then YYBEGIN INITIAL else ();
			    continue ());
<PC>"*)"                => (if leaveC () then YYBEGIN P else ();
			    continue ());
<PMC>"*)"                => (if leaveC () then YYBEGIN PM else ();
			    continue ());
<MC>"*)"                => (if leaveC () then YYBEGIN M else ();
			    continue ());
<C,PC,PMC,MC>{eol}      => (newline yypos; continue ());
<C,PC,PMC,MC>.          => (continue ());

<INITIAL,P,PM,M>"*)"	=> (error yypos "unmatched comment delimiter";
			    continue ());

<INITIAL>"\""		=> (YYBEGIN S; newS yypos; continue ());

<S>"\\a"		=> (addS #"\a"; continue ());
<S>"\\b"		=> (addS #"\b"; continue ());
<S>"\\f"		=> (addS #"\f"; continue ());
<S>"\\n"		=> (addS #"\n"; continue ());
<S>"\\r"		=> (addS #"\r"; continue ());
<S>"\\t"		=> (addS #"\t"; continue ());
<S>"\\v"		=> (addS #"\v"; continue ());

<S>"\\^"@		=> (addS (chr 0); continue ());
<S>"\\^"[a-z]	        => (addSC (yytext, ord #"a"); continue ());
<S>"\\^"[A-Z]	        => (addSC (yytext, ord #"A"); continue ());
<S>"\\^["		=> (addS (chr 27); continue ());
<S>"\\^\\"		=> (addS (chr 28); continue ());
<S>"\\^]"		=> (addS (chr 29); continue ());
<S>"\\^^"		=> (addS (chr 30); continue ());
<S>"\\^_"		=> (addS (chr 31); continue ());

<S>"\\"[0-9][0-9][0-9]	=> (addSN (yytext, yypos); continue ());

<S>"\\\""		=> (addS #"\""; continue ());
<S>"\\\\"		=> (addS #"\\"; continue ());

<S>"\\"{eol}	        => (YYBEGIN SS; newline (yypos + 1); continue ());
<S>"\\"{ws}+	        => (YYBEGIN SS; continue ());

<S>"\\".		=> (error yypos
			     ("illegal escape character in string " ^ yytext);
			    continue ());

<S>"\""		        => (YYBEGIN INITIAL; getS yypos);
<S>{eol}		=> (newline yypos;
			    error yypos "illegal linebreak in string";
			    continue ());

<S>.		        => (addS (String.sub (yytext, 0)); continue ());

<SS>{eol}	        => (newline yypos; continue ());
<SS>{ws}+	        => (continue ());
<SS>"\\"	        => (YYBEGIN S; continue ());
<SS>.		        => (error yypos
			     ("illegal character in stringskip " ^ yytext);
			    continue ());

<INITIAL,P>"("		=> (Tokens.LPAREN (yypos, yypos + 1));
<INITIAL,P>")"		=> (Tokens.RPAREN (yypos, yypos + 1));
<INITIAL>":"		=> (Tokens.COLON (yypos, yypos + 1));
<P>"+"		        => (Tokens.PLUS (yypos, yypos + 1));
<P>"-"		        => (Tokens.MINUS (yypos, yypos + 1));
<P>"*"		        => (Tokens.TIMES (yypos, yypos + 1));
<P>"<>"		        => (Tokens.NE (yypos, yypos + 2));
<P>"<="		        => (Tokens.LE (yypos, yypos + 2));
<P>"<"		        => (Tokens.LT (yypos, yypos + 1));
<P>">="		        => (Tokens.GE (yypos, yypos + 2));
<P>">"		        => (Tokens.GT (yypos, yypos + 1));
<P>"="		        => (Tokens.EQ (yypos, yypos + 1));
<P>"~"		        => (Tokens.TILDE (yypos, yypos + 1));

<P>{digit}+	        => (Tokens.NUMBER
			     (valOf (Int.fromString yytext)
			      handle _ =>
				  (error yypos "number too large"; 0),
			      yypos, yypos + size yytext));

<P>{id}                 => (Tokens.CM_ID (yytext, yypos, yypos + size yytext));

<M>({id}|{sym}+)        => (YYBEGIN INITIAL;
			    Tokens.ML_ID (yytext, yypos, yypos + size yytext));
<PM>({id}|{sym}+)       => (YYBEGIN P;
			    Tokens.ML_ID (yytext, yypos, yypos + size yytext));

<INITIAL>{eol}{sharp}{ws}*"if"	 => (YYBEGIN P;
				     newline yypos;
				     Tokens.IF (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"then" => (YYBEGIN P;
				     newline yypos;
				     Tokens.THEN (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"elif" => (YYBEGIN P;
				     newline yypos;
				     Tokens.ELIF (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"else" => (YYBEGIN P;
				     newline yypos;
				     Tokens.ELSE (yypos, yypos + size yytext));
<INITIAL>{eol}{sharp}{ws}*"endif" => (YYBEGIN P;
				      newline yypos;
				      Tokens.ENDIF (yypos,
						    yypos + size yytext));

<INITIAL,M,PM>{eol}     => (newline yypos; continue ());
<P>{eol}                => (YYBEGIN INITIAL; newline yypos; continue ());

<INITIAL,M,PM,P>{ws}+   => (continue ());

<M,PM>.                 => (error yypos
			    ("illegal character at start of ML symbol: " ^
			     yytext);
			    continue ());

<INITIAL>{cmid}		=> (idToken (yytext, yypos));


<INITIAL>.		=> (error yypos
			    ("illegal character: " ^ yytext);
			    continue ());
