(* scan.sml *)

signature LEX = sig
    structure Token: TOKEN
    structure Symbol: SYMBOL
    val nextToken: Token.token ref
    val idValue: Symbol.symbol ref
    val intValue: int ref
    val realValue : string ref
    val stringValue: string ref
    val advance: unit -> unit
    val getSymbol : unit -> Symbol.symbol
    val checkToken: Token.token -> unit
    val at: Token.token -> bool
    type source
    val openSource : instream * string -> source * source
    val resumeSource : source -> source
    val closeSource : source -> unit
    val toplevel : bool ref
    val debugTokens: bool ref
    val flush : unit -> unit
end


structure Lex : LEX = struct

(* uses: Ascii, CharScan, ErrorMsg, CharBuffer *)

structure Token: TOKEN = Token
structure Symbol: SYMBOL = Symbol

(* character types

 t_garb	     garbage 
 t_nl	     new line 
 t_sp	     white space (space and tab) 
 t_punc	     punctuation (";", ",", "{", etc) 
 t_num	     digits 
 t_alph	     alphabetic characters 
 t_e	     the letter E
 t_symb	     "symbolic" characters 
 t_quot	     double quote (string delimiter) 
 t_esc	     string escape "\" (also a symbol character) 
 t_ast	     asterisk (symbol character and part of comment delim) 
 t_open	     open (left) parenthsis (punctuation and comment) 
 t_dot	     period (decimal pnt and qualified name separator) 
 t_twid	     ~ character, used in id's and negative literals 
 t_strg	     printable character not in any above class 
 t_eof
  *)

(* scanner actions

  a_error	(* complain, discard *)
  a_nl		(* inc lineNum, continue, discard *)
  a_null	(* continue, discard *)
  a_go		(* continue, save current char *)
  a_numst	(* start scanning number, save *)
  a_numen	(* finish scanning number, defer *)
  a_alfst	(* start scanning alphanumeric, save *)
  a_alfen	(* finish alphanumeric, defer *)
  a_symst	(* start symbolic, save *)
  a_twid	(* start either a symbolic or a numberic literal *)
  a_twnum	(* negative integer literal *)
  a_twsym	(* symbolic beginning with ~ *)
  a_symen	(* finish symbolic, defer *)
  a_dot		(* period within token, check qualified id or real literal *)
  a_iddot	(* character after period within token *)
  a_iderr	(* non-identifier character after period within token *)
  a_dotst	(* initial dot, check for DOTDOTDOT *)
  a_strst	(* start string, discard *)
  a_stren	(* finish string, discard *)
  a_bstr	(* newline in string, complain, discard *)
  a_esc		(* process string escape, discard *)
  a_ast		(* asterisk, check for comment end *)
  a_open	(* left paren, check for comment start *)
  a_punc	(* punctuation, return token, discard *)
  a_eof		(* return EOF token *)

*)
(* scanner states *)

val s_look	= 0	(* looking for lexeme *)
and s_num	= 1	(* scanning number *)
and s_alfid	= 2	(* scanning alphanumeric identifier *)
and s_symid	= 3	(* scanning "symbolic" identifier *)
and s_com	= 4	(* scanning comment *)
and s_str	= 5	(* scanning string *)
and s_twid	= 6     (* seen ~ *)
and s_iddot	= 7	(* seen id with trailing dot *)
and s_frac	= 8	(* in fraction part of real number *)
and s_exp1	= 9	(* after the E of an exponent *)
and s_exp2	= 10	(* after E~ of an exponent *)
and s_exp3	= 11	(* after E2 or E~2 of an exponent *)

open ErrorMsg
open CharBuffer
open SymbolTable

val stream = ref std_in
val next = ref Ascii.newline
val toplevel = ref true

fun getchar() = 
    next := (ord(input(!stream, 1))
	     handle Ord => 128
		  | Io_failure("end of stream") => 128)

fun flush () = (while can_input(!stream, 1) do getchar();
		next := Ascii.newline)

exception Escape

fun stringEscape () =
      (if !next = Ascii.dquote orelse !next = Ascii.backslash
		 then (insertChar (!next); getchar())
	else if !next = Ascii.lc_n then (insertChar Ascii.newline; getchar())
	else if !next = Ascii.lc_t then (insertChar Ascii.tab; getchar())
	else if !next = Ascii.caret then 
	    (getchar();
	     if !next < 64 orelse !next > 96
		 then raise Escape
		 else (insertChar (!next - 64); getchar()))
	else if Ascii.IsDigit (!next) then
            let val n = ref 0
	    in  n := 100 * (!next - Ascii.zero);
	        getchar();
	        if not (Ascii.IsDigit (!next))
		    then raise Escape
		    else (n := !n + 10 * (!next - Ascii.zero);
		          getchar();
		          if not (Ascii.IsDigit (!next))
		              then raise Escape
			      else insertChar (!n + !next - Ascii.zero);
			  getchar())
	    end
	else if Ascii.IsFormat (!next) then
	    (while Ascii.IsFormat(!next)
	       do (if !next = Ascii.newline then inc ErrorMsg.lineNum else ();
	           getchar());
	     if !next <> Ascii.backslash
	         then raise Escape
		 else getchar())
	else (insertChar(!next); getchar())
       )
       handle Escape => complain "illegal string escape"

local 
  open Token
in

val commentLevel = ref 0
val intValue = ref 0
val idValue = ref (Symbol.new("bogus"))
val stringValue = ref "bogus"
val realValue = ref "0.0"


exception Found of Token.token
fun found t = raise Found(t)

fun a_error s =  (complain("illegal character: "^Ascii.CharName(!next));
	          getchar(); s_look)
and a_nl    s = (inc ErrorMsg.lineNum;
		 if !interactive
		 then if !toplevel	
		      then (output(std_out, !System.Control.primaryPrompt);
			    flush_out std_out)
		      else (output(std_out, !System.Control.secondaryPrompt);
			    flush_out std_out)
		 else ();
		 getchar(); s)
and a_null  s = (getchar(); s)
and a_go    s = (insertChar (!next); getchar(); s)
and a_numst s = (CharBuffer.reset(); s_num)
and a_numen s = (intValue := makeInt(); found INT)
and a_alfst s = (CharBuffer.reset(); insertChar(!next); getchar(); s_alfid)
and a_alfen s = let val tok = lexClass(idValue := makeId(); !idValue)
	         in found (case tok
	           of ID => if ordof(Symbol.name(!idValue),0) = Ascii.squote
				then TYVAR else ID
		    | _ => tok)
	       end
and a_symst s =(CharBuffer.reset(); insertChar(!next); getchar(); s_symid)
and a_twid  s = (CharBuffer.reset(); insertChar(!next); getchar(); s_twid)
and a_twsym s = s_symid
and a_twnum s = s_num
and a_symen s = (idValue := makeId(); found (lexClass(!idValue)))
and a_dot   s = case (idValue := makeId(); lexClass(!idValue))
		  of ID => (getchar(); s_iddot)
		   | x => found x  (* illegal dot will be caught next time *)
and a_iddot s = found IDDOT
and a_iderr s = (complain "non-identifier in qualified name"; found IDDOT)
and a_dotst s =  (* first DOT *)
	      (case (getchar(); !next)
	         of 46 (* Ascii.dot *) =>  (* second DOT *)
		      (case (getchar(); !next)
		         of 46 (* Ascii.dot *) =>  (* third DOT *)
			      (getchar(); found DOTDOTDOT)
			  | c => (complain "illegal dots, ignored"; s_look))
		  | c => (complain "illegal dot, ignored";  s_look))
and a_strst s = (CharBuffer.reset(); getchar(); s_str)
and a_stren s = (stringValue := makeString(); getchar(); found STRING)
and a_bstr  s = condemn "unclosed string"
and a_esc   s = (getchar(); stringEscape(); s)
and a_ast   s = (case (getchar(); !next)
		  of 41 => (* Ascii.rparen *)
	                   (dec commentLevel; getchar();
		            if !commentLevel = 0 then s_look
						 else s)
	           | _ => s)
and a_open  s = (case (getchar(); !next)
		  of 42 => (* Ascii.asterisk *)
	                    (inc commentLevel; getchar(); s_com)
	           | _ => if !commentLevel = 0 then found LPAREN else s)
and a_punc  s =	found (case (!next before getchar())
		  of 44 => COMMA
		   | 123 => LBRACE
		   | 91 => LBRACKET
		   | 125 => RBRACE
		   | 93 => RBRACKET
		   | 41 => RPAREN
		   | 59 => SEMICOLON
		   | _ => impossible "Lex.34")
and a_eof   s = found EOF
and a_frac  s = let val prev = !next
	         in insertChar prev; getchar();
		    if prev = Ascii.dot
		      andalso not(!next >= Ascii.zero
				  andalso !next <= Ascii.nine)
		      then complain "at least one digit must \
				    \follow dot in real number"
		      else ();
		    s_frac
		end
and a_rend  s = (realValue := makeString(); found REAL)
and a_rerr  s = (complain "illegal real number"; found REAL)
and a_exp1  s = (insertChar(!next); getchar(); s_exp1)
and a_exp2  s = (insertChar(!next); getchar(); s_exp2)
and a_exp3  s = (insertChar(!next); getchar(); s_exp3)

(* action table *)

local val a = arrayoflist
    in
(*             s_look  s_num   s_alfid s_symid s_com   s_str   s_twid  s_iddot s_frac  s_exp1  s_exp2  s_exp3 *)
val t_garb = a[a_error,a_error,a_error,a_error,a_error,a_error,a_error,a_iderr,a_error,a_error,a_error,a_error]
val t_nl   = a[a_nl,   a_numen,a_alfen,a_symen,a_nl,   a_bstr, a_twsym,a_iderr,a_rend, a_rerr, a_rerr, a_rend ]
val t_sp   = a[a_null, a_numen,a_alfen,a_symen,a_null, a_go,   a_twsym,a_iderr,a_rend, a_rerr, a_rerr, a_rend ]
val t_punc = a[a_punc, a_numen,a_alfen,a_symen,a_null, a_go,   a_twsym,a_iderr,a_rend, a_rerr, a_rerr, a_rend ]
val t_num  = a[a_numst,a_go,   a_go,   a_symen,a_null, a_go,   a_twnum,a_iderr,
a_go,   a_exp3, a_exp3, a_go   ]
val t_alph = a[a_alfst,a_numen,a_go,   a_symen,a_null, a_go,   a_twsym,a_iddot,a_rend, a_rerr, a_rerr, a_rend ]
val t_e    = a[a_alfst,a_exp1, a_go,   a_symen,a_null, a_go,   a_twsym,a_iddot,a_exp1, a_rerr, a_rerr, a_rend ]
val t_symb = a[a_symst,a_numen,a_alfen,a_go,   a_null, a_go,   a_twsym,a_iddot,a_rend, a_rerr, a_rerr, a_rend ]
val t_quot = a[a_strst,a_numen,a_alfen,a_symen,a_null, a_stren,a_twsym,a_iderr,a_rend, a_rerr, a_rerr, a_rend ]
val t_esc  = a[a_symst,a_numen,a_alfen,a_go,   a_null, a_esc,  a_twsym,a_iddot,a_rend, a_rerr, a_rerr, a_rend ]
val t_ast  = a[a_symst,a_numen,a_alfen,a_go,   a_ast,  a_go,   a_twsym,a_iddot,a_rend, a_rerr, a_rerr, a_rend ]
val t_open = a[a_open, a_numen,a_alfen,a_symen,a_open, a_go,   a_twsym,a_iderr,a_rend, a_rerr, a_rerr, a_rend ]
val t_dot  = a[a_dotst,a_frac, a_dot,  a_dot,  a_null, a_go,   a_twsym,a_iderr,
a_rerr, a_rerr, a_rerr, a_rend ]
val t_twid = a[a_twid, a_numen,a_alfen,a_go,   a_null, a_go,   a_twsym,a_iddot,
a_rerr, a_exp2, a_rerr, a_rend ]
val t_strg = a[a_error,a_error,a_error,a_error,a_go,   a_go,   a_twsym,a_iderr,a_rend, a_rerr, a_rerr, a_rend ]
val t_eof  = a[a_eof,  a_numen,a_alfen,a_symen,a_eof,  a_bstr,a_twsym,a_iderr, a_rend, a_rerr, a_rerr, a_rend ]
    end;

val ctype = arrayoflist
   [t_garb, t_garb, t_garb, t_garb, t_garb, t_garb, t_garb, t_garb,
    t_garb, t_sp,   t_nl,   t_garb, t_garb, t_garb, t_garb, t_garb,
    t_garb, t_garb, t_garb, t_garb, t_garb, t_garb, t_garb, t_garb,
    t_garb, t_garb, t_garb, t_garb, t_garb, t_garb, t_garb, t_garb,
    t_sp,   t_symb, t_quot, t_symb, t_symb, t_symb, t_symb, t_alph,
    t_open, t_punc, t_ast,  t_symb, t_punc, t_symb, t_dot,  t_symb,
    t_num,  t_num,  t_num,  t_num,  t_num,  t_num,  t_num,  t_num, 
    t_num,  t_num,  t_symb, t_punc, t_symb, t_symb, t_symb, t_symb,
    t_symb, t_alph, t_alph, t_alph, t_alph, t_e,    t_alph, t_alph,
    t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph,
    t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph,
    t_alph, t_alph, t_alph, t_punc, t_esc,  t_punc, t_symb, t_alph,
    t_symb, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph,
    t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph,
    t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph, t_alph,
    t_alph, t_alph, t_alph, t_punc, t_symb, t_punc ,t_twid, t_garb, t_eof];

fun scan state = scan (((ctype sub !next) sub state) state)

(* interface functions for parser  *)

val nextToken = ref SEMICOLON

val debugTokens = ref false

fun advance () =
    (nextToken := (scan(s_look) handle Found(t) => t);
     if !debugTokens
	then (print("advance: "); print(tokenName(!nextToken));
	      if !nextToken = ID 
		then (print(" "); print(Symbol.name(!idValue)); ()) else ();
	      print("\n"); ())
	else ())

fun getSymbol () = !idValue before advance()

type source = {stream : instream,
	       nextchar: int ref,
	       nexttok : token ref,
	       filename : string,
	       comlev : int ref,
	       linenum : int ref,
	       interactive : bool}

val currentSource = ref {stream = std_in, nextchar = ref Ascii.newline,
		         nexttok = ref SEMICOLON, filename ="std_in",
		         linenum = ref 0, comlev = ref 0, interactive = true}

fun suspend() = let val ref{nextchar=c,nexttok=t,linenum=l,comlev=cl,...} =
		          currentSource
	         in c := !next; t := !nextToken; l := !ErrorMsg.lineNum;
		    cl := !commentLevel
	        end

fun resume() = let val ref{nextchar=c,nexttok=t,linenum=l,comlev=cl,
			   filename,stream=s,interactive=i,...} =
		       currentSource
	        in next := !c; nextToken := !t; ErrorMsg.lineNum := !l;
	           commentLevel := !cl; ErrorMsg.fileName := filename;
		   stream := s; interactive := i
	       end


fun openSource(f,fname) =
   (!currentSource,
    (suspend();
     currentSource := 
		{stream = f, nextchar = ref Ascii.newline,
		 nexttok = ref SEMICOLON, filename = fname,
		 linenum = ref 1, comlev = ref 0,
		 interactive = is_term_in f};
     resume();
     !currentSource))

fun resumeSource s = (!currentSource before
		      (suspend(); currentSource := s; resume()))

fun closeSource ({stream,...} : source) = close_in stream

fun checkToken (tok: token) =
    if !nextToken = tok
        then advance()
        else case !nextToken of
	       ID => complain("expected " ^ tokenName tok ^
				", found ID (" ^ Symbol.name(!idValue) ^ ")")
	     | _ => complain("expected " ^ tokenName tok ^
				", found " ^ tokenName(!nextToken))

fun at (tok: token) : bool =
    if !nextToken = tok then (advance(); true) else false

end (* local open Token... *)
end (* structure Lex *)
