(* scan.sml *)

signature LEX = sig
    structure Token: TOKEN
    structure Symbol: SYMBOL
    val NextToken: Token.token ref
    val IdValue: Symbol.symbol ref
    val IntValue: int ref
    val RealValue : string ref
    val StringValue: string ref
    val Advance: unit -> unit
    val getSymbol : unit -> Symbol.symbol
    val CheckToken: Token.token -> unit
    val At: Token.token -> bool
    type source
    val openSource : instream * string -> source * source
    val resumeSource : source -> source
    val closeSource : source -> unit
    val debugTokens: bool ref
    val flush : unit -> unit
end;


structure Lex : LEX = struct

(* uses: Ascii, Token, CharScan, ErrorMsg, CharBuffer *)

structure Token: TOKEN = Token;
structure Symbol: SYMBOL = Symbol;

infix before
fun a before b = a

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
  a_nl		(* inc LineNum, continue, discard *)
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

(* open Token; -- Token already opened in symtab.sml *)
open ErrorMsg;
open CharBuffer;
open SymbolTable;

val stream = ref std_in;
val next = ref 128

fun getchar() = 
    next := (ord(input(!stream, 1))
	     handlex ord => 128
		 || io_failure with "end of stream" => 128)

fun flush () = (while can_input(!stream, 1) do getchar();
		getchar())

exceptionx escape

fun StringEscape () =
      (if !next = Ascii.DQUOTE orelse !next = Ascii.BACKSLASH
		 then (InsertChar (!next); getchar())
	else if !next = Ascii.LC_N then (InsertChar Ascii.NEWLINE; getchar())
	else if !next = Ascii.LC_T then (InsertChar Ascii.TAB; getchar())
	else if !next = Ascii.CARET then 
	    (getchar();
	     if !next < 64 orelse !next > 96
		 then raisex escape
		 else (InsertChar (!next - 64); getchar()))
	else if Ascii.IsDigit (!next) then
            let val n = ref 0
	    in  n := 100 * (!next - Ascii.ZERO);
	        getchar();
	        if not (Ascii.IsDigit (!next))
		    then raisex escape
		    else (n := !n + 10 * (!next - Ascii.ZERO);
		          getchar();
		          if not (Ascii.IsDigit (!next))
		              then raisex escape
			      else InsertChar (!n + !next - Ascii.ZERO);
			  getchar())
	    end
	else if Ascii.IsFormat (!next) then
	    (while Ascii.IsFormat(!next)
	       do (if !next = Ascii.NEWLINE then inc ErrorMsg.LineNum else ();
	           getchar());
	     if !next <> Ascii.BACKSLASH
	         then raisex escape
		 else getchar())
	else (InsertChar(!next); getchar())
       )
       handlex escape => Complain "illegal string escape"

local 
  open Token
in

val CommentLevel = ref 0
val IntValue = ref 0
val IdValue = ref (Symbol.New("bogus"))
val StringValue = ref "bogus"
val RealValue = ref "0.0"


exceptionx Found : Token.token
fun found t = raisex Found with t

fun a_error s =  (Complain("illegal character: "^Ascii.CharName(!next));
	          getchar(); s_look)
and a_nl    s = (inc ErrorMsg.LineNum; getchar(); s)
and a_null  s = (getchar(); s)
and a_go    s = (InsertChar (!next); getchar(); s)
and a_numst s = (CharBuffer.Reset(); s_num)
and a_numen s = (IntValue := MakeInt(); found INT)
and a_alfst s = (CharBuffer.Reset(); InsertChar(!next); getchar(); s_alfid)
and a_alfen s = let val tok = LexClass(IdValue := MakeId(); !IdValue)
	         in found (case tok
	           of ID => if ordof(Symbol.Name(!IdValue),0) = Ascii.SQUOTE
				then TYVAR else ID
		    | _ => tok)
	       end
and a_symst s =(CharBuffer.Reset(); InsertChar(!next); getchar(); s_symid)
and a_twid  s = (CharBuffer.Reset(); InsertChar(!next); getchar(); s_twid)
and a_twsym s = s_symid
and a_twnum s = s_num
and a_symen s = (IdValue := MakeId(); found (LexClass(!IdValue)))
and a_dot   s = case (IdValue := MakeId(); LexClass(!IdValue))
		  of ID => (getchar(); s_iddot)
		   | x => found x  (* illegal dot will be caught next time *)
and a_iddot s = found IDDOT
and a_iderr s = (Complain "non-identifier in qualified name"; found IDDOT)
and a_dotst s =  (* first DOT *)
	      (case (getchar(); !next)
	         of 46 (* Ascii.DOT *) =>  (* second DOT *)
		      (case (getchar(); !next)
		         of 46 (* Ascii.DOT *) =>  (* third DOT *)
			      (getchar(); found DOTDOTDOT)
			  | c => (Complain "illegal dots, ignored"; s_look))
		  | c => (Complain "illegal dot, ignored";  s_look))
and a_strst s = (CharBuffer.Reset(); getchar(); s_str)
and a_stren s = (StringValue := MakeString(); getchar(); found STRING)
and a_bstr  s = (Complain "unclosed string"; a_stren s)
and a_esc   s = (getchar(); StringEscape(); s)
and a_ast   s = (case (getchar(); !next)
		  of 41 => (* Ascii.RPAREN *)
	                   (dec CommentLevel; getchar();
		            if !CommentLevel = 0 then s_look
						 else s)
	           | _ => s)
and a_open  s = (case (getchar(); !next)
		  of 42 => (* Ascii.ASTERISK *)
	                    (inc CommentLevel; getchar(); s_com)
	           | _ => if !CommentLevel = 0 then found LPAREN else s)
and a_punc  s =	found (case (!next before getchar())
		  of 44 => COMMA
		   | 123 => LBRACE
		   | 91 => LBRACKET
		   | 125 => RBRACE
		   | 93 => RBRACKET
		   | 41 => RPAREN
		   | 59 => SEMICOLON
		   | _ => Impossible "Lex.34")
and a_eof   s = found EOF
and a_frac  s = let val prev = !next
	         in InsertChar prev; getchar();
		    if prev = Ascii.DOT
		      andalso not(!next >= Ascii.ZERO
				  andalso !next <= Ascii.NINE)
		      then Complain "at least one digit must \
				    \follow dot in real number"
		      else ();
		    s_frac
		end
and a_rend  s = (RealValue := MakeString(); found REAL)
and a_rerr  s = (Complain "illegal real number"; found REAL)
and a_exp1  s = (InsertChar(!next); getchar(); s_exp1)
and a_exp2  s = (InsertChar(!next); getchar(); s_exp2)
and a_exp3  s = (InsertChar(!next); getchar(); s_exp3)

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

fun Scan state = Scan (((ctype sub !next) sub state) state)

(* interface functions for parser  *)

val NextToken = ref ID

val debugTokens = ref false

fun Advance () =
    (NextToken := (Scan(s_look) handlex Found with t => t);
     if !debugTokens
	then (print("Advance: "); print(TokenName(!NextToken));
	      if !NextToken = ID 
		then (print(" "); print(Symbol.Name(!IdValue)); ()) else ();
	      print("\n"); ())
	else ())

fun getSymbol () = !IdValue before Advance()

type source = {stream : instream,
	       nextchar: int ref,
	       nexttok : token ref,
	       filename : string,
	       comlev : int ref,
	       linenum : int ref}

val currentSource = ref {stream=std_in, nextchar= ref 128,
		         nexttok = ref EOF, filename="std_in",
		         linenum = ref 0, comlev = ref 0}

fun suspend() = let val ref{nextchar=c,nexttok=t,linenum=l,comlev=cl,...} =
		       currentSource
	       in c := !next; t := !NextToken; l := !ErrorMsg.LineNum;
		  cl := !CommentLevel
	      end
fun resume() =  let val ref{nextchar=c,nexttok=t,linenum=l,comlev=cl,
			  filename,stream=s,...} =
		       currentSource
	       in next := !c; NextToken := !t; ErrorMsg.LineNum := !l;
	          CommentLevel := !cl; ErrorMsg.FileName := filename;
		  stream := s
	      end


fun openSource(f,fname) = 
   (!currentSource,
    (suspend(); currentSource := 
		    {stream=f, nextchar= ref 128,
		         nexttok = ref EOF, filename=fname,
		         linenum = ref 1, comlev = ref 0};
     resume();
     getchar();
     Advance();
     !currentSource))

fun resumeSource s = (!currentSource before
			(suspend(); currentSource := s; resume()))

fun closeSource {stream,nextchar,nexttok,filename,linenum,comlev} =
	close_in stream
(*
fun closeSource ({stream,...} : source) = close_in stream
  -- didn't work - must be a type error *)

fun CheckToken (tok: token) =
    if !NextToken = tok
        then Advance()
        else Complain("expected - "^TokenName(tok)^
				" Found - "^TokenName(!NextToken))

fun At (tok: token) : bool =
    if !NextToken = tok then (Advance(); true) else false


end; (* local -- Token *)

end; (* structure Lex *)
