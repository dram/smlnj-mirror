(* lexerfn.sml *)

(* tokenization (aka lexical analysis) breaks down a sequence of characters 
 * into meaningful units like number literals, identifiers, keywords, operator
 * symbols, and punctuation marks like parentheses.  Here the tokenizer is
 * implemented in terms of a reader monad. *)

signature LEXER =
sig

  type instream 

  val tokenRdr : (Token.token,instream) Reader.reader

end


functor LexerFn(S: CHAR_STREAM) :> LEXER where type instream = S.instream  =
struct

  open Reader Token
  type instream = S.instream
  type 'a reader = ('a, instream) Reader.reader

  (* return next char if it satisfies prop, otherwise fail immediately *)
  fun sat (prop : char -> bool) : char reader =
      chain S.charRdr (fn c => if prop c then return c else fail)

  (* checkChar : char -> char reader 
   * check that the next character available is c, then return c *)
  fun checkChar c = sat (fn c' => c = c')

  (* checkChars: char list -> char list reader
   * check that the instream starts with s, then return s *) 
  fun checkChars ([] : char list) = return []
    | checkChars (s as (c::cs)) = 
      chain (checkChar c) (fn _ => chain (checkChars cs) (fn _ => (return s)))

  val isAlpha : char reader = sat Char.isAlpha;
  val isAlphaNum : char reader = sat Char.isAlphaNum;
  val isDigit : char reader = sat (Char.isDigit);
  val isSpace : char reader = sat Char.isSpace
  val nonSpace : char reader = sat (not o Char.isSpace)

  (* ident -- alphanumeric identifiers *)
  val ident : string reader = 
      chain isAlpha (fn c => 
       chain (star isAlphaNum) (fn cs =>
	return (implode(c::cs))));

  val nat : int reader = 
      chain (starPlus isDigit) (fn cs =>
       return(valOf(Int.fromString(implode cs))));

  val space : unit reader =
      chain (star isSpace) (fn _ =>
       return ())

  (* tokenizing -- space separated tokens *)

  (* token: discard leading space *)
  fun token (r: 'a reader) : 'a reader =
      chain space (fn () => 
       chain r (fn v => 
        return v))

  val identifier : string reader = token ident
  val natural : int reader = token nat

  fun checkString (s: string) : char list reader =
      checkChars (explode s)

  fun symbol s = token (checkString s)

  fun keyword (s: string) : char list reader =
      chain (token (checkString s)) (fn (v: char list) =>
       invert v isAlphaNum)

  val +++ = Reader.choice
  infixr 6 +++

(* tokens: including ...
 *   natural number literals
 *   identitiers (variables bound to numbers or functions)
 *   keywords: let, fun, if, then, else, not
 *   operator symbols (arithmetic, relational, and boolean)
 *)

  val tokenRdr : Token.token reader =
    chain natural (fn n =>
     return (NAT n))
    +++
    chain (keyword "quit") (fn _ => 
     return QUIT)
    +++
    chain (keyword "let") (fn _ => 
     return LET)
    +++
    chain (keyword "letrec") (fn _ => 
     return LETREC)
    +++
    chain (keyword "in") (fn _ => 
     return IN)
    +++
    chain (keyword "val") (fn _ => 
     return VAL)
    +++
    chain (keyword "fun") (fn _ => 
     return FUN)
    +++
    chain (keyword "fn") (fn _ => 
     return FN)
    +++
    chain (keyword "Fn") (fn _ => 
     return TFN)
    +++
    chain (keyword "case") (fn _ =>
     return CASE)
    +++
    chain (keyword "of") (fn _ =>
     return OF)
    +++
    chain (keyword "Inl") (fn _ =>
     return INL)
    +++
    chain (keyword "Inr") (fn _ =>
     return INR)
    +++
    chain (keyword "if") (fn _ => 
     return IF)
    +++
    chain (keyword "then") (fn _ => 
     return THEN)
    +++
    chain (keyword "else") (fn _ => 
     return ELSE)
    +++
    chain (keyword "true") (fn _ =>
     return TRUE)
    +++
    chain (keyword "false") (fn _ =>
     return FALSE)
    +++
    chain (keyword "not") (fn _ =>
     return NOT)
    +++
    chain (keyword "Int") (fn _ =>
     return INT)
    +++
    chain (keyword "Bool") (fn _ =>
     return BOOL)
    +++
    chain (keyword "Rec") (fn _ =>
     return REC)
    +++
    chain identifier (fn s =>
     return (ID s))
    +++
    chain (symbol "->") (fn _ => 
     return TARROW)
    +++
    chain (symbol "=>") (fn _ => 
     return DARROW)
    +++
    chain (symbol "==") (fn _ =>
     return EQUAL)
    +++
    chain (symbol "/=") (fn _ =>
     return NOTEQUAL)
    +++
    chain (symbol "<=") (fn _ =>
     return LESSEQ)
    +++
    chain (symbol ">=") (fn _ =>
     return GREATEREQ)
    +++
    chain (symbol "&&") (fn _ =>
     return AND)
    +++
    chain (symbol "||") (fn _ =>
     return OR)
    +++
    chain (symbol "|") (fn _ =>
     return BAR)
    +++
    chain (symbol "=") (fn _ => 
     return EQ)
    +++
    chain (symbol "<") (fn _ =>
     return LESS)
    +++
    chain (symbol ">") (fn _ =>
     return GREATER)
    +++
    chain (symbol "+") (fn _ =>
     return PLUS)
    +++
    chain (symbol "-") (fn _ =>
     return MINUS)
    +++
    chain (symbol "*") (fn _ =>
     return TIMES)
    +++
    chain (symbol "/") (fn _ =>
     return DIV)
    +++
    chain (symbol "%") (fn _ =>
     return MOD)
    +++
    chain (symbol "~") (fn _ =>
     return NEG)
    +++
    chain (symbol "(") (fn _ =>
     return LPAR)
    +++
    chain (symbol ")") (fn _ => 
     return RPAR)
    +++
    chain (symbol "[") (fn _ =>
     return LBRACKET)
    +++
    chain (symbol "]") (fn _ => 
     return RBRACKET)
    +++
    chain (symbol ":") (fn _ => 
     return COLON)
    +++
    chain (symbol ";") (fn _ => 
     return SEMI)
    +++
    chain (symbol ",") (fn _ => 
     return COMMA)

(*
  (* tokenize : token list reader *)
  val tokenize = star tokenRdr
*)

end (* structure Tokens *)

(* Notes

1. Order of token reading
We have to try to recognize keywords (or reserved words) before reading identifiers
in general. Otherwise the keywords will be read as identifiers.

Similarly, we have to read two character symbol tokens like "==" and "<=" before 
reading single charater tokens that are prefixes, like "=" and "<".  Otherwise the
single character prefixes will be read first, and "==" will be read as two successive
"=" tokens.

*)

