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

structure Lexer :> LEXER where type instream = StringStream.instream  =
struct

local
  structure S = StringStream
  open Reader Token
in
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

  val nat : string reader = 
      chain (starPlus isDigit) (fn cs =>
       return(implode cs));

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
  val natural : string reader = token nat

  fun checkString (s: string) : char list reader =
      checkChars (explode s)

  fun symbol s = token (checkString s)

  val +++ = Reader.choice
  infixr 6 +++

(* tokens: including ...
 *   natural number literals (as strings)
 *   identitiers (variables or datacon names, as strings)
 *   operator symbols ("|")
 *)

  val tokenRdr : Token.token reader =
    chain natural (fn n =>
     return (NAT n))
    +++
    chain identifier (fn s =>
     return (ID s))
    +++
    chain (symbol "|") (fn _ =>
     return BAR)
    +++
    chain (symbol "(") (fn _ =>
     return LPAR)
    +++
    chain (symbol ")") (fn _ => 
     return RPAR)
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

end (* local *)
end (* structure Tokens *)
