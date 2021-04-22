(* parserfn.sml *)

(* Functional, monadic parser for patterns.
 * Doesn't do layered or vector patterns (yet)
 *)

signature PARSER =
sig
  type instream
  val patterns : (PatSyntax.pattern list, instream) Reader.reader
end 

structure Parser :> PARSER where type instream = StringStream.instream  =
struct

local
  structure S = StringStream
  structure R = Reader
  structure T = Token
  structure L = Lexer
  open PatSyntax
in

type instream = StringStream.instream

val return = R.return
val fail = R.fail
val chain = R.chain

val +++ = R.choice
infixr 6 +++

val tokenRdr = L.tokenRdr

type 'a parser = ('a, S.instream) R.reader

(* isToken : token -> token parser *)
fun isToken (t: T.token) =
    chain tokenRdr (fn t' =>
     if t = t' then return t
     else fail)

(* numpat : pattern parser *)
val numpat =
    chain tokenRdr (fn t =>
     case t
       of T.NAT n => return (NumPat n)
        |  _ => fail)

(* identpat : pat parser, translates identifier to Id pat *)
val identpat =
    chain tokenRdr (fn t =>
     case t
       of T.ID s => return (Id s)
        | _ => fail)

(* variable = parses identifier token, but returns associated string *)
val identifier : string parser =
    chain tokenRdr (fn t =>
     case t
       of T.ID s => return s
        | _ => fail)

fun atompat instream =
   (numpat
    +++
    identpat
    +++
    parenpat
   ) instream

and parenpat instream =
    (chain (isToken T.LPAR) (fn _ =>
       chain patseq (fn ps =>
	 chain (isToken T.RPAR) (fn _ =>
           return (TuplePat ps)))
       +++
       chain (isToken T.RPAR) (fn _ =>
	 return NullPat))
    ) instream

and patseq instream =
    (chain pattern (fn p1 =>
       chain (isToken T.COMMA) (fn _ =>
         chain patseq (fn ps =>
           return (p1::ps)))
       +++
       return [p1])
    ) instream

and orpat instream =
    (chain apppat (fn p1 =>
       chain (isToken T.BAR) (fn _ =>
	 chain apppat (fn p2 =>
           return (OrPat(p1,p2))))
       +++
       (return p1))
    ) instream
      
and apppat instream = 
    (chain identifier (fn id =>
       chain atompat (fn arg =>
	 return (AppPat(id,arg))))
    ) instream
    
and pattern instream =
    (apppat
     +++
     orpat
     +++
     atompat
    ) instream

val one_pattern : pattern parser =
    chain pattern (fn s =>
     chain (isToken T.SEMI) (fn _ =>
      return s))

(* stmts: pattern list parser
 * a list of patterns separated by semicolons *)
fun patterns instream =
   (chain pattern (fn s =>
     chain (isToken T.SEMI) (fn _ =>
      chain patterns (fn p =>
       return (s::p)))
     +++
     return [s])
   ) instream

end (* local *)
end (* structure Parser *)
