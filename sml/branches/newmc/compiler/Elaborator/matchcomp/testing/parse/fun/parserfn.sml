(* parserfn.sml *)

(* Functional, monadic parser for Fun.
 * For the grammar for expressions and declarations, see the README file *)

signature PARSER =
sig
  type instream
  val stmt : (Syntax.stmt, instream) Reader.reader
end 

functor ParserFn(S : CHAR_STREAM) (* :> PARSER where type instream = S.instream *)
=
struct

structure R = Reader
structure T = Token
structure L = LexerFn(S)

open Syntax

val return = R.return
val fail = R.fail
val chain = R.chain

val +++ = R.choice
infixr 6 +++

val tokenRdr = L.tokenRdr

type 'a parser = ('a, S.instream) R.reader

(* auxiliary function for rearranging curried applications,
 * handling mixed curried applications with both expression
 * and type arguments *)
datatype arg = Ex of expr | Ty of ty

(* fixApp : expr * arg list -> expr *)
fun fixApp (rator, (Ex(e)::args)) = fixApp(App(rator,e),args)
  | fixApp (rator, (Ty(t)::args)) = fixApp(TApp(rator,t),args)
  | fixApp (rator, nil) = rator

(* function to translate primop tokens into variable expressions *)
fun relTokToVar tok =
    Var(case tok
         of T.EQUAL => "=="
	  | T.NOTEQUAL => "/="
	  | T.LESS => "<"
	  | T.GREATER => ">"
	  | T.LESSEQ => "<="
	  | T.GREATEREQ => ">="
	  | _ => raise Fail "unexpected relational operator")

(* isToken : token -> token parser *)
fun isToken (t: T.token) =
    chain tokenRdr (fn t' =>
     if t = t' then return t
     else fail)

(* numexpr : expr parser *)
val numexpr =
    chain tokenRdr (fn t =>
     case t
       of T.NAT n => return (Num n)
        |  _ => fail)

(* boolexpr : expr parser *)
val boolexpr =
    (chain (isToken T.TRUE) (fn _ =>
      return (Bool(true)))
     +++
     chain (isToken T.FALSE) (fn _ =>
      return (Bool(false))))

(* identexpr : expr parser, translates identifier to Var expr *)
val identexpr =
    chain tokenRdr (fn t =>
     case t
       of T.ID s => return (Var s)
        | _ => fail)

(* variable = parses identifier token, but returns associated string *)
val variable : string parser =
    chain tokenRdr (fn t =>
     case t
       of T.ID s => return s
        | _ => fail)

(* tyexpr : ty parser  -- type expressions *)
fun tyexpr instream =
    chain tyterm (fn ty1 =>
     chain (isToken T.TARROW) (fn _ =>
      chain tyexpr (fn ty2 => 
       return (FUNty(ty1,ty2))))
     +++
     return ty1) instream

and tyterm instream =
    chain tyfactor (fn ty1 =>
     chain (isToken T.PLUS) (fn _ =>
      chain tyterm (fn ty2 =>
       return (SUMty(ty1,ty2))))
     +++
     return ty1) instream

and tyfactor instream =
    chain tyatom (fn ty1 =>
     chain (isToken T.TIMES) (fn _ =>
      chain tyfactor (fn ty2 =>
       return (PRODty(ty1,ty2))))
     +++
     return ty1) instream

(* tyfactor : ty parser -- atomic type expressions *)
and tyatom instream =
   (chain (isToken T.INT) (fn _ =>
     return (INTty))
    +++
    chain (isToken T.BOOL) (fn _ =>
     return (BOOLty))
    +++
    chain variable (fn v =>
     return (VARty v))
    +++
    chain (isToken T.LPAR) (fn _ =>
     chain tyexpr (fn ty =>
      chain (isToken T.RPAR) (fn _ =>
       return ty)))
   ) instream


(* aexpr :  expr parser -- arithmetic expression *)
fun aexpr instream = 
    chain aterm (fn t =>
     chain (isToken T.PLUS) (fn _ =>
      chain aexpr (fn e =>
       return (App(Var "+", Pair(t, e)))))
     +++
     chain (isToken T.MINUS) (fn _ =>
      chain aexpr (fn e =>
       return (App(Var "-", Pair(t, e)))))
     +++
     return t) instream

(* aterm : expr parser *)
and aterm instream =
    chain afactor (fn f =>
     chain (isToken T.TIMES) (fn _ =>
      chain aterm (fn t =>
       return (App(Var "*", Pair(f, t)))))
     +++
     chain (isToken T.DIV) (fn _ =>
      chain aterm (fn u =>
       return (App(Var "/", Pair(f, u)))))
     +++
     chain (isToken T.MOD) (fn _ =>
      chain aterm (fn v =>
       return (App(Var "mod", Pair(f, v)))))
     +++
     return f) instream

(* afactor : expr parser *)
and afactor instream =
   (appexpr
    +++
    atom
   ) instream

(* rexpr: relational expressions -- atomic boolean expressions *)
(* rexpr: expr parser *)
and rexpr instream =
   (chain aexpr (fn e1 =>
     chain ((isToken T.EQUAL) +++
            (isToken T.NOTEQUAL) +++
            (isToken T.LESS) +++
            (isToken T.GREATER) +++
            (isToken T.LESSEQ) +++
            (isToken T.GREATEREQ)) (fn reltok =>
      chain aexpr (fn e2 =>
       return (App(relTokToVar reltok,Pair(e1,e2))))))
   ) instream

(* bexpr: boolean expressions formed by infix && and || and unary "not"
 * && has higher precedence than || and they are both right associative.
 * Subsumes rexpr. *)
and bexpr instream =
   (chain bterm (fn be1 =>
     chain (isToken T.OR) (fn _ =>
      chain bexpr (fn be2 => 
       return (App(Var "||", Pair(be1, be2)))))
     +++
     return be1)
   ) instream

and bterm instream =
    (chain bfactor (fn be1 =>
      chain (isToken T.AND) (fn _ =>
       chain bterm (fn be2 =>
        return (App(Var "&&", Pair(be1, be2)))))
      +++
      return be1)
    ) instream

and bfactor instream =
    (rexpr
     +++
     aexpr
    ) instream

and atom instream =
   (numexpr
    +++
    boolexpr
    +++
    identexpr
    +++
    chain (isToken T.INL) (fn _ =>
     return (Var "inl"))
    +++
    chain (isToken T.INR) (fn _ =>
     return (Var "inr"))
    +++
    chain (isToken T.LPAR) (fn _ =>
     chain expr (fn e1 =>
      chain (isToken T.COMMA) (fn _ =>  (* Pair *)
       chain expr (fn e2 =>
        chain (isToken T.RPAR) (fn _ =>
         return (Pair(e1,e2)))))
      +++
      chain (isToken T.RPAR) (fn _ =>   (* parenthesized expr *)
       return e1)))
   ) instream

and appexpr instream =
   (chain atom (fn rator =>
     (chain appargs (fn args =>
       return (fixApp(rator,args)))))
   ) instream

and appargs instream =
   (chain atom (fn arg =>
     chain appargs (fn args =>
      return (Ex(arg)::args))
     +++
     return [Ex(arg)])
    +++ 
    chain (isToken T.LBRACKET) (fn _ =>
     chain tyexpr (fn ty =>
      chain (isToken T.RBRACKET) (fn _ =>
       chain appargs (fn args =>
        return (Ty(ty)::args))
       +++
       return [Ty(ty)])))
   ) instream
 
(* cexpr: conditional expressions "if bexpr then cexpr else cexpr", where
 * the condition must be a bexpr and the then and else branches are 
 * (arithmetic) conditionals. Subsumes expr. *)
(* cexpr : expr parser *)
and cexpr instream =
   (chain (isToken T.IF) (fn _ =>
     chain expr (fn condExpr =>
      chain (isToken T.THEN) (fn _ =>
       chain expr (fn thenExpr =>
        chain (isToken T.ELSE) (fn _ =>
         chain expr (fn elseExpr =>
          return (If(condExpr, thenExpr, elseExpr))))))))
   ) instream

(* letexpr: simple let expressions *)
and letexpr instream =
   (chain (isToken T.LET) (fn _ =>
     chain variable (fn v => 
      chain (isToken T.EQ) (fn _ =>
       chain expr (fn def =>
        chain (isToken T.IN) (fn _ =>
         chain expr (fn body =>
          return(Let(def,(v,body)))))))))
   ) instream

(* letrecexpr: simple let expressions *)
and letrecexpr instream =
   (chain (isToken T.LETREC) (fn _ =>
     chain variable (fn f => 
      chain (isToken T.COLON) (fn _ =>
       chain tyexpr (fn ty1 => 
	chain (isToken T.EQ) (fn _ =>
         chain fexpr (fn Fun(y,ty2,defbody) =>
          chain (isToken T.IN) (fn _ =>
           chain expr (fn body =>
            return(Letrec(f,ty1,((y,ty2,defbody),body)))))))))))
   ) instream

and caseexpr instream =
   (chain (isToken T.CASE) (fn _ =>
     chain expr (fn sc => 
      chain (isToken T.OF) (fn _ =>
       chain (isToken T.INL) (fn _ =>
        chain variable (fn v1 =>
         chain (isToken T.DARROW) (fn _ =>
          chain expr (fn left =>
           chain (isToken T.BAR) (fn _ =>
            chain (isToken T.INR) (fn _ =>
             chain variable (fn v2 =>
              chain expr (fn right =>
               return (Case(sc,(v1,left),(v2,right))))))))))))))
   ) instream

and fexpr instream =
   (chain (isToken T.FN) (fn _ =>
     chain variable (fn v =>
      chain (isToken T.COLON) (fn _ =>
       chain tyexpr (fn ty =>
        chain (isToken T.DARROW) (fn _ =>
         chain expr (fn body =>
          return (Fun(v,ty,body))))))))
   ) instream

and tfexpr instream =
   (chain (isToken T.TFN) (fn _ =>
     chain variable (fn v =>
      chain (isToken T.DARROW) (fn _ =>
       chain expr (fn body =>
        return (TFun(v,body))))))
   ) instream

and foldexpr instream =
   (chain (isToken T.FOLD) (fn _ =>
     chain (isToken T.LBRACKET) (fn _ =>
      chain tyexpr (fn ty =>
       chain (isToken T.RBRACKET) (fn _ =>
        chain atom (fn e =>
         return (Fold(ty,e)))))))
   ) instream

and unfoldexpr instream =
   (chain (isToken T.UNFOLD) (fn _ =>
     chain atom (fn e =>
      return (Unfold e)))
   ) instream

(* expr: expr parser *)
and expr instream =
   (letexpr
    +++
    letrecexpr
    +++
    fexpr
    +++
    tfexpr
    +++
    foldexpr
    +++
    unfoldexpr
    +++
    cexpr
    +++
    bexpr
   ) instream

(* decl: parses a simple or function declaration *)
val decl : decl parser =
    chain (isToken T.VAL) (fn _ =>
     chain variable (fn v =>
      chain (isToken T.EQ) (fn _ =>
       chain expr (fn e =>
        return (ValDef(v,e))))))
    +++
    chain (isToken T.FUN) (fn _ =>
     chain variable (fn fname =>     (* function name *)
      chain (isToken T.LPAR) (fn _ =>
       chain variable (fn pname =>    (* parameter name *)
        chain (isToken T.COLON) (fn _ =>
         chain tyexpr (fn ty_arg =>
          chain (isToken T.RPAR) (fn _ =>
           chain (isToken T.COLON) (fn _ =>
	    chain tyexpr (fn ty_res =>
             chain (isToken T.EQ) (fn _ =>
              chain expr (fn body =>
               return (FunDef(fname, pname, ty_arg, ty_res, body)))))))))))))

(* statement: a declaration or an expression, which can be arithmetic or boolean *)
val stmt : stmt parser =
    chain decl (fn d =>
     return (Decl d))
    +++
    chain expr (fn e =>
     return (Expr e))
    +++
    chain (isToken T.QUIT) (fn _ =>
     return Quit)

val top : stmt parser =
    chain stmt (fn s =>
     chain (isToken T.SEMI) (fn _ =>
      return s))

(* stmts: stmt list parser   (not used)
 * a list of statments separated by semicolons *)
fun stmts instream =
   (chain stmt (fn s =>
     chain (isToken T.SEMI) (fn _ =>
      chain stmts (fn p =>
       return (s::p)))
     +++
     return [s])
   ) instream

end (* structure Parser *)

(* Notes

This defines a parser for a fairly powerful explicitly typed lambda
calculus that includes arithmetic and boolean expressions, let expressions,
(recursive) functions, product, sum, and function types, recursive types,
and polymorphism.

Note that the distinction between aexprs (arithmetic expressions) and
bexprs (boolean expressions) is not really about the type of the values
returned, but about the binding power of the operators involved.  Boolean
operators like || and && bind more loosely than arithmetic operators,
and arithmetic operators are subdivided into additive and multiplicative
groups, with the multiplicative operators having higher precedence.  Function
application, "f(arg)", binds more tightly than any binary operator.

The top-level expression parsing function is

    bexpr : expr parser

This encompases conditional (cexpr) and arithmetic (aexpr) expressions.

Given a list of tokens as input, these parser functions will consume as large
a prefix as can be parsed into the respective grammar phrase, but they are not
guarantee to consume all the tokens.  So in a given input line, there may remain
some unparsed "junk" when the parsing function (e.g. stmt) returns. One could
detect this situation and report an error, but we don't bother in this version.

This functional, monadic style of parser is quite inefficient, but is fairly
straightforward to implement, despite requiring some fiddling to
get it right for any nontrivial grammar.  For a "serious" language, we would
use a more powerful and robust parsing tool, such as ml-yacc.  Similarly,
the tokenization function can be generated from a lexical specification
using tools like ml-lex and lexgen to generate.

These are the kinds of tools used in real language implementations like SML/NJ.

*)
