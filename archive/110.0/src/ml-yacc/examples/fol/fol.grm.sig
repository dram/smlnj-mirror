signature Fol_TOKENS =
sig
type ('a,'b) token
type svalue
val INT: (string) *  'a * 'a -> (svalue,'a) token
val UCID: (string) *  'a * 'a -> (svalue,'a) token
val LCID: (string) *  'a * 'a -> (svalue,'a) token
val PARSEQUERY:  'a * 'a -> (svalue,'a) token
val PARSEPROG:  'a * 'a -> (svalue,'a) token
val EXISTS:  'a * 'a -> (svalue,'a) token
val FORALL:  'a * 'a -> (svalue,'a) token
val TRUE:  'a * 'a -> (svalue,'a) token
val BAR:  'a * 'a -> (svalue,'a) token
val ARROW:  'a * 'a -> (svalue,'a) token
val DOUBLEARROW:  'a * 'a -> (svalue,'a) token
val BACKARROW:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val SEMICOLON:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val EOF:  'a * 'a -> (svalue,'a) token
end
signature Fol_LRVALS=
sig
structure Tokens : Fol_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
