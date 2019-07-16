signature Pascal_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val YAMP:  'a * 'a -> (svalue,'a) token
val YSLASH:  'a * 'a -> (svalue,'a) token
val YSTAR:  'a * 'a -> (svalue,'a) token
val UNARYSIGN:  'a * 'a -> (svalue,'a) token
val YBAR:  'a * 'a -> (svalue,'a) token
val YMINUS:  'a * 'a -> (svalue,'a) token
val YPLUS:  'a * 'a -> (svalue,'a) token
val YGREATER:  'a * 'a -> (svalue,'a) token
val YEQUAL:  'a * 'a -> (svalue,'a) token
val YLESS:  'a * 'a -> (svalue,'a) token
val YTILDE:  'a * 'a -> (svalue,'a) token
val YRBRA:  'a * 'a -> (svalue,'a) token
val YLBRA:  'a * 'a -> (svalue,'a) token
val YCARET:  'a * 'a -> (svalue,'a) token
val YCOLON:  'a * 'a -> (svalue,'a) token
val YCOMMA:  'a * 'a -> (svalue,'a) token
val YSEMI:  'a * 'a -> (svalue,'a) token
val YRPAR:  'a * 'a -> (svalue,'a) token
val YLPAR:  'a * 'a -> (svalue,'a) token
val YDOT:  'a * 'a -> (svalue,'a) token
val YEXTERN:  'a * 'a -> (svalue,'a) token
val YILLCH:  'a * 'a -> (svalue,'a) token
val YCASELAB:  'a * 'a -> (svalue,'a) token
val YHEX:  'a * 'a -> (svalue,'a) token
val YOCT:  'a * 'a -> (svalue,'a) token
val YBINT:  'a * 'a -> (svalue,'a) token
val YWITH:  'a * 'a -> (svalue,'a) token
val YWHILE:  'a * 'a -> (svalue,'a) token
val YVAR:  'a * 'a -> (svalue,'a) token
val YUNTIL:  'a * 'a -> (svalue,'a) token
val YTYPE:  'a * 'a -> (svalue,'a) token
val YDOWNTO:  'a * 'a -> (svalue,'a) token
val YTHEN:  'a * 'a -> (svalue,'a) token
val YSTRING:  'a * 'a -> (svalue,'a) token
val YSET:  'a * 'a -> (svalue,'a) token
val YREPEAT:  'a * 'a -> (svalue,'a) token
val YRECORD:  'a * 'a -> (svalue,'a) token
val YPROG:  'a * 'a -> (svalue,'a) token
val YFUNCTION:  'a * 'a -> (svalue,'a) token
val YNIL:  'a * 'a -> (svalue,'a) token
val YPACKED:  'a * 'a -> (svalue,'a) token
val YOR:  'a * 'a -> (svalue,'a) token
val YOF:  'a * 'a -> (svalue,'a) token
val YNUMB:  'a * 'a -> (svalue,'a) token
val YNOT:  'a * 'a -> (svalue,'a) token
val YMOD:  'a * 'a -> (svalue,'a) token
val YLABEL:  'a * 'a -> (svalue,'a) token
val YINT:  'a * 'a -> (svalue,'a) token
val YIN:  'a * 'a -> (svalue,'a) token
val YIF:  'a * 'a -> (svalue,'a) token
val YID:  'a * 'a -> (svalue,'a) token
val YGOTO:  'a * 'a -> (svalue,'a) token
val YPROCEDURE:  'a * 'a -> (svalue,'a) token
val YFORWARD:  'a * 'a -> (svalue,'a) token
val YFOR:  'a * 'a -> (svalue,'a) token
val YFILE:  'a * 'a -> (svalue,'a) token
val YEND:  'a * 'a -> (svalue,'a) token
val YELSE:  'a * 'a -> (svalue,'a) token
val YTO:  'a * 'a -> (svalue,'a) token
val YDOTDOT:  'a * 'a -> (svalue,'a) token
val YDO:  'a * 'a -> (svalue,'a) token
val YDIV:  'a * 'a -> (svalue,'a) token
val YCONST:  'a * 'a -> (svalue,'a) token
val YCASE:  'a * 'a -> (svalue,'a) token
val YBEGIN:  'a * 'a -> (svalue,'a) token
val YARRAY:  'a * 'a -> (svalue,'a) token
val YAND:  'a * 'a -> (svalue,'a) token
end
signature Pascal_LRVALS=
sig
structure Tokens : Pascal_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
