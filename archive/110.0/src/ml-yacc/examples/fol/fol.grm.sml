
functor FolLrValsFun (structure Token : TOKEN
			       structure Absyn : ABSYN ) : Fol_LRVALS = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\002\000\000\000\000\000\
\\001\000\002\000\044\000\000\000\
\\001\000\002\000\045\000\000\000\
\\001\000\003\000\022\000\004\000\021\000\006\000\047\000\007\000\020\000\
\\009\000\019\000\000\000\
\\001\000\003\000\022\000\004\000\021\000\007\000\020\000\009\000\019\000\
\\010\000\052\000\000\000\
\\001\000\003\000\030\000\006\000\051\000\007\000\029\000\000\000\
\\001\000\005\000\012\000\011\000\011\000\012\000\010\000\013\000\009\000\
\\016\000\008\000\000\000\
\\001\000\005\000\018\000\011\000\017\000\012\000\016\000\016\000\008\000\000\000\
\\001\000\006\000\054\000\000\000\
\\001\000\006\000\059\000\000\000\
\\001\000\014\000\004\000\015\000\003\000\000\000\
\\001\000\016\000\026\000\017\000\025\000\000\000\
\\001\000\016\000\042\000\017\000\041\000\018\000\040\000\000\000\
\\061\000\000\000\
\\062\000\000\000\
\\063\000\003\000\030\000\007\000\029\000\000\000\
\\064\000\005\000\018\000\011\000\017\000\012\000\016\000\016\000\008\000\000\000\
\\065\000\003\000\022\000\004\000\021\000\007\000\020\000\009\000\019\000\000\000\
\\066\000\005\000\012\000\011\000\011\000\012\000\010\000\013\000\009\000\
\\016\000\008\000\000\000\
\\067\000\000\000\
\\068\000\003\000\022\000\009\000\019\000\000\000\
\\069\000\003\000\022\000\004\000\021\000\009\000\019\000\000\000\
\\070\000\003\000\030\000\000\000\
\\071\000\000\000\
\\072\000\003\000\022\000\004\000\021\000\007\000\020\000\009\000\019\000\000\000\
\\073\000\003\000\022\000\004\000\021\000\007\000\020\000\009\000\019\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\003\000\030\000\000\000\
\\078\000\003\000\022\000\004\000\021\000\009\000\019\000\000\000\
\\079\000\003\000\030\000\007\000\029\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\005\000\023\000\000\000\
\\083\000\000\000\
\\084\000\003\000\053\000\000\000\
\\085\000\000\000\
\\086\000\000\000\
\\087\000\000\000\
\\088\000\000\000\
\\089\000\000\000\
\\090\000\000\000\
\\091\000\005\000\055\000\000\000\
\\092\000\000\000\
\"
val actionRowNumbers =
"\010\000\018\000\016\000\026\000\
\\017\000\014\000\034\000\011\000\
\\011\000\019\000\006\000\032\000\
\\015\000\013\000\011\000\028\000\
\\007\000\006\000\007\000\006\000\
\\006\000\012\000\006\000\001\000\
\\002\000\006\000\003\000\006\000\
\\007\000\007\000\005\000\004\000\
\\022\000\021\000\020\000\038\000\
\\036\000\008\000\039\000\044\000\
\\043\000\025\000\042\000\041\000\
\\024\000\027\000\030\000\029\000\
\\031\000\033\000\006\000\012\000\
\\035\000\012\000\023\000\037\000\
\\009\000\040\000\000\000"
val gotoT =
"\
\\001\000\058\000\000\000\
\\003\000\005\000\004\000\004\000\006\000\003\000\000\000\
\\002\000\013\000\005\000\012\000\006\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\022\000\000\000\
\\009\000\025\000\000\000\
\\000\000\
\\004\000\026\000\006\000\003\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\029\000\000\000\
\\000\000\
\\005\000\030\000\006\000\011\000\000\000\
\\004\000\031\000\006\000\003\000\000\000\
\\005\000\032\000\006\000\011\000\000\000\
\\004\000\033\000\006\000\003\000\000\000\
\\004\000\034\000\006\000\003\000\000\000\
\\007\000\037\000\008\000\036\000\010\000\035\000\000\000\
\\004\000\041\000\006\000\003\000\000\000\
\\000\000\
\\000\000\
\\004\000\044\000\006\000\003\000\000\000\
\\000\000\
\\004\000\046\000\006\000\003\000\000\000\
\\005\000\047\000\006\000\011\000\000\000\
\\005\000\048\000\006\000\011\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\054\000\006\000\003\000\000\000\
\\007\000\055\000\008\000\036\000\010\000\035\000\000\000\
\\000\000\
\\007\000\056\000\008\000\036\000\010\000\035\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 59
val numrules = 32
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit | INT of  (string)
 | UCID of  (string) | LCID of  (string) | start of  (Absyn.absyn)
end
type svalue = MlyValue.svalue
type result = Absyn.absyn
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_change = 
(nil
,(T 1) :: nil
)::
nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "DOT"
  | (T 2) => "COMMA"
  | (T 3) => "SEMICOLON"
  | (T 4) => "LPAREN"
  | (T 5) => "RPAREN"
  | (T 6) => "BACKARROW"
  | (T 7) => "DOUBLEARROW"
  | (T 8) => "ARROW"
  | (T 9) => "BAR"
  | (T 10) => "TRUE"
  | (T 11) => "FORALL"
  | (T 12) => "EXISTS"
  | (T 13) => "PARSEPROG"
  | (T 14) => "PARSEQUERY"
  | (T 15) => "LCID"
  | (T 16) => "UCID"
  | (T 17) => "INT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 10) :: (T 11) :: (T 12) :: (T 13)
 :: (T 14) :: nil
end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(_,_,clause1right))::(_,(_,PARSEPROG1left,_))::rest671) => 
let val result=MlyValue.start((Absyn.null))
 in (LrTable.NT 0,(result,PARSEPROG1left,clause1right),rest671) end
| (1,(_,(_,_,query1right))::(_,(_,PARSEQUERY1left,_))::rest671) => 
let val result=MlyValue.start((Absyn.null))
 in (LrTable.NT 0,(result,PARSEQUERY1left,query1right),rest671) end
| (2,(_,(_,dform1left,dform1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 1,(result,dform1left,dform1right),rest671) end
| (3,rest671) => let val result=MlyValue.ntVOID(())
 in (LrTable.NT 1,(result,defaultPos,defaultPos),rest671) end
| (4,(_,(_,gform1left,gform1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 2,(result,gform1left,gform1right),rest671) end
| (5,rest671) => let val result=MlyValue.ntVOID(())
 in (LrTable.NT 2,(result,defaultPos,defaultPos),rest671) end
| (6,(_,(_,TRUE1left,TRUE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,TRUE1left,TRUE1right),rest671) end
| (7,(_,(_,_,gform2right))::_::(_,(_,gform1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,gform1left,gform2right),rest671) end
| (8,(_,(_,_,gform2right))::_::(_,(_,gform1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,gform1left,gform2right),rest671) end
| (9,(_,(_,_,dform1right))::_::(_,(_,gform1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,gform1left,dform1right),rest671) end
| (10,(_,(_,_,gform3right))::_::_::_::(_,(_,gform1left,_))::rest671)
 => let val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,gform1left,gform3right),rest671) end
| (11,(_,(_,_,gform1right))::_::(_,(_,FORALL1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,FORALL1left,gform1right),rest671) end
| (12,(_,(_,_,gform1right))::_::(_,(_,EXISTS1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,EXISTS1left,gform1right),rest671) end
| (13,(_,(_,atom1left,atom1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,atom1left,atom1right),rest671) end
| (14,(_,(_,_,RPAREN1right))::_::(_,(_,LPAREN1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 3,(result,LPAREN1left,RPAREN1right),rest671) end
| (15,(_,(_,TRUE1left,TRUE1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 4,(result,TRUE1left,TRUE1right),rest671) end
| (16,(_,(_,_,dform2right))::_::(_,(_,dform1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (LrTable.NT 4,(result,dform1left,dform2right),rest671) end
| (17,(_,(_,_,gform1right))::_::(_,(_,dform1left,_))::rest671) => let 
val result=MlyValue.ntVOID(())
 in (LrTable.NT 4,(result,dform1left,gform1right),rest671) end
| (18,(_,(_,_,dform1right))::_::(_,(_,FORALL1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 4,(result,FORALL1left,dform1right),rest671) end
| (19,(_,(_,atom1left,atom1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 4,(result,atom1left,atom1right),rest671) end
| (20,(_,(_,_,RPAREN1right))::_::(_,(_,LPAREN1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 4,(result,LPAREN1left,RPAREN1right),rest671) end
| (21,(_,(_,LCID1left,LCID1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 5,(result,LCID1left,LCID1right),rest671) end
| (22,(_,(_,_,RPAREN1right))::_::_::(_,(_,LCID1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 5,(result,LCID1left,RPAREN1right),rest671) end
| (23,(_,(_,term1left,term1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 6,(result,term1left,term1right),rest671) end
| (24,(_,(_,_,termlist1right))::_::(_,(_,term1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 6,(result,term1left,termlist1right),rest671) end
| (25,(_,(_,id1left,id1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 7,(result,id1left,id1right),rest671) end
| (26,(_,(_,INT1left,INT1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 7,(result,INT1left,INT1right),rest671) end
| (27,(_,(_,_,RPAREN1right))::_::_::(_,(_,LCID1left,_))::rest671) => 
let val result=MlyValue.ntVOID(())
 in (LrTable.NT 7,(result,LCID1left,RPAREN1right),rest671) end
| (28,(_,(_,_,DOT1right))::(_,(_,LCID1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (LrTable.NT 8,(result,LCID1left,DOT1right),rest671) end
| (29,(_,(_,_,DOT1right))::(_,(_,UCID1left,_))::rest671) => let val 
result=MlyValue.ntVOID(())
 in (LrTable.NT 8,(result,UCID1left,DOT1right),rest671) end
| (30,(_,(_,LCID1left,LCID1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 9,(result,LCID1left,LCID1right),rest671) end
| (31,(_,(_,UCID1left,UCID1right))::rest671) => let val result=
MlyValue.ntVOID(())
 in (LrTable.NT 9,(result,UCID1left,UCID1right),rest671) end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Fol_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun BACKARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun DOUBLEARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FORALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EXISTS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun PARSEPROG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PARSEQUERY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun LCID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.LCID i,p1,p2))
fun UCID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.UCID i,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.INT i,p1,p2))
end
end
