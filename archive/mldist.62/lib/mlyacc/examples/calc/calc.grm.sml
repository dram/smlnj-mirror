functor CalcLrValsFun(structure Token : TOKEN)
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Sample interactive calculator for ML-Yacc *)

fun lookup "bogus" = 10000
  | lookup s = 0


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionT =
"\
\\001\000\006\000\002\000\005\000\005\000\004\000\
\\006\000\021\000\007\000\021\000\000\000\001\000\
\\003\000\011\000\004\000\010\000\006\000\020\000\
\\007\000\020\000\008\000\009\000\009\000\008\000\010\000\007\000\000\000\001\000\
\\001\000\006\000\002\000\005\000\000\000\001\000\
\\003\000\022\000\004\000\022\000\006\000\022\000\
\\007\000\022\000\008\000\022\000\009\000\022\000\010\000\022\000\000\000\001\000\
\\003\000\023\000\004\000\023\000\006\000\023\000\
\\007\000\023\000\008\000\023\000\009\000\023\000\010\000\023\000\000\000\001\000\
\\001\000\006\000\002\000\005\000\000\000\001\000\
\\001\000\006\000\002\000\005\000\000\000\001\000\
\\001\000\006\000\002\000\005\000\000\000\001\000\
\\001\000\006\000\002\000\005\000\000\000\001\000\
\\001\000\006\000\002\000\005\000\000\000\001\000\
\\003\000\011\000\004\000\010\000\006\000\019\000\
\\007\000\019\000\008\000\009\000\009\000\008\000\010\000\007\000\000\000\001\000\
\\003\000\027\000\004\000\010\000\006\000\027\000\
\\007\000\027\000\008\000\009\000\009\000\008\000\010\000\027\000\000\000\001\000\
\\003\000\026\000\004\000\026\000\006\000\026\000\
\\007\000\026\000\008\000\009\000\009\000\026\000\010\000\026\000\000\000\001\000\
\\003\000\028\000\004\000\028\000\006\000\028\000\
\\007\000\028\000\008\000\009\000\009\000\028\000\010\000\028\000\000\000\001\000\
\\003\000\025\000\004\000\025\000\006\000\025\000\
\\007\000\025\000\008\000\009\000\009\000\025\000\010\000\025\000\000\000\001\000\
\\003\000\024\000\004\000\010\000\006\000\024\000\
\\007\000\024\000\008\000\009\000\009\000\008\000\010\000\024\000\000\000\001\000\
\\006\000\000\000\007\000\000\000\000\000\001\000\
\"
val gotoT =
"\
\\001\000\001\000\002\000\016\000\000\000\000\000\
\\000\000\000\000\
\\001\000\010\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\001\000\011\000\000\000\000\000\
\\001\000\012\000\000\000\000\000\
\\001\000\013\000\000\000\000\000\
\\001\000\014\000\000\000\000\000\
\\001\000\015\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\"
val numstates = 17
val string_to_int = fn(s,index) => (ordof(s,index) + 
			ordof(s,index+1)*256,index+2)
	val convert_string_to_row = fn (conv_key,conv_entry) =>
	     fn(s,index) =>
		let fun f (r,index) =
			let val (num,index) = string_to_int(s,index)
			    val (i,index) = string_to_int(s,index)
			in if num=0 then ((rev r,conv_entry i),index)
			   else f((conv_key (num-1),conv_entry i)::r,index)
			end
		in f(nil,index)
		end
	 val convert_string_to_row_list = fn conv_funcs => fn s =>
		    let val convert_row =convert_string_to_row conv_funcs
		 	fun f(r,index) =
			  if index < String.length s then
			    let val (newlist,index) = convert_row (s,index)
			    in f(newlist::r,index)
			    end
			  else rev r
		    in f(nil,0)
		    end
	 val entry_to_action = fn j =>
		       if j=0 then ACCEPT
		       else if j=1 then ERROR
		       else if j >= (numstates+2) then REDUCE (j-numstates-2)
		       else SHIFT (STATE (j-2))
	 val make_goto_table = convert_string_to_row_list(NT,STATE)
	 val make_action_table=convert_string_to_row_list(T,entry_to_action)
	 val gotoT = map (fn (a,b) => a) (make_goto_table gotoT)
	 val actionT = make_action_table actionT
     in LrTable.mkLrTable {actions=actionT,gotos=gotoT,
	  numStates=numstates,initialState=STATE 0}
     end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NUM of unit ->  (int) | ID of unit ->  (string)
 | START of unit ->  (int option) | EXP of unit ->  (int)
end
type svalue = MlyValue.svalue
type result = int option
end
structure EC=
struct
open LrTable
val is_keyword =
fn (T 5) => true | (T 4) => true | _ => false
val preferred_insert =
fn (T 9) => true | (T 8) => true | (T 3) => true | (T 2) => true | _ => false
val preferred_subst =
fn (T 0) =>(T 4)::nil
| _ => nil
val noShift = 
fn (T 6) => true | _ => false
val showTerminal =
fn (T 0) => "ID"
  | (T 1) => "NUM"
  | (T 2) => "PLUS"
  | (T 3) => "TIMES"
  | (T 4) => "PRINT"
  | (T 5) => "SEMI"
  | (T 6) => "EOF"
  | (T 7) => "CARAT"
  | (T 8) => "DIV"
  | (T 9) => "SUB"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn (T 0) => MlyValue.ID(fn () => ("bogus")) | 
_ => MlyValue.VOID
end
val terms = (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6) :: (T 7) :: (T 8
) :: (T 9) :: nil
end
structure Actions =
struct 
exception mlyAction of int
val actions = 
let open Header
in
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of (0,(_,(MlyValue.EXP (EXP1),EXPleft as EXP1left,EXPright as 
EXP1right)) :: (_,(_,PRINTleft as PRINT1left,PRINTright as PRINT1right
)) :: rest671) =>
let val result = 
MlyValue.START (fn () => (let 
val EXP as EXP1 = EXP1()
 in (
print EXP;
                     print "\n";
                     SOME EXP
) end ))
in (LrTable.NT 1,(result,PRINT1left,EXP1right),rest671)
end
| (1,(_,(MlyValue.EXP (EXP1),EXPleft as EXP1left,EXPright as EXP1right
)) :: rest671) =>
let val result = 
MlyValue.START (fn () => (let 
val EXP as EXP1 = EXP1()
 in (SOME EXP) end ))
in (LrTable.NT 1,(result,EXP1left,EXP1right),rest671)
end
| (2,rest671) =>
let val result = 
MlyValue.START ( fn () => ((NONE)))
in (LrTable.NT 1,(result,defaultPos,defaultPos),rest671)
end
| (3,(_,(MlyValue.NUM (NUM1),NUMleft as NUM1left,NUMright as NUM1right
)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val NUM as NUM1 = NUM1()
 in (NUM) end ))
in (LrTable.NT 0,(result,NUM1left,NUM1right),rest671)
end
| (4,(_,(MlyValue.ID (ID1),IDleft as ID1left,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val ID as ID1 = ID1()
 in (lookup ID) end ))
in (LrTable.NT 0,(result,ID1left,ID1right),rest671)
end
| (5,(_,(MlyValue.EXP (EXP2),EXP2left,EXP2right)) :: (_,(_,
PLUSleft as PLUS1left,PLUSright as PLUS1right)) :: (_,(MlyValue.EXP (
EXP1),EXPleft as EXP1left,EXPright as EXP1right)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val EXP as EXP1 = EXP1()
val EXP2 = EXP2()
 in (EXP1+EXP2) end ))
in (LrTable.NT 0,(result,EXP1left,EXP2right),rest671)
end
| (6,(_,(MlyValue.EXP (EXP2),EXP2left,EXP2right)) :: (_,(_,
TIMESleft as TIMES1left,TIMESright as TIMES1right)) :: (_,(MlyValue.
EXP (EXP1),EXPleft as EXP1left,EXPright as EXP1right)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val EXP as EXP1 = EXP1()
val EXP2 = EXP2()
 in (EXP1*EXP2) end ))
in (LrTable.NT 0,(result,EXP1left,EXP2right),rest671)
end
| (7,(_,(MlyValue.EXP (EXP2),EXP2left,EXP2right)) :: (_,(_,DIVleft as 
DIV1left,DIVright as DIV1right)) :: (_,(MlyValue.EXP (EXP1),
EXPleft as EXP1left,EXPright as EXP1right)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val EXP as EXP1 = EXP1()
val EXP2 = EXP2()
 in (EXP1 div EXP2) end ))
in (LrTable.NT 0,(result,EXP1left,EXP2right),rest671)
end
| (8,(_,(MlyValue.EXP (EXP2),EXP2left,EXP2right)) :: (_,(_,SUBleft as 
SUB1left,SUBright as SUB1right)) :: (_,(MlyValue.EXP (EXP1),
EXPleft as EXP1left,EXPright as EXP1right)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val EXP as EXP1 = EXP1()
val EXP2 = EXP2()
 in (EXP1-EXP2) end ))
in (LrTable.NT 0,(result,EXP1left,EXP2right),rest671)
end
| (9,(_,(MlyValue.EXP (EXP2),EXP2left,EXP2right)) :: (_,(_,
CARATleft as CARAT1left,CARATright as CARAT1right)) :: (_,(MlyValue.
EXP (EXP1),EXPleft as EXP1left,EXPright as EXP1right)) :: rest671) =>
let val result = 
MlyValue.EXP (fn () => (let 
val EXP as EXP1 = EXP1()
val EXP2 = EXP2()
 in (
let fun e (m,0) = 1
                                | e (m,l) = m*e(m,l-1)
                         in e (EXP1,EXP2)       
			 end
) end ))
in (LrTable.NT 0,(result,EXP1left,EXP2right),rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Calc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun NUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.NUM (fn () => i),p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun CARAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun SUB (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
end
end
