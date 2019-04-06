functor MlyaccLrValsFun(structure Token : TOKEN)
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi *)

(* parser for the ML parser generator *)
open Header

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionT =
"\
\\011\000\003\000\000\000\001\000\
\\000\000\098\000\
\\007\000\023\000\014\000\022\000\016\000\021\000\
\\019\000\020\000\020\000\019\000\021\000\018\000\022\000\017\000\
\\024\000\016\000\025\000\015\000\026\000\014\000\027\000\013\000\
\\028\000\012\000\030\000\011\000\034\000\010\000\035\000\009\000\
\\036\000\008\000\038\000\007\000\039\000\006\000\000\000\001\000\
\\000\000\097\000\
\\012\000\024\000\000\000\001\000\
\\000\000\111\000\
\\012\000\026\000\000\000\001\000\
\\012\000\027\000\000\000\001\000\
\\012\000\029\000\000\000\001\000\
\\000\000\126\000\
\\000\000\126\000\
\\031\000\032\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\000\000\113\000\
\\000\000\126\000\
\\000\000\126\000\
\\012\000\026\000\000\000\001\000\
\\000\000\112\000\
\\012\000\043\000\000\000\001\000\
\\000\000\126\000\
\\031\000\045\000\000\000\001\000\
\\012\000\048\000\000\000\001\000\
\\031\000\049\000\000\000\001\000\
\\004\000\050\000\000\000\099\000\
\\023\000\051\000\000\000\121\000\
\\000\000\102\000\
\\004\000\052\000\000\000\106\000\
\\010\000\053\000\000\000\001\000\
\\012\000\054\000\000\000\105\000\
\\012\000\054\000\000\000\101\000\
\\005\000\055\000\000\000\001\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\114\000\
\\000\000\134\000\
\\000\000\129\000\
\\000\000\132\000\
\\012\000\063\000\015\000\062\000\032\000\061\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\000\000\001\000\
\\000\000\139\000\
\\012\000\054\000\000\000\103\000\
\\012\000\054\000\000\000\107\000\
\\004\000\050\000\000\000\100\000\
\\000\000\109\000\
\\012\000\054\000\000\000\104\000\
\\000\000\108\000\
\\012\000\048\000\000\000\096\000\
\\000\000\124\000\
\\005\000\066\000\000\000\001\000\
\\000\000\115\000\
\\012\000\067\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\069\000\000\000\001\000\
\\012\000\070\000\000\000\001\000\
\\000\000\125\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\000\000\133\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\006\000\075\000\032\000\074\000\000\000\001\000\
\\005\000\076\000\000\000\001\000\
\\000\000\131\000\
\\000\000\142\000\
\\000\000\141\000\
\\000\000\140\000\
\\000\000\123\000\
\\000\000\126\000\
\\023\000\079\000\000\000\119\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\120\000\
\\010\000\080\000\000\000\001\000\
\\000\000\117\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\110\000\
\\012\000\039\000\013\000\038\000\000\000\135\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\136\000\
\\000\000\130\000\
\\012\000\063\000\015\000\062\000\000\000\001\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\004\000\083\000\000\000\122\000\
\\012\000\054\000\029\000\085\000\000\000\144\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\087\000\000\000\001\000\
\\005\000\088\000\000\000\001\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\138\000\
\\000\000\126\000\
\\031\000\090\000\000\000\001\000\
\\012\000\091\000\000\000\001\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\118\000\
\\000\000\116\000\
\\012\000\039\000\013\000\038\000\017\000\037\000\
\\031\000\036\000\037\000\035\000\000\000\001\000\
\\012\000\054\000\029\000\085\000\000\000\144\000\
\\000\000\127\000\
\\000\000\143\000\
\\001\000\058\000\002\000\057\000\012\000\039\000\
\\013\000\038\000\000\000\137\000\
\\031\000\094\000\000\000\001\000\
\\000\000\128\000\
\\008\000\000\000\000\000\001\000\
\"
val gotoT =
"\
\\001\000\093\000\000\000\000\000\
\\006\000\002\000\000\000\000\000\
\\005\000\003\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\002\000\023\000\000\000\000\000\
\\000\000\000\000\
\\013\000\026\000\000\000\000\000\
\\003\000\028\000\000\000\000\000\
\\003\000\029\000\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\031\000\000\000\000\000\
\\000\000\000\000\
\\003\000\038\000\000\000\000\000\
\\003\000\039\000\000\000\000\000\
\\002\000\040\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\003\000\042\000\000\000\000\000\
\\000\000\000\000\
\\010\000\045\000\011\000\044\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\004\000\058\000\008\000\057\000\000\000\000\000\
\\007\000\062\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\010\000\063\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\066\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\069\000\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\070\000\000\000\000\000\
\\007\000\032\000\014\000\071\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\003\000\076\000\009\000\075\000\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\004\000\079\000\000\000\000\000\
\\007\000\032\000\014\000\080\000\000\000\000\000\
\\000\000\000\000\
\\012\000\082\000\000\000\000\000\
\\007\000\032\000\014\000\084\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\003\000\087\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\007\000\032\000\014\000\090\000\000\000\000\000\
\\012\000\091\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\007\000\054\000\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\\000\000\000\000\
\"
val numstates = 94
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
datatype svalue = VOID | ntVOID of unit | UNKNOWN of  (string)
 | TYVAR of  (string) | PROG of  (string) | PREC of  (Header.lexvalue)
 | INT of  (string) | IDDOT of  (string) | ID of  (string*int)
 | HEADER of  (string) | TY of  (string)
 | SUBST_DECL of  ( ( Header.symbol*Header.symbol )  list)
 | RULE_PREC of  (Header.symbol option)
 | RULE_LIST of  (Header.rule list) | RULE of  (Header.rule list)
 | RHS_LIST of  (Header.rhsData) | RECORD_LIST of  (string)
 | QUAL_ID of  (string) | MPC_DECLS of  (Header.declData)
 | MPC_DECL of  (Header.declData) | LABEL of  (string)
 | ID_LIST of  (Header.symbol list)
 | CONSTR_LIST of  ( ( Header.symbol*Header.ty )  list)
 | BEGIN of  (string*Header.declData* ( Header.rule list ) )
end
type svalue = MlyValue.svalue
type result = string*Header.declData* ( Header.rule list ) 
end
structure EC=
struct
open LrTable
val is_keyword =
fn _ => false
val preferred_insert =
fn _ => false
val preferred_subst =
fn  _ => nil
val noShift = 
fn _ => false
val showTerminal =
fn (T 0) => "ARROW"
  | (T 1) => "ASTERISK"
  | (T 2) => "BLOCK"
  | (T 3) => "BAR"
  | (T 4) => "COLON"
  | (T 5) => "COMMA"
  | (T 6) => "DELIMITER"
  | (T 7) => "EOF"
  | (T 8) => "EQUAL"
  | (T 9) => "FOR"
  | (T 10) => "HEADER"
  | (T 11) => "ID"
  | (T 12) => "IDDOT"
  | (T 13) => "PERCENT_HEADER"
  | (T 14) => "INT"
  | (T 15) => "KEYWORD"
  | (T 16) => "LBRACE"
  | (T 17) => "LPAREN"
  | (T 18) => "NAME"
  | (T 19) => "NODEFAULT"
  | (T 20) => "NONTERM"
  | (T 21) => "NOSHIFT"
  | (T 22) => "OF"
  | (T 23) => "PERCENT_EOP"
  | (T 24) => "PERCENT_PURE"
  | (T 25) => "PERCENT_POS"
  | (T 26) => "PERCENT_ARG"
  | (T 27) => "PREC"
  | (T 28) => "PREC_TAG"
  | (T 29) => "PREFER"
  | (T 30) => "PROG"
  | (T 31) => "RBRACE"
  | (T 32) => "RPAREN"
  | (T 33) => "SUBST"
  | (T 34) => "START"
  | (T 35) => "TERM"
  | (T 36) => "TYVAR"
  | (T 37) => "VERBOSE"
  | (T 38) => "VALUE"
  | (T 39) => "UNKNOWN"
  | (T 40) => "BOGUS_VALUE"
  | _ => "bogus-term"
val errtermvalue=
let open Header in
fn _ => MlyValue.VOID
end
val terms = (T 0) :: (T 1) :: (T 2) :: (T 3) :: (T 4) :: (T 5) :: (T 6
) :: (T 7) :: (T 8) :: (T 9) :: (T 13) :: (T 15) :: (T 16) :: (T 17)
 :: (T 18) :: (T 19) :: (T 20) :: (T 21) :: (T 22) :: (T 23) :: (T 24)
 :: (T 25) :: (T 26) :: (T 28) :: (T 29) :: (T 31) :: (T 32) :: (T 33)
 :: (T 34) :: (T 35) :: (T 37) :: (T 38) :: (T 40) :: nil
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
of (0,(_,(MlyValue.RULE_LIST (RULE_LIST1 as RULE_LIST),
RULE_LISTleft as RULE_LIST1left,RULE_LISTright as RULE_LIST1right
)) :: (_,(_,DELIMITERleft as DELIMITER1left,DELIMITERright as 
DELIMITER1right)) :: (_,(MlyValue.MPC_DECLS (MPC_DECLS1 as MPC_DECLS),
MPC_DECLSleft as MPC_DECLS1left,MPC_DECLSright as MPC_DECLS1right
)) :: (_,(MlyValue.HEADER (HEADER1 as HEADER),HEADERleft as 
HEADER1left,HEADERright as HEADER1right)) :: rest671) =>
let val result = 
MlyValue.BEGIN (((HEADER,MPC_DECLS,rev RULE_LIST)))
in (LrTable.NT 0,(result,HEADER1left,RULE_LIST1right),rest671)
end
| (1,(_,(MlyValue.MPC_DECL (MPC_DECL1 as MPC_DECL),MPC_DECLleft as 
MPC_DECL1left,MPC_DECLright as MPC_DECL1right)) :: (_,(MlyValue.
MPC_DECLS (MPC_DECLS1 as MPC_DECLS),MPC_DECLSleft as MPC_DECLS1left,
MPC_DECLSright as MPC_DECLS1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECLS (((join_decls(MPC_DECLS,MPC_DECL))))
in (LrTable.NT 5,(result,MPC_DECLS1left,MPC_DECL1right),rest671)
end
| (2,rest671) =>
let val result = 
MlyValue.MPC_DECLS (((
{prec=nil,nonterm=NONE,term=NONE,eop=nil,control=nil,
		prefer=nil,keyword=nil,subst=nil,
		value=nil}
)))
in (LrTable.NT 5,(result,defaultPos,defaultPos),rest671)
end
| (3,(_,(MlyValue.CONSTR_LIST (CONSTR_LIST1 as CONSTR_LIST),
CONSTR_LISTleft as CONSTR_LIST1left,CONSTR_LISTright as 
CONSTR_LIST1right)) :: (_,(_,TERMleft as TERM1left,TERMright as 
TERM1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{ prec=nil,nonterm=NONE,
	       term = SOME CONSTR_LIST, eop =nil,control=nil,
		prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,TERM1left,CONSTR_LIST1right),rest671)
end
| (4,(_,(MlyValue.CONSTR_LIST (CONSTR_LIST1 as CONSTR_LIST),
CONSTR_LISTleft as CONSTR_LIST1left,CONSTR_LISTright as 
CONSTR_LIST1right)) :: (_,(_,NONTERMleft as NONTERM1left,
NONTERMright as NONTERM1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{ prec=nil,control=nil,nonterm= SOME CONSTR_LIST,
	       term = NONE, eop=nil,prefer=nil,subst=nil,keyword=nil,
	       value=nil}
)))
in (LrTable.NT 4,(result,NONTERM1left,CONSTR_LIST1right),rest671)
end
| (5,(_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),ID_LISTleft as 
ID_LIST1left,ID_LISTright as ID_LIST1right)) :: (_,(MlyValue.PREC (
PREC1 as PREC),PRECleft as PREC1left,PRECright as PREC1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec= [(PREC,ID_LIST)],control=nil,
	      nonterm=NONE,term=NONE,eop=nil,prefer=nil,subst=nil,
	      keyword=nil,value=nil}
)))
in (LrTable.NT 4,(result,PREC1left,ID_LIST1right),rest671)
end
| (6,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as ID1right
)) :: (_,(_,STARTleft as START1left,STARTright as START1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=[START_SYM ID],nonterm=NONE,
	       term = NONE, eop = nil,prefer=nil,subst=nil,keyword=nil,
	       value=nil}
)))
in (LrTable.NT 4,(result,START1left,ID1right),rest671)
end
| (7,(_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),ID_LISTleft as 
ID_LIST1left,ID_LISTright as ID_LIST1right)) :: (_,(_,
PERCENT_EOPleft as PERCENT_EOP1left,PERCENT_EOPright as 
PERCENT_EOP1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=nil,nonterm=NONE,term=NONE,
		eop=ID_LIST, prefer=nil,subst=nil,keyword=nil,
	 	value=nil}
)))
in (LrTable.NT 4,(result,PERCENT_EOP1left,ID_LIST1right),rest671)
end
| (8,(_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),ID_LISTleft as 
ID_LIST1left,ID_LISTright as ID_LIST1right)) :: (_,(_,KEYWORDleft as 
KEYWORD1left,KEYWORDright as KEYWORD1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=ID_LIST,
	 	value=nil}
)))
in (LrTable.NT 4,(result,KEYWORD1left,ID_LIST1right),rest671)
end
| (9,(_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),ID_LISTleft as 
ID_LIST1left,ID_LISTright as ID_LIST1right)) :: (_,(_,PREFERleft as 
PREFER1left,PREFERright as PREFER1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
		prefer=ID_LIST, subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,PREFER1left,ID_LIST1right),rest671)
end
| (10,(_,(MlyValue.SUBST_DECL (SUBST_DECL1 as SUBST_DECL),
SUBST_DECLleft as SUBST_DECL1left,SUBST_DECLright as SUBST_DECL1right
)) :: (_,(_,SUBSTleft as SUBST1left,SUBSTright as SUBST1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=nil,nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=SUBST_DECL,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,SUBST1left,SUBST_DECL1right),rest671)
end
| (11,(_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),ID_LISTleft as 
ID_LIST1left,ID_LISTright as ID_LIST1right)) :: (_,(_,NOSHIFTleft as 
NOSHIFT1left,NOSHIFTright as NOSHIFT1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=[NSHIFT ID_LIST],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,NOSHIFT1left,ID_LIST1right),rest671)
end
| (12,(_,(MlyValue.PROG (PROG1 as PROG),PROGleft as PROG1left,
PROGright as PROG1right)) :: (_,(_,PERCENT_HEADERleft as 
PERCENT_HEADER1left,PERCENT_HEADERright as PERCENT_HEADER1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=[FUNCTOR PROG],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,PERCENT_HEADER1left,PROG1right),rest671)
end
| (13,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: (_,(_,NAMEleft as NAME1left,NAMEright as NAME1right
)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=[PARSER_NAME ID],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil, value=nil}
)))
in (LrTable.NT 4,(result,NAME1left,ID1right),rest671)
end
| (14,(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as 
TY1right)) :: (_,(_,COLONleft as COLON1left,COLONright as COLON1right
)) :: (_,(MlyValue.PROG (PROG1 as PROG),PROGleft as PROG1left,
PROGright as PROG1right)) :: (_,(_,PERCENT_ARGleft as PERCENT_ARG1left
,PERCENT_ARGright as PERCENT_ARG1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
{prec=nil,control=[PARSE_ARG(PROG,TY)],nonterm=NONE,term=NONE,eop=nil,
		prefer=nil,subst=nil,keyword=nil, value=nil}
)))
in (LrTable.NT 4,(result,PERCENT_ARG1left,TY1right),rest671)
end
| (15,(_,(_,VERBOSEleft as VERBOSE1left,VERBOSEright as VERBOSE1right
)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
 {prec=nil,control=[Header.VERBOSE],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,VERBOSE1left,VERBOSE1right),rest671)
end
| (16,(_,(_,NODEFAULTleft as NODEFAULT1left,NODEFAULTright as 
NODEFAULT1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
 {prec=nil,control=[Header.NODEFAULT],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,NODEFAULT1left,NODEFAULT1right),rest671)
end
| (17,(_,(_,PERCENT_PUREleft as PERCENT_PURE1left,
PERCENT_PUREright as PERCENT_PURE1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
 {prec=nil,control=[Header.PURE],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,PERCENT_PURE1left,PERCENT_PURE1right),rest671)
end
| (18,(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as 
TY1right)) :: (_,(_,PERCENT_POSleft as PERCENT_POS1left,
PERCENT_POSright as PERCENT_POS1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
 {prec=nil,control=[Header.POS TY],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=nil}
)))
in (LrTable.NT 4,(result,PERCENT_POS1left,TY1right),rest671)
end
| (19,(_,(MlyValue.PROG (PROG1 as PROG),PROGleft as PROG1left,
PROGright as PROG1right)) :: (_,(MlyValue.ID (ID1 as ID),IDleft as 
ID1left,IDright as ID1right)) :: (_,(_,VALUEleft as VALUE1left,
VALUEright as VALUE1right)) :: rest671) =>
let val result = 
MlyValue.MPC_DECL (((
 {prec=nil,control=[Header.NODEFAULT],
	        nonterm=NONE,term=NONE,eop=nil,
	        prefer=nil,subst=nil,keyword=nil,
		value=[(ID,PROG)]}
)))
in (LrTable.NT 4,(result,VALUE1left,PROG1right),rest671)
end
| (20,(_,(MlyValue.ID (ID2),ID2left,ID2right)) :: (_,(_,FORleft as 
FOR1left,FORright as FOR1right)) :: (_,(MlyValue.ID (ID1 as ID),
IDleft as ID1left,IDright as ID1right)) :: (_,(_,BARleft as BAR1left,
BARright as BAR1right)) :: (_,(MlyValue.SUBST_DECL (SUBST_DECL1
 as SUBST_DECL),SUBST_DECLleft as SUBST_DECL1left,SUBST_DECLright as 
SUBST_DECL1right)) :: rest671) =>
let val result = 
MlyValue.SUBST_DECL ((((ID1,ID2)::SUBST_DECL)))
in (LrTable.NT 12,(result,SUBST_DECL1left,ID2right),rest671)
end
| (21,(_,(MlyValue.ID (ID2),ID2left,ID2right)) :: (_,(_,FORleft as 
FOR1left,FORright as FOR1right)) :: (_,(MlyValue.ID (ID1 as ID),
IDleft as ID1left,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.SUBST_DECL ((([(ID1,ID2)])))
in (LrTable.NT 12,(result,ID1left,ID2right),rest671)
end
| (22,(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as 
TY1right)) :: (_,(_,OFleft as OF1left,OFright as OF1right)) :: (_,(
MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as ID1right)) :: 
(_,(_,BARleft as BAR1left,BARright as BAR1right)) :: (_,(MlyValue.
CONSTR_LIST (CONSTR_LIST1 as CONSTR_LIST),CONSTR_LISTleft as 
CONSTR_LIST1left,CONSTR_LISTright as CONSTR_LIST1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST ((((ID,SOME TY)::CONSTR_LIST)))
in (LrTable.NT 1,(result,CONSTR_LIST1left,TY1right),rest671)
end
| (23,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: (_,(_,BARleft as BAR1left,BARright as BAR1right)) :: 
(_,(MlyValue.CONSTR_LIST (CONSTR_LIST1 as CONSTR_LIST),
CONSTR_LISTleft as CONSTR_LIST1left,CONSTR_LISTright as 
CONSTR_LIST1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST ((((ID,NONE)::CONSTR_LIST)))
in (LrTable.NT 1,(result,CONSTR_LIST1left,ID1right),rest671)
end
| (24,(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as 
TY1right)) :: (_,(_,OFleft as OF1left,OFright as OF1right)) :: (_,(
MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST ((([(ID,SOME TY)])))
in (LrTable.NT 1,(result,ID1left,TY1right),rest671)
end
| (25,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: rest671) =>
let val result = 
MlyValue.CONSTR_LIST ((([(ID,NONE)])))
in (LrTable.NT 1,(result,ID1left,ID1right),rest671)
end
| (26,(_,(MlyValue.RHS_LIST (RHS_LIST1 as RHS_LIST),RHS_LISTleft as 
RHS_LIST1left,RHS_LISTright as RHS_LIST1right)) :: (_,(_,COLONleft as 
COLON1left,COLONright as COLON1right)) :: (_,(MlyValue.ID (ID1 as ID),
IDleft as ID1left,IDright as ID1right)) :: rest671) =>
let val result = 
MlyValue.RULE (((
map (fn {rhs,code,prec} => {lhs=ID,rhs=rev rhs,code=code,prec=prec})
	 RHS_LIST
)))
in (LrTable.NT 9,(result,ID1left,RHS_LIST1right),rest671)
end
| (27,(_,(MlyValue.RULE (RULE1 as RULE),RULEleft as RULE1left,
RULEright as RULE1right)) :: (_,(MlyValue.RULE_LIST (RULE_LIST1
 as RULE_LIST),RULE_LISTleft as RULE_LIST1left,RULE_LISTright as 
RULE_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RULE_LIST (((RULE@RULE_LIST)))
in (LrTable.NT 10,(result,RULE_LIST1left,RULE1right),rest671)
end
| (28,(_,(MlyValue.RULE (RULE1 as RULE),RULEleft as RULE1left,
RULEright as RULE1right)) :: rest671) =>
let val result = 
MlyValue.RULE_LIST (((RULE)))
in (LrTable.NT 10,(result,RULE1left,RULE1right),rest671)
end
| (29,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: (_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),
ID_LISTleft as ID_LIST1left,ID_LISTright as ID_LIST1right)) :: rest671) =>
let val result = 
MlyValue.ID_LIST (((ID::ID_LIST)))
in (LrTable.NT 2,(result,ID_LIST1left,ID1right),rest671)
end
| (30,rest671) =>
let val result = 
MlyValue.ID_LIST (((nil)))
in (LrTable.NT 2,(result,defaultPos,defaultPos),rest671)
end
| (31,(_,(MlyValue.PROG (PROG1 as PROG),PROGleft as PROG1left,
PROGright as PROG1right)) :: (_,(MlyValue.RULE_PREC (RULE_PREC1
 as RULE_PREC),RULE_PRECleft as RULE_PREC1left,RULE_PRECright as 
RULE_PREC1right)) :: (_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),
ID_LISTleft as ID_LIST1left,ID_LISTright as ID_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RHS_LIST ((([{rhs=ID_LIST,code=PROG,prec=RULE_PREC}])))
in (LrTable.NT 8,(result,ID_LIST1left,PROG1right),rest671)
end
| (32,(_,(MlyValue.PROG (PROG1 as PROG),PROGleft as PROG1left,
PROGright as PROG1right)) :: (_,(MlyValue.RULE_PREC (RULE_PREC1
 as RULE_PREC),RULE_PRECleft as RULE_PREC1left,RULE_PRECright as 
RULE_PREC1right)) :: (_,(MlyValue.ID_LIST (ID_LIST1 as ID_LIST),
ID_LISTleft as ID_LIST1left,ID_LISTright as ID_LIST1right)) :: (_,(_,
BARleft as BAR1left,BARright as BAR1right)) :: (_,(MlyValue.RHS_LIST (
RHS_LIST1 as RHS_LIST),RHS_LISTleft as RHS_LIST1left,RHS_LISTright as 
RHS_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RHS_LIST ((({rhs=ID_LIST,code=PROG,prec=RULE_PREC}::RHS_LIST)))
in (LrTable.NT 8,(result,RHS_LIST1left,PROG1right),rest671)
end
| (33,(_,(MlyValue.TYVAR (TYVAR1 as TYVAR),TYVARleft as TYVAR1left,
TYVARright as TYVAR1right)) :: rest671) =>
let val result = 
MlyValue.TY (((TYVAR)))
in (LrTable.NT 13,(result,TYVAR1left,TYVAR1right),rest671)
end
| (34,(_,(_,RBRACEleft as RBRACE1left,RBRACEright as RBRACE1right
)) :: (_,(MlyValue.RECORD_LIST (RECORD_LIST1 as RECORD_LIST),
RECORD_LISTleft as RECORD_LIST1left,RECORD_LISTright as 
RECORD_LIST1right)) :: (_,(_,LBRACEleft as LBRACE1left,LBRACEright as 
LBRACE1right)) :: rest671) =>
let val result = 
MlyValue.TY ((("{ "^RECORD_LIST^" } ")))
in (LrTable.NT 13,(result,LBRACE1left,RBRACE1right),rest671)
end
| (35,(_,(_,RBRACEleft as RBRACE1left,RBRACEright as RBRACE1right
)) :: (_,(_,LBRACEleft as LBRACE1left,LBRACEright as LBRACE1right
)) :: rest671) =>
let val result = 
MlyValue.TY ((("{}")))
in (LrTable.NT 13,(result,LBRACE1left,RBRACE1right),rest671)
end
| (36,(_,(MlyValue.PROG (PROG1 as PROG),PROGleft as PROG1left,
PROGright as PROG1right)) :: rest671) =>
let val result = 
MlyValue.TY (((" ( "^PROG^" ) ")))
in (LrTable.NT 13,(result,PROG1left,PROG1right),rest671)
end
| (37,(_,(MlyValue.QUAL_ID (QUAL_ID1 as QUAL_ID),QUAL_IDleft as 
QUAL_ID1left,QUAL_IDright as QUAL_ID1right)) :: (_,(MlyValue.TY (TY1
 as TY),TYleft as TY1left,TYright as TY1right)) :: rest671) =>
let val result = 
MlyValue.TY (((TY^" "^QUAL_ID)))
in (LrTable.NT 13,(result,TY1left,QUAL_ID1right),rest671)
end
| (38,(_,(MlyValue.QUAL_ID (QUAL_ID1 as QUAL_ID),QUAL_IDleft as 
QUAL_ID1left,QUAL_IDright as QUAL_ID1right)) :: rest671) =>
let val result = 
MlyValue.TY (((QUAL_ID)))
in (LrTable.NT 13,(result,QUAL_ID1left,QUAL_ID1right),rest671)
end
| (39,(_,(MlyValue.TY (TY2),TY2left,TY2right)) :: (_,(_,
ASTERISKleft as ASTERISK1left,ASTERISKright as ASTERISK1right)) :: 
(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as TY1right
)) :: rest671) =>
let val result = 
MlyValue.TY (((TY1^"*"^TY2)))
in (LrTable.NT 13,(result,TY1left,TY2right),rest671)
end
| (40,(_,(MlyValue.TY (TY2),TY2left,TY2right)) :: (_,(_,ARROWleft as 
ARROW1left,ARROWright as ARROW1right)) :: (_,(MlyValue.TY (TY1 as TY),
TYleft as TY1left,TYright as TY1right)) :: rest671) =>
let val result = 
MlyValue.TY (((TY1 ^ " -> " ^ TY2)))
in (LrTable.NT 13,(result,TY1left,TY2right),rest671)
end
| (41,(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as 
TY1right)) :: (_,(_,COLONleft as COLON1left,COLONright as COLON1right
)) :: (_,(MlyValue.LABEL (LABEL1 as LABEL),LABELleft as LABEL1left,
LABELright as LABEL1right)) :: (_,(_,COMMAleft as COMMA1left,
COMMAright as COMMA1right)) :: (_,(MlyValue.RECORD_LIST (RECORD_LIST1
 as RECORD_LIST),RECORD_LISTleft as RECORD_LIST1left,
RECORD_LISTright as RECORD_LIST1right)) :: rest671) =>
let val result = 
MlyValue.RECORD_LIST (((RECORD_LIST^","^LABEL^":"^TY)))
in (LrTable.NT 7,(result,RECORD_LIST1left,TY1right),rest671)
end
| (42,(_,(MlyValue.TY (TY1 as TY),TYleft as TY1left,TYright as 
TY1right)) :: (_,(_,COLONleft as COLON1left,COLONright as COLON1right
)) :: (_,(MlyValue.LABEL (LABEL1 as LABEL),LABELleft as LABEL1left,
LABELright as LABEL1right)) :: rest671) =>
let val result = 
MlyValue.RECORD_LIST (((LABEL^":"^TY)))
in (LrTable.NT 7,(result,LABEL1left,TY1right),rest671)
end
| (43,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: rest671) =>
let val result = 
MlyValue.QUAL_ID ((((fn (a,_) => a) ID)))
in (LrTable.NT 6,(result,ID1left,ID1right),rest671)
end
| (44,(_,(MlyValue.QUAL_ID (QUAL_ID1 as QUAL_ID),QUAL_IDleft as 
QUAL_ID1left,QUAL_IDright as QUAL_ID1right)) :: (_,(MlyValue.IDDOT (
IDDOT1 as IDDOT),IDDOTleft as IDDOT1left,IDDOTright as IDDOT1right
)) :: rest671) =>
let val result = 
MlyValue.QUAL_ID (((IDDOT^QUAL_ID)))
in (LrTable.NT 6,(result,IDDOT1left,QUAL_ID1right),rest671)
end
| (45,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: rest671) =>
let val result = 
MlyValue.LABEL ((((fn (a,_) => a) ID)))
in (LrTable.NT 3,(result,ID1left,ID1right),rest671)
end
| (46,(_,(MlyValue.INT (INT1 as INT),INTleft as INT1left,INTright as 
INT1right)) :: rest671) =>
let val result = 
MlyValue.LABEL (((INT)))
in (LrTable.NT 3,(result,INT1left,INT1right),rest671)
end
| (47,(_,(MlyValue.ID (ID1 as ID),IDleft as ID1left,IDright as 
ID1right)) :: (_,(_,PREC_TAGleft as PREC_TAG1left,PREC_TAGright as 
PREC_TAG1right)) :: rest671) =>
let val result = 
MlyValue.RULE_PREC (((SOME ID)))
in (LrTable.NT 11,(result,PREC_TAG1left,ID1right),rest671)
end
| (48,rest671) =>
let val result = 
MlyValue.RULE_PREC (((NONE)))
in (LrTable.NT 11,(result,defaultPos,defaultPos),rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.BEGIN x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a 
end
end
structure Tokens : Mlyacc_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ASTERISK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun BLOCK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun BAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun DELIMITER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun HEADER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.HEADER i,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.ID i,p1,p2))
fun IDDOT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.IDDOT i,p1,p2))
fun PERCENT_HEADER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.INT i,p1,p2))
fun KEYWORD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NODEFAULT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun NONTERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun NOSHIFT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_EOP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_PURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_POS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PERCENT_ARG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun PREC (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.PREC i,p1,p2))
fun PREC_TAG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun PREFER (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun PROG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.PROG i,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun SUBST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun START (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun TYVAR (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.TYVAR i,p1,p2))
fun VERBOSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun VALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun UNKNOWN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.UNKNOWN i,p1,p2))
fun BOGUS_VALUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
end
end
