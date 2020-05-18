(*
 * This takes a bunch of RTL and build a database that can be reused.
 *)
structure MDBuildRTL : BUILD_RTL =
struct
   structure L   = LambdaRTL
   structure RTL = L.RTL
   structure T   = RTL.T
   open L 

   type ty = int

   fun newOper name = ref{name=name,hash=0w0,attribs=0w0}

   fun wordConst ty w = T.LI32(w)
   fun intConst ty i = wordConst ty (Word32.fromInt i)

   exception BuildRTL

   fun error msg = (MDError.error("error in "^msg); raise BuildRTL)

   fun fetch ty loc = T.FETCH(ty,loc)

   fun get name (T.FETCH(ty,T.AGG(_,_,T.CELL("MEM",_,ea)))) = 
             T.LOAD(ty,ea,())
     | get name (T.FETCH(ty,T.AGG(_,_,T.CELL("GP",_,T.LI32 r)))) =
             T.REG(ty,Word32.toInt r)
     | get name e  = e

   fun getCC name x = x

     (*
     | get name (T.FETCH _)  = error("get: FETCH "^name)
     | get name (T.FPR _)  = error("get: FPR "^name)
     | get name (T.CCR _)  = error("get: CCR "^name)
      *)

   fun op:= ty (x,y) = T.ASSIGN(x,y)
   fun $ (k,ty) addr = T.CELL(k,ty,get "$" addr)

   fun aggb (t1,t2) cell = T.AGG(t2,T.BIG_ENDIAN,cell)
   fun aggl (t1,t2) cell = T.AGG(t2,T.LITTLE_ENDIAN,cell)
   fun idaggr t cell     = T.AGG(t,T.LITTLE_ENDIAN,cell)
   val dummyTy = 32

   fun ! x = T.ARG(dummyTy,"reg",x)

   (* Integer operators *)
   fun unary f ty (x : RTL.exp)= f(ty,get "unary" x) : RTL.exp
   fun binary f ty (x : RTL.exp,y : RTL.exp) = 
        f(ty,get "binary" x,get "binary" y) : RTL.exp

   fun operand ty opn = T.ARG(ty,"opn",opn)
   fun label ty label = T.ARG(ty,"lab",label)

   datatype kind = GP | FP | CC

   fun newOp name = 
   let val oper = newOper name 
   in  fn xs => T.OP(32,oper,xs) : RTL.exp 
   end

   val newCond = newOp

   fun sx (t1,t2) e = T.SX(t2,t1,get "sx" e)
   fun zx (t1,t2) e = T.ZX(t2,t1,get "zx" e)
   fun ? ty = newOp "?" []

   fun bitslice t2 ranges e =
       let val t1 = foldr (fn ((a,b),l) => b-a+1+l) 0 ranges
           val r =  map (fn (a,b) => {from=T.LI a,to=T.LI b}) ranges
       in  T.SLICE(t1,r,t2,get "bitslice" e) end

   val not   = T.NOT
   val False = T.FALSE
   val True  = T.TRUE

   val op +  = binary T.ADD
   val op -  = binary T.SUB
   val muls  = binary T.MULS
   val mulu  = binary T.MULU
   val divs  = binary T.DIVS
   val divu  = binary T.DIVU
   val rems  = binary T.REMS
   val remu  = binary T.REMU
   fun ~ ty x = (op - ty) (intConst ty 0,x)

   val andb  = binary T.ANDB
   val orb   = binary T.ORB
   val xorb  = binary T.XORB
   val notb  = unary  T.NOTB
   val <<    = binary T.SLL
   val >>    = binary T.SRL
   val ~>>   = binary T.SRA
   fun eqvb ty (x,y) = notb ty (xorb ty (x,y))

   (* Trapping operators *)
   val addt  = binary T.ADDT
   val subt  = binary T.SUBT
   val mult  = binary T.MULT
   val divt  = binary T.DIVT
   val remt  = binary T.REMT

   fun cond ty (x,y,z) = 
       T.COND(ty, getCC "cond" x, get "cond" y, get "cond" z) : RTL.exp

   (* Integer comparisons *)
   fun cmp cond ty (x : RTL.exp,y : RTL.exp) =
       let val get = get (T.Basis.condToString cond)
       in  T.CMP(ty,cond,get x,get y) : RTL.cond end

   val ==    = cmp T.EQ
   val op <> = cmp T.NE
   val op >  = cmp T.GT
   val op <  = cmp T.LT
   val op <= = cmp T.LE
   val op >= = cmp T.GT
   val ltu   = cmp T.LTU
   val leu   = cmp T.LEU
   val gtu   = cmp T.GTU
   val geu   = cmp T.GEU

   (* Floating point operators *)
   fun funary f =
   let val oper = newOper f 
   in  fn ty => fn (x : RTL.exp) => T.OP(ty,oper,[get "funary" x]) : RTL.exp end
   fun fbinary f =
   let val oper = newOper f 
   in  fn ty => fn (x : RTL.exp, y:RTL.exp) => 
         T.OP(ty,oper,[get "binary" x, get "binary" y]) : RTL.exp
   end

   val fadd  = fbinary "FADD"
   val fsub  = fbinary "FSUB"
   val fmul  = fbinary "FMUL"
   val fdiv  = fbinary "FDIV"
   val fabs  = funary  "FABS"
   val fneg  = funary  "FNEG"
   val fsqrt = funary  "FSQRT"

   (* Floating point comparisons *)
   fun fcmp fcond =
   let val name = T.Basis.fcondToString fcond
       val oper = newOper name
       val get  = get name
   in  fn ty => fn (x : RTL.exp, y : RTL.exp) => 
          T.CMP(ty,T.NE,T.OP(ty,oper,[get x,get y]),T.LI 0) : RTL.cond
   end

   val |?|     = fcmp T.?
   val |!<=>|  = fcmp T.!<=>
   val |==|    = fcmp T.==
   val |?=|    = fcmp T.?=
   val |!<>|   = fcmp T.!<>
   val |!?>=|  = fcmp T.!?>=
   val |<|     = fcmp T.<
   val |?<|    = fcmp T.?<
   val |!>=|   = fcmp T.!>=
   val |!?>|   = fcmp T.!?>
   val |<=|    = fcmp T.<=
   val |?<=|   = fcmp T.?<=
   val |!>|    = fcmp T.!>
   val |!?<=|  = fcmp T.!?<=
   val |>|     = fcmp T.>
   val |?>|    = fcmp T.?>
   val |!<=|   = fcmp T.!<=
   val |!?<|   = fcmp T.!?<
   val |>=|    = fcmp T.>=
   val |?>=|   = fcmp T.?>=
   val |!<|    = fcmp T.!<
   val |!?=|   = fcmp T.!?=
   val |<>|    = fcmp T.<>
   val |!=|    = fcmp T.!=
   val |!?|    = fcmp T.!?
   val |<=>|   = fcmp T.<=>
   val |?<>|   = fcmp T.?<>

   (* Action combinators *)
   val ||    = T.PAR
   val Nop   = T.SEQ []
   fun Jmp  ty e = T.JMP([],get "Jmp" e,[])
   fun Call ty e = T.CALL(get "Call" e,[],[],[],[],[],())
   val Ret   = T.RET([],[])
   fun If(x,y,z) = T.IF([],getCC "If" x,y,z)

   fun map ty = List.map
end
