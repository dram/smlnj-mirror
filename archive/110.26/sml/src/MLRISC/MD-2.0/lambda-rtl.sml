(* 
 * 
 *)
structure LambdaRTL : LAMBDA_RTL =
struct
   structure RTL  = MLTreeRTL
   structure T    = RTL.T
   structure Util = RTL.Util

   type rtl = string * string list * RTL.action

   datatype object = OBJ of name * RTL.ty * kind
   and kind = OPND | LAB | IMM | REG of cellkind | REGS of cellkind 
            | MEM | CELLSET | REGION | UNUSED
   and name = PARAM of string
            | CONST of int
   withtype cellkind = string
        and defUse = object list * object list

   datatype inOut = IN_OUT of 
       {name:string, input:int option, output:int option, obj:object}

   structure ObjectSet =
   struct
       infix @@ join
       fun compare(OBJ(x,_,a),OBJ(y,_,b)) = 
           (case cmp'(a,b) of
              EQUAL => cmp(x,y)
            | x => x
           )
       and cmp(PARAM x,PARAM y) = String.compare(x,y)
         | cmp(CONST i,CONST j) = Int.compare(i,j)
         | cmp(PARAM _,CONST _) = LESS
         | cmp(CONST _,PARAM _) = GREATER

       and cmp'(a,b) = Int.compare(num a, num b)

       and num(OPND)      = 1
         | num(LAB)       = 2 
         | num(IMM)       = 3 
         | num(REG _)     = 4 
         | num(REGS _)    = 5 
         | num(MEM)       = 6 
         | num(CELLSET)   = 7 
         | num(REGION)    = 8
         | num(UNUSED)    = 9 

       val uniq = ListMergeSort.uniqueSort compare 
       fun lt(x,y) = compare(x,y) = LESS
       fun diff(x,y) =
           let fun loop([],   ys) = []
                 | loop(xs,   []) = xs
                 | loop(x::xs, y::ys) = if x = y then loop(xs,ys)
                                        else if lt(x,y) then x::loop(xs,y::ys)
                                        else loop(x::xs, ys)
           in  loop(uniq x, uniq y) end

       fun (d1,u1) @@ (d2,u2) = (d1 @ d2, u1 @ u2)

       fun (d1,u1) join (d2,u2) =
       let val delta1 = diff(d1, d2)
           val delta2 = diff(d2, d1)
       in  (d1 @ d2, u1 @ u2 @ delta1 @ delta2)
       end
   end

   open MDError ObjectSet

   fun defUse (f, args, a) =
   let infix @@ join
       val wtoi = Word32.toInt
       fun stm(T.SEQ [])               = ([], [])
         | stm(T.EXT(RTL.ASSIGN(x,y))) = lhsLoc x @@ ([], rexp y)
         | stm(T.IF(_,x,y,z))          = ([], ccexp x) @@ (stm y join stm z)
         | stm(T.JMP(_,x,_))           = ([], rexp x)
         | stm(T.CALL{funct, ...})      = ([], rexp funct)
         | stm(T.RET _)                = ([], [])
         | stm(T.EXT(RTL.PAR(a,b)))    = stm a @@ stm b
         | stm(T.COPY(_,dst,src))      = ([], [])

       and rexp e = 
           case e of
           T.REG(t,x) => [] (* XXX *)
         | T.LI _ => []
         | T.LI32 _ => []
         | T.LI64 _ => []
         | T.LABEL _ => []
         | T.CONST _ => [] 
         | T.NEG(_,x) => rexp x 
         | T.ADD(_,x,y) => rexp x @ rexp y
         | T.SUB(_,x,y) => rexp x @ rexp y
         | T.MULS(_,x,y) => rexp x @ rexp y
         | T.DIVS(_,x,y) => rexp x @ rexp y
         | T.QUOTS(_,x,y) => rexp x @ rexp y
         | T.REMS(_,x,y) => rexp x @ rexp y
         | T.MULU(_,x,y) => rexp x @ rexp y
         | T.DIVU(_,x,y) => rexp x @ rexp y
         | T.REMU(_,x,y) => rexp x @ rexp y
         | T.NEGT(_,x) => rexp x 
         | T.ADDT(_,x,y) => rexp x @ rexp y
         | T.SUBT(_,x,y) => rexp x @ rexp y
         | T.MULT(_,x,y) => rexp x @ rexp y
         | T.DIVT(_,x,y) => rexp x @ rexp y
         | T.QUOTT(_,x,y) => rexp x @ rexp y
         | T.REMT(_,x,y) => rexp x @ rexp y
         | T.ANDB(_,x,y) => rexp x @ rexp y
         | T.ORB(_,x,y) => rexp x @ rexp y
         | T.XORB(_,x,y) => rexp x @ rexp y
         | T.NOTB(_,x) => rexp x 
         | T.SRA(_,x,y) => rexp x @ rexp y
         | T.SRL(_,x,y) => rexp x @ rexp y
         | T.SLL(_,x,y) => rexp x @ rexp y
         | T.CVTI2I(_,_,_,e) => rexp e
         | T.COND(_,a,b,c) => ccexp a @ rexp b @ rexp c
         | T.LOAD(_,e,_) => rexp e
         | T.MARK(e,_) => rexp e
         | T.REXT(_,RTL.FORALL e) => rexp e
         | T.REXT(ty,RTL.SLICE(_,_,e)) => rexp e
         | T.REXT(ty,RTL.OP(_,es)) => rexps es
         | T.REXT(ty,RTL.ARG("opn",n)) => [OBJ(PARAM n,ty,OPND)]
         (*| T.REXT(ty,RTL.ARG("lab",n)) => [OBJ(PARAM n,ty,LAB)]*)
         | T.REXT(ty,RTL.ARG("lab",n)) => []
         | T.REXT(ty,RTL.ARG("imm",n)) => [OBJ(PARAM n,ty,IMM)]
         | T.REXT(ty,RTL.ARG("cellset",n)) => [OBJ(PARAM n,ty,CELLSET)]
         | T.REXT(ty,RTL.ARG("region",n)) => [OBJ(PARAM n,ty,REGION)]
         | T.REXT(ty,RTL.FETCH loc) => rhsLoc loc

       and rexps es = List.concat(map rexp es)

       and ccexp e =
            case e of
            T.CMP(_,_,x,y) => rexp x @ rexp y
          | T.NOT x => ccexp x
          | T.AND(x,y) => ccexp x @ ccexp y
          | T.XOR(x,y) => ccexp x @ ccexp y
          | T.OR(x,y) => ccexp x @ ccexp y
          | T.TRUE => []
          | T.FALSE => []
          | _ => fail "ccexp"

       and ccexps es = List.concat(map ccexp es)

       and lhsLoc(RTL.AGG(_,_,c)) = lhsCell c

       and lhsCell(RTL.CELL("MEM",_,e,region)) = (rexp region, rexp e)
         | lhsCell(RTL.CELL("cellset",ty,T.REXT(_,RTL.ARG(_,r)),region)) =
              (rexp region @ [OBJ(PARAM r,ty,CELLSET)],[])
         | lhsCell(cell as RTL.CELL("cellset",_,_,_)) = badLhsCell cell
         | lhsCell(RTL.CELL(k,ty,T.REXT(_,RTL.FORALL
             (T.REXT(_,RTL.ARG("reg",r)))),_)) = 
             ([OBJ(PARAM r,ty,REGS k)],[])
         | lhsCell(RTL.CELL(k,ty,T.REXT(_,RTL.ARG("reg",r)),_)) = 
             ([OBJ(PARAM r,ty,REG k)],[])
         | lhsCell(RTL.CELL(k,ty,T.LI32 w,_)) = 
             ([OBJ(CONST(wtoi w),ty,REG k)],[])
         | lhsCell(cell) = badLhsCell cell

       and badLhsCell(RTL.CELL(k,_,e,_)) = 
            fail("lhs $"^k^"["^Util.rexpToString e^"]")

       and rhsLoc(RTL.AGG(_,_,c)) = rhsCell c

       and rhsCell(RTL.CELL("MEM",_,e,r))       = rexp e @ rexp r
         | rhsCell(RTL.CELL("cellset",ty,T.REXT(_,RTL.ARG(_,r)),region)) = 
             [OBJ(PARAM r,ty,CELLSET)] @ rexp region
         | rhsCell(cell as RTL.CELL("cellset",_,_,_)) = badRhsCell cell
         | rhsCell(RTL.CELL(k,ty,T.REXT(_,
              RTL.FORALL(T.REXT(_,RTL.ARG("reg",r)))),_)) = 
             [OBJ(PARAM r,ty,REGS k)]
         | rhsCell(RTL.CELL(k,ty,T.REXT(_,RTL.ARG("reg",r)),_)) = 
             [OBJ(PARAM r,ty,REG k)]
         | rhsCell(RTL.CELL(k,ty,T.LI32 w,_)) = [OBJ(CONST(wtoi w),ty,REG k)]
         | rhsCell(cell) = badRhsCell cell

       and badRhsCell(RTL.CELL(k,_,e,_)) = 
            fail("rhs $"^k^"["^Util.rexpToString e^"]")
       val (d, u) = stm a
   in  (uniq d, uniq u) end

   fun typToString(n) = "#"^Int.toString n
   fun nameToString(PARAM x) = x
     | nameToString(CONST i) = Int.toString i
   fun kindToString(OPND)= "opnd"
     | kindToString(LAB)= "lab"
     | kindToString(IMM)= "imm"
     | kindToString(REG k)= k
     | kindToString(REGS k)= k^"s"
     | kindToString(MEM)= "mem"
     | kindToString(CELLSET)= "cellset"
     | kindToString(REGION)= "region"
     | kindToString(UNUSED)= "unused"
   fun objectToString(OBJ(name,ty,kind)) = 
        nameToString name^":"^typToString ty^" "^kindToString kind
   fun listify f =
   let fun g [] = ""
         | g [x] = f x
         | g (x::xs) = f x^","^g xs
   in  g end
 
   fun defUseToString(defs,uses) = 
   let val prList = listify objectToString
   in  "defs=["^prList defs^"] uses=["^prList uses^"]" 
   end

   fun inOut (rtl as (name,args,body)) =
   let val (def,use) = defUse rtl 
       fun look(x,[],n) = NONE
         | look(x,(obj as OBJ(PARAM x',_,_))::objs,n) =
             if x = x' then SOME(x,n,obj) else look(x,objs,n+1)
         | look(x,_::objs,n) = look(x,objs,n+1)
       fun scan x = 
        case (look(x,def,0),look(x,use,0)) of
          (NONE,NONE) => 
             IN_OUT{name=x,obj=OBJ(PARAM x,0,UNUSED), input= NONE, output= NONE}
        | (SOME(x,n,obj),NONE) => 
             IN_OUT{name=x,obj=obj,output=SOME n,input=NONE}
        | (NONE,SOME(x,n,obj)) => 
             IN_OUT{name=x,obj=obj,input=SOME n,output=NONE}
        | (SOME(_,m,_),SOME(x,n,obj)) =>
             IN_OUT{name=x,obj=obj,input=SOME n,output=SOME m}
   in  map scan args end          
 
end
