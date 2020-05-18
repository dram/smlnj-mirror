structure MDTerm : MD_TERM =
struct

   datatype typ = TYP of string option * int option
                | TYVAR of typ option ref

   and stm = 
      ACTION of string * exp list
   |  ASSIGN of typ * cell * exp
   |  CCASSIGN of typ * cell * cond
   |  PAR    of stm * stm 
   |  SEQ    of stm * stm
   |  IF     of cond * stm * stm 
   |  JMP    of exp
   |  CALL   of exp
   |  RET
   |  NOP  

   and exp = OP of typ * string * exp list
           | DEREF of typ * cell
           | DEREFOPND of typ * operand
           | LAB of typ * label
           | LI of typ * int
           | LI32 of typ * Word32.word
           | COND of typ * cond * exp * exp
           | VAR of typ * exp
           | UNDEF

   and cond = CMP of typ * string * exp list
            | CCDEREF of typ * cell

   and cell = REG of typ * string 
            | CCREG of typ * string
            | MEM of typ * exp

   and operand = OPND of typ * string

   and kind = REGISTER | MEMORY | CELLSET | OPERAND | LABEL

   withtype label = string

   type rtl = string * string list * stm

   fun tyvar() = TYVAR(ref NONE)

   fun kindToString REGISTER = "reg"
     | kindToString MEMORY   = "mem"
     | kindToString CELLSET  = "cellset"
     | kindToString OPERAND  = "operand"
     | kindToString LABEL    = "label"

   fun defUseToString(d,u) =
   let fun pr(r,k) = r^":"^kindToString k
       fun prList xs = foldr (fn (x,"") => pr x | (x,l) => pr x^", "^l) "" xs
   in  "defs="^prList d^" uses="^prList u end

   (* Compute def/use *)
   fun defUse(f, args, a) =
   let infix @@ join
       fun uniq x = ListMergeSort.uniqueSort 
                     (fn ((x,_),(y,_)) => String.compare(x,y)) x

       fun eq((x:string,_),(y,_)) = x=y
       fun lt((x:string,_),(y,_)) = String.<(x,y)
       fun diff(x,y) = 
           let fun loop([],   ys) = []
                 | loop(xs,   []) = xs
                 | loop(x::xs, y::ys) = if eq(x, y) then loop(xs,ys)
                                        else if lt(x,y) then x::loop(xs,y::ys)
                                        else loop(x::xs, ys)
           in  loop(uniq x, uniq y) end
       fun (d1,u1) @@ (d2,u2)   = (d1 @ d2, u1 @ u2)

       fun (d1,u1) join (d2,u2) = 
       let val delta1 = diff(d1, d2)
           val delta2 = diff(d2, d1)
       in  (d1 @ d2, u1 @ u2 @ delta1 @ delta2)
       end

       fun stm(ACTION(_,es))  = ([], exps es)
         | stm(ASSIGN(_,x,e)) = cell x @@ ([], exp e)
         | stm(CCASSIGN(_,x,e)) = cell x @@ ([], cond e)
         | stm(PAR(a,b))      = stm a @@ stm b
         | stm(SEQ(a,b))      = stm a @@ stm b 
         | stm(IF(a,b,c))     = ([], cond a) @@ (stm b join stm c)
         | stm(JMP x)         = ([], exp x)
         | stm(CALL x)        = ([], exp x)
         | stm(RET)           = ([], [])
         | stm(NOP)           = ([], [])
       and exp(OP(_,_,es))             = exps es
         | exp(DEREF(_,REG(_,x)))      = [(x,REGISTER)]
         | exp(DEREF(_,MEM(_,addr)))   = exp addr
         | exp(DEREFOPND(_,OPND(_,x))) = [(x,OPERAND)]
         | exp(LAB(_,x))               = [(x,LABEL)]
         | exp(LI _)               = []
         | exp(LI32 _)             = []
         | exp(COND(_,a,b,c))      = cond a @ exp b @ exp c
         | exp UNDEF               = []
       and cond(CMP(_,_,xs)) = exps xs
         | cond(CCDEREF(_,REG(_,x))) = [(x,REGISTER)]
       and exps es                 = List.concat(map exp es)
       and cell(REG(_,x))          = ([(x,REGISTER)], [])
         | cell(MEM(_,addr))       = ([], exp addr)
       val (d, u) = stm a
   in (uniq d, uniq u)
   end

   open PP

   (*
   fun toString rtl = 
   let fun stm(ACTION(f,es)) = !f ++ paren(map exp es)
         | stm(ASSIGN(t,c,e)) = cell c ++ !! ":=" ++ exp e
         | stm(CCASSIGN(t,c,cd)) = cell c ++ !! "::=" ++ cond cd
         | stm(PAR(a,b)) = stm a ++ !! "||" ++ stm b
         | stm(SEQ(a,b)) = stm a ++ !"Then" ++ stm b
         | stm(IF(c,a,b)) = !"If" ++ 
               paren(cond c ++ comma ++ stm a ++ comma ++ stm b)
         | stm(JMP a) = !"Jmp" ++ paren(exp e)
         | stm(CALL a) = !"Call" ++ paren(exp e)
         | stm(RET)    = !"Ret"
         | stm(NOP)    = !"Nop"

       and exp(OP(t,f,es)) = 
         | exp(DEREF of typ * cell
         | exp(DEREFOPND of typ * operand
         | exp(LAB of typ * label
         | exp(LI of typ * int
         | exp(LI32 of typ * Word32.word
         | exp(COND of typ * cond * exp * exp
         | exp(VAR of typ * exp
         | exp(UNDEF) = ! "?"

       and cond(CMP of typ * string * exp list) = 
         | cond(CCDEREF of typ * cell) = 

       and cell(REG of typ * string) =  
         | cell(CCREG of typ * string) = 
         | cell(MEM of typ * exp) = 

       and operand(OPND of typ * string) = 

   in  stm
   end   
   *)


end
