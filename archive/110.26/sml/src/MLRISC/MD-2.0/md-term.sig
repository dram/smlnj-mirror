signature MD_TERM =
sig
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

   val tyvar : unit -> typ

   val defUse : rtl -> (string * kind) list * (string * kind) list
   val kindToString : kind -> string
   val defUseToString : (string * kind) list * (string * kind) list -> string
end
