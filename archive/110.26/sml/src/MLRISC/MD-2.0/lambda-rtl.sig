(*
 * Simple lambda syntax
 *)
signature LAMBDA_RTL =
sig
   structure RTL : MLTREE_RTL

   type rtl = string * string list * RTL.action

   datatype object = OBJ of name * RTL.ty * kind
   and kind = OPND | LAB | IMM | REG of cellkind | REGS of cellkind 
            | MEM | CELLSET | REGION | UNUSED
   and name = PARAM of string
            | CONST of int

   withtype cellkind = string
        and defUse = object list * object list

   val defUse : rtl -> defUse

   datatype inOut = IN_OUT of 
       {name:string, obj:object, input:int option, output:int option}

   val inOut : rtl -> inOut list 

   (* Pretty printing *)
   val defUseToString : defUse -> string
end
