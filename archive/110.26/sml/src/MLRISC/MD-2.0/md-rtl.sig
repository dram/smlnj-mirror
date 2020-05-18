(*
 * Process rtl descriptions
 *)
signature MD_RTL =
sig
   structure Ast  : MD_AST
   structure Comp : MD_COMPILE
     sharing Comp.Ast = Ast

   type rtlmd (* rtl in digested form *)

   val rtls    : LambdaRTL.rtl list ref

   val compile   : Comp.md -> rtlmd
   val md        : rtlmd -> Comp.md
   val gen       : rtlmd -> unit

   val getRTLs      : rtlmd -> LambdaRTL.rtl list   
   val getUserDecls : rtlmd -> Ast.decl 
   val lookupRTL    : rtlmd -> Ast.id -> LambdaRTL.rtl 

   (* code generation *)
   val queryFun     : 
        rtlmd ->
        {name       : Ast.id, 
         extraArgs  : Ast.id list,
         args       : Ast.id list,
         localDecls : Ast.decl list,
         extraExps  : Ast.id list,
         body:{instr : Ast.consbind, 
               rtl   : LambdaRTL.rtl, 
               const : Ast.exp->Ast.exp} -> 
               {pat  : Ast.pat list,
                exp  : Ast.exp
               },
         composite: {instr : Ast.consbind, id:Ast.id, ty:Ast.ty} -> Ast.exp
        } -> Ast.decl

   (* Generate an expression *)
   val queryExp : 
         rtlmd ->
         {name    : string,
          reg     : Ast.exp * string * 'a -> 'a,
          fixreg  : Ast.exp * string * 'a -> 'a,
          regs    : Ast.exp * string * 'a -> 'a,
          cellset : Ast.exp * 'a -> 'a,
          opnd    : Ast.exp * 'a -> 'a,
          lab     : Ast.exp * 'a -> 'a,
          imm     : Ast.exp * 'a -> 'a,
          region  : Ast.exp * 'a -> 'a
         } -> LambdaRTL.object list * 'a -> 'a

   exception UseDefault
   val queryPatExp : 
         rtlmd ->
         {name    : string,
          defUse  : string,
          reg     : (unit -> Ast.exp) * string * 'a -> 'a,
          fixreg  : (unit -> Ast.exp) * Ast.exp * string * 'a -> 'a,
          regs    : (unit -> Ast.exp) * string * 'a -> 'a,
          cellset : (unit -> Ast.exp) * Ast.exp * 'a -> 'a,
          opnd    : (unit -> Ast.exp) * 'a -> 'a,
          lab     : (unit -> Ast.exp) * 'a -> 'a,
          imm     : (unit -> Ast.exp) * 'a -> 'a,
          region  : (unit -> Ast.exp) * 'a -> 'a
         } -> LambdaRTL.object list * 'a -> Ast.pat * 'a

   (* Generate function for processing operands *)
   val queryOpnd : 
         rtlmd ->
         {name : string,
          extraArgs : string list,
          reg    : Ast.exp -> Ast.exp, 
          opnd   : Ast.exp -> Ast.exp,
          imm    : Ast.exp -> Ast.exp,
          default : Ast.exp 
         } -> Ast.decl

   (* Generate function for generating operands *)
   val queryGenOpnd : 
         rtlmd ->
         {name: string} ->
         {reg   : string,
          immed : string
         } 

end
