signature MD_GEN_MODULE =
sig

   structure Ast  : MD_AST
   structure Comp : MD_COMPILE
     sharing Ast = Comp.Ast

   val gen : Comp.md -> unit

end   

signature MD_GEN2_MODULE =
sig

   structure Ast  : MD_AST
   structure Comp : MD_COMPILE
   structure RTL  : MD_RTL
     sharing Ast = Comp.Ast = RTL.Ast
     sharing Comp = RTL.Comp

   val gen : RTL.rtlmd -> unit

end 

signature MD_GEN =
sig
   val gen : string -> unit
end
