(*
 * Type checking 
 *)
signature MD_TYPING =
sig
   structure Ast      : MD_AST
   structure TypeUtil : MD_TYPE_UTILS
   structure Env      : MD_ENV
   structure Comp     : MD_COMPILE
     sharing Env.Ast = TypeUtil.Ast = Ast 
     sharing Comp.Env = Env

   val isPolymorphic : Ast.ty -> bool
   val typeCheck : Comp.md -> Ast.decl -> Ast.decl * Env.env
end
