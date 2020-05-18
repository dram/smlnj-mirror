(*
 * Various translation functions
 *)
signature MD_TRANS =
sig

   structure Ast : MD_AST

   type 'a rewriter = ('a -> 'a) -> ('a -> 'a) 

   type clients = {exp  : Ast.exp rewriter,
                   decl : Ast.decl rewriter,
                   pat  : Ast.pat rewriter,
                   ty   : Ast.ty rewriter
                  }
   type trans = {exp  : Ast.exp -> Ast.exp,
                 decl : Ast.decl -> Ast.decl,
                 pat  : Ast.pat -> Ast.pat,
                 ty   : Ast.ty -> Ast.ty
                }

   val noRewrite : 'a rewriter
   val rewrite : clients -> trans
end
