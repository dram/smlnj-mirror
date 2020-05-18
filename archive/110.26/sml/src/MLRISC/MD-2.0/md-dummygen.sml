(*
 * Placeholder for undefined modules
 *)
functor MDDummyGen(Comp : MD_COMPILE) : MD_GEN_MODULE =
struct
   structure Ast  = Comp.Ast
   structure Comp = Comp
   fun gen _ = ()
end
