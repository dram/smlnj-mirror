signature TARGET_CONVENTIONS = sig
  structure M : MLTREE
  structure F : FRAME

  val mkCall :
    {frame: F.frame,
     proc : M.rexp,
     nArgs : int,
     defs : M.mlrisc list,
     uses : M.mlrisc list
    } -> M.stm list

  val functionPrologue : unit -> M.stm list

  val functionEpilogue : unit -> M.stm list 

  val memArgs : int * Tree.exp list -> Tree.stm
end
