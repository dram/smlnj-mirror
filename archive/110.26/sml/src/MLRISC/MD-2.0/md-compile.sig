(*
 * Compile the machine description into an internal digestable form
 *)
signature MD_COMPILE =
sig

   structure Ast   : MD_AST
   structure Env   : MD_ENV
   structure Util  : MD_AST_UTIL
   structure Trans : MD_TRANS
   structure AstPP : MD_PP
   structure Error : MD_ERROR
      sharing Ast = Env.Ast = Util.Ast = Trans.Ast = AstPP.Ast

   type md  (* machine description *)
   type filename = string

   (* Extract info from a machine description *)
   val endianess : md -> Ast.endianess    (* endianess *)
   val archKind  : md -> Ast.archKind     (* kind of architecture *)
   val asmCase   : md -> Ast.assemblycase (* assembly case *)
   val name      : md -> string           (* name of description *)
   val filename  : md -> string           (* filename *)
   val version   : md -> string           (* version of description *)
   val env       : md -> Env.env          (* environment *)
   val delayslots: md -> int              (* size of delay slots *)
   val cells     : md -> Ast.storagedecl list 
   val cellSets  : md -> Ast.storagedecl list (* all cellkinds with cellsets *)
   val cellSetsAliases : md -> Ast.storagedecl list (* include all aliases *)
   val locations : md -> Ast.locbind list 
   val formats   : md -> (int * Ast.formatbind) list 
   val debugging : md -> string -> bool
   val lookupCellKind : md -> string -> Ast.storagedecl 
   val lookupDatatype : md -> string -> Ast.datatypebind 
   val hasCopyImpl : md -> bool

   val resources : md -> Ast.id list
   val pipelines : md -> Ast.pipelinebind list
   val cpus      : md -> Ast.cpubind list
   val latencies : md -> Ast.latencybind list

   (* Compile an AST into a machine description *)
   val compile   : filename * Ast.decl list -> md

   (* Simplification *)
   val simpExp   : Ast.exp -> Ast.exp
   val simpDecl  : Ast.decl -> Ast.decl
   val stripMarks : Ast.decl -> Ast.decl

   (* Generating constants *)
   type constTable 
   val newConstTable : unit -> constTable
   val const         : constTable -> Ast.exp -> Ast.exp
   val genConsts     : constTable -> Ast.decl list
   val withConsts    : ((Ast.exp -> Ast.exp) -> Ast.decl) -> Ast.decl

   (* Extract info from the environment *)
   val declOf    : md -> string -> Ast.decl  (* body of structure *)
   val fctArgOf  : md -> string -> Ast.decl  (* functor argument *)
   val typeOf    : md -> string -> Ast.decl  (* type definitions *)  
   val instructions : md -> Ast.consbind list 

   (* Code generation functions *)
   type module = string
   type arguments = string list
   type signatureName = string
   val signame   : md -> module -> string
   val strname   : md -> module -> string
   val mkCode    : Ast.decl list -> PP.pp 
   val mkStr     : md -> string -> signatureName -> Ast.decl list -> PP.pp
   val mkSig     : md -> module -> Ast.decl list -> PP.pp
   val mkFct     : md -> module -> arguments -> signatureName -> 
                    Ast.decl list -> PP.pp
   val mkFct'    : md -> module -> Ast.decl -> signatureName ->  
                    Ast.decl list -> PP.pp
   val outfile   : md -> module -> string -> string -> unit
   val pathName  : md -> module -> string -> string
   val codegen   : md -> module -> PP.pp list -> unit

end
