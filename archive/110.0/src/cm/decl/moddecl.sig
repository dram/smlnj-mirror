(*
 * decl/moddecl.sig:
 *
 *   Copyright (c) 1995 by AT&T Bell Laboratories
 *   Copyright (c) 1993 by Carnegie Mellon University,
 *                         School of Computer Science
 *                         contact: Gene Rollins (rollins+@cs.cmu.edu)
 *
 * contact: Matthias Blume (blume@cs.princeton.edu)
 *)
signature MODDECL = sig

    structure ModuleName: MODULE_NAME

    datatype decl =
	StrDecl of {
		    name: ModuleName.t,
		    def: strExp,
		    constraint: strExp option
		   } list    
      | FctDecl of { name: ModuleName.t, def:fctExp } list    
      | LocalDecl of decl * decl
      | SeqDecl of decl list    
      | OpenDecl of strExp list
      | DeclRef of ModuleName.set

    and strExp = 
	VarStrExp of ModuleName.path   
      | BaseStrExp of decl
      | AppStrExp of ModuleName.path * strExp list
      | LetStrExp of decl * strExp  
      | AugStrExp of strExp * ModuleName.set
      | ConStrExp of strExp * strExp

    and fctExp = 
	VarFctExp of ModuleName.path * fctExp option 
      | BaseFctExp of {
		       params: (ModuleName.t option * strExp) list,
		       body: strExp,
		       constraint: strExp option
		      }
      | AppFctExp of ModuleName.path * strExp list * fctExp option
      | LetFctExp of decl * fctExp

end
