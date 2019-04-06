(* shareglue: a functor application common to vaxglue and m68glue, just to
   avoid duplication of this linking phase. (NICK) *)

functor IntShare(Machm: CODEGENERATOR) =
   Interact(structure Machm = Machm
	    structure Importer =
	       Importer(structure ModuleComp =
			   ModuleComp(structure Absyn = BareAbsyn
				      structure Lambda = Lambda
			              structure Opt = Opt
				      structure Machm = Machm
				     )
			val statPrinter = (*PrModule.pr_module*)
					  fn _ => ""
				(* provide a function which generates a non-null
				   string, and that string will appear in a
				   .lstat file. *)
		       )
           )
