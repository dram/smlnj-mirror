(* Copyright 1989 by AT&T Bell Laboratories *)
(* shareglue: a functor application common to vaxglue and m68glue, just to
   avoid duplication of this linking phase. (NICK) *)

functor IntShare(structure Machm: CODEGENERATOR
		 val fileExtension: string
		 structure D : DEBUGINTERFACE
		) =
   Interact(structure Machm = Machm
	    structure Importer =
	       Importer(structure FilePaths = UnixPths()
			val fileExtension = fileExtension
			structure ModlComp =
			   ModlComp(structure Machm = Machm))
	    structure D = D)

