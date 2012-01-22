(* bug631.sml *)
(* PrintVal can't print a value of the Ast type *)

structure Interface = struct
  local
    open Environment
  in fun ast name =
	 let val f = TextIO.openIn name
	     val source = Source.newSource (name,f,false,
                             ErrorMsg.defaultConsumer())
	       
	  in SmlFile.parse source
	 end
  end
end;

				(* return ast of this file *)
Interface.ast "bug631.sml";  

