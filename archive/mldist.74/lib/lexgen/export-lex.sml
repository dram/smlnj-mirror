structure Run =
   struct
      fun lexGen (name :: file :: nil,_) = LexGen.lexGen file
        | lexGen (name :: _,_)  = output(std_out,"Usage: sml-lex " ^ "f1\n")
        | lexGen _ = output(std_out,"sml-lex: internal error!\n")
      val _ = exportFn("sml-lex",lexGen)
   end;
