(* Copyright 1989 by AT&T Bell Laboratories *)
signature CODEGENERATOR =
sig
 exception BadReals of string list
 val generate : Lambda.lexp -> string
end

signature ASSEMBLER =
sig 
 exception BadReals of string list
 val generate : Lambda.lexp * outstream -> unit
end
