(* Copyright 1989 by AT&T Bell Laboratories *)
signature CODEGENERATOR =
sig
 val generate : Lambda.lexp * ErrorMsg.complainer -> string
end

signature ASSEMBLER =
sig 
 val generate : (Lambda.lexp * ErrorMsg.complainer) * outstream -> unit
end
