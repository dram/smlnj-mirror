signature CODEGENERATOR =
sig
 val generate : Lambda.lexp -> (int * ((int -> unit)->unit))
end

signature ASSEMBLER =
sig 
 val generate : Lambda.lexp * outstream -> unit
end
