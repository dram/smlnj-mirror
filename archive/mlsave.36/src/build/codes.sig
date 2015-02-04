signature CODEGENERATOR =
sig
 val generate : Lambda.lexp -> string
end

signature ASSEMBLER =
sig 
 val generate : Lambda.lexp * outstream -> unit
end
