signature EMITTERS = sig
    type emitter_triple
    val LittleEndian : emitter_triple
    val BigEndian : emitter_triple
    val emitted_string : unit -> string
    val MipsAsm : outstream -> emitter_triple
    val address : int ref
end

