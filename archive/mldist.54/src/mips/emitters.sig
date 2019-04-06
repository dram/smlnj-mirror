signature EMITTER = sig val emit: int * int -> unit
			val order_real: string -> string
			val low_order_offset: int
			val emit_string: int -> string -> unit
			val emit_comment: string -> unit
		    end

signature EMITTERS = sig
    structure LittleEndian : EMITTER
    structure BigEndian : EMITTER
    structure MipsAsm: EMITTER
    val emitted_string : unit -> string
    val address : int ref
    val asmstream: outstream ref
end

