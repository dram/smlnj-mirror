signature MIPSCODER = sig

eqtype Label
datatype Register = Reg of int
datatype EA = Direct of Register | Immed of int | Immedlab of Label

structure M :
sig
    val newlabel : unit -> Label
    val align : unit -> unit
    val mark : unit -> unit
    val define : Label -> unit
    val emitstring : string -> unit
    val realconst : string -> unit
    val emitlong : int -> unit
    val emitlab : int * Label -> unit
    
    val slt : Register * EA * Register -> unit
    val beq : bool * Register * Register * Label -> unit
    
    val jump : Register -> unit
    
    val add : Register * EA * Register -> unit
    val and' : Register * EA * Register -> unit
    val or : Register * EA * Register -> unit
    val xor : Register * EA * Register -> unit
    val sub : Register * Register * Register -> unit
    val div : Register * Register * Register -> unit
    val mult : Register * Register * Register -> unit
    
    val lbu  : Register * EA * int -> unit
    val sb  : Register * EA * int -> unit
    val lw  : Register * EA * int -> unit
    val sw  : Register * EA * int -> unit
    
    val sll : EA * Register * Register -> unit
    val sra : EA * Register * Register -> unit
    
    val comment : string -> unit
  end
end (* signature CODER *)
