(* A small assembler for the MIPS                                *)

(* We generate code in several stages.                                      *)
(* This is nearly the lowest stage; it is like an assembler.                *)
(* The user can call any function in the MIPSCODER signature.               *)
(* Each one corresponds to an assembler pseudo-instruction.                 *)
(* Most correspond to single MIPS instructions.                             *)
(* The assembler remembers all the instructions that have been              *)
(* requested, and when codegen is called it generates MIPS              *)
(* code for them.                                                           *)
(*                                                                          *)
(* Some other structure will be able to use the MIPS structure to implement *)
(* a CMACHINE, which is the abstract machine that ML thinks it is running *)
(* on.                                                                      *)
(* (What really happens is a functor maps some structure                    *)
(* implementing MIPSCODER to a different structure implementing         *)
(* CMACHINE.)                                                           *)
(*                                                                          *)
(* Any function using a structure of this signature must avoid         *)
(* touching registers 1~and~31.                                             *)
(* Those registers are reserved for use by the assembler.                  *)



signature MIPSCODER = sig

eqtype Label
datatype Register = Reg of int | FloatReg of int
    (* Registers 1 and 31 are reserved for use by this assembler *)
datatype EA = Direct of Register | Immed of int | Immedlab of Label
                                (* effective address *)

structure M : sig

    (* Emit various constants into the code *)

    val emitstring : string -> unit     (* put a literal string into the
                                           code (null-terminated?) and
                                           extend with nulls to 4-byte 
                                           boundary. Just chars, no 
                                           descriptor or length *)
    exception BadReal of string
    val low_order_offset : int		(* does the low-order word of a 
					   floating point literal come
					   first (0) or second (1) *)
    val realconst : string -> unit      (* emit a floating pt literal *)
    val emitlong : int -> unit          (* emit a 4-byte integer literal *)


    (* Label bindings and emissions *)

    val newlabel : unit -> Label        (* new, unbound label *)
    val define : Label -> unit          (* cause the label to be bound to
                                           the code about to be generated *)
    val emitlab : int * Label -> unit   (* L3: emitlab(k,L2) is equivalent to
                                           L3: emitlong(k+L2-L3) *)

    (* Control flow instructions *)

    val slt : Register * EA * Register -> unit
                (* (operand1, operand2, result) *)
                                        (* set less than family *)
    val sltu : Register * EA * Register -> unit
                (* (operand1, operand2, result) *)
                                        (* set less than, unsigned *)
    val beq : bool * Register * Register * Label -> unit
                (* (beq or bne, operand1, operand2, branch address) *)
                                        (* branch equal/not equal family *)
    
    val jump : Register -> unit         (* jump register instruction *)

    val slt_double : Register * Register -> unit
					(* floating pt set less than *)
    val seq_double : Register * Register -> unit
					(* floating pt set equal *)
    val bcop1 : bool * Label -> unit    (* floating pt conditional branch *)


    (* Arithmetic instructions *)
            (* arguments are (operand1, operand2, result) *)

    val add : Register * EA * Register -> unit
    val and' : Register * EA * Register -> unit
    val or : Register * EA * Register -> unit
    val xor : Register * EA * Register -> unit
    val sub : Register * Register * Register -> unit
    val div : Register * Register * Register -> unit
					(* first arg is some register
					   guaranteed to overflow when
					   added to itself.  Used to
					   detect divide by zero. *)
    val mult : Register * Register -> unit
    val mflo : Register -> unit		(* low word of 64-bit multiply *)
    val mfhi : Register -> unit		(* high word of 64-bit multiply *)
    
    (* Floating point arithmetic *)

    val neg_double : Register * Register -> unit
    val mul_double : Register * Register * Register -> unit
    val div_double : Register * Register * Register -> unit
    val add_double : Register * Register * Register -> unit
    val sub_double : Register * Register * Register -> unit

    (* Move pseudo-instruction :  move(src,dest) *)

    val move : EA * Register -> unit
    val mov_double: Register * Register -> unit

    (* Load and store instructions *)
            (* arguments are (destination, source address, offset) *)
 
    val lbu  : Register * EA * int -> unit (* bytes *)
    val sb  : Register * EA * int -> unit
    val lw  : Register * EA * int -> unit  (* words *)
    val sw  : Register * EA * int -> unit
    val lwc1: Register * EA * int -> unit  (* floating point coprocessor *)
    val swc1: Register * EA * int -> unit
    val lui : Register * int -> unit



    (* Shift instructions *)
            (* arguments are (shamt, operand, result) *)
            (* shamt as Immedlab _ is senseless *)

    val sll : EA * Register * Register -> unit
    val sra : EA * Register * Register -> unit
    

    (* Miscellany *)

    val align : unit -> unit            (* cause next data to be emitted on
                                           a 4-byte boundary *)
    val mark : unit -> unit             (* emit a back pointer, 
                                           also called mark *)

    val comment : string -> unit

  end (* signature of structure M *)

  val codegen : unit->unit

  val codestats : outstream -> unit	(* write statistics on stream *)

end (* signature MIPSCODER *)
