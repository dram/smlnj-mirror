(*
 * The standard basis used in our lambda RTL language.
 *)
structure Basis =
struct
   (* Primitive types *)
   type #n bits    (* a value that is n bits wide *)
   type #n loc     (* a location containing an n-bit value *)
   type #n cell    (* one of a sequence of n-bit storage cells *)
   type bool       (* a boolean *)
   type effect     (* an action *)
   type #n operand (* user defined operand of #n bits wide *)
   type label      (* a label *)
   type region     (* a memory register *)
   type #n immed   (* an immediate value of n bits *)
   type cellset
   type (#n, 'a) list (* an n-element list of type 'a *)
   type int
   type word
   type string
   type region

   (* We are allowed to use map in the basis.  
    * This is the only higher order function we have. 
    *)
   val map     : ('a -> 'b) -> (#n, 'a) list -> (#n, 'b) list
   val newOp   : string -> 'a
   val newCond : string -> 'a

   val aggb  : #n cell -> #m loc
   val aggl  : #n cell -> #m loc
   val idaggr: #n cell -> #n loc
   val fetch : #n loc -> #n bits
   val :=    : #n loc * #n bits -> effect
   val forall : #n bits -> #n bits

   val intConst  : int -> #n bits
   val wordConst : word -> #n bits
   val ?       : #n bits
   val operand : #n operand -> #n bits
   val label   : label -> #n bits
   val immed   : #n immed -> #n bits

   (* Signed/unsigned promotion *)
   val sx  : #n bits -> #m bits
   val zx  : #n bits -> #m bits

   (* Integer operators *)
   val ~ notb : #n bits -> #n bits
   val + - muls mulu divs divu rems remu 
      andb orb xorb eqvb << >> ~>> 
      addt subt mult divt remt
      : #n bits * #n bits -> #n bits  

   (* Boolean operators *)
   val cond    : bool * #n bits * #n bits -> #n bits
   val not     : bool -> bool
   val andalso : bool * bool -> bool
   val orelse  : bool * bool -> bool

   (* Integer comparisons *)
   val == <> > < <= >= ltu leu gtu geu : #n bits * #n bits -> bool

   (* Floating point operators *)
   val fadd fsub fmul fdiv : #n bits * #n bits -> #n bits
   val fabs fneg fsqrt : #n bits -> #n bits

   (* Floating point comparisons *)
   val |?| |!<=>| |==| |?=| |!<>| |!?>=| |<| |?<| 
       |!>=| |!?>| |<=| |?<=| |!>| |!?<=| |>| |?>|
       |!<=| |!?<| |>=| |?>=| |!<| |!?=| |<>| |!=|
       |!?| |<=>| |?<>| : #n bits * #n bits -> bool

   (* Effect combinators *)
   val ||    : effect * effect -> effect  (* parallel effects *)
   val Nop   : effect                     (* empty effect *)
   val Jmp   : #n bits -> effect          (* jump to address *)
   val Call  : #n bits -> effect          (* call address *)
   val Ret   : effect                     (* return *)
   val If    : bool * effect * effect -> effect (* if/then/else *)
end 
