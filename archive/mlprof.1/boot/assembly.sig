(* All string arguments to system calls should be zero padded *)
signature ASSEMBLY =
  sig
    datatype flags = READ | WRITE | APPEND
    datatype time = TIME of {sec : int, usec : int}
    exception Div
    exception Float of string
    exception Interrupt
    exception Io_failure of string
    exception Overflow
    type object
    val array : int * 'a -> 'a array
    val array0 : 'a array
    val boot : string -> ('a -> 'b)
    val bytearray0 : string
    val chdir : string -> unit
    val close : int -> unit
    val control : {gcmessages : bool ref}
    val create_b : int -> string
    val create_s : int -> string
    val dup2 : int * int -> unit
    val execute : string -> unit
    val exportFn : int * (string list * string list -> unit) -> unit
    val exportML : int -> bool
    val fionread : int -> int
    val floor : real -> int
    val fork : unit -> int
    val isatty : int -> bool
    val logb : real -> int
    val openf : string * flags -> int
    val pipe : unit -> int * int
    val pstruct : object ref
    val read : int * string -> int
    val scalb : real * int -> real
    val seql : string * string -> bool
    val system : string -> unit
    val timer : unit -> time * time
    val write : int * string * int -> int
  end
