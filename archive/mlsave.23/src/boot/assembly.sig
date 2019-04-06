(* All string arguments to system calls should be zero padded *)
signature ASSEMBLY =
  sig
    datatype flags = READ | WRITE | APPEND
    datatype time = TIME of {sec : int, usec : int}

    structure C :
	sig 
            datatype datalist = DATANIL 
			      | DATACONS of (string * string * datalist)

	    type object
	    exception Div
	    exception Float of string
	    exception Interrupt
	    exception Overflow
	    exception SystemCall of string
	    val array0 : 'a array
	    val bytearray0 : string
	    val collected : int ref
	    val collectedfrom : int ref
	    val current : string ref
	    val datalist : datalist
	    val external : object list
	    val gcmessages : int ref
	    val majorcollections : int ref
	    val minorcollections : int ref
	    val opsys : int   (* 1 = vax bsd ultrix, 4.2, 4.3
				 2 = sunos 3.0, 4.0 
				 3 = vax v9 (bell labs) *)
	    val pstruct : object ref
	    val ratio : int ref
	end
    val array : int * 'a -> 'a array
(* obs    val boot : string -> ('a -> 'b) *)
(* obs    val chdir : string -> unit *)
(* obs    val close : int -> unit *)
    val create_b : int -> string
    val create_s : int -> string
(* obs    val dup2 : int * int -> unit *)
(* obs    val execute : string -> 'a *)
    val exportFn : int * (string list * string list -> unit) -> unit
    val exportML : int -> bool
    val fionread : int -> int
    val floor : real -> int
    val fork : unit -> int
(* obs    val isatty : int -> bool *)
    val logb : real -> int
(* obs    val openf : string * flags -> int *)
(* obs    val pipe : unit -> int * int *)
(* obs    val profoff : unit -> unit *)
(* obs    val profon : unit -> unit *)
    val profvec : int * int -> int
(* obs    val read : int * string -> int *)
    val scalb : real * int -> real
    val syscall : int * string list * int -> int
    val system : string -> unit
    val timer : unit -> time * time
(* obs    val write : int * string * int -> int *)
  end
