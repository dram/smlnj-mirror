signature ARRAY =
  sig
    type 'a array
    exception Subscript
    val array : int * 'a -> 'a array
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val length : 'a array -> int
    val arrayoflist : 'a list -> 'a array
  end

signature BASICIO = 
  sig
    type instream 
    type outstream
    exception Io_failure of string

    val std_in : instream
    val std_out : outstream

    val open_in : string -> instream
    val open_out : string -> outstream

    val input : instream * int -> string
    val lookahead : instream -> string
    val close_in : instream -> unit
    val end_of_stream : instream -> bool

    val output : outstream * string -> unit
    val close_out : outstream -> unit
  end

signature EXTENDEDIO =
  sig
    structure BasicIO : BASICIO
    val execute : string -> BasicIO.instream * BasicIO.outstream
    val flush_out : BasicIO.outstream -> unit
    val can_input : BasicIO.instream * int -> bool
    val input_line : BasicIO.instream -> string
    val open_append : string -> BasicIO.outstream
    val is_term_in : BasicIO.instream -> bool
    val is_term_out : BasicIO.outstream -> bool
  end

signature BOOL =
  sig
    datatype bool = true | false
    val not : bool -> bool
    val print : bool -> bool
    val makestring : bool -> string
  end

signature BYTE_ARRAY =
  sig
    type byte_array
    exception Byte_array
    val create : int * int -> byte_array
    val length : byte_array -> int
    val store : byte_array * int * int -> unit
    val fetch : byte_array * int -> int
    val extract : byte_array * int * int -> string
    val fold : ((int * 'b) -> 'b) -> byte_array -> 'b -> 'b
    val revfold : ((int * 'b) -> 'b) -> byte_array -> 'b -> 'b
    val app : (int -> 'a) -> byte_array -> unit
    val revapp : (int -> unit) -> byte_array -> unit
  end

signature INTEGER = 
  sig
    exception Div
    exception Mod
    exception Overflow
    type int
    val ~ : int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val + : int * int -> int
    val - : int * int -> int
    val >  : int * int -> bool
    val >= : int * int -> bool
    val <  : int * int -> bool
    val <= : int * int -> bool
    val min : int * int -> int
    val max : int * int -> int
    val abs : int -> int
    val print : int -> int
    val makestring : int -> string
  end

signature LIST =
  sig
    datatype 'a list = op :: of ('a * 'a list) | nil
    exception Hd
    exception Tl
    exception Nth
    val hd : 'a list -> 'a
    val tl : 'a list -> 'a list 
    val null : 'a list -> bool 
    val length : 'a list -> int 
    val @ : 'a list * 'a list -> 'a list
    val rev : 'a list -> 'a list 
    val map :  ('a -> 'b) -> 'a list -> 'b list
    val fold : (('a * 'b) -> 'b) -> 'a list -> 'b -> 'b
    val revfold : (('a * 'b) -> 'b) -> 'a list -> 'b -> 'b
    val app : ('a -> 'b) -> 'a list -> unit
    val revapp : ('a -> unit) -> 'a list -> unit
    val nth : 'a list * int -> 'a 
    val exists : (('a -> bool) * 'a list) -> bool
  end

signature REAL =
  sig
    type real

    exception Floor and Sqrt and Exp and Ln
    exception Float of string

    val ~ : real -> real 
    val + : (real * real) -> real 
    val - : (real * real) -> real 
    val * : (real * real) -> real 
    val / : (real * real) -> real 
    val > : (real * real) -> bool
    val < : (real * real) -> bool
    val >= : (real * real) -> bool
    val <= : (real * real) -> bool
    val abs : real ->  real 
    val real : int -> real 
    val floor : real -> int 
    val sqrt : real -> real 
    val sin : real -> real 
    val cos : real -> real 
    val arctan : real -> real 
    val exp : real -> real 
    val ln : real -> real 
    val print : real -> real 
    val makestring : real -> string 
  end

signature REF = 
  sig
    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val inc : int ref -> unit
    val dec : int ref -> unit
  end

signature STRING =
  sig
    type string
    exception Substring
    val length : string -> int
    val size : string -> int
    val substring : string * int * int -> string
    val explode : string -> string list
    val implode : string list -> string
    val <= : string * string -> bool
    val <  : string * string -> bool
    val >= : string * string -> bool
    val >  : string * string -> bool
    val ^  : string * string -> string
    exception Chr
    val chr : int -> string 
    exception Ord
    val ord : string -> int 
    val ordof : string * int -> int 
    val print : string -> string
  end

signature GENERAL =
  sig
    exception Bind
    exception Match
    exception Varstruct  (* = Bind, for compatibility *)
    exception Interrupt
    val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val before : ('a * 'b) -> 'a (* this may be renamed soon *)
    val exportML : string -> unit
    val export1 : string * (string list -> unit) -> unit
    val use : string -> unit
    val system : string -> unit
    val cd : string -> unit
    exception Equal
    datatype 'a option = NONE | SOME of 'a
    type exn
    type unit
  end

signature PERVASIVES =
  sig
    structure Ref : REF
    structure List : LIST
    structure Array : ARRAY
    structure Byte_array : BYTE_ARRAY
    structure BasicIO : BASICIO
    structure ExtendedIO : EXTENDEDIO
    structure Bool : BOOL
    structure String : STRING
    structure Integer : INTEGER
    structure Real : REAL
    structure General : GENERAL
  end
