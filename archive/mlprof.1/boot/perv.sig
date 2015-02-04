signature REF = 
  sig
    infix 3 :=
    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val inc : int ref -> unit
    val dec : int ref -> unit
  end

signature LIST =
  sig
    infixr 5 :: @
    datatype 'a list = :: of ('a * 'a list) | nil
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
    val revapp : ('a -> 'b) -> 'a list -> unit
    val nth : 'a list * int -> 'a 
    val exists : ('a -> bool) -> 'a list -> bool
  end

signature ARRAY =
  sig
    infix 3 sub
    type 'a array
    exception Subscript
    val array : int * 'a -> 'a array
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val length : 'a array -> int
    val arrayoflist : 'a list -> 'a array
  end

signature BYTEARRAY =
  sig
    infix 3 sub
    type bytearray
    exception Subscript
    exception Range
    val array : int * int -> bytearray
    val sub : bytearray * int -> int
    val update : bytearray * int * int -> unit
    val length : bytearray -> int
    val extract : bytearray * int * int -> string
    val fold : ((int * 'b) -> 'b) -> bytearray -> 'b -> 'b
    val revfold : ((int * 'b) -> 'b) -> bytearray -> 'b -> 'b
    val app : (int -> 'a) -> bytearray -> unit
    val revapp : (int -> 'b) -> bytearray -> unit
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
    val close_in : instream -> unit
    val close_out : outstream -> unit
    val output : outstream -> string -> unit
    val input : instream -> int -> string
    val lookahead : instream -> string
    val end_of_stream : instream -> bool
  end

signature IO =
  sig
    type instream 
    type outstream
    exception Io_failure of string
    val std_in : instream
    val std_out : outstream
    val open_in : string -> instream
    val open_out : string -> outstream
    val open_append : string -> outstream
    val open_string : string -> instream
    val close_in : instream -> unit
    val close_out : outstream -> unit
    val output : outstream -> string -> unit
    val input : instream -> int -> string
    val input_line : instream -> string
    val lookahead : instream -> string
    val end_of_stream : instream -> bool
    val can_input : instream -> int
    val flush_out : outstream -> unit
    val is_term_in : instream -> bool
    val is_term_out : outstream -> bool
    val set_term_in : instream * bool -> unit
    val set_term_out : outstream * bool -> unit
    val execute : string -> instream * outstream
    val exportML : string -> bool
    val exportFn : string * (string list * string list -> unit) -> unit
    val use : string -> unit
    val use_stream : instream -> unit
    val reduce : ('a -> 'b) -> ('a -> 'b)
(* the following are temporary components *)
    val reduce_r : ('a -> 'b) -> ('a -> 'b) ref
    val cleanup : unit -> unit
    val use_f : (string -> unit) ref
    val use_s : (instream -> unit) ref
    val reduce_r : (('a -> 'b) -> ('a -> 'b)) ref
  end

signature BOOL =
  sig
    datatype bool = true | false
    val not : bool -> bool
    val print : bool -> bool
    val makestring : bool -> string
  end

signature STRING =
  sig
    infix 6 ^
    infix 4 > < >= <=
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

signature INTEGER = 
  sig
    infix 7 * div mod
    infix 6 + -
    infix 4 > < >= <=
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

signature REAL =
  sig
    infix 7 * /
    infix 6 + -
    infix 4 > < >= <=
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

signature GENERAL =
  sig
    infix 3 o
    infix before
    exception Bind
    exception Match
    exception Interrupt
    val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val before : ('a * 'b) -> 'a (* this may be renamed soon *)
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
    structure ByteArray : BYTEARRAY
    structure IO : IO
    structure Bool : BOOL
    structure String : STRING
    structure Integer : INTEGER
    structure Real : REAL
    structure General : GENERAL
  end
