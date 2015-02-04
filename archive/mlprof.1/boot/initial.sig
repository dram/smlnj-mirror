signature INITIAL =
  sig

    structure Pervasives : PERVASIVES

 (* PERVASIVES *)
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

 (* REF *)
    infix 3 :=
    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val inc : int ref -> unit
    val dec : int ref -> unit

 (* LIST *)
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

 (* ARRAY *)
    infix 3 sub
    type 'a array
    exception Subscript
    val array : int * 'a -> 'a array
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val length : 'a array -> int
    val arrayoflist : 'a list -> 'a array

 (* IO *)
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

 (* BOOL *)
    datatype bool = true | false
    val not : bool -> bool
    val print : bool -> bool
    val makestring : bool -> string

 (* STRING *)
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

 (* INTEGER *)
    infix 7 * div mod
    infix 6 + -
    infix 4 > < >= <=
    exception Div
    exception Mod
    exception Overflow
    type int
    val ~ : int -> int
    val * : int * int -> int
    val div: int * int -> int
    val mod: int * int -> int
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

 (* REAL *)
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
    val >=  : (real * real) -> bool
    val <=  : (real * real) -> bool
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

 (* GENERAL *)
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

    structure System : SYSTEM 

(* temporary *)
    val polyequal : 'a * 'a -> bool
    val intequal : (int * int -> bool)
    val refequal : ('a ref * 'a ref -> bool)
    val arrayequal : ('a array * 'a array -> bool)
    val realequal : (real * real -> bool)
    val stringequal : string * string -> bool

end  (* signature INITIAL *)
