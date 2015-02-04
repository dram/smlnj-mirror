(* The following must be in the symbol table before this is parsed.
     1)  Magical words that can be free in signatures:
	     int string bool unit real list array ref instream outstream exn
     2)  Built-in constructors:
	      :: nil ref true false   (from PrimTypes as DataTypes)
     3)  Built-in structures:
	      PrimTypes 	  (matches DATATYPES)
              InLinePrim	  (matches INLINE)


NOTES:
	Make sure that all matches in this file are exhaustive!

*)

signature RUNTIMECONTROL =
  sig
    val gcmessages : bool ref
  end
signature MCCONTROL =
  sig
    val printArgs : bool ref
    val printRet : bool ref
    val bindContainsVar : bool ref
    val bindExhaustive : bool ref
    val matchExhaustive : bool ref
    val matchRedundant : bool ref
  end
signature CGCONTROL =
  sig
    val knownfunc : bool ref
    val tailrecur : bool ref
    val primapp : bool ref
    val recordopt : bool ref
    val tail : bool ref
    val closurecount : bool ref
    val closureprint : bool ref
    val chained : bool ref
    val hoist : bool ref
  end
signature CONTROL =
  sig
    structure Runtime : RUNTIMECONTROL
    structure MC : MCCONTROL
    structure CG : CGCONTROL
    val debugging : bool ref
    val primaryPrompt : string ref
    val secondaryPrompt : string ref
    val internals : bool ref
    val debugLook : bool ref
    val debugCollect : bool ref

  end

signature ASSEMBLY =
  sig
    datatype flags = READ | WRITE | APPEND
    datatype time = TIME of {sec : int, usec : int}
    exception Div
    exception Float of string
    exception Interrupt
    exception Io_failure of string
    exception Overflow
    val array : int * 'a -> 'a array
    val array0 : 'a array
    type object
    val boot : string -> unit -> ((object list -> object) * string list)
    val byte_array0 : string
    val chdir : string -> unit
    val close : int -> unit
    val control : {gcmessages : bool ref}
    val create_b : int -> string
    val create_s : int -> string
    val execute : unit -> unit (* bogus type for now *)
    val export : int -> bool
    val export1 : int * (string list -> unit) -> unit
    val fionread : int -> int
    val floor : real -> int
    val isatty : int -> bool
    val logb : real -> int
    val openf : string * flags -> int	(* string must be zero padded *)
    val pstruct : object ref
    val read : int * string -> int
    val scalb : real * int -> real
    val seql : string * string -> bool
    val system : string -> unit
    val timer : unit -> time * time
    val write : int * string * int -> int
  end

signature DATATYPES =
  sig
    datatype bool = false | true
    datatype 'a list = op :: of 'a * 'a list | nil
  end

signature INLINE =
  sig
    val ! : 'a ref -> 'a
    val * : int * int -> int
    val + : int * int -> int
    val - : int * int -> int
    val := : 'a ref * 'a -> unit
    val < : int * int -> bool
    val <= : int * int -> bool
    val > : int * int -> bool
    val >= : int * int -> bool
    val alength : 'a array -> int
    val cast : 'a -> 'b
    val div : int * int -> int
    val fadd : real * real -> real
    val fdiv : real * real -> real
    val feql : real * real -> bool
    val fge : real * real -> bool
    val fgt : real * real -> bool
    val fle : real * real -> bool
    val flt : real * real -> bool
    val fmul : real * real -> real
    val fneg : real -> real
    val fneq : real * real -> bool
    val fsub : real * real -> real
    val ieql : int * int -> bool
    val ineq : int * int -> bool
    val makeref : 'a -> 'a ref
    val ordof : string * int -> int
    val slength : string -> int
    val store : string * int * int -> unit
    val subscript : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val	~ : int -> int
  end

signature TIMER =
  sig  
    datatype time = TIME of {sec : int, usec : int}
    type timer
    val start_timing : unit -> timer
    val check_time : timer -> time
  end

signature TAGS =
  sig
    val width_tags : int
    val power_tags : int
    val tag_record : int
    val tag_array : int
    val tag_bytearray : int
    val tag_string : int
    val tag_embedded : int
    val tag_closure : int
    val tag_backptr : int
    val tag_forwarded : int
  end

signature SYSTEM =
  sig
    type object
    structure Assembly : ASSEMBLY	(* temporary *)
    structure Control : CONTROL
    structure Tags : TAGS
    structure Timer : TIMER
    val interactive : bool ref
    val readfile : string -> string
    val boot : string -> unit -> ((object list -> object) * string list)
    val boot1 : string -> ((int->object) -> object array)
    val create_s : int -> string
    val store_s : string * int * int -> unit
    val pstruct : object ref
    val useref : (string -> unit) ref
    exception Boxity
    val tuple : object -> object array
    val string : object -> string
    val real : object -> real
    val int : object -> int
    val exn_name : exn -> string
  end

signature INITIAL =
  sig

    structure Pervasives : PERVASIVES

 (* PERVASIVES *)
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

 (* ARRAY *)
    type 'a array
    exception Subscript
    val array : int * 'a -> 'a array
    val sub : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val length : 'a array -> int
    val arrayoflist : 'a list -> 'a array

 (* BASICIO *)
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

 (* EXTENDEDIO *)
    val execute : string -> BasicIO.instream * BasicIO.outstream
    val flush_out : BasicIO.outstream -> unit
    val can_input : BasicIO.instream * int -> bool
    val input_line : BasicIO.instream -> string
    val open_append : string -> BasicIO.outstream
    val is_term_in : BasicIO.instream -> bool
    val is_term_out : BasicIO.outstream -> bool

 (* BOOL *)
    datatype bool = true | false
    val not : bool -> bool
    val print : bool -> bool
    val makestring : bool -> string

 (* BYTE_ARRAY *)
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

 (* INTEGER *)
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

 (* LIST *)
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

 (* REAL *)
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

 (* REF *) 
    val ! : 'a ref -> 'a
    val := : 'a ref * 'a -> unit
    val inc : int ref -> unit
    val dec : int ref -> unit

 (* STRING *)
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

    structure System : SYSTEM 

 (* GENERAL *)
    exception Bind
    exception Match
    exception Varstruct  (* = Bind, for compatibility *)
    exception Interrupt
    val o : ('b -> 'c) * ('a -> 'b) -> ('a -> 'c)
    val before : ('a * 'b) -> 'a (* this may be renamed soon *)
    val use : string -> unit
    val exportML : string -> unit
    val export1 : string * (string list -> unit) -> unit
    val system : string -> unit
    val cd : string -> unit
    exception Equal
    datatype 'a option = NONE | SOME of 'a
    type exn
    type unit

(* temporary *)
    val polyequal : 'a * 'a -> bool
    val intequal : (int * int -> bool)
    val refequal : ('a ref * 'a ref -> bool)
    val arrayequal : ('a array * 'a array -> bool)
    val realequal : (real * real -> bool)
    val stringequal : string * string -> bool

end  (* signature INITIAL *)

functor PervFunc(Assembly : ASSEMBLY) : INITIAL =
struct

  structure Pervasives =
  struct

    structure DataTypes : DATATYPES = PrimTypes
    structure InLine : INLINE = InLinePrim
   
    structure Control = struct
	structure Runtime = struct
	  val {gcmessages,...} = Assembly.control
	end
	structure MC = struct
	  val printArgs = ref false
	  val printRet = ref false
	  val bindContainsVar = ref true
	  val bindExhaustive = ref true
	  val matchExhaustive = ref true
	  val matchRedundant = ref true
	end
	structure CG = struct
	  val knownfunc = ref true
	  val tailrecur = ref true
	  val primapp = ref true
	  val recordopt = ref true
	  val tail = ref true
	  val closurecount = ref false
	  val closureprint = ref false
	  val chained = ref true
	  val hoist = ref true
        end
	val debugging = ref false
	val primaryPrompt = ref "- "
	val secondaryPrompt = ref "= "
	val internals = ref false
	val debugLook = ref false
	val debugCollect = ref false
	val interactive = ref true (* thinned out of final Control structure *)
    end (* structure Control *)

    structure Ref: REF = 
     struct
       infix 3 :=
       open DataTypes InLine
       fun inc r = r := InLine.+(!r,1)
       fun dec r = r := InLine.-(!r,1)
       val op := = InLine.:=
       val op ! = InLine.!
     end
   
    structure List: LIST =
     struct
       infixr 5 :: 
       infixr 5 @ 
       infix 6 + -
       open DataTypes InLine
       exception Hd
       exception Tl
       exception Nth
       fun hd (a::r) = a | hd nil = raise Hd
       fun tl (a::r) = r | tl nil = raise Tl    
       fun null nil = true | null _ = false
       fun length nil = 0
	 | length (a::r) = 1 + length r
       fun op @ (nil,l) = l
	 | op @ (a::r, l) = a :: (r@l)
       fun rev l =
	   let fun f (nil, h) = h
		 | f (a::r, h) = f(r, a::h)
	    in f(l,nil)
	   end
       fun map f =
	   let fun m nil = nil
		 | m (a::r) = f a :: m r
	    in m
	   end
       fun fold f =
	 let fun f2 nil = (fn b => b)
	       | f2 (e::r) = (fn b => f(e,(f2 r b)))
	  in f2
	 end
       fun revfold f =
	 let fun f2 nil = (fn b => b)
	       | f2 (e::r) = (fn b => (f2 r (f(e,b))))
	  in f2
	 end
       fun app f = let fun a2 (e::r) = (f e; a2 r) | a2 nil = () in a2 end
       fun revapp f = let fun a2 (e::r) = (a2 r; f e; ()) | a2 nil = () in a2 end
       fun nth(e::r,0) = e 
	 | nth(e::r,n) = nth(r,n-1)
	 | nth _ = raise Nth
       fun exists(f,[]) = false
	 | exists(f,h::t) = f(h) orelse exists(f,t)
     end (* structure List *)

   structure Array : ARRAY =
   struct
     infix 3 sub
     infix 4 < > <= >=
     infixr 5 ::
     infix 6 +     
     open DataTypes InLine
     type 'a array = 'a array
     exception Subscript
     val array = fn arg as (n,v) =>
		     if n<=0 then if n<0 then raise Subscript
					 else Assembly.array0
			     else Assembly.array arg
     val op sub = fn (a,i) =>
		if i<0 orelse i >= InLine.alength a then raise Subscript
			else InLine.subscript(a,i)
     val update = fn (a,i,v) => 
		if i<0 orelse i >= InLine.alength a then raise Subscript
			else InLine.update(a,i,v)
     val length = InLine.alength
     fun arrayoflist nil = cast(Assembly.array0)
       | arrayoflist (l as (e::r)) =
	   let val a = array(List.length l, e)
	       fun init ((e::r),n) = (update(a,n,e); init(r,n+1))
	         | init (nil,_) = ()
	    in init(l,0); a
           end
    end (* structure Array *)

    structure PreString =
    struct
      infix 4 = < > >= <=
      infix 6 +
      open InLine
      exception Substring
      fun substr("",0,0) = "" (* never call create_s with 0 *)
	| substr("",_,_) = raise Substring
        | substr(s,i,0) = if i<0 orelse i > slength s
			    then raise Substring
			    else ""
        | substr(s,0,1) = if ieql(slength s, 1) then s else cast(ordof(s,0))
        | substr(s,i,1) = if i<0 orelse i >= slength s
			    then raise Substring
			    else cast(ordof(s,i))
	| substr(s,i,len) = 
	    if i<0 orelse i+len > slength s orelse len<0
	      then raise Substring
	      else let val a = Assembly.create_s(len)
		       fun copy j = 
			   if ieql(j,len) then ()
			      else (store(a,j,ordof(s,i+j)); copy(j+1))
		    in copy 0; a
		   end
      fun sconcat("",s) = s
	| sconcat(s,"") = s
	| sconcat(x,y) =
	  let val xl = slength(x) and yl = slength(y)
	      val a = Assembly.create_s(xl+yl)
	      fun copyx n = if ieql(n,xl) then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
	      fun copyy n = if ieql(n,yl) then ()
			      else (store(a,xl+n,ordof(y,n)); copyy(n+1))
	   in if ieql(xl,1) then store(a,0,cast(x)) else copyx 0;
	      if ieql(yl,1) then store(a,xl,cast(y)) else copyy 0;
              a
	  end
    end (* structure PreString *)	   

    structure Byte_array : BYTE_ARRAY =
    struct
      infix 4 = < > <= >=
      infix 6 + -
      open InLine
      open PreString
      type byte_array = string
      exception Byte_array
      fun create(len,v) =
	  if len<0 orelse v<0 orelse v>=256
	    then raise Byte_array
	    else if ieql(len,0) then Assembly.byte_array0
	    else let val a = Assembly.create_b(len)
		     fun init i = if ieql(i,len) then () else (store(a,i,v); init(i+1))
		  in init 0; a
		 end
      val length = cast(slength)
      val store = fn arg as (s,i,c) =>
	  if i<0 orelse i>=slength(s) orelse c<0 orelse c>255
		  then raise Byte_array
		  else InLine.store arg
      val fetch = fn (s,i) =>
	  if i<0 orelse i>= slength s then raise Byte_array else ordof(s,i)
      fun extract(ba,i,1) = if i<0 orelse i >= slength ba
			    then raise Byte_array
			    else cast(ordof(ba,i))
	| extract(s,i,len) = 
	    if i<0 orelse i+len > slength s orelse len<0
	      then raise Byte_array
	      else if ieql(len,0) then Assembly.byte_array0
	      else let val a = Assembly.create_b(len)
		       fun copy j = 
			   if ieql(j,len) then ()
			      else (store(a,j,ordof(s,i+j)); copy(j+1))
		    in copy 0; a
		   end
      fun app f ba = 
	let val len = slength ba
	    fun app'(i) = 
	      if i >= len then ()
	      else (f(fetch(ba,i)); app'(i+1))
	 in app'(0) end;
  
      fun revapp f ba = 
	let fun revapp'(i) = 
	      if i <= 0 then ()
	      else (f(fetch(ba,i)); revapp'(i-1))
	 in revapp'(slength ba) end;
  
      fun fold f ba x = 
	let fun fold'(i,x) = 
	    if i <= 0 then x else fold'(i-1, f(ordof(ba,i),x))
	 in fold'(slength ba, x) end;
  
      fun revfold f ba x = 
	let val len = slength ba
	    fun revfold'(i,x) =
	      if i >= len then x else revfold'(i+1,f(ordof(ba,i),x))
	 in revfold'(0,x) end

    end (* structure Byte_array *)

    structure IO =
    struct
       infix 3 :=
       infix 4 = < > <= >=
       infix 6 + -
       infix 7 *
       open InLine Ref PreString DataTypes
       type byte_array = string
       exception Io_failure of string = Assembly.Io_failure
       type instream = {filid: int, pos: int ref, len: int ref,
		        closed : bool ref, buf: byte_array}
       type outstream = {filid: int, pos: int ref,
		        closed : bool ref, buf: byte_array, tty : bool ref}
   
       fun isatty filid = Assembly.isatty filid handle Io_failure _ => true

       val std_in = {filid=0, pos= ref 0, len= ref 0, 
		     closed = ref false, buf=Assembly.create_b(1024)}
       val std_out ={filid=1, pos= ref 0, 
		     closed = ref false, buf=Assembly.create_b(1024),
		     tty = ref(isatty 1)}

       (* the filename passed to Assembly.openf must be zero-padded;
          you need 2 zeros in case padding a null string. *)
       val c00 = "\000\000"
   
       fun open_in s = {filid=Assembly.openf(sconcat(s,c00),Assembly.READ),
			pos= ref 0, len=ref 0, closed = ref false,
			buf=Assembly.create_b(1024)}
       fun open_o mode = fn s =>
	    let val f = Assembly.openf(sconcat(s,c00),mode)
	     in {filid=f, pos= ref 0, closed = ref false,
		 buf=Assembly.create_b(1024), tty = ref(isatty f)}
	    end

       val open_out = open_o Assembly.WRITE
       val open_append = open_o Assembly.APPEND
   
       fun filbuf (f as {filid,pos,len,buf,closed}) =
		if !closed then raise Io_failure("Input stream is closed")
		  else (len := Assembly.read(filid,buf);
		        pos := 0)
   
       fun flsbuf (f as {filid,pos,buf,closed,tty}) =
		if !closed then raise Io_failure("Output stream is closed")
		  else (if InLine.<(Assembly.write(filid,buf,!pos),!pos)
		       then raise Io_failure("failure in write")
		       else pos := 0)
   
       (* non-standard:  input on a closed stream should return "".
	  See filbuf and input_line as well. *)
       fun input({closed=ref true,...},_) =
	       raise Io_failure("Input stream is closed")
         | input(f as {filid,pos,len,buf,closed}, i) =
	   if i < 0
	   then raise Io_failure("input called with negative character count")
	   else if (!len - !pos) >= i
	        then let val s = substr(buf,!pos,i)
		      in pos := !pos+i; s
		     end
	        else let val s = substr(buf,!pos, !len - !pos)
			 val j = i - (!len - !pos)
		      in filbuf f;
			 if ieql(!len,0) then s
			 else sconcat(s, input(f,j))
		     end

       fun input_line({closed=ref true,...}) =
	       raise Io_failure("Input stream is closed")
         | input_line(f as {filid,pos,len,buf,closed}) =
	   let val l = !len
	       fun next j = if ieql(j,l)
			     then let val s = substr(buf, !pos, l - !pos)
				   in filbuf f;
				      if ieql(!len,0) then s
				      else sconcat(s,input_line f)
				  end
			     else if ieql(InLine.ordof(buf,j),10)
			          then input(f, j+1 - !pos)
				  else next(j+1)
           in next (!pos)
	  end
       
       fun lookahead (f as {filid,pos,len,buf,closed}) =
	   if !len > !pos
	   then cast(InLine.ordof(buf,!pos))
	   else (filbuf f;
		 if ieql(!len,0) then "" else lookahead f)
   
       fun end_of_stream f = case lookahead f of "" => true | _ => false
   
       fun output (f as {filid,buf,pos,closed,tty}, s) =
	   let val l = slength s
	    in if ieql(l,1)
		    then (InLine.store(buf,!pos,InLine.cast(s));
		          pos := InLine.+(!pos,1);
			  if ieql(!pos,1024) orelse
			     (!tty andalso ieql(InLine.cast(s),10))
			   then flsbuf f else ())
		    else let val i = ref 0
		          in while !i < l
			      do let val c = InLine.ordof(s,!i)
				  in InLine.store(buf,!pos,c);
				     inc pos; inc i;
			             if ieql(!pos,1024) orelse
			                (!tty andalso ieql(c,10))
			              then flsbuf f else ()
			         end
		         end
	   end
   
       fun close_in {filid,pos,len,buf,closed} = 
		(closed := true; len := 0; pos := 0; Assembly.close filid)
       fun close_out (f as {filid,pos,buf,closed,tty}) = 
	    (flsbuf f; pos := 1023; closed := true; Assembly.close filid)

       val flush_out : outstream -> unit = flsbuf

       fun can_input({filid,pos, len, buf,closed=ref false},i) =
		!len - !pos >= i orelse 
		    !len - !pos + Assembly.fionread filid >= i
         | can_input _ = raise Io_failure("can_input on closed file")

       fun execute s = raise Io_failure("execute is unimplemented")
       fun is_term_in {filid,pos,len,buf,closed=ref false} = 
		 Assembly.isatty filid
         | is_term_in _ = raise Io_failure("is_term on closed file")
       fun is_term_out {filid,pos,buf,closed,tty=ref x} = x

       fun exportML filename = 
	   let val filid=Assembly.openf(sconcat(filename,c00),Assembly.WRITE)
	       val {pos,len,...} = std_in
	       val {tty,...} = std_out
	    in flush_out std_out;
	       if Assembly.export filid
		 then (pos := 0; len := 0; tty := isatty 1;
		       Control.interactive := is_term_in std_in)
		 else (Assembly.close filid)
	   end

       val useref = ref (fn (_ : string) => ())

       fun export1 (filename,func) = 
	   let val filid=Assembly.openf(sconcat(filename,c00),Assembly.WRITE)
	       val {pos,len,...} = std_in
	       val {tty,...} = std_out
	    in flush_out std_out;
	       useref := (fn (_ : string) => ());
	       Assembly.export1 (filid,func)
	   end

       fun readfile s = 
	   let val filid=Assembly.openf(sconcat(s,c00),Assembly.READ)
	       val b = Assembly.create_b 4
	       val _ = if ieql(4,Assembly.read(filid,b)) then ()
			 else raise Io_failure("no length")
	       val len = ((ordof(b,0)*256+ordof(b,1))*256+ordof(b,2))*256+ordof(b,3)
	       val c = Assembly.create_s len
	       val _ = if ieql(len,Assembly.read(filid,c)) then ()
			 else raise Io_failure("not long enough")
	    in Assembly.close filid; c
	   end
    end (* structure IO *)

    structure BasicIO : BASICIO = IO

    structure ExtendedIO : EXTENDEDIO = 
    struct
	    structure BasicIO = BasicIO
	    open IO
    end (* structure ExtendedIO *)

    structure Bool : BOOL =
    struct
      open DataTypes
      fun not true = false
	| not false = true
      fun makestring true = "true"
	| makestring false = "false"
      fun print b = (BasicIO.output(BasicIO.std_out, makestring b); b)
    end (* structure BOOL *)

    structure String : STRING =
    struct
      open DataTypes InLine
      infix 4 = > < >= <=
      infixr 5 ::
      infix 6 + ^ -
      type string = string
      val length = slength
      val size = slength
      exception Substring = PreString.Substring
      val substring = PreString.substr
      fun explode s =
	    case slength s
	       of 1 => [s]
	        | n => let fun f i = if ieql(i,n) then nil
			               else cast(ordof(s,i)) :: f(i+1)
		        in f 0
	               end
      val op ^ = PreString.sconcat
      exception Chr
      fun chr i = if i<0 orelse i>255 then raise Chr else cast i
      exception Ord
      fun ord s = case slength s of 1 => cast s | 0 => raise Ord | _ => ordof(s,0)
      val ordof = fn (s,i) =>
	  case slength s
           of 1 => if ieql(i,0) then cast(s) else raise Ord
	    | n => if i<0 orelse i>= n then raise Ord else ordof(s,i)
      fun print s = (BasicIO.output(BasicIO.std_out,s); s)
      fun implode (sl:string list) =
	  let val len = List.fold(fn(s,l) => slength s + l) sl 0
	  in  case len
	       of 0 => ""
		| 1 => let fun find (""::tl) = find tl
			     | find (hd::_) = cast hd
			     | find nil = "" (* impossible *)
		       in  find sl
		       end
		| _ => let val new = Assembly.create_s len
			   fun copy (nil,_) = ()
			     | copy (s::tl,base) =
				let val len = slength s
				    fun copy0 0 = ()
				      | copy0 i =
					let val next = i-1
					in  store(new,base+next,ordof(s,next));
					    copy0 next
					end
				in  copy0 len;
				    copy(tl,base+len)
				end
			in  copy(sl,0);
			    new
			end
	  end
      fun sgtr(a,b) =
	  let val al = slength a and bl = slength b
	      val n = if al<bl then al else bl
	      fun f i = if ieql(i,n) then al > bl
			else if ieql(ordof(a,i),ordof(b,i)) then f(i+1)
			else ordof(a,i) > ordof(b,i)
	   in f 0
	  end
  
      fun op <= (a,b) = Bool.not(sgtr(a,b))
      fun op < (a,b) = sgtr(b,a)
      fun op >= (a,b) = Bool.not(sgtr(b,a))
      val op > = sgtr
    end  (* structure String *)

    structure Integer : INTEGER =
    struct
      infix 4 > < >= <=
      infix 6 + -
      infix 7 *
      infix 7 div mod
      exception Div = Assembly.Div
      exception Mod
      exception Overflow = Assembly.Overflow
      type int = int
      val ~ = InLine.~
      val op * = InLine.*
      val op div = InLine.div
      fun op mod(a,b) = InLine.-(a,InLine.*(InLine.div(a,b),b))
		        handle Div => raise Mod
      val op + = InLine.+
      val op - = InLine.-
      val op > = InLine.>
      val op >= = InLine.>=
      val op < = InLine.<
      val op <= = InLine.<=
      fun min(a,b) = if a<b then a else b
      fun max(a,b) = if a>b then a else b
      fun abs a = if a<0 then ~a else a
      fun makestring i = if i<0 then String.^("~", makestring(~i))
	  else if i<10 then InLine.cast(InLine.cast("0")+i)
	  else let val j = i div 10
	        in String.^(makestring j, makestring(i-j*10))
	       end
      fun print i = (BasicIO.output(BasicIO.std_out,makestring i); i)
    end  (* structure Integer *)

    structure Real : REAL =
    struct
      infix 4 > < >= <=
      infix 6 + -
      infix 7 * /
      infixr 5 ::
      type real = real
      exception Unimplemented_real_operator
      exception Floor and Sqrt and Exp and Ln
      exception Float of string = Assembly.Float
      fun f(x) = raise Unimplemented_real_operator
      val ~ = InLine.fneg
      val op + = InLine.fadd
      val op - = InLine.fsub
      val op * = InLine.fmul
      val op / = InLine.fdiv
      val op > = InLine.fgt
      val op < = InLine.flt
      val op >= = InLine.fge
      val op <= = InLine.fle
      val abs = f
      val real = f
      val floor = f
      val sqrt = f
      val sin = f
      val cos = f
      val arctan = f
      val exp = f
      val ln = f
      val zero = 0.0
      val one = 1.0
      val two = 2.0
      val ten = 10.0
      val scalb = Assembly.scalb
      val logb = Assembly.logb
      val floor = Assembly.floor
      fun abs x = if x < zero then ~x else x
      fun real 0 = zero
	| real n = if Integer.<(n,0)
		   then ~(real(Integer.~n))
		   else 2.0 * real(Integer.div(n,2))
			+ (case Integer.mod(n,2) of 0 => zero
						  | _ => one)
      fun makestring r =
	  let val itoa = Integer.makestring
	      val ^ = String.^
	      fun scistr(a::b::tl,e) =
		    let fun trail nil = ""
			  | trail (0::tl) =
			    let val rest = trail tl
			    in  case rest of "" => ""
					   | _ => ^("0",rest)
			    end
			  | trail (hd::tl) = ^(itoa hd,trail tl)
			val rest = trail tl
		    in  ^(itoa a,^(".",^(itoa b,^(rest,^("E",itoa e)))))
		    end
		 | scistr _ = "" (* prevents non-exhaustive match *)
	      fun normstr(digits,e) =
		    let fun n(nil,_) = ""
			  | n(hd::nil,0) = ^(itoa hd,".0")
			  | n(hd::tl,0) = ^(itoa hd,^(".",n(tl,~1)))
			  | n(0::tl,d) =
			      let val rest = n(tl,Integer.-(d,1))
			      in  case (Integer.<(d,~1),rest) of
				    (true,"") => rest
				    | _ => ^("0",rest)
			      end
			  | n(hd::tl,d) = ^(itoa hd,n(tl,Integer.-(d,1)))
			fun header n =
			  let fun zeros 1 = ""
				| zeros n = ^("0",zeros(Integer.-(n,1)))
			  in  ^("0.",zeros n) end
		    in  if Integer.<(e,0)
			then ^(header(Integer.~(e)),n(digits,e))
			else n(digits,e)
		    end
	      fun mkdigits(f,0) = (nil,if f < 5.0 then 0 else 1)
		| mkdigits(f,i) =
		    let	val digit = floor f
			val new = ten * (f - real digit)
			val (digits,carry) = mkdigits(new,Integer.-(i,1))
			val (digit,carry) = case (digit,carry) of
					 (9,1) => (0,1)
					| _ => (Integer.+(digit,carry),0)
		    in  (digit::digits,carry)
		    end
	      (* should eventually speed this up by using log10 *)
	      fun mkstr(f,e) =
		  if f >= ten then mkstr(f/ten,Integer.+(e,1))
		  else if f < one then mkstr(f*ten,Integer.-(e,1))
		  else let val (digits,carry) = mkdigits(f,15)
			   val (digits,e) = case carry of
					      0 => (digits,e)
					    | _ => (1::digits,Integer.+(e,1))
		       in  if Integer.>(e,~5) andalso Integer.<(e,15)
			   then normstr(digits,e)
			   else scistr(digits,e)
		       end
	  in
	      if r < zero then ^("~",mkstr(~r,0))
	      else mkstr(r,0)
	  end
      fun print r = (BasicIO.output(BasicIO.std_out,makestring r); r)
    end  (* structure Real *)

  structure System : SYSTEM =
   struct type object = Assembly.object
	  structure Assembly = Assembly
	  val interactive = Control.interactive
	  structure Control : CONTROL = Control (* interactive thinned out *)
	  structure Tags = struct
	    val width_tags = 4
	    val power_tags = 16
	    val tag_record =	1
	    val tag_array =	9
	    val tag_bytearray =	11
	    val tag_string =	15
	    val tag_embedded =	7
	    val tag_closure =	13
	    val tag_backptr =	5
	    val tag_forwarded =	3
	   end (* structure Tags *)
	  structure Timer : TIMER =
	     struct open Assembly
		    infix 4 > < >= <=
		    infix 6 + -
		    infix 7 *
		    open InLine
		    type timer = time * time
		    val start_timing = Assembly.timer
		    fun delta(s2,u2,s1,u1) =
			let val (sec,usec) = (s2-s1,u2-u1)
			    val (sec,usec) = if usec < 0
					     then (sec-1,usec+1000000)
					     else (sec,usec)
			in  (sec,usec)
			end
		    fun check_time (TIME{sec=start_s,usec=start_u},
				    TIME{sec=gstart_s,usec=gstart_u}) =
			let val (TIME{sec=curr_s,usec=curr_u},
			         TIME{sec=gcurr_s,usec=gcurr_u}) = Assembly.timer()
			    val (sec,usec) = delta(curr_s,curr_u,start_s,start_u)
			    val (g_sec,g_usec) = delta(gcurr_s,gcurr_u,
							gstart_s,gstart_u)
			    val (sec,usec) = delta(sec,usec,g_sec,g_usec)
			in  TIME{sec=sec,usec=usec}
			end
		end (* structure Timer *)
          val readfile = IO.readfile
	  val boot = Assembly.boot
	  val boot1 : string ->
		     ((int->object) -> object Array.array)
		    = InLinePrim.cast(boot)
	  val create_s = Assembly.create_s
	  val store_s : string * int * int -> unit
		    = InLinePrim.cast(Byte_array.store)
	  val pstruct = Assembly.pstruct
	  val useref = IO.useref
	  local
	      datatype A = unboxed | boxed of object
	      val cast = InLinePrim.cast
	  in  exception Boxity
	      val tuple : object -> object array
		    = cast(fn unboxed => raise Boxity
			    | x as boxed _ => x)
	      val string : object -> string = cast (fn x=>x)
	      val real : object -> real = cast (fn x=>x)
	      val int : object -> int
		    = cast(fn x as unboxed => x
			    | boxed _ => raise Boxity)
	  end
	  val exn_name : exn -> string = InLinePrim.cast(fn(_,ref s) => s)
   end (* structure System *)

    structure General : GENERAL =
     struct
      exception Bind
      exception Match
      exception Varstruct = Bind
      exception Interrupt = Assembly.Interrupt

      fun o(f,g) = fn x => f(g x)
      fun before(a,b) = a (* this may be renamed soon *)
      fun use f = Ref.! System.useref f
      val exportML = IO.exportML
      val export1 = IO.export1
      val system = fn s => Assembly.system(String.^(s,"\000\000"))
      val cd = fn s => Assembly.chdir(String.^(s,"\000\000"))

      exception Equal
      datatype 'a option = NONE | SOME of 'a
      type exn = exn
      type unit = unit
     end (* structure General *)

  end (* structure Pervasives *)


  structure System : SYSTEM = Pervasives.System
  structure Pervasives : PERVASIVES = Pervasives

  (* the following equality functions are temporary *)
  local
      open System.Tags
      exception Equal = Pervasives.General.Equal
      val cast = InLinePrim.cast
      val ieq : ('a * 'b) -> bool = cast InLinePrim.ieql
      datatype A = unboxed | boxed of System.Assembly.object
  in  fun polyequal (a as unboxed,b as unboxed) = ieq(a,b)
	| polyequal (a as boxed _,b as boxed _) =
		(* assumes identical tags, with string=embedded *)
		let val tag = InLinePrim.subscript(cast a,~1)
		    val tag = case tag of boxed _ => raise Equal
			    | _ => Pervasives.Integer.mod(
				    Pervasives.Integer.+(
			     Pervasives.Integer.*(
				      cast tag,2),1),power_tags)
		    val alen = Pervasives.Array.length(cast a)
		    val blen = Pervasives.Array.length(cast b)
		    fun maprec () =
			let val Sub = cast InLinePrim.subscript
			    fun m i =
			      if ieq(i,alen) then true
	 		      else if polyequal(Sub(a,i),Sub(b,i))
				   then m(Pervasives.Integer.+(i,1))
			      else false
			in m 0 end
		    fun mapstr () =
			let val Sub = cast InLinePrim.subscript
			    fun m i =
			      if Pervasives.Integer.>(i,alen) then true
	 		      else if ieq(Sub(a,i),Sub(b,i))
				   then m(Pervasives.Integer.+(i,4))
			      else false
			in m 1 end
		in  if ieq(tag,tag_closure) then raise Equal
		    else if ieq(a,b) then true
		    else if ieq(tag,tag_array) then false
		    else if ieq(tag,tag_bytearray) then false
		    else if InLinePrim.ineq(alen,blen) then false
		 	    else if ieq(tag,tag_record) then maprec()
		    else mapstr()
		end
	| polyequal _ = false
      val polyequal : 'a * 'a -> bool = cast polyequal
  end (* local open Tags... *)
  val intequal = InLinePrim.ieql : (int * int -> bool)
  val refequal = InLinePrim.ieql : ('a ref * 'a ref -> bool)
  val arrayequal = InLinePrim.ieql : ('a array * 'a array -> bool)
  val realequal = InLinePrim.feql : (real * real -> bool)
  val stringequal = Assembly.seql
  (* the above equality functions are temporary *)

  open Pervasives
  open Array BasicIO ExtendedIO Bool Byte_array Integer List Real Ref String
  open General

  val _ = (output(std_out,"hello there\n"); flush_out std_out)

end (* functor PervFunc *)

