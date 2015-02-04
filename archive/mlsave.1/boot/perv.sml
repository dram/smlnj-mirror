(* The following must be in the symbol table before this is parsed.
     1)  Magical words that can be free in signatures:
	     int string bool unit real list array ref instream outstream
     2)  Built-in constructors:
	      :: nil ref true false   (from PrimTypes as DataTypes)
     3)  Built-in structures:
	      PrimTypes 	  (matches DATATYPES)
              InLinePrim	  (matches INLINE)


NOTES:
	Make sure that all matches in this file are exhaustive!

*)

signature DATATYPES =
sig
    datatype bool = false | true
    datatype 'a list = op :: of 'a * 'a list | nil
    datatype 'a ref = ref of 'a
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
    val ordof : string * int -> int
    val slength : string -> int
    val store : string * int * int -> unit
    val subscript : 'a array * int -> 'a
(*    val unboxedassign : int ref -> int
    val unboxedupdate : int array * int * int -> unit *)
    val update : 'a array * int * 'a -> unit
    val	~ : int -> int
end    

signature ASSEMBLY =
sig
    datatype flags = READ | WRITE | APPEND
    exceptionx div : unit
    exceptionx float : string
    exceptionx interrupt : unit
    exceptionx io_failure : string
    exceptionx overflow : unit
    val array : int * 'a -> 'a array
    val array0: 'a array
    type Structure
    val boot : string -> unit -> ((Structure list -> Structure) * string list)
    val close : int -> unit
    val create : int -> string
    val export : int -> bool
    val fionread : int -> int
    val floor : real -> int
    val isatty : int -> bool
    val logb : real -> int
    val openf : string * flags -> int
    val pstruct : Structure ref
    val read : int * string -> int
    val scalb : real * int -> real
    val seql : string * string -> bool
    val write : int * string * int -> int
end


functor PervFunc(Assembly : ASSEMBLY) =
struct

  structure Pervasives =
  struct

    structure DataTypes : DATATYPES = PrimTypes
    structure InLine : INLINE = InLinePrim
   
    structure Ref: REF = 
     struct
       infix 3 :=
       open DataTypes InLine
       fun inc r = r := InLine.+(!r,1)
       fun dec r = r := InLine.-(!r,1)
       val op := = InLine.:=
     end
   
    structure List: LIST =
     struct
       infixr 5 :: 
       infix 5 @ 
       infix 6 + -
       open DataTypes InLine
       exceptionx hd : unit
       exceptionx tl : unit
       exceptionx nth : unit
       fun hd (a::r) = a | hd nil = raisex hd
       fun tl (a::r) = r | tl nil = raisex tl    
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
	 | nth _ = raisex nth
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
     exceptionx subscript
     val array = fn arg as (n,v) =>
		     if n<=0 then if n<0 then raisex subscript
					 else Assembly.array0
			     else Assembly.array arg
     val op sub = fn (a,i) =>
		if i<0 orelse i >= InLine.alength a then raisex subscript
			else InLine.subscript(a,i)
     val update = fn (a,i,v) => 
		if i<0 orelse i >= InLine.alength a then raisex subscript
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
      exceptionx substr
      fun substr("",0,0) = ""
	| substr("",_,_) = raisex substr
        | substr (s,0,1) = if ieql(slength s, 1) then s else cast(ordof(s,0))
        | substr(s,i,1) = if i<0 orelse i >= slength s
			    then raisex substr
			    else cast(ordof(s,i))
	| substr(s,i,len) = 
	    if i<0 orelse i+len > slength s orelse len<0
	      then raisex substr
	      else let val a = Assembly.create(len)
		       fun copy j = 
			   if ieql(j,len) then ()
			      else (store(a,j,ordof(s,i+j)); copy(j+1))
		    in copy 0; a
		   end
      fun sconcat("",s) = s
	| sconcat(s,"") = s
	| sconcat(x,y) =
	  let val xl = slength(x) and yl = slength(y)
	      val a = Assembly.create(xl+yl)
	      fun copyx n = if ieql(n,xl) then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
	      fun copyy n = if ieql(n,yl) then ()
			      else (store(a,xl+n,ordof(y,n)); copyy(n+1))
	   in if ieql(xl,1) then store(a,0,cast(x)) else copyx 0;
	      if ieql(yl,1) then store(a,xl,cast(y)) else copyy 0;
              a
	  end
    end (* structure PreString *)	   

    structure Byte_array: BYTE_ARRAY =
    struct
      infix 4 = < > <= >=
      infix 6 + -
      open InLine
      open PreString
      type byte_array = string
      exceptionx byte_array
      fun create(len,v) =
	  if len<0 orelse v<0 orelse v>=256
	    then raisex byte_array
	    else let val a = Assembly.create(len)
		     fun init i = if ieql(i,len) then () else (store(a,i,v); init(i+1))
		  in init 0; a
		 end
      val length = cast(slength)
      val store = fn arg as (s,i,c) =>
	  if i<0 orelse i>=slength(s) orelse c<0 orelse c>255
		  then raisex byte_array
		  else InLine.store arg
      val fetch = fn (s,i) =>
	  if i<0 orelse i>= slength s then raisex byte_array else ordof(s,i)
      fun extract(ba,i,1) = if i<0 orelse i >= slength ba
			    then raisex byte_array
			    else cast(ordof(ba,i))
	| extract(s,i,len) = 
	    if i<0 orelse i+len > slength s orelse len<0
	      then raisex byte_array
	      else let val a = Assembly.create(len)
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
	    if i < 0 then x else fold'(i-1,f(ordof(ba,i),x))
	 in fold'(slength ba - 1, x) end
  
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
       exceptionx io_failure : string = Assembly.io_failure
       type instream = {filid: int, pos: int ref, len: int ref,
		        closed : bool ref, buf: byte_array}
       type outstream = {filid: int, pos: int ref,
		        closed : bool ref, buf: byte_array, tty : bool ref}
   

        fun isatty filid = Assembly.isatty filid handlex io_failure => true

       val std_in = {filid=0, pos= ref 0, len= ref 0, 
		     closed = ref false, buf=Assembly.create(1024)}
       val std_out ={filid=1, pos= ref 0, 
		     closed = ref false, buf=Assembly.create(1024),
		     tty = ref(isatty 1)}
   
       val c00 = sconcat(cast(0),cast(0))
   
       fun open_in s = {filid=Assembly.openf(sconcat(s,c00),Assembly.READ),
			pos= ref 0, len=ref 0, closed = ref false,
			buf=Assembly.create(1024)}
       fun open_o mode = fn s =>
	    let val f = Assembly.openf(sconcat(s,c00),mode)
	     in {filid=f, pos= ref 0, closed = ref false,
		 buf=Assembly.create(1024), tty = ref(isatty f)}
	    end

       val open_out = open_o Assembly.WRITE
       val open_append = open_o Assembly.APPEND
   
       fun filbuf (f as {filid,pos,len,buf,closed}) =
		if !closed then raisex io_failure with "Input stream is closed"
		  else (len := Assembly.read(filid,buf);
		        pos := 0)
   
       fun flsbuf (f as {filid,pos,buf,closed,tty}) =
		if !closed then raisex io_failure with "Output stream is closed"
		  else (if InLine.<(Assembly.write(filid,buf,!pos),!pos)
		       then raisex io_failure with "failure in write"
		       else pos := 0)
   

       fun input(f as {filid,pos,len,buf,closed}, i) =
	   if (!len - !pos) >= i
	       then let val s = substr(buf,!pos,i)
		     in pos := !pos+i; s
		    end
	       else let val s = substr(buf,!pos, !len - !pos)
			val j = i - (!len - !pos)
		     in filbuf f;
			if ieql(!len,0) then s
			else sconcat(s, input(f,j))
		    end

       fun input_line(f as {filid,pos,len,buf,closed}) =
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
         | can_input _ = raisex io_failure with "can_input on closed file"

       fun execute s = raisex io_failure with "execute is unimplemented"
       fun is_term_in {filid,pos,len,buf,closed=ref false} = 
		 Assembly.isatty filid
         | is_term_in _ = raisex io_failure with "is_term on closed file"
       fun is_term_out {filid,pos,buf,closed,tty=ref x} = x

   fun ExportML filename = 
	let val filid=Assembly.openf(sconcat(filename,c00),Assembly.WRITE)
	    val {pos,len,...} = std_in
	    val {tty,...} = std_out
	 in flush_out std_out;
	    if Assembly.export filid
		 then (pos := 0; len := 0; tty := isatty 1)
		 else (Assembly.close filid)
	end

 fun readfile s = 
  let val filid=Assembly.openf(sconcat(s,c00),Assembly.READ)
      val b = Assembly.create 4
      val _ = if ieql(4,Assembly.read(filid,b)) then ()
		 else raisex io_failure with "no length"
      val len = ((ordof(b,0)*256+ordof(b,1))*256+ordof(b,2))*256+ordof(b,3)
      val c = Assembly.create len
      val _ = if ieql(len,Assembly.read(filid,c)) then ()
		 else raisex io_failure with "not long enough"
    in Assembly.close filid; c
   end
    end

    structure BasicIO : BASICIO = IO
    structure ExtendedIO : EXTENDEDIO = 
      struct
	    structure BasicIO = BasicIO
	    open IO
      end

    structure Bool : BOOL =
    struct
      open DataTypes
      fun not true = false
	| not false = true
      fun makestring true = "true"
	| makestring false = "false"
      fun print b = (BasicIO.output(BasicIO.std_out, makestring b); b)
    end

    structure String: STRING =
    struct
      open DataTypes InLine
      infix 4 = > < >= <=
      infixr 5 ::
      infix 6 + ^
      type string = string
      val length = slength
      val size = slength
      exceptionx substring = PreString.substr
      val substring = PreString.substr
      fun explode s =
	    case slength s
	       of 1 => [s]
	        | n => let fun f i = if ieql(i,n) then nil
			               else cast(ordof(s,i)) :: f(i+1)
		        in f 0
	               end
      val op ^ = PreString.sconcat
      exceptionx chr
      fun chr i = if i<0 orelse i>255 then raisex chr else cast i
      exceptionx ord
      fun ord s = case slength s of 1 => cast s | _ => raisex ord
      val ordof = fn (s,i) =>
	  case slength s
           of 1 => if ieql(i,0) then cast(s) else raisex ord
	    | n => if i<0 orelse i>= n then raisex ord else ordof(s,i)
      fun print s = (BasicIO.output(BasicIO.std_out,s); s)
      fun implode [x] = cast(ord(x))
	| implode l =
	  let val n = List.length l
	      val s = Assembly.create(n)
	      fun f (i,a::r) = (store(s,i,ord(a)); f(i+1,r))
		| f (_,nil) = ()
	   in f (0,l); s
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
    end

    structure Integer: INTEGER =
    struct
      infix 4 > < >= <=
      infix 6 + -
      infix 7 *
      infix 7 div mod
      exceptionx div : unit = Assembly.div
      exceptionx mod : unit
      exceptionx overflow : unit = Assembly.overflow
      type int = int
      val ~ = InLine.~
      val op * = InLine.*
      val op div = InLine.div
      fun op mod(a,b) = InLine.-(a,InLine.*(InLine.div(a,b),b))
		        handlex div => raisex mod
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
    end

    structure Real : REAL =
    struct
      infix 4 > < >= <=
      infix 6 + -
      infix 7 * /
      infixr 5 ::
      type real = real
      exceptionx unimplemented_real_operator : unit
      exceptionx floor:unit and sqrt:unit and exp:unit and ln:unit
      exceptionx float : string
      fun f(x) = raisex unimplemented_real_operator
      val ~ = InLine.fneg
      val op + = InLine.fadd
      val op - = InLine.fsub
      val op * = InLine.fmul
      val op / = InLine.fdiv
      val op > = InLine.fgt
      val op < = InLine.flt
      val op >= = InLine.fge
      val op <= = InLine.fle
      val sqrt = f
      val sin = f
      val cos = f
      val arctan = f
      val exp = f
      val ln = f

      val zero = 0.0
      val half = 0.5
      val one = 1.0
      val ten = 10.0
      val scalb = Assembly.scalb
      val logb = Assembly.logb
      val floor = Assembly.floor
      fun abs x = if x < zero then ~x else x
      fun real 0 = 0.0
	| real n = if Integer.<(n,0) then ~(real(Integer.~n))
				else 2.0 * real(Integer.div(n,2))
				+ (case Integer.mod(n,2) of 0 => 0.0
							  | 1 => 1.0)
      fun copysign(a,b) =
	    case (a<zero,b<zero) of
		    (true,true) => a
		  | (false,false) => a
		  | _ => ~a
      (* Not a true drem - rounding on .5 should go to the even case. *)
      (* Should be okay for our purposes. *)
      fun drem(x,y) =
	  let val N = real(floor(x/y+half))
	      val rem = x - N * y
	  in
	      rem
	  end
      fun makestring r =
	  let val itoa = Integer.makestring
	      val ^ = String.^
	      fun S(a::b::tl,e) =
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
	      fun N(digits,e) =
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
	      in  if Integer.<(e,0) then ^(header(Integer.~(e)),n(digits,e))
		  else n(digits,e)
	      end
	      fun F(f,0) = (nil,if f < 5.0 then 0 else 1)
		| F(f,i) =
		    let	val digit = floor f
			val new = ten * (f - real digit)
			val (digits,carry) = F(new,Integer.-(i,1))
			val (digit,carry) = case (digit,carry) of
					 (9,1) => (0,1)
					| _ => (Integer.+(digit,carry),0)
		    in  (digit::digits,carry)
		    end
	      (* should eventually speed this up by using log10 *)
	      fun M(f,e) =
		  if f >= ten then M(f/ten,Integer.+(e,1))
		  else if f < one then M(f*ten,Integer.-(e,1))
		  else let val (digits,carry) = F(f,15)
			   val (digits,e) = case carry of 0 => (digits,e)
						| 1 => (1::digits,Integer.+(e,1))
		       in  if Integer.>(e,~5) andalso Integer.<(e,15)
			   then N(digits,e)
			   else S(digits,e)
		       end
	  in
	      if r < zero then ^("~",M(~r,0))
	      else M(r,0)
	  end
      fun print r = (BasicIO.output(BasicIO.std_out,makestring r); r)
    end

  end (* structure Pervasives *)

  val readfile = Pervasives.IO.readfile
  val ExportML = Pervasives.IO.ExportML

  structure Pervasives : PERVASIVES = Pervasives

  type unit = unit

  exceptionx bind : unit
  exceptionx match : unit
  exceptionx varstruct = bind
  exceptionx interrupt : unit = Assembly.interrupt
  exceptionx Interrupt = interrupt

  fun o(f,g) = fn x => f(g x)

  open Pervasives
  open Array BasicIO ExtendedIO Bool Byte_array Integer List Real Ref String

  infix 3 o := sub
  infix 4 = <> < > <= >=
  infixr 5 ::
  infix 5 @
  infix 6 + - ^
  infix 7 * / div mod

  val intequal = InLinePrim.ieql : (int * int -> bool)
  fun refequal x = InLinePrim.ieql : ('a ref * 'a ref -> bool)
  val stringequal = Assembly.seql
  val realequal = InLinePrim.feql : (real * real -> bool)
  fun arrayequal (eq : 'a*'a->bool) (a : 'a array, b : 'a array) =
	let fun test ~1 = true
	      | test i = (if eq(a sub i, b sub i) 
			    then test(Integer.-(i,1)) 
			    else false)
	 in InLinePrim.ieql(Array.length a, Array.length b)
	    andalso test(Integer.-(Array.length a,1))
        end
	    
  structure Boot = 
   struct type Structure = Assembly.Structure
	  type Object = Structure
	  val boot = Assembly.boot
	  val boot1 : Byte_array.byte_array ->
		     ((int->Object) -> Object Array.array)
		    = InLinePrim.cast(boot)
	  val pstruct = Assembly.pstruct
	  val useref = ref (fn ( _ : string list) => ())
	  local datatype A = unboxed | boxed of Structure
		val cast = InLinePrim.cast
	   in exceptionx boxity
	      val tuple : Object -> Object Array.array
		    = cast(fn unboxed => raisex boxity
			    | x as boxed _ => x)
	      val string : Object -> string = cast (fn x=>x)
	      val real : Object -> real = cast (fn x=>x)
	      val int : Object -> int
		    = cast(fn x as unboxed => x
			    | boxed _ => raisex boxity)
	  end
   end

  val op = = InLinePrim.=
  val op <> = InLinePrim.<>

  fun use l = !Boot.useref l

  type exn = exn
  val exn_name : (exn -> string) = InLinePrim.cast(fn(_,ref (s:string)) => s)

  structure Assembly = Assembly

  fun system s = ()
  val _ = (output(std_out,"hello there\n"); flush_out std_out)

end (* functor PervFunc *)

