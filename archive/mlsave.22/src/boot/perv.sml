structure Initial =
struct

structure  Inside : sig structure Pervasives : PERVASIVES
		        structure System : SYSTEM
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
= struct

 open Core

(* create a type-safe version of the InLine structure while preserving
   the inline property of the functions. *)
structure InLine =
  struct
    infix 7 * div
    infix 6 + -
    infix 4 < > <= >=
    infix 3 :=
    val ! : 'a ref -> 'a = InLine.!
    val op * : int * int -> int = InLine.*
    val op + : int * int -> int = InLine.+
    val op - : int * int -> int = InLine.-
    val op := : 'a ref * 'a -> unit = InLine.:=
    val op < : int * int -> bool = InLine.<
    val op <= : int * int -> bool = InLine.<=
    val op > : int * int -> bool = InLine.>
    val op >= : int * int -> bool = InLine.>=
    val alength : 'a array -> int = InLine.alength
    val boxed : 'a -> bool = InLine.boxed
    val cast : 'a -> 'b = InLine.cast
    val op div : int * int -> int = InLine.div
    val fadd : real * real -> real = InLine.fadd
    val fdiv : real * real -> real = InLine.fdiv
    val feql : real * real -> bool = InLine.feql
    val fge : real * real -> bool = InLine.fge
    val fgt : real * real -> bool = InLine.fgt
    val fle : real * real -> bool = InLine.fle
    val flt : real * real -> bool = InLine.flt
    val fmul : real * real -> real = InLine.fmul
    val fneg : real -> real = InLine.fneg
    val fneq : real * real -> bool = InLine.fneq
    val fsub : real * real -> real = InLine.fsub
    val ieql : int * int -> bool = InLine.ieql
    val ineq : int * int -> bool = InLine.ineq
    val makeref : 'a -> 'a ref = InLine.makeref
    val ordof : string * int -> int = InLine.ordof
    val slength : string -> int = InLine.slength
    val store : string * int * int -> unit = InLine.store
    val subscript : 'a array * int -> 'a = InLine.subscript
    val update : 'a array * int * 'a -> unit = InLine.update
    val	~ : int -> int = InLine.~
    val reql : 'a ref * 'a ref -> bool = InLine.ieql
    val aeql : 'a array * 'a array -> bool = InLine.ieql
  end


(* The datatype ref is defined in the built-in structure PrimTypes.
   It is not mention here because it has a unique representation; an
   explicit datatype declaration would destroy this representation.
   Similarly, there is no datatype specification in the REF signature
   itself. *)
structure Ref : REF = 
  struct
    infix 3 :=
    val ! = InLine.!
    val op := = InLine.:=
    fun inc r = r := InLine.+(!r,1)
    fun dec r = r := InLine.-(!r,1)
  end (* structure Ref *)
   
structure List : LIST =
  struct
    infixr 5 :: @
    open PrimTypes InLine
    exception Hd
    exception Tl
    exception Nth
    fun hd (a::r) = a | hd nil = raise Hd
    fun tl (a::r) = r | tl nil = raise Tl    
    fun null nil = true | null _ = false
    fun length l = 
	let fun j(k,nil) = k
	      | j(k, a::x) = j(k+1,x)
	 in j(0,l)
	end
    fun op @ (nil,l) = l
      | op @ (a::r, l) = a :: (r@l)
    fun rev l =
	let fun f (nil, h) = h
	      | f (a::r, h) = f(r, a::h)
	in  f(l,nil)
	end
    fun map f =
	let fun m nil = nil
	      | m (a::r) = f a :: m r
	in  m
	end
    fun fold f =
	 let fun f2 nil b = b
	       | f2 (e::r) b = f(e,(f2 r b))
	 in  f2
	 end
    fun revfold f =
	 let fun f2 nil b = b
	       | f2 (e::r) b = f2 r (f(e,b))
	 in  f2
	 end
    fun app f = let fun a2 (e::r) = (f e; a2 r) | a2 nil = () in a2 end
    fun revapp f = let fun a2 (e::r) = (a2 r; f e; ()) | a2 nil = () in a2 end
    fun nth(e::r,0) = e 
      | nth(e::r,n) = nth(r,n-1)
      | nth _ = raise Nth
    fun exists pred =
	let fun f nil = false
	      | f (hd::tl) = pred hd orelse f tl
	in  f
	end
  end (* structure List *)

structure Array : ARRAY =
  struct
    infix 3 sub
    open PrimTypes InLine
    type 'a array = 'a array
    exception Subscript
    val array = fn arg as (n,v) =>
		     if n<=0 then if n<0 then raise Subscript
					 else Assembly.array0
			     else Assembly.array arg
    val op sub = fn (a,i) =>
			if i<0 orelse i >= alength a then raise Subscript
			else subscript(a,i)
    val update = fn (a,i,v) => 
			if i<0 orelse i >= alength a then raise Subscript
			else update(a,i,v)
    val length = alength
    fun arrayoflist nil = cast(Assembly.array0)
      | arrayoflist (l as (e::r)) =
	  let val a = array(List.length l, e)
	      fun init ((e::r),n) = (update(a,n,e); init(r,n+1))
	        | init (nil,_) = ()
	  in  init(r,1); a
          end
  end (* structure Array *)

structure PreString : sig exception Substring
			  infix 6 ^
			  val substring : string * int * int -> string
			  val ^ : string * string -> string
		      end =
  struct
    open InLine
    exception Substring
    fun substring("",0,0) = "" (* never call create_s with 0 *)
      | substring("",_,_) = raise Substring
      | substring(s,i,0) = if i>=0 
			    then if boxed s then if i <= slength s
					         then "" else raise Substring
 					    else if i<=1 
					         then "" else raise Substring
			    else raise Substring
      | substring(s,0,1) = if boxed s then cast(ordof(s,0)) else s
      | substring(s,i,1) =
	     if boxed s then if i>=0 andalso i < slength s 
				    then cast(ordof(s,i))
				    else raise Substring
			else if ieql(i,0) then s else raise Substring
      | substring(s,i,len) = 
	  if boxed s andalso i>=0 andalso i+len <= slength s
		andalso len >= 0
	  then let val a = Assembly.create_s(len)
		   fun copy j = if ieql(j,len) then ()
				else (store(a,j,ordof(s,i+j)); copy(j+1))
	       in  copy 0; a
	       end
	  else raise Substring

    infix 6 ^
    fun op ^ ("",s) = s
      | op ^ (s,"") = s
      | op ^ (x,y) =
	  if boxed x 
	  then if boxed y
	       then let val xl = slength x and yl = slength y
			val a = Assembly.create_s(xl+yl)
			fun copyx n = if ieql(n,xl) then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
			fun copyy n = if ieql(n,yl) then ()
			      else (store(a,xl+n,ordof(y,n)); copyy(n+1))
		     in copyx 0; copyy 0; a
		    end
	      else let val xl = slength x
		       val a = Assembly.create_s(xl+1)
			fun copyx n = if ieql(n,xl) then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
		    in copyx 0; store(a,xl,cast y); a
		   end
	  else if boxed y		       
	       then let val yl = slength y
			val a = Assembly.create_s(1+yl)
			fun copyy n = if ieql(n,yl) then ()
			      else (store(a,1+n,ordof(y,n)); copyy(n+1))
		     in store(a,0,cast x); copyy 0; a
		    end
	      else let val a = Assembly.create_s 2
		    in store(a,0,cast x); store(a,1,cast y); a
		   end

  end (* structure PreString *)	   

abstraction ByteArray : BYTEARRAY =
  struct
    open InLine PreString
    infix 3 sub
    type bytearray = string
    exception Subscript
    exception Range
    fun array(len,v) =
	  if len<0 then raise Subscript
	  else if v<0 orelse v>=256 then raise Range
	  else if ieql(len,0) then Assembly.bytearray0
	  else let val a = Assembly.create_b len
		   fun init i = if ieql(i,len) then ()
				  else (store(a,i,v); init(i+1))
		   in  init 0; a
	       end
    val length = cast slength
    fun update(arg as (s,i,c)) =
	  if i<0 orelse i >= slength s then raise Subscript
	  else if c<0 orelse c>255 then raise Range
	  else store arg
    val op sub = fn (s,i) => if i<0 orelse i>= slength s then raise Subscript
			     else ordof(s,i)
    fun extract(ba,i,1) = if i<0 orelse i >= slength ba then raise Subscript
			  else cast(ordof(ba,i))
      | extract(s,i,len) = 
	  if i<0 orelse i+len > slength s orelse len<0 then raise Subscript
	  else if ieql(len,0) then Assembly.bytearray0
	  else let val a = Assembly.create_b len
		   fun copy j =  if ieql(j,len) then ()
				 else (store(a,j,ordof(s,i+j)); copy(j+1))
	       in  copy 0; a
	       end
    fun app f ba = 
	  let val len = slength ba
	      fun app' i = if i >= len then ()
			   else (f(ba sub i); app'(i+1))
	  in  app' 0
	  end
    fun revapp f ba = 
	  let fun revapp' i = if i < 0 then ()
			      else (f(ba sub i); revapp'(i-1))
	  in  revapp'(slength ba - 1)
	  end
    fun fold f ba x = 
	  let fun fold'(i,x) = if i < 0 then x else fold'(i-1, f(ordof(ba,i),x))
	  in  fold'(slength ba - 1, x)
	  end
    fun revfold f ba x = 
	  let val len = slength ba
	      fun revfold'(i,x) = if i >= len then x
				  else revfold'(i+1,f(ordof(ba,i),x))
	  in  revfold'(0,x)
	  end

  end (* abstraction ByteArray *)

structure PreLim =
  struct
	 val exn_name : exn -> string = InLine.cast(fn(_,ref s) => s)
	 val interactive = ref true
	 val prLambda = ref (fn () => ())
  end (* structure PreLim *)

structure PreStats =
  struct
    local open Assembly Ref
    in
    val zerotime = TIME{sec=0,usec=0}
    val lines = ref 0
    val parse = ref zerotime
    val translate = ref zerotime
    val codeopt = ref zerotime
    val convert = ref zerotime
    val cpsopt = ref zerotime
    val closure = ref zerotime
    val globalfix = ref zerotime
    val spill = ref zerotime
    val codegen = ref zerotime
    val execution = ref zerotime
    fun reset() = 
	(lines := 0;
	 parse := zerotime;
	 translate := zerotime;
	 codeopt := zerotime;
	 convert := zerotime;
	 cpsopt := zerotime;
	 closure := zerotime;
	 globalfix := zerotime;
	 spill := zerotime;
	 codegen := zerotime;
	 execution := zerotime)
    end
  end

abstraction IO : IO =
struct
  local open InLine Ref PreString PrimTypes in
  type bytearray = string
  exception Io_failure of string
  type instream = {filid : int, pos : int ref, len : int ref,
		   closed : bool ref, buf : bytearray, tty : bool ref}
  type outstream = {filid : int, pos : int ref,
		    closed : bool ref, buf : bytearray, tty : bool ref}

  fun isatty filid = Assembly.isatty filid

  val bufsize = 1024

  val std_in = {filid = 0, pos = ref 0, len = ref 0, 
		closed = ref false, buf = Assembly.create_b bufsize,
		tty = ref(isatty 0)}
  val std_out ={filid = 1, pos = ref 0, 
		closed = ref false, buf = Assembly.create_b bufsize,
		tty = ref(isatty 1)}

  (* should be flushed on exportML, flushed and closed on exportFn *)
  val outstreams = ref [std_out]
  (* should be closed on exportFn *)
  val instreams = ref [std_in]
  fun add_in s = instreams := s :: !instreams
  fun add_out s = outstreams := s :: !outstreams
  fun init_streams() =
	let val {pos=pos_in,tty=tty_in,closed=closed_in,len,...} = std_in
	    val {pos=pos_out,tty=tty_out,closed=closed_out,...} = std_out
	in  pos_in := 0; tty_in := isatty 0; closed_in := false; len := 0;
	    pos_out := 0; tty_out := isatty 1; closed_out := false;
	    PreLim.interactive := !tty_in;
	    instreams := [std_in];
	    outstreams := [std_out]
	end

  (* the filename passed to Assembly.openf must be zero-padded;
     you need 2 zeros in case padding a null string. *)
  val c00 = "\000\000"

  fun open_in s =
	let val f = Assembly.openf(s ^ c00,Assembly.READ)
			handle Assembly.SystemCall s =>
				raise Io_failure("open_in: " ^ s)
	    val s = {filid = f, pos = ref 0, closed = ref false,
		     len = ref 0,
		     buf = Assembly.create_b bufsize, tty = ref(isatty f)}
	in  add_in s; s
	end
  fun open_o mode = fn s =>
	let val f = Assembly.openf(s ^ c00,mode)
	    val s = {filid = f, pos = ref 0, closed = ref false,
		     buf = Assembly.create_b bufsize, tty = ref(isatty f)}
	in  add_out s; s
	end

  val open_out = open_o Assembly.WRITE
			handle Assembly.SystemCall s =>
				raise Io_failure("open_out: " ^ s)
  val open_append = open_o Assembly.APPEND
			handle Assembly.SystemCall s =>
				raise Io_failure("open_append: " ^ s)

  fun open_string s =
	if boxed s
	 then {filid = ~1, pos = ref 0, len = ref (slength s),
	       closed = ref false, tty = ref false,
	       buf = s}
         else {filid = ~1, pos = ref 0, len = ref 1,
	       closed = ref false, tty = ref false,
	       buf =let val a = Assembly.create_s 1
		      in  store(a,0,cast s); a
		      end}

  fun filbuf ({closed = ref true,...} : instream) =
	raise Assembly.SystemCall "closed stream" 
    | filbuf {filid = ~1,pos,len,...} = (len := 0; pos := 0)
    | filbuf {filid,pos,buf,len,...} =
	(pos := 0; len := Assembly.read(filid,buf))

  fun flush_out ({closed = ref true,...} : outstream) =
	raise Io_failure "flush_out: closed stream" 
    | flush_out {pos = ref 0,...} = ()
    | flush_out {filid,pos,buf,...} =
	(if Assembly.write(filid,buf,!pos) < !pos
	  then raise Io_failure "flush_out: write failed"
	  else pos := 0)
	 handle Assembly.SystemCall s =>
	 raise Io_failure("flush_out: " ^ s)
  val flsbuf = flush_out

  fun close_in ({closed = ref true,...} : instream) =
	raise Io_failure "close_in: closed stream"
    | close_in {filid = ~1,closed,...} = closed := true
    | close_in {filid,pos,len,closed,...} = 
	(closed := true; len := 0; pos := 0;
	 instreams := List.fold
			(fn(s as {filid=f,...}:instream,tl) =>
					if ieql(filid,f) then tl else s::tl)
			(!instreams)
			nil;
	 Assembly.close filid handle Assembly.SystemCall s =>
				raise Io_failure("close_in: " ^ s))
  fun close_out ({closed = ref true,...} : outstream) =
	raise Io_failure "close_out: closed stream"
    | close_out (f as {filid,pos,closed,...}) = 
	(flsbuf f; pos := 0; closed := true;
	 outstreams := List.fold
			(fn(s as {filid=f,...}:outstream,tl) =>
					if ieql(filid,f) then tl else s::tl)
			(!outstreams)
			nil;
	 Assembly.close filid handle Assembly.SystemCall s =>
				raise Io_failure("close_out: " ^ s))

  fun look ({closed = ref true,...} : instream) =
	raise Assembly.SystemCall "closed stream"
    | look (f as {len,pos,buf,...}) =
	    if !len > !pos
	    then cast(ordof(buf,!pos))
	    else (filbuf f; if ieql(!len,0) then "" else look f)
  fun lookahead f = look f handle Assembly.SystemCall s =>
			raise Io_failure("lookahead: " ^ s)
  fun end_of_stream ({closed = ref true,...} : instream) =
	raise Io_failure "end_of_stream: closed stream"
    | end_of_stream f = case look f of "" => true | _ => false
	handle Assembly.SystemCall s =>
			raise Io_failure("end_of_stream: " ^ s)

  fun biginput("",filid,k) =
	let val a = Assembly.create_s k
	    val len = Assembly.read(filid,a)
         in if len < k then PreString.substring(a,0,len)
		       else a
	end
    | biginput(s,filid,k) =
       if boxed s
	then let val len = slength s
	    val a = Assembly.create_s(k+len)
	    fun move ~1 = ()
	      | move i = (store(a,i+len,ordof(a,i)); move(i-1))
	    fun copy ~1 = ()
	      | copy i = (store(a,i,ordof(s,i)); copy(i-1))
	    val len' = Assembly.read(filid,a);
	 in move(len'-1); copy(len-1);
	    if len' < k then PreString.substring(a,0,len+len')
		       else a
	end
	else let val len = 1
	    val a = Assembly.create_s(k+len)
	    fun move ~1 = ()
	      | move i = (store(a,i+len,ordof(a,i)); move(i-1))
	    val len' = Assembly.read(filid,a);
	in  move(len'-1); store(a,0,cast s);
	    if len' < k then PreString.substring(a,0,len+len')
		       else a
	end

  fun input(f as {filid,pos,len,buf,closed,...} : instream) =
	let val filbuf = fn f => (filbuf f
				  handle Assembly.SystemCall s =>
				  raise Io_failure("input: " ^ s))
	    fun get i =
	      if !closed then raise Io_failure "input: closed stream"
	      else if ieql(i,1) then if !len > !pos
				       then let val s = cast(ordof(buf,!pos))
					    in  inc pos; s
					    end
				     else (filbuf f;
					   if ieql(!len,0) then "" else get 1)
	      else if i < 0
	        then raise Io_failure "input: negative character count"
	      else if (!len - !pos) >= i then let val s = substring(buf,!pos,i)
					      in  pos := !pos+i; s
					      end
	      else let val s = substring(buf,!pos, !len - !pos)
		       val j = i - (!len - !pos)
		   in  if (j >= bufsize) andalso (filid > ~1)
			 then let val chars = Assembly.fionread filid
				  val k = if j < chars then j else chars
			      in  if k>=bufsize then biginput(s,filid,k)
				  else (filbuf f;
					if ieql(!len,0) then s
					else s ^ get j)
			      end
		       else (filbuf f;
			     if ieql(!len,0) then s
			     else s ^ get j)
		   end
	in  get
	end

  fun input_line({closed = ref true,...} : instream) =
	raise Io_failure "input_line: closed stream"
    | input_line(f as {pos,len,buf,...}) =
	let val l = !len
	    val filbuf = fn f => (filbuf f
				  handle Assembly.SystemCall s =>
				  raise Io_failure("input_line: " ^ s))
	    fun next j = if ieql(j,l)
			   then let val s = substring(buf, !pos, l - !pos)
				in  filbuf f;
				    if ieql(!len,0) then s
				    else s ^ input_line f
				end
			    else if ieql(ordof(buf,j),10) then input f (j+1 - !pos)
			    else next(j+1)
	in  next (!pos)
	end

  fun output(f as {filid,buf,pos,tty,closed,...} : outstream) =
	let val write = fn x => (Assembly.write x
				 handle Assembly.SystemCall s =>
				 raise Io_failure ("output: " ^ s))
	    fun put s =
	      if !closed then raise Io_failure "output: closed stream"
	      else if boxed s
	        then let val l = slength s in
		      if l > 4 * bufsize
		      then (flsbuf f;
			   if Assembly.write(filid,s,l) < l
			     then raise Io_failure "output: write failed"
			   else ())
	              else let val i = ref 0
			    in  while !i < l
				 do let val c = ordof(s,!i)
				    in  store(buf,!pos,c);
					inc pos; inc i;
					if ieql(!pos,bufsize)
					   orelse (!tty andalso ieql(c,10))
					then flsbuf f else ()
				    end
			   end
		    end
	      else (store(buf,!pos,cast s);
			   inc pos;
			   if ieql(!pos,bufsize)
			      orelse (!tty andalso ieql(cast s,10))
			   then flsbuf f else ())
	in  put
	end

  fun can_input ({closed = ref true,...} : instream) =
	raise Io_failure "can_input: closed stream"
    | can_input {filid = ~1,pos,len,...} = !len - !pos
    | can_input {filid,pos,len,...} =
	!len - !pos + Assembly.fionread filid
		handle Assembly.SystemCall s => raise Io_failure("can_input: " ^ s)

  fun execute s =
	let val (r0,w1) = Assembly.pipe() handle Assembly.SystemCall s =>
				raise Io_failure("execute: " ^ s)
	    val (r1,w0) = Assembly.pipe() handle Assembly.SystemCall s =>
				raise Io_failure("execute: " ^ s)
(* There can be no handlers around the fork because the exit from the
   handler will change the stack, so the process will die if vfork
   was used. *)
	    val pid = Assembly.fork()
	in  if ieql(0,pid)
	    then (List.app (fn {filid,...} => Assembly.close filid)
			   (!instreams);
		  List.app (fn {filid,...} => Assembly.close filid)
			   (!outstreams);
		  Assembly.close r0;
		  Assembly.close w0;
		  Assembly.dup2(r1,0);
		  Assembly.dup2(w1,1);
		  Assembly.close r1;
		  Assembly.close w1;
		  Assembly.execute(s ^ c00);
(* The following line is necessary even though execute never
   returns.  When the fork is a vfork (as on the sun), care
   must be taken that the stack not be overwritten.  In this
   case, the code generator will pop the stack at the last
   action in a function; this next line prevents that from
   happening before the execute. *)
		  (std_in,std_out))
	    else if ieql(~1,pid)
	    then raise Io_failure "execute: could not create child process"
	    else (Assembly.close r1;
		  Assembly.close w1;
		  let val r = {filid = r0, pos = ref 0, len = ref 0, 
			       closed = ref false,
			       buf = Assembly.create_b bufsize,
			       tty = ref false}
		      val w = {filid = w0, pos = ref 0, 
			       closed = ref false,
			       buf = Assembly.create_b bufsize,
			       tty = ref false}
		  in  add_in r;
		      add_out w;
		      (r,w)
		  end)
	end

  fun is_term_in ({closed = ref true,...} : instream) =
	raise Io_failure "is_term_in: closed stream"
    | is_term_in {tty=ref x,...} = x
  fun is_term_out ({closed = ref true,...} : outstream) =
	raise Io_failure "is_term_out: closed stream"
    | is_term_out {tty=ref x,...} = x

  fun set_term_in ({closed = ref true,...} : instream,_) =
	raise Io_failure "set_term_in: closed stream"
    | set_term_in ({tty,...},t) = tty := t
  fun set_term_out ({closed = ref true,...} : outstream,_) =
	raise Io_failure "set_term_out: closed stream"
    | set_term_out ({tty,...},t) = tty := t

  fun exportML filename = 
	let val filid = Assembly.openf(filename ^ c00,Assembly.WRITE)
	in  List.app flush_out (!outstreams);
	    if Assembly.exportML filid
	    then ((* mark previously opened streams as closed
		     without flushing them *)
		  List.app (fn {closed,...} : outstream => closed := true)
			   (!outstreams);
		  List.app (fn {closed,...} : instream => closed := true)
			   (!instreams);
		  (* restore std_in and std_out *)
		  init_streams();
		  (* reset timing statistics *)
		  PreStats.reset();
		  true)
	    else (Assembly.close filid; false)
	end

  val use_f = ref (fn (_ : string) => ())
  val use_s = ref (fn (_ : instream) => ())
  fun use f = !use_f f
  fun use_stream s = !use_s s
  val reduce_r = ref (fn a : (unit -> unit) => a)
  val reduce : ('a -> 'b) -> ('a -> 'b) = fn x => cast(!reduce_r) x

  (* close/flush all streams; watch out, some may be damaged. *)
  fun cleanup () =
	let fun c_out s = close_out s handle Assembly.SystemCall _ => ()
	    fun c_in s = close_in s handle Assembly.SystemCall _ => ()
	in  List.app c_out (!outstreams);
	    List.app c_in (!instreams)
	end

  fun exportFn (filename,func) = 
	let val filid=Assembly.openf(filename ^ c00,Assembly.WRITE)
	    val pr = output std_out
	    fun restart arg =
		(init_streams(); func arg; cleanup())
		handle exn => (* global exception handler of the exported fn *)
			(pr "uncaught exception ";
			 pr (PreLim.exn_name exn);
			 pr "\n";
			 cleanup())
	in  cleanup();
	    use_f := (fn (_ : string) => ());
	    use_s := (fn (_ : instream) => ());
	    reduce_r := (fn (x:unit->unit) => x);
	    PreLim.prLambda := (fn () => ());
	    Assembly.exportFn (filid,restart)
	end

  end (* local open ... *)
end (* abstraction IO *)

structure Bool : BOOL =
  struct
    open PrimTypes (* for datatype bool *)
    fun not true = false
      | not false = true
    fun makestring true = "true"
      | makestring false = "false"
    local val pr = IO.output IO.std_out in
    fun print b = (pr(makestring b); b)
    end
  end (* structure Bool *)

structure String : STRING =
  struct
    open PrimTypes InLine
    infix 4 > < >= <=
    infix 6 ^
    type string = string
    fun length s = if boxed s then slength s else 1
    val size = length
    exception Substring = PreString.Substring
    val substring = PreString.substring
    fun explode s =
	  if boxed s
	    then let fun f(l,~1) = l
		       | f(l, i) = f(cast(ordof(s,i)) :: l, i-1)
		  in f(nil, slength s - 1)
		 end
	    else [s]
    val op ^ = PreString.^
    exception Chr
    fun chr i = if i<0 orelse i>255 then raise Chr else cast i
    exception Ord
    fun ord "" = raise Ord
      | ord s = if boxed s then ordof(s,0) else cast s
    val ordof = fn (s,i) =>
	  if boxed s
            then if i<0 orelse i>= slength s then raise Ord else ordof(s,i)
	    else if ieql(i,0) then cast s else raise Ord
    local val pr = IO.output IO.std_out in
    fun print s = (pr s; s)
    end
    fun implode (sl:string list) =
	  let val len = List.fold(fn(s,l) => length s + l) sl 0
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
				let val len = length s
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
    fun sgtr(_,"") = true
      | sgtr("",_) = false
      | sgtr(a,b) =
	if boxed a
         then if boxed b
	     then let val al = slength a and bl = slength b
		      val n = if al<bl then al else bl
		      fun f i = if ieql(i,n) then al > bl
				else if ieql(InLine.ordof(a,i),InLine.ordof(b,i)) then f(i+1)
				else InLine.ordof(a,i) > InLine.ordof(b,i)
		   in  f 0
		  end
	     else InLine.ordof(a,0) >= cast(b)
	 else if boxed b
	     then cast(a) > InLine.ordof(b,0)
	     else cast(a) > cast(b)  
    fun op <= (a,b) = Bool.not(sgtr(a,b))
    fun op < (a,b) = sgtr(b,a)
    fun op >= (a,b) = Bool.not(sgtr(b,a))
    val op > = sgtr
  end  (* structure String *)

structure Integer : INTEGER =
  struct
    infix 7 * div mod
    infix 6 + -
    infix 4 > < >= <=
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
    fun makestring i =
	  if i<0 then PreString.^("~", makestring(~i))
	  else if i<10 then InLine.cast(InLine.cast "0" + i)
	  else let val j = i div 10
	       in  PreString.^(makestring j, makestring(i-j*10))
	       end
    local val pr = IO.output IO.std_out in
    fun print i = (pr(makestring i); i)
    end
  end  (* structure Integer *)

structure Real : REAL =
  struct
    infix 7 * /
    infix 6 + -
    infix 4 > < >= <=
    type real = real
    exception Unimplemented_real_operator
    exception Float = Assembly.Float
    exception Floor
    structure Math = NewMath
    open Math PreString
    val ~ = InLine.fneg
    val op + = InLine.fadd
    val op - = InLine.fsub
    val op * = InLine.fmul
    val op / = InLine.fdiv
    val op > = InLine.fgt
    val op < = InLine.flt
    val op >= = InLine.fge
    val op <= = InLine.fle
    fun floor r = Assembly.floor r handle Float _ => raise Floor
    val zero = 0.0
    val one = 1.0
    val ten = 10.0
    fun abs x = if x < zero then ~x else x
    fun real 0 = zero
      | real n = if InLine.<(n,0)
		   then ~(real(InLine.~n))
		   else 2.0 * real(InLine.div(n,2))
			+ (case Integer.mod(n,2) of 0 => zero
						  | _ => one)
    fun makestring r =
	  let val itoa = Integer.makestring
	      fun scistr(a::b::tl,e) =
		    let fun trail nil = ""
			  | trail (0::tl) =
			    let val rest = trail tl
			    in  case rest of "" => ""
					   | _ => "0"^rest
			    end
			  | trail (hd::tl) = itoa hd ^ trail tl
			val rest = trail tl
		    in  itoa a ^ "." ^ itoa b ^ rest ^ "E" ^ itoa e
		    end
		 | scistr _ = "" (* prevents non-exhaustive match *)
	      fun normstr(digits,e) =
		    let fun n(nil,_) = ""
			  | n(hd::nil,0) = itoa hd ^ ".0"
			  | n(hd::tl,0) = itoa hd ^ "." ^ n(tl,~1)
			  | n(0::tl,d) =
			      let val rest = n(tl,InLine.-(d,1))
			      in  case (InLine.<(d,~1),rest) of
				    (true,"") => rest
				    | _ => "0" ^ rest
			      end
			  | n(hd::tl,d) = itoa hd ^ n(tl,InLine.-(d,1))
			fun header n =
			  let fun zeros 1 = ""
				| zeros n = "0" ^ zeros(InLine.-(n,1))
			  in  "0." ^ zeros n
			  end
		    in  if InLine.<(e,0)
			then header(InLine.~ e) ^ n(digits,e)
			else n(digits,e)
		    end
	      fun mkdigits(f,0) = (nil,if f < 5.0 then 0 else 1)
		| mkdigits(f,i) =
		    let	val digit = floor f
			val new = ten * (f - real digit)
			val (digits,carry) = mkdigits(new,InLine.-(i,1))
			val (digit,carry) = case (digit,carry) of
					 (9,1) => (0,1)
					| _ => (InLine.+(digit,carry),0)
		    in  (digit::digits,carry)
		    end
	      (* should eventually speed this up by using log10 *)
	      fun mkstr(f,e) =
		  if f >= ten then mkstr(f/ten,InLine.+(e,1))
		  else if f < one then mkstr(f*ten,InLine.-(e,1))
		  else let val (digits,carry) = mkdigits(f,15)
			   val (digits,e) = case carry of
					      0 => (digits,e)
					    | _ => (1::digits,InLine.+(e,1))
		       in  if InLine.>(e,~5) andalso InLine.<(e,15)
			   then normstr(digits,e)
			   else scistr(digits,e)
		       end
	  in  if r < zero then "~" ^ mkstr(~r,0)
	      else if InLine.feql(r,zero) then "0.0"
	      else mkstr(r,0)
	  end
    local val pr = IO.output IO.std_out in
    fun print r = (pr(makestring r); r)
    end
  end  (* structure Real *)

structure System : SYSTEM =
  struct
    type object = Assembly.object
    structure ByteArray : BYTEARRAY = ByteArray
    structure Assembly : ASSEMBLY = Assembly
    structure Control : CONTROL =
      struct
	structure Runtime : RUNTIMECONTROL =
	  struct
	    val {collected,collectedfrom,gcmessages,
 		 majorcollections,minorcollections} = Assembly.control
	  end
        structure MC : MCCONTROL =
	  struct
            val printArgs = ref false
            val printRet = ref false
            val bindContainsVar = ref true
            val bindExhaustive = ref true
            val matchExhaustive = ref true
            val matchRedundant = ref true
	    val expandResult = ref true
          end
        structure CG : CGCONTROL =
	  struct
	    structure M68 =
	      struct
	        val trapv = ref true
	      end
            val tailrecur = ref true
            val recordopt = ref true
            val tail = ref true
            val profile = ref false
            val closureprint = ref false
            val closureStrategy = ref 1
	    val path = ref false
            val hoist = ref true
            val reduce = ref true
          end
	structure Print : PRINTCONTROL =
	  struct
	    val printDepth = ref 5
	    val stringDepth = ref 70
	    val signatures = ref true
	  end
        val debugging = ref false
        val primaryPrompt = ref "- "
        val secondaryPrompt = ref "= "
        val internals = ref false
        val debugLook = ref false
        val debugCollect = ref false
        val debugBind = ref false
	val saveLambda = ref false
	val saveLvarNames = ref false
	val timings = ref false
        val reopen = ref false
      end (* structure Control *)
    structure Tags : TAGS = (* taken from runtime/tags.h *)
      struct
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
      struct
	open Assembly (* for datatype time *)
	open InLine PreString
	type timer = time * time
	val start_timer = timer
	fun delta(s2,u2,s1,u1) =
	      let val (sec,usec) = (s2-s1,u2-u1)
		  val (sec,usec) = if usec < 0
				     then (sec-1,usec+1000000)
				     else (sec,usec)
	      in  (sec,usec)
	      end
	fun check_timer (TIME{sec=start_s,usec=start_u},
			 TIME{sec=gstart_s,usec=gstart_u}) =
	      let val (TIME{sec=curr_s,usec=curr_u},
		       TIME{sec=gcurr_s,usec=gcurr_u}) = timer()
		  val (sec,usec) = delta(curr_s,curr_u,start_s,start_u)
		  val (g_sec,g_usec) = delta(gcurr_s,gcurr_u,gstart_s,gstart_u)
		  val (sec,usec) = delta(sec,usec,g_sec,g_usec)
	      in  TIME{sec=sec,usec=usec}
	      end
	fun check_timer_gc (TIME{sec=start_s,usec=start_u},
			 TIME{sec=gstart_s,usec=gstart_u}) =
	      let val (TIME{sec=curr_s,usec=curr_u},
		       TIME{sec=gcurr_s,usec=gcurr_u}) = timer()
		  val (g_sec,g_usec) = delta(gcurr_s,gcurr_u,gstart_s,gstart_u)
	      in  TIME{sec=g_sec,usec=g_usec}
	      end
	fun makestring(TIME{sec,usec}) =
	      let val filler = if usec <= 0 then ""
			       else if usec < 10 then "00000"
			       else if usec < 100 then "0000"
			       else if usec < 1000 then "000"
			       else if usec < 10000 then "00"
			       else if usec < 100000 then "0"
			       else ""
		  val itoa = Integer.makestring
	      in  itoa sec ^ "." ^ filler ^ itoa usec
	      end
	fun add_time(TIME{sec=s0,usec=u0},TIME{sec=s1,usec=u1}) =
	      let val (s,u) = (s0+s1,u0+u1)
		  val (s,u) = if u > 1000000 then (s+1,u-1000000)
			      else (s,u)
	      in  TIME{sec=s,usec=u}
	      end
      end (* structure Timer *)
    structure Stats : STATS =
      struct
	open Timer Ref PreStats Control.Runtime
	fun update(a,b) = a := add_time(!a,b)
	fun summary() =
	      let val pr = IO.output IO.std_out
		  val (total,garbagetime) = start_timer()
		  val itoa = Integer.makestring
	      in  pr (itoa(!lines));
		  pr " lines\n";
		  pr "parse, "; pr(makestring(!parse)); pr "s\n";
		  pr "translate, "; pr(makestring(!translate)); pr "s\n";
		  pr "codeopt, "; pr(makestring(!codeopt)); pr "s\n";
		  pr "convert, "; pr(makestring(!convert)); pr "s\n";
		  pr "cpsopt, "; pr(makestring(!cpsopt)); pr "s\n";
		  pr "closure, "; pr(makestring(!closure)); pr "s\n";
		  pr "globalfix, "; pr(makestring(!globalfix)); pr "s\n";
		  pr "spill, "; pr(makestring(!spill)); pr "s\n";
		  pr "codegen, "; pr(makestring(!codegen)); pr "s\n";
		  pr "execution, "; pr(makestring(!execution)); pr "s\n";
		  pr "total "; pr(makestring total); pr "s\n";
		  pr "collections: "; pr(itoa(!minorcollections));
		  pr " minor, "; pr(itoa(!majorcollections));
		  pr " major\n"; pr(itoa(!collected));
		  pr " collected from "; pr(itoa(!collectedfrom));
		  pr " possible (";
		  case !collectedfrom of 0 => () | _ =>
		  pr(itoa(Real.floor(Real.*(Real./(Real.real(!collected),
			Real.real(!collectedfrom)),100.0))));
		  pr "%)\n";
		  pr "garbagetime "; pr(makestring garbagetime); pr "s\n";
		  ()
	      end
	abstraction Timer : TIMER = Timer
      end (* structure Stats *)
    abstraction Timer : TIMER = Timer (* to hide timer datatype *)

    val version = "Standard ML of New Jersey, Version 0.22, 9 September 1988"
    val exn_name = PreLim.exn_name
    val prLambda = PreLim.prLambda
    val cleanup = IO.cleanup
    val interactive = PreLim.interactive
    val boot = Assembly.boot
    val cast = InLine.cast
    val create_s = Assembly.create_s
    val store_s : string * int * int -> unit
		    = InLine.cast ByteArray.update
    val dummy : object = cast()
    val pstruct = ref {core = dummy, initial=dummy, math=dummy}
    local
	datatype A = unboxed | boxed of object
	val cast = InLine.cast
    in  exception Boxity
	val tuple : object -> object array
		    = cast(fn unboxed => raise Boxity
			    | x as boxed _ => x)
	val string : object -> string = cast (fn x=>x)
	val real : object -> real = cast (fn x=>x)
	val int : object -> int
		    = cast(fn x as unboxed => x
			    | boxed _ => raise Boxity)
    end (* local datatype A ... *)
    local
	datatype A = unboxed | boxed of object
	open Tags InLine
    in
	datatype value = RECORDval of int * object
		       | ARRAYval of object array
		       | STRINGval of string
		       | BYTEARRAYval of ByteArray.bytearray
		       | CLOSUREval of object * int
		       | INTval of int
		       | OTHERval of object
	fun followpath (obj, [0]) = obj
          | followpath (obj, [n]) = cast(cast obj + n + n)
	  | followpath (obj, n::l) = followpath(subscript(cast obj,n),l)
	  | followpath(obj,_) = obj (* exhaustivematch *)
	val getvalue = cast (
	      fn (obj as unboxed) => INTval(cast obj)
	       | obj => 
		let val tag = subscript(cast obj,~1)
		    val tag = case tag of boxed _ => tag_closure
				 | _ => Integer.mod(cast tag * 2 + 1,power_tags)
		in  if ieql(tag,tag_closure) then
			 let val i = followpath(cast obj,[0,~2,0])
			 in  if ieql(i,0) then OTHERval(cast obj)
			     else CLOSUREval(cast obj,cast i)
			 end
		    else if ieql(tag,tag_record) then
			 RECORDval(2*(cast(subscript(cast obj,~1)) div power_tags),
				   cast obj)
		    else if ieql(tag,tag_string) orelse ieql(tag,tag_embedded)
			 then STRINGval(cast obj)
		    else if ieql(tag,tag_array) then ARRAYval(cast obj)
		    else if ieql(tag,tag_bytearray) then BYTEARRAYval(cast obj)
		    else OTHERval(cast obj)
		end)
    end (* local datatype A ... *)
    structure AA : sig
      datatype datalist = DATANIL | DATACONS of (string * string * datalist)
      val datalist : datalist
    end = Assembly
    open AA
  end (* structure System *)

structure General : GENERAL =
  struct
    infix 3 o
    infix before
    exception Bind = Bind
    exception Match = Match
    exception Interrupt = Assembly.Interrupt
    exception SystemCall = Assembly.SystemCall

    fun f o g = fn x => f(g x)
    fun a before b = a (* this may be renamed soon *)
    (* the filename passed to the Assembly system and chdir must
       be zero-padded; you need 2 zeros in case padding a null string. *)
    val c00 = "\000\000"
    val system = fn s => Assembly.system(PreString.^(s,c00))
    val cd = fn s => Assembly.chdir(PreString.^(s,c00))

    exception Equal
    datatype 'a option = NONE | SOME of 'a
    type exn = exn
    type unit = unit
  end (* structure General *)

structure Pervasives : PERVASIVES =
  struct
    structure Ref : REF = Ref
    structure List : LIST = List
    structure Array : ARRAY = Array
    structure ByteArray : BYTEARRAY = ByteArray
    structure IO : IO = IO
    structure Bool : BOOL = Bool
    structure String : STRING = String
    structure Integer : INTEGER = Integer
    structure Real : REAL = Real
    structure General : GENERAL = General
  end (* structure Pervasives *)


end (* structure Inside *)

open Inside
open Ref List Array IO Bool String Integer Real General
open Pervasives


val _ = output std_out "hello there\n"

end (* structure Initial *)
