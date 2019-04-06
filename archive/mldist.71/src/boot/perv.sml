(* Copyright 1989 by AT&T Bell Laboratories *)
structure Initial =
struct

(* Define the List, System, IO, Bool, String and ByteArray structures.
   Do not signature match the ByteArray and String structures, to 
   preserve the inline properties of String.ordof, ByteArray.update,
   and ByteArray.sub *)

local
open Core

(* create a type-safe version of the InLine structure while preserving
   the inline property of the functions. *)
structure InLine =
  struct
    infix 7 * div
    infix 6 + -
    infix 4 < > <= >=
    infix 3 :=
    val capture : ('a cont -> 'a) -> 'a = InLine.capture
    val callcc : ('a cont -> 'a) -> 'a = InLine.callcc
    val throw : 'a cont -> 'a -> 'b = InLine.throw
    val ! : 'a ref -> 'a = InLine.!
    val op * : int * int -> int = InLine.*
    val op + : int * int -> int = InLine.+
    val op - : int * int -> int = InLine.-
    val op := : 'a ref * 'a -> unit = InLine.:=
    val op < : int * int -> bool = InLine.<
    val op <= : int * int -> bool = InLine.<=
    val op > : int * int -> bool = InLine.>
    val op >= : int * int -> bool = InLine.>=
    val lessu : int * int -> bool = InLine.lessu
    val gequ : int * int -> bool = InLine.gequ
    val alength : 'a array -> int = InLine.alength
    val boxed : 'a -> bool = InLine.boxed
    val unboxed : 'a -> bool = InLine.unboxed
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
    val fneq : real * real -> bool = InLine.fneq
    val fsub : real * real -> real = InLine.fsub
    val ieql : int * int -> bool = InLine.ieql
    val ineq : int * int -> bool = InLine.ineq
    val makeref : 'a -> 'a ref = InLine.makeref
    val ordof : string * int -> int = InLine.ordof
    val slength : string -> int = InLine.slength
    val store : string * int * int -> unit = InLine.store
    val byteof : Assembly.A.bytearray * int -> int = InLine.ordof
    val blength : Assembly.A.bytearray -> int = InLine.slength
    val bstore : Assembly.A.bytearray * int * int -> unit = InLine.store
    val subscript : 'a array * int -> 'a = InLine.subscript
    val update : 'a array * int * 'a -> unit = InLine.update
    val inlsubscript : 'a array * int -> 'a = InLine.inlsubscript
    val inlupdate : 'a array * int * 'a -> unit = InLine.inlupdate
    val inlbyteof : Assembly.A.bytearray * int -> int = InLine.inlbyteof
    val inlbstore : Assembly.A.bytearray * int * int -> unit = InLine.inlstore
    val inlordof: string * int -> int = InLine.inlordof
    val	~ : int -> int = InLine.~
    val reql : 'a ref * 'a ref -> bool = InLine.ieql
    val aeql : 'a array * 'a array -> bool = InLine.ieql
(*
    val floor : real -> int = InLine.floor
    val round : real -> int = InLine.round
    val real: int -> real = InLine.real   
*)
    val subscriptf : Assembly.A.bytearray * int -> real = InLine.subscriptf
    val updatef : Assembly.A.bytearray * int * real -> unit = InLine.updatef
    val inlsubscriptf : Assembly.A.bytearray * int -> real = InLine.inlsubscriptf
    val inlupdatef : Assembly.A.bytearray * int * real -> unit = InLine.updatef
    type 'a vector = 'a Assembly.A.vector 
    val subscriptv : 'a vector * int -> 'a = InLine.subscriptv
    val andb : int * int -> int = InLine.andb
    val orb : int * int -> int = InLine.orb
    val xorb : int * int -> int = InLine.xorb
    val rshift : int * int -> int = InLine.rshift
    val lshift : int * int -> int = InLine.lshift
    val notb : int -> int = InLine.notb
    val delay : int * 'a -> 'a = InLine.delay
    val getvar : unit -> 'a = InLine.getvar
    val setvar : 'a -> unit = InLine.setvar
  end  (* structure InLine *)


    val lookup_r : (int->Assembly.object) ref= InLine.cast(ref (fn _ => ()))
local open PrimTypes in
   val compreturn = ref nil : string list ref
end
    val toplevelcont : unit cont ref = let exception Die
	      in
		ref (InLine.callcc(fn k1 => (
		      InLine.callcc(fn k2 => (InLine.throw k1 k2));
		      raise Die)))
	      end

(* The datatype ref is defined in the built-in structure PrimTypes.
   It is not mention here because it has a unique representation; an
   explicit datatype declaration would destroy this representation.
   Similarly, there is no datatype specification in the REF signature
   itself. *)
structure Ref = 
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
    exception NthTail
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
    fun fold f [] = (fn b => b)
      | fold f (a::r) = (fn b => let fun f2(e,[]) = f(e,b)
				       | f2(e,a::r) = f(e,f2(a,r))
				 in f2(a,r)
				 end)
    fun revfold f [] = (fn b => b)
      | revfold f (a::r) = (fn b => let fun f2(e,[],b) = f(e,b)
					  | f2(e,a::r,b) = f2(a,r,f(e,b))
				    in f2(a,r,b)
				    end)	
    fun app f = let fun a2 (e::r) = (f e; a2 r) | a2 nil = () in a2 end
    fun revapp f = let fun a2 (e::r) = (a2 r; f e; ()) | a2 nil = () in a2 end
    fun nthtail(e,0) = e 
      | nthtail(e::r,n) = nthtail(r,n-1)
      | nthtail _ = raise NthTail
    fun nth x = hd(nthtail x) handle NthTail => raise Nth | Hd => raise Nth
    fun exists pred =
	let fun f nil = false
	      | f (hd::tl) = pred hd orelse f tl
	in  f
	end
  end (* structure List *)

structure PreString : sig exception Substring
			  infix 6 ^
			  val substring : string * int * int -> string
			  val ^ : string * string -> string
			  val imakestring : int -> string
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
	  then let val a = Assembly.A.create_s(len)
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
			val a = Assembly.A.create_s(xl+yl)
			fun copyx n = if ieql(n,xl) then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
			fun copyy n = if ieql(n,yl) then ()
			      else (store(a,xl+n,ordof(y,n)); copyy(n+1))
		     in copyx 0; copyy 0; a
		    end
	      else let val xl = slength x
		       val a = Assembly.A.create_s(xl+1)
			fun copyx n = if ieql(n,xl) then ()
			      else (store(a,n,ordof(x,n)); copyx(n+1))
		    in copyx 0; store(a,xl,cast y); a
		   end
	  else if boxed y		       
	       then let val yl = slength y
			val a = Assembly.A.create_s(1+yl)
			fun copyy n = if ieql(n,yl) then ()
			      else (store(a,1+n,ordof(y,n)); copyy(n+1))
		     in store(a,0,cast x); copyy 0; a
		    end
	      else let val a = Assembly.A.create_s 2
		    in store(a,0,cast x); store(a,1,cast y); a
		   end
    fun imakestring i =
	  if i<0 then "~" ^ imakestring(~i)
	  else if i<10 then InLine.cast(InLine.cast "0" + i)
	  else let val j = i div 10
	       in  imakestring j ^ imakestring(i-j*10)
	       end

  end (* structure PreString *)	   

(*abstraction ByteArray : BYTEARRAY = *)
structure ByteArray (* : BYTEARRAY*) =
  struct
   local open InLine PreString
   in
    infix 3 sub
    type bytearray = Assembly.A.bytearray
    exception Subscript = Core.Ord
    exception Range = Core.Range
    val length = blength
    fun array(len,v) =
	if len<0 then raise Subscript
	else if v<0 orelse v>=256 then raise Range
	else if ieql(len,0) then Assembly.bytearray0
	else let val a = Assembly.A.create_b len
		 fun init i = if ieql(i,len) then ()
			      else (bstore(a,i,v); init(i+1))
	      in init 0; a
	     end
    val update = InLine.inlbstore
    val op sub = InLine.inlbyteof
(*
    fun update(arg as (s,i,c)) =
	if i<0 orelse i >= length s then raise Subscript
	else if c<0 orelse c>255 then raise Range
	else bstore arg
    val op sub = fn (s, i) =>
          if lessu(i, length s) then byteof(s, i) else raise Subscript
*)
    fun extract(ba,i,1) =
	  if lessu(i, length ba) then cast(byteof(ba, i)) else raise Subscript
      | extract(ba,i,len) = 
	  if i<0 orelse i+len > length ba orelse len<0 then raise Subscript
	  else if ieql(len,0) then ""
	  else let val a = Assembly.A.create_s len
		   fun copy j =  if ieql(j,len) then ()
				 else (store(a,j,byteof(ba,i+j)); copy(j+1))
	       in  copy 0; a
	       end
    fun app f ba = 
	let val len = length ba
	    fun app' i = if i >= len then ()
			 else (f(ba sub i); app'(i+1))
	in  app' 0
	end
    fun revapp f ba = 
	let fun revapp' i = if i < 0 then ()
			    else (f(ba sub i); revapp'(i-1))
	in  revapp'(length ba - 1)
	end
    fun fold f ba x = 
	let fun fold'(i,x) = if i < 0 then x else fold'(i-1, f(byteof(ba,i),x))
	in  fold'(length ba - 1, x)
	end
    fun revfold f ba x = 
	let val len = length ba
	    fun revfold'(i,x) = if i >= len then x
				else revfold'(i+1,f(byteof(ba,i),x))
	in  revfold'(0,x)
	end
   end (*local*)
  end (* abstraction ByteArray *)

structure PreLim =
  struct
    val exn_name : exn -> string = InLine.cast(fn(_,ref s) => s)
    val interactive = ref true
    val prLambda = ref (fn () => ())
  end (* structure PreLim *)

structure Time =
  struct
    datatype time = TIME of {sec : int, usec : int}    
  end (* Time *)

structure PreStats =
  struct
    open Time
    local open Assembly.A Ref
    in
    val zerotime = TIME{sec=0,usec=0}
    val lines = ref 0
    val parse = ref zerotime
    val debuginstrum = ref zerotime
    val translate = ref zerotime
    val codeopt = ref zerotime
    val convert = ref zerotime
    val hoist = ref zerotime
    val cpsopt = ref zerotime
    val closure = ref zerotime
    val globalfix = ref zerotime
    val spill = ref zerotime
    val codegen = ref zerotime
    val freemap = ref zerotime
    val execution = ref zerotime
    val codesize = ref 0
    fun reset() = 
	(lines := 0;
	 parse := zerotime;
	 debuginstrum := zerotime;
	 translate := zerotime;
	 codeopt := zerotime;
	 convert := zerotime;
	 cpsopt := zerotime;
	 closure := zerotime;
	 globalfix := zerotime;
	 spill := zerotime;
	 codegen := zerotime;
	 freemap := zerotime;
	 execution := zerotime;
	 codesize := 0)
    end
  end

structure CInterface =
  struct
    open Time

    exception CFunNotFound of string

    fun c_function s = let
	  fun f (Assembly.FUNC(p,t,rest)) = if stringequal(s,t) then p else (f rest)
	    | f Assembly.FUNCNIL = raise (CFunNotFound s)
          val cfun = f Assembly.external
	  in
	    fn x => (Assembly.A.callc (cfun, x))
	  end

  (* zero pad a string to make it acceptable to C; two zeros are required in
   * case s is a null string. *)
    fun c_string s = PreString.^(s, "\000\000")

  (* type-safe interface to the C functions *)
    val argv	    : unit -> string list = c_function "argv"
    val environ	    : unit -> string list = c_function "environ"
    val gethostname : unit -> string = c_function "gethostname"
    val exec	    : (string * string list * string list) -> (int * int) =
	  c_function "exec"
    val system      : string -> int = c_function "system"
    val export      : int -> bool = c_function "export"
    val blas	    : (int * 'a) -> 'a = c_function "blas"
    val salb	    : string -> 'a = c_function "salb"
    val flush_cache : string -> unit = c_function "cach"
    val gc	    : int -> unit = c_function "gc"

    local
      val gettime' : unit -> (int * int * int * int * int * int) = c_function "gettime"
    in
    fun gettime () = let val (ts, tu, ss, su, gs, gu) = gettime' ()
	  in {
	    usr=TIME{sec=ts, usec=tu},
	    sys=TIME{sec=ss, usec=su},
	    gc=TIME{sec=gs, usec=gu}
	  } end
    end (* local *)

    local
      val setitimer' : (int * int * int * int * int) -> unit = c_function "setitimer"
    in
      fun setitimer (which, TIME{sec=s1, usec=u1}, TIME{sec=s2, usec=u2}) =
	    setitimer' (which, s1, u1, s2, u2)
    end (* local *)

  (* type-safe interface to some system calls *)
    val syscall	    : (int * string list) -> int = c_function "syscall"
    exception SystemCall of string
    exception SysError = Assembly.SysError

    val exit : int -> 'a = InLine.cast (fn x => syscall(1, [x]))
    val chdir : string -> unit = (fn s => (syscall(12, [c_string s]); ()))
    val getpid : unit -> int = (fn () => (syscall(20, [])))
    val getuid : unit -> int = (fn () => (syscall(24, [])))
    val getgid : unit -> int = (fn () => (syscall(47, [])))

    local open PreString in
    fun wrap_sysfn name f x = (f x)
	  handle SysError(_, s) => raise (SystemCall(name ^ " failed, " ^ s))
    end (* local *)
  end (* structure CInterface *)


structure SysIO =
  struct
    type bytearray = ByteArray.bytearray
    open Time

    type fd = int
    type fileid = string
    datatype fname	= DESC of fd | PATH of string
    datatype mode	= O_READ | O_WRITE | O_APPEND
    datatype whence	= L_SET | L_INCR | L_XTND
    datatype access	= A_READ | A_WRITE | A_EXEC
    datatype file_type	= F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK

    local
      open CInterface InLine
      fun sysfn name = (wrap_sysfn name (c_function name))
      fun fileOf (DESC fd) = (cast fd)
	| fileOf (PATH s) = (c_string s)
      infix 3 o
      fun f o g = (fn x => f(g x))
    in

    val dtablesize = Assembly.dtablesize;

    local
      val openf' : (string * int) -> fd = sysfn "open"
    in
    fun openf (path, mode) = let
	  val flag = case mode of O_READ => 0 | O_WRITE => 1 | O_APPEND => 2
	  in
	      openf' (c_string path, flag)
	  end
    end (* local *)

    val closef : fd -> unit = wrap_sysfn "close"
	  (fn filid => (syscall(6, [cast filid]); ()))
    val unlink : string -> unit = wrap_sysfn "unlink"
	  (fn name => (syscall(10, [c_string name]); ()))
    val mkdir : (string * int) -> unit = wrap_sysfn "mkdir"
	  (fn (path, mask) => (syscall(136, [c_string path, cast mask]); ()))
    val dup : fd -> fd = wrap_sysfn "dup"
	  (fn filid => (syscall(41, [cast filid])))

    val pipe	      : unit -> (int * int) = sysfn "pipe"
    val connect_unix  : string -> fd = (sysfn "connect_unix") o c_string
    val connect_inet  : (string * string) -> fd = let
	  val f = sysfn "connect_inet"
	  in
	    fn (s1, s2) => f(c_string s1, c_string s2)
	  end

    val read	  : (fd * bytearray * int) -> int = sysfn "read"
    val readi	  : (fd * bytearray * int * int) -> int = sysfn "readi"
    val write	  : (fd * bytearray * int) -> unit = sysfn "write"
    val writei	  : (fd * bytearray * int * int) -> unit = sysfn "writei"
    val writev	  : (fd * (bytearray * int) list) -> unit = sysfn "writev"
    val send_obd  : (fd * bytearray * int) -> unit = sysfn "send_obd"
    val getdirent : fd -> string list = sysfn "getdirent"
    val readlink  : string -> string = (sysfn "readlink") o c_string

    local
      val link' : (bool * string * string) -> unit = (c_function "link")
    in
    val link = wrap_sysfn "link"
		(fn (name, lname) => link'(false, c_string name, c_string lname))
    val symlink = wrap_sysfn "symlink"
		(fn (name, lname) => link'(true, c_string name, c_string lname))
    end

    local
      val truncate' : (string * int) -> unit = sysfn "truncate"
    in
    fun truncate (f, len) = truncate'(fileOf f, len)
    end

    local
      val lseek' : int * int * int -> int = wrap_sysfn "lseek"
	    (fn (d, off, w) => (syscall(19, [cast d, cast off, cast w])))
    in
      fun lseek (d, off, whence) = let
	    val w = case whence of L_SET => 0 | L_INCR => 1 | L_XTND => 2
	    in
	      lseek' (d, off, w)
	    end
    end

    local
      val chmod' : (string * int) -> unit = sysfn "chmod"
    in
    fun chmod (f, m) = chmod'(fileOf f, m)
    end

    local
      val access' : (string * int list) -> bool = sysfn "access"
      val map_mode = List.map (fn A_READ => 0 | A_WRITE => 1 | A_EXEC => 2)
    in
    fun access (path, alist) = access' (c_string path, map_mode alist)
    end (* local *)

    val umask : int -> int = wrap_sysfn "unlink" (fn m => (syscall(60, [cast m])))

    local
      val ftype' : string -> int = (sysfn "ftype")
    in
    fun ftype f = case (ftype' (fileOf f))
	   of 0 => F_REGULAR | 1 => F_DIR | 2 => F_SYMLINK
	    | 3 => F_SOCK | 4 => F_CHR | 5 => F_BLK
    end (* local val ftype' *)

    val getfid	    : fname -> fileid	    = sysfn "getfid" o fileOf
    val getmod	    : fname -> int	    = sysfn "getmod" o fileOf
    val isatty	    : int -> bool	    = sysfn "isatty"
    val fionread    : int -> int	    = sysfn "fionread"
    val getownid    : fname -> (int * int)  = (sysfn "getownid") o fileOf
    val fsize	    : fname -> int	    = (sysfn "fsize") o fileOf
    local
      fun fileTime f s = let val (s, u) = f (fileOf s) in TIME{sec=s, usec=u} end
    in
    val atime	    : fname -> time	    = fileTime (sysfn "atime")
    val ctime	    : fname -> time	    = fileTime (sysfn "ctime")
    val mtime	    : fname -> time	    = fileTime (sysfn "mtime")
    end

    local
      val select' : (int list * int list * int list * (int * int))
		      -> (int list * int list * int list) = (sysfn "select")
    in
    fun select (rfds, wfds, efds, t) = let
	  val timeout =
		case t of NONE => (cast 0) | SOME(TIME{sec, usec}) => (sec, usec)
	  in
	    select' (rfds, wfds, efds, timeout)
	  end
    end (* local val select' *)
    end (* local *)
  end (* SysIO *)


structure CleanUp =
  struct
    datatype clean_mode
      = CleanForExportML | CleanForExportFn | CleanForQuit | CleanForInit

    local
      open Ref List
      val cleaners = ref ([] : (string * (clean_mode -> unit)) list)
    in

  (* add the named cleaner, replacing the previous definition if necessary *)
    fun addCleaner (arg as (name, _)) = let
	  fun add ((x as (s, _))::r) = if (stringequal(name, s))
		then arg::r
		else (x::(add r))
	  val (newlist, res) = (add(!cleaners), false)
				  handle Match => (arg::(!cleaners), true)
	  in
	    cleaners := newlist; res
	  end
  (* remove the named cleaner; raise Match is not found *)
    fun removeCleaner name = let
	  fun remove ((x as (s, _))::r) = if (stringequal(name, s))
		then r
		else x::(remove r)
	  in
	    cleaners := remove(!cleaners)
	  end
  (* apply the list of cleaners *)
    fun cleanup mode =
	  app (fn (_, f) => ((f mode) handle _ => ())) (!cleaners)
  (* shutdown with cleanup *)
    fun shutdown () = (cleanup CleanForQuit; CInterface.exit 0)

    end (* local *)
  end (* CleanUp *)

structure Signals : SIGNALS = 
  struct
    local open Ref in

    val nsigs = 14

    datatype signal
      = SIGHUP | SIGINT | SIGQUIT | SIGALRM | SIGTERM | SIGURG
      | SIGCHLD | SIGIO | SIGWINCH | SIGUSR1 | SIGUSR2
      | SIGTSTP | SIGCONT (* not yet supported *)
      | SIGGC

    datatype sig_sts
      = ENABLED of ((int * unit cont) -> unit cont)
      | DISABLED

    val sigvec = Assembly.A.array(nsigs, DISABLED)

  (* sigHandler : (int * int * unit cont) -> unit cont
   * This is the root ML signal handler
   *)
    fun sigHandler (code, count, resume_k) = (
	  case (InLine.subscript(sigvec, code))
	   of DISABLED => resume_k
	    | (ENABLED handler) => handler (count, resume_k))

  (* Install the root handler *)
    val _ = (Assembly.sighandler := sigHandler)

    exception UnimplementedSignal

  (* convert SML signal names to run-time signal codes.  these must
   * agree with the codes in "runtime/ml_signal.h"
   *)
    fun sig2code SIGHUP	    = 0
      | sig2code SIGINT	    = 1
      | sig2code SIGQUIT    = 2
      | sig2code SIGALRM    = 3
      | sig2code SIGTERM    = 4
      | sig2code SIGURG	    = 5
      | sig2code SIGCHLD    = 6
      | sig2code SIGIO	    = 7
      | sig2code SIGWINCH   = 8
      | sig2code SIGUSR1    = 9
      | sig2code SIGUSR2    = 10
      | sig2code SIGTSTP    = (* 11 *) raise UnimplementedSignal
      | sig2code SIGCONT    = (* 12 *) raise UnimplementedSignal
      | sig2code SIGGC	    = 13

    val maskSigs : bool -> unit = CInterface.c_function "masksigs"

  (* signal masking *)
    local
      val maskLevel = ref 0
    in
      fun maskSignals true = (
	    case (!maskLevel)
	     of 0 => (maskSigs true; maskLevel := 1)
	      | n => (maskLevel := InLine.+(n, 1)))
	| maskSignals false = (
	    case (!maskLevel)
	     of 0 => ()
	      | 1 => (maskLevel := 0; maskSigs false)
	      | n => (maskLevel := InLine.-(n, 1)))
      fun masked () = (InLine.>(!maskLevel, 0))
    end (* local *)

    val enableSig : (int * bool) -> unit = CInterface.c_function "enablesig"

    fun setHandler (signal, newHandler) = let
	  val code = sig2code signal
	  in
	    maskSignals true;
	    case (newHandler, InLine.subscript(sigvec, code))
	     of (SOME h, DISABLED) => (
		  InLine.update (sigvec, code, ENABLED h);
		  enableSig(code, true))
	      | (SOME h, _) => InLine.update (sigvec, code, ENABLED h)
	      | (NONE, ENABLED _) => (
		  enableSig (code, false);
		  InLine.update (sigvec, code, DISABLED))
	      | _ => ()
	    (* end case *);
	    maskSignals false
	  end

    fun inqHandler signal = (
	  case (InLine.subscript(sigvec, sig2code signal))
	   of DISABLED => NONE
	    | (ENABLED handler) => SOME handler)

  (* sleep until the next signal (sigpause(2)) *)
    fun pause () = (CInterface.syscall(111, [InLine.cast 0]); ()) handle _ => ()

    end (* local open *)
  end (* Signals *)


(* Buffered I/O module.  In order to preserve the consistency of the buffers,
 * a number of operations must be done with signals masked.
 *)
structure PreIO = 
  struct
    exception Io of string
    local
      open InLine Ref PreString PrimTypes CInterface SysIO CleanUp
      val charOf : (ByteArray.bytearray * int) -> string = cast byteof
      type bytearray = ByteArray.bytearray
      val bufsize = 1024
      val close = wrap_sysfn "close" closef
      val pipe = wrap_sysfn "pipe" pipe
      fun error (cmd, name, s) = raise Io(cmd ^ " \"" ^ name ^ "\": " ^ s)
    (* protect a function call against signals *)
      fun protect f x = let
	    val _ = Signals.maskSignals true
	    val y = (f x) handle ex => (Signals.maskSignals false; raise ex)
	    in
	      Signals.maskSignals false; y
	    end
    (* system functions to wait for I/O *)
      val in_wait : fd -> unit = wrap_sysfn "in_wait" (c_function "wait_for_in")
      val out_wait : fd -> unit = wrap_sysfn "out_wait" (c_function "wait_for_out")
    in

    datatype instream = INSTRM of {
	    filid : int,	(* the file descriptor, ~1 for strings *)
	    name : string,	(* the file name *)
	    closed : bool ref,  (* true, if closed *)
	    tty : bool ref,	(* true, if this is a tty *)
	    buf : bytearray,	(* the buffer *)
	    pos : int ref,	(* the next character in the buffer to read *)
	    len : int ref,      (* the amount of data in the buffer *)
            at_eof: bool ref    (* whether the previous read returned 0 chars *)
	  }
    datatype outstream = OUTSTRM of {
	    filid : int,	(* the file descriptor *)
	    name : string,	(* the file name *)
	    closed : bool ref,  (* true, if closed *)
	    tty : bool ref,	(* true, if this is a tty *)
	    buf : bytearray,	(* the buffer *)
	    pos : int ref	(* the next character in the buffer to read *)
	  }

  (* the standard streams *)
    val std_in = INSTRM {
	    filid = 0, name = "<std_in>", closed = ref false, tty = ref(isatty 0),
	    buf = Assembly.A.create_b bufsize, pos = ref 0, len = ref 0,
	    at_eof=ref false
	  }
    val std_out = OUTSTRM {
	    filid = 1, name = "<std_out>", closed = ref false, tty = ref(isatty 1),
	    buf = Assembly.A.create_b bufsize, pos = ref 0
	  }
    val std_err = OUTSTRM {
	    filid = 2, name = "<std_err>", closed = ref false, tty = ref(isatty 2),
	    buf = Assembly.A.create_b bufsize, pos = ref 0
	  }

  (* the lists of open streams *)
    val instreams = ref [std_in]
    val outstreams = ref[std_out, std_err]

  (* add a stream to the stream lists *)
    val add_in = protect(fn s => (instreams := s :: !instreams))
    val add_out = protect(fn s => (outstreams := s :: !outstreams))

  (* remove a stream from the stream list *)
    fun remove_in (INSTRM{pos, ...}) = let
	  fun remove [] = []
	    | remove ((s as INSTRM{pos=p, ...})::r) =
		if ieql(cast pos, cast p) then r else s :: (remove r)
	  in
	    instreams := remove(!instreams)
	  end
    val remove_in = protect remove_in
    fun remove_out (OUTSTRM{filid, ...}) = let
	  fun remove [] = []
	    | remove ((s as OUTSTRM{filid=f, ...})::r) =
		if ieql(filid, f) then r else s :: (remove r)
	  in
	    outstreams := remove(!outstreams)
	  end
    val remove_out = protect remove_out

  (* open an input stream *)
    fun open_in s = let
	  val f = openf(s, O_READ)
		    handle (SystemCall message) =>
		      raise Io("open_in \"" ^ s ^ "\": " ^ message)
	  val s = INSTRM {
		  filid = f, name = s, closed = ref false, tty = ref(isatty f),
		  buf = Assembly.A.create_b bufsize, pos = ref 0, len = ref 0,
		  at_eof = ref false
		}
	  in
	    add_in s; s
	  end
  (* open a string as an input stream *)
    fun open_string s = let
	  val (buffer, n) = if (boxed s)
		then (cast s, slength s)
		else let val a = Assembly.A.create_b 1
		  in bstore(a, 0, cast s); (a, 1) end
	  in
	    INSTRM {
		filid = ~1, name = "<string>", closed = ref false, tty = ref false,
		buf = buffer, pos = ref 0, len = ref n, at_eof=ref false
	      }
	  end

  (* open an outstream in the given mode *)
    local
      fun open_o (mode, cmd) s = let
	    val f = openf(s, mode)
	    val s = OUTSTRM {
		    filid = f, name = s, closed = ref false, tty = ref(isatty f),
		    buf = Assembly.A.create_b bufsize, pos = ref 0
		  }
	    in
	      add_out s; s
	    end
	      handle (SystemCall msg) => error(cmd, s, msg)
    in
    val open_out = open_o (O_WRITE, "open_out")
    val open_append = open_o (O_APPEND, "open_append")
    end (* local *)

  (* fill an input stream buffer *)
    fun filbuf (INSTRM{closed = ref true, len, pos, at_eof,...}) = 
	               (len := 0; pos := 0; at_eof:= true)
      | filbuf (INSTRM{filid = ~1, pos, len, at_eof,...}) = 
                       (len := 0; pos := 0; at_eof:=true)
      | filbuf (INSTRM{filid, pos, buf, len, at_eof, ...}) = (
	  in_wait filid;
	  protect (fn _ => (pos := 0; len := read(filid, cast buf, bufsize);
			    at_eof := ieql(!len,0))) ())

  (* flush an output stream buffer *)
    fun flushbuf (OUTSTRM{closed = ref true, name, ...}) = ()
      | flushbuf (OUTSTRM{pos = ref 0,...}) = ()
      | flushbuf (OUTSTRM{filid, pos, buf, ...}) = (
	  out_wait filid;
	  protect (fn _ => (write(filid, buf, !pos); pos := 0)) ())

  (* flush an output stream (user's version) *)
    fun flush_out (f as OUTSTRM{name, ...}) = (flushbuf f)
	  handle (SystemCall msg) => error("flush_out", name, msg)

    fun flush_output_ttys((f as OUTSTRM{tty,...})::rest) =
	         (if !tty then flush_out f else (); flush_output_ttys rest)
      | flush_output_ttys nil = ()

    local
      fun close_file (fd, cmd, name) = (close fd)
	    handle (SystemCall msg) => error(cmd, name, msg)
    in
  (* close an instream *)
    fun close_in (INSTRM{closed = ref true, name,...}) = ()
      | close_in (INSTRM{filid = ~1, closed, len, pos,...}) = (
	  closed := true; len := 0; pos := 1)
      | close_in (f as INSTRM{filid, pos, len, closed, name,...}) = (
	  closed := true; len := 0; pos := 1; remove_in f;
	  close_file (filid, "close_in", name))
  (* close and flush an outstream *)
    fun close_out (OUTSTRM{closed = ref true,name,...}) = ()
      | close_out (f as OUTSTRM{filid,pos,closed,name,...}) = (
	  flushbuf f; pos := 0; closed := true; remove_out f;
	  close_file (filid, "close_out", name))
    end (* local *)

  (* return the next character in an instream *)
    fun look (INSTRM{closed = ref true,...}) = ""
      | look (INSTRM{at_eof = ref true,...}) = ""
      | look (f as INSTRM{len, pos, buf,...}) = if (!len > !pos)
	  then charOf(buf, !pos)
	  else (filbuf f; if ieql(!len, 0) then "" else look f)
  (* return the next character in an instream (user's version) *)
    fun lookahead (f as INSTRM{name, ...}) = (look f)
	  handle (SystemCall msg) => error("lookahead", name, msg)

  (* test an instream for EOF *)
    fun end_of_stream (INSTRM{closed = ref true, ...}) = true
      | end_of_stream (f as INSTRM{name, ...}) = (
	  case (look f) of "" => true | _ => false)
	    handle (SystemCall msg) => error("end_of_stream", name, msg)

  (* read a large amount of data *)
    fun biginput (filid, k) = let
	  val a = Assembly.A.create_s k
	  val len = read(filid, cast a, k)
	  in
	    if (ieql(len, k)) then a else PreString.substring(a, 0, len)
	  end

  (* input characters from an input stream (curried version) *)
    fun inputc (f as INSTRM{filid, pos, len, buf, closed, name, at_eof,...}) i = let
	  val remaining = (!len - !pos)
	  in
	    if (remaining >= i)
	      then if ieql(i, 1)
		then let
		  val p = !pos
		  in
		    pos := p+1; charOf(buf, p)
		  end
	      else if (i < 0)
		then error("input", name, "negative character count")
	      else let
		val s = ByteArray.extract(buf, !pos, i)
		in
		  pos := !pos + i; s
		end
	    else if (remaining > 0)
	      then let
		val s = ByteArray.extract(buf, !pos, remaining)
		in
		  pos := !len; s
		end
	    else if (!closed) orelse (filid < 0)
	      then ""
	    else let
              val _ = if ieql(filid,0) then flush_output_ttys (!outstreams)
				       else ()
	      val avail = if (i <= bufsize) then i
			    else let val c = fionread filid
			      in if (i < c) then i else c end
	      in
		if avail > bufsize
		  then (at_eof:=false; biginput (filid, avail))
		  else (filbuf f; if ieql(!len, 0) 
                        then ""
                        else inputc f i)
	      end
		handle (SystemCall s) => error("input", name, s)
	  end

  (* read some characters from an input stream *)
    fun input (f, i) = let
	  val s = inputc f i
	  val len = if boxed s then InLine.slength s else 1
	  in
	    if ieql(len, 0) then ""
	    else if ieql(len,i) then s
	    else (s ^ input(f, i - len))
	end

  (* read a line from an instream *)
    fun input_line (INSTRM{closed = ref true, ...}) = ""
      | input_line (f as INSTRM{pos, len, buf, name, filid, ...}) = let
	  val l = !len
	  fun next j = if ieql(j, l)
		then let
		  val s = ByteArray.extract(buf, !pos, l - !pos)
		  in 
                    if ieql(filid,0) then flush_output_ttys (!outstreams)
				     else ();
		    filbuf f;
		    if ieql(!len, 0) then s else s ^ input_line f
		  end
		else if (ieql(byteof(buf, j), 10 (* "\n" *)))
		  then inputc f ((j + 1) - !pos)
		  else next(j+1)
	   in
	      next (!pos)
	   end
	     handle (SystemCall s) => error("input_line", name, s)

  (* write a string to an outstream (curried version) *)
    fun outputc (f as OUTSTRM{filid, buf, pos, tty, closed, name,...}) s = (
	  if !closed
	    then error("output", name, "closed outstream")
	  else if (boxed s)
	    then let val l = slength s
	    in
	      if (l > 4 * bufsize)
		then (flushbuf f; write(filid, cast s, l))
		else let
		  val istty = !tty
		  fun loop (i, j) = if (i < l)
			then let val c = ordof(s, i) and k = j+1
			  in
			    bstore (buf, j, c);
			    if ieql(k, bufsize) orelse (istty andalso ieql(c, 10))
			      then (pos := k; flushbuf f; loop(i+1, 0))
			      else (loop(i+1, k))
			  end
			else (pos := j)
		  in
		    loop (0, !pos)
		  end
	    end
	  else (
	    bstore(buf,!pos,cast s);
	    pos := !pos + 1;
	    if ieql(!pos, bufsize) orelse (!tty andalso ieql(cast s, 10))
	      then (flushbuf f)
	      else ())
	  ) handle (SystemCall s) => error("output", name, s)
  (* write a string to an outstream *)
    fun output(f,s) = outputc f s

  (* return the number of available characters in an instream *)
    fun can_input (INSTRM{closed = ref true, ...}) = 0
      | can_input (INSTRM{filid = ~1, pos, len, ...}) = (!len - !pos)
      | can_input (INSTRM{filid, pos, len, name, ...}) = let
	  val n = (!len - !pos) + fionread filid
	  in
	    if (n < 0)
	      then error("can_input", name, "negative character count")
	      else n
	  end
	    handle SysError (_, s) => error("can_input", name, s)	

  (* execute the specified command (with our environment) *)
    fun execute cmd = let
	  val (fdin, fdout) = exec (c_string cmd, [], environ())
		handle (SysError(_,msg)) => error("execute", cmd, msg)
	  val r = INSTRM {
		  filid = fdin, name = "<pipe_in>", closed = ref false,
		  tty = ref false, buf = Assembly.A.create_b bufsize,
		  pos = ref 0, len = ref 0, at_eof=ref false
		}
	  val w = OUTSTRM {
		  filid = fdout, name="<pipe_out>", closed = ref false,
		  tty = ref false, buf = Assembly.A.create_b bufsize,
		  pos = ref 0
		}
	  in
	    add_in r; add_out w;
	    (r, w)
	  end

    fun is_term_in (INSTRM{tty=ref x,...}) = x
    fun is_term_out (OUTSTRM{tty=ref x,...}) = x

    fun set_term_in (INSTRM{tty, ...}, t) = (tty := t)
    fun set_term_out (OUTSTRM{tty, ...}, t) = (tty := t)

  (* export the current world to the given file *)
    fun exportML filename = let
	  val filid = openf (filename, O_WRITE)
	  in
	    cleanup CleanForExportML;
	    if (export filid)
	      then (
		cleanup CleanForInit;
		PreStats.reset();  (* reset timing statistics *)
		true)
	    else (close filid; false)
	end handle SysError(_,s) => error("exportML",filename,s)
		 | SystemCall s => error ("exportML",filename,s)

    val use_f = ref(fn s:string => ())
    val use_s : (instream -> unit) ref = ref(fn _:instream => ())
    fun use f = !use_f f
    fun use_stream s = !use_s s

    val debugInterface = 
	ref (cast(fn x:int => let exception DebuggerNotPresent
		         in raise DebuggerNotPresent
		        end): int -> unit);

  (* export a function to a file *)
    fun exportFn (filename, func) = let
	  val filid = openf (filename, O_WRITE)
		handle (SystemCall s) => error("exportFn", filename, s)
	  val pr = outputc std_out
	  in
	    cleanup CleanForExportFn;
	    use_f := (fn (_ : string) => ());
	    use_s := (fn (_ : instream) => ());
	    debugInterface := cast (fn () => ());
	    Core.getDebugf := cast (fn () => ());
	    lookup_r := cast(fn ()=>());
	    compreturn := [
	      cast(shutdown,
		fn e => (pr "uncaught exception "; pr (PreLim.exn_name e); pr "\n";
			  shutdown()),
		ref())];
	    PreLim.prLambda := (fn () => ());
	    Signals.maskSignals true;
	    toplevelcont := cast ();
	    if (export filid)
	      then (
		callcc (fn k => (
		  toplevelcont := k;
		  Signals.maskSignals false;
		  cleanup CleanForInit;
		  (func (argv(), environ()))
		    handle exn => (
		      pr "uncaught exception "; pr (PreLim.exn_name exn); pr "\n");
		  shutdown()));
		(* we can only get here via a throw to the top-level cont *)
		pr "\nInterrupt\n";
		shutdown())
	      else shutdown()
	  end (* exportFn *)

  (* blast objects *)
    fun blast_write (s as OUTSTRM{filid, name, ...}, obj) = (
	  flushbuf s; blas(filid, obj); ())
	    handle (SystemCall msg) => error("blast_write", name, msg)
	         | SysError(_,msg) => error("blast_write", name, msg)
    fun blast_read f = salb (input(f,(can_input f)))

  (* IO cleanup code *)
    fun cleanIO CleanForQuit = (
	(* close and flush all streams *)
	  List.app (fn s => ((close_in s) handle _ => ())) (!instreams);
	  List.app (fn s => ((close_out s) handle _ => ())) (!outstreams))
      | cleanIO CleanForInit = let
	(* mark all streams as closed *)
	  val _ = List.app (fn INSTRM{closed, ...} => closed := true) (!instreams)
	  val _ = List.app (fn OUTSTRM{closed, ...} => closed := true) (!outstreams)
	(* set up the standard streams *)
	  val INSTRM{pos=pos_in,tty=tty_in,closed=closed_in,len,...} = std_in
	  val OUTSTRM{pos=pos_out,tty=tty_out,closed=closed_out,...} = std_out
	  val OUTSTRM{pos=pos_err,tty=tty_err,closed=closed_err,...} = std_err
	  in
	    pos_in := 0; tty_in := isatty 0; closed_in := false; len := 0;
	    pos_out := 0; tty_out := isatty 1; closed_out := false;
	    pos_err := 0; tty_err := isatty 1; closed_err := false;
	    PreLim.interactive := !tty_in;
	    instreams := [std_in];
	    outstreams := [std_out, std_err]
	  end
      | cleanIO _ =  List.app flushbuf (!outstreams)  (* for export *)
    val _ = (addCleaner("IO", cleanIO))

  end (* local open ... *)
end (* structure PreIO *)

abstraction IO : IO = PreIO

structure Bool : BOOL =
  struct
    open PrimTypes (* for datatypes bool and option *)
    fun not true = false
      | not false = true
    fun makestring true = "true"
      | makestring false = "false"
    local val pr = IO.outputc IO.std_out in
    fun print b = pr(makestring b)
    end
  end (* structure Bool *)


structure String (*: STRING*) =
  struct
    local open PrimTypes InLine
    in 
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
    fun chr i = if lessu(i, 256) then (cast i) else raise Chr
    exception Ord = Core.Ord
    fun ord "" = raise Ord
      | ord s = if boxed s then ordof(s,0) else cast s
    val ordof = InLine.inlordof
(*    val ordof = fn (s,i) =>
	  if boxed s
            then if lessu(i, slength s) then ordof(s, i) else raise Ord
	    else if ieql(i,0) then cast s else raise Ord
*)
    val print = IO.outputc IO.std_out
    fun implode (sl:string list) =
	  let val len = List.fold(fn(s,l) => length s + l) sl 0
	  in  case len
	       of 0 => ""
		| 1 => let fun find (""::tl) = find tl
			     | find (hd::_) = cast hd
			     | find nil = "" (* impossible *)
		       in  find sl
		       end
		| _ => let val new = Assembly.A.create_s len
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
    local fun sgtr("","") = false
      | sgtr(_,"") = true
      | sgtr("",_) = false
      | sgtr(a,b) =
	if boxed a
        then if boxed b
	     then let val al = slength a and bl = slength b
		      val n = if al<bl then al else bl
		      fun f i = 
			  if ieql(i,n) then al > bl
			  else if ieql(InLine.ordof(a,i),InLine.ordof(b,i)) then f(i+1)
			  else InLine.ordof(a,i) > InLine.ordof(b,i)
		   in  f 0
		  end
	     else InLine.ordof(a,0) >= cast(b)
	else if boxed b
	     then cast(a) > InLine.ordof(b,0)
	     else cast(a) > cast(b)  
    in fun op <= (a,b) = Bool.not(sgtr(a,b))
       fun op < (a,b) = sgtr(b,a)
       fun op >= (a,b) = Bool.not(sgtr(b,a))
       val op > = sgtr
    end (*local fun sgtr*)
   end (*local open*)
  end  (* structure String *)

structure System : SYSTEM =
  struct
    structure ByteArray : BYTEARRAY = ByteArray
    structure Control : CONTROL =
      struct
	structure Runtime : RUNTIMECONTROL = Assembly
        structure MC : MCCONTROL =
	  struct
            val printArgs = ref false
            val printRet = ref false
            val bindContainsVar = ref true
            val bindExhaustive = ref true
            val matchExhaustive = ref true
            val matchRedundant = ref true
	    val expandResult = ref false
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
	    val closureStrategy = ref 0
	    val rounds = ref 1
	    val path = ref false
	    val betacontract = ref true
	    val eta = ref true
	    val selectopt = ref true
	    val dropargs = ref true
	    val deadvars = ref true
	    val flattenargs = ref true
	    val switchopt = ref true
	    val handlerfold = ref true
	    val branchfold = ref true
	    val arithopt = ref true
	    val betaexpand = ref true
	    val hoistup = ref true
	    val hoistdown = ref true
	    val maxregs = ref 1000
	    val recordcopy = ref true
	    val tagopt = ref true
	    val machdep = ref true
	    val recordpath = ref true
	    val misc1 = ref false
	    val misc2 = ref true
	    val misc3 = ref 0
	    val misc4 = ref 0
	    val hoist = ref false
	    val argrep = ref true
	    val reduce = ref false
	    val bodysize = ref 0
	    val reducemore = ref 15
	    val alphac = ref true
	    val comment = ref false
	    val knowngen = ref 0
	    val stdgen = ref 0
	    val knowncl = ref 0
	    val foldconst = ref true
	    val etasplit = ref true
	    val printit = ref false
	    val printsize = ref false
	    val scheduling = ref true
	    val cpsopt = ref true
            val cse = ref false
	    val optafterclosure = ref false
	    val calleesaves = ref(Assembly.calleesaves)
	    val extraflatten = ref true
            val uncurry = ref true
	    val ifidiom = ref true
            val comparefold = ref true
            val csehoist = ref false
            val rangeopt = ref false
            val floatargs = ref 0
            val floatvars = ref 0
            val floatreg_params = ref true
	    val icount = ref false
          end
	structure Print : PRINTCONTROL =
	  struct
	    val printDepth = ref 5
	    val printLength = ref 12
	    val stringDepth = ref 70
	    val signatures = ref true
	  end
        structure ProfileInternals : PROFILEINTERNALS =
	  struct
            structure ByteArray = ByteArray
	    open InLine
            val other = ByteArray.array(20,0)
	    val toplevel : ByteArray.bytearray = cast(Core.toplevel)
            val gc : ByteArray.bytearray = cast Assembly.gcprof
 	    val globalProfile = ref [other]

	    fun setToplevel () = 
		cast(Assembly.current) := toplevel

	    fun setOther () = 
		cast(Assembly.current) := other

	    fun listofarray a =
		let fun loop(~1,x) = x
		      | loop(i,x) = loop(i-1, InLine.subscript(a,i) :: x)
		in loop(InLine.alength a - 1, nil)
		end

            fun zeroCount e = (InLine.update(cast e,0,0);
			       InLine.update(cast e,1,0))

	    local
	      open PreStats
	      val t0 = TIME{sec=0, usec=0} and t10 = TIME{sec=0, usec=10000}
	    in
	    val timerval = (1 (* ITIMER_VIRTUAL *), t10, t10)
	    val timerval0 = (1 (* ITIMER_VIRTUAL *), t0, t0)
	    end

	    fun copy(ba,str) =
		let val len = slength str
		    fun loop i = 
			if ieql(i,len)
			then ()
			else (ByteArray.update(ba,i,ordof(str,i)); loop(i+1))
		 in loop 0
		end

	    val _ = copy(other,   "aaaaaaaa(unprofiled)")
	    val _ = copy(toplevel,"aaaaaaaa(toplevel)")
	    val _ = copy(gc,      "aaaaaaaa(gc)")

	    fun add profile =
	        if boxed profile
		then let val entries = listofarray profile
		      in List.app zeroCount entries;
		         globalProfile := List.@(entries,!globalProfile)
		     end
		else ()

	    fun extractInt(ba,start,len) =
		!(cast(ByteArray.extract(ba,start,len)):int ref)

	    fun trans ba =
		(extractInt(ba,0,4), extractInt(ba,4,4),
		 ByteArray.extract(ba,8,ByteArray.length ba - 8))

	    local fun insert (a,nil) = [a]
	            | insert (a0 as (a',a,_),l as (x0 as (x',x,_))::y) =
		       if a<x orelse ieql(a,x) andalso a'<x'
			 then x0::insert(a0,y)  else a0::l
	      in  fun sort nil = nil
	            | sort (a::b) = insert(a,sort b)
             end


	    fun field (st,w) = 
		let val s = PreString.^("                    ", st)
		 in PreString.substring(s,String.length s - w, w)
		end

	    fun decimal(st,w) =
	      PreString.^(PreString.^(
			        (PreString.substring(st,0,String.length st - w)
				  handle PreString.Substring => "")
			        ,"."),
		    let val st' = PreString.^("0000000000",st)
		     in PreString.substring(st',String.length st' - w,w)
		    end)

	    fun muldiv(i,j,k) =
		(i*j div k) 
		   handle Assembly.Overflow => muldiv(i,j div 2, k div 2)

	    fun decfield(n,j,k,w1,w2) = 
		field(decimal(PreString.imakestring (muldiv(n,j,k)),w1)
			handle Assembly.Div => "",w2)

	    fun printReport f l = 
	      let val l' as (_,_,_,ticks)::_
		    = List.fold (fn((n,m,s),l as (_,_,_,cum)::_)=>(n,m,s,m+cum)::l
				  |((n,m,s),nil)=>[(n,m,s,m)]) (sort l) nil
		  val pr = IO.outputc f
	      in pr(field("%time",6));
		 pr(field("cumsecs",9));
		 pr(field("#call",10));
		 pr(field("ms/call",10));
		 pr("  name\n");
		 List.app (fn (n,m,s,cumm) =>
			    (pr(decfield(m,10000,ticks,2,6));
			     pr(decfield((ticks-cumm+m),2,1,2,9));
			     pr(field(PreString.imakestring n,10));
			     pr(decfield(m,50000,n,4,10));
			     pr "  "; pr s; pr "\n"))
		          l';
		 ()
	       end

           structure P =
	     struct
	       structure IO = IO
	       val profiling = ref false
	       fun profileOn () = (CInterface.setitimer timerval; ())
	       fun profileOff () = (CInterface.setitimer timerval0; ())
	       fun reset () = List.app zeroCount (!globalProfile)
	       fun clear () = (globalProfile := [toplevel,gc,other]; reset())
	       fun report f =
		   printReport f (List.map trans (!globalProfile))
	       val _ = clear()
             end
           val add = InLine.cast add
	  end (* structure ProfileInternals *)
        structure Profile = ProfileInternals.P
        structure Debug : DEBUG =
          struct
	    val debugging = ref false
	    val getDebugf = InLine.cast Core.getDebugf
	    val interface = InLine.cast PreIO.debugInterface
          end
	val prLambda = PreLim.prLambda
        val debugging = ref false
        val primaryPrompt = ref "- "
        val secondaryPrompt = ref "= "
        val internals = ref false
	val weakUnderscore = ref false
        val interp = ref false
        val debugLook = ref false
        val debugCollect = ref false
        val debugBind = ref false
	val saveLambda = ref false
	val saveLvarNames = ref false
	val timings = ref false
        val reopen = ref false
        val markabsyn = ref true
        val indexing = ref false
      end (* structure Control *)
    structure Tags : TAGS =
      struct
	datatype tag = TAG of int
      (* taken from runtime/tags.h *)
	val width_tags = 4
	val tag_record		= TAG 1
	val tag_array		= TAG 9
	val tag_bytearray	= TAG 11
	val tag_string		= TAG 15
	val tag_real		= tag_string
	val tag_embedded_string	= TAG 7
	val tag_embedded_real	= tag_embedded_string
	val tag_special		= TAG 13
	val tag_backptr		= TAG 5
      (* build a descriptor from a tag and length *)
	fun make_desc (len, TAG t) = InLine.orb(InLine.lshift(len, width_tags), t)
      (* fixed descriptors *)
	val desc_real = make_desc(8, tag_real)
	val desc_embedded_real = make_desc(8, tag_embedded_real)
      (* special descriptors *)
	val desc_unevaled_susp = make_desc(0, tag_special)
	val desc_evaled_susp = make_desc(1, tag_special)
	val desc_weak = make_desc(2, tag_special)
	val desc_nulled_weak = make_desc(3, tag_special)
      end (* structure Tags *)
    abstraction Weak : WEAK =
      struct
	type 'a weak = 'a
	val weak_desc = InLine.div(Tags.desc_weak, 2)
	fun weak (x : 'a) : 'a weak = InLine.cast(InLine.delay(weak_desc, x))
	fun strong (x : 'a weak) : 'a option =
	      if (InLine.ieql(InLine.subscript(InLine.cast x, ~1), weak_desc))
		then SOME(InLine.subscript(InLine.cast x, 0))
		else NONE
      end (* Weak *)
    structure Timer =
      struct
	open Time InLine PreString

	datatype timer = Timer of {usr : time, sys : time, gc : time}
	fun timer () = Timer(CInterface.gettime())
           
	val start_timer = timer
	fun sub_time (TIME{sec=s2,usec=u2}, TIME{sec=s1,usec=u1}) = let
	      val (s, u) = (s2-s1, u2-u1)
	      val (s, u) = if (u < 0) then (s-1, u+1000000) else (s, u)
	      in
		TIME{sec=s, usec=u}
	      end
	fun check_timer (Timer{usr=u_start, sys=s_start, gc=g_start}) = let
	      val (Timer{usr=u_cur, sys=s_cur, gc=g_cur}) = timer()
	      in
		sub_time(sub_time(u_cur, u_start), sub_time(g_cur, g_start))
	      end
	fun check_timer_sys (Timer{sys=s_start, ...}) = let
	      val (Timer{sys=s_cur, ...}) = timer()
	      in
		sub_time(s_cur, s_start)
	      end
	fun check_timer_gc (Timer{gc=g_start, ...}) = let
	      val (Timer{gc=g_cur, ...}) = timer()
	      in
		sub_time(g_cur, g_start)
	      end
	fun makestring(TIME{sec,usec}) =
	      let val filler = if usec <= 0 then ""
			       else if usec < 10 then "00000"
			       else if usec < 100 then "0000"
			       else if usec < 1000 then "000"
			       else if usec < 10000 then "00"
			       else if usec < 100000 then "0"
			       else ""
	      in  imakestring sec ^ "." ^ filler ^ imakestring usec
	      end
	fun add_time (TIME{sec=s0,usec=u0},TIME{sec=s1,usec=u1}) =
	      let val (s,u) = (s0+s1,u0+u1)
		  val (s,u) = if u > 1000000 then (s+1,u-1000000)
			      else (s,u)
	      in  TIME{sec=s,usec=u}
	      end
	fun earlier (TIME{sec=s1,usec=u1}, TIME{sec=s2,usec=u2}) = (
	      (s1 < s2) orelse (ieql(s1, s2) andalso (u1 < u2)))
      end (* structure Timer *)
    structure Stats : STATS =
      struct
	open Timer Ref PreStats Control.Runtime PreString
        fun update(a,b) = if earlier(zerotime,b)
                             then a := add_time(!a, b) else ()
	fun summary() =
	      let val pr = IO.outputc IO.std_out
		  fun prTime t = (pr(makestring t); pr "s\n")
		  val Timer{usr=total,sys=system,gc=garbagetime} = timer()
	      in  pr (imakestring(!lines));
		  pr " lines\n";
		  pr "parse  "; prTime(!parse);
		  if !Control.Debug.debugging then
		    (pr "dbginstrum "; prTime(!debuginstrum))
		  else ();
		  pr "translate  "; prTime(!translate);
		  pr "codeopt    "; prTime(!codeopt);
		  pr "convert    "; prTime(!convert);
		  pr "cpsopt     "; prTime(!cpsopt);
		  pr "closure    "; prTime(!closure);
		  pr "globalfix  "; prTime(!globalfix);
		  pr "spill      "; prTime(!spill);
		  pr "codegen    "; prTime(!codegen);
		  pr "freemap    "; prTime(!freemap);
		  pr "execution  "; prTime(!execution);
		  pr "GC time    "; prTime garbagetime;
		  pr "total(usr) "; prTime total;
		  pr "total(sys) "; prTime system;
		  pr "collections: "; pr(imakestring(!minorcollections));
		  pr " minor, "; pr(imakestring(!majorcollections));
		  pr " major\n"; pr(imakestring(!collected));
		  pr " collected from "; pr(imakestring(!collectedfrom));
		  pr " possible (";
		  case (!collectedfrom)
		   of 0 => ()
		    | _ => pr(imakestring(InLine.div(InLine.*(!collected,100),
			 		    !collectedfrom)));
		  pr "%)\n";
		  pr "code bytes: "; pr(imakestring(!codesize)); pr "\n";
		  ()
	      end
	abstraction Timer : TIMER = Timer (* to hide timer datatype *)
      end (* structure Stats *)

    structure Timer : TIMER = Stats.Timer
    structure Signals : SIGNALS = Signals

  abstraction CC : sig type 'a control_cont end =
    struct type 'a control_cont = 'a cont end

    structure Unsafe : UNSAFE =
      struct
        structure Assembly : ASSEMBLY = Assembly
	structure CInterface : CINTERFACE = CInterface
	structure SysIO : SYSIO = SysIO
	structure CleanUp : CLEANUP = CleanUp
	structure Weak : WEAK = Weak
        type object = Assembly.object
        val boxed = InLine.cast()
        val unboxed = InLine.cast()
        val ordof = InLine.cast()
        val slength = InLine.cast()
        val store = InLine.cast()
        val bstore = InLine.cast()
        val subscript = InLine.cast()
        val update = InLine.cast()
	val delay = InLine.cast()
	val force = InLine.cast()
        val getvar = InLine.cast()
	val setvar = InLine.cast()
	type 'a control_cont = 'a CC.control_cont
	val capture = InLine.cast()
	val escape = InLine.cast()
	fun boot (x: string) : 'a -> 'b = 
	    InLine.cast(InLine.+(InLine.cast x, 4),0)
	val cast = InLine.cast
	val blast_write = cast PreIO.blast_write
	val blast_read = cast PreIO.blast_read
	val create_s = Assembly.A.create_s
	val create_b = Assembly.A.create_b
	val store_s : string * int * int -> unit = InLine.cast ByteArray.update
        val lookup_r = lookup_r
        val lookup = fn i => case lookup_r of ref f => f i

        val toplevelcont = toplevelcont

        val use_f = PreIO.use_f
        val use_s = cast PreIO.use_s
        val forcer_p = cast Core.forcer_p

	exception Top_level_callcc

        val do_it = InLine.callcc(fn c0 => let
	      val (f,arg,id) = InLine.callcc(fn c =>
				InLine.throw c0 (InLine.throw c))
	      val v = (f arg)
		    handle e => let
		      val ref(op :: ((k,h,_),r)) = cast compreturn
		      in
			InLine.:=(compreturn, cast r); h e
		      end
	      val ref(op ::((k,h,id'),r)) = cast compreturn
	      in
		InLine.:=(compreturn, cast r);
	        if InLine.reql(id,id') then k v else h(Top_level_callcc)
	      end)

        fun isolate f arg = InLine.callcc(fn c => 
		raise (InLine.callcc(fn c' => let
		  val id = ref()
		  in
		    InLine.:=(cast compreturn,
		      op ::( (InLine.throw c, InLine.throw c', id),
			cast(InLine.! compreturn)));
		    do_it (f, arg, id)
		  end)))

	val pstruct = cast(ref {core = (), initial=(), math=()})
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
	structure AA : sig
	  datatype datalist = DATANIL | DATACONS of (string * string * datalist)
	  val datalist : datalist
	end = Assembly
	open AA
      end (* Unsafe *)

  structure Directory : DIRECTORY =
    struct
      local open CInterface SysIO List String
      in

      fun isDir path =
	    (case ftype(PATH path) of F_DIR => true | _ => false)
	      handle (SystemCall _) => false

      exception NotDirectory

      fun listDir path = let
	    val fd = openf (path, O_READ)
	    fun f l = (case (getdirent fd) of [] => l | l' => f(l' @ l))
	    val dirlist = rev (f [])
	    in
	      closef fd; dirlist
	    end
	      handle (SystemCall _) => raise NotDirectory

      fun cd s = (chdir s) handle (SysError _) => raise NotDirectory

      fun getWD () = let
	    val root_id = getfid (PATH "/")
	    fun walk_up (path, curdir) = let
		  val curid = getfid(PATH curdir)
		  in
		    if (stringequal(curid, root_id))
		      then (case path of [] => "/" | _ => implode path)
		      else let
			val nextdir = curdir ^ "../"
(* NOTE: this code could be optimized by examining the directory entries incrementally *)
			fun scanDir nil = raise NotDirectory
			  | scanDir (f::r) = let
			      val fid = getfid(PATH(nextdir ^ f))
			      in
				if (stringequal(fid, curid)) then f else (scanDir r)
			      end
			val next = scanDir(listDir nextdir)
			in
			  walk_up ("/" :: next :: path, nextdir)
			end
		  end
	    in
	      walk_up ([], "./")
	    end
      end (* local *)
    end (* Directory *)

    open PreLim
    val version = "Standard ML of New Jersey, Version 0.71, 23 July 1991"
    local open CInterface in
    val system = wrap_sysfn "system" (fn x => (system(c_string x); ()))
    end

    val argv = CInterface.argv
    val environ = CInterface.environ

  end (* structure System *)

in
structure System : SYSTEM = System
structure List : LIST = List
structure IO : IO = IO
structure Bool : BOOL = Bool
structure String = String
structure ByteArray = ByteArray
end  (* local *)

open PrimTypes

(* The following structures must be without signatures so that inlining 
   can take place *)

structure General =
  struct
    infix 3 o
    infix before
    exception Bind = Core.Bind
    exception Match = Core.Match
    exception Interrupt

    val callcc : ('a cont -> 'a) -> 'a = InLine.callcc
    val throw : 'a cont -> 'a -> 'b = InLine.throw

    fun f o g = fn x => f(g x)
    fun a before b = a
    (*** datatype 'a option = NONE | SOME of 'a ***) (* moved to Bool *)
    type 'a cont = 'a cont
    type exn = exn
    type unit = unit
    infix 4 = <>
    val op = : ''a * ''a -> bool  = InLine.=
    val op <> : ''a * ''a -> bool = InLine.<>
  end (* structure General *)

structure Bits =
  struct
    val andb : int * int -> int = InLine.andb
    val orb : int * int -> int = InLine.orb
    val lshift : int * int -> int = InLine.lshift
    val rshift : int * int -> int = InLine.rshift
    val notb : int -> int = InLine.notb
    val xorb : int * int -> int = InLine.xorb
  end

structure Ref = 
  struct
    infix 3 :=
    val ! : 'a ref -> 'a = InLine.!
    val op := : 'a ref * 'a -> unit = InLine.:=
    fun inc r = r := (InLine.+ : int * int -> int) (!r,1)
    fun dec r = r := (InLine.- : int * int -> int) (!r,1)
  end


structure Vector = struct
  local
      open InLine
  in
    exception Size
    exception Subscript = Core.Subscript
    type 'a vector = 'a Core.Assembly.A.vector
    val length : 'a vector -> int = alength
    val vector_n : int * 'a list -> 'a vector = 
	fn (n,al) => if <=(n,0) then if <(n,0) then raise Size 
				     else Core.Assembly.vector0
		     else Core.Assembly.A.create_v(n,al)
    fun tabulate(n,f) =
        let fun tab j = if <(j,n) then f j :: tab (+(j,1)) else nil
        in vector_n(n,tab 0)
        end
    val vector : 'a list -> 'a vector = (fn al => vector_n(List.length al,al))
    val sub : 'a vector * int -> 'a = 
	fn (a,i) => if lessu(i, length a) then subscriptv(a, i)
		    else raise Subscript
  end
end

structure Array = struct
  local 
      open InLine 
  in
    type 'a array = 'a array
    exception Subscript = Vector.Subscript
    exception Size = Vector.Size
    val array : int * '1a -> '1a array =
	  fn arg as (n,v) =>
	     if <=(n,0) then if <(n,0) then raise Subscript
			     else Core.Assembly.array0
	     else Core.Assembly.A.array arg
    val sub : 'a array * int -> 'a = InLine.inlsubscript
    val length : 'a array -> int = alength
    fun arrayoflist nil = Core.Assembly.array0
      | arrayoflist (l as (e::r)) =
	  let val a = array(List.length l, e)
	      fun init ((e::r),n) = (update(a,n,e); init(r,+(n,1)))
	        | init (nil,_) = ()
	  in  init(r,1); a
          end
    val update : 'a array * int * 'a -> unit = InLine.inlupdate
    fun tabulate(i,f) = 
	let fun tab j = if <(j,i) then f j :: tab(+(j,1)) else nil
	in if <(i,0) then raise Size else arrayoflist (tab 0)
	end
  end (* local open ... *)
end (* structure Array *)

structure FpArray = struct
  local    
      open InLine
  in
    infix 9 sub
    exception FpSubscript = Core.FpSubscript
    exception Size
    type fparray = ByteArray.bytearray
    val length : fparray -> int = fn fpa => div(InLine.slength fpa, 8)
    val array : int * real -> fparray =
        fn (n,v) => if <=(n,0) then (if <(n,0) then raise Size
                                     else Core.Assembly.bytearray0)
                    else let val a = Core.Assembly.A.create_b (InLine.*(n,8))
                             fun init i = if ieql(i,n) then ()
                                          else (updatef(a,i,v); init (+(i,1)))
                         in init 0; a
                         end
    val op sub : fparray * int -> real = inlsubscriptf
    val op update : fparray * int * real -> unit = inlupdatef
  end
end

structure Integer =
  struct
    infix 7 * div mod quot rem
    infix 6 + -
    infix 4 > < >= <=
    exception Div = Core.Assembly.Div
    exception Mod = Div
    exception Overflow = Core.Assembly.Overflow
    exception Sum=Overflow and Diff=Overflow and Quot=Overflow and Abs=Overflow
          and Prod=Overflow and Neg=Overflow
    type int = int
    val ~ : int -> int = InLine.~
    val op * : int * int -> int = InLine.*
    val op + : int * int -> int = InLine.+
    val op - : int * int -> int = InLine.-
    val op > : int * int -> bool = InLine.>
    val op >= : int * int -> bool = InLine.>=
    val op < : int * int -> bool = InLine.<
    val op <= : int * int -> bool = InLine.<=
    fun op div(a:int,b:int):int =
	if b>=0
 	    then if a>=0 then InLine.div(a,b)
		         else InLine.div(a+1,b)-1
	    else if a>0  then InLine.div(a-1,b)-1
			 else InLine.div(a,b)
     fun op mod(a:int,b:int):int =
	if b>=0
 	    then if a>=0 then a-InLine.div(a,b)*b
		         else a-InLine.div(a+1,b)*b+b
	    else if a>0  then a-InLine.div(a-1,b)*b+b
                         else if InLine.ieql(a,~1073741824)
			         andalso InLine.ieql(b,~1) then 0
			        else a-InLine.div(a,b)*b
    val op quot : int * int -> int = InLine.div
    fun op rem(a:int,b:int):int = a-(a quot b)*b
    fun min(a,b) = if a<b then a else b
    fun max(a,b) = if a>b then a else b
    fun abs a = if a<0 then ~a else a
    fun makestring i =
	  if i<0 then (String.^("~", makestring(~i))
			handle Overflow => "~1073741824")
	  else if i<10 then InLine.cast(InLine.cast "0" + i)
	  else let val j = i quot 10
	       in  String.^(makestring j, makestring(i-j*10))
	       end
    local val pr = IO.outputc IO.std_out in
    fun print i = pr(makestring i)
    end
  end  (* structure Integer *)

structure Real =
  struct
   local 
    open Math String
    val negone = ~1.0
    val zero = 0.0
    val half = 0.5
    val one = 1.0
    val two = 2.0
    val five = 5.0
    val ten = 10.0
   in
    infix 7 * /
    infix 6 + -
    infix 4 > < >= <=
    type real = real
    exception Div=Integer.Div
    exception Overflow=Integer.Overflow
    exception Sum=Overflow and Diff=Overflow and Prod=Overflow
    and Exp=Overflow and Floor=Overflow
    val ~ : real -> real = fn x => InLine.fsub(0.0,x)
    val op + : real * real -> real = InLine.fadd
    val op - : real * real -> real = InLine.fsub
    val op * : real * real -> real = InLine.fmul
    val op / : real * real -> real = InLine.fdiv
    val op > : real * real -> bool = InLine.fgt
    val op < : real * real -> bool = InLine.flt
    val op >= : real * real -> bool = InLine.fge
    val op <= : real * real -> bool = InLine.fle
    val sin = sin and cos = cos and sqrt = sqrt and arctan = arctan
    and exp = exp and ln = ln
    exception Sqrt = Sqrt and Ln = Ln

    val floor = Core.Assembly.A.floor
    fun realfloor _ = let exception Unimplemented_realfloor in
		       raise Unimplemented_realfloor end
    fun truncate n = if n < 0.0 then InLine.~(floor(~n)) else floor n
    fun ceiling n = InLine.~(floor(~n))


    fun abs x = if x < zero then ~x else x
    local
      fun loop 0 = zero
	| loop n = let val x = two * loop(InLine.rshift(n,1))
		    in case InLine.andb(n,1) of 0 => x | 1 => x + one
		   end
    in
      fun real n = if InLine.<(n,0) then ~(loop(InLine.~ n)) else loop n
    end
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
	      fun mkdigits(f,0) = (nil,if f < five then 0 else 1)
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
	  end (* fun makestring *)
    local
      val pr = IO.outputc IO.std_out
    in
      fun print r = pr(makestring r)
    end
   end (* local *)
  end (* structure Real *)

  structure System =
  struct open System
         structure Unsafe =
	 struct open Unsafe
		val cast : 'a -> 'b = InLine.cast
		val boxed : 'a -> bool = InLine.boxed
		val ordof : string * int -> int = InLine.ordof
		val slength : string -> int = InLine.slength
		val store : string * int * int -> unit = InLine.store
		val bstore : string * int * int -> unit = InLine.store
		val subscript : 'a array * int -> 'a = InLine.subscript
		val update : 'a array * int * 'a -> unit = InLine.update
		val delay : int * 'a -> 'a = InLine.delay
		val force : 'a -> 'a = InLine.force
		abstype 'a delay = D of 'a with end
		val delay' : 'a -> 'a delay = InLine.delay
		val force' : 'a delay -> 'a = InLine.force
		val capture : ('a control_cont -> 'a) -> 'a = InLine.capture
		val escape : 'a control_cont -> 'a -> 'b = InLine.throw
		val getvar : unit -> 'a = InLine.getvar
		val setvar : 'a -> unit = InLine.setvar
	 end
	 val argv = Unsafe.CInterface.argv
	 val environ = Unsafe.CInterface.environ
  end
	
  open Ref String IO Bool List Integer Real General

(* Install some default signal handlers *)
  local
    open System.Signals
    fun quit s _ = let val msg = (s ^ " (no coredump)\n\000")
	  in
	  (* use write, since IO.output will block if we've lost the ptty *)
	    System.Unsafe.SysIO.write(2, System.Unsafe.cast msg, size msg)
	      handle _ => ();
	    System.Unsafe.CleanUp.shutdown()
	  end
  in
  val _ = setHandler(SIGHUP, SOME(quit "\nHangup"))
  val _ = setHandler(SIGINT, SOME(fn _ => !System.Unsafe.toplevelcont))
  val _ = setHandler(SIGQUIT, SOME(quit "\nQuit"))
  val _ = setHandler(SIGTERM, SOME(quit "\nSoftware termination"))
  end (* local *)

  val _ = IO.outputc IO.std_out "Initial done\n"

end (* structure Initial *)
