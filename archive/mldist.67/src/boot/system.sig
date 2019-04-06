(* Copyright 1989 by AT&T Bell Laboratories *)
signature RUNTIMECONTROL =
  sig
    val collected : int ref
    val collectedfrom : int ref
    val gcmessages : int ref
    val majorcollections : int ref
    val minorcollections : int ref
    val ratio : int ref
    val softmax : int ref
  end

signature MCCONTROL =
  sig
    val printArgs : bool ref
    val printRet : bool ref
    val bindContainsVar : bool ref
    val bindExhaustive : bool ref
    val matchExhaustive : bool ref
    val matchRedundant : bool ref
    val expandResult : bool ref
  end

signature CGCONTROL =
  sig
    structure M68 : sig val trapv : bool ref end
    val tailrecur : bool ref
    val recordopt : bool ref
    val tail : bool ref
    val profile : bool ref
    val closureprint : bool ref
    val closureStrategy : int ref
    val cpsopt : bool ref
    val rounds : int ref
    val path : bool ref
    val betacontract : bool ref
    val eta : bool ref
    val selectopt : bool ref
    val dropargs : bool ref
    val deadvars : bool ref
    val flattenargs : bool ref
    val switchopt : bool ref
    val handlerfold : bool ref
    val branchfold : bool ref
    val arithopt : bool ref
    val betaexpand : bool ref
    val hoistup : bool ref
    val hoistdown : bool ref
    val maxregs : int ref
    val recordcopy : bool ref
    val tagopt : bool ref
    val recordpath : bool ref
    val machdep : bool ref
    val misc1 : bool ref
    val misc2 : bool ref
    val misc3 : int ref
    val misc4 : int ref
    val hoist : bool ref
    val argrep : bool ref
    val reduce : bool ref
    val bodysize : int ref
    val reducemore : int ref
    val alphac : bool ref
    val comment : bool ref
    val knowngen : int ref
    val stdgen : int ref
    val knowncl : int ref
    val foldconst : bool ref
    val etasplit : bool ref
    val printit : bool ref
    val printsize : bool ref
    val scheduling : bool ref
    val cse : bool ref
    val optafterclosure : bool ref
    val calleesaves : int ref
    val extraflatten : bool ref
    val uncurry : bool ref
    val ifidiom : bool ref
    val comparefold : bool ref
    val csehoist : bool ref
    val rangeopt : bool ref
    val floatargs : int ref
    val floatvars : int ref
  end

signature PRINTCONTROL =
  sig
    val printDepth : int ref
    val printLength : int ref
    val stringDepth : int ref
    val signatures : bool ref
  end

signature DEBUG =
  sig
    val debugging : bool ref
    val getDebugf : (int -> 'a) ref
    val interface : (int -> ('a -> 'b)) ref
  end

signature PROFILE =
  sig
    structure IO : sig type outstream end
    val profiling : bool ref	      (* controls profiling mode of compilation *)
    val profileOn : unit -> unit      (* turn interrupt timer on *)
    val profileOff : unit -> unit     (* turn interrupt timer off *)
    val clear : unit -> unit          (* clear profiling database *)
    val reset : unit -> unit          (* reset profiling counts to zero *)
    val report : IO.outstream -> unit (* print profiling report to stream *)
  end

signature PROFILEINTERNALS = 
  sig 
    val add : 'a -> unit
    val setToplevel : unit -> unit
    val setOther : unit -> unit
    structure P : PROFILE
  end

signature CONTROL =
  sig
    structure Runtime : RUNTIMECONTROL
    structure MC : MCCONTROL
    structure CG : CGCONTROL
    structure Print : PRINTCONTROL
    structure ProfileInternals : PROFILEINTERNALS
    structure Profile : PROFILE
    structure Debug : DEBUG
    val prLambda : (unit -> unit) ref
    val debugging : bool ref
    val primaryPrompt : string ref
    val secondaryPrompt : string ref
    val internals : bool ref
    val weakUnderscore : bool ref
    val interp : bool ref
    val debugLook : bool ref
    val debugCollect : bool ref
    val debugBind : bool ref
    val saveLambda : bool ref
    val saveLvarNames : bool ref
    val timings : bool ref
    val reopen : bool ref
    val markabsyn : bool ref
    val indexing : bool ref
  end

signature TIMER =
  sig
    datatype time = TIME of {sec : int, usec : int}
    type timer
    val start_timer : unit -> timer
    val check_timer : timer -> time
    val check_timer_sys: timer -> time
    val check_timer_gc: timer -> time
    val makestring : time -> string
    val add_time : time * time -> time
    val sub_time : time * time -> time
    val earlier : time * time -> bool
  end

signature TAGS =
  sig
    type tag
    val width_tags : int (* number of bits to hold a tag *)
  (* tag values *)
    val tag_record : tag
    val tag_array : tag
    val tag_string : tag
    val tag_real : tag
    val tag_embedded_string : tag
    val tag_embedded_real : tag
    val tag_bytearray : tag
    val tag_backptr : tag
  (* build a descriptor from a tag and length *)
    val make_desc : (int * tag) -> int
  (* fixed descriptors *)
    val desc_real : int
    val desc_embedded_real : int
  (* special descriptors *)
    val desc_evaled_susp : int
    val desc_unevaled_susp : int
    val desc_weak : int
    val desc_nulled_weak : int
  end

signature STATS =
  sig
    structure Timer : TIMER
    val lines : int ref
    val parse : Timer.time ref
    val translate : Timer.time ref
    val codeopt : Timer.time ref
    val convert : Timer.time ref
    val hoist : Timer.time ref
    val cpsopt : Timer.time ref
    val closure : Timer.time ref
    val globalfix : Timer.time ref
    val spill : Timer.time ref
    val codegen : Timer.time ref
    val freemap : Timer.time ref
    val execution : Timer.time ref
    val update : Timer.time ref * Timer.time -> unit
    val summary : unit -> unit
  end

signature CINTERFACE =
  sig
    exception CFunNotFound of string
    exception SysError of (int * string)
    exception SystemCall of string

    type time

    val c_function : string -> ('a -> 'b)
    val c_string : string -> string (* insure that a string is safe to pass to C *)
    val wrap_sysfn : string -> ('a -> 'b) -> 'a -> 'b

  (* C functions *)
    val argv	    : unit -> string list
    val environ	    : unit -> string list
    val gethostname : unit -> string
    val exec	    : (string * string list * string list) -> (int * int)
    val system      : string -> int
    val export      : int -> bool
    val blas	    : (int * 'a) -> 'a
    val salb	    : string -> 'a
    val gettime     : unit -> {usr : time, sys : time, gc : time}
    val setitimer   : (int * time * time) -> unit
    val flush_cache : string -> unit
    val gc          : int -> unit
    val syscall	    : (int * string list) -> int
  (* System calls *)
    val exit	    : unit -> 'a
    val getpid	    : unit -> int
    val getuid	    : unit -> int
    val getgid	    : unit -> int
    val chdir	    : string -> unit
  end (* CINTERFACE *)

signature SYSIO =
  sig
    type bytearray
    type time

    type fd
    eqtype fileid
    datatype fname	= DESC of fd | PATH of string
    datatype mode	= O_READ | O_WRITE | O_APPEND
    datatype whence	= L_SET | L_INCR | L_XTND
    datatype access	= A_READ | A_WRITE | A_EXEC
    datatype file_type	= F_REGULAR | F_DIR | F_SYMLINK | F_SOCK | F_CHR | F_BLK

    val dtablesize	: int
    val openf		: (string * mode) -> fd
    val closef		: fd -> unit
    val unlink		: string -> unit
    val pipe		: unit -> (fd * fd)
    val connect_unix	: string -> fd
    val connect_inet	: (string * string) -> fd
    val link		: (string * string) -> unit
    val symlink		: (string * string) -> unit
    val mkdir		: (string * int) -> unit
    val dup		: fd -> fd

    val read		: (fd * bytearray * int) -> int
    val readi		: (fd * bytearray * int * int) -> int
    val write		: (fd * bytearray * int) -> unit
    val writei		: (fd * bytearray * int * int) -> unit
    val writev		: (fd * (bytearray * int) list) -> unit
    val send_obd	: (fd * bytearray * int) -> unit
    val getdirent	: fd -> string list
    val readlink	: string -> string
    val truncate	: (fname * int) -> unit
    val lseek		: (fd * int * whence) -> int

    val getmod		: fname -> int
    val chmod		: (fname * int) -> unit
    val umask		: int -> int

    val access		: (string * access list) -> bool
    val isatty		: fd -> bool
    val fionread	: fd -> int
    val getfid		: fname -> fileid
    val ftype		: fname -> file_type
    val getownid	: fname -> (int * int)
    val fsize		: fname -> int
    val atime		: fname -> time
    val ctime		: fname -> time
    val mtime		: fname -> time
    val select		: (fd list * fd list * fd list * time option)
			    -> (fd list * fd list * fd list)
  end (* SYSIO *)

signature CLEANUP =
  sig
    datatype clean_mode
      = CleanForExportML | CleanForExportFn | CleanForQuit | CleanForInit
    val addCleaner : (string * (clean_mode -> unit)) -> bool
    val removeCleaner : string -> unit
    val cleanup : clean_mode -> unit
    val shutdown : unit -> 'a
  end (* CLEANUP *)

signature WEAK = 
  sig
    type 'a weak
    val weak : 'a -> 'a weak
    val strong : 'a weak -> 'a option
  end (* WEAK *)

signature UNSAFE =
  sig
    type object
    structure Assembly : ASSEMBLY
    structure CInterface : CINTERFACE
    structure SysIO : SYSIO
    structure CleanUp : CLEANUP
    structure Weak : WEAK
    val boxed : 'a -> bool
    val ordof : 'a * int -> int
    val slength : 'a -> int
    val store : string * int * int -> unit
    val bstore : Assembly.A.bytearray * int * int -> unit
    val subscript : 'a array * int -> 'a
    val update : 'a array * int * 'a -> unit
    val delay : int * 'a -> 'a
    val force : 'a -> 'a
    val boot : string -> ('a -> 'b)
    val cast : 'a -> 'b
    val blast_write : 'outstream * 'a -> unit
    val blast_read :  'instream -> 'a
    val create_s : int -> string
    val create_b : int -> Assembly.A.bytearray
    val store_s : string * int * int -> unit
    val lookup_r : (int -> object) ref
    val lookup : int -> object
    val isolate : ('a -> 'b) -> 'a -> 'b
    val toplevelcont : unit cont ref
    val pstruct : {core: object, initial: object, math: object} ref
    exception Boxity
    val tuple : object -> object array
    val string : object -> string
    val real : object -> real
    val int : object -> int
    val use_f : (string -> unit) ref
    val use_s : ('instream -> unit) ref
    val forcer_p : ('a -> 'a) ref
    datatype datalist = DATANIL | DATACONS of (string * string * datalist)
    val datalist : datalist
  end

signature SIGNALS =
  sig
    datatype signal
      = SIGHUP | SIGINT | SIGQUIT | SIGALRM | SIGTERM | SIGURG
      | SIGCHLD | SIGIO | SIGWINCH | SIGUSR1 | SIGUSR2
      | SIGTSTP | SIGCONT (* not yet supported *)
      | SIGGC
    val setHandler : (signal * ((int * unit cont) -> unit cont) option) -> unit
    val inqHandler : signal -> ((int * unit cont) -> unit cont) option
    val maskSignals : bool -> unit
    val pause : unit -> unit
	(* sleep until the next signal *)
  end

signature DIRECTORY =
  sig
    val isDir : string -> bool
        (* return true, if path is a directory *)
    exception NotDirectory
    val listDir : string -> string list
        (* return a list of the files in the specified directory, raises NotDirectory *)
    val cd : string -> unit
        (* change directory, raises NotDirectory*)
    val getWD : unit -> string
        (* return the current working directory *)
  end (* DIRECTORY *)

signature SYSTEM =
  sig
    structure ByteArray : BYTEARRAY
    structure Control : CONTROL
    structure Tags : TAGS
    structure Timer : TIMER
    structure Stats : STATS
    structure Unsafe : UNSAFE
    structure Signals : SIGNALS
    structure Directory : DIRECTORY
    val exn_name : exn -> string
    val version : string
    val interactive : bool ref
    val system : string -> unit (* execute a shell command *)
    val argv : unit -> string list
    val environ : unit -> string list
  end
