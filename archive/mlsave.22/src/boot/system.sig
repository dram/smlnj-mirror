signature RUNTIMECONTROL =
  sig
    val collected : int ref
    val collectedfrom : int ref
    val gcmessages : int ref
    val majorcollections : int ref
    val minorcollections : int ref
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
    val path : bool ref
    val hoist : bool ref
    val reduce : bool ref
  end

signature PRINTCONTROL =
  sig
    val printDepth : int ref
    val stringDepth : int ref
    val signatures : bool ref
  end

signature CONTROL =
  sig
    structure Runtime : RUNTIMECONTROL
    structure MC : MCCONTROL
    structure CG : CGCONTROL
    structure Print : PRINTCONTROL
    val debugging : bool ref
    val primaryPrompt : string ref
    val secondaryPrompt : string ref
    val internals : bool ref
    val debugLook : bool ref
    val debugCollect : bool ref
    val debugBind : bool ref
    val saveLambda : bool ref
    val saveLvarNames : bool ref
    val timings : bool ref
    val reopen : bool ref
  end

signature TIMER =
  sig  
    datatype time = TIME of {sec : int, usec : int}
    type timer
    val start_timer : unit -> timer
    val check_timer : timer -> time
    val check_timer_gc: timer -> time
    val makestring : time -> string
    val add_time : time * time -> time
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

signature STATS =
  sig
    structure Timer : TIMER
    val lines : int ref
    val parse : Timer.time ref
    val translate : Timer.time ref
    val codeopt : Timer.time ref
    val convert : Timer.time ref
    val cpsopt : Timer.time ref
    val closure : Timer.time ref
    val globalfix : Timer.time ref
    val spill : Timer.time ref
    val codegen : Timer.time ref
    val execution : Timer.time ref
    val update : Timer.time ref * Timer.time -> unit
    val summary : unit -> unit
  end

signature SYSTEM =
  sig
    type object
    structure ByteArray : BYTEARRAY
    structure Assembly : ASSEMBLY	(* temporary *)
    structure Control : CONTROL
    structure Tags : TAGS
    structure Timer : TIMER
    structure Stats : STATS
    val exn_name : exn -> string
    val prLambda : (unit -> unit) ref
    val version : string
    val interactive : bool ref
    val cleanup : unit -> unit
    val boot : string -> ('a -> 'b)
    val cast : 'a -> 'b
    val create_s : int -> string
    val store_s : string * int * int -> unit
    val pstruct : {core: object, initial: object, math: object} ref
    exception Boxity
    val tuple : object -> object array
    val string : object -> string
    val real : object -> real
    val int : object -> int
    datatype value = RECORDval of int * object
		   | ARRAYval of object array
		   | STRINGval of string
		   | BYTEARRAYval of ByteArray.bytearray
		   | CLOSUREval of object * int
		   | INTval of int
		   | OTHERval of object
    val getvalue : 'a -> value
    val followpath : (object * int list) -> object
     datatype datalist = DATANIL | DATACONS of (string * string * datalist)
     val datalist : datalist
  end
