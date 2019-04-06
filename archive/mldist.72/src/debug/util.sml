(* DebugUtil.sml 

   Utility functions for debugger code.

*)

signature DEBUG_UTIL =
sig
  (* Basic list and array operators. *)
  val smash: ('a -> 'b list) -> 'a list -> 'b list
  val pairlist: 'a list -> 'b list -> ('a * 'b) list
  val unpairlist: ('a * 'b) list -> (('a list) * ('b list))
  val tln: ('a list*int) -> 'a list  (* last n elements of list *)
  val filter: ('a ->bool) -> 'a list -> 'a list
  val listofarray: 'a array -> 'a list  
  val copyarray: '1a array -> '1a array
  val resetarray: 'a array -> 'a array -> unit
  exception Index
  val index: ('a -> bool) -> 'a list -> int

  (* A very large signed integer. We implicitly assume that all real time
   values are less than this, which is rather dangerous... *)
  val infinity: int

  (* Diagnostic printing. *)
  val debugdebug: bool ref  (* True if diagnostic reports are to be printed. *)
  val dbgprint: string -> unit  (* Print diagnostic report. *)
  val sizereport: (string -> unit) ref  (* Perform and print size report. *)

  val debugEnv: Modules.env ref  
   (* Environment to use for printing types when we've nothing better to
      use (e.g., in diagnostic reports). This should *not* be a special
      debugger environment or diagnostic printing in the implementation of
      special envs will loop! *)

  (* General purpose error-handling. *)
  exception DebugError
  val debugPanic: string -> 'a
  val assert: (bool * string) -> unit

  (* Abstract syntax operators. *)
  val patvars: (Variables.var -> 'a) -> Absyn.pat -> 'a list
  val vblextract: (Variables.var -> 'a) ->	Absyn.vb list -> 'a list

  (* Encourage a minor garbage collection. *)
  val forcegc: unit -> unit

  (* Handy continuation stuff. *)
  val makeCont:unit -> 'a cont
  val emptyUnitCont:unit cont

  (* Misc. functions *)
  val isFn: Types.ty -> bool
  val printVal : (System.Unsafe.object * Types.ty) -> unit

  (* Possible results of executing a thunk of user code. *)
  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORTED 
    | INTERRUPTED
    | SUSPENDED
    | INTERPOLATION
end

structure DebugUtil: DEBUG_UTIL =
struct
  open Array List Access Variables Absyn ErrorMsg
  infix 9 sub

  fun smash f l = fold (fn (a,c) => f a @ c) l []
  fun pairlist (a::ar) (b::br) = (a,b)::(pairlist ar br)
    | pairlist _ nil = nil
    | pairlist nil _ = nil 

  fun unpairlist l = (map (fn (x,y) => x) l, map (fn (x,y) => y) l)

  fun tln (l,0) = l
    | tln (l,n) = tln (tl l,n-1)

  fun filter b l =
    let fun f (e::r) = if b e then e::(f r) else (f r)
	  | f nil = nil
    in f l
    end
  fun listofarray a =
    let fun f n = ((a sub n)::(f (n+1))) handle Subscript => nil
    in
      f 0
    end
  fun copyarray olda = 
    let val newa = array (Array.length olda, System.Unsafe.subscript(olda,0))
	fun d n = (System.Unsafe.update(newa,n,olda sub n); d (n+1))
    in (d 1) handle Subscript => newa
    end
  fun resetarray target source =
    let fun d n = (System.Unsafe.update(target,n, source sub n); d (n+1))
    in (d 0) handle Subscript => ()
    end
  exception Index
  fun index p = 
    let fun f _ nil = raise Index
	  | f c (h::t) = if p h then c else f (c+1) t
    in f 0
    end
  val infinity = 1000000000 (* hides one in Basics *)
  
  fun patvars f p =
    let fun patv (VARpat(v as VALvar{access = PATH _,...})) = [f v]
	  | patv (VARpat(VALvar{access = INLINE _,...})) = []  (* ??? *)
	  | patv (VARpat _) = impossible "non-PATH in DebugUtil.patvars"
	  | patv (RECORDpat{fields,...}) = smash (fn (_,p) => patv p) fields
	  | patv (APPpat(_,p)) = patv p
	  | patv (CONSTRAINTpat (p,_)) = patv p
	  | patv (LAYEREDpat(p,q)) = patv p @ patv q
	  | patv _ = []
    in patv p
    end

  fun vblextract f vbl = smash (fn (VB{pat,...}) => patvars f pat) vbl
  
  exception DebugError
  fun debugPanic t = ErrorMsg.impossible ("DebugError:" ^ t ^ "\n")
  fun assert (true,_) = ()
    | assert (false,s) = debugPanic ("Assertion failure " ^ s)

  fun forcegc () = System.Unsafe.CInterface.gc 0
 
  fun makeCont () = 
      callcc(fn a => (callcc (fn b => throw a b);
		      debugPanic "using empty cont"))
  val emptyUnitCont:unit cont = makeCont()

  val debugdebug = ref false
  fun dbgprint (s:string) = if !debugdebug then print s else ()

  val sizereport = ref (fn (s:string) => ())

  val debugEnv = ref(Env.empty: Modules.env)

  val isFn = BasicTypes.isArrowType 
        (* N.B. This doesn't appear to work quite right! *)

  fun printVal(v,t) = PrintVal.printVal (!debugEnv) (v,t,100)


  datatype 'a result =
      NORMAL of 'a
    | EXCEPTION of exn
    | ABORTED 
    | INTERRUPTED
    | SUSPENDED
    | INTERPOLATION
end










