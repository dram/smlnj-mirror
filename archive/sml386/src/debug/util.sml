signature DEBUGUTIL =
sig
  val smash: ('a -> 'b list) -> 'a list -> 'b list
  val pairlist: 'a list -> 'b list -> ('a * 'b) list
  val unpairlist: ('a * 'b) list -> (('a list) * ('b list))
  val tln: ('a list*int) -> 'a list
  val filter: ('a ->bool) -> 'a list -> 'a list
  val listofarray: 'a array -> 'a list  
  exception Index
  val index: ('a -> bool) -> 'a list -> int

  val infinity: int

  exception DebugError
  val debugPanic: string -> 'a

  val patvars: (Basics.var -> 'a) -> Absyn.pat -> 'a list
  val vblextract: (Basics.var -> 'a) ->	Absyn.vb list -> 'a list

  val forcegc: unit -> unit

  val debugdebug: bool ref
  val sizing:bool ref

  val sizereport: string -> unit
end

structure DbgUtil: DEBUGUTIL =
struct
  open Access Basics Absyn ErrorMsg

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
  exception Index
  fun index p = 
    let fun f _ nil = raise Index
	  | f c (h::t) = if p h then c else f (c+1) t
    in f 0
    end
  val infinity = 1000000000 (* hides one in Basics *)
  
  fun patvars f p =
    let fun patv (VARpat(v as VALvar{access = LVAR _,...})) = [f v]
	  | patv (VARpat(VALvar{access = INLINE _,...})) = []
	  | patv (VARpat _) = impossible "non-LVAR in DbgUtil.patvars"
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

(*  local 
    open System.Unsafe
    fun c_function s = 
        let exception C_function_not_found of string
            fun f (Assembly.FUNC(p,t,rest)) =
		    if s=t then p
		    else f rest
	      | f (Assembly.FUNCNIL) = raise (C_function_not_found s)
            val cfun = f Assembly.external
        in fn x => Assembly.A.callc(cfun,x)
        end

    val gc = c_function "gc"
  in 
    fun forcegc () = (gc 0; ())
  end
*)

  fun forcegc () = System.Unsafe.CInterface.gc 0
  val debugdebug = ref false
  val sizing = ref false

  fun sizereport label =
    if (!sizing) then (print ("Size report: " ^ label ^ "\n"); 
     		       exportML ("/dev/null"); ())
    else ()
		       
end









