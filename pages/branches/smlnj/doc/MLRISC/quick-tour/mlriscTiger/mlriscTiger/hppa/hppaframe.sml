structure HppaFrame = struct
  exception Alpha32Frame

  fun error msg = (print("Alpha32Frame." ^ msg ^ "\n"); raise Alpha32Frame)

  structure T = Tree

  datatype access = 
      InFrame of int			(* In local area *)
    | InReg of Temp.temp		(* in register *)
    | InArg of int			(* in incoming arg area *)

  datatype frame =
    FRAME of {
       name : Temp.label,		(* name of frame *)
       formals: access list,
       actuals: access list,
       locals: int ref			(* number of locals allocated *)
     }
  datatype frag = PROC of  {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val wordSize = 4

  val PV = 1				(* any scratch register will do *)
  val RA = SOME 2
  val FP = 18
  val RV = 28
  val SP = 30

  fun upto(n,m) = if n > m then [] else n::upto(n+1,m)
  infix upto

  val argRegs = [26, 25, 24, 23]
  val calleesaved = [3,4,5,6,7,8,9,10,11,12,13,14,15,16,17]
  val callersaved = [1, 2, 19, 20, 21, 22, 29, 31] @ argRegs

  val availR = SortedList.uniq (28::29::31::(1 upto 17) @ (19 upto 26))
  val dedicated = SortedList.remove(availR, SortedList.uniq(0 upto 31))

  val maxOutgoingArgs = 8
  val alignment = 64
  val hiToLowStackGrowth = false
  val argOffset = 9			(* slots *)
  val fixedFrameArea = 9

 (* It's hard to believe, but this is the stack layout recommended 
    by HPUX for C functions.  WOW!

   Variable arguments   (optional; any number may be allocated)

   SP-(4*(N+9))		arg word N
   	:		    :			
      SP-56		arg word 5     
      SP-52		arg word 4

   Fixed arguments	(must be allocated; may remain unused)

      SP-48		arg word 3
      SP-44		arg word 2		
      SP-40		arg word 1     
      SP-36		arg word 0

   Frame Marker		(must be allocated; may remain unused)

      SP-32		External Data Pointer (DP)
      SP-28		External sr4
      SP-24		External/stub RP (RP')
      SP-20		Current RP		
      SP-16		Static Link   
      SP-12		Clean up
      SP-8		Calling Stub RP (RP'')
      SP-4		Previous SP

   Top of Frame

      SP-0		Stack Pointer (points to next available address)

   *)
  fun exp(InReg t) _ = T.TEMP t
    | exp(InFrame offset) fp = 
        T.MEM(T.BINOP(T.PLUS, fp, T.CONST(offset * 4)))
    | exp(InArg _) _ = error "exp: InArg"

 (* newFrame: create a new frame. 
  *    Note: stack grows from low to high addresses
  *)
  fun newFrame{name: Temp.label, formals:bool list} = let
    fun actuals nArgs = let
      val nArgRegs = length argRegs
      val nMemArgs = nArgs - nArgRegs
    in
      if nMemArgs <= 0 then
	map InReg (List.take (argRegs, nArgs))
      else let
	  val frameMarkerSize = 9		(* slots *)
	  val start = frameMarkerSize + nArgRegs
	  val finish = frameMarkerSize + nArgs - 1
	in map InReg argRegs @  map InArg (start upto finish)
	end
    end

    fun formalsLoc([], localSz, fmls) = (localSz, rev fmls)
      | formalsLoc(escape::rest, size, fmls) =
        if escape then formalsLoc(rest, size+1, InFrame(size)::fmls)
	else formalsLoc(rest, size, InReg(Temp.newtemp())::fmls)

    val (locals, fmls) = formalsLoc(formals, 0, [])
    val nFormals = length formals
  in
    if nFormals > maxOutgoingArgs then error "newFrame" else ();
    FRAME{name=name,
	  formals=fmls,
	  actuals=actuals nFormals,
	  locals=ref locals
	 }
  end (* newFrame *)

  val currFrame = ref (newFrame{name=Temp.newlabel(), formals=[]})

  fun name(FRAME{name, ...}) = name
  fun formals(FRAME{formals, ...}) = formals
  fun actuals(FRAME{actuals, ...}) = actuals

  (* allocate a local in a frame. *)
  fun allocLocal(FRAME{locals, ...}) true = let
        val loc = !locals
      in InFrame(loc) before locals := loc + 1
      end
    | allocLocal _ false = InReg(Temp.newtemp())

  fun localAreaSz(FRAME{locals, ...}) = !locals

  fun externalCall(cfun, args) = T.CALL(T.NAME(Temp.namedlabel cfun), args)

  (* Note: For the MLRisc backend we do not insert the prologue and 
   * epilogue functions using this mechanism. We need to be able to refer
   * to the frame size from within MLRisc instructions.
   *)
  fun procEntryExit1(_, body) = body
end



