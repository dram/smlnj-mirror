structure Alpha32Frame = struct
  exception Alpha32Frame

  fun error msg = (print("Alpha32Frame." ^ msg ^ "\n"); raise Alpha32Frame)

  structure T = Tree

  datatype access = 
      InFrame of int			(* In local area *)
    | InReg of Temp.temp		(* in register *)
    | InArg of int			(* in incoming arg area *)

  datatype frame =
    FRAME of 
      {
       name : Temp.label,		(* name of frame *)
       formals: access list,
       actuals: access list,
       locals: int ref			(* number of locals allocated *)
      }
  datatype frag = PROC of  {body: Tree.stm, frame: frame}
                | STRING of Temp.label * string

  val wordSize = 4

  val PV = 27				(* procedure value *)
  val AT = 28				(* assembler temp *)
  val GP = 29				(* global pointer *)
  val SP = 30				(* stack pointer *)
  val RA = SOME 26			(* return address register *)
  val ZERO = 31				(* constant zero *)
  val RV = 0				(* return value *)
  val FP = 15				(* frame pointer *)

  fun upto(n, m) = if n > m then [] else (n)::upto(n+1, m)
  infix upto

  val argRegs = [16,17,18,19,20,21]
  val calleesaved = [9,10,11,12,13,14]
  val callersaved = [0,1,2,3,4,5,6,7,8, 
                     16,17,18,19,20,21,22,23,24,25,26,27]

  val availR = SortedList.uniq((0 upto 14) @ (16 upto 27))
  val dedicated = SortedList.remove(availR, SortedList.uniq(0 upto 31))

  val maxOutgoingArgs = 8
  val alignment = 16
  val hiToLowStackGrowth = true
  val argOffset = 0
  val fixedFrameArea = 0

  fun exp (InReg t) _ = T.TEMP t
    | exp (InFrame offset) fp = 
        T.MEM(T.BINOP(T.PLUS, fp, T.CONST (~(offset+1) * 4)))
    | exp (InArg _) _ = error "exp: InArg"

  fun newFrame{name: Temp.label, formals: bool list} = let
    fun actuals nArgs = let
      val nMemArgs = nArgs - length argRegs
    in
      if nMemArgs <= 0 then 
	map InReg (List.take(argRegs, nArgs))
      else 
	map InReg argRegs @ map InArg (0 upto (nMemArgs-1))
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
  end

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

  (* call external (C) function *)
  fun externalCall(cfun, args) = 
    T.ESEQ(T.MOVE(T.TEMP PV, T.NAME(Temp.namedlabel cfun)),
	   T.CALL(T.TEMP PV, args))

  (* Note: For the MLRisc backend we do not insert the prologue and 
   * epilogue functions using this mechanism. We need to be able to refer
   * to the frame size from within MLRisc instructions.
   *)
  fun procEntryExit1(_, body) = body

end