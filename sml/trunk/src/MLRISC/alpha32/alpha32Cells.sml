(* alpha32Cells.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

structure Alpha32Cells : CELLS = struct
  structure S = SortedList

  exception Alpha32Cells

  val stackptrR		= 30
  val asmTmpR		= 28
  val fasmTmp		= 30
  val firstPseudoReg	= 128

  val counter = ref firstPseudoReg
  val regCnt = ref 0
  val fregCnt = ref 0
  fun bump (r as ref c) = (r := c+1; c)
  fun downto0 0 = [0]
    | downto0 n = n::downto0(n-1)
  val physicalRegs = downto0 31

  fun newReg ()   = (bump regCnt; bump counter)
  fun newFreg()   = (bump fregCnt; bump counter)
  fun newCCreg()  = (bump regCnt; bump counter)
  fun maxReg()    = !counter
  fun maxFreg()   = !counter
  fun numRegs()   = !regCnt
  fun numFregs()  = !fregCnt
  fun resetRegs() = let 
    val regmap = Intmap.new(64, Alpha32Cells)
    val enter = Intmap.add regmap
  in
    counter:=firstPseudoReg; 
    regCnt :=0; 
    fregCnt:=0; 
    app (fn r => enter(r,r)) physicalRegs;
    regmap
  end

  type cellset = int list * int list	(* (regs * fregs) *)
  fun cellset2string(regs, fregs) = let
    val gp = "gp=" :: map (fn r => (" $" ^ Int.toString r)) regs
    val fp = " fp=" :: map (fn f => (" $f" ^ Int.toString f)) fregs
  in String.concat(gp @ fp)
  end
  val empty = ([], [])
  fun addReg(r, (rc,fc)) = (S.enter(r,rc), fc)
  fun addFreg(f, (rc,fc)) = (rc, S.enter(f, fc))
  val addCCreg = addReg

  fun cellsetToRegs(regmap, (regs,fregs)) = let
    val lookup = Intmap.map regmap 
    fun trans r = lookup r handle _ => r
    fun ftrans r = let
      val r = lookup r handle _ => r
    in  if r < 32 then r + 32 else r 
    end
  in (map ftrans fregs @ map trans regs)
  end

end










(*
 * $Log: alpha32Cells.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:01  george
 * Version 110.5
 *
 *)
