structure HppaAsmIO = struct
  fun globl name  = "\t.EXPORT\t" ^ name ^ ",ENTRY\n"
  fun enter name = "\t.PROC\n\t.CALLINFO\n" ^ name ^ "\n"
  fun endf name = "\t.PROCEND\n"
  fun label lab = "\n" ^ Label.nameOf lab ^ "\n"
  fun ascii string = "\t.STRING\t\"" ^ string ^ "\"\n"
  fun dotData () = "\n\t.DATA\n"
  fun dotText() = "\n\t.CODE\n"
  fun dotWord n = "\t.WORD " ^ Int.toString(n) ^ "\n"
end