structure Alpha32AsmIO = struct
  fun globl name = "\t.globl\t" ^ name ^ "\n"
  fun enter name = "\t.ent\t" ^ name ^ "\n" ^ name ^ ":\n"
  fun endf name = "\t.end\t" ^ name  ^ "\n"
  fun label lab = Label.nameOf lab ^ ":\n"
  fun ascii string = "\t.ascii\t\"" ^ string ^ "\"\n"
  fun dotData () = "\n\t.data\n"
  fun dotText() = "\n\t.text\n"
  fun dotWord n = "\t.long " ^ Int.toString(n) ^ "\n"
end



