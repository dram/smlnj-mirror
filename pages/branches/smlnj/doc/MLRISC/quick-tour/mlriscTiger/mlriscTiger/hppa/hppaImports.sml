structure HppaImports = struct
  datatype import_kind = MILLICODE | DATA | CODE | ENTRY

  exception Imports
  val imports:(import_kind * Label.label) Intmap.intmap = Intmap.new(16, Imports)

  val divu = Label.newLabel "$$divU"
  val divo = Label.newLabel "$$divI"
  val mulo = Label.newLabel "$$muloI"
  val mulu = Label.newLabel "$$mulU"

  (* all labels encountered are assumed to be imported code labels
   * until a definition is found.
   *)
  fun add(arg as (kind, lab)) = let
    val key = Label.id lab
  in
   (case Intmap.map imports key
    of (ENTRY, _) => ()
     | _ => Intmap.add imports (key, arg)
    (*esac*)) 
       handle _ => Intmap.add imports (key, arg)
  end  

  fun output () = let
    val symbols = map #2 (Intmap.intMapToList imports)
    fun importDecl(MILLICODE, lab) = 
        "\t.IMPORT\t" ^ Label.nameOf lab ^ ",MILLICODE\n"
      | importDecl(DATA, lab) =
        "\t.IMPORT\t" ^ Label.nameOf lab ^ ",DATA\n"
      | importDecl(CODE, lab) =
        "\t.IMPORT\t" ^ Label.nameOf lab ^ ",CODE\n"
      | importDecl(ENTRY, lab) =
        "\t.EXPORT\t" ^ Label.nameOf lab ^ ",ENTRY\n"
  in
    Intmap.clear imports;
    String.concat(map importDecl symbols)
  end
end
