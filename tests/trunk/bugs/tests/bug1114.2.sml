(* bug1114.2.sml *)

functor TYPE () =
struct
  datatype ty = Tyc of string
end;

structure Type = TYPE();

structure P =
struct

fun pp_type ppstrm (Type.Tyc tyname) = 
   (PrettyPrint.openBox ppstrm (PrettyPrint.Abs 0);
    PrettyPrint.string ppstrm tyname;
    PrettyPrint.closeBox ppstrm)

end;

val _ = CompilerPPTable.install_pp ["Type","ty"] P.pp_type;

val test_type = Type.Tyc "bool";
