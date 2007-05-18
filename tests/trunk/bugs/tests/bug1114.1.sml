(* bug1114.1.sml *)

structure Type = 
struct
  datatype ty = Tyc of string
end;

structure P =
struct

fun pp_type ppstrm (Type.Tyc tyname) = 
   (PrettyPrint.openBox ppstrm (PrettyPrint.Abs 0);
    PrettyPrint.string ppstrm tyname;
    PrettyPrint.closeBox ppstrm)

end;

val _ = CompilerPPTable.install_pp ["Type","ty"] P.pp_type;

val test_type = Type.Tyc "bool";
