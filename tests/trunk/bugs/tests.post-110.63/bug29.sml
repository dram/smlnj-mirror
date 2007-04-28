(* bug29.sml *)

structure Foo =
struct
  val x = Backend.Interact.useStream(TextIO.openString "val _ = Foo.x;")
end;
