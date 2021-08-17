(* tests/json/t0a.sml *)

val x =
  let val SOME(w, _) = Word.scan StringCvt.HEX Substring.getc (Substring.full "12")
   in UTF8.encode w
  end;
