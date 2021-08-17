(* tests/json/t0.sml *)

fun f (s: string) =
    let val SOME(w, _) = Word.scan StringCvt.HEX Substring.getc (Substring.full s)
     in UTF8.encode w
    end
