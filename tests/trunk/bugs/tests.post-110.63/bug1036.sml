(* bug1036.sml *)

val a = CharArray.fromList(explode "abcdefgh");
val v = CharArraySlice.vector(CharArraySlice.slice(a, 0, SOME 4));

(* note the garbage bytes after the "abcd" prefix *)

size v;

(* asked for 4 bytes, got 8 *)

val a = Word8Array.array(8, 0w65);
val v = Word8ArraySlice.vector(Word8ArraySlice.slice(a, 0, SOME 4));
Word8Vector.length v;
(Unsafe.cast v):string;
