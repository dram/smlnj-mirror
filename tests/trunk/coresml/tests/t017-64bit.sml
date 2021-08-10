(* t017.sml *)
(* for 64-bit architecture: Int.int is 63 bits *)

val minInt1 = ~4611686018427387904;      (* ~2^62 *)
val maxInt1 =  4611686018427387903;      (* 2^62 - 1 *)

val minInt2  = ~0x4000000000000000;
val maxInt21 =  0x3fffffffffffffff;
val maxInt22 =  0x3FFFFFFFFFFFFFFF;

val test1 = if minInt1 = minInt2 then "OK" else "WRONG";
val test2 = if maxInt1 = maxInt21 andalso maxInt21 = maxInt22 then "OK" else "WRONG";

val maxWord1 = 0w9223372036854775807;   (* 2^63 - 1 *)
val maxWord2 = 0wx7fffffffffffffff;

val test3 = if maxWord1 = maxWord2 andalso maxWord1 = Word.fromInt ~1 then "OK" else "WRONG";

val maxWord8_1 = 0w255;
val maxWord8_2 = 0wxFF;

val test4 = if maxWord8_1 = maxWord8_2 andalso maxWord8_1 = Word.fromInt 255 then "OK" else "WRONG"
