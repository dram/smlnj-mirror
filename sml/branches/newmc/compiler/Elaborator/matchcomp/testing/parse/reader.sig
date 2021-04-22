(* reader.sig *)

signature READER =
sig

  type ('a,'b) reader = 'b -> ('a * 'b) option

  val return : 'a -> ('a,'b) reader
  val fail : ('a,'b) reader
  val chain: ('a,'b) reader -> ('a -> ('c,'b) reader) -> ('c,'b) reader

  val choice: ('a,'b) reader * ('a,'b) reader -> ('a,'b) reader
  val star : ('a,'b) reader -> ('a list,'b) reader 
  val starPlus : ('a,'b) reader -> ('a list,'b) reader
  val invert: 'a -> ('c,'b) reader -> ('a,'b) reader

end (* signature READER *)
