(* Copyright 1989 by AT&T Bell Laboratories *)
(* namespace.sml *)

structure NameSpace: NAMESPACE =
struct

  open Symbol

  (* there are 6 namespaces, numbered 0 to 5 *)
  val varSpace = 0
  val tycSpace = 1
  val sigSpace = 2
  val strSpace = 3
  val fctSpace = 4
  val fixSpace = 5

  fun varIndex(id: symbol) = number(id)+varSpace
  fun conIndex(id: symbol) = number(id)+varSpace
  fun tycIndex(id: symbol) = number(id)+tycSpace
  fun sigIndex(id: symbol) = number(id)+sigSpace
  fun strIndex(id: symbol) = number(id)+strSpace
  fun fctIndex(id: symbol) = number(id)+fctSpace
  fun fixIndex(id: symbol) = number(id)+fixSpace

  fun varKey(id: symbol) = (number(id)+varSpace, name(id))
  fun conKey(id: symbol) = (number(id)+varSpace, name(id))
  fun tycKey(id: symbol) = (number(id)+tycSpace, name(id))
  fun sigKey(id: symbol) = (number(id)+sigSpace, name(id))
  fun strKey(id: symbol) = (number(id)+strSpace, name(id))
  fun fctKey(id: symbol) = (number(id)+fctSpace, name(id))
  fun fixKey(id: symbol) = (number(id)+fixSpace, name(id))

  fun key(namespace:int, id:symbol) = 
      (number id + namespace, name id)

end (* NameSpace *)
