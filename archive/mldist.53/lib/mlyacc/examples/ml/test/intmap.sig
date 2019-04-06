(* forgot all the vals *)

signature INTMAP =
  sig
    type 'a intmap
    namednew : string * int * exn -> '1a intmap
    new : int * exn -> '1a intmap
    add : '2a intmap -> int * '2a -> unit
    rem : 'a intmap -> int -> unit
    map : 'a intmap -> int -> 'a
    app : (int * 'a -> unit) -> 'a intmap -> unit
    intMapToList: 'a intmap -> (int * 'a) list
  end
