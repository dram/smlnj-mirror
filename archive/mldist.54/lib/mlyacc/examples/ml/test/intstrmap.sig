signature INTSTRMAP =
  sig
    type 'a intstrmap
    val namednew : string * int * exn -> '1a intstrmap
    val new : int * exn -> '1a intstrmap
    val add : '2a intstrmap -> int * string * '2a -> unit
    val rem : 'a intstrmap -> int * string -> unit
    val map : 'a intstrmap -> int * string -> 'a
    val app : (int * string * 'a -> unit) -> 'a intstrmap -> unit
    val intMapToList: 'a intstrmap -> (int * string * 'a) list
  end
