signature IMPORTS = sig
  type import_kind
  val add : import_kind * Label.label -> unit
  val output : unit -> string
end
