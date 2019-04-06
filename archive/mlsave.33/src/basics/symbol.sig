signature SYMBOL = sig
    type symbol
    val eq: symbol * symbol -> bool
    and new: string * int -> symbol
    and name: symbol -> string
    and number: symbol -> int
end
