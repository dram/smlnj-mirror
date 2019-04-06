signature SYMBOL = sig
    type symbol
    val eq: symbol * symbol -> bool
    and symbol: string -> symbol
    and name: symbol -> string
    and number: symbol -> int
end
