structure RawMemInlineT = struct
    val w8l  : word32 -> word32        = InLine.raww8l
    val i8l  : word32 -> int32         = InLine.rawi8l
    val w32l : word32 -> word32        = InLine.raww32l
    val i32l : word32 -> int32         = InLine.rawi32l
    val f64l : word32 -> real          = InLine.rawf64l
    val w8s  : word32 * word32 -> unit = InLine.raww8s
    val i8s  : word32 * int32  -> unit = InLine.rawi8s
    val w32s : word32 * word32 -> unit = InLine.raww32s
    val i32s : word32 * int32  -> unit = InLine.rawi32s
    val f64s : word32 * real   -> unit = InLine.rawf64s
end
