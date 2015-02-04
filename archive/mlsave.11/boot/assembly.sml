structure Dummy : ASSEMBLY =
struct
    datatype flags = READ | WRITE | APPEND
    datatype time = TIME of {sec : int, usec : int}
    exception Div
    exception Float of string
    exception Interrupt
    exception Io_failure of string
    exception Overflow
    val array = InLinePrim.cast()
    val array0 = InLinePrim.cast()
    type object = unit
    val boot = InLinePrim.cast()
    val byte_array0 = InLinePrim.cast()
    val chdir = InLinePrim.cast()
    val close = InLinePrim.cast()
    val control = InLinePrim.cast()
    val create_b = InLinePrim.cast()
    val create_s = InLinePrim.cast()
    val execute = InLinePrim.cast()
    val export = InLinePrim.cast()
    val export1 = InLinePrim.cast()
    val floor = InLinePrim.cast()
    val fionread = InLinePrim.cast()
    val isatty = InLinePrim.cast()
    val logb = InLinePrim.cast()
    val openf = InLinePrim.cast()
    val pstruct = InLinePrim.cast()
    val read =  InLinePrim.cast()
    val scalb =  InLinePrim.cast()
    val seql =  InLinePrim.cast()
    val system =  InLinePrim.cast()
    val timer = InLinePrim.cast()
    val write = InLinePrim.cast()
end

structure Initial = PervFunc(Dummy)
