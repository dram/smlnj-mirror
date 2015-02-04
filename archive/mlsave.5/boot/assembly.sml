structure Dummy : ASSEMBLY =
struct
    datatype flags = READ | WRITE | APPEND
    exceptionx div : unit
    exceptionx float : string
    exceptionx interrupt : unit
    exceptionx io_failure : string
    exceptionx overflow : unit
    val array = InLinePrim.cast()
    val array0 = InLinePrim.cast()
    type Structure = unit
    val boot = InLinePrim.cast()
    val byte_array0 = InLinePrim.cast()
    val close = InLinePrim.cast()
    val create_b = InLinePrim.cast()
    val create_s = InLinePrim.cast()
    val export = InLinePrim.cast()
    val floor = InLinePrim.cast()
    val fionread = InLinePrim.cast()
    val isatty = InLinePrim.cast()
    val logb = InLinePrim.cast()
    val openf = InLinePrim.cast()
    val pstruct = InLinePrim.cast()
    val read =  InLinePrim.cast()
    val scalb =  InLinePrim.cast()
    val seql =  InLinePrim.cast()
    val write = InLinePrim.cast()
end;

structure Initial = PervFunc(Dummy);
