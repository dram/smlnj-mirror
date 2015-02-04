structure Dummy : ASSEMBLY =
  struct
    datatype flags = READ | WRITE | APPEND
    datatype time = TIME of {sec : int, usec : int}
    datatype datalist = DATANIL | DATACONS of (string * string * datalist)
    exception Div
    exception Float of string
    exception Interrupt
    exception Overflow
    exception SystemCall of string
    type object = unit
    val array = InLine.cast()
    val array0 = InLine.cast()
    val boot = InLine.cast()
    val bytearray0 = InLine.cast()
    val chdir =  InLine.cast()
    val close = InLine.cast()
    val control = InLine.cast()
    val create_b = InLine.cast()
    val create_s = InLine.cast()
    val datalist = InLine.cast()
    val dup2 = InLine.cast()
    val execute = InLine.cast()
    val exportFn = InLine.cast()
    val exportML = InLine.cast()
    val fionread = InLine.cast()
    val floor = InLine.cast()
    val fork = InLine.cast()
    val isatty = InLine.cast()
    val logb = InLine.cast()
    val openf = InLine.cast()
    val pipe = InLine.cast()
    val profvec = InLine.cast()
    val pstruct = InLine.cast()
    val read =  InLine.cast()
    val scalb =  InLine.cast()
    val seql =  InLine.cast()
    val system =  InLine.cast()
    val timer = InLine.cast()
    val write = InLine.cast()
  end

structure Core = CoreFunc(Dummy)
