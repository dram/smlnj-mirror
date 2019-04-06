structure Dummy : ASSEMBLY =
  struct
    datatype flags = READ | WRITE | APPEND
    datatype time = TIME of {sec : int, usec : int}
    structure C = struct
      datatype datalist = DATANIL | DATACONS of (string * string * datalist)
      type object = unit
      exception Div
      exception Float of string
      exception Interrupt
      exception Overflow
      exception SystemCall of string
      val array0 = InLine.cast()
      val bytearray0 = InLine.cast()
      val collected = InLine.cast()
      val collectedfrom = InLine.cast()
      val current = InLine.cast()
      val datalist = InLine.cast()
      val external = InLine.cast()
      val gcmessages = InLine.cast()
      val majorcollections = InLine.cast()
      val minorcollections = InLine.cast()
      val opsys = InLine.cast()
      val pstruct = InLine.cast()
      val ratio = InLine.cast()
    end
    val array = InLine.cast()
    val create_b = InLine.cast()
    val create_s = InLine.cast()
    val exportFn = InLine.cast()
    val exportML = InLine.cast()
    val fionread = InLine.cast()
    val floor = InLine.cast()
    val fork = InLine.cast()
    val logb = InLine.cast()
    val profvec = InLine.cast()
    val scalb =  InLine.cast()
    val syscall =  InLine.cast()
    val system =  InLine.cast()
    val timer = InLine.cast()
  end

structure Core = CoreFunc(Dummy)
