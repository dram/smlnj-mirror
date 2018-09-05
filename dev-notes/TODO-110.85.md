## TODO list for 110.85

* There are many Basis Library modules that include definitions of the form
  ````sml
    fun x ++ y = InlineT.Word31.copyt_int31 (InlineT.Word31.copyf_int31 x +
					     InlineT.Word31.copyf_int31 y)
    fun x ++ y = InlineT.Word31.copyt_int31 (InlineT.Word31.copyf_int31 x +
					     InlineT.Word31.copyf_int31 y)
  ````
  Perhaps we should add unchecked add and subtract operations to InlineT.Int31?

* Switch environment pickling to use ASDL

