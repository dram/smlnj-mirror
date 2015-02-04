structure CGoptions =
struct
   val knownfunc = ref true
   val tailrecur = ref true
   val primapp = ref true
   val recordopt = ref true
   val tail = ref true
   val closurecount = ref false
   val closureprint = ref false
   val chained = ref true
   val hoist = ref true
end
