functor Interact( Glue : sig val compileIn : unit -> unit end ) : sig end =
 struct val _  = Glue.compileIn() end


structure CompVax = Compile(VaxGlue)
structure CompM68 = Compile(M68Glue)
structure IntVax = Interact(VaxGlue)
structure IntM68 = Interact(M68Glue)
