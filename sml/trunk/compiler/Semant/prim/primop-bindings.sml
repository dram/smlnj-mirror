structure PrimopBindings : sig

    val prims : PrimopBind.primop_bind list

  end = struct

  (* combine the new and old bindings to produce an environment that can
   * support either naming scheme.
   *)
    val prims = OldPrimopBindings.prims @ NewPrimopBindings.prims

  end
