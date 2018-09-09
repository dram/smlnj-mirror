## SML/NJ TO-DO LIST

This is a list of things (both major and minor) that should be fixed/improved/changes
in the SML/NJ compiler.

* The `CPS.SWITCH` construct only supports tagged integer arguments (this limitation
  is because of the way that jump-table indices is handled in `CodeGen/mlriscGen.sml`).
  It should be generalized to boxed, fixed-precision, integer types.

* switches on strings use string equality as the basic operation (the `CPS.P.streq` and
  `CPS.strneq` operators), which does not allow for binary search.  Replace these with
  a `STRCMP` three-way branch in the `cexp` type.

* All of the intermediate representations (FLINT, CPS, etc.) use the same LambdaVar.lvar
  type to represent variables.  There should be distinct types for these to avoid
  potential confusion and errors.
