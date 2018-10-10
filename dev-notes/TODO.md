# SML/NJ TO-DO LIST

This is a list of things (both major and minor) that should be fixed/improved/changes
in the SML/NJ compiler.  It is organized into short-term goals by proposed release
target plus an additional "wish-list" of long-term goals.

## For Version 110.85 (late 2018)

  * Rationalization and cleanup of primops that are exported by the compiler
    (see `dev-notes/primop-list.md`).

  * There are many Basis Library modules that include definitions of the form
    ````sml
      fun x ++ y = InlineT.Word31.copyt_int31 (InlineT.Word31.copyf_int31 x +
					       InlineT.Word31.copyf_int31 y)
      fun x -- y = InlineT.Word31.copyt_int31 (InlineT.Word31.copyf_int31 x -
					       InlineT.Word31.copyf_int31 y)
    ````
    Perhaps we should add unchecked add and subtract operations to InlineT.Int31?

  * CPS support for `Word64.word` and `Int64.int` operations.

  * Switch `Position` structure to be `Int64.int`.

  * Switch to new encoding of literals (see new-literals.md)

## For Version 110.86 (Spring 2019)

  * Switch environment pickling to use ASDL

## For Version 110.87 (Summer 2019)

  * Must have 64-bit port done by June 2019 (but earlier would be better).

## Future work

  * The `CPS.SWITCH` construct only supports tagged integer arguments (this limitation
    is because of the way that jump-table indices is handled in `CodeGen/mlriscGen.sml`).
    It should be generalized to boxed, fixed-precision, integer types.

  * switches on strings use string equality as the basic operation (the `CPS.P.streq` and
    `CPS.strneq` operators), which does not allow for binary search.  Replace these with
    a `STRCMP` three-way branch in the `cexp` type.

  * All of the intermediate representations (FLINT, CPS, etc.) use the same LambdaVar.lvar
    type to represent variables.  There should be distinct types for these to avoid
    potential confusion and errors.

  * FLINT types (`FLINT/kernel`) need a thorough overhaul. There are too
    many layers of type representations, and the complexity of Nadathur
    closures is probably unjustified. Even the hash-consing of types may
    no longer be justified -- depends on the space blowup without
    it. FLINT primative types for numbers do not distinguish between
    signed and unsigned numbers (i.e. ints and words). Should they?

  * The `unboxedFloats` flag in the `MACH_SPEC` signature is true for all targets; can
    we get rid of it?

  * Rewrite of pattern-match compilation to be an source to source translation of the
    `Absyn` IR.  Include direct translation of "or" patterns and support for Successor
    ML views.

  * Add support for `Real32.real`, including arrays and vectors.
