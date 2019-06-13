# SML/NJ TO-DO LIST

This is a list of things (both major and minor) that should be fixed/improved/changes
in the SML/NJ compiler.  It is organized into short-term goals by proposed release
target plus an additional "wish-list" of long-term goals.

## For Version 110.91

  * Switch over to the C99 standard `<fenv.h>` API, instead of the home-grown
    code in `smlnj-math/fp-dep.h` etc.

  * Switch to new encoding of literals (see new-literals.md)

## For Verion 110.92+

  * Must have 64-bit port done by Fall 2019 (but earlier would be better).

## Future work

  * Cleanup `compiler/CPS/clos/closure.sml`; there is a lot of code that was
    written to support quasi-stacks, which is no longer needed.

  * Switch environment pickling to use ASDL

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

  * Many (most?) of the runtime system functions in the Windows port return `true` or
    `NONE` to signal an error, instead of raising the `SysErr` exception.  Furthermore,
    when a `SysErr` is raised, it does not contain the error information from the OS.
    We should use the `GetLastError` and `FormatMessage` functions to generate better
    information (see [Retrieving the Last Error
    Code](https://docs.microsoft.com/en-us/windows/desktop/Debug/retrieving-the-last-error-code)).

  * Remove runtime-typing from FLINT; the files involved include:
    - compiler/FLINT/reps/reify.sml
    - compiler/FLINT/reps/rttype.sml
    - compiler/FLINT/reps/typeoper.sml

