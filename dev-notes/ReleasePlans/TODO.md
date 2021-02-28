# SML/NJ TO-DO LIST

This is a list of things (both major and minor) that should be fixed/improved/changes
in the SML/NJ compiler.  It is organized into short-term goals by proposed release
target plus an additional "wish-list" of long-term goals.

## For 2021.1

  * check if the problem with IntInf literals has gone away.

  * Rewrite of pattern-match compilation to be an source to source translation of the
    `Absyn` IR.  Include direct translation of "or" patterns and support for Successor
    ML views.

## High priority future work

  * 64-bit support for Windows

  * LLVM code generator (prototype under construction)

  * 64-bit ARM support.

## Future work

We divide future work into small and big projects.

### Small projects

  * Better type error messages:
    - Do not use the **same** name to refer to types that are **different**
    - Do not use **different** names to refer to types that are the **same**
    - Highlight points of difference when comparing types
    - For record types, distinguish between field name mismatches, missing/extra
      fields, and field type mismatches.
    - Sort error messages by file position.

  * Expand the `FSGN` primop in `CPS/convert/convert.sml` (as we do for `REAL_TO_BITS`)
    and remove it from the `CPS.P.branch` type.  This change would simplify the code
    generator.  Eventually, we should move the `REAL_TO_BITS` primop into the
    code generator so that it can be exposed to machines for which a register-to-register
    operation is possible.  We should also add a `BITS_TO_REAL` primop.

  * Cleanup `compiler/CPS/clos/closure.sml`; there is a lot of code that was
    written to support quasi-stacks, which is no longer needed.

  * The `CPS.SWITCH` construct only supports tagged integer arguments (this limitation
    is because of the way that jump-table indices is handled in `CodeGen/mlriscGen.sml`).
    It should be generalized to boxed, fixed-precision, integer types.

  * switches on strings use string equality as the basic operation (the `CPS.P.streq` and
    `CPS.strneq` operators), which does not allow for binary search.  Replace these with
    a `STRCMP` three-way branch in the `cexp` type.

  * All of the intermediate representations (FLINT, CPS, etc.) use the same LambdaVar.lvar
    type to represent variables.  There should be distinct types for these to avoid
    potential confusion and errors.

  * The handling of int/word types in FLINT/CPS should be overhauled.  Currently we
    make a distinction between wrapped and unwrapped native integers, which allows
    them to be represented in unboxed form across function applications.  We should
    do something similar for tagged int/word types (e.g., Word8).  In particular,
    this could avoid some overhead when reading/writing 8-bit data from vectors
    and arrays, but should also allow us to take advantage of native hardware
    support for 32-bit arithmetic on 64-bit hardware.

  * FLINT types (`FLINT/kernel`) need a thorough overhaul. There are too
    many layers of type representations, and the complexity of Nadathur
    closures is probably unjustified. Even the hash-consing of types may
    no longer be justified -- depends on the space blowup without
    it. FLINT primative types for numbers do not distinguish between
    signed and unsigned numbers (i.e. ints and words). Should they?

  * The `unboxedFloats` flag in the `MACH_SPEC` signature is `true` for all targets; can
    we get rid of it?

  * Functor specialization in the compiler.  This feature would allow more code
    sharing in the libraries (since functors could be used to generate multiple
    implementations).  It will require both the new Absyn representation and ASDL
    pickling.

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

    This is partially done for 110.92.  We have removed runtime types for arrays, but the
    mechanisms are still being used for polymorphic equality and datatype constructor
    representations (when they are functor arguments).

  * Expose the two-level representation of arrays and vectors and add compiler support
    for slices.

  * FLINT replacement (switch to 3CPS?)

### Big projects

  * New-new-runtime: new GC and core services with old version of C functions
    and the LLVM code generator.

  * Switch environment pickling to use ASDL

  * New post-typechecking Absyn representation that makes the polymorphic type machinery
    explicit (*i.e.*, type abstraction and applications explicit).

  * Compile-time application of functors.  This change will yield several significant
    benefits:
    - allow functors to be used to refactor the basis and SML/NJ library code
      so that there is less code duplication without sacrificing performance.
    - better performance for heavily functorized code (*e.g.*, MLton).
    - Eliminate the need for special tricks to get datatype representations
      right (*e.g.*, it avoids the `type 'a list = nil | cons of 'a t` problem.
      It should also allow more aggressive specialization of datatype representations.
    This change depends on the migration to ASDL for pickling (because we will need
    to pickle functors) and on the overhaul of the Absyn representation.

