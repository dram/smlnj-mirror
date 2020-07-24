# SML/NJ Calling Conventions

This note describes the different calling conventions used by the compiler
to transfer control between fragments (extended basic blocks).  For the
**MLRISC**-based code generator, the relevant code is in the files
`CodeGen/cpscompile/argPassing.{sig,sml}` and `CodeGen/main/mlrisc-gen-fn.sml`.

## Registers

The calling conventions pass all arguments in registers (note that on some
targets, some registers are actually allocated in memory).  There are four
special registers used in the calling conventions:

1. `STD_LINK`
2. `STD_CLOS`
3. `STD_ARG`
4. `STD_CONT`

In addition, there are a target-specific number of miscellaneous registers
(*i.e.*, `MISC1`, ..., `MISCn`) and a set of available floating-point
registers.  We define the *general-purpose argument registers* to be
the list `STD_ARG`, `MISC1`, , ..., `MISCn`.

The compiler defines a number of registers used to hold *callee-save*
values.  We use *NCS* to designate the number of general-purpose callee-save
registers and *NFCS* to designate the number of floating-point callee-save
registers.  Currently the compiler uses *NCS* = 3 and *NFCS* = 0.

## Standard function call

This convention is used to transfer control to an unkonwn or escaping function.
Such a call must have at least *NCS* + *NFCS* + 3 arguments.
The first three arguments are

1. `STD_LINK`
2. `STD_CLOS`
3. `STD_CONT`

Then come the callee-save and floating-point callee-save registers

The remaining registers are assigned to argument registers by type.
Pointer and integer arguments are mapped to the general-purpose
argument registers and floating-point arguments are mapped to the
available floating-point registers.

## Standard continuation call

This convention is used to transfer control to an unkonwn or escaping
continuation.  The convention depends on whether $NCS$ is zero or not,
but we first assume that it is at least one.   In this case, the call
must have at least *NCS* + *NFCS* + 1 arguments.  The first argument
is mapped to the `STD_CONT` register, then come the callee saves
followed by the floating-point callee saves.  Any remaining arguments
are mapped by type to either the general-purpose argument registers
or the available floating-point registers.

## Call of a known function

When all of the calls to a function *f* occur from known call sites
(and those call sites only call *f*), then an internal calling
convention is used.  Essentially, such a call is a **goto** with
arguments.  Arguments are assigned by type to **MLRISC** pseudo
registers such that the same registers are used at the call sites
and the function entry.

A call to a known function may also involve a heap limit check.
For some targets (*Alpha*, *PowerPC*, and *Sparc*), this limit
check is done at the call site and the result of the check is
placed in the reserved `EXHAUSTED` condition-code register.
It is unclear what the benefit of splitting the test across the
call is.
