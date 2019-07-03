# Notes on LLVM code generation for SML/NJ

We want to eventually replace MLRisc (which is not well supported) with LLVM.
This document describes a plan for how that would work.

## Basic approach

We will use **asdlgen** to generate a pickler for the CPS representation.
The `MLRiscGen` functor will be replaced by an LLVM code generator in
the runtime system.  It will be a C++ program that takes the pickled CPS
code and then unpickles it into a C++ data structure from which code
is generated.  We will use the *Jump With Arguments* calling convention
that was described in the 2016 ML Workshop paper.

The AMD64 calling convention is described in the LLVM source file
`lib/Target/X86/X86CallingConv.td`
