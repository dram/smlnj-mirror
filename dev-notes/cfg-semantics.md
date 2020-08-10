# Informal Semantics of the CFG IR

The **CFG** IR was introduced to provide a bridge between the first-order
**CPS** IR and the **LLVM** code generator.  This document gives an informal
description of the semantics of the **CFG** IR in terms of the abstract
machine model for **SML/NJ**.


## The SML/NJ Abstract Machine


## The CFG IR

The **CFG** IR has two main types:

* Expressions (`exp`), which are trees that define computations.  These roughly
  correspond to the **MLRISC** `mltree` type and the **LLVM** `Value`
  type.

* Statements (`stm`), which are extended basic blocks with operations
  for allocation, updates, and control flow.

## The Semantics of CFG Statements

* `LET(e, (x, ty), stm)` evaluates the expression `e` and binds the result to `x`
  in the continuation `stm`.

* `ALLOC(p, args, x, stm)` allocates memory as specified by the allocation primop
  `p` and binds a pointer to the object to `x` in the continuation `stm`.  The
  allocation primops are

    * `RECORD{desc, mut}` allocates a record of ML values initialized to the
      results of evaluating `args` list.  The `desc` value is the object's descriptor
      and the `mut` flag is true if the object is mutable (*i.e.*, a `ref` cell).

    * `RAW_RECORD{desc, kind, sz}` allocates a record of raw numeric values of the
      specified kind and size in bits.  The `desc` value is the object's descriptor
      and the object is initialized to the results of evaluating the `args` list.

    * `RAW_ALLOC{desc, align, len}` allocates an uninitialized heap object of `len`
      bytes.  The descriptor is optional (and is not counted in the object's length).
      The `align` value is the alignment requirement for the object in bytes.

* `ARITH(p, args, (x, ty), k)`

* `SETTER(p, args, k)`

    * `UNBOXED_UPDATE`

    * `UPDATE`

    * `UNBOXED_ASSIGN`

    * `ASSIGN`

    * `RAW_UPDATE (numkind kind, int sz)`

    * `RAW_STORE (numkind kind, int sz)`

    * `SET_HDLR`

    * `SET_VAR`

* `APPLY(f::args, tys)` makes the call `f (f, args)` using the standard calling
  convention for escaping functions.  The `tys` are the types of the arguments
  list (including the function `f`, which is the first argument in a standard call).

* `THROW(k::args, tys)` makes the call `k (k, args)` using the standard calling
  convention for escaping continuations.  The `tys` are the types of the arguments
  list (including the continuation `k`).

* `GOTO(cc, lab, args, tys)` transfers control to the label `lab` with arguments
  `args` using the calling convention specified by `cc`.  The `tys` are the types
   of the arguments list.

* `SWITCH(arg, cases)` evaluates `arg` to an integer in the range
  `0..length(cases)-1` and then dispatches control to the specified case.

* `BRANCH(p, args, prob, kTrue, kFalse)` evaluates the `args` and then applies the
  conditional primop `p` to the argument results.  If the result is true, then the
  `kTrue` continuation is executed, otherwise the `kFalse` continuation is
  executed.  The `prob` field is an integer in the range `0..99`, representing
  the likelihood that the test will return true.  A probability value of `0`
  means that there is no prediction of the branch.

* `RCC{reentrant, name, proto, args, results, live, k}`

## The Semantics of CFG Expressions

CFG expressions are *pure* in that they do not allocate or modify memory,
or raise exceptions.

* `VAR x` evaluates the the value bound to `x`

* `LABEL lab` evaluates the the address of the label `lab`

* `NUM{iv, signed, sz}` evaluates to an signed or unsigned integer of `sz` bits.

* `LOOKER(p, args)` evaluates the argument expressions and then applies the
  primop to the results.  The `looker` primops are

    * `DEREF` fetches the contents of the reference cell specified by the argument.

    * `SUBSCRIPT`

    * `RAW_SUBSCRIPT(INT, sz)`

    * `RAW_SUBSCRIPT(FLT, sz)`

    * `GET_HDLR`

    * `GET_VAR`

* `PURE(p, args)` evaluates the argument expressions and then applies the
  primop to the results.  The `pure` primops are

    * `ADD`

    * `SUB`

    * `SMUL`

    * `SDIV`

    * `SREM`

    * `UMUL`

    * `UDIV`

    * `UREM`

    * `LSHIFT`

    * `RSHIFT`

    * `RSHIFTL`

    * `ORB`

    * `XORB`

    * `ANDB`

    * `FADD`

    * `FSUB`

    * `FMUL`

    * `FDIV`

    * `FNEG`

    * `FABS`

    * `FSQRT`

* `SELECT(i, arg)` extracts the `i`th element from the record specified by
  `arg` (0-based).  The selected value should a valid SML value; *i.e.*,
  either a pointer or a tagged integer.

* `OFFSET(i, arg)` extracts the `i`th element from the record specified by
  `arg` (0-based).

