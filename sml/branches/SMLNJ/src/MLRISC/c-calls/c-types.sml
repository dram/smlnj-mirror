(* c-types.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies
 *
 * A representation of C Types for specifying the arguments and results
 * of C function calls.
 *)


structure CTypes =
  struct

    datatype c_type
      = C_void
      | C_float
      | C_double
      | C_long_double
      | C_unsigned of c_int
      | C_signed of c_int
      | C_PTR
      | C_ARRAY of (c_type * int)
      | C_STRUCT of c_type list

    and c_int
      = I_char
      | I_short
      | I_int
      | I_long
      | I_long_long

  end
