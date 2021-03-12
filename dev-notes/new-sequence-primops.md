# New Sequence Primitives (a Design Proposal)

Many years ago, we switched the representation of arrays in SML/NJ to consist
of a header object that consisted of a data pointer and a length (in elements)
field.  This design had some benefits for the garbage collector (it allows
object lengths to be in words) and also allows memory-mapped files to be
treated as arrays/vectors.  The obvious cost of this design was an extra
level of indirection when accessing or updating elements.

At the time of this change, it was expected that direct access to the
data objects would be provided to allow more efficient implementation
of array/vector iterators (*e.g.*, Array.app).  Also, by exposing the
representation inside the compiler, it would be possible to use CSE
to combine loads of the data pointer.  Unfortunately, because of
concerns about the impact on type safety in FLINT, which at that time
was going to be used for certified compilation, the representation
largely stayed abstract (there is a bit of support for access to the
representation that is used by polymorphic equality).

This document proposes a new set of primitive operations for the `Inline`
structure, as well as internal compiler primops to expose the representation
of arrays and vectors.  Furthermore, we extend this representation
with compiler support for slices.


## Types

We need to introduce new types into the compiler for the data objects.

``` sml
type 'a arr_data
type 'a vec_data
type w8arr_data
type w8vec_data
type r64arr_data
type r64vec_data
type 'a slice
```

## Polymorphic Arrays

  * `arr_empty_data : 'a arr_data` -- empty data object for a polymorphic array

  * `arr_data : 'a array -> 'a arr_data` -- get the data object for a polymorphic array

  * `arr_length : 'a array -> int` -- get the length for a polymorphic array

  * `arr_unsafe_make : 'a arr_data * int -> 'a array` -- make an array from a
    a data object and length; this operation is marked `unsafe` to signal the
    fact that it is the user's responsibility to use a correct length value.

  * `arr_data_unsafe_sub : 'a arr_data * int -> 'a` -- unchecked subscript operation
    on a polymorphic-array data object.

  * `arr_data_unsafe_update : 'a arr_data * int * 'a -> unit` -- unchecked
    update operation on a polymorphic-array data object.

## Polymorphic Vectors

  * `vec_empty_data : 'a vec_data` -- empty data object for a polymorphic vector

  * `vec_data : 'a vector -> 'a vec_data` -- get the data object for a polymorphic vector

  * `vec_length : 'a vector -> int` -- get the length for a polymorphic vector

  * `vec_unsafe_make : 'a vec_data * int -> 'a vector` -- make a vector from a
    a data object and length; this operation is marked `unsafe` to signal the
    fact that it is the user's responsibility to use a correct length value.

  * `vec_data_unsafe_sub : 'a vec_data * int -> 'a` -- unchecked subscript operation
    on a polymorphic-vector data object.

## Monomorphic Arrays

Here we give the types for the operations that work on arrays of `Word8.word` values;
similar operations should be provided for the other monomorphic array types that are
supported by the compiler.

  * `w8arr_empty_data : w8arr_data` -- empty data object for a word8 array

  * `w8arr_data : word8array -> w8arr_data` -- get the data object for a word8 array

  * `w8arr_length : word8array -> int` -- get the length for a word8 array

  * `w8arr_unsafe_make : w8arr_data * int -> word8array` -- make an array from a
    a data object and length; this operation is marked `unsafe` to signal the
    fact that it is the user's responsibility to use a correct length value.

  * `w8arr_data_unsafe_sub : w8arr_data * int -> word8` -- unchecked subscript operation
    on a word8-array data object.

  * `w8arr_data_unsafe_update : w8arr_data * int * word8 -> unit` -- unchecked
    update operation on a word8-array data object.

## Monomorphic Vectors

  * `w8vec_empty_data : w8vec_data` -- empty data object for a word8 vector

  * `w8vec_data : word8vector -> w8vec_data` -- get the data object for a word8 vector

  * `w8vec_length : word8vector -> int` -- get the length for a word8 vector

  * `w8vec_unsafe_make : w8vec_data * int -> word8vector` -- make a vector from a
    a data object and length; this operation is marked `unsafe` to signal the
    fact that it is the user's responsibility to use a correct length value.

  * `w8vec_data_unsafe_sub : 'a vec_data * int -> word8` -- unchecked subscript
    operation on a word8-vector data object.

  * `w8vec_data_unsafe_update : w8vec_data * int * word8 -> unit` -- unchecked
    update operation on a word8-vector data object.

## Slices

For slices, we use a type constructor for the internal slice representation
that has the type of the underlying data source (array or vector) as its
type argument; *e.g.* the `substring` type would be represented in the
Basis Library implementation as `word8_vector slice` (since the `string` type
is represented as a `word8_vector`).

  * `arr_slice_data : 'a array slice -> 'a arr_data` -- get the data object
    for a polymorphic array slice.

  * `vec_slice_data : 'a vector slice -> 'a arr_data` -- get the data object
    for a polymorphic array slice.

  * `w8arr_slice_data : word8array slice -> w8arr_data` -- get the data object
    for a polymorphic array slice.

  * `w8vec_slice_data : word8vector slice -> w8vec_data` -- get the data object
    for a polymorphic array slice.

  * `slice_length : 'a slice -> int` -- get the length of a slice.

  * `slice_index : 'a slice -> int` -- get the start index of a slice.

  * `slice_base_length : 'a slice -> int` -- get the length of the underlying
    data source for the slice.

  * `slice_make : 'a * int * int -> 'a slice` -- make a slice header object from
    a data source (array or vector), a start index, and a slice length.  This
    operation will extract the kind field from the data source (as well as the
    data object and base length).

  * `slice_base : 'a slice -> 'a` -- return the underlying data source for the
    slice.

### Representation

We will add two new object-descriptor tags for array and vector slice headers
(see `runtime-rep.md`).
As is the case for array and vector descriptors, the slice descriptors will use
the length field to store the kind of the array or vector and we will use
the same encoding of kinds.

## Assembly Code Support

The runtime system provides a small number of assembly-code routines to support
creation of arrays and vectors.  We propose to replace these operations with
functions for creating data objects as follows:

  * `arr_data_new : int * 'a -> 'a arr_data` -- allocates a new polymorphic-array
    data object with the given number of elements, which are all initialized
    to the second argument.

  * `arr_data_fromlist : int * 'a list -> 'a arr_data` -- allocates a new
    polymorphic-array data object with the given number of elements, which
    are initialized from the given list of values.

  * `vec_data_fromlist : int * 'a list -> 'a vec_data` -- allocates a new
    polymorphic-vector data object with the given number of elements, which
    are initialized from the given list of values.

  * `raw_data : int -> 'a` -- allocates an uninitialized raw data object
    of the given number of words.  The object will be word aligned.

  * `raw64_data : int -> 'a` -- allocates an uninitialized raw data object
    of the given number of 64-bit elements.  The object will be 64-bit
    aligned.<br/>
    On 64-bit machines, this function is identical to `raw_data`.

## Discussion

Note that testing for array equality requires testing that the data pointers
are equal (not the header pointers), since there can be multiple header
objects for a given array.

## Examples

Here is an example of how the `Array.modify` function could be implemented
using direct access to the data object:
``` sml
fun modify f arr = let
      val data = InlineT.arr_data arr
      val len = InlineT.arr_length arr
      fun lp i = if (i < len)
            then (
              InlineT.arr_data_unsafe_update(
                data, i,
                f (InlineT.arr_data_unsafe_sub(data, i)));
              lp (i+1))
            else ()
      in
        lp 0
      end
```
