(* x86jumps.sig
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *
 *)

signature X86JUMPS = 
sig
   datatype JumpKind = JMP | Jcc of int | LEA of int | LABPTR of int
   datatype Size = SevenBits | Byte | Word | Long

   val sizeint	: int			-> Size
   val ebyte	: int			-> string
   val elong	: int			-> string
   val sizejump : JumpKind*int*int*int	-> int
   val emitjump : JumpKind*int*int*int	-> string
   val emitlong : int			-> string
   val emitBackptr : int		-> string

   (* Word32 support *)
   val sizeintW32	: Word32.word		-> Size
   val ebyteW32 	: Word32.word		-> string
   val elongW32 	: Word32.word		-> string
end

(*
 * $Log: x86jumps.sig,v $
 * Revision 1.2  1998/02/12 20:48:54  jhr
 *   Removed references to System.Tags.
 *
 * Revision 1.1.1.1  1997/01/14 01:38:50  george
 *   Version 109.24
 *
 *)
