(* Copyright 1989 by      Department of Computer Science, 
 *                        The Technical University of Denmak
 *                        DK-2800 Lyngby 
 *
 * 17 Dec. 1991    Yngvi Skaalum Guttesen       (ysg@id.dth.dk)
 *)

signature JUMPS386 = 
sig
	datatype JumpKind = JMP | Jcc of int | LEA of int | LABPTR of int
	datatype Size = Byte | Word | Long

	val sizeint  : int                   -> Size
	val ebyte    : int                   -> string
	val elong    : int                   -> string
	val sizejump : JumpKind*int*int*int  -> int
	val emitjump : JumpKind*int*int*int  -> string
	val emitlong : int                   -> string
end
