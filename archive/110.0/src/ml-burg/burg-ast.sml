(* 
 * burg-ast.sml
 *
 * Abstract syntax trees for BURG specifications.
 *
 * $Log: burg-ast.sml,v $
 * Revision 1.1.1.1  1999/12/03 19:59:20  dbm
 * Import of 110.0.6 src
 *
 * Revision 1.1.1.1  1997/01/14 01:37:59  george
 *   Version 109.24
 *
 * Revision 1.1.1.2  1997/01/11  18:52:28  george
 *   ml-burg Version 109.24
 *
 * Revision 1.1.1.1  1996/01/31  16:01:24  george
 * Version 109
 * 
 *)

structure BurgAST =
  struct

    datatype decl_ast = START of string
		      | TERM of (string * string option) list
		      | TERMPREFIX of string
		      | RULEPREFIX of string
		      | SIG of string

    datatype pattern_ast = PAT of (string * pattern_ast list)

    datatype rule_ast = RULE of (string * pattern_ast * string * int list)

    datatype spec_ast = SPEC of {head : string list,
				 decls : decl_ast list, 
				 rules : rule_ast list,
				 tail : string list}
  end (* BurgAST *)

