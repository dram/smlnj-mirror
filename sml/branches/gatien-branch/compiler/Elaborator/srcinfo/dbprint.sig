(* module containing printers for the types defined in DBtypes *)
signature DBPRINT =
sig
   val rtoS : DBTypes.location -> string
   val stoS : Symbol.symbol -> string
   val ptoS : Symbol.symbol list -> string
   val rptoS : InvPath.path -> string

   val print_ty' : DBTypes.ty' -> unit
   val print_tycon' : DBTypes.tycon' -> unit

   val print_key : DBTypes.key -> string

   val print_occ : DBTypes.occurrence -> unit
   val print_var : DBTypes.var_elem -> unit
   val print_type : DBTypes.type_elem -> unit
   val print_cons : DBTypes.cons_elem -> unit
   val print_str : DBTypes.str_elem -> unit
   val print_sig : DBTypes.sig_elem -> unit
   val print_ext : DBTypes.ext_elem -> unit

end (* signature DBPRINT*)
