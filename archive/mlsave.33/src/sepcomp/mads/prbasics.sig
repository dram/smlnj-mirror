(* prbasics.sig *)

signature PR_BASICS=
 sig 
    structure B: BASICS sharing B=Basics
    type dir
    val emptydir: dir
    val pr_binding: B.binding -> dir -> string * dir
    and pr_int: int -> dir -> string * dir
    and pr_lvar: B.Access.lvar -> dir -> string * dir
    and pr_stampInfo: B.stampInfo -> dir -> string * dir
    and pr_strenv: {s: B.Structure array, t: B.tycon array} -> dir ->
                   string * dir
    and pr_symbol: B.Symbol.symbol -> dir -> string * dir
    and pr_symtable: (string * B.binding) B.Intmap.intmap -> dir ->
                        string * dir
    and pr_ty: B.ty -> dir -> string * dir

    and pr_dir: dir -> string    

 end;

