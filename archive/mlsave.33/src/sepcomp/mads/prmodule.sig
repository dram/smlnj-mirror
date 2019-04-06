(* pr_module.sig, printing of a static module in symbolic form;
   the symbolic form is very verbose but it is helpful in understanding
   the inner workings of the parser and the type checker *)

signature PR_MODULE=
  sig
    structure Env: sig  type statModule end
    val pr_module: Env.statModule -> string
  end;
