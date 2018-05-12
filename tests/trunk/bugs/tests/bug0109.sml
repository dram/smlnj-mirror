(* bug109.sml *)
signature EQSIG =
sig
  type r
  datatype s = S of r
       and t = T of s
  sharing type r = t
end;

