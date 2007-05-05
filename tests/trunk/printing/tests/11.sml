(* Datatype Replication Printing
   The pretty printer prints the name of the replicated type rather than the new 
   name. 

   Incorrect Behavior:
   signature S = 
     sig
       datatype t = datatype t
     end
 *)

datatype t = A

signature S =
sig
  datatype u = datatype t
end
