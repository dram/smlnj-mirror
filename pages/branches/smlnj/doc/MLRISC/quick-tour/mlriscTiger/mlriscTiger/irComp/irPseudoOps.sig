signature IR_PSEUDO_OPS = sig
    datatype pseudo_op =
      PROLOGUE of string
    | EPILOGUE of string
    | STRING of Label.label * string 
end
