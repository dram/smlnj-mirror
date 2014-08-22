(* right-ptr-bm.sml
 * this file created by bm2mlx
 * from:  mit/right_ptr mit/right_ptrmsk
 * on: Wed Mar  6 15:25:27 EST 1991
 *)
structure RightPtrBM =
  struct
    val right_ptr = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\000\008",
                "\000\024",
                "\000\056",
                "\000\120",
                "\000\248",
                "\001\248",
                "\003\248",
                "\007\248",
                "\000\248",
                "\000\216",
                "\001\136",
                "\001\128",
                "\003\000",
                "\003\000",
                "\000\000"
              ]]
          }
    val right_ptrmsk = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\012",
                "\000\028",
                "\000\060",
                "\000\124",
                "\000\252",
                "\001\252",
                "\003\252",
                "\007\252",
                "\015\252",
                "\015\252",
                "\001\252",
                "\003\220",
                "\003\204",
                "\007\128",
                "\007\128",
                "\003\000"
              ]]
          }
  end (* RightPtrBM *)
