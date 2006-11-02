(* left-ptr-bm.sml
 * this file created by bm2mlx
 * from:  mit/left_ptr mit/left_ptrmsk
 * on: Wed Mar  6 15:25:02 EST 1991
 *)
structure LeftPtrBM =
  struct
    val left_ptr = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\016\000",
                "\024\000",
                "\028\000",
                "\030\000",
                "\031\000",
                "\031\128",
                "\031\192",
                "\031\224",
                "\031\000",
                "\027\000",
                "\017\128",
                "\001\128",
                "\000\192",
                "\000\192",
                "\000\000"
              ]]
          }
    val left_ptrmsk = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\048\000",
                "\056\000",
                "\060\000",
                "\062\000",
                "\063\000",
                "\063\128",
                "\063\192",
                "\063\224",
                "\063\240",
                "\063\240",
                "\063\128",
                "\059\192",
                "\051\192",
                "\001\224",
                "\001\224",
                "\000\192"
              ]]
          }
  end (* LeftPtrBM *)
