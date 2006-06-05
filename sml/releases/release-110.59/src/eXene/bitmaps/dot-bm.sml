(* dot-bm.sml
 * this file created by bm2mlx
 * from:  mit/dot
 * on: Wed Mar  6 15:22:34 EST 1991
 *)
structure DotBM =
  struct
    val dot = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\000\000",
                "\000\000",
                "\003\192",
                "\015\240",
                "\015\240",
                "\031\248",
                "\031\248",
                "\031\248",
                "\031\248",
                "\015\240",
                "\015\240",
                "\003\192",
                "\000\000",
                "\000\000",
                "\000\000"
              ]]
          }
  end (* DotBM *)
