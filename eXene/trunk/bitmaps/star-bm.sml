(* star-bm.sml
 * this file created by bm2mlx
 * from:  mit/star mit/starMask
 * on: Wed Mar  6 15:25:33 EST 1991
 *)
structure StarBM =
  struct
    val star = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\001\000",
                "\001\000",
                "\017\016",
                "\009\032",
                "\005\064",
                "\002\128",
                "\124\124",
                "\002\128",
                "\005\064",
                "\009\032",
                "\017\016",
                "\001\000",
                "\001\000",
                "\000\000",
                "\000\000"
              ]]
          }
    val starMask = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\003\128",
                "\003\128",
                "\059\184",
                "\063\248",
                "\063\248",
                "\031\240",
                "\255\254",
                "\255\254",
                "\255\254",
                "\031\240",
                "\063\248",
                "\063\248",
                "\059\184",
                "\003\128",
                "\003\128",
                "\000\000"
              ]]
          }
  end (* StarBM *)
