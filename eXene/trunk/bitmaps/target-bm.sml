(* target-bm.sml
 * this file created by bm2mlx
 * from:  mit/target
 * on: Wed Mar  6 15:24:05 EST 1991
 *)
structure TargetBM =
  struct
    val target = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\001\128",
                "\001\128",
                "\007\224",
                "\009\144",
                "\017\136",
                "\019\200",
                "\126\126",
                "\126\126",
                "\019\200",
                "\017\136",
                "\009\144",
                "\007\224",
                "\001\128",
                "\001\128",
                "\000\000"
              ]]
          }
  end (* TargetBM *)
