(* boxes-bm.sml
 * this file created by bm2mlx
 * from:  mit/boxes
 * on: Wed Mar  6 15:22:18 EST 1991
 *)
structure BoxesBM =
  struct
    val boxes = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\240\240",
                "\144\144",
                "\144\144",
                "\240\240",
                "\015\015",
                "\009\009",
                "\009\009",
                "\015\015",
                "\240\240",
                "\144\144",
                "\144\144",
                "\240\240",
                "\015\015",
                "\009\009",
                "\009\009",
                "\015\015"
              ]]
          }
  end (* BoxesBM *)
