(* dropbar-bm.sml
 * this file created by bm2mlx
 * from:  mit/dropbar7 mit/dropbar8
 * on: Wed Mar  6 15:22:45 EST 1991
 *)
structure DropBarBM =
  struct
    val dropbar7 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=7, ht=7},
            data = [[
                "\000",
                "\252",
                "\134",
                "\134",
                "\254",
                "\126",
                "\000"
              ]]
          }
    val dropbar8 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=8, ht=8},
            data = [[
                "\000",
                "\254",
                "\131",
                "\131",
                "\131",
                "\255",
                "\127",
                "\000"
              ]]
          }
  end (* DropBarBM *)
