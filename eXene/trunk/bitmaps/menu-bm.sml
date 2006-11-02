(* menu-bm.sml
 * this file created by bm2mlx
 * from:  mit/menu8 mit/menu10 mit/menu12 mit/menu16
 * on: Wed Mar  6 15:23:33 EST 1991
 *)
structure MenuBM =
  struct
    val menu8 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=8, ht=8},
            data = [[
                "\254",
                "\130",
                "\187",
                "\131",
                "\187",
                "\131",
                "\255",
                "\063"
              ]]
          }
    val menu10 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=10, ht=10},
            data = [[
                "\127\128",
                "\064\128",
                "\064\192",
                "\076\192",
                "\064\192",
                "\076\192",
                "\064\192",
                "\064\192",
                "\127\192",
                "\031\192"
              ]]
          }
    val menu12 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=12, ht=12},
            data = [[
                "\127\192",
                "\064\064",
                "\064\096",
                "\078\096",
                "\064\096",
                "\078\096",
                "\064\096",
                "\078\096",
                "\064\096",
                "\064\096",
                "\127\224",
                "\031\224"
              ]]
          }
    val menu16 = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\063\248",
                "\032\008",
                "\032\012",
                "\032\012",
                "\039\204",
                "\032\012",
                "\039\204",
                "\032\012",
                "\039\204",
                "\032\012",
                "\039\204",
                "\032\012",
                "\032\012",
                "\032\012",
                "\063\252",
                "\015\252"
              ]]
          }
  end (* MenuBM *)
