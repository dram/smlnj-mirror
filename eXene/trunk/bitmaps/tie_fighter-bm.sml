(* tie_fighter-bm.sml
 * this file created by bm2mlx
 * from:  mit/tie_fighter
 * on: Wed Mar  6 15:24:13 EST 1991
 *)
structure TieFighterBM =
  struct
    val tie_fighter = EXeneBase.IMAGE{
            sz = Geometry.SIZE{wid=16, ht=16},
            data = [[
                "\000\000",
                "\000\000",
                "\016\008",
                "\032\004",
                "\064\002",
                "\064\002",
                "\071\226",
                "\124\062",
                "\072\018",
                "\124\062",
                "\071\226",
                "\064\002",
                "\066\066",
                "\038\100",
                "\020\040",
                "\000\000"
              ]]
          }
  end (* TieFighterBM *)
