CM.autoload "full-cm.cm";
CM.autoload "host-cmb.cm";
fun serv n =
    CM.Server.start { name = n, pref = 0, pathtrans = NONE,
		      cmd = ("./testml", ["sml", "@CMslave"]) };
