(*
 * Running ML-Burg from CM.
 *
 *   (C) 1999 Lucent Technologies, Bell Laboratories
 *
 * Author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure BurgTool = struct
    val _ = Tools.registerStdShellCmdTool
	{ tool = "ML-Burg",
	  class = "mlburg",
	  suffixes = ["burg"],
	  cmdStdPath = "ml-burg",
	  extensionStyle = Tools.REPLACE (["burg"], [("sml", SOME "sml")]) }
end
