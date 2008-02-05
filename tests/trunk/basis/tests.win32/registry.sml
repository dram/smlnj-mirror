val key = Windows.Reg.openKeyEx(Windows.Reg.currentUser, "Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings", Windows.Key.allAccess);

fun enumerate (x : (Windows.Reg.hkey * int) -> string option, key : Windows.Reg.hkey, startValue : int) =
    case x (key, startValue) of
	SOME str => (print str; enumerate (x, key, startValue+1))
      | NONE => 1;

enumerate (Windows.Reg.enumKeyEx, key, 0);
enumerate (Windows.Reg.enumValueEx, key, 0);

Windows.Reg.closeKey key;

val softwareKey = Windows.Reg.openKeyEx(Windows.Reg.currentUser, "Software", Windows.Key.execute);
Windows.Reg.createKeyEx(softwareKey, "SMLNJ", Windows.Key.allAccess);
Windows.Reg.closeKey softwareKey;

val newKey = Windows.Reg.openKeyEx(Windows.Reg.currentUser, "Software\\SMLNJ", Windows.Key.allAccess);
Windows.Reg.setValueEx (newKey, "dword", Windows.Reg.DWORD(0w1));
val dwvalue = Windows.Reg.queryValueEx (newKey, "dword");

Windows.Reg.setValueEx (newKey, "string", Windows.Reg.SZ("stringo"));
val strvalue = Windows.Reg.queryValueEx (newKey, "string");

Windows.Reg.setValueEx (newKey, "expstring", Windows.Reg.EXPAND_SZ("expand%HOME%"));
val estrvalue = Windows.Reg.queryValueEx (newKey, "expstring");

Windows.Reg.setValueEx (newKey, "multistring", Windows.Reg.MULTI_SZ(["multi", "string"]));
val mstrvalue = Windows.Reg.queryValueEx (newKey, "multistring");

Windows.Reg.setValueEx (newKey, "multistring2", Windows.Reg.MULTI_SZ(["multi"]));
val mstrvalue2 = Windows.Reg.queryValueEx (newKey, "multistring2");

Windows.Reg.setValueEx (newKey, "binary", Windows.Reg.BINARY(Word8Vector.fromList([0w1, 0w47])));
val binvalue = Windows.Reg.queryValueEx (newKey, "binary");

Windows.Reg.deleteValue(newKey, "dword");
Windows.Reg.closeKey newKey;
Windows.Reg.deleteKey(Windows.Reg.currentUser, "Software\\SMLNJ");
