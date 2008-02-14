(* note that for this to work, you must already have a running copy of IE open *)
val conversation = Windows.DDE.startDialog("iexplore", "WWW_OpenURL");
Windows.DDE.executeString(conversation, "http://www.microsoft.com/", 0, Time.zeroTime);
Windows.DDE.stopDialog(conversation);
