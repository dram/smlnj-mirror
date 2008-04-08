let
    (* echointoout is basically just a redirecter from input back to output until pipes close *)
    val proc = Windows.execute("C:\\temp\\EchoInToOut\\EchoInToOut\\bin\\Debug\\EchoInToOut.exe", "");
    val textout = Windows.textOutstreamOf proc;
    val textin = Windows.textInstreamOf proc;
in
    (proc, textout, textin)
end;
val (a,b,c) = it;

TextIO.output(b,"asdf\n");
TextIO.flushOut(b);

TextIO.input(c);

Windows.reap a;
