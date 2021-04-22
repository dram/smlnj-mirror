(* test.sml *)

local
  structure I = InterpFn(StringStream);
in 
  fun run_string (str: string) = I.run(StringStream.mkInstream str);
end;

local
  structure I = InterpFn(FileStream);
in 
  fun run_file (filename: string) = I.run(FileStream.mkInstream filename);
end;

local
  structure I = InterpFn(InteractStream);
in 
  fun run_interact () = I.run(InteractStream.mkInstream());
end;



