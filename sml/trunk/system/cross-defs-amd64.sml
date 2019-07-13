let val s = CMB.symval
in
   #set (s "ARCH_X86") NONE;
   #set (s "ARCH_AMD64") (SOME 1);
   #set (s "TARGET_SIZE_64") (SOME 1);
   #set (s "TARGET_LITTLE_ENDIAN") (SOME 1)
end
