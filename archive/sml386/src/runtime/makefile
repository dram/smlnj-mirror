OBJS =  sml.obj run.obj run_ml.obj callgc.obj gc.obj prim.obj\
        timers.obj ml_objec.obj cfuns.obj intrface.obj\
        util.obj moveback.obj

#ml_state.h ==>
#ml_types.h ==> ml_state.h tags.h sml.h
#tags.h     ==>
#request.h  ==>
#cause.h    ==>
#ml_os.h    ==> time.h

sml.exe :  $(OBJS) sml.res sml.def
    link /BATCH/NOD/CO/m @<<
         sml+
         run+
         run_ml+
         callgc+
         timers+
         ml_objec+
         cfuns+
         gc+
         util+
         intrface+
         prim+
         moveback
         sml.exe
         sml.map
         slibcew+libw+winmem32
         sml.def
<<
    rc sml.res

sml.res :
    rc -r sml.rc

sml.obj : sml.c sml.h ml_types.h
    cl -BATCH -c -G2 -Gw sml.c

run.obj : run.c sml.h ml_state.h ml_types.h tags.h
    cl -BATCH -c -G2 -Gw run.c

run_ml.obj : run_ml.c sml.h ml_state.h ml_types.h tags.h request.h cause.h
    cl -BATCH -c -G2 -Gw run_ml.c

callgc.obj : callgc.c sml.h ml_os.h ml_state.h ml_types.h tags.h cause.h request.h
    cl -BATCH -c -G2 -Gw callgc.c

cfuns.obj : cfuns.c sml.h ml_os.h ml_state.h ml_types.h cause.h tags.h
    cl -BATCH -c -G2 -Gw cfuns.c

gc.obj : gc.asm tags.inc
    masm /MX gc,,,,

util.obj : util.asm tags.inc
    masm /MX util,,,,

intrface.obj : intrface.asm tags.inc
    masm /MX intrface,,,,

moveback.obj : moveback.asm tags.inc
    masm /MX moveback,,,,

timers.obj : timers.c sml.h ml_os.h ml_types.h ml_state.h tags.h 
    cl -BATCH -c -G2 -Gw timers.c

ml_objec.obj : ml_objec.c sml.h ml_state.h ml_types.h tags.h
    cl -BATCH -c -G2 -Gw ml_objec.c

prim.obj: prim.asm tags.inc request.inc
    masm /MX prim,,,,

