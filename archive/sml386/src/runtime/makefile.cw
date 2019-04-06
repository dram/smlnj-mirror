OBJS =  sml.obj run.obj run_ml.obj callgc.obj gc.obj prim.obj\
        export.obj timers.obj ml_objec.obj cfuns.obj intrface.obj\
        util.obj moveback.obj

#ml_state.h ==>
#ml_types.h ==> ml_state tags sml
#tags.h     ==>
#request.h  ==>
#cause.h    ==>
#ml_os.h    ==> time

sml.exe :  $(OBJS) sml.res sml.def
    link /BATCH/NOD/CO/m @<<
         sml+
         run+
         run_ml+
         callgc+
         export+
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

sml.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi sml.c

run.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi run.c

run_ml.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi run_ml.c

callgc.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi callgc.c

cfuns.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi cfuns.c

gc.obj : tags.inc
    masm /MX /ZI gc,,,,

util.obj : tags.inc
    masm /MX /ZI util,,,,

intrface.obj : tags.inc
    masm /MX /ZI intrface,,,,

moveback.obj : tags.inc
    masm /MX /ZI moveback,,,,

timers.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi timers.c

ml_objec.obj :
    cl -BATCH -c -Fc -Od -G2 -Gw -Zi ml_objec.c

prim.obj:
    masm /MX /ZI prim,,,,

export.obj :
     cl -BATCH -c -Fc -Od -G2 -Gw -Zi export.c
