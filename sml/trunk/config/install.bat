REM begin by creating rudimentary directory hierarchy
mkdir bin
mkdir bin\.run
mkdir bin\.heap
mkdir lib

REM compile runtime system and move executable to bin\.run
cd src\runtime\objs
nmake -f mk.x86-win32
move run.x86-win32.exe ..\..\..\bin\.run
cd ..\..\..

REM put helper .bat scripts into bin
copy config\link-sml.bat bin
copy config\ml-build.bat bin
copy config\sml.bat bin
copy config\ml-lex.bat bin
copy config\ml-yacc.bat bin

REM create heap image and lib hierarchy ("boot")
copy config\preloads preloads.standard
cd sml.boot.x86-win32
..\bin\.run\run.x86-win32 @SMLboot=BOOTLIST @SMLheap=sml @SMLalloc=1M @SMLverbose
cd ..
move sml.x86-win32 bin\.heap
del preloads.standard
cd sml.boot.x86-win32
for /D %%a in (*.*) do echo %%a %%a >>..\lib\pathconfig
for /D %%a in (*.*) do move %%a ..\lib
cd ..

REM compile and install ml-lex
cd src\ml-lex
%COMSPEC% /C "..\..\bin\ml-build ml-lex.cm ExportLexGen.lexGen ml-lex"
move ml-lex.x86-win32 ..\..\bin\.heap
cd ..\..
echo ml-lex ..\bin >>lib\pathconfig

REM compile and install ml-yacc
cd src\ml-yacc\src
%COMSPEC% /C "..\..\..\bin\ml-build ml-yacc.cm ExportParseGen.parseGen ml-yacc"
move ml-yacc.x86-win32 ..\..\..\bin\.heap
cd ..\..\..
echo ml-yacc ..\bin >>lib\pathconfig
