@echo off
del bfacs.exe
del bfacslib.dll
del *.chm
del *.dcu
del VCLfix\*.dcu
cd ..\bfacslib
del *.lib
make
cd ..\bfacs
dcc32 bfacs.dpr
upx -9 -v --compress-icons=0 bfacs.exe
upx -9 -v bfacslib.dll
hhc bfacs_US.hhp
hhc bfacs_DE.hhp
