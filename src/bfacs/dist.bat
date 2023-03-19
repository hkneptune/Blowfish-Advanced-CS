@echo off
call buildall.bat
set INSTALL_ZIP=%TEMP%\bfacs%1.zip
del %INSTALL_ZIP%
zip -9 -j %INSTALL_ZIP% bfacs.exe bfacslib.dll bfacs_US.chm bfacs_US.sr bfacs_DE.chm bfacs_DE.sr readme.txt liesmich.txt changes.txt
