path c:\msfort\bin;c:\watfor;c:\dos
set library=c:\watfor\watfor;c:\watfor\wgks
SET LIB=C:\MSFORT\LIB;C:\WATFOR\WATFOR;C:\WATFOR\WGKS
set tmp=C:\msfort\tmp
set include=C:\msfort\include
FL /Gt2048  /Ge /W0 bkgfit.FOR  bsmsq.FOR menurd.FOR macbkg.FOR bread.FOR
COPY BKGFIT.EXE ..\RUNABLE
