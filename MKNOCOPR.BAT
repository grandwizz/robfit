path c:\msfort\bin;c:\watfor;c:\dos
FORMAT A: /V:NOCOPROEXE
COPY C:\ROBFIT\RUNABLE.BAT A:
SET LIB=C:\MSFORT\LIB;C:\WATFOR\WATFOR;C:\WATFOR\WGKS
set tmp=C:\msfort\tmp
set include=C:\msfort\include
ECHO ON
copy chared.tbj chared.obj
ECHO IN FOR
FL /FPa /Gt2048 /Ge /Od /c menurd.for CMS.FOR TCHREAD.FOR
FL /FPa /Gt2048  /Ge /W0 fspfit.FOR VRLINK.FOR bread.for pfit.for bsmsq.for MAOPEN.FOR menurd macbkg.for CMS TCHREAD
FL /FPa /Gt2048  /Ge /W0 stgen.FOR SHLINK.FOR arbin.for bsmsq menurd CMS TCHREAD chared bread MAOPEN
FL /FPa /Gt2048  /Ge /W0 bkgfit.FOR BKLINK.FOR bread bsmsq menurd macbkg MAOPEN CMS TCHREAD
FL /FPa /Gt2048 /Ge /W0 /c /Olt MSPLOTS.FOR
FL /FPa /Gt2048 /Ge /W0 /c FSPDIS.FOR NUMOUT.FOR G3LINK.FOR
LINK FSPDIS BREAD NUMOUT MENURD G3LINK CMS MSPLOTS MAOPEN,,,GRAPHICS LLIBFORA,,
copy fspfit.exe A:
copy stgen.exe A:
copy bkgfit.exe A:
copy fspdis.exe A:
DEL *.EXE
DEL *.OBJ
