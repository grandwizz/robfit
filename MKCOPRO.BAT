path c:\msfort\bin;c:\watfor;c:\dos
FORMAT A: /V:COPROEXE
COPY C:\ROBFIT\RUNABLE.BAT A:
SET LIB=C:\msfort\LIB
set tmp=C:\msfort\tmp
set include=C:\msfort\include
copy chared.tbj chared.obj
ECHO IN FOR
FL /Gt2048 /Ge /Od /c menurd.for CMS.FOR TCHREAD.FOR
FL /Gt2048  /Ge /W0 fspfit.FOR VRLINK.FOR bread.for pfit.for bsmsq.for MAOPEN.FOR menurd CMS TCHREAD macbkg.for
FL /Gt2048  /Ge /W0 stgen.FOR SHLINK.FOR arbin.for bsmsq menurd CMS chared bread MAOPEN TCHREAD
FL /Gt2048  /Ge /W0 bkgfit.FOR BKLINK.for bread bsmsq menurd CMS macbkg MAOPEN TCHREAD
FL /Gt2048 /Ge /W0 /c /Olt MSPLOTS.FOR
FL /Gt2048 /Ge /W0 /c FSPDIS.FOR NUMOUT.FOR G3LINK.FOR
LINK FSPDIS BREAD NUMOUT G3LINK MENURD CMS MSPLOTS MAOPEN,,,GRAPHICS LLIBFOR7,,
copy *.exe a:
del *.exe
