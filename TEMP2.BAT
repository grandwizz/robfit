path c:\msfort\bin;c:\watfor;c:\dos
SET LIB=C:\msfort\LIB
set tmp=C:\msfort\tmp
set include=C:\msfort\include
FL /Gt2048 /Ge /Od /c menurd.for
FL /Gt2048 /Ge /W0 /c /Olt MSPLOTS.FOR
LINK FSPDIS BREAD NUMOUT MENURD MSPLOTS MAOPEN G3LINK,,,GRAPHICS LLIBFOR7,,
