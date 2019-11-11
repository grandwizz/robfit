        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FI'
        BLOCK DATA
C NC>0 ROUTINE IS IN GRAPHICS MODE, NC=0  NOT IN GRAPHICS MODE
C NC=1 FOR MONO, NC=2 FOR COLOR
      COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
        DATA NC,IL,ILP/3*0/
        END
        SUBROUTINE STPL(XE,YE)
C CALL STPL(XE,YE) AT THE FIRST POINT TO START PLOTTING.  CALL STPL
C WITH ANY NEGATIVE X,Y TO END A PLOT
C THE VECTOR IC CONTAINS IL ENDS OF LINES DRAWN FROM IBP TO IC(I)
C THE VECTOR ICP CONTAINS ILP LOCATIONS OF POINTS TO BE PLOTTED
      COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
      IF(NC.NE.0)GOTO 5
      IL=0
      ILP=0
      NC=1
      CALL GPHMOD
5       CONTINUE
C *** THE IF LOOP OUTPUTS LINE ENDS STORED IN XC,YC
         IF(IL.GT.1)THEN
         CALL GPL(IL,XC,YC)
         IL=0
         ENDIF
C *** THE IF LOOP OUTPUTS POINTS/LINE ENDS STORED IN XCP,YCP
         IF(ILP.GE.1)THEN
         CALL GPM(ILP,XCP,YCP)
         ILP = 0
         ENDIF
      IF(XE.LT.0.)THEN
          CALL ANSI
          RETURN
      ENDIF
C *** SETS BEGINNING POINT FOR PLOTTING
         IL=1
         XC(1)=XE
         YC(1)=YE
        RETURN
        END
        SUBROUTINE PLOT(XE,YE)
C CALL PLOT(XE,YE) TO DRAW A LINE TO XE,YE FROM THE XE,YE IN THE
C LAST CALL TO EITHER PLOT OR STPL
      COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
        IL=IL+1
        XC(IL)=XE
        YC(IL)=YE
        IF(IL.EQ.255)THEN
          CALL GPL(IL,XC,YC)
          XC(1)=XC(IL)
          YC(1)=YC(IL)
          IL=1
        ENDIF
      RETURN
        END
        SUBROUTINE PONT(XE,YE)
C TO PLOT A POINT AT IXE,IYE AFTER A CALL HAS AT SOMETIME BEEN MADE
C to STPL SIMPLY CALL PONT(IXE,IYE).  THE POINT MAY NOT BE ACTUALLY
C seen UNTIL THE SET OF PLOT CALLS ARE ENDED WITH A LAST CALL TO STPL
      COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
        IF(NC.EQ.0)CALL STPL(0.,0.)
        ILP=ILP+1
        XCP(ILP)=XE
        YCP(ILP)=YE
        IF(ILP.EQ.255)THEN
          CALL GPM(ILP,XCP,YCP)
          ILP=0
        ENDIF
        RETURN
        END
      SUBROUTINE BCHART(XE,YE,CHAR)
      CHARACTER*64 CHAR
      CHARACTER* ( 64 ) VALU
      COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
      IF(NC.EQ.0) CALL STPL(0.,0.)
        IT=INDEX(CHAR,'\')
        IF(IT.EQ.0)IT=65
        VALU=CHAR(1:IT-1)
        XC1=AMIN1(XE,.90)
        CALL GTX(XC1,YE,VALU)
      RETURN
      END
      SUBROUTINE CLEARS
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 REGS(10)
        CALL clearscreen( $GCLEARSCREEN )
C       REGS(1)=3
C       CALL INTR(16,REGS)
      RETURN
      END
        SUBROUTINE ANSI
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 REGS(10), dummy
        COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
        NC=0
        IL=0
        ILP=0
        dummy = setvideomode( $DEFAULTMODE )
        REGS(1)=3
C       CALL INTR(16,REGS)
        RETURN
        END
      SUBROUTINE COLOR(CHAR)
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 dummy,IC
        CHARACTER*64 CHAR
        RECORD /videoconfig/ myscreen
        CALL getvideoconfig( myscreen )
        IF( myscreen.monitor .EQ. $MONO ) RETURN
        IF(CHAR(1:3).EQ.'RED')IC=4
        IF(CHAR(1:5).EQ.'WHITE')IC=7
        IF(CHAR(1:4).EQ.'BLUE')IC=9
        IF(CHAR(1:5).EQ.'GREEN')IC=10
        IF(CHAR(1:6).EQ.'BWHITE')IC=15
        DUMMY=SETCOLOR(IC)
        RETURN
        END
        SUBROUTINE TEXTMOD
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 REGS(10), dummy
        INTEGER WKID/1/
        dummy = setvideomode( $DEFAULTMODE )
        REGS(1)=3
c       CALL INTR(16,REGS)
        RETURN
          END
      SUBROUTINE LINET(CHAR)
      COMMON/CGPH/NCOLOR
      CHARACTER*64 CLOR
      INTEGER*2 IC
      CALL STPL(0.,0.0)
      IF(NCOLOR.EQ.0)THEN
        CLOR='WHITE'
        IF(CHAR.EQ.'DOT ')CLOR='RED'
        IF(CHAR.EQ.'DODA')CLOR='BLUE'
        IF(CHAR.EQ.'SDAS')CLOR='GREEN'
        IF(CHAR.EQ.'LDAS')CLOR='BWHITE'
        CALL COLOR(CLOR)
        RETURN
      ELSE
        IC=#FFFF
        IF(CHAR.EQ.'DOT ')IC=#AAAA
        IF(CHAR.EQ.'DODA')IC=#AAFF
C       IF(CHAR.EQ.'SDAS')IC=#F0F0
        IF(CHAR.EQ.'LDAS')IC=#FF00
        CALL SETLINESTYLE(IC)
        RETURN
      ENDIF
        END
        SUBROUTINE GPHMOD
      COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
      COMMON/CGPH/NCOLOR
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 dummy, maxx, maxy, maxcol, maxrow
        RECORD /videoconfig/ myscreen
        COMMON   maxx, maxy, maxcol, maxrow
        NC=1
        PRINT*,' NCOLOR=',NCOLOR
C
C       Microsoft FORTRAN Advanced Topics, p. 154
C       Find graphics mode.
C
        CALL getvideoconfig( myscreen )
        IF( myscreen.monitor .EQ. $MONO ) THEN
           NCOLOR=1
           SELECT CASE ( myscreen.adapter )
             CASE( $CGA )
               dummy = setvideomode( $HRESBW )
             CASE( $EGA, $OEGA )
               dummy = setvideomode( $ERESNOCOLOR )
             CASE( $VGA, $OVGA, $MCGA )
               dummy = setvideomode( $HERCMONO )
             CASE DEFAULT
               dummy = 0
           END SELECT
        ELSE
           SELECT CASE ( myscreen.adapter )
             CASE( $CGA )
               dummy = setvideomode( $MRES4COLOR )
             CASE( $OCGA )
               dummy = setvideomode( $ORESCOLOR )
             CASE( $EGA, $OEGA )
               dummy = setvideomode( $ERESCOLOR )
             CASE( $VGA, $OVGA )
               dummy = setvideomode( $VRES16COLOR )
             CASE( $MCGA )
               print*,' case mcga 256 color'
               read(*,*)itest
               dummy = setvideomode( $MRES256COLOR )
             CASE DEFAULT
               dummy = 0
           END SELECT
        ENDIF
        IF( dummy .EQ. 0) STOP 'Error: cannot set graphics mode'
C
C       Determine the minimum and maximum dimensions.
C
        CALL getvideoconfig( myscreen )
        maxx = myscreen.numxpixels
        maxy = myscreen.numypixels - 10
        maxcol = myscreen.numtextcols
        maxrow = myscreen.numtextrows
        RETURN
        END
        SUBROUTINE GPM(ILP, XCP, YCP)
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 dummy, newx, newy, locx, locy, maxx, maxy
        INTEGER*2 maxcol, maxrow
        DIMENSION XCP(255), YCP(255)
        COMMON  maxx, maxy, maxcol, maxrow
        DO 10 I=1,ILP
           X = maxx*XCP(I)
           Y = maxy*YCP(I)
           locx = X
           locy = maxy - Y
           dummy = setpixel( locx, locy)
10      CONTINUE
        RETURN
        END
        SUBROUTINE GPL(IL, XC, YC)
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        INTEGER*2 newx, newy, maxx, maxy, maxcol, maxrow, dummy
        DIMENSION XC(IL), YC(IL)
        RECORD /xycoord/ xy
        COMMON  maxx, maxy, maxcol, maxrow
        X = maxx*XC(1)
        Y = maxy*YC(1)
        newx = X
        newy = maxy - Y
        CALL moveto( newx,newy, xy)
        DO 10 I=2,IL
           X = maxx*XC(I)
           Y = maxy*YC(I)
           newx = X
           newy = maxy - Y
           dummy = lineto( newx, newy)
           CALL moveto( newx, newy, xy)
10      CONTINUE
        RETURN
        END
        SUBROUTINE GTX(XC, YE, VALU)
        INCLUDE 'C:\MSFORT\INCLUDE\FGRAPH.FD'
        CHARACTER*64 VALU,TEMP
        CHARACTER*1 TEXT(64)
        INTEGER*2 maxx, maxy, maxcol, maxrow, newx, newy
        INTEGER*2 dummy, icolor
        RECORD /rccoord/ s
        COMMON   maxx, maxy, maxcol, maxrow
        EQUIVALENCE (TEXT(1),TEMP(1:1))
        TEMP = VALU
        X = maxcol*XC
        Y = maxrow*YE
        newx = X
        newy = maxrow - Y
        DO 10 I=1,64
        IF(TEXT(I) .NE. ' ') THEN
        CALL settextposition( newy, newx, s)
        CALL outtext( TEXT(I) )
        ENDIF
        newx = newx + 1
10      CONTINUE
        CALL settextposition( 0, 0, s)
        RETURN
        END
        SUBROUTINE PRTSCN
        INTEGER*2 REGS(10)
C       CALL INTR(5,REGS)
        RETURN
        END
        SUBROUTINE INTR(I,REGS)
        INTEGER*2 REGS(10)
        WRITE(6,*) ' YOU ARE IN INTR'
        RETURN
        END
        SUBROUTINE CHREAD(IUNIT,IMAX,RANS,CANS,NR,NC,IEND)              TCH00010
        DIMENSION RANS(1)                                               TCH00020
        CHARACTER*1 CANS(1),C(140)                                      TCH00030
        IEND=0                                                          TCH00040
           IF(IUNIT.EQ.9)THEN                                           TCH00050
           DO I = 1,NR                                                  TCH00060
           RANS(I)=0                                                    TCH00070
           ENDDO                                                        TCH00080
           DO I= 1,NC                                                   TCH00090
           CANS(I)=' '                                                  TCH00100
           ENDDO                                                        TCH00110
C          PRINT*,' IN CHREAD'                                          TCH00120
C          READ(*,*)ITEST                                               TCH00130
        READ(*,'(140A1)',END=60)(C(I),I=1,IMAX)                         TCH00140
           ELSE                                                         TCH00150
        READ(IUNIT,'(140A1)',END=60)(C(I),I=1,IMAX)                     TCH00160
           ENDIF                                                        TCH00170
        IP=1                                                            TCH00180
        DO 10 I=1,NR                                                    TCH00190
        RANS(I)=RNUMB(C,IP,IMAX)                                        TCH00200
10      CONTINUE                                                        TCH00210
        DO 20 I=1,NC                                                    TCH00220
20      CANS(I)=' '                                                     TCH00230
30      IF(IP.GT.IMAX)RETURN                                            TCH00240
        IF(C(IP).NE.' '.AND.C(IP).NE.',')GOTO 40                        TCH00250
        IP=IP+1                                                         TCH00260
        GOTO 30                                                         TCH00270
40      NU=MIN0(NC,IMAX+1-IP)                                           TCH00280
        DO 50 I=1,NU                                                    TCH00290
50      CANS(I)=C(I+IP-1)                                               TCH00300
        RETURN                                                          TCH00310
60      CONTINUE                                                        TCH00320
        IF(IUNIT.EQ.9)REWIND(5)                                         TCH00330
        IEND=1                                                          TCH00340
        END                                                             TCH00350
