        BLOCK DATA
C NC=1 ROUTINE IS IN GRAPHICS MODE, NC=0  NOT IN GRAPHICS MODE
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
         ILP=0
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
      CHARACTER*64 VALU,CHAR
      COMMON/OR/NC
      IF(NC.EQ.0)CALL STPL(0.,0.)
        IT=INDEX(CHAR,'\')
        IF(IT.EQ.0)IT=65
        VALU=CHAR(1:IT-1)
        XC=AMIN1(XE,.90)
        CALL GTX(XC,YE,VALU)
      RETURN
      END
      SUBROUTINE CLEARS
        INTEGER*2 REGS(10)
        REGS(1)=3
        CALL INTR(16,REGS)
      RETURN
      END
        SUBROUTINE ANSI
        COMMON/OR/NC,IL,XC(255),YC(255),ILP,XCP(255),YCP(255)
        INTEGER*2 REGS(10)
        INTEGER WKID/1/
        NC=0
        IL=0
        ILP=0
        REGS(1)=3
        CALL GDAWK(WKID)
        CALL GCLWK(WKID)
        CALL GCLKS
        CALL INTR(16,REGS)
        RETURN
        END
      SUBROUTINE COLOR(CHAR)
      CHARACTER*64 CHAR
      IF(CHAR(1:3).EQ.'RED')CALL GSPLCI(2)
      IF(CHAR(1:5).EQ.'WHITE')CALL GSPLCI(1)
         IF(CHAR(1:4).EQ.'BLUE')CALL GSPLCI(3)
         IF(CHAR(1:5).EQ.'GREEN')CALL GSPLCI(4)
         RETURN
         END
        SUBROUTINE TEXTMOD
        INTEGER*2 REGS(10)
        INTEGER WKID/1/
        REGS(1)=3
        CALL GDAWK(WKID)
        CALL GCLWK(WKID)
        CALL GCLKS
        CALL INTR(16,REGS)
        RETURN
          END
      SUBROUTINE LINET(CHAR)
      CHARACTER*4 CHAR
        CHARACTER*64 CLOR
      CHARACTER*1 ISOL,IDOT,IDOTDA,ISDASH,ILDASH
C       INTEGER*4 FORECOLOR,BACKCOLOR,COLORBIT,COLOR
C       parameter (FORECOLOR=Z'86210000',BACKCOLOR=Z'86310000',
C    +             COLORBIT=Z'86408000')
      DATA ISOL/'\'/,IDOT/'a'/,IDOTDA/'b'/,ISDASH/'c'/,ILDASH/'d'/
      CALL STPL(0.,0.0)
        CLOR='WHITE'
      IF(CHAR.EQ.'DOT ')CLOR='RED'
      IF(CHAR.EQ.'DODA')CLOR='BLUE'
      IF(CHAR.EQ.'SDAS')CLOR='GREEN'
      IF(CHAR.EQ.'LDAS')CLOR='WHITE'
C       CALL TOOLBX(FORECOLOR,COLOR)
          CALL COLOR(CLOR)
      RETURN
        END
        SUBROUTINE GPHMOD
        COMMON/OR/NC
        INTEGER ERRFID/0/,SCRRES/3/,WKID/1/
        NC=1
        CALL GIVGA18(SCRRES)
        CALL GOPKS(ERRFID)
        CALL GOPWK(WKID,ERRFID,SCRRES)
        CALL GACWK(WKID)
        CALL GSCHH(0.02)
c Set colour
c
        CALL GSCR(WKID,1,1.0,1.0,1.0)
        CALL GSCR(WKID,2,1.0,0.0,0.0)
        CALL GSCR(WKID,3,0.0,0.0,1.0)
        CALL GSCR(WKID,4,0.0,1.0,0.0)
        CALL GSTXCI(1)
c set marker
c
        CALL GSMK(1)
c
C *** OUT OF CHARLIES
        CALL GSPMCI(15)
        CALL GSPLCI(15)
        CALL GSTXCI(15)
C ROBFIT graphics initialization
C     SUBROUTINE RSETG
C       INTEGER WKID,SCRES
C       COMMON/RG1/WKID,SCRES
c
c set screen type
c
C       WKID=1
C       SCRES=18
c
c init GKS
c
C       CALL GECLKS
C       CALL GOPKS(0)
C       CALL GIVGA18(SCRES)
C       CALL GOPWK(WKID,0,SCRES)
C       CALL GACWK(WKID)
c
c Set character height
c
C       CALL GSCHH(10./500.)
c
c
C       RETURN
C     END
C RSETG ABOVE
        RETURN
        END
        SUBROUTINE PRTSCN
        INTEGER*2 REGS(10)
        CALL INTR(5,REGS)
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
