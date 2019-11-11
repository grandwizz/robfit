        SUBROUTINE STPL(XE,YE)                                          GDP00010
      INTEGER OPTLST(8)/6,1, 8,85,110,0,7,10/                           GDP00020
C CALL STPL(IX,IY) AT THE FIRST POINT TO START PLOTTING.                GDP00030
      COMMON /PL/NC                                                     GDP00040
      IF(NC.NE.0)GOTO 5                                                 GDP00050
      CALL FSINIT                                                       GDP00060
      CALL DSOPEN(3, 4, 'IMG240 ', 8, OPTLST, 1, 'GRAFOUT ')            GDP00070
      CALL DSUSE(1, 3)                                                  GDP00080
      CALL GSWIN(0.0,8.5, 0.0,11.0)                                     GDP00090
      CALL GSSEG(1)                                                     GDP00100
      CALL GSCM(3)                                                      GDP00110
      CALL GSCB(.14,.20)                                                GDP00120
      CALL GSCA(0.0,-1.0)                                               GDP00130
      CALL GSMS(8)                                                      GDP00140
      NC=1                                                              GDP00150
5     IX=1024*XE                                                        GDP00160
      IY=768*YE                                                         GDP00170
      IX=MAX0(0,MIN0(1023,IX))                                          GDP00180
      IY=MAX0(0,MIN0(767,IY))                                           GDP00190
      CALL GSMOVE(.5+IY*.1E-1,10.8-IX*.102539E-1)                       GDP00200
      RETURN                                                            GDP00210
        END                                                             GDP00220
        SUBROUTINE PLOT(XE,YE)                                          GDP00230
C CALL PLOT(IX,IY) TO DRAW A LINE TO IX,IY FROM THE IX1,IY1 IN THE      GDP00240
C LAST CALL TO EITHER PLOT OR STPL                                      GDP00250
      IX=1024*XE                                                        GDP00260
      IY=768*YE                                                         GDP00270
       IX=MAX0(0,MIN0(1023,IX))                                         GDP00280
       IY=MAX0(0,MIN0(767,IY))                                          GDP00290
      CALL GSLINE(.5+IY*.1E-1,10.8-IX*.102539E-1)                       GDP00300
      RETURN                                                            GDP00310
        END                                                             GDP00320
      SUBROUTINE BCHART(XE,YE,CHAR)                                     GDP00330
      CHARACTER*1 CHAR(64),COUT(64)                                     GDP00340
      DO 5 I=1,64                                                       GDP00350
      COUT(I)=CHAR(I)                                                   GDP00360
      IF(CHAR(I).EQ.'\')GOTO 6                                          GDP00370
5     IF(CHAR(I).EQ.'\')GOTO 6                                          GDP00380
      I=64                                                              GDP00390
6     N=I-1                                                             GDP00400
      COUT(N+1)=' '                                                     GDP00410
      IX=1024*XE                                                        GDP00420
      IY=768*YE                                                         GDP00430
      IX=MIN0(1023,MAX0(1,IX))                                          GDP00440
      IY=MIN0(767,MAX0(1,IY))                                           GDP00450
C      WRITE(7,101)IX,IY,N,(COUT(I),I=1,N)                              GDP00460
101   FORMAT(' IX,IY,N,COUT',3I5,(20A1))                                GDP00470
      CALL GSCHAR(.5+IY*.1E-1,10.8-IX*.102539E-1,N+1,COUT)              GDP00480
      NP=N+1                                                            GDP00490
      RETURN                                                            GDP00500
      END                                                               GDP00510
      SUBROUTINE CLEARS                                                 GDP00520
       COMMON /PL/NC                                                    GDP00530
        NC=0                                                            GDP00540
      CALL FSFRCE                                                       GDP00550
      CALL FSTERM                                                       GDP00560
      RETURN                                                            GDP00570
      END                                                               GDP00580
       BLOCK DATA                                                       GDP00590
       COMMON /PL/NC                                                    GDP00600
       DATA NC/0/                                                       GDP00610
       END                                                              GDP00620
      SUBROUTINE ANSI                                                   GDP00630
      RETURN                                                            GDP00640
      END                                                               GDP00650
      SUBROUTINE LINET(CHAR)                                            GDP00660
      CHARACTER*4 CHAR                                                  GDP00670
      CHARACTER*1 ISOL,IDOT,IDOTDA,ISDASH,ILDASH                        GDP00680
      CHARACTER*4 LTYPE(5)                                              GDP00690
      DIMENSION IBML(5)                                                 GDP00700
      DATA LTYPE/'SOLI','DOT ','DODA','SDAS','LDAS'/                    GDP00710
      DATA IBML/0,2,3,5,6/                                              GDP00720
      CALL STPL(0.,0.)                                                  GDP00730
      DO 10 I=1,5                                                       GDP00740
      IF(CHAR.EQ.LTYPE(I))GOTO 15                                       GDP00750
10    CONTINUE                                                          GDP00760
      I=1                                                               GDP00770
15    CALL GSLT(IBML(I))                                                GDP00780
       WRITE(6,*)IBML(I)                                                GDP00790
      RETURN                                                            GDP00800
      END                                                               GDP00810
      SUBROUTINE CMS(CHAR,IRT)                                          GDP00820
      CHARACTER*64 CHAR                                                 GDP00830
      IRT=0                                                             GDP00840
      RETURN                                                            GDP00850
      END                                                               GDP00860
        SUBROUTINE PONT(XE,YE)                                          GDP00870
C TO PLOT A POINT AT XE,YE AFTER A CALL HAS AT SOMETIME BEEN MADE TO    GDP00880
C STPL SIMPLY CALL PONT(IX,IY).  THE POINT MAY NOT BE ACTUALLY SEEN     GDP00890
C UNTIL THE SET OF PLOT CALLS ARE ENDED WITH A LAST CALL TO STPL        GDP00900
        COMMON/OR/IDUM(18),ILP,ICP(16)                                  GDP00910
        IX=1024*XE                                                      GDP00920
        IY=768*YE                                                       GDP00930
        IX=MAX0(0,MIN0(1023,IX))                                        GDP00940
        IY=MAX0(0,MIN0(767,IY))                                         GDP00950
      CALL GSMARK(.5+IY*.1E-1,10.8-IX*.102539E-1)                       GDP00960
        RETURN                                                          GDP00970
        END                                                             GDP00980
      SUBROUTINE CHSIZE(I)                                              GDP00990
      DIMENSION ISIZE(4)                                                GDP01000
       CALL GSCB(.07,.10)                                               GDP01010
       RETURN                                                           GDP01020
       END                                                              GDP01030
        SUBROUTINE CHREAD(IUNIT,IMAX,RANS,CANS,NR,NC,IEND)              GDP01040
        DIMENSION RANS(1)                                               GDP01050
        CHARACTER*1 CANS(1),C(140)                                      GDP01060
        IEND=0                                                          GDP01070
           IF(IUNIT.EQ.9)THEN                                           GDP01080
           DO I = 1,NR                                                  GDP01090
           RANS(I)=0                                                    GDP01100
           ENDDO                                                        GDP01110
           DO I= 1,NC                                                   GDP01120
           CANS(I)=' '                                                  GDP01130
           ENDDO                                                        GDP01140
C          PRINT*,' IN CHREAD'                                          GDP01150
C          READ(*,*)ITEST                                               GDP01160
        READ(*,'(140A1)',END=60)(C(I),I=1,IMAX)                         GDP01170
           ELSE                                                         GDP01180
        READ(IUNIT,'(140A1)',END=60)(C(I),I=1,IMAX)                     GDP01190
           ENDIF                                                        GDP01200
        IP=1                                                            GDP01210
        DO 10 I=1,NR                                                    GDP01220
        RANS(I)=RNUMB(C,IP,IMAX)                                        GDP01230
10      CONTINUE                                                        GDP01240
        DO 20 I=1,NC                                                    GDP01250
20      CANS(I)=' '                                                     GDP01260
30      IF(IP.GT.IMAX)RETURN                                            GDP01270
        IF(C(IP).NE.' '.AND.C(IP).NE.',')GOTO 40                        GDP01280
        IP=IP+1                                                         GDP01290
        GOTO 30                                                         GDP01300
40      NU=MIN0(NC,IMAX+1-IP)                                           GDP01310
        DO 50 I=1,NU                                                    GDP01320
50      CANS(I)=C(I+IP-1)                                               GDP01330
        RETURN                                                          GDP01340
60      CONTINUE                                                        GDP01350
        IF(IUNIT.EQ.9)REWIND(5)                                         GDP01360
        IEND=1                                                          GDP01370
        END                                                             GDP01380
