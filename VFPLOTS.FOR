        FUNCTION DLF(XI,NDIG,IXT,NDP)
        XIT=AMAX1(1.E-35,XI)
        AEXP=ALOG10(XIT)
        IF(AEXP.LT.0.)AEXP=AEXP-1
        NEXP=AEXP+1
        IXT=XIT*10.**(NDIG-NEXP)+.5
        DLF=IXT*10.**(NEXP-NDIG)
        NDP=1
        IF(NEXP-NDIG.GE.0.AND.NEXP-NDIG.LE.3)NDP=0
        IF(NEXP-NDIG.LE.-1)NDP=2
        RETURN
        END
        FUNCTION IBN(YMIN,YMAX,NMIN,NMAX)
C *** IBN IS THE BEST NUMBER OF TIC MARKS FOR THE DESIRED INTERVAL
        DIFF=DLF(YMAX-YMIN,2,IXT,NEXP)
        IRB=1000000
        DO 20 I=NMIN,NMAX
        IREM=IXT-I*(IXT/I)
        IF(IRB.LT.IREM)GOTO 20
        IBN=I
        IF(IREM.EQ.0)RETURN
        IRB=IREM
20      CONTINUE
        RETURN
        END
        SUBROUTINE AXIS(VMIN,VMAX,LFLAG,NRES,HMIN,HMAX,II,CHI,LABEL)
        CHARACTER*8 II
        CHARACTER*64 TITLE,LABEL
        COMMON/AXISP/BHORI,BVERT,SHORI,SVERT,EHORI,EVERT
        DATA T1/0./
        BHORI=50
        BVERT=40
        EVERT= 420
        EHORI=550
C       PRINT*,' ENTER EVERT, EHORI'
C       READ(*,*)T1,T2
        IF(T1.EQ.0.)GOTO 15
        EVERT=T1
        EHORI=T2
        PRINT*,' ENTER BVERT, BHORI'
        READ(*,*)T1,T2
        IF(T1.EQ.0.)GOTO 15
        BVERT=T1
        BHORI=T2
15      CONTINUE
        SVERT=(EVERT-79-BVERT)/(VMAX-VMIN)
        SHORI=(EHORI-BHORI-9)/(HMAX-HMIN)
        TITLE=II//' CHIS=\'
        I2=EVERT-34
        I1=EHORI-349
        ADEC=VMAX-VMIN
        NPY=AMAX1(0.,3.-ALOG10(ADEC))
        ADEC=HMAX-HMIN
        NPX=AMAX1(0.,3.-ALOG10(ADEC))
        CALL BCHART(I1,I2,TITLE)
        CALL NUMOUT(I1+150,I2,CHI,1,0)
        I1=BHORI
        I2=EVERT
        CALL STPL(I1,I2)
        I2=BVERT
        CALL PLOT(I1,I2)
        I1=EHORI
        CALL PLOT( I1,I2)
        T1=.25*(EVERT-79-BVERT)
        I1=BHORI
        I2=I1+10
        DO 10 I=1,5
        IVERT=BVERT+T1*(I-1)
        IF(I.EQ.1)GOTO 8
        IF(NRES.EQ.1.AND.I.EQ.5)GOTO 8
        CALL STPL(I1,IVERT)
        CALL PLOT(I2,IVERT)
8       RP=VMIN+(VMAX-VMIN)*.25*(I-1)
        IF(LFLAG.EQ.1)RP=EXP(VMIN+.25*(I-1)*(VMAX-VMIN))
10      CALL NUMOUT(I1-10,IVERT-5 ,RP,NPY,1)
        I1=BHORI-70
        I2=.5*(BVERT+EVERT)
        CALL BCHART(I1,I2,'COUNTS\')
        IF(NRES.NE.1)GOTO 16
        I1=BHORI
        I2=EVERT-19
        CALL STPL(I1,I2)
        I3=EHORI
        CALL PLOT(I3,I2)
        I2=I2-30
        CALL STPL(I1,I2)
        CALL PLOT(I3,I2)
        I2=I2-30
        CALL STPL(I1,I2)
        CALL PLOT(I3,I2)
16      T1=(EHORI-BHORI-9)*.1
        I1=BVERT
        I2=I1+10
        DO 20 J=1,11
        IXM=BHORI+T1*(J-1)
        IF(J.EQ.1)GOTO 18
        CALL STPL(IXM,I1)
        CALL PLOT(IXM,I2)
18      RPX=HMIN+(J-1)*(HMAX-HMIN)/10.
20      CALL NUMOUT(IXM,I1-20,RPX,NPX,2)
        I1=.5*(BHORI+EHORI-70)
        I2=BVERT-32
        TITLE=LABEL(1:14)//'\'
        CALL BCHART(I1,I2,TITLE)
        RETURN
        END
        SUBROUTINE PLINTERP(FORIG,XORIG,FINT,XINT,IB,IEX)
        DIMENSION FORIG(4),XORIG(4),FINT(4),XINT(4)
        DX=(XORIG(IB+1)-XORIG(IB))/(IEX+1)
        DO 100 IP=1,IEX
        XINT(IP)=XORIG(IB)+IP*DX
        FINT(IP)=0
        DO 90 IA=1,4
        A1=1
        DO 80 J1=1,4
        IF(IA.EQ.J1)GOTO 80
        A1=A1*(XINT(IP)-XORIG(J1))/(XORIG(IA)-XORIG(J1))
80      CONTINUE
90      FINT(IP)=FINT(IP)+A1*FORIG(IA)
100     CONTINUE
        RETURN
        END
        SUBROUTINE NUMOUT(XE,YE,R,NMIN,NADJ)
C *** NADJ = 0 FOR RIGHT OF IX, 1 FOR LEFT OF IX, 2 TO CENTER ON IX
        CHARACTER CNUMOU*64,C(64)*1
        EQUIVALENCE (CNUMOU,C(1))
        CNUMOU=' '
        CALL RTOC(R,C)
5       IF(C(8-NMIN).NE.' ')GOTO 20
        DO 10 I=1,NMIN
10      C(7-NMIN+I)=C(8-NMIN+I)
        C(8)='0'
        GOTO 5
20      CONTINUE
        IB=0
        DO 30 I=1,8
        IF(C(I).NE.' ')GOTO 40
        IB=IB+1
30      CONTINUE
40      C(9)='\'
        IF(NADJ.EQ.0)XT=AMAX1(0.,AMIN1(XE-.0125*IB,.89))
        IF(NADJ.EQ.1)XT=AMAX1(0.,AMIN1(XE-.1,.89))
        IF(NADJ.EQ.2)XT=AMAX1(0.,AMIN1(XE-.05-.00625*IB,.89))
        YT=AMAX1(0.,AMIN1(YE,.99))
        CALL BCHART(XT,YT,CNUMOU)
        RETURN
        END
        SUBROUTINE NUMOU(IXE,IYE,RI,NDP,NADJ)
        CHARACTER*64 CNUMOU,CTEMP
        CHARACTER*1 CNUM(10)
        DATA CNUM/'1','2','3','4','5','6','7','8','9','0'/
C *** THIS ROUTINE CONVERTS A NUMBER TO TEXT AND TELLS CHART WHERE TO PLACE IT
        ITR=ABS(RI)*10**NDP
        IC=1
        CNUMOU='\'
        DO 10 I=1,11
        INEXT=ITR/10
        ICHAR=ITR-10*INEXT
        IF(ICHAR.EQ.0)ICHAR=10
        CTEMP=CNUM(ICHAR)//CNUMOU
        CNUMOU=CTEMP
        IF(IC.EQ.NDP)THEN
        IC=IC+1
        CTEMP='.'//CNUMOU
        CNUMOU=CTEMP
        ENDIF
        ITR=INEXT
        IF(INEXT.EQ.0.AND.IC.GT.NDP+1)GOTO 20
10      IC=IC+1
20      CONTINUE
        IF(IC.EQ.0)CNUMOU='0\'
        IF(RI.LT.0.)THEN
        IC=IC+1
        CTEMP='-'//CNUMOU
        CNUMOU=CTEMP
        ENDIF
        IF(NADJ.EQ.0)IXC=IXE
        IF(NADJ.EQ.1)IXC=IXE-6*IC
        IF(NADJ.EQ.2)IXC=IXE-3*IC
        IF(RI.LT.0..AND.NADJ.EQ.0)IXC=IXC-14
        CALL BCHART(IXC,IYE,CNUMOU)
        RETURN
        END
      SUBROUTINE CCHAR(NTAPE,NC)
      CHARACTER*1 NTAPE(1)
      DO 10 I=1,64
      IF(NTAPE(I).EQ.'\')GOTO 20
      IF(NTAPE(I).EQ.' '.AND.NTAPE(I+1).EQ.' ')GOTO 20
10    CONTINUE
20    NC=I-1
      RETURN
      END
      SUBROUTINE CCHARB(NTAPE,NC,NMAX)
      CHARACTER*1 NTAPE(1)
      DO 10 I=1,NMAX
      IF(NTAPE(I).EQ.' ')GOTO 20
10    CONTINUE
20    NC=I-1
      RETURN
      END
      SUBROUTINE GREAD(X,Y,N,NAME)
      DIMENSION X(1),Y(1),RANS(2)
      CHARACTER*1 CT(80)
      CHARACTER*64 NAME,STATUS
      STATUS='OLD'
      IHFLAG=1
      NMAX=N
3     N=1
       IF(NAME(1:1).NE.'1')CALL MAOPEN(1,NAME,STATUS)
5       CONTINUE
        IF(IHFLAG.EQ.1)THEN
           CALL CHREAD(1,80,RANS,CT,2,0,IEND)
           IF(IEND.EQ.1)GOTO 400
           X(N)=RANS(1)
           Y(N)=RANS(2)
           IF(IHFLAG.EQ.1.AND.RANS(2).NE.0)IHFLAG=0
        ELSE
           READ(1,*,END=400)X(N),Y(N)
        ENDIF
        N=N+1
        IF(N.LT.NMAX)GOTO 5
400     N=N-1
        IF(IHFLAG.EQ.1)THEN
          DO 450 J=1,N
          Y(J)=X(J)
          X(J)=J
450       CONTINUE
        ENDIF
        IF(N.GT.0)RETURN
        PRINT*,' FILE=',NAME,' WAS EMPTY'
        GOTO 550
500     PRINT*,' COULD NOT OPEN FILE=',NAME
550     PRINT*,'ENTER NAME, STOP, OR \ '
        READ(*,'(A)')NAME
        PRINT*,NAME
        IF(NAME.EQ.'STOP')STOP
        IF(NAME.NE.'\')GOTO 3
        N=0
        RETURN
        END
        FUNCTION RNUMB(C,IP,IMAX)
        CHARACTER*1 C(140),KNUM(9)
        DATA KNUM/'1','2','3','4','5','6','7','8','9'/
        RNUMB=0
        IPT=IP
10      IF(IP.GT.IMAX)RETURN
        IF(C(IP).NE.' ')GOTO 20
        IP=IP+1
        GOTO 10
20      SI=1
        IF(C(IP).EQ.'-')THEN
        SI=-1
        IP=IP+1
        ENDIF
        IDPFL=0
        NDP=0
30      IF(IP.GT.IMAX)GOTO 500
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')GOTO 500
        IF(C(IP).EQ.'E'.OR.C(IP).EQ.'E'.OR.C(IP).EQ.'D')GOTO 400
        IF(C(IP).EQ.'.')THEN
        IDPFL=1
        IP=IP+1
        GOTO 30
        ENDIF
        IF(C(IP).EQ.'0')GOTO 35
        DO 32 J=1,9
32      IF(C(IP).EQ.KNUM(J))GOTO 35
        GOTO 500
35      IF(IDPFL.EQ.1)NDP=NDP+1
        RNUMB=RNUMB*10
        DO 40 J=1,9
40      IF(C(IP).EQ.KNUM(J))RNUMB=RNUMB+J
        IP=IP+1
        GOTO 30
400     IP=IP+1
        IEXP=0
        ISEXP=1
        IF(IP.GT.IMAX)GOTO 500
        IF(C(IP).EQ.'-')THEN
        ISEXP=-1
        IP=IP+1
        ENDIF
        IF(C(IP).EQ.'+')IP=IP+1
430     IF(IP.GT.IMAX)GOTO 480
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')GOTO 480
        IF(C(IP).EQ.'0')GOTO 435
        DO 432 J=1,9
432     IF(C(IP).EQ.KNUM(J))GOTO 435
        GOTO 480
435     IEXP=10*IEXP
        DO 440 J=1,9
440     IF(C(IP).EQ.KNUM(J))IEXP=IEXP+J
        IP=IP+1
        GOTO 430
480     CONTINUE
        IF(ISEXP*IEXP.GT.38)THEN
           RNUMB=SI*1.E38
        ELSE
           RNUMB=SI*RNUMB*10.**(ISEXP*IEXP-NDP)
        ENDIF
        IF(IP.GT.IMAX)RETURN
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')IP=IP+1
        RETURN
500     CONTINUE
        RNUMB=SI*RNUMB*10.**(-1*NDP)
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')IP=IP+1
        IF(IP.EQ.IPT)IP=IP+1
        RETURN
        END
        SUBROUTINE RTOC(R,C)                                            RTO00010
        CHARACTER*1 C(8),CNUM(10)                                       RTO00020
        DIMENSION I1(7)                                                 RTO00030
        DATA CNUM/'0','1','2','3','4','5','6','7','8','9'/              RTO00040
        DO 10 I=1,8                                                     RTO00050
10      C(I)=' '                                                        RTO00060
        CALL RTOI(R,I1,IEXP)                                            RTO00070
        JT=1                                                            RTO00080
        IF(R.LT.0.)THEN                                                 RTO00090
          C(1)='-'                                                      RTO00100
          JT=JT+1                                                       RTO00110
        ENDIF                                                           RTO00120
C *** 123456.7 OR -12345.6                                              RTO00130
        IF(IEXP.LT.5.AND.IEXP.GE.-1)GOTO 100                            RTO00140
        IF(R.GT.0..AND.IEXP.LT.6.AND.IEXP.GE.-2)GOTO 100                RTO00150
C *** 1.23456E6 OR -1.234E6                                             RTO00160
        IF(IEXP.GT.0.AND.IEXP.LE.9)GOTO 200                             RTO00170
C *** 1.234E-6 OR -1.23E-6                                              RTO00180
        IF(IEXP.GE.-9.AND.IEXP.LT.0)GOTO 300                            RTO00190
C *** 1.234E32 OR -1.23E32                                              RTO00200
        IF(IEXP.GT.9)GOTO 400                                           RTO00210
C *** 1.23E-32 OR -1.2E-32                                              RTO00220
        IF(IEXP.LT.-9)GOTO 500                                          RTO00230
100     IPFL=0                                                          RTO00240
        IF(IEXP.LT.0)THEN                                               RTO00250
          IPFL=1                                                        RTO00260
          C(JT)='0'                                                     RTO00270
          C(JT+1)='.'                                                   RTO00280
          JT=JT+2                                                       RTO00290
          ILM=-IEXP-1                                                   RTO00300
          DO 110 J=1,ILM                                                RTO00310
          C(JT)='0'                                                     RTO00320
110       JT=JT+1                                                       RTO00330
        ENDIF                                                           RTO00340
        JC=1                                                            RTO00350
        DO 120 J=1,8                                                    RTO00360
        IF(JT.GT.8.OR.JC.GT.7)GOTO 121                                  RTO00370
        IF(I1(JC).EQ.11.AND.IPFL.EQ.1)GOTO 121                          RTO00380
        IF(JC.EQ.IEXP+2.AND.IPFL.EQ.0)THEN                              RTO00390
          C(JT)='.'                                                     RTO00400
          IPFL=1                                                        RTO00410
          JT=JT+1                                                       RTO00420
          GOTO 120                                                      RTO00430
        ENDIF                                                           RTO00440
        IT=I1(JC)+1                                                     RTO00450
        IF(IT.GT.10)IT=1                                                RTO00460
        C(JT)=CNUM(IT)                                                  RTO00470
        JC=JC+1                                                         RTO00480
        JT=JT+1                                                         RTO00490
120     CONTINUE                                                        RTO00500
121     CONTINUE                                                        RTO00510
        IB=0                                                            RTO00520
        DO 130 I=1,8                                                    RTO00530
        IF(C(9-I).NE.' ')GOTO 131                                       RTO00540
130     IB=IB+1                                                         RTO00550
131     IM=8-IB                                                         RTO00560
        DO 140 I=1,IM                                                   RTO00570
140     C(9-I)=C(9-I-IB)                                                RTO00580
        DO 150 I=1,IB                                                   RTO00590
150     C(I)=' '                                                        RTO00600
        RETURN                                                          RTO00610
200     C(JT)=CNUM(I1(1)+1)                                             RTO00620
        C(JT+1)='.'                                                     RTO00630
        JT=JT+2                                                         RTO00640
        JC=2                                                            RTO00650
        DO 220 J=JT,6                                                   RTO00660
        IF(I1(JC).EQ.11)GOTO 221                                        RTO00670
        C(J)=CNUM(I1(JC)+1)                                             RTO00680
220     JC=JC+1                                                         RTO00690
221     C(7)='e'                                                        RTO00700
        C(8)=CNUM(IEXP+1)                                               RTO00710
229     IF(C(6).NE.' ')RETURN                                           RTO00720
        DO 235 I=1,5                                                    RTO00730
235     C(7-I)=C(6-I)                                                   RTO00740
        C(1)=' '                                                        RTO00750
        GOTO 229                                                        RTO00760
300     C(JT)=CNUM(I1(1)+1)                                             RTO00770
        C(JT+1)='.'                                                     RTO00780
        JT=JT+2                                                         RTO00790
        JC=2                                                            RTO00800
        DO 320 J=JT,5                                                   RTO00810
        IF(I1(JC).EQ.11)GOTO 321                                        RTO00820
        C(J)=CNUM(I1(JC)+1)                                             RTO00830
320     JC=JC+1                                                         RTO00840
321     C(6)='e'                                                        RTO00850
        C(7)='-'                                                        RTO00860
        C(8)=CNUM(1-IEXP)                                               RTO00870
329     IF(C(5).NE.' ')RETURN                                           RTO00880
        DO 335 I=1,4                                                    RTO00890
335     C(6-I)=C(5-I)                                                   RTO00900
        C(1)=' '                                                        RTO00910
        GOTO 329                                                        RTO00920
400     C(JT)=CNUM(I1(1)+1)                                             RTO00930
        C(JT+1)='.'                                                     RTO00940
        JT=JT+2                                                         RTO00950
        JC=2                                                            RTO00960
        DO 420 J=JT,5                                                   RTO00970
        IF(I1(JC).EQ.11)GOTO 421                                        RTO00980
        C(J)=CNUM(I1(JC)+1)                                             RTO00990
420     JC=JC+1                                                         RTO01000
421     C(6)='e'                                                        RTO01010
        IB=IEXP/10                                                      RTO01020
        IS=IEXP-10*IB                                                   RTO01030
        C(7)=CNUM(IB+1)                                                 RTO01040
        C(8)=CNUM(IS+1)                                                 RTO01050
429     IF(C(5).NE.' ')RETURN                                           RTO01060
        DO 435 I=1,4                                                    RTO01070
435     C(6-I)=C(5-I)                                                   RTO01080
        C(1)=' '                                                        RTO01090
        GOTO 429                                                        RTO01100
500     IF(I1(1).EQ.11)THEN                                             RTO01110
           I1(1)=0                                                      RTO01120
           IEXP=0                                                       RTO01130
           GOTO 100                                                     RTO01140
        ENDIF                                                           RTO01150
        C(JT)=CNUM(I1(1)+1)                                             RTO01160
        C(JT+1)='.'                                                     RTO01170
        JT=JT+2                                                         RTO01180
        JC=2                                                            RTO01190
        DO 520 J=JT,4                                                   RTO01200
        IF(I1(JC).EQ.11)GOTO 521                                        RTO01210
        C(J)=CNUM(I1(JC)+1)                                             RTO01220
520     JC=JC+1                                                         RTO01230
521     C(5)='e'                                                        RTO01240
        C(6)='-'                                                        RTO01250
        IB=-IEXP/10                                                     RTO01260
        IS=-IEXP-10*IB                                                  RTO01270
        C(7)=CNUM(IB+1)                                                 RTO01280
        C(8)=CNUM(IS+1)                                                 RTO01290
529     IF(C(4).NE.' ')RETURN                                           RTO01300
        DO 535 I=1,3                                                    RTO01310
535     C(5-I)=C(4-I)                                                   RTO01320
        C(1)=' '                                                        RTO01330
        GOTO 529                                                        RTO01340
        END                                                             RTO01350
        SUBROUTINE RTOI(R,I1,IEXP)                                      TEM00010
        DIMENSION I1(7),I2(7)                                           TEM00020
        RA=ABS(R)                                                       TEM00030
        IF(RA.EQ.0.)THEN                                                TEM00040
          IEXP=0                                                        TEM00050
          I1(1)=0                                                       TEM00060
          DO 5 I=2,7                                                    TEM00070
5         I1(I)=11                                                      TEM00080
          RETURN                                                        TEM00090
        ENDIF                                                           TEM00100
        RL=ALOG10(RA)                                                   TEM00110
        IEXP=RL                                                         TEM00130
        IF(RL.LT.0.)IEXP=IEXP-1                                         TEM00140
        DO 10 I=1,7                                                     TEM00150
10      I1(I)=11                                                        TEM00160
        IF(IEXP.GE.0.AND.IEXP.LT.6)I7=6                                 TEM00170
        IF(IEXP.GE.6.AND.IEXP.LT.10)I7=5                                TEM00180
        IF(IEXP.GE.10)I7=4                                              TEM00190
        IF(IEXP.LT.0.AND.IEXP.GT.-10)I7=4                               TEM00200
        IF(IEXP.LE.-10)I7=3                                             TEM00210
        IF(R.LT.0.)I7=I7-1                                              TEM00220
        RAT=RA*10.**(I7-1-IEXP)                                         TEM00230
        INT=RAT+.5                                                      TEM00240
        ISIG=0                                                          TEM00250
        IF(RAT-INT.GT.2.E-7*RAT)ISIG=1                                  TEM00260
        IFLAG=0                                                         TEM00270
        ILP=0                                                           TEM00280
        ILM=0                                                           TEM00290
        DO 20 J=1,7                                                     TEM00300
      IF(INT.EQ.0)GOTO 22                                               TEM00310
      ILP=ILP+1                                                         TEM00320
        INTS=INT/10                                                     TEM00330
        I2(J)=INT-10*INTS                                               TEM00340
        IF(I2(J).GT.0)IFLAG=1                                           TEM00350
        IF(IFLAG.EQ.1)GOTO 20                                           TEM00360
        IF(ISIG.EQ.0)ILM=ILM-1                                          TEM00370
20      INT=INTS                                                        TEM00380
22    IL=ILP+ILM                                                        TEM00390
      IF(ILP.GT.I7)IEXP=IEXP+1                                          TEM00400
        DO 30 J=1,IL                                                    TEM00410
30      I1(J)=I2(ILP+1-J)                                               TEM00420
        RETURN                                                          TEM00430
        END                                                             TEM00440
