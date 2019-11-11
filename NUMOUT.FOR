        SUBROUTINE NUMOUT(XE,YE,R,NADJ)
        CHARACTER CNUMOU*64,C(64)*1
        EQUIVALENCE (CNUMOU,C(1))
        CNUMOU=' '
        CALL RTOC(R,C)
        C(9)='\'
        IBL=0
        DO 10 I=1,8
        IF(C(I).NE.' ')GOTO 12
        IBL=IBL+1
10      CONTINUE
12      IC=8-IBL
        IF(NADJ.EQ.0)XC=XE-.009375*IBL
        IF(NADJ.EQ.1)XC=XE-.009375*(IC+IBL)
        IF(NADJ.EQ.2)XC=XE-.009375*(.5*IC+IBL)
        CALL BCHART(XC,YE,CNUMOU)
        RETURN
        END
       SUBROUTINE AXIS(VMIN,VMAX,LFLAG,NRES,HMIN,HMAX,II,CHI,LABEL)
        CHARACTER*8 II
        CHARACTER*64 TITLE,TEMP,LABEL
         COMMON/AXISP/BHORI,BVERT,SHORI,SVERT,EHORI,EVERT
         EVERT=1.0
         EHORI=1.0
         BVERT=.083333
         BHORI=.125
15       CONTINUE
         SVERT=(EVERT-.1645-BVERT)/(VMAX-VMIN)
         SHORI=(EHORI-BHORI-.01406)/(HMAX-HMIN)
        TITLE=II//' CHIS=\'
         X2=EVERT-.12
         X1=EHORI-.5453
        CALL BCHART(X1,X2,TITLE)
        CALL NUMOUT(X1+.2343,X2,CHI,0)
         X1=BHORI
         X2=EVERT
        CALL STPL(X1,X2)
         X2=BVERT
        CALL PLOT(X1,X2)
         X1=EHORI
        CALL PLOT( X1,X2)
         T1=.25*(EVERT-.1646-BVERT)
         X1=BHORI
         X2=X1+.015635
        DO 10 I=1,5
        XVERT=BVERT+T1*(I-1)
         IF(I.EQ.1)GOTO 8
         IF(NRES.EQ.1.AND.I.EQ.5)GOTO 8
        CALL STPL(X1,XVERT)
        CALL PLOT(X2,XVERT)
8       RP=VMIN+(VMAX-VMIN)*.25*(I-1)
        IF(LFLAG.EQ.1)RP=EXP(VMIN+.25*(I-1)*(VMAX-VMIN))
         CALL NUMOUT(X1-.0219,XVERT-.01,RP,1)
10       CONTINUE
         X1=BHORI-.1094
         X2=.5*(BVERT+EVERT)
        TEMP='counts\'
        CALL BCHART(X1,X2,TEMP)
        IF(NRES.NE.1)GOTO 16
         X1=BHORI
         X2=EVERT-.04
         CALL STPL(X1,X2)
         X3=EHORI
       CALL PLOT(X3,X2)
         X2=X2-.0625
        CALL STPL(X1,X2)
        CALL PLOT(X3,X2)
         X2=X2-.0625
        CALL STPL(X1,X2)
        CALL PLOT(X3,X2)
16       T1=(EHORI-BHORI-.0140625)*.1
         X1=BVERT
         X2=X1+.020833
         XMHOR=EHORI-.046875
        DO 20 J=1,11
        XM=BHORI+T1*(11-J)
         IF(J.EQ.1)GOTO 18
        CALL STPL(XM,X1)
        CALL PLOT(XM,X2)
18      RPX=HMIN+(11-J)*(HMAX-HMIN)/10.
        CALL NUMOUT(AMIN1(XM,XMHOR),X1-.03,RPX,2)
20    CONTINUE
         X1=.5*(BHORI+EHORI-.1094)
         X2=BVERT-.08333
        CALL BCHART(X1,X2,LABEL)
        RETURN
      END
        SUBROUTINE PLINTERP(FORIG,XORIG,FINT,XINT,IB,IEX)
C *** INTERPOLATION ROUTINE USED IN STDIS
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
100   CONTINUE
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
        SUBROUTINE RTOI(R,I1,IEXP)
        DIMENSION I1(7),I2(7)
        RA=ABS(R)
        IF(RA.EQ.0.)THEN
          IEXP=0
          I1(1)=0
          DO 5 I=2,7
5         I1(I)=11
          RETURN
        ENDIF
        RL=ALOG10(RA)
        IEXP=RL
        IF(RL.LT.0.)IEXP=IEXP-1
        DO 10 I=1,7
10      I1(I)=11
        IF(IEXP.GE.0.AND.IEXP.LT.6)I7=4
        IF(IEXP.GE.6.AND.IEXP.LT.10)I7=5
        IF(IEXP.GE.10)I7=4
        IF(IEXP.LT.0.AND.IEXP.GT.-10)I7=4
        IF(IEXP.LE.-10)I7=3
        IF(R.LT.0.)I7=I7-1
        RAT=RA*10.**(I7-1-IEXP)
        INT=RAT+.5
        ISIG=0
        IF(RAT-INT.GT.2.E-7*RAT)ISIG=1
        IFLAG=0
        ILP=0
        ILM=0
        DO 20 J=1,7
      IF(INT.EQ.0)GOTO 22
      ILP=ILP+1
        INTS=INT/10
        I2(J)=INT-10*INTS
        IF(I2(J).GT.0)IFLAG=1
        IF(IFLAG.EQ.1)GOTO 20
        IF(ISIG.EQ.0)ILM=ILM-1
20      INT=INTS
22    IL=ILP+ILM
      IF(ILP.GT.I7)IEXP=IEXP+1
        DO 30 J=1,IL
30      I1(J)=I2(ILP+1-J)
        RETURN
        END
