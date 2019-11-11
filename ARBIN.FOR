        SUBROUTINE RTOC(R,C)
        CHARACTER*1 C(8),CNUM(10)
        DIMENSION I1(7)
        DATA CNUM/'0','1','2','3','4','5','6','7','8','9'/
        DO 10 I=1,8
10      C(I)=' '
        CALL RTOI(R,I1,IEXP)
        JT=1
        IF(R.LT.0.)THEN
          C(1)='-'
          JT=JT+1
        ENDIF
C *** 123456.7 OR -12345.6
        IF(IEXP.LT.5.AND.IEXP.GE.-1)GOTO 100
        IF(R.GT.0..AND.IEXP.LT.6.AND.IEXP.GE.-2)GOTO 100
C *** 1.23456E6 OR -1.234E6
        IF(IEXP.GT.0.AND.IEXP.LE.9)GOTO 200
C *** 1.234E-6 OR -1.23E-6
        IF(IEXP.GE.-9.AND.IEXP.LT.0)GOTO 300
C *** 1.234E32 OR -1.23E32
        IF(IEXP.GT.9)GOTO 400
C *** 1.23E-32 OR -1.2E-32
        IF(IEXP.LT.-9)GOTO 500
100     IPFL=0
        IF(IEXP.LT.0)THEN
          IPFL=1
          C(JT)='0'
          C(JT+1)='.'
          JT=JT+2
          ILM=-IEXP-1
          DO 110 J=1,ILM
          C(JT)='0'
110       JT=JT+1
        ENDIF
        JC=1
        DO 120 J=1,8
        IF(JT.GT.8.OR.JC.GT.7)GOTO 121
        IF(I1(JC).EQ.11.AND.IPFL.EQ.1)GOTO 121
        IF(JC.EQ.IEXP+2.AND.IPFL.EQ.0)THEN
          C(JT)='.'
          IPFL=1
          JT=JT+1
          GOTO 120
        ENDIF
        IT=I1(JC)+1
        IF(IT.GT.10)IT=1
        C(JT)=CNUM(IT)
        JC=JC+1
        JT=JT+1
120     CONTINUE
121     CONTINUE
        IB=0
        DO 130 I=1,8
        IF(C(9-I).NE.' ')GOTO 131
130     IB=IB+1
131     IM=8-IB
        DO 140 I=1,IM
140     C(9-I)=C(9-I-IB)
        DO 150 I=1,IB
150     C(I)=' '
        RETURN
200     C(JT)=CNUM(I1(1)+1)
        C(JT+1)='.'
        JT=JT+2
        JC=2
        DO 220 J=JT,6
        IF(I1(JC).EQ.11)GOTO 221
        C(J)=CNUM(I1(JC)+1)
220     JC=JC+1
221     C(7)='e'
        C(8)=CNUM(IEXP+1)
        RETURN
300     C(JT)=CNUM(I1(1)+1)
        C(JT+1)='.'
        JT=JT+2
        JC=2
        DO 320 J=JT,5
        IF(I1(JC).EQ.11)GOTO 321
        C(J)=CNUM(I1(JC)+1)
320     JC=JC+1
321     C(6)='e'
        C(7)='-'
        C(8)=CNUM(1-IEXP)
        RETURN
400     C(JT)=CNUM(I1(1)+1)
        C(JT+1)='.'
        JT=JT+2
        JC=2
        DO 420 J=JT,5
        IF(I1(JC).EQ.11)GOTO 421
        C(J)=CNUM(I1(JC)+1)
420     JC=JC+1
421     C(6)='e'
        IB=IEXP/10
        IS=IEXP-10*IB
        C(7)=CNUM(IB+1)
        C(8)=CNUM(IS+1)
        RETURN
500     IF(I1(1).EQ.11)THEN
           I1(1)=0
           IEXP=0
           GOTO 100
        ENDIF
        C(JT)=CNUM(I1(1)+1)
        C(JT+1)='.'
        JT=JT+2
        JC=2
        DO 520 J=JT,4
        IF(I1(JC).EQ.11)GOTO 521
        C(J)=CNUM(I1(JC)+1)
520     JC=JC+1
521     C(5)='e'
        C(6)='-'
        IB=-IEXP/10
        IS=-IEXP-10*IB
        C(7)=CNUM(IB+1)
        C(8)=CNUM(IS+1)
        RETURN
        END
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
        IEXP=RL+1.E-8
        IF(RL.LT.0.)IEXP=IEXP-1
        DO 10 I=1,7
10      I1(I)=11
        IF(IEXP.GE.0.AND.IEXP.LT.6)I7=6
        IF(IEXP.GE.6.AND.IEXP.LT.10)I7=5
        IF(IEXP.GE.10)I7=4
        IF(IEXP.LT.0.AND.IEXP.GT.-10)I7=4
        IF(IEXP.LE.-10)I7=3
        IF(R.LT.0.)I7=I7-1
        RAT=RA*10.**(I7-1-IEXP)
        INT=RAT+.5
        ISIG=0
        IF(RAT-INT.GT.2.E-7*RAT)ISIG=1
        IL=I7
        IFLAG=0
        DO 20 J=1,I7
        INTS=INT/10
        I2(J)=INT-10*INTS
        IF(I2(J).GT.0)IFLAG=1
        IF(IFLAG.EQ.1)GOTO 20
        IF(ISIG.EQ.0)IL=IL-1
20      INT=INTS
        DO 30 J=1,IL
30      I1(J)=I2(I7+1-J)
        RETURN
        END
