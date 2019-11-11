C$NOEXT
C$NOWAR
      PROGRAM STGEN
C *** THE STANDARD GENERATING PROGRAM (STGEN)
      PARAMETER (MAXCHN=1024)
      EXTERNAL GAUSS,FLOREN,EXALG,ALICOS
      REAL*8 CHI
      CHARACTER*64 NA
      CHARACTER*1 IAN,NO
      CHARACTER*4 INT1,INT,ZERO,BKG,LCDAT,HDAT
      CHARACTER*4 IDAT,Z4DAT
      COMMON/DATA/XI(MAXCHN),FI(MAXCHN),WX(MAXCHN),NP
      DIMENSION IHD(MAXCHN)
      COMMON/KNOTS/W(10),XP(10),C(10),NSC,NB,CB(4),XB,NEGSP
      COMMON/PASS/ETA,XSUP(100),YSUP(100),NPSUP
      EQUIVALENCE (IHD(1),FI(1))
      CHARACTER*80 II(4)
      CHARACTER*64 NADAT
C**********************************************
C
C   VSHAPE DATA
C
C*************************************************
      CHARACTER*64 STNAME,TYNAME,GRNAME,STATUS
      COMMON/CHVSHA/STNAME,TYNAME,GRNAME
      COMMON/CVSHAP/NBB,NS
      COMMON/BATCFL/IBATCH
C*************************************************
C   VOIGHT DATA
C*************************************************
      COMMON/CVOIGH/ETAA
C*************************************************
C*************************************************
C   BREAD DATA
C*************************************************
      CHARACTER*64 DATAFN
      COMMON/CHBREA/DATAFN
      COMMON/CBREAD/IBEGC,IENDC,IEXTWT
C *** IEXTWT=0 (ERRI=MAX(1,F(I)**.5), IEXTWT=1 ,CALC ERRS,
C *** IEXTWT=2 READ ERRORS FROM NADIFF FILE
C*************************************************
      DATA IDAT/'IDAT'/,Z4DAT/'Z4DA'/
        DATA NO,ZERO/'N','ZERO'/
        DATA BKG/'BKGR'/,LCDAT/'LCDA'/,HDAT/'HDAT'/
C***************************
C  CALL THE MENU PAGES
C****************************
      IBATCH=0
      IEXTWT=0
      CALL SHLINK(NA)
      IF(NA.EQ.'STOP')THEN
        STOP
C M       RETURN
      ENDIF
      IF(IENDC-IBEGC.GT.1023)THEN
        PRINT*,' STGEN IS DIMENSIONED FOR ONLY 1024 POINTS CHANGE '
        PRINT*,' MAXCH AND RECOMPILE'
        STOP
      ENDIF
      NB=NBB
      ETA=ETAA/DSQRT(DLOG(2.D0))
C  *** ETAA IS THE RATION OF THE DOPPLER PEAK TO THE LORENTZIAN
      NEGSP=1
C
C *** FLOREN IS A LORENTZIAN, GAUSS IS A GAUSSIAN
C
      INT1=TYNAME
        IF(NB.EQ.0)CB(1)=0
        IF(INT1.EQ.'GAUS'.OR.INT1.EQ.'LORE'.OR.INT1.EQ.'VOIG'.OR.
     2 INT1.EQ.'gaus'.OR.INT1.EQ.'lore'.OR.INT1.EQ.'voig')GOTO 60
        IT=INDEX(DATAFN,'.')
        IF(DATAFN(IT+1:IT+3).EQ.'WDA')THEN
          IEXTWT=2
C *** IEXTWT=2 TELLS OTHER CODE THAT THE WEIGHTS WERE READ IN
          STATUS='OLD'
          CALL MAOPEN(8,DATAFN,STATUS)
          DO 30 I=1,8192
          READ(8,*,END=32,ERR=32)DUM,FI(I),WX(I)
          IF(I.EQ.1)XE=DUM
          WX(I)=SQRT(WX(I))
30        CONTINUE
32        NP=I-1
          CLOSE(8)
C *** NB IS THE NUMBER OF COEFFICIENTS IN THE BACKGROUND POLY
C *** NS IS THE MAXIMUM NUMBER OF SPLINES USED TO FIT THE SHAPE
C *** NOTE THAT XI IS A DUMMY IN BREAD TO BE DEFINED AFTERWARDS
      ELSE
        CALL BREAD(XE,FI,II,IHD,DATAFN,IBEGC,IENDC,NP)
        DO 55 I=1,NP
        WX(I)=1./AMAX1(1.,FI(I))
55      XI(I)=XE+I
      ENDIF
60       II(1)=INT1
         II(2)='    '
        IF(INT1.EQ.'VOIG'.OR.INT1.EQ.'voig')GOTO 90
        IF(INT1.EQ.'GAUS'.OR.INT1.EQ.'gaus')GOTO 80
        IF(INT1.EQ.'LORE'.OR.INT1.EQ.'lore')GOTO 70
        GOTO 290
70      CALL BLI(XI,FI,-32.E0,32.E0,NP,200,FLOREN,1)
        GOTO 290
80      CALL BLI(XI,FI,-3.E0,3.E0,NP,200,GAUSS,1)
        GOTO 290
90      RANGE=(2/ETA**2)*(-1+SQRT(1+85*ETA**2))
C       WRITE(*,124)RANGE
124     FORMAT(' THE RANGE OF X IS 0 TO',F10.5)
        CALL BLI(XSUP,YSUP,0.E0,RANGE,NPSUP,100,EXALG,0)
C       PRINT*,'AFTER FIRST BLI CALL'
        CALL BLI(XI,FI,-32.E0,32.E0,NP,200,ALICOS,1)
C       PRINT*,'AFTER SECOND BLI CALL'
290     XB=XI(1)
        NSC=1
300   CALL XQMIN(NS,CHI,IEXTWT)
        CALL XPLOT(II,CHI,IEXTWT)
        WRITE(*,125)INT1
125     FORMAT(' THE CONSTANTS ARE FOR ',A4)
        IF(INT1.EQ.'VOIG'.OR.INT1.EQ.'voig')WRITE(*,126)ETA
126     FORMAT(' THE ETA PARAMETER IS ',F10.5)
        CALL LMAX
        STOP
        END
C*INCLUDE C:\ROBFIT\FORTRAN\TCHREAD
C*INCLUDE C:\ROBFIT\FORTRAN\SHLINK
C*INCLUDE C:\ROBFIT\FORTRAN\MENURD
C*INCLUDE C:\ROBFIT\FORTRAN\MAOPEN
C*INCLUDE C:\ROBFIT\FORTRAN\BREAD
C*INCLUDE C:\ROBFIT\FORTRAN\ARBIN
C*INCLUDE C:\ROBFIT\FORTRAN\CHARED
C*INCLUDE C:\ROBFIT\FORTRAN\BSMSQ
C$INCLUDE TCHREAD
C$INCLUDE SHLINK
C$INCLUDE MENURD
C$INCLUDE MAOPEN
C$INCLUDE BREAD
C$INCLUDE ARBIN
C$INCLUDE CHARED
C$INCLUDE BSMSQ
        SUBROUTINE XQMIN(NS,CHI,IEXTWT)
        PARAMETER (MAXCHN=1024)
        COMMON/DATA/XI(MAXCHN),FI(MAXCHN),WX(MAXCHN),NP
        REAL*8 CHI,CHB,CHL,PC,PPCC,A,FR,CONS,SM,AM
        CHARACTER*2 COUT,AOUT
        INTEGER*2 IOUT
        DIMENSION P(34),CONS(34),SM(34),PC(34),PPCC(595),FRES(MAXCHN)
     # ,AM(595),EC(10),EXP(10),EW(10)
        COMMON/KNOTS/W(10),XP(10),C(10),NSC,NB,CB(4),XB
        EQUIVALENCE (AOUT,IOUT)
C *** ASSUME THE XI'S ARE IN INCREASING ORDER AND DETERMINE
C *** AN APPROPRIATE PENALTY TERM
        COUT=' '
        AOUT=' '
        NIB=NP/4
        NIE=(3*NP)/4
        ASP=2*ABS(XI(NIB)-XI(NIE))/NP
C       WRITE(*,1932)NIB,NIE,ASP
1932    FORMAT(' NIB,NIE,ASP',2I5,E15.6)
        IF(NSC.GT.1)GOTO 16
        DO 14 J=1,4
14    CB(J)=0.E0
        CB(1)=AMIN1(FI(1),FI(NP))
        IF(NB.EQ.1)GOTO 15
        CB(1)=FI(1)
        CB(2)=(FI(NP)-FI(1))/NP
15    CONTINUE
        DO 1501 I=1,NP
        IF(IEXTWT.EQ.0)WX(I)=1./SQRT(AMAX1(1.,FI(I)))
1501    FRES(I)=(FI(I)-CB(1)-CB(2)*(XI(I)-XB))*WX(I)
       CALL RESIDL(XI,FI,FRES,NP,C(1),XP(1),W(1))
       W(1)=AMAX1(ASP,W(1))
16      IF(NSC.GT.1)NSC=NSC-1
        DO 18 J=1,NB
        CONS(J)=CB(J)
18    SM(J)=10.**(J-1)
23    CONTINUE
        IE=0
        CHB=1.E31
        CHL=1.E32
        FR=0.95
        NT=NB+3*NSC
        NPART=NB+1
        DO 24 J=1,NSC
        CONS(NPART)=C(J)
        SM(NPART)=1.E-2
        CONS(NPART+1)=XP(J)
        SM(NPART+1)=SM(NPART)*1.D6
        CONS(NPART+2)=W(J)
        SM(NPART+2)=SM(NPART)*1.D4
24    NPART=NPART+3
        NKIT=200
        DO 2500 KIT=1,NKIT
        IF(KIT.EQ.NKIT)CHB=CHL
        CHI=0.
        K=0
        DO 30 J=1,NT
        PC(J)=0.
        DO 30 I=1,J
        K=K+1
30    PPCC(K)=0.
        DO 200 I=1,NP
        FA=XPOLY(XI(I),P)
        FRES(I)=FI(I)-FA
        IF(IEXTWT.EQ.0)WX(I)=1./SQRT(AMAX1(1.,FA,FI(I)))
        FRES(I)=FRES(I)*WX(I)
        CHI=CHI+FRES(I)**2
        W1=-2*FRES(I)*WX(I)
        W2=2*WX(I)*WX(I)
        L=0
        DO 80 J=1,NT
        PC(J)=PC(J)+W1*P(J)
        W2P=W2*P(J)
        DO 80 K=1,J
        L=L+1
80    PPCC(L)=PPCC(L)+P(K)*W2P
200   CONTINUE
        NPART=NB+3
        PEN=0.E0
        DO 210 J=1,NSC
        IF(W(J).GE.ASP)GOTO 210
        PT=(ASP-W(J))**3
        PEN=PEN+PT
        CHI=CHI+PT
        PC(NPART)=PC(NPART)-3.E0*(ASP-W(J))**2
        L=NPART*(NPART+1)/2
        PPCC(L)=PPCC(L)+6.*(ASP-W(J))
210   NPART=NPART+3
        IF(CHL-CHB.GT..1E-1)GOTO 248
        IF(KIT.LE.3)GOTO 248
        IF(IE.EQ.1)GOTO 2502
        IE=1
        IF(CHI.LT.CHL)GOTO 2502
248   CALL SMSQ(CHI,CHB,CHL,PC,PPCC,AM,FR,CONS,SM,NT,0)
        DO 250 J=1,NB
250   CB(J)=CONS(J)
        NPART=NB+1
        DO 260 J=1,NSC
        C(J)=CONS(NPART)
        XP(J)=CONS(NPART+1)
        W(J)=ABS(CONS(NPART+2))
260   NPART=NPART+3
        CALL CHARED(IOUT)
        IF(AOUT.EQ.'S'.OR.AOUT.EQ.'s')THEN
          IF(COUT.NE.'S')THEN
            COUT='S'
            PRINT*,' We will be stopping once the min with this'
            PRINT*,' number of constants has been achieved'
          ELSE
            PRINT*,' Patience, at the min we will stop.'
          ENDIF
        ENDIF
2500   CONTINUE
2502  CONTINUE
        IF(NSC.GE.NS.OR.COUT.EQ.'S')GOTO 3000
        CALL RESIDL(XI,FI,FRES,NP,C(NSC+1),XP(NSC+1),W(NSC+1))
        W(NSC+1)=AMAX1(ASP,W(NSC+1))
3000  CONTINUE
        WRITE(*,104)NP
104   FORMAT(' WE ARE FITTING',I5,' POINTS')
        CHI=CHI-PEN
        WRITE(*,102)CHI,(I,CB(I),I=1,NB)
102   FORMAT(' CHISQUARE IS',E20.6/' THE BKGRD CONS ARE'/(I5,E20.6))
        IF(NSC.GT.0)WRITE(*,103)(I,C(I),XP(I),W(I),I=1,NSC)
103   FORMAT(' THE B SPLINE COEFFS'/'    #',8X,'C(I)',17X,'XP(I)',
     # 18X,'W(I)'/(I5,3E22.8))
        CHIT=CHI-NP+NB+3*NSC
C       IF(CHIT.LT.0.D0)RETURN
        IF(COUT.EQ.'S')RETURN
      IF(NSC.GE.NS)GOTO 3200
      NSC=NSC+1
      IF(NSC.LE.NS)GOTO 23
3200  NSC=NS
        RETURN
        END
        FUNCTION XPOLY(X,P)
C THIS ROUTINE CALCULATES POLY=SUM CI*SI ALONG WITH DPOLY/DCI
C THE FIRST NB SI'S ARE THE BACKGROUND POLYNOMIAL
C THE NEXT NS SI'S, WHICH HAVE DERIVATIVES WITH RESPECT TO C,XP,AND W
C ARE BSPLINES
        COMMON/KNOTS/W(10),XP(10),C(10),NS,NB,CB(4),XB
        DIMENSION P(1)
        XPOLY=CB(1)
        IF(NB.EQ.0) XPOLY=0
        P(1)=1.
        IF(NB.LT.2)GOTO 120
        DO 100 J=2,NB
        P(J)=(X-XB)**(J-1)
100   XPOLY=XPOLY+CB(J)*P(J)
120   NPART=NB+1
        DO 200 J=1,NS
        P(NPART)=0.
        P(NPART+1)=0.
        P(NPART+2)=0.
        XM=X-XP(J)
        IF(XM.LE.-W(J).OR.XM.GE.W(J))GOTO 200
        XM=XM/W(J)
C  PARTIAL WRT C(J)
        P(NPART)=2.*C(J)*((1.+XM)*(1.-XM))**3
        FADD=.5*C(J)*P(NPART)
        XPOLY=XPOLY+FADD
C PARTIAL WRT XP(J)
        P(NPART+1)=-3.*(FADD/(1.+XM)-FADD/(1.-XM))/W(J)
C PARTIAL WRT W(J)
        P(NPART+2)=P(NPART+1)*XM
200   NPART=NPART+3
        RETURN
        END
        SUBROUTINE LMAX
        CHARACTER*1 IA(80),IBL,IP
        COMMON/KNOTS/W(10),XP(10),C(10),NS,NB,CB(4),XB
C *** VSHAPE DATA
        CHARACTER*64 STNAME,STATUS
        COMMON/CHVSHA/STNAME
        DIMENSION CS(10),ISS(5)
        DATA IBL,IP/' ','+'/
C     FIND MAX X AND MIN X OF PEAK
        STATUS='UNKNOWN'
        CALL MAOPEN(12,STNAME,STATUS)
        XPS=1.D32
        XPL=-1.D32
        DO 30 J=1,NS
        XPS=AMIN1(XPS,XP(J)-W(J))
30    XPL=AMAX1(XPL,XP(J)+W(J))
31    II=0
        H=(XPL-XPS)/200.
        X=XPS+H/2.
33    CONTINUE
        FB=-1.E32
        DO 40 I=1,200
         F=POLYF(X)
         IF(FB.GE.F)GOTO 40
         FB=F
         XB=X
40    X=X+H
        II=II+1
        IF(II.GE.4)GOTO 50
        X=XB-H
        H=H*.01
        GOTO 33
50    CONTINUE
        DO 55 I=1,NS
        C(I)=C(I)/SQRT(FB)
        XP(I)=XP(I)-XB
        CS(I)=C(I)**2
55    CONTINUE
        XPS=XPS-XB
        XPL=XPL-XB
C FINDING THE HALF MAXIMUM POINTS
        XLH=-1
        CALL XXHMAX(XLH)
        XUH=1
        CALL XXHMAX(XUH)
        WRITE(*,105)XLH,XUH
105   FORMAT(' THE VALUES FOR WHICH F IS .5 ARE',2E20.12)
        AM=1./(XUH-XLH)
        XPS=XPS*AM
        XPL=XPL*AM
        DO 65 I=1,NS
        XP(I)=XP(I)*AM
65    W(I)=W(I)*AM
        WRITE(*,106)XPS,XPL
106   FORMAT(' THE STANDARD COVERS THE INTERVAL',2E20.12)
        WRITE(*,103)
103   FORMAT(' THE FOLLOWING ARE THE CONSTANTS READY FOR RLFIT')
        WRITE(12,101)(I,CS(I),XP(I),W(I),I=1,NS)
        CLOSE(12)
101   FORMAT(I5,3E20.7)
        WRITE(*,101)(I,CS(I),XP(I),W(I),I=1,NS)
C GRAPHING THE STANDARD PEAK
C       H=(XPL-XPS)/50.
C       X=XPS+H/2.
C       AI=0.
C       IS=-4
C       DO 72 I=1,5
C       ISS(I)=IS
C72    IS=IS+1
C        WRITE(*,108)ISS
C 108   FORMAT(1H1,3X,5I20)
C        DO 80 I=1,50
C        F=POLYF(X)
C       AI=AI+.02*F
C       DO 76 J=1,80
C76    IA(J)=IBL
C        IF(F.GT.0.E0)      IC=20.*ALOG10(F)+80.0000001E0
C       IC=MIN0(80,IC)
C       IF(IC.GE.1)IA(IC)=IP
C       WRITE(*,102)X,F,IA
C80    X=X+H
C102   FORMAT(2E12.4,80A1)
        WRITE(*,107)XB,XB+XLH,XB+XUH,AI
107   FORMAT(' XB=',E14.6,' HALF MAXS AT',2E14.6,' AI=',E14.6)
        RETURN
        END
        SUBROUTINE XXHMAX(XT)
C IF XT INITIALLY < 0 THE XT RETURNED WILL BE THE LOWER VALUE
C IF XT INITIALLY > O THE XT RETURNED WILL BE THE UPPER VALUE
        NLOOP=0
        FA=.5
        XA=0
        XB=XT
5       FB=POLYF(XB)-.5
        IF(FB.LT.0.)GOTO 10
        XB=2*XB
        GOTO 5
10      XT=XA-FA*(XA-XB)/(FA-FB)
        FT=POLYF(XT)-.5
        NLOOP=NLOOP+1
        IF(NLOOP.GT.100)GOTO 50
        IF(ABS(FT).LT.1.E-6.OR.ABS(XA-XB).LT.1.E-6)RETURN
        IF(FT.LT.0)GOTO 20
        FA=FT
        XA=XT
        GOTO 10
20      FB=FT
        XB=XT
        GOTO 10
50      WRITE(*,100)NLOOP,XA,XB,XT,FA,FB,FT
100     FORMAT(' XHMAX IN A LOOP, NLOOP,XA,XB,XT,FA,FB,FT'/I5,6E
     # 20.6)
       END
       FUNCTION POLYF(X)
C THIS ROUTINE CALCULATES POLYF=SUM CI*SI
        COMMON/KNOTS/W(10),XP(10),C(10),NS,NB,CB(4),XB
        POLYF=0.
        DO 200 J=1,NS
        XM=X-XP(J)
        IF(XM.LE.-W(J).OR.XM.GE.W(J))GOTO 200
        XM=XM/W(J)
        FADD=C(J)*C(J)*((1.+XM)*(1.-XM))**3
        POLYF=POLYF+FADD
200   CONTINUE
        RETURN
        END
        SUBROUTINE XPLOT(II,CHIS,IEXTWT)
        PARAMETER (MAXCHN=1024)
        REAL*8 CHIS
      CHARACTER*64 STNAME,TYNAME,GRNAME,STATUS
      COMMON/CHVSHA/STNAME,TYNAME,GRNAME
        COMMON/DATA/XI(MAXCHN),FI(MAXCHN),WX(MAXCHN),NP
        COMMON/KNOTS/W(10),XP(10),C(10),NSC,NB,CB(4),XB
        CHARACTER*80 II(4)
        DIMENSION P(34)
        INTEGER*2 IDIV,IXMUL,IX,IRES,IDAT,IFA,IFB,ICH(6)
        DATA IFA,IFB,ICH/8*0/
      IXMUL=32767/XI(NP)
      IDIV=1
      IFA=1
      FSMALL=0
      IXMUL=32767/XI(NP)
      WRITE(*,1234)IDIV,IFA,IXMUL
1234    FORMAT(' IDIV,IFA,IXMUL',3I10)
        STATUS='UNKNOWN'
        CALL MAOPEN(3,GRNAME,STATUS)
        WRITE(3,'(2Z4,A8,E20.12)')IDIV,IXMUL,II(1)(1:8),CHIS
        CHISL=0.
      DO 1090 J=1,NP
      FB=0
      IF(NB.GT.0) FB=CB(1)
      XMXB=XI(J)-XB
      XMXBP=XMXB
        DO 100 I=2,NB
        FB=FB+CB(I)*XMXBP
100     XMXBP=XMXBP*XMXB
      FA=XPOLY(XI(J),P)
      IF(IEXTWT.EQ.0)WX(I)=1./SQRT(AMAX1(1.,FA,FI(I)))
      TEMP=AMIN1(32767.,100*(FI(J)-FA)*WX(I))
      IRES=0
      IF(TEMP.GT.-32767)IRES=TEMP
        IFA=1000.*ALOG(AMAX1(1.E-3,FA-FSMALL))
        IFB=1000.*ALOG(AMAX1(1.E-3,FB-FSMALL))
      IX=AMIN1(32767.,XI(J)*IXMUL)
        IDAT=1000.*ALOG(AMAX1(1.E-3,FI(J)-FSMALL))
      IF(NSC.EQ.0)GOTO 1085
C *** NOW FOR THE PEAKS
        DO 1070 I=1,6
1070    ICH(I)=-10000
        KP=0
        DO 1080 K=1,NSC
        KP=KP+1
        IF(KP.EQ.7)KP=1
        ARG=.5*C(K)*P(NB+1+3*(K-1))
       ARG=1000.*ALOG(AMAX1(1.E-10,ARG))
        ICH(KP)=AMAX1(1.*ICH(KP),ARG)
1080   CONTINUE
1085    CONTINUE
        WRITE(3,'(11Z4)')IRES,IDAT,IFA,IFB,IX,ICH
        CHISL=CHISL+(.01*IRES)**2
1090    CONTINUE
        CLOSE (3)
        PRINT*,' CHISL=',CHISL
        RETURN
        END
        SUBROUTINE RESIDL(XI,FI,FR,NP,C,XP,W)
        DIMENSION XI(NP),FI(NP),FR(NP)
        CALL XRESL(FR,ALR,JB,NP)
C        PRINT*,' ALR,JB,NP',ALR,JB,NP
         XP=XI(JB)
        JS=MAX0(1,JB-8)
        JL=MIN0(NP,JB+8)
50    W=XI(JL)-XI(JS)
        C=ALR*SQRT(ABS(FI(JB)))/W
        C=SQRT(ABS(C))
C       WRITE(*,101)C,XP,W
101   FORMAT(' IN ZEROTH APPROX C,X,W'/3D12.5)
        RETURN
        END
        SUBROUTINE XRESL(FRES,ALR,ILR,N)
        DIMENSION FRES(N)
20    ALR=0
        NM2=N-2
        FSP2=FRES(2)
        FSP1=FRES(1)
        FS=0
        FSM1=0
        FSM2=0
        FSM3=0
        SUM=FSP1+FSP2
      DO 40 I=1,N
        FSM3=FSM2
        FSM2=FSM1
        FSM1=FS
        FS=FSP1
        FSP1=FSP2
        FSP2=0
        IF(I.LT.NM2)FSP2=FRES(I+2)
        SUM=SUM+FSP2-FSM3
      IF(SUM.LT.ALR)GOTO 40
      ILR=I
      ALR=SUM
40    CONTINUE
        ALR=ALR/2.236
      RETURN
      END
      SUBROUTINE BLI(XI,FI,B,E,NP,N,FLOR,NW)
      DIMENSION D(200),XI(N),FI(N)
        NP=3
        XI(1)=B
        FI(1)=FLOR(XI(1))
        XI(2)=(B+E)/2
        FI(2)=FLOR(XI(2))
        XI(3)=E
        FI(3)=FLOR(XI(3))
        D(1)=-1
        D(3)=-1
        IM=2
90      IB=MAX0(2,IM-1)
        IE=MIN0(NP-1,IM+3)
        DO 100 I=IB,IE
        D(I)=ABS(FI(I)-FI(I-1)-((XI(I)-XI(I-1))/(XI(I+1)-XI(I-1)
     #))*(FI(I+1)-FI(I-1)))
        D(I)=ABS(D(I)*(XI(I+1)-XI(I-1)))
100      CONTINUE
C *** FINDING THE NEW IM
        IM=2
        DM=D(2)
        DO 120 I=3,NP
        IF(DM.GT.D(I))GOTO 120
        DM=D(I)
        IM=I
120     CONTINUE
102     FORMAT(' IN BLI DM, IM',E20.6,I5)
        FSAVE=FI(IM)
        XSAVE=XI(IM)
        XP=.5*(XI(IM)+XI(IM+1))
        XM=.5*(XI(IM)+XI(IM-1))
C *** SHOVE THE STACK UP
        NMOVE=NP-IM
        J=NP
        DO 160 I=1,NMOVE
        XI(J+2)=XI(J)
        FI(J+2)=FI(J)
        D(J+2)=D(J)
160      J=NP-I
        XI(IM)=XM
        FI(IM)=FLOR(XM)
        XI(IM+1)=XSAVE
        FI(IM+1)=FSAVE
        XI(IM+2)=XP
        FI(IM+2)=FLOR(XP)
        NP=NP+2
        IF(NP.LT.N-2.AND.NP.LT.198)GOTO 90
        IF(NW.NE.1)RETURN
C       WRITE(*,1985)NP
1985    FORMAT(I5)
C       WRITE(*,1986)
1986      FORMAT(18X,'XI',18X,'FI')
1987  FORMAT(2E20.6)
        RETURN
        END
        FUNCTION GAUSS(X)
        GAUSS=0.E0
        X2=X*X
        IF(X2.GT.85.E0)RETURN
        GAUSS=1000.*EXP(-X2)
        RETURN
        END
        FUNCTION FLOREN(X)
        FLOREN=1000./(X*X+1.E0)
        RETURN
        END
        FUNCTION EXALG(X)
        COMMON/PASS/ETA,XSUP(200),NPSUP
        EXALG=1000.*(1+ETA)*EXP(-X-(ETA*X/2)**2)
        RETURN
        END
        FUNCTION ALICOS(ALPHA)
        IMPLICIT REAL*8 (A-H,O-Z)
        COMMON/PASS/ETA,XI(100),F(100),NP
         REAL*4 ALICOS,ALPHA,ETA,XI,F
C *** CALCULATES VM INTEGRAL OF DCOS(ALPHA*X)*F(X)
        AIFUN(EF0,EF1,EF2,EF3,X)=DCOS(X)*(EF3*(3*X*X-6)+EF2*2*X
     # +EF1)+DSIN(X)*(EF3*X*(X*X-6)+EF2*(X*X-2)+EF1*X+EF0)
       ALPA=(1+ETA)*ALPHA
       IF(DABS(ALPA).LT.1.D-7)ALPA=1.D-7
       ALPAD=1/ALPA
       ALPAD2=ALPAD*ALPAD
       ALPAD3=ALPAD2*ALPAD
       ALPAD4=ALPAD3*ALPAD
       ALICOS=0
       NPMM=NP-2
       DO 100 I=2,NPMM
       X1=XI(I-1)
       X2=XI(I)
       X3=XI(I+1)
       X4=XI(I+2)
       F1=F(I-1)
       F1=F1/((X1-X2)*(X1-X3)*(X1-X4))
       F2=F(I)/((X2-X1)*(X2-X3)*(X2-X4))
       F3=F(I+1)/((X3-X1)*(X3-X2)*(X3-X4))
       F4=F(I+2)/((X4-X1)*(X4-X2)*(X4-X3))
       EF0=X2*X3*X4*F1+X1*X3*X4*F2+X1*X2*X4*F3+X1*X2*X3*F4
       EF0=-EF0*ALPAD
       EF1=(X2*X3+X2*X4+X3*X4)*F1+(X1*X3+X1*X4+X3*X4)*F2
     # +(X1*X2+X1*X4+X2*X4)*F3+(X1*X2+X1*X3+X2*X3)*F4
       EF1=EF1*ALPAD2
       EF2=(X2+X3+X4)*F1+(X1+X3+X4)*F2+(X1+X2+X4)*F3+(X1+X2+X3)*F4
       EF2=-EF2*ALPAD3
       EF3=(F1+F2+F3+F4)*ALPAD4
       XUB=XI(I+1)*ALPA
       IF(I.EQ.NPMM)XUB=XI(NP)*ALPA
       XLB=XI(I)*ALPA
       IF(I.EQ.2)XLB=XI(1)*ALPA
       FUB=AIFUN(EF0,EF1,EF2,EF3,XUB)
       FLB=AIFUN(EF0,EF1,EF2,EF3,XLB)
       IF(DABS((FUB-FLB)/FLB).LT.1.D-5.AND.DABS(XUB-XLB).LT..5D0)GOTO90
       ALICOS=ALICOS+FUB-FLB
       GOTO 100
90      XA=.5*(XUB+XLB)
        ALICOS=ALICOS+DCOS(XA)*(EF0+XA*(EF1+XA*(EF2+XA*EF3)))*(XUB-XLB)
100     CONTINUE
        RETURN
        END
