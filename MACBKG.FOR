        SUBROUTINE BKGFIT(XOFF,F,FA,WX,N,CONS,NV,NVM,BKGF,LFLAG,NCALL,
     # CUT,FNAME)
         IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION XC(256),FC(256),WC(256),FAC(256),
     # CONS(1),P(120),F(1),FA(1),WX(1)
        COMMON /SMSQS/PPCC(1830),PPCCI(1830)
        COMMON/CLFL/ILFLAG
        CHARACTER*64 FNAME,STATUS
        CHARACTER*4 BKGF
         CHARACTER*1 ANS
        REAL*4 XOFF,F,FA,WX
        ILFLAG=LFLAG
        PRINT*,' IN BKGFIT, BKGF=',BKGF
        IF(BKGF.EQ.'NOBF'.AND.NCALL.GT.0)RETURN
        CALL FALPAB(ALP,A,B,CUT,ASHIFT,CHIL)
         PRINT*,' A,B,ALP,ASHIFT ',A,B,ALP,ASHIFT
         NITT=3
         IF(BKGF.EQ.'FITB'.OR.BKGF.EQ.'AFIT')NITT=10
         NONEWK=0
         IF(BKGF.EQ.'NONK'.OR.NV.GE.NVM.OR.BKGF.EQ.'FINK')NONEWK=1
         IF(NCALL.GT.0)GOTO 60

C *** TRY TO READ THE OLD CONSTANTS AND SET FA TO WHERE IT WAS

        PRINT*,' IN BKGFIT FILE NAME IS ',FNAME
        IF(FNAME.EQ.'NONE')GOTO 40
         NONEWK=1
        STATUS='OLD'
        CALL MAOPEN(1,FNAME,STATUS)
         READ(1,*)LFLAG
         ILFLAG=LFLAG
        NV=0
20      READ(1,*,END=25)CONS(NV+1)
        NV=NV+1
        PRINT*,NV,CONS(NV)
        GOTO 20
25      CLOSE(1)
         IF(NV.GT.6)THEN
         IF(CONS(NV-1).EQ.0.D0)GOTO 45
         ENDIF
        DO 30 J=1,N
         XDP=XOFF+J
        CALL POLY(XDP,P,NV,CONS,FDP)
         FA(J)=FDP
30      CONTINUE
         NCALL=1
         IF(BKGF.NE.'NOBF')NCALL=30
         IF(BKGF.NE.'FITB')RETURN
40      CONTINUE
        NV=MAX0(1,NV)
         NONEWK=1
         ILFLAG=LFLAG
C *** INITIALIZE FA
45       CONTINUE
        DO 50 I=1,N
        FA(I)=F(I)
50      CONTINUE
60      NN=((N-1)/16+1)
C *** ABOVE MAKES THE NUMBER OF POINTS A MULTIPLE OF 16
        WAVE=0
        XAVE=0
      FAVE=0
      DO 120 I=1,10
        WAVE=WAVE+WX(N-I+1)
        XAVE=XAVE+XOFF+N+1-I
120   FAVE=F(N-I+1)+FAVE
        WAVE=.1*WAVE
        XAVE=.1*XAVE
      FAVE=.1*FAVE
160     CONTINUE
        DO 500 ITT=1,NITT
C *** DATA COMPRESSION
        JC=1
        DO 200 I=1,NN
        FC(I)=0
        WC(I)=0
        XC(I)=0
        JCP=JC+15
        DO 180 J=JC,JCP
        XAD=XAVE*WAVE
        FAD=FAVE*WAVE
        WAD=WAVE
        WT=1
        IF(J.LT.N)THEN
        Y=(F(J)-FA(J))**2*WX(J)
        ALPFP1=DMAX1(.1D-2,1+ALP*Y)
        WT=ALPFP1*A
C *** NOTE THAT WX IS 1/ER**2 IN THE ORIGINAL DATA
        IF(F(J).GT.FA(J).OR.BKGF(1:1).EQ.'A')WT=B/ALPFP1
C *** HELPING THE FIT AT THE BEGINNING
         IF(J.LT.10)WT=WT*4
        WAD=WX(J)*WT
        FAD=(F(J))*WAD
        XAD=(XOFF+J)*WAD
        ENDIF
C *** NOTE THAT WC WILL BE 1/ERR**2
        WC(I)=WC(I)+WAD
        XC(I)=XC(I)+XAD
180     FC(I)=FC(I)+FAD
        XC(I)=XC(I)/WC(I)
        FC(I)=FC(I)/WC(I)
         CALL POLY(XC(I),P,NV,CONS,FAC(I))
200     JC=JC+16
        IF(NCALL.EQ.0.)THEN
          ANUM=0
          ADEN=0
          DO 210 I=1,NN
          ANUM=ANUM+WC(I)*FC(I)
210       ADEN=ADEN+WC(I)
          CONS(1)=ANUM/ADEN
          IF(ILFLAG.EQ.1)CONS(1)=DLOG(CONS(1))
        ENDIF
        IF(NV.GT.118)THEN
           PRINT*,' HAVE A HEART, THIS IS ONLY A PC MAX BKG CONS=118'
           READ(*,*)ITEST
           STOP
        ENDIF
        IF(BKGF.NE.'FIXK'.AND.BKGF.NE.'FINK'.AND.NV.GT.58)THEN
           PRINT*,' THE PC VERSION OF ROBFIT CAN ONLY HANDLE 60'
           PRINT*,' FULLY VARIABLE CONS, SINCE NV=',NV,' THE'
           PRINT*,' NEXT 4 CANNOT BE ADDED WITH VARIABLE KNOTS'
           PRINT*,' THUS ALL KNOTS WILL BE HELD FIXED'
           PRINT*,' FUTURE KNOTS WILL BE PLACED BETWEEN OLD KNOTS'
           BKGF='FIXK'
           IF(NV.EQ.60)THEN
           CALL ORDER(CONS,NV)
             NV=61
             CONS(NV)=0
             NV=62
             CONS(NV)=N
             GOTO 300
          ENDIF
        ENDIF
         IF(ITT.NE.1)GOTO 300
         IF(NONEWK.EQ.1)GOTO 300
        NV=NV+1
        CONS(NV)=0
        IF(NV.LT.5)GOTO 300
        NV=NV+1
         PRINT*,' BEFORE MAX3D NN,NV ',NN,NV
        IF(BKGF.EQ.'FIXK')THEN
           CONS(NV)=.5*(CONS(126-NV)+CONS(124-NV))
           GOTO 300
        ENDIF
C        CALL MAX3D(ILR,XC,FC,FAC,WC,NN,NV)
        CALL BRESL(FC,FAC,WC,ILR,NN,LFLAG)
        ILR=MAX0(1,ILR)
        IUP=ILR+MAX0(5,NN/(4*NV))
        IUP=MIN0(NN,IUP)
        PRINT'('' NEW KNOTS AT'',G14.6)',XC(ILR),XC(IUP)
        CONS(NV)=XC(ILR)
        NV=NV+2
        CONS(NV-1)=0
        CONS(NV)=XC(IUP)
300     CONTINUE
        IF(BKGF.NE.'FIXK'.AND.BKGF.NE.'FINK')THEN
          NVS=NV
          CALL SPFIT(XC,FC,FAC,WC,NN,NV,CONS,PPCC,PPCCI)
          IF(NV.NE.NVS)GOTO 45
        ELSE
          CALL SPFIXK(XC,FC,FAC,WC,NN,NV,CONS,PPCC,PPCCI)
        ENDIF
        PRINT*,'BKG ,CHIL',NN,NN*CHIL
        PRINT*,'Background constants are'
        DO 555 I=1,NV
555     PRINT*,CONS(I)
C *** REMOVING UNUSED CONSTANTS
        DO 350 I=6,NV,2
        IF(CONS(I)-XOFF.GT.1.D0)
     2 GOTO 350
        NV=NV-2
        IM=I-1
        DO 340 J=IM,NV
340     CONS(J)=CONS(J+2)
350     CONTINUE
C *** DATA EXPANSION
        DO 430 J=1,N
         XDP=XOFF+J
        CALL POLY(XDP,P,NV,CONS,FDP)
         FA(J)=FDP
430     CONTINUE
500     CONTINUE
        IF(NV.GT.6.AND.NV.LE.60)CALL ORDER(CONS,NV)
        NCALL=1
        RETURN
      END
        SUBROUTINE POLY(X,P,NV,CONS,FA)
C *** THE FIRST FOUR COEFFICIENTS REPRESENT A CUBIC
C *** THE REST ARE IN THE FORM C(I)*(C(I+1)-X)+ **3
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION P(NV),CONS(NV)
        COMMON/CLFL/ILFLAG
        NVS=NV-4
        P(1)=1.D0
        FA=CONS(1)
        NPMAX=MIN0(4,NV)
        DO 5 I=2,NPMAX
        P(I)=X*P(I-1)
5       FA=FA+CONS(I)*P(I)
        DO 20 I=5,NV,2
        P(I)=0.D0
        P(I+1)=0.D0
        IF(CONS(I+1).LE.X)GOTO 20
        DIFF=CONS(I+1)-X
        DIFF2=DIFF*DIFF
        P(I+1)=3*CONS(I)*DIFF2
        P(I)=DIFF2*DIFF
        FA=FA+CONS(I)*P(I)
20      CONTINUE
        IF(ILFLAG.EQ.0)RETURN
        FA=DEXP(DMIN1(DMAX1(-20.D0,FA),20.D0))
        DO 30 I=1,NV
30      P(I)=FA*P(I)
        RETURN
        END
        SUBROUTINE SPFIT(XC,FC,FAC,WC,N,NV,CONS,PPCC,PPCCI)
        IMPLICIT REAL *8 (A-H,O-Z)
        CHARACTER*4 CHAR
C *** THIS ROUTINE MINIMIZES A SPLINE FIT TO THE COMPRESSED
C *** BACKGROUND
        DIMENSION PPCC(1),PPCCI(1),P(100),PC(100),CONS(NV)
        DIMENSION SM(100)
        DIMENSION XC(1  ),FC(1  ),WC(1  ),FAC(1  )
5       ALAM=25.6D0*N**3/NV**3
        IF(NV.GT.4)PRINT*,' ALAM=',ALAM
        DO 10 I=1,4
10      SM(I)=1.D1
        DO 12 I=5,NV,2
        SM(I)=1.D1
12      SM(I+1)=1.D4
14      FR=0.999
        CHB=1.D71
        CHL=1.D72
        NVD=NV*(NV+1)/2
         NIT=1
1045    CONTINUE
        DO 13 I=1,NV
13     PC(I)=0.
        DO 15 I=1,NVD
15       PPCC(I)=0.
         DO 18 I=6,NV,2
18       CONS(I)=DABS(CONS(I))
22       CHI=0
        DO 38 IT=1,N
        CALL POLY(XC(IT),P,NV,CONS,FAC(IT))
        ERR=(FC(IT)-FAC(IT))
        ERRS=ERR*ERR*WC(IT)
        CHI=CHI+ERRS
        IF(ERRS.GT.1.D28)THEN
          PRINT*,' ERRS TOO LARGE=',ERRS,' XC,FAC,WC,FC',
     2    XC(IT),FAC(IT),WC(IT),FC(IT)
          READ(*,*)ITEST
        ENDIF
        K=0
        DO 25 I=1,NV
25    PC(I)=PC(I)-2*ERR*P(I)*WC(IT)
        W3=2*WC(IT)
        DO 30 I=1,NV
        W3T=W3*P(I)
        DO 30 J=1,I
        K=K+1
30    PPCC(K)=PPCC(K)+W3T*P(J)
38      CONTINUE
        NKK=(NV-4)/2
        DO 42 I=1,NKK
        NCOEF=4+I*2
        ISDD=KIJ(NCOEF,NCOEF)
        DO 40 J=1,NKK
        IF(I.EQ.J)GOTO 40
        NCNN=4+J*2
        DIFF=(CONS(NCOEF)-CONS(NCNN))
        DIFF2=DIFF*DIFF
        CHI=CHI+.5*ALAM/DIFF2
        DIFF3=DIFF2*DIFF
        PC(NCOEF)=PC(NCOEF)-2*ALAM/DIFF3
        ISD=KIJ(NCOEF,NCNN)
        DIFF4=DIFF*DIFF3
        PPCC(ISD)=PPCC(ISD)-6*ALAM/DIFF4
        PPCC(ISDD)=PPCC(ISDD)+6*ALAM/DIFF4
40      CONTINUE
42      CONTINUE
        IF(((CHL-CHB.GT..001D0.AND.(CHL-CHB).GT.1D-6*CHB)
     2     .AND.NIT.LT.200).OR.CHI.GT.CHL*(1+1.D-6))THEN
           CALL SMSQ(CHI,CHB,CHL,PC,PPCC,PPCCI,FR,CONS,SM,NV,0)
           NIT=NIT+1
           GOTO 1045
        ENDIF
        CHIT=CHI
         PRINT'(A,G11.4)',' AT END OF SPFIT CHIT=',CHIT
         RETURN
      END
        SUBROUTINE SPFIXK(XC,FC,FAC,WC,N,NV,CONS,PPCC,PPCCI)
        IMPLICIT REAL *8 (A-H,O-Z)
        CHARACTER*4 CHAR
C *** THIS ROUTINE MINIMIZES A SPLINE FIT TO THE COMPRESSED
C *** BACKGROUND
        DIMENSION PPCC(1),PPCCI(1),P(100),PC(100),CONS(NV)
        DIMENSION SM(60),TCONS(60)
        DIMENSION XC(1  ),FC(1  ),WC(1  ),FAC(1  )
        NVT=(NV-4)/2+4
        DO 10 I=1,NVT
10      SM(I)=1.D1
        FR=0.99999
        CHB=1.D71
        CHL=1.D72
        NVD=NVT*(NVT+1)/2
         NIT=1
1045    CONTINUE
        DO 13 I=1,NVT
13     PC(I)=0.
        DO 15 I=1,NVD
15       PPCC(I)=0.
        DO 18 I=1,4
18      TCONS(I)=CONS(I)
        DO 20 I=5,NVT
20      TCONS(I)=CONS(3+2*(I-4))
         CHI=0
        DO 38 IT=1,N
        CALL POLY(XC(IT),P,NV,CONS,FAC(IT))
        ERR=(FC(IT)-FAC(IT))
        ERRS=ERR*ERR*WC(IT)
        CHI=CHI+ERRS
        K=0
        DO 25 I=1,NVT
        IARG=I
        IF(I.GT.5)IARG=3+2*(I-4)
25    PC(I)=PC(I)-2*ERR*P(IARG)*WC(IT)
        W3=2*WC(IT)
        DO 30 I=1,NVT
        IARG=I
        IF(I.GT.5)IARG=3+2*(I-4)
        W3T=W3*P(IARG)
        DO 30 J=1,I
        JARG=J
        IF(J.GT.5)JARG=3+2*(J-4)
        K=K+1
30    PPCC(K)=PPCC(K)+W3T*P(JARG)
38      CONTINUE
         PRINT'(A,3G20.6)',' SPFIXK CHI,CHB,CHL',CHI,CHB,CHL
        IF(((CHL-CHB.GT..01D0.AND.(CHL-CHB).GT.1D-5*CHB)
     2     .AND.NIT.LT.200).OR.CHI.GT.CHL)THEN
           CALL SMSQ(CHI,CHB,CHL,PC,PPCC,PPCCI,FR,TCONS,SM,NVT,0)
           DO 48 I=1,NVT
           IARG=I
           IF(I.GT.5)IARG=3+2*(I-4)
48         CONS(IARG)=TCONS(I)
           NIT=NIT+1
           GOTO 1045
        ENDIF
        CHIT=CHI
         PRINT'(A,G11.4)',' AT END OF SPFIXK CHIT=',CHIT
         RETURN
      END
        SUBROUTINE FALPAB(ALP,A,B,CUT,ASHIFT,CHIL)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION XG1(5),WG1(5),XG2(5),WG2(5),XG0(5),WG0(5)
        DATA XG1,WG1/.5133812615D0,.1188866291D1,.189642447D1,
     # .2661918482D1,.355390392D1,.260877802D0,.1993334055D0,
     # .3797122484D-1,.180587934D-2,.1168498619D-4/
        DATA XG2,WG2/.6568095668D0,.1326557084D1,.2025948015D1,
     # .2783290099D1,.3668470847D1,.185225285D0,.2062921868D0,
     # .4888991002D-1,.2686707670D-2,.1937314074D-4/
       DATA XG0,WG0/.3429013272D0,1.036610829D0,1.756683649D0,
     # 2.532731674D0,3.436159118D0,.6108626337D0,.2401386110D0,
     # .3387439445D-1,.1343645746D-2,.7640432855D-5/
         TSPI=.636619772
         C15=15.D0*DSQRT(TSPI)
         NC=0
         OALP=0
         ALP=.1D-4
         OFTZER=-2*CUT
C *** A AND B KEEP THE AVERAGE CORRECT FOR A GAUSSIAN DISTRIBUTION NO
C *** MATTER WHAT THE VALUE OF ALP.  AN OVERALL INCREASE OF BOTH A AND
C *** B ALSO KEEPS CHI**2 CORRECT
10       CONTINUE
        NC=NC+1
C *** B AND C ARE DETERMINED BY GAUSSIAN QUADRATURE
         BI=0
         C=0
         DI=0
         DO 20 I=1,5
         X2=XG1(I)*XG1(I)
         T1=1/(1+2*ALP*X2)
         BI=BI+WG1(I)*T1*(1-2*ALP*X2*T1)
         DI=DI+WG0(I)/(1+2*ALP*XG0(I)*XG0(I))
         DI=DI-20*WG2(I)/(1+2*ALP*XG2(I)*XG2(I))**2
         DI=DI+32*WG2(I)*XG2(I)*XG2(I)/(1+2*ALP*XG2(I)*XG2(I))**3
20       C=C+WG2(I)/(1+2*ALP*XG2(I)*XG2(I))
         BI=4*BI
C *** 1.128379 IS 2/PI**.5
         C=C*1.1283792D0
         DI=DI*15*1.1283792D0
C *** 2=A(1+3ALP)+2*B*C
         A=1
         B=(2-A*(1+3*ALP))/(2*C)
         C2=B*DI+30*A*(1+6*ALP)
         TCUT=1/(1+ALP*CUT*CUT)
         C1=2*B*CUT*TCUT*(1-2*CUT*ALP*TCUT)
         ASHIFT=-C1/C2
         CHIL=.5*C1*C1/C2
         FTZER=C15*(A*(2+8*ALP)-B*BI)-C1
         FP=(FTZER-OFTZER)/(ALP-OALP)
         OALP=ALP
         ALP=ALP-FTZER/FP
         OFTZER=FTZER
         IF(NC.GT.250)THEN
           PRINT*,' CANNOT FIND ALP IN FALPAB'
           STOP
         ENDIF
         IF(DABS(ALP).LE.1.D-30)GOTO 10
         IF(DABS((ALP-OALP)/ALP).GT.1.D-7)GOTO 10
         RETURN
         END
        SUBROUTINE BRESL(F,FB,WX,ILR,N,LFLAG)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION F(N),WX(N),FB(N)
20    ALR=0
        NM2=N-2
          FSP2=(F(2)-FB(2))*DSQRT(WX(2))
          FSP1=(F(1)-FB(1))*DSQRT(WX(1))
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
        IF(I.LT.NM2)THEN
          FSP2=(F(I+2)-FB(I+2))*DSQRT(WX(I+2))
        ENDIF
        SUM=SUM+FSP2-FSM3
      IF(ALR.GT.DABS(SUM))GOTO 40
      ILR=I
      ALR=DABS(SUM)
40    CONTINUE
      RETURN
      END
        SUBROUTINE ORDER(CONS,NV)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION CONS(NV),VNODE(58),CNODE(58)
C *** ORDER THE KNOTS
      WRITE(*,'(A/(2E20.6))')' BEFORE SORTING THE CONS ARE',(CONS(I),I=1
     # ,NV)
      NMAX=0
      DO 600 I=6,NV,2
      IF(NMAX.LT.1)THEN
        J=0
        GOTO 535
      ENDIF
      CALL LOCATE(CONS(I),VNODE,NMAX,J)
      IF(J.EQ.NMAX)GOTO 535
      JU=NMAX+1
      JL=NMAX
530   VNODE(JU)=VNODE(JL)
      CNODE(JU)=CNODE(JL)
      JU=JU-1
      JL=JL-1
      IF(JL.GT.J)GOTO 530
535   JP=J+1
      VNODE(JP)=CONS(I)
      CNODE(JP)=CONS(I-1)
      NMAX=NMAX+1
600   CONTINUE
      DO 700 I=6,NV,2
      JK=I/2-2
      CONS(I-1)=CNODE(JK)
700   CONS(I)=VNODE(JK)
       WRITE(*,'(A/(2E20.6))')' AFTER SORTING THE CONS ARE',(CONS(I),I=1
     # ,NV)
        RETURN
        END
