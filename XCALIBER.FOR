C$NOWAR
C$NOEXTEN
        PROGRAM XCALIBER
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION XC(80),EXC(80),EC(80),AANS(40),
     2   EEC(80),STR(80),ESTR(80),STRC(80),ESTRC(80),
     3   ECON(40),EFFCON(40),
     4   P(40),PC(40),PPCC(820),EPPCCI(820),EFPPCI(820)
        CHARACTER*80 EFFTIT,DATTIT
        CHARACTER*64 NA,STATUS
        CHARACTER*1 COM(30,80)
        CHARACTER*1 ANS,ANSE,CANS(80)
        CHARACTER*1 CAREA(8),CEAREA(8)
        REAL*4 AREA,EAREA
C*************************************************
C
C   DATA TO MAIN ROUTINE XCALIBER
C
C*************************************************
      CHARACTER*64 NAENER,NAEFF,NACAL,NAPEAK
      COMMON/XCNAME/NAENER,NAEFF,NACAL,NPEAKS,NAPEAK(20)
      COMMON/BATCFL/IBATCH
C*************************************************
        EXTERNAL EPOLY,EFPOLY
        IBATCH=0
        CALL XCLINK(NA)
        IF(NA.EQ.'STOP')THEN
          STOP
        ENDIF
        PRINT'(A64/)',NAENER,NAEFF,NACAL
        PRINT'(A,I3/(A))',' NPEAKS ',NPEAKS,NAPEAK(1),NAPEAK(2)
        ANSE='N'
        IF(NAENER.EQ.'NONE'.AND.NACAL.EQ.'NONE')THEN
          PRINT*,' A FIT OF ENERGY VERSUS CHANNEL CANNOT BE MADE'
          PRINT*,' UNTIL YOU CONSTRUCT A FILE OF CHANNELS VERSUS',
     2 'ENERGY AND PLACE ITS NAME IN THE MENU'
          STOP
        ENDIF
        IF(NAENER.NE.'NONE'.AND.NAENER.NE.'MAKE')THEN
           STATUS='OLD'
           CALL MAOPEN(4,NAENER,STATUS)
           READ(4,*)JM
           JMD=JM*(JM+1)/2
           READ(4,*)(ECON(I),I=1,JM)
           READ(4,*)(EPPCCI(I),I=1,JMD)
           CLOSE(4)
           PRINT'(A/(4G16.6))',' ECON ARE',(ECON(I),I=1,JM)
        ENDIF
        IF(NAEFF.NE.'NONE'.AND.NAEFF.NE.'MAKE')THEN
           STATUS='OLD'
           CALL MAOPEN(2,NAEFF,STATUS)
           READ(2,*)JME
           JMED=JME*(JME+1)/2
           READ(2,*)(EFFCON(I),I=1,JME)
           READ(2,*)(EFPPCI(I),I=1,JMED)
           CLOSE(2)
           PRINT'(A/(4G16.6))',' EFFCON ARE',(EFFCON(I),I=1,JME)
        ENDIF
        IF(NACAL.NE.'NONE')THEN
          NIV=6
          IF(NAEFF.NE.'NONE')NIV=8
          IL=INDEX(NACAL,'.')-1
          NA=NACAL(:IL)//'.FCA'
          STATUS='OLD'
          CALL MAOPEN(1,NACAL,STATUS)
          STATUS='UNKNOWN'
          CALL MAOPEN(3,NA,STATUS)
          DO 13 JK=1,5
          READ(1,'(A80,A20)')EFFTIT
          WRITE(*,'(1X,A79)')EFFTIT
          WRITE(3,'(A80)')EFFTIT
13         CONTINUE
          N=1
5        CALL LINPUT(1,CANS,AANS,NIV,ICANS,IEND)
        IF(IEND.EQ.1)GOTO 10
        XC(N)=AANS(1)
        EXC(N)=AANS(2)
        IF(EXC(N).LE.0.D0)GOTO 5
        EC(N)=AANS(3)
        EEC(N)=AANS(4)
        STR(N)=AANS(5)
        ESTR(N)=AANS(6)
        STRC(N)=AANS(7)
        ESTRC(N)=AANS(8)
        DO 9 I=1,30
9       COM(I,N)=CANS(I)
          N=N+1
          IF(N.LE.80)GOTO 5
10        N=N-1
          CLOSE(1)
          IF(NAENER.EQ.'MAKE')THEN
            CALL EFIT(ECON,PC,EPPCCI,PPCC,P,XC,EXC,EC,EEC,N,JM)
            WRITE(*,'('' THE ENERGY CONSTANTS''/(4E15.6))')
     2       (ECON(I),I=1,JM)
            WRITE(3,'('' THE ENERGY CONSTANTS''/(4E15.6))')
     2       (ECON(I),I=1,JM)
            IL=INDEX(NACAL,'.')-1
            NA=NACAL(:IL)//'.XDA'
            STATUS='UNKNOWN'
            CALL MAOPEN(4,NA,STATUS)
            WRITE(4,*)JM
            WRITE(4,'(2G16.6)')(ECON(I),I=1,JM)
            JMD=JM*(JM+1)/2
            WRITE(4,'(2G16.6)')(EPPCCI(I),I=1,JMD)
            WRITE(4,*)
            CLOSE(4)
C ***       DATA FOR GPLOT TO SHOW THE NON-LINEARITY
            H=1.2*XC(N)/100
            NA='EFIT.DAT'
            STATUS='UNKNOWN'
            CALL MAOPEN(4,NA,STATUS)
            DO 8 I=1,100
            X=H*I
            CALL FAEFA(X,.01D0,ECON,EPPCCI,JM,P,EPOLY,E,DE,ERR)
            E=E-(ECON(1)+X*ECON(2))
8           WRITE(4,*)X,E
            CLOSE(4)
            NA='EFIT.POI'
            STATUS='UNKNOWN'
            CALL MAOPEN(4,NA,STATUS)
            DO 12 I=1,N
12          WRITE(4,*)XC(I),EC(I)
            CLOSE(4)
C ***       DISPLAYING THE ENERGY FIT
            WRITE(3,'(7X,''CHAN'',11X,''ENERGY'',9X,''ECAL'',
     2       10X,''RES'')')
            WRITE(*,'(7X,''CHAN'',11X,''ENERGY'',9X,''ECAL'',
     2       10X,''RES'')')
            RESM=0
            DO 20 I=1,N
            CALL FAEFA(XC(I),EXC(I),ECON,EPPCCI,JM,P,EPOLY,E,DE,ERR)
            RES=(E-EC(I))/ERR
            IF(DABS(RES).GT.RESM)THEN
              IRESM=I
              RESM=DABS(RES)
             ENDIF
          WRITE(3,'(F8.2,'' +-'',F5.2,F8.2,'' +-'',F5.2,F8.2,'' +-'',
     2    F5.2,F8.2,5X,19A1)')XC(I),EXC(I),E,ERR,EC(I),EEC(I),RES,
     3    (COM(J,I),J=1,19)
          WRITE(*,'(F8.2,'' +-'',F5.2,F8.2,'' +-'',F5.2,F8.2,'' +-'',
     2    F5.2,F8.2,5X,30A1)')XC(I),EXC(I),E,ERR,EC(I),EEC(I),RES,
     3    (COM(J,I),J=1,30)
20        CONTINUE
          IF(RESM.GT.3.D0)THEN
           PRINT'('' NOTE THAT THE RESIDUAL FOR THE'',I3,'' THE POINT'',
     2     '' IS'',F5.2,'' THIS MAY INDICATE AN ERROR'')',IRESM,RESM
            READ(*,'(A)')ANS
          ENDIF
        ENDIF
C ***   FITTING THE EFFICIENCIES
        IF(NAEFF.EQ.'NONE')GOTO 35
        IF(NAEFF.EQ.'MAKE')THEN
        PRINT*,' DO YOU HAVE EFFICIENCY DATA'
        READ(*,'(A1)')ANSE
        IF(ANSE.NE.'Y')GOTO 35
        CALL EFFFIT(EFFCON,PC,EFPPCI,PPCC,P,XC,EXC,STR,ESTR,
     2   STRC,ESTRC,N,JME)
            WRITE(*,'('' THE EFFICIENCY CONSTANTS''/(4E15.6))')
     2       (EFFCON(I),I=1,JME)
            WRITE(3,'('' THE EFFICIENCY CONSTANTS''/(4E15.6))')
     2       (EFFCON(I),I=1,JME)
            IL=INDEX(NACAL,'.')-1
            NA=NACAL(:IL)//'.EFF'
            STATUS='UNKNOWN'
            CALL MAOPEN(4,NA,STATUS)
            WRITE(4,*)JME
            WRITE(4,'(2G16.6)')(EFFCON(I),I=1,JME)
            JMED=JME*(JME+1)/2
            WRITE(4,'(2G16.6)')(EFPPCI(I),I=1,JMED)
            WRITE(4,*)
            CLOSE(4)
            H=1.2*XC(N)/100
            NA='EFFIT.DAT'
            STATUS='UNKNOWN'
            CALL MAOPEN(4,NA,STATUS)
            DO 22 I=1,100
            X=H*I
            CALL FAEFA(X,1D-2,EFFCON,EFPPCI,JME,P,EFPOLY,EFFI,DEFFI,ERR)
22          WRITE(4,*)X,EFFI
            CLOSE(4)
            NA='EFFIT.POI'
            STATUS='UNKNOWN'
            CALL MAOPEN(4,NA,STATUS)
            DO 24 I=1,N
24          WRITE(4,*)XC(I),STR(I)/STRC(I)
            CLOSE(4)
C *** DISPLAYING THE EFFICIENCY FIT
       WRITE(3,'(''    CHAN  ENERGY   CSTRENGTH  STRENGTH    RES'')')
       WRITE(*,'(''    CHAN  ENERGY   CSTRENGTH  STRENGTH    RES'')')
       RESM=0
        DO 30 I=1,N
        CALL FAEFA(XC(I),EXC(I),EFFCON,EFPPCI,JME,P,EFPOLY,EFFI,
     2    DEFFI,ERR)
        STRENG=STR(I)/EFFI
        ESTREN=DSQRT((STR(I)*ERR/EFFI**2)**2+(ESTR(I)/EFFI)**2)
        RES=(STRENG-STRC(I))/ESTREN
        IF(DABS(RES).GT.RESM)THEN
           IRESM=I
           RESM=DABS(RES)
        ENDIF
        WRITE(3,'(4F8.2,'' +-'',2F6.2,5X,30A1)')XC(I),EC(I),
     2 STRC(I),STRENG,ESTREN,RES,(COM(J,I),J=1,30)
        WRITE(*,'(4F8.2,'' +-'',2F6.2,5X,30A1)')XC(I),EC(I),
     2 STRC(I),STRENG,ESTREN,RES,(COM(J,I),J=1,30)
30     CONTINUE
        IF(RESM.GT.3.D0)THEN
           PRINT'('' NOTE THAT THE RESIDUAL FOR THE'',I3,'' THE POINT'',
     2     '' IS'',F5.2,'' THIS MAY INDICATE AN ERROR'')',IRESM,RESM
        READ(*,*)ITEST
        ENDIF
        ENDIF
35     CONTINUE
       ENDIF
        DO 500 IPK=1,NPEAKS
         CLOSE(3)
        WRITE(*,'(1X,A)')NAPEAK(IPK)
        STATUS='OLD'
        CALL MAOPEN(1,NAPEAK(IPK),STATUS)
        IL=INDEX(NAPEAK(IPK),'.')-1
        NA=NAPEAK(IPK)(:IL)//'.PCA'
        STATUS='UNKNOWN'
        CALL MAOPEN(3,NA,STATUS)
         WRITE(3,'(A)')' DATA FROM ',NAPEAK(IPK)
         WRITE(3,'(A)')' CALIBRATION FROM ',NACAL,NAENER,NAEFF
         WRITE(3,'('' THE ENERGY CONSTANTS''/(4E15.6))')
     2    (ECON(I),I=1,JM)
         IF(NAEFF.NE.'NONE'.OR.ANSE.EQ.'Y')WRITE(3,
     2     '('' THE EFF CONSTANTS''/(4E15.6))')(EFFCON(I),I=1,JME)
        READ(1,'(A80)')DATTIT
        WRITE(3,'(1X,A79)')DATTIT
        WRITE(*,'(1X,A79)')DATTIT
        READ(1,'(A4,I5)')DATTIT,IP
        WRITE(3,'(1X,A4,I3)')DATTIT,IP
        WRITE(*,'(1X,A4,I3)')DATTIT,IP
        DO 15 I=1,2
        READ(1,'(A80)')DATTIT
        WRITE(3,'(1X,A79)')DATTIT
15      WRITE(*,'(1X,A79)')DATTIT
        IF(IP.NE.0)READ(1,'(10F8.4)')(ARAT,WRAT,I=1,IP)
      IF(NAEFF.NE.'NONE')GOTO 327
          WRITE(3,'(7X,''CHANNEL'',11X,''ENERGY'',11X,''FWHM'',11X,
     2    ''STRENGTH'')')
          WRITE(*,'(7X,''CHANNEL'',11X,''ENERGY'',11X,''FWHM'',11X,
     2    ''STRENGTH'')')
295       READ(1,*,END=490)X,EX,FWHM,EFWHM,AREA,EAREA
          CALL RTOC(AREA,CAREA)
          CALL RTOC(EAREA,CEAREA)
          CALL FAEFA(X,EX,ECON,EPPCCI,JM,P,EPOLY,E,EDER,ERR)
          FWHM=EDER*FWHM
          EFWHM=EFWHM*EDER
          WRITE(3,'(3(F8.2,'' +-'',F6.2),2X,8A1,'' +-'',8A1)')
     2      X,EX,E,ERR,FWHM,EFWHM,CAREA,CEAREA
          WRITE(*,'(3(F8.2,'' +-'',F6.2),2X,8A1,'' +-'',8A1)')
     2      X,EX,E,ERR,FWHM,EFWHM,CAREA,CEAREA
          GOTO 295
327   CONTINUE
        WRITE(*,110)
110     FORMAT(' ENTER THE ENERGY RANGE FOR INTENSITY',
     # ' NORMALIZATION'/' OR 0,0 TO NORMALIZE USING THE STRONGEST',
     # ' PEAK')
        READ(*,*)ERB,ERE
        WRITE(*,*)ERB,ERE
298     ANORM=0
300     READ(1,*,END=400)X,EX,FWHM,EFWHM,AREA,EAREA
        IF(ERB.LT.1.D-5)GOTO 350
        CALL FAEFA(X,EX,ECON,EPPCCI,JM,P,EPOLY,E,DE,ERR)
        IF(E.GT.ERE)GOTO 400
        IF(E.LT.ERB)GOTO 300
        CALL FAEFA(X,EX,EFFCON,EFPPCI,JME,P,EFPOLY,EFFIC,DEFF,ERR)
        ANORM=ANORM+AREA/EFFIC
        GOTO 300
350     CALL FAEFA(X,EX,EFFCON,EFPPCI,JME,P,EFPOLY,EFFIC,DEFF,ERR)
        ANORM=DMAX1(ANORM,AREA/EFFIC)
        GOTO 300
400     REWIND 1
        READ(1,'(A80)')DATTIT
        READ(1,'(A4,I5)')DATTIT,IP
        READ(1,'(A80)')DATTIT
        READ(1,'(A80)')DATTIT
        IF(IP.NE.0)READ(1,'(10F8.4)')(ARAT,WRAT,I=1,IP)
        PRINT*,' ANORM=',ANORM
        WRITE(*,1909)
1909    FORMAT(' ENTER THE SIZE DESIRED FOR THE NORMALIZING PEAK')
        READ(*,*)ASIZE
        ANORM=ANORM/ASIZE
        PRINT*,' ANORM=',ANORM
        WRITE(*,1978)
        WRITE(3,1978)
1978    FORMAT('   CHANNEL',6X,'ENERGY',11X,'FWHM',10X,'AREA',10X,
     2 'INTENSITY')
410     READ(1,*,END=490)X,EX,FWHM,EFWHM,AREA,EAREA
        IF(X.LT.0.D0)GOTO 490
        CALL FAEFA(X,EX,ECON,EPPCCI,JM,P,EPOLY,E,DE,ERR)
        CALL FAEFA(X,EX,EFFCON,EFPPCI,JME,P,EFPOLY,EFFIC,DEFF,ERRF)
        AR=AREA/(EFFIC*ANORM)
        EARC=DSQRT((EAREA/(ANORM*EFFIC))**2+(AR*ERRF/EFFIC)**2)
        FWHM=DE*FWHM
        EFWHM=EFWHM*DE
        WRITE(*,460)X,E,ERR,FWHM,EFWHM,AREA,EAREA,AR,EARC
        WRITE(3,460)X,E,ERR,FWHM,EFWHM,AREA,EAREA,AR,EARC
460     FORMAT(2F8.2,' +-',2F6.2,' +-',F4.2,F10.2,' +-',2F8.2,
     2 ' +-',F6.2)
        GOTO 410
490     CLOSE (1)
500     CONTINUE
510     WRITE(*,115)
115   FORMAT(' DO YOU WISH TO CONVERT A COUNTS VS CHANNEL FILE TO',
     # ' A COUNTS VS ENERGY FILE?')
      READ(*,'(A1)')ANS
      IF(ANS.EQ.'Y')THEN
         CLOSE(3)
        CALL ECHAN(ECON,EPPCCI,JM)
        GOTO 510
      ENDIF
         CLOSE(3)
        STOP
        END
      SUBROUTINE XCLINK(NA)
      DIMENSION FFCOM(200),FFVAR(200)
      CHARACTER*64 CSTR,VALU,NA
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/NSTR,NCMENU
      CHARACTER*40 FFCOM,FFVAR
C*************************************************
C
C   DATA TO MAIN ROUTINE XCALIBER
C
C*************************************************
      CHARACTER*64 NAENER,NAEFF,NACAL,NAPEAK
      COMMON/XCNAME/NAENER,NAEFF,NACAL,NPEAKS,NAPEAK(20)
C*************************************************
      CHARACTER*64 NADIRM,NADIRD,NADIRC,STATUS
C *** READ THE DIRECTORY INFORMATION
      NA='VRMAIN.MNU'
      STATUS='OLD'
      CALL MAOPEN(17,NA,STATUS)
      DO 5 I=1,16
5     READ(17,'(44X,A36)')NADIRM
      READ(17,'(44X,A36)')NADIRD
      READ(17,'(44X,A36)')NADIRC
      CLOSE(17)
      CALL CCHAR(NADIRM,NCM)
      CALL CCHAR(NADIRD,NCD)
      CALL CCHAR(NADIRC,NCC)
      NA='XCALIBER.MNU'
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\XCALIBER.MNU'
      NCMENU=1
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')RETURN
      DO 10 J=1,10
        NS=J*20+2
        IF(J.EQ.1)THEN
C*************************************************
        NS=NS+1
        NACAL=FFVAR(NS)
        IF(NCC.GT.0)NACAL=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        NAENER=FFVAR(NS)
        IF(NCC.GT.0.AND.NAENER.NE.'NONE'.AND.NAENER.NE.'MAKE')
     2   NAENER=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        NAEFF=FFVAR(NS)
        IF(NCC.GT.0.AND.NAEFF.NE.'NONE'.AND.NAEFF.NE.'MAKE')
     2   NAEFF=NADIRC(1:NCC)//'\'//FFVAR(NS)
C*******************************
        ENDIF
        IF(J.EQ.2)THEN
C*******************************
           NPEAKS=0
           DO 20 I=1,20
             NS=NS+1
             IF(FFVAR(NS)(1:1).EQ.' ')GOTO 20
             NAPEAK(I)=FFVAR(NS)
             IF(NCC.GT.0)NAPEAK(I)=NADIRC(1:NCC)//'\'//FFVAR(NS)
             NPEAKS=NPEAKS+1
20         CONTINUE
C*******************************
        ENDIF
10    CONTINUE
        RETURN
        END
        SUBROUTINE ECHAN(ECON,EPPCCI,JM)
        IMPLICIT REAL*8 (A-H,O-Z)
        REAL*4 F1(4096),F2(4096),FTRUN,XE
        DIMENSION ECON(1),EPPCCI(1),P(40),IHOUT(4096)
        CHARACTER*80 IHEAD(4),ITT(4)
        CHARACTER*78 HEAD,STATUS
        CHARACTER*64 NA,NAS,NAOLD,NAOUT
        CHARACTER*1 ANS,ANSOLD
        EXTERNAL EPOLY
        EQUIVALENCE(F1(1),IHOUT(1))
        DATA NAOLD/'NULL'/
      PRINT*,' CHANGES HAVE BEEN MADE SINCE THIS ROUTINES WAS LAST'
      PRINT*,' TESTED, CHECK YOUR FILES FOR POSSIBLE GLITCHES'
        PRINT*,' FIRST THE FILE TO BE CONVERTED'
        READ(*,'(A)')NA
        N1=1
        N2=4096
        CALL BREAD(XE,F1,IHEAD,IHOUT,NA,N1,N2,IMRD)
C       CALL REDAT(F1,IMRD,NA,IHEAD)
        PRINT*,' IS THE DATA BEING ADDED TO THAT IN ANOTHER FILE'
        READ(*,*)ANSOLD
        IF(ANSOLD.NE.'Y'.AND.ANSOLD.NE.'y')GOTO 150
        PRINT*,' ENTER THE NAME OF THE OLD DATA FILE'
        READ(*,'(A)')NAOLD
        STATUS='OLD'
        CALL MAOPEN(3,NAOLD,STATUS)
        READ(3,*)E1,E2,NCHAN
        CLOSE(3)
        GOTO 170
150     PRINT*,' ENTER EBEG, EEND, # OF CHANNELS'
        READ(*,*)E1,E2,NCHAN
170     EB=E1
        PRINT*,' EB,E2,NCHAN',EB,E2,NCHAN
        DO 200 I=1,NCHAN
200     F2(I)=0
        NOUTC=0
        NADJ=0
        DE=(E2-E1)/(NCHAN-1)
        E1=E1-DE
        COLD=CHAN(E1,ECON,EPPCCI,JM,300D0)
        PRINT*,' AFTER CALL TO CHAN COLD=',COLD
        CG=COLD+2
10      NOUTC=NOUTC+1
1001    NCOLD=COLD-NADJ
12      E1=E1+DE
        F2(NOUTC)=(NCOLD+1+NADJ-COLD)*FTRUN(F1,(NCOLD+1),IMRD)
     # +F2(NOUTC)
        N=NCOLD+1
        C1=CHAN(E1,ECON,EPPCCI,JM,CG)
        NC1=C1-NADJ
15      IF(N.GE.NC1)GOTO 20
        F2(NOUTC)=F2(NOUTC)+FTRUN(F1,(N+1),IMRD)
        N=N+1
        GOTO 15
20      F2(NOUTC)=(C1-N-NADJ)*FTRUN(F1,(NC1+1),IMRD)+F2(NOUTC)
        CG=C1+(C1-COLD)
        COLD=C1
        IF(E1.LT.E2)GOTO 10
        E1=EB
        IF(ANSOLD.EQ.'Y'.OR.ANSOLD.EQ.'y')THEN
C         CALL REDAT(F1,NIR,NAOLD,ITT)
        N1=1
        N2=4096
        CALL BREAD(XE,F1,ITT,IHOUT,NAOLD,N1,N2,NIR)
          PRINT*,' IMRD=',IMRD,'NIR=',NIR,' NCHAN=',NCHAN
          DO 300 I=1,NCHAN
          F2(I)=F2(I)+FTRUN(F1,I,NIR)
300       CONTINUE
          ENDIF
        PRINT*,' ENTER THE OUTPUT FILE NAME'
        READ(*,'(A)')NAOUT
        STATUS='UNKNOWN'
        CALL MAOPEN(4,NAOUT,STATUS)
        WRITE(4,*)E1,E2,NCHAN
        WRITE(4,'('' DATA FROM '',A20,'' PLUS '',A20)')NA,NAOLD
C        IHEAD(30)='HDAT'
        WRITE(4,'(A80)')ITT(3),IHEAD(1)
        WRITE(4,'(I5,'',HDAT'')')NCHAN
        DO 400 I=1,NCHAN
400     IHOUT(I)=F2(I)+.5
        WRITE(4,'(Z7,9Z8)')(IHOUT(I),I=1,NCHAN)
        CLOSE(4)
        RETURN
        END
        FUNCTION FTRUN(F1,JK,IM)
        DIMENSION F1(1)
        FTRUN=0
        IF(JK.GT.0.AND.JK.LT.IM)FTRUN=F1(JK)
        RETURN
        END
        FUNCTION CHAN(E,ECON,EPPCCI,JM,CG)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION ECON(1),EPPCCI(1),P(40)
        EXTERNAL EPOLY
        EX=0
        CHAN=CG
        NLOOP=0
10      CALL FAEFA(CHAN,EX,ECON,EPPCCI,JM,P,EPOLY,EC,EDER,ERR)
        IF(DABS((E-EC)/DMAX1(E,1.D0)).LT.1.D-8)GOTO 30
        CHAN=CHAN-(EC-E)/EDER
        NLOOP=NLOOP+1
        IF(NLOOP.LT.100)GOTO 10
        WRITE(*,1935)NLOOP,E,EC,EDER
1935    FORMAT(' CHAN NOT SOLUBLE IN DILUTE MATH, E, EC, EDER',
     # I5,3E20.6)
        READ(*,*)ITEST
        STOP
30      CHAN=DMAX1(1D-7,CHAN)
        RETURN
        END
        SUBROUTINE EFIT(CONS,PC,PPCCI,PPCC,P,XC,EXC,EC,EEC,N,JM)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION CONS(1),PC(1),PPCCI(1),PPCC(1),P(1),XC(1),EXC(1),
     2 EC(1),EEC(1),SM(40)
C *** INITIAL ENERGY CONSTANT ESTIMATION USES A STRAIGHT LINE BETWEEN
C *** THE MOST ACCURATE TWO POINTS
        XS=1D32
        X2S=1D31
        IS=0
        DO 10 I=1,N
        IF(EXC(I).GT.X2S)GOTO 10
        IF(EXC(I).LT.XS)THEN
          X2S=XS
          I2S=IS
          XS=EXC(I)
          IS=I
        ELSE
          X2S=EXC(I)
          I2S=I
        ENDIF
10      CONTINUE
        PRINT*,' IN EFIT AFTER DO 10'
        CONS(2)=(EC(I2S)-EC(IS))/(XC(I2S)-XC(IS))
        CONS(1)=EC(IS)-CONS(2)*XC(IS)
        IPP=1
        JM=2
        SM(1)=1
        SM(2)=1
22      CHL=1.D32
        CHB=1.D31
        FR=.95
        JMD=JM*(JM+1)/2
25      CONTINUE
        CHI=0
        DO 30 I=1,JM
30      PC(I)=0
        DO 35 I=1,JMD
35      PPCC(I)=0
        RES2B=0
        DO 200 I=1,N
        CALL EPOLY(XC(I),P,JM,CONS,FA,DFA)
        W=1/(EEC(I)*EEC(I)+(DFA*EXC(I))**2)
        ERR=(EC(I)-FA)
        RES2=ERR*ERR*W
        IF(RES2.GT.RES2B)THEN
          IR2B=I
          RES2B=RES2
        ENDIF
        CHI=CHI+RES2
        K=0
        DO 130 J=1,JM
        PC(J)=PC(J)-2*ERR*W*P(J)
        DO 130 L=1,J
        K=K+1
130     PPCC(K)=PPCC(K)+2*W*P(J)*P(L)
200     CONTINUE
        IF((CHI-CHL.LT.1.D-6*CHL.AND.CHL-CHB.LT.1.D-6*CHB)
     2    .OR.CHI.LT.1.D0)THEN
        PRINT*,' CHI-RES2B=',CHI-RES2B,' WITH ',N,' DATA POINTS'
        PRINT'(A/(4G16.6))',' CONSTANTS ARE',(CONS(I),I=1,JM)
        PRINT*,' ENTER A 1 TO ADD MORE CONSTANTS -1 TO EXIT FIT '
        READ(*,*)ITEST
C *** HAVE FOUND A MINUMUM WITH JM CONS
         IF(ITEST.EQ.-1)THEN
           JMT=JM
           DO 220 I=1,JMD
220        PPCCI(I)=PPCC(I)
C *** DO NOT EXPECT TO BE ABLE TO SEE KNOT PARTS
          K=0
          DO 222 I=1,JM
          K=K+I
          IF(I.LT.3.OR.I.NE.2*(I/2))GOTO 222
          PPCCI(K)=1.D20
222       CONTINUE
223        CALL SMINV(PPCCI,JMT,IFL)
           IF(IFL.EQ.-1)THEN
             PRINT*,' NON INVERTABLE MATRIX ENTER A 1 TO REMOVE CONS'
             DO 225 I=1,JMD
225          PPCCI(I)=0
             JMT=JMT-1
             JMTD=JMT*(JMT+1)/2
             DO 228 I=1,JMTD
228          PPCCI(I)=PPCC(I)
             READ(*,*)ITEST
             IF(ITEST.EQ.1)GOTO 223
           ENDIF
           PRINT*,' PPCCI'
           PRINT'(4G20.6)',(PPCCI(I),I=1,JMD)
           AMULT=DMAX1(CHI/(N-JM+.1D-7),1.D0)
           DO 230 I=1,JMD
230        PPCCI(I)=AMULT*PPCCI(I)
           RETURN
         ELSE
           PRINT'(A/(4G20.6))',' MINIMUM CONS',(CONS(I),I=1,JM)
           CONS(JM+1)=0
           SM(JM+1)=1
           NU=MIN0(N,IR2B+2)
           JM=JM+2
           SM(JM)=1.D6
           CONS(JM)=XC(NU)
           PRINT*,' CONS(JM)',CONS(JM)
           GOTO 22
         ENDIF
        ENDIF
        CALL SMSQ(CHI,CHB,CHL,PC,PPCC,PPCCI,FR,CONS,SM,JM,IPP)
        GOTO 25
        END
        SUBROUTINE EPOLY(X,P,JM,CONS,FA,DFA)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION P(1),CONS(1)
        P(1)=1
        P(2)=X
        FA=CONS(1)+X*CONS(2)
        DFA=CONS(2)
        DO 20 I=3,JM,2
        P(I)=0
        P(I+1)=0
        XM=CONS(I+1)-X
        IF(XM.GT.0)THEN
          P(I)=XM*XM
          P(I+1)=CONS(I)*2*XM
          FA=FA+CONS(I)*P(I)
          DFA=DFA+2*CONS(I)*XM
        ENDIF
20      CONTINUE
        RETURN
        END
        SUBROUTINE EFPOLY(X,P,JM,CONS,FA,DFA)
        IMPLICIT REAL*8 (A-H,O-Z)
C *** STRC*FA=STR
        DIMENSION P(1),CONS(1)
        P(1)=1
        P(2)=X
        FA=CONS(1)+X*CONS(2)
        DFA=CONS(2)
        DO 20 I=3,JM,2
        P(I)=0
        P(I+1)=0
        XM=CONS(I+1)-X
        IF(XM.GT.0)THEN
          P(I)=XM*XM
          P(I+1)=CONS(I)*2*XM
          FA=FA+CONS(I)*P(I)
          DFA=DFA+2*CONS(I)*XM
        ENDIF
20      CONTINUE
        FA=DEXP(DMIN1(FA,60.D0))
        DFA=FA*DFA
        DO 25 I=1,JM
25      P(I)=P(I)*FA
        RETURN
        END
        SUBROUTINE EFFFIT(CONS,PC,PPCCI,PPCC,P,XC,EXC,STR,ESTR,
     2 STRC,ESTRC,N,JM)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION CONS(1),PC(1),PPCCI(1),PPCC(1),P(1),XC(1),EXC(1),
     2 STR(1),ESTR(1),STRC(1),ESTRC(1),SM(40)
C *** INITIAL ENERGY CONSTANT ESTIMATION USES A STRAIGHT LINE BETWEEN
C *** THE MOST ACCURATE TWO POINTS
        ANUM=0
        ADEN=0
        DO 10 I=1,N
        W=(STR(I)/ESTR(I))**2
        ANUM=ANUM+W*STR(I)/STRC(I)
        ADEN=ADEN+W
10      CONTINUE
        CONS(1)=DLOG(ANUM/ADEN)
        CONS(2)=0
        IPP=1
        JM=2
        SM(1)=1
        SM(2)=1
22      CHL=1.D32
        CHB=1.D31
        FR=.95
        JMD=JM*(JM+1)/2
25      CONTINUE
        CHI=0
        DO 30 I=1,JM
30      PC(I)=0
        DO 35 I=1,JMD
35      PPCC(I)=0
        RES2B=0
        DO 200 I=1,N
        CALL EFPOLY(XC(I),P,JM,CONS,FA,DFA)
        W=1/((ESTR(I)/STRC(I))**2+(ESTRC(I)*STR(I)/(STRC(I)*STRC(I)))**2
     2 +(DFA*EXC(I))**2)
        ERR=(STR(I)/STRC(I)-FA)
        RES2=ERR*ERR*W
        IF(RES2.GT.RES2B)THEN
          IR2B=I
          RES2B=RES2
        ENDIF
        CHI=CHI+RES2
        K=0
        DO 130 J=1,JM
        PC(J)=PC(J)-2*ERR*W*P(J)
        DO 130 L=1,J
        K=K+1
130     PPCC(K)=PPCC(K)+2*W*P(J)*P(L)
200     CONTINUE
        IF((CHI-CHL.LT.1.D-6*CHL.AND.CHL-CHB.LT.1.D-6*CHB)
     2    .OR.CHI.LT.1.D0)THEN
        PRINT*,' CHI-RES2B=',CHI-RES2B,' WITH ',N,' DATA POINTS'
        PRINT'(A/(4G16.6))',' CONSTANTS ARE',(CONS(I),I=1,JM)
        PRINT*,' ENTER A 1 TO ADD MORE CONSTANTS -1 TO EXIT FIT '
        READ(*,*)ITEST
C *** HAVE FOUND A MINUMUM WITH JM CONS
         IF(ITEST.EQ.-1)THEN
           JMT=JM
           DO 220 I=1,JMD
220        PPCCI(I)=PPCC(I)
C *** DO NOT EXPECT TO BE ABLE TO SEE KNOT PARTS
          K=0
          DO 222 I=1,JM
          K=K+I
          IF(I.LT.3.OR.I.NE.2*(I/2))GOTO 222
          PPCCI(K)=1.D20
222       CONTINUE
223        CALL SMINV(PPCCI,JMT,IFL)
           IF(IFL.EQ.-1)THEN
             PRINT*,' NON INVERTABLE MATRIX ENTER A 1 TO REMOVE CONS'
             DO 225 I=1,JMD
225          PPCCI(I)=0
             JMT=JMT-1
             JMTD=JMT*(JMT+1)/2
             DO 228 I=1,JMTD
228          PPCCI(I)=PPCC(I)
             READ(*,*)ITEST
             IF(ITEST.EQ.1)GOTO 223
           ENDIF
           PRINT*,' PPCCI'
           PRINT'(4G20.6)',(PPCCI(I),I=1,JMD)
           AMULT=DMAX1(CHI/(N-JM+.1D-7),1.D0)
           DO 230 I=1,JMD
230        PPCCI(I)=AMULT*PPCCI(I)
           RETURN
         ELSE
           PRINT'(A/(4G20.6))',' MINIMUM CONS',(CONS(I),I=1,JM)
           CONS(JM+1)=0
           SM(JM+1)=1
           NU=MIN0(N,IR2B+2)
           JM=JM+2
           SM(JM)=1.D6
           CONS(JM)=XC(NU)
           PRINT*,' CONS(JM)',CONS(JM)
           GOTO 22
         ENDIF
        ENDIF
C       PRINT'(A/(4G20.6))','CONS ',(CONS(I),I=1,JM)
C       PRINT'(A/(4G20.6))','PC ',(PC(I),I=1,JM)
        CALL SMSQ(CHI,CHB,CHL,PC,PPCC,PPCCI,FR,CONS,SM,JM,IPP)
        GOTO 25
        END
        SUBROUTINE FAEFA(X,EX,CON,EMAT,JM,P,POLY,FA,DFA,EFA)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION CON(1),EMAT(1),P(1)
        CALL POLY(X,P,JM,CON,FA,DFA)
        K=0
        EFA=0
        DO 20 I=1,JM
        K=K+I
20      EFA=EFA-2*EMAT(K)*P(I)*P(I)
        K=0
        DO 30 I=1,JM
        DO 30 J=1,I
        K=K+1
30      EFA=EFA+4*EMAT(K)*P(I)*P(J)
        EFA=DSQRT(EFA+(DFA*EX)**2)
        RETURN
        END
        SUBROUTINE LINPUT(IUNIT,CANS,AANS,N,ICANS,IEND)
C ***  LINPUT READS FIRST N NUMBERS THEN REST OF LINE IS
C *** TRANSFERED AS CHARACTERS
        REAL*8 AANS(40)
        CHARACTER*1 CANS(80),C(80),KNUM(10)
      DATA KNUM/'1','2','3','4','5','6','7','8','9','0'/
        IEND=1
        DO 8 I=1,40
8       AANS(I)=0
        DO 10 I=1,80
10      CANS(I)=' '
        READ(IUNIT,'(80A1)',END=150)C
        IEND=0
        PRINT'(80A1)',C
        INUM=0
        ICANS=0
        IAANS=0
        IDPFL=0
        ISFLAG=0
        NDP=0
        NUMF=0
        ANUM=0
      DO 110 I=1,79
        IF((C(I).EQ.' '.OR.C(I).EQ.',').AND.INUM.LT.N)GOTO 90
        IF(INUM.EQ.N)THEN
        IF(C(I).EQ.' '.AND.C(I+1).EQ.' ')GOTO 90
        ICANS=ICANS+1
        IF(ICANS.GT.80)GOTO 150
        CANS(ICANS)=C(I)
        GOTO 100
        ENDIF
        IF(C(I).EQ.'.')THEN
         IDPFL=1
         GOTO 100
        ENDIF
        IF(C(I).EQ.'-')THEN
         ISFLAG=1.
         GOTO 100
        ENDIF
        DO 30 J=1,10
        IF(C(I).NE.KNUM(J))GOTO 30
        IF(IDPFL.EQ.1.)NDP=NDP+1
          NUMF=1
          IF(J.NE.10)THEN
           ANUM=ANUM*10+J
          ELSE
           ANUM=ANUM*10
          ENDIF
        GOTO 100
 30     CONTINUE
90    CONTINUE
        IF(NUMF.EQ.1)THEN
          INUM=INUM+1
          IF(ISFLAG.EQ.1.)ANUM=-ANUM
          DO 95 J=1,NDP
          ANUM=ANUM/10
95        CONTINUE
          AANS(INUM)=ANUM
          ANUM=0
         ENDIF
        IDPFL=0
        ISFLAG=0
        NDP=0
        NUMF=0
100   CONTINUE
110   CONTINUE
150   RETURN
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
C*INCLUDE MENURD.FOR
C*INCLUDE TCHREAD.FOR
C*INCLUDE BREAD.FOR
C*INCLUDE BSMSQ.FOR
C*INCLUDE MAOPEN.FOR
C$INCLUDE MENURD
C$INCLUDE TCHREAD
C$INCLUDE BREAD
C$INCLUDE BSMSQ
C$INCLUDE MAOPEN
