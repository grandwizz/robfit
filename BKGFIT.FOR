C$NOWAR
C$NOEXT
      PROGRAM VBKGFI
        IMPLICIT REAL *8 (A-H,O-Z)
        REAL*4 X,F,WX,FA
        CHARACTER*1 ANS
        CHARACTER*8 TITLE
        CHARACTER*64 STATUS
        CHARACTER*64 NA
        DIMENSION CONS(120)
        DIMENSION FA(4096)
        DIMENSION  F(4096),WX(4096)
        DIMENSION IFDT(4096)
        CHARACTER*80 II(4)
        EQUIVALENCE (WX(1),IFDT(1))
C**********************************************************
C
C      VBKGFIT DATA
C
C***********************************************************
C     REAL*4 CUT
      COMMON/CVBKGF/NVM,CUT
      COMMON/COUNTR/ICSMIN
C******************************************************************
C   BREAD DATA
C**********************************************************
      CHARACTER*64 DATAFN,NADIFF
      COMMON/CHBREA/DATAFN,NADIFF
      COMMON/CBREAD/IBEGC,IENDC,IEXTWT
      COMMON/BATCFL/IBATCH
C*****************************************************************
C      VBKGFIT FILE DATA
C***********************************************************
      CHARACTER*64 NABKG,NAGR
      COMMON/CHVBKG/NABKG,NAGR
      CHARACTER*64 FNAME
      CHARACTER*4 BKGF
      COMMON/CHBKGF/FNAME
      COMMON/CBKGFI/IBKGFL,BKGF
        DATA CONS/120*0.D0/
        DATA NE/0/,ICONT/0/
      IBATCH=0
2     CONTINUE
      DO 3 I=1,4096
3     FA(I)=0
      ICSMIN=0
      NV=0
C        CALL ERRSET(208,256,-1,1)
15      CALL BKLINK(NA)
        IF(NA.EQ.'STOP')STOP
C M       IF(NA.EQ.'STOP')RETURN
        LFLAG=IBKGFL
20    CALL BREAD(X,F,II,IFDT,DATAFN,IBEGC,IENDC,N)
        IF(IEXTWT.GT.2.OR.IEXTWT.LT.0)THEN
          PRINT*,' IEXTWT=',IEXTWT,' NOT ALLOWED, TRY AGAIN'
          GOTO 15
        ENDIF
        IF(IEXTWT.EQ.2)THEN
          CALL BREAD(X,F,II,IFDT,NADIFF,IBEGC,IENDC,N)
        ELSE
          CALL WXCALC(F,FA,WX,N,IEXTWT)
        ENDIF
         NCALL=0
        PRINT*,' BEFORE BKGFIT CALL NV, NVM',NV,NVM
        PRINT*,' BEFORE CALLING BKGFIT BKGF=',BKGF
25      CALL BKGFIT(X,F,FA,WX,N,CONS,NV,NVM,BKGF,LFLAG,NCALL,CUT,FNAME)
        PRINT*,' AFTER BKGFIT CALL NV,NVM',NV,NVM
        STATUS='UNKNOWN'
        CALL MAOPEN(2,NABKG,STATUS)
         WRITE(2,*)LFLAG
        WRITE(2,'(G20.12)')(CONS(I),I=1,NV)
        CLOSE(UNIT=2)
        CHIS=0
        DO 30 I=1,N
        CHIS=CHIS+(F(I)-FA(I))**2*WX(I)
30      CONTINUE
        PRINT*,' CHIS=',CHIS
C       CALL CMS('FILEDEF 3 DISK BKGCONS GR',IERR)
C*********************************************************
C  OPEN THE GRAPHICS OUTPUT FILE
C*********************************************************
      STATUS='UNKNOWN'
      CALL MAOPEN(3,NAGR,STATUS)
      IT1=MAX0(5,INDEX(DATAFN,'.'))
      IT1B=MAX0(1,IT1-7)
      TITLE=DATAFN(IT1B:IT1B+7)
      WRITE(3,'(A,G20.12)')TITLE,CHIS
      IP=0
      WRITE(3,'(2I5)')IP
C*********************************************************
C  WRITE THE RAW DATA PARAMETERS FOR FIT
C*********************************************************
      WRITE(3,'(I4/(A64))')IEXTWT,DATAFN,NADIFF,NABKG,NABKG
      WRITE(3,'(2I5)')IBEGC,IENDC
      CLOSE(3)
C****************************************************
         IF(NV.LT.NVM)GOTO 25
        IF(IBATCH.EQ.3)GOTO 2
        STOP
C M        RETURN
         END
      SUBROUTINE LOCATE(X,R,NMAX,J)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION R(1)
      JL=0
      JU=NMAX+1
10    IF(JU-JL.LE.1)GOTO 20
      JM=(JU+JL)/2
      IF(X.GT.R(JM))THEN
      JL=JM
      ELSE
      JU=JM
      ENDIF
      GOTO 10
20    J=JL
      RETURN
      END
C*INCLUDE C:\ROBFIT\FORTRAN\MENURD
C*INCLUDE C:\ROBFIT\FORTRAN\TCHREAD
C*INCLUDE C:\ROBFIT\FORTRAN\BSMSQ
C*INCLUDE C:\ROBFIT\FORTRAN\MACBKG
C*INCLUDE C:\ROBFIT\FORTRAN\BREAD
C*INCLUDE C:\ROBFIT\FORTRAN\MAOPEN
C*INCLUDE C:\ROBFIT\FORTRAN\BKLINK
C$INCLUDE MENURD
C$INCLUDE TCHREAD
C$INCLUDE BSMSQ
C$INCLUDE MACBKG
C$INCLUDE BREAD
C$INCLUDE MAOPEN
C$INCLUDE BKLINK
