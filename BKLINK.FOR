      SUBROUTINE BKLINK(NA)
      DIMENSION FFCOM(200),FFVAR(200)
      CHARACTER*64 CSTR,VALU,NA
      CHARACTER*64 NADIRM,NADIRD,NADIRC,STATUS
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/ NSTR,NCMENU
      CHARACTER*40 FFCOM,FFVAR
C*********************************************************
C
C DATA TO MAIN ROUTINE
C
C**********************************************************
C
C      VBKGFIT DATA
C
C***********************************************************
      REAL*4 CUT
      COMMON/CVBKGF/NVM,CUT
C******************************************************************
C      BREAD DATA
C******************************************************************
      CHARACTER*64 DATAFN,NADIFF
      COMMON/CHBREA/DATAFN,NADIFF
      COMMON/CBREAD/IBEGC,IENDC,IEXTWT
C******************************************************************
C      BKGFIT DATA
C******************************************************************
      CHARACTER*64 IBKGFN
      CHARACTER*4 BKGF
      COMMON/CHBKGF/IBKGFN
      COMMON/CBKGFI/IBKGFL,BKGF
C*****************************************************************
C      VBKGFIT FILE DATA
C***********************************************************
      CHARACTER*64 NABKG,NAGR
      COMMON/CHVBKG/NABKG,NAGR
C*****************************************************************
      NCMENU=1
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
      NA='BKGFIT.MNU'
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\BKGFIT.MNU'
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')RETURN
      DO 10 J=1,10
        NS=J*20+2
        IF(J.EQ.1) THEN
C1*******************************
        NS=NS+1
        CSTR=FFVAR(NS)
        CALL CTON(CSTR,IMISS,ANUM)
        NVM=ANUM
C       PRINT*,'NVM=',NVM
        NS=NS+1
        CSTR=FFVAR(NS)
        CALL CTON(CSTR,IMISS,ANUM)
        CUT=ANUM
        NS=NS+1
        DATAFN=FFVAR(NS)
      IF(NCD.GT.0)DATAFN=NADIRD(1:NCD)//'\'//FFVAR(NS)
C       PRINT*,'DATAFN=',DATAFN
        NS=NS+1
        IT=INDEX(FFVAR(NS),',')
        IF(FFVAR(NS)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NS)(IT+1:IT+3).EQ.'def')THEN
          IBEGC=1
          IENDC=4096
        ELSE
          CSTR=FFVAR(NS)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IBEGC=ANUM
          CSTR=FFVAR(NS)(IT+1:37)
          CALL CTON(CSTR,IMISS,ANUM)
          IENDC=ANUM
        ENDIF
C        PRINT*,'IBEG,IEND=',IBEGC,IENDC
        NS=NS+1
        CSTR=FFVAR(NS)
        IBKGFN=CSTR
        IF(NCC.GT.0.AND.CSTR.NE.'NONE')
     2    IBKGFN=NADIRC(1:NCC)//'\'//CSTR
        NS=NS+1
        CSTR=FFVAR(NS)
        IF(CSTR(1:1).EQ.'Y'.OR.CSTR(1:1).EQ.'y')THEN
          IBKGFL=1
        ELSE
          IBKGFL=0
        ENDIF
C       PRINT*,'IBKGFN,FLAG',IBKGFN,IBKGFL
        NS=NS+1
        CSTR=FFVAR(NS)
        CALL CTON(CSTR,IMISS,ANUM)
        IEXTWT=ANUM
C     PRINT*,'IEXTWT=',IEXTWT
        NS=NS+1
        NADIFF=FFVAR(NS)
      IF(NCD.GT.0)NADIFF=NADIRM(1:NCD)//'\'//FFVAR(NS)
C     PRINT*,'WEIGHTS FILE IS',NADIFF
      NS=NS+1
      BKGF=FFVAR(NS)(1:4)
C1**********************************
      ELSE IF(J.EQ.2) THEN
C2***********************************
        NS=NS+1
        NABKG=FFVAR(NS)
        IF(NCC.GT.0)NABKG=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        NAGR=FFVAR(NS)
        IF(NCC.GT.0)NAGR=NADIRC(1:NCC)//'\'//FFVAR(NS)
      ENDIF
10    CONTINUE
      RETURN
      END
