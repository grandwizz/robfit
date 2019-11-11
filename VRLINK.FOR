      SUBROUTINE VRLINK(NAMENU)
      DIMENSION FFVAR(160)
      CHARACTER*64 NADIRM,NADIRD,NADIRC,STATUS
      CHARACTER*64 CSTR,VALU,NA,NAMENU
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/NSTR,NCMENU
      CHARACTER*40 FFVAR
C*********************************************************
C
C DATA TO MAIN ROUTINE VROBFIT
C
C**********************************************************
C
C      VROBFIT DATA
C
C***********************************************************
      REAL*8 WID,ERRO,CUTA,CUTB
      DIMENSION WID(20),ERRO(20),ICHAN(20)
      CHARACTER*64 NABKG,NAPK,NAGR
      CHARACTER*4 IFW,BKGF,NOGP
      COMMON/CHVROB/BKGF,IFW,NOGP,NABKG,NAPK,NAGR
      COMMON/CVROB/WID,ERRO,CUTA,CUTB,ICHAN,NPPMAX,IMREF,NBMAX,NBMIN,
     # NPEAKT
C*****************************************************************
C      POLYG DATA
C******************************************************************
      DIMENSION STAN(5)
      CHARACTER*64 STAN
      COMMON/CHPOLY/STAN
      COMMON/CPOLYG/NSTAN
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
      COMMON/CHBKGF/IBKGFN
      COMMON/CBKGFI/IBKGFL
C******************************************************************
C      ROPKS DATA
C******************************************************************
      CHARACTER*64 IPEKFN
      CHARACTER*1 APH
      COMMON/CHROPK/IPEKFN,APH
C*****************************************************************
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
      NA=NAMENU
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\'//NAMENU(1:17)
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')THEN
        NAMENU='STOP'
        RETURN
      ENDIF
      DO 10 J=1,10
        NS=J*20+2
        IF(J.EQ.1) THEN
C1*******************************
        NS=NS+1
        CSTR=FFVAR(NS)
        CALL CTON(CSTR,IMISS,ANUM)
        NPPMAX=ANUM
        NS=NS+1
        IT=INDEX(FFVAR(NS),',')
        CSTR=FFVAR(NS)(1:IT-1)
        CALL CTON(CSTR,IMISS,ANUM)
        CUTA=ANUM
        VALU=FFVAR(NS)(IT+1:37)
        IT=INDEX(VALU,',')
        CSTR=VALU(1:IT-1)
        CALL CTON(CSTR,IMISS,ANUM)
        CUTB=ANUM
        CSTR=VALU(IT+1:37)
        CALL CTON(CSTR,IMISS,ANUM)
        IMREF=ANUM
        NS=NS+1
        CSTR=FFVAR(NS)
        IFW=CSTR
        NS=NS+1
        CSTR=FFVAR(NS)
        BKGF=CSTR
        NS=NS+1
           IT=INDEX(FFVAR(NS),',')
        CSTR=FFVAR(NS)(1:IT-1)
        CALL CTON(CSTR,IMISS,ANUM)
        NBMIN=ANUM
          CSTR=FFVAR(NS)(IT+1:37)
          CALL CTON(CSTR,IMISS,ANUM)
       NBMAX=ANUM
        NOGP='GP'
        NS=NS+1
        CSTR=FFVAR(NS)
        CALL CTON(CSTR,IMISS,ANUM)
        IEXTWT=ANUM
        NS=NS+1
        NADIFF=FFVAR(NS)
        IF(NCD.GT.0.AND.NADIFF.NE.'NONE')
     2     NADIFF=NADIRD(1:NCD)//'\'//FFVAR(NS)
C1*******************************
        ELSE IF(J.EQ.2) THEN
C2**********************************
        NS=NS+1
        DATAFN=FFVAR(NS)
        IF(NCD.GT.0)DATAFN=NADIRD(1:NCD)//'\'//FFVAR(NS)
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
        NS=NS+1
        CSTR=FFVAR(NS)
        IPEKFN=CSTR
        IF(NCC.GT.0.AND.CSTR.NE.'NONE')
     2      IPEKFN=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        APH=FFVAR(NS)
        NS=NS+1
        CSTR=FFVAR(NS)
        IBKGFN=CSTR
        IF(NCC.GT.0.AND.CSTR.NE.'NONE')
     2    IBKGFN=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        CSTR=FFVAR(NS)
        IF(CSTR(1:1).EQ.'Y'.OR.CSTR(1:1).EQ.'y')THEN
          IBKGFL=1
        ELSE
          IBKGFL=0
        ENDIF
C2**********************************
      ELSE IF(J.EQ.3) THEN
C3**********************************
        NS=NS+1
        NABKG=FFVAR(NS)
        IF(NCC.GT.0)NABKG=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        NAPK=FFVAR(NS)
        IF(NCC.GT.0)NAPK=NADIRC(1:NCC)//'\'//FFVAR(NS)
        NS=NS+1
        NAGR=FFVAR(NS)
        IF(NCC.GT.0)NAGR=NADIRC(1:NCC)//'\'//FFVAR(NS)
C3**********************************
      ELSE IF(J.EQ.4) THEN
C4**********************************
           NSTAN=0
           NPEAKT=0
           IV=0
           DO 20 I=1,5
             NS=NS+1
             IF(FFVAR(NS)(1:1).EQ.' ')GOTO 20
             STAN(I)=FFVAR(NS)
             IF(NCC.GT.0)STAN(I)=NADIRC(1:NCC)//'\'//FFVAR(NS)
             NSTAN=NSTAN+1
             NPEAKT=NPEAKT+1
        DO 30 II=1,2
          IV=IV+1
          NS=NS+1
        IT=INDEX(FFVAR(NS),',')
        CSTR=FFVAR(NS)(1:IT-1)
        CALL CTON(CSTR,IMISS,ANUM)
        WID(IV)=ANUM
        VALU=FFVAR(NS)(IT+1:37)
        IT=INDEX(VALU,',')
        CSTR=VALU(1:IT-1)
        CALL CTON(CSTR,IMISS,ANUM)
        ERRO(IV)=ANUM
        CSTR=VALU(IT+1:37)
        CALL CTON(CSTR,IMISS,ANUM)
        ICHAN(IV)=ANUM
30      CONTINUE
20         CONTINUE
C4**********************************
      ENDIF
10    CONTINUE
      RETURN
      END
