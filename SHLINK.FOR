      SUBROUTINE SHLINK(NA)
      DIMENSION FFCOM(200),FFVAR(200)
      CHARACTER*64 NADIRM,NADIRD,NADIRC,STATUS
      CHARACTER*64 CSTR,VALU,NA
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/NSTR,NCMENU
      CHARACTER*40 FFCOM,FFVAR
C*************************************************
C
C   DATA TO MAIN ROUTINE VSHAPE
C
C*************************************************
C
C   VSHAPE DATA
C
C*************************************************
      CHARACTER*64 STNAME,TYNAME,GRNAME
      COMMON/CHVSHA/STNAME,TYNAME,GRNAME
      COMMON/CVSHAP/NBB,NS
C*************************************************
C   BREAD DATA
C*************************************************
      CHARACTER*64 DATAFN
      COMMON/CHBREA/DATAFN
      COMMON/CBREAD/IBEGC,IENDC
C*************************************************
C   VOIGHT DATA
C*************************************************
      COMMON/CVOIGH/ETAA
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
      IF(NADIRM.EQ.'CMS'.OR.NADIRM.EQ.'MVS'.OR.NADIRM.EQ.'WATFOR')THEN
        NCM=0
        NCC=0
      ENDIF
      NA='STGEN.MNU'
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\STGEN.MNU'
C*************************************************
      NCMENU=1
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')RETURN
      DO 10 J=1,10
        NSS=J*20+2
        IF(J.EQ.1)THEN
C*************************************************
      NSS=NSS+1
      STNAME=FFVAR(NSS)
      IF(NCC.GT.0)STNAME=NADIRC(1:NCC)//'\'//FFVAR(NSS)
      NSS=NSS+1
      GRNAME=FFVAR(NSS)
      IF(NCC.GT.0)GRNAME=NADIRC(1:NCC)//'\'//FFVAR(NSS)
      NSS=NSS+1
      TYNAME=FFVAR(NSS)
      NSS=NSS+1
      IT=INDEX(FFVAR(NSS),',')
      CSTR=FFVAR(NSS)(1:IT-1)
      CALL CTON(CSTR,IMISS,ANUM)
      NBB=ANUM
      CSTR=FFVAR(NSS)(IT+1:37)
      CALL CTON(CSTR,IMISS,ANUM)
      NS=ANUM
C     PRINT*,'NBB,NS=',NBB,NS
C*******************************
        ENDIF
        IF(J.EQ.2)THEN
C*******************************
      NSS=NSS+1
      CSTR=FFVAR(NSS)
      CALL CTON(CSTR,IMISS,ANUM)
      ETAA=ANUM
C     PRINT*,'ETAA=',ETAA
      NSS=NSS+1
        DATAFN=FFVAR(NSS)
        IF(NCD.GT.0)DATAFN=NADIRD(1:NCD)//'\'//FFVAR(NSS)
C       PRINT*,'DATAFN=',DATAFN
        NSS=NSS+1
        IT=INDEX(FFVAR(NSS),',')
        IF(FFVAR(NSS)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NSS)(IT+1:IT+3).EQ.'def')THEN
          IBEGC=0
          IENDC=0
        ELSE
          CSTR=FFVAR(NSS)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IBEGC=ANUM
          CSTR=FFVAR(NSS)(IT+1:37)
          CALL CTON(CSTR,IMISS,ANUM)
          IENDC=ANUM
        ENDIF
C        PRINT*,'IBEG,IEND=',IBEGC,IENDC
C*******************************
        ENDIF
10    CONTINUE
        RETURN
        END
