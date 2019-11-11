      SUBROUTINE GSLINK(NAMENU)
      DIMENSION FFCOM(600),FFVAR(600)
      CHARACTER*64 CSTR,VALU,NA,NAMENU
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/NSTR,NCMENU
      COMMON/CGPH/NCOLOR
      CHARACTER*40 FFCOM,FFVAR
C*************************************************
C
C   DATA TO MAIN ROUTINE GRASHAPE
C
C*************************************************
C
C   GRASHAPE DATA
C
C*************************************************
      CHARACTER*64 NADIRM,NADIRD,NADIRC,STATUS
      CHARACTER*64 GDNAME
      CHARACTER*1 VCF,VSP,VBG
      COMMON/CHGRAS/GDNAME,VCF,VSP,VBG
      COMMON/CGRASH/ABEGC,AENDC,IMIN,IMAX,LFLAG
C*************************************************
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
      CALL CCHAR(NADIRC,NCC)
      NA=NAMENU
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\'//NAMENU(1:17)
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')THEN
        NAMENU='STOP'
        RETURN
      ENDIF
      DO 10 J=1,10
        NSS=J*20+2
        IF(J.EQ.1)THEN
C*************************************************
      NSS=NSS+1
      GDNAME=FFVAR(NSS)
      IF(NCC.GT.0)GDNAME=NADIRC(1:NCC)//'\'//FFVAR(NSS)
      NSS=NSS+1
        IT=INDEX(FFVAR(NSS),',')
        IF(FFVAR(NSS)(IT+1:IT+3).EQ.'DEF'.OR.FFVAR(NSS)(
     1IT+1:IT+3).EQ.'def')THEN
          ABEGC=0
          AENDC=0
        ELSE
          CSTR=FFVAR(NSS)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          ABEGC=ANUM
          CSTR=FFVAR(NSS)(IT+1:37)
          CALL CTON(CSTR,IMISS,ANUM)
          AENDC=ANUM
        ENDIF
        NSS=NSS+1
        IT=INDEX(FFVAR(NSS),',')
        IF(FFVAR(NSS)(IT+1:IT+3).EQ.'DEF'.OR.FFVAR(NSS)(
     1IT+1:IT+3).EQ.'def')THEN
          IMIN=0
          IMAX=0
        ELSE
          CSTR=FFVAR(NSS)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IMIN=ANUM
          CSTR=FFVAR(NSS)(IT+1:37)
          CALL CTON(CSTR,IMISS,ANUM)
          IMAX=ANUM
        ENDIF
      NSS=NSS+1
          CSTR=FFVAR(NSS)(1:3)
          IF(CSTR.EQ.'LOG'.OR.CSTR.EQ.'log')THEN
            LFLAG=1
          ELSE
            LFLAG=0
          ENDIF
          NSS=NSS+1
          NCOLOR=1
          IF(FFVAR(NSS)(1:1).EQ.'Y'.OR.FFVAR(NSS)(1:1).EQ.'y')NCOLOR=0
      ENDIF
C*************************************
      IF(J.EQ.2) THEN
C*************************************
        NSS=NSS+1
        VCF=FFVAR(NSS)(1:1)
        NSS=NSS+1
        VSP=FFVAR(NSS)(1:1)
        NSS=NSS+1
        VBG=FFVAR(NSS)(1:1)
C******************************
      ENDIF
10    CONTINUE
        RETURN
        END
