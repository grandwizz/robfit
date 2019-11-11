      SUBROUTINE G3LINK(NAMENU)
C *** THE DOS VERSION HAS '\' TO CONCATENATE FILES WITH THE MENU
C *** THE MAC VERSION HAS ':' USE SEARCH TO CHANGE THESE
      CHARACTER*64 CSTR,VALU,NA,NAMENU
      CHARACTER*40 FFCOM(200),FFVAR(200)
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/NSTR,NCMENU
C*************************************************
C
C   DATA TO MAIN ROUTINE GRAF3
C
C*************************************************
C
C   GRAF3 DATA
C
C*************************************************
      CHARACTER*64 GDNAME,GENAME
      CHARACTER*1 VCF,VPE,VBG,VRE
      COMMON/CHGRAS/GDNAME,GENAME,VCF,VPE,VBG,VRE
      COMMON/CGRASH/IBEGC,IENDC,IMIN,IMAX,SCFACT,LFLAG
      COMMON/CBREAD/IB,IE,IEXTWT
      COMMON/CGPH/NCOLOR
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
      CALL CCHAR(NADIRC,NCC)
      NA=NAMENU
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\'//NAMENU
      NCMENU=1
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')THEN
        NAMENU='STOP'
        RETURN
      ENDIF
      IF(NCM.GT.0)NAMENU=NA(NCM+2:)
      DO 10 J=1,10
        NSS=J*20+2
        IF(J.EQ.1)THEN
C*************************************************
      NSS=NSS+1
      GDNAME=FFVAR(NSS)
      IF(NCC.GT.0)GDNAME=NADIRC(1:NCM)//'\'//FFVAR(NSS)
      NSS=NSS+1
      GENAME=FFVAR(NSS)
      IF(NCC.GT.0.AND.GENAME.NE.'NONE')
     2 GENAME=NADIRC(1:NCM)//'\'//FFVAR(NSS)
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
      NSS=NSS+1
      IP=1
      SCFACT=RNUMB(FFVAR(NSS),IP,40)
        NSS=NSS+1
        IT=INDEX(FFVAR(NSS),',')
        IF(FFVAR(NSS)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NSS)(IT+1:IT+3).EQ.'def')THEN
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
        VRE=FFVAR(NSS)(1:1)
        NSS=NSS+1
        VCF=FFVAR(NSS)(1:1)
        NSS=NSS+1
        VPE=FFVAR(NSS)(1:1)
        NSS=NSS+1
        VBG=FFVAR(NSS)(1:1)
C******************************
      ENDIF
10    CONTINUE
        RETURN
        END
