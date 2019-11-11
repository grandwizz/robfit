      SUBROUTINE G4LINK(NA)
      DIMENSION FFCOM(80),FFVAR(80)
      CHARACTER*64 CSTR,VALU,NA
      DIMENSION UFILE(2),CFILE(2),FBEGC2(2),FENDC2(2),IBEGC2(2),
     #IENDC2(2)
      CHARACTER*64 NADIRM,NADIRD,NADIRC,STATUS
      CHARACTER*64 UFILE,CFILE,XYFILE,FPNAME,FMNAME,TITLE
      COMMON/GLFILE/UFILE,CFILE,XYFILE,FPNAME,FMNAME,TITLE
      COMMON/CGPH/NCOLOR
      COMMON/GLINK/CORR,DIFF,DIFPLT,RAT1,RAT2,FBEGC2,FENDC2,
     #IMIN2,IMAX2,LFLAG2,
     #IBEGC2,IENDC2,
     #IBEGC1,IENDC1,
     #IMIN1,IMAX1,
     #LFLAG1
      CHARACTER*40 FFCOM,FFVAR
      COMMON/USPAGE/FFVAR
      COMMON/FCALL/NSTR,NCMENU
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
      IF(NADIRM.EQ.'CMS'.OR.NADIRM.EQ.'MVS'.OR.NADIRM.EQ.'WATFOR')THEN
        NCM=0
        NCC=0
      ENDIF
      NA='RAWDD.MNU'
      IF(NCM.GT.0)NA=NADIRM(1:NCM)//'\RAWDD.MNU'
      CALL MENURD(NA)
      IF(NA.EQ.'STOP')RETURN
      DO 10 J=1,3
        NS=J*20+2
        IT=INDEX(FFVAR(NS+1),',')
        IF(J.EQ.1) THEN
C1*******************************
          NS=NS+1
          IF(IT.GT.1)THEN
            IF(NCD.GT.0)THEN
              UFILE(1)=NADIRD(1:NCD)//'\'//FFVAR(NS)(1:IT-1)
              UFILE(2)=NADIRD(1:NCD)//'\'//FFVAR(NS)(IT+1:IT+18)
            ELSE
              UFILE(1)=FFVAR(NS)(1:IT-1)
              UFILE(2)=FFVAR(NS)(IT+1:IT+18)
            ENDIF
          ELSE
            UFILE(2)='\'
            IF(NCD.GT.0)THEN
              UFILE(1)=NADIRD(1:NCD)//'\'//FFVAR(NS)
            ELSE
              UFILE(1)=FFVAR(NS)
            ENDIF
          ENDIF
          NS=NS+1
        IT=INDEX(FFVAR(NS),',')
        IF(IT.GT.0)THEN
          CSTR=FFVAR(NS)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IBEGC1=ANUM
        ENDIF
        IF(IT.EQ.0)THEN
          IT=INDEX(FFVAR(NS),' ')
          IBEGC=0
        ENDIF
        IF(FFVAR(NS)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NS)(IT+1:IT+3).EQ.'def') THEN
          IENDC1=4096
        ELSE
          CSTR=FFVAR(NS)(IT+1:IT+18)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          IENDC1=ANUM
        ENDIF
          NS=NS+1
          IT=INDEX(FFVAR(NS),',')
        IF(IT.EQ.0)IT=INDEX(FFVAR(NS),' ')
        IF(FFVAR(NS)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NS)(IT+1:IT+3).EQ.'def') THEN
          IMIN1=0
          IMAX1=0
        ELSE
          CSTR=FFVAR(NS)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IMIN1=ANUM
          CSTR=FFVAR(NS)(IT+1:IT+18)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          IMAX1=ANUM
        ENDIF
          NS=NS+1
          CSTR=FFVAR(NS)(1:3)
          IF(CSTR.EQ.'LOG'.OR.CSTR.EQ.'log')THEN
            LFLAG1=1
          ELSE
            LFLAG1=0
          ENDIF
          NS=NS+1
          NCOLOR=1
          IF(FFVAR(NS)(1:1).EQ.'Y'.OR.FFVAR(NS)(1:1).EQ.'y')NCOLOR=0
          NS=NS+1
          TITLE=FFVAR(NS)(1:17)
C2***************************************************
        ELSE IF(J.EQ.2) THEN
C5*******************************
          NS=NS+1
          CSTR=FFVAR(NS)(1:1)
          DIFF=0
          IF(CSTR.EQ.'Y'.OR.CSTR.EQ.'y')THEN
            DIFF=1
            CSTR=FFVAR(NS+1)(1:1)
            IF(CSTR.EQ.'Y'.OR.CSTR.EQ.'y')THEN
              DIFPLT=1
            ELSE
               DIFPLT=0
            ENDIF
            XYFILE='NULL'
            CSTR=FFVAR(NS+2)(1:17)
            FPNAME=CSTR
            IF(NCD.GT.0)FPNAME=NADIRD(1:NCD)//'\'//CSTR
            CSTR=FFVAR(NS+3)(1:17)
            FMNAME=CSTR
            IF(NCD.GT.0)FMNAME=NADIRD(1:NCD)//'\'//CSTR
        IT=INDEX(FFVAR(NS+4),',')
        IF(IT.EQ.0)IT=INDEX(FFVAR(NS),' ')
        IF(FFVAR(NS+4)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NS+4)(IT+1:IT+3).EQ.'def') THEN
          IMIN2=0
          IMAX2=0
        ELSE
          CSTR=FFVAR(NS+4)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          IMIN2=ANUM
          CSTR=FFVAR(NS+4)(IT+1:IT+18)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          IMAX2=ANUM
        ENDIF
          CSTR=FFVAR(NS+5)(1:3)
          IF(CSTR.EQ.'LOG'.OR.CSTR.EQ.'log')THEN
            LFLAG2=1
          ELSE
            LFLAG2=0
          ENDIF
        IT=INDEX(FFVAR(NS+6),',')
        IF(IT.EQ.0)IT=INDEX(FFVAR(NS),' ')
        IF(FFVAR(NS+6)(IT+1:IT+3).EQ.'DEF'
     1.OR.FFVAR(NS+6)(IT+1:IT+3).EQ.'def') THEN
          RAT1=1
          RAT2=1
        ELSE
          CSTR=FFVAR(NS+6)(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          RAT1=ANUM
          CSTR=FFVAR(NS+6)(IT+1:IT+18)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          RAT2=ANUM
        ENDIF
C     PRINT*,'RAT1,RAT2=',RAT1,RAT2
            ELSE
              DIFPLT=0
              IMIN2=0
              IMAX2=0
              LFLAG2=0
              XYFILE='NULL'
              FPNAME='NULL'
              FMNAME='NULL'
              RAT1=1
              RAT2=1
            ENDIF
C2*******************************
        ELSE IF(J.EQ.3) THEN
C3********************************
          NS=NS+1
          CSTR=FFVAR(NS)(1:1)
          CORR=0
          IF(CSTR.EQ.'Y'.OR.CSTR.EQ.'y')THEN
            CORR=1
C*****************************************************************
C*****************************************************************
           DO 20 I=1,2
          IT=INDEX(FFVAR(NS+1),'/')
          IF(I.EQ.1)THEN
            VALU=FFVAR(NS+1)(1:IT-1)
          ELSE
            VALU=FFVAR(NS+1)(IT+1:37)
          ENDIF
        IT=INDEX(VALU,',')
        IF(IT.EQ.0)IT=INDEX(FFVAR(NS),' ')
        IF(VALU(IT+1:IT+3).EQ.'DEF'.OR.VALU(IT+1:IT+3).EQ.'def') THEN
          IBEGC2(I)=0
          IENDC2(I)=0
        ELSE
          CSTR=VALU(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          IBEGC2(I)=ANUM
          CSTR=VALU(IT+1:IT+18)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,J
          ENDIF
          IENDC2(I)=ANUM
        ENDIF
C***************************************************************
C***************************************************************
      IT=INDEX(FFVAR(NS+2),'/')
      IF(I.EQ.1)THEN
        VALU=FFVAR(NS+2)(1:IT-1)
      ELSE
        VALU=FFVAR(NS+2)(IT+1:37)
      ENDIF
           CSTR=VALU(1:4)
           IF(CSTR.NE.'NONE')THEN
             CFILE(I)=VALU
             IF(NCD.GT.0)CFILE(I)=NADIRD(1:NCD)//'\'//VALU
           ELSE
             CFILE(I)='NULL'
           ENDIF
          IT=INDEX(FFVAR(NS+3),'/')
          IF(I.EQ.1)THEN
            VALU=FFVAR(NS+3)(1:IT-1)
          ELSE
            VALU=FFVAR(NS+3)(IT+1:37)
          ENDIF
C     PRINT*,'VALU=',VALU
        IT=INDEX(VALU,',')
        IF(IT.EQ.0)IT=INDEX(FFVAR(NS),' ')
C     PRINT*,'IT=',IT
        IF(VALU(IT+1:IT+3).EQ.'DEF'.OR.VALU(IT+1:IT+3).EQ.'def') THEN
          FBEGC2(I)=0
          FENDC2(I)=0
        ELSE
          CSTR=VALU(1:IT-1)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,NS
          ENDIF
          FBEGC2(I)=ANUM
          CSTR=VALU(IT+1:IT+18)
          CALL CTON(CSTR,IMISS,ANUM)
          IF(IMISS.NE.0)THEN
C           WRITE(9,200)IMISS,NS
          ENDIF
          FENDC2(I)=ANUM
        ENDIF
20    CONTINUE
         ELSE
           DO 30 I=1,2
           IBEGC2(I)=0
           IENDC2(I)=0
           CFILE(I)='NULL'
           FBEGC2(I)=0
           FENDC2(I)=0
30         CONTINUE
         ENDIF
C3*******************************
      ENDIF
10    CONTINUE
C     PRINT*,'UFILE',UFILE(1),UFILE(2)
C     PRINT*,'CFILE',CFILE(1),CFILE(2)
C     PRINT*,'XYFILE=',XYFILE
C     PRINT*,'FPNAME=',FPNAME
C     PRINT*,'FMNAME=',FMNAME
C     PRINT*,'TITLE=',TITLE
C     PRINT*,'CORR,DIFF,DIFPLT=',CORR,DIFF,DIFPLT
C     PRINT*,'FBEGC2,FEND=',FBEGC2(1),FBEGC2(2)
C     PRINT*,FENDC2(1),FENDC2(2)
C     PRINT*,'IMIN2,IMAX2,LFLAG2,LFLAG1',IMIN2,IMAX2,LFLAG2,LFLAG1
C     PRINT*,'IMIN1,IMAX1',IMIN1,IMAX1
C     PRINT*,'IBEGC1,IENDC1',IBEGC1,IENDC1
C     PRINT*,'IBEGC2,IENDC2',IBEGC2(1),IBEGC2(2)
C     PRINT*,IENDC2(1),IENDC2(2)
        RETURN
        END
