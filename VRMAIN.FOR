C$NOWAR
C$NOEXT
      PROGRAM VRMAIN
      COMMON/BATCFL/IBATCH
      DIMENSION FFVAR(200)
      CHARACTER*64 VALU,NA,STATUS,NAOPEN,NAMENU
      CHARACTER*1 CSTR,ANS
      CHARACTER*8 CLL(7),II
      CHARACTER*40 FFVAR
      COMMON/USPAGE/FFVAR
C *** THE RAWDD INFO
      CHARACTER*64 UFILE(2),CFILE(2),XYFILE,FPNAME,FMNAME,TITLE
      COMMON/GLFILE/UFILE,CFILE,XYFILE,FPNAME,FMNAME,TITLE
      COMMON/GLINK/CORR,DIFF
C******************************************************************
C      BREAD DATA
C******************************************************************
      CHARACTER*64 DATAFN,NADIFF
      COMMON/CHBREA/DATAFN,NADIFF
      COMMON/CBREAD/IBEGC,IENDC,IEXTWT
C*************************************************
C
C   VSHAPE DATA
C
C*************************************************
      CHARACTER*64 STNAME,TYNAME,GRNAME
      COMMON/CHVSHA/STNAME,TYNAME,GRNAME
C*************************************************
C
C   GRAF3 AND GRASHAPE DATA
C
C*************************************************
      CHARACTER*64 GDNAME,GENAME
      COMMON/CHGRAS/GDNAME,GENAME
C*****************************************************************
C*************************************************
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
C**********************************************************
C
C      VROBFIT DATA
C
C***********************************************************
      REAL*8 WID,ERRO,CUTA,CUTB
      DIMENSION WID(20),ERRO(20),ICHAN(20)
      CHARACTER*64 NABKG2,NAPK,NAGR2
      CHARACTER*4 IFW,BKGF2,NOGP
      COMMON/CHVROB/BKGF2,IFW,NOGP,NABKG2,NAPK,NAGR2
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
C      ROPKS DATA
C******************************************************************
      CHARACTER*64 IPEKFN
      CHARACTER*1 APH
      COMMON/CHROPK/IPEKFN,APH
      DATA CLL/'RAWDD','STGEN','STDIS','BKGFIT','FSPFIT','FSPDIS',
     2 'XCALIBER'/
      PRINT*,'  '
      PRINT*,'This is the ROBFIT spectral analysis code version 2.07'
      PRINT*,'     Copyright University of Florida 1991'
      PRINT*,'  '
      PRINT*,'Note to the user: If you modify the source code in any'
      PRINT*,'way, please DO NOT refer to the modified code as ROBFIT.'
10    IBATCH=0
      NA='VRMAIN.MNU'
      CALL MENURD(NA)
      IF(FFVAR(23).EQ.'DOS')GOTO 5000
C SET UP THE SECOND BATCH FILE
      STATUS='BIG'
      NAOPEN='ROB2.EXEC'
      CALL MAOPEN(1,NAOPEN,STATUS)
      WRITE(1,'(''&TRACE OFF'')')
      IF(NA.EQ.'STOP')THEN
        WRITE(1,'(''&EXIT'')')
        STOP
      ENDIF
C *** TESTING THE RETURNED FFVAR
        NR1=0
        NR2=0
        DO 20 J=1,8
        IOFF=62
        IF(FFVAR(IOFF+J).NE.' '.AND.FFVAR(IOFF+J)(1:1).NE.'N'
     2 .AND.FFVAR(IOFF+J)(1:1).NE.'n')THEN
          IF(NR1.EQ.0)THEN
            NR1=J
          ELSE
            NR2=J
          ENDIF
        ENDIF
20      CONTINUE
      IF(NR1.EQ.0)THEN
        PRINT*,' NO OPTIONS WERE CHOSEN, DO YOU WISH TO STOP?'
        READ(*,'(A1)')ANS
        IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')STOP
        GOTO 10
      ENDIF
      IF(NR2.NE.0)THEN
        PRINT*,' OPERATIONS ',NR1,' AND ',NR2,' WERE CHOSEN'
        PRINT*,' ROBFIT CAN ONLY DO ONE PER MENU'
        PRINT*,' TRY AGAIN'
        GOTO 10
      ENDIF
C SET UP THE SECOND BATCH FILE
      STATUS='UNKNOWN'
      NAOPEN='ROB2.EXEC'
      CALL MAOPEN(1,NAOPEN,STATUS)
C *** FOR MVS SET UP THE JOB FILE
      IF(FFVAR(23).EQ.'MVS')THEN
         IF(NR1.GE.8.OR.NR1.LE.0)THEN
           PRINT*,' CHOICE ',NR1,' OUT OF RANGE'
           PRINT*,' TRY AGAIN'
           GOTO 10
         ENDIF
C************SET UP THE ROBFIT JOB HEADER*********************
         NAOPEN='ROBFIT.JOB'
         CALL MAOPEN(19,NAOPEN,STATUS)
         WRITE(19,101)
101   FORMAT('//ROBFIT JOB ,PHYSICS,CLASS=R,REGION=600K'/
     2 '/*JOBPARM TIME=999')
        IF(NR1.EQ.1.OR.NR1.EQ.3.OR.NR1.EQ.6) WRITE(19,104)
104   FORMAT('//OUT1 OUTPUT DEST=NER3820')
         WRITE(19,102)
102   FORMAT('// EXEC FORTVE,PARM=NOXUFLOW,')
         WRITE(19,103)CLL(NR1)
103   FORMAT('// LIBRARY=''UF.B0035007.S1.ROBFIT'',PROGRAM=',A/
     2 '//IAINPMNU DD *'/'/*INCLUDE VRMAIN.MNU')
        IF(NR1.EQ.1.OR.NR1.EQ.3.OR.NR1.EQ.6) WRITE(19,107)
107   FORMAT('//GO.GRAFOUT DD SYSOUT=A,DCB=RECFM=VM,OUTPUT=*.OUT1')
      CALL CMS('VMFCLEAR',IRT)
      GOTO (30,40,50,60,70,80,90)NR1
30    CALL G4LINK(NAMENU)
      IF(NAMENU.EQ.'STOP')STOP
      CALL CMVSA(NAMENU,NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      WRITE(19,'(''/*INCLUDE '',A)')NAMENU
      IF(UFILE(1)(1:3).NE.'UF.')THEN
        CALL CMVSA(UFILE(1),NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')UFILE(1)
      ENDIF
      IF(UFILE(2)(1:3).NE.'UF.'.AND.UFILE(2)(1:1).NE.'\')THEN
        CALL CMVSA(UFILE(2),NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')UFILE(2)
      ENDIF
      IF(DIFF.EQ.1.)THEN
        CALL CMVSA(FPNAME,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        CALL CMVSA(FMNAME,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      ENDIF
      WRITE(19,'(''//GO.SYSIN DD *''/''   ''/''C'')')
      WRITE(1,'(''EXEC SUBMIT ROBFIT'')')
      STOP
40    NAMENU='STGEN.MNU'
      CALL SHLINK(NAMENU)
      IF(NAMENU.EQ.'STOP')STOP
      IF(TYNAME.EQ.'FITD')THEN
        IF(DATAFN(1:3).NE.'UF.')THEN
          CALL CMVSA(DATAFN,NAOPEN,NC,'I')
          WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
          WRITE(19,'(''/*INCLUDE '',A)')DATAFN
        ENDIF
        IF(NADIFF(1:3).NE.'UF.'.AND.NADIFF(1:4).NE.'NONE')THEN
          CALL CMVSA(NADIFF,NAOPEN,NC,'I')
          WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
          WRITE(19,'(''/*INCLUDE '',A)')NADIFF
        ENDIF
      ENDIF
      CALL CMVSA(STNAME,NAOPEN,NC,'O')
      WRITE(19,'(''//'',A,'' DD SYSOUT=S'')')NAOPEN(1:NC)
      IF(GRNAME(1:3).NE.'UF.')THEN
        CALL CMVSA(GRNAME,NAOPEN,NC,'O')
        WRITE(19,'(''//'',A,'' DD SYSOUT=G'')')NAOPEN(1:NC)
      ENDIF
      WRITE(1,'(''EXEC SUBMIT ROBFIT'')')
      STOP
50    NAMENU='STDIS.MNU'
      CALL GSLINK(NAMENU)
      IF(NAMENU.EQ.'STOP')STOP
      CALL CMVSA(NAMENU,NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      WRITE(19,'(''/*INCLUDE '',A)')NAMENU
      IF(GDNAME(1:3).NE.'UF.')THEN
        CALL CMVSA(GDNAME,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')GDNAME
      ENDIF
      WRITE(19,'(''//GO.SYSIN DD *''/''   ''/''C'')')
      WRITE(1,'(''EXEC SUBMIT ROBFIT'')')
      STOP
60    CALL BKLINK(NAMENU)
      IF(NAMENU.EQ.'STOP')STOP
      CALL CMVSA(NAMENU,NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      WRITE(19,'(''/*INCLUDE '',A)')NAMENU
      IF(IBKGFN(1:4).NE.'NONE')THEN
        CALL CMVSA(IBKGFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')IBKGFN
      ENDIF
      IF(DATAFN(1:3).NE.'UF.')THEN
        CALL CMVSA(DATAFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')DATAFN
      ENDIF
      IF(NADIFF(1:3).NE.'UF.'.AND.NADIFF(1:4).NE.'NONE')THEN
        CALL CMVSA(NADIFF,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')NADIFF
      ENDIF
      CALL CMVSA(NABKG,NAOPEN,NC,'O')
      WRITE(19,'(''//'',A,'' DD SYSOUT=C'')')NAOPEN(1:NC)
      CALL CMVSA(NAGR,NAOPEN,NC,'O')
      WRITE(19,'(''//'',A,'' DD SYSOUT=G'')')NAOPEN(1:NC)
      WRITE(1,'(''EXEC SUBMIT ROBFIT'')')
      STOP
70    NAMENU='FSPFIT.MNU'
      CALL VRLINK(NAMENU)
      CALL CMVSA(NAMENU,NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      WRITE(19,'(''/*INCLUDE '',A)')NAMENU
      IF(IPEKFN(1:4).NE.'NONE')THEN
        CALL CMVSA(IPEKFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')IPEKFN
      ENDIF
      IF(IBKGFN(1:4).NE.'NONE')THEN
        CALL CMVSA(IBKGFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')IBKGFN
      ENDIF
      DO J=1,NSTAN
        CALL CMVSA(STAN(J),NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')STAN(J)
      ENDDO
      IF(DATAFN(1:3).NE.'UF.')THEN
        CALL CMVSA(DATAFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')DATAFN
      ENDIF
      IF(NADIFF(1:3).NE.'UF.'.AND.NADIFF(1:4).NE.'NONE')THEN
        CALL CMVSA(NADIFF,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')NADIFF
      ENDIF
      CALL CMVSA(NAPK,NAOPEN,NC,'O')
      WRITE(19,'(''//'',A,'' DD SYSOUT=P'')')NAOPEN(1:NC)
      CALL CMVSA(NABKG2,NAOPEN,NC,'O')
      WRITE(19,'(''//'',A,'' DD SYSOUT=C'')')NAOPEN(1:NC)
      CALL CMVSA(NAGR2,NAOPEN,NC,'O')
      WRITE(19,'(''//'',A,'' DD SYSOUT=G'')')NAOPEN(1:NC)
      WRITE(1,'(''EXEC SUBMIT ROBFIT'')')
      STOP
80    NAMENU='FSPDIS.MNU'
      CALL G3LINK(NAMENU)
      IF(NAMENU.EQ.'STOP')STOP
      CALL CMVSA(GDNAME,NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      WRITE(19,'(''/*INCLUDE '',A)')GDNAME
      CALL CMVSA(NAMENU,NAOPEN,NC,'I')
      WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
      WRITE(19,'(''/*INCLUDE '',A)')NAMENU
      IF(GENAME(1:3).NE.'UF.'.AND.GENAME(1:4).NE.'NONE')THEN
        CALL CMVSA(GENAME,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')GENAME
      ENDIF
      STATUS='OLD'
      CALL MAOPEN(3,GDNAME,STATUS)
      READ(3,'(A,G020.12)')II,CHIS
      READ(3,*)NSTAN
      IF(NSTAN.GT.5)THEN
        PRINT*,' ATTEMPT TO DEFINE MORE THAN FIVE STANDARDS'
        READ(*,*)ITEST
        STOP
      ENDIF
C *** READ THE STANDARD FILE NAMES
      IF(NSTAN.GT.0)THEN
      DO  J=1,NSTAN
        READ(3,'(A)')STAN(J)
        CALL CMVSA(STAN(J),NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')STAN(J)
      ENDDO
C *** READ THE BACK SLASH CARD
      READ(3,'(A)')NA
      ENDIF
      READ(3,'(I4/3(A64/),A64)')IEXTWT,DATAFN,NADIFF,IBKGFN,IPEKFN
      CLOSE(3)
      IF(DATAFN(1:3).NE.'UF.')THEN
        CALL CMVSA(DATAFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')DATAFN
      ENDIF
      IF(NADIFF(1:3).NE.'UF.'.AND.NADIFF(1:4).NE.'NONE')THEN
        CALL CMVSA(NADIFF,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')NADIFF
      ENDIF
      IF(IPEKFN(1:4).NE.'NONE')THEN
        CALL CMVSA(IPEKFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')IPEKFN
      ENDIF
      IF(IBKGFN(1:4).NE.'NONE')THEN
        CALL CMVSA(IBKGFN,NAOPEN,NC,'I')
        WRITE(19,'(''//'',A,'' DD  *'')')NAOPEN(1:NC)
        WRITE(19,'(''/*INCLUDE '',A)')IBKGFN
      ENDIF
      WRITE(19,'(''//GO.SYSIN DD *''/''   ''/''C'')')
      WRITE(1,'(''EXEC SUBMIT ROBFIT'')')
      STOP
90    PRINT*,' XCALIBER NEEDS TO RUN INTERACTIVELY'
      PRINT*,' TRY AGAIN WITH CMS'
      GOTO 10
      ENDIF
C********************END OF MVS SECTION ************************

      IF(FFVAR(23).EQ.'WATFOR')THEN
      WRITE(1,'(''GLOBAL TXTLIB ARBIO CIRLIB'')')
      GOTO (200,300,400,500,600,700,800,900,1000)NR1
C*************************************************
200     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 RAWDD'')')
        STOP
C*************************************
300     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 STGEN'')')
        STOP
C*************************************
400     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 STDIS'')')
        STOP
C*************************************
500     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 BKGFIT'')')
        STOP
C*************************************
600     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 FSPFIT'')')
        STOP
C*************************************
700     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 FSPDIS'')')
        STOP
C*************************************
800     WRITE(1,'(''VMFCLEAR''/''EXEC WATFOR77 XCALIBER'')')
        STOP
C*************************************
900     PRINT*,' ENTER THE FILE NAME FOR EDITING'
        READ(*,'(A)')NA
        WRITE(1,180)NA
180     FORMAT('XEDIT ',A)
        STOP
C*************************************
1000    WRITE(1,190)
190     FORMAT('REM STOPPING ROBFIT')
        STOP
      ENDIF
C*************************************

      IF(FFVAR(23).EQ.'CMS')THEN
      WRITE(1,'(''VMFCLEAR''/''GLOBAL TXTLIB VSF2LINK VSF2FORT ESVVLIB''
     2,'' CIRLIB ARBIO'')')
      GOTO (1200,1300,1400,1500,1600,1700,1800,1900,2000)NR1
C*************************************************
1200     WRITE(1,'(''LOAD RAWDD G4LINK MENURD CMSPLOTS NUMOUT BREAD'',
     2 '' CMSOPEN (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1300     WRITE(1,'(''LOAD STGEN TCHREAD SHLINK MENURD CMSOPEN BREAD '',
     2 ''ARBIN CHARED BSMSQ (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1400     WRITE(1,'(''LOAD STDIS GSLINK MENURD CMSPLOTS NUMOUT BREAD'',
     2 '' CMSOPEN (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1500     WRITE(1,'(''LOAD BKGFIT BKLINK MACBKG TCHREAD BSMSQ MENURD'',
     2 '' BREAD CMSOPEN (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1600     WRITE(1,'(''LOAD FSPFIT PFIT VRLINK MENURD TCHREAD BSMSQ'',
     2 '' MACBKG BREAD CMSOPEN (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1700     WRITE(1,'(''LOAD FSPDIS G3LINK MENURD NUMOUT CMSPLOTS BREAD'',
     2 '' CMSOPEN (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1800     WRITE(1,'(''LOAD XCALIBER MENURD TCHREAD BREAD BSMSQ'',
     2 '' CMSOPEN (NOAUTO CLEAR''/''START * NOXUFLOW'')')
        STOP
C*************************************
1900     PRINT*,' ENTER THE FILE NAME FOR EDITING'
        READ(*,'(A)')NA
        WRITE(1,'(''XEDIT '',A)')NA
        STOP
C*************************************
2000    WRITE(1,190)
        STOP
      ENDIF
C*************************************
5000  CONTINUE
C*** DOS SECTION
      IF(NA.EQ.'STOP')THEN
        STOP
      ENDIF
C *** TESTING THE RETURNED FFVAR
        NR1=0
        NR2=0
        DO 5050 J=1,8
        IOFF=62
        IF(FFVAR(IOFF+J).NE.' '.AND.FFVAR(IOFF+J)(1:1).NE.'N'
     2 .AND.FFVAR(IOFF+J)(1:1).NE.'n')THEN
          IF(NR1.EQ.0)THEN
            NR1=J
          ELSE
            NR2=J
          ENDIF
        ENDIF
5050    CONTINUE
      IF(NR1.EQ.0)THEN
        PRINT*,' NO OPTIONS WERE CHOSEN, DO YOU WISH TO STOP?'
        READ(*,'(A1)')ANS
        IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')STOP
        GOTO 10
      ENDIF
      IF(NR2.NE.0)THEN
        PRINT*,' OPERATIONS ',NR1,' AND ',NR2,' WERE CHOSEN'
        PRINT*,' ROBFIT CAN ONLY DO ONE PER MENU'
        PRINT*,' TRY AGAIN'
        GOTO 10
      ENDIF
C SET UP THE SECOND BATCH FILE
      STATUS='UNKNOWN'
      NAOPEN='ROB2.BAT'
      CALL MAOPEN(1,NAOPEN,STATUS)
      WRITE(1,'(''ECHO OFF'')')
      GOTO (5200,5300,5400,5500,5600,5700,5800,5900,6000)NR1
C*************************************************
5200     WRITE(1,'(''RAWDD.EXE'')')
        STOP
C*************************************
5300     WRITE(1,'(''STGEN.EXE'')')
        STOP
C*************************************
5400     WRITE(1,'(''STDIS.EXE'')')
        STOP
C*************************************
5500     WRITE(1,'(''BKGFIT.EXE'')')
        STOP
C*************************************
5600     WRITE(1,'(''FSPFIT.EXE'')')
        STOP
C*************************************
5700     WRITE(1,'(''FSPDIS.EXE'')')
        STOP
C*************************************
5800     WRITE(1,'(''XCALIBER.EXE'')')
        STOP
C*************************************
5900     PRINT*,' ENTER THE FILE NAME FOR EDITING'
        READ(*,'(A)')NA
        WRITE(1,'(''ELIN '',A)')NA
        STOP
C*************************************
6000    WRITE(1,'(''REM STOPPING ROBFIT'')')
        STOP
C*************************************
      END
C*INCLUDE C:\ROBFIT\FORTRAN\MENURD
C*INCLUDE C:\ROBFIT\FORTRAN\TCHREAD
C*INCLUDE C:\ROBFIT\FORTRAN\MAOPEN
C*INCLUDE C:\ROBFIT\FORTRAN\G4LINK
C*INCLUDE C:\ROBFIT\FORTRAN\SHLINK
C*INCLUDE C:\ROBFIT\FORTRAN\GSLINK
C*INCLUDE C:\ROBFIT\FORTRAN\BKLINK
C*INCLUDE C:\ROBFIT\FORTRAN\VRLINK
C*INCLUDE C:\ROBFIT\FORTRAN\G3LINK
C$INCLUDE MENURD FORTRAN
C$INCLUDE TCHREAD FORTRAN
C$INCLUDE MAOPEN FORTRAN
C$INCLUDE G4LINK FORTRAN
C$INCLUDE SHLINK FORTRAN
C$INCLUDE GSLINK FORTRAN
C$INCLUDE BKLINK FORTRAN
C$INCLUDE VRLINK FORTRAN
C$INCLUDE G3LINK FORTRAN
