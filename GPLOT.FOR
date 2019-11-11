C$NOEXT
C$NOWAR
         DIMENSION X(1000,10),Y(1000,10),INC(10),RANS(1)
         CHARACTER*64 DF(11),CXLF,JNAME,PCHAR,CTEMP
         CHARACTER*7 CYLF
         CHARACTER*5 CHARN
         CHARACTER*4 LT(5),TERM,ACC1,ACC2
         CHARACTER*2 LLSYM(10)
         CHARACTER*1 ANS,LSYM(10),PTANS(10),CXL(64),CT(1),CYL(7),
     2 DF1(64,11),PCHA(64)
         EQUIVALENCE (CXL(1),CXLF),(CYL(1),CYLF),(DF(1),DF1(1,1))
         EQUIVALENCE (PCHAR(1:1),PCHA(1))
         DATA CYLF/'\'/,CXLF/'\'/,ICLS/0/
         DATA LT/'SOLI','DOT ','DODA','SDAS','LDAS'/                    GPL00120
         DATA LLSYM/10*'--'/,YNORM,XNORM/1.,1./
         DATA ILOOP/0/
C *** FOLLOWING SPECIFIC FOR NERDC TERM/BATCH OPERATION
         READ(5,'(A4)')TERM
         BX=1.E35
         GX=-BX
         BY=1.E35
         GY=-BY
         DO 16 K=1,10
         PRINT*,' ENTER THE NAME OF THE DATA FILE'
         READ(*,'(A)')DF(K)
         IF(TERM.EQ.'TERM')WRITE(11,'(A)')DF(K)
         IF(DF(K).EQ.'/')GOTO 18
         IF(DF(K).EQ.'\')GOTO 18
C *** FOLLOWING IF THEN WRITES THE JCL TO RUN THE BATCH VERSION OF GPLOT
      IF(TERM.EQ.'TERM'.AND.K.EQ.1)THEN                                 GPL00260
C *** FIND FIRST BLANK IN DF(K)                                         GPL00270
      JNAME=DF(K)                                                       GPL00280
      DO 173 KB=1,7                                                     GPL00290
      IF(JNAME(KB:KB).EQ.' ')GOTO 182                                   GPL00300
173    CONTINUE                                                         GPL00310
 182    WRITE(7,1001) JNAME(1:KB)                                       GPL00320
1001  FORMAT('//',A,' JOB ,COLDWELL,CLASS=A,REGION=900K'/               GPL00330
     2 '/*JOBPARM TIME=9,LINES=10'/                                     GPL00340
     3'//OUT1 OUTPUT DEST=NER3820'/                                     GPL00350
     4'//  EXEC FORTVE,LIBRARY=''UF.B0035007.S1.LIB'',PROGRAM=GPLOT'/   GPL00360
     5'//GO.GRAFOUT DD SYSOUT=A,DCB=RECFM=VM,OUTPUT=*.OUT1')            GPL00370
      ENDIF                                                             GPL00380
C *** END OF FIRST JCL WRITE
         IF(TERM.EQ.'TERM')WRITE(7,1102)K,DF(K)                         GPL00560
1102     FORMAT('//GO.FT01F00',I1,' DD *'/'/*INCLUDE ',A20)             GPL00570
C *** END OF SECOND JCL WRITE
         INC(K)=5000
         IF(TERM.EQ.'TERM')THEN
              CALL GREAD(X(1,K),Y(1,K),INC(K),DF(K))
         ELSE
              CALL GREAD(X(1,K),Y(1,K),INC(K),'1')
         ENDIF
         IF(INC(K).EQ.0)GOTO 18
         DO 12 I=1,INC(K)
         BX=AMIN1(BX,X(I,K))
         GX=AMAX1(GX,X(I,K))
         BY=AMIN1(BY,Y(I,K))
         GY=AMAX1(GY,Y(I,K))
12       CONTINUE
16       CONTINUE
18       NDSETS=K-1
C *** THIRD JCL WRITE
         IF(TERM.EQ.'TERM')WRITE(7,1003)                                GPL00850
1003     FORMAT('//GO.FT16F001 DD DUMMY'/'//GO.SYSIN DD *'/'BATCH'/     GPL00860
     # '/*INCLUDE VPLOT PDATA')                                         GPL00870
C *** AFTER 3RD JCL WRITE
C ***  SET DEFAULTS FOR GRAPHING
         IXAM=IBN(BX,GX,4,12)
         IYAM=IBN(BY,GY,3,8)
         DLX=DLF((GX-BX)/IXAM,2,IXT,NDPX)
         DLY=DLF((GY-BY)/IYAM,2,IYT,NDPY)
      IBX=BX/DLX
      IF(BX.LT.0.)IBX=IBX-1
      BX=IBX*DLX
      GX=BX+(IXAM+1)*DLX
      IBY=BY/DLY
      IF(BY.LT.0.)IBY=IBY-1
      BY=IBY*DLY
      GY=BY+(IYAM+1)*DLY
       IXAM=IXAM+1
       IYAM=IYAM+1
127       CONTINUE
      IF(TERM.EQ.'TERM')THEN
        CALL MENU(BX,GX,IXAM,DLX,CXL,BY,GY,IYAM,DLY,CYL,XSF,YSF,
     2 LLSYM,NDSETS,YNORM,XNORM,ICLS)
        WRITE(11,1222)BX,GX,IXAM,DLX,CXLF,BY,GY,IYAM,DLY,CYLF,XSF,YSF,
     2 LLSYM,NDSETS,YNORM,XNORM,ICLS
1222   FORMAT(2G15.7,I5,G15.7/A/2G15.7,I5,G15.7/A7,2G15.7,10A3/I3,
     3 2G15.7,I3)
       ELSE
        READ(*,1222,END=500)BX,GX,IXAM,DLX,CXLF,BY,GY,IYAM,DLY,
     2 CYLF,XSF,YSF,LLSYM,NDSETS,YNORM,XNORM,ICLS
       ENDIF
        IF(ICLS.EQ.0.AND.TERM.EQ.'TERM')CALL CLEARS
C        CALL LINET('SOLI')
         CALL STPL(.12,.12)
         CALL PLOT(.12,.98)
         CALL PLOT(.98,.98)
         CALL PLOT(.98,.12)
         CALL PLOT(.12,.12)
         DTT=DLX*.86/(GX-BX)
         DO 300 I=1,IXAM+1
         XCT=(I-1)*DTT+.12
         CALL STPL(XCT,.12)
         CALL PLOT(XCT,.14)
         CALL STPL(XCT,.96)
         CALL PLOT(XCT,.98)
         RIY=BX+(I-1)*DLX
         IF(ABS(RIY).LT.1.D-4*DLX)RIY=0
         CALL NUMOUT(XCT,.08,XNORM*RIY,3,2)
 300     CONTINUE
         DTT=DLY*(.86/(GY-BY))
         DO 310 I=1,IYAM+1
         YCT=(I-1)*DTT+.12
         CALL STPL(.12,YCT)
         CALL PLOT(.14,YCT)
         CALL STPL(.98,YCT)
         CALL PLOT(.96,YCT)
         IF(I.EQ.IYAM/2.AND.CYLF.NE.'\')THEN
            YCH=YCT+.5*DTT
            CTEMP=CYLF
            CALL BCHART(0.0,YCH,CTEMP)
         ENDIF
         CALL NUMOUT(0.1,YCT-.010,YNORM*(BY+(I-1)*DLY),3,1)
 310     CONTINUE
        KLT=1
      DO 340 K=1,NDSETS
      IBPLOT=0
      DF(K)=DF(K)(:15)//'\'
C******** WRITE HEADING FOR GRAPH (DATA FILENAME)
C         THE STRING (HEADING) THAT IS OUTPUTTED MUST END WITH A BACKSLASH
          CALL CCHAR(CXL,IXLF)
          IF(IXLF.EQ.0)THEN
            CALL BCHART(.727,.916-.035*K,DF(K))
          ELSE
            BEGX=.454-IXLF*.002
            CALL BCHART(BEGX,.025,CXLF)
          ENDIF
C******** IF PLOTTING MORE THAN 1 DATA FILE, CALL LINET TO DETERMINE
C         COLOR
       IF(K.GT.1.) THEN
       KLT=KLT+1
       CALL LINET(LT(KLT))
       IF(KLT.EQ.5) KLT=0
       ENDIF
         DO 320 I=1,INC(K)
         XCT=.12+(XSF*(X(I,K)-BX))
         YCT=.12+(YSF*(Y(I,K)-BY))
        IF(I.EQ.1)XCTO=XCT
      IF(XCT.LT..08.OR.XCT.GT.1.0)GOTO 320
       IF(ABS(XCT-XCTO).GT..50)IBPLOT=0
      YCT=AMIN1(.98,AMAX1(.12,YCT))
      IF(LLSYM(K).EQ.'--'.OR.LLSYM(K).EQ.' \') GOTO 318
      IF(LLSYM(K).NE.'p\'.AND.LLSYM(K).NE.'P\')THEN
         PCHAR=LLSYM(K)
         CTEMP=PCHAR
         CALL BCHART(XCT-.007,YCT-.017,CTEMP)
      ELSE
         CALL PONT(XCT,YCT)
      ENDIF
      GOTO 319
318   IF(IBPLOT.EQ.0) CALL STPL(XCT,YCT)
      IF(IBPLOT.GT.0) CALL PLOT(XCT,YCT)
          XCTO=XCT
319   IBPLOT=IBPLOT+1
320     CONTINUE
340     CONTINUE
C *** STPL(0.,0.) OUTPUTS THE GRAPHICS TO THIS POINT
        CALL STPL(0.,0.)
C *** CHREAD WAITS UNTIL USER HITS CARRIAGE RETURN
        IF(TERM.EQ.'TERM')CALL CHREAD(9,80,RANS,CT,1,0,IEND)
C *** CALL STPL, CALL ANSI IS EQUIVALENT TO CALL STPL(-1.,Y)
C *** THE INTERMEDIATE CHREAD USED HERE ALLOWS THE SCREEN TO BE
C *** OBSERVED UNTIL A <CR> IS ENTERED ANSI LEAVES THE GRAPHICS SCREEN
      IF(TERM.NE.'TERM')CALL CLEARS
      IF(TERM.NE.'BATC')CALL ANSI
          GOTO 127
500      STOP
         END
        SUBROUTINE MENU(BX,GX,IXAM,DLX,CXLF,BY,GY,IYAM,DLY,CYLF,XSF,YSF,
     2 LLSYM,NDSETS,YNORM,XNORM,ICLS)
        CHARACTER*1 ANS,CT(64),CXLF(64),CYLF(7)
        CHARACTER*2 LLSYM(10)
        CHARACTER*64      CANS,CLS(2)
        DIMENSION RANS(10)
        EQUIVALENCE (CT(1),CANS)
        SAVE IXLF,IYLF
        DATA IXLF,IYLF/0,0/
        DATA CLS/'SCREEN WILL BE CLEARED BEFORE NEXT PLOT',
     2 'THIS PLOT WILL BE PLACED ON TOP OF THE PREVIOUS ONE.'/
127     XSF=.86*(GX-BX)/((GX-BX)**2+1.E-36)
        YSF=.86*(GY-BY)/((GY-BY)**2+1.E-36)
128     WRITE(*,1)
 1      FORMAT(' CURRENT SETUP:')
        WRITE(*,2)XNORM*BX,XNORM*GX,XNORM
 2      FORMAT(' 1. LOW-X:',1PG15.2,' HIGH-X:',1PG15.2,' XNORM',G12.4)
        WRITE(*,3)IXAM
 3      FORMAT(' 2. # OF X AXIS MARKS:',I13)
        WRITE(*,4)DLX*XNORM
 4      FORMAT(' 3. DELTA-X:    ',G12.4)
        WRITE(*,5)(CXLF(I),I=1,IXLF+1)
 5      FORMAT(' 4. X COORDINANTE LABEL: ',64A1)
        WRITE(*,6)
 6      FORMAT(' ---------------------------')
        WRITE(*,7)YNORM*BY,YNORM*GY,YNORM
 7      FORMAT(' 5. LOW-Y:',1PG12.4,' HIGH-Y:',1PG12.4,' YNORM',G12.4)
        WRITE(*,8)IYAM
 8      FORMAT(' 6. # OF Y AXIS MARKS:',I13)
        WRITE(*,9)DLY*YNORM
 9      FORMAT(' 7. DELTA-Y:    ',G12.4)
        WRITE(*,10)(CYLF(I),I=1,IYLF+1)
 10     FORMAT(' 8. Y COORDINATE LABEL: ',7A1)
        WRITE(*,1001)(I,LLSYM(I),I=1,NDSETS)
1001    FORMAT(' 9. PLOT SYMBOLS',10(I2,A2))
        WRITE(*,1003)CLS(ICLS+1)
1003    FORMAT('10. ',A)
        WRITE(*,11)
 11     FORMAT(' ---------------------------')
C******** THRU STATEMENT #299, DEFAULT VALUES CAN BE CHANGED
        WRITE(*,12)
 12     FORMAT(' CHANGE WHICH? <0>=NONE ,11 = STOP')
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        ICH=RANS(1)+1.E-7
        IF(ICH.EQ.0)RETURN
        GOTO (210,220,230,240,250,260,270,280,290,300),ICH
        IF(ICH.EQ.11)STOP
        GOTO 127
 210    WRITE(*,211)BX*XNORM,GX*XNORM,XNORM
 211    FORMAT('ENTER LOW-X:',1PG15.2,' HIGH-X:',1PG15.2,' XNORM',G12.4)
        CALL CHREAD(9,80,RANS,CT,3,0,IEND)
        IF(RANS(3).NE.0.)XNORM=RANS(3)
        IF(RANS(1).NE.0..OR.RANS(2).NE.0.)THEN
          BX=RANS(1)/XNORM
          GX=RANS(2)/XNORM
        ELSE
          GOTO 127
        ENDIF
        DLXT=DLF((GX-BX)/IXAM,2,IXT,NDPX)
        PRINT*,' ENTER  0 FOR DX= ',DLX*XNORM
        PRINT*,' ENTER -1 FOR DX= ',DLXT*XNORM
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).EQ.0.)GOTO 127
        IF(RANS(1).LT.0)THEN
          DLX=DLXT
        ELSE
          DLX=RANS(1)/XNORM
        ENDIF
        IXAM=(GX-BX)/DLX
 220    WRITE(*,221)IXAM
 221    FORMAT(' ENTER # OF X-AXIS MARKS ',I13,'!')
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).EQ.0.)GOTO 127
        IXAM=RANS(1)
        GOTO 127
 230    WRITE(*,231)DLX*XNORM
 231    FORMAT(' ENTER DELTA-X ',F15.2,'!')
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).EQ.0)GOTO 127
        DLX=RANS(1)/XNORM
        IXAM=(GX-BX)/DLX
        WRITE(*,232)IXAM
 232    FORMAT(' # OF X-AXIS MARKS, DEFAULT = ',I5)
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).NE.0.)IXAM=RANS(1)+1.E-7
        GOTO 127
 240    WRITE(*,241)(CXLF(I),I=1,IXLF+1)
 241    FORMAT(' ENTER X COORDINATE LABEL ',64A1)
        CALL CHREAD(9,80,RANS,CXLF,0,63,IEND)
        CXLF(64)='\'
        CALL CCHAR(CXLF,IXLF)
        CXLF(IXLF+1)='\'
        GOTO 128
 250    WRITE(*,251)YNORM*BY,YNORM*GY,YNORM
 251    FORMAT(' ENTER LOW-Y',1PG15.2,' HIGH-Y',1PG15.2,' YNORM',
     2  G15.2,'!')
        CALL CHREAD(9,80,RANS,CT,3,0,IEND)
        IF(RANS(3).NE.0.)YNORM=RANS(3)
        IF(RANS(1).NE.0..OR.RANS(2).NE.0.)THEN
          BY=RANS(1)/YNORM
          GY=RANS(2)/YNORM
        ELSE
          GOTO 127
        ENDIF
        DLYT=DLF((GY-BY)/IYAM,2,IYT,NDPY)
        PRINT*,' ENTER  0 FOR DY= ',DLY*YNORM
        PRINT*,' ENTER -1 FOR DY= ',DLYT*XNORM
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).EQ.0.)GOTO 127
        IF(RANS(1).LT.0)THEN
          DLY=DLYT
        ELSE
          DLY=RANS(1)/YNORM
        ENDIF
        IYAM=(GY-BY)/DLY
 260    WRITE(*,221)IYAM
 261    FORMAT(' ENTER # OF Y-AXIS MARKS ',I13,'!')
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).EQ.0.)GOTO 127
        IYAM=RANS(1)
        GOTO 127
 270    WRITE(*,271)DLY*YNORM
 271    FORMAT(' ENTER DELTA-Y ',G15.2,'!')
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).EQ.0.)GOTO 127
        DLY=RANS(1)/YNORM
        IYAM=(GY-BY)/DLY
        WRITE(*,272)IYAM
 272    FORMAT(' Y-AXIS MARKS? DEFAULT = ',I13,'!')
        CALL CHREAD(9,80,RANS,CT,1,0,IEND)
        IF(RANS(1).GT.0)THEN
          IYAM=RANS(1)+1.E-7
        ENDIF
        GOTO 127
 280    WRITE(*,281)(CYLF(I),I=1,IYLF+1)
 281    FORMAT(' ENTER Y COORDIANTE LABEL (6 CHARACER LIMIT)'/1X,7A1)
        CALL CHREAD(9,80,RANS,CYLF,0,6,IEND)
        CYLF(7)='\'
        CALL CCHAR(CYLF,IYLF)
        CYLF(IYLF+1)='\'
        GOTO 128
290     PRINT*,' ENTER DATA SET #, PLOT SYMBOL'
        CALL CHREAD(9,80,RANS,CT,1,2,IEND)
        K=RANS(1)+1.E-7
        K=MAX0(1,K)
        LLSYM(K)=CT(1)//'\'
        IF(CT(2).EQ.'-')LLSYM(K)=CT(1)//CT(2)
        GOTO 127
300     IF(ICLS.EQ.0)THEN
          ICLS=1
        ELSE
          ICLS=0
        ENDIF
        GOTO 127
        END
C M     INCLUDE VFPLOTS.FOR
C M     INCLUDE VPLOTS.FOR
C$INCLUDE VFPLOTS   FORTRAN
C$INCLUDE VPLOTS    FORTRAN
C$INCLUDE ERRSET
