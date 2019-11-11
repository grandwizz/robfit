      SUBROUTINE BREAD(XE,F,II,IIDAT,NA,N1,N2,NREAD)
C *** DATA FILES ARE ASSUMED TO CONTAIN 4 LINES OF INFO FOLLOWED BY
C *** ONE LINE CONTAINING THE FORMAT TYPE A4, NUMBER OF CHANNELS FREE
C *** FREE FORMAT ASSUMES A SPACE OR COMMA BETWEEN DATA POINTS
C *** THE HDAT USES HEXADECIMAL TO GET MORE FORMATTED INFO PER LINE
C *** Z4DA ALLOWS FOR UP TO 65,536 COUNTS ONLY.  UF DATA IS UFORMATTED
C *** AND PROBABLY WRITTEN BY A FORTRAN CODE -- HARD TO TRANSFER BETWEEN
C *** MACHINES IT IS NOT RECOMMENDED CONTAINS XE,N IN FIRST RECORD
C *** THEN N DATA POINTS IN SECOND -- SALLY DATA WAS ORIGINALLY SUPPOSED
C *** TO CORRESPOND TO KMACS BY BUZZ PIERCEY, BUT I UNDERSTAND THAT IT
C *** HAS BEEN CHANGED.
      DIMENSION F(1),IIDAT(1)
      CHARACTER*6 FFMT(4)
      CHARACTER*4 DTYPE
      CHARACTER*64 CSTR,NA
      CHARACTER*64 STATUS,CIO
C*****************************************************************
      CHARACTER*80 II(4)
      DATA FFMT/'(10Z8)','(10I8)','(20Z4)','(10I7)'/
      N1=MAX0(N1,1)
      NSKIP=N1-1
      XE=NSKIP
      IT=INDEX(NA,'.')
      IF(NA(IT+1 :IT+3).EQ.'UF')GOTO 1100
      IF(NA(IT+1 :IT+5).EQ.'SALLY')GOTO 2000
      STATUS='OLD'
      CALL MAOPEN(8,NA,STATUS)
      READ(8,'(A80)')II
      READ(8,'(A)')CIO
      DTYPE=CIO(1:4)
        DO 5 IB=6,74
        IF(CIO(IB:IB).NE.' ')THEN
          CSTR=CIO(IB:)
          CALL CTON(CSTR,IMISS,ANUM)
          GOTO 6
        ENDIF
5       CONTINUE
6       CONTINUE
      IF(IMISS.NE.0)THEN
        PRINT*,' IMISS IN CTON CSTR,ANUM',CSTR,ANUM
        STOP
      ELSE
        N=ANUM
      ENDIF
      N2=MIN0(N,N2)
      NREAD=N2-NSKIP
      IF(DTYPE.EQ.'FREE')THEN
        READ(8,*)(DUM,I=1,NSKIP),(F(I),I=1,NREAD)
        CLOSE(8)
        RETURN
      ENDIF
C     DATA FFMT/'(10Z8)','(10I8)','(20Z4)','(10I7)'/
      IFOR=0
      IF(DTYPE.EQ.'HDAT')IFOR=1
      IF(DTYPE.EQ.'IDAT')IFOR=2
      IF(DTYPE.EQ.'Z4DA')IFOR=3
      IF(DTYPE.EQ.'I7DA')IFOR=4
      IF(IFOR.GT.0)THEN
        READ(8,FFMT(IFOR))(IDUM,I=1,NSKIP),(IIDAT(I),I=1,NREAD)
        CLOSE(8)
        DO 10 I=1,NREAD
10      F(I)=IIDAT(I)
        RETURN
      ELSE
        PRINT*,' LINE FIVE OF YOUR INPUT FILE DOES NOT CONTAIN A'
        PRINT*,' VALID DATA TYPE, E.G. FREE,HDAT,IDAT,Z4DA,I7DA'
        READ(*,*)ITEST
        STOP
      ENDIF
1100     CONTINUE
         II(1)=NA
         STATUS='OLD'
         CALL MAOPUF(8,NA,STATUS)
         READ(8)N,XE
         READ(8)(DUM,I=1,NSKIP),(F(I),I=1,NREAD)
         CLOSE(8)
         RETURN
2000  CONTINUE
         STATUS='OLD'
         CALL MAOPUF(8,NA,STATUS)
C      PRINT*,' IN SALLY PART OF BREAD'
       READ(8)(IIDAT(I),I=1,128)
       I1=0
      DO 2010 I=1,300
      READ(8,END=2015)(IIDAT(I1+J),J=1,128)
      I1=I1+128
2010  CONTINUE
2015  N=I1+J-1
      CLOSE(8)
        RETURN
        END

      SUBROUTINE WXCALC(F,FA,WX,N,IEXTWT)
      DIMENSION F(1),FA(1),WX(1)
C********************************
C
C  CALCULATED WEIGHTS
C
C*********************************
      IF(IEXTWT.EQ.0)THEN
        DO I=1,N
        WX(I)=1./AMAX1(1.,F(I))
        ENDDO
        RETURN
      ELSE
        DO I=1,N
        IM=I-1
        IF(IM.EQ.0)IM=3
        IP=I+1
        IF(IP.GT.N)IP=N-2
        FA(I)=(2*F(I)-F(IM)-F(IP))**2
        ENDDO
        DO I=1,N
        SUMERS=0
       IF(I.LT.8)THEN
          DO J=1,16
          SUMERS=SUMERS+FA(J)
          ENDDO
       ELSE
         IF(I.GT.N-8)THEN
           DO J=1,16
           SUMERS=SUMERS+FA(I-8+J)
           ENDDO
         ELSE
           DO J=1,16
           SUMERS=SUMERS+FA(I-8+J)
           ENDDO
         ENDIF
       ENDIF
       SUMERS=SUMERS/(16.*6.)
       WX(I)=1.0/AMAX1(.000001,SUMERS)
       ENDDO
      ENDIF
      DO 666 I=1,N
      FA(I)=0.
666   CONTINUE
      RETURN
      END
        SUBROUTINE ROPKS(C,SC,W,IPT,WF,SW,XP,SXP,N,NPP,FW,
     # XOFF)
        REAL*8 C,SC,W,SW,WF,XP,SXP,FW,DSQRT,STR,XPF,EPF,ARG
        CHARACTER*64 NA
C*********************************************************
C
C DATA TO MAIN ROUTINE VROBFIT
C
C**********************************************************
      CHARACTER*64 NAI,STATUS,CIO
      COMMON/CHROPK/NAI
      COMMON/CROPKS/IPEKFL
        COMMON/FPEAKS/NPF,IPF,XPF(50),EPF(50),IPTF(50)
C*****************************************************************
        DIMENSION C(1),IPT(1),SC(1),W(1),WF(1),SW(1),
     # XP(1),SXP(1),FW(3,5),ARAT(5),WRAT(5)
        DATA ARAT,WRAT/10*1./
C *** THIS ROUTINE IS A FIRST PASS ATTEMPT TO PLACE PEAKS IN
C *** SPECIFIED LOCATIONS -- PEAKS MUST BE ORDERED BY CHANNEL
C *** NUMBER
C       PRINT*,'IN ROPKS BEFORE OPEN',NAI
        NPP=0
        IF(NAI.EQ.'NONE')RETURN
        STATUS='OLD'
        CALL MAOPEN(10,NAI,STATUS)
        READ(10,'(A)')NA
      READ(10,'(A)')CIO
        DO  IB=6,74
        IF(CIO(IB:IB).NE.' ')THEN
          NA=CIO(IB:)
          CALL CTON(NA,IMISS,ANUM)
          GOTO 2
        ENDIF
        ENDDO
2       IP=ANUM
        DO 4 I=1,2
        READ(10,'(A)')NA
4       CONTINUE
        IF(IP.NE.0)READ(10,'(10F8.4)')(ARAT(I),WRAT(I),I=1,IP)
104     FORMAT(1X,A64)
5       READ(10,*,END=160)XP(NPP+1),DUMM,W(NPP+1),DUM2,STR,DUM3
     # ,IPT(NPP+1)
        IF(XP(NPP+1).EQ.-1000.)GOTO 300
        IF(XP(NPP+1)-XOFF.LT.0.)GOTO 5
        IF(XP(NPP+1)-XOFF.GT.N)GOTO 150
        NPP=NPP+1
        K=IABS(IPT(NPP))
        ARG=FW(1,K)+XP(NPP)*(FW(2,K)+XP(NPP)*FW(3,K))
        ARG=DMAX1(.5D0,ARG)
        WF(NPP)=DSQRT(ARG)
        IF(W(NPP).NE.0.D0)WF(NPP)=W(NPP)
        SW(NPP)=.99D0
        W(NPP)=WF(NPP)
        IRAT=IABS(IPT(NPP))
        STR=STR/ARAT(IRAT)
        W(NPP)=W(NPP)/WRAT(IRAT)
        IF(STR.GT.0)THEN
          C(NPP)=DSQRT(STR/W(NPP))
          IPT(NPP)=IABS(IPT(NPP))
        ELSE
          C(NPP)=-DSQRT(-STR/W(NPP))
          IPT(NPP)=-IABS(IPT(NPP))
        ENDIF
        GOTO 5
150     CONTINUE
152     READ(10,*,END=160)XTEST
        IF(XTEST.EQ.-1000.)GOTO 300
        GOTO 152
160     CLOSE(10)
235     RETURN
300     CONTINUE
        NPF=0
305     READ(10,*,END=160)XPF(NPF+1),EPF(NPF+1),IPTF(NPF+1)
        NPF=NPF+1
        IF(NPF.LE.20)GOTO 305
        PRINT*,' IN ROPKS TOO MANY FIXED PEAKS NPF=',NPF
        READ(*,*)ITEST
        STOP
        END
