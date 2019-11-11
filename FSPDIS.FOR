C$NOEXT
C$NOWAR
      PROGRAM FSPDIS
C M   SUBROUTINE FSPDIS
      PARAMETER (MAXCHN=4096,MAXPKS=256,MAXFPK=50,MAXBGN=120)
C MAXCHN IS THE MAXIMUM NUMBER OF CHANNELS
C MAXPKS IS THE MAXIMUM NUMBER OF PEAKS
C MAXFPK IS THE MAXIMUM NUMBER OF FIXED PEAKS
C MAXBGN IS THE MAXIMUM NUMBER OF BACKGROUND CONSTANTS
      INTEGER*2 IX,IFB
      REAL*8 POLYG3,POLYB3,SP
      REAL*8 XT,XTT,ST
      REAL*8 CONS,W1,W2,FW
      REAL*8 C,SC,W,WF,SW,XP,SXP
      REAL*8 XDP,PD,FDP
      REAL*8 ERRSQ,CHIS,BWSPW
      REAL*8 DU,XPS,XPL,XG3,X
      DIMENSION FORIG(4),XORIG(4),FINT(4),XINT(4),EK(10),AK(10)
      DIMENSION F(MAXCHN),WX(MAXCHN),FAA(MAXCHN),IHD(MAXCHN)
      EQUIVALENCE (IHD(1),F(1))
      DIMENSION C(MAXPKS),W(MAXPKS),PD(768 ),IPT(MAXPKS),WF(MAXPKS)
      EQUIVALENCE (PD(1),FAA(1))
      DIMENSION XP(MAXPKS),SW(MAXPKS),SXP(MAXPKS),SC(MAXPKS)
      EQUIVALENCE (SW(1),FAA(1)),(SXP(1),FAA(1)),(SC(1),FAA(1))
      DIMENSION FW(3,5)
      DIMENSION CONS(200)
      DIMENSION RANS(3)
        CHARACTER*8 II,IFTEST
        CHARACTER*80 IIT(4)
        CHARACTER*64 CLINE,NAMENU,LABEL,STATUS,CTEMP
        CHARACTER*64 NA,NAINP,NAWDAT
        CHARACTER*1 ANS,CANS(140)
        EQUIVALENCE (CLINE,CANS(1))
         COMMON/AXISP/BHORI,BVERT,SHORI,SVERT,EHORI,EVERT
C*************************************************
C
C   GRAF3 DATA
C
C*************************************************
      CHARACTER*64 GDNAME,GENAME
      CHARACTER*1 VCF,VPE,VBG,VRE,VDATA
      COMMON/CHGRAS/GDNAME,GENAME,VCF,VPE,VBG,VRE,VDATA
      COMMON/CGRASH/IBEGC,IENDC,IMIN,IMAX,AMULT,LFLAG
C *** GRAPHICS COMMON
C***********************************************
C  GRAPHICS COMMONS
C***********************************************
      CHARACTER*64 NAPK
      COMMON/SCONS/DU(150),XPS(5),XPL(5),IP,NC
      COMMON/PYG3/NST(5)
C*****RAW DATA
      CHARACTER*64 DATAFN,NADIFF
      COMMON/CHBREA/DATAFN,NADIFF
      COMMON/CBREAD/IBS,IES,IEXTWT
C*****
C*****ROPKS STUFF
      CHARACTER*64 IPEKFN
      COMMON/CHROPK/IPEKFN
      COMMON/CROPKS/IPEKFL
C*****
C*****BKGFIT
      CHARACTER*64 IBKGFN
      COMMON/CHBKGF/IBKGFN
      COMMON/CBKGFI/IBKGFL
C*****
C****************************************************
      DIMENSION STAN(5)
      CHARACTER*64 STAN
      COMMON/CHPOLY/STAN
      COMMON/CPOLYG/NSTAN
      COMMON/BATCFL/IBATCH
C*****************************************************
C *** GETTING INORMATION FROM THE MENU SYSTEM
      IBS=1
      IES=8192
      IBATCH=0
      NAMENU='FSPDIS.MNU'
5      CONTINUE
        CALL G3LINK(NAMENU)
        IF(NAMENU.EQ.'STOP')THEN
           CALL STPL(-1.,1.)
           STOP
C M        RETURN
        ENDIF
C *** IRES=0 DOES NOT PLOT THE RESIDUALS, =1 DOES, SPECIFIED IN MENU
      IRES=0
      IF(VRE.EQ.'Y'.OR.VRE.EQ.'y')IRES=1
        ICFLAG=0
C ICFLAG=0 USES CHANNEL =1 CONVERTS TO ENERGY - SWITCH MADE IF .XDA FILE
C *** SPECIFIED IN MENU
          IT=INDEX(GENAME,'.')
          ANS='N'
          IF(GENAME(IT+1:).EQ.'XDA'.OR.GENAME(IT+1:).EQ.'xda')ANS='Y'
        NK=0
C *** NK IS THE NUMBER OF ENERGY CONSTANTS BEYOND LINEAR (0 IF NO XDA)
        E1=0
        E2=1
        E3=0
        LABEL='CHANNEL NUMBER\'
        IF(ANS.EQ.'Y'.OR.ANS.EQ.'y')THEN
          STATUS='OLD'
          CALL MAOPEN(2,GENAME,STATUS)
          LABEL='ENERGY (KeV)\'
          ICFLAG=1
        READ(2,*)JM
        NK=(JM-2)/2
          READ(2,*)E1,E2
          DO 8 I=1,NK
          READ(2,*)EK(I),AK(I)
8       CONTINUE
        CLOSE(2)
      ENDIF
C*********************************************
C
C  INLINE GBGRAF
C
C**********************************************
C *** IREP=0 MEANS THAT THIS IS THE FIRST PASS - NO QUESTIONS- ALL VALUES
C *** FROM THE MENU  GDNAME IS THE NAME OF THE FILE MADE BY FSPFIT WITH
C *** THE GRAPHICS INFO (CONST FILE NAME, STANDARD FILES, PEAK FILEE)
C *** NOTE THAT THESE CAN BE CHANGED WITH AN EDITOR AND FSPDIS WILL SHOW
C *** RESULTS OF NEW VALUES
210   IREP=0
      NA=GDNAME
102   FORMAT(A40)
      STATUS='OLD'
      CALL MAOPEN(3,NA,STATUS)
      READ(3,'(A,G020.12)')II,CHIS
        DO 212 I=1,7
212     IF(II(I:I).EQ.'\')II(I:I)=' '
      READ(3,*)IP
C *** INITIALISE THE STANDARD
      NSTAN=IP
      IF(NSTAN.GT.0)THEN
        NC=0
        XT=0
        XTT=POLYG3(XT,1,ST)
      ENDIF
      READ(3,*)IEXTWT
      READ(3,'(3(A64/),A64)')DATAFN,NADIFF,IBKGFN,IPEKFN
      READ(3,'(2I5)')IB,IE
      READ(3,'(2G20.12)',END=335,ERR=335)(FW(1,I),FW(2,I),I=1,IP)
      DO 333 J=1,IP
333   FW(3,J)=0.
335   CLOSE(3)
C *** BREAD READS IN THE DATA FILE AND CONSTRUCTS THE APPROPRIATE WEIGHTS

      CALL BREAD(XOFF,F,IIT,IHD,DATAFN,1,4096,N)
        IF(IEXTWT.GT.2.OR.IEXTWT.LT.0)THEN
          PRINT*,' IEXTWT=',IEXTWT,' NOT ALLOWED'
          READ(*,*)ITEST
          STOP
C M       RETURN
        ENDIF
        IF(IEXTWT.EQ.2)THEN
          CALL BREAD(XOFF,F,IIT,IHD,NADIFF,IB,IE,N)
        ELSE
          CALL WXCALC(F,FAA,WX,N,IEXTWT)
        ENDIF

      IXOFF=XOFF
      IF(IENDC.GT.N.AND.ICFLAG.EQ.0)IENDC=N
      IDEFE=0
      IF(IENDC.EQ.0)THEN
        IENDC=IE
        IDEFE=1
      ENDIF
      NDT=N+XOFF
C *** READ IN THE BACKROUND CONSTS
      DO 555 I=1,200
555   CONS(I)=0.0
      STATUS='OLD'
      CALL MAOPEN(1,IBKGFN,STATUS)
      READ(1,*)IBKGFL
      NV=0
20    READ(1,*,END=25)CONS(NV+1)
      NV=NV+1
      GOTO 20
25    CLOSE(1)
C *** ROPKS READS THE PEAK FILE AND CALCULATES THE NEEDED CONSTANTS
      NPP=0
      IF(NSTAN.GT.0)
     2 CALL ROPKS(C,SC,W,IPT,WF,SW,XP,SXP,N,NPP,FW,XOFF)
C*******************************************
C
C INLINE GBGRAF
C
C********************************************
        RCHIS=CHIS
        IXDUM=IXOFF
        IXDUM=MIN0(4096,MAX0(0,IXDUM))
        IX=IXDUM
301     CONTINUE
302     IMC=NDT-1
320     CONTINUE
        IF(ICFLAG.EQ.1.)THEN
           EBEG=E1+E2*IB
           DO 36 IK=1,NK
36         EBEG=EBEG+EK(IK)*AMAX1(0.,AK(IK)-IB)**2
           ITEMP=EBEG
           EBEG=ITEMP
           EEND=E1+E2*IMC
           DO 377 IK=1,NK
377        EEND=EEND+EK(IK)*AMAX1(0.,AK(IK)-IMC)**2
           ITEMP=EEND
           EEND=ITEMP
        ENDIF
        IF(IREP.EQ.1)THEN
          WRITE(*,103)IX,IMC
103       FORMAT(' DATA HAS CHANNELS',2I5,
     # ' INITIAL CHANNEL?, END CHANNEL?,<-1>FOR MENU <-2> TO STOP')
        ENDIF
           EENDT=0.
        IF(ICFLAG.EQ.1)THEN
           IF(IDEFE.EQ.0)THEN
             EBEGT=IBEGC
             EENDT=IENDC
           ELSE
             EENDT=0
           ENDIF
           IF(IREP.EQ.1)THEN
           PRINT*,' DATA COVERS ENERGIES ',EBEG,EEND,' ENTER EBEG,EEND'
           PRINT*,' ENTER <-1> FOR MENU, <-2> TO STOP'
             CALL CHREAD(9,80,RANS,CANS,2,0,IEND)
             IF(RANS(1).EQ.-2.)THEN
               CALL STPL(-1.,-1.)
               STOP
C M            RETURN
             ENDIF
             IF(RANS(1).EQ.-1.)GOTO 5
             EBEGT=RANS(1)
             EENDT=RANS(2)
           ENDIF
         ENDIF
380   CONTINUE
           IF(EENDT.NE.0..AND.ICFLAG.EQ.1)THEN
             EBEG=EBEGT
             EEND=EENDT
             IBEGC=(EBEG-E1)/E2+1
                DO 39 IT=1,3
                ERR1=E1+IBEGC*E2-EBEG
                EPP1=E2
                DO 38 IK=1,NK
                ERR1=ERR1+EK(IK)*AMAX1(0.,AK(IK)-IBEGC)**2
                EPP1=EPP1-2*EK(IK)*AMAX1(0.,AK(IK)-IBEGC)
38              CONTINUE
39              IBEGC=IBEGC-ERR1/EPP1
             IBEGC=MAX0(1,MIN0(IBEGC,N))
             IENDC=(EEND-E1)/E2+1
                DO 40 IT=1,3
                ERR2=E1+IENDC*E2-EEND
                EPP2=E2
                DO 399 IK=1,NK
                ERR2=ERR2+EK(IK)*AMAX1(0.,AK(IK)-IENDC)**2
399             EPP2=EPP2-2*EK(IK)*AMAX1(0.,AK(IK)-IENDC)
                IENDC=IENDC-ERR2/EPP2
40              CONTINUE
             IENDC=MIN0(MAX0(2,IENDC),N)
           ENDIF
        IF(IREP.EQ.1.AND.ICFLAG.EQ.0)THEN
          CALL CHREAD(9,80,RANS,CANS,2,0,IEND)
          IBEGC=RANS(1)
          IENDC=RANS(2)
        ENDIF
        IF(IBEGC.EQ.-2)THEN
          CALL STPL(-1.,-1.)
          STOP
C M       RETURN
        ENDIF
        IF(IBEGC.EQ.-1)GOTO 5
        IF(IENDC.NE.0)GOTO 330
        IBEGC=IB
        IENDC=IE
330     IXDUM=IX
        IBEGC=MAX0(0,IBEGC)
        IENDC=MIN0(N,IENDC)
        IF(IBEGC.GE.IENDC)THEN
        WRITE(*,104)IBEGC,IENDC
104     FORMAT(' THERE ARE NO CHANNELS IN THE INTERVAL',I10,',',I10)
        CALL CHREAD(9,80,RANS,CANS,2,0,IEND)
        STOP
C M     RETURN
        ENDIF
        IF(ICFLAG.NE.1)THEN
           EBEG=IBEGC
           EEND=IENDC
        ENDIF
C *** FINDING THE DATA SIZE RANGE
        AMIN=2000000000
        AMAX=-AMIN
        IBT=MAX0(1,IBEGC)
        DO 340 I=IBT,IENDC
        AMIN=AMIN1(AMIN,F(I-IXOFF))
340     AMAX=AMAX1(AMAX,F(I-IXOFF))
        AMINT=AMIN
       IF(IREP.EQ.1)WRITE(*,108)AMINT,AMAX
108     FORMAT(' AMIN=',F8.0,'  AMAX=',F8.0,' ENTER DESIRED MIN',
     # ',DESIRED MAX')
        IF(IREP.EQ.1)THEN
          CALL CHREAD(9,80,RANS,CANS,2,0,IEND)
          IMIN=RANS(1)+.5
          IMAX=RANS(2)+.5
        ENDIF
        IF(IMAX.EQ.0)GOTO 37
        AMIN=IMIN
        AMINT=AMIN
        AMAX=IMAX
37      CONTINUE
        IF(IREP.EQ.1)WRITE(*,120)
120     FORMAT(' DO YOU WANT A LOG SCALE?')
        IF(IREP.EQ.1)THEN
          CALL CHREAD(9,80,RANS,CANS,0,1,IEND)
          LFLAG=0
          IF(CANS(1).EQ.'Y'.OR.CANS(1).EQ.'y')LFLAG=1
        ENDIF
      IF(LFLAG.EQ.1)THEN
C *** SCALING THE MAX AND MIN TO LOGARITHMS
        AMIN=AMAX1(.01,AMIN)
        AMAX=ALOG(AMAX)
        AMIN=ALOG(AMIN)
      ENDIF
        IF(LFLAG.EQ.0)AMIN=AMINT
      IF(IBATCH.NE.4)CALL STPL(-1.,-1.)
        IAXIS=1
        CALL AXIS(AMIN,AMAX,LFLAG,IRES,EBEG,EEND,II,RCHIS,LABEL)
         HRES=EVERT-.1021
        XSF=SHORI*E2/2
        ISKIP=2/(1000*SHORI*E2)
        ISKIP=MAX0(1,ISKIP)
        IF(VDATA.EQ.'N')GOTO 505
        DO 500 I=IBT,IENDC,ISKIP
        ET=E1+E2*I
        DO 418 IK=1,NK
418     ET=ET+EK(IK)*AMAX1(0.,AK(IK)-I)**2
420     XPP=BHORI+SHORI*(ET-EBEG)
        XPP=AMAX1(BHORI,XPP)
        IF(I.EQ.IBT)XO=BHORI
      IF(LFLAG.EQ.0)THEN
        ADAT=AMULT*F(I-IXOFF)
      ELSE
        ADAT=ALOG(AMAX1(1E-3*AMULT,AMULT*F(I-IXOFF)))
      ENDIF
        YDAT=SVERT*(ADAT-AMIN)+BVERT
        YDAT=AMAX1(BVERT,YDAT)
C *** THE FIRST PLOT CALL
        IF(I.EQ.IBT)CALL STPL(XO,YDAT)
        IF(ISKIP.GT.1)GOTO 495
        IF(I.GT.IBEGC)CALL PLOT(XO,YDAT)
        XO=XPP+XSF
        CALL PLOT(XO,YDAT)
        GOTO 500
495     ADMAX=-2.E9
        ADMIN=-ADMAX
        DO 497 J=1,ISKIP
        IARG=MIN0(IMC,I+J-1)
        ACOMP=AMULT*F(IARG-IXOFF)
        ADMAX=AMAX1(ACOMP,ADMAX)
497     ADMIN=AMIN1(ADMIN,ACOMP)
      IF(LFLAG.EQ.1)THEN
        ADMIN=ALOG(AMAX1(1.E-3,ADMIN))
        ADMAX=ALOG(AMAX1(1.E-3,ADMAX))
      ENDIF
        YDMIN=SVERT*(ADMIN-AMIN)+BVERT
        YDMAX=SVERT*(ADMAX-AMIN)+BVERT
        YDMIN=AMAX1(YDMIN,BVERT)
        YDMAX=AMAX1(YDMAX,BVERT)
        CALL PLOT(XO,YDMIN)
        XO=XPP+XSF
        CALL PLOT(XO,YDMAX)
500     CONTINUE
505     CONTINUE
      ANS=VCF
        IF(ANS.NE.'Y'.AND.ANS.NE.'y')GOTO 650
        IIPKS=0
          INT=1+SHORI*E2*100
          IMIN=INT/2+1
        CALL LINET('SDAS')
        DO 600 I=IBT,IENDC
          DO 580 J=1,INT
          XG3=I+(J-IMIN)/(1.*INT)
          ET=E1+XG3*E2
          DO 572 IK=1,NK
572       ET=ET+EK(IK)*DMAX1(0D0,AK(IK)-XG3)**2
        XPP=BHORI+SHORI*(ET-EBEG)
        XPP=AMAX1(XPP,BHORI)
          CALL G3POLY(XG3,PD,NV,CONS,FDP)
        FA=FDP
      IF(NSTAN.GT.0)FA=POLYB3(XG3,PD,C,XP,W,IPT,NPP,0)+FDP
        AFRA=AMULT*FA
      IF(LFLAG.EQ.1)THEN
        AFRA=ALOG(AMAX1(1.E-3,AMULT*FA))
      ENDIF
       Y=SVERT*(AFRA-AMIN)+BVERT
       IF(I.EQ.IBT.AND.J.EQ.1)THEN
         CALL STPL(XPP,Y)
       ELSE
         CALL PLOT(XPP,Y)
       ENDIF
580       CONTINUE
600     CONTINUE
650     CONTINUE
        IF(NSTAN.EQ.0)GOTO 780
        ANS=VBG
        IF(ANS.NE.'Y'.AND.ANS.NE.'y')GOTO 700
        CALL LINET('DODA')
        DO 680 I=IBT,IENDC,ISKIP
        ET=E1+E2*I
        DO 672 IK=1,NK
672     ET=ET+EK(IK)*AMAX1(0.,AK(IK)-I)**2
        XPP=BHORI+SHORI*(ET-EBEG)
        XPP=AMAX1(XPP,BHORI)
        CALL G3POLY(1.D0*I,PD,NV,CONS,FDP)
      IF(LFLAG.EQ.1)THEN
        FDP=DLOG(DMAX1(1.D-3,AMULT*FDP))
      ELSE
        FDP=AMULT*FDP
      ENDIF
        YB=SVERT*(FDP-AMIN)+BVERT
        IF(I.EQ.IBT)CALL STPL(XPP,YB)
        CALL PLOT(XPP,YB)
680     CONTINUE
700     CONTINUE
        IIPKS=1
      ANS=VPE
        IF(ANS.NE.'Y'.AND.ANS.NE.'y')GOTO 780
        CALL LINET('DOT ')
          INT=1+SHORI*E2*100
          IMIN=INT/2+1
        DO 755 K=1,NPP
        ISM=W(K)*XPS(IABS(IPT(K)))+XP(K)
        ILA=W(K)*XPL(IABS(IPT(K)))+XP(K)
        I1=MAX0(ISM,IBT)
        I2=MIN0(ILA,IENDC)
        DO 750 I=I1,I2
          DO 745 J=1,INT
          XG3=I+(J-IMIN)/(1.*INT)
          ET=E1+XG3*E2
          DO 742 IK=1,NK
742       ET=ET+EK(IK)*DMAX1(0D0,AK(IK)-XG3)**2
        XPP=BHORI+SHORI*(ET-EBEG)
        XPP=AMAX1(XPP,BHORI)
          CALL G3POLY(XG3,PD,NV,CONS,FDP)
      FA=C(K)*C(K)*POLYG3((XG3-XP(K))/W(K),IPT(K),SP)+FDP
        AFRA=AMULT*FA
      IF(LFLAG.EQ.1)THEN
        AFRA=ALOG(AMAX1(1.E-3*AMULT,AMULT*FA))
      ENDIF
        Y=SVERT*(AFRA-AMIN)+BVERT
        IF(I.EQ.I1.AND.J.EQ.1)THEN
          CALL STPL(XPP,Y)
        ELSE
          CALL PLOT(XPP,Y)
        ENDIF
745     CONTINUE
750     CONTINUE
755     CONTINUE
780     IF(IRES.EQ.0)GOTO 900
        CALL LINET('SOLI')
        CHISL=0
        ICSTPL=0
        DO 800 I=IBT,IENDC
        XG3=I
        CALL G3POLY(XG3,PD,NV,CONS,FDP)
        FA=FDP
      IF(NSTAN.GT.0)FA=POLYB3(XG3,PD,C,XP,W,IPT,NPP,0)+FDP
      IF(IEXTWT.EQ.0)WX(I-IXOFF)=1./(AMAX1(1.,FA))
        NRES=100*AMIN1(327.,(F(I-IXOFF)-FA)*SQRT(WX(I-IXOFF)))
        IF(NRES.GT.32767)NRES=NRES-65528
        ET=E1+E2*I
        DO 784 IK=1,NK
784     ET=ET+EK(IK)*AMAX1(0.,AK(IK)-I)**2
        XPP=BHORI+SHORI*(ET-EBEG)
        IF(XPP.LT.BHORI)GOTO 800
        FIX=HRES+.3125E-3*NRES

        IF(ICSTPL.EQ.0)THEN
          CALL STPL(XPP,FIX)
          ICSTPL=1
        ELSE
          CALL PLOT(XPP,FIX)
        ENDIF

        CHISL=CHISL+(NRES/100.)**2
800     CONTINUE
        CLINE='CHISL=\'
        CALL BCHART(0.,0.,CLINE)
        CALL NUMOUT(0.1,0.,CHISL,0)
900     CONTINUE
        CALL STPL(0.,0.)
        CALL CHREAD(9,80,RANS,CANS,0,20,IEND)
        IF(CANS(1).EQ.'C'.AND.CANS(2).EQ.' ')THEN
           CALL CLEARS
           STOP
        ENDIF
        IF(CANS(1).NE.' ')THEN
          CTEMP=CLINE(1:20)//' '
          IT=INDEX(CTEMP,'.')
          IF(IT.GT.0)THEN
          IF(CTEMP(IT+1:).EQ.'MNU'.OR.CTEMP(IT+1:).EQ.'mnu')THEN
            IBATCH=4
            NAMENU=CTEMP
            CALL LINET('SOLI')
            GOTO 5
          ENDIF
          ENDIF
        IF((CANS(1).EQ.'M'.OR.CANS(1).EQ.'M').AND.IT.EQ.0)THEN
           IBATCH=0
           CALL STPL(-1.,-1.)
           GOTO 5
        ENDIF
        IF(CANS(1).EQ.'Y'.OR.CANS(1).EQ.'y'.OR.CANS(1).EQ.'S'.
     # OR.CANS(1).EQ.'s'.OR.CANS(1).EQ.'E'.OR.CANS(1).EQ.'e'
     # )THEN
        CALL STPL(-1.,-1.)
        STOP
C M     RETURN
      ENDIF
      IREP=1
      IP=1
      IF(ICFLAG.EQ.1)THEN
        EBEGT=RNUMB(CANS,IP,20)
        EENDT=RNUMB(CANS,IP,20)
        CALL STPL(-1.,-1.)
        GOTO 380
      ENDIF
      IBEGC=RNUMB(CANS,IP,20)
      IENDC=RNUMB(CANS,IP,20)
      CALL STPL(-1.,-1.)
      GOTO 330
       ENDIF
        IBATCH=0
        CLINE= ' STOP <S>; MAKE WDAT FILE <W>;'//
     2 ' CONTINUE <CR>; OVERLAY <NAME.MNU>\'
        CALL BCHART(0.,.98,CLINE)
        CLINE=' '
        CALL CHREAD(9,80,RANS,CANS,0,20,IEND)
          CTEMP=CLINE(1:20)//' '
          IT=INDEX(CTEMP,'.')
          IF(IT.GT.0)THEN
          IF(CLINE(IT+1:).EQ.'MNU'.OR.CLINE(IT+1:).EQ.'mnu')THEN
            IBATCH=4
            NAMENU=CLINE
            GOTO 5
          ENDIF
          ENDIF
        IF((CANS(1).EQ.'M'.OR.CANS(1).EQ.'M').AND.IT.EQ.0)THEN
          CALL STPL(-1.,-1.)
          GOTO 5
        ENDIF
        IF(CANS(1).EQ.'Y'.OR.CANS(1).EQ.'y'.OR.CANS(1).EQ.'S'.
     # OR.CANS(1).EQ.'s'.OR.CANS(1).EQ.'E'.OR.CANS(1).EQ.'e'
     # )THEN
        CALL STPL(-1.,-1.)
        STOP
C M     RETURN
      ENDIF
C *** PART FROM PGRAF3 AT END OF LOOKING AT SPECTRUM
        IF(CANS(1).EQ.'W'.OR.CANS(1).EQ.'w')THEN
          PRINT*,' ENTER THE NAME OF THE FILE TO WRITE TO'
          READ(*,'(A)')NAINP
          NAINP=NAINP//'.'
          PRINT*,NAINP
          IT=INDEX(NAINP,'.')
          IF(IT.EQ.0)IT=LEN(NAINP)+1
          NAWDAT=NAINP(:IT-1)//'.WDAT'
          PRINT*,' DATA FOR VSHAPE WILL BE IN FILE ',IT,NAWDAT
          STATUS='UNKNOWN'
          CALL MAOPEN(12,NAWDAT,STATUS)
2997      PRINT *,' ENTER CHANNEL OF PEAK TO DELETE <-1000> IF NONE'
          READ(*,*)XPDEL
          IF(XPDEL.EQ.-1000.)GOTO 2999
C *** FIND PEAK CLOSEST TO XPDEL AND SET ITS COEFF TO ZERO
          XDIFF=DABS(XPDEL-XP(1))
          IPD=1
          DO 2998 I=2,NPP
          XC=DABS(XPDEL-XP(I))
          IF(XC.GT.XDIFF)GOTO 2998
          IPD=I
          XDIFF=XC
2998      CONTINUE
          C(IPD)=0.D0
          GOTO 2997

2999      PRINT*,' ENTER THE BEGINNING AND ENDING CHANNELS TO WRITE'
          READ(*,*)IBEGC,IENDC
          IF(IENDC.EQ.0)GOTO 3050
          DO 3000 I=IBEGC,IENDC
          XG3=I
          CALL G3POLY(XG3,PD,NV,CONS,FDP)
          FA=POLYB3(XG3,PD,C,XP,W,IPT,NPP,0)+FDP
          WRITE(12,*)XG3,F(I-IXOFF)-FA,WX(I-IXOFF)
3000      CONTINUE
3050      CLOSE(12)
          IREP=1
          GOTO 320
        ENDIF
      IF(CANS(1).NE.' ')THEN
      IREP=1
      IP=1
      IF(ICFLAG.EQ.1)THEN
        EBEGT=RNUMB(CANS,IP,20)
        EENDT=RNUMB(CANS,IP,20)
        GOTO 380
      ENDIF
      IBEGC=RNUMB(CANS,IP,20)
      IENDC=RNUMB(CANS,IP,20)
      GOTO 330
      ENDIF
        IREP=1
       GOTO 320
        END
C*INCLUDE G3LINK FORTRAN
C*INCLUDE MENURD FORTRAN
C*INCLUDE NUMOUT FORTRAN
C*INCLUDE CMSPLOTS FORTRAN
C*INCLUDE BREAD FORTRAN
C*INCLUDE MAOPEN FORTRAN
C$NOLIST
C$INCLUDE C:\ROBFIT\FORTRAN\G3LINK
C$INCLUDE C:\ROBFIT\FORTRAN\MENURD
C$INCLUDE C:\ROBFIT\FORTRAN\NUMOUT
C$INCLUDE C:\ROBFIT\FORTRAN\VPLOTS
C$INCLUDE C:\ROBFIT\FORTRAN\BREAD
C$INCLUDE C:\ROBFIT\FORTRAN\MAOPEN
C$LIST
       FUNCTION POLYB3(X,P,C,XP,W,IPT,NPPS,IXMID)
C**************************************
C 6 EXTRA COEFS HAVE BEEN ADDED TO P
C
C  6/6/88 GB
C**************************************
       IMPLICIT REAL*8(A-H,O-Z)
C THIS ROUTINE CALCULATES POLYB=SUM CI*SI ALONG WITH DPOLYB/DCI
C THE NEXT NS SI'S, WHICH HAVE DERIVATIVES WITH RESPECT TO C,XP,AND W
C ARE BSPLINES
      COMMON/SCONS/DU(150),XPS(5),XPL(5)
       DIMENSION P(1),C(256),XP(256),W(256),IPT(256)
      NS=NPPS
      NC=1
5     NPART=1
      POLYB3=0
      DO 200 J=1,NS
        ISIGN=1
        IF(IPT(J).LT.0)ISIGN=-1
        ITEST=IABS(IPT(J))
      P(NPART)=0.
      P(NPART+1)=0.
      P(NPART+2)=0.
      XM=X-XP(J)
      IF(XM.LE.W(J)*XPS(ITEST).OR.XM.GE.W(J)*XPL(ITEST))GOTO 200
      XM=XM/W(J)
C  PARTIAL WRT C(J)
       FADD=ISIGN*C(J)*POLYG3(XM,ITEST,SP)
      P(NPART)=2.*FADD
      POLYB3=POLYB3+C(J)*FADD
C PARTIAL WRT XP(J)
      P(NPART+1)=-ISIGN*C(J)*C(J)*SP/W(J)
C PARTIAL WRT W(J)
      P(NPART+2)=P(NPART+1)*XM
200   NPART=NPART+3
      RETURN
      END
        SUBROUTINE G3POLY(X,P,NV,CONS,FA)
C *** THE FIRST FOUR COEFFICIENTS REPRESENT A CUBIC
C *** THE REST ARE IN THE FORM C(I)*(C(I+1)-X)+ **3
        IMPLICIT REAL*8 (A-H,O-Z)
         COMMON/CBKGFI/IBKGFL
        DIMENSION P(100),CONS(200)
        NVS=NV-4
        P(1)=1.D0
        FA=CONS(1)
        DO 5 I=2,4
        P(I)=X*P(I-1)
5       FA=FA+CONS(I)*P(I)
        DO 20 I=5,NV,2
        P(I)=0.D0
        P(I+1)=0.D0
        IF(CONS(I+1).LE.X)GOTO 20
        DIFF=CONS(I+1)-X
        DIFF2=DIFF*DIFF
        P(I+1)=3*CONS(I)*DIFF2
        P(I)=DIFF2*DIFF
        FA=FA+CONS(I)*P(I)
20      CONTINUE
      IF(IBKGFL.EQ.0)RETURN
      FA=DEXP(DMAX1(-60D0,DMIN1(30D0,FA)))
      DO 30 I=1,NV
30    P(I)=FA*P(I)
        RETURN
         END
      FUNCTION POLYG3(X,IPT,SP)
       IMPLICIT REAL*8(A-H,O-Z)
        CHARACTER*64 NA,STATUS
C****************************************************
C
C  DATA FROM MAIN ROUTINE
C
C****************************************************
      DIMENSION STAN(5)
      CHARACTER*64 STAN
      COMMON/CHPOLY/STAN
      COMMON/CPOLYG/NSTAN
C*****************************************************
      COMMON/SCONS/W(10,5),XP(10,5),C(10,5),XPS(5),XPL(5),IP,NC
C THIS ROUTINE CALCULATES POLYG=SUM CI*SI ALONG WITH DPOLY/DX
C IPT GIVES THE PEAK TYPE RANGING FROM 1 TO 5
      COMMON/PYG3/NST(5)
      IF(NC.EQ.1)GOTO 30
      NC=1
      NS=0
      IF(NSTAN.EQ.0)THEN
        POLYG3=0.
        NST(1)=0
        RETURN
      ENDIF
      DO 3 IP=1,NSTAN
      IF(IP.GT.5)GOTO 300
      READ(3,'(A)')STAN(IP)
      STATUS='OLD'
      CALL MAOPEN(11,STAN(IP),STATUS)
      NS=1
5     READ(11,101,END=20)C(NS,IP),XP(NS,IP),W(NS,IP)
101   FORMAT(5X,3E20.7)
      NS=NS+1
      GOTO 5
20    NS=NS-1
      CLOSE(11)
      XPS(IP)=1.D32
      XPL(IP)=-1.D32
      DO 25 J=1,NS
      XPS(IP)=DMIN1(XPS(IP),XP(J,IP)-W(J,IP))
25    XPL(IP)=DMAX1(XPL(IP),XP(J,IP)+W(J,IP))
        NST(IP)=NS
3     CONTINUE
        IP=NSTAN
C *** READ THE BLANK CARD
      READ(3,'(A)')NA
30    CONTINUE
       POLYG3=0.
      SP=0.
C
        IQT=IABS(IPT)
        ISIGN=1
        IF(IPT.LT.0)ISIGN=-1
        IF(NST(IQT).EQ.0)RETURN
C
        NS=NST(IQT)
C***** LIMIT THE VECTORIZATION
C*VDIR: ASSUME COUNT(5)
      DO 200 J=1,NS
      XM=X-XP(J,IQT)
      IF(XM.LE.-W(J,IQT).OR.XM.GE.W(J,IQT))GOTO 200
      XM=XM/W(J,IQT)
      FADD=C(J,IQT)*((1.+XM)*(1.-XM))**3
      POLYG3=POLYG3+ISIGN*FADD
       SP=SP+3.*ISIGN*(FADD/(1.+XM)-FADD/(1.-XM))/W(J,IQT)
200   CONTINUE
      RETURN
300     PRINT*,' ATTEMPT TO DEFINE MORE THAN FIVE STANDARDS'
        READ(*,*)ITEST
        STOP
      END
