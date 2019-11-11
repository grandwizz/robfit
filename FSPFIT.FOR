C$NOWAR
C$NOEXT
C$NOLIST
      PROGRAM FSPFIT
      IMPLICIT REAL*8 (A-H,O-Z)
      PARAMETER (MAXCHN=4096,MAXPKS=256,MAXFPK=50,MAXBGN=120)
C MAXCHN IS THE MAXIMUM NUMBER OF CHANNELS
C MAXPKS IS THE MAXIMUM NUMBER OF PEAKS
C MAXFPK IS THE MAXIMUM NUMBER OF FIXED PEAKS
C MAXBGN IS THE MAXIMUM NUMBER OF BACKGROUND CONSTANTS
      COMMON/SCONS/DU(150),XPS(5),XPL(5),IP,NGPCAL,XBEG,XEND
      COMMON/G3PLG/CG3(20,5),XPG3(20,5),WG3(20,5),NST(5)
      COMMON/FPEAKS/NPF,IPF,XPF(MAXFPK),EPF(MAXFPK),IPTF(MAXFPK)
      REAL*4 F,FA,FB,AMAX1,CUTOFF,WX,ALR,CHIOLD,
     #  XOFF,WXT
      CHARACTER*64 STATUS
      CHARACTER*64 NAMENU
      CHARACTER*80 II(4)
      CHARACTER*4 IFW,NOGP
      CHARACTER*1 APH
C *** WX=1/(EP*EP)
      CHARACTER*4 BKGF
      DIMENSION F(MAXCHN),FB(MAXCHN),WX(MAXCHN),FA(MAXCHN),WXT(MAXCHN)
      DIMENSION C(MAXPKS),W(MAXPKS),XP(MAXPKS),PD(768 ),IPT(MAXPKS)
      DIMENSION BCONS(MAXBGN),ARAT(5),WRAT(5)
      DIMENSION IHD(MAXCHN),SC(MAXPKS),SW(MAXPKS),SXP(MAXPKS),WF(MAXPKS)
      DIMENSION CHIOLD(3),FW(3,5),SFW(5,5)
C *** F(N) IS THE SPECTRUM CURRENTLY BEING FITTED.
C *** FA(N) IS A FOURIER SERIES LEAST SQUARES FIT TO THE BACKGROUND
C *** FB(N) IS THE FIT TO THE PEAKS.
C %%%%%%%
C*********************************************************
C
C DATA TO MAIN ROUTINE VROBFIT
C
C*****************************************************************
C
C      VROBFIT DATA
C
C***********************************************************
      DIMENSION WID(20),ERRO(20),ICHAN(20)
      CHARACTER*64 NABKG,NAPK,NAGR
      COMMON/CHVROB/BKGF,IFW,NOGP,NABKG,NAPK,NAGR
      COMMON/CVROB/WID,ERRO,CUTA,CUTB,ICHAN,NPPMAX,IMREF,NBMAX,NBMIN,
     # NPEAKT
      COMMON/BATCFL/IBATCH
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
      CHARACTER*64 NADAT,NAWT
      COMMON/CHBREA/NADAT,NAWT
      COMMON/CBREAD/N1,N2,IEXTWT
C******************************************************************
C      BKGFIT DATA
C******************************************************************
      CHARACTER*64 NAOBKG
      COMMON/CHBKGF/NAOBKG
      COMMON/CBKGFI/LFLAG
C
      DIMENSION VNODE(200)
      COMMON/PNODE/ VNODE,NOTS
C******************************************************************
C      ROPKS DATA
C******************************************************************
      CHARACTER*64 NAOPK
      COMMON/CHROPK/NAOPK,APH
C*****************************************************************
      DATA XVB/0.0D0/,XVE/0.D0/,FA/MAXCHN*0./,FB/MAXCHN*0./,NV/0/
      DATA IPW/0/,PCON/1.D6/
      DATA BCONS/MAXBGN*0.D0/
      NAMENU='FSPFIT.MNU'
      IBATCH=0
2     CALL VRLINK(NAMENU)
        IF(NAMENU.EQ.'STOP')STOP
        I1=1
        NPF=0
        IPF=0
        IDIR=0
      DO 33 I=1,4096
      FA(I)=0
33    FB(I)=0
      DO 31 I=1,256
      SC(I)=0.
      SXP(I)=0.
      SW(I)=0.
31    CONTINUE
3        NCALLB=0
         NGPCAL=0
        XT=0
        XTT=POLYG(XT,1,ST)
        DO 5 I=1,3
5       CHIOLD(I)=1.E30
        IREFIT=0
        IREC4=2
        IFPF=0
      NPPMAX=MAX0(1,MIN0(NPPMAX,255))
      WMAX=0
      DO 7 I=1,IP
C***************************
C
C  NEW BIT
C
C****************************
        IV=2*I-1
        W1=WID(IV)
        EW1=ERRO(IV)
        NC1=ICHAN(IV)
        W2=WID(IV+1)
        EW2=ERRO(IV+1)
        NC2=ICHAN(IV+1)
        WMAX=DMAX1(WMAX,W1+EW1,W2+EW2)
C***************************
       FW(2,I)=(W2**2-W1**2)/(NC2-NC1)
        FW(1,I)=W1**2-NC1*FW(2,I)
        EW12=(2*W1*EW1)**2
        EW22=(2*W2*EW2)**2
        SFW(2,I)=(EW22 -EW12)/(NC2-NC1)
        SFW(1,I)=EW12-NC1*SFW(2,I)
        DO 6 J=3,5
6       SFW(J,I)=0
        FW(3,I)=0
7       CONTINUE
         CUTOFF=CUTA
         XVB=0
         XVE=1
        PRINT*,' THE DATA IS FROM ',NADAT,' CHANNELS',N1,N2
        PRINT*,' THE WEIGHTS ARE FROM ',NAWT
        PRINT*,' BEFORE BREAD'
        CALL BREAD(XOFF,F,II,IHD,NADAT,N1,N2,N)
        IF(IEXTWT.GT.2.OR.IEXTWT.LT.0)THEN
          PRINT*,' IEXTWT=',IEXTWT,' NOT ALLOWED, TRY AGAIN'
          GOTO 2
        ENDIF
        IF(IEXTWT.EQ.2)THEN
          CALL BREAD(XOFF,F,II,IHD,NAWT,N1,N2,N)
        ELSE
          CALL WXCALC(F,FA,WX,N,IEXTWT)
        ENDIF
        PRINT*,' THE BACKGROUND CONS WILL BE PUT INTO ',NABKG
        PRINT*,' THE STARTING ESTIMATES FOR THE BKG WERE IN ',NAOBKG
        PRINT*,' THE PEAKS FOUND WILL BE PUT INTO ',NAPK
        PRINT*,' A STARTING ESTIMATE FOR THE PEAKS CAME FROM ',NAOPK
        PRINT*,' ARE THE PEAKS THE CORRECT HEIGHT?'
        PRINT*,' ENTER THE FILE NAME FOR THE GRAPHICAL DATA'
        PRINT*,' THE GRAPHICAL DATA IS IN ',NAGR
        NPP=0
        CALL ROPKS(C,SC,W,IPT,WF,SW,XP,SXP,N,NPP,FW,XOFF)
        XBEG=100000
        XEND=0
        DO 50 I=1,NPP
        ITEST=IABS(IPT(I))
        XBEG=DMIN1(XBEG,W(I)*XPS(ITEST))
50      XEND=DMAX1(XEND,W(I)*XPL(ITEST))
         IF(NPP.EQ.0)GOTO 90
         IF(APH.EQ.'Y')GOTO 90
         BKGCUT=CUTA
         DO 65 I=1,50
        CALL BKGFIT(XOFF,F,FA,WX,N,BCONS,NV,NBMAX,BKGF,LFLAG,NCALLB,
     # BKGCUT,NAOBKG)
        IF(NV.GE.NBMIN.OR.NCALLB.EQ.30)GOTO 68
65      CONTINUE
68      CONTINUE
         DO 72 I=1,N
         F(I)=F(I)-FA(I)
72      CONTINUE
         DO 75 I=1,NPP
         C(I)=CFEST(W(I),IPT(I),F,FA,WX,XP(I)-XOFF,N,ERRSQ,IFW)
75       CONTINUE
         PRINT'(A/(3G15.6,I3))',' AFTER CFEST C W XP,IPT',
     #   (C(I),W(I),XP(I),IPT(I),I=1,NPP)
90       CONTINUE
       WRITE(*,99)W1,W2
99      FORMAT(' FOR THE FIRST 50 PEAKS, PEAK WIDTHS ARE ASSUMED'/
     # ' LINEAR BEGINNING AT',F7.2,' AND ENDING AT',F7.2)
      WRITE(*,101)N,NPPMAX,XOFF
101   FORMAT(I5,' CHANNELS, AT MOST',I5,' PEAKS.'/
     #'  ZEROTH CHANNEL IS ',F8.0)
      NIT=0
       IF(IFW.EQ.'VWID')IWF=2
       IF(IFW.EQ.'VWIN')IWF=-2
       IF(IFW.EQ.'FIXD')IWF=9
       IF(IFW.EQ.'FIXN')IWF=-9
        IF(IWF.EQ.0)THEN
          PRINT*,' YOU HAVE CHOSEN FIT TYPE ',IFW
          PRINT*,' ALLOWED TYPES ARE:'
          PRINT*,' VWID pos peaks, varing widths and locations'
          PRINT*,' VWIN pos and neg peaks, vary w and loc'
          PRINT*,' FIXD pos peaks vary strengths only'
          PRINT*,' FIXN pos and neg peaks vary strength only'
        ENDIF
       NITREF=0
290    CONTINUE
      WRITE(*,107)CUTOFF
107   FORMAT(' THE CODE WILL ATTEMPT TO ADD PEAKS UNTIL THE LARGEST',
     # ' RESIDUAL IS <',F10.2)
        XT=XOFF+N/2
        IF(NPP.GT.20)XT=.5*(XP(1)+XP(NPP))
        DO 295 I=1,N
         XI=I+XOFF
         FB(I)=POLYA(XI,C,XP,W,IPT,NPP)
         F(I)=F(I)+FA(I)-FB(I)
         WXT(I)=WX(I)
         WX(I)=WX(I)/(1+.5*FB(I)*FB(I)*WX(I))
295      CONTINUE
297     BKGCUT=CUTOFF
        IF(NV.GE.NBMAX.AND.BKGF.EQ.'CONT')BKGF='NONK'
        IF(NV.GE.NBMAX.AND.BKGF.EQ.'FIXK')BKGF='FINK'
        PRINT*,' NV,NBMAX,BKGF',NV,NBMAX,BKGF
        CALL BKGFIT(XOFF,F,FA,WX,N,BCONS,NV,NBMAX,BKGF,LFLAG,NCALLB,
     # BKGCUT,NAOBKG)
        IF(NV.LT.NBMIN.AND.BKGF.NE.'NOBF')THEN
           BKGF='CONT'
           GOTO 297
        ENDIF
300   CHIS=0.D0
      DO 1119 I=1,N
        F(I)=F(I)-FA(I)+FB(I)
         WX(I)=WXT(I)
         IF(IEXTWT.EQ.0)WX(I)=1./AMAX1(1.,FA(I)+FB(I),
     #     1.E-4*(FA(I)+FB(I))**2)
         IF(WX(I).LT.0.)THEN
            PRINT*,'IEXTWT=',IEXTWT
            PRINT*,' WX<0 ,I,FA,FB,WX ',I,FA(I),FB(I),WX(I)
            READ(*,*)ITEST
         ENDIF
      CHIS=CHIS+(F(I)-FB(I))**2*WX(I)
1119  CONTINUE
      WRITE(*,1120)NV,CHIS
1120  FORMAT(' AFTER BKGFIT WITH',I5,' COEFS, CHIS=',E20.12)
C *** NCALLB = 30 IS A FLAG TO SAY THAT NO NEW FITTING WAS DONE

        IF(NCALLB.EQ.30)GOTO 290

        IPOSPK=0
        IF(IABS(IWF).EQ.9)GOTO 315
        CHIAVE=CHIOLD(1)
        DO 310 I=1,2
        CHIAVE=CHIAVE+CHIOLD(I+1)
310     CHIOLD(I)=CHIOLD(I+1)
        CHIOLD(3)=CHIS
        IF(CHIAVE-3*CHIS.LT.300)GOTO 315
        IF(IREC4.GT.0)GOTO 390

C *** REFIT SECTION

315     IREC4=2
        IF(NPP.EQ.0)GOTO 550
        IF(NPP.GT.15.AND.IFW.NE.'FWID'.AND.IABS(IWF).NE.9)
     # IWF=3
        IF(IFW.EQ.'VFWI'.AND.NPP.GT.20)IWF=4
        WRITE(*,1399)
1399    FORMAT(' REFITTING ALL THE PEAKS')
        CHSBRE=CHIS
        IP1=1
        I=1
320     ILR=I
        CALL PFIT(N,ILR,F,FB,WX,C,SC,W,IPT,WF,SW,XP,SXP,NPP,
     # CHIS,2,IWF,IP1,IPJ,FW,SFW,XOFF,IPW,WP)
        DO 330 K=IP1,IPJ
1857    FORMAT(I5,F8.2,F6.2,F7.1,G11.3,F6.2,F6.2,I2,F10.0)
330     WRITE(*,1857)K,XP(K),SXP(K),C(K),SC(K),W(K),SW(K),IPT(K),CHIS
        I=IPJ+1
        IF(I.LE.NPP)GOTO 320
        DO 340 I=1,3
340     CHIOLD(I)=1.E30
        CHIS=0
        DO 342 I=1,N
        CHIS=CHIS+(F(I)-FB(I))**2*WX(I)
342     CONTINUE
        WRITE(*,1832)CHIS
1832    FORMAT(' THE CHIS AFTER REFITTING',E12.6)
        NITREF=NITREF+1
        PRINT*,' NITREF,CHSBRE,CHIS',NITREF,CHSBRE,CHIS
        IF(NITREF.GT.3)GOTO 550
        IF((ABS(CHSBRE-CHIS).GT..5))GOTO 290
        GOTO 550

C *** NORMAL PEAK ADDING SECTION


390     CONTINUE
C *** CHECK TO SEE THAT ALL FIXED PEAKS ARE PRESENT
        PRINT*,' INTO FIXED PEAK SECTION, NPF=',NPF
        IP1=1
        IFTC=0
        DO 420 K=1,NPF
        IF(XPF(K)+2*EPF(K).LT.XOFF)GOTO 420
        IF(XPF(K)+XOFF-2*EPF(K).GT.N)GOTO 420

C *** FIND PEAK CLOSEST TO XPF(K)
        JF=1
        XDIST=1.E30
        DO 405 J=1,NPP
        XC=DABS(XPF(K)-XP(J))
        IF(IPT(J).NE.IPTF(K))GOTO 405
        IF(XC.GT.XDIST)GOTO 405
        JF=J
        XDIST=XC
405     CONTINUE
        IF(XDIST.LT.2*EPF(K))GOTO 420
        CALL LOCATE(XPF(K),XP,NPP,JB)
        JB=JB+1
        DO 410 I=NPP,JB,-1
        SC(I+1)=SC(I)
        SXP(I+1)=SXP(I)
        SW(I+1)=SW(I)
        C(I+1)=C(I)
        XP(I+1)=XP(I)
        W(I+1)=W(I)
410     IPT(I+1)=IPT(I)
        XP(JB)=XPF(K)
        IPT(JB)=IPTF(K)
        L=IABS(IPT(JB))
        XT=XP(JB)
        W(JB)=DSQRT(DMAX1(3D0,FW(1,L)+XT*(FW(2,L)+XT*FW(3,L))))
        C(JB)=CFEST(W(JB),IPT(JB),F,FA,WX,XP(JB)-XOFF,N,ERRSQ,IFW)
        NPP=NPP+1
        IFTC=1
        ILR=JB
        PRINT*,' BEFORE CALL TO PFIT JB,XP(JB),ILR ',JB,XP(JB),ILR
        PRINT'(3G16.6,I2)',(XP(I),C(I),W(I),IPT(I),I=1,NPP)
        CALL PFIT(N,ILR,F,FB,WX,C,SC,W,IPT,WF,SW,XP,SXP,NPP,
     # CHIS,2,IWF,IP1,IPJ,FW,SFW,XOFF,IPW,WP)
      WRITE(*,'('' FPEAK NPP JB IWF,XP(JB),W,CHIS''/
     # I4,I3,I2,F7.1,F5.2,G16.8)')NPP,JB,IWF,XP(JB),W(JB),CHIS
        PRINT'(3G16.6,I2)',(XP(I),C(I),W(I),IPT(I),I=1,NPP)
420     CONTINUE
        IFPF=IFPF+1
        IF(IFTC.EQ.1.AND.IFPF.LE.1)GOTO 290
        IQM=0
501     CONTINUE
        IF(NPP.GT.NPPMAX)GOTO 290
        NRESP=20*WMAX
        CALL RESL(F,FB,WX,FW,ALR,IQM,ILR,IFTC,NRESP,N,XOFF,
     # CUTOFF)
        IF(ALR.GT.0.OR.IWF.NE.-2)IPOSPK=IPOSPK+1
        IF(IFTC.EQ.1.AND.IPOSPK.EQ.0)GOTO 315
         IF(IFTC.EQ.1.AND.IPOSPK.LT.3)IREC4=IREC4-1
        IF(IFTC.EQ.1)GOTO 290
        NPPO=NPP
        CHISO=CHIS
        IFITT=1
        IF(ALR.LT.0)IFITT=-1
        CALL PFIT(N,ILR,F,FB,WX,C,SC,W,IPT,WF,SW,XP,SXP,NPP,
     # CHIS,IFITT,IWF,I1,JP,FW,SFW,XOFF,IPW,WP)
        TNPKS=.1*(ALR-CUTOFF)**2
        TNPKS=DMAX1(TNPKS,1.D0)
        XT=ILR+XOFF
      WRITE(*,119)NPP,IQM,IWF,XT,WP,ALR,CHIS
119   FORMAT(I5,I3,I2,4G14.6)
        IF(ALR.LT.0.D0.AND.IWF.NE.-2)GOTO 501
        IF(ALR.GT.0.D0.AND.IWF.EQ.-2)GOTO 501
        IF(IABS(IWF).EQ.9)GOTO 501
        IF(NPP.GT.NPPO)GOTO 501
        IF(CHISO-CHIS.GT.TNPKS)GOTO 501
        IFITT=3
        IF(ALR.LT.0)IFITT=-3
        CALL PFIT(N,ILR,F,FB,WX,C,SC,W,IPT,WF,SW,XP,SXP,NPP,
     # CHIS,IFITT,IWF,I1,JP,FW,SFW,XOFF,IPW,WP)
      WRITE(*,119)NPP,IQM,IWF,XT,WP,ALR,CHIS
        GOTO 501

C *** NORMAL EXITING

550     CONTINUE
        CHIS=0
        DO 600 I=1,N
        CHIS=CHIS+(F(I)-FB(I))**2*WX(I)
        F(I)=F(I)+FA(I)
600     FB(I)=FB(I)+FA(I)
        WRITE(*,108)NPP,CHIS
108     FORMAT(' AT END OF MINIMIZATION',I4,' PEAKS, CHIS',E20.6)
C *** STORE THE BKG CONS
        STATUS='UNKNOWN'
        CALL MAOPEN(2,NABKG,STATUS)
        WRITE(2,*)LFLAG
        WRITE(2,'(G20.12)')(BCONS(I),I=1,NV)
        CLOSE(2)
C*********************************************************
C  OPEN THE GRAPHICS OUTPUT FILE
C*********************************************************
      PRINT*,'HELLO JUST OPENNED',NAGR
        STATUS='UNKNOWN'
        CALL MAOPEN(3,NAGR,STATUS)
        IT1=MAX0(9,INDEX(NADAT,'.'))
      WRITE(3,'(A,G20.12)')NADAT(IT1-8:IT1-1),CHIS
      WRITE(3,'(2I5)')IP
      DO 357 NIP=1,NSTAN
      WRITE(3,'(A)')STAN(NIP)
357   CONTINUE
      WRITE(3,'(A)')'\'
C*********************************************************
C  WRITE THE RAW DATA PARAMETERS FOR FIT
C*********************************************************
      WRITE(3,'(I4/(A64))')IEXTWT,NADAT,NAWT,NABKG,NAPK
      WRITE(3,'(2I5)')N1,N2
C     WRITE(3,'(4G20.12)')(FW(1,I),FW(2,I),I=1,IP)
      DO 506 I=1,IP
      WRITE(3,'(2G20.12)')FW(1,I),FW(2,I)
506   CONTINUE
      CLOSE(3)
C****************************************************
        WRITE(*,110)(XP(I),SXP(I),C(I),SC(I),W(I),SW(I),I=1,NPP)
110     FORMAT(F10.3,F7.3,F10.3,F7.3,F8.3,F6.3)
        STATUS='UNKNOWN'
        CALL MAOPEN(4,NAPK,STATUS)
        WRITE(4,'('' OUTPUT FROM ROBFIT''/'' IP='',I5)') IP
      WRITE(4,113)NPP,N,CHIS,NV,CUTOFF
      WRITE(*,113)NPP,N,CHIS,NV,CUTOFF
113   FORMAT(I5,' PEAKS',I5,' CHAN, CHIS=',G12.4,' NITB=',I5/
     # ' CUTOFF=',F7.2)
C *** THE BACKGROUND ERROR IS TAKEN TO BE THAT IN A ONE CONSTANT FIT
C *** TO ALL POINTS WITHIN THE RANGE OF ITS SPLINES ABOUT THE PEAK CENTER
C *** THUS THIS SOURCE OF ERROR IN C(I)**2 IS SQRT(SUM((FB-F))**2*WX/
C *** (SUM WX)))
        DO 800 I=1,NPP
         ITYPE=IABS(IPT(I))
        IRES=XP(I)-XOFF
        NBEG=IRES+W(I)*XPS(ITYPE)*4
         NBEG=MAX0(1,NBEG)
        NEND=IRES+W(I)*XPL(ITYPE)*4
        IF(NEND.LE.N)GOTO 710
        NEND=N
        NBEG=MAX0(1,N-IRES)
710     ANUM=0
        ADEN=0
        PRINT*,' NBEG,NEND',NBEG,NEND
        DO 720 J=NBEG,NEND
        ANUM=ANUM+(F(J)-FB(J))**2*WX(J)
720     ADEN=ADEN+WX(J)
        EB2=0.D0
        IF(ADEN.GT.0.D0)EB2=ANUM*NV/(ADEN*N)
C *** IN THIS VERSION OF THE CODE SC IS THE ERROR IN C**2*W
C *** AND IS CALCULATED IN QMIN
C *** THOUGH CHANGED HERE, NOTE THAT SC IS NOT USED ANYWHERE
C *** AND WILL BE RECALCULATED IN THE NEXT REFIT
        ARG=SC(I)*SC(I)+EB2*W(I)
        IF(ARG.LT.0.D0)THEN
           PRINT*,' BEFORE 800 POS DEF QUANTITY < 0'
           PRINT*,' I=',I,' SC(I)',SC(I),' EB2 ',EB2,' W(I)',W(I)
           READ(*,*)ITEST
        ENDIF
800     SC(I)=DSQRT(SC(I)*SC(I)+EB2*W(I))
      CALL FWHM(ARAT,WRAT)
      WRITE(4,'(10F8.4)')(ARAT(I),WRAT(I),I=1,IP)
      DO 910 I=1,NPP
        ITEST=IABS(IPT(I))
        STR=C(I)*C(I)*W(I)
        STR=ARAT(ITEST)*STR
        SC(I)=ARAT(ITEST)*SC(I)
        WOUT=WRAT(ITEST)*W(I)
        STTW=WRAT(ITEST)*SW(I)
        STROUT=STR
        IF(IPT(I).LT.0)STROUT=-STR
      WRITE(*,114)XP(I),SXP(I),WOUT,STTW,STROUT,SC(I),IPT(I)
114   FORMAT(F10.3,F8.3,F11.3,F8.3,G14.6,G12.4,I3)
      WRITE(4,114)XP(I),SXP(I),WOUT,STTW,STROUT,SC(I),IPT(I)
910     CONTINUE
        IF(NPF.GT.0)THEN
        WRITE(4,'(A)')'-1000.,0,0,0,0,0,0'
        DO 915 I=1,NPF
915     WRITE(4,*)XPF(I),EPF(I),IPTF(I)
        ENDIF
        CLOSE(4)
C       IF(NOGP.EQ.'F SP')CALL FSPECT(1,N,FA,F,WX,C,XP,W,IPT,PD,
C    # NPP,XOFF)
C       IF(NOGP.EQ.'F PS')CALL PSPECT(1,N,FA,F,WX,C,XP,W,IPT,PD,
C    # NPP,XOFF)
      IF(NOGP.NE.'GP  ')GOTO 920
C **** NOTE THAT PD IS USED BY RCPLOT FOR DERIVATIVE VALUES
920   CONTINUE

C *** RECALCULATING CUTOFF AND EITHER STOPPING OR CONTINUING
        IF(DABS(CUTA-CUTB).LT..1D0)GOTO 950
        IF(CUTOFF-CUTB.LT..1D0)GOTO 950
         CUTOFF=CUTOFF-(CUTA-CUTB)/IMREF
         IF(CUTOFF.GE.CUTB)THEN
         DO 940 I=1,N
         F(I)=F(I)-FA(I)
940      FB(I)=FB(I)-FA(I)
         IFPF=0
         NITREF=0
         GOTO 290
        ENDIF
950     CONTINUE
        IF(IBATCH.EQ.3)GOTO 2
960     STOP
      END

C
      SUBROUTINE LOCATE(X,R,NMAX,J)
      IMPLICIT REAL*8 (A-H,O-Z)
      DIMENSION R(1)
      JL=0
      JU=NMAX+1
10    IF(JU-JL.LE.1)GOTO 20
      JM=(JU+JL)/2
      IF(X.GT.R(JM))THEN
      JL=JM
      ELSE
      JU=JM
      ENDIF
      GOTO 10
20    J=JL
      RETURN
      END
          FUNCTION POLYA(X,C,XP,W,IPT,NS)
       IMPLICIT REAL*8(A-H,O-Z)
C THIS ROUTINE CALCULATES POLYA= SUM C(I)*STAN(I,X) USING POLYG FOR STAN
C NOTE THAT BY NOT HAVING XPS OR XPL LARGE, WE ASSUME THAT POLYB HAS BEE
C CALLED BEFORE THE FIRST CALL TO POLYA
      COMMON/SCONS/DU(150),XPS(5),XPL(5),IP,IDUM,XBEG,XEND
      DIMENSION C(1),XP(1),W(1),IPT(1)
      POLYA=0
      IF(NS.EQ.0)RETURN
150   CALL LOCATE(X+XBEG-XEND,XP,NS,JB)
        JB=JB+1
        CALL LOCATE(X+XEND-XBEG,XP,NS,JE)
      DO 200 J=JB,JE
      XM=X-XP(J)
        ITEST=IABS(IPT(J))
      IF(XM.LE.W(J)*XPS(ITEST).OR.XM.GE.W(J)*XPL(ITEST))GOTO 200
        ISIGN=1
        IF(IPT(J).LE.0)ISIGN=-1
      XM=XM/W(J)
C  PARTIAL WRT C(J)
       FADD=ISIGN*C(J)*POLYG(XM,ITEST,SP)
      POLYA=POLYA+C(J)*FADD
200   CONTINUE
      RETURN
      END
       FUNCTION POLYB(X,P,C,XP,W,IPT,NS)
       IMPLICIT REAL*8(A-H,O-Z)
C THIS ROUTINE CALCULATES POLYB=SUM CI*SI ALONG WITH DPOLYB/DCI
C THE NEXT NS SI'S, WHICH HAVE DERIVATIVES WITH RESPECT TO C,XP,AND W
C ARE BSPLINES
      COMMON/SCONS/DU(150),XPS(5),XPL(5)
       DIMENSION P(1),C(1),XP(1),W(1),IPT(1)
      NC=1
5     NPART=1
      POLYB=0
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
       FADD=ISIGN*C(J)*POLYG(XM,ITEST,SP)
      P(NPART)=2.*FADD
      POLYB=POLYB+C(J)*FADD
C PARTIAL WRT XP(J)
      P(NPART+1)=-ISIGN*C(J)*C(J)*SP/W(J)
C PARTIAL WRT W(J)
      P(NPART+2)=P(NPART+1)*XM
200   NPART=NPART+3
      RETURN
      END
      FUNCTION POLYG(X,IPT,SP)
       IMPLICIT REAL*8(A-H,O-Z)
C****************************************************
C
C  DATA FROM MAIN ROUTINE
C
C****************************************************
      DIMENSION STAN(5)
      CHARACTER*64 STAN,STATUS
      COMMON/CHPOLY/STAN
      COMMON/CPOLYG/NSTAN
C*****************************************************
      COMMON/SCONS/W(10,5),XP(10,5),C(10,5),XPS(5),XPL(5),IP,NC
     2 ,XBEG,XEND
C *** SET UP GRAPHICS ARRAY
      COMMON/G3PLG/CG3(20,5),XPG3(20,5),WG3(20,5),NST(5)
C THIS ROUTINE CALCULATES POLYG=SUM CI*SI ALONG WITH DPOLY/DX
C IPT GIVES THE PEAK TYPE RANGING FROM 1 TO 5
      IF(NC.EQ.1)GOTO 30
      NC=1
        XBEG=1.D32
        XEND=-1.D32
        IP=0
3       CONTINUE
        IF(IP+1.GT.NSTAN)GOTO 30
        STATUS='OLD'
        CALL MAOPEN(11,STAN(IP+1),STATUS)
      WRITE(*,102)STAN(IP+1)
102   FORMAT(' THESE CONSTANTS,PEAKS,AND WIDTHS ARE',
     X ' STANDARD PEAK ',A)
        IP=IP+1
        NS=1
        IF(IP.GT.5)GOTO 300
5     READ(11,101,END=20)C(NS,IP),XP(NS,IP),W(NS,IP)
      CG3(NS,IP)=C(NS,IP)
      XPG3(NS,IP)=XP(NS,IP)
      WG3(NS,IP)=W(NS,IP)
101   FORMAT(5X,3E20.7)
      WRITE(*,103)NS,C(NS,IP),XP(NS,IP),W(NS,IP)
103   FORMAT(I5,3E20.7)
      NS=NS+1
      GOTO 5
20    NS=NS-1
      XPS(IP)=1.D32
      XPL(IP)=-1.D32
      DO 25 J=1,NS
      XPS(IP)=DMIN1(XPS(IP),XP(J,IP)-W(J,IP))
25    XPL(IP)=DMAX1(XPL(IP),XP(J,IP)+W(J,IP))
        XBEG=DMIN1(XBEG,XPS(IP))
        XEND=DMAX1(XEND,XPL(IP))
        NST(IP)=NS
        CLOSE(11)
        GOTO 3
30    CONTINUE
       POLYG=0.
      SP=0.
        NS=NST(IPT)
C***** LIMIT THE VECTORIZATION
C*VDIR: ASSUME COUNT(5)
      DO 200 J=1,NS
      XM=X-XP(J,IPT)
      IF(XM.LE.-W(J,IPT).OR.XM.GE.W(J,IPT))GOTO 200
      XM=XM/W(J,IPT)
      FADD=C(J,IPT)*((1.+XM)*(1.-XM))**3
      POLYG=POLYG+FADD
       SP=SP+3.*(FADD/(1.+XM)-FADD/(1.-XM))/W(J,IPT)
200   CONTINUE
      RETURN
300     PRINT*,' ATTEMPT TO DEFINE MORE THAN FIVE STANDARDS'
        STOP
      END
      SUBROUTINE QMIN(FB,F,WX,IB,IE,NSC,C,XP,W,IPT,WF,FW,SFW,
     #  XOFF,IPP,EC,EX,EW)
C FB ARE THE BACKGROUND VALUES, F THE MEASURED VALUES, FW THE
C FIT TO THE WIDTHS, C THE CONSTANTS FOR THE PEAKS, XP THE
C PEAK LOCATIONS, W THE PEAK WIDTHS.  THESE LAST THREE ARE
C RETURNED BY QMIN, BUT INITIAL ESTIMATES MUST BE SUPPLIED.
      IMPLICIT REAL*8(A-H,O-Z)
      PARAMETER (MAXFPK=50)
      REAL*4 FB,F,WX,FA,AMAX1,AMIN1,XOFF
      DIMENSION FB(1),F(1),WX(1),C(1),XP(1),W(1),WF(1),FW(3,5)
      DIMENSION IPT(1),P(30),CONS(30),SM(30),PC(30),
     #  EC(1),EW(1),EX(1),SFW(5,5)
      COMMON/FPEAKS/NPF,IPF,XPF(MAXFPK),EPF(MAXFPK),IPTF(MAXFPK)
      COMMON/SMSQS/PPCC(480),AM(480)
9     CHB=1.D71
      CHL=1.D72
      FR=0
      IEN=0
      NT=3*NSC
      NPART=1
       DO 24 J=1,NSC
      CONS(NPART)=C(J)
      SM(NPART)=1.D-2
      CONS(NPART+1)=XP(J)
      SM(NPART+1)=SM(NPART)*1.D6
      L=IABS(IPT(J))
      WPP=DSQRT(DMAX1(3D0,FW(1,L)+XP(J)*(FW(2,L)+
     # XP(J)*FW(3,L))))
        SW=DSQRT(DABS(SFW(1,L)+XP(J)*(SFW(2,L)+XP(J)*(SFW(3,L)
     # +XP(J)*(SFW(4,L)+XP(J)*SFW(5,L))))))
        SW=SW/(2*WPP)
      W(J)=DMAX1(1.5D0,WPP-2*SW,DMIN1(W(J),WPP+2*SW))
      CONS(NPART+2)=W(J)
      SM(NPART+2)=SM(NPART)*1.D4
24    NPART=NPART+3
        NDT=NT*(NT+1)/2
      NKIT=150
      KIT=1
28      CHI=0
         PEN=0
      DO 30 J=1,NT
30    PC(J)=0.
      DO 35 J=1,NDT
35    PPCC(J)=0.
      DO 200 I=IB,IE
      X=I+XOFF
      FA=POLYB(X,P,C,XP,W,IPT,NSC)+FB(I)
        RES=AMIN1(1.E12,AMAX1(-1.E12,F(I)-FA))
      CHI=CHI+RES**2*WX(I)
      IF(CHI.LT.0.D0)THEN
         PRINT*,' IN QMIN POS DEF QUANTITY < 0'
         PRINT*,' I=',I,' WX(I) ',WX(I),' RES ',RES
         READ(*,*)ITEST
      ENDIF
      W1=2*(FA-F(I))*WX(I)
      W2=2*WX(I)
        KJ=0
      DO 80 J=1,NT
      PC(J)=PC(J)+W1*P(J)
      W2P=W2*P(J)
      DO 80 K=1,J
        KJ=KJ+1
80    PPCC(KJ)=PPCC(KJ)+P(K)*W2P
200   CONTINUE

       CALL WADJ(FW,SFW,PC,PPCC,
     #  PEN,W,XP,IPT,NSC)
       IF(NPF.GT.0.AND.IPF.GT.0)
     # CALL LADJ(PC,PPCC,PEN,XP,IPT,NSC)
      CHI=CHI+PEN

        IEND=0
        IF(KIT.GT.NKIT.AND.CHI.LT.CHL)IEND=1
        IF(CHL-CHB.LT..01D0)IEND=1
        IF(CHL-CHB.LT.1D-6*CHB)IEND=1
        IF(IEND.NE.1)THEN
           CALL SMSQ(CHI,CHB,CHL,PC,PPCC,AM,FR,CONS,SM,NT,IPP)
           NPART=1
           DO 260 J=1,NSC
           CONS(NPART)=DABS(CONS(NPART))
           C(J)=DMAX1(.1D0,CONS(NPART))
           XP(J)=CONS(NPART+1)
           W(J)=CONS(NPART+2)
260        NPART=NPART+3
           KIT=KIT+1
           GOTO 28
        ENDIF

        CHI=CHI-PEN
        DO 2503 I=1,NDT
2503    AM(I)=PPCC(I)
        CALL SMINV(AM,NT,IFL)
        DO 2510 I=1,NDT
2510    AM(I)=DMIN1(1.D9,DMAX1(-1.D9,AM(I)))
        CHIP=DMAX1(1.D0,CHI/MAX0(1,IE-IB+1-NT))
        DO 2520 J=1,NSC
        JC=1+3*(J-1)
        JX=JC+1
        JW=JX+1
        JCC=JC*(JC+1)/2
        JWW=JW*(JW+1)/2
        ICT=KIJ(JC,JC+2)
        ESTR2=(2*C(J)*W(J))**2*AM(JCC)
        ESTR2=ESTR2+4*C(J)**3*W(J)*AM(ICT)
        ESTR2=ESTR2+C(J)**4*AM(JWW)
        EC(J)=DSQRT(DMAX1(1.D-30,ESTR2*CHIP*2))
        EX(J)=DSQRT(DMAX1(1.D-30,2*AM(JX*(JX+1)/2)*CHIP))
        EX(J)=DMIN1(DMAX1(10.D0,2*W(J)),EX(J))
        EW(J)=DSQRT(DMAX1(1.D-30,2*AM(JW*(JW+1)/2)*CHIP))
        EW(J)=DMIN1(DMAX1(10.D0,2*W(J)),EW(J))
        WF(J)=W(J)
2520    CONTINUE
         IF(IPP.EQ.0)RETURN
        NP=IE-IB+1
      WRITE(*,104)NP
104   FORMAT(' WE ARE FITTING',I5,' POINTS')
      WRITE(*,102)CHI,PEN
102   FORMAT(' CHISQUARE IS',E20.7,' PEN IS',E20.7)
C     IF(NSC.GT.0)WRITE(*,103)(I,C(I),EC(I),XP(I),EX(I),W(I),EW(
C    # I),I=1,NSC)
103   FORMAT(' THE B SPLINE COEFFS'/'    #',8X,'C(I)',17X,'XP(I)',
     # 18X,'W(I)'/(I5,3(G12.4,G14.6)))
       RETURN
       END
        SUBROUTINE FQMIN(FB,F,WX,IB,IE,NSC,C,XP,W,IPT,WF
     # ,XOFF,IPP,EC,EX,EW)
C FB ARE THE BACKGROUND VALUES, F THE MEASURED VALUES, FW THE
C FIT TO THE WIDTHS, C THE CONSTANTS FOR THE PEAKS, XP THE
C PEAK LOCATIONS, W THE PEAK WIDTHS.  THESE LAST THREE ARE
C RETURNED BY QMIN, BUT INITIAL ESTIMATES MUST BE SUPPLIED.
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 FB,F,WX,FA,AMAX1,AMIN1,XOFF
      DIMENSION FB(1),F(1),WX(1),C(1),XP(1),W(1),WF(1)
      DIMENSION IPT(1),P(30),CONS(30),SM(30),PC(30),
     #         EC(1),EW(1),EX(1)
        COMMON/SMSQS/PPCC(480),AM(480)
9     CHB=1.D71
      CHL=1.D72
      FR=0
      IEN=0
      NT=NSC
      NPART=1
       DO 24 J=1,NSC
      CONS(NPART)=C(J)
      SM(NPART)=1.D-2
24    NPART=NPART+1
        NDT=NT*(NT+1)/2
      NKIT=150
      KIT=1
28      CHI=0
         PEN=0
      DO 30 J=1,NT
30    PC(J)=0.
      DO 35 J=1,NDT
35    PPCC(J)=0.
      DO 200 I=IB,IE
      X=I+XOFF
      FA=POLYB(X,P,C,XP,W,IPT,NSC)+FB(I)
        RES=AMIN1(1.E12,AMAX1(-1.E12,F(I)-FA))
      CHI=CHI+RES**2*WX(I)
      W1=2*(FA-F(I))*WX(I)
      W2=2*WX(I)
        KJ=0
      DO 80 J=1,NT
      JC=3*(J-1)+1
      PC(J)=PC(J)+W1*P(JC)
      W2P=W2*P(JC)
      DO 80 K=1,J
        KC=3*(K-1)+1
        KJ=KJ+1
80    PPCC(KJ)=PPCC(KJ)+P(KC)*W2P
200   CONTINUE
      IF(CHI.LE.1.01*CHL.AND.KIT.GE.NKIT)GOTO 2502
      IF(CHI.LE.1.01*CHL.AND.IEN.EQ.1)GOTO 2502
      IF(CHL-CHB.GT..01D0)GOTO 248
      IF(KIT.LE.3)GOTO 248
        IEN=1
      IF(CHI.LT.CHL)GOTO 2502
248   CALL SMSQ(CHI,CHB,CHL,PC,PPCC,AM,FR,CONS,SM,NT,IPP)
      DO 260 J=1,NSC
      CONS(J)=DABS(CONS(J))
260   C(J)=DMAX1(.1D0,CONS(J))
      KIT=KIT+1
      IF(KIT.GT.2*NKIT)GOTO 280
      GOTO 28
2502  CONTINUE
        CHI=CHI-PEN
        DO 2503 I=1,NDT
2503    AM(I)=PPCC(I)
        CALL SMINV(AM,NT,IFL)
        DO 2510 I=1,NDT
2510    AM(I)=DMIN1(1.D9,DMAX1(-1.D9,AM(I)))
        CHIP=DMAX1(1.D0,CHI/MAX0(1,IE-IB+1-NT))
        DO 2520 J=1,NSC
        EC(J)=2*(2*C(J)*W(J))**2*AM(J*(J+1)/2)*CHIP
        EC(J)=DSQRT(DMAX1(1.D-30,EC(J)))
        EW(J)=0
        EX(J)=0
        WF(J)=W(J)
2520    CONTINUE
         IF(IPP.EQ.0)RETURN
        NP=IE-IB+1
      WRITE(*,104)NP
104   FORMAT(' WE ARE FITTING',I5,' POINTS')
      WRITE(*,102)CHI,PEN
102   FORMAT(' CHISQUARE IS',E20.7,' PEN IS',E20.7)
C     IF(NSC.GT.0)WRITE(*,103)(I,C(I),EC(I),XP(I),EX(I),W(I),EW(
C    # I),I=1,NSC)
103   FORMAT(' THE B SPLINE COEFFS'/'    #',8X,'C(I)',17X,'XP(I)',
     # 18X,'W(I)'/(I5,3(F12.4,F10.6)))
       RETURN
C280   WRITE(*,105)CHI,CHB,CHL,(CONS(I),I=1,NT)
C105   FORMAT(' LOOPING IN QMIN, CHI,CHB,CHL/CONS'/(3E20.12))
280   PRINT*,'Looping in QMIN, CHI,CHB,CHL',CHI,CHB,CHL
      PRINT*,'Constants are'
      DO 105 J=1,NT
105   PRINT*,CONS(I)
      NPART=1
      DO 290 J=1,NSC
290   C(J)=0
      RETURN
      END
        SUBROUTINE FNMIN(FB,F,WX,IB,IE,NSC,C,XP,W,IPT,WF
     # ,XOFF,IPP,EC,EX,EW)
C FB ARE THE BACKGROUND VALUES, F THE MEASURED VALUES, FW THE
C FIT TO THE WIDTHS, C THE CONSTANTS FOR THE PEAKS, XP THE
C PEAK LOCATIONS, W THE PEAK WIDTHS.  THESE LAST THREE ARE
C RETURNED BY QMIN, BUT INITIAL ESTIMATES MUST BE SUPPLIED.
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 FB,F,WX,FA,AMAX1,AMIN1,XOFF
      DIMENSION FB(1),F(1),WX(1),C(1),XP(1),W(1),WF(1)
      DIMENSION IPT(1),P(30),CONS(30),SM(30),PC(30),
     #         EC(1),EW(1),EX(1)
        COMMON/SMSQS/PPCC(480),AM(480)
9     CHB=1.D71
      CHL=1.D72
      FR=0
      IEN=0
      NT=NSC
      NPART=1
       DO 24 J=1,NSC
      CONS(NPART)=C(J)
      SM(NPART)=1.D-2
24    NPART=NPART+1
        NDT=NT*(NT+1)/2
      NKIT=150
      KIT=1
        KSS=0
28      CHI=0
         PEN=0
      DO 30 J=1,NT
30    PC(J)=0.
      DO 35 J=1,NDT
35    PPCC(J)=0.
      DO 200 I=IB,IE
      X=I+XOFF
      FA=POLYB(X,P,C,XP,W,IPT,NSC)+FB(I)
        RES=AMIN1(1.E12,AMAX1(-1.E12,F(I)-FA))
      CHI=CHI+RES**2*WX(I)
      W1=2*(FA-F(I))*WX(I)
      W2=2*WX(I)
        KJ=0
      DO 80 J=1,NT
      JC=3*(J-1)+1
      PC(J)=PC(J)+W1*P(JC)
      W2P=W2*P(JC)
      DO 80 K=1,J
        KC=3*(K-1)+1
        KJ=KJ+1
80    PPCC(KJ)=PPCC(KJ)+P(KC)*W2P
200   CONTINUE
      IF(CHI.LE.1.01*CHL.AND.KIT.GE.NKIT)GOTO 2502
      IF(CHI.LE.1.01*CHL.AND.IEN.EQ.1)GOTO 2502
      IF(CHL-CHB.GT..01D0)GOTO 248
      IF(KIT.LE.3)GOTO 248
        IEN=1
      IF(CHI.LT.CHL)GOTO 2502
248   CALL SMSQ(CHI,CHB,CHL,PC,PPCC,AM,FR,CONS,SM,NT,IPP)
        PRINT'(A/(5G16.6))',' CONS AFTER SMSQ',(CONS(I),I=1,NT)
      DO 260 J=1,NSC
      CONS(J)=DABS(CONS(J))
       C(J)=DMAX1(.1D0,CONS(J))
260   CONTINUE
      KIT=KIT+1
      IF(KIT.GT.2*NKIT)GOTO 280
      GOTO 28
2502  CONTINUE
        IF(CHI.LT.CHL.AND.KSS.LT.10)THEN
        PRINT*,' CHANGING SIGNS KSS=',KSS
           KSS=KSS+1
           KIT=1
           ICS=0
           IEN=0
           DO 2210 J=1,NSC
           IF(CONS(J).GT..2D0)GOTO 2210
           IPT(J)=-IPT(J)
           PRINT*,' CHANGED SIGN OF CONS(J)=',CONS(J),J,IPT(J)
           ICS=1
2210       CONTINUE
           CHB=1.D71
           CHL=1.D72
           IF(ICS.EQ.1)GOTO 28
        ENDIF
        CHI=CHI-PEN
        DO 2503 I=1,NDT
2503    AM(I)=PPCC(I)
        CALL SMINV(AM,NT,IFL)
        DO 2510 I=1,NDT
2510    AM(I)=DMIN1(1.D9,DMAX1(-1.D9,AM(I)))
        CHIP=DMAX1(1.D0,CHI/MAX0(1,IE-IB+1-NT))
        DO 2520 J=1,NSC
        EC(J)=2*(2*C(J)*W(J))**2*AM(J*(J+1)/2)*CHIP
        EC(J)=DSQRT(DMAX1(1.D-30,EC(J)))
        EW(J)=0
        EX(J)=0
        WF(J)=W(J)
2520    CONTINUE
         IF(IPP.EQ.0)RETURN
        NP=IE-IB+1
      WRITE(*,104)NP
104   FORMAT(' WE ARE FITTING',I5,' POINTS')
      WRITE(*,102)CHI,PEN
102   FORMAT(' CHISQUARE IS',E20.7,' PEN IS',E20.7)
C     IF(NSC.GT.0)WRITE(*,103)(I,C(I),EC(I),XP(I),EX(I),W(I),EW(
C    # I),I=1,NSC)
103   FORMAT(' THE B SPLINE COEFFS'/'    #',8X,'C(I)',17X,'XP(I)',
     # 18X,'W(I)'/(I5,3(F12.4,F10.6)))
       RETURN
C280   WRITE(*,105)CHI,CHB,CHL,(CONS(I),I=1,NT)
C105   FORMAT(' LOOPING IN QMIN, CHI,CHB,CHL/CONS'/(3E20.12))
280   PRINT*,'Looping in QMIN, CHI,CHB,CHL',CHI,CHB,CHL
      PRINT*,'Constants are'
      DO 105 J=1,NT
105   PRINT*,CONS(I)
      NPART=1
      DO 290 J=1,NSC
290   C(J)=0
      RETURN
      END
C$LIST
        FUNCTION CQMIN(FB,F,WX,XOFF,N,XPG,WG,L,FW,SFW,PSBKGE)
C FB ARE THE BACKGROUND VALUES, F THE MEASURED VALUES, FW THE
C FIT TO THE WIDTHS,  XP THE PEAK LOCATIONS, W THE PEAK WIDTHS.
C THESE LAST THREE ARE RETURNED, BUT INITIAL ESTIMATES MUST BE SUPPLIED.
      IMPLICIT REAL*8(A-H,O-Z)
      REAL*4 FB,F,WX,ABS,XOFF,FA,SQRT
      DIMENSION FB(1),F(1),WX(1),FW(3,5),IPT(2),SFW(5,5)
      DIMENSION P(3),CONS(3),SM(3),PC(3),PPCC(6),AM(6),C(2),W(2),XP(2)
9     CHB=1.E31
      CHL=1.E32
      IPT(1)=L
      LA=IABS(L)
      FR=0
      IEN=0
      NT=3
      IX=XPG-XOFF
      C(1)=SQRT(ABS(F(IX)-FB(IX)))
      CONS(1)=C(1)
      SM(1)=1.D-2
      XP(1)=XPG
      CONS(2)=XPG
      SM(2)=SM(1)*1.D6
      WPP=DSQRT(DMAX1(2D0,FW(1,LA)+XPG*(FW(2,LA)+
     # XPG*FW(3,LA))))
      W(1)=WPP
        SW=DSQRT(DABS(SFW(1,LA)+XPG*(SFW(2,LA)+XPG*(SFW(3,LA)
     # +XPG*(SFW(4,LA)+XPG*SFW(5,LA))))))
        SW=SW/(2*WPP)
      CONS(3)=W(1)
      SM(3)=SM(2)*1.D4
        NDT=NT*(NT+1)/2
        IB=XPG-2*W(1)-2*SW-XOFF
        IB=MAX0(1,IB)
        IE=XPG+2*W(1)+2*SW-XOFF
        IE=MIN0(N,IE)
      NKIT=30
      KIT=1
28      CHI=0
         PEN=0
      DO 30 J=1,NT
30    PC(J)=0.
      DO 35 J=1,NDT
35    PPCC(J)=0.
      DO 200 I=IB,IE
      X=I+XOFF
      FA=POLYB(X,P,C,XP,W,IPT,1)+FB(I)
        RES=AMIN1(1.E12,AMAX1(-1.E12,F(I)-FA))
      CHI=CHI+RES**2*WX(I)
      W1=2*(FA-F(I))*WX(I)
      W2=2*WX(I)
        KJ=0
      DO 80 J=1,NT
      PC(J)=PC(J)+W1*P(J)
      W2P=W2*P(J)
      DO 80 K=1,J
        KJ=KJ+1
80    PPCC(KJ)=PPCC(KJ)+P(K)*W2P
200   CONTINUE

       CALL WADJ(FW,SFW,PC,PPCC,
     #  PEN,W,XP,IPT,1)

      IF(KIT.EQ.1)CHIO=CHI

         CHI=CHI+PEN
      IF(CHI.LE.1.01*CHL.AND.KIT.GE.NKIT)GOTO 2502
      IF(CHI.LE.1.01*CHL.AND.IEN.EQ.1)GOTO 2502
      IF(CHL-CHB.GT..1D0)GOTO 248
      IF(KIT.LE.3)GOTO 248
        IEN=1
      IF(CHI.LT.CHL)GOTO 2502
248   CALL SMSQ(CHI,CHB,CHL,PC,PPCC,AM,FR,CONS,SM,NT,0)
250   NPART=1
      C(1)=DMAX1(.1D0,CONS(NPART))
      XP(1)=DMAX1(1.D0*(IB+XOFF),DMIN1(1.D0*(IE+XOFF),CONS(2)))
C     W(1)=DMAX1(1.5D0,WPP-2*SW,DMIN1(CONS(3),WPP+2*SW))
        W(1)=CONS(3)
      KIT=KIT+1
      IF(KIT.GT.2*NKIT)GOTO 280
      GOTO 28
2502  CONTINUE
        CHI=CHI-PEN
C *** THE ERROR IN THE BACKGROUND IS (IE-IB+1)/SQRT(SUM(WX(I)))
        EBKGS=0
        DO 3000 I=IB,IE
        EBKGS=EBKGS+WX(I)
3000    CONTINUE
        EBKG=(IE-IB+1)/DSQRT(EBKGS)
        PSBKGE=0
        IF(W(1).GT.0.D0)PSBKGE=C(1)*C(1)*W(1)/EBKG
        IF(W(1).GT.WPP+2*SW.OR.W(1).LT.WPP-2*SW)PSBKGE=.1*PSBKGE
        CQMIN=C(1)
        WG=W(1)
        XPG=XP(1)
       RETURN
C280   WRITE(*,105)CHI,CHB,CHL,(CONS(I),I=1,NT)
C105   FORMAT(' LOOPING IN CQMIN, CHI,CHB,CHL/CONS'/(3E20.12))
280   PRINT*,'Looping in CQMIN, CHI,CHB,CHL',CHI,CHB,CHL
      PRINT*,'Constants are'
      DO 105 J=1,NT
105   PRINT*,CONS(I)
      NPART=1
      CQMIN=0
      ERRSQ=1.D32
      RETURN
       END
        SUBROUTINE WADJ(FW,SFW,PC,PPCC,
     #  PEN,W,XP,IPT,NSC)
        IMPLICIT REAL*8 (A-H,O-Z)
        DIMENSION PC(1),PPCC(1),FW(3,5),SFW(5,5),AI(1),
     # XP(1),IPT(1),W(1)
        NPART=3
         PEN=0.D0
         DO 210 J=1,NSC
        L=IABS(IPT(J))
        NPM22=(NPART-2)*(NPART-1)/2
        T11=DMAX1(1.D-12,DMIN1(1.D12,PPCC(NPM22)))
        NPM2NP=NPART-2+NPART*(NPART-1)/2
        T12=DMAX1(-1.D12,DMIN1(1.D12,PPCC(NPM2NP)))
        NPNP=NPART*(NPART+1)/2
        T22=DMAX1(1.D-12,DMIN1(1.D12,PPCC(NPNP)))
C       IF(XP(J).GT.XVB.AND.XP(J).LT.XVE)GOTO 210
      WPP=DSQRT(DMAX1(3D0,FW(1,L)+XP(J)*(FW(2,L)+
     # XP(J)*FW(3,L))))
        SW=DSQRT(DABS(SFW(1,L)+XP(J)*(SFW(2,L)+XP(J)*(SFW(3,L)
     # +XP(J)*(SFW(4,L)+XP(J)*SFW(5,L))))))
        SW=SW/(2*WPP)
        SW=DMAX1(.1D-2,SW)
        SW2=SW*SW
        DELTAW=(W(J)-WPP)
208     PT=DMAX1(0.D0,(2*T11/SW2+T12*T12-T11*T22)/(2*T11))
        PC(NPART)=2*PT*DELTAW+PC(NPART)
        PPCC(NPNP)=2*PT+PPCC(NPNP)
      PEN=PEN+PT*DELTAW*DELTAW
210   NPART=NPART+3
        RETURN
        END
        SUBROUTINE LADJ(PC,PPCC,PEN,XP,IPT,NSC)
        IMPLICIT REAL*8 (A-H,O-Z)
        PARAMETER (MAXFPK=50)
        COMMON/FPEAKS/NPF,IPF,XPF(MAXFPK),EPF(MAXFPK),IPTF(MAXFPK)
        DIMENSION PC(1),PPCC(1),XP(1),IPT(1)
        DO 500 I=1,NPF
C *** FIND PEAK CLOSEST TO XPF
        JF=1
        XDIST=1.E30
        DO 200 J=1,NSC
        XC=DABS(XPF(I)-XP(J))
        IF(IPT(J).NE.IPTF(I))GOTO 200
        IF(XC.GT.XDIST)GOTO 200
        JF=J
        XDIST=XC
200     CONTINUE
        IF(XDIST.GT.2*EPF(I))GOTO 500
        NPART=3*JF
        NPM22=(NPART-2)*(NPART-1)/2
        T11=DMAX1(1.D-12,DMIN1(1.D12,PPCC(NPM22)))
        NPM2M1=NPART*(NPART-1)/2-1
        T12=DMAX1(-1.D12,DMIN1(1.D12,PPCC(NPM2M1)))
        NPNP=NPART*(NPART-1)/2
        T22=DMAX1(1.D-12,DMIN1(1.D12,PPCC(NPNP)))
        SW2=EPF(I)*EPF(I)
        DELTAW=XP(JF)-XPF(I)
208     PT=DMAX1(0.D0,(2*T11/SW2+T12*T12-T11*T22)/(2*T11))
        PC(NPART-1)=2*PT*DELTAW+PC(NPART-1)
        PPCC(NPNP)=2*PT+PPCC(NPNP)
        PEN=PEN+PT*DELTAW*DELTAW
500     CONTINUE
        RETURN
        END
         FUNCTION CFEST(W,ITT,F,FB,WX,XT,N,ERRSQ,IFW)
         IMPLICIT REAL*8 (A-H,O-Z)
         CHARACTER*4 IFW
         REAL*4 F(1),FB(1),WX(1)
         NIT=0
         IBG=XT-2*W
         IBG=MAX0(1,IBG)
         IEND=XT+W
         IEND=MIN0(N,IEND)
         ANUM=0
         ADEN=0
         ERRT=0
         ERRSQ=0
         DO 10 I=IBG,IEND
         X=(I-XT)/W
         DF=(F(I)-FB(I))
         ERRSQ=ERRSQ+DF*DF*WX(I)
         FA=DMAX1(1.D-4,POLYG(X,IABS(ITT),SP) )
         ANUM=ANUM+DF*FA*WX(I)
         ADEN=ADEN+FA*FA*WX(I)
10       CONTINUE
         CFEST=0
         IF(ADEN.LE.0.)RETURN
         CFEST=ANUM/ADEN
         ERRSQ=ERRSQ-CFEST*ANUM
         ERRSQ=ERRSQ/(IEND+1-IBG)
         IF(CFEST.GT.0)THEN
           CFEST=DSQRT(CFEST)
           ITT=IABS(ITT)
           RETURN
         ELSE
           IF(IFW.EQ.'FIXN')THEN
             CFEST=DSQRT(-CFEST)
             ITT=-1
             RETURN
           ELSE
             CFEST=0
             RETURN
           ENDIF
         ENDIF
         END
        SUBROUTINE FWHM(AI,WRAT)
      IMPLICIT REAL*8 (A-H,O-Z)
         DIMENSION AI(1),WRAT(1)
      COMMON/SCONS/DU(150),XPS(5),XPL(5),NTYPE
C     TRAP RULE INTEGRATION AND HALF WIDTH FINDING
         DO 80 ITY=1,NTYPE
      XLH=-1.D0
        CALL XHMAX(XLH,ITY)
        XUH=1.D0
        CALL XHMAX(XUH,ITY)
        WRAT(ITY) =XUH-XLH
         H=WRAT(ITY)/50
      X=XPS(ITY)-H/2
      AI(ITY)=0.D0
25      X=X+H
      F=POLYG(X,ITY,SP)
      AI(ITY)=AI(ITY)+F
        IF(X.LT.XPL(ITY))GOTO 25
      AI(ITY)=AI(ITY)*H
      WRITE(*,101)XLH,XUH,AI(ITY)
101   FORMAT(' FWHM --- XLH=',E16.7,' XUH=',E16.7,' AI=',E16.7)
80    CONTINUE
      RETURN
      END
        SUBROUTINE XHMAX(XT,ITYPE)
        IMPLICIT REAL*8(A-H,O-Z)
C IF XT INITIALLY < 0 THE XT RETURNED WILL BE THE LOWER VALUE
C IF XT INITIALLY > O THE XT RETURNED WILL BE THE UPPER VALUE
        NLOOP=0
        FA=.5
        XA=0
        XB=XT
5       FB=POLYG(XB,ITYPE,SP)-.5
        IF(FB.LT.0.)GOTO 10
        XB=2*XB
        GOTO 5
10      XT=XA-FA*(XA-XB)/(FA-FB)
        FT=POLYG(XT,ITYPE,SF)-.5
        NLOOP=NLOOP+1
        IF(NLOOP.GT.100)GOTO 50
        IF(DABS(FT).LT.1.D-6.OR.DABS(XA-XB).LT.1.D-6)RETURN
        IF(FT.LT.0)GOTO 20
        FA=FT
        XA=XT
        GOTO 10
20      FB=FT
        XB=XT
        GOTO 10
50      WRITE(*,100)NLOOP,XA,XB,XT,FA,FB,FT
100     FORMAT(' XHMAX IN A LOOP, NLOOP,XA,XB,XT,FA,FB,FT'/I5,6E
     # 20.6)
        RETURN
        END
      SUBROUTINE RESL(F,FB,WX,FW,ALR,IQM,ILR,IFTC,IC,N,XOFF,CUTOFF)
      DIMENSION ILRO(50),F(1),FB(1),WX(1),IWT(5)
         COMMON/SCONS/DU(150),XPS(5),XPL(5),IP,NGPCAL
      REAL*8 FW(3,5),XPS,XPL,DU
         SAVE ILRO,N10
        DATA N10/50/
      IFTC=0
      IF(IQM.LE.N10)GOTO 20
15    IFTC=1
      RETURN
20    ALR=0
        XT=XOFF+N/2
      DO 90 J=1,IP
        WPG=DSQRT(DMAX1(2D0,FW(1,J)+XT*(FW(2,J)+XT*FW(3,J))))
C *** PREVIOUSLY USED WPG*4
        IW=WPG
        IW=MAX0(2,MIN0(100,IW))
        IWT(J)=IW
        JM=J-1
        DO 30 K=1,JM
30      IF(IW.EQ.IWT(K))GOTO 90
        CALL SCAN(F,FB,WX,N,ILRTE,ALRT,IW,ILRO,IQM,IC)
      IF(ABS(ALR).GT.ABS(ALRT))GOTO 40
      ILR=ILRTE
      ILRT=J
      ALR=ALRT
40    CONTINUE
90    CONTINUE
      IF(ABS(ALR).LT.CUTOFF.OR.IQM.GT.50)GOTO 15
        IQM=IQM+1
        IF(IQM.LE.50.AND.ALR.GT.0.D0)ILRO(IQM)=ILR
        IF(IQM.LE.50.AND.ALR.LT.0.D0)ILRO(IQM)=-ILR
      RETURN
      END
        SUBROUTINE SCAN(F,FB,WX,N,ILR,ALR,IW,ILRO,IQM,IC)
        DIMENSION FSM(100),ILRO(50),F(1),FB(1),WX(1)
        IWM=IW-1
        IWS2=IW/2
        DO 10 I=1,IW
10      FSM(I)=0
        SUM=0
        ALR=0
        DO 100 I=1,N
        FSM(IW)=(F(I)-FB(I))*SQRT(ABS(WX(I)))
        SUM=SUM+FSM(IW)-FSM(1)
        DO 20 J=1,IWM
20      FSM(J)=FSM(J+1)
      IF(ABS(ALR).GT.ABS(SUM))GOTO 40
      ILRT=I-IW+1
      ILRA=ILRT+1
      FMAX=-1.E32
      DO 25 J=2,IWM
      IF(FSM(J).LT.FMAX)GOTO 25
      FMAX=FSM(J)
      ILRA=ILRT+J
25    CONTINUE
      ILRT=ILRA
      DO 30 J=1,IQM
      IF(SUM*ILRO(J).GT.0.D0)THEN
        IF(ILRT.EQ.IABS(ILRO(J)))GOTO 40
        IF(IABS(ILRT-IABS(ILRO(J))).LE.IC)GOTO 40
      ENDIF
30    CONTINUE
35    CONTINUE
      ALR=SUM
      ILR=ILRT
40    CONTINUE
100   CONTINUE
      ALR=ALR/SQRT(1.*IW)
      RETURN
      END
C#INCLUDE C:\ROBFIT\FORTRAN\PFIT
C#INCLUDE C:\ROBFIT\FORTRAN\VRLINK
C#INCLUDE C:\ROBFIT\FORTRAN\MENURD
C#INCLUDE C:\ROBFIT\FORTRAN\TCHREAD
C#INCLUDE C:\ROBFIT\FORTRAN\BSMSQ
C#INCLUDE C:\ROBFIT\FORTRAN\MACBKG
C#INCLUDE C:\ROBFIT\FORTRAN\BREAD
C#INCLUDE C:\ROBFIT\FORTRAN\MAOPEN
C$INCLUDE PFIT FOR
C$INCLUDE VRLINK FOR
C$INCLUDE MENURD FOR
C$INCLUDE TCHREAD FOR
C$INCLUDE BSMSQ FOR
C$INCLUDE MACBKG FOR
C$INCLUDE BREAD FOR
C$INCLUDE MAOPEN FOR
