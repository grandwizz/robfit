      BLOCK DATA                                                        VPL00010
        COMMON/OR/NC,IL,IBP,IC(16),ILP,ICP(16)                          VPL00020
      DATA NC,IL,ILP/3*0/                                               VPL00030
      END                                                               VPL00040
        SUBROUTINE STPL(XE,YE)                                          VPL00050
C CALL STPL(IX,IY) AT THE FIRST POINT TO START PLOTTING.  CALL STPL     VPL00060
C WITH ANY IX,IY TO EITHER END A PLOT OR SET UP FOR POINT PLOTTING      VPL00070
C THE VECTOR IC CONTAINS THE ENDS OF LINES DRAWN FROM IBP TO IC(1)      VPL00080
C TO IC(2) TO IC(3) TO ETC. *** THIS ROUTINE HAS ASCI IN NORMAL ORDER **VPL00090
C *** DESIGNED WITH DAVID NESTLE'S ARBIO ROUTINES FOR USE WITH PROTOCOL VPL00100
C *** CONVERTER --- 3270 EMULATOR --- NOTE ARBOPN IN STPL ARBCLS IN ANSIVPL00110
C THE VECTOR ICP CONTAINS ILP LOCATIONS OF POINTS TO BE PLOTTED         VPL00120
C NOTE THAT THE VAX REQUIRES ALL HEX VALUES IN THE CONSTANTS TO BE IN   VPL00130
C  REVERSE ORDER, DOES NOT NEED 0003 IN IVG, BUT DOES NEED <CR> <LF> IN VPL00140
C IBL.                                                                  VPL00150
        COMMON/OR/NC,IL,IBP,IC(16),ILP,ICP(16)                          VPL00160
      INTEGER IBUF(80)                                                  VPL00170
      CHARACTER*1 CLINE(320),ETEK(6)                                    VPL00180
      EQUIVALENCE (CLINE(1),IBUF(1))                                    VPL00190
      DATA IVG/Z0003001D/,IBL/Z0A0D2020/,IPG/Z00001C00/                 VPL00200
      DATA ETEK/Z1B,Z5B,Z3F,Z33,Z38,Z68/                                VPL00210
      IF(NC.NE.0)GOTO 5                                                 VPL00220
C *** IL IS THE NUMBER OF LINE ENDS NOT YET OUTPUTTED, ILP THE          VPL00230
C *** THE NUMBER OF POINTS NOT YET OUTPUTTED                            VPL00240
      IL=0                                                              VPL00250
      ILP=0                                                             VPL00260
C *** NC=1 TELLS STPL THAT ARB IS OPEN AND THAT WE ARE IN PLOT MODE     VPL00270
      NC=1                                                              VPL00280
      CALL ARBOPN(-1,IRET)                                              VPL00290
C *** ONCE ARB HAS BEEN OPENED NORMAL I-O CAUSES ERROR UNTIL IT IS      VPL00300
C *** CLOSED.  THE CLOSE IS IN THE ROUTINE ANSI WHICH RESETS NC TO 0    VPL00310
      IF (IRET .NE. 0) THEN                                             VPL00320
         WRITE(*,*)'ARBOPN: IRET =', IRET                               VPL00330
         STOP                                                           VPL00340
      ENDIF                                                             VPL00350
      CALL ARBPUT(ETEK,6,IRET)                                          VPL00360
      IF (IRET .NE. 0) THEN                                             VPL00370
          WRITE(*,*)'ARBPUT STPL ETEK OUTPUT: IRET =', IRET             VPL00380
          STOP                                                          VPL00390
      ENDIF                                                             VPL00400
5     CONTINUE                                                          VPL00410
C *** THE IF LOOP OUTPUTS LINE ENDS STORED IN IC                        VPL00420
         IF(IL.GE.1)THEN                                                VPL00430
         IBUF(1)=IVG                                                    VPL00440
         IBUF(2)=IBP                                                    VPL00450
         DO 30 I=1,IL                                                   VPL00460
 30      IBUF(I+2)=IC(I)                                                VPL00470
         IBUF(IL+3)=IBL                                                 VPL00480
         IOUT=4*(IL+3)-2                                                VPL00490
         CALL ARBPUT(CLINE,IOUT,IRET)                                   VPL00500
        IF (IRET .NE. 0) THEN                                           VPL00510
          WRITE(*,*)'ARBPUT STPL LINE END OUTPUT: IRET =', IRET         VPL00520
          STOP                                                          VPL00530
        ENDIF                                                           VPL00540
      ENDIF                                                             VPL00550
C *** THE FOLLOWING IF THEN OUTPUTS POINTS STORED IN ICP                VPL00560
      IF(ILP.GE.1)THEN                                                  VPL00570
        IBUF(1)=IVG                                                     VPL00580
        IBUF(2)=IPG                                                     VPL00590
        DO 40 I=1,ILP                                                   VPL00600
 40     IBUF(I+2)=ICP(I)                                                VPL00610
        IBUF(ILP+3)=IBL                                                 VPL00620
        IOUT=4*(ILP+3)-2                                                VPL00630
        CALL ARBPUT(CLINE,IOUT,IRET)                                    VPL00640
        IF (IRET .NE. 0) THEN                                           VPL00650
          WRITE(*,*)'ARBPUT STPL POINT OUTPUT: IRET =', IRET            VPL00660
          STOP                                                          VPL00670
        ENDIF                                                           VPL00680
        ILP=0                                                           VPL00690
      ENDIF                                                             VPL00700
      IF(XE.LT.0.)THEN                                                  VPL00710
        CALL ANSI                                                       VPL00720
        RETURN                                                          VPL00730
      ENDIF                                                             VPL00740
      IXE=1024*AMIN1(.99,AMAX1(.01,XE))                                 VPL00750
      IYE=768*AMIN1(.99,AMAX1(.02,YE))                                  VPL00760
      IX=MAX0(0,MIN0(1023,IXE))                                         VPL00770
      IY=MAX0(0,MIN0(767,IYE))                                          VPL00780
C *** CALCULATES THE DESIRED CURSOR POSITION TO BEGIN DRAWING LINES     VPL00790
        IBP=IX-32*(IX/32)+64+256*((IX/32)+32+256*(IY                    VPL00800
     2 -32*(IY/32)+96+256*((IY/32)+32)))                                VPL00810
        IL=0                                                            VPL00820
      RETURN                                                            VPL00830
        END                                                             VPL00840
        SUBROUTINE PLOT(XE,YE)                                          VPL00850
C CALL PLOT(IX,IY) TO DRAW A LINE TO IX,IY FROM THE IX1,IY1 IN THE      VPL00860
C LAST CALL TO EITHER PLOT OR STPL                                      VPL00870
      COMMON/OR/NC,IL,IBP,IC(16)                                        VPL00880
      INTEGER IBUF(80)                                                  VPL00890
      CHARACTER*1 CLINE(320)                                            VPL00900
      EQUIVALENCE (CLINE(1),IBUF(1))                                    VPL00910
      DATA IVG/Z0003001D/,IBL/Z0A0D2020/                                VPL00920
      IXE=1024*AMIN1(.99,AMAX1(.01,XE))                                 VPL00930
      IYE=768*AMIN1(.99,AMAX1(.02,YE))                                  VPL00940
      IX=MAX0(0,MIN0(1023,IXE))                                         VPL00950
      IY=MAX0(0,MIN0(767,IYE))                                          VPL00960
C *** ADDS ONE TO THE NUMBER OF LINE ENDS STORED IN IC                  VPL00970
        IL=IL+1                                                         VPL00980
        IC(IL)=(IX-32*(IX/32))+64+256*((IX/32)+32                       VPL00990
     2 +256*((IY-32*(IY/32))+96+256*((IY/32)+32)))                      VPL01000
C *** IF IC=16 OUTPUTS THE LINE ENDS STORED IN IC AND RESETS IBP        VPL01010
      IF(IL.LT.16)RETURN                                                VPL01020
         IBUF(1)=IVG                                                    VPL01030
         IBUF(2)=IBP                                                    VPL01040
         DO 30 I=1,IL                                                   VPL01050
 30     IBUF(I+2)=IC(I)                                                 VPL01060
        IBUF(IL+3)=IBL                                                  VPL01070
        IOUT=4*(IL+3)                                                   VPL01080
        CALL ARBPUT(CLINE,IOUT,IRET)                                    VPL01090
        IF (IRET .NE. 0) THEN                                           VPL01100
          WRITE(*,*)'ARBPUT PLOT LINE END OUTPUT: IRET =', IRET         VPL01110
          STOP                                                          VPL01120
        ENDIF                                                           VPL01130
        IBP=IC(IL)                                                      VPL01140
        IL=0                                                            VPL01150
      RETURN                                                            VPL01160
        END                                                             VPL01170
      SUBROUTINE PONT(XE,YE)                                            VPL01180
C TO PLOT A POINT AT IXE,IYE AFTER A CALL HAS AT SOMETIME BEEN MADE TO  VPL01190
C STPL SIMPLY CALL PONT(IXE,IYE).  THE POINT MAY NOT BE ACTUALLY SEEN   VPL01200
C UNTIL THE SET OF PLOT CALLS ARE ENDED WITH A LAST CALL TO STPL        VPL01210
        CALL STPL(XE-.002,YE)                                           VPL01220
        CALL PLOT(XE+.002,YE)                                           VPL01230
        RETURN                                                          VPL01240
        END                                                             VPL01250
      SUBROUTINE BCHART(XE,YE,CHAR)                                     VPL01260
      COMMON/OR/NC,IL,IBP(4)                                            VPL01270
      CHARACTER*64 CHAR                                                 VPL01280
      CHARACTER*1 IBUF(73),IVG(4),IPR,IBL(4),IBP,CT(64)                 VPL01290
      DATA IVG,IPR/Z00,Z03,Z00,Z1D,Z1F/,IBL/Z0A,Z0D,2*Z20/              VPL01300
      DO 2 I=1,64                                                       VPL01310
      CT(I)=CHAR(I:I)                                                   VPL01320
      IF(CT(I).EQ.'\')GOTO 4                                            VPL01330
2     IF(CT(I).EQ.'\')GOTO 4                                            VPL01340
      I=65                                                              VPL01350
4     N=I-1                                                             VPL01360
      XC=AMAX1(.01,AMIN1(XE,.99-.013*N))                                VPL01370
      YC=AMAX1(.05,YE)                                                  VPL01380
      CALL STPL(XC,YC)                                                  VPL01390
      DO 6 I=1,4                                                        VPL01400
 6    IBUF(I)=IVG(I)                                                    VPL01410
      DO 8 I=1,4                                                        VPL01420
8     IBUF(I+4)=IBP(I)                                                  VPL01430
      IBUF(9)=IPR                                                       VPL01440
      CALL CETOAS(CT,N)                                                 VPL01450
      DO 9 I=1,N                                                        VPL01460
9     IBUF(I+9)=CT(I)                                                   VPL01470
      DO 12,I=1,4                                                       VPL01480
12    IBUF(N+9+I)=IBL(I)                                                VPL01490
      CALL ARBPUT(IBUF,N+13,IRET)                                       VPL01500
      RETURN                                                            VPL01510
      END                                                               VPL01520
      SUBROUTINE ANSI                                                   VPL01530
      COMMON/OR/NC                                                      VPL01540
C IANSI RETURNS US TO THE NON-GRAPHICS SELANAR SCREEN                   VPL01550
      CHARACTER*1 IBUF(8),XTEK(8)                                       VPL01560
      DATA IBUF/Z1B,Z32,Z00,Z03,Z0A,Z0D,2*Z20/                          VPL01570
      DATA XTEK/Z1B,Z5B,Z3F,Z33,Z38,Z31,2*Z20/                          VPL01580
      IF(NC.EQ.0)RETURN                                                 VPL01590
      CALL ARBPUT(IBUF,8,IRET)                                          VPL01600
C     CALL ARBPUT(XTEK,8,IRET)                                          VPL01610
C *** XTEK SHOULD EXIT TEK FOR KERMIT, DOES NOT ENTER VT102             VPL01620
      CALL ARBCLS(IRET)                                                 VPL01630
      NC=0                                                              VPL01640
      RETURN                                                            VPL01650
      END                                                               VPL01660
      SUBROUTINE CLEARS                                                 VPL01670
      CHARACTER*1 IBUF(16)                                              VPL01680
      COMMON/OR/NC                                                      VPL01690
      DATA IBUF/Z1B,Z31,Z00,Z03,Z1B,Z0C,Z00,Z00,                        VPL01700
     2 Z1B,Z32,Z00,Z00,Z0A,Z0D,Z20,Z20/                                 VPL01710
      CALL STPL(0.,0.)                                                  VPL01720
      CALL ARBPUT(IBUF,16,IRET)                                         VPL01730
      CALL STPL(-1.,-1.)                                                VPL01740
      RETURN                                                            VPL01750
      END                                                               VPL01760
      SUBROUTINE LINET(CHAR)                                            VPL01770
      CHARACTER*1 CLINE(12),CSESC(4),CBL(2)                             VPL01780
      CHARACTER*4 CHAR                                                  VPL01790
      CHARACTER*1 CSOL,CDOT,CDOTDA,CSDASH,CLDASH                        VPL01800
      DATA CSESC/Z00,Z03,Z1B,Z00/                                       VPL01810
      DATA CSOL/Z5C/,CDOT/Z61/,CDOTDA/Z62/,                             VPL01820
     2 CSDASH/Z63/,CLDASH/Z64/                                          VPL01830
      DATA CBL/Z0A,Z0D/                                                 VPL01840
      CALL STPL(0.,0.)                                                  VPL01850
      DO 10 I=1,4                                                       VPL01860
10    CLINE(I)=CSESC(I)                                                 VPL01870
      IF(CHAR.EQ.'SOLI')CLINE(5)=CSOL                                   VPL01880
      IF(CHAR.EQ.'DOT ')CLINE(5)=CDOT                                   VPL01890
      IF(CHAR.EQ.'DODA')CLINE(5)=CDOTDA                                 VPL01900
      IF(CHAR.EQ.'SDAS')CLINE(5)=CSDASH                                 VPL01910
      IF(CHAR.EQ.'LDAS')CLINE(5)=CLDASH                                 VPL01920
      CLINE(6)=CBL(1)                                                   VPL01930
      CLINE(7)=CBL(2)                                                   VPL01940
      CALL ARBPUT(CLINE,7,IRET)                                         VPL01950
      IF (IRET .NE. 0) THEN                                             VPL01960
        WRITE(*,*)'ARBPUT LINET OUTPUT: IRET =', IRET                   VPL01970
        STOP                                                            VPL01980
      ENDIF                                                             VPL01990
      RETURN                                                            VPL02000
      END                                                               VPL02010
        SUBROUTINE CHREAD(IUNIT,IMAX,RANS,CANS,NR,NC,IEND)              VPL02020
        COMMON/OR/NGRA                                                  VPL02030
        DIMENSION RANS(1)                                               VPL02040
        CHARACTER*1 CANS(1),C(140)                                      VPL02050
        CHARACTER*140 CTEMP                                             VPL02060
        EQUIVALENCE (CTEMP,C(1))                                        VPL02070
        IEND=0                                                          VPL02080
           IF(IUNIT.EQ.9)THEN                                           VPL02090
              IF(NGRA.NE.1)THEN                                         VPL02100
                DO 5 I=1,NR                                             VPL02110
5               RANS(I)=0                                               VPL02120
                DO 8 I=1,NC                                             VPL02130
8               CANS(I)=' '                                             VPL02140
                READ(*,'(140A1)',END=60,ERR=60)(C(I),I=1,IMAX)          VPL02150
              ELSE                                                      VPL02160
                 CTEMP=' '                                              VPL02170
                 CALL ARBGET(C,IL,IRET)                                 VPL02180
                 IF (IRET .NE. 0) THEN                                  VPL02190
                   WRITE(*,*)'ARBGET CHREAD INPUT: IRET =', IRET        VPL02200
                   STOP                                                 VPL02210
                 ENDIF                                                  VPL02220
                 CALL CATOEB(C,IL)                                      VPL02230
              ENDIF                                                     VPL02240
           ELSE                                                         VPL02250
        IF(IMAX.GT.80)READ(IUNIT,'(140A1)',END=60)(C(I),I=1,IMAX)       VPL02260
        IF(IMAX.LE.80)READ(IUNIT,'(80A1)',END=60)(C(I),I=1,IMAX)        VPL02270
           ENDIF                                                        VPL02280
        IP=1                                                            VPL02290
        DO 10 I=1,NR                                                    VPL02300
        RANS(I)=RNUMB(C,IP,IMAX)                                        VPL02310
10      CONTINUE                                                        VPL02320
        DO 20 I=1,NC                                                    VPL02330
20      CANS(I)=' '                                                     VPL02340
30      IF(IP.GT.IMAX)RETURN                                            VPL02350
        IF(C(IP).NE.' '.AND.C(IP).NE.',')GOTO 40                        VPL02360
        IP=IP+1                                                         VPL02370
        GOTO 30                                                         VPL02380
40      NU=MIN0(NC,IMAX+1-IP)                                           VPL02390
        DO 50 I=1,NU                                                    VPL02400
50      CANS(I)=C(I+IP-1)                                               VPL02410
        RETURN                                                          VPL02420
60      CONTINUE                                                        VPL02430
        IF(IUNIT.EQ.9)THEN                                              VPL02440
          REWIND(5)                                                     VPL02450
        ELSE                                                            VPL02460
          REWIND(IUNIT)                                                 VPL02470
        ENDIF                                                           VPL02480
        IEND=1                                                          VPL02490
        END                                                             VPL02500
      SUBROUTINE CETOAS(CTEMP,NMAX)                                     VPL02510
C *** CONVERTS EBCIDC TO ASCII                                          VPL02520
      CHARACTER*1 CTEMP(NMAX)                                           VPL02530
      CHARACTER*4 CT                                                    VPL02540
      INTEGER*4 IC                                                      VPL02550
      EQUIVALENCE (CT,IC)                                               VPL02560
      DO 10 I=1,NMAX                                                    VPL02570
      IC=0                                                              VPL02580
      CT=CT(1:3)//CTEMP(I)                                              VPL02590
      IC=JTRAN(IC)                                                      VPL02600
      CTEMP(I)=CT(4:4)                                                  VPL02610
10    CONTINUE                                                          VPL02620
      RETURN                                                            VPL02630
      END                                                               VPL02640
      SUBROUTINE CATOEB(CTEMP,NMAX)                                     VPL02650
C *** CONVERTS ASCII TO EBCIDC                                          VPL02660
      CHARACTER*1 CTEMP(NMAX)                                           VPL02670
      CHARACTER*4 CT                                                    VPL02680
      INTEGER*4 IC                                                      VPL02690
      EQUIVALENCE (CT,IC)                                               VPL02700
      DO 10 I=1,NMAX                                                    VPL02710
      IC=0                                                              VPL02720
      CT=CT(1:3)//CTEMP(I)                                              VPL02730
      IC=ITRAN(IC)                                                      VPL02740
      CTEMP(I)=CT(4:4)                                                  VPL02750
10    CONTINUE                                                          VPL02760
      RETURN                                                            VPL02770
      END                                                               VPL02780
        FUNCTION ITRAN(IX)                                              VPL02790
C ITRAN CONVERTS ASCII VALUES TO EBCIDC                                 VPL02800
      DIMENSION ITABLE(128)                                             VPL02810
      DATA ITABLE/  0,  1,  2,  3, 55, 45, 46, 47, 22,  5, 37, 11, 12,  VPL02820
     # 13, 14, 15, 16, 17, 18, 59, 60, 61, 50, 38, 24, 25, 63, 39, 28,  VPL02830
     # 29, 30, 31, 64, 90,127,123, 91,108, 80,125, 77, 93, 92, 78,107,  VPL02840
     # 96, 75, 97,240,241,242,243,244,245,246,247,248,249,122, 94, 76,  VPL02850
     #126,110,111,124,193,194,195,196,197,198,199,200,201,209,210,211,  VPL02860
     #212,213,214,215,216,217,226,227,228,229,230,231,232,233,173,224,  VPL02870
     #189, 95,109,121,129,130,131,132,133,134,135,136,137,145,146,147,  VPL02880
     #148,149,150,151,152,153,162,163,164,165,166,167,168,169,192, 79,  VPL02890
     #155,161,  7/                                                      VPL02900
      IX=MIN0(127,MAX0(0,IX))                                           VPL02910
      ITRAN=ITABLE(IX+1)                                                VPL02920
      RETURN                                                            VPL02930
      END                                                               VPL02940
        FUNCTION JTRAN(IX)                                              VPL02950
C ITRAN CONVERTS EBCIDC VALUES TO ASCII                                 VPL02960
      DIMENSION ITABLE(256)                                             VPL02970
      DATA ITABLE/0,1,2,3, -1,  9, -1,255, -1, -1, -1, 11, 12, 13, 14,  VPL02980
     A  15, 16, 17, 18, -1, -1, -1,  8, -1, 24, 25, -1, -1, 28, 29, 30, VPL02990
     C  31, -1, -1, -1, -1, -1, 10, 23, 27, -1, -1, -1, -1, -1,  5,  6, VPL03000
     D   7, -1, -1, 22, -1, -1, -1, -1,  4, -1, -1, -1, 19, 20, 21, -1, VPL03010
     E  26, 32, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 46, 60, 40, 43, VPL03020
     F 124, 38, -1, -1, -1, -1, -1, -1, -1, -1, -1, 33, 36, 42, 41, 59, VPL03030
     G  94, 45, 47, -1, -1, -1, -1, -1, -1, -1, -1, -1, 44, 37, 95, 62, VPL03040
     H  63, -1, -1, -1, -1, -1, -1, -1, -1, -1, 96, 58, 35, 64, 39, 61, VPL03050
     I  34, -1, 97, 98, 99,100,101,102,103,104,105, -1, -1, -1, -1, -1, VPL03060
     J  -1, -1,106,107,108,109,110,111,112,113,114, -1,125, -1, -1, -1, VPL03070
     K  -1, -1,126,115,116,117,118,119,120,121,122, -1, -1, -1, 91, -1, VPL03080
     L  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 93, -1, VPL03090
     M  -1,123, 65, 66, 67, 68, 69, 70, 71, 72, 73, -1, -1, -1, -1, -1, VPL03100
     N  -1, -1, 74, 75, 76, 77, 78, 79, 80, 81, 82, -1, -1, -1, -1, -1, VPL03110
     O  -1, 92, -1, 83, 84, 85, 86, 87, 88, 89, 90, -1, -1, -1, -1, -1, VPL03120
     P  -1, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, -1, -1, -1, -1, -1, VPL03130
     Q  -1/                                                             VPL03140
      JTRAN=ITABLE(IX+1)                                                VPL03150
      RETURN                                                            VPL03160
      END                                                               VPL03170
