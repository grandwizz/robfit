      SUBROUTINE MENURD(NA)
      DIMENSION IR(20),FFCOM(200),FFVAR(200),RANS(10)
        CHARACTER*64 CTEMP
        CHARACTER*1 CANS(140)
      CHARACTER*64 NA,STATUS
      CHARACTER*40 FFCOM,FFVAR,STRIN1,STRIN2
      COMMON/USPAGE/FFVAR
      COMMON/BATCFL/IBATCH
      EQUIVALENCE (CTEMP(1:1),CANS(1))
      NC=0
      ICHANG=0
1     IF(IBATCH.EQ.3)THEN
        CALL CHREAD(21,80,RANS,CANS,0,64,IEND)
        IF(CANS(1).EQ.' ')GOTO 6000
        NA=CTEMP
      ENDIF
2     STATUS='OLD'
      CALL MAOPEN(17,NA,STATUS)
      IRN=0
      DO 10 I=1,20
        READ(17,*)IR(I)
        IF(IR(I).GE.100) GOTO 20
        IRN=IRN+1
10    CONTINUE
20    CONTINUE
      DO 30 I=1,160
        FFCOM(I)=' '
        FFVAR(I)=' '
30    CONTINUE
      DO 100 I=1,IRN
        IFF=(I-1)*20
        DO 200 J=1,IR(I)
          IFF=IFF+1
          READ(17,5000)K,FFCOM(IFF),FFVAR(IFF)
5000      FORMAT(1X,I2,1X,A40,A36)
200     CONTINUE
100   CONTINUE
        IF(IR(IRN+1).EQ.101.OR.IBATCH.GE.3)THEN
          CLOSE(17)
          RETURN
        ENDIF
1000   CONTINUE
C CLEAR SCREEN
      IF(NC.GT.0)CALL CMS('VMFCLEAR',IRT)
      NC=NC+1
      DO 300 I=1,IR(1)
      IF(I.GT.2)THEN
        WRITE(*,5000)I-2,FFCOM(I),FFVAR(I)
      ELSE
        WRITE(*,'(3X,A40)')FFCOM(I)
       ENDIF
300   CONTINUE
      PRINT*,' ENTER MENU #, <CR> TO RUN, S TO STOP, O FOR MORE OPTIONS'
      CALL CHREAD(9,80,RANS,CANS,0,1,IEND)
      IF(CANS(1).NE.'O'.AND.CANS(1).NE.'O'.AND.CANS(1).NE.'0'.AND.
     2 CANS(1).NE.'S'.AND.CANS(1).NE.'s')THEN
        IF(CANS(1).EQ.' ')THEN
          LLINE=100
        ELSE
          IP=1
          CANS(2)=' '
          RANS(1)=RNUMB(CANS,IP,1)
          LLINE=RANS(1)+.5
        ENDIF
      ELSE
        IF(CANS(1).NE.'S'.AND.CANS(1).NE.'s')THEN
          WRITE(*,5100)
5100      FORMAT(' ENTER LINE NUMBER FOR MENU, OR'/
     *'             96 TO START WITH NEW MENU NAME'/
     *'             97 TO WRITE MENU FILE TO DISK'/
     *'             98 FOR A GLOBAL CHANGE'/
     *'             99 TO STOP EXECUTION'/
     *'            100 OR <CR> TO RUN'/
     *'            101 TO make multiple passes (FSPFIT, BKGFIT ONLY)')
          CALL CHREAD(9,80,RANS,CANS,1,0,IEND)
          LLINE=RANS(1)+.5
          IF(RANS(1).EQ.0)LLINE=100
        ELSE
          LLINE=99
        ENDIF
      ENDIF
      IF(LLINE.NE.100)ICHANG=1
      IF(LLINE.EQ.96)THEN
        CLOSE(17)
        CALL CLBS(NA,NCLBS)
        PRINT*,'PRESENT FILE=',NA(NCLBS+1:)
        STATUS=NA(1:NCLBS)
        PRINT*,' ENTER A NEW FILE NAME OR <CR> FOR THIS ONE'
        CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
        IF(CANS(1).NE.' ')NA=STATUS(1:NCLBS)//CTEMP
        GOTO 2
      ENDIF
      IF(LLINE.EQ.97)THEN
        CLOSE(17)
        PRINT*,'PRESENT FILE=',NA
        PRINT*,' ENTER A NEW FILE NAME OR <CR> FOR THIS ONE'
        CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
        IF(CANS(1).NE.' ')NA=CTEMP
        STATUS='UNKNOWN'
        CALL MAOPEN(17,NA,STATUS)
        DO 4600 I=1,IRN+1
        WRITE(17,*)IR(I)
4600    CONTINUE
        DO 4700 I=1,IRN
        IFF=(I-1)*20
        DO 4800 J=1,IR(I)
        IFF=IFF+1
        WRITE(17,5000)J,FFCOM(IFF),FFVAR(IFF)
4800    CONTINUE
4700    CONTINUE
      ENDIF
      IF(LLINE.EQ.98)THEN
        PRINT*,' ENTER STRING TO BE CHANGED'
        CALL GSTRIN(STRIN1,L1)
        PRINT*,' ENTER STRING TO CHANGE TO'
        CALL GSTRIN(STRIN2,L2)
        DO 5200 I=1,IRN
        IFF=(I-1)*20
        DO 5200 J=1,IR(I)
        IFF=IFF+1
        CALL CHANGE(STRIN1,L1,STRIN2,L2,FFVAR(IFF),40)
5200    CONTINUE
      ENDIF
      IF(LLINE.EQ.101)THEN
        IBATCH=3
        CLOSE(17)
        PRINT*,' ENTER NAME OF FILE WITH MENU NAMES OR ',
     2 '<CR> TO MAKE SUCH A FILE'
        CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
        IF(CANS(1).NE.' ')THEN
          NA=CTEMP
          STATUS='UNKNOWN'
          CALL MAOPEN(21,NA,STATUS)
          GOTO 1
        ELSE
5205    PRINT*,' ENTER THE DESIRED NAME FOR THE FILE OF MENU NAMES'
          CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
          IF(CANS(1).EQ.' ')GOTO 5205
          NA=CTEMP
          STATUS='UNKNOWN'
          CALL MAOPEN(21,NA,STATUS)
          NMEN=0
5206      NMEN=NMEN+1
          PRINT*,' ENTER THE ',NMEN,' TH MENU NAME <CR> TO STOP'
          CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
          IF(CANS(1).EQ.' ')THEN
            REWIND 21
            GOTO 1
          ENDIF
          WRITE(21,'(A)')CTEMP
          GOTO 5206
        ENDIF
      ENDIF
      IF(LLINE.NE.99.AND.LLINE.NE.100)THEN
C     IF(LLINE.LT.10.OR.LLINE.GT.IRN-1)GOTO 1000
      IF(LLINE.GT.IRN-1)GOTO 1000
      ENDIF
      IF(LLINE.EQ.100.OR.LLINE.EQ.99)GOTO 3000
2000  CONTINUE

C CLEAR SCREEN
      CALL CMS('VMFCLEAR',IRT)

      IOFF=LLINE*20
      DO 400 I=1,IR(LLINE+1)
        IOFF=IOFF+1
      IF(I.GT.2)THEN
        WRITE(*,5000)I-2,FFCOM(IOFF),FFVAR(IOFF)
      ELSE
        WRITE(*,'(3X,A40)')FFCOM(IOFF)
      ENDIF
400   CONTINUE
      IOFF=LLINE*20
      WRITE(*,5300)
5300  FORMAT(' ENTER LINE NUMBER TO CHANGE A LINE
     1'/'  <CR> TO RETURN TO THE MAIN MENU; LN # OR
     1'/'   LN #,0,VAR TO CHANGE A LINE; LN #,1 TO DELETE A LINE;
     1'/'   OR LN #,100 TO ADD A COMPLETE NEW LINE')
5       CALL CHREAD(9,80,RANS,CANS,2,64,IEND)
        LSLINE=RANS(1)+.5
        IF(LSLINE.NE.0)ICHANG=1
        CHNGE=RANS(2)
        IF(RANS(2).EQ.0..AND.RANS(1).NE.0.)CHNGE=10
      LSLINE=LSLINE+2
      IF(LSLINE.LT.1.OR.LSLINE.GT.(IR(LLINE+1)+1))GOTO 1000
      IF(CHNGE.EQ.0)GOTO 1000
      IF(CHNGE.EQ.10.AND.NA.EQ.'VRMAIN.MNU'.AND.LLINE.EQ.3)THEN
        DO 420 I=1,20
420     FFVAR(60+I)=' '
        FFVAR(60+LSLINE)='RUN'
        GOTO 2000
      ENDIF
      IF(CHNGE.EQ.1)THEN
        NP=0
        DO 500 I=1,20
          NP=NP+1
          IOFF=IOFF+1
          IF(NP.GE.LSLINE)THEN
            LTEST=LLINE*20+20
            IF(NP.EQ.LTEST)THEN
              FFCOM(IOFF)=' '
              FFVAR(IOFF)=' '
            ELSE
              FFCOM(IOFF)=FFCOM(IOFF+1)
              FFVAR(IOFF)=FFVAR(IOFF+1)
            ENDIF
          ENDIF
500    CONTINUE
        IR(LLINE+1)=IR(LLINE+1)-1
      ENDIF
      IF(CHNGE.EQ.10)THEN
        IOFF=LLINE*20+LSLINE
        IF(CTEMP.NE.' ')THEN
          FFVAR(IOFF)=CTEMP
          GOTO 2000
        ENDIF
        IF(FFVAR(IOFF).EQ.'YES')THEN
          FFVAR(IOFF)='NO'
          GOTO 2000
        ENDIF
        IF(FFVAR(IOFF).EQ.'NO')THEN
          FFVAR(IOFF)='YES'
          GOTO 2000
        ENDIF
        IF(FFVAR(IOFF).EQ.'LOG')THEN
          FFVAR(IOFF)='LINEAR'
          GOTO 2000
        ENDIF
        IF(FFVAR(IOFF).EQ.'LINEAR')THEN
          FFVAR(IOFF)='LOG'
          GOTO 2000
        ENDIF
        WRITE(*,'('' ENTER THE NEW VARIABLE'')')
        CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
        FFVAR(IOFF)=CTEMP
      ENDIF
      IF(CHNGE.EQ.100) THEN
        IIOFF=LLINE*20
        DO 555 I=1,IR(LLINE+1)
         II=IR(LLINE+1)-I+1
         IF(LSLINE.LE.II)THEN
         FFCOM(IIOFF+II+1)=FFCOM(IIOFF+II)
         FFVAR(IIOFF+II+1)=FFVAR(IIOFF+II)
         ENDIF
555     CONTINUE
        IOFF=LLINE*20+LSLINE
        WRITE(*,5500)
5500    FORMAT(' ENTER THE NEW COMMENT')
        CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
        FFCOM(IOFF)=CTEMP
        WRITE(*,'('' ENTER THE NEW VARIABLE'')')
        CALL CHREAD(9,80,RANS,CANS,0,64,IEND)
        FFVAR(IOFF)=CTEMP
        IR(LLINE+1)=IR(LLINE+1)+1
      ENDIF
      GOTO 2000
3000  CONTINUE
      IF(ICHANG.EQ.1)THEN
        REWIND(17)
        DO 600 I=1,IRN+1
        WRITE(17,*)IR(I)
600     CONTINUE
        DO 700 I=1,IRN
        IFF=(I-1)*20
        DO 800 J=1,IR(I)
        IFF=IFF+1
        WRITE(17,5000)J,FFCOM(IFF),FFVAR(IFF)
800     CONTINUE
700     CONTINUE
      ENDIF
      CLOSE(17)
      IF(LLINE.EQ.99)NA='STOP'
      RETURN
6000  PRINT*,' NO MORE MENU NAMES'
      STOP
      END
      SUBROUTINE CTON(CI,NMISS,ANUM)
      CHARACTER*64 CI
      CHARACTER*1 KNUM(10),C
      DATA KNUM/'1','2','3','4','5','6','7','8','9','0'/
      POIFLG=0.
      SFLAG=0.
      NDP=0
      NUM=0
      NMISS=0
      DO 10 I=1,80
      NONUM=0
      C=CI(I:I+1)
      IF(C.EQ.' ')GOTO 100
        IF(C.EQ.'.')POIFLG=1.
        IF(C.EQ.'-')SFLAG=1.
        IF(C.EQ.'.')GOTO 10
        IF(C.EQ.'-')GOTO 10
        IF(POIFLG.EQ.1.)NDP=NDP+1
        DO 20 J=1,10
        IF(C.EQ.KNUM(J))THEN
          IF(J.NE.10)THEN
            NUM=NUM*10+J
          ELSE
            NUM=NUM*10
          ENDIF
          NONUM=1
        ENDIF
 20     CONTINUE
        IF(NONUM.NE.1)THEN
          NMISS=NMISS+1
        ENDIF
 10   CONTINUE
100   CONTINUE
      ANUM=NUM
      IF(SFLAG.EQ.1.)ANUM=-ANUM
      DO 200 J=1,NDP
      ANUM=ANUM/10
200   CONTINUE
      IF(NMISS.NE.0)THEN
        PRINT*,'THIS IS NOT AN INTEGER NMISS=',NMISS
        DIV=0.
        AX=1/DIV
      ENDIF
      RETURN
      END
        FUNCTION RNUMB(C,IP,IMAX)
        CHARACTER*1 C(140),KNUM(9)
        DATA KNUM/'1','2','3','4','5','6','7','8','9'/
        RNUMB=0
        IPT=IP
10      IF(IP.GT.IMAX)RETURN
        IF(C(IP).NE.' ')GOTO 20
        IP=IP+1
        GOTO 10
20      SI=1
        IF(C(IP).EQ.'-')THEN
        SI=-1
        IP=IP+1
        ENDIF
        IDPFL=0
        NDP=0
30      IF(IP.GT.IMAX)GOTO 500
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')GOTO 500
        IF(C(IP).EQ.'E'.OR.C(IP).EQ.'e')GOTO 400
        IF(C(IP).EQ.'.')THEN
        IDPFL=1
        IP=IP+1
        GOTO 30
        ENDIF
        IF(C(IP).EQ.'0')GOTO 35
        DO 32 J=1,9
32      IF(C(IP).EQ.KNUM(J))GOTO 35
        GOTO 500
35      IF(IDPFL.EQ.1)NDP=NDP+1
        RNUMB=RNUMB*10
        DO 40 J=1,9
40      IF(C(IP).EQ.KNUM(J))RNUMB=RNUMB+J
        IP=IP+1
        GOTO 30
400     IP=IP+1
        IEXP=0
        ISEXP=1
        IF(IP.GT.IMAX)GOTO 500
        IF(C(IP).EQ.'-')THEN
        ISEXP=-1
        IP=IP+1
        ENDIF
        IF(C(IP).EQ.'+')IP=IP+1
430     IF(IP.GT.IMAX)GOTO 480
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')GOTO 480
        IF(C(IP).EQ.'0')GOTO 435
        DO 432 J=1,9
432     IF(C(IP).EQ.KNUM(J))GOTO 435
        GOTO 480
435     IEXP=10*IEXP
        DO 440 J=1,9
440     IF(C(IP).EQ.KNUM(J))IEXP=IEXP+J
        IP=IP+1
        GOTO 430
480     CONTINUE
        IF(ISEXP*IEXP.GT.38)THEN
           RNUMB=SI*1.E38
        ELSE
           RNUMB=SI*RNUMB*10.**(ISEXP*IEXP-NDP)
        ENDIF
        IF(IP.GT.IMAX)RETURN
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')IP=IP+1
        RETURN
500     CONTINUE
        RNUMB=SI*RNUMB*10.**(-1*NDP)
        IF(C(IP).EQ.' '.OR.C(IP).EQ.',')IP=IP+1
        IF(IP.EQ.IPT)IP=IP+1
        RETURN
        END
        SUBROUTINE CHANGE(STR1,L1,STR2,L2,STR3,L3)
        CHARACTER*40 STR1,STR2,STR3,CTEMP
        IF(STR1(1:L1).NE.STR3(1:L1))RETURN
        CTEMP=STR3(L1+1:L3)
        STR3=STR2(1:L2)//CTEMP
        RETURN
        END
        SUBROUTINE GSTRIN(STRIN,LENGTH)
       CHARACTER*40 STRIN
       CHARACTER*40 STRING
       CHARACTER*1 CANS(64)
       DIMENSION RANS(2)
       EQUIVALENCE (CANS(1),STRING)
       STRING=' '
       CALL CHREAD(9,80,RANS,CANS,0,40,IEND)
        DO 20 I=38,0,-1
        IF(CANS(I+1).NE.' '.AND.CANS(I+2).NE.' ')GOTO 25
20      CONTINUE
25      LENGTH=I+2
        STRIN=STRING
       RETURN
       END
      SUBROUTINE CCHAR(NTAPE,NC)
      CHARACTER*64 NTAPE
      NC=64
      DO 10 I=1,64
      IF(NTAPE(NC:NC).NE.' ')GOTO 20
      NC=NC-1
10    CONTINUE
20    RETURN
      END
      SUBROUTINE CLBS(NTAPE,NC)
      CHARACTER*64 NTAPE
      NC=64
      DO 10 I=1,64
      IF(NTAPE(NC:NC).EQ.'\')GOTO 20
      NC=NC-1
10    CONTINUE
20    RETURN
      END
      SUBROUTINE CMVSA(NTAPE,TEMP,NC,IO)
      CHARACTER*64 NTAPE,TEMP
      CHARACTER*1 IO
      CALL CCHAR(NTAPE,NC)
      NB=MAX0(1,NC-6)
      TEMP=IO//NTAPE(NB:NC)
      NC=MIN0(8,NC)
      DO 10 I=2,NC
      IF(TEMP(I:I).EQ.' ')TEMP(I:I)='B'
      IF(TEMP(I:I).EQ.'.')TEMP(I:I)='P'
10    CONTINUE
20    RETURN
      END
