        SUBROUTINE CHREAD(IUNIT,IMAX,RANS,CANS,NR,NC,IEND)              TCH00010
        DIMENSION RANS(1)                                               TCH00020
        CHARACTER*1 CANS(1),C(140)                                      TCH00030
        IEND=0                                                          TCH00040
           IF(IUNIT.EQ.9)THEN                                           TCH00050
           DO I = 1,NR                                                  TCH00060
           RANS(I)=0                                                    TCH00070
           ENDDO                                                        TCH00080
           DO I= 1,NC                                                   TCH00090
           CANS(I)=' '                                                  TCH00100
           ENDDO                                                        TCH00110
C          PRINT*,' IN CHREAD'                                          TCH00120
C          READ(*,*)ITEST                                               TCH00130
        READ(*,'(140A1)',END=60)(C(I),I=1,IMAX)                         TCH00140
           ELSE                                                         TCH00150
        READ(IUNIT,'(140A1)',END=60)(C(I),I=1,IMAX)                     TCH00160
           ENDIF                                                        TCH00170
        IP=1                                                            TCH00180
        DO 10 I=1,NR                                                    TCH00190
        RANS(I)=RNUMB(C,IP,IMAX)                                        TCH00200
10      CONTINUE                                                        TCH00210
        DO 20 I=1,NC                                                    TCH00220
20      CANS(I)=' '                                                     TCH00230
30      IF(IP.GT.IMAX)RETURN                                            TCH00240
        IF(C(IP).NE.' '.AND.C(IP).NE.',')GOTO 40                        TCH00250
        IP=IP+1                                                         TCH00260
        GOTO 30                                                         TCH00270
40      NU=MIN0(NC,IMAX+1-IP)                                           TCH00280
        DO 50 I=1,NU                                                    TCH00290
50      CANS(I)=C(I+IP-1)                                               TCH00300
        RETURN                                                          TCH00310
60      CONTINUE                                                        TCH00320
        IF(IUNIT.EQ.9)REWIND(5)                                         TCH00330
        IEND=1                                                          TCH00340
        END                                                             TCH00350
