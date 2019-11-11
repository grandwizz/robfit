        SUBROUTINE MAOPEN(L,NA,STATUS)                                  CMS00010
        CHARACTER*64 NA,STATUS                                          CMS00020
        CHARACTER*64 TEMP                                               CMS00030
        CALL ERRSET(212,256,-1,1)                                       CMS00040
5       IT=INDEX(NA,'.')                                                CMS00050
        IF(IT.GT.0)THEN                                                 CMS00060
          TEMP='/'//NA(1:IT-1)//' '//NA(IT+1:)                          CMS00070
          IF(STATUS.EQ.'BIG')THEN                                       CMS00080
            TEMP='/'//NA(:IT-1)//' '//NA(IT+1:)                         CMS00090
C *** LRECL MUST BE LARGEST RECORD +4                                   CMS00100
            CALL FILEINF(IRT,'RECFM','V','LRECL',164)                   CMS00110
            OPEN(L,FILE=TEMP,STATUS='UNKNOWN',ERR=1197)                 CMS00120
            RETURN                                                      CMS00130
          ELSE                                                          CMS00140
            OPEN(L,FILE=TEMP,STATUS=STATUS,ERR=1197)                    CMS00150
          ENDIF                                                         CMS00160
          RETURN                                                        CMS00170
        ELSE                                                            CMS00180
          IT=INDEX(NA,' ')                                              CMS00190
          IF(NA(IT+1:IT+2).NE.' ')THEN                                  CMS00200
            TEMP='/'//NA                                                CMS00210
            IF(STATUS.EQ.'BIG')THEN                                     CMS00220
              CALL FILEINF(IRT,'RECFM','V','LRECL',164)                 CMS00230
              OPEN(L,FILE=TEMP,STATUS='UNKNOWN',ERR=1197)               CMS00240
              RETURN                                                    CMS00250
            ELSE                                                        CMS00260
              OPEN(L,FILE=TEMP,STATUS=STATUS,ERR=1197)                  CMS00270
              RETURN                                                    CMS00280
            ENDIF                                                       CMS00290
          ENDIF                                                         CMS00300
          TEMP='/'//NA(:IT-1)//' OUT'                                   CMS00310
          OPEN(L,FILE=TEMP,STATUS=STATUS,ERR=1197)                      CMS00320
          RETURN                                                        CMS00330
        ENDIF                                                           CMS00340
1197    PRINT*,' COULD NOT OPEN FILE= ',NA                              CMS00350
        PRINT*,' ENTER A NEW NAME OR STOP'                              CMS00360
        READ(*,'(A)')NA                                                 CMS00370
        IF(NA.EQ.'STOP'.OR.NA.EQ.'stop')STOP                            CMS00380
        GOTO 5                                                          CMS00390
        END                                                             CMS00400
        SUBROUTINE MAOPUF(L,NA,STATUS)                                  CMS00410
        CHARACTER*64 NA,STATUS                                          CMS00420
        CHARACTER*64 TEMP                                               CMS00430
5       IT=INDEX(NA,'.')                                                CMS00440
        IF(IT.GT.0)THEN                                                 CMS00450
          TEMP='/'//NA(1:IT-1)//' '//NA(IT+1:)                          CMS00460
          OPEN(L,FILE=TEMP,STATUS=STATUS,ERR=1197,FORM='UNFORMATTED')   CMS00470
          RETURN                                                        CMS00480
        ENDIF                                                           CMS00490
1197    PRINT*,' COULD NOT OPEN UNFORMATTED FILE= ',NA                  CMS00500
        PRINT*,' ENTER A NEW NAME OR STOP'                              CMS00510
        READ(*,'(A)')NA                                                 CMS00520
        IF(NA.EQ.'STOP'.OR.NA.EQ.'stop')STOP                            CMS00530
        GOTO 5                                                          CMS00540
        END                                                             CMS00550
