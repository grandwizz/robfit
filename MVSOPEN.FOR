        SUBROUTINE MAOPEN(L,NA,STATUS)                                  MVS00010
        CHARACTER*64 NA,STATUS                                          MVS00020
        CHARACTER*64 TEMP                                               MVS00030
        CHARACTER*1 IO                                                  MVS00040
        IF(NA(1:2).NE.'UF')THEN                                         MVS00050
          IO='O'                                                        MVS00060
          IF(STATUS.EQ.'OLD')IO='I'                                     MVS00070
          CALL CMVSA(NA,TEMP,NC,IO)                                     MVS00080
          OPEN(L,FILE=TEMP,STATUS='OLD',ERR=1197)                       MVS00090
          RETURN                                                        MVS00100
        ELSE                                                            MVS00110
          TEMP='/'//NA                                                  MVS00120
          OPEN(L,FILE=TEMP,STATUS='OLD',ERR=1197)                       MVS00130
          RETURN                                                        MVS00140
        ENDIF                                                           MVS00150
1197    PRINT*,' COULD NOT OPEN FILE= ',NA                              MVS00160
        PRINT*,' WHICH BECAME ',TEMP                                    MVS00170
        PRINT*,' POSSIBLY BECAUSE STATUS OF ',STATUS                    MVS00180
        PRINT*,' WAS NOT OLD'                                           MVS00190
        STOP                                                            MVS00200
        END                                                             MVS00210
        SUBROUTINE MAOPUF(L,NA,STATUS)                                  MVS00220
        CHARACTER*64 NA,STATUS                                          MVS00230
        CHARACTER*64 TEMP                                               MVS00240
5       IT=INDEX(NA,'.')                                                MVS00250
        IF(IT.GT.0)THEN                                                 MVS00260
          TEMP='/'//NA(1:IT-1)//' '//NA(IT+1:)                          MVS00270
          OPEN(L,FILE=TEMP,STATUS=STATUS,ERR=1197,FORM='UNFORMATTED')   MVS00280
          RETURN                                                        MVS00290
        ENDIF                                                           MVS00300
1197    PRINT*,' COULD NOT OPEN UNFORMATTED FILE= ',NA                  MVS00310
        PRINT*,' ENTER A NEW NAME OR STOP'                              MVS00320
        READ(*,'(A)')NA                                                 MVS00330
        IF(NA.EQ.'STOP'.OR.NA.EQ.'stop')STOP                            MVS00340
        GOTO 5                                                          MVS00350
        END                                                             MVS00360
        SUBROUTINE CMS(NA,IRT)                                          MVS00370
        CHARACTER*64 NA                                                 MVS00380
        IRT=0                                                           MVS00390
        RETURN                                                          MVS00400
        END                                                             MVS00410
