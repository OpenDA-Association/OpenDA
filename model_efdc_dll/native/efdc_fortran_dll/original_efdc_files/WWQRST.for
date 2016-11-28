      SUBROUTINE WWQRST(ISRST)  
C  
C CHANGE RECORD  
C WRITE SPATIAL DISTRIBUTIONS AT THE END OF SIMULATION TO UNIT IWQORST.  
C  
      USE GLOBAL  

      CHARACTER*64 RESTFN ! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
C  
C WRITE ASCII RESTART FILE:  
C  
               IF (ISRST.EQ.0) THEN ! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
      OPEN(1,FILE='WQWCRST.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='WQWCRST.OUT',STATUS='UNKNOWN')  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      WRITE(1,101) N,TIME  
      WRITE(1,102)  
      NWQV0=NWQV  
      IF(IDNOTRVA.GT.0) NWQV0=NWQV0+1  
      DO L=2,LA  
        DO K=1,KC  
          WRITE(1,90) L,K,(WQV(L,K,NW),NW=1,NWQV0)  
        ENDDO  
      ENDDO  
      CLOSE(1)  
!
! { GEOSR X-species RESTART FILE EVERY REFERENCE TIME : JGCHO 2016.1.26
      if (NXSP.ge.1) then
        OPEN(1,FILE='WQWCRSTX.OUT',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='WQWCRSTX.OUT',STATUS='UNKNOWN')  
        WRITE(1,101) N,TIME  
        WRITE(1,103) 
        DO L=2,LA  
          DO K=1,KC  
            WRITE(1,90) L,K,(WQVX(L,K,nsp),nsp=1,NXSP)  
          ENDDO  
        ENDDO  
        CLOSE(1)  
      endif !if (NXSP.ge.1) then
! } GEOSR X-species RESTART FILE EVERY REFERENCE TIME : JGCHO 2016.1.26
!
! { GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
               ELSE  ! IF (ISRST.EQ.0) THEN
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      WRITE(RESTFN,'(A,I3.3,A)') 'WQWCRST',NINT(TIME),'.OUT'
      OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
!      OPEN(1,FILE='WQWCRST.OUT',STATUS='UNKNOWN')  
      WRITE(1,101) N,TIME  
      WRITE(1,102)  
      NWQV0=NWQV  
      IF(IDNOTRVA.GT.0) NWQV0=NWQV0+1  
      DO L=2,LA  
        DO K=1,KC  
          WRITE(1,90) L,K,(WQV(L,K,NW),NW=1,NWQV0)  
        ENDDO  
      ENDDO  
      CLOSE(1)  
!
! { GEOSR X-species RESTART FILE EVERY REFERENCE TIME : JGCHO 2016.1.26
      if (NXSP.ge.1) then
        WRITE(RESTFN,'(A,I3.3,A)') 'WQWCRSTX',NINT(TIME),'.OUT'
        OPEN(1,FILE=TRIM(RESTFN),STATUS='UNKNOWN')  
        WRITE(1,101) N,TIME  
        WRITE(1,103) 
        DO L=2,LA  
          DO K=1,KC  
            WRITE(1,90) L,K,(WQVX(L,K,nsp),nsp=1,NXSP)  
          ENDDO  
        ENDDO  
        CLOSE(1)  
      endif !if (NXSP.ge.1) then
! } GEOSR X-species RESTART FILE EVERY REFERENCE TIME : JGCHO 2016.1.26
!
               ENDIF ! IF (ISRST.EQ.0) THEN
! } GEOSR WRITE RESTART FILE EVERY REFERENCE TIME : JGCHO 2011.5.23
C  
C ALSO WRITE BINARY RESTART FILE:  
C  
   90 FORMAT(2I5, 1P, 22E12.4)  
  101 FORMAT('CC  WQ RESTART FILE TIME STEP, TIME = ',I10,F12.5)  
  102 FORMAT('C   L    K  BC          BD          BG          ',  
     &    'RPOC        LPOC        DOC         ',  
     &    'RPOP        LPOP        DOP         PTO4        ',  
     &    'RPON        LPON        DON         AMN         ',  
     &    'NIT         SU          SA          COD         ',  
     &    'DO          TAM         FCB        MALG')  
  103 FORMAT('C   L    K  Xn (NXSP)')
      RETURN  
      END  

