      SUBROUTINE WQ3DINP  
C  
C  READ WATER QUALITY SUBMODEL INPUT FILES  
C  ORGINALLY CODED BY K.-Y. PARK  
C  OPTIMIZED AND MODIFIED BY J. M. HAMRICK  
C CHANGE RECORD  
C  
      USE GLOBAL  
      USE MPI
      CHARACTER*3 CWQHDR(NWQVM)  
!{GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, YSSONG, 2015.12.10    
      CHARACTER*11 FLN1,FLN2
      CHARACTER*12 FLNX
!}
C PMC      CHARACTER*11  HHMMSS  
      DATA IWQTICI,IWQTAGR,IWQTSTL,IWQTSUN,IWQTBEN,IWQTPSL,IWQTNPL/7*0/  
      DATA ISMTICI/0/  
      IWQTICI=IWQTICI  
      IWQTAGR=IWQTAGR  
      IWQTSTL=IWQTSTL  
      IWQTSUN=IWQTSUN  
      IWQTBEN=IWQTBEN  
      IWQTPSL=IWQTPSL  
      IWQTNPL=IWQTNPL  
      ISMTICI=ISMTICI  
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
      IF(MYRANK.EQ.0)THEN
      OPEN(8702,FILE='WQ3D.OUT',STATUS='UNKNOWN')  
      CLOSE(8702,STATUS='DELETE')  
      ENDIF
      CALL MPI_BARRIER(MPI_COMM_WORLD,IERR)
C  
C **  HARDWIRE BY PASS OF RATE COEFFICIENT MAPS  
C  
      NWQKCNT=0  
      NWQKDPT=1  
      UHEQ(1)=0.0  
      UHEQ(LC)=0.0  
      DO ND=1,NDMWQ  
        LF=2+(ND-1)*LDMWQ  
        LL=LF+LDM-1  
        DO L=LF,LL  
          UHEQ(L)=1.0  
        ENDDO  
      ENDDO  
      ITNWQ = 0  
!{GeoSR, 2013.07.08 YSSONG, VARIABLE DZC
C      RKCWQ = 1.0/REAL(KC)  
C      DO K=1,KC  
C        WQHT(K)=REAL(KC-K)*RKCWQ  
C      ENDDO  
       WQHT(KC)=0.0
       DO K=KS,1,-1
         WQHT(K)=WQHT(K+1)+DZC(K+1)
       ENDDO  
C  
C      WQTSNAME(1)  = 'CHL'  
C      WQTSNAME(2)  = 'TPC'  
C      WQTSNAME(3)  = 'DOC'  
C      WQTSNAME(4)  = 'TPP'  
C      WQTSNAME(5)  = 'DOP'  
C      WQTSNAME(6)  = 'P4T'  
C      WQTSNAME(7)  = 'P4D'  
C      WQTSNAME(8)  = 'APC'  
C      WQTSNAME(9)  = 'TNN'  
C      WQTSNAME(10) = 'DON'  
C      WQTSNAME(11) = 'NH4'  
C      WQTSNAME(12) = 'NO3'  
C      WQTSNAME(13) = 'TSI'  
C      WQTSNAME(14) = 'SUU'  
C      WQTSNAME(15) = 'SAA'  
C      WQTSNAME(16) = 'SAD'  
C      WQTSNAME(17) = 'COD'  
C      WQTSNAME(18) = 'DOO'  
C      WQTSNAME(19) = 'TAM'  
C      WQTSNAME(20) = 'TMP'  
C      WQTSNAME(21) = 'FCB'  
C  
      WQTSNAME(1)  = 'CHC'  
      WQTSNAME(2)  = 'CHD'  
      WQTSNAME(3)  = 'CHG'  
      WQTSNAME(4)  = 'ROC'  
      WQTSNAME(5)  = 'LOC'  
      WQTSNAME(6)  = 'DOC'  
      WQTSNAME(7)  = 'P4D'  
      WQTSNAME(8)  = 'ROP'  
      WQTSNAME(9)  = 'LOP'  
      WQTSNAME(10) = 'DOP'  
      WQTSNAME(11) = 'RON'  
      WQTSNAME(12) = 'LON'  
      WQTSNAME(13) = 'DON'  
      WQTSNAME(14) = 'NHX'  
      WQTSNAME(15) = 'NOX'  
      WQTSNAME(16) = 'SUU'  
      WQTSNAME(17) = 'SAA'  
      WQTSNAME(18) = 'COD'  
      WQTSNAME(19) = 'DOX'  
      WQTSNAME(20) = 'TAM'  
      WQTSNAME(21) = 'FCB'  
C      DO M=0,NWQPS  
C        WQPSQ(M)=0.0   ! *** PMC-NOT USED
C        WQPSQC(M)=0.0  ! *** PMC-NOT USED
C        DO J=1,NWQV  
C          WQWPSLC(M,J)=0.0  
C        ENDDO  
C      ENDDO  
      DO K=1,KC  
        IWQPSC(1,K)=0  
C        WQDSQ(1,K)=0.0  
        IWQPSC(LC,K)=0  
C        WQDSQ(LC,K)=0.0  
      ENDDO  
      DO ND=1,NDMWQ  
        LF=2+(ND-1)*LDMWQ  
        LL=LF+LDM-1  
        DO K=1,KC  
          DO L=LF,LL  
            IWQPSC(L,K)=0  
            IWQPSV(L,K)=0  
C            WQDSQ(L,K)=0.0  
          ENDDO  
        ENDDO  
      ENDDO  
      DO J=1,NWQV
        DO K=1,KC  
          DO L=1,LC  
            WQWDSL(L,K,J)=0.0  
            WQWPSL(L,K,J)=0.0  
          ENDDO  
        ENDDO  
      ENDDO  
      CALL RWQC1  
C  
C      CALL RWQC2  
C      CALL RWQMAP  
C  
      IF(DEBUG)THEN
      IF(MYRANK.EQ.0)THEN
      OPEN(1,FILE='WQWCTS.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='WQWCTS.OUT',STATUS='UNKNOWN')  
      ENDIF
      NWQVOUT=0  
      DO NW=1,NWQV  
        IF(ISTRWQ(NW).EQ.1)THEN  
          NWQVOUT=NWQVOUT+1  
          CWQHDR(NWQVOUT)=WQTSNAME(NW)  
        ENDIF  
      ENDDO  
      IF(MYRANK.EQ.0) WRITE(1,1969)(CWQHDR(NW),NW=1,NWQVOUT)  
 1969 FORMAT('C   I    J    K    TIME',7X,A3,8X,A3,8X,A3,  
     &    8X,A3,8X,A3,8X,A3,8X,A3,8X,A3,8X,A3,  
     &    8X,A3,8X,A3,8X,A3,8X,A3,8X,A3,8X,A3,  
     &    8X,A3,8X,A3,8X,A3,8X,A3,8X,A3,8X,A3)  
      IF(MYRANK.EQ.0) CLOSE(1)  
      ENDIF
C  
C **  INITIALIZE DIURNAL DO ANALYSIS  
C  
      IF(NDDOAVG.GE.1.AND.DEBUG)THEN  
        IF(MYRANK.EQ.0)THEN
        OPEN(1,FILE='DIURNDO.OUT')  
        CLOSE(1,STATUS='DELETE')  
        ENDIF
        DO K=1,KC  
          DO L=2,LA  
            DDOMAX(L,K)=-1.E6  
            DDOMIN(L,K)=1.E6  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  INITIALIZE LIGHT EXTINCTION ANALYSIS  
C  
      IF(NDLTAVG.GE.1)THEN  
        IF(MYRANK.EQ.0)THEN
        OPEN(1,FILE='LIGHT.OUT')  
        CLOSE(1,STATUS='DELETE')  
        ENDIF
        NDLTCNT=0  
        DO K=1,KC  
          DO L=2,LA  
            RLIGHTT(L,K)=0.  
            RLIGHTC(L,K)=0.  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C ** INITIALIZE WATER QUALITY AVERAGING SUMMATION ARRAYS:  
C  
      CALL WQZERO  
      IF(IWQICI.EQ.2) CALL RWQRST  
      IF(IWQBEN.EQ.1)THEN  
        CALL SMINIT  
        CALL SMRIN1
C  
        IF(ISMICI.EQ.2) CALL RSMRST  
      ENDIF  

      ! *** READ WQ TIMESERIES
      CALL RWQCSR
C  
!{GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, YSSONG, 2015.12.10   
        IF(IWQTS.GE.1)THEN
          IF(ISCOMP .EQ. 3. OR. ISCOMP .EQ. 4)THEN
            DO K=1,KC
              WRITE(FLN1,"('WQRTS',I2.2,'.DAT')") K
              WRITE(FLN2,"('WQLIM',I2.2,'.DAT')") K
              IF(MYRANK.EQ.0)THEN
              OPEN(1,FILE=FLN1,STATUS='UNKNOWN')
              OPEN(3,FILE=FLN2,STATUS='UNKNOWN')
              CLOSE(1,STATUS='DELETE')        
              CLOSE(3,STATUS='DELETE')        
              IF(NXSP.GT.0) THEN
                WRITE(FLNX,"('WQLIMX',I2.2,'.DAT')") K
                OPEN(333,FILE=FLNX,STATUS='UNKNOWN')
                CLOSE(333,STATUS='DELETE')             
              ENDIF
              ENDIF
            ENDDO  
          ENDIF
        ENDIF
!}GeoSR, GROWTH LIMIT AND ALGAL RATE PRINT, YSSONG, 2015.12.10 
C 100 FORMAT('  TIME = ',A11,' HH.MM.SS.HH')  
      RETURN  
      END  

