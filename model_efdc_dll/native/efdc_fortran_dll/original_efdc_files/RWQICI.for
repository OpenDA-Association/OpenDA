      SUBROUTINE RWQICI  
C  
C CHANGE RECORD  
C READ IN SPATIALLY AND/OR TEMPORALLY VARYING ICS (UNIT INWQICI).  
C  
      USE GLOBAL  
      CHARACTER TITLE(3)*79, ICICONT*3  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::XWQV  
      IF(.NOT.ALLOCATED(XWQV))THEN
		ALLOCATE(XWQV(NWQVM))
	    XWQV=0.0 
	ENDIF
C  
      OPEN(1,FILE=ICIFN,STATUS='UNKNOWN')  
      OPEN(2,FILE='WQ3D.OUT',STATUS='UNKNOWN',POSITION='APPEND')  

      WRITE(2,60)'* READING INITIAL CONDITIONS'  
      READ(1,50) (TITLE(M),M=1,3)  
      WRITE(2,999)  
      WRITE(2,50) (TITLE(M),M=1,3)  

      READ(1,999)  
      READ(1,50) TITLE(1)  
      WRITE(2,50) TITLE(1)  
      DO M=2,LA  
        READ(1,84) I,J,(XWQV(NW),NW=1,NWQV)  
        IF(IJCT(I,J).LT.1 .OR. IJCT(I,J).GT.8)THEN  
          PRINT*, 'I, J, LINE# = ', I,J,M-1  
          STOP 'ERROR!! INVALID (I,J) IN FILE 1'  
        ENDIF  
        L=LIJ(I,J)  
        DO K=1,KC
          DO NW=1,NWQV  
            WQV(L,K,NW)=XWQV(NW)  
          ENDDO  
        ENDDO
        WRITE(2,84) I,J,(WQV(L,1,NW),NW=1,NWQV)  
      ENDDO  
C  
C: WQCHLX=1/WQCHLX  
C  
      DO L=2,LA  
        DO K=1,KC  
          WQCHL(L,K) = WQV(L,K,1)*WQCHLC + WQV(L,K,2)*WQCHLD  
     &        + WQV(L,K,3)*WQCHLG  
          IF(IWQSRP.EQ.1)THEN  
            O2WQ_ = MAX(WQV(L,K,19), 0.0)  
            WQTAMD = MIN( WQTAMDMX*EXP(-WQKDOTAM*O2WQ_), WQV(L,K,20) )  
            WQTAMP(L,K) = WQV(L,K,20) - WQTAMD  
            WQPO4D(L,K) = WQV(L,K,10) / (1.0 + WQKPO4P*WQTAMP(L,K))  
            WQSAD(L,K)  = WQV(L,K,17) / (1.0 + WQKSAP*WQTAMP(L,K))  
          ELSE IF(IWQSRP.EQ.2)THEN  
            WQPO4D(L,K) = WQV(L,K,10) / (1.0 + WQKPO4P*SEDT(L,K))  
            WQSAD(L,K)  = WQV(L,K,17) / (1.0 + WQKSAP*SEDT(L,K))  
          ELSE  
            WQPO4D(L,K) = WQV(L,K,10)  
            WQSAD(L,K)  = WQV(L,K,17)  
          ENDIF  
        ENDDO  
      ENDDO  

      IWQICI = 0  
      CLOSE(1)  
      CLOSE(2)  

  999 FORMAT(1X)  
   50 FORMAT(A79)  
   52 FORMAT(I7, 1X, A3)  
   60 FORMAT(/, A24, I5, A24)  
!   84 FORMAT(3I5, 21E12.4)   ! BUG -> EDITED BY GEOSR : JGCHO 2010.11.11
   84 FORMAT(2I5,21E12.4)    ! EDITED BY GEOSR : JGCHO 2010.11.11
      RETURN  
      END  

