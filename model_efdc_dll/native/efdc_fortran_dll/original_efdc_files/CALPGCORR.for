      SUBROUTINE CALPGCORR
C  
C **  SUBROUTINE CALPGCORR APPLIES A CORRECTOR TO THE EXTERNAL PRESSURE 
C **  GRADIENT FORCINGS
C  
C **  THIS SUBROUTINE IS PART OF EFDC_DS 
C  
C----------------------------------------------------------------------C  
C  
C CHANGE RECORD  
C DATE MODIFIED     BY                 DATE APPROVED    BY  
C
      USE GLOBAL

      INTEGER,SAVE::LASTCOR

      REAL,   SAVE::BEGRELAX, ENDRELAX

      ! *** Timed Correction Pass
      RELAX=0.005
      RATIO=0.5
      IF(N.EQ.1)THEN
        BEGRELAX = TIMEDAY+(1.-RATIO)*RELAX
        ENDRELAX = BEGRELAX+RATIO*RELAX
        LASTCOR=0
      ENDIF

      IF(TIMEDAY.GE.BEGRELAX.AND.TIMEDAY.LE.ENDRELAX)THEN
        DO L=2,LA
          FPGXE(L)=0.
          FPGYE(L)=0.
        ENDDO
        LASTCOR=1
      ELSE
        IF(LASTCOR.EQ.1)THEN
          BEGRELAX = BEGRELAX+RELAX
          ENDRELAX = BEGRELAX+RATIO*RELAX
        ENDIF
        LASTCOR=0
      ENDIF
        
      RETURN
      END