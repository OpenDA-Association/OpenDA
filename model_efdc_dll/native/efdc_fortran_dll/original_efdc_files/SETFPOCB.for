C
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE SETFPOCB(ITYPE)
C
C **  THIS SUBROUTINE IS PART OF  EFDC-FULL VERSION 1.0a 
C
C **  LAST MODIFIED BY JOHN HAMRICK ON 1 NOVEMBER 2001
C
C----------------------------------------------------------------------C
C
C CHANGE RECORD
C DATE MODIFIED     BY                 DATE APPROVED    BY
C----------------------------------------------------------------------C
C
C**********************************************************************C
C
C **  SUBROUTINE SETFPOC SETS FPOC BASED ON SEDIMENT BED COMPOSITION
C **  FUNCTIONAL RELATIONSHIPS FOR HOUSATONIC RIVER PROVIDED BY
C **  PROVIDED BY HYDROQUAL DOCUMENT DATED MARCH 17, 2003
C **  NOTE THAT PER CENTS IN PROVIDED RELATIONS ARE CONVERTED
C **  TO FRACTIONS CONSISTENT WITH EFDC VARIABLES
C
C**********************************************************************C
C
      USE GLOBAL

      REAL TMPVAL, TMPVAL1, TMPVAL2
C###########################################################################
C HQI Change to include spatially varying, but time constant bulk foc and 
C pseudo-foc 
      REAL PREDFOCB
C RM, 02/29/04
C###########################################################################

C
C
C**********************************************************************C
C
C   NOTE LIMIT VALUES TO LESS THAN 100 %
C
      IF(ITYPE.EQ.0) THEN
C
C     INITIALIZE ALL FULL BED LAYERS
C
C     COHESIVE SEDIMENT (CLASS 1)
C
        DO K=1,KB
          DO L=2,LA
            IF(K.LE.KBT(L))THEN
              IF(VFRBED(L,K,1).GT.0.0)THEN
C#######################################################################
C     09/18/03, RM, HQI, relationship for cohesives corrected and
C     regression based foc limited to 100%
C     02/29/04, RM, HQI, log-space to arithmetic space conversion
c             STFPOCB(L,K,1)=0.8657/((100.*VFRBED(L,K,1))**1.3326)
                STFPOCB(L,K,1)=0.0384*((100.*VFRBED(L,K,1))**0.0366)
                STFPOCB(L,K,1)=LOG(STFPOCB(L,K,1))
                STFPOCB(L,K,1)=STFPOCB(L,K,1) + ((0.5955**2)/2)
                STFPOCB(L,K,1)=EXP(STFPOCB(L,K,1))
                IF(STFPOCB(L,K,1).GT.1.) STFPOCB(L,K,1)=1.0
C#######################################################################
              ELSE
                STFPOCB(L,K,1)=0.0
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C     COARSER NONCOHESIVE SEDIMENT (CLASS 2)
C#######################################################################
C     09/18/03, RM, HQI, this is Non-Cohesive 1
C#######################################################################

        DO K=1,KB
          DO L=2,LA
            IF(K.LE.KBT(L))THEN
              IF(VFRBED(L,K,2).GT.0.0)THEN
C#######################################################################
C     09/18/03, RM, HQI, Change to account for NC4 and
C     regression based foc limited to 100%
C     02/29/04, RM, HQI, log-space to arithmetic space conversion
                TMPVAL2=( (100.*VFRBED(L,K,2))**0.9189)
C             TMPVAL=VFRBED(L,K,3)/(VFRBED(L,K,1)+VFRBED(L,K,2))
                TMPVAL=VFRBED(L,K,1)/
     +             (VFRBED(L,K,2)+VFRBED(L,K,3)+VFRBED(L,K,4))
                TMPVAL1=TMPVAL**0.9204
                STFPOCB(L,K,2)=0.7428*TMPVAL1/TMPVAL2
                STFPOCB(L,K,2)=LOG(STFPOCB(L,K,2))
                STFPOCB(L,K,2)=STFPOCB(L,K,2) + ((1.1371**2)/2)
                STFPOCB(L,K,2)=EXP(STFPOCB(L,K,2))
                IF(STFPOCB(L,K,2).GT.1.) STFPOCB(L,K,2)=1.0
C#######################################################################
              ELSE
                STFPOCB(L,K,2)=0.0
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C     FINER NONCOHESIVE SEDIMENT (CLASS 2)
C#######################################################################
C     09/18/03, RM, HQI, this is Non-Cohesive 2
C#######################################################################

        DO K=1,KB
          DO L=2,LA
            IF(K.LE.KBT(L))THEN
              IF((VFRBED(L,K,3)+VFRBED(L,K,4)).GT.0.0)THEN
C#######################################################################
C     09/18/03, RM, HQI, regression corrected, account for NC4 and
C     regression based foc limited to 100%
C     02/29/04, RM, HQI, log-space to arithmetic space conversion
c             STFPOCB(L,K,3)=0.033*((100.*VFRBED(L,K,3))**0.152)
                STFPOCB(L,K,3)=0.4084/
     +             ((100.*(VFRBED(L,K,3)+VFRBED(L,K,4)))**1.116)
                STFPOCB(L,K,3)=LOG(STFPOCB(L,K,3))
                STFPOCB(L,K,3)=STFPOCB(L,K,3) + ((1.4436**2)/2)
                STFPOCB(L,K,3)=EXP(STFPOCB(L,K,3))
                IF(STFPOCB(L,K,3).GT.1.) STFPOCB(L,K,3)=1.0
                STFPOCB(L,K,4)=STFPOCB(L,K,3)
C#######################################################################
              ELSE
                STFPOCB(L,K,3)=0.0
                STFPOCB(L,K,4)=0.0
              ENDIF
            ENDIF
          ENDDO
        ENDDO
C
C###########################################################################
C HQI Change to include spatially varying, but time constant bulk foc and 
C pseudo-foc
        DO K=1,KB
          DO L=2,LA
            IF(K.LE.KBT(L))THEN
              PREDFOCB = (STFPOCB(L,K,1)*VFRBED(L,K,1)) +
     +                (STFPOCB(L,K,2)*VFRBED(L,K,2)) +
     +                (STFPOCB(L,K,3)*VFRBED(L,K,3)) +
     +                (STFPOCB(L,K,4)*VFRBED(L,K,4))
	        IF(PREDFOCB.GT.0.0)THEN
                STFPOCB(L,K,1) = STFPOCB(L,K,1)*PFPOCB(L,K)/PREDFOCB
                STFPOCB(L,K,2) = STFPOCB(L,K,2)*PFPOCB(L,K)/PREDFOCB
                STFPOCB(L,K,3) = STFPOCB(L,K,3)*PFPOCB(L,K)/PREDFOCB
                STFPOCB(L,K,4) = STFPOCB(L,K,4)*PFPOCB(L,K)/PREDFOCB
	        ENDIF
              IF(STFPOCB(L,K,1).GT.1.) STFPOCB(L,K,1)=1.0
              IF(STFPOCB(L,K,2).GT.1.) STFPOCB(L,K,2)=1.0
              IF(STFPOCB(L,K,3).GT.1.) STFPOCB(L,K,3)=1.0
              IF(STFPOCB(L,K,4).GT.1.) STFPOCB(L,K,4)=1.0
            ENDIF
          ENDDO
        ENDDO
C RM, 02/29/04
C###########################################################################
      ENDIF
C
CC**********************************************************************C
C
      IF(ITYPE.EQ.1) THEN
C
C     UPDATE TOP LAYER OF BED
C
C     COHESIVE SEDIMENT (CLASS 1)
C
        DO L=2,LA
          K=KBT(L)
          IF(VFRBED(L,K,1).GT.0.0)THEN
C#######################################################################
C     09/18/03, RM, HQI, relationship for cohesives corrected and
C     regression based foc limited to 100%
C     02/29/04, RM, HQI, log-space to arithmetic space conversion
c           STFPOCB(L,K,1)=0.8657/((100.*VFRBED(L,K,1))**1.3326)
            STFPOCB(L,K,1)=0.0384*((100.*VFRBED(L,K,1))**0.0366)
            STFPOCB(L,K,1)=LOG(STFPOCB(L,K,1))
            STFPOCB(L,K,1)=STFPOCB(L,K,1) + ((0.5955**2)/2)
            STFPOCB(L,K,1)=EXP(STFPOCB(L,K,1))             
            IF(STFPOCB(L,K,1).GT.1.) STFPOCB(L,K,1)=1.0
C#######################################################################
          ELSE
            STFPOCB(L,K,1)=0.0
          ENDIF
        ENDDO
C
C     COARSER NONCOHESIVE SEDIMENT (CLASS 2)
C#######################################################################
C     09/18/03, RM, HQI, this is Non-Cohesive 1
C#######################################################################

        DO L=2,LA
          K=KBT(L)
          IF(VFRBED(L,K,2).GT.0.0) THEN
C#######################################################################
C     09/18/03, RM, HQI, Change to account for NC4 and
C     regression based foc limited to 100%
C     02/29/04, RM, HQI, log-space to arithmetic space conversion
C           TMPVAL=VFRBED(L,K,3)/(VFRBED(L,K,1)+VFRBED(L,K,2))
            TMPVAL2=( (100.*VFRBED(L,K,2))**0.9189)
            TMPVAL=VFRBED(L,K,1)/
     +             (VFRBED(L,K,2)+VFRBED(L,K,3)+VFRBED(L,K,4))
            TMPVAL1=TMPVAL**0.9204
            STFPOCB(L,K,2)=0.7428*TMPVAL1/TMPVAL2
            STFPOCB(L,K,2)=LOG(STFPOCB(L,K,2))
            STFPOCB(L,K,2)=STFPOCB(L,K,2) + ((1.1371**2)/2)
            STFPOCB(L,K,2)=EXP(STFPOCB(L,K,2))
            IF(STFPOCB(L,K,2).GT.1.) STFPOCB(L,K,2)=1.0
C#######################################################################
          ELSE
            STFPOCB(L,K,2)=0.0
          ENDIF
        ENDDO
C
C     FINER NONCOHESIVE SEDIMENT (CLASS 2)
C#######################################################################
C     09/18/03, RM, HQI, this is Non-Cohesive 2
C#######################################################################

        DO L=2,LA
          K=KBT(L)
          IF(VFRBED(L,K,3).GT.0.0)THEN
C#######################################################################
C     09/18/03, RM, HQI, regression corrected, account for NC4 and
C     regression based foc limited to 100%
C     02/29/04, RM, HQI, log-space to arithmetic space conversion
c           STFPOCB(L,K,3)=0.033*((100.*VFRBED(L,K,3))**0.152)
            STFPOCB(L,K,3)=0.4084/
     +             ((100.*(VFRBED(L,K,3)+VFRBED(L,K,4)))**1.116)
            STFPOCB(L,K,3)=LOG(STFPOCB(L,K,3))
            STFPOCB(L,K,3)=STFPOCB(L,K,3) + ((1.4436**2)/2)
            STFPOCB(L,K,3)=EXP(STFPOCB(L,K,3))
            IF(STFPOCB(L,K,3).GT.1.) STFPOCB(L,K,3)=1.0
            STFPOCB(L,K,4)=STFPOCB(L,K,3)
C#######################################################################
          ELSE
            STFPOCB(L,K,3)=0.0
            STFPOCB(L,K,4)=0.0
          ENDIF
        ENDDO
C###########################################################################
C HQI Change to include spatially varying, but time constant bulk foc and 
C pseudo-foc
        DO L=2,LA
          K=KBT(L)
          PREDFOCB = (STFPOCB(L,K,1)*VFRBED(L,K,1)) +
     +          (STFPOCB(L,K,2)*VFRBED(L,K,2)) +
     +          (STFPOCB(L,K,3)*VFRBED(L,K,3)) +
     +          (STFPOCB(L,K,4)*VFRBED(L,K,4))
	    IF(PREDFOCB.GT.0.0)THEN
            STFPOCB(L,K,1) = STFPOCB(L,K,1)*PFPOCB(L,K)/PREDFOCB
            STFPOCB(L,K,2) = STFPOCB(L,K,2)*PFPOCB(L,K)/PREDFOCB
            STFPOCB(L,K,3) = STFPOCB(L,K,3)*PFPOCB(L,K)/PREDFOCB
            STFPOCB(L,K,4) = STFPOCB(L,K,4)*PFPOCB(L,K)/PREDFOCB
	    ENDIF
          IF(STFPOCB(L,K,1).GT.1.) STFPOCB(L,K,1)=1.0
          IF(STFPOCB(L,K,2).GT.1.) STFPOCB(L,K,2)=1.0
          IF(STFPOCB(L,K,3).GT.1.) STFPOCB(L,K,3)=1.0
          IF(STFPOCB(L,K,4).GT.1.) STFPOCB(L,K,4)=1.0
        ENDDO
C RM, 02/29/04
C###########################################################################
C
      ENDIF
C
C**********************************************************************C
C
      RETURN
      END
