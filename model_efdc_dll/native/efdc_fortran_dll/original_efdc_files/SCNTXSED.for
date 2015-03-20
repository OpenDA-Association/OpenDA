      SUBROUTINE SCNTXSED
      
      USE GLOBAL
      CHARACTER*80 SKIP
      CHARACTER*10 INFILE

      ! *** NOW FIND MAX FOR TOXICS AND SEDIMENTS
      DO N=1,3
        NCSERNC=0
        IF(N.EQ.1)THEN
          NC=5  ! MSVTOX(1)
          IF(NTOX.GT.0.AND.NTOXSER.GT.0)THEN
            WRITE(*,'(A)')'SCANNING INPUT FILE: TXSER.INP'
            INFILE='TXSER.INP'
            OPEN(1,FILE='TXSER.INP',STATUS='UNKNOWN')
            NLOOP=NTOX
            NCSERNC=NTOXSER  !NCSER(NC)
          ENDIF
        ELSEIF(N.EQ.2)THEN
          NC=NTOX+1  ! MSVSED(1)
          IF(NSED.GT.0.AND.NSEDSER.GT.0)THEN
            WRITE(*,'(A)')'SCANNING INPUT FILE: SDSER.INP'
            INFILE='SDSER.INP'
            OPEN(1,FILE='SDSER.INP',STATUS='UNKNOWN')
            NLOOP=NSED
            NCSERNC=NSEDSER  !NCSER(NC)
          ENDIF
        ELSEIF(N.EQ.3)THEN
          NC=NTOX+NSED+1  ! MSVSND(1)
          IF(NSND.GT.0.AND.NSNDSER.GT.0)THEN
            WRITE(*,'(A)')'SCANNING INPUT FILE: SNSER.INP'
            INFILE='SNSER.INP'
            OPEN(1,FILE='SNSER.INP',STATUS='UNKNOWN')
            NLOOP=NSND
            NCSERNC=NSNDSER  !NCSER(NC)
          ENDIF
        ENDIF

        IF(NCSERNC.GT.0)THEN
          ! *** SKIP HEADER
          DO IS=1,15
            READ(1,60)SKIP
          ENDDO
          ! *** LOOP OVER THE NUMBER OF SERIES
          DO NS=1,NCSERNC
            READ(1,*,ERR=20,END=40)ISTYP,NDATAPTS   !,X1,X2,X3,X4

            ! *** SKIP THE CONVERSIONS
            IF(N.NE.1)THEN
              DO NT=2,NLOOP
                READ(1,60)SKIP
              ENDDO
            ENDIF
            
            NDCSER=MAX(NDCSER,NDATAPTS)
            IF(ISTYP.EQ.1)THEN
              ! *** SKIP THE SPLITS
              READ(1,60)SKIP
            ENDIF
            ! *** SKIP TO THE NEXT SERIES
            DO M=1,NDATAPTS
              DO NT=1,NLOOP
                READ(1,60)SKIP
              ENDDO
            ENDDO
          ENDDO
          CLOSE(1)
        ENDIF
      ENDDO
      RETURN

   20 WRITE(*,30)INFILE
      WRITE(8,30)INFILE
   30 FORMAT(' READ ERROR IN FILE: ',A10)
      STOP
   40 WRITE(*,50)INFILE
      WRITE(8,50)INFILE
   50 FORMAT(' UNEXPECTED END OF FILE: ',A10)
   60 FORMAT(A80)
      STOP

      END