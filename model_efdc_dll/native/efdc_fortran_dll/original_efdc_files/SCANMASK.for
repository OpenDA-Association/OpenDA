      SUBROUTINE SCANMASK
      USE GLOBAL
      USE MPI

      IF(MYRANK.EQ.0)WRITE(*,'(A)')'SCANNING INPUT FILE: MASK.INP'

      OPEN(1,FILE='MASK.INP',STATUS='UNKNOWN')
      ! *** FINE MAXIMUM NUMBER OF MASK TYPE OVER 5
      DO L=1,6
        READ(1,*)
      ENDDO
      NGTMSKE=0
      NGTMSKF=0
      READ(1,*,IOSTAT=ISO) MMASK
      DO M=1,MMASK
        READ(1,*,IOSTAT=ISO) I,J,MTYPE
        IF(ISO.GT.0)GOTO 20  
        IF (MTYPE.EQ.5 .OR. MTYPE.EQ.6) NGTMSKE=NGTMSKE+1
        IF (MTYPE.EQ.7 .OR. MTYPE.EQ.8) NGTMSKF=NGTMSKF+1
      ENDDO

      CLOSE(1)

      RETURN

C  10 FORMAT(A80)   
   20 CONTINUE
      IF(MYRANK.EQ.0) WRITE(*,30)'MASK.INP'
      IF(MYRANK.EQ.0) WRITE(8,30)'MASK.INP'
   30 FORMAT(' READ ERROR IN FILE: GATECTL.INP ')
      STOP

      END
