C     utsort.f 2.1 9/4/91
C
C
C
      SUBROUTINE   ASRTC
     I                   (OPT,LEN,CNT,
     M                    CVAL,
     O                    POS)
C
C     + + + PURPOSE + + +
C     sorts character strings
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OPT,LEN,CNT,POS(CNT)
      CHARACTER*1 CVAL(LEN,CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPT    - sort option
C              0 - sort in place
C              1 - move values in array to sorted position
C     LEN    - length of string to sort
C     CNT    - count of string to sort
C     CVAL   - array of character strings to sort
C     POS    - array containing sorted order of strings
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,JPT,JPT1,K,DONE
      CHARACTER*1 CDUM(64)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR
C
C     + + + END SPECIFICATIONS + + +
C
C     set default positions(assume in order)
      DO 10 I= 1, CNT
        POS(I)= I
 10   CONTINUE
C
C     make a pointer to alpha list of names with bubble sort
      DO 40 I= CNT,2,-1
        DO 30 J= 1,I-1
          JPT = POS(J)
          JPT1= POS(J+1)
          K   = 0
          DONE= 0
 20       CONTINUE
            K= K+ 1
            IF (CVAL(K,JPT).GT.CVAL(K,JPT1)) THEN
              POS(J+1)= JPT
              POS(J)  = JPT1
              DONE= 1
            ELSE IF (CVAL(K,JPT).LT.CVAL(K,JPT1)) THEN
C             dont exchange
              DONE= 1
            END IF
          IF (K.LT.6.AND.DONE.EQ.0) GO TO 20
 30     CONTINUE
 40   CONTINUE
C
      IF (OPT.EQ.1) THEN
C       move character values to their sorted positions
        DO 60 I= 1,CNT
          IF (POS(I).NE.I) THEN
C           need to move characters, first save whats in target space
            CALL CHRCHR (LEN,CVAL(1,I),CDUM)
C           move sorted data to target position
            CALL CHRCHR (LEN,CVAL(1,POS(I)),CVAL(1,I))
C           move temp data to source position
            CALL CHRCHR (LEN,CDUM,CVAL(1,POS(I)))
C
C           find the pointer to the other value we are moving
            J= I
50          CONTINUE
              J= J+ 1
            IF (POS(J).NE.I) GO TO 50
            POS(J)= POS(I)
            POS(I)= I
          END IF
60      CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTI
     I                   (OPT,CNT,
     M                    IVAL,
     O                    POS)
C
C     + + + PURPOSE + + +
C     sorts integers
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     OPT,CNT,POS(CNT)
      INTEGER     IVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPT    - sort option
C              0 - sort in place
C              1 - move values in array to sorted position
C     CNT    - count of integers to sort
C     IVAL   - array of integers to sort
C     POS    - array containing sorted order of integers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,JPT,JPT1,ITMP
C
C     + + + END SPECIFICATIONS + + +
C
C     set default positions(assume in order)
      DO 10 I= 1, CNT
        POS(I)= I
 10   CONTINUE
C
C     make a pointer to values with bubble sort
      DO 40 I= CNT,2,-1
        DO 30 J= 1,I-1
          JPT = POS(J)
          JPT1= POS(J+1)
          IF (IVAL(JPT).GT.IVAL(JPT1)) THEN
            POS(J+1)= JPT
            POS(J)  = JPT1
          END IF
 30     CONTINUE
 40   CONTINUE
C
      IF (OPT.EQ.1) THEN
C       move integer values to their sorted positions
        DO 60 I= 1,CNT
          IF (POS(I).NE.I) THEN
C           need to move characters, first save whats in target space
            ITMP= IVAL(I)
C           move sorted data to target position
            IVAL(I)= IVAL(POS(I))
C           move temp data to source position
            IVAL(POS(I))= ITMP
C           find the pointer to the other value we are moving
            J= I
50          CONTINUE
              J= J+ 1
            IF (POS(J).NE.I) GO TO 50
            POS(J)= POS(I)
            POS(I)= I
          END IF
60      CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTIP
     I                    (CNT,
     M                     IVAL)
C
C     + + + PURPOSE + + +
C     sorts integers in their array
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNT
      INTEGER     IVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNT    - count of integers to sort
C     IVAL   - array of integers to sort
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,JPT,JPT1,ITMP
C
C     + + + END SPECIFICATIONS + + +
C
C     move integer values to their sorted positions
      DO 20 I= CNT,2,-1
        DO 10 JPT= 1,I-1
          JPT1= JPT+ 1
          IF (IVAL(JPT).GT.IVAL(JPT1)) THEN
            ITMP      = IVAL(JPT)
            IVAL(JPT) = IVAL(JPT1)
            IVAL(JPT1)= ITMP
          END IF
10      CONTINUE
20    CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTR
     I                   (OPT,CNT,
     M                    RVAL,
     O                    POS)
C
C     + + + PURPOSE + + +
C     sorts decimal numbers
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPT,CNT,POS(CNT)
      REAL      RVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPT    - sort option
C              0 - sort in place
C              1 - move values in array to sorted position
C     CNT    - count of decimal numbers to sort
C     RVAL   - array of decimal numbers to sort
C     POS    - array containing sorted order of decimal numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,JPT,JPT1
      REAL        RTMP
C
C     + + + END SPECIFICATIONS + + +
C
C     set default positions(assume in order)
      DO 10 I= 1, CNT
        POS(I)= I
 10   CONTINUE
C
C     make a pointer to values with bubble sort
      DO 40 I= CNT,2,-1
        DO 30 J= 1,I-1
          JPT = POS(J)
          JPT1= POS(J+1)
          IF (RVAL(JPT).GT.RVAL(JPT1)) THEN
            POS(J+1)= JPT
            POS(J)  = JPT1
          END IF
 30     CONTINUE
 40   CONTINUE
C
      IF (OPT.EQ.1) THEN
C       move decimal values to their sorted positions
        DO 60 I= 1,CNT
          IF (POS(I).NE.I) THEN
C           need to move, first save whats in target space
            RTMP= RVAL(I)
C           move sorted data to target position
            RVAL(I)= RVAL(POS(I))
C           move temp data to source position
            RVAL(POS(I))= RTMP
C           find the pointer to the other value we are moving
            J= I
50          CONTINUE
              J= J+ 1
            IF (POS(J).NE.I) GO TO 50
            POS(J)= POS(I)
            POS(I)= I
          END IF
60      CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTRP
     I                    (CNT,
     M                     RVAL)
C
C     + + + PURPOSE + + +
C     sorts decimal numbers in their array
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     CNT
      REAL        RVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNT    - count of decimal numbers to sort
C     RVAL   - array of decimal numbers to sort
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,JPT,JPT1
      REAL        RTMP
C
C     + + + END SPECIFICATIONS + + +
C
C     move decimal numbers to their sorted positions
      DO 20 I= CNT,2,-1
        DO 10 JPT= 1,I-1
          JPT1= JPT+ 1
          IF (RVAL(JPT).GT.RVAL(JPT1)) THEN
            RTMP      = RVAL(JPT)
            RVAL(JPT) = RVAL(JPT1)
            RVAL(JPT1)= RTMP
          END IF
10      CONTINUE
20    CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTD
     I                   (OPT,CNT,
     M                    DVAL,
     O                    POS)
C
C     + + + PURPOSE + + +
C     sorts double precision numbers
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          OPT,CNT,POS(CNT)
      DOUBLE PRECISION DVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPT    - sort option
C              0 - sort in place
C              1 - move values in array to sorted position
C              2 - sort in place, quick move of zero to
C                  first position
C     CNT    - count of dprec numbers to sort
C     DMIN   - minimum value to sort
C     DMAX   - maximum value to sort
C     DVAL   - array of dprec numbers to sort
C     POS    - array containing sorted order of dprec numbers
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          I,J,JPT,JPT1,IZCNT,IZFND
      DOUBLE PRECISION DTMP
C
C     + + + END SPECIFICATIONS + + +
C
C     set default positions(assume in order)
      DO 10 I= 1, CNT
        POS(I)= I
 10   CONTINUE
C
      IZCNT= 0
      IZFND= 0
C
      IF (OPT .EQ. 2) THEN
C       quick move of zero to first position
        I= 0
 12     CONTINUE
          I  = I+ 1
          JPT= POS(I)
          IF (DVAL(JPT) .GT. 0.0) THEN
C           look for a zero to move
            IF (IZFND .LE. I) THEN
C             have never looked later than here
              J= I+1
            ELSE
C             already looked past here, dont look again
              J= IZFND+ 1
            END IF
 15         CONTINUE
              IF (DVAL(J) .LE. 0.0) THEN
C               switch
                POS(I)= J
                POS(J)= JPT
                IZCNT = I
                IZFND = J
C               kick out
                J     = CNT
              ELSE
C               still looking
                J= J+ 1
              END IF
            IF (J .LT. CNT) GO TO 15
          ELSE
C           zero here
            IZCNT= I
          END IF
        IF (IZFND .LT. CNT) GO TO 12
        WRITE(99,*) 'zero count in sort:',IZCNT
      ELSE
C       no quick move of zero to first position
        IZCNT= 0
      END IF
C
C     make a pointer to values with bubble sort
      DO 40 I= CNT,IZCNT+2,-1
        DO 30 J= IZCNT+1,I-1
          JPT = POS(J)
          JPT1= POS(J+1)
          IF (DVAL(JPT).GT.DVAL(JPT1)) THEN
            POS(J+1)= JPT
            POS(J)  = JPT1
          END IF
 30     CONTINUE
 40   CONTINUE
C
      IF (OPT.EQ.1) THEN
C       move integer values to their sorted positions
        DO 60 I= 1,CNT
          IF (POS(I).NE.I) THEN
C           need to move characters, first save whats in target space
            DTMP= DVAL(I)
C           move sorted data to target position
            DVAL(I)= DVAL(POS(I))
C           move temp data to source position
            DVAL(POS(I))= DTMP
C           find the pointer to the other value we are moving
            J= I
50          CONTINUE
              J= J+ 1
            IF (POS(J).NE.I) GO TO 50
            POS(J)= POS(I)
            POS(I)= I
          END IF
60      CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ASRTDP
     I                    (CNT,
     M                     DVAL)
C
C     + + + PURPOSE + + +
C     sorts double precision numbers in their array
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          CNT
      DOUBLE PRECISION DVAL(CNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CNT    - count of decimal numbers to sort
C     DVAL   - array of dprec numbers to sort
C
C     + + + LOCAL VARIABLES + + +
      INTEGER          I,JPT,JPT1
      DOUBLE PRECISION DTMP
C
C     + + + END SPECIFICATIONS + + +
C
C     move dprec numbers to their sorted positions
      DO 20 I= CNT,2,-1
        DO 10 JPT= 1,I-1
          JPT1= JPT+ 1
          IF (DVAL(JPT).GT.DVAL(JPT1)) THEN
            DTMP      = DVAL(JPT)
            DVAL(JPT) = DVAL(JPT1)
            DVAL(JPT1)= DTMP
          END IF
10      CONTINUE
20    CONTINUE
C
      RETURN
      END
