      SUBROUTINE   WTBCDI
     I                   ( NROWS, NINDEX, INDEX, VAL,
     O                     TBRBUF )
C
C     + + + PURPOSE + + +
C     Place column of integer data into table buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NROWS, NINDEX, INDEX
      INTEGER   VAL(NROWS)
      REAL      TBRBUF(NINDEX,NROWS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROWS  - number of rows to be added to table buffer
C     NINDEX - number of actual columns in table buffer
C     INDEX  - actual column being added to table buffer
C     VAL    - column of integer values to be added to table buffer
C     TBRBUF - table buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ROW
      REAL        TEMPX
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TEMPX,TEMP)
      INTEGER     TEMP
C
C     + + + END SPECIFICATIONS + + +
C
      ROW = 0
 100  CONTINUE
C       move value for this row into buffer
        ROW = ROW + 1
        TEMP = VAL(ROW)
        TBRBUF(INDEX,ROW) = TEMPX
      IF (ROW .LT. NROWS) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBCDR
     I                   ( NROWS, NINDEX, INDEX, VAL,
     O                     TBRBUF )
C
C     + + + PURPOSE + + +
C     Place column of real data into table buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NROWS, NINDEX, INDEX
      REAL      VAL(NROWS)
      REAL      TBRBUF(NINDEX,NROWS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROWS  - number of rows to be added to table buffer
C     NINDEX - number of actual columns in table buffer
C     INDEX  - actual column being added to table buffer
C     VAL    - column of real values to be added to table buffer
C     TBRBUF - table buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ROW
C
C     + + + END SPECIFICATIONS + + +
C
      ROW = 0
 100  CONTINUE
C       move value for this row into buffer
        ROW = ROW + 1
        TBRBUF(INDEX,ROW) = VAL(ROW)
      IF (ROW .LT. NROWS) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBCDD
     I                   ( NROWS, NINDEX, INDEX, VAL,
     O                     TBRBUF )
C
C     + + + PURPOSE + + +
C     Place column of double precision data into table buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER           NROWS, NINDEX, INDEX
      REAL              TBRBUF(NINDEX,NROWS)
      DOUBLE PRECISION  VAL(NROWS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROWS  - number of rows to be added to table buffer
C     NINDEX - number of actual columns in table buffer
C     INDEX  - actual column being added to table buffer
C     VAL    - column of double precision values to be added to table buffer
C     TBRBUF - table buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ROW, INDX2
      REAL        TEMPX(2)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TEMPX,TEMP)
      DOUBLE PRECISION  TEMP
C
C     + + + END SPECIFICATIONS + + +
C
      ROW = 0
      INDX2 = INDEX + 1
 100  CONTINUE
C       move value for this row into buffer
        ROW = ROW + 1
        TEMP = VAL(ROW)
        TBRBUF(INDEX,ROW) = TEMPX(1)
        TBRBUF(INDX2,ROW) = TEMPX(2)
      IF (ROW .LT. NROWS) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBCDC
     I                   ( NROWS, NINDEX, INDEX, LEN, VAL,
     O                     TBRBUF )
C
C     + + + PURPOSE + + +
C     Place column of character data into table buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NROWS, NINDEX, INDEX, LEN
      REAL        TBRBUF(NINDEX,NROWS)
      CHARACTER*1 VAL(LEN,NROWS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NROWS  - number of rows to be added to table buffer
C     NINDEX - number of actual columns in table buffer
C     INDEX  - actual columns being added to table buffer
C     VAL    - columns of character values to be added to table buffer
C     TBRBUF - table buffer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ROW, LENW, INDX, L
      REAL         TEMPX(20)
      CHARACTER*80 TEMPC
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (TEMPX,TEMP)
      INTEGER     TEMP(20)
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 20A4 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( 80A1 )
C
C     + + + END SPECIFICATIONS + + +
C
C     blank out temporary character string
      TEMPC = ' '
C     determine number of 4-character strings
      LENW = (LEN + 3) / 4
C     process each row
      DO 250 ROW = 1, NROWS
C       convert 1-character array to an array string
        WRITE (TEMPC(1:LEN),2000) (VAL(L,ROW), L = 1, LEN)
C       read back into an integer array
        READ (TEMPC,1000) (TEMP(L), L = 1, LENW)
C       move into table buffer
        INDX = INDEX
        DO 220 L = 1, LENW
C         real equivalent of integer is moved into table buffer
          TBRBUF(INDX,ROW) = TEMPX(L)
          INDX = INDX + 1
 220    CONTINUE
 250  CONTINUE
C
      RETURN
      END
