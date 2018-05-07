C
C
C
      SUBROUTINE   SCCLAL
C
C     + + + PURPOSE + + +
C     homes the cursor and clears the screen
C     *** ASCII STANDARD- TESTED ON VT100 ***
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I7
      CHARACTER*1 HOMCLS(7)
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    SCPRST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I7/7/
C
C     + + + END SPECIFICATIONS + + +
C
C     ESC
      HOMCLS(1)= CHAR(27)
C     [
      HOMCLS(2)= CHAR(91)
C     semi
      HOMCLS(3)= CHAR(59)
C     H
      HOMCLS(4)= CHAR(72)
      HOMCLS(5)= HOMCLS(1)
      HOMCLS(6)= HOMCLS(2)
C     J
      HOMCLS(7)= CHAR(74)
      CALL SCPRST (I7,HOMCLS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCLR
C
C     + + + PURPOSE + + +
C     clears the screen
C     *** ASCII STANDARD- TESTED ON VT100 ***
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I3
      CHARACTER*1 CLS(3)
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    SCPRST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I3/3/
C
C     + + + END SPECIFICATIONS + + +
C
C     ESC
      CLS(1)= CHAR(27)
C     [
      CLS(2)= CHAR(91)
C     J
      CLS(3)= CHAR(74)
C
      CALL SCPRST (I3,CLS)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCUMV
     I                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves cursor to the absolute row and column specified
C     *** ASCII STANDARD- TESTED ON VT100 ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - row to move cursor to
C     CCOL   - column to move cursor to
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ILEN,JUST,LEN
      CHARACTER*1 BUFF(8)
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    INTCHR, SCPRST
C
C     + + + END SPECIFICATIONS + + +
C
      ILEN= 2
      JUST= 1
C
C     ESC
      BUFF(1)= CHAR(27)
C     [
      BUFF(2)= CHAR(91)
C     b(3,4) crow
      CALL INTCHR(CROW,ILEN,JUST,LEN,BUFF(3))
      IF (LEN.LT.2) THEN
        BUFF(4)= BUFF(3)
        BUFF(3)= '0'
      END IF
C     semi
      BUFF(5)= CHAR(59)
C     b(6,7) ccol
      CALL INTCHR(CCOL,ILEN,JUST,LEN,BUFF(6))
      IF (LEN.LT.2) THEN
        BUFF(7)= BUFF(6)
        BUFF(6)= '0'
      END IF
C     H
      BUFF(8)= CHAR(72)
C
      LEN= 8
      CALL SCPRST (LEN,BUFF)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCURM
     I                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves the cursor to the relative row and column specified
C     *** ASCII STANDARD- TESTED ON VT100 ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - number of rows to move
C     CCOL   - number of columns to move
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ILEN,JUST,LEN,POS,TEMP
      CHARACTER*1 BUFF(10)
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS, CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   INTCHR, SCPRST
C
C     + + + END SPECIFICATIONS + + +
C
      ILEN= 2
      JUST= 1
      POS = 0
C
C     ESC
      BUFF(1)= CHAR(27)
C     [
      BUFF(2)= CHAR(91)
C
      IF (CROW.NE.0) THEN
C       move rows
        TEMP = ABS ( CROW )
        CALL INTCHR(TEMP,ILEN,JUST,LEN,BUFF(3))
        IF (LEN.LT.2) THEN
          BUFF(4)= BUFF(3)
          BUFF(3)= '0'
        END IF
        IF (CROW.LT.0) THEN
C         cursor moves up crow A
          BUFF(5)= CHAR(65)
        ELSE IF (CROW.GT.0) THEN
C         cursor moves down crow B
          BUFF(5)= CHAR(66)
        END IF
C
        POS= POS+ 5
      END IF
C
      IF (CCOL.NE.0) THEN
C       move columns
        BUFF(POS+1)= BUFF(1)
        BUFF(POS+2)= BUFF(2)
        TEMP = ABS ( CCOL )
        CALL INTCHR(TEMP,ILEN,JUST,LEN,BUFF(POS+3))
        IF (LEN.LT.2) THEN
          BUFF(POS+4)= BUFF(POS+3)
          BUFF(POS+3)= '0'
        END IF
        IF (CCOL.LT.0) THEN
C         cursor moves back ccol D
          BUFF(POS+5)= CHAR(68)
        ELSE IF (CCOL.GT.0) THEN
C         cursor moves forward ccol C
          BUFF(POS+5)= CHAR(67)
        END IF
        POS= POS+ 5
      END IF
      IF (CROW.NE.0.OR.CCOL.NE.0) THEN
        CALL SCPRST (POS,BUFF)
      END IF
C
      RETURN
      END
