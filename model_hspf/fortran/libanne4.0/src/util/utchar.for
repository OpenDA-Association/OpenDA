C     utchar.f 2.1 9/4/91
C
C
C
      SUBROUTINE   CARVAR
     I                    (LENA, CARY, LENV,
     O                     CVAR)
C
C     + + + PURPOSE + + +
C     Convert a character*1 array of size LENA
C     to a character variable of length LEN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       LENA, LENV
      CHARACTER*1   CARY(LENA)
      CHARACTER*(*) CVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LENA   - size of input character*1 array
C     CARY   - input character array of size LENA
C     LENV   - length of output character variable
C     CVAR   - output character variable of length LENV
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L, L1
      CHARACTER*1 BLNK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK / ' ' /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LENA .GE. LENV) THEN
C       copy input array to output string, truncate if necessary
        DO 100 L = 1, LENV
          CVAR(L:L) = CARY(L)
 100    CONTINUE
      ELSE
C       copy input array to output string, pad with blanks as needed
        DO 200 L = 1, LENA
          CVAR(L:L) = CARY(L)
 200    CONTINUE
        L1 = LENA + 1
        DO 220 L = L1, LENV
          CVAR(L:L) = BLNK
 220    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHDECE
     I                   (LEN,STR,
     O                    RVAL,ERRFLG)
C
C     + + + PURPOSE + + +
C     Convert a character array to its real equivalent.  The
C     real is expected to be right justified in STR.  For an
C     invalid real, ERRFLG is set to 1.  Valid characters are
C     '0' - '9', '+', '-', '.', 'D', and 'E'.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,ERRFLG
      REAL        RVAL
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     STR    - character array containing real value
C     RVAL   - real value
C     ERRFLG - flag indicating success of conversion
C              0 - successful
C              1 - unsuccessful
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL       ERRREA
C
C     + + + FUNCTIONS + + +
      REAL       CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRDEC, NUMINI
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init maching dependent numeric constants (const.inc)
      CALL NUMINI
C
      ERRREA = -R0MAX
      RVAL  = CHRDEC (LEN,STR)
      ERRFLG= 0
      IF (RVAL.LE.ERRREA) THEN
C       ivalid real
        ERRFLG= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHINTE
     I                   (LEN,STR,
     O                    IVAL,ERRFLG)
C
C     + + + PURPOSE + + +
C     Convert a character array to its integer equivalent.
C     The integer is expected to be right justified in STR.
C     For an invalid integer, ERRFLG is set to 1.
C     Valid characters are '0' - '9', '+', and '-'.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,ERRFLG,IVAL
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     STR    - character array containing an integer value
C     IVAL   - integer value
C     ERRFLG - flag indicating success of conversion
C              0 - successful
C              1 - unsuccessful
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DIG,I,SIGN
      CHARACTER*1 CDIG
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRDIG
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRDIG
C
C     + + + DATA INITIALIZATIONS + + +
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL  = 0
      ERRFLG= 0
      SIGN  = 1
      I = 1
  10  CONTINUE
        CDIG = STR(I)
        IF ((CDIG.EQ.'-' .OR. CDIG.EQ.'+' .OR. CDIG.EQ.' ') .AND.
     1       IVAL.EQ.0) THEN
C         replace blank, - sign, or + sign with a 0 if no number yet
          IF (CDIG.EQ.'-') THEN
C           change sign
            SIGN = -1
          END IF
          CDIG = '0'
        END IF
        DIG = CHRDIG(CDIG)
        IVAL= IVAL* 10+ DIG
        I = I + 1
      IF (I.LE.LEN .AND. DIG.GE.0) GO TO 10
C
      IF (DIG.EQ.-1) THEN
C       invalid integer
        ERRFLG= 1
      END IF
C
      IVAL= IVAL* SIGN
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CHKSTR
     I                           (LEN,NSTR,STR1,STR2)
C
C     Search thru STR2 for a match to the character array STR1.
C     Return the array location of the match, or a zero if there
C     is no match.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,NSTR
      CHARACTER*1 STR1(LEN),STR2(LEN,NSTR)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     NSTR   - number of strings to be compared
C     STR1   - character array of size LEN to search for
C     STR2   - character array of size LEN,NSTR
C              to be searched for a match
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,MAT
C
C     + + + END SPECIFICATIONS + + +
C
      CHKSTR = 0
      J = 1
C
  10  CONTINUE
        I = 1
        MAT = 1
  20    CONTINUE
          IF (STR1(I).NE.STR2(I,J)) MAT = 0
          I = I + 1
        IF (I.LE.LEN.AND.MAT.EQ.1) GO TO 20
        IF (MAT.EQ.1) CHKSTR = J
        J = J + 1
      IF (J.LE.NSTR.AND.CHKSTR.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHRCHR
     I                    (LEN,STR1,
     O                     STR2)
C
C     + + + PURPOSE + + +
C     Copy LEN characters from array STR1 to array STR2.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR1(LEN),STR2(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character arrays
C     STR1   - input character array
C     STR2   - output character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,LEN
        STR2(I) = STR1(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   CHRDEC
     I                         (LEN,STR)
C
C     + + + PURPOSE + + +
C     Convert a character array to its real equivalent.  Return
C     a -R0MAX for an invalid array.  The real is expected to
C     be right justified in the array.  Leading blanks are ignored.
C     Trailing blanks are treated as a zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     STR    - character array containing a real value
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     CHKORD(6,6),ERFLG,EXPFLG,KIND,OKIND,POS,FRCFLG
      REAL        DECSGN,EXPSGN,EXPON,FRC,SUM,TEN,TVAL,ERRREA
      CHARACTER*1 DIG
C
C     + + + FUNCTIONS + + +
      REAL      RNOP
      INTEGER   CHRDIG
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRDIG, NUMINI
C
C     + + + DATA INITIALIZATIONS + + +
C
C                   DOWN OKIND, ACROSS KIND
      DATA CHKORD /
     1              1, 2, 0, 4, 0, 6,
     2              1, 2, 0, 4, 0, 6,
     3              1, 2, 3, 0, 0, 6,
     4              0, 0, 3, 4, 0, 6,
     5              1, 2, 0, 0, 0, 6,
     6              0, 0, 0, 0, 0, 0/
C
C     + + + STATEMENT FUNCTION DEFINITIONS + + +
C     No OPperation, used to keep Ryan/McFarland optimization honest
      RNOP(ERRREA) = ERRREA
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init maching dependent numeric constants (const.inc)
      CALL NUMINI
C
      ERRREA = -R0MAX
      ERFLG = 0
      EXPFLG = 0
      FRCFLG = 0
      OKIND = 6
      POS = 1
      EXPSGN = 1.0
      DECSGN = 1.0
      TEN = 10.
      FRC = 1.0
      SUM = 0.0
      EXPON = 0.0
C
  10  CONTINUE
        DIG = STR(POS)
        KIND = 0
        IF (DIG.EQ.'+') KIND = 1
        IF (DIG.EQ.'-') KIND = 2
        IF (DIG.EQ.'.') KIND = 3
        IF (DIG .EQ. 'D' .OR. DIG .EQ. 'd' .OR.
     1      DIG .EQ. 'E' .OR. DIG .EQ. 'e') KIND = 4
C       treat a blank as a zero
        IF (DIG.EQ.' ') THEN
          IF (OKIND.EQ.6) THEN
C           leading blank
            KIND = 6
          ELSE
C           not a leading blank - assume zero
            KIND = 5
            TVAL = 0
          END IF
        END IF
C
        IF (KIND.EQ.0) THEN
          TVAL = CHRDIG(DIG)
          IF (TVAL.GE.0.0) KIND = 5
        END IF
C
        IF (KIND.EQ.0) THEN
          ERFLG = 5
        ELSE
          ERFLG = CHKORD(KIND,OKIND)
        END IF
C
 40     CONTINUE
        IF (ERFLG.NE.0) THEN
C         this is a quick exit
          CHRDEC = ERRREA
          RETURN
        END IF
C
        GO TO (110,120,130,140,150,160),KIND
C
 110    CONTINUE
C         process a plus sign
          GO TO 200
C
 120    CONTINUE
C         process a minus sign
          IF (EXPFLG.EQ.0) THEN
            DECSGN = -1.0
          ELSE
            EXPSGN = -1.0
          END IF
          GO TO 200
C
 130    CONTINUE
C         process a decimal point
          IF (EXPFLG.EQ.1) THEN
            ERFLG = 3
            GO TO 40
          END IF
C
C         no fractional exponents allowed
          IF (FRCFLG.EQ.1) THEN
            ERFLG = 3
            GO TO 40
          END IF
C
C         no double decimal point
          FRCFLG = 1
          GO TO 200
C
 140    CONTINUE
C         process an exponent (E, e, D, or d)
          IF (EXPFLG.EQ.1) THEN
            ERFLG = 4
            GO TO 40
          END IF
C
C         no double exponent
          EXPFLG = 1
          GO TO 200
C
 150    CONTINUE
C         process a number
          IF (EXPFLG.EQ.0) THEN
            IF (FRCFLG.EQ.0) THEN
              SUM = (SUM* TEN) + TVAL
            ELSE
              FRC = FRC/ TEN
              FRC = RNOP(FRC)
              SUM = SUM + (FRC* TVAL)
            END IF
          ELSE
            EXPON = (EXPON* TEN) + TVAL
          END IF
          GO TO 200
C
 160    CONTINUE
C         process a leading blank
          GO TO 200
C
 200    CONTINUE
C       repeat until finished
        OKIND = KIND
        POS = POS + 1
      IF (POS.LE.LEN) GO TO 10
      CHRDEC = DECSGN* SUM* TEN** (EXPSGN* EXPON)
C
      RETURN
      END
C
C
C
      DOUBLE PRECISION   FUNCTION   CHRDPR
     I                                    (LEN,STR)
C
C     + + + PURPOSE + + +
C     Convert a character array to its double precision equivalent.
C     Return a -D0MAX for an invalid array.  The double precision
C     is expected to be right justified in the array.  Leading blanks
C     are ignored.  Trailing blanks are treated as a zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     STR    - character array containing a real value
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      REAL*8      DECSGN,EXPSGN,EXPON,FRC,SUM,TEN,TVAL
      INTEGER     CHKORD(6,6),ERFLG,EXPFLG,KIND,OKIND,POS,FRCFLG
      CHARACTER*1 DIG
      DOUBLE PRECISION  ERRREA
C
C     + + + FUNCTIONS + + +
      INTEGER   CHRDIG
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRDIG, NUMINI
C
C                   DOWN OKIND, ACROSS KIND
      DATA CHKORD /
     1              1, 2, 0, 4, 0, 6,
     2              1, 2, 0, 4, 0, 6,
     3              1, 2, 3, 0, 0, 6,
     4              0, 0, 3, 4, 0, 6,
     5              1, 2, 0, 0, 0, 6,
     6              0, 0, 0, 0, 0, 0/
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init maching dependent numeric constants (const.inc)
      CALL NUMINI
C
      ERRREA = -D0MAX
      ERFLG = 0
      EXPFLG = 0
      FRCFLG = 0
      OKIND = 6
      POS = 1
      EXPSGN = 1.0
      DECSGN = 1.0
      TEN = 10.
      FRC = 1.0
      SUM = 0.0
      EXPON = 0.0
C
  10  CONTINUE
        DIG = STR(POS)
        KIND = 0
        IF (DIG.EQ.'+') KIND = 1
        IF (DIG.EQ.'-') KIND = 2
        IF (DIG.EQ.'.') KIND = 3
        IF (DIG .EQ. 'D' .OR. DIG .EQ. 'd' .OR.
     1      DIG .EQ. 'E' .OR. DIG .EQ. 'e') KIND = 4
C       treat a blank as a zero
        IF (DIG.EQ.' ') THEN
          IF (OKIND.EQ.6) THEN
C           leading blank
            KIND = 6
          ELSE
C           not a leading blank - assume zero
            KIND = 5
            TVAL = 0
          END IF
        END IF
C
        IF (KIND.EQ.0) THEN
          TVAL = CHRDIG(DIG)
          IF (TVAL.GE.0.0) KIND = 5
        END IF
C
        IF (KIND.EQ.0) THEN
          ERFLG = 5
        ELSE
          ERFLG = CHKORD(KIND,OKIND)
        END IF
C
 40     CONTINUE
        IF (ERFLG.NE.0) THEN
C         this is a quick exit
          CHRDPR = ERRREA
          RETURN
        END IF
C
        GO TO (110,120,130,140,150,160),KIND
C
 110    CONTINUE
C         process a plus sign
          GO TO 200
C
 120    CONTINUE
C         process a minus sign
          IF (EXPFLG.EQ.0) THEN
            DECSGN = -1.0
          ELSE
            EXPSGN = -1.0
          END IF
          GO TO 200
C
 130    CONTINUE
C         process a decimal point
          IF (EXPFLG.EQ.1) THEN
            ERFLG = 3
            GO TO 40
          END IF
C
C         no fractional exponents allowed
          IF (FRCFLG.EQ.1) THEN
            ERFLG = 3
            GO TO 40
          END IF
C
C         no double decimal point
          FRCFLG = 1
          GO TO 200
C
 140    CONTINUE
C         process an exponent (E, e, D, or d)
          IF (EXPFLG.EQ.1) THEN
            ERFLG = 4
            GO TO 40
          END IF
C
C         no double exponent
          EXPFLG = 1
          GO TO 200
C
 150    CONTINUE
C         process a number
          IF (EXPFLG.EQ.0) THEN
            IF (FRCFLG.EQ.0) THEN
              SUM = (SUM* TEN) + TVAL
            ELSE
              FRC = FRC/ TEN
              SUM = SUM + (FRC* TVAL)
            END IF
          ELSE
            EXPON = (EXPON* TEN) + TVAL
          END IF
          GO TO 200
C
 160    CONTINUE
C         process a leading blank
          GO TO 200
C
 200    CONTINUE
C       repeat until finished
        OKIND = KIND
        POS = POS + 1
      IF (POS.LE.LEN) GO TO 10
      CHRDPR = DECSGN* SUM* TEN** (EXPSGN* EXPON)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHRDEL
     I                    (LEN,POS,
     M                     STRING)
C
C     + + + PURPOSE + + +
C     Delete the character in array position POS and shift
C     the rest of the array left one position.  The last
C     character in the array is set to blank.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,POS
      CHARACTER*1 STRING(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     POS    - array position of the character to be deleted from STRING
C     STRING - character array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (POS.LT.LEN) THEN
C       no problem with array dimensions
        DO 10 I = POS,LEN-1
          STRING(I) = STRING(I + 1)
 10     CONTINUE
        STRING(LEN) = ' '
      ELSE IF (POS.EQ.LEN) THEN
C       just delete last character
        STRING(LEN) = ' '
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CHRDIG
     I                           (CHR)
C
C     Convert a single character to an integer. Valid characters
C     are '0' - '9'.  Return -1 for an invalid character.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1 CHR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CHR    - a single character
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*1 TDIG(10)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TDIG  /'0','1','2','3','4','5','6','7','8','9'/
C
C     + + + END SPECIFICATIONS + + +
C
      CHRDIG = -1
      I = 1
  10  CONTINUE
        IF (CHR.EQ.TDIG(I)) CHRDIG = I- 1
        I = I + 1
      IF (CHRDIG.EQ.-1.AND.I.LE.10) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHRINS
     I                    (LEN,COL,CHAR,
     M                     STRING)
C
C     + + + PURPOSE + + +
C     Insert the character CHAR into position COL in the
C     array STRING.  First shift the characters in
C     positions COL thru LEN-1 one position to the right.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,COL
      CHARACTER*1 CHAR,STRING(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - output length of character string
C     COL    - position in STRING that CHAR is to be inserted
C     CHAR   - character to be inserted in STRING
C     STRING - character string
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      IF (COL.EQ.LEN) THEN
C       just fill last position
        STRING(LEN) = CHAR
      ELSE IF (COL.EQ.1 .AND. LEN.EQ.2) THEN
C       trap weird dimension case
        STRING(2) = STRING(1)
        STRING(1) = CHAR
      ELSE
C       all ok
        DO 10 I = COL,LEN-1
          J = LEN + COL- I- 1
          STRING(J+1) = STRING(J)
 10     CONTINUE
        STRING(COL) = CHAR
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CHRINT
     I                           (LEN,STR)
C
C     + + + PURPOSE + + +
C     Convert a character array to its integer equivalent.  Return
C     a 0 for an invalid array.  The integer is expected to be
C     right justified in the array.  Leading blanks are ignored.
C     Trailing blanks are treated as a zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     STR    - character array containing an integer value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ERRINT
C
C     + + + FUNCTIONS + + +
      INTEGER     CRINTE
C
C     + + + EXTERNALS + + +
      EXTERNAL   CRINTE
C
C     + + + END SPECIFICATIONS + + +
C
      ERRINT= 0
      CHRINT= CRINTE(ERRINT,LEN,STR)
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CKNBLK
     I                           (LEN,CBUF)
C
C     + + + PURPOSE + + +
C     Check character array CBUF for all blanks.  Return
C     a zero if all blanks, otherwise return a one.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 CBUF(LEN)
C
C     + + + ARGUMENT DEFINITION + + +
C     LEN    - size of character array
C     CBUF   - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      J = 0
      DO 10 I = 1,LEN
        IF (CBUF(I) .NE. ' ') J = 1
 10   CONTINUE
C
      CKNBLK = J
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CRINTE
     I                           (ERRINT,LEN,STR)
C
C     + + + PURPOSE + + +
C     Convert a character array to its integer equivalent.
C     The integer is expected to be right justified in STR.
C     For an invalid integer, return ERRINT.  Valid characters
C     are '0' - '9', '+', '-', '.', 'D', and 'E'.  Leading
C     blanks are ignored.  Trailing blanks are treated as 0.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4     ERRINT,LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ERRINT - value to be returned fo an invalid array
C     LEN    - size of character array
C     STR    - character array containing an integer value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DIG,I,SIGN,ITMP
      CHARACTER*1 CDIG
      LOGICAL     NEARMAX
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRDIG
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRDIG
C
C     + + + END SPECIFICATIONS + + +
C
      ITMP= 0
      SIGN= 1
      I   = 0
      NEARMAX = .FALSE.
  10  CONTINUE
        I   = I + 1
        CDIG= STR(I)
C       replace a blank with a zero
        IF ((CDIG.EQ.'-' .OR. CDIG.EQ.'+' .OR. CDIG.EQ.' ') .AND.
     1       ITMP.EQ.0) THEN
C         replace blank, - sign, or + sign with a 0 if no number yet
          IF (CDIG.EQ.'-') THEN
C           change sign
            SIGN= -1
          END IF
          CDIG = '0'
        END IF
        DIG = CHRDIG(CDIG)
        IF (NEARMAX) THEN
          IF ((DIG .GT. 8) .OR. (DIG .EQ. 8 .AND. SIGN .EQ. 1)) THEN
C            the last digit cannot be greater than 8, and it can only
C            be 8 if the number is negative.
             I = I - 1
             GO TO 20
          END IF
        END IF
        ITMP= ITMP* 10+ DIG
        IF (I.LT.LEN .AND. DIG.GE.0) THEN
          IF (ITMP .EQ. 214748364) THEN
C            right at the border, an additional digit may be possible
             NEARMAX = .TRUE.
          ELSE IF (ITMP .GT. 214748364) THEN
C            no room for another digit, no matter what it is
             GO TO 20
          END IF
          GO TO 10
        END IF
C
 20   CONTINUE
C
      IF (DIG.EQ.-1 .OR. I.LT.LEN) THEN
C       bad conversion
        CRINTE = ERRINT
      ELSE
C       ok conversion
        CRINTE = ITMP* SIGN
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CRINTX
     I                           (LEN,STR)
C
C     + + + PURPOSE + + +
C     Convert a character array to its integer equivalent.
C     The integer is expected to be right justified in STR.
C     For an invalid integer, -999 is returned.  Valid characters
C     are '0' - '9', '+', and '-' .  Leading blanks are ignored.
C     Trailing blanks are treated as 0.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     STR    - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ERRINT
C
C     + + + FUNCTIONS + + +
      INTEGER     CRINTE
C
C     + + + EXTERNALS + + +
      EXTERNAL   CRINTE
C
C     + + + END SPECIFICATIONS + + +
C
      ERRINT= -999
      CRINTX= CRINTE(ERRINT,LEN,STR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CTRSTR
     I                    (LEN,
     M                     TITLE)
C
C     + + + PURPOSE + + +
C     Center the characters within the array TITLE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 TITLE(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array, 1 <= LEN <= 132
C     TITLE  - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L,IE,IS,I,J,K,B,TLEN
      CHARACTER*1 BUFF(132),BLNK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      TLEN = LEN
      IF (TLEN.GT.132) TLEN = 132
      I = 0
 10   CONTINUE
        I = I + 1
      IF (TITLE(I).EQ.BLNK.AND.I.LT.TLEN) GO TO 10
      IS = I
C     found beginning of non-blanks
C
      I = TLEN + 1
 20   CONTINUE
        I = I - 1
      IF (TITLE(I).EQ.BLNK.AND.I.GT.1) GO TO 20
      IE = I
C     found end of non-blanks
C
      IF (IE.GE.IS) THEN
C       title not all blanks
        L = IE - (IS-1)
        B = (TLEN-L)/2
        DO 30 I = 1,TLEN
          BUFF(I) = TITLE(I)
          TITLE(I) = BLNK
 30     CONTINUE
        DO 40 I = 1,L
          J = B + I
          K = IS + I - 1
          TITLE(J) = BUFF(K)
 40     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CVARAR
     I                    (LENV, CVAR, LENA,
     O                     CARY)
C
C     + + + PURPOSE + + +
C     Convert a character variable of length LENV to
C     a character array of size LENA.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       LENV, LENA
      CHARACTER*(*) CVAR
      CHARACTER*1   CARY(LENA)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LENV   - length of input character variable
C     CVAR   - input character variable of length LENV
C     LENA   - size of output character array
C     CARY   - output character array of size LENA
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   L, L1
      CHARACTER*1 BLNK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK  / ' ' /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LENV .GE. LENA) THEN
C       copy variable to array, truncate if needed
        DO 100 L = 1, LENA
          CARY(L) = CVAR(L:L)
 100    CONTINUE
      ELSE
C       copy variable to array, padding with blanks
        DO 200 L = 1, LENV
          CARY(L) = CVAR(L:L)
 200    CONTINUE
        L1 = LENV + 1
        DO 220 L = L1, LENA
          CARY(L) = BLNK
 220    CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATCHR
     I                    (YR, MO, DY,
     O                     OLEN, DBUFF)
C
C     + + + PURPOSE + + +
C     Put a date into a character array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     YR, MO, DY, OLEN
      CHARACTER*1 DBUFF(10)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year
C     MO     - month
C     DY     - day
C     OLEN   - actual output size of character array
C     DBUFF  - output character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LEN, LOC, JUST
      CHARACTER*1 SLSH,BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZIPC, INTCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  SLSH,BLNK  / '/',' ' /
C
C     + + + END SPECIFICATIONS + + +
C
      JUST= 1
      LOC = 1
      LEN = 10
      CALL ZIPC (LEN,BLNK,DBUFF)
      LEN = 4
      CALL INTCHR( YR, LEN, JUST, OLEN, DBUFF(LOC) )
      LOC = LOC + OLEN
      IF(MO .GT. 0  .AND.  MO .LT. 13) THEN
        DBUFF(LOC) = SLSH
        LOC = LOC + 1
        LEN = 2
        CALL INTCHR( MO, LEN, JUST, OLEN, DBUFF(LOC) )
        LOC = LOC + OLEN
        IF(DY .GT. 0  .AND. DY .LT. 32) THEN
          DBUFF(LOC) = SLSH
          LOC = LOC + 1
          LEN = 2
          CALL INTCHR( DY, LEN, JUST, OLEN, DBUFF(LOC) )
          LOC = LOC + OLEN
        END IF
      END IF
      OLEN = LOC - 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   DATLST
     I                    (DATE,
     O                     DSTRNG,LEN,ERRCOD)
C
C     + + + PURPOSE + + +
C     Put DATE into the character array DSTRNG in the
C     format 1986 FEB. 14 10:30:00.  If hours, minutes,
C     and seconds are all zero, the format is 1986 FEB. 14.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DATE(6),LEN,ERRCOD
      CHARACTER*1 DSTRNG(21)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date
C     DSTRNG - output character array
C     LEN    - actual number of characters output to character array
C     ERRCOD - flag indicating valid date
C              0 - valid date
C              1 - invalid year
C              2 - invalid month
C              3 - invalid day
C              4 - invalid hour
C              5 - invalid minute
C              6 - invalid second
C              the ERRCOD for the smallest invalid date element is
C              returned, if day and hour are both invalid, a 4 is
C              returned
C
C     + + +   LOCAL VARIABLES   + + +
      INTEGER     JUST,OLEN,LOC,MO,J
      CHARACTER*1 MOCHAR(39),BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER     DAYMON
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRCHR, DAYMON, INTCHR, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA MOCHAR/'J','A','N',    'F','E','B',    'M','A','R',    'A',
     1            'P','R',    'M','A','Y',    'J','U','N',    'J','U',
     2            'L',    'A','U','G',    'S','E','P',    'O','C','T',
     3                'N','O','V',    'D','E','C',    'E','R','R'    /
      DATA BLNK  /' '/
C
C     + + + END SPECIFICATIONS + + +
C
C     check values
      J= 21
      CALL ZIPC (J,BLNK,DSTRNG)
      ERRCOD = 0
C
      IF (DATE(1).LE.0.OR.DATE(1).GT.2100) ERRCOD = 1
      IF (DATE(2).LE.0.OR.DATE(2).GT.12) ERRCOD = 2
C
      IF (ERRCOD.EQ.0) THEN
        IF (DATE(3).LE.0.OR.DATE(3).GT.DAYMON(DATE(1),DATE(2)))
     1                                                 ERRCOD = 3
        IF (DATE(4).LT.0.OR.DATE(4).GT.24) ERRCOD = 4
        IF (DATE(5).LT.0.OR.DATE(5).GT.60) ERRCOD = 5
        IF (DATE(6).LT.0.OR.DATE(6).GT.60) ERRCOD = 6
C
      END IF
      IF (ERRCOD.EQ.0) THEN
C
C       put date in character array
C       add year
        JUST = 1
        LEN = 4
        LOC = 1
        IF (DATE(1).GT.9999.OR.DATE(1).LT.-999) DATE(1) = 9999
        CALL INTCHR (DATE(1),LEN,JUST,OLEN,DSTRNG(LOC))
        LOC = LOC + OLEN + 1
C
C       add month
        LEN = 3
        MO = 3*(DATE(2)-1) + 1
        IF (MO.LT.1.OR.MO.GT.34) MO = 37
        CALL CHRCHR (LEN,MOCHAR(MO),DSTRNG(LOC))
        LOC = LOC + LEN + 1
C
C       add day
        LEN = 2
        IF (DATE(3).GT.99.OR.DATE(3).LT.-9) DATE(3) = 99
        CALL INTCHR (DATE(3),LEN,JUST,OLEN,DSTRNG(LOC))
        LOC = LOC + OLEN + 1
C
C       add hour
        IF (DATE(4)+DATE(5)+DATE(6) .GT. 0) THEN
          LEN = 2
          IF (DATE(4).GT.99.OR.DATE(4).LT.0) DATE(4) = 99
          IF (DATE(4).LT.10) LEN = 1
          IF (LEN.EQ.1) THEN
            DSTRNG(LOC)= '0'
            LOC = LOC + 1
          END IF
          CALL INTCHR (DATE(4),LEN,JUST,OLEN,DSTRNG(LOC))
          LOC = LOC + OLEN
C
C         add colon
          DSTRNG(LOC)= ':'
          LOC = LOC + 1
C
C         add minute
          LEN = 2
          IF (DATE(5).GT.99.OR.DATE(5).LT.0) DATE(5) = 99
          IF (DATE(5).LT.10) LEN = 1
          IF (LEN.EQ.1) THEN
            DSTRNG(LOC)= '0'
            LOC = LOC + 1
          END IF
          CALL INTCHR (DATE(5),LEN,JUST,OLEN,DSTRNG(LOC))
          LOC = LOC + OLEN
C
C         add colon
          DSTRNG(LOC)= ':'
          LOC = LOC + 1
C
C         add second
          LEN = 2
          IF (DATE(6).GT.99.OR.DATE(6).LT.0) DATE(6) = 99
          IF (DATE(6).LT.10) LEN = 1
          IF (LEN.EQ.1) THEN
            DSTRNG(LOC)= '0'
            LOC = LOC + 1
          END IF
          CALL INTCHR (DATE(6),LEN,JUST,OLEN,DSTRNG(LOC))
          LOC = LOC + OLEN + 1
        END IF
        LEN = LOC - 2
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DECCHR
     I                    (REAIN, LENGTH,
     M                     JUST,
     O                     JLEN, STR)
C
C
C     + + + PURPOSE + + +
C     This routine converts a real number to a character string.
C     The number is right justified in the string if the flag
C     JUST is zero or if the number requires an exponent,
C     otherwise it is left justified.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LENGTH, JUST, JLEN
      REAL         REAIN
      CHARACTER*1  STR(LENGTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REAIN  - real value to be converted to a character string
C     LENGTH - available length for output character string
C     JUST   - output justification
C              0 - right justified
C              1 - left justified
C              will be forced to 0 if an exponent is required
C     JLEN   - actual length of output character string
C     STR    - output character string
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C 
C     + + + LOCAL VARIABLES + + + 
      INTEGER      I0, I1, I2, I, J, K, L, M, PREC, EXPO, WHDIGS, 
     $             EXDIGS, FDIGS, NOCNT, KLEN
      REAL         R1, R2, R3, ARTMP, RSUB, MINFIX, MAXFIX, X
      CHARACTER*1  FMTBUF(12), BUF(79),  ZERO,  BLNK,  POINT,  STAR
      CHARACTER*12 FMT
      CHARACTER*79 BUFSTR
      LOGICAL      FLOAT, FRAC, NEG, KILL, NOFIT
C
C     + + + LOCAL DEFINITIONS + + +
C     NOCNT  - for real values less than 1.0, NOCNT is the number of
C              zeros immediately following the decimal point (which
C              "don't count" in terms of the number of significant
C              digits representable by the machine); for real values
C              greater than or equal to 1.0, NOCNT = 0
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (BUF,BUFSTR)
      EQUIVALENCE (FMT,FMTBUF)
C
C     + + + FUNCTIONS + + +
      INTEGER      RWDIGS
      REAL         FPART, RNOP
      LOGICAL      NONZER, NZFRAC
C
C     + + + INTRINSICS + + +
      INTRINSIC    IABS, ABS, ALOG10, REAL, LEN, INDEX, 
     $             INT, AINT, ANINT, NINT
C
C     + + + EXTERNALS + + +
      EXTERNAL     INTCHR, NUMINI, RWDIGS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  I0, I1, I2, ZERO, BLNK, POINT, STAR
     .    /  0,  1,  2,  '0',  ' ',   '.',  '*' /
C
C     + + + STATEMENT FUNCTION DEFINITIONS + + +
C     absolute value of the FRACtional PART (of a real number)
      FPART(X)  = ABS(X) - ABS(AINT(X))
C     NON-ZERo real number? - true or false  N.B. X should not be < 0.0
      NONZER(X) = X .GT. 0.0
C     Non-Zero FRACtional part of a real number? - true or false
      NZFRAC(X) = NONZER(FPART(X))
C     number of Whole DIGitS - for X > 0;
C     double precision needed in the following to ensure correct calc.
C     of log for DG/UX
Cmyg  WDIGS(X)  = IDINT(DLOG10(DBLE(X*RP1MIN))) + 1 
C     to keep Ryan/McFarland optimization honest
      RNOP(X) = X
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init machine-dependent numeric constants (const.inc)
      CALL NUMINI
C
C     save time with the common case of REAIN .eq. 0
      IF (ABS(REAIN) .LE. R0MIN) THEN
         IF (LENGTH .GE. 3) THEN
            BUFSTR = '0.0'
         ELSE IF (LENGTH .EQ. 2) THEN
            BUFSTR = '0.'
         ELSE IF (LENGTH .EQ. 1) THEN
            BUFSTR = '0'
         ELSE
            NOFIT = .TRUE.
         END IF
      ELSE
C
C        the number of decimal digits of precision offered can be found
C        in the variable RPREC of the ICONST common block.
         PREC = RPREC
C
C        is the number negative?  set NEG and define MAXFIX/MINFIX
         IF (REAIN .LT. 0.0) THEN
            NEG = .TRUE.
            MAXFIX = 10.0**(LENGTH-1)
            MINFIX = 1.0/10.0**(LENGTH-1)
         ELSE
            NEG = .FALSE.
            MAXFIX = 10.0**LENGTH
            MINFIX = 1.0/10.0**LENGTH
         END IF
C
C        this copy of the absolute value of the number is used frequently
         ARTMP = ABS(REAIN)
C
C        using scientific notation, how many digits will be in the exponent?
C        the answer will be in EXDIGS.
         R1 = ALOG10(ARTMP*RP1MIN)
C        (RNOP must be used to keep Ryan/McFarland optimization from causing
C        problems.  If RNOP is not used, and RM optimization is turned on,
C        then R1 is left on the stack as an 80-bit number instead of being
C        popped and pushed.)
         R1 = RNOP(R1)
         EXPO = INT(R1)
         IF (R1 .LT. 0.0) THEN
            IF (NZFRAC(R1)) THEN
               EXPO = EXPO - 1
            END IF
         END IF
         IF (EXPO .EQ. 0) THEN
            EXDIGS = 1
         ELSE
            EXDIGS = INT(ALOG10(REAL(IABS(EXPO))*RP1MIN)) + 1
         END IF
C
C        how many digits in the whole part of the number?
         IF (ARTMP .GE. 1.0) THEN
            WHDIGS = RWDIGS(ARTMP)
         ELSE
C           one digit to allow room for 0
            WHDIGS = 1
         END IF
C
         IF (ARTMP.LT.MINFIX .OR. ARTMP.GT.MAXFIX 
     $       .OR. WHDIGS.GT.PREC) THEN
C           use scientific notation
            FLOAT = .FALSE.
C           force right justification with scientific notation
            JUST = 0
         ELSE
C           use floating point notation
            FLOAT  = .TRUE.
C           does the number have a non-zero fractional part?
            FRAC   = NZFRAC(REAIN)
C           here, in the context of a floating point number, EXDIGS is
C           a count of EXtra whole DIGits (not digits in the exponent).
            EXDIGS = 0
            NOCNT  = 0
            IF (FRAC) THEN
C              if the fractional part were rounded, would the number of
C              of whole digits increase?  (EXtra whole DIGitS)
               IF (ANINT(FPART(REAIN)) .GT. 0.0) THEN
C                  IF ((ARTMP+1.0) .GE. 1.0) THEN
                     IF (RWDIGS(ARTMP+1.0) .GT. WHDIGS) EXDIGS = 1
C                  END IF
               END IF
               IF (ARTMP .LT. 1.0) THEN
C                 since there is a non-zero fractional part (FRAC = TRUE),
C                 and the absolute value of the number is less than 1.0,
C                 how many consecutive zeros immediately follow the decimal
C                 point?  The NOCNT (no count) zeros don't count against the
C                 precision available.
                  R1 = ABS(ALOG10(ARTMP*RP1MIN))
                  IF (NZFRAC(R1)) THEN
                     NOCNT = INT (R1)
                  ELSE
                     NOCNT = INT (R1) - 1
                  END IF
               END IF
            END IF
         END IF
C
         IF (FLOAT) THEN
C           using floating point notation
            RSUB = REAIN
C
C           first, assume it's not negative, taking into account the '.':
            FDIGS = LENGTH - (WHDIGS + 1)
            IF (FDIGS-NOCNT .GT. PREC) FDIGS = PREC+NOCNT
C           if it is negative, there will be less room for fractional digits
            IF (NEG) FDIGS = FDIGS - 1
C           if the number of WHole digits is one, it might just be a zero
C           in which case, for purposes of precision, we can consider the
C           number of whole digits to be zero.
            IF (WHDIGS .EQ. 1) THEN
C              if the whole number IS a zero, then maybe it would be better
C              to add an extra digit of precision to the fraction rather than
C              display the zero whole number.  However, don't do this if
C              the precision would be exceeded.
               IF (AINT(ARTMP) .LE. 0.0 .AND. FDIGS.LT.PREC+NOCNT) THEN
                  WHDIGS = 0
C                 we could just add that extra digit of precision to the
C                 fraction, but if it's just zero too, it looks better to
C                 keep the zero to the left of the decimal.
                  IF (FRAC) THEN
C                    r1 gets the number that we had planned to display, but
C                    raised to a power of 10 such that it is a whole number.
                     R1 = ANINT(ARTMP*10.0**FDIGS)
C                    r2 gets a fraction representing digits which we had
C                    not planned to display and r3 is that fraction times 10.
                     R2 = (ARTMP*10.0**FDIGS) - R1
                     R3 = R2*10
C                    now, if 1) the number we were not going to display is,
C                    when rounded, non-zero, or if 2) the number passed in
C                    is negative, but everything we had planned on displaying
C                    was zero, then we add the extra digit of precision.
C                    in the first case, we just end up dropping the zero to
C                    the left of the decimal and adding an extra digit of
C                    precision; in the second case, the extra digit of
C                    precision is added only if the digits that were to be
C                    displayed previously were all zeros and the number
C                    itself is negative, so if the extra digit is zero, it
C                    is displayed to make up for the fact that the negative
C                    sign (for which room has been given) won't be displayed,
C                    but if the extra digit is non-zero, the negative sign
C                    will be displayed and the zero before the decimal will
C                    be lost.
                     IF (NINT(R3).NE.0 .OR. 
     $                  (NEG .AND. R1.LE.1.0E-20)) THEN
                           FDIGS  = FDIGS + 1
                     END IF
                  END IF
C                 if nocnt, the number of zero digits immediately
C                 following the decimal point (which don't count against the
C                 number of digits of precision representable by the machine),
C                 is greater than zero (this can only be the case if the
C                 absolute value of the number is less than one, as here)
C                 then we must ensure that the number of digits displayed
C                 does not exceed the actual machine precision
               END IF
            END IF
C
            IF ((WHDIGS + FDIGS - NOCNT) .GT. PREC) THEN
               FDIGS = PREC - WHDIGS + NOCNT
            END IF
C
            NOFIT = .FALSE.
            FMT = '(F  .  )'
            IF (FDIGS .GT. 0) THEN
C               expensive internal write replaced, but this is what it did
C               WRITE(FMT,96) LENGTH,FDIGS
               CALL INTCHR (LENGTH,I2,I0,KLEN,FMTBUF(3))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(6))
            ELSE IF (FDIGS .EQ. 0) THEN
               RSUB = ANINT (RSUB)
C               expensive internal write replaced, but this is what it did
C               WRITE(FMT,96) LENGTH,FDIGS
               CALL INTCHR (LENGTH,I2,I0,KLEN,FMTBUF(3))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(6))
            ELSE IF (FDIGS .EQ. -1) THEN
               RSUB = ANINT (RSUB)
               FDIGS = 0
C               expensive internal write replaced, but this is what it did
C               WRITE(FMT,96) LENGTH+1,FDIGS
               I = LENGTH + 1
               CALL INTCHR (I,I2,I0,KLEN,FMTBUF(3))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(6))
            ELSE
               NOFIT = .TRUE.
            END IF
C
         ELSE
C           using scientific notation
            RSUB  = REAIN
            NOFIT = .FALSE.
            IF (NEG) THEN
               IF (EXDIGS+5 .LE. LENGTH) THEN
                  FDIGS = LENGTH-(EXDIGS+5)
                  IF (FDIGS+1 .GT. PREC) FDIGS = PREC - 1
               ELSE
                  NOFIT = .TRUE.
               END IF
            ELSE
               IF (EXDIGS+4 .LE. LENGTH) THEN
                  FDIGS = LENGTH-(EXDIGS+4)
                  IF (FDIGS+1 .GT. PREC) FDIGS = PREC - 1
               ELSE
                  NOFIT = .TRUE.
               END IF
            END IF
C            expensive internal write replaced, but this is what it did
C            IF (.NOT. NOFIT) WRITE(FMT,95) LENGTH,FDIGS,EXDIGS
            IF (.NOT. NOFIT) THEN
               FMT = '(1PE  .  E )'
               CALL INTCHR (LENGTH,I2,I0,KLEN,FMTBUF(5))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(8))
               CALL INTCHR (EXDIGS,I1,I0,KLEN,FMTBUF(11))
            END IF
         END IF
C
C        here we either put asterisks or use 'write' to convert the number
         IF (NOFIT) THEN
            JLEN = 0
            DO 400 I = 1,LENGTH
               BUF(I) = STAR
  400       CONTINUE
         ELSE
C           some compilers don't like blanks embedded in the format string
C           as they are now, so first squeeze it together, and then use it
C           to format the given number.
            I = INDEX(FMT,BLNK)
            IF (I .GT. 0) THEN
               J = I
               L = LEN(FMT)
               DO 420 K = I,L
                  IF (FMTBUF(K) .NE. BLNK) THEN
                     FMTBUF(J) = FMTBUF(K)
                     J = J + 1
                  END IF
  420          CONTINUE
               DO 430 K = J,L
                  FMTBUF(K) = BLNK
  430          CONTINUE
            END IF
C
            I = INDEX(FMT,BLNK)
            IF (I .EQ. 0) I = LEN(FMT)
C
            WRITE(BUFSTR,FMT(1:I)) RSUB
C
         END IF
C
C        in either form, at most 1 trailing zero should be left.
C        make J point to the '.' if there is one
         J = INDEX (BUFSTR,POINT)
         IF (FLOAT) THEN
            IF (J .NE. 0) THEN
C              kludge to change " .0" to "0.0" in some cases on some 'chines
               IF ((J-1) .GT. 0) THEN
                  IF (BUF(J-1) .EQ. BLNK) THEN
                     BUF(J-1) = ZERO
                  END IF
               END IF
C              we don't want to truncate any zeros if there's no '.'
C              make K point to the last zero (if there is one)
               I = 0
               K = 0
  510          CONTINUE
               I = INDEX (BUFSTR(K+1:79),ZERO)
               IF (I .NE. 0) THEN
                  K = K + I
                  GO TO 510
               END IF
               KILL = .TRUE.
               IF (K .NE. 79) THEN
C                 only kill zeros that are not significant.  if the last
C                 zero is followed by something other than a blank,
C                 then you can't get rid of it.
                  IF (BUF(K+1) .NE. BLNK) KILL = .FALSE.
               END IF
               IF (K .GT. J+1 .AND. KILL) THEN
C                 only truncate zeros if they're to the right of the decimal
C                 and don't get rid of a zero that is immediately to the
C                 right of the decimal point.
                  DO 520 I = K,J+2,-1
                     IF (BUF(I) .EQ. ZERO) THEN
                        BUF(I) = BLNK
                     ELSE
C                       once a non-zero is found, get out of the loop
                        GO TO 525
                     END IF
  520             CONTINUE
  525             CONTINUE
               END IF
            END IF
         ELSE
C           get rid of trailing zeros in scientific notation
            IF (J .NE. 0) THEN
C              we don't won't to truncate any zeros if there's no '.'
C              make L point to the 'E'
               L = INDEX (BUFSTR,'E')
               IF (L .NE. 0) THEN
C                 make K point to the last digit before the 'E'
                  K = L - 1
                  IF (K .GT. J+1) THEN
C                    only truncate zeros between '.' and 'E', and don't
C                    get rid of the zero (if there is one) immediately to the
C                    right of the decimal.
                     M = J+2
                     DO 620 I = K,J+2,-1
                        IF (BUF(I) .EQ. ZERO) THEN
C                          don't truncate the last trailing zero
                           BUF(I) = BLNK
                        ELSE
                           M = I + 1
                           GO TO 625
                        END IF
  620                CONTINUE
  625                CONTINUE
C                    a non-zero was found, slide the exponent down to
C                    fill in the gap left by the truncated zeros.
                     DO 630 I = M,LENGTH
                        IF (L .LE. LENGTH) THEN
                           BUF(I) = BUF(L)
                        ELSE
                           BUFSTR(I:LENGTH) = BLNK
                           GO TO 635
                        END IF
                        L = L + 1
  630                CONTINUE
  635                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
C
C     copy the final string, blanks and all
      IF (JUST .NE. 0) THEN
C        left justified (i.e., JUST != 0)
         J = 1
         DO 710 I = 1,LENGTH
            IF (BUF(J) .NE. BLNK) GO TO 715
            J = J + 1
  710    CONTINUE
  715    CONTINUE
         JLEN = INDEX (BUFSTR(J:LENGTH),BLNK)
         IF (JLEN .EQ. 0) THEN
            JLEN = LENGTH - J + 1
         ELSE
            JLEN = JLEN - 1
         END IF
         K = 1
         DO 720 I = J,LENGTH
            STR(K) = BUF(I)
            K = K + 1
  720    CONTINUE
         DO 730 I = K,LENGTH
            STR(I) = BLNK
  730    CONTINUE
      ELSE
C        right justified (i.e., JUST == 0)
         J = LENGTH
         DO 810 I = LENGTH,1,-1
            IF (BUF(J) .NE. BLNK) GO TO 815
            J = J - 1
  810    CONTINUE
  815    CONTINUE
         DO 820 I = 1,LENGTH-J
            STR(I) = BLNK
  820    CONTINUE
         K = 1
         DO 830 I = LENGTH-J+1,LENGTH
            STR(I) = BUF(K)
            K = K + 1
  830    CONTINUE
C        with right justified strings, JLEN is always LENGTH
         JLEN = LENGTH
      END IF
C
      RETURN
C
C      expensive internal writes were replaced, but this is what they did
C  95  FORMAT('(1PE',I2,'.',I2,'E',I1,')')
C  96  FORMAT('(F',I2,'.',I2,')')
C
      END
C
C
C
      SUBROUTINE   DPRCHR
     I                    (DPRIN, LENGTH,
     M                     JUST,
     O                     JLEN, STR)
C
C
C     + + + PURPOSE + + +
C     This routine converts a double precision number to a
C     character string.  The number is right justified in the
C     string if the flag JUST is zero or if the number requires
C     an exponent, otherwise it is left justified.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      LENGTH, JUST, JLEN
      DOUBLE PRECISION  DPRIN
      CHARACTER*1  STR(LENGTH)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DPRIN  - double precision value to be converted to a character string
C     LENGTH - available length for output character string
C     JUST   - output justification
C              0 - right justified
C              1 - left justified
C              will be forced to 0 if an exponent is required
C     JLEN   - actual length of output character string
C     STR    - output character string
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I0, I1, I2, I, J, K, L, M, PREC, EXPO, WHDIGS, 
     $             EXDIGS, FDIGS, NOCNT, KLEN
      DOUBLE PRECISION  R1, R2, R3, ARTMP, RSUB, MINFIX, MAXFIX, X
      CHARACTER*1  FMTBUF(12), BUF(79),  ZERO,  BLNK,  POINT,  STAR
      CHARACTER*12 FMT
      CHARACTER*79 BUFSTR
      LOGICAL      FLOAT, FRAC, NEG, KILL, NOFIT
C
C     + + + LOCAL DEFINITIONS + + +
C     NOCNT  - for values less than 1.0, NOCNT is the number of
C              zeros immediately following the decimal point (which
C              "don't count" in terms of the number of significant
C              digits representable by the machine); for values
C              greater than or equal to 1.0, NOCNT = 0
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (BUF,BUFSTR)
      EQUIVALENCE (FMT,FMTBUF)
C
C     + + + FUNCTIONS + + +
      INTEGER      DWDIGS
      DOUBLE PRECISION  FPART
      LOGICAL      NONZER, NZFRAC
C
C     + + + INTRINSICS + + +
      INTRINSIC    IABS, DABS, DLOG10, DBLE, LEN, INDEX, 
     $             IDINT, DINT, DNINT, IDNINT
C
C     + + + EXTERNALS + + +
      EXTERNAL     INTCHR, NUMINI, DWDIGS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  I0, I1, I2,  ZERO, BLNK, POINT, STAR
     .    /  0,  1,  2,   '0',  ' ',   '.',  '*' /
C
C     + + + STATEMENT FUNCTION DEFINITIONS + + +
C     absolute value of the FRACtional PART (of a real number)
      FPART(X)  = DABS(X) - DABS(DINT(X))
C     NON-ZERo real number? - true or false  N.B. X should not be < 0.0
      NONZER(X) = X .GT. 0.0
C     Non-Zero FRACtional part of a real number? - true or false
      NZFRAC(X) = NONZER(FPART(X))
C     number of Whole DIGitS - for X > 0
Cmyg  WDIGS(X)  = IDINT(DLOG10(X*DP1MIN)) + 1
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init machine-dependent numeric constants (const.inc)
      CALL NUMINI
C
C     save time with the common case of DPRIN .eq. 0
      IF (DABS(DPRIN) .LE. D0MIN) THEN
         IF (LENGTH .GE. 3) THEN
            BUFSTR = '0.0'
         ELSE IF (LENGTH .EQ. 2) THEN
            BUFSTR = '0.'
         ELSE IF (LENGTH .EQ. 1) THEN
            BUFSTR = '0'
         ELSE
            NOFIT = .TRUE.
         END IF
      ELSE
C
C        the number of decimal digits of precision offered can be found
C        in the variable DPREC of the ICONST common block.
         PREC = DPREC
C
C        is the number negative?  set NEG and define MAXFIX/MINFIX
         IF (DPRIN .LT. 0.0) THEN
            NEG = .TRUE.
            MAXFIX = DBLE(10.0)**(LENGTH-1)
            MINFIX = DBLE(1.0/10.0)**(LENGTH-1)
         ELSE
            NEG = .FALSE.
            MAXFIX = DBLE(10.0)**LENGTH
            MINFIX = DBLE(1.0/10.0)**LENGTH
         END IF
C
C        this copy of the absolute value of the number is used frequently
         ARTMP = DABS(DPRIN)
C
C        using scientific notation, how many digits will be in the exponent?
C        the answer will be in EXDIGS.
         R1 = DLOG10(ARTMP*DP1MIN)
         EXPO = IDINT(R1)
         IF (R1 .LT. 0.0) THEN
            IF (NZFRAC(R1)) THEN
               EXPO = EXPO - 1
            END IF
         END IF
         IF (EXPO .EQ. 0) THEN
            EXDIGS = 1
         ELSE
            EXDIGS = IDINT(DLOG10(DBLE(IABS(EXPO))*DP1MIN)) + 1
         END IF
C
C        how many digits in the whole part of the number?
         IF (ARTMP .GE. 1.0) THEN
            WHDIGS = DWDIGS(ARTMP)
         ELSE
C           one digit to allow room for 0
            WHDIGS = 1
         END IF
C
         IF (ARTMP.LT.MINFIX .OR. ARTMP.GT.MAXFIX
     .      .OR. WHDIGS.GT.PREC) THEN
C           use scientific notation
            FLOAT = .FALSE.
C           force right justification with scientific notation
            JUST = 0
         ELSE
C           use floating point notation
            FLOAT  = .TRUE.
C           does the number have a non-zero fractional part?
            FRAC   = NZFRAC(DPRIN)
C           here, in the context of a floating point number, EXDIGS is
C           a count of EXtra whole DIGits (not digits in the exponent).
            EXDIGS = 0
            NOCNT  = 0
            IF (FRAC) THEN
C              if the fractional part were rounded, would the number of
C              of whole digits increase?  (EXtra whole DIGitS)
               IF (DNINT(FPART(DPRIN)) .GT. 0.0) THEN
C                  IF ((ARTMP+DBLE(1.0)) .GE. 1.0) THEN
                     IF (DWDIGS(ARTMP+DBLE(1.0)) .GT. WHDIGS) EXDIGS = 1
C                  END IF
               END IF
               IF (ARTMP .LT. 1.0) THEN
C                 since there is a non-zero fractional part (FRAC = TRUE),
C                 and the absolute value of the number is less than 1.0,
C                 how many consecutive zeros immediately follow the decimal
C                 point?  The NOCNT (no count) zeros don't count against the
C                 precision available.
                  R1 = DABS(DLOG10(ARTMP*DP1MIN))
                  IF (NZFRAC(R1)) THEN
                     NOCNT = IDINT (R1)
                  ELSE
                     NOCNT = IDINT (R1) - 1
                  END IF
               END IF
            END IF
         END IF
C
         IF (FLOAT) THEN
C           using floating point notation
            RSUB = DPRIN
C
C           first, assume it's not negative, taking into account the '.':
            FDIGS = LENGTH - (WHDIGS + 1)
            IF (FDIGS-NOCNT .GT. PREC) FDIGS = PREC+NOCNT
C           if it is negative, there will be less room for fractional digits
            IF (NEG) FDIGS = FDIGS - 1
C           if the number of WHole digits is one, it might just be a zero
C           in which case, for purposes of precision, we can consider the
C           number of whole digits to be zero.
            IF (WHDIGS .EQ. 1) THEN
C              if the whole number IS a zero, then maybe it would be better
C              to add an extra digit of precision to the fraction rather than
C              display the zero whole number.  However, don't do this if
C              the precision would be exceeded.
               IF (DINT(ARTMP) .LE. 0.0 .AND. FDIGS.LT.PREC+NOCNT) THEN
                  WHDIGS = 0
C                 we could just add that extra digit of precision to the
C                 fraction, but if it's just zero too, it looks better to
C                 keep the zero to the left of the decimal.
                  IF (FRAC) THEN
C                    r1 gets the number that we had planned to display, but
C                    raised to a power of 10 such that it is a whole number.
                     WRITE(99,*) 'ARTMP =',ARTMP
                     WRITE(99,*) 'LENGTH=',LENGTH
                     WRITE(99,*) 'FDIGS =',FDIGS 
                     WRITE(99,*) 'DBLE(10.0)**FDIGS =',DBLE(10.0)**FDIGS
                     WRITE(99,*) 'ARTMP*DBLE(10.0)**FDIGS =',
     $                            ARTMP*DBLE(10.0)**FDIGS
                     WRITE(99,*) 'DNINT(ARTMP*DBLE(10.0)**FDIGS) =',
     $                            DNINT(ARTMP*DBLE(10.0)**FDIGS)
                     WRITE(99,*) '--'
                     WRITE(99,*) ' '
                     R1 = DNINT(ARTMP*DBLE(10.0)**FDIGS)
C                    r2 gets a fraction representing digits which we had
C                    not planned to display and r3 is that fraction times 10.
                     R2 = (ARTMP*DBLE(10.0)**FDIGS) - R1
                     R3 = R2*10
C                    now, if 1) the number we were not going to display is,
C                    when rounded, non-zero, or if 2) the number passed in
C                    is negative, but everything we had planned on displaying
C                    was zero, then we add the extra digit of precision.
C                    in the first case, we just end up dropping the zero to
C                    the left of the decimal and adding an extra digit of
C                    precision; in the second case, the extra digit of
C                    precision is added only if the digits that were to be
C                    displayed previously were all zeros and the number
C                    itself is negative, so if the extra digit is zero, it
C                    is displayed to make up for the fact that the negative
C                    sign (for which room has been given) won't be displayed,
C                    but if the extra digit is non-zero, the negative sign
C                    will be displayed and the zero before the decimal will
C                    be lost.
                     IF (IDNINT(R3).NE.0 .OR. 
     .                  (NEG .AND. R1.LE.1.0D-20)) THEN
                           FDIGS  = FDIGS + 1
                     END IF
                  END IF
C                 if nocnt, the number of zero digits immediately
C                 following the decimal point (which don't count against the
C                 number of digits of precision representable by the machine),
C                 is greater than zero (this can only be the case if the
C                 absolute value of the number is less than one, as here)
C                 then we must ensure that the number of digits displayed
C                 does not exceed the actual machine precision
               END IF
            END IF
C
            IF ((WHDIGS + FDIGS - NOCNT) .GT. PREC) THEN
               FDIGS = PREC - WHDIGS + NOCNT
            END IF
C
            NOFIT = .FALSE.
            FMT = '(F  .  )'
            IF (FDIGS .GT. 0) THEN
C               expensive internal write replaced, but this is what it did
C               WRITE(FMT,96) LENGTH,FDIGS
               CALL INTCHR (LENGTH,I2,I0,KLEN,FMTBUF(3))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(6))
            ELSE IF (FDIGS .EQ. 0) THEN
               RSUB = DNINT (RSUB)
C               expensive internal write replaced, but this is what it did
C               WRITE(FMT,96) LENGTH,FDIGS
               CALL INTCHR (LENGTH,I2,I0,KLEN,FMTBUF(3))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(6))
            ELSE IF (FDIGS .EQ. -1) THEN
               RSUB = DNINT (RSUB)
               FDIGS = 0
C               expensive internal write replaced, but this is what it did
C               WRITE(FMT,96) LENGTH+1,FDIGS
               I = LENGTH + 1
               CALL INTCHR (I,I2,I0,KLEN,FMTBUF(3))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(6))
            ELSE
               NOFIT = .TRUE.
            END IF
C
         ELSE
C           using scientific notation
            RSUB  = DPRIN
            NOFIT = .FALSE.
            IF (NEG) THEN
               IF (EXDIGS+5 .LE. LENGTH) THEN
                  FDIGS = LENGTH-(EXDIGS+5)
                  IF (FDIGS+1 .GT. PREC) FDIGS = PREC - 1
               ELSE
                  NOFIT = .TRUE.
               END IF
            ELSE
               IF (EXDIGS+4 .LE. LENGTH) THEN
                  FDIGS = LENGTH-(EXDIGS+4)
                  IF (FDIGS+1 .GT. PREC) FDIGS = PREC - 1
               ELSE
                  NOFIT = .TRUE.
               END IF
            END IF
C            expensive internal write replaced, but this is what it did
C            IF (.NOT. NOFIT) WRITE(FMT,95) LENGTH,FDIGS,EXDIGS
            IF (.NOT. NOFIT) THEN
               FMT = '(1PE  .  E )'
               CALL INTCHR (LENGTH,I2,I0,KLEN,FMTBUF(5))
               CALL INTCHR (FDIGS,I2,I0,KLEN,FMTBUF(8))
               CALL INTCHR (EXDIGS,I1,I0,KLEN,FMTBUF(11))
            END IF
         END IF
C
C        here we either put asterisks or use 'write' to convert the number
         IF (NOFIT) THEN
            JLEN = 0
            DO 400 I = 1,LENGTH
               BUF(I) = STAR
  400       CONTINUE
         ELSE
C           some compilers don't like blanks embedded in the format string
C           as they are now, so first squeeze it together, and then use it
C           to format the given number.
            I = INDEX(FMT,BLNK)
            IF (I .GT. 0) THEN
               J = I
               L = LEN(FMT)
               DO 420 K = I,L
                  IF (FMTBUF(K) .NE. BLNK) THEN
                     FMTBUF(J) = FMTBUF(K)
                     J = J + 1
                  END IF
  420          CONTINUE
               DO 430 K = J,L
                  FMTBUF(K) = BLNK
  430          CONTINUE
            END IF
C
            I = INDEX(FMT,BLNK)
            IF (I .EQ. 0) I = LEN(FMT)
C
            WRITE(BUFSTR,FMT(1:I)) RSUB
C
         END IF
C
C        in either form, at most 1 trailing zero should be left.
C        make J point to the '.' if there is one
         J = INDEX (BUFSTR,POINT)
         IF (FLOAT) THEN
            IF (J .NE. 0) THEN
C              kludge to change " .0" to "0.0" in some cases on some 'chines
               IF ((J-1) .GT. 0) THEN
                  IF (BUF(J-1) .EQ. BLNK) THEN
                     BUF(J-1) = ZERO
                  END IF
               END IF
C              we don't want to truncate any zeros if there's no '.'
C              make K point to the last zero (if there is one)
               I = 0
               K = 0
  510          CONTINUE
               I = INDEX (BUFSTR(K+1:79),ZERO)
               IF (I .NE. 0) THEN
                  K = K + I
                  GO TO 510
               END IF
               KILL = .TRUE.
               IF (K .NE. 79) THEN
C                 only kill zeros that are not significant.  if the last
C                 zero is followed by something other than a blank,
C                 then you can't get rid of it.
                  IF (BUF(K+1) .NE. BLNK) KILL = .FALSE.
               END IF
               IF (K .GT. J+1 .AND. KILL) THEN
C                 only truncate zeros if they're to the right of the decimal
C                 and don't get rid of a zero that is immediately to the
C                 right of the decimal point.
                  DO 520 I = K,J+2,-1
                     IF (BUF(I) .EQ. ZERO) THEN
                        BUF(I) = BLNK
                     ELSE
C                       once a non-zero is found, get out of the loop
                        GO TO 525
                     END IF
  520             CONTINUE
  525             CONTINUE
               END IF
            END IF
         ELSE
C           get rid of trailing zeros in scientific notation
            IF (J .NE. 0) THEN
C              we don't won't to truncate any zeros if there's no '.'
C              make L point to the 'E'
               L = INDEX (BUFSTR,'E')
               IF (L .NE. 0) THEN
C                 make K point to the last digit before the 'E'
                  K = L - 1
                  IF (K .GT. J+1) THEN
C                    only truncate zeros between '.' and 'E', and don't
C                    get rid of the zero (if there is one) immediately to the
C                    right of the decimal.
                     M = J+2
                     DO 620 I = K,J+2,-1
                        IF (BUF(I) .EQ. ZERO) THEN
C                          don't truncate the last trailing zero
                           BUF(I) = BLNK
                        ELSE
                           M = I + 1
                           GO TO 625
                        END IF
  620                CONTINUE
  625                CONTINUE
C                    a non-zero was found, slide the exponent down to
C                    fill in the gap left by the truncated zeros.
                     DO 630 I = M,LENGTH
                        IF (L .LE. LENGTH) THEN
                           BUF(I) = BUF(L)
                        ELSE
                           BUFSTR(I:LENGTH) = BLNK
                           GO TO 635
                        END IF
                        L = L + 1
  630                CONTINUE
  635                CONTINUE
                  END IF
               END IF
            END IF
         END IF
      END IF
C
C     copy the final string, blanks and all
      IF (JUST .NE. 0) THEN
C        left justified (i.e., JUST != 0)
         J = 1
         DO 710 I = 1,LENGTH
            IF (BUF(J) .NE. BLNK) GO TO 715
            J = J + 1
  710    CONTINUE
  715    CONTINUE
         JLEN = INDEX (BUFSTR(J:LENGTH),BLNK)
         IF (JLEN .EQ. 0) THEN
            JLEN = LENGTH - J + 1
         ELSE
            JLEN = JLEN - 1
         END IF
         K = 1
         DO 720 I = J,LENGTH
            STR(K) = BUF(I)
            K = K + 1
  720    CONTINUE
         DO 730 I = K,LENGTH
            STR(I) = BLNK
  730    CONTINUE
      ELSE
C        right justified (i.e., JUST == 0)
         J = LENGTH
         DO 810 I = LENGTH,1,-1
            IF (BUF(J) .NE. BLNK) GO TO 815
            J = J - 1
  810    CONTINUE
  815    CONTINUE
         DO 820 I = 1,LENGTH-J
            STR(I) = BLNK
  820    CONTINUE
         K = 1
         DO 830 I = LENGTH-J+1,LENGTH
            STR(I) = BUF(K)
            K = K + 1
  830    CONTINUE
C        with right justified strings, JLEN is always LENGTH
         JLEN = LENGTH
      END IF
C
      RETURN
C
C      expensive internal writes were replaced, but this is what they did
C  95  FORMAT('(1PE',I2,'.',I2,'E',I1,')')
C  96  FORMAT('(F',I2,'.',I2,')')
C
      END
C
C
C
      INTEGER   FUNCTION   RWDIGS
     I                           (RVAL)
C
C     + + + PURPOSE + + +
C     Determines number of whole digits in input argument.  Used
C     for real values greater than zero.
C
C     + + + DUMMY ARGUMENTS + + +
      REAL         RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RVAL   - real value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LOGVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC    INT, ALOG10
C
C     + + + END SPECIFICATIONS + + +
C
      LOGVAL = INT(ALOG10(RVAL))
      IF (.NOT. (RVAL.GE.10.**LOGVAL .AND. RVAL.LT.10.**(LOGVAL+1)))
     $  LOGVAL = LOGVAL + 1
C
      RWDIGS = LOGVAL + 1
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   DWDIGS
     I                           (DVAL)
C
C     + + + PURPOSE + + +
C     Determines number of whole digits in input argument.  Used
C     for double precision values greater than zero.
C
C     + + + DUMMY ARGUMENTS + + +
      DOUBLE PRECISION  DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DVAL   - double precision value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LOGVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC    IDINT, DLOG10
C
C     + + + END SPECIFICATIONS + + +
C
      LOGVAL = IDINT(DLOG10(DVAL))
      IF (.NOT. (DVAL.GE.10.D0**LOGVAL .AND. DVAL.LT.10.D0**(LOGVAL+1)))
     $  LOGVAL = LOGVAL + 1
C
      DWDIGS = LOGVAL + 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   DECCHX
     I                    (REAIN,LEN,SIGDIG,DECPLA,
     O                     STR)
C
C     + + + PURPOSE + + +
C     Right justifies the real number REAIN into the
C     character array STR of size LEN.  The number will have
C     SIGDIG significant digits and DECPLA decimal places.
C     If the number will not fit into STR as specified, it
C     will be output using an exponent.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,SIGDIG,DECPLA
      REAL        REAIN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     REAIN  - real value to be converted to a character array
C     LEN    - available size for output character array
C     SIGDIG - significant digits for output
C     DECPLA - number of decimal places for output
C               0 - no decimal places
C              <0 - force exponential output
C     STR    - output character array
C
C     + + + COMMON BLOCKS + + +
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,K,L,JUST,TSDIG,DPTFG,LDZRO,ITMP,MCRY,FORCE,
     1             LDECPL,EXPFLG,FRSTD,L20,L1
      REAL         RWRIT
      CHARACTER*20 UBBUF
      CHARACTER*9  UFMTL
      CHARACTER*1  UFMT(9),USBUF(20),C1
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (UBBUF,USBUF),(UFMTL,UFMT)
C
C     + + + FUNCTIONS + + +
      INTEGER      CHRDIG
      CHARACTER*1  DIGCHR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   CHRCHR, CHRDIG, DIGCHR, INTCHR, CHRDEL, CHRINS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  L1,L20/1,20/,  C1/'1'/
C
C     + + + END SPECIFICATIONS + + +
C
      TSDIG= SIGDIG
      IF (TSDIG.EQ.0) TSDIG= 5
C
      IF (DECPLA.LT.0) THEN
C       force exponential instead of zero
        FORCE = 1
        LDECPL= -DECPLA
      ELSE
C       dont force exponential
        FORCE = 0
        LDECPL= DECPLA
      END IF
      JUST= 0
C
      UFMT(1)= '('
      UFMT(2)= ' '
      UFMT(3)= ' '
      UFMT(4)= 'F'
      I= 2
      CALL INTCHR (LEN,I,JUST,J,UFMT(5))
      UFMT(7)= '.'
      UFMT(8)= DIGCHR(LDECPL)
      UFMT(9)= ')'
C
      WRITE (UBBUF,UFMTL) REAIN
      IF (CHRDIG(USBUF(1)).LT.0.AND.USBUF(1).NE.' '.AND.
     1    USBUF(1).NE.'.'     .AND.USBUF(1).NE.'-') THEN
C       real number didn't fit in user specified format, try E
        EXPFLG = 1
      ELSE IF (FORCE.EQ.1) THEN
C       check to see if value written is zero
        READ (UBBUF,UFMTL) RWRIT
Chnb is following condition appropriate (for extremely large magnitude numbers
Chnb that might have a large absolute difference?)
        IF ((ABS(RWRIT).LT.1.0E-10).OR.ABS(RWRIT-REAIN).GT.1.E-6)
     1    EXPFLG= 1
      ELSE
C       no exponent needed here
        EXPFLG = 0
      END IF
      IF (EXPFLG.EQ.1) THEN
C       try exponential format
        UFMT(2)= '1'
        UFMT(3)= 'P'
        UFMT(4)= 'E'
        I= LEN- 6
        IF (REAIN.LT.0) I= I- 1
        IF (TSDIG-1.LT.I) THEN
C         don't want too many digits in exp output
          I= TSDIG- 1
        END IF
        UFMT(8)= DIGCHR(I)
        WRITE (UBBUF,UFMTL) REAIN
      ELSE IF (TSDIG.GT.0) THEN
C       it fit, remove insignificant digits, if any
        I= 0
        J= 0
        DPTFG= 0
        LDZRO= 1
        FRSTD= 0
 10     CONTINUE
          I= I+ 1
C         have we passed the decimal point
          IF (USBUF(I).EQ.'.') THEN
            DPTFG= I
          ELSE
C           are we on a digit?
            ITMP= CHRDIG(USBUF(I))
            IF (ITMP.GT.0.OR.(ITMP.EQ.0.AND.LDZRO.EQ.0)) THEN
C             set counter for first 0-9 digit
              IF (FRSTD .EQ. 0) FRSTD= I
              LDZRO= 0
C             J is counter for significant digits
              J= J+ 1
              IF (J.GT.TSDIG) THEN
C               we have enough significant digits
                IF (ITMP.GE.5.AND.J.EQ.TSDIG+1) THEN
C                 we need to carry from the first insig digit
                  K= I- 1
C                 K is next digit back
 20               CONTINUE
                    IF (K.EQ.DPTFG) K= K-1
                    L= CHRDIG(USBUF(K))+ 1
                    IF (L.EQ.10) THEN
                      IF (K .EQ. FRSTD) THEN
C                       need to shift over since 9 rounded to 10
                        CALL CHRINS (L20,K,C1,USBUF)
                        MCRY = 0
                        L = 0
                        IF (USBUF(1) .EQ. ' ') THEN
C                         shift back if 1st char blank
                          CALL CHRDEL (L20, L1, USBUF)
                          FRSTD = FRSTD - 1
                        ELSE
C                         can't shift so reset pointers
                          I = I + 1
                          K = K + 1
                        END IF
                      ELSE
                        MCRY= 1
                        L= 0
                      END IF
                    ELSE
                      MCRY= 0
                    END IF
                    USBUF(K)= DIGCHR(L)
                    K= K- 1
                  IF (MCRY.EQ.1) GO TO 20
                END IF
C               now get rid of insig digits
                IF (DPTFG.EQ.0) THEN
C                 before the decimal point
                  USBUF(I)= '0'
                ELSE
C                 after the decimal point
                  USBUF(I)= ' '
                END IF
              END IF
            END IF
          END IF
        IF (I.LT.LEN) GO TO 10
      END IF
C
C     fill in output buffer
      CALL CHRCHR (LEN,USBUF,STR)
C
      RETURN
      END
C
C
C
      CHARACTER*1 FUNCTION   DIGCHR
     I                             (DIG)
C
C     Convert a single digit to a character.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DIG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIG    - digit to be converted to a character
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
      CHARACTER*1 TDIG(9),TZERO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA TDIG  /'1','2','3','4','5','6','7','8','9'/
      DATA TZERO /'0'/
C
C     + + + END SPECIFICATIONS + + +
C
      I = 1
      DIGCHR = TZERO
  10  CONTINUE
        IF (I.EQ.DIG) DIGCHR = TDIG(I)
        I = I + 1
      IF (I.LT.10.AND.DIGCHR.EQ.TZERO) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   INTCHR
     I                    (INTIN,LENA,JUST,
     O                     JLEN,STRNG)
C
C     + + + PURPOSE + + +
C     Convert the integer number INTIN to a character array.
C     The number will be right justified in the array if the
C     flag JUST is zero, otherwise is will be left justified.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LENA,INTIN,JLEN,JUST
      CHARACTER*1 STRNG(LENA)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     INTIN  - integer value to be converted to a character array
C     LENA   - available size for output character array
C     JUST   - output justification
C              0 - right justified in the field
C              1 - left justified in the field
C     JLEN   - actual number of characters output to character array
C     STRNG  - output character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IM,IST,NUMCHR, LEN
      INTEGER*4   JTMP1,JTMP2,PTEN,CHKSIZ,REMTMP,INTTMP
      CHARACTER*1 BLNK,ZERO,MINUS,FILCHR,STR(9)
C
C     + + + FUNCTIONS + + +
      CHARACTER*1 DIGCHR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ALOG10, INT, MOD, REAL
C
C     + + + EXTERNALS + + +
      EXTERNAL    DIGCHR, ZIPC, COPYC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  MINUS, BLNK, ZERO, FILCHR
     &     /  '-',  ' ',  '0',    '*' /
C
C     + + + END SPECIFICATIONS + + +
C
      INTTMP = INTIN
      LEN  = LENA
      IF (LEN.GT.9) LEN = 9
      CALL ZIPC (LEN,BLNK,STR)
      IF (INTTMP.EQ.0) THEN
C       process zero
        IF (JUST.EQ.0) THEN
          STR(LEN) = ZERO
          JLEN = LEN
        ELSE
          STR(1) = ZERO
          JLEN = 1
        END IF
      ELSE
        IST = 1
        IF (INTTMP.LT.0) THEN
          IST   = 2
          INTTMP= -INTTMP
          STR(1)= MINUS
        END IF
Chnb  guess what, the integral portion of the log base 10 of a number
Chnb  like 9,999,999 is different than that for 9,999,999*1.00001
Chnb  (therefore, the following code won't work!)
        NUMCHR = INT (ALOG10(REAL(INTTMP)*1.00001))
        IF (JUST.EQ.0) THEN
          JLEN = LEN
        ELSE
          JLEN = NUMCHR + IST
          IF (JLEN.GT.LEN) JLEN = LEN
        END IF
C                 **** ADDED BECAUSE OF HP BUG IN SSB #5838 ***
        JTMP1 = 10
        JTMP2 = (JLEN - IST + 1)
C       IF (JTMP2.GT.4) WRITE (*,'("INTCHR,JTMP2:",I8)') JTMP2
        PTEN = JTMP1** JTMP2
        CHKSIZ = INTTMP- PTEN
        IF (CHKSIZ.GE.0) THEN
C         resulting string will be too long- fill with stars
          CALL ZIPC (JLEN,FILCHR,STR)
        ELSE
Chnb  Enough is enough.  I hadn't said anything to this point, but
Chnb  filling the character array with the most significant digit
Chnb  first is just too ridiculous.  Change this to
Chnb  "DO 130 I = JLEN,IST,-1" or something like that.
          DO 130 I = IST, JLEN
            PTEN = PTEN/ 10
            REMTMP = MOD(INTTMP,PTEN)
            IM = (INTTMP-REMTMP)/PTEN
            STR(I) = DIGCHR(IM)
            IM = I- 1
            IF (IM.GT.0) THEN
              IF (STR(I).EQ.ZERO) THEN
                IF (STR(IM).EQ.BLNK.OR.STR(IM).EQ.MINUS) THEN
                  STR(I) = STR(IM)
                  STR(IM)= BLNK
                END IF
              END IF
            ELSE
              IF (STR(1).EQ.ZERO) STR(1) = BLNK
            END IF
            INTTMP = REMTMP
 130      CONTINUE
        END IF
      END IF
C
      CALL ZIPC ( LENA, BLNK, STRNG )
      IF (LEN .EQ. LENA) THEN
C       available field length within max allowed length
        CALL COPYC ( LEN, STR, STRNG )
      ELSE IF (JUST .NE. 0) THEN
C       available field length bigger than max, left justify
        CALL COPYC ( LEN, STR, STRNG )
      ELSE
C       available field length bigger than max, right justify
        I = LENA - LEN + 1
        CALL COPYC (LEN, STR, STRNG(I) )
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   LENSTR
     I                           (LEN,STR)
C
C     Return the actual length of the character array,
C     excluding trailing blanks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     STR    - character array
C
C     + + + LOCAL VARIALBES + + +
      INTEGER     LENT,DONE
C
C     + + + END SPECIFICATIONS + + +
C
      DONE = 0
      LENT = LEN
  10  CONTINUE
        IF (STR(LENT).EQ.' ') THEN
          LENT = LENT- 1
        ELSE
          DONE = 1
        END IF
      IF (LENT.GT.0.AND.DONE.EQ.0) GO TO 10
C
      LENSTR= LENT
C
      RETURN
      END
C
C
C
      SUBROUTINE   LFTSTR
     I                    (LEN,
     M                     TITLE)
C
C     + + + PURPOSE + + +
C     Left justify characters in the array TITLE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 TITLE(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array, 1 <= LEN <= 132
C     TITLE  - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L,IS,I,K,TLEN
      CHARACTER*1 BUFF(132),BLNK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      TLEN = LEN
      IF (TLEN.GT.132) TLEN = 132
      I = 0
 10   CONTINUE
        I = I + 1
      IF (TITLE(I).EQ.BLNK.AND.I.LT.TLEN) GO TO 10
      IF (TITLE(I).EQ.BLNK.AND.I.EQ.TLEN) I= I+ 1
      IS = I
C     found beginning of non-blanks
C
      IF (IS.LE.TLEN .AND. IS.NE.1) THEN
C       title not all blanks and not currently left justified
        L = TLEN - (IS-1)
        DO 30 I = 1,TLEN
          BUFF(I) = TITLE(I)
          TITLE(I)= BLNK
 30     CONTINUE
        DO 40 I = 1,L
          K = IS + I - 1
          TITLE(I) = BUFF(K)
 40     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTLIN
     I                    (DATE,NUMDSN,DATA,WDTH,SGFD,DPLA,THRSH,
     O                     OLEN,TBUFF)
C
C     + + + PURPOSE + + +
C     Convert DATE and an array of real numbers into a character array.
C
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NUMDSN,DATE(6),WDTH,OLEN,SGFD,DPLA
      REAL        DATA(NUMDSN), THRSH(2)
      CHARACTER*1 TBUFF(250)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date
C     NUMDSN - number of data values to output ( 1 <= NUMDSN <= 30 )
C     DATA   - array of NUMDSN data values to be output
C     WDTH   - output width ( <= 250 )
C     SGFD   - significant digits for output
C     DPLA   - number of decimal places for output
C     THRSH  - limits for output
C              (1) - minimum value
C              (2) - maximum value
C     OLEN   - actual number of characters output to TBUFF
C     TBUFF  - output character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,IPOS,ERRFLG,JUST,J,LEN,ID
      CHARACTER*1 BLNK, STAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   DATLST, DECCHR, DECCHX
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/,  STAR/'*'/
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1,WDTH
        TBUFF(I) = BLNK
 10   CONTINUE
      ID = 23
C     ID is position date end and data starts in ID+1
      JUST = 0
      IPOS = 2
      CALL DATLST (DATE,TBUFF(IPOS),OLEN,ERRFLG)
C
      IF (DATE(3).LT.10) THEN
C       shift date string if day  < 10
        DO 12 I = 1,11
          J = ID - I
          TBUFF(J) = TBUFF(J-1)
 12     CONTINUE
        TBUFF(11) = BLNK
      END IF
C
      IPOS = ID
      LEN = (WDTH-22)/NUMDSN
      IF (LEN.GT.12) LEN = 12
      DO 20 I = 1,NUMDSN
        IF (DATA(I).LT.THRSH(1).OR.DATA(I).GT.THRSH(2)) THEN
C         DO 16 J = 2,LEN
C           TBUFF(IPOS-2+J) = BLNK
C16       CONTINUE
          TBUFF(IPOS-2+LEN) = STAR
        ELSE
          IF (DPLA .LT. 0) THEN
            CALL DECCHR (DATA(I),LEN,JUST,OLEN,TBUFF(IPOS))
          ELSE
            CALL DECCHX (DATA(I),LEN,SGFD,DPLA,TBUFF(IPOS))
          END IF
        END IF
        IPOS = IPOS + LEN
 20   CONTINUE
      OLEN = IPOS - 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTLNO
     I                    (DATE,NUMDSN,DATA,WDTH,SGFD,DPLA,DTFG,
     O                     OLEN,TBUFF)
C
C     + + + PURPOSE + + +
C     Convert DATE and an array of real numbers into a character array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NUMDSN,DATE(6),WDTH,OLEN,SGFD,DPLA,DTFG(NUMDSN)
      REAL        DATA(NUMDSN)
      CHARACTER*1 TBUFF(250)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATE   - date
C     NUMDSN - number of data values to output ( 1 <= NUMDSN <= 30 )
C     DATA   - array of NUMDSN data values to be output
C     WDTH   - output width ( <= 250 )
C     SGFD   - significant digits for output
C     DPLA   - number of decimal places for output
C     DTFG   - array of indicator flag to include data value
C              0 - leave field blank
C              1 - convert data to character
C     OLEN   - actual number of characters output to TBUFF
C     TBUFF  - output character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,IPOS,ERRFLG,JUST,J,LEN,ID
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL   DATLST, DECCHR, DECCHX, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  BLNK
     #    /  ' '/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL ZIPC ( WDTH, BLNK, TBUFF )
      ID = 23
C     ID is position date end and data starts in ID+1
      JUST = 0
      IPOS = 2
      CALL DATLST (DATE,TBUFF(IPOS),OLEN,ERRFLG)
C
      IF (DATE(3).LT.10) THEN
C       shift date string if day  < 10
        DO 12 I = 1,11
          J = ID - I
          TBUFF(J) = TBUFF(J-1)
 12     CONTINUE
        TBUFF(11) = BLNK
      END IF
C
      IPOS = ID
      LEN = (WDTH-22)/NUMDSN
      IF (LEN.GT.12) LEN = 12
      DO 20 I = 1,NUMDSN
        IF (DTFG(I) .EQ. 0) THEN
          TBUFF(IPOS-2+LEN) = BLNK
        ELSE
          IF (DPLA .LT. 0) THEN
            CALL DECCHR (DATA(I),LEN,JUST,OLEN,TBUFF(IPOS))
          ELSE
            CALL DECCHX (DATA(I),LEN,SGFD,DPLA,TBUFF(IPOS))
          END IF
        END IF
        IPOS = IPOS + LEN
 20   CONTINUE
      OLEN = IPOS - 1
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   STRFND
     I                          (LEN,STR,FLEN,FSTR)
C
C     Return the position in the array STR of size LEN that
C     the array FSTR of size FLEN occurs.  If FSTR is not
C     found, a zero is returned.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,FLEN
      CHARACTER*1 STR(LEN),FSTR(FLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array being searched
C     STR    - character array to be searched
C     FLEN   - size of character array to search for
C     FSTR   - character array to be searched for
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,FNDPOS
C
C     + + + END SPECIFICATIONS + + +
C
      FNDPOS= 0
      IF (FLEN.LE.LEN) THEN
        I= 0
        J= 0
 10     CONTINUE
          J= 0
 20       CONTINUE
            I= I+ 1
            J= J+ 1
            IF (J.EQ.FLEN.AND.STR(I).EQ.FSTR(J)) FNDPOS= I- FLEN+ 1
          IF (STR(I).EQ.FSTR(J).AND.FNDPOS.EQ.0.AND.J.LT.FLEN) GO TO 20
          I= I- J+ 1
        IF (I.LT.LEN-FLEN+1.AND.FNDPOS.EQ.0) GO TO 10
      END IF
C
      STRFND= FNDPOS
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   STRLNX
     I                           (NPTS,BUFF)
C
C     Return the number of characters in the array BUFF, including
C     imbedded blanks and excluding leading and trailing blanks.
C     A value of 1 is returned for all blanks.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NPTS
      CHARACTER*1 BUFF(NPTS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NPTS   - size of the character array
C     BUFF   - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     K,KE,KB
      CHARACTER*1 BLNK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
      K = 1
 5    CONTINUE
        IF (BUFF(K).NE.BLNK) GO TO 6
        K = K + 1
      IF (K.LE.NPTS) GO TO 5
 6    CONTINUE
      KB = K -1
C
      K = NPTS
 7    CONTINUE
        IF (BUFF(K).NE.BLNK) GO TO 8
        K = K - 1
      IF (K.GE.1) GO TO 7
 8    CONTINUE
      KE = K
C
      K= KE- KB
      IF (K.LE.0) K=1
      STRLNX = K
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZLJUST
     M                   (STRING)
C
C     + + + PURPOSE + + +
C     Remove leading blanks from a character variable.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character variable to left jusfify
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,L
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + END SPECIFICATIONS + + +
C
      IF (STRING(1:1) .EQ. ' ') THEN
C       has at least 1 leading blank
        L = LEN(STRING)
        I = 0
 10     CONTINUE
C         loop to look for other leading blanks
          I= I+ 1
        IF (I.LT.L .AND. STRING(I:I).EQ.' ') GO TO 10
C       remove leading blanks
Chnb  some compilers (OTG) complain about "STRING= STRING(I:L)", so ...
        K = 1
        DO 15, J=I,L
           STRING(K:K) = STRING(J:J)
           K = K + 1
 15     CONTINUE
        STRING(K:) = ' '
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRTSTR
     I                    (FUNIT,LEN,STR)
C
C     + + + PURPOSE + + +
C     Write a character array to the given file unit.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FUNIT,LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FUNIT  - Fortran unit number for output
C     LEN    - size of character array
C     STR    - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (250A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LEN.GT.0) THEN
C       output string, then cr/lf
        J = LENSTR( LEN, STR )
        WRITE (FUNIT,2000) (STR(I),I=1,J)
      ELSE
C       only write a cr/lf
        WRITE (FUNIT,2000)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   ZLNTXT
     I                          (STRING)
C
C     + + + PURPOSE + + +
C     Determine length of text in a character variable,
C     i.e., the index of the last non-blank or non-null character.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character variable
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,DONFG
      CHARACTER*1 NULL,CTMP
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN, CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      NULL = CHAR(0)
      I    = LEN(STRING)
      DONFG= 0
      IF (I.GT.0) THEN
C       string is not all blanks
 10     CONTINUE
          CTMP= STRING(I:I)
          IF (CTMP.EQ.' ' .OR. CTMP.EQ.NULL) THEN
C           still nothing useful
            I= I- 1
          ELSE
C           a character
            DONFG= 1
          END IF
        IF (I.GT.0 .AND. DONFG.EQ.0) GOTO 10
      END IF
C
      ZLNTXT= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZTRIM
     M                  (STRING)
C
C     + + + PURPOSE + + +
C     Remove embeded blanks in a character variable
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C      + + + ARGUMENT DEFINITIONS + + +
C     STRING - character variable having blanks removed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
      CHARACTER TEMP*255
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + END SPECIFICATIONS + + +
C
      J = 0
      DO 100 I = 1, LEN(STRING)
C       look at each char
        IF (STRING(I:I) .NE. ' ') THEN
C         not blank, save it
          J = J + 1
          TEMP(J:J) = STRING(I:I)
        END IF
 100  CONTINUE
C
      IF (J .GT. 0) THEN
C       not all blank, return string with no blanks
        STRING = TEMP(1:J)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   QUPCAS
     I                    (LEN,
     M                     STRING)
C
C     + + + PURPOSE + + +
C     To convert a character string from lower case to upper case
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STRING(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of character string
C     STRING - character string to be made upper case
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ICH
C
C     + + + INTRINSICS + + +
      INTRINSIC  ICHAR, MOD, CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I = 1, LEN
        ICH= ICHAR(STRING(I))
        ICH= MOD(ICH,128)
        IF (ICH.GE.97 .AND. ICH.LE.122) THEN
C         character is lower case
          STRING(I)= CHAR(ICH-32)
        END IF
 10   CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   CKNBLV
     I                           (LEN,CBUF)
C
C     + + + PURPOSE + + +
C     Check character array CBUF for all blanks.  Return
C     a zero if all blanks, otherwise return position in
C     character array of first non-blank character.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 CBUF(LEN)
C
C     + + + ARGUMENT DEFINITION + + +
C     LEN    - size of character array
C     CBUF   - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      I = 0
      J = 0
 10   CONTINUE
        I = I+ 1
        IF (CBUF(I) .NE. ' ') J = I
      IF (I.LT.LEN .AND. J.EQ.0) GO TO 10
C
      CKNBLV = J
C
      RETURN
      END
C
C
C
      SUBROUTINE   RHTSTR
     I                    (LEN,
     M                     TITLE)
C
C     + + + PURPOSE + + +
C     Right justify characters in the array TITLE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 TITLE(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array, 1 <= LEN <= 132
C     TITLE  - character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IS,I,TLEN,I1,I2
      CHARACTER*1 BLNK
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK/' '/
C
C     + + + END SPECIFICATIONS + + +
C
C     find number of blanks on the right
      TLEN = LEN
      IF (TLEN.GT.132) TLEN = 132
      I = TLEN + 1
 10   CONTINUE
        I = I - 1
      IF (TITLE(I).EQ.BLNK.AND.I.GT.1) GO TO 10
      IS = I
C     found end of non-blanks
C
      IF (IS.LT.TLEN) THEN
C       title not currently right justified
        DO 30 I = 1,IS
          I1 = TLEN - I + 1
          I2 = IS - I + 1
          TITLE(I1) = TITLE(I2)
 30     CONTINUE
        DO 40 I = 1,TLEN-IS
          TITLE(I) = BLNK
 40     CONTINUE
      END IF
C
      RETURN
      END   
C
C
C
      SUBROUTINE   ADCOMA
     I                   (LEN,
     M                    STR)
C
C     + + + PURPOSE + + +
C     This routine places a comma(s) in the real number in the string
C     every 3 digits.  String may include sign, decimal point, and
C     decimal digits.  If there is not enough room in the string, the
C     string is returned as it was entered.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of the string
C     STR    - character string of a decimal number
C
C     + + + PARAMETERS + + + 
      INTEGER   MAXTMP
      PARAMETER (MAXTMP=50)
C 
C     + + + LOCAL VARIABLES + + + 
      INTEGER   LOC,I1,NEG,SHFT,OLEN,I,IDIG,IOLD,INEW,JUST
      CHARACTER*1 TEMP(MAXTMP),DOT,MINUS,BLNK
C
C     + + + FUNCTIONS + + + 
      INTEGER  LENSTR, STRFND
C
C     + + + EXTERNAL + + + 
      EXTERNAL  LENSTR, LFTSTR, RHTSTR, STRFND, ZIPC, COPYC
C
C     + + + DATA INITIALIZATIONS + + + 
      DATA MINUS/'-'/,  DOT/'.'/,  BLNK/' '/, I1/1/
C
C     + + + END SPECIFICATIONS + + + 
C
C     check if right of left justified
      IF (STR(LEN) .EQ. ' ') THEN
C       assume string was left justified
        JUST = 0
      ELSE
        JUST = 1
      END IF
C     shift to left
      CALL LFTSTR (LEN,STR)
C     locate position of decimal point if any
      LOC = STRFND (LEN,STR,I1,DOT)
C     check for minus sign
      IF (STR(1) .EQ. MINUS) THEN               
        NEG = 1
      ELSE
        NEG = 0
      END IF
C     determine length of string
      OLEN = LENSTR (LEN,STR) 
C     reset LOC for cases of no decimal point
      IF (LOC .LE. 0) LOC = OLEN + 1
C     determine number of commas needed, 10000 or greater
      SHFT = (LOC-2-NEG)/3
C     blank out temp buffer
      CALL ZIPC (MAXTMP,BLNK,TEMP)        
C
      IF (OLEN+SHFT .LE. LEN) THEN
C       sufficient room for commas
        IF (OLEN+SHFT .LE. MAXTMP) THEN
C         sufficient room for temp buffer
          IDIG = 0
          DO 10 I = 1,OLEN
C           loop thru the character string, right to left
C           IOLD is position in string without commas
C           INEW is position in string with commas
            IOLD = OLEN - I + 1
            INEW = IOLD + SHFT
            IF (IOLD .GE. LOC) THEN
C             found decimal digit or point, just shift over
              TEMP(INEW) = STR(IOLD)
            ELSE IF (STR(IOLD) .EQ. '-') THEN
C             INEW should be equal to IOLD because SHFT should be 0
              TEMP(INEW) = STR(IOLD)
            ELSE IF (IDIG  .LT. 3) THEN
C             within cluster of 3, not ready to add comma
              TEMP(INEW) = STR(IOLD)
C             count digits
              IDIG = IDIG + 1
            ELSE
C             must be time to add a comma, IDIG = 3
              TEMP(INEW) = ','
              TEMP(INEW-1) = STR(IOLD)
              IDIG = 1
              SHFT = SHFT - 1
            END IF
 10       CONTINUE
C
C         replace with new string
          CALL COPYC (LEN,TEMP,STR)
          IF (JUST .EQ. 1 .AND. LEN .GT. SHFT+OLEN) THEN
C           was right justified and blank spaces remain
C           so right justify
            CALL RHTSTR (LEN,STR)
          END IF
        END IF
      END IF
C
      RETURN
      END                   
