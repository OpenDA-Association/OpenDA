C
C
C
      SUBROUTINE   COPYD
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the double precision array ZIP of size LEN
C     to the double precision array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      DOUBLE PRECISION   ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
        X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPYI
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the integer array ZIP of size LEN to
C     the integer array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      INTEGER     ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPYR
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the real array ZIP of size LEN to the
C     real array X.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      REAL        ZIP(LEN), X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of arrays
C     ZIP    - input array of size LEN
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP(L)
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   COPYC
     I                   (LEN, ZIP,
     O                    X)
C
C     + + + PURPOSE + + +
C     Copy the character array ZIP of size LEN to
C     the character array X of size LEN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 ZIP(LEN),X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character arrays
C     ZIP    - input character array
C     X      - output character array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L= 1, LEN
        X(L)= ZIP(L)
 100  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPD
     I                  (LEN, ZIP,
     O                   X)
C
C     + + + PURPOSE + + +
C     Fill the double precision array X of size LEN
C     with the given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      DOUBLE PRECISION   ZIP, X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     ZIP    - value to fill array
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
        X(L) = ZIP
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPI
     I                  (LEN, ZIP,
     O                   X)
C
C     + + + PURPOSE + + +
C     Fill the integer array X of size LEN with
C     the given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN, ZIP
      INTEGER     X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     ZIP    - value to fill array
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPR
     I                  (LEN, ZIP,
     O                   X)
C
C     + + + PURPOSE + + +
C     Fill the real array X of size LEN with the
C     given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      REAL        ZIP, X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of array
C     ZIP    - value to fill array
C     X      - output array of size LEN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L = 1, LEN
         X(L) = ZIP
  100 CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZIPC
     I                  (LEN, ZIP,
     O                   X)
C
C     Fill the character array X of size LEN with
C     the given value ZIP.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 ZIP,X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - size of character array
C     ZIP    - character to fill array
C     X      - character array to be filled
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     L
C
C     + + + END SPECIFICATIONS + + +
C
      DO 100 L= 1, LEN
        X(L)= ZIP
 100  CONTINUE
C
      RETURN
      END
