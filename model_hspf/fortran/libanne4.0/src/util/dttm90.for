C
C
C
      SUBROUTINE   WDSYSD
     O                   (IDATE)
C
C     + + + PURPOSE + + +
C     Fetch system date and time for DSN creation/modification
C     attributes.  *** FORTRAN 90 ONLY ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IDATE(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IDATE  - integer array containing character representation of date
C              (1) - 4-digit year
C              (2) - 2-digit month and 2-digit day
C              (3) - 2-digit hour and 2-digit month
C              (4) - 2-digit second and 2 blanks
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   VALUES(8)
      CHARACTER*10 DATE,TIME,ZONE
C
C     + + + INTRINSICS + + +
      INTRINSIC DATE_AND_TIME
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (2A4)
 1010 FORMAT (A4,A2,2X)
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE_AND_TIME (DATE,TIME,ZONE,VALUES)
      READ (DATE(1:8),1000) IDATE(1),IDATE(2)
      READ (TIME(1:6),1010) IDATE(3),IDATE(4)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATM
     O                   ( YR, MO, DY, HR, MN, SC )
C
C     + + + PURPOSE + + +
C     Returns the current date and time.  Calls the Fortran 90
C     intrinsic date_and_time for the date and time.
Cy2k  Note:  Returns a 2-digit year for backwords compatability
Cy2k  with older code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DY, HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS
C     YR     - year (2-digit)
C     MO     - month
C     DY     - day
C     HR     - hour
C     MN     - minute
C     SC     - second
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   VALUES(8)
      CHARACTER*10 DATE,TIME,ZONE
C
C     + + + INTRINSICS + + +
      INTRINSIC DATE_AND_TIME
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE_AND_TIME (DATE,TIME,ZONE,VALUES)
      YR = VALUES(1)
      YR = MOD ( YR, 100 )
      MO = VALUES(2)
      DY = VALUES(3)
      HR = VALUES(5)
      MN = VALUES(6)
      SC = VALUES(7)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYDATE
     O                   ( YR, MO, DA )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system date.
C     Uses the Fortran 90 intrinsic date_and_time.
Cy2k  Note:  Returns a 2-digit year for backwords compatability
Cy2k  with older code.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   YR, MO, DA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     YR     - year (2-digit)
C     MO     - month
C     DA     - day
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   VALUES(8)
      CHARACTER*10 DATE,TIME,ZONE
C
C     + + + INTRINSICS + + +
      INTRINSIC DATE_AND_TIME
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE_AND_TIME (DATE,TIME,ZONE,VALUES)
      YR = VALUES(1)
      YR = MOD ( YR, 100 )
      MO = VALUES(2)
      DA = VALUES(3)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SYTIME
     O                   ( HR, MN, SC )
C
C     + + + PURPOSE + + +
C     This subroutine is used to retrieve the system time.
C     Uses the Fortran 90 intrinsic date_and_time.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   HR, MN, SC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     HR     - Number of hours since midnight
C     MN     - Number of minutes since hour
C     SC     - Number of seconds since minute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   VALUES(8)
      CHARACTER*10 DATE,TIME,ZONE
C
C     + + + INTRINSICS + + +
      INTRINSIC DATE_AND_TIME
C
C     + + + END SPECIFICATIONS + + +
C
      CALL DATE_AND_TIME (DATE,TIME,ZONE,VALUES)
      HR = VALUES(5)
      MN = VALUES(6)
      SC = VALUES(7)
C
      RETURN
      END

