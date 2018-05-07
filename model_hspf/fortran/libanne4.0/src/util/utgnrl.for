C
C
C
      REAL   FUNCTION   CNVTDG
     I                         ( LATLNG )
C
C     + + + PURPOSE + + +
C     This routine computes latitude or longitude in degrees
C     from an integer representation of degrees-minutes-seconds.
C     Examples:
C               593000 is converted to 59.5
C              -723015 is converted to -72.5042
C
C     + + + HISTORY + + +
C     kmf - Nov 26, 1996, corrected computation
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LATLNG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LATLNG - latitude or longitude in degrees, minutes, seconds
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DEG, MIN, SEC
C
C     + + + INTRINSICS + + +
      INTRINSIC   REAL
C
C     + + + END SPECIFICATIONS + + +
C
      DEG =  LATLNG  /  10000
      MIN = (LATLNG  -  DEG * 10000)  /  100
      SEC =  LATLNG  -  DEG * 10000   -  MIN * 100
C
      CNVTDG = REAL( DEG ) + REAL( MIN ) / 60.0 + REAL( SEC ) / 3600.0
C
      RETURN
      END
