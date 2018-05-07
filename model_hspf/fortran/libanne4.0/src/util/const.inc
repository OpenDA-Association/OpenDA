C     CONST.INC 2.1 9/4/91
C
C     These COMMON block allows for the storage of machine dependent
C     numeric constants.
C
      COMMON /ICONST/ RPREC,DPREC
      INTEGER*4       RPREC,DPREC
C
      COMMON /RCONST/ R0MIN,RP1MIN,R0MAX
      REAL*4          R0MIN,RP1MIN,R0MAX
C
      COMMON /DCONST/ D0MIN,DP1MIN,D0MAX
      REAL*8          D0MIN,DP1MIN,D0MAX
C
      SAVE   /ICONST/, /RCONST/, /DCONST/
C
C     RPREC  - the number of (decimal) digits of precision of a real number.
C     DPREC  - the number of (decimal) digits of precision of a double.
C     R0MIN  - the smallest (positive) representable real number.
C     RP1MIN - the minimum representable real number greater than (plus) 1.0.
C     R0MAX  - the largest representable real number.
C     D0MIN  - the smallest (positive) representable double.
C     DP1MIN - the minimum representable double greater than (plus) 1.0.
C     D0MAX  - the largest representable double.
C
