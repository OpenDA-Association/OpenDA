C     utnumb.f 2.1 9/4/91
C
C
C
      SUBROUTINE   CHKINT
     I                    (IMIN,IMAX,IDEF,
     M                     IVAL,
     O                     ICHK)
C
C     + + + PURPOSE + + +
C     Check the integer IVAL against the minimum (IMIN)
C     and maximum (IMAX) values.  IVAL is set to the
C     default (IDEF) if IVAL is zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ICHK,IMIN,IMAX,IDEF,IVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMIN   - minimum allowable value, -999 if there is no minimum
C     IMAX   - maximum allowable value, -999 if there is no maximum
C     IDEF   - default value, or -999 if there is no default
C     IVAL   - value to be checked
C     ICHK   - indicator flag for valid IVAL
C              0 - invalid IVAL
C              1 - valid IVAL
C
C     + + + END SPECIFICATIONS + + +
C
      ICHK = 0
      IF (IVAL.EQ.0) IVAL = IDEF
      IF (IVAL.GE.IMIN.AND.IVAL.LE.IMAX) ICHK = 1
      IF (IVAL.GE.IMIN.AND.IMAX.EQ.-999) ICHK = 1
      IF (IMIN.EQ.-999.AND.IVAL.LE.IMAX) ICHK = 1
      IF (IMIN.EQ.-999.AND.IMAX.EQ.-999) ICHK = 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKREA
     I                    (RMIN,RMAX,RDEF,
     M                     RVAL,
     O                     RCHK)
C
C     + + + PURPOSE + + +
C     Check the real RVAL against the minimum (RMIN)
C     and maximum (RMAX) values.  RVAL is set to the
C     default (RDEF) if RVAL is zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RCHK
      REAL      RMIN,RMAX,RDEF,RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RMIN   - minimum allowable value, -999. if there is no minimum
C     RMAX   - maximum allowable value, -999. if there is no maximum
C     RDEF   - default value, or -999. if there is no default
C     RVAL   - value to be checked
C     RCHK   - indicator flag for valid RVAL
C              0 - invalid RVAL
C              1 - valid RVAL
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MINCHK,MAXCHK
      REAL      FACT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   NUMINI
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init maching dependent numeric constants (const.inc)
      CALL NUMINI
C
      RCHK= 0
Chnb      FACT= 0.999
      FACT= RP1MIN
Chnb      IF (ABS(RVAL).LT.1.0E-20)  RVAL= RDEF
      IF (ABS(RVAL).LE.R0MIN)  RVAL= RDEF
C
      MINCHK= 0
Chnb      IF      ((ABS(RMIN+999.).LT.0.01)
Chnb     1  .OR.  ((RVAL.GE.0.0) .AND. (RVAL.GE.RMIN*FACT))
Chnb     2  .OR.  ((RVAL.LT.0.0) .AND. (RVAL.GT.RMIN/FACT)))
Chnb     3        MINCHK= 1
      IF      ((ABS(RMIN+999.0).LT.(RP1MIN - 1.0))
     1  .OR.  ((RVAL.GE.0.0) .AND. (RVAL.GE.RMIN/FACT))
     2  .OR.  ((RVAL.LT.0.0) .AND. (RVAL.GT.RMIN*FACT)))
     3        MINCHK= 1
C
      MAXCHK= 0
Chnb      IF      ((ABS(RMAX+999.).LT.0.01)
Chnb     1  .OR.  ((RVAL.GE.0.0) .AND. (RVAL.LE.RMAX/FACT))
Chnb     2  .OR.  ((RVAL.LT.0.0) .AND. (RVAL.LT.RMAX/FACT)))
Chnb     3        MAXCHK= 1
      IF      ((ABS(RMAX+999.0).LT.(RP1MIN - 1.0))
     1  .OR.  ((RVAL.GE.0.0) .AND. (RVAL.LE.RMAX*FACT))
     2  .OR.  ((RVAL.LT.0.0) .AND. (RVAL.LT.RMAX/FACT)))
     3        MAXCHK= 1
C
      IF (MINCHK.EQ.1 .AND. MAXCHK.EQ.1)  RCHK= 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKDPR
     I                    (DMIN,DMAX,DDEF,
     M                     DVAL,
     O                     DCHK)
C
C     + + + PURPOSE + + +
C     Check the double precision DVAL against the minimum (DMIN)
C     and maximum (DMAX) values.  DVAL is set to the
C     default (DDEF) if DVAL is zero.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER          DCHK
      DOUBLE PRECISION DMIN,DMAX,DDEF,DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DMIN   - minimum allowable value, -999. if there is no minimum
C     DMAX   - maximum allowable value, -999. if there is no maximum
C     DDEF   - default value, or -999. if there is no default
C     DVAL   - value to be checked
C     DCHK   - indicator flag for valid DVAL
C              0 - invalid DVAL
C              1 - valid DVAL
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MINCHK,MAXCHK
      REAL      FACT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   NUMINI
C
C     + + + END SPECIFICATIONS + + +
C
C     may need to init maching dependent numeric constants (const.inc)
      CALL NUMINI
C
      DCHK= 0
Chnb      FACT= 0.999
      FACT= DP1MIN
Chnb      IF (ABS(DVAL).LT.1.0E-20)  DVAL= DDEF
      IF (ABS(DVAL).LE.D0MIN)  DVAL= DDEF
C
      MINCHK= 0
Chnb      IF      ((ABS(DMIN+999.).LT.0.01)
Chnb     1  .OR.  ((DVAL.GE.0.0) .AND. (DVAL.GE.DMIN*FACT))
Chnb     2  .OR.  ((DVAL.LT.0.0) .AND. (DVAL.GT.DMIN/FACT)))
Chnb     3        MINCHK= 1
      IF      ((ABS(DMIN+999.0).LT.(DP1MIN - 1.0))
     1  .OR.  ((DVAL.GE.0.0) .AND. (DVAL.GE.DMIN/FACT))
     2  .OR.  ((DVAL.LT.0.0) .AND. (DVAL.GT.DMIN*FACT)))
     3        MINCHK= 1
C
      MAXCHK= 0
Chnb      IF      ((ABS(DMAX+999.).LT.0.01)
Chnb     1  .OR.  ((DVAL.GE.0.0) .AND. (DVAL.LE.DMAX/FACT))
Chnb     2  .OR.  ((DVAL.LT.0.0) .AND. (DVAL.LT.DMAX/FACT)))
Chnb     3        MAXCHK= 1
      IF      ((ABS(DMAX+999.0).LT.(DP1MIN - 1.0))
     1  .OR.  ((DVAL.GE.0.0) .AND. (DVAL.LE.DMAX*FACT))
     2  .OR.  ((DVAL.LT.0.0) .AND. (DVAL.LT.DMAX/FACT)))
     3        MAXCHK= 1
C
      IF (MINCHK.EQ.1 .AND. MAXCHK.EQ.1)  DCHK= 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   NUMINI
C
C     + + + PURPOSE + + +
C     Initialize machine dependent floating point constants.
C
C     + + + COMMON BLOCKS + + +
C     numeric constants
      INCLUDE 'const.inc'
C
C     + + + SAVES + + +
      INTEGER       INITFG
      SAVE          INITFG
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       PRECFG
      INTEGER*4     R2PREC,D2PREC,TI
      REAL*4        R1,R2,R3,R4,  TR
      REAL*8        D1,D2,D3,D4
C
C     + + + FUNCTIONS + + +
      REAL*4   RNOP
      REAL*8   DNOP

C     + + + INTRINSICS + + +
      INTRINSIC  INT,LOG10
C
C     + + + EQUIVALENCE STATEMENTS + + +
      EQUIVALENCE (TR,TI)
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INITFG /0/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT(' UNKNOWN DOUBLE PRECISION FORMAT, using default double pre
     .cision values!')
 2010 FORMAT(' UNKNOWN MACHINE TYPE, using default precision values!')
C
C     + + + STATEMENT FUNCTION DEFINITIONS + + +
C     No OPperation, used to keep Ryan/McFarland optimization honest
      RNOP(R1) = R1
      DNOP(D1) = D1
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INITFG.EQ.0) THEN
C       Need to calculate machine dependent numeric constants.
C       Set flag to do so only once.
        INITFG= 1
C
C       Determine the number of decimal digits of REAL precision number and
C       the smallest REAL greater than 1.0.
C       First find the number of significant binary digits, then convert
C       it to the number of significant decimal digits.  Any machine used
C       today is going to have more than 7 binary digits of precision
C       (actually, we're cheating, because 1 is added to R2PREC after
C       it is tested.  This usually results in 7 decimal digits of
C       precision, which is usually the case, whereas strictly
C       speaking only 6 decimal digits are guaranteed, and 6 is
C       usually the result if R2PREC is initialized to 6).
        R2PREC = 7
        R1 = 1.0
        R2 = 0.0078125
 100    CONTINUE
C         loop until PRECFG=1
          PRECFG= 0
          R3 = R1 + R2
C         this 'nop' keeps Ryan/McFarland optimization honest.  Without it,
C         the precision of an 80 bit floating point register is computed
C         (instead of a 4-byte real) when R/M optimization is turned on.
C         You can comment out the call to NOP if you observe that by
C         doing so DECCHR doesn't provide extra digits of precision.
          R4 = RNOP(R3)
C         The following conditional causes a warning on the PRIME,
C         but this statement needs to be left as is since we are looking
C         for the case of the two real numbers being exactly the same
C         to help determine the precision of the machine.
          IF (R1 .NE. R3) THEN
             RP1MIN = R3
             R2PREC = R2PREC + 1
             R2 = R2 / 2.0
             PRECFG= 1
          END IF
        IF (PRECFG.EQ.1) GO TO 100
C
        RPREC = INT(LOG10(2.0**R2PREC))
C
C       Determine the number of decimal digits of the typical DOUBLE precision
C       number and the smallest DOUBLE greater than 1.0D0.
        D2PREC = 7
        D1 = 1.0D0
        D2 = 0.0078125D0
 200    CONTINUE
C         loop until PRECFG=1
          PRECFG= 0
          D3 = D1 + D2
C         this 'nop' keeps Ryan/McFarland optimization honest.  Without it,
C         the precision of an 80 bit floating point register is computed
C         (instead of a 4-byte real) when R/M optimization is turned on.
C         You can comment out the call to NOP if you observe that by
C         doing so DECCHR doesn't provide extra digits of precision.
          D4 = DNOP(D3)
C         The following conditional causes a warning on the PRIME,
C         but this statement needs to be left as is since we are looking
C         for the case of the two real numbers being exactly the same
C         to help determine the precision of the machine.
          IF (D1 .NE. D3) THEN
             DP1MIN = D3
             D2PREC = D2PREC + 1
             D2 = D2 / 2.0D0
             PRECFG= 1
          END IF
        IF (PRECFG.EQ.1) GO TO 200
C
        DPREC = INT(LOG10(2.0D0**D2PREC))
C
        TR = 1.0
        IF (TI .EQ. 1065353216) THEN
C         this should be the case for the Sun or Ryan/McFarland
          R1   = 1.0E-19
          R0MIN = 1.1754945E-19 * R1
          R1   = 1.0E+19
          R0MAX = 3.40282347E+19 * R1
C         On DG (unix) system, RP1MIN is calculated a little too small
C         to have the needed correcting effect in DECCHR/DPRCHR when
C         using LOG10 intrinsics on those machines.  Thus, increase it a
C         little bit to force correction. (also see DP1MIN below)
          RP1MIN= RP1MIN**4
          D1   = 1.0D-28
          D0MIN = (2.22507385850720219D-28 * D1**10)
          D1   = 1.0D+28
          D0MAX = (1.7976931348623157D0 * D1**11)
          DP1MIN= DP1MIN**4
        ELSE IF (TI .EQ. 16512) THEN
C         this should be the case for the VAX
          R1   = 1.0E-20
          R0MIN = 2.9387359E-19 * R1
          R1   = 1.0E+18
          R0MAX = 1.7014117E+20 * R1
          IF (DPREC .EQ. 17) THEN
C            this should be the case for the default /NOG_FLOAT compiler option
             D1   = 1.0D-20
             D0MIN = 2.938735877055719D-19 * D1
             D1   = 1.0E+19
             D0MAX = 1.7014118346046923D+19 * D1
          ELSE IF (DPREC .EQ. 16) THEN
C            this should be the case for the /G_FLOAT option
             D3 = 1.0D-21
             D4 = 1.0D-20
             D0MIN = (5.562684646268008D-20)*(D4**5)*(D3**9)
             D3 = 1.0D+21
             D4 = 1.0D+20
             D0MAX = (8.988465674311578D+21) * (D3**6) * (D4**8)
          ELSE
C            assume D_FLOAT real*8 type
             WRITE(*,2000)
             D1   = 1.0D-20
             D0MIN = 2.938735877055719D-19 * D1
             D1   = 1.0E+19
             D0MAX = 1.7014118346046923D+19 * D1
          END IF
        ELSE
C         not quite sure of machine type
          WRITE(*,2010)
          R1    = 1.0E-19
          R0MIN = 1.1754945E-19 * R1
          R1    = 1.0E+18
C         On PRIME system, RP1MIN is calculated a little too small
C         to have the needed correcting effect in DECCHR/DPRCHR when
C         using LOG10 intrinsics on those machines.  Thus, increase it a
C         little bit to force correction. (also see DP1MIN below)
          RP1MIN= RP1MIN**4
          R0MAX = 1.7014117E+20 * R1
          D1    = 1.0D-20
          D0MIN = 2.938735877055719D-19 * D1
          D1    = 1.0E+19
          D0MAX = 1.7014118346046923D+19 * D1
          DP1MIN= DP1MIN**4
        ENDIF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   DECPRC
     I                   (SIGDIG,DECPLA,
     M                    RVAL)
C
C     + + + PURPOSE + + +
C     Adjust a real value to contain a specified
C     number of significant digits and decimal places.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SIGDIG,DECPLA
      REAL      RVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SIGDIG - desired number of significant digits
C     DECPLA - desired number of decimal places
C     RVAL   - real value
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,ILEN
      CHARACTER*1 STR(20)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
      REAL        CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CHRDEC, DECCHX
C
C     + + + END SPECIFICATIONS + + +
C
C     put real value into string with desired number of significant digits
      ILEN = 20
      CALL DECCHX (RVAL,ILEN,SIGDIG,DECPLA,
     O             STR)
C     determine output length of string
      I= LENSTR(ILEN,STR)
C     get adjusted real value from string
      RVAL= CHRDEC(I,STR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SHIFTD
     I                   (LEN, VAL, VALTOL,
     M                    X,
     O                    IPOS)
C
C     + + + PURPOSE + + +
C     Moves all array elements equal to VAL to end of array,
C     shifting remaining elements forward, preserving their order.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER           LEN, IPOS
      DOUBLE PRECISION  VAL, VALTOL, X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - array length
C     VAL    - value to be moved to end of array
C     VALTOL - tolerance for determining how close array elements
C              need to be to VAL to be moved to end of array
C     X      - double precision array
C     IPOS   - position of last "good" value, when all VALs moved to end
C
C     + + + LOCAL VARIABLES + + +
      INTEGER           I, INDX, VALCNT, LENARR
      DOUBLE PRECISION  TMPVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     determine number of values to be moved to end
      VALCNT = 0
      DO 10 I = 1, LEN
        IF (ABS(X(I) - VAL) .LE. VALTOL) VALCNT = VALCNT + 1
 10   CONTINUE
C
C     determine position of last "good" value when done shifting ele.s
      IPOS = LEN - VALCNT
C
      LENARR = LEN
      INDX = 1
      IF (VALCNT.GT.0 .AND. VALCNT.LT.LEN) THEN
C       at least one value to move, but all elements not equal to VAL
 20     CONTINUE
          IF (ABS(X(INDX) - VAL) .LE. VALTOL) THEN
            TMPVAL = X(INDX)
C           shift array elements left
            DO 25 I = INDX, LENARR-1
              X(I) = X(I+1)
 25         CONTINUE
C           place value in last position
            X(LENARR) = TMPVAL
            LENARR = LENARR - 1
C           decrement number of values to be moved
            VALCNT = VALCNT - 1
          ELSE
            INDX = INDX + 1
          END IF
        IF (VALCNT .NE. 0) GOTO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SHIFTI
     I                   (LEN, VAL, VALTOL,
     M                    X,
     O                    IPOS)
C
C     + + + PURPOSE + + +
C     Moves all array elements equal to VAL to end of array,
C     shifting remaining elements forward, preserving their order.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEN, VAL, VALTOL, X(LEN), IPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - array length
C     VAL    - value to be moved to end of array
C     VALTOL - tolerance for determining how close array elements
C              need to be to VAL to be moved to end of array
C     X      - integer array
C     IPOS   - position of last "good" value, when all VALs moved to end
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I, INDX, VALCNT, LENARR, TMPVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     determine number of values to be moved to end
      VALCNT = 0
      DO 10 I = 1, LEN
        IF (ABS(X(I) - VAL) .LE. VALTOL) VALCNT = VALCNT + 1
 10   CONTINUE
C
C     determine position of last "good" value when done shifting ele.s
      IPOS = LEN - VALCNT
C
      LENARR = LEN
      INDX = 1
      IF (VALCNT.GT.0 .AND. VALCNT.LT.LEN) THEN
C       at least one value to move, but all elements not equal to VAL
 20     CONTINUE
          IF (ABS(X(INDX) - VAL) .LE. VALTOL) THEN
            TMPVAL = X(INDX)
C           shift array elements left
            DO 25 I = INDX, LENARR-1
              X(I) = X(I+1)
 25         CONTINUE
C           place value in last position
            X(LENARR) = TMPVAL
            LENARR = LENARR - 1
C           decrement number of values to be moved
            VALCNT = VALCNT - 1
          ELSE
            INDX = INDX + 1
          END IF
        IF (VALCNT .NE. 0) GOTO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SHIFTR
     I                   (LEN, VAL, VALTOL,
     M                    X,
     O                    IPOS)
C
C     + + + PURPOSE + + +
C     Moves all array elements equal to VAL to end of array,
C     shifting remaining elements forward, preserving their order.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LEN, IPOS
      REAL      VAL, VALTOL, X(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - array length
C     VAL    - value to be moved to end of array
C     VALTOL - tolerance for determining how close array elements
C              need to be to VAL to be moved to end of array
C     X      - real array
C     IPOS   - position of last "good" value, when all VALs moved to end
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I, INDX, VALCNT, LENARR
      REAL       TMPVAL
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS
C
C     + + + END SPECIFICATIONS + + +
C
C     determine number of values to be moved to end
      VALCNT = 0
      DO 10 I = 1, LEN
        IF (ABS(X(I) - VAL) .LE. VALTOL) VALCNT = VALCNT + 1
 10   CONTINUE
C
C     determine position of last "good" value when done shifting ele.s
      IPOS = LEN - VALCNT
C
      LENARR = LEN
      INDX = 1
      IF (VALCNT.GT.0 .AND. VALCNT.LT.LEN) THEN
C       at least one value to move, but all elements not equal to VAL
 20     CONTINUE
          IF (ABS(X(INDX) - VAL) .LE. VALTOL) THEN
            TMPVAL = X(INDX)
C           shift array elements left
            DO 25 I = INDX, LENARR-1
              X(I) = X(I+1)
 25         CONTINUE
C           place value in last position
            X(LENARR) = TMPVAL
            LENARR = LENARR - 1
C           decrement number of values to be moved
            VALCNT = VALCNT - 1
          ELSE
            INDX = INDX + 1
          END IF
        IF (VALCNT .NE. 0) GOTO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   UTVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     util library.
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  VERSN
C
C     + + + END SPECIFICATIONS + + +
C
      INCLUDE 'fversn.inc'
C
      RETURN
      END
