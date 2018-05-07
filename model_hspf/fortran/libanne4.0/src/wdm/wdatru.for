C
C
C
      SUBROUTINE   INDSFN
     I                   ( WDMFL, DSTYPE,
     I                     TSTYPE, STAID, ISTAID, TSSTEP, TCODE,
     I                     DSNMAX,
     O                     DSNFND, DSNKNT )
C
C     + + + PURPOSE + + +
C     Identify the data sets that have the specified attributes.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMFL, DSTYPE, ISTAID, TSSTEP, TCODE, DSNMAX, DSNKNT
      INTEGER      DSNFND(DSNMAX)
      CHARACTER*1  TSTYPE(4), STAID(16)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the wdm file
C     DSTYPE - type of data set to be found
C     TSTYPE - attribute containing a 4-character desciption
C     STAID  - station id attribute, 16-character alpha-numeric
C     ISTAID - integer station id
C     TSSTEP - time step attribute, not used if DSTYPE is not 1
C     TCODE  - time code attribute, used if DSTYPE = 1
C     DSNMAX - maximum number of data sets to be found
C     DSNFND - array of data set that were found
C     DSNKNT - number of data sets found
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IPT, IPC, IPN, LEN, LCK, I, SACNT,
     >             SAIND(5), SATYP(5), SABEG(5), SALEN(5),
     >             SAIMN(5), SAIMX(5), SACOND(5), SAOR(5),
     >             DSNRMN, DSNRMX, DSNFUL, ZERO
      INTEGER*4    SAVAL(30), ZERO4
      REAL         SARMN(5), SARMX(5)
      CHARACTER*20 CODE
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  LENSTR, ZIPI, WDFNDS
C
C     + + + DATA INITIALZATIONS + + +
      DATA   SACOND, SAOR, DSNRMN, DSNRMX, ZERO, ZERO4, SARMN, SARMX
     >     /    5*1,  5*0,      1,  32000,    0,     0, 5*0.0, 5*0.0 /
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT ( 20A4 )
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( 80A1 )
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize pointers, counters, and arrays
      IPT = 0
      IPC = 0
      IPN = 0
      LEN = 5
      CALL ZIPI ( LEN, ZERO, SAIND )
      CALL ZIPI ( LEN, ZERO, SATYP )
      CALL ZIPI ( LEN, ZERO, SABEG )
      CALL ZIPI ( LEN, ZERO, SALEN )
      CALL ZIPI ( LEN, ZERO, SAIMN )
      CALL ZIPI ( LEN, ZERO, SAIMX )
      LEN = 30
      CALL ZIPI ( LEN, ZERO4, SAVAL )
      CALL ZIPI ( DSNMAX, ZERO, DSNFND )
      DSNKNT = 0
      DSNFUL = 0
C
      WRITE (CODE,2000) TSTYPE, STAID
C     is there a TSTYPE available?
      LEN = 4
      LCK = LENSTR ( LEN, TSTYPE )
      IF (LCK .GT. 0) THEN
C       look for matching tstype
        IPT = IPT + 1
        IPC = IPN + 1
        IPN = IPN + 1
        SAIND(IPT) = 1
        SATYP(IPT) = 3
        SALEN(IPT) = 4
        SABEG(IPT) = IPC
        READ (CODE(1:4),1000) SAVAL(IPC)
      END IF
C     is there a STAID available
      LEN = 16
      LCK = LENSTR ( LEN, STAID )
      IF (LCK .GT. 0) THEN
C       look for a matching staid
        IPT = IPT + 1
        IPC = IPN + 1
        IPN = IPN + 4
        SAIND(IPT) = 2
        SATYP(IPT) = 3
        SALEN(IPT) = 16
        SABEG(IPT) = IPC
        READ (CODE(5:20),1000) (SAVAL(I), I = IPC, IPN)
      END IF
C
      IF (ISTAID .NE. 0) THEN
C       integer station id
        IPT = IPT + 1
        SAIND(IPT) = 51
        SATYP(IPT) = 1
        SALEN(IPT) = 1
        SABEG(IPT) = IPT
        SAIMN(IPT) = ISTAID
        SAIMX(IPT) = ISTAID
      END IF
C
      IF (DSTYPE .EQ. 1) THEN
C       include time step and time units
        IF (TCODE .GT. 0  .AND.  TCODE .LT. 7) THEN
C         valid tcode
          IPT = IPT + 1
          SAIND(IPT) = 17
          SATYP(IPT) = 1
          SALEN(IPT) = 1
          SAIMN(IPT) = TCODE
          SAIMX(IPT) = TCODE
        END IF
        IF (TSSTEP .GT. 0  .AND.  TSSTEP .LE. 1440) THEN
C         valid time step
          IPT = IPT + 1
          SAIND(IPT) = 33
          SATYP(IPT) = 1
          SALEN(IPT) = 1
          SAIMN(IPT) = TSSTEP
          SAIMX(IPT) = TSSTEP
        END IF
      END IF
C
      SACNT = IPT
      SAOR(1) = SACNT
C
C     find data sets
      CALL WDFNDS ( WDMFL, DSTYPE, SACNT, SAIND, SATYP, SABEG,
     I              SAVAL, SALEN, SAIMN, SAIMX, SARMN, SARMX,
     I              SACOND, SAOR, DSNMAX, DSNRMN, DSNRMX,
     M              DSNFND, DSNKNT,
     O              DSNFUL )
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGL
     I                   ( WDMSFL, DSN,
     O                     LAT, LNG, RETCOD )
C
C     + + + PURPOSE + + +
C     Retrieve latitude and longitude from a wdm data set.  The
C     subroutine will first attempt to retrieve decimal degrees from
C     attributes LATDEG (54) and LNGDEG (55).  If there is a problem
C     with either LATDEG or LNGDEG, the program will look for
C     attributes LATDMS (8) and LNGDMS (9) and convert them to 
C     decimal degrees.  If there is a problem with either LATDMS or
C     LNGDMS, the program will look for attributes LATCTR (96) and
C     LNGCTR (97).  If none of these attributes are present, the
C     program will return 0.0 for LAT and LNG and the RETCOD from
C     the attribute retrieval that failed.
C
C     + + + HISTORY + + +
C     kmf - Nov 26, 1996
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, RETCOD
      REAL      LAT, LNG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the wdm file
C     DSN    - data set in wdm
C     LAT    - latitude, in decimal degrees
C     LNG    - longitude, in decimal degrees
C     RETCOD - return code
C                0 - attribute value returned
C               -81 - data set does not exist
C              -107 - attribute not present on this data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INDX, LEN, RETC, LATDMS, LNGDMS
      REAL      LATDEG, LNGDEG
C
C     + + + FUNCTIONS + + +
      INTEGER   CNVTDG
C
C     + + + EXTERNALS + + +
      EXTERNAL   CNVTDG, WDBSGR, WDBSGI
C
C     + + + END SPECIFICATIONS + + +
C
      LEN = 1
C
C     look for latitude in decimal degree
      INDX = 8
      CALL WDBSGR ( WDMSFL, DSN, INDX, LEN, LATDEG, RETC )
      IF (RETC .EQ. 0) THEN
C       found latitude, get longitude in decimal degree
        INDX = 9
        CALL WDBSGR ( WDMSFL, DSN, INDX, LEN, LNGDEG, RETC )
      END IF
      IF (RETC .EQ. 0) THEN
C       successfully retrieved lat & long in decimal degrees
        LAT = LATDEG
        LNG = LNGDEG
        RETCOD = 0
      ELSE
C       problem retrieving lat/lng in decimal degrees, try dddmmss
        INDX = 54
        CALL WDBSGI ( WDMSFL, DSN, INDX, LEN, LATDMS, RETC )
        IF (RETC .EQ. 0) THEN
C         found latitude, get longitude in dddmmss
          INDX = 55
          CALL WDBSGI ( WDMSFL, DSN, INDX, LEN, LNGDMS, RETC )
        END IF
        IF (RETC .EQ. 0) THEN
C         successfully retrieved lat & long in dddmmss, convert
          LAT = CNVTDG ( LATDMS )
          LNG = CNVTDG ( LNGDMS )
          RETCOD = 0
        ELSE
C         last chance, look for center lat & long, decimal degrees
          INDX = 96
          CALL WDBSGR ( WDMSFL, DSN, INDX, LEN, LATDEG, RETC )
          IF (RETC .EQ. 0) THEN
C           found center lat, get long in decimal degrees
            INDX = 97
            CALL WDBSGR ( WDMSFL, DSN, INDX, LEN, LNGDEG, RETC )
          END IF
          IF (RETC .EQ. 0) THEN
C           successfully retrieved center lat & long
            LAT = LATDEG
            LNG = LNGDEG
            RETCOD = 0
          ELSE
C           unable to retieve any lat & long, return 0.0
            LAT = 0.0
            LNG = 0.0
            RETCOD = RETC
          END IF
        END IF
      END IF
C      
      RETURN
      END
