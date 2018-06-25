C
C
C
      SUBROUTINE   WDTGET
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTRAN,QUALFG,TUNITS,
     O                     RVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     gets timeseries information from the WDMSFL
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTRAN,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for get
C     DATES  - starting date
C     NVAL   - number of values
C     DTRAN  - transformation code
C              0 - ave,same
C              1 - sum,div
C              2 - max
C              3 - min
C     QUALFG - allowed quality code
C     TUNITS - time units for get
C     RVAL   - array to place retrieved values in
C     RETCOD - return code
C                0 - everything O.K.
C               -8 - invalid date
C              -14 - date specified not within valid range for data set
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG,GPOSEN,GPIND,LTSTEP,LTUNIT,TDSFRC,TGROUP,TSPTAD,
     1          ENDDAT(6),GPSDAT(6),GETDAT(6),TSPSC1,TSPSC2,
     2          COMPFG,TSFORM,VBTIME,TSSTEP,TCODE,GETQK,RIND,
     3          LWDMFL,LDSN
      INTEGER*4 I4NVAL
      REAL      DEFVAL,TOLR,TSFILL,GETQRA,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPR, WTPMCK, WTFNDG, WDATCP, WTGTVL, WDRCGO, WTDSPX
      EXTERNAL  WTSCSC,WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I4NVAL= NVAL
      LTSTEP= DELT
      LTUNIT= TUNITS
      GPFLG = 1
      TSFILL= 0.0
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C
C     check the user supplied parameters
      CALL WTPMCK (GPFLG,DTRAN,DATES,NVAL,QUALFG,
     M             LTSTEP,LTUNIT,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       check the data set and figure out which groups have been req.
        CALL WTFNDG (LWDMFL,LDSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O               TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     1               GPIND,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END IF
C     fill in RVAL with defaults
      DEFVAL= TSFILL
C     max
      IF (DTRAN.EQ.2) DEFVAL= -1.0E30
C     min
      IF (DTRAN.EQ.3) DEFVAL= 1.0E30
      CALL ZIPR (NVAL,DEFVAL,
     O           RVAL)
C
      IF (RETCOD.EQ.0) THEN
C       get additional parameters
        RIND= WDRCGO(LWDMFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        GETQK= 0
C       can we do a quick get?
        IF (VBTIME.EQ.1) THEN
C         yes, if time units and step ok
          IF (TCODE.LE.4.AND.LTUNIT.LE.4) THEN
C           time units days or shorter, a quick get may work
            CALL WTSCSC (LTUNIT,LTSTEP,TSPSC1)
            CALL WTSCSC (TCODE,TSSTEP,TSPSC2)
            GETQRA= 1.0E-8+ FLOAT(TSPSC2)/FLOAT(TSPSC1)
            RTMP  = GETQRA
            IF (RTMP.LT.1.0) THEN
C             wdm interval less than user interval
              RTMP= 1.0/ GETQRA
            END IF
            IF (MOD(RTMP,1.0).LT.1.0E-6) THEN
C             ok to do a quick get
              GETQK= 1
            END IF
          ELSE IF (TCODE.EQ.LTUNIT) THEN
C           time units are the same, a quick get will work
            GETQK = 1
            GETQRA= 1.0E-8+ FLOAT(TSSTEP)/FLOAT(LTSTEP)
          END IF
        ELSE
C         do a general get
          GETQK= 0
        END IF
C       make a working copy of the starting date
        CALL WDATCP (DATES,GETDAT)
C       get the data
        CALL WTGTVL (LWDMFL,LDSN,GPOSEN,NVAL,LTUNIT,LTSTEP,DTRAN,
     I               QUALFG,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I               GETQK,GETQRA,VBTIME,
     M               RVAL,GETDAT,GPSDAT,GPIND,
     O               RETCOD)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSCSC
     I                    (TUNITS,TSSTEP,
     I                     TSPSEC)
C
C     + + + PURPOSE + + +
C     converts the given time units and time step to seconds
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TUNITS,TSSTEP,TSPSEC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TUNITS - time units
C     TSSTEP - time step
C     TSPSEC - time in seconds
C
C     + + + END SPECIFICATIONS + + +
C
      TSPSEC= TSSTEP
      GO TO (40,30,20,10), TUNITS
 10   CONTINUE
C       day units to hours
        TSPSEC= TSPSEC* 24
 20   CONTINUE
C       hours to minutes
        TSPSEC= TSPSEC* 60
 30   CONTINUE
C       minutes to seconds
        TSPSEC= TSPSEC* 60
 40   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTPMCK
     I                    (GPFLG,DXX,DATES,NVAL,QUALVL,
     M                     LTSTEP,LTUNIT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     checks the parameters supplied to either WDTPUT or WDTGET
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GPFLG,LTSTEP,DXX,DATES(6),NVAL,QUALVL,LTUNIT,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GPFLG  - get/put flag
C              1 - get
C              2 - put
C     DXX    - transform or overwrite flag
C     DATES  - starting date
C     NVAL   - number of values
C     QUALVL - quality of data code
C     LTSTEP - time step
C     LTUNIT - time units
C     RETCOD - return code
C                0 - everything is O.K.
C               -8 - invalid date
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG,I,CONV(7)
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  TSBINI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CONV/60,60,24,999,12,100,999/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
C     may need to initialize time-series buffer stuff
      CALL TSBINI
C
C     check dates
      IF (DATES(1).LT.1.OR.DATES(1).GT.32000  .OR.
     1    DATES(2).LT.1.OR.DATES(2).GT.12     .OR.
     2    DATES(3).LT.1.OR.DATES(3).GT.31     .OR.
     3    DATES(4).LT.0.OR.DATES(4).GT.24     .OR.
     4    DATES(5).LT.0.OR.DATES(5).GT.59     .OR.
     5    DATES(6).LT.0.OR.DATES(6).GT.59) RETCOD= -8
C     check DXX (DTRAN for get, DTOVWR for put)
      IF (DXX.LT.0.OR.(GPFLG.EQ.1.AND.DXX.GT.3).OR.
     1    (GPFLG.EQ.2.AND.DXX.GT.0)) THEN
C       bad parameter
        RETCOD= -20
      END IF
C     check other parameters
      IF (NVAL.LT.1.OR.
     1    LTUNIT.LT.1.OR.LTUNIT.GT.7.OR.
     2    QUALVL.LT.0.OR.QUALVL.GT.31) THEN
C       bad parameter
        RETCOD= -20
      END IF
      IF (RETCOD.EQ.0) THEN
C       check LTSTEP with regard to specified LTUNIT
        I= LTUNIT
        DONFG= 0
 10     CONTINUE
          IF (MOD(LTSTEP,CONV(I)).EQ.0) THEN
            LTSTEP= LTSTEP/CONV(I)
            I= I+ 1
            LTUNIT= I
          ELSE
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 10
        IF (LTSTEP.LT.1.OR.LTSTEP.GT.63) THEN
C         bad parameter
          RETCOD= -20
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTFNDG
     I                    (WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O                     TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     O                     GPOSST,GPOSEN,GPSDAT,ENDDAT,RETCOD)
C
C     + + + PURPOSE + + +
C     check the data set,
C     computes ending date, start and end group pointers and
C     number of values to skip in first group
C
C     + + + HISTORY + + +
C     kmf May 25, 2001 - corrected problem reading last group in a
C                        time series data set.  Comparison between
C                        GRPOFF and TGRNUM changed from .GE. to .GT.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GPFLG,DATES(6),LTSTEP,LTUNIT,
     1          TDSFRC,TGROUP,TSPTAD,GPOSST,GPOSEN,
     2          ENDDAT(6),GPSDAT(6),RETCOD
      INTEGER*4 I4NVAL
      REAL      TSFILL,TOLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C     GPFLG  - get/put flag
C              1 - get
C              2 - put
C     DATES  - starting date
C     LTSTEP - time step
C     LTUNIT - time units
C     I4NVAL - number of values
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     TOLR   - compression tolerance
C     TSPTAD - put data transform code
C     GPOSST - start data group pointer index
C     GPOSEN - end data group pointer index
C     GPSDAT - start date of first group
C     ENDDAT - end date for get or put
C     RETCOD - return code
C                0 - everything O.K.
C              -14 - date specified not within valid range for data set
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,TBSDAT(6),TGRPST,RIND,PDAT,PDATV,DSTYP,GRCNT
      INTEGER*4 GRPOFF,TGRNUM
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSCHA, WDRCGO, WTDSPM, TIMADD, TIMCNV, WTSGRP, TIMDIF
      EXTERNAL  TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
C     check basics about data set
      DSTYP= 1
      CALL WDSCHA (WDMSFL,DSN,DSTYP,GPFLG,
     O             TDSFRC,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       get data-set parameters
        RIND= WDRCGO(WDMSFL,TDSFRC)
        CALL WTDSPM (WDMSFL,WIBUFF(1,RIND),WRBUFF(1,RIND),
     O               TSFILL,TGROUP,TOLR,TBSDAT,TSPTAD)
C
C       calculate pointer to first group pointer
        PDAT  = WIBUFF(11,RIND)
        TGRPST= PDAT+ 2
C
C       calculate max number of group pointers
        PDATV = WIBUFF(12,RIND)
        TGRNUM= PDATV- PDAT- 2
C
C       calculate ending date
        CALL TIMADD (DATES,LTUNIT,LTSTEP,I4NVAL,
     O               ENDDAT)
C
C       convert end date to old date format
        CALL TIMCNV (ENDDAT)
C
C       determine beginning of first group
        CALL WTSGRP (DATES,TGROUP,
     O               GPSDAT)
C
        IF (TIMCHK(TBSDAT,GPSDAT).EQ.-1) THEN
C         base year after start of data to add
          GRPOFF= -1
        ELSE
C         determine starting pointer index
          I= 1
          CALL TIMDIF (TBSDAT,GPSDAT,TGROUP,I,
     O                 GRPOFF)
        END IF
Ckmf    IF (GRPOFF.LT.0 .OR. GRPOFF.GE.TGRNUM) THEN
        IF (GRPOFF.LT.0 .OR. GRPOFF.GT.TGRNUM) THEN
C         date specified not within valid range for data set
          RETCOD= -14
        END IF
        GPOSST= TGRPST+ GRPOFF
C
C       determine ending pointer index
        I= 1
        CALL TIMDIF (TBSDAT,ENDDAT,TGROUP,I,
     O               GRPOFF)
Ckmf    IF (GRPOFF.LT.0 .OR. GRPOFF.GE.TGRNUM) THEN
        IF (GRPOFF.LT.0 .OR. GRPOFF.GT.TGRNUM) THEN
C         date specified not within valid range for data set
          RETCOD= -14
        ELSE
C         do we end exactly on boundary
          CALL WTSGRP (ENDDAT,TGROUP,
     O                 TBSDAT)
          IF (TIMCHK(ENDDAT,TBSDAT).EQ.0) THEN
C           yes, we do
            GRPOFF= GRPOFF- 1
          END IF
          GPOSEN= TGRPST+ GRPOFF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDSPM
     I                    (WDMSFL,TIBUFF,TRBUFF,
     O                     TSFILL,TGROUP,TOLR,TBSDAT,TSPTAD)
C
C     + + + PURPOSE + + +
C     obtains values for a variety of TIMSER parms from labels
C     or defaults
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TGROUP,TBSDAT(6),TSPTAD
      INTEGER*4 TIBUFF(512)
      REAL      TRBUFF(512),TSFILL,TOLR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     TIBUFF - data-set label - integer version
C     TRBUFF - data-set label - decimal version
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     TOLR   - compression tolerance
C     TBSDAT - beginning date of data
C     TSPTAD - put data transform code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,SAIND,POS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV, WTBYFX
C
C     + + + END SPECIFICATIONS + + +
C
C     set default dates
      TBSDAT(1)= -999
      TBSDAT(2)= 1
      TBSDAT(3)= 1
      TBSDAT(4)= 0
      TBSDAT(5)= 0
      TBSDAT(6)= 0
C
C     get beginning date pointers
      DO 10 I= 1,4
        SAIND= I+ 26
        POS  = WDSASV(SAIND,TIBUFF)
        IF (POS.GT.0) THEN
C         from label
          TBSDAT(I)= TIBUFF(POS)
        END IF
 10   CONTINUE
C
C     get missing data filler code
      SAIND= 32
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TSFILL= TRBUFF(POS)
      ELSE
C       default
        TSFILL= 0.0
      END IF
C
C     get units for data group pointer
      SAIND= 34
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TGROUP= TIBUFF(POS)
      ELSE
C       default
        TGROUP= 6
      END IF
C
C     get compression tolerance
      SAIND= 36
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TOLR= TRBUFF(POS)
      ELSE
C       default
        TOLR= 1.0E-8
      END IF
C
C     get put transform code
      SAIND= 60
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       from label
        TSPTAD= TIBUFF(POS)
      ELSE
C       default
        TSPTAD= 0
      END IF
C
      IF (TBSDAT(1).EQ.-999) THEN
C       missing base year attribute, add it
        CALL WTBYFX (WDMSFL,TIBUFF(5),TGROUP,
     O               TBSDAT(1))
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBYFX
     I                   (WDMSFL,DSN,TGROUP,
     O                    TBSYR)
C
C     + + + PURPOSE + + +
C     add base year attribute to a timeseries data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,TGROUP,TBSYR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C     TGROUP - data group pointer units
C     TBSYR  - base year
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TDSFRC,RETCOD,RIND,PDAT,DPCNT,I,
     1          GPIND,GPPTR,GREC,GOFF,GIND,GPDAT(6),BASDAT(6),GVAL,
     2          SAIND,SATYP,SALEN,PSAVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK,WDSASP,WDRCGO,WDRCUP,TIMDIF,WDATSP,WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
C     determine where data set starts
      CALL WDDSCK (WDMSFL,DSN,
     O             TDSFRC,RETCOD)
C     bring label into memory
      RIND= WDRCGO(WDMSFL,TDSFRC)
C     where does data start
      PDAT= WIBUFF(11,RIND)
C     how many data groups are present
      DPCNT= WIBUFF(PDAT,RIND)
      IF (DPCNT.GT.0) THEN
C       data is present, find out what base year was assumed to be
        GPPTR= 0
        GPIND= PDAT+ 1
C       loop to look for data
 10     CONTINUE
          GPIND= GPIND+ 1
          IF (WIBUFF(GPIND,RIND).GT.0) THEN
C           data exist in this group
            GPPTR= WIBUFF(GPIND,RIND)
C           find where group starts
            CALL WDPTSP (GPPTR,
     O                   GREC,GOFF)
            GIND= WDRCGO(WDMSFL,GREC)
C           find group where data begins
            CALL WDATSP (WIBUFF(GOFF,GIND),
     O                   GPDAT)
C           set other dates
            GPDAT(5) = 0
            GPDAT(6) = 0
            BASDAT(1)= 1899
            BASDAT(2)= 1
            BASDAT(3)= 1
            BASDAT(4)= 0
            BASDAT(5)= 0
            BASDAT(6)= 0
            I =1
            CALL TIMDIF (BASDAT,GPDAT,TGROUP,I,
     O                   GVAL)
C           convert units to groups
            GPIND= GPIND- PDAT- 2
            IF (GPIND.EQ.GVAL) THEN
C             old case
              TBSYR= 1899
            ELSE
C             new case
              TBSYR= 1900
            END IF
          END IF
        IF (GPPTR.EQ.0) GO TO 10
      ELSE
C       no data, default base year
        TBSYR= 1900
      END IF
C     write base year attribute on label to avoid this next time
      SAIND= 27
      SALEN= 1
      SATYP= 1
C     be sure label in memory
      RIND= WDRCGO(WDMSFL,TDSFRC)
C     where do we put it?
      CALL WDSASP (SAIND,SALEN,SATYP,
     M             WIBUFF(1,RIND),
     O             PSAVAL,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       added attribute space successfully, fill in value
        WIBUFF(PSAVAL,RIND)= TBSYR
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDSPX
     I                    (TIBUFF,
     O                     COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
C
C     + + + PURPOSE + + +
C     obtains values for a variety of timeseries parms from labels
C     or defaults
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   COMPFG,TSFORM,VBTIME,TSSTEP,TCODE
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TIBUFF - data-set label - integer version
C     COMPFG - compression flag
C     TSFORM - form of data
C     VBTIME - variable time step option
C     TSSTEP - timeseries timestep
C     TCODE  - timeseries time code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAIND,POS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
C     get compression flag
      SAIND= 83
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        COMPFG= TIBUFF(POS)
C       write(99,*) 'compfg from label:',COMPFG
      ELSE
C       default to compressed
        COMPFG= 1
      END IF
C
C     form of data
      SAIND= 84
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        TSFORM= TIBUFF(POS)
      ELSE
C       default to mean
        TSFORM= 1
      END IF
C
C     variable time step option
      SAIND= 85
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        VBTIME= TIBUFF(POS)
      ELSE
C       default to time step may vary
        VBTIME= 2
      END IF
C
C     time step
      SAIND= 33
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        TSSTEP= TIBUFF(POS)
      ELSE
C       no default
        TSSTEP= -999
      END IF
C
C     time units
      SAIND= 17
      POS  = WDSASV(SAIND,TIBUFF)
      IF (POS.GT.0) THEN
C       available from label
        TCODE= TIBUFF(POS)
      ELSE
C       no default
        TCODE= -999
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSKVL
     I                    (WDMSFL,GPIND,GPSDAT,STDAT,
     I                     TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                     CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     skips values within a WDMSFL timeseries group, also
C     outputs current block information in common CWTSDS, also
C     end of group date
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GPIND,GPSDAT(6),STDAT(6),RETCOD,TDSFRC,TGROUP,
     1          BADJFG,ADDAFG,VBTIME,CURREC,
     2          CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,CURDAT(6)
      INTEGER*4 CURNOV,CURCNT
      REAL      TSFILL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     GPIND  - group index number
C     GPSDAT - starting date of group
C     STDAT  - date to skip to
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     BADJFG - block adjustment for efficiency flag
C              0 - no
C              1 - yes
C     ADDAFG - data present flag
C     VBTIME - variable timestep indicator
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURCNT - current position within block
C     CURDAT - current date of start of current value
C     RETCOD - return code
C                  0 - everything O.K.
C                -10 - no data in this group
C                -11 - no non missing data, data has not started yet
C                -21 - date from WDM doesn't match expected date
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   BLEDAT(6),BLSDAT(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WTSKVX
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WTSKVX (WDMSFL,GPIND,GPSDAT,STDAT,
     I             TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O             CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O             CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O             RETCOD,BLSDAT,BLEDAT)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSKVX
     I                    (WDMSFL,GPIND,GPSDAT,STDAT,
     I                     TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                     CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                     RETCOD,BLSDAT,BLEDAT)
C
C     + + + PURPOSE + + +
C     skips values within a WDMSFL TIMESERIES group, also
C     returns current block information and end of group date
C     can fill an empty group with dummy data if ADDAFG=1
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GPIND,GPSDAT(6),STDAT(6),
     1          RETCOD,BADJFG,ADDAFG,VBTIME,
     2          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     3          CURDAT(6),TDSFRC,TGROUP,BLSDAT(6),BLEDAT(6)
      INTEGER*4 CURNOV,CURCNT
      REAL      TSFILL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     GPIND  - group index number
C     GPSDAT - starting date of group
C     STDAT  - data to skip to
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     BADJFG - block adjustment for efficiency flag
C              0 - no
C              1 - yes
C     ADDAFG - data present flag
C     VBTIME - variable timestep indicator
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURCNT - current position within block
C     CURDAT - current date of start of current value
C     RETCOD - return code
C                  0 - everything O.K.
C                -10 - no data in this group
C                -11 - no non missing data, data has not started yet
C                -21 - date from WDM doesn't match expected date
C     BLSDAT - block start date array
C     BLEDAT - block end date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DIND,RIND,CHK,PDAT,TSPREC,SAIND,I,GTUNIT,GTSTEP,
     1          TDAT(6),EGPFLG,GPEDAT(6),TSTDAT(6),MINQUA,POS
      INTEGER*4 GPPTR,BCW,I4ZRO,NUMSKP,I4ONE,TDFREE,GVAL,LVAL,I4TMP,
     1          SVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,TIMCHK,WDSASV,WDRCGX
      INTEGER*4 WDPTCL,WDATCL,WBCWCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDSASV, WDPTSP, WDRCGX, WDPTCL, WDATCL, WTEGRP
      EXTERNAL   TIMDIF, TIMADD, TIMCHK, WBCWCL, WTNWBK, WDRCUP, WDATSP
      EXTERNAL   WDSKBK, WBCWSP, WBCWSQ, WDATCP
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I4ZRO = 0
      I4ONE = 1
      DIND  = WDRCGO (WDMSFL,TDSFRC)
      GPPTR = WIBUFF(GPIND,DIND)
      IF (GPPTR.EQ.I4ZRO) THEN
        IF (ADDAFG.EQ.0) THEN
C         no data in this group
          RETCOD= -10
        ELSE
C         this group doesnt exist yet, create it with dummy data
C         time step
          SAIND= 33
          POS  = WDSASV(SAIND,WIBUFF(1,DIND))
          IF (POS.GT.0) THEN
C           from label
            GTSTEP= WIBUFF(POS,DIND)
          ELSE
C           default
            GTSTEP= 1
          END IF
C         figure out where free space starts
          PDAT  = WIBUFF(11,DIND)
          TDFREE= WIBUFF(PDAT+1,DIND)
C         get pointer record flag from label if available
          SAIND = 31
          I     = WDSASV(SAIND,WIBUFF(1,DIND))
          IF (I.GT.0) THEN
C           from label
            TSPREC= WIBUFF(I,DIND)
          ELSE
C           default pointer new record flag to no
            TSPREC= 0
          END IF
          CALL WDPTSP (TDFREE,
     O                 CURREC,CURPOS)
          IF (TSPREC.EQ.1.OR.CURPOS.GT.510.OR.CURPOS.EQ.0) THEN
C           the new group will start on a new record, it follows current
            I   = 0
            RIND  = WDRCGX(WDMSFL,I,CURREC)
            CURPOS= 5
            CURREC= RECNO(RIND)
          END IF
C         calc the group pointer to go into the directory later
          GPPTR = WDPTCL(CURREC,CURPOS)
C         fill in date field and undefined values in the new group
          RIND  = WDRCGO(WDMSFL,CURREC)
          WIBUFF(CURPOS,RIND)= WDATCL(GPSDAT)
          CURBKS= CURPOS+ 1
          CURPOS= CURBKS+ 1
          CURCMP= 1
          CURQUA= 31
          IF (VBTIME.NE.2) THEN
C           need constant interval data
            GTUNIT= ADDAFG
          ELSE
C           use most efficient units
            GTUNIT= TGROUP
            GTSTEP= 1
          END IF
C         find end of group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
 10       CONTINUE
C           find number of intervals in group
            CALL TIMDIF (GPSDAT,GPEDAT,GTUNIT,GTSTEP,
     O                   GVAL)
C           be sure we end on a group boundary
Ckmf
Ckmf  write(99,*) '      wtskvx: gpsdat =', gpsdat
Ckmf  write(99,*) '                tdat =',  tdat
Ckmf  write(99,*) '              gtunit =', gtunit
Ckmf  write(99,*) '              gtstep =', gtstep
Ckmf  write(99,*) '                gval =',  gval
Ckmf  write(99,*) ' -->  timadd'
            CALL TIMADD (GPSDAT,GTUNIT,GTSTEP,GVAL,
     O                   TDAT)
Ckmf  write(99,*) ' <--  timadd:   tdat =', tdat
C           check boundary of starting date
            CALL TIMDIF (GPSDAT,STDAT,
     O                   GTUNIT,GTSTEP,SVAL)
            CALL TIMADD (GPSDAT,GTUNIT,GTSTEP,SVAL,
     O                   TSTDAT)
            EGPFLG= 0
            IF (TIMCHK(TDAT,GPEDAT).NE.0.OR.
     1          TIMCHK(TSTDAT,STDAT).NE.0) THEN
C             oops, did not end on a group boundary
C             or data start not on boundary
              EGPFLG= 1
              GTUNIT= GTUNIT- 1
            END IF
          IF (EGPFLG.EQ.1) GO TO 10
C         fill new group with dummy values
 20       CONTINUE
            LVAL = GVAL
            I4TMP= 32767
            IF (LVAL.GT.I4TMP) LVAL= I4TMP
            WIBUFF(CURBKS,RIND)=WBCWCL(LVAL,GTSTEP,GTUNIT,CURCMP,CURQUA)
            WRBUFF(CURPOS,RIND)= TSFILL
            CURPOS= CURPOS+ 1
            CALL WTNWBK (WDMSFL,
     M                   CURREC,CURPOS,
     O                   CURBKS)
            RIND= WDRCGO(WDMSFL,CURREC)
            GVAL= GVAL- LVAL
          IF (GVAL.GT.I4ZRO) GO TO 20
          CALL WDRCUP(WDMSFL,RIND)
C         update the directory
          DIND  = WDRCGO(WDMSFL,TDSFRC)
          PDAT  = WIBUFF(11,DIND)
C         update active group counter
          WIBUFF(PDAT,DIND)  = WIBUFF(PDAT,DIND)+ 1
C         update the data-set free space pointer
          TDFREE= WDPTCL(CURREC,CURBKS)
          WIBUFF(PDAT+1,DIND)= TDFREE
C         pointer to this groups data
          WIBUFF(GPIND,DIND) = GPPTR
C         write out the revised directory
          CALL WDRCUP(WDMSFL,DIND)
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       split up the pointer into record and offset
        CALL WDPTSP (GPPTR,
     O               CURREC,CURPOS)
C       get the starting record of the group
        DIND= WDRCGO (WDMSFL,CURREC)
C       check date from WDMS with expected date
        CALL WDATSP (WIBUFF(CURPOS,DIND),
     O               BLSDAT)
        BLSDAT(5)= 0
        BLSDAT(6)= 0
        IF (TIMCHK(BLSDAT,GPSDAT).NE.0) THEN
C         date from WDM doesn't match expected date
          RETCOD= -21
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       increment the offset to position of first BCW
        CURBKS= CURPOS
        NUMSKP= 1
        MINQUA= 31
 30     CONTINUE
C         skip to next BCW
          CALL WDSKBK (WDMSFL,NUMSKP,
     M                 CURREC,CURBKS)
C         be sure record is in buffer
          DIND= WDRCGO (WDMSFL,CURREC)
C         get the BCW
          BCW= WIBUFF(CURBKS,DIND)
C         split up the BCW
          IF (BADJFG.EQ.0) THEN
            CALL WBCWSP (BCW,
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
          ELSE
            CALL WBCWSQ (BCW,
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
          END IF
          IF (CURQUA.LT.MINQUA) MINQUA= CURQUA
C         find the end of this block
          CALL TIMADD (BLSDAT,CURTUN,CURTST,CURNOV,
     O                 BLEDAT)
C         are we before of after the start we are after
          CHK= TIMCHK (BLEDAT,STDAT)
          IF (CHK.GE.0) THEN
C           more values to skip, prepare to skip to next block
            NUMSKP= 2
            IF (CURCMP.EQ.0) THEN
C             uncompressed, skip number of values + bcw
              NUMSKP= CURNOV+ 1
            END IF
            CALL WDATCP (BLEDAT,BLSDAT)
          ELSE
C           we are in the block we want
C           recalc BCW without adjustment JLK 3/18/87
            CALL WBCWSP (BCW,
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
C           set NUMSKP to stop skipping
            NUMSKP= 0
C           figure out where in block
            CALL TIMDIF (BLSDAT,STDAT,CURTUN,CURTST,
     O                   CURCNT)
            IF (CURCNT.LT.0) THEN
C             reset boundary case
              CURCNT= 0
            END IF
C           figure out which date we are on
            CALL TIMADD (BLSDAT,CURTUN,CURTST,CURCNT,
     O                   CURDAT)
C           update the current position if on boundary
            CHK= TIMCHK(CURDAT,STDAT)
            IF (CHK.GE.0) THEN
C             reset boundary case
              CURCNT= CURCNT+ 1
            END IF
            IF (CURCNT.GT.CURNOV) THEN
C             we are at the beginning of next block, skip to it
              NUMSKP= 2
              IF (CURCMP.EQ.0) THEN
C               uncompressed, skip number of values + bcw
                NUMSKP= CURNOV+ 1
              END IF
              CALL WDATCP (BLEDAT,BLSDAT)
            END IF
          END IF
        IF (NUMSKP.NE.I4ZRO) GO TO 30
C       calculate the current value
        CURPOS= CURBKS+ 1
        IF (CURCMP.EQ.0) THEN
C         where is the current value
          CURPOS= CURBKS+ CURCNT
        END IF
        CURVAL= WRBUFF(CURPOS,DIND)
C       calculate the previous value
        IF (CURCNT.GT.I4ONE) THEN
C         skip ahead
          NUMSKP= CURBKS+ 1
          IF (CURCMP.EQ.0) THEN
C           where is the previous value
            NUMSKP= CURBKS+ CURCNT- 1
          END IF
          PREVAL= WRBUFF(NUMSKP,DIND)
        ELSE
C         don't know where previous value is
          PREVAL= TSFILL
        END IF
        IF (ADDAFG.EQ.0.AND.MINQUA.EQ.31) THEN
C         no non missing data, data has not started yet
          RETCOD= -11
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSKBK
     I                    (WDMSFL,NUMSKP,
     M                     CURREC,CURPOS)
C
C     + + + PURPOSE + + +
C     skips to next WDMSFL block
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,CURREC,CURPOS
      INTEGER*4 NUMSKP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     NUMSKP - number of elements to skip
C     CURREC - current record number
C     CURPOS - current position within record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      CURPOS= CURPOS+ NUMSKP
      IF (CURPOS.EQ.512) THEN
C       force new record
        CURPOS= 513
      END IF
      IF (CURPOS.GT.512) THEN
C       new record needed
 10     CONTINUE
          RIND= WDRCGO(WDMSFL,CURREC)
C         get the pointer to next record
          CURREC= WIBUFF(4,RIND)
          CURPOS= CURPOS- 508
        IF (CURPOS.GT.512) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGTVL
     I                    (WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     I                     QUALFG,ENDDAT,TDSFRC,TSFILL,TGROUP,
     I                     GETQK,GETQRA,VBTIME,
     M                     RVAL,GETDAT,GPSDAT,GPIND,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     fills in RVAL array with data values from WDMS DSN
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    WDMSFL,DSN,GPOSEN,NVAL,GTTUN,GTTST,GTTRN,
     1           QUALFG,ENDDAT(6),TDSFRC,TGROUP,GETQK,
     1           VBTIME,GETDAT(6),GPSDAT(6),GPIND,RETCOD
      REAL       RVAL(NVAL),TSFILL,GETQRA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data set number
C     GPOSEN - end data group pointer index
C     NVAL   - number of values
C     GTTUN  - get time units
C     GTTST  - get time step
C     GTTRN  - get transformation code
C     QUALFG - get quality code
C     ENDDAT - end of get date array
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     GETQK  - do a quick get
C     GETQRA - quick get time step ratio (user/dsn)
C     VBTIME - variable timestep indicator
C     RVAL   - array of values retrieved from WDMS file
C     GETDAT - current get date array
C     GPSDAT - start date of first group
C     GPIND  - get group index number
C     RETCOD - return code
C                  0 - everything O.K.
C                -21 - date from WDM doesn't match expected date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwtsds.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CURNXT(6),GETNXT(6),TMPNXT(6),WDADD,GTADD,IONE,CHK,ICNT,
     1          CHKS,TMPTUN,GPEDAT(6),NEWGRP,BADJFG,ADDAFG,EGPOS,TMPOS,
     2          I,TMPDAT(6)
      INTEGER*4 GETSPN,TMPSPN,CURSPN,I4ONE,I4NVAL,DPOS
      REAL      FRAC,CFRAC,RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT,ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDATCP, WTEGRP, WTSKVL, WTGTNV, TIMADD, TIMDIF, TIMCHK
C
C     + + + END SPECIFICATIONS + + +
C
C     always calculate ending intervals on first time through
      WDADD = 1
      GTADD = 1
      NEWGRP= 1
      IONE  = 1
      I4ONE = 1
      DPOS  = 1
      BADJFG= 1
      ADDAFG= 0
      I4NVAL= NVAL
      FRAC  = 0.0
      CFRAC = 0.0
      EGPOS = 0
      CALL WDATCP (GETDAT,TMPDAT)
C
 10   CONTINUE
        IF (NEWGRP.GE.1) THEN
C         find out the end of the group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
          IF (GPIND.EQ.GPOSEN) THEN
C           this is the last group, dont fill too far
            CALL WDATCP (ENDDAT,GPEDAT)
          END IF
C         skip values in group as required
          CALL WTSKVL (WDMSFL,GPIND,GPSDAT,TMPDAT,
     I                 TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O                 CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                 CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                 RETCOD)
          IF (RETCOD.EQ.-11) THEN
C           data has not started yet, this is ok
            RETCOD= 0
          END IF
C         how many intervals in group
          CALL TIMDIF (TMPDAT,GPEDAT,GTTUN,GTTST,
     O                 TMPOS)
          EGPOS = EGPOS+ TMPOS
          IF (GETQK.NE.0.AND.EGPOS.EQ.TMPOS) THEN
C           first quick get, be sure to use correct boundary
            CALL TIMDIF (CURDAT,GETDAT,GTTUN,GTTST,
     O                   GETQK)
            GETQK= GETQK+ 1
          END IF
          NEWGRP= 0
        END IF
        IF (RETCOD.EQ.0) THEN
          IF (WDADD.EQ.1) THEN
C           calculate new ending date on WDMS date
C           get the next WDS value
            CALL WTGTNV (WDMSFL,
     M                   CURCNT,CURNOV,CURCMP,CURREC,CURBKS,CURTST,
     1                   CURTUN,CURQUA,CURPOS,CURDAT,
     O                   CURVAL,CURNXT)
            TMPTUN= GTTUN
            IF (CURTUN.LT.TMPTUN) TMPTUN= CURTUN
          END IF
          IF (GETQK.EQ.0) THEN
C           not a quick get, do it all
            IF (GTADD.EQ.1) THEN
C             calculate new ending date for RVAL
              CALL TIMADD (GETDAT,GTTUN,GTTST,I4ONE,
     O                     GETNXT)
C             how many short units in get and WDMS block
              CALL TIMDIF (GETDAT,GETNXT,TMPTUN,IONE,
     O                     GETSPN)
              IF (GTTRN .LE. 1) THEN
C               some sort of data available, so initialize to zero
                RVAL(DPOS) = 0.0
              END IF
            END IF
C
C           figure out which interval should be incremented
            CHK= TIMCHK(GETNXT,CURNXT)
            IF (CHK.EQ.1) THEN
C             add interval to get counter
              GTADD= 1
              WDADD= 0
            ELSE IF (CHK.EQ.0) THEN
C             add intervals to both counters
              GTADD= 1
              WDADD= 1
            ELSE
C             add interval to WDMS fill counter
              GTADD= 0
              WDADD= 1
            END IF
C
            IF (QUALFG.GE.CURQUA) THEN
C             only process data of acceptable quality
              IF (GTTRN.LE.1) THEN
C               store end of this interval
                IF (CHK.EQ.1) THEN
C                 interval ends due to get
                  CALL WDATCP (GETNXT,TMPNXT)
                ELSE
C                 interval ends due to wdm or both
                  CALL WDATCP (CURNXT,TMPNXT)
                END IF
C               calc short units to end of interval
                CHKS= TIMCHK(GETDAT,CURDAT)
                IF (CHKS.EQ.1) THEN
                  CALL TIMDIF (CURDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                ELSE
                  CALL TIMDIF (GETDAT,TMPNXT,TMPTUN,IONE,
     O                         TMPSPN)
                END IF
C
                IF (GTTRN.EQ.0) THEN
C                 transform is ave,same
                  FRAC= FLOAT(TMPSPN)/FLOAT(GETSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
                  CFRAC= CFRAC+ FRAC
C
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum,div
C                 how many short units spanned in WDMS interval
                  CALL TIMDIF (CURDAT,CURNXT,TMPTUN,IONE,
     O                         CURSPN)
                  FRAC= FLOAT(TMPSPN)/ FLOAT(CURSPN)
                  RVAL(DPOS)= RVAL(DPOS)+ FRAC* CURVAL
                  CFRAC= CFRAC+ (FLOAT(TMPSPN)/FLOAT(GETSPN))
                END IF
                IF (FRAC.GT.1.0) THEN
                  WRITE (*,*) 'BAD FRAC,TMPSPN:',FRAC,TMPSPN
                  WRITE (*,*) '  DSN,CTST,CTUN:',DSN,CURTST,CURTUN
                  WRITE (*,*) '  GETSPN,CURSPN:',GETSPN,CURSPN
                  WRITE (*,*) '  CURDAT:       ',CURDAT
                  WRITE (*,*) '  CURNXT:       ',CURNXT
                  WRITE (*,*) '  GETDAT:       ',GETDAT
                  WRITE (*,*) '  GETNXT:       ',GETNXT
                  WRITE (*,*) '  TMPNXT:       ',TMPNXT
                END IF
              ELSE IF (GTTRN.EQ.2) THEN
C               transform is max
                IF (RVAL(DPOS).LT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
                  CFRAC     = 1.0
                END IF
              ELSE IF (GTTRN.EQ.3) THEN
C               transform is min
                IF (RVAL(DPOS).GT.CURVAL) THEN
                  RVAL(DPOS)= CURVAL
                  CFRAC     = 1.0
                END IF
              END IF
            END IF
C
            IF (GTADD.EQ.1) THEN
C             get ready to increment get counter
              CALL WDATCP (GETNXT,GETDAT)
C             adjust value if some data didnt meet quality
              IF (CFRAC.LT.1.0.AND.CFRAC.GT.0.0) THEN
                RVAL(DPOS)= RVAL(DPOS)/ CFRAC
              ELSE IF (CFRAC.LE.0.0) THEN
                RVAL(DPOS)= TSFILL
              END IF
              CFRAC= 0.0
              DPOS = DPOS+ 1
            END IF
C
            IF (WDADD.EQ.1) THEN
C             get ready to increment WDMS counter
              CALL WDATCP (CURNXT,CURDAT)
              CURCNT= CURCNT+ 1
              IF (TIMCHK(CURDAT,GPEDAT).LE.0) THEN
C               at the group boundary, update start of group date
                CALL WDATCP (GPEDAT,GPSDAT)
                CALL WDATCP (GPEDAT,TMPDAT)
                NEWGRP= 1
                GPIND = GPIND+ 1
              END IF
            END IF
          ELSE
C           a quick get
            FRAC= FRAC+ GETQRA
            IF (ABS(GETQRA-1.0).LT.1.0E-5) THEN
C             no transform required
              IF (QUALFG.GE.CURQUA) THEN
C               only use data of acceptable quality
                RVAL(DPOS)= CURVAL
              END IF
              DPOS= DPOS+ 1
              FRAC= 0.0
            ELSE IF (GETQRA.LT.1.0) THEN
C             dsn interval less than user requested
              IF (QUALFG.GE.CURQUA) THEN
C               only use data of acceptable quality
                IF (GTTRN.LE.1 .AND. CFRAC.LT.1.0E-20) THEN
C                 data available & 1st time,   so initialize to zero
                  RVAL(DPOS) = 0.0
                END IF
                CFRAC= CFRAC+ GETQRA
                IF (GTTRN.EQ.0) THEN
C                 transform is aver
                  RVAL(DPOS)= RVAL(DPOS)+ GETQRA* CURVAL
                ELSE IF (GTTRN.EQ.1) THEN
C                 transform is sum
                  RVAL(DPOS)= RVAL(DPOS)+ CURVAL
                ELSE IF (GTTRN.EQ.2) THEN
C                 transform is max
                  IF (CURVAL.GT.RVAL(DPOS)) RVAL(DPOS)= CURVAL
                ELSE IF (GTTRN.EQ.3) THEN
C                 transform is min
                  IF (CURVAL.LT.RVAL(DPOS)) RVAL(DPOS)= CURVAL
                END IF
              END IF
              IF (ABS(FRAC-1.0).LT.1.0E-5) THEN
C               completed this user interval
                IF (ABS(CFRAC-1.0).GT.1.0E-5) THEN
C                 some missing data
                  IF (GTTRN.LE.1.AND.CFRAC.GT.0.0) THEN
C                   adjust result
                    RVAL(DPOS)= RVAL(DPOS)/CFRAC
                  END IF
                END IF
                DPOS = DPOS+ 1
                FRAC = 0.0
                CFRAC= 0.0
              END IF
            ELSE
C             dsn interval greater than user requested
              ICNT= GETQRA
C             may not start of wdm data boundary
              I   = GETQK
              RTMP= CURVAL
              IF (GTTRN.EQ.1) THEN
C               transform is sum/div
                RTMP= RTMP/GETQRA
              END IF
 20           CONTINUE
                IF (QUALFG.GE.CURQUA) THEN
C                 only use data of acceptable quality
                  RVAL(DPOS)= RTMP
                END IF
                DPOS= DPOS+ 1
                I   = I+ 1
              IF (I.LE.ICNT.AND.DPOS.LE.I4NVAL) GO TO 20
C             assume start on data boundary next time
              GETQK= 1
            END IF
C           always get the next dsn data value
            CALL WDATCP (CURNXT,CURDAT)
            CURCNT= CURCNT+ 1
          END IF
        ELSE IF (RETCOD.EQ.-10) THEN
C         missing entire group
          DPOS  = EGPOS+ 1
C         reset where we are in process of getting data
          CALL WDATCP (GPEDAT,GETDAT)
          GTADD = 1
          RETCOD= 0
        END IF
C
        IF (DPOS.GT.EGPOS .AND. NEWGRP.EQ.0) THEN
C         at the group boundary, update start of group date
          CALL WDATCP (GPEDAT,GPSDAT)
          CALL WDATCP (GPEDAT,TMPDAT)
          NEWGRP= 1
          GPIND = GPIND+ 1
        END IF
C
      IF (DPOS.LE.I4NVAL.AND.RETCOD.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGTNV
     I                    (WDMSFL,
     M                     CURCNT,CURNOV,CURCMP,CURREC,CURBKS,CURTST,
     M                     CURTUN,CURQUA,CURPOS,CURDAT,
     O                     CURVAL,CURNXT)
C
C     + + + PURPOSE + + +
C     routine to get the next value from a WDS timeseries DSN
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,CURCMP,CURREC,CURBKS,CURTST,CURTUN,CURQUA,
     1          CURPOS,CURDAT(6),CURNXT(6)
      INTEGER*4 CURCNT,CURNOV
      REAL      CURVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     CURCNT - current position within block
C     CURNOV - current number of values in current block
C     CURCMP - current compression code
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURTST - current time step
C     CURTUN - current time units
C     CURQUA - current quality code
C     CURPOS - current position in current block
C     CURDAT - current date of start of current value
C     CURVAL - current value
C     CURNXT - internal end date array
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND
      INTEGER*4 NUMSKP,BCW,I4ONE
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSKBK, WDRCGO, WBCWSP, TIMADD
C
C     + + + END SPECIFICATIONS + + +
C
      I4ONE= 1
C
      IF (CURCNT.GT.CURNOV) THEN
C       time for a new block
        NUMSKP= 2
        IF (CURCMP.EQ.0) NUMSKP= CURNOV+ 1
C       skip to next BCW
        CALL WDSKBK (WDMSFL,NUMSKP,
     M               CURREC,CURBKS)
C       be sure record is in buffer
        RIND= WDRCGO (WDMSFL,CURREC)
C       get the BCW
        BCW= WIBUFF(CURBKS,RIND)
        CURVAL= WRBUFF(CURBKS+1,RIND)
C       split the BCW
        IF (ABS(CURVAL).GT.1.0E-3) THEN
C         zero maybe compressed (JLK 3/12/86 NOT ALL CASES,TEMP OLD WAY)
          CALL WBCWSP (BCW,
     O                 CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        ELSE
C         non zero, dont adjust CURTST and CURNOV
          CALL WBCWSP (BCW,
     O                 CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        END IF
C
        CURCNT= 1
      ELSE
C       be sure record is in buffer
        RIND= WDRCGO(WDMSFL,CURREC)
      END IF
C     get the current value
      CURPOS= CURBKS+ 1
      IF (CURCMP.EQ.0.AND.CURCNT.GT.0) CURPOS= CURBKS+ CURCNT
      CURVAL= WRBUFF(CURPOS,RIND)
C     calculate new ending date on WDMS interval
      CALL TIMADD (CURDAT,CURTUN,CURTST,I4ONE,
     O             CURNXT)
      RETURN
      END
C
C
C
      SUBROUTINE   WDTPFX
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTOVWR,QUALFG,TUNITS,RVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     puts timeseries information into the WDMSFL.  This routine
C     was originally called WDTPUT.  There is a known problem with the
C     DTOVWR = 1 option.  The new WDTPUT traps that condition and has
C     a work-around for it.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for put
C     DATES  - starting date
C     NVAL   - number of values
C     DTOVWR - data overwrite flag,
C              0 - dont overwrite
C              1 - overwrite O.K.
C     QUALFG - allowed quality code
C     TUNITS - time units for put
C     RVAL   - array for writing out values
C     RETCOD - return code
C                0 - everything is O.K.
C               -8 - invalid date
C               -9 - data not present in current group
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -15 - VBTIME=1 and DELT,TUNITS do not agree
C                    with the data set
C              -20 - problem with one or more of following:
C                    DTOVWR, NVAL, QUALFG, TUNITS, DELT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cwtsds.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG,GPOSEN,GPIND,LTSTEP,LTUNIT,ALTERD,
     1          ENDDAT(6),GPSDAT(6),GPEDAT(6),DATNOW(6),TDAT(6),
     2          TDSFRC,TGROUP,TSPTAD,REMTUN,REMTST,RIND,
     3          COMPFG,TSFORM,VBTIME,TSSTEP,TCODE,TSTEPF,TCDCMP
      INTEGER*4 TVAL,DPOS,I4NVAL,I4ONE
      REAL      TOLR,TSFILL,REMVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTPMCK, WTFNDG, WDATCP, WTEGRP, WTGPCK, TIMADD, TIMDIF
      EXTERNAL   WTPTVL, WDRCGO, WTDSPX, CMPTIM
C
C     + + + END SPECIFICATIONS + + +
C
      I4NVAL= NVAL
      I4ONE = 1
      LTSTEP= DELT
      LTUNIT= TUNITS
      GPFLG = 2
      REMTUN= 0
      ALTERD= 0
C
C     check the user supplied parameters
      CALL WTPMCK (GPFLG,DTOVWR,DATES,NVAL,QUALFG,
     M             LTSTEP,LTUNIT,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       check the data set and figure out which groups have been req.
        CALL WTFNDG (WDMSFL,DSN,GPFLG,DATES,LTSTEP,LTUNIT,I4NVAL,
     O               TDSFRC,TSFILL,TGROUP,TOLR,TSPTAD,
     1               GPIND,GPOSEN,GPSDAT,ENDDAT,RETCOD)
      END IF
      IF (RETCOD.EQ.0) THEN
C       get additional parameters
        RIND= WDRCGO(WDMSFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        IF (VBTIME.EQ.1) THEN
C         the time units and step must be exactly right
          CALL CMPTIM ( LTUNIT, LTSTEP, TCODE, TSSTEP, TSTEPF, TCDCMP )
          IF (TCDCMP .NE. 0) THEN
C           not a match, quick get wont work
            RETCOD= -15
          END IF
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
        CALL WDATCP (DATES,DATNOW)
        DPOS= 1
 10     CONTINUE
C         find end of group
          CALL WTEGRP (GPSDAT,TGROUP,
     O                 GPEDAT)
C
C         skip values in this group as required
          CALL WTGPCK (WDMSFL,GPIND,GPSDAT,DATNOW,LTUNIT,
     I                 TDSFRC,TSFILL,TGROUP,VBTIME,
     O                 CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                 CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                 RETCOD)
          IF (RETCOD.EQ.0) THEN
C           update current date if part value starts this group
            IF (REMTUN.GT.0) THEN
              CALL TIMADD (DATNOW,REMTUN,REMTST,I4ONE,
     M                     TDAT)
C              WRITE (*,*) 'WDTPUT:     DATNW1:',DATNOW
C              WRITE (*,*) '            DATNW2:',TDAT
C              WRITE (*,*) '       RTUN,RTST,1:',REMTUN,REMTST,I4ONE
              CALL WDATCP (TDAT,DATNOW)
            END IF
C           find how many values we need to write
            CALL TIMDIF (DATNOW,GPEDAT,LTUNIT,LTSTEP,
     O                   TVAL)
C           put the data for this group
            CALL WTPTVL (WDMSFL,NVAL,LTUNIT,LTSTEP,
     1                   TDSFRC,TSFILL,TOLR,QUALFG,RVAL,TVAL,TSPTAD,
     2                   GPEDAT,COMPFG,
     M                   CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     1                   CURTST,CURTUN,CURCMP,CURQUA,CURDAT,
     2                   DPOS,REMTUN,REMTST,REMVAL)
            GPIND= GPIND+ 1
            ALTERD= 1
C
C           update the current date
            CALL WDATCP (GPEDAT,DATNOW)
C           update date to start group
            CALL WDATCP (DATNOW,GPSDAT)
          END IF
        IF (RETCOD.EQ.0.AND.GPIND.LE.GPOSEN.AND.DPOS.LE.I4NVAL) GO TO 10
      END IF
C
      IF (ALTERD .EQ. 1) THEN
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTGPCK
     I                    (WDMSFL,GPIND,GPSDAT,DATNOW,LTUNIT,
     I                     TDSFRC,TSFILL,TGROUP,VBTIME,
     O                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O                     CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     checks information related to a group, skip to starting
C     value, fill in current information
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GPIND,GPSDAT(6),DATNOW(6),LTUNIT,
     1          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     2          CURDAT(6),RETCOD,TDSFRC,TGROUP,VBTIME
      INTEGER*4 CURNOV,CURCNT
      REAL      TSFILL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     GPIND  - group index number
C     GPSDAT - starting date of group
C     DATNOW - current date
C     LTUNIT - time units
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TGROUP - data group pointer units
C     VBTIME - variable timestep flag
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURCNT - current position within block
C     CURDAT - current date of start of current value
C     RETCOD - return code
C                  0 - everything O.K.
C                 -9 - data not present in current group
C                -10 - no data in this group
C                -11 - no non missing data, data has not started yet
C                -21 - date from WDM doesn't match expected date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,DIND,SIND,JX,JY,JZ,TREC,TPOS,SREC,SPOS,SQUA,
     1          PDAT,BADJFG,ADDAFG,OREC,DREC
      INTEGER*4 TDFREE,SNOV,I4TMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCDL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTSKVL, WDSKBK, WDRCGO, WBCWSP, WDPTSP, WDRCDL
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      DREC  = 0
      BADJFG= 0
      ADDAFG= LTUNIT
C
C     skip to place to start write
      CALL WTSKVL (WDMSFL,GPIND,GPSDAT,DATNOW,
     I             TDSFRC,TSFILL,TGROUP,BADJFG,ADDAFG,VBTIME,
     O             CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O             CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O             RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       update CURNOV for overwriting
        CURNOV= CURCNT- 1
C
C       check for defined values in rest of group and record
C       because overwriting not allowed
        IF (CURQUA.NE.31) THEN
C         data present in current group
          RETCOD= -9
        ELSE
C         may be additional groups on this record unless free
C         skip other undef values if available
          SREC= CURREC
          SPOS= CURBKS
 10       CONTINUE
            OREC= SREC
            I4TMP= 2
            CALL WDSKBK (WDMSFL,I4TMP,
     M                   SREC,SPOS)
            IF (OREC.NE.SREC) THEN
C             we have moved on to a new rec which only contains fillers
              DREC= SREC
            END IF
            IF (SREC.GT.0) THEN
              SIND= WDRCGO(WDMSFL,SREC)
              I4TMP= WIBUFF(SPOS,SIND)
              IF (I4TMP.GT.0) THEN
                CALL WBCWSP (I4TMP,
     O                       SNOV,JX,JY,JZ,SQUA)
              ELSE
                SQUA= 0
              END IF
            ELSE
C             we are at the end of the record chain for this DSN
              SQUA= -1
            END IF
          IF (SQUA.EQ.31) GO TO 10
C
          IF (SQUA.GE.0) THEN
C           check to see if free space points to where we skipped to
            DIND  = WDRCGO(WDMSFL,TDSFRC)
            PDAT  = WIBUFF(11,DIND)
            TDFREE= WIBUFF(PDAT+1,DIND)
            CALL WDPTSP (TDFREE,
     O                   TREC,TPOS)
            IF (TREC.NE.SREC.OR.TPOS.NE.SPOS) THEN
C             data not present in current group
              RETCOD= -9
            END IF
          END IF
        END IF
C
        IF (RETCOD.EQ.0.AND.DREC.NE.0) THEN
C         delete the record containing only fillers
          I= WDRCDL(WDMSFL,DREC)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTPTVL
     I                    (WDMSFL,NVAL,LTUNIT,LTSTEP,
     I                     TDSFRC,TSFILL,TOLR,QUALFG,RVAL,TVAL,TSPTAD,
     I                     GPEDAT,COMPFG,
     M                     CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     M                     CURTST,CURTUN,CURCMP,CURQUA,CURDAT,
     M                     DPOS,REMTUN,REMTST,REMVAL)
C
C     + + + PURPOSE + + +
C     writes all or part of a WDMS group into a WDMS timeseries data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NVAL,LTUNIT,LTSTEP,QUALFG,
     1          TDSFRC,TSPTAD,GPEDAT(6),COMPFG,REMTUN,REMTST,
     2          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     3          CURDAT(6)
      INTEGER*4 TVAL,DPOS,CURNOV
      REAL      TSFILL,TOLR,RVAL(NVAL),REMVAL,CURVAL,PREVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     NVAL   - number of values
C     LTUNIT - time units
C     LTSTEP - time step
C     TDSFRC - first record in data set
C     TSFILL - missing data filler code
C     TOLR   - compression tolerance
C     QUALFG - get quality code
C     RVAL   - array of data values to put on WDMS file
C     TVAL   - number of values to write in this group
C     TSPTAD - disaggregation code
C              0 - same
C              1 - div
C     GPEDAT - group ending date
C     COMPFG - compression flag
C              1 - alow compression
C              2 - no compression
C     CURREC - current record number
C     CURBKS - starting position of current block within current record
C     CURPOS - current position in current block
C     CURNOV - current number of values in current block
C     CURVAL - current value
C     PREVAL - previous value
C     CURTST - current time step
C     CURTUN - current time units
C     CURCMP - current compression code
C     CURQUA - current quality code
C     CURDAT - current date of start of current value
C     DPOS   - current position within RVAL
C     REMTUN - remaining time unit across group border
C     REMTST - remaining time step across group border
C     REMVAL - remaining data value across group border
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,CHK,I,DIND,PDAT,TDAT(6),EGPFLG,LCMPFG
      INTEGER*4 BCW,TDFREE,I4ZRO,I4ONE,CVAL,I4NVAL,FVAL,XVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK,WDRCGO
      INTEGER*4 WBCWCL,WDPTCL
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WBCWCL, WTNWBK, TIMADD, WDATCP, TIMCHK, TIMDIF
      EXTERNAL  WDRCUP, WDPTCL
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO = 0
      I4ONE = 1
      I4NVAL= NVAL
      CVAL  = 0
      IF (QUALFG .LT. 31) THEN
        LCMPFG= COMPFG
      ELSE
C       always allow compression of missing data
        LCMPFG= 1
      END IF
C
      RIND  = WDRCGO(WDMSFL,CURREC)
C
C     WRITE (*,*) 'WTPTVL:     GPEDAT:',GPEDAT
C     WRITE (*,*) '            CURDAT:',CURDAT
C     WRITE (*,*) '              TVAL:',TVAL
      IF (REMTUN.GT.0) THEN
C       WRITE (*,*) 'STRT:REMTUN,REMTST:',REMTUN,REMTST
C       the current value is partly in the last group
        CURVAL= REMVAL
        CURTST= REMTST
        CURTUN= REMTUN
        REMTUN= 0
        BCW   = WBCWCL (I4ONE,CURTST,CURTUN,I4ZRO,QUALFG)
        WIBUFF(CURBKS,RIND)= BCW
        WRBUFF(CURPOS,RIND)= CURVAL
        CURPOS= CURPOS+ 1
        CALL WTNWBK (WDMSFL,
     M               CURREC,CURPOS,
     O               CURBKS)
        RIND = WDRCGO (WDMSFL,CURREC)
C
        CALL TIMADD (CURDAT,CURTUN,CURTST,I4ONE,
     O               TDAT)
        CALL WDATCP (TDAT,CURDAT)
C       update pointer to current value
        DPOS= DPOS+ 1
      END IF
C
      CURVAL= RVAL(DPOS)
C     figure out if we can continue the last block
      IF (LTSTEP.NE.CURTST.OR.LTUNIT.NE.CURTUN.OR.QUALFG.NE.CURQUA) THEN
C       we cant continue
        CHK= 0
      ELSE
C       ok to continue
        CHK= 1
      END IF
C
C     write (99,*) 'write:  ',CURDAT(1),CURDAT(2),CURDAT(3),CURCMP
C     write (99,*) '        ',CHK,CURNOV,LCMPFG,COMPFG
C     write (99,*) '        ',CURVAL,PREVAL,TOLR
      IF (TVAL.GT.0) THEN
 10     CONTINUE
          CVAL= CVAL+ 1
C
          IF (ABS(CURVAL-PREVAL).LE.TOLR.AND.CHK.EQ.1.AND.
     1        CURNOV.LT.32000.AND.LCMPFG.EQ.1) THEN
C           we want to be compressed
            IF (CURCMP.EQ.1) THEN
C             we already are
              CURNOV= CURNOV+ 1
            ELSE
C             currently uncompressed, finish old block
C             delete last value from old block
              CURNOV= CURNOV- 1
              CURPOS= CURPOS- 1
              IF (CURNOV.GT.I4ZRO) THEN
                BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
                WIBUFF(CURBKS,RIND)= BCW
                CALL WTNWBK (WDMSFL,
     M                       CURREC,CURPOS,
     O                       CURBKS)
                RIND = WDRCGO( WDMSFL, CURREC )
C               write (99,*) 'now cmp:',TVAL,CURREC,CURPOS,CURNOV,CURQUA
              END IF
              WRBUFF(CURPOS,RIND)= PREVAL
              CURNOV= 2
              CURCMP= 1
            END IF
          ELSE
C           we want to be uncompressed
            IF (CHK.NE.1.OR.CURCMP.EQ.1) THEN
              IF (CURNOV.GT.I4ZRO) THEN
C               finish the old block
                BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
                WIBUFF(CURBKS,RIND)= BCW
                IF (CURCMP.EQ.1.OR.CHK.EQ.0) CURPOS= CURPOS+ 1
                CALL WTNWBK (WDMSFL,
     M                       CURREC,CURPOS,
     O                       CURBKS)
                RIND  = WDRCGO(WDMSFL,CURREC)
C               write (99,*) 'now unc:',TVAL,CURREC,CURPOS,CURNOV,CURQUA
                CURNOV= 0
              END IF
              IF (CHK.EQ.0) THEN
                CURQUA= QUALFG
                CURTUN= LTUNIT
                CURTST= LTSTEP
              ELSE IF (CHK.EQ.-1) THEN
                CURQUA= 31
              END IF
            END IF
            CURCMP= 0
            WRBUFF(CURPOS,RIND)= CURVAL
            CURPOS= CURPOS+ 1
            CURNOV= CURNOV+ 1
            IF (CURPOS.GT.512.AND.CVAL.LT.TVAL) THEN
C             out of space in this record, finish block
              BCW= WBCWCL (CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
              WIBUFF(CURBKS,RIND)= BCW
              CALL WTNWBK (WDMSFL,
     M                     CURREC,CURPOS,
     O                     CURBKS)
              RIND= WDRCGO(WDMSFL,CURREC)
C             write (99,*) 'end rec:',TVAL,CURREC,CURPOS,CURNOV,CURQUA
              CURNOV= 0
            END IF
          END IF
          CHK= 1
C         dont allow compression across record boundary
          IF (CURNOV.EQ.I4ZRO) THEN
C           jlk 11/89, insure quality code on trailing data
            CHK= 0
            CURQUA= QUALFG
          END IF
C
          IF (CURCMP.EQ.0) THEN
C           update value to compare next value for compressed values
            PREVAL= CURVAL
          END IF
          DPOS= DPOS+ 1
          IF (DPOS.LE.I4NVAL) THEN
            CURVAL= RVAL(DPOS)
          ELSE
            CURVAL= TSFILL
            IF (CURQUA.NE.31) THEN
C             allow compression of trailing values
              LCMPFG= 1
              CHK= -1
            END IF
          END IF
C
        IF (CVAL.LT.TVAL) GO TO 10
C
C       finish the last block
        BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        WIBUFF(CURBKS,RIND)= BCW
C       write (99,*) 'lst blk:',TVAL,CURREC,CURPOS,CURNOV,CURQUA
C       update the current position
        IF (CURCMP.EQ.1) CURPOS= CURPOS+ 1
C
      END IF
C
C     calculate the current date
      CALL TIMADD (CURDAT,LTUNIT,LTSTEP,TVAL,
     O             TDAT)
      CALL WDATCP (TDAT,CURDAT)
C     are we at the end of the group?
      IF (TIMCHK(CURDAT,GPEDAT).NE.0) THEN
C       no, better fill in, how much?
        CURTUN= LTUNIT
        CURTST= 1
 20     CONTINUE
          CALL TIMDIF (CURDAT,GPEDAT,CURTUN,CURTST,
     O                 FVAL)
          CALL TIMADD (CURDAT,CURTUN,CURTST,FVAL,
     O                 TDAT)
C         WRITE (*,*) 'EGRP:       CURDAT:',CURDAT
          EGPFLG= 0
          IF (TIMCHK(TDAT,GPEDAT).NE.0) THEN
C           still not at even end of group
            CURTUN= CURTUN- 1
            IF (CURTUN.LE.0) THEN
              STOP 'BIG PROBLEM, END OF GROUP CURTUN=0'
            END IF
            EGPFLG= 1
          END IF
        IF (EGPFLG.EQ.1) GO TO 20
C
        IF (FVAL.GT.1) THEN
          CURTST= FVAL
          IF (CURTST.GT.63) THEN
            WRITE(99,*) 'BIG PROBLEM FILLING UP GROUP,STEP:',CURTST
            WRITE (*,*) 'BIG PROBLEM FILLING UP GROUP,STEP:',CURTST
          END IF
        END IF
C       figure out how much goes into next group
        CALL TIMADD (CURDAT,LTUNIT,LTSTEP,I4ONE,
     O               TDAT)
        I= 1
        CALL TIMDIF (GPEDAT,TDAT,CURTUN,I,
     O               XVAL)
        REMTST= XVAL
        IF (REMTST.GT.63) THEN
          WRITE(99,*) 'BIG PROBLEM STARTING GROUP,STEP:',REMTST
          WRITE (*,*) 'BIG PROBLEM STARTING GROUP,STEP:',REMTST
        END IF
        REMTUN= CURTUN
C
        CURNOV= 1
        CURCMP= 0
C
C       figure out what the fill value and quality is
        IF (DPOS.LE.I4NVAL) THEN
          IF (TSPTAD.EQ.0) THEN
C           disagg code is same
            CURVAL= RVAL(DPOS)
            REMVAL= CURVAL
          ELSE
C           disagg code is div
            CALL TIMADD (CURDAT,LTUNIT,LTSTEP,I4ONE,
     O                   TDAT)
            CALL TIMDIF (CURDAT,TDAT,CURTUN,CURTST,
     O                   CVAL)
            CURVAL= (RVAL(DPOS)* FVAL)/ CVAL
            REMVAL= RVAL(DPOS)- CURVAL
          END IF
        ELSE
          CURVAL= TSFILL
          REMVAL= TSFILL
          CURQUA= 31
        END IF
C
C       get the next block
        CALL WTNWBK (WDMSFL,
     M               CURREC,CURPOS,
     O               CURBKS)
        RIND= WDRCGO(WDMSFL,CURREC)
C
        BCW= WBCWCL(CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
        WIBUFF(CURBKS,RIND)= BCW
        WRBUFF(CURPOS,RIND)= CURVAL
        CURPOS= CURPOS+ 1
      END IF
C
C     update the current record
      CALL WDRCUP(WDMSFL,RIND)
C     update the data-set free space pointer
      IF (CURPOS.GT.511) CURPOS = 511
      TDFREE= WDPTCL (CURREC,CURPOS)
      DIND  = WDRCGO(WDMSFL,TDSFRC)
      PDAT  = WIBUFF(11,DIND)
      WIBUFF(PDAT+1,DIND)= TDFREE
      CALL WDRCUP(WDMSFL,DIND)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTNWBK
     I                    (WDMSFL,
     M                     CURREC,CURPOS,
     O                     CURBKS)
C
C     + + + PURPOSE + + +
C     starts a new WDMS timeseries block, on a new record if req.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,CURREC,CURPOS,CURBKS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     CURREC - current record number
C     CURPOS - current position in current block
C     CURBKS - starting position of current block within current record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CIND,I
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCGX
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDRCGX
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CURPOS.GT.511) THEN
C       update the old record
        CIND= WDRCGO(WDMSFL,CURREC)
        CALL WDRCUP (WDMSFL,CIND)
C       new block nust start on new record
        I= 0
C       write(1,*) 'WTNWBK,782: wdmsfl,i,currec=',wdmsfl,i,currec
        CIND  = WDRCGX(WDMSFL,I,CURREC)
        CURREC= RECNO(CIND)
        CURPOS= 5
      END IF
C
      CURBKS= CURPOS
      CURPOS= CURPOS+ 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTPUT
     I                    (WDMSFL,DSN,DELT,DATES,NVAL,
     I                     DTOVWR,QUALFG,TUNITS,RVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Puts time series data into a WDM file.  This routine traps the
C     problem with overwritting existing data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELT,DATES(6),NVAL,DTOVWR,QUALFG,
     1          TUNITS,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELT   - time step for put
C     DATES  - starting date
C     NVAL   - number of values
C     DTOVWR - data overwrite flag,
C              0 - dont overwrite
C              1 - overwrite O.K.
C     QUALFG - allowed quality code
C     TUNITS - time units for put
C     RVAL   - array for writing out values
C     RETCOD - return code
C                0 - everything is O.K.
C               -8 - invalid date
C               -9 - data not present in current group
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -15 - VBTIME=1 and DELT,TUNITS do not agree
C                    with the data set
C              -20 - problem with one or more of following:
C                    DTOVWR, NVAL, QUALFG, TUNITS, DELT
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ALLFLG, DXX, LWDMFL, LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDTPFX, WTDDEL, TSBCLR, WID2UD
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   ALLFLG, DXX
     #     /      0,   0 /
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C     try to put time series data into WDM file
      CALL WDTPFX ( LWDMFL, LDSN, DELT, DATES, NVAL,
     I              DXX, QUALFG, TUNITS, RVAL,
     O              RETCOD )
      IF (RETCOD .EQ. -9  .AND.  DTOVWR .EQ. 1) THEN
C       data existed, delete it
        CALL WTDDEL ( LWDMFL, LDSN, DATES, ALLFLG,
     O                RETCOD )
        IF (RETCOD .EQ. 0) THEN
C         data successfully deleted, add data
          CALL WDTPFX ( LWDMFL, LDSN, DELT, DATES, NVAL,
     I                  DXX, QUALFG, TUNITS, RVAL,
     O                  RETCOD )
        END IF
      END IF
C     clear modified data from time-series buffer
      CALL TSBCLR (LWDMFL,LDSN)
C
      RETURN
      END
