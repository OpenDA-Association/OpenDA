C
C
C
      SUBROUTINE   TSBTIM
     I                   (TUNIT,TSTEP,DTRANS,QUALFG)
C
C     + + + PURPOSE + + +
C     Set the current time units, time step, data transformation,
C     and data quality flag for the time-series data in use.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TUNIT,TSTEP,DTRANS,QUALFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TUNIT  - time units of data,
C              1 - seconds          4 - days
C              2 - minutes          5 - months
C              3 - hours            6 - years
C     TSTEP  - time step of data
C     DTRANS - data transformation code,
C              0 - ave,same
C              1 - sum,div
C              2 - max
C              3 - min
C     QUALFG - allowed quality code
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsbuf.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      CTU = TUNIT
      CTS = TSTEP
      CTRANS = DTRANS
      CQUAL  = QUALFG
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSBWDS
     I                   (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     Set the current WDM file unit number and data-set number
C     for the time-series data in use.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file containing data
C     DSN    - data-set number on WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsbuf.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LWDMFL,LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL    WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C
      CFILUN = LWDMFL
      CDSN   = LDSN
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSBGET
     I                   (SDATE,NVAL,
     O                    RVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     Retrieve the time-series data specified by the current
C     time-series specifications (in ctsbuf.inc).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDATE(6),NVAL,RETCOD
      REAL      RVAL(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SDATE  - starting date of data being retrieved
C     NVAL   - number of values to retrieve
C     RVAL   - buffer of time-series data retrieved
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
      INCLUDE 'ctsbuf.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,I1,I6,MID,CEDATE(6),OPOS,LNVAL,GNVAL,LSTART(6),
     $           TBSDAT(6),GRSDAT(6),GREDAT(6),LSDAT(6),LEDAT(6),
     $           MAXGRP,SOFF,EOFF,CDATID,DATFG,DELCNT,INSPTR,
     $           ENDREC,COPYFG,GPEREC,RECPGR,TGROUP,BPOS,IREC,IPOS
      REAL       NODAT
C
C     + + + INTRINSICS + + +
      INTRINSIC  ABS, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBASE, ZIPR, TIMADD, CKDATE, TIMDIF
      EXTERNAL   COPYI, COPYR, WDTGET, TIMCVT, TSBINI
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I6 = 6
      NODAT = -1.0E10
C
C     may need to initialize time-series buffer stuff
      CALL TSBINI
C
C     make copy of start date and be sure its midnight convention of 00:00
      CALL COPYI (I6,SDATE,LSTART)
      CALL TIMCVT (LSTART)
C     determine ending date for data requested
      CALL TIMADD (LSTART,CTU,CTS,NVAL,
     O             CEDATE)
C     get base/group dates and other info for data
      CALL WDBASE (CFILUN,CDSN,LSTART,
     O             TBSDAT,TGROUP,GRSDAT,GREDAT,MAXGRP,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       convert to midnight convention of 00:00
        CALL TIMCVT (GRSDAT)
        CALL TIMCVT (GREDAT)
        IF (TGROUP.EQ.6) THEN
C         use leap year to assure max number of values in group
          GRSDAT(1)= 1992
          GREDAT(1)= 1993
        ELSE IF (TGROUP.EQ.5) THEN
C         use January to assure max number of values in group
          GRSDAT(2)= 1
          GREDAT(1)= GRSDAT(1)
          GREDAT(2)= 2
        END IF
C       determine max number of values in group
        CALL TIMDIF (GRSDAT,GREDAT,CTU,CTS,
     O               GNVAL)
        IF (GNVAL.LT.BUFLEN) THEN
C         may fit more than 1 group on each record
          GPEREC= BUFLEN/GNVAL
          RECPGR= 1
        ELSE
C         how may records needed for each group
          RECPGR= GNVAL/BUFLEN+ 1
          GPEREC= 1
        END IF
        IF (GPEREC.GE.MAXGRP) THEN
C         don't exceed max number of groups allowable for data set
          GPEREC= MAXGRP- 1
        END IF
C
        IF (RECPGR.LT.MXID) THEN
C         use time-series buffer for data retrieval
          OPOS = 1
          DATFG= 0
C         start looping through data groups at base date
          CALL COPYI (I6,TBSDAT,GRSDAT)
 100      CONTINUE
C           loop through groups of data
            CALL TIMADD (GRSDAT,TGROUP,I1,GPEREC,
     O                   GREDAT)
            IF (DATFG.EQ.0) THEN
C             see if data group surrounds requested start date
              CALL CKDATE (GREDAT,LSTART,
     O                     DATFG)
              IF (DATFG.LT.0) THEN
C               end of group preceeds requested start date, set flag to skip
                DATFG= 0
              END IF
            ELSE
C             passed start date, is group after end date?
              CALL CKDATE (CEDATE,GRSDAT,
     O                     DATFG)
              IF (DATFG.EQ.0) THEN
C               group starts at requested end date, exit data get loop
                DATFG= -1
              END IF
            END IF
C
            IF (DATFG.EQ.1) THEN
C             this group needed for requested data, set current id
              IF (TGROUP.LT.6) THEN
C               need to include more than just year
                CDATID= 0
                DO 110 I= 1,7-TGROUP
                  J= 7- TGROUP- I
                  CDATID= CDATID+ GRSDAT(I)*100**J
 110            CONTINUE
              ELSE
C               only group starting year needed
                CDATID= GRSDAT(1)
              END IF
              MID= 0
              IF (NUMID.GT.0) THEN
C               data previously stored in buffer, look for match of current
                I= 0
 120            CONTINUE
C                 look through specs of previous data
                  I= I+ 1
                  IF (BFILUN(I).EQ.CFILUN .AND. BDSN(I).EQ.CDSN .AND.
     $                BTU(I).EQ.CTU .AND. BTS(I).EQ.CTS .AND.
     $                BDATID(I).EQ.CDATID .AND. BTRANS(I).EQ.CTRANS)THEN
                    MID = I
                  END IF
                IF (I.LT.NUMID .AND. MID.EQ.0) GO TO 120
              END IF
C
CTMP            if (mid.ne.0) write (*,*) 'match found for data at pos',MID
              IF (MID.EQ.0) THEN
C               needed data not found in buffer
                ENDREC= FREREC+ RECPGR- 1
                IF (ENDREC.GT.MXID) THEN
C                 at end of buffer records, go back to 1st record
CTMP           write (*,*) 'crossing max buffer boundary'
CTMP           write (99,*) 'crossing max buffer boundary'
                  FREREC= 1
                  ENDREC= RECPGR
                END IF
C
                DELCNT= 0
                INSPTR= 0
                IF (NUMID.GT.0) THEN
C                 clear out any IDs using needed buffer records
                  DO 140 I= 1,NUMID
C                   check each ID
                    IF (BSREC(I).GE.FREREC .AND. BSREC(I).LE.ENDREC)THEN
C                     this IDs records conflict with needed records, remove 'em
                      DELCNT= DELCNT+ 1
                      IF (INSPTR.EQ.0) THEN
C                       always insert at 1st ID being deleted
                        INSPTR= I
                      END IF
                    ELSE IF (INSPTR.EQ.0 .AND. I.LT.NUMID) THEN
C                     see if data is to be inserted here (when not deleting)
                      IF (FREREC.GE.BSREC(I)+BNREC(I) .AND.
     $                    FREREC.LE.BSREC(I+1)) THEN
C                       insert data at this position in ID arrays
                        INSPTR= I+ 1
                      END IF
                    END IF
 140              CONTINUE
                END IF
C
                IF (INSPTR.EQ.0) THEN
C                 not inserting, must be adding to end
                  INSPTR= NUMID+ 1
                END IF
C
                IF (DELCNT.GT.0) THEN
C                 need to remove some existing data IDs
CTMP                write (*,*) 'remove',DELCNT,' data IDs, starting at',INSPTR
                  NUMID= NUMID- DELCNT
                  DO 150 I= INSPTR,NUMID
C                   adjust positions in arrays
CTMP                write (*,*) 'removing data ID at position',I
                    BFILUN(I)= BFILUN(I+DELCNT)
                    BDSN(I)  = BDSN(I+DELCNT)
                    BTU(I)   = BTU(I+DELCNT)
                    BTS(I)   = BTS(I+DELCNT)
                    BTRANS(I)= BTRANS(I+DELCNT)
                    CALL COPYI (I6,BSDATE(1,I+DELCNT),BSDATE(1,I))
                    BNVAL(I) = BNVAL(I+DELCNT)
                    BQUAL(I) = BQUAL(I+DELCNT)
                    BSREC(I) = BSREC(I+DELCNT)
                    BNREC(I) = BNREC(I+DELCNT)
                    BDATID(I)= BDATID(I+DELCNT)
 150              CONTINUE
                END IF
C
                NUMID= NUMID+ 1
CTMP              write (*,*) 'inserting ID at position',INSPTR,' of',NUMID
                IF (INSPTR.LT.NUMID) THEN
C                 need to adjust arrays to insert data ID
CTMP          if (delcnt.eq.0) write (*,*)
CTMP         $         'insert ID into empty space at position',INSPTR
                  DO 160 I= NUMID,INSPTR+1,-1
C                   adjust positions in arrays
                    BFILUN(I)= BFILUN(I-1)
                    BDSN(I)  = BDSN(I-1)
                    BTU(I)   = BTU(I-1)
                    BTS(I)   = BTS(I-1)
                    BTRANS(I)= BTRANS(I-1)
                    CALL COPYI (I6,BSDATE(1,I-1),BSDATE(1,I))
                    BNVAL(I) = BNVAL(I-1)
                    BQUAL(I) = BQUAL(I-1)
                    BSREC(I) = BSREC(I-1)
                    BNREC(I) = BNREC(I-1)
                    BDATID(I)= BDATID(I-1)
 160              CONTINUE
                END IF
                BFILUN(INSPTR)= CFILUN
                BDSN(INSPTR)  = CDSN
                BTU(INSPTR)   = CTU
                BTS(INSPTR)   = CTS
                BTRANS(INSPTR)= CTRANS
                CALL COPYI (I6,GRSDAT,BSDATE(1,INSPTR))
                CALL TIMDIF (GRSDAT,GREDAT,CTU,CTS,
     O                       BNVAL(INSPTR))
                BQUAL(INSPTR) = CQUAL
                BSREC(INSPTR) = FREREC
                BNREC(INSPTR) = RECPGR
                BDATID(INSPTR)= CDATID
C               init buffer records to indicate no data retrieved
                I= RECPGR*BUFLEN
                CALL ZIPR (I,NODAT,TSBUF(1,FREREC))
                MID= INSPTR
                FREREC= FREREC+ BNREC(INSPTR)
              END IF
C
C             put requested data from this group into output data array
              CALL COPYI (I6,GRSDAT,LSDAT)
              BPOS  = 1
              IREC  = BSREC(MID)
              COPYFG= 0
              DO 200 I= 1,GPEREC
C               loop through data groups in this buffer ID
                CALL TIMADD (LSDAT,TGROUP,I1,I1,
     O                       LEDAT)
                IF (COPYFG.EQ.0) THEN
C                 see if data group surrounds requested start date
                  CALL CKDATE (LEDAT,LSTART,
     O                         COPYFG)
                  IF (COPYFG.LT.0) THEN
C                   end of group preceeds requested start date, set flag to skip
                    COPYFG= 0
                  END IF
                ELSE
C                 passed start date, is group after end date?
                  CALL CKDATE (CEDATE,LSDAT,
     O                         COPYFG)
                  IF (COPYFG.EQ.0) THEN
C                   group starts at requested end date, no copy
                    COPYFG= -1
                  END IF
                END IF
C
C               how many values in this data group
                CALL TIMDIF (LSDAT,LEDAT,CTU,CTS,
     O                       GNVAL)
                IF (COPYFG.EQ.1) THEN
C                 has this data group actually been retrieved
                  IF (ABS(TSBUF(BPOS,IREC)-NODAT).LT.1.0E-10) THEN
C                   1st data value of group is unitialized, get data group
CTMP           write (*,*) 'WDTGETing',GNVAL,' values from DSN',CDSN
                    CALL WDTGET (CFILUN,CDSN,CTS,LSDAT,GNVAL,
     I                           CTRANS,CQUAL,CTU,
     O                           TSBUF(BPOS,IREC),RETCOD)
                  END IF
C                 need to offset for start date?
                  CALL TIMDIF (LSDAT,LSTART,CTU,CTS,
     O                         SOFF)
C                 need to offset for end date?
                  CALL TIMDIF (CEDATE,LEDAT,CTU,CTS,
     O                         EOFF)
C                 copy data on record to output array
                  LNVAL= GNVAL- SOFF- EOFF
                  IPOS = BPOS+ SOFF
                  IF (IPOS.GT.BUFLEN) THEN
C                   need to adjust record and offset
                    IREC= IREC+ (IPOS/BUFLEN)
                    IPOS= MOD(IPOS,BUFLEN)
                    IF (IPOS.EQ.0) THEN
C                     position at end of previous record
                      IREC= IREC- 1
                      IPOS= BUFLEN
                    END IF
                  END IF
CTMP           write (*,*) 'putting',LNVAL,' values to output buffer'
CTMP           write (*,*) 'starting at position',SOFF+1,' of buffer record',I
                  CALL COPYR (LNVAL,TSBUF(IPOS,IREC),RVAL(OPOS))
                  OPOS= OPOS+ LNVAL
                END IF
C               increment to start of next data group
                CALL COPYI (I6,LEDAT,LSDAT)
C               increment position on buffer record
                BPOS= BPOS+ GNVAL
 200          CONTINUE
            END IF
C
C           increment to start of next data group
            CALL COPYI (I6,GREDAT,GRSDAT)
          IF (DATFG.GE.0 .AND. RETCOD.EQ.0) GO TO 100
        ELSE
C         too much data to fit in time-series buffer, use generic get
          CALL WDTGET (CFILUN,CDSN,CTS,LSTART,NVAL,CTRANS,CQUAL,CTU,
     O                 RVAL,RETCOD)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBASE
     I                   (WDMSFL,DSN,SDATE,
     O                    TBSDAT,TGROUP,GRSDAT,GREDAT,MAXGRP,RETCOD)
C
C     + + + PURPOSE + + +
C     Get base date, start/end dates of group, time units,
C     time step, and variable time flag for WDM data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    WDMSFL,DSN,SDATE(6),TGROUP,TBSDAT(6),
     $           GRSDAT(6),GREDAT(6),MAXGRP,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data set number on WDM file
C     SDATE  - starting date of data
C     TBSDAT - base starting date
C     TGROUP - data group pointer
C     GRSDAT - starting date of data group
C     GREDAT - ending date of data group
C     MAXGRP - max number of data groups allowed for data set
C     RETCOD - return code,
C              -14 - requested start date preceeds base start date
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,SAIND,SAVAL(1),IPOS,FREC,PDAT,PDATV
C
C     + + + FUNCTIONS + + +
      INTEGER    WDGIVL, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDGIVL, TIMCHK, WDDSCK, WDBSGI, WTSGRP, WTEGRP
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
C
      CALL WDDSCK (WDMSFL,DSN,
     O             FREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       get pointer to first group pointer
        IPOS= 11
        PDAT= WDGIVL(WDMSFL,FREC,IPOS)
C       get max number of group pointers
        IPOS= 12
        PDATV = WDGIVL(WDMSFL,FREC,IPOS)
C       determine max number of data groups on data set
        MAXGRP= PDATV- PDAT- 2
C
C       set default base date
        TBSDAT(1)= -999
        TBSDAT(2)= 1
        TBSDAT(3)= 1
        TBSDAT(4)= 0
        TBSDAT(5)= 0
        TBSDAT(6)= 0
C       get base date pointers
        DO 10 I= 1,4
          SAIND= I+ 26
          CALL WDBSGI (WDMSFL,DSN,SAIND,I1,
     O                 SAVAL,RETCOD)
          IF (RETCOD.EQ.0) THEN
C           valid attribute value exists
            TBSDAT(I)= SAVAL(1)
          END IF
 10     CONTINUE
C
C       get group pointer for data
        SAIND= 34
        CALL WDBSGI (WDMSFL,DSN,SAIND,I1,
     O               SAVAL,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         group pointer found
          TGROUP= SAVAL(1)
        ELSE
C         group pointer not found, set to default
          TGROUP= 6
C         reset return code
          RETCOD= 0
        END IF
C
C       determine start of data group
        CALL WTSGRP (SDATE,TGROUP,
     O               GRSDAT)
C       determine end of 1st data group
        CALL WTEGRP (SDATE,TGROUP,
     O               GREDAT)
C
        IF (TIMCHK(TBSDAT,GRSDAT).EQ.-1) THEN
C         base year after start of data group requested
          RETCOD= -14
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSBCLR
     I                   (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     Remove data from the time-series buffer which may have been
C     modified in the WDM file, but not in the buffer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsbuf.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I0,I6,ID
C
C     + + + EXTERNALS + + +
      EXTERNAL   COPYI, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I6 = 6
C
      IF (NUMID.GT.0) THEN
C       data stored in buffer
        DO 10 ID= 1,NUMID
C         see if input data set matches any data in buffer
          IF (BFILUN(ID).EQ.WDMSFL .AND. BDSN(ID).EQ.DSN) THEN
C           WDM file and data set match, remove them
            IF (ID.LT.NUMID) THEN
C             shift stored data information in buffer
              DO 50 I= ID,NUMID-1
C               move all info stored after this ID
                BFILUN(I)= BFILUN(I+1)
                BDSN(I)  = BDSN(I+1)
                BTU(I)   = BTU(I+1)
                BTS(I)   = BTS(I+1)
                BTRANS(I)= BTRANS(I+1)
                CALL COPYI (I6,BSDATE(1,I+1),BSDATE(1,I))
                BNVAL(I) = BNVAL(I+1)
                BQUAL(I) = BQUAL(I+1)
                BSREC(I) = BSREC(I+1)
                BNREC(I) = BNREC(I+1)
                BDATID(I)= BDATID(I+1)
 50           CONTINUE
            END IF
C           zero out last ID of information
            BFILUN(NUMID)= 0
            BDSN(NUMID)  = 0
            BTU(NUMID)   = 0
            BTS(NUMID)   = 0
            BTRANS(NUMID)= 0
            CALL ZIPI (I6,I0,BSDATE(1,NUMID))
            BNVAL(NUMID) = 0
            BQUAL(NUMID) = 0
            BSREC(NUMID) = 0
            BNREC(NUMID) = 0
            BDATID(NUMID)= 0
            NUMID= NUMID- 1
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   TSBINI
C
C     + + + PURPOSE + + +
C     Initialize time-series buffer common variables.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctsbuf.inc'
C
C     + + + SAVES + + +
      INTEGER    INITFG
      SAVE       INITFG
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I0
      REAL       R0
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, ZIPR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INITFG /0/
C
C     + + + EXTERNALS + + +
C
      I0 = 0
      R0 = 0.0
C
      IF (INITFG.EQ.0) THEN
C       init data buffer specification parameters
        CALL ZIPI (MXID,I0,BFILUN)
        CALL ZIPI (MXID,I0,BDSN)
        CALL ZIPI (MXID,I0,BTU)
        CALL ZIPI (MXID,I0,BTS)
        CALL ZIPI (MXID,I0,BTRANS)
        CALL ZIPI (MXID,I0,BQUAL)
        CALL ZIPI (6*MXID,I0,BSDATE)
        CALL ZIPI (MXID,I0,BNVAL)
        CALL ZIPI (MXID,I0,BSREC)
        CALL ZIPI (MXID,I0,BNREC)
        CALL ZIPI (MXID,I0,BDATID)
C       init counter and free data buffer position
        NUMID = 0
        FREREC= 1
C       init actual data buffer
        CALL ZIPR (BUFLEN*MXID,R0,TSBUF)
C       indicate intialization has been performed
        INITFG= 1
      END IF
C
      RETURN
      END
