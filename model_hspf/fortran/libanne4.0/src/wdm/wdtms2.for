C
C
C
      SUBROUTINE   WTFNDT
     I                    (WDMSFL,DSN,GPFLG,
     O                     TDSFRC,SDAT,EDAT,RETCOD)
C
C     + + + PURPOSE + + +
C     determine starting and ending dates of data in data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GPFLG,TDSFRC,SDAT(6),EDAT(6),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     GPFLG  - get(1)/put(2) flag
C     TDSFRC - data-set first record number
C     SDAT   - starting date of data in dsn
C     EDAT   - ending date of data in dsn
C     RETCOD - return code
C                0 - everything is O.K.
C               -6 - no data present
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GRPSTR,GRPEND,GRPIND,GRSPOS,GREPOS,
     1          PDAT,PDATV,TGRNUM,TGRPST,RIND,TSTEP,
     2          TGROUP,TBSDAT(6),TDAT(6),XDAT(6),NDAT(6),
     3          CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,CURQUA,
     4          TSPTAD,I,MSFLG,DSTYP,GRCNT,LWDMFL,LDSN
      INTEGER*4 I4ZRO,I4NVAL,GRSPTR,GREPTR,NUMSKP,CURNOV
      REAL      TSFILL,TOLR
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI, WDSCHA, WDRCGO, WTDSPM, TIMADD, WDPTSP, WDSKBK
      EXTERNAL  WBCWSP, WDATCP, WTEGRP, TIMCHK, TIMCNV, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I4ZRO = 0
      TSTEP = 1
      RETCOD= 0
      I     = 6
      CALL ZIPI (I,RETCOD,SDAT)
      CALL ZIPI (I,RETCOD,EDAT)
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C
      DSTYP = 1
      CALL WDSCHA (LWDMFL,LDSN,DSTYP,GPFLG,
     O             TDSFRC,GRCNT,RETCOD)
Caqt  IF (TDSFRC.EQ.0) THEN
      IF (TDSFRC.EQ.0 .AND. RETCOD.EQ.0) THEN
        WRITE(99,*)'WDM:WDTMS2:WTFNDT:',LWDMFL,LDSN,DSTYP,GPFLG,TDSFRC
Ckmf    write(*,*) 'WDM:WDTMS2:WTFNDT:',LWDMFL,LDSN,DSTYP,GPFLG,TDSFRC
        RETCOD= -6
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       bring label into buffer
        RIND = WDRCGO(LWDMFL,TDSFRC)
        CALL WTDSPM (LWDMFL,WIBUFF(1,RIND),WRBUFF(1,RIND),
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
C       look for first group with data
        GRPSTR= 0
        GRPIND= 0
        GRPEND= 0
        GRSPOS= TGRPST- 1
C
 10     CONTINUE
          GRPIND= GRPIND+ 1
          GRSPOS= GRSPOS+ 1
          IF (WIBUFF(GRSPOS,RIND).NE.I4ZRO) THEN
            GRPSTR= GRPIND
            GRSPTR= WIBUFF(GRSPOS,RIND)
          END IF
        IF (GRPSTR.EQ.0.AND.GRSPOS.LT.PDATV-1) GOTO 10
C
        IF (GRPSTR.GT.0) THEN
C         look for last group with data
          GREPOS= PDATV- 1
          GRPIND= TGRNUM+ 1
C
 20       CONTINUE
            GRPIND= GRPIND- 1
            IF (WIBUFF(GREPOS,RIND).NE.I4ZRO) THEN
              GRPEND= GRPIND
              GREPTR= WIBUFF(GREPOS,RIND)
            ELSE
              GREPOS= GREPOS- 1
            END IF
          IF (GRPEND.EQ.0) GOTO 20
C
C         calc exact starting date for date
          I4NVAL= GRPSTR- 1
C         find start date of beginning of group
          CALL TIMADD (TBSDAT,TGROUP,TSTEP,I4NVAL,
     O                 SDAT)
C         get starting record for data
          CALL WDPTSP (GRSPTR,
     O                 CURREC,CURPOS)
          RIND= WDRCGO (LWDMFL,CURREC)
C
          CURBKS= CURPOS
          NUMSKP= 1
C         loop to look for defined data
 30       CONTINUE
C           skip to next block control word
            CALL WDSKBK (LWDMFL,NUMSKP,
     M                   CURREC,CURBKS)
C           be sure record is in buffer
            RIND= WDRCGO (LWDMFL,CURREC)
C           split up block control word
            CALL WBCWSP (WIBUFF(CURBKS,RIND),
     O                   CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
            IF (CURQUA.EQ.31) THEN
C             skip this block
              CALL TIMADD (SDAT,CURTUN,CURTST,CURNOV,
     O                     TDAT)
              CALL WDATCP (TDAT,SDAT)
            END IF
            NUMSKP= 2
            IF (CURCMP.EQ.0) NUMSKP= CURNOV+ 1
          IF (CURQUA.EQ.31) GO TO 30
C
C         calc exact ending date for data
          MSFLG = 0
C         find start date of beginning of group
          I4NVAL= GRPEND- 1
          IF (GRPEND.NE.GRPSTR) THEN
C           ending record is not starting record
            CALL TIMADD (TBSDAT,TGROUP,TSTEP,I4NVAL,
     O                   XDAT)
C           get starting record for last group with data
            CALL WDPTSP (GREPTR,
     O                   CURREC,CURPOS)
            RIND  = WDRCGO (LWDMFL,CURREC)
            CURBKS= CURPOS
            NUMSKP= 1
          ELSE
C           we are looking in the starting group, from current date
            CALL TIMADD (SDAT,CURTUN,CURTST,CURNOV,
     O                   XDAT)
C           are we at the end of the starting group and data?
            CALL WTEGRP (SDAT,TGROUP,
     O                   NDAT)
            IF (TIMCHK(NDAT,XDAT).EQ.0) THEN
              MSFLG= -1
            END IF
          END IF
          IF (MSFLG.EQ.0) THEN
C           find start of next group
            CALL WTEGRP (XDAT,TGROUP,
     O                   NDAT)
C
C           loop to look for end of defined data
 40         CONTINUE
C             skip to next block control word
              CALL WDSKBK (LWDMFL,NUMSKP,
     M                     CURREC,CURBKS)
C             be sure record is in buffer
              RIND= WDRCGO (LWDMFL,CURREC)
C             split up block control word
              CALL WBCWSP (WIBUFF(CURBKS,RIND),
     O                     CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
              IF (CURQUA.EQ.31.AND.MSFLG.EQ.0) THEN
C               save start of this block
                CALL WDATCP (XDAT,EDAT)
                MSFLG= 1
              ELSE
                MSFLG= 0
              END IF
C             skip this block
              CALL TIMADD (XDAT,CURTUN,CURTST,CURNOV,
     O                     TDAT)
              CALL WDATCP (TDAT,XDAT)
              NUMSKP= 2
              IF (CURCMP.EQ.0) THEN
                NUMSKP= CURNOV+ 1
              END IF
            IF (TIMCHK(XDAT,NDAT).EQ.1.AND.CURNOV.GT.I4ZRO) GO TO 40
          END IF
C
          IF (MSFLG.LE.0) THEN
            CALL WDATCP (XDAT,EDAT)
          END IF
C         convert ending date to old format
          CALL TIMCNV (EDAT)
        ELSE
C        no data present
         RETCOD= -6
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDSCU
     I                    (SWDSFL,SDSN,TWDSFL,TDSN,
     I                     UPTFL,GSDAT,GEDAT,NRVAL,
     O                     RVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     copy from a data set to another any timeseries data between
C     start and end dates
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SWDSFL,SDSN,TWDSFL,TDSN,
     1          UPTFL,GSDAT(6),GEDAT(6),NRVAL,RETCOD
      REAL      RVAL(NRVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SWDSFL - source watershed data management file unit number
C     SDSN   - source data-set number
C     TWDSFL - target watershed data management file unit number
C     TDSN   - target data-set number
C     UPTFL  - update file unit number
C     GSDAT  - copy start date array
C     GEDAT  - copy end date array
C     NRVAL  - intermediate storage buffer size (recommended size 1000)
C     RVAL   - intermediate storage buffer
C     RETCOD - return code
C                0 - everything is O.K.
C               -9 - data not present in current group
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SDSFRC,STGRP,STSPT,SGPIND,SGPSEN,SGPSDT(6),
     1          SREC,SBKS,SPOS,STST,STUN,SCMP,SQUA,SCDAT(6),
     2          SGPEDT(6),SNGRP,LUPTFL,ALTERD,
     3          TDSFRC,TTGRP,TTSPT,TGPIND,TGPSEN,TGPSDT(6),
     4          TREC,TBKS,TPOS,TTST,TTUN,TCMP,TQUA,TCDAT(6),
     5          TGPEDT(6),TNGRP,TLSTGP,CGPEDT(6),
     6          UPFLG,         UTST,UTUN,     UQUA,UCSDAT(6),UCEDAT(6),
     7                         CTST,CTUN,     CQUA,
     8                         OTST,OTUN,     OQUA,
     9          GPFLG,XDAT(6),LTSTEP,LTUNIT,BADJFG,ADDAFG,CURDAT(6),
     A          TIMFLG,DONFG,CHK,RCNT,REMTUN,REMTST,TSPTAD,RIND,
     B          CARFLG,CSDAT(6),COMPFG,TSFORM,VBTIME,TSSTEP,TCODE
      INTEGER*4 SNOV,TNOV,SCNT,TCNT,NVAL,I4ONE,TGCNT,DPOS
      REAL      SVAL,STSFIL,STOLR,REMVAL,
     1          TVAL,TTSFIL,TTOLR,TPRVAL,
     2          UVAL,XVAL,CVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   TIMCHK, WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDATCP, TIMDFX, WTFNDG, WTEGRP, WTSKVL, WTGPCK, TIMADD
      EXTERNAL   TIMCHK, WTGTNV, TIMDIF, WTPTVL, WTDSPX, WDRCGO, TSBCLR
      EXTERNAL   WDMODT
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (I6,5I3,I6,I7,F11.0,I7)
C
C     + + + END SPECIFICATIONS + + +
C
      LUPTFL= UPTFL
      RETCOD= 0
      ALTERD= 0
      BADJFG= 0
      I4ONE = 1
      DONFG = 0
      SNGRP = 1
      TNGRP = 1
      TLSTGP= 0
      CARFLG= 0
      IF (LUPTFL.EQ.0) THEN
C       no update file
        UPFLG = 0
      ELSE
        UPFLG = 1
C       be sure we are at start of update file
        REWIND LUPTFL
      END IF
      CALL WDATCP (GSDAT,CSDAT)
C
C     get units and timestep for copy
      CALL TIMDFX (CSDAT,GEDAT,
     O             NVAL,LTUNIT,LTSTEP)
C
C     get info about source data set
      GPFLG= 1
      CALL WTFNDG (SWDSFL,SDSN,GPFLG,CSDAT,LTSTEP,LTUNIT,NVAL,
     O             SDSFRC,STSFIL,STGRP,STOLR,STSPT,
     1             SGPIND,SGPSEN,SGPSDT,XDAT,RETCOD)
C
C     get info about target data set
      GPFLG= 2
      CALL WTFNDG (TWDSFL,TDSN,GPFLG,CSDAT,LTSTEP,LTUNIT,NVAL,
     O             TDSFRC,TTSFIL,TTGRP,TTOLR,TTSPT,
     1             TGPIND,TGPSEN,TGPSDT,XDAT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       get other info about target dsn
        RIND= WDRCGO(TWDSFL,TDSFRC)
        CALL WTDSPX (WIBUFF(1,RIND),
     O               COMPFG,TSFORM,VBTIME,TSSTEP,TCODE)
        CALL WDATCP (CSDAT,CURDAT)
C
        IF (VBTIME .EQ. 1) THEN
C         adjust local time units to match requirments of target dataset
          LTUNIT= TCODE
        END IF
C
 10     CONTINUE
C         group loop
          IF (SNGRP.GT.0) THEN
            IF (SNGRP.EQ.2) SGPIND= SGPIND+ 1
C           find out end of group
            CALL WTEGRP (SGPSDT,STGRP,
     O                   SGPEDT)
            IF (SGPIND.EQ.SGPSEN) THEN
C             this is last group
              CALL WDATCP (GEDAT,SGPEDT)
            END IF
C           skip values in source dsn, dont need source VBTIME since
            ADDAFG= 0
            CALL WTSKVL (SWDSFL,SGPIND,SGPSDT,CSDAT,
     I                   SDSFRC,STSFIL,STGRP,BADJFG,ADDAFG,ADDAFG,
     O                   SREC,SBKS,SPOS,SNOV,SVAL,XVAL,
     O                   STST,STUN,SCMP,SQUA,SCNT,SCDAT,
     O                   RETCOD)
            SNGRP= 0
          END IF
          IF (TNGRP.NE.0) THEN
            IF (TNGRP.EQ.2) THEN
              TGPIND= TGPIND+ 1
            END IF
C           find out end of group
            CALL WTEGRP (TGPSDT,TTGRP,
     O                   TGPEDT)
            IF (TGPIND.EQ.TGPSEN) THEN
C             this is last group
              TLSTGP= 1
CJK 6/94      dont go too far
              CALL WDATCP (GEDAT,CGPEDT)
            ELSE
C             ck group at end on target group
              CALL WDATCP (TGPEDT,CGPEDT)
            END IF
            TNGRP= 0
          END IF
C         skip values in target dsn
          CALL WTGPCK (TWDSFL,TGPIND,TGPSDT,CSDAT,LTUNIT,
     I                 TDSFRC,TTSFIL,TTGRP,VBTIME,
     O                 TREC,TBKS,TPOS,TNOV,TVAL,TPRVAL,
     O                 TTST,TTUN,TCMP,TQUA,TCNT,TCDAT,
     O                 RETCOD)
C
          RCNT= 0
          IF (CARFLG.EQ.1) THEN
C           fill in saved value
            RCNT= RCNT+ 1
            RVAL(RCNT)= CVAL
            CALL TIMADD (CURDAT,CTUN,CTST,I4ONE,
     O                   XDAT)
            CALL WDATCP (XDAT,CURDAT)
            CARFLG= 0
          END IF
C
 20       CONTINUE
C           loop to fill buffer of similar values for put
            IF (RETCOD.EQ.0.AND.UPFLG.NE.0) THEN
 30           CONTINUE
C               get current update file info
                READ (LUPTFL,1000,END=40) UCSDAT,UTST,UTUN,UVAL,UQUA
                CALL TIMADD (UCSDAT,UTUN,UTST,I4ONE,
     O                       UCEDAT)
C               check to see if update ends after current date
              IF (TIMCHK(UCEDAT,CURDAT).EQ.1) GO TO 30
              GO TO 50
 40           CONTINUE
C               end of update file, dont try to read more
                LUPTFL= 0
 50           CONTINUE
              UPFLG= 0
            END IF
C
            IF (RCNT.GT.0) THEN
C             save old values
              OTST= CTST
              OTUN= CTUN
              OQUA= CQUA
            END IF
C
C           now copy some data, which source
            IF (LUPTFL.GT.0) THEN
C             try update file
              TIMFLG= TIMCHK(CURDAT,UCSDAT)
            ELSE
C             no update file, use source data set
              TIMFLG= 1
            END IF
            IF (TIMFLG.LT.0) THEN
C             part of update interval is current
C             how much, what value, adjust UTUN,UTST,UVAL
C             ************* NOT YET IMPLEMENTED ************
              RETCOD= -5
            END IF
            IF (TIMFLG.LE.0) THEN
C             save current update value for target dsn
              CVAL= UVAL
              CTST= UTST
              CTUN= UTUN
              CQUA= UQUA
C             read more data from update file
              UPFLG= 1
            ELSE
              TIMFLG= TIMCHK(CURDAT,SCDAT)
              IF (TIMFLG.EQ.0) THEN
C               save current source value for target dsn
                CVAL= SVAL
                CTST= STST
                CTUN= STUN
                CQUA= SQUA
              ELSE
C               only part of source interval is needed
C               how much, what value, adjust SVAL,STUN,STST
C               *********** NOT YET IMPLEMENTED ***********
                RETCOD= -4
              END IF
            END IF
C
            IF (RCNT.EQ.0) THEN
C             first time thru
              CHK = 1
              OTST= CTST
              OTUN= CTUN
              OQUA= CQUA
            ELSE
              IF (OTST.EQ.CTST.AND.OTUN.EQ.CTUN.AND.OQUA.EQ.CQUA) THEN
C               this value has same characteristics as last one
                CHK= 1
              ELSE
C               set flag to stop filling buffer
                CHK= 0
              END IF
            END IF
C
            IF (CHK.EQ.1) THEN
              RCNT= RCNT+ 1
              RVAL(RCNT)= CVAL
              CALL TIMADD (CURDAT,CTUN,CTST,I4ONE,
     O                     XDAT)
              CALL WDATCP (XDAT,CURDAT)
C             check for end of buffer
              IF (RCNT.EQ.NRVAL) THEN
C               end of buffer
                CHK= -2
              END IF
C             check for end of group
              IF (TIMCHK(CURDAT,CGPEDT).LE.0) THEN
                CHK  = -1
                TNGRP= 2
C               set beginning of next group (is this needed?)
                CALL WDATCP (TGPEDT,TGPSDT)
              END IF
              IF (TLSTGP.EQ.1) THEN
                IF (TIMCHK(CURDAT,GEDAT).LE.0) THEN
C                 last group done, finished
                  DONFG= 1
                END IF
              END IF
            END IF
C
C           get next values(S) from source dsn
 60         CONTINUE
              SCNT= SCNT+ 1
              IF (SCNT.GT.SNOV) THEN
C               check for group boundary
                CALL TIMADD (SCDAT,STUN,STST,I4ONE,
     O                       XDAT)
                IF (TIMCHK(XDAT,SGPEDT).EQ.0) THEN
C                 end of group
                  SNGRP= 2
C                 set start of new group
                  CALL WDATCP (SGPEDT,SGPSDT)
                END IF
              END IF
              IF (SNGRP.EQ.0) THEN
                CALL WTGTNV (SWDSFL,SCNT,
     M                       SNOV,SCMP,SREC,SBKS,STST,
     1                       STUN,SQUA,SPOS,SCDAT,
     O                       SVAL,XDAT)
                CALL WDATCP (XDAT,SCDAT)
              END IF
C             with an updated value, get whole span
            IF (UPFLG.EQ.1.AND.TIMCHK(SCDAT,UCEDAT).EQ.1.AND.
     1          SNGRP.EQ.0) GO TO 60
C
          IF (CHK.EQ.1.AND.SNGRP.EQ.0) GO TO 20
C
          IF (RETCOD.EQ.0) THEN
C           fill in target dsn
            DPOS  = 1
            REMTUN= 0
            CALL TIMDIF (CSDAT,TGPEDT,OTUN,OTST,
     O                   TGCNT)
            TSPTAD= 0
            CALL WTPTVL (TWDSFL,RCNT,OTUN,OTST,
     1                   TDSFRC,TTSFIL,TTOLR,OQUA,RVAL,TGCNT,TSPTAD,
     2                   TGPEDT,COMPFG,
     M                   TREC,TBKS,TPOS,TNOV,TVAL,TPRVAL,
     1                   TTST,TTUN,TCMP,TQUA,TCDAT,
     2                   DPOS,REMTUN,REMTST,REMVAL)
Ckmf        IF (DPOS.LT.RCNT) WRITE (*,*) 'BIG PROBLEM WITH COPY PUT'
            IF (DPOS.LT.RCNT) THEN
              WRITE(99,*)'WDTMS2:WTDSCU:  BIG PROBLEM WITH COPY PUT'
              WRITE (*,*) 'BIG PROBLEM WITH COPY PUT'
            END IF
            CALL WDATCP (CURDAT,CSDAT)
            IF (SNGRP.EQ.0.AND.CHK.NE.-2) CARFLG= 1
C           clear modified data from time-series buffer
            CALL TSBCLR (TWDSFL,TDSN)
            ALTERD= 1
          END IF
        IF (DONFG.EQ.0.AND.RETCOD.EQ.0) GO TO 10
C
        IF (ALTERD .EQ. 1) THEN
C         set dataset modification date attribute for target dataset
          CALL WDMODT (TWDSFL,TDSN)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDDEL
     I                    (WDMSFL,DSN,DELDAT,ALLFLG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     delete all data following a specified date in the given
C     data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DELDAT(6),ALLFLG,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DELDAT - delete from date array
C     ALLFLG - delete all flag, 0 - only delete group
C                               1 - all data after date
C     RETCOD - return code
C                0 - everything is O.K.
C               -6 - no data present
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -21 - date from WDM doesn't match expected date
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,DREC,NREC,SREC,SPOS,STST,STUN,SCMP,SQUA,SCNT,
     1          SDAT(6),SBKS,STSPT,SGRP,ADDAFG,BADJFG,I,GRDLCT,
     2          GRPIND,GRPDEL,GRPEND,GPSDAT(6),GPEDAT(6),PDAT,GPFLG,
     3          ENDDAT(6),VBTIME
      INTEGER*4 SFREE,SNOV,SGNOV,TNOV,LNOV,I4TMP,I4ZRO,GRPPTR
      REAL      SVAL,SFIL,STOLR
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCDL,TIMCHK
      INTEGER*4 WDPTCL,WBCWCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WTFNDT, TIMDFX, WTFNDG, TIMCHK, WDRCGO, WDPTSP, WTSKVL
      EXTERNAL   WBCWCL, WDRCUP, WTEGRP, WTNWBK, WDPTCL, WDRCDL, WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
      ADDAFG= 0
      VBTIME= 0
      BADJFG= 0
      GPFLG = 2
      I4ZRO = 0
      GRDLCT= 0
C
C     get end date for data
      CALL WTFNDT (WDMSFL,DSN,GPFLG,
     O             DREC,SDAT,ENDDAT,RETCOD)
C     are we deleting any data
      I= TIMCHK(DELDAT,ENDDAT)
      IF (I.EQ.1) THEN
C       figure out how much time is being deleted
        CALL TIMDFX (DELDAT,ENDDAT,
     O               SGNOV,STUN,STST)
C
C       figure out which groups to delete
        CALL WTFNDG (WDMSFL,DSN,GPFLG,DELDAT,STST,STUN,SGNOV,
     O               DREC,SFIL,SGRP,STOLR,STSPT,
     O               GRPDEL,GRPEND,GPSDAT,SDAT,RETCOD)
C
        IF (RETCOD.EQ.0) THEN
C         lets do the delete
          IF (TIMCHK(DELDAT,GPSDAT).EQ.0.OR.ALLFLG.EQ.1) THEN
C           on group boundary or deleting everything,
C           delete whole group or first one with data
            RIND= WDRCGO(WDMSFL,DREC)
 10         CONTINUE
              GRPPTR= WIBUFF(GRPDEL,RIND)
              IF (GRPPTR.EQ.I4ZRO) THEN
C               nothing in current group, try next one
                GRPDEL= GRPDEL+ 1
              ELSE
                CALL WDPTSP (GRPPTR,
     O                       SREC,SPOS)
              END IF
            IF (GRPPTR.EQ.I4ZRO) GO TO 10
C           increment number of groups deleted counter
            GRPDEL= GRPDEL- 1
          ELSE
C           skip to where delete starts
            CALL WTSKVL (WDMSFL,GRPDEL,GPSDAT,DELDAT,
     1                   DREC,SFIL,SGRP,BADJFG,ADDAFG,VBTIME,
     O                   SREC,SBKS,SPOS,SNOV,SVAL,SVAL,
     1                   STST,STUN,SCMP,SQUA,SCNT,SDAT,
     2                   RETCOD)
            IF (RETCOD.EQ.0) THEN
              IF (SCNT.EQ.1) THEN
C               at beginning of block BLOCK, DELETE IT ALL
                SPOS= SBKS
              ELSE
C               save part of block
                SNOV= SCNT- 1
                RIND= WDRCGO(WDMSFL,SREC)
                WIBUFF(SBKS,RIND)= WBCWCL(SNOV,STST,STUN,SCMP,SQUA)
                CALL WDRCUP(WDMSFL,RIND)
                IF (SCMP .EQ. 1) SPOS = SPOS + 1
              END IF
C             fill rest of group with undefined values
C             find end of group
              CALL WTEGRP (SDAT,SGRP,
     O                     GPEDAT)
              SCMP= 1
              SQUA= 31
C             figure out how much time left in group
              CALL TIMDFX (SDAT,GPEDAT,
     O                     TNOV,STUN,STST)
              I4TMP= 32767
C
 20           CONTINUE
                LNOV= TNOV
                IF (LNOV.GT.I4TMP) LNOV= I4TMP
                CALL WTNWBK (WDMSFL,
     M                       SREC,SPOS,
     O                       SBKS)
                RIND= WDRCGO(WDMSFL,SREC)
                WIBUFF(SBKS,RIND)= WBCWCL(LNOV,STST,STUN,SCMP,SQUA)
                WRBUFF(SPOS,RIND)= SFIL
                SPOS= SPOS+ 1
                TNOV= TNOV- LNOV
              IF (TNOV.GT.I4ZRO) GO TO 20
C             write out revised record
              CALL WDRCUP(WDMSFL,RIND)
            END IF
          END IF
        END IF
C
        IF (RETCOD.EQ.0) THEN
C         fix directory
          RIND= WDRCGO(WDMSFL,DREC)
C         fix group pointers to show no data
          DO 70 GRPIND= GRPDEL+1,GRPEND
            IF (WIBUFF(GRPIND,RIND).GT.0) GRDLCT= GRDLCT+ 1
            WIBUFF(GRPIND,RIND)= 0
 70       CONTINUE
C         update free pos and number of groups
          SFREE= WDPTCL(SREC,SPOS)
          PDAT = WIBUFF(11,RIND)
          WIBUFF(PDAT,RIND)= WIBUFF(PDAT,RIND)- GRDLCT
          WIBUFF(PDAT+1,RIND)= SFREE
C         update directory
          CALL WDRCUP(WDMSFL,RIND)
C
C         get record where data to delete starts
          RIND= WDRCGO(WDMSFL,SREC)
C         zero out rest of current record
          DO 80 I= SPOS,512
            WIBUFF(I,RIND)= 0
 80       CONTINUE
C         update retcord
          CALL WDRCUP(WDMSFL,RIND)
          NREC= WIBUFF(4,RIND)
C
          IF (NREC.GT.0) THEN
C           delete any and all records which follow
 90         CONTINUE
              SREC= NREC
              NREC= WDRCDL(WDMSFL,SREC)
            IF (NREC.GT.0) GOTO 90
          END IF
        END IF
C
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTEGRP
     I                    (DAT,TGROUP,
     O                     EGRDAT)
C
C     + + + PURPOSE + + +
C     determines end of group which contains a given date,
C     if at group boundary, returns date of end of group
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAT(6),TGROUP,EGRDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - current date array
C     TGROUP - group time unit
C     EGRDAT - end of group date array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZIPI, DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I= 6
      J= -999
      CALL ZIPI(I,J,EGRDAT)
C
      GO TO (10,20,30,40,50,60,70), TGROUP
 10   CONTINUE
C       second group pointers are not supported
        GO TO 90
C
 20   CONTINUE
C       minute group pointers are not supported
        GO TO 90
C
 30   CONTINUE
C       hour pointer
        EGRDAT(1)= DAT(1)
        EGRDAT(2)= DAT(2)
        EGRDAT(3)= DAT(3)
C       always increment hour for end of group
        EGRDAT(4)= DAT(4)+ 1
        IF (EGRDAT(4).GT.24) THEN
C         new day
          EGRDAT(4)= 1
          EGRDAT(3)= EGRDAT(3)+ 1
          IF (EGRDAT(3).GT.DAYMON(EGRDAT(1),EGRDAT(2))) THEN
C           new month
            EGRDAT(3)= 1
            EGRDAT(2)= EGRDAT(2)+ 1
            IF (EGRDAT(2).GT.12) THEN
C             new year
              EGRDAT(2)= 1
              EGRDAT(1)= EGRDAT(1)+ 1
            END IF
          END IF
        END IF
        GO TO 90
C
 40   CONTINUE
C       day pointer
        EGRDAT(1)= DAT(1)
        EGRDAT(2)= DAT(2)
        IF (DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(3)= DAT(3)+ 1
          IF (EGRDAT(3).GT.DAYMON(EGRDAT(1),EGRDAT(2))) THEN
C           new month
            EGRDAT(3)= 1
            EGRDAT(2)= EGRDAT(2)+ 1
            IF (EGRDAT(2).GT.12) THEN
C             new year
              EGRDAT(2)= 1
              EGRDAT(1)= EGRDAT(1)+ 1
            END IF
          END IF
        ELSE
          EGRDAT(3)= DAT(3)
        END IF
        EGRDAT(4)= 24
        GO TO 90
C
 50   CONTINUE
C       month pointer
        EGRDAT(1)= DAT(1)
        EGRDAT(2)= DAT(2)
        IF (DAT(3).EQ.DAYMON(DAT(1),DAT(2)).AND.DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(2)= EGRDAT(2)+ 1
          IF (EGRDAT(2).GT.12) THEN
C           new year
            EGRDAT(2)= 1
            EGRDAT(1)= EGRDAT(1)+ 1
          END IF
        END IF
        EGRDAT(3)= DAYMON(EGRDAT(1),EGRDAT(2))
        EGRDAT(4)= 24
        GO TO 90
C
 60   CONTINUE
C       year pointer
        EGRDAT(1)= DAT(1)
        IF (DAT(2).EQ.12.AND.DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(1)= EGRDAT(1)+ 1
        END IF
        EGRDAT(2)= 12
        EGRDAT(3)= 31
        EGRDAT(4)= 24
        GO TO 90
C
 70   CONTINUE
C       century pointers
        IF (MOD(DAT(1)+1,100).EQ.0.AND.DAT(2).EQ.12.AND.
     1      DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on boundary
          EGRDAT(1)= DAT(1)+ 100
        ELSE
          EGRDAT(1)= ((DAT(1)/100)+1)*100- 1
        END IF
        EGRDAT(2)= 12
        EGRDAT(3)= 31
        EGRDAT(4)= 24
        GO TO 90
C
 90   CONTINUE
      EGRDAT(5)= 0
      EGRDAT(6)= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTSGRP
     I                    (DAT,TGROUP,
     O                     SGRDAT)
C
C     + + + PURPOSE + + +
C     determines start of group which contains given date,
C     if at group boundary, returns given date
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAT(6),TGROUP,SGRDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - current date array
C     TGROUP - group time unit
C     SGRDAT - start of group date array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + FUNCTIONS + + +
      INTEGER   DAYMON
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI, DAYMON
C
C     + + + END SPECIFICATIONS + + +
C
      I= 6
      J= -999
      CALL ZIPI(I,J,SGRDAT)
C
      GO TO (10,20,30,40,50,60,70), TGROUP
 10   CONTINUE
C       second gorup pointers not supported
        GO TO 90
C
 20   CONTINUE
C       minute group pointers not supported
        GO TO 90
C
 30   CONTINUE
C       hour pointers
        SGRDAT(1)= DAT(1)
        SGRDAT(2)= DAT(2)
        SGRDAT(3)= DAT(3)
        SGRDAT(4)= DAT(4)
        IF (SGRDAT(4).EQ.0) THEN
C         back a day
          SGRDAT(4)= 24
          SGRDAT(3)= SGRDAT(3)- 1
          IF (SGRDAT(3).EQ.0) THEN
C           back a month
            SGRDAT(2)= SGRDAT(2)- 1
            IF (SGRDAT(2).EQ.0) THEN
C             back a year
              SGRDAT(2)= 12
              SGRDAT(1)= SGRDAT(1)- 1
            END IF
            SGRDAT(3)= DAYMON(SGRDAT(1),SGRDAT(2))
          END IF
        END IF
        GO TO 90
C
 40   CONTINUE
C       day pointers
        SGRDAT(1)= DAT(1)
        SGRDAT(2)= DAT(2)
        IF (DAT(4).EQ.24) THEN
C         on boundary
          SGRDAT(3)= DAT(3)
        ELSE
          SGRDAT(3)= DAT(3)- 1
          IF (SGRDAT(3).EQ.0) THEN
C           back a month
            SGRDAT(2)= SGRDAT(2)- 1
            IF (SGRDAT(2).EQ.0) THEN
C             back a year
              SGRDAT(2)= 12
              SGRDAT(1)= SGRDAT(1)- 1
            END IF
            SGRDAT(3)= DAYMON(SGRDAT(1),SGRDAT(2))
          END IF
        END IF
        SGRDAT(4)= 24
        GO TO 90
C
 50   CONTINUE
C       month pointers
        SGRDAT(1)= DAT(1)
        IF (DAT(3).EQ.DAYMON(DAT(1),DAT(2)).AND.DAT(4).EQ.24) THEN
C         on a boundary
          SGRDAT(2)= DAT(2)
        ELSE
          SGRDAT(2)= DAT(2)- 1
          IF (SGRDAT(2).EQ.0) THEN
C           back a year
            SGRDAT(2)= 12
            SGRDAT(1)= SGRDAT(1)- 1
          END IF
        END IF
        SGRDAT(3)= DAYMON(SGRDAT(1),SGRDAT(2))
        SGRDAT(4)= 24
        GO TO 90
C
 60   CONTINUE
C       year pointer
        IF (DAT(2).EQ.12.AND.DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on year boundary
          SGRDAT(1)= DAT(1)
        ELSE
          SGRDAT(1)= DAT(1)- 1
        END IF
        SGRDAT(2)= 12
        SGRDAT(3)= 31
        SGRDAT(4)= 24
        GO TO 90
C
 70   CONTINUE
C       century pointer
        IF (MOD(DAT(1)+1,100).EQ.0.AND.DAT(2).EQ.12.AND.
     1      DAT(3).EQ.31.AND.DAT(4).EQ.24) THEN
C         on the boundary
          SGRDAT(1)= DAT(1)
        ELSE
          SGRDAT(1)= (DAT(1)/100)*100- 1
        END IF
        SGRDAT(2)= 12
        SGRDAT(3)= 31
        SGRDAT(4)= 24
        GO TO 90
C
 90   CONTINUE
C
      SGRDAT(5)= 0
      SGRDAT(6)= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDATCP
     I                   (ODAT,NDAT)
C
C     copies an old array date into a new one
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ODAT(6),NDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ODAT   - from date array
C     NDAT   - to date array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      DO 10 I= 1,6
        NDAT(I)= ODAT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTDATE
     I                   (WDMFL, DSNCNT, DSN, CMMXFG,
     O                    SDATE, EDATE, ERRCOD)
C
C     + + + PURPOSE + + +
C     Find common or maximum period with data from a list of
C     time-series data sets on a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SDATE(6), EDATE(6), DSNCNT, CMMXFG, WDMFL, ERRCOD
      INTEGER   DSN(DSNCNT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - WDM file unit number
C     DSNCNT - number of data sets
C     DSN    - array of data-set numbers for timeseries
C     CMMXFG - common(1) or maximum(2) period flag
C     SDATE  - starting date (yr...sec)
C     EDATE  - ending date (yr...sec)
C     ERRCOD - error code 0 = ok, 2 = end before start, other - bad date
C              return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RETCOD, ICK, N, L3, L0, GPFLG, DREC, FLG,
     &          SD(6,2), ED(6,2), L2, L6, TEMP(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WTFNDT, DLIMIT, ZIPI, CKDATE, COPYI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   L0, L2, L3, L6
     &      / 0,  2,  3,  6/
C
C     + + + END SPECIFICATIONS + + +
C
      ERRCOD= 0
      GPFLG = 0
      CALL ZIPI (L3, L0, SD(4,1))
      CALL ZIPI (L3, L0, ED(4,1))
      SD(1,1) = 1000
      ED(1,1) = 9999
      SD(2,1) = 1
      SD(3,1) = 1
      ED(2,1) = 12
      ED(3,1) = 31
C
      DO 30 N= 1, DSNCNT
C       determine start and end dates
        CALL WTFNDT (WDMFL,DSN(N),GPFLG,
     O               DREC,SD(1,2),ED(1,2),RETCOD)
        IF (RETCOD .EQ. 0) THEN
C         compare start dates
          IF (CMMXFG .EQ. 1 .OR. N .EQ. 1) THEN
C           common period or first time thru, later start date
            FLG= 2
          ELSE
C           max period, earlier start date
            FLG= 1
          END IF
          CALL DLIMIT (SD, L2, FLG, TEMP)
          CALL COPYI (L6, TEMP, SD(1,1))
C         compare end dates
          IF (CMMXFG .EQ. 1 .OR. N .EQ. 1) THEN
C           common period or first time thru, earlier end date
            FLG= 1
          ELSE
C           max period, later start date
            FLG= 2
          END IF
          CALL DLIMIT (ED, L2, FLG, TEMP)
          CALL COPYI (L6, TEMP, ED(1,1))
        ELSE
          ERRCOD= RETCOD
        END IF
 30   CONTINUE
C
C     check final
      IF (ERRCOD .EQ. 0) THEN
C       compare final start and end dates
        CALL CKDATE (SD(1,1), ED(1,1), ICK)
        IF (ICK .EQ. 1) THEN
C         end before start
          ERRCOD = 2
        END IF
      END IF
C
      CALL COPYI (L6,SD,SDATE)
      CALL COPYI (L6,ED,EDATE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDATIM
     I                   ( WDMSFL, DSN, 
     O                     STRT, STOP, TSTEP, TCODE,
     O                     RETCOD )
C     
C     + + + PURPOSE + + +
C     Gets the start and end dates of the data in a time series data set
C     and the time step of the data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, STRT(6), STOP(6), TSTEP, TCODE,
     $          RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data set number
C     STRT   - start of record
C     STOP   - end of record
C     TSTEP  - time step of data, in TCODE units
C     TCODE  - time units of data
C              1 - second        4 - day
C              2 - minute        5 - month
C              3 - hour          6 - year
C     RETCOD - return code
C                  0 - all information successfully retrieved
C                 -6 - no data in the data set
C                -81 - data set does not exist
C                -82 - data set is not a time-series data set
C               -107 - one or more attributes not present in data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG, DREC, RETC, LEN, ZERO, FLAG, INDX
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI
      EXTERNAL  WTFNDT, WDBSGI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  GPFLG, ZERO
     $     /   1,    0  /
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
C     get period of record
      CALL WTFNDT ( WDMSFL, DSN, GPFLG,
     O              DREC, STRT, STOP, RETC )
      IF (RETC .EQ. -81  .OR.  RETC .EQ. -82) THEN
C       data set does not exist or is wrong type
        RETCOD = RETC
        LEN = 6
        CALL ZIPI ( LEN, ZERO, STRT )
        CALL ZIPI ( LEN, ZERO, STOP )
        TSTEP = 0
        TCODE = 0
      ELSE
C       data set does exist
        IF (RETC .EQ. -6) THEN
C         no data in data set
          RETCOD = RETC
          LEN = 6
          CALL ZIPI ( LEN, ZERO, STRT )
          CALL ZIPI ( LEN, ZERO, STOP )
        END IF
        FLAG = 0
C       get time step
        LEN = 1
        INDX= 33
        CALL WDBSGI ( WDMSFL, DSN, INDX, LEN,
     $                TSTEP, RETC )
        IF (RETC .EQ. -107) THEN
C         time step not present
          TSTEP = 0
          FLAG = 1
        END IF
C       get time units 
        INDX = 17
        CALL WDBSGI ( WDMSFL, DSN, INDX, LEN,
     $                TCODE, RETC )
        IF (RETC .EQ. -107) THEN
C         time code not present
          TCODE = 0
          FLAG = 1
        END IF
        IF (FLAG .EQ. 1  .AND.  RETCOD .NE. -6) THEN
C         missing some attributes
          RETCOD = -107
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDAINF
     I                   ( WDMSFL, DSN, NUMI, NUMR, INDXI, INDXR,
     O                     DATES, ATRIBI, ATRIBR,
     O                     RETCOD )
C
C     + + + PURPOSE + + +
C     This routine gets general atribute information about a time-series
C     data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSN, NUMI, NUMR, INDXI(*), INDXR(*),
     $          DATES(6,2), ATRIBI(*), RETCOD
      REAL      ATRIBR(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data set number
C     NUMI   - number of integer attributes to retrieve
C     NUMR   - number of real attributes to retrieve
C     INDXI  - array of index numbers of the integer attributes
C     INDXR  - array of index numbers of the real attributes
C     DATES  - period of available record
C              (_,1) - start of record
C              (_,2) - end of record
C     ATRIBI - array of retrieved integer attributes,
C              -999 returned if not present in data set
C     ATRIBR - array of retrieved real attributes
C              -999. returned if not present in data set
C     RETCOD - return code
C                  0 - all information successfully retrieved
C                 -6 - no data in the data set 
C                -81 - data set does not exist
C                -82 - data set is not a time-series data set
C               -107 - one or more attributes not present in data set
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG, DREC, RETC, LEN, ZERO, MISSI, FLAG, N
      REAL      MISSR
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI,   ZIPR
      EXTERNAL  WTFNDT, WDBSGI, WDBSGR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  GPFLG, ZERO, MISSI, MISSR
     $     /   1,    0,   -999, -999.  /
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
C     get period of record
      CALL WTFNDT ( WDMSFL, DSN, GPFLG,
     O              DREC, DATES(1,1), DATES(1,2), RETC )
      IF (RETC .EQ. -81  .OR.  RETC .EQ. -82) THEN
C       data set does not exist or is wrong type
        RETCOD = RETC
        LEN = 12
        CALL ZIPI ( LEN, ZERO, DATES )
        IF (NUMI .GT. 0) CALL ZIPI ( NUMI, MISSI, ATRIBI )
        IF (NUMR .GT. 0) CALL ZIPR ( NUMR, MISSR, ATRIBR )
      ELSE
C       data set does exist
        IF (RETC .EQ. -6) THEN
C         no data in data set
          LEN = 12
          CALL ZIPI ( LEN, ZERO, DATES )
          RETCOD = -6
        END IF
        FLAG = 0
        IF (NUMI .GT. 0) THEN
C         get integer attribute values
          LEN = 1
          DO 100 N = 1, NUMI
            CALL WDBSGI ( WDMSFL, DSN, INDXI(N), LEN,
     $                    ATRIBI(N), RETC )
            IF (RETC .EQ. -107) THEN
C             attribute not present on this data set
              ATRIBI(N) = -999
              FLAG = 1
            END IF
 100      CONTINUE
        END IF
        IF (NUMR .GT. 0) THEN
C         get real attribute values
          LEN = 1
          DO 200 N = 1, NUMR
            CALL WDBSGR ( WDMSFL, DSN, INDXR(N), LEN,
     O                    ATRIBR(N), RETC )
            IF (RETC .EQ. -107) THEN
C             attribute not present on this data set
              ATRIBR(N) = -999.
              FLAG = 1
            END IF
 200      CONTINUE
        END IF
        IF (FLAG .EQ. 1  .AND.  RETCOD .NE. -6) THEN
C         missing some attributes
          RETCOD = -107
        END IF
      END IF
C
      RETURN
      END
