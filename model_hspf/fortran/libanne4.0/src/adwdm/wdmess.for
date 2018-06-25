C
C
C
      SUBROUTINE   WDDSDL
     I                    (WDMSFL,DSN,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     routine to delete a data set from the WDMSFL with no user interaction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - dataser number to be deleted
C     RETCOD - return code
C                 0 - data set successfully deleted
C               -81 - data set does not exist
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,RREC,RIND,DSTYPE
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, WDRCDL
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDRCDL, WDFCUP, WDFDUP
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDDSCK (WDMSFL,DSN,
     O             RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       what dsn type?
        RIND= WDRCGO (WDMSFL,RREC)
        I= 6
        DSTYPE= WIBUFF(I,RIND)
C       fix pointers to other data sets
        I= 2
        CALL WDFCUP (WDMSFL,DSTYPE,DSN,I)
C       delete directory entry
        I= 0
        CALL WDFDUP (WDMSFL,DSN,I)
C       delete actual data set records
 100    CONTINUE
          RREC= WDRCDL (WDMSFL,RREC)
        IF (RREC.GT.0) GO TO 100
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSASP
     I                    (SAIND,SALEN,SATYP,
     M                     TIBUFF,
     O                     PSAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     adds space for search attribute on a dsn label if not present on it
C
C     + + + HISTORY + + +
C     08/30/94  kmf  added code for case where no space for index (-103)
C     05/25/01  kmf  Corrected calculation of the last available space
C                    for the attribute value.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SAIND,SALEN,SATYP,PSAVAL,RETCOD
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SAIND  - index number of search attribute to look for
C     SALEN  - length of search attribute
C     SATYP  - type of search attribute
C     TIBUFF - array containing data-set label
C     PSAVAL - pointer to search attribute information if available
C     RETCOD - flag indicating status of search attribute locate
C                 0 - attribute space added
C              -102 - attribute already on label
C              -103 - no space for this attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PSA,SACNT,SANMX,SASMX,SASTR,TSALEN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      TSALEN= SALEN
      IF (SATYP.EQ.3) THEN
C       character attribute, adjust size from characters to words
        TSALEN= TSALEN/4
      END IF
C
      PSAVAL= WDSASV (SAIND,TIBUFF)
      IF (PSAVAL.GT.0) THEN
C       attribute already on label
        RETCOD= -102
      ELSE
C       not there yet, try to add it
C       get pointer to search attribute start
        PSA  = TIBUFF(10)
C       current number of search attributes
        SACNT= TIBUFF(PSA)+ 1
C       max number of search attributes allowed
        SANMX= (TIBUFF(PSA+1)- PSA- 2)/2
C       last space to put search attributes in
        SASMX= TIBUFF(11)- 1
        IF (SACNT.LE.SANMX) THEN
C         space available for index, now check for space for value
          SASTR = PSA+ (SACNT*2)
          PSAVAL= TIBUFF(SASTR-1)
          IF (SACNT.GT.1) THEN
C           look for free space after last attribute
 10         CONTINUE
              PSAVAL= PSAVAL+ 1
            IF (TIBUFF(PSAVAL).NE.-999) GO TO 10
          END IF
C         check to see that enough space available
Ckmf      IF (PSAVAL+TSALEN.GT.SASMX) THEN
          IF (PSAVAL+TSALEN-1.GT.SASMX) THEN
C           oops, it wont fit
            PSAVAL= 0
            RETCOD= -103
          ELSE
C           update the label values
            TIBUFF(PSA)    = SACNT
            TIBUFF(SASTR)  = SAIND
            TIBUFF(SASTR+1)= PSAVAL
          END IF
        ELSE
C         no space for index for attribute
          RETCOD = -103
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDDTFG
     I                            (DREC,TIBUFF)
C
C     + + + PURPOSE + + +
C     determines if data is present in a WDMS data set,
C       returns 1 for yes, 0 for no
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DREC
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DREC   - label record number
C     TIBUFF - array containing data-set label information
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PREC,POFF,DATFLG,PDATV
      INTEGER*4 TDFREE,PDAT
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
      PDAT  = TIBUFF(11)
      PDATV = TIBUFF(12)
      TDFREE= TIBUFF(PDAT+1)
      CALL WDPTSP (TDFREE,
     O             PREC,POFF)
      IF (PREC.EQ.DREC.AND.POFF.EQ.PDATV) THEN
C       no data present on dataset
        DATFLG= 0
      ELSE
C       free space not first possible space, data is present
        DATFLG= 1
      END IF
C
      WDDTFG= DATFLG
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFDUP
     I                    (WDMSFL,DSN,DSFREC)
C
C     + + + PURPOSE + + +
C     updates a WDMS file directory record,
C     adds value if DSFREC> 0, deletes if DSFREC<=0
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSFREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DSFREC - data-set first record number, <0 for delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DIND,DREC,OPT,I,DELFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   WDDRRC,WDRCGO,WDRCDL
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDDRRC, WDRCDL, WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
      OPT = 1
      DREC= WDDRRC(WDMSFL,DSN,OPT)
      DIND= WDRCGO(WDMSFL,DREC)
      I   = MOD(DSN,500)+ 4
      IF (I.EQ.4) THEN
C       last position, not first
        I= 504
      END IF
      DELFLG= 0
      IF (DSFREC.GT.0) THEN
C       add this data set
        WIBUFF(  I,DIND)= DSFREC
        WIBUFF(512,DIND)= WIBUFF(512,DIND)+ 1
      ELSE
C       delete this data set
        WIBUFF(  I,DIND)= 0
        WIBUFF(512,DIND)= WIBUFF(512,DIND)- 1
        IF (WIBUFF(512,DIND).EQ.0) THEN
C         delete directory record, no more data sets in it
          I= WDRCDL(WDMSFL,RECNO(DIND))
C         update file def record, delete the reference to this record
          OPT= 2
          I= WDDRRC(WDMSFL,DSN,OPT)
          DELFLG= 1
        END IF
      END IF
      IF (DELFLG.EQ.0) THEN
C       update the changed directory record
        CALL WDRCUP(WDMSFL,DIND)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFCUP
     I                    (WDMSFL,DSTYPE,DSN,OPT)
C
C     + + + PURPOSE + + +
C     updates file defintions record data set counters and
C     pointers, also prev and next record pointers in dsn recs are
C     updated.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, DSTYPE, DSN, OPT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSTYPE - type of DSN
C     DSN    - data set number
C     OPT    - option, 1=add, 2= delete DSN
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,RREC,PCNT,PFDSN,OFDSN,NXDSN,PRDSN,CDSN,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     bring file definition record into memory
      RREC = 1
      RIND = WDRCGO(WDMSFL,RREC)
C     calculate pointers within file definition record
      PCNT = PTSNUM + (DSTYPE-1)* 2
      PFDSN= PCNT+ 1
C     save old first dsn
      OFDSN= WIBUFF(PFDSN,RIND)
C
      IF (OPT.EQ.1) THEN
C       add a dsn
C       update first dsn
        WIBUFF(PFDSN,RIND)= DSN
C       update count of data sets
        WIBUFF(PCNT,RIND) = WIBUFF(PCNT,RIND)+ 1
C       write out updated file def record
        CALL WDRCUP(WDMSFL,RIND)
C       update pointers in old first dsn if it exists
        IF (OFDSN.GT.0) THEN
C         it does exist
          CALL WDDSCK(WDMSFL,OFDSN,
     O                RREC,RETCOD)
          RIND= WDRCGO(WDMSFL,RREC)
          WIBUFF(1,RIND)= DSN
          CALL WDRCUP(WDMSFL,RIND)
        END IF
C       update pointers in new first dsn
        CALL WDDSCK(WDMSFL,DSN,
     O              RREC,RETCOD)
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(2,RIND)= OFDSN
        CALL WDRCUP(WDMSFL,RIND)
      ELSE
C       delete a dsn
        CDSN= OFDSN
 10     CONTINUE
          CALL WDDSCK(WDMSFL,CDSN,
     O                RREC,RETCOD)
          RIND = WDRCGO(WDMSFL,RREC)
          NXDSN= WIBUFF(2,RIND)
          IF (CDSN.EQ.DSN) THEN
C           this is the data set to delete
            PRDSN= WIBUFF(1,RIND)
            IF (DSN.EQ.OFDSN) THEN
C             update old first dsn, it is being deleted
              OFDSN= NXDSN
            END IF
            IF (NXDSN.GT.0) THEN
C             update back pointer in next record
              CALL WDDSCK(WDMSFL,NXDSN,
     O                    RREC,RETCOD)
              RIND= WDRCGO(WDMSFL,RREC)
              WIBUFF(1,RIND)= PRDSN
              CALL WDRCUP(WDMSFL,RIND)
            END IF
            IF (PRDSN.GT.0) THEN
C             update forward pointer in prev record
              CALL WDDSCK(WDMSFL,PRDSN,
     O                    RREC,RETCOD)
              RIND= WDRCGO(WDMSFL,RREC)
              WIBUFF(2,RIND)= NXDSN
              CALL WDRCUP(WDMSFL,RIND)
            END IF
          ELSE
C           have not found the right dsn yet, get ready to try next one
            CDSN= NXDSN
          END IF
        IF (CDSN.NE.DSN.AND.CDSN.GT.0) GO TO 10
C       update counter in first record
        RREC= 1
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(PCNT,RIND)= WIBUFF(PCNT,RIND)- 1
C       update first dsn
        WIBUFF(PFDSN,RIND)= OFDSN
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCDL
     I                            (WDMSFL,DREC)
C
C     + + + PURPOSE + + +
C     deletes a record in the WDMSFL and updates pointers as required
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DREC   - record to delete
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,RREC,PRNXRC,PRBKRC,SCNXRC,SCBKRC,FREREC,I,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     get pointers for record to delete
      RIND  = WDRCGO(WDMSFL,DREC)
      PRBKRC= WIBUFF(1,RIND)
      PRNXRC= WIBUFF(2,RIND)
      SCBKRC= WIBUFF(3,RIND)
      SCNXRC= WIBUFF(4,RIND)
      IF (PRBKRC.GT.0) THEN
C       a primary backward pointer exists, update that rec
        CALL WDDSCK (WDMSFL,PRBKRC,
     O               RREC,RETCOD)
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(2,RIND)= PRNXRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
      IF (PRNXRC.GT.0) THEN
C       a primary forward pointer exists, update that rec
        CALL WDDSCK (WDMSFL,PRNXRC,
     O               RREC,RETCOD)
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(1,RIND)= PRBKRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
      IF (SCBKRC.GT.0) THEN
C       a secondary backward pointer exists, update that rec
        RIND= WDRCGO(WDMSFL,SCBKRC)
        WIBUFF(4,RIND)= SCNXRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
      IF (SCNXRC.GT.0) THEN
C       a secondary forward pointer exists, update that rec
        RIND= WDRCGO(WDMSFL,SCNXRC)
        WIBUFF(3,RIND)= SCBKRC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
C     get directory record
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
C
C     determine current free record
      FREREC= WIBUFF(PFRREC,RIND)
C
C     initialize current record with 0 and free rec pointer
      RIND= WDRCGO(WDMSFL,DREC)
      DO 10 I= 1,512
        WIBUFF(I,RIND) = 0
 10   CONTINUE
      WIBUFF(2,RIND)= FREREC
C     update current record
      CALL WDRCUP(WDMSFL,RIND)
C     update free record
      RIND= WDRCGO(WDMSFL,RREC)
      WIBUFF(PFRREC,RIND)= DREC
      CALL WDRCUP(WDMSFL,RIND)
C
      WDRCDL= SCNXRC
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLBAX
     I                    (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,
     O                     PSA)
C
C     + + + PURPOSE + + +
C     add a new data-set label, but no search attributes or data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,PSA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DSTYPE - type of data set
C     NDN    - number of down pointers
C     NUP    - number of up pointers
C     NSA    - number of search attributes
C     NSASP  - amount of search attribute space
C     NDP    - number of data pointers
C     PSA    - pointer to search attribute space
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NIND,I,PRNREC,PDP,PUP,PDAT,PDATV,PSASTR
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGX, WDRCGO, WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDFCUP, WDFDUP, WDPTCL, WDRCGO, WDRCGX, WDRCUP, WDCRDT
C
C     + + + END SPECIFICATIONS + + +
C
C     get a record to put label on
      I     = 0
      PRNREC= 0
      NIND  = WDRCGX (WDMSFL,I,PRNREC)
      PRNREC= RECNO(NIND)
C     fill in new data-set number
      WIBUFF(5,NIND)= DSN
C     fill in new data-set type
      WIBUFF(6,NIND)= DSTYPE
C     update new record
      CALL WDRCUP (WDMSFL,NIND)
C     add new data set to directories
C     first, the data set directory
      CALL WDFDUP (WDMSFL,DSN,PRNREC)
C     next pointer in file def rec and dsn pointers
      I= 1
      CALL WDFCUP (WDMSFL,DSTYPE,DSN,I)
C
C     get NIND again since RECNO array may have changed
      NIND = WDRCGO(WDMSFL,PRNREC)
C
C     reserve position 7 for future use
      WIBUFF(7,NIND)= 0
C     set down position pointer
      PDP= 13
      WIBUFF(8,NIND)= PDP
C     set up position pointer
      PUP= PDP+ 1+ NDN
      WIBUFF(9,NIND)= PUP
C
C     set search attribute position pointer
      PSA= PUP+ 1+ NUP
      WIBUFF(10,NIND)= PSA
C     set search attribute start value pointer
      PSASTR= PSA+ 2+ (2* NSA)
      WIBUFF(PSA+1,NIND)= PSASTR
C     fill in undefined search attribute values
      DO 10 I= PSASTR, PSASTR+ NSASP- 1
        WIBUFF(I,NIND)= -999
 10   CONTINUE
C
C     set data group position pointer
      PDAT= PSASTR+ NSASP
      WIBUFF(11,NIND)= PDAT
C
C     set data group pointer counter
      WIBUFF(PDAT,NIND) = 0
C
C     set data position pointer
      PDATV= PDAT+ 2+ NDP
      WIBUFF(12,NIND)= PDATV
C
C     pointer to first free data position
      WIBUFF(PDAT+1,NIND)= WDPTCL(RECNO(NIND),PDATV)
C
C     update new data-set label
      CALL WDRCUP(WDMSFL,NIND)
C
C     set dataset creation date attribute
      CALL WDCRDT (WDMSFL,DSN)
C
      RETURN
      END
