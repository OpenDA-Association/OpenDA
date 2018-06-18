C
C
C
      SUBROUTINE   WADQCK
     I                   (MESSFL,DSN,ATIND,
     O                    ATCHK)
C
C     + + + PURPOSE + + +
C     given attribute offset for this data set check to see
C     if the attribute already exists on WDM file, return
C     0 if not exist, otherwise return absolute index number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DSN,ATIND,ATCHK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM message file
C     DSN    - data-set number on WDM file
C     ATIND  - attribute number being checked
C     ATCHK  - flag indicating attribute existance,
C              0 - doesnt exist, 1 - exists, (-) - bad return code value
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IDUM,LREC,LIND,RETCOD,PDAT,PDATV,LPOS,DSTYP,
     1           MXQNUM,ITMP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDSCHK, WATWDS
C
C     + + + END SPECIFICATIONS + + +
C
      DSTYP= 8
      CALL WDSCHK (MESSFL,DSN,DSTYP,
     O             LREC,IDUM,RETCOD)
C
C     see if question exists
      LIND  = WDRCGO(MESSFL,LREC)
      PDAT  = WIBUFF(11,LIND)
      PDATV = WIBUFF(12,LIND)
      MXQNUM= PDATV- PDAT- 2
      IF (ATIND.GT.0 .AND. ATIND.LE.MXQNUM) THEN
C       reasonable ATIND
        LPOS= PDAT+ 2+ 4*(ATIND-1)
        IF (WIBUFF(LPOS,LIND).EQ.0) THEN
C         doesnt exist
          ATCHK= 0
        ELSE
C         get absolute attribute index
          ITMP= WIBUFF(LPOS+1,LIND)
          CALL WATWDS (ITMP,
     O                 IDUM,ATCHK)
        END IF
      ELSE
C       bad QNUM, doesn't exist
        ATCHK= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADNSA
     I                   (MESSFL,DSINIT,
     M                    SAIND)
C
C     + + + PURPOSE + + +
C     given a search attribute index, determine if it exists
C     on the message file, if not return the next existing
C     index or value of 0 for no more attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DSINIT,SAIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DSINIT - flag indicating if data-set buffer is to be updated
C     SAIND  - attribute index
C
C     + + + PARAMETERS + + +
      INTEGER    MAXDSN
      PARAMETER (MAXDSN=10)
C
C     + + + SAVES + + +
      INTEGER    ATTIND(2,MAXDSN),DSNCNT,DSNBUF(MAXDSN)
      SAVE       ATTIND,DSNCNT,DSNBUF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,LSAIND,DONFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   WADDSI,WADQCK
C
C     + + + END SPECIFICATIONS + + +
C
C     WRITE(*,*) 'wadnsa:',MESSFL,DSINIT,SAIND
      IF (DSINIT.EQ.1) THEN
C       update buffers of data-set numbers and start/end attributes
        CALL WADDSI (MESSFL,MAXDSN,
     O               DSNCNT,DSNBUF,ATTIND)
      END IF
C
      IF (DSNCNT.GT.0) THEN
C       attribute data sets exist
        DONFG= 0
        I    = 0
 10     CONTINUE
          I= I+ 1
          IF (SAIND.GE.ATTIND(1,I) .AND. SAIND.LE.ATTIND(2,I)) THEN
C           current index would be on this data set
            DONFG = 1
            LSAIND= SAIND- ATTIND(1,I)
C           move along data set to find first valid attribute
 20         CONTINUE
C             see if this attribute exists
              LSAIND= LSAIND+ 1
              CALL WADQCK (MESSFL,DSNBUF(I),LSAIND,
     O                     SAIND)
            IF (SAIND.EQ.0) GO TO 20
          ELSE IF (SAIND.LT.ATTIND(1,I)) THEN
C           some missing, this is the next one
            SAIND= ATTIND(1,I)
            DONFG= 1
          END IF
        IF (I.LT.DSNCNT .AND. DONFG.EQ.0) GO TO 10
C
        IF (DONFG.EQ.0) THEN
C         index not out on message file
          SAIND= 0
        END IF
      ELSE
C       no attributes exist
        SAIND= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADADI
     I                   (MESSFL,DSN,ATIND,ID,
     I                    ILEN,IBUFF,INUM,IVAL)
C
C     + + + PURPOSE + + +
C     put attribute data set information on WDM message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,DSN,ATIND,ID,ILEN,INUM
      INTEGER     IVAL(INUM)
      CHARACTER*1 IBUFF(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM message file
C     DSN    - data-set number on WDM file
C     ATIND  - question number being added
C     ID     - id for portion of attribute being stored
C     ILEN   - length of input buffer
C     IBUFF  - input buffer of character data to store
C     INUM   - number of integer values to store
C     IVAL   - input buffer of integer parameters to store
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER    DPOS,DIND,DREC,LID,TLEN,GLEN,BREC,BPOS,LREC,
     1           PDAT,GRCNT,PDATVL,BATIND,LDSN
      SAVE       DPOS,DIND,DREC,LID,TLEN,GLEN,BREC,BPOS,LREC,
     1           PDAT,GRCNT,PDATVL,BATIND,LDSN
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,LIND,LPOS,DSTYP,PDATV,MXNGR,RETCOD,QWORD,NEWPOS,
     1           BIND,IFLG,CTLEN,RDWRFG,IDUM
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR, WDRCGO, WDRCGX, WDPTCL, WATWDC
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, WDRCGO, WDRCGX, WDRCUP, WDPTCL, WATWDC
      EXTERNAL   WDNXPS, WMSPTE, WDSCHK, WDPTSP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DPOS,DIND,DREC,BIND,BPOS,GLEN,LDSN
     1    / 0,   0,   0,   0,   0,   0,   0/
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 2
      RETCOD= 0
C
      IF (ID.EQ.2) THEN
C       end of group, finish label definition and update pointers
        CALL WDNXPS (MESSFL,RDWRFG,
     M               DREC,DPOS,
     O               DIND)
        WIBUFF(DPOS,DIND)= 0
        CALL WDRCUP(MESSFL,DIND)
C       new free data position
        CALL WDNXPS (MESSFL,RDWRFG,
     M               DREC,DPOS,
     O               DIND)
        NEWPOS= WDPTCL(DREC,DPOS)
C       update label pointer to free data position
        LIND= WDRCGO(MESSFL,LREC)
        WIBUFF(PDAT+1,LIND)= NEWPOS
C       put type, length, data set usage, update flag in 4th label pos
        LPOS= PDAT+ 2+ 4*(ATIND-BATIND)
        WIBUFF(LPOS+3,LIND)= IVAL(1)
        CALL WDRCUP(MESSFL,LIND)
      ELSE IF (ID.EQ.1) THEN
C       new group
        DSTYP= 8
C       check existance of data set
        CALL WDSCHK (MESSFL,DSN,DSTYP,
     O               LREC,GRCNT,RETCOD)
C
        IF (DSN.NE.LDSN) THEN
C         on a new data set, set base attribute index
          LDSN= DSN
          BATIND= ATIND
        END IF
C
        IF (RETCOD.EQ.0) THEN
C         space for another attribute?
          LIND = WDRCGO(MESSFL,LREC)
          PDAT = WIBUFF(11,LIND)
          PDATV= WIBUFF(12,LIND)
          MXNGR= (PDATV-PDAT-1)/4
          IF (ATIND-BATIND+1.GT.MXNGR) THEN
C           no more room for more attributes on this dsn
            RETCOD= -121
          END IF
        END IF
C
        IF (RETCOD.EQ.0) THEN
C         ok to add attribute to this data set
          GRCNT= GRCNT+ 1
          WIBUFF(PDAT,LIND)= GRCNT
C         get free data pos
          PDATVL= WIBUFF(PDAT+1,LIND)
          CALL WDPTSP (PDATVL,
     O                 DREC,DPOS)
          DIND= WDRCGO(MESSFL,DREC)
          IF (DPOS.EQ.0) THEN
C           start the question on the next record,
C           first reset record to current value
            IDUM = 0
            DREC= DREC- 1
            DIND= WDRCGX(MESSFL,IDUM,DREC)
            DREC= RECNO(DIND)
            DPOS= 5
            PDATVL= WDPTCL(DREC,DPOS)
          END IF
C         put name, index, and pointer to details on start of label
          LIND= WDRCGO(MESSFL,LREC)
          LPOS= PDAT+ 2+ 4*(ATIND-BATIND)
          WIBUFF(LPOS,LIND)  = IVAL(1)
          WIBUFF(LPOS+1,LIND)= IVAL(2)
          WIBUFF(LPOS+2,LIND)= PDATVL
          CALL WDRCUP(MESSFL,LIND)
        END IF
        LID= 1
      ELSE
C       store this portion of group block control word
        IF (ID.EQ.3 .OR. ID.EQ.5) THEN
C         storing integer parms, length of 4 for each value
          CTLEN= (4* INUM)- 1
          IFLG = 1
        ELSE
C         storing character data
          CTLEN= LENSTR(ILEN,IBUFF)
          IF (CTLEN.EQ.0) THEN
C           blank line, leave blank characters
            CTLEN= 1
          END IF
          IFLG= 0
        END IF
        IF (LID.EQ.ID) THEN
C         continuation of previous information
          TLEN= TLEN+ CTLEN+ 1
        ELSE
C         new portion of group, update length and block pos
          GLEN= 0
          TLEN= CTLEN+ 1
          IF (LID.NE.1) THEN
C           DREC/DPOS at end of last info put, need to update
            CALL WDNXPS (MESSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
          END IF
          BPOS= DPOS
          BREC= DREC
          LID = ID
        END IF
C       put block control word at start of info
        BIND = WDRCGO(MESSFL,BREC)
        QWORD= WATWDC(ID,TLEN)
        WIBUFF(BPOS,BIND)= QWORD
        CALL WDRCUP(MESSFL,BIND)
C
        IF (IFLG.EQ.0) THEN
C         character buffer to store
          CALL WMSPTE (MESSFL,CTLEN,IBUFF,
     M                 DREC,DPOS,GLEN)
C
        ELSE
C         store integer word(s)
          DO 10 I= 1,INUM
            CALL WDNXPS (MESSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
            WIBUFF(DPOS,DIND)= IVAL(I)
 10       CONTINUE
          CALL WDRCUP(MESSFL,DIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGTL
     I                   (MESSFL,DSN,ATIND,
     O                    ATNAM,DPTR,ATTYP,ATLEN,ATUSWD,ATUPD)
C
C     + + + PURPOSE + + +
C     given attribute index, get type, length, data set usage,
C     and update flag as well as starting pos of other data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,DSN,ATIND,DPTR,ATTYP,ATLEN,ATUSWD,ATUPD
      CHARACTER*1 ATNAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM message file
C     DSN    - data-set number on WDM file
C     ATIND  - attribute index
C     ATNAM  - attribute name
C     DPTR   - pointer to start of other attribute data
C     ATTYP  - attribute typ
C     ATLEN  - attribute length
C     ATUSWD - integer word of required and optional data set usage
C     ATUPD  - attribute update flag
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,ITMP,IVAL(2),LREC,LPOS,LIND,PDAT,BATIND,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WATWDS, WATTSP
C
C     + + + END SPECIFICATIONS + + +
C
C     get label index
      CALL WDDSCK(MESSFL,DSN,
     O            LREC,RETCOD)
      LIND= WDRCGO(MESSFL,LREC)
C     get needed pointers
      PDAT= WIBUFF(11,LIND)
C     find base attribute
      LPOS= PDAT+ 3
      ITMP= WIBUFF(LPOS,LIND)
      CALL WATWDS (ITMP,
     O             I,BATIND)
C     now find attribute position based on offset of base index
      LPOS= PDAT+ 2+ 4*(ATIND-BATIND)
C     get first two words to build name
      IVAL(1)= WIBUFF(LPOS,LIND)
      ITMP   = WIBUFF(LPOS+1,LIND)
C     split second word into last part of name
      CALL WATWDS (ITMP,
     O             IVAL(2),I)
C     build name
      ITMP= IVAL(1)
      DO 10 J= 1,6
        ATNAM(J)= CHAR(MOD(ITMP,256))
        IF (J.EQ.4) THEN
C         switch to second word
          ITMP= IVAL(2)
        ELSE
          ITMP= ITMP/256
        END IF
 10   CONTINUE
C     get pointer to start of data
      DPTR= WIBUFF(LPOS+2,LIND)
C     get last word of label containing attribute parms
      IVAL(1)= WIBUFF(LPOS+3,LIND)
      CALL WATTSP (IVAL(1),
     O             ATTYP,ATLEN,ATUSWD,ATUPD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADDSI
     I                   (MESSFL,DSNMAX,
     O                    DSNCNT,DSN,ATTIND)
C
C     + + + PURPOSE + + +
C     determine the data-set numbers containing the attribute data sets
C     and the first and last attributes on each data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DSNMAX,DSNCNT,DSN(DSNMAX),ATTIND(2,DSNMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for WDM message file
C     DSNMAX - maximum number of attribute data sets
C     DSNCNT - number of data sets containing attributes
C     DSN    - array of data-set numbers containing attributes
C     ATTIND - array of first and last attributes on each data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,LPDAT,PDATV,LREC,LIND,LPOS,ITMP,IVAL,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDDSCK, WDRCGO, WATWDS, ZIPI
C
C     + + + END SPCIFICATIONS + + +
C
C     WRITE(*,*) 'waddsi:',MESSFL,DSNMAX
      J= 0
      CALL ZIPI(DSNMAX,J,DSN)
      I= DSNMAX*2
      CALL ZIPI(I,J,ATTIND)
C
      DSNCNT= 1
 10   CONTINUE
C       try the next data set
        IF (DSN(DSNCNT).GT.0) THEN
C         need to increment counter
          DSNCNT= DSNCNT+ 1
        END IF
C
        IF (DSNCNT.EQ.1) THEN
C         get first data set from main record label
          LREC= 1
          LIND= WDRCGO (MESSFL,LREC)
          I   = PTSNUM+ 15
          DSN(DSNCNT)= WIBUFF(I,LIND)
        ELSE
C         get next data set from last data-set label
          DSN(DSNCNT)= WIBUFF(2,LIND)
        END IF
C       WRITE(*,*) 'next attr data set:',DSNCNT,DSN(DSNCNT)
        IF (DSN(DSNCNT).GT.0) THEN
C         process this data set
          CALL WDDSCK(MESSFL,DSN(DSNCNT),
     O                LREC,RETCOD)
          LIND = WDRCGO(MESSFL,LREC)
          LPDAT= WIBUFF(11,LIND)
          PDATV= WIBUFF(12,LIND)
          LPOS = LPDAT+ 2
          IF (WIBUFF(LPOS,LIND).GT.0) THEN
C           get first label index
            IVAL= WIBUFF(LPOS+1,LIND)
            CALL WATWDS (IVAL,
     O                   I,ATTIND(1,DSNCNT))
C           now find last label
            LPOS= PDATV
 20         CONTINUE
C             back up to last label and get index
              LPOS= LPOS- 4
            IF (WIBUFF(LPOS,LIND).EQ.0) GO TO 20
C           get last label index
            IVAL= WIBUFF(LPOS+1,LIND)
            CALL WATWDS (IVAL,
     O                   I,ATTIND(2,DSNCNT))
C           WRITE(*,*) 'found:',DSNCNT,ATTIND(1,DSNCNT),ATTIND(2,DSNCNT)
          ELSE
C           nothing out on this data set, ignore it
            DSN(DSNCNT)= 0
C           WRITE(*,*) 'nothing in data set'
          END IF
          IF (DSNCNT.GT.1) THEN
C           sort indices
            I= DSNCNT
 30         CONTINUE
              IF (ATTIND(1,I).LT.ATTIND(1,I-1)) THEN
C               need to switch these indices
                ITMP= ATTIND(1,I)
                ATTIND(1,I)  = ATTIND(1,I-1)
                ATTIND(1,I-1)= ITMP
                ITMP= ATTIND(2,I)
                ATTIND(2,I)  = ATTIND(2,I-1)
                ATTIND(2,I-1)= ITMP
                ITMP    = DSN(I)
                DSN(I)  = DSN(I-1)
                DSN(I-1)= ITMP
              END IF
              I= I- 1
            IF (I.GT.1) GO TO 30
          END IF
        END IF
      IF (DSN(DSNCNT).GT.0 .AND. DSNCNT.LT.DSNMAX) GO TO 10
C
      IF (DSN(DSNCNT).EQ.0) THEN
C       count is one ahead of actual total
        DSNCNT= DSNCNT- 1
      END IF
C
C     WRITE(*,*) 'WADDSI:',DSNCNT,DSNMAX
C     IF (DSNCNT.GT.0) THEN
C       DO 40 I= 1,DSNMAX
C         WRITE(*,*) I,DSN(I),ATTIND(1,I),ATTIND(2,I)
C40     CONTINUE
C     END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDCRDT
     I                   (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     set 16-character DSN creation date attribute
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAIND, DATE(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSYSD,WDSETD,WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
      SAIND= 443
      CALL WDSYSD
     O            (DATE)
      CALL WDSETD (WDMSFL,DSN,SAIND,DATE)
      CALL WDMODT (WDMSFL,DSN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDMODT
     I                   (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     set 16-character DSN modification date attribute
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAIND, DATE(4)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSYSD,WDSETD
C
C     + + + END SPECIFICATIONS + + +
C
      SAIND= 444
      CALL WDSYSD
     O            (DATE)
      CALL WDSETD (WDMSFL,DSN,SAIND,DATE)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSETD
     I                   (WDMSFL,DSN,SAIND,DATE)
C
C     + + + PURPOSE + + +
C     set 16-character date attribute
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,SAIND,DATE(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - timeseries data-set number
C     SAIND  - search attribute index (443 for DATCRE, 444 for DATMOD)
C     DATE   - integer array containing character representation of date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TDSFRC,RETCOD,RIND,SATYP,SALEN,PSAVAL,I
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK,WDSASP,WDRCGO,WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     determine where data set starts
      CALL WDDSCK (WDMSFL,DSN,
     O             TDSFRC,RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       bring label into memory
        RIND= WDRCGO(WDMSFL,TDSFRC)
        IF ((RIND .LE. 0) .OR. (RIND .GT. 512)) THEN
C         invalid record
          WRITE (99,*) 'BAD RIND:  WDMSFL,DSN,TDSFRC,RIND',WDMSFL,DSN,
     $                  TDSFRC,RIND
        ELSE
C         look for attribute
          SALEN= 16
          SATYP= 3
C         where do we put it?
          CALL WDSASP (SAIND,SALEN,SATYP,
     M                 WIBUFF(1,RIND),
     O                 PSAVAL,RETCOD)
          IF ((RETCOD .EQ. -102) .OR. (RETCOD.EQ.0)) THEN
C           attribute already present or successfully added, fill in value
            DO 10 I= 1, 4
             WIBUFF(PSAVAL+ I- 1,RIND)= DATE(I)
 10         CONTINUE
            CALL WDRCUP(WDMSFL,RIND)
          END IF
        END IF
      END IF
C
      RETURN
      END
