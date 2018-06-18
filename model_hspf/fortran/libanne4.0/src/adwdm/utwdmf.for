C     utwdmf.f 2.1 9/4/91
C
C
C
      SUBROUTINE   WMSQCK
     I                   (WDMSFL,DSN,GNUM,
     O                    QCHK)
C
C     + + + PURPOSE + + +
C     Check to see if a group already exists in a data set on WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GNUM,QCHK
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     GNUM   - group number being checked
C     QCHK   - flag indicating existance,
C                0 - data set does not exist or bad group number
C                1 - group exists
C              -81 - data set does not exist
C              -82 - data set exists, but is not a message type data set
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IDUM,LREC,LIND,RETCOD,PDAT,PDATV,LPOS,DSTYP,MXGNUM
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDSCHK
C
C     + + + END SPECIFICATIONS + + +
C
      DSTYP= 9
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             LREC,IDUM,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       see if group exists
        LIND  = WDRCGO(WDMSFL,LREC)
        PDAT  = WIBUFF(11,LIND)
        PDATV = WIBUFF(12,LIND)
        MXGNUM= PDATV- PDAT- 2
        IF (GNUM.GT.0 .AND. GNUM.LE.MXGNUM) THEN
C         reasonable GNUM
          LPOS= PDAT+ GNUM+ 1
          IF (WIBUFF(LPOS,LIND).EQ.0) THEN
C           doesnt exist
            QCHK= 0
          ELSE
            QCHK= 1
          END IF
        ELSE
C         bad GNUM, doesn't exist
          QCHK= 0
        END IF
      ELSE
        QCHK= RETCOD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSANG
     I                   (WDMSFL,DSN,GNUM,
     O                    LREC,DREC,DPOS,GRCNT,
     O                    PDAT,PDATVL,RETCOD)
C
C     + + + PURPOSE + + +
C     Check that data-set DSN exists and that there is space
C     in the data set for a new group.  Determines pointers
C     to start of new group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GNUM,LREC,DREC,DPOS,GRCNT,
     1          PDAT,PDATVL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     GNUM   - group number in data set
C     LREC   - first record number of DSN on WDM file (contains label)
C     DREC   - record number on WDM file for start of group-number GNUM
C     DPOS   - position on DREC for start of group-number GNUM
C     GRCNT  - counter for number of groups on data set (including this group)
C     PDAT   - pointer to the start of group pointers
C     PDATVL - pointer to beginning of data for this group
C     RETCOD - return code
C                0 - group successfully added to data set
C                1 - exists
C               -3 - data set does not exist
C              -22 - data set exists, but is not message data-set type
C              -26 - no room for another group on data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    PDATV,MXNGR,DIND,LIND,IDUM,DSTYP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO, WDRCGX, WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCGX, WDPTCL, WDSCHK, WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
      IDUM  = 0
      RETCOD= 0
      DSTYP = 9
C     check existance
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             LREC,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       space for another group
        LIND = WDRCGO(WDMSFL,LREC)
        PDAT = WIBUFF(11,LIND)
        PDATV= WIBUFF(12,LIND)
        MXNGR= PDATV- PDAT- 1
        IF (GNUM.GT.MXNGR) RETCOD= -26
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       ok to add the group to this data set
        PDATVL= WIBUFF(PDAT+1,LIND)
C       increment group count
        GRCNT = GRCNT+ 1
C       get start of data (record and position)
        CALL WDPTSP (PDATVL,
     O               DREC,DPOS)
        DIND  = WDRCGO(WDMSFL,DREC)
        IF (DPOS.EQ.0) THEN
C         start the group on the next record,
C         first reset record to current value
          DREC= DREC- 1
          DIND= WDRCGX(WDMSFL,IDUM,DREC)
          DREC= RECNO(DIND)
          DPOS= 5
          PDATVL= WDPTCL(DREC,DPOS)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSADI
     I                   (WDMSFL,DSN,GNUM,CLASS,ID,ORDER,
     I                    ILEN,IBUFF,INUM,IVAL,IFLG)
C
C     + + + PURPOSE + + +
C     Add portion of group-number GNUM information to DSN.
C     Will be called more than once to define a given group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,GNUM,CLASS,ID,ORDER,ILEN,INUM,IFLG
      INTEGER     IVAL(INUM)
      CHARACTER*1 IBUFF(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file,
C              negative value indicates termination of group
C              and updating of label
C     GNUM   - group number being added
C     CLASS  - class of information being added
C              1 - 1-dimensional parameter      4 - menu
C              2 - 2-dimensional parameter      5 - file
C              3 - text
C     ID     - id for portion of group being added
C              examples:  CLASS  ID  Description
C                           1     4  default value for parameter field
C                           4     6  help for menu screen
C                           5     3  status of file
C     ORDER  - order of information being added
C     ILEN   - length of input buffer (minimum of 1)
C     IBUFF  - input buffer of character data to add
C     INUM   - number of integer values to add (minimum of 1)
C     IVAL   - input buffer of integer parameters to add
C     IFLG   - flag indicating type of data being added
C              0 - character data
C              1 - integer data
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER     DPOS,DIND,DREC,LDSN,LGNUM,LID,LORDER,TLEN,GLEN,
     1            BREC,BPOS,LREC,PDAT,GRCNT,PDATVL,ADIFG
      SAVE        DPOS,DIND,DREC,LDSN,LGNUM,LID,LORDER,TLEN,GLEN,
     1            BREC,BPOS,LREC,PDAT,GRCNT,PDATVL,ADIFG
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,LIND,RETCOD,BCWORD,NEWPOS,BIND,LPOS,
     1            CTLEN,RDWRFG,INEW
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR, WDRCGO, WDPTCL, WMSBCV
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, WDRCGO, WDRCUP, WDPTCL, WMSBCV
      EXTERNAL   WDNXPS, WMSANG, WMSPTE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DPOS,DIND,DREC,LDSN,LGNUM,BIND,BPOS,GLEN,ADIFG
     1    / 0,   0,   0,   0,    0,    0,   0,   0,   0/
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 2
      RETCOD= 0
      INEW  = 0
C
      IF (DSN.LT.0 .AND. ADIFG.GT.0) THEN
C       end of group and some info was added, put 0 in for terminator
        CALL WDNXPS (WDMSFL,RDWRFG,
     M               DREC,DPOS,
     O               DIND)
        WIBUFF(DPOS,DIND)= 0
        CALL WDRCUP(WDMSFL,DIND)
C       new free data position
        CALL WDNXPS (WDMSFL,RDWRFG,
     M               DREC,DPOS,
     O               DIND)
        NEWPOS= WDPTCL(DREC,DPOS)
C       store new label info
        LIND= WDRCGO(WDMSFL,LREC)
        WIBUFF(PDAT,LIND)  = GRCNT
        WIBUFF(PDAT+1,LIND)= NEWPOS
        LPOS= PDAT+ GNUM+ 1
        WIBUFF(LPOS,LIND)  = PDATVL
        CALL WDRCUP(WDMSFL,LIND)
C       reinit flag indicating information has been added
        ADIFG= 0
        LDSN = DSN
      ELSE IF (DSN.NE.LDSN .OR. GNUM.NE.LGNUM) THEN
C       new group
        INEW  = 1
        LDSN  = DSN
        LGNUM = GNUM
        LID   = -1
        LORDER= -1
C       set up and get parms for new group
        CALL WMSANG (WDMSFL,DSN,GNUM,
     O               LREC,DREC,DPOS,GRCNT,
     O               PDAT,PDATVL,RETCOD)
C       update flag indicating information has been added
        ADIFG= 1
      END IF
C
      IF (RETCOD.EQ.0 .AND. DSN.GT.0) THEN
C       store this portion of group block control word
        IF (IFLG.EQ.0) THEN
C         storing character data
          CTLEN= LENSTR(ILEN,IBUFF)
          IF (CTLEN.EQ.0) THEN
C           blank line, leave blank characters
            CTLEN= 1
          END IF
        ELSE
C         storing integer parms, length of 4 for each value
          CTLEN= (4* INUM)- 1
        END IF
        IF (LID.EQ.ID .AND. LORDER.EQ.ORDER .AND. ID.NE.23) THEN
C         continuation of previous information (not HIDE info)
          TLEN= TLEN+ CTLEN+ 1
        ELSE
C         new portion of group, update length and block pos
          GLEN  = 0
          TLEN  = CTLEN+ 1
          IF (INEW.EQ.0) THEN
C           only skip to next free data position if continuing group
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
          END IF
          BPOS  = DPOS
          BREC  = DREC
          LID   = ID
          LORDER= ORDER
        END IF
        BIND   = WDRCGO(WDMSFL,BREC)
        BCWORD = WMSBCV(CLASS,ID,ORDER,TLEN)
        WIBUFF(BPOS,BIND)= BCWORD
        CALL WDRCUP(WDMSFL,BIND)
C
        IF (IFLG.EQ.0) THEN
C         buffer to store
          CALL WMSPTE (WDMSFL,CTLEN,IBUFF,
     M                 DREC,DPOS,GLEN)
        ELSE
C         store integer word(s)
          DO 10 I= 1,INUM
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
            WIBUFF(DPOS,DIND)= IVAL(I)
 10       CONTINUE
          CALL WDRCUP(WDMSFL,DIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDNXPS
     I                   (WDMSFL,RDWRFG,
     M                    DREC,DPOS,
     O                    DIND)
C
C     + + + PURPOSE + + +
C     Get the next data position on a WDM file.  If RDWRFG is 1 and
C     a new record is needed, the next data record is brought into
C     the WDM buffer of records.  If RDWRFG is 2 and a new record
C     is needed, the current record is updated and the next free
C     record is brought into the WDM buffer of records.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RDWRFG,DREC,DPOS,DIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     RDWRFG - read/write flag (1- read, 2- write)
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record (both DREC and DIND)
C     DIND   - index of record in WDM buffer of records
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    IDUM
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGX, WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCUP, WDRCGX, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      IDUM= 0
      DIND= WDRCGO (WDMSFL,DREC)
      DPOS= DPOS+ 1
      IF (DPOS.GT.512) THEN
C       next record
        IF (RDWRFG.EQ.1) THEN
C         reading, bring next record into buffer
          DREC= WIBUFF(4,DIND)
          DIND= WDRCGO (WDMSFL,DREC)
        ELSE
C         writing, update current record, bring in new one
          CALL WDRCUP(WDMSFL,DIND)
          DIND= WDRCGX(WDMSFL,IDUM,DREC)
          DREC= RECNO(DIND)
        END IF
        DPOS= 5
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDNXDV
     I                   (WDMSFL,
     M                    DREC,DPOS,
     O                    DVAL)
C
C     + + + PURPOSE + + +
C     Move to the next data position and return the
C     integer equivalent of the data value.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DREC,DPOS,DVAL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record (both DREC and DIND)
C     DVAL   - data value on WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DIND,RDWRFG
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDNXPS
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 1
C     move to next data position
      CALL WDNXPS (WDMSFL,RDWRFG,
     M             DREC,DPOS,
     O             DIND)
C
C     get the data value
      DVAL= WIBUFF(DPOS,DIND)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPRPS
     I                   (WDMSFL,
     M                    DREC,DPOS,
     O                    DIND)
C
C     + + + PURPOSE + + +
C     Get the previous data position on a WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DREC,DPOS,DIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record (both DREC and DIND)
C     DIND   - index of record in WDM buffer of records
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      DIND= WDRCGO (WDMSFL,DREC)
      DPOS= DPOS- 1
      IF (DPOS.LT.5) THEN
C       bring previous record into buffer
        DREC= WIBUFF(3,DIND)
        DIND= WDRCGO (WDMSFL,DREC)
        DPOS= 512
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSPTE
     I                   (WDMSFL,CTLEN,IBUFF,
     M                    DREC,DPOS,GLEN)
C
C     + + + PURPOSE + + +
C     Add one record of text to WDM file.  May be called more
C     than once if text is multiple lines (e.g. help information).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,CTLEN,DREC,DPOS,GLEN
      CHARACTER*1 IBUFF(CTLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     CTLEN  - text length being added this call (minimum of 1)
C     IBUFF  - array of size CTLEN to be put on WDM file
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record
C     GLEN   - counter to keep track of when to write to WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER     DIND
      CHARACTER*4 CTXT4
      SAVE        DIND,CTXT4
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     J,ILEN,RDWRFG,CFLG
      CHARACTER*1 CNULL
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD, CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDNXPS
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A4)
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 2
      CNULL = CHAR(0)
C
      IF (MOD(GLEN,4).EQ.0) THEN
C       not continuing an old group with space in last pos
        CFLG = 0
        CTXT4= ' '
      ELSE
C       continue old group
        CFLG = 1
      END IF
C
      ILEN = 0
 100  CONTINUE
        ILEN= ILEN+ 1
        GLEN= GLEN+ 1
        IF (ILEN.LE.CTLEN) THEN
          CTXT4(GLEN:GLEN)= IBUFF(ILEN)
        ELSE
C         write null to terminate record
          CTXT4(GLEN:GLEN)= CNULL
        END IF
        IF (MOD(GLEN,4).EQ.0) THEN
C         time to put in buffer
          GLEN= 0
          IF (CFLG.EQ.0) THEN
C           get next data position
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
          END IF
          CFLG= 0
          READ (CTXT4,1000) WIBUFF(DPOS,DIND)
          CTXT4= ' '
        END IF
      IF (ILEN.LE.CTLEN) GO TO 100
C
      J= MOD(GLEN,4)
      IF (J.GT.0) THEN
C       characters left
        IF (CFLG.EQ.0) THEN
C         get next data position
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 DREC,DPOS,
     O                 DIND)
        END IF
        READ (CTXT4,1000) WIBUFF(DPOS,DIND)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTE
     I                   (WDMSFL,TLEN,LLEN,
     M                    DREC,DPOS,GLEN,MLEN,
     O                    OLEN,OBUFF,CONT)
C
C     + + + PURPOSE + + +
C     Get one record of text off WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,TLEN,LLEN,DREC,DPOS,GLEN,MLEN,OLEN,CONT
      CHARACTER*1 OBUFF(LLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     TLEN   - total length of text (may be more than one record)
C     LLEN   - maximum size of record to get
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record
C     GLEN   - counter to keep track of when to read off WDM file
C              should be initialized to 0 for first call
C     MLEN   - number of characters retrieved so far (must be <= TLEN)
C              should be initialized to 0 for first call
C     OLEN   - actual size of record retreived
C     OBUFF  - array of size LLEN containing OLEN characters retrieved
C     CONT   - indicator flag for text
C              0 - no more text available
C              1 - more text available
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DONFG,RDWRFG,DIND,INULL,ITMP
      CHARACTER*1 BLNK,CTMP
      CHARACTER*4 CTXT4
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, ZIPC, WDNXPS
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK  = ' '
      INULL = 0
      OLEN  = 0
      CONT  = 1
      DONFG = 0
      RDWRFG= 1
      CALL ZIPC (LLEN,BLNK,OBUFF)
      DIND  = WDRCGO(WDMSFL,DREC)
      WRITE (CTXT4,2000) WIBUFF(DPOS,DIND)
C
 10   CONTINUE
C       read until null character
        IF (MOD(GLEN,4).EQ.0) THEN
C         read off WDM file (next position if already read some)
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 DREC,DPOS,
     O                 DIND)
C         read more text
          WRITE (CTXT4,2000) WIBUFF(DPOS,DIND)
          GLEN= 0
        END IF
        GLEN= GLEN+ 1
        MLEN= MLEN+ 1
        CTMP= CTXT4(GLEN:GLEN)
        ITMP= MOD(ICHAR(CTMP),128)
        IF (ITMP.EQ.INULL) THEN
C         record terminator
          DONFG= 1
        ELSE IF (OLEN.LT.LLEN) THEN
C         save character in output buffer
          OLEN= OLEN+ 1
          OBUFF(OLEN)= CTMP
        END IF
        IF (MLEN.GE.TLEN) THEN
C         end of group text
          CONT = 0
          CTXT4= ' '
          DONFG= 1
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSBTR
     I                   (WDMSFL,NREC,
     M                    DREC,DPOS,MLEN,GLEN,
     O                    TOPFG)
C
C     + + + PURPOSE + + +
C     Back up NREC text records in a text group and reset the
C     pointers for that record.  Usually used in conjunction
C     with WMSGTE for reviewing text in a scrolling window.
C     Input values of GLEN and MLEN are set by previous call
C     to WMSGTE.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NREC,DREC,DPOS,MLEN,GLEN,TOPFG
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     NREC   - number of records to back up
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record
C     GLEN   - counter to keep track of when to read off WDM file
C     MLEN   - number of characters retrieved so far (must be <= TLEN)
C     TOPFG  - indicator flag for beginning of group
C              0 - not the beginning of the group
C              1 - beginning of group reached
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      CHARACTER*4 CTXT4
      SAVE        CTXT4
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DONFG,DIND,INULL,ITMP,CREC
      CHARACTER*1 CTMP
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR, MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WDPRPS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CTXT4/'    '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      INULL = 0
      TOPFG = 0
      CREC  = 0
C     get current data value in CTXT4
      DIND= WDRCGO (WDMSFL,DREC)
      WRITE (CTXT4,2000) WIBUFF(DPOS,DIND)
C
 10   CONTINUE
        DONFG= 0
        CREC = CREC+ 1
 20     CONTINUE
C         back up until null character
          GLEN= GLEN- 1
          IF (GLEN.EQ.0) THEN
C           read off WDM file (previous position)
            CALL WDPRPS (WDMSFL,
     M                   DREC,DPOS,
     O                   DIND)
C           read more text
            WRITE (CTXT4,2000) WIBUFF(DPOS,DIND)
            GLEN= 4
          END IF
          MLEN= MLEN- 1
          IF (MLEN.EQ.0) THEN
C           beginning of group
            TOPFG= 1
          END IF
          CTMP= CTXT4(GLEN:GLEN)
          ITMP= MOD(ICHAR(CTMP),128)
          IF (ITMP.EQ.INULL) THEN
C           record terminator
            DONFG= 1
          END IF
        IF (DONFG.EQ.0 .AND. TOPFG.EQ.0) GO TO 20
      IF (CREC.LT.NREC .AND. TOPFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTO
     I                   (WDMSFL,SUCIFL,TLEN,
     M                    DREC,DPOS)
C
C     + + + PURPOSE + + +
C     Starting at the input DREC and DPOS, get and write text to a
C     sequential file until end of text is reached.  DREC and DPOS
C     are output as the position at the end of the text.  Usually
C     used to export text portions of message data-set groups.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,TLEN,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for output file
C     TLEN   - total length of text
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,GLEN,MLEN,I256,OLEN,CONT
      CHARACTER*1 OBUFF(256)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, WMSGTE
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (256A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I256= 256
      GLEN= 0
      MLEN= 0
C
 10   CONTINUE
C       write text lines until cont is 0
        CALL WMSGTE (WDMSFL,TLEN,I256,
     M               DREC,DPOS,GLEN,MLEN,
     O               OLEN,OBUFF,CONT)
        IF (OLEN.GT.0) THEN
C         output string, then cr/lf
          J = LENSTR(OLEN,OBUFF)
          WRITE (SUCIFL,2000) (OBUFF(I),I=1,J)
        ELSE
C         only write a cr/lf
          WRITE (SUCIFL,2000)
        END IF
      IF (CONT.EQ.1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSBCX
     I                   (WDMSFL,BCWPTR,
     O                    DREC,DPOS,CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Given pointer to a block control word, return its
C     record number and position, and the information
C     stored there (CLASS,ID,ORDER,TLEN).  Usually used
C     to find start of help information.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,BCWPTR,DREC,DPOS,CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     BCWPTR - pointer to block control word
C     DREC   - record number of data on WDM file
C     DPOS   - position of data on data record
C     CLASS  - class of information being added
C              1 - 1-dimensional parameter      4 - menu
C              2 - 2-dimensional parameter      5 - file
C              3 - text
C     ID     - id for portion of group being added
C              examples:  CLASS  ID  Description
C                           1     4  default value for parameter field
C                           4     6  help for menu screen
C                           5     3  status of file
C     ORDER  - order of information
C     TLEN   - total number of characters in the block
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    DIND
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WMSBCS, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
C     split pointer into record/offset
      CALL WDPTSP (BCWPTR,
     O             DREC,DPOS)
      IF (DPOS.EQ.0) THEN
C       if position was 512, WDPTSP will return as 0, reset to 512
        DPOS= 512
        DREC= DREC- 1
        WRITE (99,*) 'In WMSBCX:  Reset DPOS from 0 to 512, DREC-1'
      END IF
      DIND= WDRCGO (WDMSFL,DREC)
      CALL WMSBCS (WIBUFF(DPOS,DIND),
     O             CLASS,ID,ORDER,TLEN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSFBC
     I                    (WDMSFL,DSN,GNUM,
     O                     DREC,DPOS,BCWORD)
C
C     + + + PURPOSE + + +
C     Get first block control word and its position
C     for group GNUM in a message data set.
C     A STOP is encountered when group GNUM does not exist.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GNUM,DREC,DPOS,BCWORD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number
C     GNUM   - group number
C     DREC   - record number of block control word on WDM file
C     DPOS   - position of block control word on DREC
C     BCWORD - block control word
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RETCOD,LREC,LPOS,LIND,PDAT
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WMSQCK, WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
C     assume group doesnt exist
      BCWORD= 0
C
C     check existence of DSN
      CALL WMSQCK (WDMSFL,DSN,GNUM,
     O             RETCOD)
C
      IF (RETCOD.EQ.1) THEN
C       all ok, get label index
        CALL WDDSCK (WDMSFL,DSN,
     O               LREC,RETCOD)
        LIND= WDRCGO(WDMSFL,LREC)
C       get needed pointers
        PDAT= WIBUFF(11,LIND)
        LPOS= PDAT+ GNUM+ 1
        CALL WDPTSP (WIBUFF(LPOS,LIND),
     O               DREC,DPOS)
        LIND = WDRCGO(WDMSFL,DREC)
        BCWORD= WIBUFF(DPOS,LIND)
      ELSE
C       bad group number
        OPEN (UNIT=88,FILE='BADWDM.FIL',STATUS='UNKNOWN')
        WRITE (88,*)
        WRITE (88,*) 'BAD WDMSFL,DSN,GNUM,RETCOD',WDMSFL,DSN,GNUM,RETCOD
        WRITE (88,*)
        CLOSE (UNIT=88)
        STOP
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSIDP
     I                   (WDMSFL,DSN,GNUM,ID,
     O                    DREC,DPOS)
C
C     + + + PURPOSE + + +
C     Return the record number and position on the WDM
C     file for the given ID.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GNUM,ID,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     GNUM   - group number in data set
C     ID     - identification of information
C     DREC   - record number on WDM file (0 returned if no match)
C     DPOS   - position on record (0 returned if no match)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,CLASS,LID,ORDER,TLEN,ILEN,IMATCH,DIND,BCWORD,RDWRFG
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSFBC, WMSBCS, WDNXPS, WDNXDV
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 1
      IMATCH= 0
C     get first block control word for group
      CALL WMSFBC (WDMSFL,DSN,GNUM,
     O             DREC,DPOS,BCWORD)
C
 10   CONTINUE
C       look through group for matching ids
        IF (BCWORD.EQ.0) THEN
C         ids never matched or bad group
          DREC  = 0
          DPOS  = 0
          IMATCH= -1
        ELSE
C         split block control word
          CALL WMSBCS (BCWORD,
     O                 CLASS,LID,ORDER,TLEN)
C         do ids match
          IF (LID.EQ.ID) THEN
C           ids match, return record and position
            IMATCH= 1
          ELSE
C           ids dont match, skip to next BCWORD
            ILEN= TLEN/4
            IF (MOD(TLEN,4).NE.0) THEN
C             one more value to skip
              ILEN= ILEN+ 1
            END IF
            DO 100 I= 1,ILEN
              CALL WDNXPS (WDMSFL,RDWRFG,
     M                     DREC,DPOS,
     O                     DIND)
 100        CONTINUE
C           get the next block control word
            CALL WDNXDV (WDMSFL,
     M                   DREC,DPOS,
     O                   BCWORD)
          END IF
        END IF
      IF (IMATCH.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSSKB
     I                    (WDMSFL,TLEN,
     M                     DREC,DPOS)
C
C     + + + PURPOSE + + +
C     Position DREC and DPOS at the end of the current data block.
C     DREC and DPOS are assumed to be input as the start of the block.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TLEN,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     TLEN   - total number of characters to skip
C     DREC   - record number on WDM file
C     DPOS   - position on record DREC
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,HLEN,RDWRFG,LIND
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDNXPS
C
C     + + + END SPECIFICATIONS + + +
C
      RDWRFG= 1
      HLEN  = TLEN/4
      IF (MOD(TLEN,4).GT.0) HLEN= HLEN+ 1
      DO 100 I= 1,HLEN
        CALL WDNXPS (WDMSFL,RDWRFG,
     M               DREC,DPOS,
     O               LIND)
 100  CONTINUE
C
      RETURN
      END
