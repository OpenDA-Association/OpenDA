C
C
C
      SUBROUTINE   WDLANG
     I                   (WDMSFL,DSN,GRTYP,ATT1,ATT2,
     O                    LREC,DREC,DPOS,GRCNT,
     O                    PDAT,PDATVL,RETCOD)
C
C     + + + PURPOSE + + +
C     ** not normally called by application programmer, see WDLPUT **
C     add new DLG group to WDM file,
C     check existence of DSN and initialize related parms
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,GRTYP,ATT1,ATT2,LREC,DREC,DPOS,GRCNT,
     1          PDAT,PDATVL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     GRTYP  - type of DLG group (1- LINE, 2- AREA, 3- NODE)
C     ATT1   - user defined major attribute of group being added
C     ATT2   - user defined minor attribute of group being added
C     LREC   - record number of label on WDM file
C     DREC   - record number of new data group on WDM file
C     DPOS   - position on DREC of start of new data group
C     GRCNT  - number of groups on data set
C     PDAT   - pointer to group pointers
C     PDATVL - pointer to beginning for new data (DREC,DPOS)
C     RETCOD - return code
C                0 - DLG group added successfully
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -51 - no space for another DLG on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,PDATV,MXNGR,DIND,LIND,LPOS,IDUM,BLCNT,
     1           SAIND,DLPREC,GRWORD,DSTYP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO, WDRCGX, WDPTCL, WDLICV, WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCGX, WDPTCL, WDLICV, WDSASV, WDRCUP
      EXTERNAL   WDSCHK, WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
      IDUM  = 0
      RETCOD= 0
C     check existance
      DSTYP = 5
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             LREC,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       space for another DLG?
        LIND = WDRCGO(WDMSFL,LREC)
        PDAT = WIBUFF(11,LIND)
        PDATV= WIBUFF(12,LIND)
        MXNGR= (PDATV- PDAT- 1)/ 2
        IF (GRCNT.GT.MXNGR) THEN
C         no space for another DLG
          RETCOD= -51
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       ok to add the group to this data set
        PDATVL= WIBUFF(PDAT+1,LIND)
C       increment DLG group count
        GRCNT = GRCNT+ 1
C       default pointer new record flag to no
        DLPREC= 0
C       get pointer record flag from label if available
        SAIND = 31
        I     = WDSASV(SAIND,WIBUFF(1,LIND))
        IF (I.GT.0) THEN
C         use pointer record flag from data set label
          DLPREC= WIBUFF(I,LIND)
        END IF
C       get start of data (record and position)
        CALL WDPTSP (PDATVL,
     O               DREC,DPOS)
        DIND  = WDRCGO(WDMSFL,DREC)
        IF (DLPREC.EQ.1 .OR. DPOS.GT.512 .OR. DPOS.EQ.0) THEN
C         start the DLG on the next record
          IF (DPOS.EQ.0) THEN
C           actually on prev record
            DREC= DREC- 1
          END IF
C         write(*,*) 'add record for new group after:',DREC,DPOS
          DIND= WDRCGX(WDMSFL,IDUM,DREC)
          DREC= RECNO(DIND)
C         write(*,*) '    record added is           :',DREC
          DPOS= 5
          PDATVL= WDPTCL(DREC,DPOS)
        END IF
C
C       write(*,*) 'add group at:',DREC,DPOS
C       calculate general group info word
        BLCNT = 1
        GRWORD= WDLICV(GRTYP,ATT1,ATT2,BLCNT)
        LPOS  = PDAT+ 2+ 2*(GRCNT-1)
        WIBUFF(LPOS,LIND)  = GRWORD
        WIBUFF(LPOS+1,LIND)= PDATVL
        CALL WDRCUP(WDMSFL,LIND)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLPUT
     I                   (WDMSFL,DSN,ITYPE,ATT1,ATT2,ID,LEN,DLGBUF,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     store DLG header or coordinate pairs on WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,ITYPE,ATT1,ATT2,ID,LEN,RETCOD
      REAL        DLGBUF(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     ITYPE  - type of DLG info (1- LINE, 2- AREA, 3- NODE)
C     ATT1   - major attribute value
C     ATT2   - minor attribute value
C     ID     - id of information being stored (1-header, 2-data)
C     LEN    - length of information being stored (4 byte words)
C     DLGBUF - buffer of information being stored
C                header: 1- internal id
C                        2- X coordinate of centroid
C                        3- Y coordinate of centroid
C                        4:LEN - text description of group
C                data    1,3,5,...,LEN-1 - X coordinates of data
C                        2,4,6,...,LEN   - Y coordinates of data
C     RETCOD - return code
C                0 - DLG data added successfully
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -51 - no space for another DLG on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER    LREC,PDAT,GRCNT,LGRP,PDATVL,LTYPE,LATT1,LATT2,LDSN,
     1           DREC,DPOS
      SAVE       LREC,PDAT,GRCNT,LGRP,PDATVL,LTYPE,LATT1,LATT2,LDSN,
     1           DREC,DPOS
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I0,RDWRFG,TSPREC,SAIND,QWORD,NEWPOS,
     1           LPOS,LIND,DIND,BREC,BPOS,BIND,TREC,TPOS,TIND,DSTYP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO, WDPTCL, WDLBCV, WDSASV, WDRCDL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDPTCL, WDLBCV, WDSASV, WDRCDL
      EXTERNAL   WDNXPS, WDLANG, WDLLCK, WDSCHK, WDPTSP, WDMODT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,LTYPE,LATT1,LATT2,LDSN,RDWRFG/0,0,0,0,0,2/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      IF (LTYPE.NE.ITYPE .OR. LATT1.NE.ATT1 .OR.
     1    LATT2.NE.ATT2 .OR. LDSN.NE.DSN) THEN
C       different data than last time, see if group exists for this type
        CALL WDLLCK (WDMSFL,DSN,ITYPE,ATT1,ATT2,
     O               LGRP)
        IF (LGRP.EQ.0) THEN
C         group does not exist, add it
C         set up and get parms for new group
          CALL WDLANG (WDMSFL,DSN,ITYPE,ATT1,ATT2,
     O                 LREC,DREC,DPOS,GRCNT,
     O                 PDAT,PDATVL,RETCOD)
C         save space for pointer to free position within group
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 DREC,DPOS,
     O                 DIND)
          LGRP= GRCNT
        ELSE
C         group exists, determine label record and group count
          DSTYP= 5
          CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O                 LREC,GRCNT,RETCOD)
          LIND= WDRCGO(WDMSFL,LREC)
          LPOS= PDAT+ 2*(LGRP-1)+ 3
C         is pointer new record on? (default no)
          TSPREC= 0
          SAIND = 31
          I= WDSASV(SAIND,WIBUFF(1,LIND))
          IF (I.GT.0) THEN
C           use pointer record flag from data set label
            TSPREC= WIBUFF(I,LIND)
          END IF
          IF (TSPREC.EQ.1 .OR. LGRP.EQ.GRCNT) THEN
C           new record for each group or on last group, ok to add data
C           determine start of data for this group
            PDATVL= WIBUFF(LPOS,LIND)
            CALL WDPTSP (PDATVL,
     O                   BREC,BPOS)
C           determine next free position from first data value
            BIND= WDRCGO (WDMSFL,BREC)
            CALL WDPTSP (WIBUFF(BPOS,BIND),
     O                   DREC,DPOS)
C           write(*,*) 'add to group brec,bpos:',brec,bpos
C           write(*,*) '             drec,dpos:',drec,dpos
          END IF
        END IF
C       update current dsn, type, and attributes
        LDSN = DSN
        LTYPE= ITYPE
        LATT1= ATT1
        LATT2= ATT2
      ELSE IF (DPOS.EQ.512) THEN
C       need to delete empty next record
        DIND= WDRCGO(WDMSFL,DREC)
        TREC= WIBUFF(4,DIND)
        IF (TREC.GT.0) THEN
C         its there to delete
          I= WDRCDL(WDMSFL,TREC)
        END IF
      END IF
C
C     write(*,*) 'adding type:',ID,' at:',DREC,DPOS
      IF (RETCOD.EQ.0) THEN
C       store this portion of group block control word
        DIND= WDRCGO(WDMSFL,DREC)
        IF (ID.EQ.1) THEN
C         header-store id, type, and length
          QWORD= WDLBCV (ID,ITYPE,I0,LEN)
          WIBUFF(DPOS,DIND)= QWORD
        ELSE IF (ID.EQ.2) THEN
C         data-store id and length
          QWORD= WDLBCV (ID,I0,I0,LEN)
          WIBUFF(DPOS,DIND)= QWORD
        END IF
C       store data or header values
        DO 10 I= 1,LEN
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 DREC,DPOS,
     O                 DIND)
C         if (dpos.eq.5) then
C           write(*,*) 'new record:',DREC
C         end if
          WRBUFF(DPOS,DIND)= DLGBUF(I)
 10     CONTINUE
C       new free data position
        CALL WDNXPS (WDMSFL,RDWRFG,
     M               DREC,DPOS,
     O               DIND)
        WIBUFF(DPOS,DIND)= 0
        NEWPOS= WDPTCL(DREC,DPOS)
        CALL WDPTSP (PDATVL,
     O               BREC,BPOS)
        IF (BREC.NE.DREC) THEN
C         different records
          CALL WDRCUP(WDMSFL,DIND)
        END IF
        BIND= WDRCGO (WDMSFL,BREC)
        WIBUFF(BPOS,BIND)= NEWPOS
        CALL WDRCUP(WDMSFL,BIND)
        IF (LGRP.EQ.GRCNT) THEN
C         free data pos for data set has changed, store new label info
C         next free pos (for a group) is one position further
          TREC= DREC
          TPOS= DPOS
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 TREC,TPOS,
     O                 TIND)
C         write (*,*) 'end of add:',DREC,DPOS,TREC,TPOS
          NEWPOS= WDPTCL(TREC,TPOS)
          LIND  = WDRCGO(WDMSFL,LREC)
          WIBUFF(PDAT,LIND)  = GRCNT
          WIBUFF(PDAT+1,LIND)= NEWPOS
          CALL WDRCUP(WDMSFL,LIND)
        END IF
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLGET
     I                   (WDMSFL,DSN,ITYPE,ATT1,ATT2,LEN,
     M                    ID,
     O                    OLEN,DLGBUF,RETCOD)
C
C     + + + PURPOSE + + +
C     retrieve DLG header or coordinate pairs from WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,ITYPE,ATT1,ATT2,LEN,ID,OLEN,RETCOD
      REAL      DLGBUF(LEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     ITYPE  - type of DLG info (1- LINE, 2- AREA, 3- NODE)
C     ATT1   - major attribute value
C     ATT2   - minor attribute value
C     LEN    - maximum length of information being retrieved (4 byte words)
C     ID     - id of information being retrieved (0-either,1-header, 2-data)
C     OLEN   - actual length of output buffer
C     DLGBUF - buffer of information being retrieved
C     RETCOD - return code
C                2 - no more data in this group
C                1 - more of current id remaining
C                0 - DLG data retrieved successfully
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -50 - major and minor attributes not found on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER    LDSN,LGRP,LREC,DREC,DPOS,IREM,LTYPE,LATT1,LATT2
      SAVE       LDSN,LGRP,LREC,DREC,DPOS,IREM,LTYPE,LATT1,LATT2
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IDUM,IVAL,ILEN,LIND,LPOS,
     1           GRCNT,PDAT,RDWRFG,QWORD,DIND,LID,DONFG,DSTYP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDLBSP, WDRCGO, WDSCHK, WDNXPS, WDLLCK, WDPTSP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA LDSN,IREM,LTYPE,LATT1,LATT2,RDWRFG/0,0,0,0,0,1/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      OLEN  = 0
C
      IF (LTYPE.NE.ITYPE .OR. LATT1.NE.ATT1 .OR.
     1    LATT2.NE.ATT2 .OR. LDSN.NE.DSN) THEN
C       looking to retrieve different info, search for new group
        CALL WDLLCK (WDMSFL,DSN,ITYPE,ATT1,ATT2,
     O               LGRP)
        IF (LGRP.GT.0) THEN
C         new group found, get label record
          DSTYP= 5
          CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O                 LREC,GRCNT,RETCOD)
C         pointer to start of data
          LIND= WDRCGO(WDMSFL,LREC)
          PDAT= WIBUFF(11,LIND)
          LPOS= PDAT+ 2*(LGRP-1)+ 3
C         determine start of data for this group
          CALL WDPTSP (WIBUFF(LPOS,LIND),
     O                 DREC,DPOS)
C         reset remaining data to zero for new info
          IREM = 0
C         update current dsn, data type, and attributes
          LTYPE= ITYPE
          LATT1= ATT1
          LATT2= ATT2
          LDSN = DSN
        ELSE
C         major and minor attributes not found on this data set
          RETCOD= -50
        END IF
      END IF
C
      IF (RETCOD.EQ.0 .AND. LGRP.GT.0) THEN
C       all ok, retrieve requested data
        IF (IREM.EQ.0) THEN
C         no data waiting to be retrieved, read next word
 10       CONTINUE
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
            QWORD= WIBUFF(DPOS,DIND)
            IF (QWORD.GT.0) THEN
C             have a value to split
              CALL WDLBSP (QWORD,
     O                     LID,IDUM,IVAL,ILEN)
              IF (LID.EQ.ID .OR. ID.EQ.0) THEN
C               looking for this ID
                DONFG= 1
C               set ID so calling routine knows type of info returned
                ID= LID
C               set length of info to retrieve
                IREM= ILEN
              ELSE
C               not this ID, check next one
                DONFG= 0
                IF (ILEN.GT.0) THEN
C                 skip to end of this data
                  DO 20 I= 1,ILEN
                    CALL WDNXPS (WDMSFL,RDWRFG,
     M                           DREC,DPOS,
     O                           DIND)
 20               CONTINUE
                END IF
              END IF
            ELSE
C             no value, end of this group
              RETCOD= 2
              DONFG = 1
C             reset current values
              LTYPE= 0
              LATT1= 0
              LATT2= 0
              LDSN = 0
            END IF
          IF (DONFG.EQ.0) GO TO 10
        END IF
C
        IF (IREM.GT.0) THEN
C         retrieve up to LEN values of IREM
          IF (IREM.GT.LEN) THEN
C           more values to retrieve than we can handle in buffer
            OLEN  = LEN
            IREM  = IREM- LEN
            RETCOD= 1
          ELSE
C           just retrieve IREM values
            OLEN= IREM
            IREM= 0
          END IF
          IF (OLEN.GT.0) THEN
C           retrieve OLEN values
            DO 30 I= 1,OLEN
C             get the data
              CALL WDNXPS (WDMSFL,RDWRFG,
     M                     DREC,DPOS,
     O                     DIND)
              DLGBUF(I)= WRBUFF(DPOS,DIND)
 30         CONTINUE
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLLCK
     I                   (WDMSFL,DSN,TYPE,ATT1,ATT2,
     O                    LGRP)
C
C     + + + PURPOSE + + +
C     ** not normally called by application programmer, see WDLPUT/GET **
C     search for group with desired data type and attributes
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,TYPE,ATT1,ATT2,LGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data set on WDM file
C     TYPE   - type of DLG information (1- line,2- area,3- node)
C     ATT1   - major attribute
C     ATT2   - minor attribute
C     LGRP   - group number of the desired type and attributes,
C              returns 0 if no group found
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    QWORD,GRCNT,IDUM,LREC,LPOS,LIND,RETCOD,
     1           LTYPE,LATT1,LATT2,LCHK,PDAT,DSTYP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDLISP, WDSCHK
C
C     + + + END SPECIFICATIONS + + +
C
      LCHK= 0
C     check data set existance, get label record and group count
      DSTYP= 5
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             LREC,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       dsn ok
        LIND= WDRCGO(WDMSFL,LREC)
        PDAT= WIBUFF(11,LIND)
        LGRP= 0
 10     CONTINUE
C         search groups till match found or out of groups
          LGRP = LGRP+ 1
          LPOS = PDAT+ 2+ 2*(LGRP-1)
          QWORD= WIBUFF(LPOS,LIND)
C         split label info word to check for type and attribute match
          CALL WDLISP (QWORD,
     O                 LTYPE,LATT1,LATT2,IDUM)
C         WRITE(99,*) 'lt:',LTYPE,LATT1,LATT2,IDUM,TYPE,ATT1,ATT2,LGRP
          IF (LTYPE.EQ.TYPE .AND. LATT1.EQ.ATT1 .AND.
     1                             LATT2.EQ.ATT2) THEN
C           type and attributes match
            LCHK= 1
          END IF
        IF (LCHK.EQ.0 .AND. LGRP.LT.GRCNT) GO TO 10
      END IF
      IF (LCHK.EQ.0) THEN
C       we didn't find a match
        LGRP= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLLSU
     I                   (WDMSFL,DSN,ILEN,
     O                    OLEN,TYPE,ATT1,ATT2,RETCOD)
C
C     + + + PURPOSE + + +
C     summarize label information for DLG data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,ILEN,OLEN,RETCOD
      INTEGER   TYPE(ILEN),ATT1(ILEN),ATT2(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     ILEN   - maximum size of information buffers
C     OLEN   - actual amount of information returned (<= ILEN)
C     TYPE   - buffer of information types on DSN
C     ATT1   - buffer of major attributes on DSN
C     ATT2   - buffer of minor attributes on DSN
C     RETCOD - return code
C                1 - more groups on DSN
C                0 - label summary returned successfully
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER    IREM,LREC,LDSN,GRCNT
      SAVE       IREM,LREC,LDSN,GRCNT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,LPOS,LIND,PDAT,IDUM,DLWORD,IPOS,DSTYP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDSCHK, WDLISP
C
C     + + + DATA INITIALIZATIONS + + +
      DATA LDSN/0/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      IF (IREM.EQ.0 .OR. DSN.NE.LDSN) THEN
C       new data set, check it
        DSTYP= 5
        CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O               LREC,GRCNT,RETCOD)
C
        IF (RETCOD.EQ.0) THEN
C         data set all right, continue with summary
          IF (GRCNT.GT.ILEN) THEN
C           more than we can summarize right now
            RETCOD= 1
            OLEN  = ILEN
            IREM  = GRCNT- ILEN
          ELSE
C           get GRCNT groups
            OLEN= GRCNT
          END IF
C         start at first group on label
          IPOS= 0
        END IF
C       reset latest data-set number
        LDSN= DSN
      ELSE
C       groups remaining to summarize, continue from last time
C       adjust starting position on label for this buffer
        IPOS  = GRCNT- IREM
        IF (IREM.GT.ILEN) THEN
C         more than we can summarize right now
          RETCOD= 1
          OLEN  = ILEN
          IREM  = IREM- ILEN
        ELSE
C         summarize IREM groups
          OLEN= IREM
          IREM= 0
        END IF
      END IF
C
      IF (RETCOD.GE.0) THEN
C       now get summary info off label
        LIND= WDRCGO(WDMSFL,LREC)
        PDAT= WIBUFF(11,LIND)
        DO 10 I= 1,OLEN
C         get OLEN groups of information
          LPOS= PDAT+ 2*(IPOS+I-1)+ 2
          DLWORD= WIBUFF(LPOS,LIND)
C         split block control word into type and attributes
          CALL WDLISP (DLWORD,
     O                 TYPE(I),ATT1(I),ATT2(I),IDUM)
 10     CONTINUE
      END IF
C
      RETURN
      END
