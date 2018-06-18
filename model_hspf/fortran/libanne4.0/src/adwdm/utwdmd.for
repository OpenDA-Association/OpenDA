C     utwdmd.f 2.1 9/4/91
C
C
C
      INTEGER   FUNCTION   WDRCGO
     I                           (WDMSFL,RREC)
C
C     + + + PURPOSE + + +
C     Determine the index of the user requested record within
C     the WDM in memory buffer of records.  The record is read
C     from the WDM file and pointers are updated, as required.
C     If the record is already in the buffer, the index is returned.
C     If the record is not in the buffer, the oldest record in
C     the buffer is replaced with the record read from the WDM file.
C     For a negative RREC, the index of an empty record is returned.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     RREC   - record number to find and place in buffer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + SAVES + + +
      INTEGER   KOUNT
      SAVE      KOUNT
C
C     + + + SAVE DEFINITIONS + + +
C     KOUNT  - counts the total number of times the record pointer
C              is invalid (<1 or > conrec).  This error is generally
C              (but not always) fatal.  To protect against an endless
C              loop with error messages filling up the disk, this
C              routine will print a message to the screen telling the
C              user to look at error.fil and will stop.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IND,RIND,PIND,NIND,XIND,I,CWDM,DONFG
C
C     + + + INTRINSICS + + +
      INTRINSIC IABS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  KOUNT / 0 /
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (  ' ***** ADDWM:UTWDMD:WDRCGO:  wdmcnt =', I10,
     $        /,' *                             cwdm =', I10,
     $        /,' *                            donfg =', I10,
     $        /,' *                     maxrec(cwdm) =', I10,
     $        /,' *                             rrec =', I10,
     $        /,' *                             rind =', I10,
     $        /,' *               is this wdmsfl open?', I10,
     $        /,' *                           wdmopn =', 5I5,
     $       /,(' *                                   ', 5I5 ) )
 2999 FORMAT (/,' *****',
     $        /,' *****',
     $        /,' ***** ERROR:  The count for the following error ',
     $        /,' *****         condition has reached the limit,', I3,
     $        /,' *****         See the error.fil file.',
     $        /,' *****',
     $        /,' *****' )
C
C     + + + END SPECIFICATIONS + + +
C
C     write(*,*) 'ADWDM:UTWDMD:WDRCGO',wdmsfl,rrec
      RIND = 0
      CWDM = 0
      DONFG= 0
C
 10   CONTINUE
        CWDM= CWDM+ 1
        IF (WDMOPN(CWDM).EQ.WDMSFL) THEN
C         we know about this wdm file
          DONFG= 1
        END IF
      IF (CWDM.LT.WDMCNT .AND. DONFG.EQ.0) GO TO 10
C     write(*,*) 'ADWDM:UTWDMD:WDRCGO2',cwdm,donfg,wdmcnt
C     write(*,*) 'ADWDM:UTWDMD:WDRCGO3',maxrec(cwdm),wdmopn(cwdm)
C
      IF (DONFG.EQ.1) THEN
C       working with a valid file
        IF (IABS(RREC).LE.MAXREC(CWDM)) THEN
C         record is within allowable range
          IF (RREC.GE.0) THEN
C           looking for an existing record
            IND = 0
 30         CONTINUE
C             look for desired record in currently avail records
              IND= IND+ 1
              IF (RECNO(IND).EQ.RREC .AND. WDMFUN(IND).EQ.WDMSFL) THEN
C               its already in memory
                RIND= IND
              END IF
            IF (IND.LT.CONREC.AND.RIND.EQ.0) GO TO 30
          END IF
C
          IF (RIND.EQ.0) THEN
C           record not found, allocate space for it
            RIND= FREPOS
            IF (RREC.GT.0) THEN
C             read existing record from file
              READ (WDMSFL,REC=RREC) (WIBUFF(I,RIND),I=1,512)
            ELSE
C             return an empty buffer
              DO 40 I= 1,512
                WIBUFF(I,RIND)= 0
 40           CONTINUE
            END IF
C
            FREPOS      = NXTPOS(FREPOS)
            RECNO(RIND) = IABS(RREC)
            WDMFUN(RIND)= WDMSFL
          ELSE
C           record found, update pointers to use this buffer space last
            IF (RIND.EQ.FREPOS .OR. PREPOS(FREPOS).EQ.RIND) THEN
C             pointers are ok
              IF (RIND.EQ.FREPOS) THEN
C               update frepos
                FREPOS= NXTPOS(RIND)
              END IF
            ELSE
C             forward pointer first
              NIND        = PREPOS(FREPOS)
              PIND        = PREPOS(RIND)
              XIND        = NXTPOS(RIND)
              NXTPOS(PIND)= NXTPOS(RIND)
              NXTPOS(RIND)= FREPOS
              NXTPOS(NIND)= RIND
C             now back pointers
              PREPOS(FREPOS)= RIND
              PREPOS(XIND)  = PREPOS(RIND)
              PREPOS(RIND)  = NIND
            END IF
          END IF
        END IF
      END IF
C
      IF (RIND .LT. 1 .OR. RIND .GT. CONREC) THEN
C       we have got a bad problem
        KOUNT = KOUNT + 1
        WRITE (99,2000) WDMCNT, CWDM, DONFG, MAXREC(CWDM), RREC, RIND,
     $                  (WDMOPN(I),I=1,WDMCNT)
        IF (KOUNT .EQ. 50) THEN
C         give it up
          WRITE (*,2999) KOUNT
          WRITE (*,2000) WDMCNT, CWDM, DONFG, MAXREC(CWDM), RREC, RIND,
     $                   (WDMOPN(I),I=1,WDMCNT)
          STOP
Cstop
        END IF
        RIND = 1
      END IF
C
      WDRCGO= RIND
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFLCK
     I                   (WDMSFL,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Check directory of WDM for major errors.  Checks version number
C     and updates old version of WDM file to current version, when required.
C     Determines the number of records in the WDM file and adds that
C     value to the common block CFBUFF.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RETCOD - return code
C               0 - everything ok
C             -89 - WDM file is invalid
C             -88 - no room for another WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,I,J
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I     = 0
C
      IF (WDMCNT.GT.0) THEN
C       see if file already open
 10     CONTINUE
          I= I+ 1
          IF (WDMOPN(I).EQ.WDMSFL) THEN
C           already open
            I= WDMCNT+ 1
          END IF
        IF (I.LT.WDMCNT) GO TO 10
      END IF
C
      IF (I.EQ.WDMCNT) THEN
C       is there room for another WDM file?
        IF (WDMCNT.LT.MXWDM) THEN
C         another file allowed, save info about wdm file
          WDMCNT= WDMCNT+ 1
          WDMOPN(WDMCNT)= WDMSFL
          MAXREC(WDMCNT)= 1
C         check new wdm file
          RREC= 1
          RIND= WDRCGO(WDMSFL,RREC)
C
C         check first position pointer
          IF (WIBUFF(1,RIND).EQ.-998) THEN
C           first position is ok, save max number of records
            MAXREC(WDMCNT)= WIBUFF(PMXREC,RIND)
          ELSE IF (WIBUFF(1,RIND).EQ.-999) THEN
C           old version, update first record
            DO 20 I= 439,40,-1
              J= I+ 73
              WIBUFF(J,RIND)= WIBUFF(I,RIND)
 20         CONTINUE
            DO 30 I= 40,112
              WIBUFF(I,RIND)= 0
 30         CONTINUE
C           indicate now new version
            WIBUFF(1,RIND)= -998
            CALL WDRCUP (WDMSFL,RIND)
            MAXREC(WDMCNT)= WIBUFF(PMXREC,RIND)
          ELSE
C           first position incorrect, return error
            RETCOD= -89
            WDMOPN(WDMCNT)= 0
            MAXREC(WDMCNT)= 0
            WDMCNT= WDMCNT- 1
          END IF
        ELSE
C         cant have another wdm file open
          RETCOD= -88
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFLCL
     I                   (WDMSFL,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     Remove a WDM file from the open WDM buffer and adjust
C     buffer accordingly.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RETCOD - return code
C                0 - everything ok
C              -87 - can't remove message WDM file from buffer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDMCHK
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      IF (WDMCNT.GT.1) THEN
C       see if file already open
        I= 1
 10     CONTINUE
          I= I+ 1
          IF (WDMOPN(I).EQ.WDMSFL) THEN
C           its open
            IF (I.LT.WDMCNT) THEN
C             move later wdm unit numbers
              DO 20 J= I+1,WDMCNT
                WDMOPN(J-1)= WDMOPN(J)
                MAXREC(J-1)= MAXREC(J)
 20           CONTINUE
            END IF
C           check pointers in wdm file
            CALL WDMCHK(WDMSFL,RETCOD)
C           close the file
            CLOSE (UNIT=WDMSFL)
C           reset the count
            WDMCNT= WDMCNT- 1
C           mark records in buffer as not usable
            DO 30 I= 1,CONREC
              IF (WDMFUN(I).EQ.WDMSFL) THEN
C               unusable
                WDMFUN(I)= 0
              END IF
 30         CONTINUE
          END IF
        IF (I.LT.WDMCNT) GO TO 10
      ELSE
C       cant close message file
        RETCOD= -87
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDRCUP
     I                   (WDMSFL,RIND)
C
C     + + + PURPOSE + + +
C     Write record index number RIND from the buffer of records
C     to the WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RIND
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RIND   - buffer index number of record to write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,I
      INTEGER*4 IOS
C
C     + + + END SPECIFICATIONS + + +
C
      RREC= RECNO(RIND)
      WRITE (WDMSFL,REC=RREC,ERR=10,IOSTAT=IOS) (WIBUFF(I,RIND),I=1,512)
      GO TO 20
 10   CONTINUE
C       big problem writing to wdm file
        WRITE (*,*) 'WDRCUP, ERROR ON WRITE, RREC, IOS:',RREC,IOS
C       STOP
 20   CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCGN
     I                           (WDMSFL,PRPRRC,SCPRRC)
C
C     + + + PURPOSE + + +
C     Get the next free record from the WDM file and add it to
C     the WDM buffer of records.  Update pointers on WDM file for
C     primary and secondary records and initialize new record pointers.
C     Returns index number in WDM buffer of records.  Returns 0 if no
C     record available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,PRPRRC,SCPRRC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     PRPRRC - primary record pointer
C     SCPRRC - secondary record pointer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,NEWREC,PPREC,PNREC,SPREC,SNREC,FNREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC   IABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
      WDRCGN= 0
C     get directory record
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
C     get next free record number
      NEWREC= WIBUFF(PFRREC,RIND)
      IF (NEWREC.NE.0) THEN
C       record is available, use it
        IF (PRPRRC.NE.0.OR.SCPRRC.NE.0) THEN
C         update previous records
          PPREC= IABS(PRPRRC)
          SPREC= SCPRRC
          IF (PRPRRC.GT.0) THEN
C           get primary previous record
            RIND= WDRCGO(WDMSFL,PPREC)
C           save old next record pointer
            PNREC= WIBUFF(2,RIND)
C           update next record pointer
            WIBUFF(2,RIND)= NEWREC
            IF (PPREC.NE.SCPRRC) THEN
C             write record
              CALL WDRCUP (WDMSFL,RIND)
            END IF
          ELSE
C           no primary previous record
            PNREC= 0
          END IF
C
          IF (SPREC.NE.0) THEN
C           get secondary previous record
            RIND= WDRCGO(WDMSFL,SPREC)
C           save old next record pointer
            SNREC= WIBUFF(4,RIND)
C           update next record pointer
            WIBUFF(4,RIND)= NEWREC
C           write record
            CALL WDRCUP(WDMSFL,RIND)
          ELSE
            SNREC= 0
          END IF
C
C         update next records
          IF (PNREC.NE.0) THEN
C           get primary previous next record
            RIND= WDRCGO(WDMSFL,PNREC)
C           update previous rec pointer
            WIBUFF(1,RIND)= NEWREC
            IF (PNREC.NE.SNREC) THEN
C             write record
              CALL WDRCUP(WDMSFL,RIND)
            END IF
          END IF
          IF (SNREC.GT.0) THEN
C           get secondary next record
            RIND= WDRCGO(WDMSFL,SNREC)
C           update previous record pointer
            WIBUFF (3,RIND)= NEWREC
C           write record
            CALL WDRCUP(WDMSFL,RIND)
          END IF
        ELSE
C         no pointers in use (directory record)
          PPREC= 0
          PNREC= 0
          SPREC= 0
          SNREC= 0
        END IF
C
C       initialize nex record
C       get next record pointer
        RIND= WDRCGO(WDMSFL,NEWREC)
        FNREC= WIBUFF(2,RIND)
C       fix pointers on new record
        WIBUFF(1,RIND)= PPREC
        WIBUFF(2,RIND)= PNREC
        WIBUFF(3,RIND)= SPREC
        WIBUFF(4,RIND)= SNREC
C       write new record
        CALL WDRCUP(WDMSFL,RIND)
C       update directory
        RREC= 1
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(PFRREC,RIND)= FNREC
        CALL WDRCUP(WDMSFL,RIND)
C       return index of new record
        WDRCGN= WDRCGO(WDMSFL,NEWREC)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDRCGX
     I                           (WDMSFL,PRPRRC,SCPRRC)
C
C     + + + PURPOSE + + +
C     Get the next free record from the WDM file. If no free records
C     are available, add twenty records to the WDM file.  Add free
C     record to the WDM buffer of records.  Update pointers on WDM file
C     for primary and secondary records and initialize new record pointers.
C     Returns index number in WDM buffer of records.  Returns 0 if no
C     record available.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,PRPRRC,SCPRRC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     PRPRRC - primary record pointer
C     SCPRRC - secondary record pointer
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,NUMADD,FREREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGN, WDRCAD
C
C     + + + END SPECIFICATIONS + + +
C
C     try to get new record
      RIND= WDRCGN(WDMSFL,PRPRRC,SCPRRC)
      IF (RIND.EQ.0) THEN
C       no new records available, add some
        NUMADD= 20
        CALL WDRCAD(WDMSFL,NUMADD,
     O              FREREC)
        RIND  = WDRCGN(WDMSFL,PRPRRC,SCPRRC)
      END IF
C
      WDRCGX= RIND
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDRCAD
     I                   (WDMSFL,NUMADD,
     O                    FREREC)
C
C     + + + PURPOSE + + +
C     Add NUMADD records to the WDM file.  Update directory
C     record on the WDM file (record 1).  Returns record number
C     of the first free record in the WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,NUMADD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     NUMADD - number of records to add
C     FREREC - record number of first free record in the WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,RREC,FREREC,LMXREC,I
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP
C
C     + + + END SPECIFICATIONS + + +
C
C     get directory
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
C
      LMXREC= WIBUFF(PMXREC,RIND)
      FREREC= WIBUFF(PFRREC,RIND)
C
      IF (FREREC.EQ.0) THEN
C       no current free records, ok to add more
C       update free record value
        FREREC= LMXREC+ 1
        RREC  = LMXREC
        LMXREC= LMXREC+ NUMADD
C       update in memory buffer
        I= 0
 20     CONTINUE
          I= I+ 1
          IF (WDMOPN(I).EQ.WDMSFL) THEN
C           this is it
            MAXREC(I)= LMXREC
            I= WDMCNT
          END IF
        IF (I.LT.WDMCNT) GO TO 20
C       loop to add new records
 10     CONTINUE
          RREC= RREC+ 1
          RIND= WDRCGO(WDMSFL,-RREC)
C         fix primary forward pointer
          IF (RREC.LT.LMXREC) WIBUFF(2,RIND)= RREC+ 1
C         write record
          CALL WDRCUP(WDMSFL,RIND)
          RREC= RECNO(RIND)
        IF (RREC.LT.LMXREC.AND.RREC.GT.0) GO TO 10
C
C       update directory
        RREC= 1
        RIND= WDRCGO(WDMSFL,RREC)
        WIBUFF(PMXREC,RIND)= LMXREC
        WIBUFF(PFRREC,RIND)= FREREC
        CALL WDRCUP(WDMSFL,RIND)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDDRRC
     I                           (WDMSFL,DSN,OPT)
C
C     + + + PURPOSE + + +
C     Determine WDM file directory record number for data-set
C     number DSN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,OPT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number
C     OPT    - option flag
C              1 - add a directory record
C              2 - delete a directory record
C              other (0) - neither
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,PREC,DPT,DIND,DREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCGX
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDRCGX
C
C     + + + END SPECIFICATIONS + + +
C
C     write(*,*) 'ADWDM:UTWDMD:WDDRRC',wdmsfl,dsn,opt
      RREC= 1
      RIND= WDRCGO(WDMSFL,RREC)
      DPT = PDIRPT+((DSN-1)/500)
      IF (DPT.GT.512) DPT= 512
      IF (OPT.EQ.2) THEN
C       delete reference to record
        WIBUFF(DPT,RIND)= 0
C       update file def record
        CALL WDRCUP(WDMSFL,RIND)
C       need a dummy value to return
        DREC= 0
      ELSE
        DREC= WIBUFF(DPT,RIND)
        IF (DREC.EQ.0.AND.OPT.EQ.1) THEN
C         add new directory record
          PREC= 0
          DIND= WDRCGX(WDMSFL,PREC,PREC)
          DREC= RECNO(DIND)
          RIND= WDRCGO(WDMSFL,RREC)
          WIBUFF(DPT,RIND)= DREC
          CALL WDRCUP(WDMSFL,RIND)
        END IF
      END IF
C
      WDDRRC= DREC
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSCHK
     I                    (WDMSFL,DSN,DSTYP,
     O                     LREC,GRCNT,RETCOD)
C
C     + + + PURPOSE + + +
C     Check WDM data set existance and type.  Return first
C     record number and number of groups in data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYP,LREC,GRCNT,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number
C     DSTYP  - data-set type
C              1 - time series      6 - rastor
C              2 - table            7 - space-time
C              3 - schematic        8 - attribute
C              4 - project          9 - message
C              5 - vector
C     LREC   - record number of first record in data set (contains label)
C     GRCNT  - number of groups in data set
C     RETCOD - return code
C                0 - data set exists and is correct DSTYP
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GPFLG
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSCHA
C
C     + + + END SPECIFICATIONS + + +
C
C     default is to assume read only
      GPFLG= 1
C
      CALL WDSCHA (WDMSFL,DSN,DSTYP,GPFLG,
     O             LREC,GRCNT,RETCOD)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSCHA
     I                    (WDMSFL,DSN,DSTYP,GPFLG,
     O                     LREC,GRCNT,RETCOD)
C
C     + + + PURPOSE + + +
C     Check WDM data set existance, type and ability to update.
C     Return first record number and number of groups in data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYP,GPFLG,LREC,GRCNT,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number
C     DSTYP  - data-set type
C              1 - time series      6 - rastor
C              2 - table            7 - space-time
C              3 - schematic        8 - attribute
C              4 - project          9 - message
C              5 - vector
C     GPFLG  - read(1)/write(2) flag
C     LREC   - record number of first record in data set (contains label)
C     GRCNT  - number of groups in data set
C     RETCOD - return code
C                0 - data set exists and is correct DSTYP
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C              -85 - trying to write to a read-only data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LIND,PDAT,SAIND,POS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     write(*,*) 'ADWDM:UTWDMD:WDSCHA',wdmsfl,dsn,dstyp,gpflg
C     does dataset exist
      CALL WDDSCK(WDMSFL,DSN,
     O            LREC,RETCOD)
C     write(*,*) 'ADWDM:UTWDMD:WDSCHA',lrec,retcod
      IF (RETCOD.EQ.0) THEN
C       data set exists, get label
        LIND = WDRCGO(WDMSFL,LREC)
C       check data-set type
        IF (DSTYP.NE.WIBUFF(6,LIND)) THEN
C         not expected type of data-set
          RETCOD= -82
        ELSE
C         calculate number of groups in data set
          PDAT = WIBUFF(11,LIND)
          GRCNT= WIBUFF(PDAT,LIND)
        END IF
      END IF
      IF (RETCOD.EQ.0 .AND. GPFLG.EQ.2) THEN
C       check read/write flag
        SAIND= 35
        POS  = WDSASV(SAIND,WIBUFF(1,LIND))
        IF (POS.GT.0) THEN
C         read/write flag available
          IF (WIBUFF(POS,LIND).EQ.1) THEN
C           trying to write to a read only data set
            RETCOD= -85
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDCKDT
     I                           (WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     Check data set for existance and type, returns:
C         0 - data set does not exist
C     or data-set type
C         1 - time series      6 - rastor
C         2 - table            7 - space-time
C         3 - schematic        8 - attribute
C         4 - project          9 - message
C         5 - vector
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number to be checked
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,DSNFRC,DSTYPE,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDDSCK(WDMSFL,DSN,
     O            DSNFRC,RETCOD)
      IF (DSNFRC.GT.0) THEN
C       data set exists
        RIND  = WDRCGO(WDMSFL,DSNFRC)
        DSTYPE= WIBUFF(6,RIND)
      ELSE
C       data set does not exist
        DSTYPE= 0
      END IF
C
      WDCKDT= DSTYPE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDSCK
     I                   (WDMSFL,DSN,
     O                    DREC,RETCOD)
C
C     + + + PURPOSE + + +
C     Check data set for existance and return record number of
C     first record in data set (contains label)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DREC,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - data-set number to be checked
C     DREC   - record number of first record in data set
C     RETCOD - return code
C                0 - data set exists
C              -81 - data set does not exist
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RIND,I,OPT,DIRREC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, WDDRRC
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDRRC, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
C     write(*,*) 'ADWDM:UTWDMD:WDDSCK',wdmsfl,dsn
      RETCOD= 0
      IF (DSN.LT.1 .OR. DSN.GT.32000) THEN
C       dataset number out of range
        RETCOD= -84
      END IF
      IF (RETCOD.EQ.0) THEN
C       get directory record number
        OPT   = 0
        DIRREC= WDDRRC(WDMSFL,DSN,OPT)
C       write(*,*) 'ADWDM:UTWDMD:WDDSCK:dirrec',dirrec
C
        IF (DIRREC.GT.0) THEN
C         directory exists, get it
          RIND= WDRCGO(WDMSFL,DIRREC)
C         calculate offset within record for our dsn
          I= MOD(DSN,500)+ 4
          IF (I.EQ.4) I= 504
C         get record dsn begins on
          DREC= WIBUFF(I,RIND)
        ELSE
C         no directory, dsn cant exist
          DREC= 0
        END IF
C       write(*,*) 'ADWDM:UTWDMD:WDDSCK:drec',drec
C
        IF (DREC.EQ.0) THEN
C         data set does not exist
          RETCOD= -81
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBFIN
C
C     + + + PURPOSE + + +
C     Initialize pointers and counters for WDM buffer of records.
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
C     initialize  common CFBUFF variables as required
      DO 10 I= 1, CONREC
        NXTPOS(I)= I+ 1
        PREPOS(I)= I- 1
        RECNO(I) = 0
        WDMFUN(I)= 0
 10   CONTINUE
C
      NXTPOS(CONREC)= 1
      PREPOS(1)= CONREC
      FREPOS= 1
      WDMCNT= 0
      DO 20 I= 1,MXWDM
        WDMOPN(I)= 0
        MAXREC(I)= 0
 20   CONTINUE
C
C     initialize  CDRLOC
      PFNAME= 9
      PMXREC= 29
      PFRREC= 31
      PTSNUM= 32
      PDIRPT= 113
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDCREA
     I                    (WDMSFL)
C
C     + + + PURPOSE + + +
C     Adds directory record and 19 empty records to a new WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,FREREC,I,NUMADD,RIND,LMXREC,RETCOD
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDRCUP, WDRCAD
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I= 0
 10   CONTINUE
        I= I+ 1
        IF (WDMOPN(I).EQ.WDMSFL) THEN
C         its already open, can't create it
          RETCOD= -83
          I     = WDMCNT
        END IF
      IF (I.LT.WDMCNT) GO TO 10
C
      IF (RETCOD.EQ.0) THEN
C       ok to create a new file
        WDMCNT= WDMCNT+ 1
        WDMOPN(WDMCNT)= WDMSFL
        MAXREC(WDMCNT)= 1
C
        RREC= -1
        RIND= WDRCGO(WDMSFL,RREC)
C
C       fill in values, previous record pointer
        WIBUFF(1,RIND)= -998
C       last record
        LMXREC= 1
        WIBUFF(PMXREC,RIND)= LMXREC
C       write the first record
        CALL WDRCUP(WDMSFL,RIND)
C       now fill in the other records with all zero except for pointers
        NUMADD= 19
        CALL WDRCAD (WDMSFL,NUMADD,
     O               FREREC)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDSASV (SAIND,TIBUFF)
C
C     + + + PURPOSE + + +
C     determines where values for particular search attribute
C     start within data-set label, returns 0 if attribute is
C     not present
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SAIND
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SAIND  - index of particular search attribute
C     TIBUFF - buffer of search attributes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,PSA,POS,SAMAX
C
C     + + + END SPECIFICATIONS + + +
C
      PSA  = TIBUFF(10)
      SAMAX= TIBUFF(PSA)
      POS  = 0
      I    = 0
C     loop to look for desired attribute index number in label
 10   CONTINUE
        I= I+ 1
        J= PSA+ I* 2
        IF (TIBUFF(J).EQ.SAIND) THEN
C         attribute is present
          POS= TIBUFF(J+1)
        END IF
      IF (I.LT.SAMAX.AND.POS.EQ.0) GO TO 10
C
      WDSASV= POS
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION   WDGIVL
     I                           (WDMSFL,REC,POS)
C
C     + + + PURPOSE + + +
C     Returns the integer value stored in the WDM
C     file at the specified position.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,REC,POS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     REC    - record number
C     POS    - position within record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IND,IVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      IND= WDRCGO(WDMSFL,REC)
      IF (IND .GT. 0) THEN
C       valid record
        IVAL= WIBUFF(POS,IND)
      ELSE
C       problem record
        IVAL= -999
      END IF
C
      WDGIVL= IVAL
C
      RETURN
      END
C
C
C
      REAL   FUNCTION   WDGRVL
     I                        (WDMSFL,REC,POS)
C
C     + + + PURPOSE + + +
C     Returns the real value stored in the WDM
C     file at the specified position.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,REC,POS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     REC    - record number
C     POS    - position within record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IND
      REAL      RVAL
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      IND= WDRCGO(WDMSFL,REC)
      IF (IND .GT. 0) THEN
C       valid record
        RVAL= WRBUFF(POS,IND)
      ELSE
C       problem record
        RVAL= -999.0
      END IF
C
      WDGRVL= RVAL
C
      RETURN
      END
C
C
C
      CHARACTER*4   FUNCTION   WDGCVL
     I                               (WDMSFL,REC,POS)
C
C     + + + PURPOSE + + +
C     Returns the character value stored in the WDM
C     file at the specified position.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,REC,POS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     REC    - record number
C     POS    - position within record
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     IND,IVAL
      CHARACTER*4 CVAL
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      IND= WDRCGO(WDMSFL,REC)
      IF (IND .GT. 0) THEN
C       valid record
        IVAL= WIBUFF(POS,IND)
        WRITE (CVAL,2000) IVAL
      ELSE
C       problem record
        CVAL= ' '
      END IF
C
      WDGCVL= CVAL
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDGDRT
     O                   (LPTSNM)
C
C     + + + PURPOSE + + +
C     returns pointer to position where count of timeseries datasets
C     is stored in WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LPTSNM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LPTSNM - value of pointer
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      LPTSNM= PTSNUM
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDSNX
     I                   (WDMSFL,
     M                    DSN)
C
C     + + + PURPOSE + + +
C     Calls WDDSNP to search for the next data set greater than
C     or equal to DSN.
C 
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - input:  first data set to be checked
C                      valid range is 1 to 32000, inclusive
C              output: -1 if the input DSN is outside the valid range
C                  or, the input DSN if it exists
C                  or, the next existing data set after DSN
C                  or, -1 if no data sets >= the input DSN exist
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   INCR
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSNP
C
C     + + + END SPECIFICATIONS + + +
C
      INCR = 1
      CALL WDDSNP (WDMSFL, INCR,
     M             DSN)
C
      RETURN
      END
C
C
C
      SUBROUTINE   AWVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     adwdm library.
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
C
C
C
      SUBROUTINE   WDDSNP
     I                   (WDMSFL, INCR,
     M                    DSN)
C
C     + + + PURPOSE + + +
C     Checks the wdm file directory records for existing data sets.
C     Beginning with data set DSN, and incrementing or decrementing by 
C     INCR, the directory records are checked for an existing data set. 
C     The first existing data set is returned in DSN.  If no data set 
C     greater than or equal to (or less than or equal to) the input DSN 
C     is found, a value of -1 is returned for DSN.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,INCR,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     INCR   - value by which DSN is incremented/decremented in looking
C              for next/previous data set
C     DSN    - input:  first data set to be checked
C                      valid range is 1 to 32000, inclusive
C              output: -1 if the input DSN is outside the valid range
C                  or, the input DSN if it exists
C                  or, the next existing data set after/before DSN
C                  or, -1 if no data sets >= (or <=) the input DSN exist
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DIFF,NUMINC,DIRDSN,DIRREC,DONFG,OPT,DIND,DOFF,RINDX,
     $          RINDX2
C
C     + + + FUNCTIONS + + +
      INTEGER   WDDRRC,WDRCGO
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDRRC,WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
      DONFG= 0
 10   CONTINUE
        IF (DSN.LT.1 .OR. DSN.GT.32000) THEN
C         not in range
          DONFG = 1
          DSN   = -1
        ELSE
C         might be ok, get directory record
          OPT   = 0
          DIRREC= WDDRRC(WDMSFL,DSN,OPT)
          IF (DIRREC .GT. 0) THEN
C           directory exists
            DIND= WDRCGO(WDMSFL,DIRREC)
            RINDX = (DSN-1)/500 + 1
 20         CONTINUE
C             calculate offset within record for dsn
              DOFF= MOD(DSN,500)+ 4
              IF (DOFF.EQ.4) THEN
C               last position on the directory record
                DOFF= 504
              END IF
              IF (WIBUFF(DOFF,DIND) .GT. 0) THEN
C               it's a dataset
                DONFG = 1
              ELSE
C               no data set here, try another
                DSN= DSN+ INCR
                IF (DSN.GT.0 .AND. DSN.LT.32001) THEN
                  RINDX2 = (DSN-1)/500 + 1
                ELSE
                  RINDX2 = 0
                END IF
              END IF
C             look more on this directory record?
            IF (DONFG.EQ.0 .AND. RINDX.EQ.RINDX2) GO TO 20
          ELSE
C           no directory
            IF (INCR .GT. 0) THEN
C             skip to next possible directory
              DIRDSN= (((DSN-1)/500)+ 1)* 500+ 1
            ELSE IF (INCR .LT. 0) THEN
C             skip to previous directory, if any
              DIRDSN= ((DSN-1)/500)* 500
            END IF
C           find next dsn w/in that directory based on INCR
            DIFF = DIRDSN - DSN
            NUMINC = DIFF/INCR
            IF (MOD(DIFF,INCR) .NE. 0) NUMINC = NUMINC + 1
            DSN = DSN + INCR*NUMINC
          END IF
        END IF
      IF (DONFG .EQ. 0) GO TO 10
C
      RETURN
      END
