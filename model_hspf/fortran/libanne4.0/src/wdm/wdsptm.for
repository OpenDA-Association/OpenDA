C
C
C
      SUBROUTINE   WSTFGP
     I                   (WDMSFL,DSN,STDAT,RWFLG,
     O                    GREC,GPOS,GRPIND,DREC,DPOS,FRAC,
     1                    DIMX,DIMY,DIMZ,DIMDAT,DTYPE,RETCOD)
C
C     + + + PURPOSE + + +
C     ** not normally used by application programmers **
C     ** arguments subject to change                  **
C     find space time data within a data set, return pointers to it
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),RWFLG,GREC,GPOS,GRPIND,
     1         DREC(2),DPOS(2),DIMX,DIMY,DIMZ,DIMDAT,DTYPE,RETCOD
      REAL     FRAC(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Starting date for data
C     RWFLG  - Read/write flag, 1-read, 2-write
C     GREC   - Starting record for group
C     GPOS   - Starting position for group
C     GRPIND - Group index number
C     DREC   - Starting record for data(1-before,2-after)
C     DPOS   - Starting offset for data(1-before,2-after)
C     FRAC   - Fraction of each data value to use(1-before,2-after)
C     DIMX   - Max dimension in X direction
C     DIMY   - Max dimension in Y direction
C     DIMZ   - Max dimension in Z direction
C     DIMDAT - Size of data value(words)(integer or real-1, double-2)
C     DTYPE  - Type of data,1-int,2-real,3-double
C     RETCOD - Return code
C                0 - data found
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,I1,J,DSTYP,CNOV,PDAT,NREC,NPOS,NIND,TNOV1,TNOV2,
     1           LREC,LPOS,LIND,GIND,EREC,EPOS,TMPDAT(6),TMPDT2(6),
     2           GRPCNT,GRPDAT(6),GRPTUN,GRPTST,GRPNOV,BLKLEN
C
C     + + + INTRINSICS + + +
      INTRINSIC  FLOAT
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO,TIMCHK,WDRCGX,WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDSCHK,WDRCGO,WDRCUP,WDATSP,WSTGSP,TIMCHK,TIMDIF
      EXTERNAL   WSTDIM,WDPTSP,WDRCGX,WDPTCL,WSTFDT,TIMADD
C
C     + + + END SPECIFICATIONS + + +
C
      I1     = 1
      DREC(1)= 0
      DPOS(1)= 0
      DREC(2)= 0
      DPOS(2)= 0
      FRAC(1)= 0.0
      FRAC(2)= 0.0
      GREC   = 0
      GPOS   = 0
      GRPIND = 0
      DIMX   = 0
      DIMY   = 0
      DIMZ   = 0
      DIMDAT = 0
      DTYPE  = 0
C     does data set exist and is it space time?
      DSTYP  = 7
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             LREC,GRPCNT,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists and is space time
        IF (GRPCNT.EQ.0) THEN
C         no data present
          RETCOD= -40
        ELSE
C         get label
          LIND= WDRCGO(WDMSFL,LREC)
C         data-set dimensions and type
          CALL WSTDIM(WIBUFF(1,LIND),
     O                DIMX,DIMY,DIMZ,DIMDAT,DTYPE)
C         look for data we want
          PDAT  = WIBUFF(11,LIND)
          LPOS  = PDAT- 1
          GRPIND= 0
  10      CONTINUE
C           loop to look for group containing specified data
            GRPIND= GRPIND+ 1
            I     = GRPIND
            LPOS  = LPOS+ 3
            CALL WDATSP (WIBUFF(LPOS,LIND),
     O                   GRPDAT)
            CALL WSTGSP (WIBUFF(LPOS+1,LIND),
     O                   GRPDAT(5),GRPDAT(6),GRPTUN,GRPTST,GRPNOV)
            J= TIMCHK(GRPDAT,STDAT)
            IF (J.EQ.0) THEN
C             on a group boundary
              IF (GRPTUN.GT.0) THEN
C               whoops, should have caught value at end of last block
C               data group must be missing
                RETCOD= -39
              ELSE
C               dummy initial boundary block, on it exactly
                FRAC(1)= 1.0
                FRAC(2)= 0.0
C               find start position of data
                CNOV   = 1
                CALL WDPTSP (WIBUFF(LPOS+2,LIND),
     O                       GREC,GPOS)
                CALL WSTFDT (WDMSFL,GREC,GPOS,CNOV,
     O                       EREC,EPOS,DREC(1),DPOS(1))
C               force exit from group loop
                I= GRPCNT
              END IF
            ELSE IF (J.GT.0) THEN
C             may be in correct group, check end
              CALL TIMDIF (GRPDAT,STDAT,GRPTUN,GRPTST,
     O                     CNOV)
              IF (CNOV.EQ.0 .AND. GRPTUN.GT.0) THEN
C               in between last value of prev group and first value of
C               this group
                J= TIMCHK(GRPDAT,TMPDAT)
                IF (J.EQ.0) THEN
C                 no space between groups
                  CNOV= 1
C                 after data position in this group
                  CALL WDPTSP (WIBUFF(LPOS+2,LIND),
     O                         GREC,GPOS)
                  CALL WSTFDT (WDMSFL,GREC,GPOS,CNOV,
     O                         EREC,EPOS,DREC(2),DPOS(2))
C                 determine frac of each block by comparing secords
                  CALL TIMDIF (TMPDAT,STDAT,I1,I1,
     O                         TNOV1)
                  CALL TIMADD (TMPDAT,GRPTUN,GRPTST,I1,
     O                         TMPDT2)
                  CALL TIMDIF (TMPDAT,TMPDT2,I1,I1,
     O                         TNOV2)
                  FRAC(2)= FLOAT(TNOV1)/FLOAT(TNOV2)
                  FRAC(1)= 1.0- FRAC(2)
                ELSE
C                 missing part of time required
                  RETCOD= -38
                END IF
                I= GRPCNT
              ELSE IF (CNOV.LE.GRPNOV) THEN
C               this a the group we want
                IF (CNOV.EQ.0) CNOV= 1
C               find the position of start (or all) of data
                CALL WDPTSP (WIBUFF(LPOS+2,LIND),
     O                       GREC,GPOS)
                CALL WSTFDT (WDMSFL,GREC,GPOS,CNOV,
     O                       EREC,EPOS,DREC(1),DPOS(1))
C               what fraction is needed
                CALL TIMADD (GRPDAT,GRPTUN,GRPTST,CNOV,
     O                       TMPDAT)
                J= TIMCHK(STDAT,TMPDAT)
                IF (J.EQ.0) THEN
C                 right on the boundary
                  FRAC(1)= 1.0
                  FRAC(2)= 0.0
                ELSE IF (GRPTUN.GT.0) THEN
C                 in a real group
                  CNOV= CNOV+ 1
                  IF (CNOV.LE.GRPNOV) THEN
C                   after data position also in this group
                    CALL WSTFDT (WDMSFL,GREC,GPOS,CNOV,
     O                           EREC,EPOS,DREC(2),DPOS(2))
C                   determine frac of each block by comparing secords
                    CALL TIMDIF (TMPDAT,STDAT,I1,I1,
     O                           TNOV1)
                    CALL TIMADD (TMPDAT,GRPTUN,GRPTST,I1,
     O                           TMPDT2)
                    CALL TIMDIF (TMPDAT,TMPDT2,I1,I1,
     O                           TNOV2)
                    FRAC(2)= FLOAT(TNOV1)/FLOAT(TNOV2)
                    FRAC(1)= 1.0- FRAC(2)
                  ELSE
C                   were in first part of next group
                    FRAC(1)= 0.0
                  END IF
                END IF
                IF (FRAC(1).GT.0.0) THEN
C                 we have what we need
                  I= GRPCNT
                END IF
              END IF
            ELSE
C             no data available
              RETCOD= -37
              IF (FRAC(1).GT.0) THEN
                I= GRPCNT
              END IF
            END IF
          IF (I.LT.GRPCNT) GO TO 10
C
          IF (DREC(1).EQ.0 .AND. RETCOD.EQ.0) THEN
C           no data exists, but the group does
            IF (RWFLG.EQ.1) THEN
C             no data to read
              RETCOD= -41
            ELSE
C             where is free space for this group
              GIND  = WDRCGO(WDMSFL,GREC)
              CALL WDPTSP(WIBUFF(GPOS+1,GIND),
     O                    DREC(1),DPOS(1))
C             make space for data starting at free position
              BLKLEN= DIMX* DIMY* DIMZ* DIMDAT
              NREC  = DREC(1)
              NPOS  = DPOS(1)+ BLKLEN
              IF (NPOS.GT.512) THEN
                J= 0
C               need more records for this block
 40             CONTINUE
                  NIND= WDRCGX(WDMSFL,J,NREC)
                  NREC= RECNO(NIND)
                  NPOS= NPOS- 508
                IF (NPOS.GT.512) GO TO 40
              END IF
C             add to block counter
              GIND= WDRCGO(WDMSFL,GREC)
              WIBUFF(GPOS,GIND)  = WIBUFF(GPOS,GIND)+ 1
              WIBUFF(GPOS+1,GIND)= WDPTCL(NREC,NPOS)
              IF (GREC.NE.EREC) THEN
C               save the revised general block info now
                CALL WDRCUP(WDMSFL,GIND)
                GIND= WDRCGO(WDMSFL,EREC)
              END IF
C             fix pointer to new block
              WIBUFF(EPOS,GIND)= WDPTCL(DREC(1),DPOS(1))
              CALL WDRCUP(WDMSFL,GIND)
            END IF
          ELSE IF (FRAC(2).GT.0.0 .AND. DREC(2).EQ.0) THEN
            IF (RWFLG.EQ.1) THEN
C             missing needed following data for a get
              RETCOD= -36
            END IF
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTFDT (WDMSFL,GREC,GPOS,GOFF,
     O                     EREC,EPOS,DREC,DPOS)
C
C     + + + PURPOSE + + +
C     ** not normally used by application programmers **
C     ** arguments subject to change                  **
C     find start position of data from group pointer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,GREC,GPOS,GOFF,EREC,EPOS,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     GREC   - Start of group record
C     GPOS   - Start of group position
C     GOFF   - Offset to block containing data desired
C     EREC   - Pointer to data record
C     EPOS   - Pointer to data position
C     DREC   - Start of data record
C     DPOS   - Start of data position
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    EIND
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO,WDPTSP
C
C     + + + END SPECIFICATIONS + + +
C
C     get record where data block pointers start
      EIND= WDRCGO(WDMSFL,GREC)
      EREC= GREC
      EPOS= GPOS+ GOFF+ 1
      IF (EPOS.GT.512) THEN
C       data pointer on a later record
 10     CONTINUE
          EREC= WIBUFF(4,EIND)
          EIND= WDRCGO(WDMSFL,EREC)
          EPOS= EPOS- 508
        IF (EPOS.GT.512) GO TO 10
      END IF
      IF (WIBUFF(EPOS,EIND).GT.0) THEN
C       calc actual pointer to data values
        CALL WDPTSP (WIBUFF(EPOS,EIND),
     O               DREC,DPOS)
      ELSE
C       no data in block
        DREC= 0
        DPOS= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTAGP
     I                   (WDMSFL,DSN,NGPDAT,NGPTUN,NGPTST,NGPNOV,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     add a group to a space time data set, physically allocate space for it
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,NGPDAT(6),NGPTUN,NGPTST,NGPNOV,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     NGPDAT - Starting date for group
C     NGPTUN - Time units for group
C     NGPTST - Time step for group
C     NGPNOV - Number of timesteps in group
C     RETCOD - Return code
C                0 - group added
C              -42 - overlap an existing group
C              -43 - can't add another space time group
C              -46 - bad space time group specification parameter
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,J,J1,J2,LREC,LIND,LPOS,DREC,DPOS,DIND,EREC,EPOS,EIND,
     1           SDFREE,MXGRP,TSPREC,SAPOS,DSTYP,PDAT,PDATV,
     2           GRPCNT,GRPDAT(6),GRPTUN,GRPTST,GRPNOV,
     3           GRPEDT(6),NGPEDT(6)
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO,TIMCHK,WDSASV,WDATCL,WSTGCL,WDPTCL,WDRCGX
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDSCHK,WDRCGO,WDATSP,WSTGSP,TIMCHK,TIMADD
      EXTERNAL   WDSASV,WDATCL,WSTGCL,WDPTCL,WDRCUP,WDPTSP,WDRCGX
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      IF (NGPTUN.LT.1 .OR. NGPTUN.GT.7  .OR.
     1    NGPTST.LT.1 .OR. NGPTST.GT.63 .OR. NGPNOV.LT.1) THEN
C       may have bad input paramter
        IF (NGPTUN.NE.0 .OR. NGPTST.NE.0 .OR. NGPNOV.NE.1) THEN
C         yup, a bad input parameter
          RETCOD= -46
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       check data-set details
        DSTYP = 7
        CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O               LREC,GRPCNT,RETCOD)
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       data set exists and is space time
        LIND= WDRCGO(WDMSFL,LREC)
        PDAT= WIBUFF(11,LIND)
C       determine end of new group
        CALL TIMADD (NGPDAT,NGPTUN,NGPTST,NGPNOV,
     O               NGPEDT)
        IF (GRPCNT.GT.0) THEN
C         data present, room for another group?
          PDATV= WIBUFF(12,LIND)
          MXGRP= (PDATV-PDAT-2)/3
          IF (GRPCNT+1.GT.MXGRP) THEN
C           cant add another space time group
            RETCOD= -43
          ELSE
C           check for overlap
            LPOS= PDAT- 1
            I   = 0
  10        CONTINUE
C             loop to look for any group containing all or part of
C             new group
              I   = I+ 1
              LPOS= LPOS+ 3
              CALL WDATSP (WIBUFF(LPOS,LIND),
     O                     GRPDAT)
              CALL WSTGSP (WIBUFF(LPOS+1,LIND),
     O                     GRPDAT(5),GRPDAT(6),GRPTUN,GRPTST,GRPNOV)
C             determine end of existing group
              CALL TIMADD (GRPDAT,GRPTUN,GRPTST,GRPNOV,
     O                     GRPEDT)
              J1= TIMCHK(GRPDAT,NGPDAT)
              J2= TIMCHK(GRPEDT,NGPEDT)
              IF (J1.NE.J2 .OR. J1.EQ.0) THEN
                J2= TIMCHK(GRPEDT,NGPDAT)
                IF (J1.EQ.0 .AND. J2.EQ.0) THEN
C                 may add group after a dummy group
                  RETCOD= 0
                  I     = I+ 1
                  LPOS  = LPOS+ 3
                ELSE
C                 overlap an existing group
                  RETCOD= -42
                END IF
              ELSE IF (J1.EQ.1) THEN
C               may be a new group
                J1= TIMCHK(GRPEDT,NGPDAT)
                IF (J1.EQ.-1) THEN
C                 also overlaps an existing group
                  RETCOD= -42
                ELSE IF (I.EQ.GRPCNT) THEN
C                 a new group, set flag to add it
                  RETCOD= 1
                  I     = I+ 1
                  LPOS  = LPOS+ 3
                END IF
              ELSE IF (J1.EQ.-1) THEN
C               may be a new group
                J1= TIMCHK(NGPEDT,GRPDAT)
                IF (J1.EQ.-1) THEN
C                 overlaps an existing group
                  RETCOD= -42
                ELSE
C                 add a group between existing groups
                  RETCOD= 1
                END IF
              END IF
            IF (I.LT.GRPCNT .AND. RETCOD.EQ.0) GO TO 10
          END IF
        ELSE
C         no data present, add first group
          LPOS= PDAT+ 2
          I   = 1
        END IF
        IF (RETCOD.GE.0) THEN
C         reset return code
          RETCOD= 0
C         add to group counter
          GRPCNT= GRPCNT+ 1
          WIBUFF(PDAT,LIND)= GRPCNT
          IF (I.LT.GRPCNT) THEN
C           move existing groups later than new one
            DO 30 J= GRPCNT,I+1,-1
              J1= PDAT-1+(J*3)
              WIBUFF(J1,LIND)  = WIBUFF(J1-3,LIND)
              WIBUFF(J1+1,LIND)= WIBUFF(J1-2,LIND)
              WIBUFF(J1+2,LIND)= WIBUFF(J1-1,LIND)
 30         CONTINUE
          END IF
C         does new group start on a new record
          J    = 31
          SAPOS= WDSASV(J,WIBUFF(1,LIND))
          IF (SAPOS.GT.0) THEN
C           use attribute from label
            TSPREC= WIBUFF(SAPOS,LIND)
            IF (TSPREC.EQ.0) THEN
C             new group must start on new record
              TSPREC= 1
            END IF
          ELSE
C           no attribute, assume on
            TSPREC= 1
          END IF
          SDFREE= WIBUFF(PDAT+1,LIND)
          CALL WDPTSP (SDFREE,
     O                 DREC,DPOS)
          IF (TSPREC.EQ.1 .OR. DPOS.EQ.0 .OR. DPOS.GT.510) THEN
C           start group on a new record
            J   = 0
            DIND= WDRCGX(WDMSFL,J,DREC)
            DREC= RECNO(DIND)
            DPOS= 5
          END IF
C         save info about new group
          WIBUFF(PDAT,LIND)  = GRPCNT
          WIBUFF(LPOS,LIND)  = WDATCL(NGPDAT)
          WIBUFF(LPOS+1,LIND)= WSTGCL(NGPDAT(5),NGPDAT(6),
     1                                NGPTUN,NGPTST,NGPNOV)
          WIBUFF(LPOS+2,LIND)= WDPTCL(DREC,DPOS)
C         update label on wdm file
          CALL WDRCUP(WDMSFL,LIND)
C         add space for new groups pointers
          DIND= WDRCGO(WDMSFL,DREC)
C         calc where data should start
          EPOS= DPOS+1+NGPNOV
          EREC= DREC
          IF (EPOS.GT.512) THEN
            J = 0
C           data pointer on a later record
 40         CONTINUE
              EIND= WDRCGX(WDMSFL,J,EREC)
              EREC= RECNO(EIND)
              EPOS= EPOS- 508
            IF (EPOS.GT.512) GO TO 40
          END IF
C         be sure data starts on an even word
          IF (MOD(EPOS,2).EQ.1) EPOS= EPOS+ 1
C         be sure label still in memory
          LIND= WDRCGO(WDMSFL,LREC)
C         pointer to free data space to store next group
          WIBUFF(PDAT+1,LIND)= WDPTCL(EREC,EPOS)
          CALL WDRCUP(WDMSFL,LIND)
C         be sure first data pointer still in memory
          DIND= WDRCGO(WDMSFL,DREC)
C         pointer to free data space to store next block
          WIBUFF(DPOS+1,DIND)= WDPTCL(EREC,EPOS)
          CALL WDRCUP(WDMSFL,DIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTDGP
     I                   (WDMSFL,DSN,STDATE,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     delete group from a space time data set, free up space it uses
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDATE(6),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDATE - Date in group to delete
C     RETCOD - Return code
C                0 - group deleted
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -42 - overlap an existing group
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    RWFLG,GREC,GPOS,GIND,GRPIND,DREC(2),DPOS(2),PDAT,
     1           DIMX,DIMY,DIMZ,DIMDAT,LTYPE,GRPCNT,I,J,NXTREC,BLKCNT,
     2           SAPOS,TSPREC,TOTLEN,GRPMIN,GRPSEC,GRPTUN,GRPTST,GRPNOV,
     3           LIND,LREC
      REAL       FRAC(2)
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO,WDSASV,WDRCDL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTFGP,WDDSCK,WDRCGO,WDRCUP,WDSASV,WSTGSP,WDRCDL
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 1
C     does group exist?
      CALL WSTFGP (WDMSFL,DSN,STDATE,RWFLG,
     O             GREC,GPOS,GRPIND,DREC,DPOS,FRAC,
     1             DIMX,DIMY,DIMZ,DIMDAT,LTYPE,RETCOD)
      IF (RETCOD.EQ.0 .OR. RETCOD.EQ.-41) THEN
C       group exists
        RETCOD= 0
C       get label
        CALL WDDSCK(WDMSFL,DSN,
     O              LREC,RETCOD)
        LIND  = WDRCGO(WDMSFL,LREC)
C       check where new groups start
        J     = 31
        SAPOS = WDSASV(J,WIBUFF(1,LIND))
        IF (SAPOS.GT.0) THEN
C         use attribute from label
          TSPREC= WIBUFF(SAPOS,LIND)
        ELSE
C         no attribute, assume on
          TSPREC= 1
        END IF
        IF (TSPREC.EQ.1) THEN
C         ok to delete the group
          PDAT  = WIBUFF(11,LIND)
          GRPCNT= WIBUFF(PDAT,LIND)
C         reduce group count
          WIBUFF(PDAT,LIND)= WIBUFF(PDAT,LIND)- 1
C         how many values in group
          J= PDAT+(GRPIND*3)
          CALL WSTGSP (WIBUFF(J,LIND),
     O                 GRPMIN,GRPSEC,GRPTUN,GRPTST,GRPNOV)
          IF (GRPIND.LT.GRPCNT) THEN
C           move group pointers
            DO 10 I= GRPIND+1,GRPCNT
              J= PDAT-1+(I*3)
              WIBUFF(J-3,LIND)= WIBUFF(J,LIND)
              WIBUFF(J-2,LIND)= WIBUFF(J+1,LIND)
              WIBUFF(J-1,LIND)= WIBUFF(J+2,LIND)
 10         CONTINUE
          END IF
          J= PDAT-1+(GRPCNT*3)
          WIBUFF(J,LIND)  = 0
          WIBUFF(J+1,LIND)= 0
          WIBUFF(J+2,LIND)= 0
C         update label
          CALL WDRCUP(WDMSFL,LIND)
C         how many blocks
          GIND  = WDRCGO(WDMSFL,GREC)
          BLKCNT= WIBUFF(GPOS,GIND)
C         delete data records
          TOTLEN= (DIMX*DIMY*DIMZ*DIMDAT*BLKCNT)+GRPNOV+2
 20       CONTINUE
            NXTREC= WDRCDL(WDMSFL,GREC)
            GREC  = NXTREC
            TOTLEN= TOTLEN- 508
          IF (TOTLEN.GT.0.AND.GREC.GT.0) GO TO 20
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTDIM
     I                    (IBUFF,
     O                     DIMX,DIMY,DIMZ,DIMDAT,DTYPE)
C
C     + + + PURPOSE + + +
C     determine dimensions a space time data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IBUFF(512),DIMX,DIMY,DIMZ,DIMDAT,DTYPE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IBUFF  - Label of space time data set
C     DIMX   - X dimension
C     DIMY   - Y dimension
C     DIMZ   - Z dimension
C     DIMDAT - Data size (int-1,real-1,dprec-2)
C     DTYPE  - Type of data(int-1,real-2,dprec-3)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SAIND,SAPOS
      CHARACTER*4 CTMP
C
C     + + + FUNCTIONS + + +
      INTEGER    WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDSASV
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      SAIND= 266
      SAPOS= WDSASV(SAIND,IBUFF)
      IF (SAPOS.GT.0) THEN
C       use attribute from label
        DIMX = IBUFF(SAPOS)
      ELSE
C       default attribute
        DIMX = 1
      END IF
C
      SAIND= 267
      SAPOS= WDSASV(SAIND,IBUFF)
      IF (SAPOS.GT.0) THEN
C       use attribute from label
        DIMY = IBUFF(SAPOS)
      ELSE
C       default attribute
        DIMY = 1
      END IF
C
      SAIND= 268
      SAPOS= WDSASV(SAIND,IBUFF)
      IF (SAPOS.GT.0) THEN
C       use attribute from label
        DIMZ = IBUFF(SAPOS)
      ELSE
C       default attribute
        DIMZ = 1
      END IF
C
      SAIND= 265
      SAPOS= WDSASV(SAIND,IBUFF)
      IF (SAPOS.GT.0) THEN
C       use attribute from label
        WRITE (CTMP,2000) IBUFF(SAPOS)
        IF (CTMP(1:1).EQ.'D') THEN
C         double precision
          DIMDAT= 2
          DTYPE = 3
        ELSE IF (CTMP(1:1).EQ.'R') THEN
C         real
          DIMDAT= 1
          DTYPE = 2
        ELSE
C         integer
          DIMDAT= 1
          DTYPE = 1
        END IF
      ELSE
C       default attribute(as integer)
        DIMDAT= 1
        DTYPE = 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTGSU
     I                   (WDMSFL,DSN,GRPIND,
     O                    GSDAT,GEDAT,GTUN,GTST,GNOV,GFRAC,RETCOD)
C
C     + + + PURPOSE + + +
C     summarize a group in a space time data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,GRPIND,GSDAT(6),GEDAT(6),GTUN,GTST,GNOV,RETCOD
      REAL     GFRAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data set number
C     GRPIND - Index of group to summarize
C     GSDAT  - Start date of group
C     GEDAT  - End date of group
C     GTUN   - Group time units
C     GTST   - Group time steps
C     GNOV   - Number of values in group
C     GFRAC  - Fraction of group containing data
C     RETCOD - Return code
C                0 - group summarized
C              -49 - group doesn't exist
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DSTYP,GRPCNT,PDAT,NBLK,
     1          LIND,LREC,LPOS,DIND,DREC,DPOS
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSCHK,WDRCGO,WDATSP,WSTGSP,TIMADD,WDPTSP
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT
C
C     + + + END SPECIFICATIONS + + +
C
C     does this space time data set exist
      DSTYP= 7
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             LREC,GRPCNT,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists and is space time
        IF (GRPIND.GT.GRPCNT) THEN
C         group doesn't exist
          RETCOD= -49
        ELSE
          LIND= WDRCGO(WDMSFL,LREC)
          PDAT= WIBUFF(11,LIND)
          LPOS= PDAT- 1+ (GRPIND*3)
          CALL WDATSP (WIBUFF(LPOS,LIND),
     O                 GSDAT)
          CALL WSTGSP (WIBUFF(LPOS+1,LIND),
     O                 GSDAT(5),GSDAT(6),GTUN,GTST,GNOV)
C         determine end of existing group
          CALL TIMADD (GSDAT,GTUN,GTST,GNOV,
     O                 GEDAT)
C         determine fraction full of data
          CALL WDPTSP (WIBUFF(LPOS+2,LIND),
     O                 DREC,DPOS)
          DIND = WDRCGO(WDMSFL,DREC)
          NBLK = WIBUFF(DPOS,DIND)
          GFRAC= FLOAT(NBLK)/FLOAT(GNOV)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,
     I                     NDIM,NUMN,BASN,SKPN,
     M                     DPOS,DIND,DBLK,
     O                     FRAC,DONFG,RETCOD)
C
C     + + + PURPOSE + + +
C     determine next wdm space time data offset (from beginning of data)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),RWFLG,DTYPE,NDIM,NUMN(NDIM),
     1         BASN(NDIM),SKPN(NDIM),DPOS(2),DIND(2),DBLK,DONFG,RETCOD
      REAL     FRAC(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     RWFLG  - Read/write flag(1-read,2-write)
C     DTYPE  - Data type to get/put
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     DPOS   - Current data offset within record(1-before,2-after)
C     DIND   - Current index of data record(1-before,2-after)
C     DBLK   - Current offset within block
C     FRAC   - Fraction of each data value to use(1-before,2-after)
C     DONFG  - Done getting data
C     RETCOD - Return code
C                0 - offset found
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   LBLK,CURX,CURY,CURZ,DIMX,DIMY,DIMZ,DIMDAT,BLKLEN,
     1          DREC(2),DNUM
      SAVE      LBLK,CURX,CURY,CURZ,DIMX,DIMY,DIMZ,DIMDAT,BLKLEN,
     1          DREC,DNUM
      INTEGER   J,LTYPE,GREC,GPOS,GIND,DBL1,DBL2
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTFGP,WDRCGO,WDRCUP
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      DONFG = 0
C
      IF (DBLK.EQ.0) THEN
C       first time through
        CALL WSTFGP (WDMSFL,DSN,STDAT,RWFLG,
     O               GREC,GPOS,GIND,DREC,DPOS,FRAC,
     1               DIMX,DIMY,DIMZ,DIMDAT,LTYPE,RETCOD)
        IF (DTYPE.NE.LTYPE) THEN
C         oops, types dont match
          RETCOD= -45
        END IF
        IF (RETCOD.EQ.0) THEN
C         data exists for the period specified,read/write it
          DIND(1)= WDRCGO(WDMSFL,DREC(1))
          IF (FRAC(2).GT.0.0 .AND. RWFLG.EQ.1) THEN
C           interpolation required to calculate value
            DIND(2)= WDRCGO(WDMSFL,DREC(2))
            DNUM   = 2
          ELSE
C           writing to wdm or no interpolation required
            DNUM= 1
          END IF
          BLKLEN= DIMX* DIMY* DIMZ* DIMDAT
        ELSE
C         get out with bad return code
          DONFG= 1
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       point to the value specified
        IF (NDIM.EQ.1) THEN
C         only worry about x dimension
          IF (DBLK.EQ.0) THEN
C           first time through
            DBLK= (BASN(1)-1)* DIMDAT+ 1
            LBLK= 0
            CURX= 0
          ELSE
C           additional time thru
            LBLK= DBLK
            DBLK= DBLK+ (SKPN(1)*DIMDAT)
          END IF
C         increment counter of values got
          CURX= CURX+ 1
          IF (CURX.GE.NUMN(1)) THEN
C           got all the data we need
            DONFG= 1
          END IF
        ELSE IF (NDIM.EQ.2) THEN
C         x and y data
          IF (DBLK.EQ.0) THEN
C           first time through
            DBL1= DIMX* (BASN(2)-1)
            DBLK= (DBL1+ BASN(1)-1)* DIMDAT+ 1
            LBLK= 0
            CURX= 1
            CURY= 1
          ELSE
C           additional time thru
            LBLK= DBLK
            CURX= CURX+ 1
            IF (NUMN(1).EQ.1 .OR. MOD(CURX,NUMN(1)).EQ.1) THEN
C             finished a pass thru x, go to next y
              CURX= 1
              CURY= CURY+ 1
              DBL1= DIMX* (BASN(2)-1+SKPN(2)*(CURY-1))
              DBLK= (DBL1+ BASN(1)-1)*DIMDAT+1
            ELSE
              DBLK= DBLK+ (SKPN(1)*DIMDAT)
            END IF
          END IF
C         increment counter of values got
          IF (CURX.GE.NUMN(1).AND.CURY.GE.NUMN(2)) THEN
C           got all the data we need
            DONFG= 1
          END IF
        ELSE
C         x,y,z data
          IF (DBLK.EQ.0) THEN
C           first time through
            DBL2= DIMY* (BASN(3)-1)
            DBL1= DIMX* (BASN(2)-1+ DBL2)
            DBLK= (DBL1+ BASN(1)-1)* DIMDAT+ 1
            LBLK= 0
            CURX= 1
            CURY= 1
            CURZ= 1
          ELSE
C           additional time thru
            LBLK= DBLK
            CURX= CURX+ 1
            IF (NUMN(1).EQ.1 .OR. MOD(CURX,NUMN(1)).EQ.1) THEN
C             finished a pass thru x, go to next y
              CURX= 1
              CURY= CURY+ 1
              IF (NUMN(2).EQ.1 .OR. MOD(CURY,NUMN(2)).EQ.1) THEN
C               finished a pass thru y, go to next z
                CURY= 1
                CURZ= CURZ+ 1
              END IF
              DBL2= DIMY* (BASN(3)-1+SKPN(3)*(CURZ-1))
              DBL1= DIMX* (BASN(2)-1+SKPN(2)*(CURY-1)+ DBL2)
              DBLK= (DBL1+ BASN(1)-1)* DIMDAT+1
            ELSE
              DBLK= DBLK+ (SKPN(1)*DIMDAT)
            END IF
          END IF
C         increment counter of values got
          IF (CURX.GE.NUMN(1) .AND.
     1        CURY.GE.NUMN(2) .AND.
     2        CURZ.GE.NUMN(3)) THEN
C           got all the data we need
            DONFG= 1
          END IF
        END IF
C
        IF (DBLK.LE.BLKLEN.AND.DBLK.GT.0) THEN
C         get the next values position
          DO 30 J= 1,DNUM
C           may need to get position of 2 data values
            DPOS(J)= DPOS(J)+ DBLK- LBLK
            IF (DPOS(J).GT.512) THEN
C             move to next record
              IF (RWFLG.EQ.2) THEN
C               save record we have been writing to
                CALL WDRCUP(WDMSFL,DIND(J))
              END IF
 10           CONTINUE
C               time to read next wdm record
                DREC(J)= WIBUFF(4,DIND(J))
                DIND(J)= WDRCGO(WDMSFL,DREC(J))
                DPOS(J)= DPOS(J)- 508
              IF (DPOS(J).GT.512) GO TO 10
            ELSE IF (DPOS(J).LT.5) THEN
C             move back a record
              IF (RWFLG.EQ.2) THEN
C               save record we have been writing to
                CALL WDRCUP(WDMSFL,DIND(J))
              END IF
 20           CONTINUE
C               time to read previous wdm record
                DREC(J)= WIBUFF(3,DIND(J))
                DIND(J)= WDRCGO(WDMSFL,DREC(J))
                DPOS(J)= DPOS(J)+ 508
              IF (DPOS(J).LT.5) GO TO 20
            END IF
 30       CONTINUE
        ELSE
C         trying to get/put more data than in block
          DONFG = 1
          RETCOD= -44
        END IF
      END IF
C
C      WRITE (*,2000) CURX,CURY,CURZ,DREC,DPOS,DBLK,FRAC
C2000  FORMAT (8I8,F5.3)
      RETURN
      END
C
C
C
      SUBROUTINE   WSTGTI
     I                   (WDMSFL,DSN,STDAT,NDIM,NUMN,BASN,SKPN,NVAL,
     O                    IBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     get integer space time data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),NDIM,NUMN(NDIM),BASN(NDIM),
     1         SKPN(NDIM),NVAL,IBUFF(NVAL),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     NVAL   - Total number of values to get
C     IBUFF  - Buffer to put values in
C     RETCOD - Return code
C                0 - data retrieved
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DBLK,DONFG,RWFLG,DPOS(2),DIND(2),DTYPE
      REAL       FRAC(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTWNT
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 1
      DTYPE= 1
      I    = 0
      DBLK = 0
 10   CONTINUE
        I= I+ 1
        CALL WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,NDIM,NUMN,BASN,SKPN,
     M               DPOS,DIND,DBLK,
     O               FRAC,DONFG,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         calc and put the value in users buffer
          IBUFF(I)= WIBUFF(DPOS(1),DIND(1))*FRAC(1)
          IF (FRAC(2).GT.0.0) THEN
C           fraction from next value
            IBUFF(I)= IBUFF(I)+ WIBUFF(DPOS(2),DIND(2))*FRAC(2)
          END IF
        END IF
      IF (I.LT.NVAL .AND. DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTGTR
     I                   (WDMSFL,DSN,STDAT,NDIM,NUMN,BASN,SKPN,NVAL,
     O                    RBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     get real space time data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),NDIM,NUMN(NDIM),BASN(NDIM),
     1         SKPN(NDIM),NVAL,RETCOD
      REAL     RBUFF(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     NVAL   - Total number of values to get
C     RBUFF  - Buffer to put values in
C     RETCOD - Return code
C                0 - data retrieved
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DBLK,DONFG,RWFLG,DPOS(2),DIND(2),DTYPE
      REAL       FRAC(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTWNT
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 1
      DTYPE= 2
      I    = 0
      DBLK = 0
 10   CONTINUE
        I= I+ 1
        CALL WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,NDIM,NUMN,BASN,SKPN,
     M               DPOS,DIND,DBLK,
     O               FRAC,DONFG,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         calc and put the value in users buffer
          RBUFF(I)= WRBUFF(DPOS(1),DIND(1))*FRAC(1)
          IF (FRAC(2).GT.0.0) THEN
C           fraction from next value
            RBUFF(I)= RBUFF(I)+ WRBUFF(DPOS(2),DIND(2))*FRAC(2)
          END IF
        END IF
      IF (I.LT.NVAL .AND. DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTGTD
     I                   (WDMSFL,DSN,STDAT,NDIM,NUMN,BASN,SKPN,NVAL,
     O                    DBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     get double precision space time data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),NDIM,NUMN(NDIM),BASN(NDIM),
     1         SKPN(NDIM),NVAL,RETCOD
      DOUBLE PRECISION DBUFF(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     NVAL   - Total number of values to get
C     DBUFF  - Buffer to put values in
C     RETCOD - Return code
C                0 - data retrieved
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DBLK,DONFG,RWFLG,DPOS(2),DIND(2),DTYPE
      REAL       FRAC(2),RTMP(2)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (DTMP,RTMP)
      DOUBLE PRECISION  DTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTWNT
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 1
      DTYPE= 3
      I    = 0
      DBLK = 0
 10   CONTINUE
        I= I+ 1
        CALL WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,NDIM,NUMN,BASN,SKPN,
     M               DPOS,DIND,DBLK,
     O               FRAC,DONFG,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         get the data
          RTMP(1) = WRBUFF(DPOS(1),DIND(1))
          RTMP(2) = WRBUFF(DPOS(1)+1,DIND(1))
          DBUFF(I)= DTMP* FRAC(1)
          IF (FRAC(2).GT.0.0) THEN
C           use part of next value
            RTMP(1) = WRBUFF(DPOS(2),DIND(2))
            RTMP(2) = WRBUFF(DPOS(2)+1,DIND(2))
            DBUFF(I)= DBUFF(I)+ DTMP* FRAC(2)
          END IF
        END IF
      IF (I.LT.NVAL .AND. DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTPTI
     I                   (WDMSFL,DSN,STDAT,NDIM,NUMN,BASN,SKPN,NVAL,
     I                    IBUFF,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     put integer space time data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),NDIM,NUMN(NDIM),BASN(NDIM),
     1         SKPN(NDIM),NVAL,IBUFF(NVAL),RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     NVAL   - Total number of values to get
C     IBUFF  - Buffer to write values from
C     RETCOD - Return code
C                0 - data written
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DBLK,DONFG,RWFLG,DPOS(2),DIND(2),DTYPE
      REAL       FRAC(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTWNT,WDRCUP,WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 2
      DTYPE= 1
      I    = 0
      DBLK = 0
 10   CONTINUE
        I= I+ 1
        CALL WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,NDIM,NUMN,BASN,SKPN,
     M               DPOS,DIND,DBLK,
     O               FRAC,DONFG,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put the value in the buffer
          WIBUFF(DPOS(1),DIND(1)) =IBUFF(I)
        END IF
      IF (I.LT.NVAL .AND. DONFG.EQ.0) GO TO 10
C
      IF (RETCOD.EQ.0) THEN
C       update last record
        CALL WDRCUP(WDMSFL,DIND(1))
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTPTR
     I                   (WDMSFL,DSN,STDAT,NDIM,NUMN,BASN,SKPN,NVAL,
     I                    RBUFF,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     put real space time data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),NDIM,NUMN(NDIM),BASN(NDIM),
     1         SKPN(NDIM),NVAL,RETCOD
      REAL     RBUFF(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     NVAL   - Total number of values to get
C     RBUFF  - Buffer to write values from
C     RETCOD - Return code
C                0 - data written
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DBLK,DONFG,RWFLG,DPOS(2),DIND(2),DTYPE
      REAL       FRAC(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTWNT,WDRCUP,WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 2
      DTYPE= 2
      I    = 0
      DBLK = 0
 10   CONTINUE
        I= I+ 1
        CALL WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,NDIM,NUMN,BASN,SKPN,
     M               DPOS,DIND,DBLK,
     O               FRAC,DONFG,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put the value in the buffer
          WRBUFF(DPOS(1),DIND(1)) =RBUFF(I)
        END IF
      IF (I.LT.NVAL .AND. DONFG.EQ.0) GO TO 10
C
      IF (RETCOD.EQ.0) THEN
C       update last record
        CALL WDRCUP(WDMSFL,DIND(1))
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTPTD
     I                   (WDMSFL,DSN,STDAT,NDIM,NUMN,BASN,SKPN,NVAL,
     I                    DBUFF,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     put double precision space time data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  WDMSFL,DSN,STDAT(6),NDIM,NUMN(NDIM),BASN(NDIM),
     1         SKPN(NDIM),NVAL,RETCOD
      DOUBLE PRECISION DBUFF(NVAL)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Data-set number
C     STDAT  - Date of data to get
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     NVAL   - Total number of values to get
C     DBUFF  - Buffer to write values from
C     RETCOD - Return code
C                0 - data written
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DBLK,DONFG,RWFLG,DPOS(2),DIND(2),DTYPE
      REAL       FRAC(2),RTMP(2)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (DTMP,RTMP)
      DOUBLE PRECISION  DTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL   WSTWNT,WDRCUP,WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
      RWFLG= 2
      DTYPE= 3
      I    = 0
      DBLK = 0
 10   CONTINUE
        I= I+ 1
        CALL WSTWNT (WDMSFL,DSN,STDAT,RWFLG,DTYPE,NDIM,NUMN,BASN,SKPN,
     M               DPOS,DIND,DBLK,
     O               FRAC,DONFG,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         put the data into the wdm buffer
          DTMP= DBUFF(I)
          WRBUFF(DPOS(1),DIND(1))  = RTMP(1)
          WRBUFF(DPOS(1)+1,DIND(1))= RTMP(2)
        END IF
      IF (I.LT.NVAL .AND. DONFG.EQ.0) GO TO 10
C
      IF (RETCOD.EQ.0) THEN
C       update last record
        CALL WDRCUP(WDMSFL,DIND(1))
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTSCP (DIRFG,WDMSFL,STDSN,NTMDSN,TMDSN,
     I                     STDAT,TUNIT,TSTEP,NOV,QUALFG,
     I                     NDIM,NUMN,BASN,SKPN,BUFMAX,
     O                     SBUFF,RETCOD)
C
C     + + + PURPOSE + + +
C     copy from WDM space time data set to timeseries data sets or
C          from WDM timseries data sets to space time data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     DIRFG,WDMSFL,STDSN,NTMDSN,TMDSN(NTMDSN),
     1            STDAT(6),TUNIT,TSTEP,NOV,QUALFG,
     2            NDIM,NUMN(NDIM),BASN(NDIM),SKPN(NDIM),BUFMAX,RETCOD
      REAL        SBUFF(BUFMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DIRFG  - Direction to copy flag - 1 st to ts, 2 ts to st
C     WDMSFL - Fortran unit number of WDM file
C     STDSN  - Space time data-set number
C     NTMDSN - Number of timeseries data-sets
C     TMDSN  - Timeseries data-set numbers
C     STDAT  - Starting date of data to copy
C     TUNIT  - Time units to get
C     TSTEP  - Time step to get
C     NOV    - Number of values to get
C     QUALFG - Timeseries quality of data flag
C     NDIM   - Number of dimensions specified
C     NUMN   - Number of values to get in each dimension
C     BASN   - Base value in each dimension
C     SKPN   - Skip value in each dimension
C     BUFMAX - Size of buffer for intermediate storage
C     SBUFF  - Buffer for intermediate storage
C     RETCOD - Return code
C                0 - data copied
C               -8 - invalid date
C               -9 - data not present in current group
C              -10 - no data in this group
C              -11 - no non missing data, data has not started yet
C              -14 - date specified not within valid range for data set
C              -15 - VBTIME=1 and DELT,TUNITS do not agree
C                    with the data set
C              -20 - problem with one or more of following:
C                    GPFLG, DXX, NVAL, QUALVL, LTSTEP, LTUNIT
C              -21 - date from WDM doesn't match expected date
C              -36 - missing needed following data for a get
C              -37 - no data present
C              -38 - missing part of time required
C              -39 - missing data group
C              -40 - no data available
C              -41 - no data to read
C              -42 - overlap an existing group
C              -44 - trying to get/put more data than in block
C              -45 - types don't match
C              -47 - bad direction flag
C              -48 - conflicting specification of space time dimensions and
C                    number of timeseries data sets
C              -81 - data set does not exist
C              -82 - data set exists, but is wrong DSTYP
C              -84 - data set number out of range
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   TMPDAT(6),BUFDAT(6),NXTDAT(6),REDO,
     1          I,CBUF,CTST,CDSN,TNOV,CNOV,LNOV,BCNT,STDIM,I1,I0
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDATCP,TIMADD,WSTGTR,WDTPUT,WSTPTR,WDTGET,WTDDEL
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
      I0    = 0
      I1    = 1
C
      IF (DIRFG.LT.1 .OR. DIRFG.GT.2) THEN
C       bad direction flag
        RETCOD= -47
      ELSE
C       how much space time data is specified
        STDIM= 1
        DO 10 I= 1,NDIM
          STDIM= STDIM* NUMN(I)
 10     CONTINUE
        IF (STDIM.NE.NTMDSN) THEN
C         conflicting specification of space time dimensions and
C         number of timeseries data sets
          RETCOD= -48
        ELSE
C         is the buffer big enough to store all data
          TNOV= (BUFMAX- NTMDSN)/NTMDSN
          IF (TNOV.GE.NOV) THEN
C           buffer plenty big
            BCNT= 1
            LNOV= NOV
          ELSE
C           need multiple buffers
            BCNT= NOV/TNOV
            LNOV= MOD(NOV,TNOV)
            IF (LNOV.NE.0) THEN
C             need a final buffer
              BCNT= BCNT+ 1
            ELSE
C             end on a buffer boundary
              LNOV= TNOV
            END IF
          END IF
C
C         make copy of starting date
          CALL WDATCP (STDAT,TMPDAT)
C         initialize buffer parms
          CNOV= TNOV
          CBUF= 1
 20       CONTINUE
C           save start date of buffer
            CALL WDATCP (TMPDAT,BUFDAT)
C           loop to process a buffer of data
            IF (CBUF.EQ.BCNT) THEN
C             final buffer
              CNOV= LNOV
            END IF
            IF (DIRFG.EQ.1) THEN
C             space time to timeseries
              CTST= 1
 30           CONTINUE
C               get space time data
                CALL WSTGTR (WDMSFL,STDSN,TMPDAT,
     I                       NDIM,NUMN,BASN,SKPN,NTMDSN,
     O                       SBUFF,RETCOD)
                IF (RETCOD.EQ.0) THEN
C                 put in buffer positions for timeseries
                  DO 40 CDSN= 1,NTMDSN
                    I= NTMDSN+ (CDSN-1)* CNOV+ CTST
                    SBUFF(I)= SBUFF(CDSN)
 40               CONTINUE
                END IF
                CALL TIMADD(TMPDAT,TUNIT,TSTEP,I1,
     O                      NXTDAT)
                CALL WDATCP(NXTDAT,TMPDAT)
                CTST= CTST+ 1
              IF (CTST.LE.CNOV .AND. RETCOD.EQ.0) GOTO 30
              IF (RETCOD.EQ.0) THEN
C               write timeseries data sets
                CDSN= 1
 50             CONTINUE
 55               CONTINUE
                    REDO= 0
                    I   = NTMDSN+ (CDSN-1)* CNOV+ 1
                    CALL WDTPUT (WDMSFL,TMDSN(CDSN),TSTEP,BUFDAT,CNOV,
     I                           I0,QUALFG,TUNIT,SBUFF(I),
     O                           RETCOD)
                    IF (RETCOD.EQ.-9) THEN
C                     couldn't overwrite data, delete existing data
                      CALL WTDDEL (WDMSFL,TMDSN(CDSN),BUFDAT,I1,
     O                             RETCOD)
                      IF (RETCOD.EQ.0) THEN
C                       try again
                        REDO= 1
                      END IF
                    END IF
                  IF (REDO.EQ.1) GO TO 55
                  CDSN= CDSN+ 1
                IF (RETCOD.EQ.0 .AND. CDSN.LE.NTMDSN) GO TO 50
              END IF
            ELSE
C             timeseries to space time, get timeseries data
              CDSN= 1
 60           CONTINUE
                I= NTMDSN+ (CDSN-1)* CNOV+ 1
                CALL WDTGET (WDMSFL,TMDSN(CDSN),TSTEP,BUFDAT,CNOV,
     I                       I1,QUALFG,TUNIT,SBUFF(I),
     O                       RETCOD)
                CDSN= CDSN+ 1
              IF (RETCOD.EQ.0 .AND. CDSN.LE.NTMDSN) GO TO 60
              IF (RETCOD.EQ.0) THEN
C               put space time data for each timestep
                CTST= 1
 70             CONTINUE
C                 put in buffer positions for space time
                  DO 80 CDSN= 1,NTMDSN
                    I= NTMDSN+ (CDSN-1)* CNOV+ CTST
                    SBUFF(CDSN)= SBUFF(I)
 80               CONTINUE
C                 put space time data
                  CALL WSTPTR (WDMSFL,STDSN,TMPDAT,
     I                         NDIM,NUMN,BASN,SKPN,NTMDSN,SBUFF,
     O                         RETCOD)
C                 calc next timestep
                  CALL TIMADD(TMPDAT,TUNIT,TSTEP,I1,
     O                        NXTDAT)
                  CALL WDATCP(NXTDAT,TMPDAT)
                  CTST= CTST+ 1
                IF (CTST.LE.CNOV .AND. RETCOD.EQ.0) GO TO 70
              END IF
            END IF
            CBUF= CBUF+ 1
          IF (CBUF.LE.BCNT .AND. RETCOD.EQ.0) GO TO 20
        END IF
      END IF
C
      RETURN
      END
