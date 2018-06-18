C
C
C
      SUBROUTINE   WDBCRL
     I                    (WDMSFL,DSN,DSTYPE,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     add a label to a wdmsfl
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYPE,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number to add
C     DSTYPE - type of data set, 1- timeseries, 2-table, ...
C     RETCOD - return code,
C                0 - label added
C              -71 - data set already exists
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   PSA,RREC,LWDMFL,LDSN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDLBAD, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
      CALL WDDSCK (LWDMFL,LDSN,
     O             RREC,RETCOD)
C
      IF (RETCOD.EQ.-81) THEN
C       data set does not exist, add the label
        CALL WDLBAD(LWDMFL,LDSN,DSTYPE,
     O              PSA)
        RETCOD= 0
      ELSE
C       data set already exists, can not add a label
        RETCOD= -71
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGC
     I                    (WDMSFL,DSN,SAIND,SALEN,
     O                     SAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     gets values of character search attribute for a dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,SAIND,SALEN,DSN,RETCOD
      CHARACTER*1 SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number to add
C     SAIND  - index number of attribute
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code,
C                 0 - attribute value returned
C               -81 - data set does not exist
C              -107 - attribute not present on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     SAPOS,RREC,RIND,I,J,K,LWDMFL,LDSN
      CHARACTER*4 C4DUM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDSAFL, WID2UD
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
      CALL WDDSCK (LWDMFL,LDSN,
     O             RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists, get position within buffer
        RIND= WDRCGO(LWDMFL,RREC)
C       get starting position for search attribute
        CALL WDSAFL (SAIND,WIBUFF(1,RIND),
     O               SAPOS,RETCOD)
        IF (RETCOD.EQ.0) THEN
          J= SAPOS
          DO 10 I= 1,SALEN,4
            WRITE (C4DUM,2000) WIBUFF(J,RIND)
            READ  (C4DUM,1000) (SAVAL(K),K=I,I+3)
            J= J+ 1
 10       CONTINUE
        END IF
      END IF
      IF (RETCOD.NE.0) THEN
C       no value found, return blanks
        DO 20 I= 1,SALEN
          SAVAL(I)= ' '
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGI
     I                    (WDMSFL,DSN,SAIND,SALEN,
     O                     SAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     gets the values of integer search attribute for a dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SAIND,SALEN,DSN,RETCOD
      INTEGER   SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number to add
C     SAIND  - index number of attribute
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code,
C                 0 - attribute value returned
C               -81 - data set does not exist
C              -107 - attribute not present on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAPOS,RREC,RIND,I,J,LWDMFL,LDSN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDSAFL, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
      CALL WDDSCK(LWDMFL,LDSN,
     O            RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists, get position within buffer
        RIND= WDRCGO(LWDMFL,RREC)
C       get starting position for search attribute
        CALL WDSAFL (SAIND,WIBUFF(1,RIND),
     O               SAPOS,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         get the value
          DO 10 I= 1,SALEN
            J= SAPOS+I-1
            SAVAL(I)= WIBUFF(J,RIND)
 10       CONTINUE
        END IF
      END IF
      IF (RETCOD.NE.0) THEN
C       no value found, return dummy value
        DO 20 I= 1,SALEN
          SAVAL(I)= -999
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGR
     I                    (WDMSFL,DSN,SAIND,SALEN,
     O                     SAVAL,RETCOD)
C
C     + + + PURPOSE + + +
C     Get the values of real search attribute for a data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SAIND,SALEN,DSN,RETCOD
      REAL      SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number to add
C     SAIND  - index number of attribute
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code
C                 0 - attribute value returned
C               -81 - data set does not exist
C              -107 - attribute not present on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAPOS,RREC,RIND,I,J,LWDMFL,LDSN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDSAFL, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
      CALL WDDSCK (LWDMFL,LDSN,
     O             RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       data set exists, get position within buffer
        RIND= WDRCGO(LWDMFL,RREC)
C       get starting position for search attribute
        CALL WDSAFL (SAIND,WIBUFF(1,RIND),
     O               SAPOS,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         get the value
          DO 10 I= 1,SALEN
            J= SAPOS+I-1
            SAVAL(I)= WRBUFF(J,RIND)
 10       CONTINUE
        END IF
      END IF
      IF (RETCOD.NE.0) THEN
C       no value found, return dummy value
        DO 20 I= 1,SALEN
          SAVAL(I)= -999.
 20     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDSRN
     I                    (WDMSFL,ODSN,NDSN,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     routine to renumber data sets with no user interaction
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,ODSN,NDSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     ODSN   - old data-set number
C     NDSN   - new data-set number
C     RETCOD - return code
C                0 - renumber successfully completed
C              -72 - old data set does not exist
C              -73 - new data set already exists
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cdrloc.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,OREC,NREC,RREC,RIND,DSTYPE,PRDSN,NXDSN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDRCUP, WDDSCK, WDFDUP
C
C     + + + END SPECIFICATIONS + + +
C
C     does old data set exist?
      CALL WDDSCK (WDMSFL,ODSN,
     O             OREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       old exists, does new data set exist?
        CALL WDDSCK (WDMSFL,NDSN,
     O               NREC,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         it does, can't renumber
          RETCOD= -73
        ELSE
C         it does not, reset return code
          RETCOD= 0
        END IF
      ELSE
C       no old data set
        RETCOD= -72
      END IF
      IF (RETCOD.EQ.0) THEN
C       old data set exists, new data set does not
        RIND= WDRCGO (WDMSFL,OREC)
        I= 5
        WIBUFF(I,RIND)= NDSN
        CALL WDRCUP (WDMSFL,RIND)
        I= 6
        DSTYPE= WIBUFF(I,RIND)
        I= 1
        PRDSN = WIBUFF(I,RIND)
        I= 2
        NXDSN = WIBUFF(I,RIND)
C       update directory records, delete old entry
        I= 0
        CALL WDFDUP (WDMSFL,ODSN,I)
C       add new entry
        CALL WDFDUP (WDMSFL,NDSN,OREC)
C       update data set chain
        IF (NXDSN.GT.0) THEN
C         update next data set's back pointer
          CALL WDDSCK (WDMSFL,NXDSN,
     O                 RREC,RETCOD)
          RIND= WDRCGO (WDMSFL,RREC)
          WIBUFF(1,RIND)= NDSN
          CALL WDRCUP (WDMSFL,RIND)
        END IF
        IF (PRDSN.GT.0) THEN
C         update previous data-set next pointer
          CALL WDDSCK (WDMSFL,PRDSN,
     O                 RREC,RETCOD)
          RIND= WDRCGO (WDMSFL,RREC)
          I   = 2
          WIBUFF(I,RIND)= NDSN
          CALL WDRCUP (WDMSFL,RIND)
        ELSE
C         update file label record chain DSN
          RREC= 1
          RIND= WDRCGO (WDMSFL,RREC)
          I   = PTSNUM+ (DSTYPE-1)*2+ 1
          WIBUFF(I,RIND)= NDSN
          CALL WDRCUP (WDMSFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDSCL
     I                     (OWDMFL,ODSN,NWDMFL,NDSN,NTYPE,
     O                      RETCOD)
C
C     + + + PURPOSE + + +
C     copies an old data-set label into a new data-set label
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OWDMFL,ODSN,NWDMFL,NDSN,NTYPE,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OWDMFL - old watershed data management file unit number
C     ODSN   - old data-set number
C     NWDMFL - new watershed data management file unit number
C     NDSN   - new data-set number
C     NTYPE  - new data-set type, 0 - same as old,
C                1 - timeseries, 2-table, etc.
C     RETCOD - return code
C                0 - copy complete
C              -61 - old data set doesn't exist
C              -62 - new data set already exists
C              -63 - data-set type out of range
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OFREC,OIND,NFREC,NIND,DSTYPE,I,PRNREC,PDAT
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO,WDRCGX,WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDFCUP, WDFDUP, WDPTCL, WDRCGO, WDRCGX, WDRCUP
      EXTERNAL  WDCRDT
C
C     + + + END SPECIFICATIONS + + +
C
C     write(99,*) 'WDDSCL:',OWDMFL,ODSN,NWDMFL,NDSN,NTYPE
      RETCOD= 0
C     check for old data set
      CALL WDDSCK (OWDMFL,ODSN,
     O             OFREC,RETCOD)
      IF (OFREC.EQ.0) THEN
C       old data set does not exist
        RETCOD= -61
      ELSE IF (NTYPE.LT.0 .OR. NTYPE.GT.9) THEN
C       bad dataset type
        RETCOD= -63
      ELSE
C       check for new data set
        CALL WDDSCK(NWDMFL,NDSN,
     O              NFREC,RETCOD)
        IF (NFREC.GT.0) THEN
C         new data set already exists
          RETCOD= -62
        ELSE
C         ok to copy
          RETCOD= 0
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       ok to copy
        OIND  = WDRCGO(OWDMFL,OFREC)
C       type of data set
        IF (NTYPE .EQ. 0) THEN
C         use old data set type
          DSTYPE= WIBUFF(6,OIND)
        ELSE
C         new data set type specified
          DSTYPE= NTYPE
        END IF
C       where data pointers start
        PDAT  = WIBUFF(11,OIND)
C       get a new record with no pointers
        PRNREC= 0
        I     = 0
        NIND  = WDRCGX(NWDMFL,I,PRNREC)
        PRNREC= RECNO(NIND)
C       write(99,*) '  oind,nind,ofrec,prnrec,dstype:',
C    1                 OIND,NIND,OFREC,PRNREC,DSTYPE
C       fill in new dsn
        WIBUFF(5,NIND)= NDSN
C       fill in new data set type
        WIBUFF(6,NIND)= DSTYPE
C       update new record
        CALL WDRCUP(NWDMFL,NIND)
C       add new dsn to directories
C       first data set directory
        CALL WDFDUP (NWDMFL,NDSN,PRNREC)
C       next pointer in file def rec and dsn pointers
        I= 1
        CALL WDFCUP (NWDMFL,DSTYPE,NDSN,I)
C
C       be sure old label is still in memory
        OIND= WDRCGO(OWDMFL,OFREC)
C       be sure old label is still in memory
        NIND= WDRCGO(NWDMFL,PRNREC)
C       write(99,*) '  oind,rec,nind,rec:',OIND,RECNO(OIND),
C    1                                     NIND,RECNO(NIND)
C
C       dont copy data pointers
C       copy information from old record
        DO 10 I= 7,PDAT-1
          WIBUFF(I,NIND)= WIBUFF(I,OIND)
 10     CONTINUE
C       update free data space pointer to first data value pos
        WIBUFF(PDAT+1,NIND)= WDPTCL(RECNO(NIND),WIBUFF(12,OIND))
C       update the new record
        CALL WDRCUP(NWDMFL,NIND)
C       write(99,*) '  nwdmfl,nind:',NWDMFL,NIND
C       set dataset creation date attribute
        CALL WDCRDT (NWDMFL,NDSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLBAD
     I                    (WDMSFL,DSN,DSTYPE,
     O                     PSA)
C
C     + + + PURPOSE + + +
C     ** not normally called by applications, use WDBCRL **
C     add a new data set label, but no search attributes or data,
C     use default label sizing
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,DSTYPE,PSA
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number
C     DSTYPE - type of data set
C     PSA    - pointer to search attribute space
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   NDN,NUP,NSA,NSASP,NDP
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDLBAX
C
C     + + + END SPECIFICATIONS + + +
C
      NDN  = 1
      NUP  = 1
      NSA  = 35
      NSASP= 70
      NDP  = 100
C
      CALL WDLBAX (WDMSFL,DSN,DSTYPE,NDN,NUP,NSA,NSASP,NDP,
     O             PSA)
C
      RETURN
      END
