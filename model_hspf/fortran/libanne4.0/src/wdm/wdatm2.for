C
C
C
      SUBROUTINE   WADGTN
     I                   (MESSFL,ATIND,
     O                    ATNAM)
C
C     + + + PURPOSE + + +
C     given attribute index, return attribute name
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,ATIND
      CHARACTER*1 ATNAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL  - Fortran unit number for WDM message file
C     ATIND   - attribute index
C     ATNAM   - attribute name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDSAGY
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDSAGY (MESSFL,ATIND,
     O             ATNAM,I,I,I,I,I)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGRA
     I                   (MESSFL,DPTR,ATTYP,
     O                    ATMIN,ATMAX)
C
C     + + + PURPOSE + + +
C     get the min and max values for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DPTR,ATTYP
      REAL      ATMIN,ATMAX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     ATTYP  - attribute type
C     ATMIN  - minimum value for attribute
C     ATMAX  - maximum value for attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DREC,DPOS,BCWORD,ID,TLEN,IVAL(2)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until range is found
        IF (ID.EQ.3) THEN
C         range found
          DO 20 I= 1,2
C           get min and max off message file
            CALL WDNXDV (MESSFL,
     M                   DREC,DPOS,
     O                   IVAL(I))
  20      CONTINUE
          IF (ATTYP.EQ.1) THEN
            ATMIN= IVAL(1)
            ATMAX= IVAL(2)
          ELSE
            ATMIN= RVAL(1)
            ATMAX= RVAL(2)
          END IF
          ID= 0
        ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGVA
     I                   (MESSFL,DPTR,MXLEN,
     O                    CLEN,SATVAL)
C
C     + + + PURPOSE + + +
C     get the valid values for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,DPTR,MXLEN,CLEN
      CHARACTER*1 SATVAL(MXLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     MXLEN  - maximum length of valid values
C     CLEN   - total length of valid values
C     SATVAL - array of valid values for attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DREC,DPOS,BCWORD,ID,TLEN,MLEN,GLEN,CONT,OLEN,OPOS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB, WMSGTE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until valid is found
        IF (ID.EQ.4) THEN
C         valids found
          GLEN= 0
          MLEN= 0
          I   = 130
          OPOS= 1
 20       CONTINUE
            CALL WMSGTE (MESSFL,TLEN,I,
     M                   DREC,DPOS,GLEN,MLEN,
     O                   OLEN,SATVAL(OPOS),CONT)
            OPOS= OPOS+ OLEN
          IF (CONT.EQ.1) GO TO 20
          ID= 0
        ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      CLEN= TLEN
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGDF
     I                   (MESSFL,DPTR,ATTYP,
     O                    ATDEF)
C
C     + + + PURPOSE + + +
C     get the default value for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DPTR,ATTYP
      REAL      ATDEF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     ATTYP  - attribute type
C     ATDEF  - default value for attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DREC,DPOS,IVAL,BCWORD,ID,TLEN
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until default is found
        IF (ID.EQ.5) THEN
C         default found
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 IVAL)
          IF (ATTYP.EQ.1) THEN
            ATDEF= IVAL
          ELSE
            ATDEF= RVAL
          END IF
          ID= 0
        ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGDS
     I                   (MESSFL,DPTR,
     O                    SADESC)
C
C     + + + PURPOSE + + +
C     get the description for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,DPTR
      CHARACTER*1 SADESC(47)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     SADESC - description for attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,DREC,DPOS,BCWORD,ID,TLEN,MLEN,GLEN,CONT,OLEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB, WMSGTE
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until description is found
        IF (ID.EQ.6) THEN
C         description found
          GLEN= 0
          MLEN= 0
          I   = 47
          CALL WMSGTE (MESSFL,TLEN,I,
     M                 DREC,DPOS,GLEN,MLEN,
     O                 OLEN,SADESC,CONT)
          ID= 0
        ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WADGHL
     I                   (MESSFL,DPTR,
     O                    TLEN,DREC,DPOS)
C
C     + + + PURPOSE + + +
C     get the length and starting record/pos of the help info
C     for an attribute off the message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,DPTR,TLEN,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     DPTR   - pointer to start of details for this attribute
C     TLEN   - length of help info (0 - no help)
C     DREC   - record on which help info starts
C     DPOS   - position on record where help starts
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,ID,BCWORD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDPTSP, WDNXDV, WDPRPS, WATWDS, WMSSKB
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WDPTSP (DPTR,
     O             DREC,DPOS)
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (MESSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (MESSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 10   CONTINUE
C       loop through details until help is found
        IF (ID.EQ.7) THEN
C         start of help found
          ID= 0
         ELSE
C         skip to the next block of info
          CALL WMSSKB (MESSFL,TLEN,
     M                 DREC,DPOS)
          CALL WDNXDV (MESSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        END IF
      IF (ID.GT.0) GO TO 10
C
      RETURN
      END
