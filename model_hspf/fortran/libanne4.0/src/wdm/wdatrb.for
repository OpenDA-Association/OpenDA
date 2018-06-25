C
C
C
      SUBROUTINE   WDSAGY
     I                    (MESSFL,SAIND,
     O                     SANAM,DPTR,SATYP,SALEN,SARQWD,SAUPFG)
C
C     + + + PURPOSE + + +
C     gets general detail information about specified attribute
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,DPTR,SATYP,SALEN,SARQWD,SAUPFG
      CHARACTER*1 SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SAIND  - attribute index number
C     SANAM  - name of search attribute
C     DPTR   - pointer to other details of attribute
C     SALEN  - length of attribute
C     SATYP  - type of attribute
C     SARQWD - word containing attribute requirements by dsn type
C     SAUPFG - attribute update flag
C
C     + + + PARAMETERS + + +
      INTEGER    DSNMAX
      PARAMETER (DSNMAX=10)
C
C     + + + SAVES + + +
      INTEGER     DSINIT,DSNCNT,DSN(DSNMAX),ATTIND(2,DSNMAX)
      SAVE        DSINIT,DSNCNT,DSN,ATTIND
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IGOT
      CHARACTER*1 BLNK
C
C     + + + EXTERNALS + + +
      EXTERNAL    WADDSI, WADGTL, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DSINIT / 0 /
C
C     + + + END SPECIFICATIONS + + +
C
      IF (DSINIT.EQ.0) THEN
C       determine first and last attributes on data sets
        CALL WADDSI (MESSFL,DSNMAX,
     O               DSNCNT,DSN,ATTIND)
        DSINIT= 1
      END IF
C
      I   = 0
      IGOT= 0
 10   CONTINUE
        I= I+ 1
        IF (SAIND.GE.ATTIND(1,I) .AND. SAIND.LE.ATTIND(2,I)) THEN
C         attribute is on this data set
          IGOT= 1
C         get all the label info
          CALL WADGTL (MESSFL,DSN(I),SAIND,
     O                 SANAM,DPTR,SATYP,SALEN,SARQWD,SAUPFG)
        END IF
      IF (IGOT.EQ.0 .AND. I.LT.DSNMAX) GO TO 10
      IF (IGOT.EQ.0) THEN
C       attribute index not found, set output arguments to undefined
        I     = 6
        BLNK  = ' '
        CALL ZIPC (I,BLNK,SANAM)
        DPTR  = 0
        SATYP = 0
        SALEN = 0
        SARQWD= 0
        SAUPFG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSAC
     I                    (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     adds (or modifies) character search attribute on given dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      CHARACTER*1 SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number being modified
C     MESSFL - message file unit number
C     SAIND  - index number of attribute or
C              highest attribure number if printing
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code indicating if add or mod was successful
C                0 - successful
C              -81 - data set does not exist
C             -101 - incorrect character value for attribute
C             -103 - no room on label for attribute
C             -104 - data present, can't update attribute
C             -105 - attribute not allowed for this type data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + PARAMETERS + + +
      INTEGER     MLEN
      PARAMETER  (MLEN=300)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,RREC,RIND,PSAVAL,DPTR,DELFG,
     1            CNUM,CLEN,SATYP,SAUPFG,SARQWD,LWDMFL,LDSN
      CHARACTER*1 LCBUF(MLEN)
      CHARACTER*4 C4DUM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDRCUP, CHKSTR
      EXTERNAL    WDSASP, WDSAGY, WADGVA, WDDPAR, WID2UD
C
C     + + + INPUT FORMATS + + +
 1020 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4A1)
C
C     + + + END SPECIFICATIONS + + +
C
      DELFG = 0
      RETCOD= 0
      SATYP = 3
      CALL WDSAGY (MESSFL,SAIND,
     O             LCBUF(295),DPTR,I,I,SARQWD,SAUPFG)
C     get valid attribute values
      CALL WADGVA (MESSFL,DPTR,MLEN,
     O             CLEN,LCBUF)
C     determine number of valid values
      CNUM= CLEN/SALEN
      IF (CNUM.GT.0) THEN
C       valid values exist
        I= CHKSTR(SALEN,CNUM,SAVAL,LCBUF)
        IF (I.EQ.0) THEN
C         not a valid character attribute value
          RETCOD= -101
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       does data set exist?
        CALL WDDSCK(LWDMFL,LDSN,
     O              RREC,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         it does, get the label
          RIND= WDRCGO(LWDMFL,RREC)
C         is it ok to add/update the attribute?
          CALL WDDPAR (RREC,SARQWD,SAUPFG,WIBUFF(1,RIND),DELFG,
     O                 RETCOD)
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       add the attribute, first get where it starts
        CALL WDSASP (SAIND,SALEN,SATYP,
     M               WIBUFF(1,RIND),
     O               PSAVAL,RETCOD)
        IF (RETCOD.EQ.-102) THEN
C         no problem, it was already on label so adding not required
          RETCOD= 0
        END IF
        IF (PSAVAL.GT.0) THEN
C         ok to add/modify, do it
          K= -1
          DO 30 I= 1,SALEN,4
            K= K+ 1
            WRITE(C4DUM,2000) (SAVAL(J),J=I,I+3)
            READ (C4DUM,1020) WIBUFF(PSAVAL+K,RIND)
 30       CONTINUE
          CALL WDRCUP(LWDMFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSAI
     I                    (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     adds (or modifies) integer search attribute on given dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      INTEGER   SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number being modified
C     MESSFL - message file unit number
C     SAIND  - index number of attribute or
C              highest attribure number if printing
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - return code indicating if add or mod was successful
C                0 - successful
C              -81 - data set does not exist
C             -103 - no room on label for attribute
C             -104 - data present, can't update attribute
C             -105 - attribute not allowed for this type data set
C             -108 - incorrect integer value for attribute
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,RREC,RIND,IMIN,IMAX,PSAVAL,DPTR,DELFG,
     1            ICHK,SATYP,SAUPFG,SARQWD,LWDMFL,LDSN
      REAL        RMIN,RMAX
      CHARACTER*1 TBUFF(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDRCUP, WDSASP
      EXTERNAL    WDSAGY, WADGRA, CHKINT, WDDPAR, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      DELFG = 0
      RETCOD= 0
      SATYP = 1
      CALL WDSAGY (MESSFL,SAIND,
     O             TBUFF,DPTR,I,I,SARQWD,SAUPFG)
C     get min, max, default for attribute
      CALL WADGRA (MESSFL,DPTR,SATYP,
     O             RMIN,RMAX)
      IMIN= RMIN
      IMAX= RMAX
      DO 10 I= 1,SALEN
        CALL CHKINT (IMIN,IMAX,I0,SAVAL(I),ICHK)
        IF (ICHK.EQ.0) THEN
C         bad value for integer attribute
          RETCOD= -108
        END IF
 10   CONTINUE
      IF (RETCOD.EQ.0) THEN
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       does data set exist
        CALL WDDSCK(LWDMFL,LDSN,
     M              RREC,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         data set exists
          RIND  = WDRCGO(LWDMFL,RREC)
C         is it ok to add/update the attribute?
          CALL WDDPAR (RREC,SARQWD,SAUPFG,WIBUFF(1,RIND),DELFG,
     O                 RETCOD)
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       add the attribute, first get where it starts
        CALL WDSASP (SAIND,SALEN,SATYP,
     M               WIBUFF(1,RIND),
     O               PSAVAL,RETCOD)
        IF (RETCOD.EQ.-102) THEN
C         no problem, it was already on label so adding not required
          RETCOD= 0
        END IF
        IF (PSAVAL.GT.0) THEN
C         ok to add/modify, do it
          DO 30 I= 1,SALEN
            WIBUFF(PSAVAL+I-1,RIND)= SAVAL(I)
 30       CONTINUE
          CALL WDRCUP(LWDMFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSAR
     I                    (WDMSFL,DSN,MESSFL,SAIND,SALEN,SAVAL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     adds (or modifies) real search attribute on given dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,MESSFL,SAIND,SALEN,RETCOD
      REAL        SAVAL(SALEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number being modified
C     MESSFL - message file unit number
C     SAIND  - index number of attribute or
C              highest attribure number if printing
C     SALEN  - length of attribute
C     SAVAL  - value of attribute
C     RETCOD - flag indicating if modification or addition was successful
C                0 - successful
C              -81 - data set does not exist
C             -103 - no room on label for attribute
C             -104 - data present, can't update attribute
C             -105 - attribute not allowed for this type data set
C             -109 - incorrect real value for attribute
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,RREC,RIND,PSAVAL,ICHK,SATYP,DPTR,SAUPFG,SARQWD,
     1            DELFG,LWDMFL,LDSN
      REAL        RMIN,RMAX,R0
      CHARACTER*1 TBUFF(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDDSCK, WDRCGO, WDRCUP, WDSASP
      EXTERNAL    WDSAGY, CHKREA, WADGRA, WDDPAR, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      R0    = 0.0
      DELFG = 0
      RETCOD= 0
      SATYP = 2
      CALL WDSAGY (MESSFL,SAIND,
     O             TBUFF,DPTR,I,I,SARQWD,SAUPFG)
      CALL WADGRA (MESSFL,DPTR,SATYP,
     O             RMIN,RMAX)
      DO 10 I= 1,SALEN
        CALL CHKREA (RMIN,RMAX,R0,SAVAL(I),ICHK)
        IF (ICHK.EQ.0) THEN
          RETCOD= -109
        END IF
 10   CONTINUE
      IF (RETCOD.EQ.0) THEN
C       adjust wdm and dsn as needed
        CALL WID2UD (WDMSFL,DSN,
     O               LWDMFL,LDSN)
C       does data set exist
        CALL WDDSCK(LWDMFL,LDSN,
     O              RREC,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         data set exists
          RIND  = WDRCGO(LWDMFL,RREC)
C         is it ok to add/update the attribute?
          CALL WDDPAR (RREC,SARQWD,SAUPFG,WIBUFF(1,RIND),DELFG,
     O                 RETCOD)
        END IF
      END IF
      IF (RETCOD.EQ.0) THEN
C       add the attribute, first get where it starts
        CALL WDSASP (SAIND,SALEN,SATYP,
     M               WIBUFF(1,RIND),
     O               PSAVAL,RETCOD)
        IF (RETCOD.EQ.-102) THEN
C         no problem, it was already on label so adding not required
          RETCOD= 0
        END IF
        IF (PSAVAL.GT.0) THEN
C         ok to add/modify, do it
          DO 30 I= 1,SALEN
            WRBUFF(PSAVAL+I-1,RIND)= SAVAL(I)
 30       CONTINUE
          CALL WDRCUP(LWDMFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSAD
     I                    (WDMSFL,DSN,SAIND,
     I                     SAUPFG,SARQWD,SALEN,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     deletes search attribute on given dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,DSN,SAIND,SAUPFG,SARQWD,SALEN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - watershed data management file unit number
C     DSN    - data-set number being modified
C     SAIND  - index number of attribute
C     SAUPFG - update allowed if data present flag(0-yes)
C     SARQWD - search attribute required word
C     SALEN  - length of attribute
C     RETCOD - flag indicating if deletion successful
C                0 - deletion successful
C              -81 - data set does not exist
C             -104 - data present, can't delete attribute
C             -106 - attribute required for this type data set, can't delete
C             -107 - attribute not present on this data set
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,PSA,PDAT,PSAVAL,I,J,K,SACNT,DELFG,LWDMFL,LDSN
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDSCK, WDRCGO, WDSAFL, WDRCUP, WDDPAR, WID2UD
C
C     + + + END SPECIFICATIONS + + +
C
      DELFG = 1
      RETCOD= 0
C     adjust wdm and dsn as needed
      CALL WID2UD (WDMSFL,DSN,
     O             LWDMFL,LDSN)
C     does specified data set exist
      CALL WDDSCK(LWDMFL,LDSN,
     O            RREC,RETCOD)
      IF (RETCOD.EQ.0) THEN
C       get info about data set
        RIND= WDRCGO(LWDMFL,RREC)
C       is it ok to delete the attribute?
        CALL WDDPAR (RREC,SARQWD,SAUPFG,WIBUFF(1,RIND),DELFG,
     O               RETCOD)
      END IF
      IF (RETCOD.EQ.0) THEN
C       get where attribute starts
        CALL WDSAFL (SAIND,WIBUFF(1,RIND),
     O               PSAVAL,RETCOD)
        IF (PSAVAL.GT.0) THEN
C         attribue exists and ok to delete, do it
          PSA  = WIBUFF(10,RIND)
          SACNT= WIBUFF(PSA,RIND)
          PDAT = WIBUFF(11,RIND)
          I    = 0
 10       CONTINUE
            I= I+ 1
            J= PSA+ I* 2
          IF (WIBUFF(J,RIND).NE.SAIND) GO TO 10
C
          IF (I.LT.SACNT) THEN
C           fix pointers for later attributes
            DO 20 K= I,SACNT- 1
              J= PSA+ K* 2
              WIBUFF(J,RIND)  = WIBUFF(J+2,RIND)
              WIBUFF(J+1,RIND)= WIBUFF(J+3,RIND)- SALEN
 20         CONTINUE
C           reuse attribute data space by compacting
            DO 30 K= PSAVAL,PDAT- SALEN- 1
              WIBUFF(K,RIND)= WIBUFF(K+SALEN,RIND)
 30         CONTINUE
          END IF
C         remove last attribute pointers
          J= PSA+ SACNT* 2
          WIBUFF(J,RIND)  = 0
          WIBUFF(J+1,RIND)= 0
C         remove last attribute data
          DO 40 K= PDAT-SALEN,PDAT-1
            WIBUFF(K,RIND)= -999
 40       CONTINUE
C         reduce attribute counter
          SACNT= SACNT- 1
          WIBUFF(PSA,RIND)= SACNT
C         save revised label
          CALL WDRCUP(LWDMFL,RIND)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAFL
     I                   (SAIND,TIBUFF,
     O                    SAPOS,RETCOD)
C
C     + + + PURPOSE + + +
C     determines where values for particular search attribute
C     start within data-set label, returns 0 if attribute is
C     not present
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   SAIND,SAPOS,RETCOD
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SAIND  - index of particular search attribute
C     TIBUFF - buffer of search attributes
C     SAPOS  - position of search attribute on label
C     RETCOD - return code
C                 0 - attribute found, position returned
C              -107 - attribute not present on this data set
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END DECLARATIONS + + +
C
      RETCOD= 0
      SAPOS = WDSASV(SAIND,TIBUFF)
C
      IF (SAPOS.EQ.0) THEN
C       attribute not found
        RETCOD= -107
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDDPAR
     I                   (RREC,SARQWD,SAUPFG,TIBUFF,DELFG,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     determines if either data is present and attribute can't be updated
C     or attribute is not allowed for this dataset type
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   RREC,SARQWD,SAUPFG,DELFG,RETCOD
      INTEGER*4 TIBUFF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     RREC   - record containing data-set label
C     SARQWD - search attribute required word
C     SAUPFG - flag indicating if attribute may be updated if data present
C              0 - yes, 1 - no
C     TIBUFF - array containing data-set label
C     DELFG  - delete attribute indicator
C              0 - not trying to delete
C              1 - trying to delete
C     RETCOD - return code
C                0 - attribute can be added or updated
C             -104 - data present, can't update attribue
C             -105 - attribute not allowed for this type dataset
C             -106 - attribute required for this type data set, can't delete
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DSTYPE,SAREQ(10),DPFLG
C
C     + + + FUNCTIONS + + +
      INTEGER   WDDTFG
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDDTFG,WATTUS
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     what type data set
      DSTYPE= TIBUFF(6)
C     is data present
      DPFLG = WDDTFG(RREC,TIBUFF(1))
C     determine required attributes for data sets
      CALL WATTUS (SARQWD,
     O             SAREQ)
      IF (DPFLG.EQ.1 .AND. SAUPFG.EQ.1) THEN
C       data present, cant update attribute
        RETCOD= -104
      ELSE IF (SAREQ(DSTYPE).EQ.0) THEN
C       attribute not allowed for this type data set
        RETCOD= -105
      ELSE IF (SAREQ(DSTYPE).EQ.2 .AND. DELFG.EQ.1) THEN
C       attribute required for this type data set
        RETCOD= -106
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDSAFI
     I                   (MESSFL,
     M                    SAFNAM,SAIND,
     O                    SANAM,RETCOD)
C
C     + + + PURPOSE + + +
C     Given the attribute name SAFNAM, starting at attribute
C     index SAIND, find the first attribute name which matches
C     SAFNAM.  Return the index of the next attribute which
C     also matches SAFNAM, if one exists.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,RETCOD
      CHARACTER*1 SAFNAM(6),SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SAFNAM - character array containing attribute name to search for
C     SAIND  - index of next matching attribute, if one exists,
C              otherwise, index of matching attribute
C     SANAM  - character array of first attribute name matching SAFNAM
C     RETCOD - return code
C              -110 - attributes not found on message file
C              -111 - attribute name not found (no match)
C              -112 - more attributes exist which match SAFNAM
C
C     + + + PARAMETERS + + +
      INTEGER     MXSAT,MAXDSN
      PARAMETER  (MXSAT=480,MAXDSN=10)
C
C     + + + SAVES + + +
      INTEGER     SACNT,SAVIND(MXSAT)
      CHARACTER*1 SAVNAM(6,MXSAT)
      SAVE        SACNT,SAVNAM,SAVIND
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I6,K,K1,CURIND,DONE,CHK,
     1            DSNBUF(MAXDSN),DSNCNT,ATTIND(2,MAXDSN),LSAIND
      CHARACTER*1 BLNK,TMPNAM(6)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, CHRCHR, QUPCAS, WADDSI, WADGTN
      EXTERNAL    ZIPC, ZIPI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BLNK,I0,I6,SACNT/' ',0,6,0/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     save original input attribute index
      LSAIND= SAIND
C
      IF (SACNT.EQ.0) THEN
C       get offset for attribute names on messfl, only on first call
        CALL ZIPI (MXSAT,I0,SAVIND)
        CALL ZIPC (6*MXSAT,BLNK,SAVNAM)
        DONE = 0
C       get buffer of datasets containing attributes on message file
        CALL WADDSI (MESSFL,MAXDSN,
     O               DSNCNT,DSNBUF,ATTIND)
        IF (DSNCNT.GT.0) THEN
C         attributes found, continue
          SACNT= 0
          DO 10 I= 1,DSNCNT
C           go through datasets containing attributes
            SAIND= ATTIND(1,I)
 20         CONTINUE
C             get the next attribute name from the message file
              CALL WADGTN (MESSFL,SAIND,
     O                     TMPNAM)
              IF (LENSTR(I6,TMPNAM).GT.0) THEN
C               valid name, put in save buffer
                SACNT= SACNT+ 1
                CALL CHRCHR (I6,TMPNAM,SAVNAM(1,SACNT))
                SAVIND(SACNT)= SAIND
              END IF
              SAIND= SAIND+ 1
            IF (SAIND.LE.ATTIND(2,I)) GO TO 20
 10       CONTINUE
C
C         start at original index
          SAIND= LSAIND
        ELSE
C         attributes not found on message file
          RETCOD= -110
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       start search for attribute name
C       determine index position to begin search
        I= 0
 25     CONTINUE
          I= I+ 1
          IF (SAVIND(I).EQ.SAIND) THEN
C           number in index position I matches absolute index number
            SAIND= I
            I= MXSAT
          END IF
        IF (I.LT.MXSAT) GO TO 25
C       convert to upper case for search
        CALL QUPCAS (I6,
     M               SAFNAM)
C       init output attribute name
        CALL ZIPC (I6,BLNK,SANAM)
        DONE  = 0
        LSAIND= SAIND
        CURIND= SAIND
 30     CONTINUE
          IF (CURIND.LE.SACNT) THEN
C           check strings
            K= 0
 40         CONTINUE
              K  = K+ 1
              CHK= 0
              IF (SAVNAM(K,CURIND).EQ.SAFNAM(K) .OR.
     1            SAFNAM(K).EQ.BLNK) THEN
C               characters match or blank search name
                CHK= 1
              END IF
              IF (CHK.EQ.1) THEN
                IF (K.EQ.I6) THEN
C                 sure unique match
                  SAIND= CURIND
                  DONE = 1
                ELSE
                  K1= K+ 1
                  IF (SAFNAM(K1).EQ.BLNK) THEN
C                   possible match
                    IF (SAIND.EQ.LSAIND) THEN
C                     first match, save whole name
                      CALL CHRCHR (I6,SAVNAM(1,CURIND),SANAM)
C                     save index
                      SAIND = CURIND
                    ELSE
C                     conflict match
                      RETCOD= -112
C                     done searching for this time through
                      DONE= 1
                    END IF
C                   force exit from inner loop
                    K= 7
                  END IF
                END IF
              END IF
            IF (CHK.EQ.1 .AND. K.LT.6 .AND. DONE.EQ.0) GO TO 40
            IF (DONE.EQ.0) THEN
C             try next index
              CURIND= CURIND+ 1
            END IF
          ELSE
            DONE= 1
          END IF
        IF (DONE.EQ.0) GO TO 30
C
        IF (SAIND.EQ.LSAIND .AND. CURIND.GT.SAIND .AND.
     1      LENSTR(I6,SANAM).EQ.0) THEN
C         no match was found (and searched past original index)
          RETCOD= -111
        END IF
C
C       convert index position back to absolute index number
        SAIND= SAVIND(SAIND)
      END IF
C
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDBSGX
     I                    (MESSFL,
     M                     SANAM,
     O                     SAIND,SATYP,SALEN)
C
C     + + + PURPOSE + + +
C     routine gets information about search attribute from
C     message file, returns 0 if name is not an attribute
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SAIND,SATYP,SALEN
      CHARACTER*1 SANAM(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - message file unit number
C     SANAM  - name of search attribute
C     SAIND  - search attribute index number
C     SATYP  - type of attribute
C     SALEN  - length of attribute
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,RETCOD
      CHARACTER*1 SAFNAM(6)
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, WDSAFI, WDSAGY
C
C     + + + END SPECIFICATIONS + + +
C
      I= 6
      CALL CHRCHR (I,SANAM,SAFNAM)
      SAIND= 1
      CALL WDSAFI (MESSFL,SAFNAM,
     M             SAIND,
     O             SANAM,RETCOD)
      IF (((SAIND.GT.0 .AND. SAIND.LT.37) .OR. SAIND.GT.39)
     1    .AND. RETCOD.EQ.0) THEN
C       valid name, get rest of info
        CALL WDSAGY (MESSFL,SAIND,
     O               SANAM,I,SATYP,SALEN,I,I)
      ELSE
C       bad name or help, done, all, fill other info with 0
        SALEN = 0
        SATYP = 0
        IF (RETCOD.NE.0) THEN
C         bad name, set index to not found
          SAIND= 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFNDS
     I                    (WDMSFL,TYPE,SACNT,SAIND,SATYP,SABEG,SAVAL,
     I                     SALEN,SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,
     I                     SAOR,MAXDSN,DSNMIN,DSNMAX,
     M                     DSN,DSNCNT,
     O                     DSBFUL)
C
C     + + + PURPOSE + + +
C     search WDM file for datasets matching search criteria
C     This is a non-interactive version of PRWFDS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,TYPE,SACNT,DSBFUL,MAXDSN,
     1          DSNCNT,DSNMIN,DSNMAX
      INTEGER   SAIND(*),SATYP(*),SABEG(*),SACOND(*),
     1          SALEN(*),SAIMIN(*),SAIMAX(*),SAOR(*),
     2          DSN(MAXDSN)
      INTEGER*4 SAVAL(30)
      REAL      SARMIN(*),SARMAX(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     TYPE   - type of dataset searching for, 1- timeseries
C     SACNT  - number of search attributes to be considered
C     SAIND  - array of search attribute index numbers
C     SATYP  - array of search attribute type indicators
C              1 - integer number
C              2 - real number
C              3 - alphanumeric character
C     SABEG  - array of beginning positions in value arrays
C     SAVAL  - array of character values
C     SALEN  - array of search attribute lengths
C     SAIMIN - array of minimum acceptable values, integer attributes
C     SAIMAX - array of maximum acceptable values, integer attributes
C     SARMIN - array of minimum acceptable values, real attributes
C     SARMAX - array of maximum acceptable values, real attributes
C     SACOND - array of condition values,
C              1 - true
C              2 - false
C     SAOR   - array of or condition values
C     MAXDSN - maximum number of dataset numbers allowed
C     DSNMIN - minimum dataset number to consider
C     DSNMAX - maxiumu dataset number to consider
C     DSN    - array of dataset numbers
C     DSNCNT - count of dataset numbers found
C     DSBFUL - indicator flag for full DSN array
C              0 - no
C              1 - yes
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   RREC,RIND,DSNTMP,NDSN,PFDSN,ODSNCT,
     1          I,J,NOSA,NOMAT,NOADD,NOCHK,RETC
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDDSCK, WDFNCK
C
C     + + + END SPECIFICATIONS + + +
C
      ODSNCT= DSNCNT
      NOSA  = 0
      NOMAT = 0
      NOCHK = 0
      NOADD = 0
      IF (TYPE .GE. 0) THEN
        I   = 0
 10     CONTINUE
C         loop looking by type
          I= I+ 1
          IF (I .EQ. TYPE .OR. TYPE .EQ. 0) THEN
C           bring file definition record into memory
            RREC  = 1
            RIND  = WDRCGO(WDMSFL,RREC)
C           calculate pointers within file definition record
            PFDSN = PTSNUM+ (I-1)* 2+ 1
            DSNTMP= WIBUFF(PFDSN,RIND)
C
            IF (DSNTMP .GT. 0) THEN
 20           CONTINUE
C               loop to check datasets
                CALL WDDSCK (WDMSFL,DSNTMP,RREC,RETC)
                RIND= WDRCGO(WDMSFL,RREC)
                NDSN= WIBUFF(2,RIND)
                CALL WDFNCK (DSNTMP,WIBUFF(1,RIND),WRBUFF(1,RIND),
     I                       SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                       SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                       MAXDSN,
     M                       NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                       DSBFUL)
                DSNTMP= NDSN
              IF (DSBFUL .EQ. 0 .AND. DSNTMP .GT. 0) GO TO 20
            END IF
          END IF
        IF (DSBFUL .EQ. 0 .AND. I .LT. 9) GO TO 10
      ELSE IF (TYPE .EQ. -1) THEN
C       loop looking by dsn range
        DSNTMP= DSNMIN
 40     CONTINUE
          CALL WDDSCK (WDMSFL,DSNTMP,RREC,RETC)
          IF (RETC .EQ. 0) THEN
            RIND= WDRCGO(WDMSFL,RREC)
            CALL WDFNCK (DSNTMP,WIBUFF(1,RIND),WRBUFF(1,RIND),
     I                   SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                   SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                   MAXDSN,
     M                   NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                   DSBFUL)
          END IF
          DSNTMP= DSNTMP+ 1
        IF (DSNTMP .LE. DSNMAX .AND. DSBFUL .EQ. 0) GO TO 40
      ELSE
C       loop looking for buffer subset
        I= 1
 50     CONTINUE
          DSNTMP= DSN(I)
          CALL WDDSCK (WDMSFL,DSNTMP,RREC,RETC)
          RIND  = WDRCGO(WDMSFL,RREC)
          CALL WDFNCK (DSNTMP,WIBUFF(1,RIND),WRBUFF(1,RIND),
     I                 SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                 SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                 MAXDSN,
     M                 NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                 DSBFUL)
          IF (NOADD .GT. 0) THEN
C           dataset met the search criteria
            I    = I+ 1
            NOADD= 0
          ELSE
C           not a match, dataset in buffer, remove it
            J= I
            IF (J .LT. DSNCNT) THEN
 60           CONTINUE
                DSN(J)= DSN(J+1)
                J= J+ 1
              IF (J .LT. DSNCNT) GO TO 60
            END IF
            DSN(DSNCNT)= 0
            DSNCNT= DSNCNT- 1
          END IF
        IF (I .LE. DSNCNT) GO TO 50
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDFNCK
     I                    (DSNTMP,TIBUFF,TRBUFF,
     I                     SACNT,SAIND,SATYP,SABEG,SAVAL,SALEN,
     I                     SAIMIN,SAIMAX,SARMIN,SARMAX,SACOND,SAOR,
     I                     MAXDSN,
     M                     NOSA,NOMAT,NOCHK,NOADD,DSN,DSNCNT,
     O                     DSBFUL)
C
C     + + + PURPOSE + + +
C     checks datasets for match of specified search attributes
C     This is a non-interactive version of PRWFCK.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSNTMP,SACNT,NOSA,NOMAT,NOADD,DSBFUL,NOCHK,
     1          MAXDSN,DSNCNT
      INTEGER   SAIND(*),SATYP(*),SABEG(*),SACOND(*),
     1          SALEN(*),SAIMIN(*),SAIMAX(*),SAOR(*),
     2          DSN(MAXDSN)
      INTEGER*4 SAVAL(30),TIBUFF(512)
      REAL      TRBUFF(512)
      REAL      SARMIN(*),SARMAX(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DSNTMP - current dsn being checked
C     TIBUFF - integer version of current label
C     TRBUFF - real version of current label
C     SACNT  - number of search attributes to be considered
C     SAIND  - array of search attribute index numbers
C     SATYP  - array of search attribute type indicators
C              1 - integer number
C              2 - real number
C              3 - alphanumeric character
C     SABEG  - array of beginning positions in value arrays
C     SAVAL  - array of character values
C     SALEN  - array of search attribute lengths
C     SAIMIN - array of minimum acceptable values, integer attributes
C     SAIMAX - array of maximum acceptable values, integer attributes
C     SARMIN - array of minimum acceptable values, real attributes
C     SARMAX - array of maximum acceptable values, real attributes
C     SACOND - array of condition values,
C              1 - true
C              2 - false
C     SAOR   - array of or condition values
C     MAXDSN - maximum number of dataset numbers
C     NOSA   - num dsn missing attributes
C     NOMAT  - num dsn not matching
C     NOCHK  - num dsn checked
C     NOADD  - num dsn added to buffer
C     DSN    - array of dataset numbers
C     DSNCNT - count of dataset numbers in DSN
C     DSBFUL - indicator flag for full DSN array
C              0 - no
C              1 - yes
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SAMAT,I,SAORIX,SAORFG,DSADD,SAPOS,ITMP,J,K
      REAL      RTMP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV
C
C     + + + END SPECIFICATIONS + + +
C
      DSBFUL= 0
      SAORIX= 1
      IF (SACNT .NE. 0) THEN
C       check individual attributes
        SAORFG= 1
        I     = 0
 10     CONTINUE
          SAMAT= 1
 20       CONTINUE
            I= I+ 1
            SAPOS= WDSASV (SAIND(I),TIBUFF)
            IF (SAPOS .GT. 0) THEN
C             attribute present, check value
              GO TO (30,40,50), SATYP(I)
 30           CONTINUE
C               integer
                ITMP= TIBUFF(SAPOS)
                IF (ITMP.LT.SAIMIN(I).OR.ITMP.GT.SAIMAX(I)) SAMAT= 0
                GO TO 60
 40           CONTINUE
C               real
                RTMP= TRBUFF(SAPOS)
                IF (RTMP.LT.SARMIN(I).OR.RTMP.GT.SARMAX(I)) SAMAT= 0
                GO TO 60
 50           CONTINUE
C               character
                K= SABEG(I)
                J= 0
 55             CONTINUE
                  J= J+ 1
                  IF (TIBUFF(SAPOS+J-1).NE.SAVAL(K)) THEN
C                   not a match
                    SAMAT= 0
                  END IF
                  K= K+ 1
                IF (J .LT. (SALEN(I)/4) .AND. SAMAT .EQ. 1) GO TO 55
 60           CONTINUE
              IF (SACOND(I) .EQ. 2) THEN
C               use not true condition
                IF (SAMAT .EQ. 0) THEN
                  SAMAT= 1
                ELSE
                  SAMAT= 0
                END IF
              END IF
            ELSE
              SAMAT= 0
C             only count missing attribute if on last 'or'
              IF (SAOR(SAORIX) .EQ. SACNT) THEN
C               count it
                NOSA = NOSA+ 1
              END IF
            END IF
          IF (SAMAT .EQ. 1 .AND. I .LT. SAOR(SAORIX)) GO TO 20
C         maybe we will make it on an 'or' condition
          IF (SAOR(SAORIX) .GE. SACNT) THEN
C           no or
            SAORFG= 0
          ELSE
C           or case, point to correct attribute
            I= SAOR(SAORIX)
            SAORIX= SAORIX+ 1
          END IF
        IF (SAMAT .EQ. 0 .AND. SAORFG .NE. 0) GO TO 10
        IF (SAMAT.EQ.0) NOMAT= NOMAT+ 1
      ELSE
C       no search criteria, all match
        SAMAT = 1
      END IF
C
      IF (SAMAT .EQ. 1) THEN
C       add dsn to buffer if not there
        DSADD= 1
        DO 110 I= 1,DSNCNT
          IF (DSN(I).EQ.DSNTMP) THEN
C           already there
            DSADD= 0
          END IF
 110    CONTINUE
C
        IF (DSADD.EQ.1) THEN
          IF (DSNCNT .LT. MAXDSN) THEN
C           add new dataset
            DSNCNT= DSNCNT+ 1
            DSN(DSNCNT)= DSNTMP
          ELSE
C           buffer full
            DSBFUL= 1
          END IF
        ELSE
C         didnt add it
          NOADD= NOADD+ 1
        END IF
      END IF
C
      NOCHK= NOCHK+ 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDVRSN
C
C     + + + PURPOSE + + +
C     Dummy routine to include unix what version information for the
C     wdm library.
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
