C
C
C
      SUBROUTINE   WDIINI
C
C     + + + PURPOSE + + +
C     initialize CWDMID common block
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      WIDCNT   = 0
      WIDBSE(1)= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WIDADD
     I                   (WDMSFL,MXDSN,WDID)
C
C     + + + PURPOSE + + +
C     save information about a wdm file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,MXDSN
      CHARACTER*4 WDID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - unit number of new wdm file
C     MXDSN  - maximum allowed dataset number in new file
C     WDID   - identifier of new wdm file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   IPOS,I
C
C     + + + END SPECIFICATIONS + + +
C
      IPOS = 0
      IF (WIDCNT.GT.0) THEN
C       first make sure this label hasnt already been assigned
        DO 10 I = 1,WIDCNT
          IF (WIDNAM(I).EQ.WDID) THEN
C           already assigned
            IPOS = I
          END IF
 10     CONTINUE
      END IF
C
      IF (IPOS.EQ.0) THEN
C       go ahead and add new one
        WIDCNT        = WIDCNT+ 1
        WIDFUN(WIDCNT)= WDMSFL
        WIDNAM(WIDCNT)= WDID
        IF (WIDCNT .LT. MXWDID) THEN
          WIDBSE(WIDCNT+1)= WIDBSE(WIDCNT) + MXDSN
        END IF
      ELSE
C       just update info for this wdm id
        WIDFUN(IPOS)= WDMSFL
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WUD2ID
     I                   (WDMSFL,DSN,
     O                    ID)
C
C     + + + PURPOSE + + +
C     convert a wdm unit number and dsn to an id
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,ID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - unit number of wdm file
C     DSN    - dataset number
C     ID     - id for unit number and dsn
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*4  WDID
C
C     + + + EXTERNALS + + +
      EXTERNAL     WUA2ID
C
C     + + + END SPECIFICATIONS + + +
C
      WDID = '    '
      CALL WUA2ID (WDMSFL,DSN,WDID,
     O             ID)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WUA2ID
     I                   (WDMSFL,DSN,WDID,
     O                    ID)
C
C     + + + PURPOSE + + +
C     convert a wdm unit number or 4 char id and dsn to an id
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,ID
      CHARACTER*4 WDID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - unit number of wdm file
C     DSN    - dataset number
C     ID     - id for unit number and dsn
C     WDID   - wdm file identifier
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (WDID.EQ.'    ') THEN
C       calculate id based on wdm file unit number
        I = 0
        ID= 0
 10     CONTINUE
          I = I+ 1
          IF (WDMSFL .EQ. WIDFUN(I)) THEN
            ID = WIDBSE(I)+ DSN
          END IF
        IF (ID.EQ.0 .AND. I.LT.WIDCNT) GO TO 10
      ELSE
C       calculate id based on 4 character identifier
        I = 0
        ID= 0
 20     CONTINUE
          I = I+ 1
          IF (WDID .EQ. WIDNAM(I)) THEN
            ID = WIDBSE(I)+ DSN
          END IF
        IF (ID.EQ.0 .AND. I.LT.WIDCNT) GO TO 20
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WID2UD
     I                   (WDFLG,ID,
     O                    WDMSFL,DSN)
C
C     + + + PURPOSE + + +
C     convert an id to a wdm unit number and dsn
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDFLG,ID,WDMSFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDFLG  - wdm unit number flag, > 0 means id is actual dsn
C     ID     - id for unit number and dsn
C     WDMSFL - unit number of wdm file
C     DSN    - dataset number
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*4 WDID
C
C     + + + EXTERNALS + + +
      EXTERNAL    WID2UA
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WID2UA (WDFLG,ID,
     O             WDMSFL,DSN,WDID)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WID2UA
     I                   (WDFLG,ID,
     O                    WDMSFL,DSN,WDID)
C
C     + + + PURPOSE + + +
C     convert an id to a wdm unit number and dsn and return text id
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDFLG,ID,WDMSFL,DSN
      CHARACTER*4 WDID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDFLG  - wdm unit number flag, > 0 means id is actual dsn
C     ID     - id for unit number and dsn
C     WDMSFL - unit number of wdm file
C     DSN    - dataset number
C     WDID   - character identifier for wdm file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      IF (WDFLG .GT. 0) THEN
C       not using this scheme
        WDMSFL= WDFLG
        DSN   = ID
        WDID  = '    '
      ELSE
C       figure out what id
        I  = 0
        J  = 0
 10     CONTINUE
          I = I+ 1
          IF (I .LT. WIDCNT) THEN
            IF (ID .LT. WIDBSE(I+1)) THEN
              J= 1
            END IF
          ELSE
            J= 1
          END IF
          IF (J .EQ. 1) THEN
            WDMSFL= WIDFUN(I)
            DSN   = ID - WIDBSE(I)
            WDID  = WIDNAM(I)
          END IF
        IF (J .EQ. 0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WCH2UD
     I                   (WDID,
     O                    WDMSFL)
C
C     + + + PURPOSE + + +
C     given a 4 character wdm file id, find the wdm file unit number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL
      CHARACTER*4 WDID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - unit number of wdm file
C     WDID   - character identifier for wdm file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE   'cwdmid.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      I  = 0
      J  = 0
 10   CONTINUE
        I = I+ 1
        IF (WDID.EQ.WIDNAM(I)) THEN
C         this is the wdm id
          WDMSFL= WIDFUN(I)
          J= 1
        END IF
      IF (J.EQ.0 .AND. I.LT.MXWDID) GO TO 10
C
      RETURN
      END
