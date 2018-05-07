C
C
C
      SUBROUTINE   WDBOPN
     I                    (WDMSFL,WDNAME,RONWFG,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Open a WDM file.  File is opened as new or old, depending on
C     the value of RONWFG.  The common block related to the WDM record
C     buffer are initialized the first time this routine is called.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,RONWFG,RETCOD
      CHARACTER*64 WDNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     WDNAME - name of the WDM file
C     RONWFG - read only/new file flag
C              0- normal open of existing WDM file,
C              1- open WDM file as read only (system dependent),
C              2- open new WDM file
C     RETCOD - return code
C               0 - successful open
C               1 - successful open, but invalid WDM file
C              <0 - error on open, -IOSTAT, compiler specific
C
C     + + + SAVES + + +
      INTEGER   INITFG
      SAVE INITFG
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 IOS
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBFIN, WDFLCK, WDCREA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INITFG/0/
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C     ****** NOTE: THE FOLLOWING OPEN STATEMENTS MAY BE SYSTEM SPECIFIC ******
      IF (RONWFG.EQ.1) THEN
C       open file as 'read only'
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='OLD',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=2048,
     2        ERR=10,IOSTAT=IOS)
      ELSE IF (RONWFG.EQ.2) THEN
C       open new wdm file
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='NEW',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=2048,
     2        ERR=10,IOSTAT=IOS)
      ELSE
C       open file w/out 'read only'
        OPEN (UNIT=WDMSFL,FILE=WDNAME,STATUS='OLD',
     1        ACCESS='DIRECT',FORM='UNFORMATTED',RECL=2048,
     2        ERR=10,IOSTAT=IOS)
      END IF
C     WDM file opened successfully
      IF (INITFG.EQ.0) THEN
C       first time called, initialize WDM record buffer
        CALL WDBFIN
        INITFG= 1
      END IF
      IF (RONWFG.EQ.2) THEN
C       new file, need to initialize it
        CALL WDCREA (WDMSFL)
      END IF
      IF (RETCOD.EQ.0) THEN
C       check WDM directory records
        CALL WDFLCK (WDMSFL,
     O               RETCOD)
      END IF
      GO TO 20
 10   CONTINUE
C       error on open, exact value of retcod may vary by system,
C       set it to a negative value for consistancy
        RETCOD= IOS
        IF (RETCOD.GT.0) RETCOD= -RETCOD
        IF (RETCOD.EQ.0) RETCOD= -1
 20   CONTINUE
C
      RETURN
      END
