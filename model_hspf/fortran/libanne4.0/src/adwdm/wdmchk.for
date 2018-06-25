C
C
C
      SUBROUTINE   WDMCHK
     I                    (WDMSFL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     check WDM pointer chains
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,RETCOD    
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     RETCOD - Return code
C               0 - looks ok
C             -13 - pointer count does not match available data
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cdrloc.inc'
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RREC,RIND,NDSN,PFDSN,TYP,CNT,TCNT,RETC,ODSN,DSN,ATFG
      CHARACTER*64 FNAME, WDNAME
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (I5)
 2005 FORMAT (//,' wdmchk: checking pointers in wdm file',
     $        //,'         wdmsfl =', I9,
     $         /,'         ptsnum =', I9,
     $        //,'               no. dsn',
     $         /,'         type expected   dsn pointers',
     $         /,'         ---- -------- ----- ------------------',
     $           '------------')
 2011 FORMAT (   '         ', I4, '     none' )
 2012 FORMAT (   '         ', I4, I9 )
 2015 FORMAT (   '                      ', I6, ' good' )
 2016 FORMAT (   '  -->                 ', I6, ' does not point ',
     $           'back to', I6 )
 2017 FORMAT (   '  -->                 ', I6, ' points to itself' )
 2030 FORMAT (/,' *****',
     $        /,' ***** ERROR: Discrepency between the number of ',
     $                         'expected data sets',
     $        /,' *****        and the number actually found:',
     $        /,' *****          data set type = ',I6,
     $        /,' *****               expected =', I6,
     $        /,' *****                  found =', I6,
     $        /,' *****',
     $        /,' *****        All recoverable data sets for all ',
     $                         'types will be',
     $        /,' *****        included in the wdimex input files ',
     $                         'listed below.',
     $        /,' *****' )
 2040 FORMAT (/,' *****',
     $        /,' ** ',
     $        /,' ** 1.  Be sure you have a backup of your wdm file!!!',
     $        /,' ** 2.  Run wdimex to export data sets from your wdm,',
     $        /,' **     piping the indicated records below to STDIN',
     $        /,' **     o  verify that the file name listed on line ',
     $                   '2 actually exists in the ',
     $        /,' **        indicated directory.  If not, locate the ',
     $                   'file and correct the record.',
     $        /,' **     o  be sure there is enough room for the ',
     $                   '"temp.exp" file.' )
 2041 FORMAT (  ' ** 3.  Run wdimex to import the "temp.exp" file to ',
     $        /,' **     a new wdm, piping the indicated records ',
     $        /,' **     below to STDIN (if you have a big wdm file, ',
     $                   'it will be waaayy below.',
     $        /,' **     o  this will try to create a new wdm file ',
     $                   'with the name found on line 1',
     $        /,' **        if a file of this name already exists, ',
     $                   'either back it up and delete',
     $        /,' **        it, rename it, or change the name on ',
     $                   'line 1 to something unique.',
     $        /,' *****' )
 2050 FORMAT (/,' **  Cut here for input file to export data sets ....',
     $        /,  A,
     $        /,  A,
     $        /,  'e',
     $        /,  'temp.exp',
     $        /,  ' ',
     $        /,  's' )
 2056 FORMAT (    'A' )
 2057 FORMAT (    '0',
     $        /,  'r' )
 2060 FORMAT (/,' **  ... Cut here, end of export step input',
     $        /,' **  Cut here for input file to import data sets ....',
     $        /,  A,
     $        /,  'C',
     $        /,  'N',
     $        /,  A,
     $        /,  'I',
     $        /,  'temp.exp',
     $        /,  'R'
     $        /,' **  ... Cut here, end of import step input' )
 2080 FORMAT (/,' wdmchk: finished checking pointers in wdm file,',
     $          ' no problems.' )
 2090 FORMAT (/,' ***** ',
     $        /,' ***** ERROR: A problem has been found with data ',
     $        /,' *****        set pointers in the wdm file.',
     $        /,' *****        See the ERROR.FIL file for details.',
     $        /,' ***** ' )
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDDSCK
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD = 0
      TYP    = 0
C     expected location and name of generic message file
      INCLUDE 'fmsgwd.inc'
C     checking pointers notice
      WRITE (99,2005) WDMSFL, PTSNUM
 10   CONTINUE
C       loop looking by type
        TYP  = TYP+ 1
C       bring file definition record into memory
        RREC = 1
        RIND = WDRCGO(WDMSFL,RREC)
C       calculate pointers within file definition record
        PFDSN= PTSNUM+ (TYP-1)* 2+ 1
        NDSN = WIBUFF(PFDSN,RIND)
        TCNT = WIBUFF(PTSNUM+ (TYP-1)* 2,RIND)
C
        IF (NDSN .EQ. 0) THEN
C         no data sets of this type
          WRITE (99,2011) TYP
        ELSE
C         expect tcnt data sets of this type
          WRITE (99,2012) TYP, TCNT
          ODSN= 0
          CNT = 0
 20       CONTINUE
C           loop to check datasets
            CALL WDDSCK (WDMSFL,NDSN,RREC,RETC)
            RIND= WDRCGO(WDMSFL,RREC)
            IF (NDSN .NE. WIBUFF(2,RIND)) THEN
              IF (WIBUFF(1,RIND) .EQ. ODSN) THEN
C               good chain, pointers good for ndsn
                WRITE (99,2015) NDSN
                CNT = CNT + 1
                ODSN= NDSN
                NDSN= WIBUFF(2,RIND)
              ELSE
C               dsn does not point back to ndsn
                RETCOD = -13
                WRITE (99,2016) WIBUFF(2,RIND), NDSN
              END IF
            ELSE
C             dsn points to itself
              RETCOD = -13
              WRITE (99,2017) NDSN
            END IF
          IF (CNT.LT.TCNT .AND. NDSN.GT.0 .AND. RETCOD.EQ.0) GO TO 20
          IF (CNT .NE. TCNT) THEN
C           big problem
            RETCOD = -13
            WRITE (99,2030) TYP, TCNT, CNT
C           steps to export data sets
            WRITE (99,2040)
C           steps to import data sets
            WRITE (99,2041)
C           start input for wdimex export
            INQUIRE(UNIT=WDMSFL,NAME=FNAME)
            WRITE (99,2050) FNAME, WDNAME
C           loop by dsn
            DSN = 0
            ATFG= 0
 30         CONTINUE
              DSN= DSN+ 1
              CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
              IF (RETC .EQ. 0) THEN
C               valid dsn
                WRITE(99,2000) DSN
                IF (ATFG.EQ.0) THEN
                  RIND= WDRCGO(WDMSFL,DSN)
                  IF (WIBUFF(6,RIND).LE.1) THEN 
C                   first timeseries
                    WRITE (99,2056)
                    ATFG=1
                  END IF
                END IF
              END IF
            IF (DSN .LT. 32000) GO TO 30
C           finish export input file
            WRITE (99,2057)
C           write import input file
            WRITE (99,2060) FNAME, WDNAME
          END IF
        END IF
      IF (TYP .LT. 9 .AND. RETCOD .EQ. 0) GO TO 10
C
      IF (RETCOD .EQ. 0) THEN
C       no problems
        WRITE (99,2080)
      ELSE
C       data set pointer problems, see ERROR.FIL for details
        WRITE (99,2090)
        WRITE (*,2090)
      END IF       
C
      RETURN
      END 
C
C
C
      SUBROUTINE WDATFX
     I                 (WDMSFL,OFL)
C
C     + + + PURPOSE + + +
C     fix attribute 443/444 problem
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       WDMSFL,OFL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     OFL    - Fortran unit number of output report file
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DREC,DSN,RREC,RETC,DIND,PSAVAL,
     #          PSA,SAMAX,I,PSIND,DSAIND,J
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO,WDDSCK,WDRCUP
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (  'WDATFX: wdm file has previously been checked ',
     $                  'for 443/444 attribute conflicts.' )
 2010 FORMAT (/,' ***** ',
     $        /,' ***** NOTICE:  Checking current wdm file for ',
     $                  'conflicts between attributes',
     $        /,' *****          datcre/datmod & seadbg/seadnd.',
     $        /,' ***** ',
     $        /,' *****            dsn dsaind       j    status',
     $        /,' *****          ----- ------ --------- -------' )
 2020 FORMAT (  ' *****        ', I6, I7, I10, 1X, '   ok  ' )
 2025 FORMAT (  ' *****        ', I6, I7, I10, 1X, 'updated' )
 2030 FORMAT (  ' ***** ',
     $        /,' *****          Finished checking wdm file ',
     $        /,' *****          (WDATFX)',
     $        /,' ***** ' )
C
C     + + + END SPECIFICATIONS + + +
C
C     get directory record
      DREC= 1
      DIND= WDRCGO(WDMSFL,DREC)
      IF (WIBUFF(112,DIND) .NE. 0) THEN
C       wdm has already been checked for attribute conflicts
        WRITE(OFL,2000)
      ELSE
C       begin checking for attribute conflicts
        WRITE(OFL,2010)
        DSN = 0
 10     CONTINUE
C         loop by dsn
          DSN = DSN+ 1
          CALL WDDSCK (WDMSFL,DSN,RREC,RETC)
          IF (RETC .EQ. 0) THEN
C           valid dsn, check attributes
            DIND= WDRCGO(WDMSFL,RREC)
            PSA  = WIBUFF(10,DIND)
            SAMAX= WIBUFF(PSA,DIND)
            IF (SAMAX.GT.0) THEN
C             some attributes to check
              I= 0
 30           CONTINUE
                I     = I+ 1
                PSIND = PSA+ (I*2)
                DSAIND= WIBUFF(PSIND,DIND)
                IF (DSAIND.EQ.443 .OR. DSAIND.EQ.444) THEN
C                 might be a problem
                  PSAVAL= WIBUFF(PSIND+1,DIND)
                  J= WIBUFF(PSAVAL,DIND)
                  IF (J.GE.1 .AND. J.LE.31) THEN
C                   we have a problem, fix it, new index #
                    WRITE (OFL,2025) DSN,DSAIND,J
                    WIBUFF(PSIND,DIND) = DSAIND+ 3
                    CALL WDRCUP(WDMSFL,DIND)
                  ELSE
                    WRITE (OFL,2020) DSN,DSAIND,J
                  END IF
                END IF
              IF (I.LT.SAMAX) GO TO 30
            END IF
          END IF
        IF (DSN .LT. 32000) GO TO 10
C       update directory to show fix made
        DREC= 1
        DIND= WDRCGO(WDMSFL,DREC)
        WIBUFF(112,DIND) = 1
        CALL WDRCUP(WDMSFL,DIND)
        WRITE(OFL,2030)
      END IF
C
      RETURN
      END
