C
C
C
      SUBROUTINE   PATCH1
C
C     + + + PURPOSE + + +
C     Fix attribute SEADBG and SEADND conflict
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      WDMSFL,RDOFLG,RETCOD,ATMSFL,DSAIND,L,SATYP,SALEN
      CHARACTER*64 WDNAME
      CHARACTER*1  BUFF(16)
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDBOPN, WDATFX, WDFLCL, WDSAGY, WTGPSU
      EXTERNAL     LENSTR
C
C     + + + INPUT FORMATS + + +
1000  FORMAT(A64)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( ' ***** ',
     $       /,' ***** Problem with the supplied message wdm ',
     $       /,' ***** file ', A,
     $       /,' ***** ',
     $       /,' ***** file must be at least indicated version:',
     $       /,' *****      message.wdm - libanne4.0',
     $       /,' *****      hspfmsg.wdm - hsplib11.1',
     $       /,' *****      swstms.wdm - swstat3.3',
     $       /,' ***** ' )
 2010 FORMAT ( ' Problem finding a wdm file that defines ',
     $           'attributes.',
     $       /,' Look for hspfmsg.wdm or message.wdm ',
     $           'and enter the full path and file name',
     $       /,' (carriage return to exit this option):' )
 2020 FORMAT ( ' Unable to open a required message wdm file.' )
C
C     + + + END SPECIFICATIONS + + +
C
C     open error logging file
      OPEN(UNIT=99,FILE='error.fil')
C
      ATMSFL= 90
      RDOFLG= 1
 10   CONTINUE
C       look for message wdm file and try to open it
        INCLUDE 'fmsgwd.inc'
        CALL WDBOPN (ATMSFL,WDNAME,RDOFLG, RETCOD)
        IF (RETCOD .NE. 0) THEN
C         problem finding message file, ask user for a file
          WRITE (*,2010)
          READ (*,1000) WDNAME
          IF (LENSTR(64,WDNAME) .GT. 0) THEN
C           try user supplied message wdm
            CALL WDBOPN (ATMSFL,WDNAME,RDOFLG, RETCOD)
          ELSE
C           no wdm file found
            WRITE (*,2020)
            STOP
          END IF
        END IF
        IF (RETCOD .EQ. 0) THEN
C         check that 443 in supplied message file is character
C         attribute datcre and not integer attribute seadbg.
          DSAIND= 443
          CALL WDSAGY (ATMSFL,DSAIND,
     O                 BUFF,L,SATYP,SALEN,L,L)
          IF (SATYP.NE.3) THEN
            WRITE(*,2000) BUFF,SATYP,SALEN
            STOP
          END IF
        END IF
      IF (RETCOD.NE.0) GOTO 10
C
C     get name of user's wdm file
      WRITE(*,*) ' Enter name of your WDM file: '
      READ (*,1000) WDNAME
C     open the WDM file
      WDMSFL= 91
      RDOFLG= 0
      CALL WDBOPN (WDMSFL,WDNAME,RDOFLG,
     O             RETCOD)
      IF (RETCOD.EQ.0) THEN
C       open ok, check for and fix attribute 443/444 conflict
        CALL WDATFX(WDMSFL,99)
C       see how timeseries pointers are doing
        CALL WTGPSU(WDMSFL,99)
C       this will check bad pointer problem
        CALL WDFLCL(WDMSFL)
      ELSE
        WRITE(*,*) ' Error opening WDM file, RETCOD = ',RETCOD
      END IF
C
      RETURN
      END 
