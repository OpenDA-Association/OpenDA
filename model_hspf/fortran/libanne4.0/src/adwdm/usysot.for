C
C
C
      SUBROUTINE   XOSVAR
     I                   (WDNAME,
     O                    FILNAM)
C
C     + + + PURPOSE + + +
C     check for operating system variable.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64 WDNAME,FILNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDNAME - name of the WDM file
C     FILNAM - output name from operating system
C
C     + + + FUNCTIONS + + +
      INTEGER   ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZLNTXT
      EXTERNAL  OSVAR
C
C     + + + END SPECIFICATIONS + + +
C
C     call interactor routine for environment variable
      CALL OSVAR (WDNAME,
     O            FILNAM)
      IF (ZLNTXT(FILNAM).EQ.0) THEN
C       no environment variable set, try wdname
        FILNAM = WDNAME
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   XGTARG
     M                   (FNAME)
C
C     + + + PURPOSE + + +
C     get name of input file from command line
C     *** Otg specific ***
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*64  FNAME
C
C     + + + ARGUMENT DEFINTIONS + + +
C     FNAME  - Name of input file
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER*64  TNAME
C
C     + + + FUNCTIONS + + +
      CHARACTER*64  CMNAM
C
C     + + + EXTERNALS + + +
      EXTERNAL      CMNAM
C
C     + + + END SPECIFICATIONS + + +
C
C     this is an Otg extension
      TNAME = CMNAM()
C
      IF (TNAME .NE. ' ') THEN
C       use user-supplied file name
        FNAME= TNAME
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   XGTCHR
     O                   (CH1,CH2,CH3)
C
C     + + + PURPOSE + + +
C     get characters for screen display
C     pc specific
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*1   CH1,CH2,CH3
C
C     + + + ARGUMENT DEFINTIONS + + +
C     CH1    - character for hspf screen output
C     CH2    - character for hspf screen output
C     CH3    - character for hspf screen output
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      CH1= CHAR(176)
C      CH2= CHAR(219)
      CH2= ' '
      CH3= CHAR(205)
C
      RETURN
      END
C
C
C
      SUBROUTINE   XCLRSC
C
C     + + + PURPOSE + + +
C     clear screen for hspf run
C     pc specific
C
C     + + + OUTPUT FORMATS + + +
 2040 FORMAT(1X,'[2J')
C
C     + + + END SPECIFICATIONS + + +
C
      WRITE (*,2040)
C
      RETURN
      END
C
C
C
      SUBROUTINE   XGTRCL
     I                   (BASE,
     O                    RCL)
C
C     + + + PURPOSE + + +
C     get record length
C     pc specific
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BASE,RCL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BASE - base number of record length
C     RCL  - total record length
C
C     + + + END SPECIFICATIONS + + +
C
      RCL = 4*BASE
C
      RETURN
      END
C
C
C
      SUBROUTINE   XPLFRM
     O                   (FRM)
C
C     + + + PURPOSE + + +
C     get frm for pltgen
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*30   FRM
C
C     + + + ARGUMENT DEFINTIONS + + +
C     FRM    - format needed for pltgen
C
C     + + + END SPECIFICATIONS + + +
C
      FRM= 'FORMATTED'
C
      RETURN
      END
