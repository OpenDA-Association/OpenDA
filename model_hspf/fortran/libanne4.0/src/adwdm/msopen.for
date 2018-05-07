C
C
C
      SUBROUTINE   WMSGOP
     I                   ( WDMSFL,
     O                     RETCOD )
C
C     + + + PURPOSE + + +
C     Opens the message file containing the standard set of clusters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL, RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of the WDM file
C     RETCOD - return code
C               0 - successful open
C               1 - successful open, but invalid WDM file
C              <0 - error on open, -IOSTAT, compiler specific
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      RONWFG
      CHARACTER*64 WDNAME
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDBOPN
C
C     + + + END SPECIFICATIONS + + +
C
C     open generic message file containing standard clusters
      INCLUDE 'fmsgwd.inc'
      RONWFG = 1
      CALL WDBOPN ( WDMSFL, WDNAME, RONWFG,
     O              RETCOD )
C
      RETURN
      END
