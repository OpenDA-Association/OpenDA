C
C
C
      SUBROUTINE   EHIN
C
C     + + + PURPOSE + + +
C     *****   Specific For OTG Compiler   *****
C     Write current field id to hidden place on screen
C     so that if Expert Help is invoked, the user will be
C     placed at a context-sensitive position within the help.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IROW,ICOL,LEN,RMFLG,CRFLG
      CHARACTER*30 SCNAM
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (SCNAM1,SCNAM)
      CHARACTER*1  SCNAM1(30)
C
C     + + + EXTERNALS + + +
      EXTERNAL     SCCUMV, SCPRBN, EHSCNM
C
C     + + + END SPECIFICATIONS + + +
C
      IROW = 26
      ICOL = 1
      LEN  = 30
      RMFLG= 0
      CRFLG= 0
      CALL EHSCNM (SCNAM)
      CALL SCCUMV (IROW,ICOL)
      CALL SCPRBN (LEN,RMFLG,CRFLG,SCNAM1)
      CALL SCCUMV (IROW,ICOL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   EHOUT
C
C     + + + PURPOSE + + +
C     Return cursor to current AIDE position after
C     setting to capture for Expert Help (EHIN).
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SCCUMV (ZCRLIN,ZCRCOL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   EHSCNM
     O                   (SCNAME)
C
C     + + + PURPOSE + + +
C     Build screen name for auto-lookup in Expert Help.
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*30 SCNAME
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SCNAME - screen name to look up
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cscren.inc'
      INCLUDE 'cqrsp.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      ILEN,IPOS,IP2,PROBFG,WARNFG
      CHARACTER*7  CHWARN,CHPROB
      CHARACTER*78 TSTR
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + INTRINSICS + + +
      INTRINSIC    INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CHWARN,CHPROB/'Warning','Problem'/
C
C     + + + END SPECIFICATIONS + + +
C
      WARNFG= 0
      PROBFG= 0
C
      IF (ZLNTXT(ZSCNAM) .GT. 0) THEN
C       screen has a name
        IF (INDEX(ZSCNAM,CHWARN).GT.0) THEN
C         'Warning' screen, set flag
          WARNFG= 1
        ELSE IF (INDEX(ZSCNAM,CHPROB).GT.0) THEN
C         'Problem' screen, set flag
          PROBFG= 1
        END IF
C
        IF (ZDTYP.EQ.1) THEN
C         text screen
          ILEN= INDEX(ZSCNAM,' ') - 1
          IF (WARNFG.EQ.1) THEN
            TSTR= ZSCNAM(1:ILEN)//'_'//CHWARN
            ILEN= ILEN+ 8
          ELSE IF (PROBFG.EQ.1) THEN
            TSTR= ZSCNAM(1:ILEN)//'_'//CHPROB
            ILEN= ILEN+ 8
          ELSE
            TSTR= ZSCNAM(1:ILEN)
          END IF
          IPOS= INDEX(ZSCNAM,'(')
          IF (IPOS.GT.0) THEN
C           extract path out of AIDE screen name
            IP2= INDEX(ZSCNAM,')')
            SCNAME= TSTR(1:ILEN)//'_'//ZSCNAM(IPOS+1:IP2-1)//'_Result'
          ELSE
C           just use field name
            SCNAME= TSTR(1:ILEN)//'_Result'
          END IF
        ELSE IF (ZDTYP.EQ.2) THEN
C         menu screen, get current option
          TSTR= ZMNTXT(ZHLLIN)(ZHLCOL:78)
          CALL ZLJUST(TSTR)
          IPOS= INDEX(TSTR,'-')
          IF (IPOS.GT.0) THEN
C           only use portion of menu option up to '-'
            ILEN= ZLNTXT(TSTR(1:IPOS-1))
          ELSE
C           only use portion of menu option up to 1st blank
            ILEN= INDEX(TSTR,' ') - 1
          END IF
          IPOS= INDEX(ZSCNAM,'(')
          IF (RSINIT.EQ.1 .AND. IPOS.GT.0) THEN
C           options being set on the fly, only use screen name
            SCNAME= ZSCNAM(1:IPOS-1)
          ELSE IF (IPOS.GT.0) THEN
C           extract path out of AIDE screen name
            IP2= INDEX(ZSCNAM,')')
            SCNAME= TSTR(1:ILEN)//'_'//ZSCNAM(IPOS+1:IP2-1)
          ELSE
C           just use menu option name
            SCNAME= TSTR(1:ILEN)
          END IF
        ELSE IF (ZDTYP.GE.3) THEN
C         data screen
          ILEN= INDEX(ZSCNAM,' ') - 1
          TSTR= ZSCNAM(1:ILEN)
          IPOS= INDEX(ZSCNAM,'(')
          IF (IPOS.GT.0) THEN
C           extract path out of AIDE screen name
            IP2= INDEX(ZSCNAM,')')
            SCNAME= TSTR(1:ILEN)//'_'//ZSCNAM(IPOS+1:IP2-1)//
     $              '_'//FDFMT(CFLD)
          ELSE
C           just use field name
            SCNAME= TSTR(1:ILEN)//'_'//FDFMT(CFLD)
          END IF
        END IF
      ELSE
C       screen has no name
        SCNAME= ' '
      END IF
C
      RETURN
      END
