C
C
C
      SUBROUTINE   SCCLAL
C
C     + + + PURPOSE + + +
C     homes the cursor and clears the screen
C     *** PC SPECIFIC, REQUIRES ASSEMBLER ROUTINE CLS,POSCUR  ***
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2 ULR,ULC
C
C     + + + EXTERNALS + + +
      EXTERNAL  CLEAR_SCREEN@,SET_CURSOR_POS@
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ULR,ULC/0,0/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL CLEAR_SCREEN@
      CALL SET_CURSOR_POS@ (ULC,ULR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCLR
C
C     + + + PURPOSE + + +
C     clears the screen
C     *** PC SPECIFIC, REQUIRES ASSEMBLER ROUTINES GETCUR,CLS,POSCUR ***
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2 ULR,ULC,LRR,LRC
C
C     + + + EXTERNALS + + +
      EXTERNAL  GET_CURSOR_POS@, SET_CURSOR_POS@
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  LRR,LRC/24,79/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GET_CURSOR_POS@(ULC,ULR)
      ULC= 0
C     *** whats this routine for otg ****
C     CALL CLS (ULR,ULC,LRR,LRC)
C     *** ??? ****
      CALL SET_CURSOR_POS@(ULC,ULR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCUMV
     I                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves the cursor to the absolute row and column specified
C     *** PC SPECIFIC, REQUIRES ASSEMBLER ROUTINE POSCUR ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - cursor row to go to
C     CCOL   - cursor column to go to
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2  ROW,COL
C
C     + + + EXTERNALS + + +
      EXTERNAL   SET_CURSOR_POS@
C
C     + + + END SPECIFICATIONS + + +
C
      ROW = CROW- 1
      COL = CCOL- 1
C     IF (COL.EQ.0) THEN
C       COL= 79
C       ROW= ROW- 1
C     END IF
      CALL SET_CURSOR_POS@(COL,ROW)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCURM
     I                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves the cursor to the relative row and column specified
C     *** PC SPECIFIC, REQUIRES ASSEMBLER ROUTINES GETCUR, POSCUR ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - number of rows to move
C     CCOL   - number of columns to move
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2  PROW,PCOL
C
C     + + + EXTERNALS + + +
      EXTERNAL   GET_CURSOR_POS@, SET_CURSOR_POS@
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GET_CURSOR_POS@(PCOL,PROW)
C
      PROW= PROW+ CROW
      IF (PROW.GT.23) PROW= 23
      IF (PROW.LT.0)  PROW= 0
C
      PCOL= PCOL+ CCOL
      IF (PCOL.GT.79) PCOL= 79
      IF (PCOL.LT.0)  PCOL= 0
C
      CALL SET_CURSOR_POS@(PCOL,PROW)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PCGRST
     I                   (TPAGE,TMODE)
C
C     + + + PURPOSE + + +
C     Dummy version of routine to set the screen page/mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TPAGE,TMODE
C
C     + + + EXTERNALS + + +
      EXTERNAL   TEXT_MODE@
C
C     + + + END SPECIFICATIONS + + +
C
      CALL TEXT_MODE@
C
      RETURN
      END
C
C
C
      SUBROUTINE   PCGRGT
     I                   (TPAGE,TMODE)
C
C     + + + PURPOSE + + +
C     Dummy version of routine to get the screen page/mode.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TPAGE,TMODE
C
C     + + + END SPECIFICATIONS + + +
C
      RETURN
      END
