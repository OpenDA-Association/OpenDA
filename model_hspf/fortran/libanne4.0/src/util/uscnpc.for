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
      INTEGER*2 ULR,ULC,LLR,LRC
C
C     + + + FUNCTIONS + + +
      EXTERNAL  CLS, POSCUR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA ULR,ULC,LLR,LRC/0,0,24,79/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL CLS (ULR,ULC,LLR,LRC)
      CALL POSCUR (ULR,ULC)
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
      EXTERNAL  CLS, GETCUR, POSCUR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  LRR,LRC/24,79/
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GETCUR (ULR,ULC)
      ULC= 0
      CALL CLS (ULR,ULC,LRR,LRC)
      CALL POSCUR (ULR,ULC)
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
      EXTERNAL   POSCUR
C
C     + + + END SPECIFICATIONS + + +
C
      ROW = CROW- 1
      COL = CCOL- 1
C     IF (COL.EQ.0) THEN
C       COL= 79
C       ROW= ROW- 1
C     END IF
      CALL POSCUR(ROW,COL)
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
      EXTERNAL   GETCUR, POSCUR
C
C     + + + END SPECIFICATIONS + + +
C
      CALL GETCUR(PROW,PCOL)
C
      PROW= PROW+ CROW
      IF (PROW.GT.23) PROW= 23
      IF (PROW.LT.0)  PROW= 0
C
      PCOL= PCOL+ CCOL
      IF (PCOL.GT.79) PCOL= 79
      IF (PCOL.LT.0)  PCOL= 0
C
      CALL POSCUR(PROW,PCOL)
C
      RETURN
      END

