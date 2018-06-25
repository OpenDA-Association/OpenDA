C
C
C
      SUBROUTINE   QCHR
     O                   (CHR,ICHR)
C
C     + + + PURPOSE + + +
C     gets a single character from a terminal
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ICHR
      CHARACTER*1 CHR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CHR    - character typed at terminal
C     ICHR   - ASCII integer code for character
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*2   I2CHAR
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR,MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    C1IN
C
C     + + + END SPECIFICATIONS + + +
C
      CALL C1IN(I2CHAR)
      ICHR= I2CHAR
C     get rid of extra bits
      ICHR= MOD(ICHR,128)
      IF (ICHR.EQ.10) ICHR= 13
      CHR= CHAR(ICHR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCCUHM
     O                    (CROW,CCOL)
C
C     + + + PURPOSE + + +
C     moves the cursor home (1,1)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CROW,CCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CROW   - cursor ending row=1
C     CCOL   - cursor ending column=1
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      CROW= 1
      CCOL= 1
C
      CALL SCCUMV(CROW,CCOL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRST
     I                     (LEN,STR)
C
C     + + + PURPOSE + + +
C     prints a string to the terminal with no cr/lf
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN
      CHARACTER*1 STR(256)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - length of string to write (characters)
C     STR    - characters to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   CRFLG,RMFLG
C
C     + + + EXTERNALS + + +
      EXTERNAL  SCPRBF
C
C     + + + END SPECIFICATIONS + + +
C
      CRFLG= 0
      RMFLG= 0
      CALL SCPRBF (LEN,RMFLG,CRFLG,STR)
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRBF
     I                    (LEN,RMFLG,CRFLG,STR)
C
C     + + + PURPOSE + + +
C     builds an output string buffer and writes it out
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(*)
C
C     + + + ARGUMENTS DEFINITIONS + + +
C     LEN    - length of string to write
C     RMFLG  - relative movement flag 0-check, 1-yes
C     CRFLG  - carriage return/line feed flag, 0-dont,
C              0 - don't
C              1 - do always
C              2 - do if something in buffer
C     STR    - string to write(or store in buffer)
C
C     + + + SAVES + + +
      INTEGER      STRPOS
      CHARACTER*1  OBUFF(255)
      SAVE         STRPOS,OBUFF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,LCRFLG
      CHARACTER*1 CTMP
C
C     + + + EXTERNALS + + +
      EXTERNAL    COPYC,SCPRBN
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA STRPOS/0/
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LEN.GT.0) THEN
C       save the new characters
        CALL COPYC (LEN,STR,OBUFF(STRPOS+1))
        STRPOS= STRPOS+ LEN
      END IF
      IF (STRPOS.GT.0.AND.CRFLG.NE.0.AND.RMFLG.EQ.0) THEN
C       look for relative cursor movement
        I    = 0
 10     CONTINUE
          I= I+ 1
          IF (ICHAR(OBUFF(I)).EQ.27) THEN
C           found esc
            I= I+ 4
            IF (I.LE.LEN) THEN
              CTMP= OBUFF(I)
              IF (CTMP.EQ.'A'.OR.CTMP.EQ.'B'.OR.
     1            CTMP.EQ.'C'.OR.CTMP.EQ.'D') THEN
C               start from where we are
                RMFLG= 1
              END IF
            END IF
          END IF
        IF (I.LT.LEN) GO TO 10
      END IF
C
      IF ((CRFLG.EQ.2.AND.STRPOS.GT.0) .OR. STRPOS.GT.128) THEN
C       time to write out the string before a character input or color change
        LCRFLG= 0
        CALL SCPRBN(STRPOS,RMFLG,LCRFLG,OBUFF)
        STRPOS= 0
      ELSE IF (CRFLG.EQ.1) THEN
C       time to write out the string
        IF (STRPOS.EQ.0) THEN
C         just need a cr/lf
          WRITE (*,2000)
        ELSE
          LCRFLG= 1
          CALL SCPRBN(STRPOS,RMFLG,LCRFLG,OBUFF)
        END IF
        STRPOS= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   COLSET
     I                    (FOR,BAC)
C
C     + + + PURPOSE + + +
C     routine to set the foreground and background colors
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FOR,BAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FOR    - color to set foreground to
C     BAC    - color to set background to
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'color.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(255)
C
C     + + + EXTERNALS + + +
      EXTERNAL    SCPRBF
C
C     + + + END SPECIFICATIONS + + +
C
C     force write of anything in buffer with old color
      LEN   = 0
      RMFLG = 2
      CRFLG = 2
      STR(1)= ' '
      CALL SCPRBF(LEN,RMFLG,CRFLG,STR)
C
C     change the colors
      FORE= FOR
      BACK= BAC
C
      RETURN
      END
C
C
C
      SUBROUTINE   COLGET
     O                    (FOR,BAC)
C
C     routine to return the current values of the fore- and background
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    FOR,BAC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FOR    - current foreground color
C     BAC    - current background color
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'color.inc'
C
      FOR= FORE
      BAC= BACK
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZBEEP
C
C     + + + PURPOSE + + +
C     ring terminal bell
C
C     + + + LOCAL VARIABLES + + +
      CHARACTER BELL*1
C
C     + + + INTRINSICS + + +
      INTRINSIC  CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZFMTWR
C
C     + + + END SPECIFICATIONS + + +
C
      BELL = CHAR(7)
      CALL ZFMTWR(BELL)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZFMTWR
     I                    (STRING)
C
C     + + + PURPOSE + + +
C     write string using a language-dependent format statement
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string to write
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I1,I2
      CHARACTER*255 LSTR
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (LSTR1,LSTR)
      CHARACTER*1  LSTR1(255)
C
C     + + + INTRINSICS + + +
      INTRINSIC   LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   SCPRBF
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I2  = 2
      LSTR= STRING
      CALL SCPRBF (LEN(STRING),I1,I2,LSTR1)
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZWRSCR
     I                    (STRING,LINE,COLUMN)
C
C     + + + PURPOSE + + +
C     write a string to specific screen position
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   LINE,COLUMN
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - character string to write
C     LINE   - starting line number to write
C     COLUMN - starting column number to write
C
C     + + + INTRINSICS + + +
      INTRINSIC  LEN
C
C     + + + EXTERNALS + + +
      EXTERNAL   ZFMTWR, SCCUMV
C
C     + + + END SPECIFICATIONS + + +
C
      CALL SCCUMV(LINE,COLUMN)
      CALL ZFMTWR(STRING(1:LEN(STRING)))
C
      RETURN
      END
