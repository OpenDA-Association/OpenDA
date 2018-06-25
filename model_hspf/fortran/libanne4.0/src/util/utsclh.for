C
C
C
      SUBROUTINE   GETKEY
     O                    (GROUP,CODE)
C
C     + + + PURPOSE + + +
C     *** SVS SPECIFIC ***
C     catch keyboard interrupt
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GROUP,CODE
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GROUP  - key group number:
C              =0 for failure
C              =1 printable character
C              =2 unprintable keys
C              =3 arrow keys
C                 CODE=1,2,3,4 for UP,DOWN,RIGHT,LEFT
C              =4 function/keypad keys
C                 CODE=0,1,2,3,4,5,6,7,8,9 for KP0-9
C                 CODE=10,11,12,13 for ENTER,".","-",","
C                 CODE=21,22,23,24 for PF1 PF2 PF3 PF4
C                 For EMIFE utility: CODE=1  HELP
C                                    CODE=2  CMDS
C                                    CODE=3  NEXT
C                                    CODE=4  BACK
C                                    CODE=5  END
C                                    CODE=6  EXIT
C     CODE   - ASCII code or function/keypad key number
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ICHR
      CHARACTER*1 DUMCHR
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    QCHR
C
C     + + + END SPECIFICATIONS + + +
C
C     get a character
      CALL QCHR(DUMCHR,ICHR)
      GROUP= 0
      CODE = 0
C
      IF ( (ICHR .GT. 0) .AND. (ICHR .LT. 256) ) THEN
C       a standard character
        CODE= ICHR
        IF (ICHR .GE. 32) THEN
C         a printing character
          GROUP= 1
        ELSE
C         non-printing character
          GROUP= 2
        END IF
      ELSE IF (ICHR .EQ. 0) THEN
C       special key - get rest
        CALL QCHR(DUMCHR,ICHR)
        IF (ICHR.LE.68 .AND. ICHR.GE.59) THEN
C         function key, f1 to f10
          GROUP= 4
          CODE = ICHR - 58
        ELSE IF (ICHR.EQ.133 .OR. ICHR.EQ.134) THEN
          GROUP= 4
          CODE = ICHR - 132
        ELSE
C         cursor movement key
          GROUP = 3
          IF (ICHR .EQ. 72) THEN
C           up arrow
            CODE = 1
          ELSE IF (ICHR .EQ. 80) THEN
C           down arrow
            CODE = 2
          ELSE IF (ICHR .EQ. 77) THEN
C           right arrow
            CODE = 3
          ELSE IF (ICHR .EQ. 75) THEN
C           left arrow
            CODE = 4
          ELSE IF (ICHR .EQ. 71) THEN
C           home
            CODE = 5
          ELSE IF (ICHR .EQ. 79) THEN
C           end
            CODE = 6
          ELSE IF (ICHR .EQ. 73) THEN
C           pg up
            CODE = 7
          ELSE IF (ICHR .EQ. 81) THEN
C           pg dn
            CODE = 8
          ELSE IF (ICHR .EQ. 83) THEN
C           del
            CODE = 9
          ELSE IF (ICHR .EQ. 82) THEN
C           ins
            CODE = 10
          ELSE
            GROUP= 0
            CODE = 0
          END IF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   SCPRBN
     I                    (LEN,RMFLG,CRFLG,STR)
C
C     + + + PURPOSE + + +
C     prints a string to the terminal
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     LEN,RMFLG,CRFLG
      CHARACTER*1 STR(255)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LEN    - len of string to write (characters)
C     RMFLG  - relative movement flag 0-no,1-yes
C     CRFLG  - carriage return flag 0-no,1-yes
C     STR    - characters to write
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'color.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,INIT,CFORE,CBACK,CSET,FCLRTB(0:15),BCLRTB(0:15)
      CHARACTER*1 CR,LF
      CHARACTER*8 CSTR
C
C     + + + SAVES + + +
      SAVE        INIT,CFORE,CBACK,FCLRTB,BCLRTB
C
C     + + + INTRINSICS + + +
      INTRINSIC   CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    FLUSH
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INIT/0/
      DATA FCLRTB/30,34,32,36,31,35,33,37,30,34,32,36,31,35,33,37/
      DATA BCLRTB/40,44,42,46,41,45,43,47,40,44,42,46,41,45,43,47/
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A1,'[',I2,';',I2,'m')
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INIT .EQ. 0) THEN
C       initialize output screen
        OPEN(UNIT=6,FILE='CON',ACCESS='TRANSPARENT',BLOCKSIZE=4096)
        INIT= 1
        CFORE= FORE
        CBACK= BACK
        CSET = 1
      ELSE IF (FORE.NE.CFORE .OR. BACK.NE.CBACK) THEN
C       change color
        CSET = 1
      ELSE
C       same color
        CSET = 0
      END IF
      IF (CSET .EQ. 1) THEN
C       change color with ansi.sys
        WRITE(CSTR,2000) CHAR(27),FCLRTB(FORE),BCLRTB(BACK)
        WRITE(6) CSTR
        CFORE= FORE
        CBACK= BACK
      END IF
      WRITE(6) (STR(I),I=1,LEN)
      IF (CRFLG .EQ. 1) THEN
        CR= CHAR (13)
        WRITE(6) CR
CPBD    these lines need to be commented for aide applications,
CPBD    uncommented for batch applications.
CPBD        LF= CHAR (10)
CPBD        WRITE(6) LF
      END IF
C
      CALL FLUSH(6)
C
      RETURN
      END
C
C
C
      SUBROUTINE   C1IN
     O                 (I2CHAR)
C
C     + + + PURPOSE + + +
C     get a single character
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2   I2CHAR
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     INIT
      CHARACTER*1 CHR
C
C     + + + SAVES + + +
      SAVE        INIT
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    EHIN, EHOUT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA INIT/0/
C
C     + + + END SPECIFICATIONS + + +
C
      IF (INIT .EQ. 0) THEN
C       initialize keyboard
        OPEN(UNIT=5,FILE='CON',ACCESS='TRANSPARENT',BLOCKSIZE=256)
C       allow control-break
        OPTION BREAK
        INIT= 1
      END IF
C
C     set up for Expert Help
      CALL EHIN
C
      READ(5) CHR
      I2CHAR= ICHAR(CHR)
C
C     reset from Expert Help
      CALL EHOUT
C
      RETURN
      END
C
C
C
      SUBROUTINE   C1INT
     O                  (I2CHAR)
C
C     + + + PURPOSE + + +
C     get a single character with no carriage return, time out if not there
C     *** lahey specific ***
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*2 I2CHAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     I2CHAR - integer*2 equivalent of keyboard response
C
C     + + + END SPECIFICATIONS + + +
C
C     otg equivalent of dg routine not yet written, return no keystroke
      I2CHAR= -1
C
      RETURN
      END
