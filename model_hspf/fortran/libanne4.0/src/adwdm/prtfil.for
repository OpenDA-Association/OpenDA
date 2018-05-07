C
C
C
      SUBROUTINE   PMXTFI
     I                   ( UMESFL, FUNIT, SCLU, SGRP, INUM, IVAL )
C
C     + + + PURPOSE + + +
C     This routine prints text form a message file to an output file.
C     It fills in values from IVAL whenever a delimeter is found.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL, FUNIT, SCLU, SGRP, INUM, IVAL(INUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of message file containing text
C     FUNIT  - Fortran unit number of output file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     INUM   - number of integers to include in message
C     IVAL   - array containing integers to pe included in text
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ONUM, OTYP(50), CLEN(1)
      REAL      RVAL(1)
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1 CVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXF
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   OTYP,  RVAL,  DVAL, CVAL, CLEN
     #     / 50*1, -999., -999.,  ' ',    1 /
C
C     + + + END SPECIFICATIONS + + +
C
      ONUM = INUM
      CALL PMXTXF ( UMESFL, FUNIT, SCLU, SGRP, ONUM, OTYP,
     I              IVAL, RVAL, DVAL, CLEN, CVAL )
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTFR
     I                   ( UMESFL, FUNIT, SCLU, SGRP, RNUM, RVAL )
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to an output file.
C     It fills in values from RVAL whenever a delimeter is found.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL, FUNIT, SCLU, SGRP, RNUM
      REAL      RVAL(RNUM)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of message file containing text
C     FUNIT  - Fortran unit number of output file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     RNUM   - number of reals to include in message
C     RVAL   - array containing reals to be included in text
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ONUM, OTYP(50), CLEN(1), IVAL(1)
      DOUBLE PRECISION DVAL(1)
      CHARACTER*1 CVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXF
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   OTYP, IVAL,  DVAL, CVAL, CLEN
     #     / 50*2,  999, -999.,  ' ',    1 /
C
C     + + + END SPECIFICATIONS + + +
C
      ONUM = RNUM
      CALL PMXTXF ( UMESFL, FUNIT, SCLU, SGRP, ONUM, OTYP,
     I              IVAL, RVAL, DVAL, CLEN, CVAL )
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTFC
     I                   ( UMESFL, FUNIT, SCLU, SGRP, CNUM, CLEN, CVAL )
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to an output file.
C     It fills in values from CVAL whenever a delimeter is found.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL, FUNIT, SCLU, SGRP, CNUM, CLEN(CNUM)
      CHARACTER*1 CVAL(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of message file containing text
C     FUNIT  - Fortran unit number of output file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     CNUM   - number of characters to include in message
C     CLEN   - array of lengths of character strings
C     CVAL   - array containing characters to be included in text
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   ONUM, OTYP(50), IVAL(1)
      REAL      RVAL(1)
      DOUBLE PRECISION DVAL(1)
C
C     + + + EXTERNALS + + +
      EXTERNAL   PMXTXF
C
C     + + + DATA INITIALIZATIONS + + +
      DATA   OTYP, IVAL,  RVAL,  DVAL
     #     / 50*4,  999, -999., -999. /
C
C     + + + END SPECIFICATIONS + + +
C
      ONUM = CNUM
      CALL PMXTXF ( UMESFL, FUNIT, SCLU, SGRP, ONUM, OTYP,
     I              IVAL, RVAL, DVAL, CLEN, CVAL )
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTXF
     I                   ( UMESFL, FUNIT, SCLU, SGRP,
     I                     ONUM, OTYP, IVAL, RVAL, DVAL, CLEN, CVAL )
C
C     + + + PURPOSE + + +
C     This routine prints text from a message file to a user file.  It
C     fills in values from IVAL, RVAL, DVAL, and CVAL arrays whenever
C     a delimeter is found.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   UMESFL, FUNIT, SCLU, SGRP, ONUM, OTYP(ONUM),
     #          IVAL(*), CLEN(*)
      REAL      RVAL(*)
      DOUBLE PRECISION DVAL(*)
      CHARACTER*1 CVAL(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     UMESFL - Fortran unit number of user's message file
C     FUNIT  - Fortran unit number of output file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C     ONUM   - number of variables to be added to the text
C     OTYP   - array of types for variables to be added to text
C     IVAL   - array containing integer values to include
C     RVAL   - array containing real values to include
C     DVAL   - array containing double precision values to include
C     CLEN   - array of lengths of character strings to include
C     CVAL   - array of character strings to include
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I150,OPOS,DONFG,CONT,INITFG,OLEN,
     >          ICNT,RCNT,DCNT,CCNT,OCNT,LEN,I,J
      CHARACTER*1 DELIM(1), TXT(150)
C
C     + + + FUNCTIONS + + +
      INTEGER   LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL  LENSTR, TXMULT, PRTSTR, WMSGTT, SCPRBF, COLSET
C
C     + + + END SPECIFICATIONS + + +
C
      I150 = 150
      OCNT = 1
      OPOS = 1
      DONFG= 0
      ICNT = 1
      RCNT = 1
      DCNT = 1
      CCNT = 1
      DELIM(1)= '&'
C
      INITFG= 1
 10   CONTINUE
C       get next record of text
        OLEN= I150
        CALL WMSGTT (UMESFL,SCLU,SGRP,INITFG,
     M               OLEN,
     O               TXT,CONT)
        INITFG= 0
C       replace delimeters with values to be printed
        CALL TXMULT (ONUM,OTYP,IVAL,RVAL,DVAL,CLEN,CVAL,DELIM,I150,
     M               OCNT,ICNT,RCNT,DCNT,CCNT,OPOS,TXT,CONT,
     O               DONFG)
C
C       print this part of the text
        LEN = LENSTR (I150,TXT)
        IF (LEN.EQ.0) LEN= 1
        IF (FUNIT.EQ.-1) THEN
C         write to standard output
          I = 7
          J = 0
          CALL COLSET (I,J)
          I = 0
          J = 1
          CALL SCPRBF (LEN,I,J,TXT)
        ELSE
C         write to file
          CALL PRTSTR (FUNIT,LEN,TXT)
        END IF
      IF (CONT.EQ.1) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PMXTFT
     I                   ( MESSFL, FUNIT, SCLU, SGRP )
C
C     + + + PURPOSE + + +
C     Print text from a message file to an output file.  Maximum
C     length of a line is 150 characters.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL, FUNIT, SCLU, SGRP
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of message file containing text
C     FUNIT  - Fortran unit number of output file
C     SCLU   - screen cluster number on WDM-message file
C     SGRP   - screen group number in cluster
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     INIT, LEN, CONT, I, J
      CHARACTER*1 BUFF(150)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTT,SCPRBF,COLSET
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( 150A1 )
 2001 FORMAT ( ' ' )
C
C     + + + END SPECIFICATIONS + + +
C
      INIT = 1
  100 CONTINUE
C       get next line of text
        LEN = 150
        CALL WMSGTT ( MESSFL, SCLU, SGRP, INIT,
     M                LEN,
     O                BUFF, CONT )
        INIT = 0
        IF (FUNIT.EQ.-1) THEN
C         write to standard output
          I = 7
          J = 0
          CALL COLSET (I,J)
          I = 0
          J = 1
          CALL SCPRBF (LEN,I,J,BUFF)
        ELSE
C         write to file
          IF (LEN .GT. 0) THEN
C           write text
            WRITE (FUNIT,2000) (BUFF(I), I = 1, LEN)
          ELSE
C           blank line
            WRITE (FUNIT,2001)
          END IF
        END IF
      IF (CONT .EQ. 1) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   TXMULT
     I                   (ONUM, OTYP, IVAL, RVAL, DVAL, CLEN, CVAL,
     I                    DELIM, ILEN,
     M                    OCNT, ICNT, RCNT, DCNT, CCNT, OPOS, TXT, CONT,
     O                    DONFG )
C
C     + + + PURPOSE + + +
C     This routine fills in values from the IVAL, RVAL, DVAL, and
C     CVAL arrays whenever a delimeter is found in TXT.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ONUM, OTYP(ONUM), IVAL(*), CLEN(*), ILEN,
     #          OCNT, ICNT, RCNT, DCNT, CCNT, OPOS, CONT, DONFG
      REAL      RVAL(*)
      DOUBLE PRECISION DVAL(*)
      CHARACTER*1 DELIM(1), TXT(ILEN), CVAL(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ONUM   - number of variables to be output in text buffer
C     OTYP   - array of variable types
C     IVAL   - array of integer variables
C     RVAL   - array of real variables
C     DVAL   - array of double precision variables
C     CLEN   - array of character variable lengths
C     CVAL   - array of character variables
C     DELIM  - character indicating where variable are to be placed in text
C     OCNT   - current count of variables output in text buffer
C     ICNT   - current count of integer variables output
C     RCNT   - current count of real variables output
C     DCNT   - current count of double precision variables output
C     CCNT   - current count of character variables output
C     OPOS   - position within character array of next character variable
C     TXT    - current text buffer line
C     CONT   - continue outputing text flag,
C              0 - do not continue, 1 - continue
C     DONFG  - flag indicating all variables put in text,
C              0 - not all variables in text, 1 - all variables in text
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   K, I1, JX, OLEN, JUST, J, IPOS
      REAL      RTMP
      DOUBLE PRECISION DTMP
      CHARACTER*1 CTMP(150), CNONE(6), BLNK(1)
C
C     + + + FUNCTIONS + + +
      INTEGER   STRFND
C
C     + + + INTRINSICS + + +
      INTRINSIC   ABS
C
C     + + + EXTERNALS + + +
      EXTERNAL   STRFND, INTCHR, DECCHR, DPRCHR, CHRCHR, CHRDEL, CHRINS
      EXTERNAL   ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA  I1, BLNK, CNONE
     #    /  1,  ' ', '(','n','o','n','e',')' /
C
C     + + + END SPECIFICATIONS + + +
C
      DONFG = 0
 20   CONTINUE
        K= STRFND(ILEN,TXT,I1,DELIM(1))
        IF (K.GT.0) THEN
C         found delimeter
          IF (DONFG.EQ.0) THEN
C           replace with next value
            IF (OTYP(OCNT).EQ.1) THEN
C             integer value
              IF (IVAL(ICNT).NE.-999) THEN
C               put number to write into buffer
                OLEN= 14
                JUST= 1
                CALL INTCHR (IVAL(ICNT),OLEN,JUST,
     O                       J,CTMP)
              ELSE
C               put none into buffer
                J= 6
                CALL CHRCHR (J,CNONE,CTMP)
              END IF
              ICNT= ICNT+ 1
            ELSE IF (OTYP(OCNT).EQ.2) THEN
C             real value
              RTMP= ABS(RVAL(RCNT)+999)
              IF (RTMP.GT.1.0E-5) THEN
C               put number to write into buffer
                OLEN= 14
                JUST= 1
                CALL DECCHR (RVAL(RCNT),OLEN,JUST,
     O                       J,CTMP)
              ELSE
C               put none into buffer
                J= 6
                CALL CHRCHR (J,CNONE,CTMP)
              END IF
              RCNT= RCNT+ 1
            ELSE IF (OTYP(OCNT).EQ.3) THEN
C             double precision value
              DTMP= ABS(DVAL(DCNT)+999)
              IF (DTMP.GT.1.0E-5) THEN
C               put number to write into buffer
                OLEN= 14
                JUST= 1
                CALL DPRCHR (DVAL(DCNT),OLEN,JUST,
     O                       J,CTMP)
              ELSE
C               put none into buffer
                J= 6
                CALL CHRCHR (J,CNONE,CTMP)
              END IF
              DCNT= DCNT+ 1
            ELSE IF (OTYP(OCNT).EQ.4) THEN
C             character value
              J= CLEN(CCNT)
              CALL CHRCHR (J,CVAL(OPOS),CTMP)
              OPOS= OPOS+ CLEN(CCNT)
              CCNT= CCNT+ 1
            END IF
            CALL CHRDEL (ILEN,K,TXT(1))
            DO 30 JX= 1,J
              IPOS= K+ JX- 1
              IF (IPOS.LE.ILEN) THEN
C               there's room to insert character
                CALL CHRINS (ILEN,IPOS,CTMP(JX),TXT)
              END IF
 30         CONTINUE
            OCNT= OCNT+ 1
            IF (OCNT.GT.ONUM) THEN
              DONFG= 1
            END IF
          ELSE
C           delimeter found, but no more values, clear rest of line
            CALL ZIPC (ILEN-K+1,BLNK,TXT(K))
            IF (K.EQ.1) THEN
C             stop printing
              CONT= 0
            END IF
          END IF
        END IF
      IF (DONFG.EQ.0.AND.K.GT.0) GO TO 20
C
      RETURN
      END
