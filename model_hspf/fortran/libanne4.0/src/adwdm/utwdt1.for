C     utwdt1.f 2.1 9/4/91
C
C
C
      INTEGER*4 FUNCTION   WDPTCL
     I                           (PREC,POFF)
C
C     + + + PURPOSE + + +
C     Calculate a pointer value from record number
C     and the offset within the record.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   PREC,POFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PREC   - record number
C     POFF   - offset within the record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 I
C
C     + + + END SPECIFICATIONS + + +
C
      I= PREC
      WDPTCL = (I*512)+ POFF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDPTSP
     I                   (PTR,
     O                    PREC,POFF)
C
C     + + + PURPOSE + + +
C     split up a pointer into record number and offset within record.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 PTR
      INTEGER   PREC,POFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     PTR    - pointer value
C     PREC   - record number
C     POFF   - offset within the record
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      PREC= PTR/ 512
      POFF= MOD(PTR,512)
C
      IF (PREC .LT. 1 .OR. PREC .GE. 2097152) THEN
        WRITE(99,*) ' IN WDPTSP: PREC,POFF,PTR=',PREC,POFF,PTR
        WRITE (*,*) ' IN WDPTSP: PREC,POFF,PTR=',PREC,POFF,PTR
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WBCWSP
     I                   (BCW,
     O                    NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
C     + + + PURPOSE + + +
C     Split up a block control word for a time-series type data set
C     to determine the information stored in it.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 BCW,NOV
      INTEGER   TSTEP,TCODE,COMPCD,QUALCD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCW    - block control word
C     NOV    - number of data values in the block
C     TSTEP  - time step of data block in TCODE units
C     TCODE  - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C                            7 - century
C     COMPCD - compression code
C              1 - compressed block of data
C              2 - uncompressed block of data
C     QUALCD - quality code, 0 <= QUALCD <= 31
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 TMP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      TMP= BCW
      IF (TMP.LT.0) THEN
C       neg value, integers are in 2's comp form
        TMP   = -TMP
        NOV   = 65536- (TMP/65536)- 1
        TSTEP = 64   - MOD((TMP/1024),64)-1
        TCODE =  8   - MOD((TMP/128) ,8) -1
        COMPCD=  4   - MOD((TMP/32 ) ,4) -1
        QUALCD= 32   - MOD (TMP      ,32)
      ELSE
        NOV   =     (TMP/65536)
        TSTEP = MOD((TMP/1024),64)
        TCODE = MOD((TMP/128) ,8)
        COMPCD= MOD((TMP/32 ) ,4)
        QUALCD= MOD (TMP      ,32)
      END IF
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WBCWCL
     I                           (NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
C     + + + PURPOSE + + +
C     Calculate a block control word for time-series type data.
C     Returns a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TSTEP,TCODE,COMPCD,QUALCD
      INTEGER*4 NOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NOV    - number of data values in the block
C     TSTEP  - time step of data block in TCODE units
C     TCODE  - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C                            7 - century
C     COMPCD - compression code
C              1 - compressed block of data
C              2 - uncompressed block of data
C     QUALCD - quality code, 0 <= QUALCD <= 31
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 BCW
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NOV.LE.0.OR.NOV.GT.65535.OR.TSTEP.LT.0.OR.TSTEP.GT.63.OR.
     1  TCODE.LT.0.OR.TCODE.GT.7.OR.COMPCD.LT.0.OR.COMPCD.GT.3.OR.
     2  QUALCD.LT.0.OR.QUALCD.GT.31) THEN
C       bad value, cant make a BCW
        BCW= 0
      ELSE
        BCW= NOV*65536+ TSTEP*1024+ TCODE*128+ COMPCD*32+ QUALCD
      END IF
C
      WBCWCL= BCW
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDATSP
     I                   (DATWRD,
     O                    DAT)
C
C     + + + PURPOSE + + +
C     Split up a WDMS date word.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 DATWRD
      INTEGER   DAT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DATWRD - date in compressed format
C     DAT    - date (year, month, day, hour)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
C     year
      DAT(1) = MOD((DATWRD/16384),131072)
C     month
      DAT(2) = MOD((DATWRD/1024) ,16)
C     day
      DAT(3) = MOD((DATWRD/32)   ,32)
C     hour
      DAT(4) = MOD (DATWRD       ,32)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WDATCL
     I                           (DAT)
C
C     + + + PURPOSE + + +
C     Calculate a date in compressed format from
C     its components (year- hour).  Returns a 0 if
C     any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DAT(4)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     DAT    - date (year, month, day, hour)
C              0 <= hour <= 24
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4   DATWRD,YEAR,MONTH,DAY,HOUR
C
C     + + + END SPECIFICATIONS + + +
C
      YEAR = DAT(1)
      MONTH= DAT(2)
      DAY  = DAT(3)
      HOUR = DAT(4)
      IF (YEAR.LE.0.OR.YEAR.GT.131071.OR.MONTH.LE.0.OR.MONTH.GT.12.
     1  .OR.DAY.LE.0.OR.DAY.GT.31.OR.HOUR.LT.0.OR.HOUR.GT.24) THEN
C       bad value, cant make a date word
        DATWRD= 0
      ELSE
        DATWRD= YEAR*16384+ MONTH*1024+ DAY*32+ HOUR
      END IF
C
      WDATCL= DATWRD
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WTBICL
     I                           (MSFLID,MCLU,MGRP)
C
C     + + + PURPOSE + + +
C     Calculate the identifier for a table type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MCLU,MGRP
      CHARACTER*1 MSFLID(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MSFLID - message file name id
C     MCLU   - message file cluster for table
C     MGRP   - message file group number for table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 I
C
C     + + + INTRINSICS + + +
      INTRINSIC   ICHAR
C
C     + + + END SPECIFICATIONS + + +
C
      I= ICHAR(MSFLID(1))
      I= I*256+ ICHAR(MSFLID(2))
      I= I*256+ MCLU- 1
      WTBICL= I*128+ MGRP- 1
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBISP
     I                   (TABID,
     O                    MSFLID,MCLU,MGRP)
C
C     + + + PURPOSE + + +
C     Split up an identifier for a table type
C     data set into its components.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     TABID,MCLU,MGRP
      CHARACTER*1 MSFLID(2)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TABID  - table identifier
C     MSFLID - message file name id
C     MCLU   - message file cluster for table
C     MGRP   - message file group number for table
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, CHAR
C
C     + + + END SPECIFICATIONS + + +
C
      MSFLID(1)= CHAR(TABID/8388608)
      MSFLID(2)= CHAR(MOD((TABID/32768),256))
      MCLU= MOD((TABID/128),256)+ 1
      MGRP= MOD(TABID,128)+ 1
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WTBDCL
     I                           (TABIND,NROW,NCOL,NEXT)
C
C     + + + PURPOSE + + +
C     Calculate the table dimension for a table type data set.
C     Returns a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TABIND,NROW,NCOL,NEXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TABIND - table index number, 0 < TABIND <= 128
C     NROW   - number of rows in table, 0 < NROW <= 512
C     NCOL   - space for each column(words), 0 < NCOL <= 128
C     NEXT   - amount of table extension space (words), 0 < NEXT <= 255
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (TABIND.LE.0.OR.TABIND.GT.128.OR.NROW.LE.0.OR.NROW.GT.512.
     1  .OR.NCOL.LE.0.OR.NCOL.GT.128.OR.NEXT.LT.0.OR.NEXT.GT.255) THEN
C       bad parameter, cant make a table dimension word
        I= 0
      ELSE
        I= ((TABIND-1)*16777216)+ ((NROW-1)*32768)+ ((NCOL-1)*256)+ NEXT
      END IF
C
      WTBDCL= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBDSP
     I                   (TABDIM,
     O                    TABIND,NROW,NCOL,NEXT)
C
C     + + + PURPOSE + + +
C     Split up dimension variable for table type
C     data set into its components.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   TABDIM,TABIND,NROW,NCOL,NEXT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TABDIM - table dimension variable
C     TABIND - table index number
C     NROW   - number of rows in table
C     NCOL   - space for each column(words)
C     NEXT   - amount of table extension space (words)
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      TABIND= (TABDIM/16777216)+ 1
      NROW  = MOD((TABDIM/32768),512)+ 1
      NCOL  = MOD((TABDIM/256),128) + 1
      NEXT  = MOD(TABDIM,256)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WBCWSQ
     I                   (BCW,
     O                    NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
C     + + + PURPOSE + + +
C     Split up a block control word for time-series type data set.
C     Adjusts TSTEP and NOV for faster operation, if possible.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER*4 BCW,NOV
      INTEGER   TSTEP,TCODE,COMPCD,QUALCD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BCW    - block control word
C     NOV    - number of data values in the block
C     TSTEP  - time step of data block in TCODE units
C     TCODE  - time units code
C              1 - second    4 - day
C              2 - minute    5 - month
C              3 - hour      6 - year
C                            7 - century
C     COMPCD - compression code
C              1 - compressed block of data
C              2 - uncompressed block of data
C     QUALCD - quality code, 0 <= QUALCD <= 31
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   DONFG
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL   WBCWSP
C
C     + + + END SPECIFICATIONS + + +
C
      CALL WBCWSP (BCW,
     O             NOV,TSTEP,TCODE,COMPCD,QUALCD)
C
      IF (COMPCD.EQ.1) THEN
C       try to adjust units and timestep for faster operation
        DONFG= 0
 20     CONTINUE
          IF (MOD(NOV,7).EQ.0.AND.TSTEP.LT.4000) THEN
            TSTEP= TSTEP* 7
            NOV  = NOV/ 7
          ELSE IF (MOD(NOV,5).EQ.0.AND.TSTEP.LT.6000) THEN
            TSTEP= TSTEP* 5
            NOV  = NOV/ 5
          ELSE IF (MOD(NOV,3).EQ.0.AND.TSTEP.LT.10000) THEN
            TSTEP= TSTEP* 3
            NOV  = NOV/ 3
          ELSE IF (MOD(NOV,2).EQ.0.AND.TSTEP.LT.15000) THEN
            TSTEP= TSTEP* 2
            NOV  = NOV/ 2
          ELSE
C           no more adjustment possible
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 20
      END IF
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WMSBCV
     I                           (CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Calculate a block control word for message type data set.
C     Returns a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CLASS  - class of information
C              1 - 1-dimensional parameter      4 - menu
C              2 - 2-dimensional parameter      5 - file
C              3 - text
C     ID     - id for portion of group
C              examples:  CLASS  ID  Description
C                           1     4  default value for parameter field
C                           4     6  help for menu screen
C                           5     3  status of file
C     ORDER  - order of information
C     TLEN   - total number of characters in the block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER*4 I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (CLASS.LE.0.OR.CLASS.GT.16.OR.ID.LT.0.OR.ID.GT.64.OR.
     1    ORDER.LT.0.OR.ORDER.GT.64.OR.TLEN.LT.0.OR.TLEN.GT.32768) THEN
C       bad parameter, cant make message block control word
        I= 0
      ELSE
        I= (CLASS*134217728)+ (ID*2097152)+ (ORDER*32768)+ TLEN
      END IF
C
      WMSBCV= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSBCS
     I                   (QWORD,
     O                    CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Split up a block control word for a message
C     type data set into its components.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   QWORD,CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     QWORD  - message type dataset block control word
C     CLASS  - class of information
C              1 - 1-dimensional parameter      4 - menu
C              2 - 2-dimensional parameter      5 - file
C              3 - text
C     ID     - id for portion of group
C              examples:  CLASS  ID  Description
C                           1     4  default value for parameter field
C                           4     6  help for menu screen
C                           5     3  status of file
C     ORDER  - order of information
C     TLEN   - total number of characters in the block
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      CLASS= (QWORD/134217728)
      ID   = MOD((QWORD/2097152),64)
      ORDER= MOD((QWORD/32768),64)
      TLEN = MOD(QWORD,32768)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WMSMNV
     I                           (IVAL)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters for a message
C     type data-set menu.  Returns a 0 if any one of the
C     components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVAL(5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - components of the word
C              (1)- default response value, 0 < IVAL(1) <= 64
C              (2)- response length, 0 < IVAL(2) <= 64
C              (3)- number menu options flag
C                   0 - no numbers
C                   1 - numbers
C              (4)- width of columns, 0 < IVAL(4) <= 128
C              (5)- length of columns, 0 < IVAL(5) <= 16
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IVAL(1).LT.0.OR.IVAL(1).GT.64 .OR. IVAL(2).LT.0.OR.
     1    IVAL(2).GT.64 .OR. IVAL(3).LT.0.OR.IVAL(3).GT.1 .OR.
     2    IVAL(4).LT.0.OR.IVAL(4).GT.128 .OR. IVAL(5).LT.0.OR.
     3    IVAL(5).GT.16) THEN
C       bad parameter value
        I= 0
      ELSE
        I= (IVAL(1)*262144)+ (IVAL(2)*4096)+ (IVAL(3)*2048)+
     1     (IVAL(4)*16)+ IVAL(5)
      END IF
C
      WMSMNV= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSMNS
     I                   (IMNVAL,
     O                    IDEF,ILEN,INNU,IWID,ICOL)
C
C     + + + PURPOSE + + +
C     Split up a word containing integer parameters
C     for a message type data-set menu.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IMNVAL,IDEF,ILEN,INNU,IWID,ICOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IMNVAL - the word
C     IDEF   - default response value
C     ILEN   - response length
C     INNU   - nonumber option
C              0 - no numbers
C              1 - numbers
C     IWID   - width of columns
C     ICOL   - length of columns
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IDEF= (IMNVAL/262144)
      ILEN= MOD((IMNVAL/4096),64)
      INNU= MOD((IMNVAL/2048),2)
      IWID= MOD((IMNVAL/16),128)
      ICOL= MOD(IMNVAL,16)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WMSPIV
     I                           (IVL1,IVL2)
C
C     + + + PURPOSE + + +
C     Calculate a word from two integer values
C     (1st value- 16 bits, 2nd- 15 bits).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVL1,IVL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVL1   - first integer value, 0 <= IVL1 <= 65536
C     IVL2   - second integer value, 0 <= IVL2 <= 32768
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IVL1.LT.0 .OR. IVL1.GT.65536 .OR.
     1    IVL2.LT.0 .OR. IVL2.GT.32768) THEN
C       bad parameter
        I= 0
      ELSE
        I = (IVL1*32768)+ IVL2
      END IF
C
      WMSPIV= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSPIS
     I                   (IVAL,
     O                    IVL1,IVL2)
C
C     + + + PURPOSE + + +
C     Split up a word into two integer values.
C
C     + + + DUMMY ARUGMENTS + + +
      INTEGER   IVAL,IVL1,IVL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL   - the word
C     IVL1   - 1st integer value
C     IVL2   - 2nd integer value
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IVL1= IVAL/32768
      IVL2= MOD(IVAL,32768)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WMSP2V
     I                           (FTYP,FLEN,FORDER,FPROT,FCOL)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters for a message
C     type data-set PRM2 class screen.  Return a 0 if any
C     one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   FTYP,FLEN,FORDER,FPROT,FCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTYP   - data field type, 0 <= FTYP <= 16
C              1 - integer
C              2 - real
C              3 - double precision
C              4 - character
C     FLEN   - data field length, 0 <= FLEN <= 128
C     FORDER - data field ordering, 0 <= FORDER <= 4
C              0 - no ordering
C              1 - ascending order
C              2 - descending order
C     FPROT  - data field protection, 0 <= FPROT <= 4
C              0 - no protection
C              1 - value must be in specified range
C              2 - value may not be modified
C     FCOL   - data field starting column, 0 <= FCOL <= 128
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (FTYP.LT.0.OR.FTYP.GT.16 .OR. FLEN.LT.0.OR.FLEN.GT.128 .OR.
     1    FORDER.LT.0.OR.FORDER.GT.4 .OR. FPROT.LT.0.OR.FPROT.GT.4 .OR.
     2    FCOL.LT.0.OR. FCOL.GT.128) THEN
C       bad parameter value
        I= 0
      ELSE
        I= (FTYP*262144)+ (FLEN*2048)+ (FORDER*512)+ (FPROT*128)+ FCOL
      END IF
C
      WMSP2V= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSP2S
     I                   (IP2VAL,
     O                    FTYP,FLEN,FORDER,FPROT,FCOL)
C
C     + + + PURPOSE + + +
C     Split up a word containing integer parameters for
C     a message type data-set PRM2 screen.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IP2VAL,FTYP,FLEN,FORDER,FPROT,FCOL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IP2VAL - the word
C     FTYP   - data field type
C     FLEN   - data field length
C     FORDER - data field ordering
C     FPROT  - data field protection
C     FCOL   - data field starting column
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      FTYP  = (IP2VAL/262144)
      FLEN  = MOD((IP2VAL/2048),128)
      FORDER= MOD((IP2VAL/512),4)
      FPROT = MOD((IP2VAL/128),4)
      FCOL  = MOD(IP2VAL,128)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WDLICV
     I                           (ITYPE,ATT1,ATT2,BLCNT)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters for
C     a vector (DLG) type data set.  Return a 0
C     if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ITYPE,ATT1,ATT2,BLCNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ITYPE  - vector (DLG) data type, 0 <= ITYPE <= 3
C              1- Line
C              2- Area
C              3- Node
C     ATT1   - major attribute, 0 <= ATT1 <= 1023
C     ATT2   - minor attribute, 0 <= ATT2 <= 2047
C     BLCNT  - number of blocks in group, 0 <= BLCNT <= 255
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ITYPE.LT.0.OR.ITYPE.GT.3 .OR. ATT1.LT.0.OR.ATT1.GT.1023 .OR.
     1    ATT2.LT.0.OR.ATT2.GT.2047 .OR. BLCNT.LT.0.OR.BLCNT.GT.255)THEN
C       bad paramter value
        I= 0
      ELSE
        I= (ITYPE*536870912)+ (ATT1*524288)+ (ATT2*256)+ BLCNT
      END IF
C
      WDLICV= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLISP
     I                   (IWORD,
     O                    ITYPE,ATT1,ATT2,BLCNT)
C
C     + + + PURPOSE + + +
C     Split up a word containing parameters for
C     a vector (DLG) type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IWORD,ITYPE,ATT1,ATT2,BLCNT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IWORD  - DLG parameters integer word
C     ITYPE  - DLG data type
C              1- Line
C              2- Area
C              3- Node
C     ATT1   - major attribute
C     ATT2   - minor attribute
C     BLCNT  - count of blocks
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      ITYPE= (IWORD/536870912)
      ATT1 = MOD((IWORD/524288),1024)
      ATT2 = MOD((IWORD/256),2048)
      BLCNT= MOD(IWORD,256)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WDLBCV
     I                           (ID,DTYPE,INUM,ILEN)
C
C     + + + PURPOSE + + +
C     Calculate a block control word for vector (DLG) type data set.
C     Return a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ID,DTYPE,INUM,ILEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ID     - block type, 0 <= ID <= 15
C              1- integer number
C              2- coordinatess
C              3- associated table
C     DTYPE  - vector (DLG) data type, 0 <= DTYPE <= 3
C              1- line
C              2- area
C              3- node
C     INUM   - internal number, 0 <= INUM <= 4095
C     ILEN   - length of information in block, 0 <= INUM <= 8191
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ID.LT.0.OR.ID.GT.15 .OR. DTYPE.LT.0.OR.DTYPE.GT.3 .OR.
     1    INUM.LT.0.OR.INUM.GT.4095 .OR. ILEN.LT.0.OR.ILEN.GT.8191) THEN
C       bad paramter value
        I= 0
      ELSE
        I= (ID*134217728)+ (DTYPE*33554432)+ (INUM*8192)+ ILEN
      END IF
C
      WDLBCV= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDLBSP
     I                   (BWORD,
     O                    ID,DTYPE,INUM,ILEN)
C
C     + + + PURPOSE + + +
C     Split up a block control word for vector (DLG) type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   BWORD,ID,DTYPE,INUM,ILEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     BWORD  - block control word
C     ID     - block type
C              1- integer number
C              2- coordinates
C              3- associated table
C     DTYPE  - vector (DLG) data type
C              1- line
C              2- area
C              3- node
C     INUM   - internal number
C     ILEN   - length of information in block
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      ID   = (BWORD/134217728)
      DTYPE= MOD((BWORD/33554432),4)
      INUM = MOD((BWORD/8192),4096)
      ILEN = MOD(BWORD,8192)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WATTCL
     I                           (ATTYP,ATLEN,ATUSE,ATUPD)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters for attribute type
C     data set.  Return a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ATTYP,ATLEN,ATUSE(10),ATUPD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ATTYP  - attribute type, 0 <= ATTYP <= 8
C              1- integer
C              2- real
C              3- character
C     ATLEN  - attribute length, 1 <= ATLEN <= 128
C              ATTYP= 1 or 2 - number of words
C              ATTYP= 3 - number of characters
C     ATUSE  - array of indicator flags for data-set
C              attribute usage, ATUSE(n) = 0, 1, or 2
C              0 - attribute not allowed for data-set type
C              1 - attribute optional for data-set type
C              2 - attribute required for data-set type
C     ATUPD  - attribute update flag, ATPUD = 0 or 1
C              0 - don't update attribute
C              1 - update attribute if data exists
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,ITMP,IEXP
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ATTYP.LT.1.OR.ATTYP.GT.8 .OR. ATLEN.LT.1.OR.ATLEN.GT.128 .OR.
     1    ATUPD.LT.0.OR.ATUPD.GT.1) THEN
C       bad parameter value
        I= 0
      ELSE
        ITMP= 0
        IEXP= 262144
        DO 10 I= 1,10
          ITMP= ITMP+ IEXP*ATUSE(I)
          IEXP= IEXP/4
 10     CONTINUE
        I= (ATTYP-1)*268435456+ ATLEN*2097152+ ITMP*2+ ATUPD
      END IF
C
      WATTCL= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATTSP
     I                   (IATVL,
     O                    ATTYP,ATLEN,ATUSE,ATUPD)
C
C     + + + PURPOSE + + +
C     Split up a word containing parameters for
C     attribute type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IATVL,ATTYP,ATLEN,ATUSE,ATUPD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IATVL  - attribute parameters integer word
C     ATTYP  - attribute type
C              1- integer
C              2- real
C              3- character
C     ATLEN  - attribute length
C              ATTYP= 1 or 2 - number of words
C              ATTYP= 3 - number of characters
C     ATUSE  - array of indicator flags for data-set attribute usage
C              0 - attribute not allowed for data-set type
C              1 - attribute optional for data-set type
C              2 - attribute required for data-set type
C     ATUPD  - attribute update flag
C              0 - don't update attribute
C              1 - update attribute if data exists
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      ATTYP= IATVL/268435456+ 1
      ATLEN= MOD((IATVL/2097152),128)
      ATUSE= MOD((IATVL/2),1048576)
      ATUPD= MOD(IATVL,2)
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATTUS
     I                   (ATUSWD,
     O                    ATUSE)
C
C     + + + PURPOSE + + +
C     Split up word containing array of indicator flags
C     for dataset attribute usage.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER  ATUSWD,ATUSE(10)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ATUSWD - attribute usage word
C     ATUSE  - array of indicator flags for data-set attribute usage
C              0 - attribute not allowed for data-set type
C              1 - attribute optional for data-set type
C              2 - attribute required for data-set type
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,IEXP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IEXP = 262144
      DO 10 I= 1,10
        ATUSE(I)= MOD((ATUSWD/IEXP),4)
        IEXP= IEXP/4
 10   CONTINUE
C
      RETURN
      END
C
C
C
      INTEGER FUNCTION   WATWDC
     I                         (IVAL1,IVAL2)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters
C     for attribute type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVAL1,IVAL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL1  - either last 2 characters of name or
C              id of info, 0 <= IVAL1 <= 4194304
C     IVAL2  - either attribute index or
C              length of info, 0 <= IVAL2 <= 512
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IVAL1.LT.0 .OR. IVAL1.GT.4194304 .OR.
     1    IVAL2.LT.0 .OR. IVAL2.GT.512) THEN
        I= 0
      ELSE
        I= (IVAL1*512)+ IVAL2
      END IF
C
      WATWDC= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WATWDS
     I                   (IWORD,
     O                    IVAL1,IVAL2)
C
C     + + + PURPOSE + + +
C     Split up a word containing parameters for attribute type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER    IWORD,IVAL1,IVAL2
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IWORD  - attribute parameters integer word
C     IVAL1  - either last 2 characters of name or id of info
C     IVAL2  - either attribute index or length of info
C
C     + + + INTRINSICS + + +
      INTRINSIC  MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL1= IWORD/ 512
      IVAL2= MOD(IWORD,512)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WSTGCL
     I                           (STMIN,STSEC,TUNITS,TSTEP,NOV)
C
C     + + + PURPOSE + + +
C     Calculate a block control word for space-time type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   STMIN,STSEC,TUNITS,TSTEP,NOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STMIN  - starting time of group-minutes, 0 <= STMIN <= 60
C     STSEC  - starting time of group-seconds, 0 <= STSEC <= 60
C     TUNITS - time units of group, 0 <= TUNITS <= 7
C     TSTEP  - time step of group, 0 <= STMIN <= 63
C     NOV    - number of values in group, 0 < STMIN <= 1023
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GCW
C
C     + + + END SPECIFCATIONS + + +
C
      IF (STMIN.LT.0 .OR. STMIN.GT.60 .OR.
     1    STSEC.LT.0 .OR. STSEC.GT.60 .OR.
     2    TUNITS.LT.0.OR. TUNITS.GT.7 .OR.
     3    TSTEP.LT.0 .OR. TSTEP.GT.63 .OR.
     4    NOV.LE.0   .OR. NOV.GT.1023) THEN
C       bad parameter, cant make group info control word
        GCW= 0
      ELSE
        GCW= STMIN*33554432+ STSEC*524288+ TUNITS*65536+ TSTEP*1024+ NOV
      END IF
C
      WSTGCL= GCW
C
      RETURN
      END
C
C
C
      SUBROUTINE   WSTGSP
     I                   (GCW,
     O                    STMIN,STSEC,TUNITS,TSTEP,NOV)
C
C     + + + PURPOSE + + +
C     Split up a block control word for space-time type data set.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   GCW,STMIN,STSEC,TUNITS,TSTEP,NOV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GCW    - block control word
C     STMIN  - starting time of group-minutes
C     STSEC  - starting time of group-seconds
C     TUNITS - time units of group
C     TSTEP  - time step of group
C     NOV    - number of values in group
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     TMP
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFCATIONS + + +
C
      TMP   = GCW
      STMIN =     (TMP/33554432)
      STSEC = MOD((TMP/524288),64)
      TUNITS= MOD((TMP/65536),8)
      TSTEP = MOD((TMP/1024) ,64)
      NOV   = MOD (TMP       ,1024)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WMSPOV
     I                           (OPSET,OPSTNO,OPDEF,
     I                            OPWID,OPHIDE,OPBOX)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters for a message
C     type data-set PRM1 screen data field of type Option.
C     Return a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPSET,OPSTNO,OPDEF,OPWID,OPHIDE,OPBOX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPSET  - set number for this option field, 0 <= OPSET <= 64
C     OPSTNO - order number within set of this field, 0 <= OPSTNO <= 64
C     OPDEF  - default value of option field, 0 <= OPDEF <= 1
C              0 - OFF, 1 - ON
C     OPWID  - width of field to highlight, 0 <= OPWID <= 128
C     OPHIDE - flag indicating field is to be hidden on output, 0 <= OPHIDE <=1
C     OPBOX  - flag indicating to draw box next to field, 0 <= OPBOX <= 1
C              0 - do not draw box
C              1 - draw box to left of option field
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (OPSET.LT.0.OR.OPSET.GT.64 .OR. OPSTNO.LT.0.OR.OPSTNO.GT.64
     1    .OR. OPDEF.LT.0.OR.OPDEF.GT.1 .OR. OPWID.LT.0.OR.OPWID.GT.128
     2    .OR. OPHIDE.LT.0.OR.OPHIDE.GT.1
     3    .OR. OPBOX.LT.0.OR.OPBOX.GT.1) THEN
C       bad parameter value
        I= 0
      ELSE
        I= (OPSET*65536)+ (OPSTNO*1024)+ (OPDEF*512)+
     1     (OPWID*4)+ (OPHIDE*2)+ OPBOX
      END IF
C
      WMSPOV= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSPOS
     I                   (IOPVAL,
     O                    OPSET,OPSTNO,OPDEF,OPWID,OPHIDE,OPBOX)
C
C     + + + PURPOSE + + +
C     Split up a word containing integer parameters for
C     a message type data-set PRM1 screen option type field.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IOPVAL,OPSET,OPSTNO,OPDEF,OPWID,OPHIDE,OPBOX
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IOPVAL - the word
C     OPSET  - set number for this option field
C     OPSTNO - order number within set of this field, 0 <= OPSTNO <= 64
C     OPDEF  - default value of option field
C              0 - OFF, 1 - ON
C     OPWID  - width of field to highlight
C     OPHIDE - flag indicating field is to be hidden on output,
C              0 - don't hide field
C              1 - hide field
C     OPBOX  - flag indicating to draw box next to field,
C              0 - do not draw box
C              1 - draw box to left of option field
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      OPSET = (IOPVAL/65536)
      OPSTNO= MOD((IOPVAL/1024),64)
      OPDEF = MOD((IOPVAL/512),2)
      OPWID = MOD((IOPVAL/4),128)
      OPHIDE= MOD((IOPVAL/2),2)
      OPBOX = MOD(IOPVAL,2)
C
      RETURN
      END
C
C
C
      INTEGER*4 FUNCTION   WOPWDC
     I                           (IVAL1,IVAL2,IVAL3,IVAL4)
C
C     + + + PURPOSE + + +
C     Calculate a word containing parameters for conditionals on
C     option type data fields (generic for 4 values of 7 bits each).
C     Return a 0 if any one of the components is invalid.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   IVAL1,IVAL2,IVAL3,IVAL4
C
C     + + + ARGUMENT DEFINITIONS + + +
C     IVAL1  - first value in word, 0 <= IVAL1 <= 128
C     IVAL2  - second value in word, 0 <= IVAL2 <= 128
C     IVAL3  - third value in word, 0 <= IVAL3 <= 128
C     IVAL4  - fourth value in word, 0 <= IVAL4 <= 128
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I
C
C     + + + END SPECIFICATIONS + + +
C
      IF (IVAL1.LT.0.OR.IVAL1.GT.128 .OR.
     1    IVAL2.LT.0.OR.IVAL2.GT.128 .OR.
     1    IVAL3.LT.0.OR.IVAL3.GT.128 .OR.
     2    IVAL4.LT.0.OR.IVAL4.GT.128) THEN
C       bad parameter value
        I= 0
      ELSE
        I= (IVAL1*2097152)+ (IVAL2*16384)+ (IVAL3*128)+ IVAL4
      END IF
C
      WOPWDC= I
C
      RETURN
      END
C
C
C
      SUBROUTINE   WOPWDS
     I                   (OPWORD,
     O                    IVAL1,IVAL2,IVAL3,IVAL4)
C
C     + + + PURPOSE + + +
C     Split up a word containing integer parameters for conditionals
C     for option type data fields (generic for 4 values of 7 bits each).
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   OPWORD,IVAL1,IVAL2,IVAL3,IVAL4
C
C     + + + ARGUMENT DEFINITIONS + + +
C     OPWORD - the word
C     IVAL1  - first value in word, 0 <= IVAL1 <= 128
C     IVAL2  - second value in word, 0 <= IVAL2 <= 128
C     IVAL3  - third value in word, 0 <= IVAL3 <= 128
C     IVAL4  - fourth value in word, 0 <= IVAL4 <= 128
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + END SPECIFICATIONS + + +
C
      IVAL1 = (OPWORD/2097152)
      IVAL2 = MOD((OPWORD/16384),128)
      IVAL3 = MOD((OPWORD/128),128)
      IVAL4 = MOD(OPWORD,128)
C
      RETURN
      END
