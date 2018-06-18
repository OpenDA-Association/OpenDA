C
C
C
      SUBROUTINE   WDTBTM
     I                    (MESSFL,MFID,TCLU,TGRP,WDMSFL,DSN,TABIND,NROW,
     O                     TFLDS,TNUM,TTYP,TLEN,TCOL,TSPA,MTBNAM,TGRPPT,
     O                     AFLDS,ANUM,ATYP,ALEN,ACOL,ASPA,ACLU,AGRP,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     put WDM table template on WDM file, return parameters about table
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,TCLU,TGRP,WDMSFL,DSN,TABIND,NROW,
     1             TFLDS,TNUM(4),TLEN(30),TCOL(30),TSPA,TGRPPT,
     2             AFLDS,ANUM(4),ALEN(30),ACOL(30),ASPA,ACLU,AGRP,
     3             RETCOD
      CHARACTER*1  MFID(2),TTYP(30),ATYP(30)
      CHARACTER*16 MTBNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of WDM file containing table definition
C     MFID   - Message file name id of MESSFL
C     TCLU   - Message file cluster containing table template
C     TGRP   - Group number containing table template
C     WDMSFL - Fortran unit number of WDM file to put table template in
C     DSN    - Data-set number to put table template in
C     TABIND - Table identifier number of new table
C     NROW   - Number of rows in new table
C     TFLDS  - Number of fields in table
C     TNUM   - Number of each variable type in table(I-1,R-2,C-3,D-4)
C     TTYP   - Type of each field in table(I,R,C,D)
C     TLEN   - Length of each field in table(characters)
C     TCOL   - Starting column for each field
C     TSPA   - Space required for each table row(words)
C     MTBNAM - Name of table from message file
C     TGRPPT - Pointer to group within DSN
C     AFLDS  - Number of fields in table extension
C     ANUM   - Number of each variable type in table extension(I-1,R-2,C-3,D-4)
C     ATYP   - Type of each field in table extension
C     ALEN   - Length of each field in table extension
C     ACOL   - Starting column for each associated field
C     ASPA   - Space required for table extension
C     ACLU   - Associated table cluster number
C     AGRP   - Associated table group number
C     RETCOD - Return code
C              -22 - template does not exist
C              -25 - template already exists
C              -26 - can't add another table
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     LREC,LPOS,DREC,DPOS,EREC,EPOS,LGRP,LCLU,
     1            TBCNT,RIND,PDAT,PDATV,NEWPOS,I,LMESFL,
     2            PDATVL,TABDIM,TABID,TOTSPA,MXTAB
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, WTBDCL, WTBICL, WDRCGX, WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDTBFN, WDTBSP, WDRCGO, WDRCGX, WDRCUP, WDMODT
      EXTERNAL    WDPTSP, WDPTCL, WTBDCL, WTBICL, WDTBCG
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (4A4)
C
C     + + + END SPECIFICATIONS + + +
C
C     get actual location of table data set template
      CALL WDTBCG (MESSFL,WDMSFL,TCLU,TGRP,
     O             LMESFL,LCLU,LGRP,RETCOD)
      IF (RETCOD .EQ. 0) THEN
C       get table information from message file
        CALL  WDTBSP (LMESFL,LCLU,LGRP,
     O                MTBNAM,TFLDS,TTYP,TLEN,TCOL,TSPA,TNUM,
     O                ACLU,AGRP,AFLDS,ATYP,ALEN,ACOL,ASPA,ANUM,RETCOD)
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       does template already exist?
        CALL WDTBFN (WDMSFL,DSN,TABIND,MTBNAM,
     O               TBCNT,LREC,TGRPPT,I,RETCOD)
        IF (RETCOD.NE.1) THEN
C         template already exists
          RETCOD= -25
        ELSE
          RETCOD= 0
C         space for another table?
          RIND  = WDRCGO(WDMSFL,LREC)
          PDAT  = WIBUFF(11,RIND)
          PDATV = WIBUFF(12,RIND)
          MXTAB = (PDATV- PDAT-1)/ 7
          IF (TBCNT+1.GT.MXTAB) THEN
C           can't add another table
            RETCOD= -26
          END IF
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       ok to add new template
C       calculate total space needed for table
        TOTSPA= ASPA+ TSPA*NROW+ 2
C       calc label info, pointer to table
        PDATVL= WIBUFF(PDAT+1,RIND)
C       table dimensions
        TABDIM= WTBDCL(TABIND,NROW,TSPA,ASPA)
C       table identifier
        TABID = WTBICL(MFID,TCLU,TGRP)
C       table count
        TBCNT = TBCNT+ 1
C
C       get space for table
        CALL WDPTSP (PDATVL,
     O               DREC,DPOS)
        RIND  = WDRCGO(WDMSFL,DREC)
        IF (DPOS.GT.511 .OR. DPOS.EQ.0) THEN
C         start the table on the next record
          RIND  = WDRCGX(WDMSFL,I,DREC)
          DREC  = RECNO(RIND)
          DPOS  = 5
          PDATVL= WDPTCL(DREC,DPOS)
        END IF
C       put header check info at beginning of table
        WIBUFF(DPOS,RIND)= TABID
        WIBUFF(DPOS+1,RIND)= TABDIM
        CALL WDRCUP(WDMSFL,RIND)
C       end of table location
        EPOS= DPOS+ TOTSPA
        EREC= DREC
        IF (EPOS.GT.512) THEN
C         need additional records for this table
 20       CONTINUE
            I   = 0
            RIND= WDRCGX(WDMSFL,I,EREC)
            EREC= RECNO(RIND)
            EPOS= EPOS- 508
          IF (EPOS.GT.512) GO TO 20
        END IF
C       calc new free space for next table
        NEWPOS= WDPTCL(EREC,EPOS)
C
C       store label information
        RIND  = WDRCGO(WDMSFL,LREC)
        WIBUFF(PDAT,RIND)  = TBCNT
        WIBUFF(PDAT+1,RIND)= NEWPOS
        LPOS  = PDAT+ 2+ (TGRPPT-1)* 7

       READ (MTBNAM,1000) WIBUFF(LPOS,RIND),WIBUFF(LPOS+1,RIND),
     1                     WIBUFF(LPOS+2,RIND),WIBUFF(LPOS+3,RIND)
        WIBUFF(LPOS+4,RIND)= TABID
        WIBUFF(LPOS+5,RIND)= TABDIM
        WIBUFF(LPOS+6,RIND)= PDATVL
C       save revised label
        CALL WDRCUP(WDMSFL,RIND)
C       calc which group the new template points to
        TGRPPT= 1+ (LPOS- PDAT+ 2)/ 7
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTBFX
     I                    (WDMSFL,DSN,TABIND,TABNAM,
     O                     TBCNT,LREC,TGRPPT,MFID,TCLU,TGRP,NROW,RETCOD)
C
C     + + + PURPOSE + + +
C     determines pointer to the specified table,
C     also returns its message file cluster and group number
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,TABIND,TBCNT,LREC,
     1             TGRPPT,TCLU,TGRP,NROW,RETCOD
      CHARACTER*1  MFID(2)
      CHARACTER*16 TABNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - table data-set number
C     TABIND - table identifier number
C     TABNAM - name of table
C     TBCNT  - total number of tables in data set
C     LREC   - label record number
C     TGRPPT - table group pointer
C     MFID   - message file name id
C     TCLU   - table message file cluster number
C     TGRP   - table message file group number
C     NROW   - number of rows in table
C     RETCOD - return code
C                0 - table found, pointer and other information returned

C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      LIND,LPOS,PDAT
C
C     + + + FUNCTIONS + + +
      INTEGER      WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDTBFN, WTBISP, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
C     get specs about table
      CALL WDTBFN (WDMSFL,DSN,TABIND,TABNAM,
     O             TBCNT,LREC,TGRPPT,NROW,RETCOD)
C
      IF (RETCOD.EQ.1) THEN
C       it doesnt exist yet
        RETCOD= -28
        TCLU  = 0
        TGRP  = 0
      ELSE IF (RETCOD.EQ.0) THEN
C       get message file info
        LIND= WDRCGO(WDMSFL,LREC)
C       point to group pointers
        PDAT= WIBUFF(11,LIND)
C       point to our group
        LPOS= PDAT+ 2+ (TGRPPT-1)* 7
C       get the message file group and offset
        CALL WTBISP (WIBUFF(LPOS+4,LIND),
     O               MFID,TCLU,TGRP)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTBFN
     I                    (WDMSFL,DSN,TABIND,MTBNAM,
     O                     TBCNT,LREC,TGRPPT,NROW,RETCOD)
C
C     + + + PURPOSE + + +
C     determines pointer to the specified table,
C     returns group number of next free space and RETCOD of 1
C     if not found in a valid table data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,TABIND,TBCNT,LREC,TGRPPT,NROW,RETCOD
      CHARACTER*16 MTBNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Watershed data management file unit number
C     DSN    - table data-set number
C     TABIND - table identifier number
C     MTBNAM - name of table
C     TBCNT  - total number of tables in data set
C     LREC   - label record number
C     TGRPPT - table group pointer
C     NROW   - number of rows in table
C     RETCOD - return code

C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,RIND,PDAT,LPOS,WTBIND,WNCOL,WNEXT
      CHARACTER*16 WTBNAM
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, WDSCHK, WTBDSP
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4A4)
C
C     + + + END SPECIFICATIONS + + +
C
      NROW = 0
      TGRPPT= 0
C     check table data set
      I= 2
      CALL WDSCHK (WDMSFL,DSN,I,
     O             LREC,TBCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       get info about this dsn
        RIND= WDRCGO(WDMSFL,LREC)
        PDAT= WIBUFF(11,RIND)
C       pointer to table names
        LPOS= PDAT- 5
        IF (TBCNT.GT.0) THEN
C         check existing tables
          I= 0
 10       CONTINUE
            I   = I+ 1
            LPOS= LPOS+ 7
            WRITE (WTBNAM,2000) WIBUFF(LPOS,RIND),WIBUFF(LPOS+1,RIND),
     1                          WIBUFF(LPOS+2,RIND),WIBUFF(LPOS+3,RIND)
            IF (WTBNAM.EQ.MTBNAM) THEN
C             may be a duplicate table name, check index
              CALL WTBDSP (WIBUFF(LPOS+5,RIND),
     O                     WTBIND,NROW,WNCOL,WNEXT)
              IF (WTBIND.EQ.TABIND) THEN
C               already have a template for this table
                TGRPPT= I
              END IF
            END IF
          IF (TGRPPT.EQ.0 .AND. I.LT.TBCNT) GO TO 10
        END IF
        IF (TGRPPT.EQ.0) THEN
C         table template not found
          RETCOD= 1
          NROW = 0
          TGRPPT= TBCNT+ 1
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTBSP
     I                    (MESSFL,TCLU,TGRP,
     O                     MTBNAM,TFLDS,TTYP,TLEN,TCOL,TSPA,TNUM,
     O                     ACLU,AGRP,AFLDS,ATYP,ALEN,ACOL,ASPA,ANUM,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     get WDM table specifications from message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,RETCOD,
     1             TCLU,TGRP,TFLDS,TLEN(30),TCOL(30),TSPA,TNUM(4),
     1             ACLU,AGRP,AFLDS,ALEN(30),ACOL(30),ASPA,ANUM(4)
      CHARACTER*1  TTYP(30),ATYP(30)
      CHARACTER*16 MTBNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     TCLU   - Message file cluster containing table template
C     TGRP   - Group number containing table template
C     MTBNAM - Table name
C     TFLDS  - Number of fields in table
C     TTYP   - Type(I,R,C,D,A) of each field
C     TLEN   - Length of each field
C     TCOL   - Starting column for each field
C     TSPA   - Number of columns of table data(fields adjusted for size)
C     TNUM   - Number of each variable type in table
C     ACLU   - Associated table cluster
C     AGRP   - Associated group number
C     AFLDS  - Number of associated table fields
C     ATYP   - Type(I,R,C,D,A) of each associated field
C     ALEN   - Length of each associated field
C     ACOL   - Starting column for each associated field
C     ASPA   - Space for table extension
C     ANUM   - Number of each variable type in table extension
C     RETCOD - Return code
C                0 - information successfully obtained
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I16,DREC,DPOS,DIND,QWORD,CLASS,ID,ORDER,TXTLEN,
     1            LCLU,LGRP,MLEN,GLEN,OLEN,AGRFLG,NEWPOS,RDWRFG,ITYP,
     2            IVAL,DONFG
      CHARACTER*1 LTBNAM(16)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WMSFBC, WMSBCS, WDNXDV, WMSGTE, WTBSPA
      EXTERNAL    WMSPIS, WMSP2S, WMSSKB, WDNXPS
C
C     + + + INPUT FORMATS + + +
 2000 FORMAT (16A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I16   = 16
      RDWRFG= 1
      DONFG = 0
      AGRFLG= 0
      RETCOD= 0
      NEWPOS= 1
      LCLU  = TCLU
      LGRP  = TGRP
C
 100  CONTINUE
C       loop until all info for tables retrieved
        IF (NEWPOS.EQ.1) THEN
C         determine start of data for table
          CALL WMSFBC (MESSFL,LCLU,LGRP,
     O                 DREC,DPOS,QWORD)
          IF (QWORD.LE.0) THEN
            IF (AGRFLG.EQ.0) THEN
C             not a valid table
              RETCOD= -23
            ELSE
C             not a valid associated table
              RETCOD= -24
            END IF
          END IF
          NEWPOS= 0
        END IF
C       get block control word
        DIND = WDRCGO(MESSFL,DREC)
        QWORD= WIBUFF(DPOS,DIND)
        IF (QWORD.GT.0) THEN
C         get table parameters
          CALL WMSBCS (QWORD,
     O                 CLASS,ID,ORDER,TXTLEN)
          IF (ID.GT.0) THEN
C           get next bit of info
            IF (ID.EQ.17) THEN
C             table name
              GLEN= 0
              MLEN= 0
              CALL WMSGTE (MESSFL,TXTLEN,I16,
     M                     DREC,DPOS,GLEN,MLEN,
     O                     OLEN,LTBNAM,I)
              WRITE (MTBNAM,2000) LTBNAM
            ELSE IF (ID.EQ.3 .OR. ID.EQ.18) THEN
C             get the integer value from the WDM file
              CALL WDNXDV (MESSFL,
     M                     DREC,DPOS,
     O                     IVAL)
              IF (ID.EQ.3) THEN
C               integer parameters - type, width, and start column
                IF (AGRFLG.EQ.1) THEN
C                 filling in associated table
                  CALL WMSP2S (IVAL,
     O                         ITYP,ALEN(ORDER),I,
     O                         I,ACOL(ORDER))
                  IF (ITYP.EQ.1) THEN
                    ATYP(ORDER)= 'I'
                  ELSE IF (ITYP.EQ.2) THEN
                    ATYP(ORDER)= 'R'
                  ELSE IF (ITYP.EQ.3) THEN
                    ATYP(ORDER)= 'D'
                  ELSE IF (ITYP.EQ.4) THEN
                    ATYP(ORDER)= 'C'
                  END IF
                  AFLDS= ORDER
                ELSE
C                 fill in main table
                  CALL WMSP2S (IVAL,
     O                         ITYP,TLEN(ORDER),I,
     O                         I,TCOL(ORDER))
                  IF (ITYP.EQ.1) THEN
                    TTYP(ORDER)= 'I'
                  ELSE IF (ITYP.EQ.2) THEN
                    TTYP(ORDER)= 'R'
                  ELSE IF (ITYP.EQ.3) THEN
                    TTYP(ORDER)= 'D'
                  ELSE IF (ITYP.EQ.4) THEN
                    TTYP(ORDER)= 'C'
                  END IF
                  TFLDS= ORDER
                END IF
              ELSE IF (ID.EQ.18) THEN
C               associated table cluster and group
                CALL WMSPIS (IVAL,
     O                       ACLU,AGRP)
              END IF
            ELSE
C             skip to next block control word
              CALL WMSSKB (MESSFL,TXTLEN,
     M                     DREC,DPOS)
            END IF
C           move to next data position
            CALL WDNXPS (MESSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   I)
          END IF
        ELSE
C         done with this tables info
          IF (AGRFLG.EQ.0 .AND. ACLU.GT.0 .AND. AGRP.GT.0) THEN
C           still need to get associated table info
            AGRFLG= 1
            NEWPOS= 1
            LCLU  = ACLU
            LGRP  = AGRP
          ELSE
C           done with both tables or no associated table to get
            DONFG= 1
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 100
C
      IF (RETCOD.EQ.0) THEN
C       calc space for each row
        CALL WTBSPA (TFLDS,TTYP,TLEN,
     O               TSPA,TNUM)
        IF (ACLU.GT.0) THEN
C         need space for associated table
          CALL WTBSPA (AFLDS,ATYP,ALEN,
     O                 ASPA,ANUM)
        ELSE
C         no space needed for associated table
          AFLDS= 0
          ASPA = 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBSPA
     I                    (NFLDS,FTYP,FLEN,
     O                     RWSPA,XNUM)
C
C     + + + PURPOSE + + +
C     calculates space required for a row in a table data set and
C     count types of data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     NFLDS,FLEN(NFLDS),RWSPA,XNUM(4)
      CHARACTER*1 FTYP(NFLDS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NFLDS  - number of fields in table
C     FTYP   - type of each field(I,R,D,A,C)
C     FLEN   - length of each field
C     RWSPA  - space required for row (4 byte words)
C     XNUM   - count of each data type in table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      RWSPA= 0
      I    = 4
      CALL ZIPI (I,RWSPA,XNUM)
C
      DO 10 I= 1,NFLDS
        IF (FTYP(I).EQ.'D') THEN
C         double prec
          RWSPA  = RWSPA+ 2
          XNUM(3)= XNUM(3)+ 1
        ELSE IF (FTYP(I).EQ.'I') THEN
C         integer
          RWSPA  = RWSPA+ 1
          XNUM(1)= XNUM(1)+ 1
        ELSE IF (FTYP(I).EQ.'R') THEN
C         decimal
          RWSPA  = RWSPA+ 1
          XNUM(2)= XNUM(2)+ 1
        ELSE
C         characters, need to know space
          J= (FLEN(I)/4)+ 1
          IF (MOD(FLEN(I),4).EQ.0) J= J- 1
          RWSPA  = RWSPA+ J
          XNUM(4)= XNUM(4)+ 1
        END IF
 10   CONTINUE
C
      DO 20 I= 1,4
        IF (XNUM(I).EQ.0) XNUM(I)= 1
 20   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTBSU
     I                    (WDMSFL,DSN,TABMAX,TABBAS,
     O                     TABCNT,TABNAM,TABID,TABDIM,PDATVL,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     get WDM table label info from WDM file table data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,TABMAX,TABBAS,TABCNT,TABID(TABMAX),
     1            TABDIM(TABMAX),PDATVL(TABMAX),RETCOD
      CHARACTER*1 TABNAM(16,TABMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - WDM table data-set number
C     TABMAX - Maximum number of tables to get info about
C     TABBAS - Table base pointer, first group to get info about
C     TABCNT - Total number of tables found
C     TABNAM - Name of table
C     TABID  - Id of table
C     TABDIM - Dimensions of table
C     PDATVL - Pointer to table data values
C     RETCOD - Return code, (+) if more tables than TABMAX
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I16,LREC,LIND,LPOS,PDAT
      CHARACTER*16 CBUF16
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (CBUF16,CBUF1)
      CHARACTER*1  CBUF1(16)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WDSCHK, COPYC
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (4A4)
C
C     + + + END SPECIFICATIONS + + +
C
      I16= 16
C
C     check table data set
      I= 2
      CALL WDSCHK (WDMSFL,DSN,I,
     O             LREC,TABCNT,RETCOD)
C
      TABCNT= TABCNT- TABBAS+ 1
      IF (TABCNT.LE.0) THEN
C       no tables to return info about
        RETCOD= -27
      ELSE IF (TABCNT.GT.TABMAX) THEN
C       more tables exist than we have space for
        RETCOD= 1
        TABCNT= TABMAX
      END IF
C
      IF (RETCOD.GE.0) THEN
C       get label record
        LIND = WDRCGO(WDMSFL,LREC)
        PDAT = WIBUFF(11,LIND)
C       pointer to table names
        LPOS= PDAT+ 2+ (TABBAS- 1)* 7
        DO 10 I= 1,TABCNT
          WRITE (CBUF16,2000) WIBUFF(LPOS,LIND),WIBUFF(LPOS+1,LIND),
     1                        WIBUFF(LPOS+2,LIND),WIBUFF(LPOS+3,LIND)
          CALL COPYC (I16,CBUF1,TABNAM(1,I))
          TABID(I) = WIBUFF(LPOS+4,LIND)
          TABDIM(I)= WIBUFF(LPOS+5,LIND)
          PDATVL(I)= WIBUFF(LPOS+6,LIND)
          LPOS= LPOS+ 7
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBCOD
     I                    (XFLDS,NROW,XSPA,XLEN,XTYP,XCOL,
     I                     TBCBUF,MXTBTL,
     O                     TBRBUF,RETCOD)
C
C     + + + PURPOSE + + +
C     convert data from full screen buffer into WDM internal format
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     XFLDS,NROW,XSPA,XLEN(XFLDS),XCOL(XFLDS),
     1            MXTBTL,RETCOD
      REAL        TBRBUF(MXTBTL)
      CHARACTER*1 XTYP(XFLDS),TBCBUF(80,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XFLDS  - Number of fields in table
C     NROW   - Number of rows in table
C     XSPA   - Number of columns of table data(fields adjusted for size)
C     XLEN   - Length of each field
C     XTYP   - Type(I,R,C,D,A) of each field
C     XCOL   - Starting column for each field
C     TBCBUF - Buffer containing output from full screen routine
C     MXTBTL - Size of buffer to put data in WDM internal format
C     TBRBUF - Buffer to put data in WDM internal format
C     RETCOD - Return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,K1,K2,KX,TBRPOS,LCOL
      REAL        RTMPX(2)
      CHARACTER*4 CTMP
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RTMPX,ITMP),(RTMPX,DTMP),(RTMPX,RTMP)
      INTEGER     ITMP
      REAL        RTMP
      DOUBLE PRECISION   DTMP
C
C     + + + FUNCTIONS + + +
      INTEGER     CHRINT
      REAL        CHRDEC
      DOUBLE PRECISION   CHRDPR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRINT,CHRDEC,CHRDPR
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (4A1)
C
C     + + + END SPECIFICATIONS + + +
C
      TBRPOS= 0
      RETCOD= 0
C     loop to convert rows
      I= 0
 10   CONTINUE
        I= I+ 1
C       loop to convert fields
        J= 0
 20     CONTINUE
          J   = J+ 1
          LCOL= XCOL(J)
          IF (XTYP(J).EQ.'I') THEN
C           integer field
            ITMP  = CHRINT(XLEN(J),TBCBUF(LCOL,I))
            TBRPOS= TBRPOS+ 1
            TBRBUF(TBRPOS)= RTMPX(1)
          ELSE IF (XTYP(J).EQ.'R') THEN
C           decimal field
            RTMP  = CHRDEC(XLEN(J),TBCBUF(LCOL,I))
            TBRPOS= TBRPOS+ 1
            TBRBUF(TBRPOS)= RTMPX(1)
          ELSE IF (XTYP(J).EQ.'D') THEN
C           double precision field
            DTMP  = CHRDPR(XLEN(J),TBCBUF(LCOL,I))
            TBRPOS= TBRPOS+ 1
            TBRBUF(TBRPOS)= RTMPX(1)
            TBRPOS= TBRPOS+ 1
            TBRBUF(TBRPOS)= RTMPX(2)
          ELSE
C           character field
            K= 0
30          CONTINUE
              K1= LCOL+ K
              K2= LCOL+ K+ 3
              K = K+ 4
              IF (K.GT.XLEN(J)) K2= LCOL+ XLEN(J)- 1
              WRITE (CTMP,2000) (TBCBUF(KX,I),KX=K1,K2)
              READ  (CTMP,1000) ITMP
              TBRPOS= TBRPOS+ 1
              TBRBUF(TBRPOS)= RTMPX(1)
            IF (K.LT.XLEN(J)) GO TO 30
          END IF
        IF (J.LT.XFLDS) GO TO 20
      IF (I.LT.NROW.AND.TBRPOS.LT.MXTBTL-XSPA) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBDCD
     I                    (XFLDS,NROW,XSPA,XLEN,XTYP,XCOL,
     I                     TBRBUF,MXTBTL,
     O                     TBCBUF,RETCOD)
C
C     + + + PURPOSE + + +
C     convert data from WDM internal format into full screen buffer
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     XFLDS,NROW,XSPA,XLEN(XFLDS),XCOL(XFLDS),
     1            MXTBTL,RETCOD
      REAL        TBRBUF(MXTBTL)
      CHARACTER*1 XTYP(XFLDS),TBCBUF(80,NROW)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     XFLDS  - Number of fields in table
C     NROW   - Number of rows in table
C     XSPA   - Number of columns of table data(fields adjusted for size)
C     XLEN   - Length of each field
C     XTYP   - Type(I,R,C,D,A) of each field
C     XCOL   - Starting column for each field
C     TBRBUF - Buffer containing data in WDM internal format
C     MXTBTL - Size of buffer to containing data in WDM internal format
C     TBCBUF - Buffer to put data for full screen routine
C     RETCOD - Return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,K1,K2,KX,TBRPOS,LCOL,JUST
      REAL        RTMPX(2)
      CHARACTER*4 CTMP
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RTMPX,ITMP),(RTMPX,DTMP),(RTMPX,RTMP)
      INTEGER     ITMP
      REAL        RTMP
      DOUBLE PRECISION   DTMP
C
C
C     + + + EXTERNALS + + +
      EXTERNAL    INTCHR,DECCHR,DPRCHR
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (4A1)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (A4)
C
C     + + + END SPECIFICATIONS + + +
C
      JUST  = 0
      TBRPOS= 0
      RETCOD= 0
C     loop to convert rows
      I= 0
 10   CONTINUE
        I= I+ 1
C       loop to convert fields
        J= 0
 20     CONTINUE
          J   = J+ 1
          LCOL= XCOL(J)
          IF (XTYP(J).EQ.'I') THEN
C           integer field
            TBRPOS  = TBRPOS+ 1
            RTMPX(1)= TBRBUF(TBRPOS)
            CALL INTCHR (ITMP,XLEN(J),JUST,
     O                   K,TBCBUF(LCOL,I))
          ELSE IF (XTYP(J).EQ.'R') THEN
C           decimal field
            TBRPOS  = TBRPOS+ 1
            RTMPX(1)= TBRBUF(TBRPOS)
            CALL DECCHR (RTMP,XLEN(J),JUST,
     O                   K,TBCBUF(LCOL,I))
          ELSE IF (XTYP(J).EQ.'D') THEN
C           double precision field
            TBRPOS  = TBRPOS+ 1
            RTMPX(1)= TBRBUF(TBRPOS)
            TBRPOS  = TBRPOS+ 1
            RTMPX(2)= TBRBUF(TBRPOS)
            CALL DPRCHR (DTMP,XLEN(J),JUST,
     O                   K,TBCBUF(LCOL,I))
          ELSE
C           character field
            K= 0
30          CONTINUE
              TBRPOS  = TBRPOS+ 1
              RTMPX(1)= TBRBUF(TBRPOS)
              K1= LCOL+ K
              K2= LCOL+ K+ 3
              K = K+ 4
              IF (K.GT.XLEN(J)) K2= LCOL+ XLEN(J)- 1
              WRITE (CTMP,2000) ITMP
              READ  (CTMP,1000) (TBCBUF(KX,I),KX=K1,K2)
            IF (K.LT.XLEN(J)) GO TO 30
          END IF
        IF (J.LT.XFLDS) GO TO 20
      IF (I.LT.NROW.AND.TBRPOS.LT.MXTBTL-XSPA) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBPUT
     I                    (WDMSFL,DSN,TABNAM,TABIND,DATFLG,
     I                     FROW,NROW,FSPA,NSPA,TBRBUF,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     put WDM table data into WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,TABIND,DATFLG,FROW,NROW,FSPA,NSPA,RETCOD
      REAL         TBRBUF(NSPA,NROW)
      CHARACTER*16 TABNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Watershed data management file unit number
C     DSN    - table data-set number
C     TABNAM - table name
C     TABIND - table indentifier number
C     DATFLG - data type
C              1 - main table
C              2 - extension
C     FROW   - first row to start writing
C     NROW   - number of rows of data to write
C     FSPA   - first data space to start writing
C     NSPA   - space for data in each row
C     TBRBUF - buffer for data to write onto WDM file
C     RETCOD - return code
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    LREC,LPOS,LIND,DREC,DPOS,DIND,IDUM,TBCNT,TGRPPT,
     1           PDAT,ASPA,TSPA,TROW,TABID,TABDIM,TABPTR,
     2           CROW,CPOS,LROW,LSPA,XSPA,GROW,GPOS
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDTBFN, WDPTSP, WTBDSP, WDRCGO, WDRCUP, WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
C     check table data set info and get table info
      CALL WDTBFN (WDMSFL,DSN,TABIND,TABNAM,
     O             TBCNT,LREC,TGRPPT,IDUM,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       get label
        LIND= WDRCGO(WDMSFL,LREC)
C       pointer to groups
        PDAT= WIBUFF(11,LIND)
C       point to our group
        LPOS= PDAT+ 2+ (TGRPPT-1)* 7
C       where does the table start
        TABID = WIBUFF(LPOS+4,LIND)
        TABDIM= WIBUFF(LPOS+5,LIND)
        TABPTR= WIBUFF(LPOS+6,LIND)
        CALL WDPTSP (TABPTR,
     O               DREC,DPOS)
        CALL WTBDSP (TABDIM,
     O               IDUM,TROW,TSPA,ASPA)
        IF (FROW.GT.0 .AND. FSPA.GT.0) THEN
C         are we putting the whole main table or extension
          LROW= FROW+ NROW- 1
          LSPA= FSPA+ NSPA- 1
          IF (DATFLG.EQ.1) THEN
C           main table
            XSPA= TSPA
            IF (TROW.LT.LROW .OR. TSPA.LT.LSPA) THEN
C             more than whole table
              RETCOD= -30
            END IF
          ELSE
C           extension table
            XSPA= ASPA
            IF (ASPA.LT.LSPA .OR. LROW.GT.1) THEN
C             more than whole extension
              RETCOD= -31
            END IF
          END IF
        ELSE
C         problems with row/space specs
          RETCOD= -33
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       check data header
        DIND= WDRCGO(WDMSFL,DREC)
        IF (TABID.NE.WIBUFF(DPOS,DIND) .OR.
     1      TABDIM.NE.WIBUFF(DPOS+1,DIND)) THEN
C         big problem, data header doesnt match
          RETCOD= -32
        ELSE
C         skip to data values
          DPOS= DPOS+ 2
          IF (DATFLG.EQ.1) THEN
C           skip to main table
            DPOS= DPOS+ ASPA
          END IF
C
          CROW= 1
          CPOS= 0
          GROW= 1
          GPOS= 0
 10       CONTINUE
C           loop to fill in data values
            RETCOD= 0
            IF (DPOS.GT.512) THEN
C             update current record
              CALL WDRCUP(WDMSFL,DIND)
C             get next record number
              DREC= WIBUFF(4,DIND)
C             bring next record into buffer
              DIND= WDRCGO(WDMSFL,DREC)
              DPOS= DPOS- 508
            END IF
C
            CPOS= CPOS+ 1
            IF (CPOS.GT.XSPA) THEN
C             row complete
              CROW= CROW+ 1
              IF (CROW.GT.LROW) THEN
C               table complete
                RETCOD= 1
              ELSE
C               begin next row
                CPOS= 1
              END IF
            END IF
            IF (CROW.LT.FROW .OR. CPOS.LT.FSPA .OR. CPOS.GT.LSPA) THEN
C             skip this one
              IF (RETCOD.NE.1) RETCOD= -1
            END IF
            IF (RETCOD.EQ.0) THEN
C             get ready to write the next value
              GPOS= GPOS+ 1
              IF (GPOS.GT.NSPA) THEN
                GPOS= 1
                GROW= GROW+ 1
              END IF
              WRBUFF(DPOS,DIND)= TBRBUF(GPOS,GROW)
            END IF
            DPOS= DPOS+ 1
          IF (RETCOD.LE.0) GO TO 10
C         reset return code for the outsize world
          RETCOD= 0
C         save last data record
          CALL WDRCUP(WDMSFL,DIND)
C         set dataset modification date attribute
          CALL WDMODT (WDMSFL,DSN)
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBGET
     I                    (WDMSFL,DSN,TABNAM,TABIND,DATFLG,
     I                     FROW,NROW,FSPA,NSPA,
     O                     TBRBUF,RETCOD)
C
C     + + + PURPOSE + + +
C     get the main table data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,TABIND,DATFLG,FROW,NROW,FSPA,NSPA,RETCOD
      REAL         TBRBUF(NSPA,NROW)
      CHARACTER*16 TABNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Watershed data management file unit number
C     DSN    - table data-set number
C     TABNAM - table name
C     TABIND - table indentifier number
C     DATFLG - data type
C              1 - main table
C              2 - extension
C     FROW   - first row of data to read from
C     NROW   - number of rows of data to read
C     FSPA   - first data space to read from
C     NSPA   - space for data in each row
C     TBRBUF - buffer for data to get from WDM file
C     RETCOD - return code
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    LREC,LPOS,LIND,DREC,DPOS,DIND,IDUM,TBCNT,TGRPPT,
     1           PDAT,ASPA,TSPA,TROW,TABID,TABDIM,TABPTR,
     2           CROW,CPOS,LROW,LSPA,XSPA,GROW,GPOS
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDTBFN, WDPTSP, WTBDSP, WDRCGO
C
C     + + + END SPECIFICATIONS + + +
C
C     check table data set info and get table info
      CALL WDTBFN (WDMSFL,DSN,TABIND,TABNAM,
     O             TBCNT,LREC,TGRPPT,IDUM,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       get label
        LIND= WDRCGO(WDMSFL,LREC)
C       pointer to groups
        PDAT= WIBUFF(11,LIND)
C       point to our group
        LPOS= PDAT+ 2+ (TGRPPT-1)* 7
C       where does the table start
        TABID = WIBUFF(LPOS+4,LIND)
        TABDIM= WIBUFF(LPOS+5,LIND)
        TABPTR= WIBUFF(LPOS+6,LIND)
        CALL WDPTSP (TABPTR,
     O               DREC,DPOS)
        CALL WTBDSP (TABDIM,
     O               IDUM,TROW,TSPA,ASPA)
        IF (FROW.GT.0 .AND. FSPA.GT.0) THEN
C         are we getting the whole main table or extension
          LROW= FROW+ NROW- 1
          LSPA= FSPA+ NSPA- 1
          IF (DATFLG.EQ.1) THEN
C           main table
            XSPA= TSPA
            IF (TROW.LT.LROW .OR. TSPA.LT.LSPA) THEN
C             want more than whole table
              RETCOD= -30
            END IF
          ELSE
C           extension table
            XSPA= ASPA
            IF (ASPA.LT.LSPA .OR. LROW.GT.1) THEN
C             want more than whole extension
              RETCOD= -31
            END IF
          END IF
        ELSE
C         problems with row/space specs
          RETCOD= -33
        END IF
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       check data header
        DIND= WDRCGO(WDMSFL,DREC)
        IF (TABID.NE.WIBUFF(DPOS,DIND) .OR.
     1      TABDIM.NE.WIBUFF(DPOS+1,DIND)) THEN
C         big problem, data header doesnt match
          RETCOD= -32
        ELSE
C         skip to data values
          DPOS= DPOS+ 2
          IF (DATFLG.EQ.1) THEN
C           skip to main table
            DPOS= DPOS+ ASPA
          END IF
C
          CROW= 1
          CPOS= 0
          GROW= 1
          GPOS= 0
 10       CONTINUE
C           loop to get data values
            RETCOD= 0
            IF (DPOS.GT.512) THEN
C             get next record number
              DREC= WIBUFF(4,DIND)
C             bring next record into buffer
              DIND= WDRCGO(WDMSFL,DREC)
              DPOS= DPOS- 508
            END IF
C
            CPOS= CPOS+ 1
            IF (CPOS.GT.XSPA) THEN
C             row complete
              CROW= CROW+ 1
              IF (CROW.GT.LROW) THEN
C               table complete
                RETCOD= 1
              ELSE
C               begin next row
                CPOS= 1
              END IF
            END IF
            IF (CROW.LT.FROW .OR. CPOS.LT.FSPA .OR. CPOS.GT.LSPA) THEN
C             skip this one
              IF (RETCOD.NE.1) RETCOD= -1
            END IF
            IF (RETCOD.EQ.0) THEN
C             get ready to read the next value
              GPOS= GPOS+ 1
              IF (GPOS.GT.NSPA) THEN
                GPOS= 1
                GROW= GROW+ 1
              END IF
              TBRBUF(GPOS,GROW)= WRBUFF(DPOS,DIND)
            END IF
            DPOS= DPOS+ 1
          IF (RETCOD.LE.0) GO TO 10
C         reset return code for the outsize world
          RETCOD= 0
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTBDL
     I                    (WDMSFL,DSN,TABNAM,TABIND,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     delete WDM table from WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,TABIND,RETCOD
      CHARACTER*16 TABNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     DSN    - Table data-set number
C     TABNAM - Table data-set name
C     TABIND - Table identifier
C     RETCOD - Return code
C
C     + + + COMMON BLOCK + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,LREC,LIND,PDAT,LPOS,TABCNT,TGRPPT,NROW,
     1          PDATVL,RIN1,POS1,REC1,RIN2,POS2,REC2,JP1,JP2,
     2          TABDIM,TIND,ASPA,TSPA,LDATVL,TOTSPA,RECN,RECX
C
C     + + + FUNCTIONS + + +
      INTEGER   WDRCGO, WDPTCL, WDRCDL
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDRCGO, WDRCUP, WDPTCL, WDRCDL, WDTBFN, WTBDSP, WDPTSP
      EXTERNAL  WDMODT
C
C     + + + END SPECIFICATIONS + + +
C
C     check data set existance, get record
      CALL WDTBFN (WDMSFL,DSN,TABIND,TABNAM,
     O             TABCNT,LREC,TGRPPT,NROW,RETCOD)
C
      IF (RETCOD.EQ.1) THEN
C       problems, table not found, abort delete
      ELSE
C       do delete
        LIND= WDRCGO(WDMSFL,LREC)
        I   = 11
        PDAT= WIBUFF(I,LIND)
C       determine label position of table to delete
        LPOS= PDAT+ 2+ 7* (TGRPPT- 1)
C       determine start of data to delete
        PDATVL= WIBUFF(LPOS+6,LIND)
C       get start of data to copy over
        CALL WDPTSP (PDATVL,
     O               REC1,POS1)
        RIN1= WDRCGO(WDMSFL,REC1)
        IF (TGRPPT .LT. TABCNT) THEN
C         get start of data to move
          LPOS  = PDAT+ 2+ 7* (TGRPPT)
          LDATVL= WIBUFF(LPOS+6,LIND)
          CALL WDPTSP (LDATVL,
     O                 REC2,POS2)
          RIN2= WDRCGO(WDMSFL,REC2)
C         deleting table in middle of dsn
          DO 30 I= TGRPPT,TABCNT-1
C           be sure label in memory
            LIND= WDRCGO(WDMSFL,LREC)
C           move label
            LPOS= PDAT+ 2+ 7* (I-1)
            DO 10 J= 1,7
              JP1= LPOS+ J- 1
              JP2= JP1+ 7
              WIBUFF(JP1,LIND)= WIBUFF(JP2,LIND)
 10         CONTINUE
            TABDIM= WIBUFF(LPOS+5,LIND)
C           get total space to move this table
            CALL WTBDSP (TABDIM,
     O                   TIND,NROW,TSPA,ASPA)
            TOTSPA= ASPA+ TSPA*NROW+ 2
C           calc new start of the data for this table
            PDATVL= WDPTCL(REC1,POS1)
C           change pointer to data for table being moved
            WIBUFF(LPOS+6,LIND)= PDATVL
C           update label rercord
            CALL WDRCUP(WDMSFL,LIND)
C           move data, copy next table onto space of deleted table
            DO 20 J=1,TOTSPA
              WIBUFF(POS1,RIN1)= WIBUFF(POS2,RIN2)
              POS1= POS1+ 1
              IF (POS1.GT.512) THEN
C               end of record, save record completed
                CALL WDRCUP(WDMSFL,RIN1)
C               get next record
                POS1= 5
                REC1= WIBUFF(4,RIN1)
                RIN1= WDRCGO(WDMSFL,REC1)
              END IF
              POS2= POS2+ 1
              IF (POS2.GT.512) THEN
C               end of record
                POS2= 5
                REC2= WIBUFF(4,RIN2)
                RIN2= WDRCGO(WDMSFL,REC2)
              END IF
 20         CONTINUE
 30       CONTINUE
        END IF
C       clean out rest of current record
        DO 40 I= POS1,512
          WIBUFF(I,RIN1)= 0
 40     CONTINUE
        CALL WDRCUP(WDMSFL,RIN1)
        RECN= WIBUFF(4,RIN1)
        IF (RECN.GT.0) THEN
C         delete any additional records
 50       CONTINUE
C           get rec to delete
            RIN1= WDRCGO(WDMSFL,RECN)
C           get next rec numb(if any)
            RECX= WIBUFF(4,RIN1)
C           delete the record
            I   = WDRCDL(WDMSFL,RECN)
            RECN= RECX
          IF (RECN.NE.0) GO TO 50
        END IF
C
C       be sure label in memory
        LIND= WDRCGO(WDMSFL,LREC)
C       delete label at TABCNT
        LPOS= PDAT+ 2+ 7* (TABCNT-1)
        DO 60 I= 1,7
          WIBUFF(LPOS,LIND)= 0
          LPOS= LPOS+ 1
 60     CONTINUE
C
        TABCNT= TABCNT- 1
        WIBUFF(PDAT,LIND)= TABCNT
        PDATVL= WDPTCL(REC1,POS1)
        WIBUFF(PDAT+1,LIND)= PDATVL
C       update label rercord
        CALL WDRCUP(WDMSFL,LIND)
C       set dataset modification date attribute
        CALL WDMODT (WDMSFL,DSN)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WTBCLN
     I                   (NFLDS,FLEN,
     M                    SCOL)
C
C     + + + PURPOSE + + +
C     Adjust starting columns for table template fields, if necessary.
C     If a table is more than 80 characters wide, the starting columns
C     are likely to be missing or dummy values.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   NFLDS,FLEN(NFLDS),SCOL(NFLDS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NFLDS  - number of fields in the table
C     FLEN   - array of field lengths
C     SCOL   - array of starting columns
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    I,IPOS
C
C     + + + END SPECIFICATIONS + + +
C
C     check first field
      IF (SCOL(1).LE.0) THEN
C       invalid value
        SCOL(1)= 1
      END IF
C
      IF (NFLDS.GT.1) THEN
C       check remaining fields
        IPOS= SCOL(1)
        DO 10 I= 2,NFLDS
C         check starting columns for each field
          IPOS= SCOL(I-1)+ FLEN(I-1)
          IF (IPOS.GT.SCOL(I)) THEN
C           starting column not large enough
            SCOL(I)= IPOS
          END IF
 10     CONTINUE
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WDTBCG
     I                    (MESSFL,WDMSFL,SCLU,SGRP,
     O                     TMESFL,TCLU,TGRP,RETCOD)
C
C     + + + PURPOSE + + +
C     get WDM table template cluster and group number from message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      MESSFL,WDMSFL,SCLU,SGRP,TMESFL,TCLU,TGRP,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - fortran unit number of ANNIE message file
C     WDMSFL - fortran unit number of wdm data file
C     SCLU   - screen cluster of wdm table to check
C     SGRP   - group of wdm table to check
C     TMESFL - unit number of message file with table template
C     TCLU   - Message file cluster containing table template
C     TGRP   - Group number containing table template
C     RETCOD - return code, -22 if template not found
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DONFG
C
C     + + + FUNCTIONS + + +
      INTEGER     WDCKDT
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDCKDT
C
C     + + + END SPECIFICATIONS + + +
C
      TCLU  = SCLU
      TGRP  = SGRP
      TMESFL= 0
C
      RETCOD= 0
      DONFG = 0
C
 10   CONTINUE
        IF (WDCKDT(WDMSFL,TCLU).EQ.9) THEN
C         table template stored with data
          TMESFL = WDMSFL
          DONFG  = 1
        ELSE IF (WDCKDT(MESSFL,TCLU).EQ.9) THEN
C         template stored with application message file
          TMESFL = MESSFL
          DONFG  = 1
        ELSE IF (TCLU.EQ.8 .AND. TGRP.EQ.29) THEN
C         look for table template somewhere else
          TCLU   = 20
          TGRP   = 1
          WRITE(99,*)'changing from 8/29 to 20/1 for table template'
        ELSE IF (TCLU.EQ.122 .AND. TGRP.EQ.1) THEN
C         look for table template somewhere else
          TCLU   = 20
          TGRP   = 1
          WRITE(99,*)'changing from 122/1 to 20/1 for table template'
        ELSE
C         table template is missing
          DONFG  = 1
          RETCOD = -22
        END IF
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
