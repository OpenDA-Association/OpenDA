C
C
C
      SUBROUTINE   PRWMSI
     I                    (WDMSFL,SUCIFL,CLU,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     Import message file clusters (data sets)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,CLU,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for input file
C     CLU    - cluster number being imported to WDM file
C     RETCOD - return code -  0 - no problem,
C                          -150 - errors found and reported on unit 99
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I4,I5,I40,GNUM,
     1             ITYPE,IOFF,IERR,CONT,ITMP
      CHARACTER*1  CGROU(5),BLNK,CLTYP(20),CHTYP(4),CHCLAS(5)
      CHARACTER*40 IBUFF,EBUFF,XBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (IBUF1,IBUFF)
      CHARACTER*1  IBUF1(40)
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND, CHRINT, LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     STRFND, CHRINT, LENSTR, ZIPC, WMSQCK, ZLJUST
      EXTERNAL     PRMSPA, PRMSTI, PRMSMI, PRMSFI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I4,I5,I40/4,5,40/
      DATA BLNK,CGROU/' ','#','G','R','O','U'/
      DATA CHTYP,CHCLAS/'T','Y','P','E','C','L','A','S','S'/
      DATA CLTYP/'P','R','M','1','P','R','M','2','T','E','X','T',
     1           'M','E','N','U','F','I','L','E'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A40)
C
C     + + + OUTPUT FORMATS + + +
 2010 FORMAT (' For group number',I4,' type is ',A4)
 2020 FORMAT ('  WDM FILE PROBLEMS, WDMSFL,CLU,GNUM',3I4)
 2030 FORMAT ('  FOR CLUSTER',I4,' GROUP',I4,' ALREADY EXISTS')
 2040 FORMAT ('  FILE PROBLEMS ON GROUP',I4)
 2050 FORMAT ('  Buffer is ',A40)
C
C     + + + END SPECIFICATIONS + + +
C
      CONT  = 1
      RETCOD= 0
C
 5    CONTINUE
C       find first question, read until #GROUP
        READ (SUCIFL,1000) IBUFF
      IF (STRFND(I5,IBUF1,I5,CGROU).EQ.0) GO TO 5
C
 10   CONTINUE
C       import until END DATA
        IERR = 0
        EBUFF= IBUFF
C       question found
        IBUFF= IBUFF(7:40)
C       find start of 'TYPE'
        ITMP = STRFND(I40,IBUF1,I4,CHTYP)
        IOFF = 4
        IF (ITMP.EQ.0) THEN
C         check for 'CLASS' also (legacy)
          ITMP= STRFND(I40,IBUF1,I5,CHCLAS)
C         need to skip over 5 characters instead of 4
          IOFF= 5
        END IF
        IF (ITMP.GT.0) THEN
C         find end of group number value
          I= LENSTR(ITMP-1,IBUF1)
C         determine group number
          GNUM = CHRINT(I,IBUF1)
          IF (GNUM.GT.0) THEN
C           valid group number, determine type
            I    = ITMP+IOFF
            XBUFF= IBUFF(I:40)
            IBUFF= XBUFF
            CALL ZLJUST (IBUFF)
C           search for current type in valid types string
            I    = 20
            ITMP = STRFND(I,CLTYP,I4,IBUF1)
            IF (ITMP.GT.0) THEN
C             valid type found
              ITYPE= (ITMP/4)+ 1
              WRITE(99,2010) GNUM,IBUFF(1:4)
            ELSE
C             couldn't find valid type
              IERR= 2
            END IF
          ELSE
C           invalid group number
            IERR= 2
          END IF
        ELSE
C         'TYPE' and 'CLASS' not found
          IERR= 2
        END IF
C       clear out buffer
        CALL ZIPC (I40,BLNK,IBUF1)
C
C       check valid cluster and existence of current group number
        CALL WMSQCK (WDMSFL,CLU,GNUM,
     O               IERR)
C
        IF (IERR.EQ.0) THEN
C         put question on WDM file
C
          GO TO (100,200,300,400,500), ITYPE
C
 100      CONTINUE
C           single numeric (PRM1)
            CALL PRMSPA (WDMSFL,CLU,GNUM,SUCIFL,ITYPE,
     M                   IBUFF,CONT,RETCOD)
            GO TO 900
C
 200      CONTINUE
C           multiple numeric (PRM2)
            CALL PRMSPA (WDMSFL,CLU,GNUM,SUCIFL,ITYPE,
     M                   IBUFF,CONT,RETCOD)
            GOTO 900
C
 300      CONTINUE
C           text
            CALL PRMSTI (WDMSFL,CLU,GNUM,SUCIFL,
     M                   IBUFF,CONT,RETCOD)
            GO TO 900
C
 400      CONTINUE
C           menu (logical)
            CALL PRMSMI (WDMSFL,CLU,GNUM,SUCIFL,
     M                   IBUFF,CONT,RETCOD)
            GO TO 900
C
 500      CONTINUE
C           file
            CALL PRMSFI (WDMSFL,CLU,GNUM,SUCIFL,
     M                   IBUFF,CONT,RETCOD)
            GO TO 900
C
 900      CONTINUE
C
        ELSE IF (IERR.LT.0) THEN
C         WDM file problem
          WRITE(99,2020) WDMSFL,CLU,GNUM
          RETCOD= -150
        ELSE IF (IERR.EQ.1) THEN
C         question already exists
          WRITE(99,2030) CLU,GNUM
          RETCOD= -150
        ELSE IF (IERR.EQ.2) THEN
C         problems with import file, find next question or END DATA
          WRITE(99,2040) GNUM
          WRITE(99,2050) EBUFF
          RETCOD= -150
        END IF
C
        IF (IERR.NE.0) THEN
C         find start of next question (or end of data)
 950      CONTINUE
            READ (SUCIFL,1000) IBUFF
          IF (IBUFF(1:5).NE.'#GROU' .AND. IBUFF(1:5).NE.'  END' .AND.
     1        IBUFF(1:4).NE.'END ') GO TO 950
          IF (IBUFF(1:4).EQ.'END ' .OR. IBUFF(1:5).EQ.'  END') CONT= 0
        END IF
C
      IF (CONT.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSPA
     I                   (WDMSFL,CLU,GNUM,SUCIFL,CLASS,
     M                    OBUFF,CONT,RETCOD)
C
C     + + + PURPOSE + + +
C     Import 1-dimensional and 2-dimensional type groups.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,CLU,GNUM,SUCIFL,CLASS,CONT,RETCOD
      CHARACTER*40 OBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     CLU    - cluster number on WDM file
C     GNUM   - group number to put on WDM file
C     SUCIFL - Fortran unit number for import file
C     CLASS  - class of question being imported
C              1 - 1-dimensional
C              2 - 2-dimensional
C     OBUFF  - output buffer, returns last line read to calling routine
C     CONT   - indicator to continue reading sequential file
C              0 - end of cluster
C              1 - more groups to import
C     RETCOD - return code -  0 - no problem,
C                          -150 - errors found and reported on unit 99
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I0,I1,I5,I10,I64,I78,I120,J,K,L,IREC,ISLOT,
     1              SCNFG,VALFG,NXTFG,FSTFG,ID,ORDER,FCOL,FLEN,
     2              LNLIN,PCODE,UCODE,IVAL(4),FTYP,FORDER,FPROT,
     3              ACLU,AGRP,PFLG,UFLG,IPOS,ILEN,TLEN,OPSET,OPDEF,
     4              OPWID,OPHIDE,OPBOX,OPTFLG,CNMSET,CURSET,IRECL,WDMFLG
      CHARACTER*1   CEND(5),ATSIGN(1),CCOMMA(1),BLNK(1),CBLNK
      CHARACTER*7   CSTAT
      CHARACTER*10  CACCES
      CHARACTER*11  CFORM
      CHARACTER*30  FLDNAM
      CHARACTER*78  TBUFF,LBUFF(16)
      CHARACTER*80  NAMES
      CHARACTER*120 IBUFF,XBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (IBUF1,IBUFF),(LBUF1,LBUFF),(XBUF1,XBUFF)
      CHARACTER*1   IBUF1(120),   LBUF1(78,16), XBUF1(120)
      EQUIVALENCE  (RMIN,IVAL(1)),(RMAX,IVAL(2))
      REAL          RMIN,RMAX
C
C     + + + FUNCTIONS + + +
      INTEGER       LENSTR, WMSPIV, WMSP2V, CHRINT, STRFND, CKNBLV
      INTEGER       WMSPOV, ZLNTXT
      REAL          CHRDEC
C
C     + + + INTRINSICS + + +
      INTRINSIC     INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL      LENSTR, WMSPIV, WMSP2V, CHRINT, STRFND, CKNBLV
      EXTERNAL      WMSPOV, ZLNTXT, CHRDEC, ZLJUST, ZTRIM, WMSADI
      EXTERNAL      PRMSIT, FSPARS
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT(A120)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A78)
 2010 FORMAT (' the $SCREEN directive.  The following ',
     1        'directive appears in line ',I4)
 2020 FORMAT (' ERROR: Field',A,' not found in screen text.')
 2030 FORMAT (' WARNING: Field ',A,
     1        ' was found multiple times in screen text.')
 2040 FORMAT (' ERROR: No field type for field:',I4)
 2050 FORMAT (' ERROR: Bad parameter values for field:',I4)
 2051 FORMAT (' Field type     =',I4)
 2052 FORMAT (' Field length   =',I4)
 2053 FORMAT (' Starting column=',I4)
 2060 FORMAT (' Minimum =',I8,'    Maximum =',I8)
 2061 FORMAT (' Minimum =',F10.2,'    Maximum =',F10.2)
 2070 FORMAT (' ERROR: Conditional field',A,
     1        ' not found in screen text.')
 2080 FORMAT (' ERROR: For _HIDE subdirective, field ',A,
     1        ' not found in screen text.')
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I10,I64,I78,I120/0,1,5,10,64,78,120/
      DATA CEND,ATSIGN,CCOMMA,BLNK,CBLNK
     1   /'E','N','D',' ','D','@',',',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
C     read and parse each record
      IVAL(1)= 0
      IVAL(2)= 0
      ORDER  = 0
      NXTFG  = 0
      SCNFG  = 0
      CURSET = 0
      CNMSET = 0
C
 100  CONTINUE
C       look for next directive
        IF (NXTFG .EQ. 0 .OR. IBUF1(1) .NE. '$') THEN
C         read next record
 110      CONTINUE
            I= 0
            READ (SUCIFL,1000) IBUFF
            IREC = IREC + 1
C           left justify
            CALL ZLJUST(IBUFF)
            IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1          STRFND(I10,IBUF1,I5,CEND).EQ.0) THEN
C             cant import it and we're not at the end
              TBUFF= 'ERROR: The following record is not '//
     1               'recognized as a directive.'
              WRITE(99,2000) TBUFF
              TBUFF= 'Record is >> '//IBUFF(1:65)
              WRITE(99,2000) TBUFF
              RETCOD= -150
              I= 1
            END IF
          IF (I.EQ.1) GO TO 110
        END IF
C       processing current directive
        NXTFG= 0
C       which directive
        IF (IBUFF(1:7).EQ.'$SCREEN') THEN
C         fill in the screen text or header
          SCNFG= 1
          LNLIN= 0
 160      CONTINUE
            READ (SUCIFL,1000) IBUFF
            IF (IBUF1(1).EQ.'$' .OR. IBUF1(1).EQ.'#' .OR.
     1          STRFND(I10,IBUF1,I5,CEND).GT.0) THEN
C             done menu
              NXTFG= 1
              IF (LNLIN.EQ.1) THEN
C               there wasn't anything in SCREEN directive
                TBUFF= 'WARNING: Nothing in $SCREEN '//
     1                 'directive to import.'
                WRITE(99,2000) TBUFF
                RETCOD= -150
              END IF
            ELSE
C             put screen text on WDM file
              LNLIN= LNLIN+ 1
              IF (LNLIN.LE.10 .OR.
     1            STRFND(I120,IBUF1,I1,ATSIGN).EQ.0) THEN
                ID= 1
                CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                       I120,IBUF1,I1,IVAL,I0)
C               put text in local buffer
                LBUFF(LNLIN)= IBUFF
              ELSE
C               cant have parameter fields past line 10
                NXTFG= 1
                TBUFF= 'ERROR: Parameter fields must appear '//
     1                 'in line numbers 1 through 10 of'
                WRITE(99,2000) TBUFF
                WRITE(99,2010) LNLIN
                TBUFF= 'Directive is >> '//IBUFF(1:62)
                WRITE(99,2000) TBUFF
                RETCOD= -150
              END IF
            END IF
          IF (LNLIN.LT.16 .AND. NXTFG.EQ.0) GO TO 160
        ELSE IF (IBUFF(1:6).EQ.'$FIELD') THEN
C         field information
          IBUFF = IBUFF(7:80)
          CALL ZLJUST(IBUFF)
          L = LENSTR(I64,IBUF1)
          IF (L .LT. 1) THEN
C           bad field name
            TBUFF= 'WARNING: Problem with $FIELD directive.'
            WRITE(99,2000) TBUFF
            TBUFF= 'Directive is >> '//IBUFF(1:62)
            WRITE(99,2000) TBUFF
            RETCOD= -150
          ELSE IF (SCNFG.EQ.0) THEN
C           screen directive not added yet
            TBUFF= 'WARNING: $SCREEN directive has not been'//
     1             ' imported yet.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          ELSE IF (ORDER.GE.MXFLD) THEN
C           cant add any more fields
            TBUFF= 'WARNING: Can''t import any more'//
     1             ' Fields for this group.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          ELSE
C           add this field
            FLDNAM= IBUFF(1:L)
            ORDER = ORDER + 1
            ID    = 2
            CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                   I120,IBUF1,I1,IVAL,I0)
            IF (CLASS.EQ.1) THEN
C             verify field name is in screen buffer
              I= 0
              K= 0
 170          CONTINUE
                I= I+ 1
                J= STRFND(I78,LBUF1(1,I),L,IBUF1)
                IF (J.GT.0) THEN
C                 field found in screen text
                  K= K+ 1
                END IF
              IF (I.LT.LNLIN) GO TO 170
              IF (K.EQ.0) THEN
C               field was not found, let 'em know
                WRITE(99,2020) IBUFF(1:L)
                RETCOD= -150
              ELSE IF (K.GT.1) THEN
C               field found more than once
                WRITE(99,2030) IBUFF(1:L)
                RETCOD= -150
              END IF
            END IF
C           initialize integer parms which may be used
            IREC = 1
            ISLOT= 1
            PCODE= 0
            UCODE= 0
            PFLG = 0
            UFLG = 0
C           init option type field parameters
            OPSET = 0
            OPDEF = 0
            OPWID = 0
            OPHIDE= 0
            OPBOX = 0
            OPTFLG= 0
C           initialize PRM2 parameters
            FLEN  = 0
            FORDER= 0
            FPROT = 0
            FCOL  = 0
            FTYP  = 0
C
            FSTFG= 0
            VALFG= 0
 200        CONTINUE
C             look for option subdirectives
              IF (NXTFG .EQ. 0) THEN
C               read next record
                READ (SUCIFL,1000) IBUFF
                CALL ZLJUST (IBUFF)
              END IF
              NXTFG= 0
C             process current subdirective
              IF (IBUFF(1:5) .EQ. '_TYPE') THEN
C               parameter data type
                IF (FSTFG.NE.0) THEN
C                 too late to specify type
                  TBUFF= 'The _TYPE subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
                  IBUFF = IBUFF(6:80)
                  CALL ZLJUST(IBUFF)
                  IF (IBUF1(1).EQ.'I') THEN
                    FTYP= 1
                  ELSE IF (IBUF1(1).EQ.'R') THEN
                    FTYP= 2
                  ELSE IF (IBUF1(1).EQ.'D') THEN
                    FTYP= 3
                  ELSE IF (IBUF1(1).EQ.'C') THEN
                    FTYP= 4
                  ELSE IF (IBUF1(1).EQ.'O') THEN
C                   option type field
                    FTYP= 5
                  ELSE IF (IBUF1(1).EQ.'F') THEN
C                   file type field
                    FTYP= 6
C                   init file specifications
                    CSTAT = 'OLD'
                    CACCES= 'SEQUENTIAL'
                    CFORM = 'FORMATTED'
                    IRECL = 0
                    WDMFLG= 0
                  END IF
                END IF
              ELSE IF (IBUFF(1:6).EQ.'_WIDTH' .AND. CLASS.EQ.2) THEN
C               field width for PRM2 type screen
                IF (FSTFG.NE.0) THEN
C                 too late to specify width
                  TBUFF= 'The _WIDTH subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
                  IBUFF= IBUFF(7:80)
                  CALL ZLJUST(IBUFF)
                  L    = LENSTR(I10,IBUF1)
                  FLEN = CHRINT(L,IBUF1)
                END IF
              ELSE IF (IBUFF(1:6).EQ.'_WIDTH' .AND. FTYP.EQ.5) THEN
C               field width for PRM1 type screen, option field only
                IF (OPTFLG.NE.0) THEN
C                 too late to specify Width subdirective
                  TBUFF= 'The _WIDTH subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
C                 process Width subdirective
                  XBUFF= IBUFF(7:80)
                  IBUFF= XBUFF
                  CALL ZLJUST (IBUFF)
                  J    = 2
                  ILEN = LENSTR(J,IBUF1)
                  IF (ILEN.GT.0) THEN
C                   determine width of field to highlight
                    OPWID = CHRINT(ILEN,IBUF1)
                  END IF
                  IF (INDEX(IBUFF(3:80),'QUIET').GT.0) THEN
C                   field name is to be hidden on output
                    OPHIDE= 1
                  END IF
                END IF
              ELSE IF (IBUFF(1:6).EQ.'_ORDER' .AND. CLASS.EQ.2) THEN
C               ordering for field
                IF (FSTFG.NE.0) THEN
C                 too late to specify order
                  TBUFF= 'The _ORDER subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
                  IBUFF= IBUFF(7:80)
                  CALL ZLJUST(IBUFF)
                  IF (IBUF1(1).EQ.'A') THEN
C                   ascending order
                    FORDER= 1
                  ELSE IF (IBUF1(1).EQ.'D') THEN
C                   descending order
                    FORDER= 2
                  END IF
                END IF
              ELSE IF (IBUFF(1:8).EQ.'_PROTECT') THEN
C               protection for field
                IF (FSTFG.NE.0) THEN
C                 too late to specify protection
                  TBUFF= 'The _PROTECT subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
                  IBUFF= IBUFF(9:80)
                  CALL ZLJUST(IBUFF)
                  IF (IBUF1(1).EQ.'C') THEN
C                   correct range for value required
                    FPROT= 1
                  ELSE IF (IBUF1(1).EQ.'P') THEN
C                   field is protected, cant change value
                    FPROT= 2
                  END IF
                END IF
              ELSE IF (IBUFF(1:7).EQ.'_COLUMN' .AND. CLASS.EQ.2) THEN
C               starting column for field
                IF (FSTFG.NE.0) THEN
C                 too late to specify column
                  TBUFF= 'The _COLUMN subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
                  IBUFF= IBUFF(8:80)
                  CALL ZLJUST(IBUFF)
                  L    = LENSTR(I10,IBUF1)
                  FCOL = CHRINT(L,IBUF1)
                END IF
              ELSE IF (IBUFF(1:8) .EQ. '_DEFAULT') THEN
C               default value
                IF (FSTFG.EQ.0) THEN
C                 need to put id 3 parms on WDM file first
                  IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                  ID= 3
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  FSTFG= 1
                  IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C                   no field type for this parm 1 field
                    WRITE(99,2040) ORDER
                    RETCOD= -150
                  ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                     FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C                   bad parameters for parm 2 screen
                    WRITE(99,2050) ORDER
                    WRITE(99,2051) FTYP
                    WRITE(99,2052) FLEN
                    WRITE(99,2053) FCOL
                    RETCOD= -150
                  END IF
                END IF
                IF (VALFG.EQ.0 .AND. FTYP.NE.5) THEN
C                 need to put valid or range on before default
                  TBUFF= 'ERROR: Can''t import Default value'
                  WRITE(99,2000) TBUFF
                  TBUFF= 'No data range or valid values have'//
     1                   ' been specified'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                END IF
                IF (FSTFG.GT.0 .AND. (VALFG.GT.0 .OR. FTYP.EQ.5)) THEN
C                 type and valid range of values already set, default ok
                  IBUFF= IBUFF(9:80)
                  CALL ZLJUST(IBUFF)
                  IF (FTYP.LE.4 .OR. FTYP.EQ.6) THEN
C                   character, numeric, or file field
                    ID = 4
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I1,IVAL,I0)
                  ELSE
C                   option field, save default for later storage
                    IF (IBUFF(1:2).EQ.'ON') THEN
C                     default is for option to be on
                      OPDEF= 1
                    ELSE
C                     default is for option to be off
                      OPDEF= 0
                    END IF
                  END IF
                END IF
              ELSE IF (IBUFF(1:6) .EQ. '_RANGE') THEN
C               range for numeric values
                IF (FTYP.LE.3) THEN
C                 valid subdirective for this field type
                  IBUFF = IBUFF(7:80)
                  CALL ZLJUST(IBUFF)
                  J = INDEX(IBUFF,':')
                  IF (FSTFG.EQ.0) THEN
C                   need to put parms on WDM file first
                    IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                    ID= 3
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I1,IVAL,I1)
                    FSTFG= 1
                    IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C                     no field type for this parm 1 field
                      WRITE(99,2040) ORDER
                      RETCOD= -150
                    ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                       FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C                     bad parameters for parm 2 screen
                      WRITE(99,2050) ORDER
                      WRITE(99,2051) FTYP
                      WRITE(99,2052) FLEN
                      WRITE(99,2053) FCOL
                      RETCOD= -150
                    END IF
                  END IF
                  IF (J.GT.0) THEN
C                   all ok, min value
                    IBUFF(J:J)= ' '
                    L= LENSTR(J,IBUF1)
                    IF (FTYP.EQ.1) THEN
C                     store as integer
                      IVAL(1)= CHRINT(L,IBUF1)
                    ELSE
C                     store as real
                      RMIN= CHRDEC(L,IBUF1)
                    END IF
                    XBUFF= IBUFF(J+1:80)
                    IBUFF= XBUFF
C                   max value
                    CALL ZLJUST(IBUFF)
                    L= LENSTR(I10,IBUF1)
                    IF (FTYP.EQ.1) THEN
C                     store as integer
                      IVAL(2)= CHRINT(L,IBUF1)
                      IF (IVAL(2).LT.IVAL(1) .AND. IVAL(2).GT.-999) THEN
C                       invalid range values
                        TBUFF= 'ERROR: Invalid Range values.'
                        WRITE(99,2000) TBUFF
                        WRITE(99,2060) IVAL(1),IVAL(2)
                        RETCOD= -150
                      END IF
                    ELSE
C                     store as real
                      RMAX= CHRDEC(L,IBUF1)
                      IF (RMAX.LT.RMIN .AND. RMAX.GT.-999.0) THEN
C                       invalid range values
                        TBUFF= 'ERROR: Invalid Range values.'
                        WRITE(99,2000) TBUFF
                        WRITE(99,2061) RMIN,RMAX
                        RETCOD= -150
                      END IF
                    END IF
                    I = 2
                    ID= 5
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I,IVAL,I1)
                    IVAL(1)= 0
                    IVAL(2)= 0
                    VALFG  = 1
                  ELSE
C                   bad range record
                    TBUFF= 'ERROR: problem with _RANGE record'
                    WRITE(99,2000) TBUFF
                    TBUFF= 'Record is >> '//IBUFF(1:65)
                    WRITE(99,2000) TBUFF
                    RETCOD= -150
                  END IF
                ELSE
C                 '_RANGE' not valid subdirective for this type field
                  TBUFF= 'WARNING: _RANGE subdirective being ignored,'//
     1                   ' not valid for this field type.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                END IF
              ELSE IF (IBUFF(1:6) .EQ. '_VALID') THEN
C               valid values
                IF (FSTFG.EQ.0) THEN
C                 need to put parms on WDM file first
                  IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                  ID= 3
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  FSTFG= 1
                  IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C                   no field type for this parm 1 field
                    WRITE(99,2040) ORDER
                    RETCOD= -150
                  ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                     FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C                   bad parameters for parm 2 screen
                    WRITE(99,2050) ORDER
                    WRITE(99,2051) FTYP
                    WRITE(99,2052) FLEN
                    WRITE(99,2053) FCOL
                    RETCOD= -150
                  END IF
                END IF
                IF (FSTFG.GT.0) THEN
C                 type already set, ok
                  IBUFF= IBUFF(7:80)
                  CALL ZLJUST(IBUFF)
                  ID= 6
                  CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,
     I                         CLASS,ID,ORDER,I120,
     M                         IBUF1,NXTFG)
                  VALFG= 1
                  IF (FTYP.EQ.6) THEN
C                   check validity of file specification parameters
                    CALL FSPARS (IBUFF,
     M                           CSTAT,CACCES,CFORM,IRECL,
     O                           NAMES,WDMFLG,RETCOD)
                    IF (RETCOD.NE.0) THEN
C                     problem with file specifications
                      TBUFF= 'Problem with _VALID file specifications'//
     $                       ' for File type data field.'
                      WRITE(99,2000) TBUFF
                      TBUFF= 'Record is >> '//IBUFF(1:65)
                      WRITE(99,2000) TBUFF
                    END IF
                  END IF
                END IF
              ELSE IF (IBUFF(1:8) .EQ. '_INVALID') THEN
C               invalid values
                IF (FSTFG.EQ.0) THEN
C                 need to put parms on WDM file first
                  IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                  ID= 3
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  FSTFG= 1
                  IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C                   no field type for this parm 1 field
                    WRITE(99,2040) ORDER
                    RETCOD= -150
                  ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                     FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C                   bad parameters for parm 2 screen
                    WRITE(99,2050) ORDER
                    WRITE(99,2051) FTYP
                    WRITE(99,2052) FLEN
                    WRITE(99,2053) FCOL
                    RETCOD= -150
                  END IF
                END IF
                IF (FSTFG.GT.0) THEN
C                 type already set, ok
                  IBUFF= IBUFF(9:80)
                  CALL ZLJUST(IBUFF)
                  ID= 7
                  CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,
     I                         CLASS,ID,ORDER,I120,
     M                         IBUF1,NXTFG)
                END IF
              ELSE IF (IBUFF(1:4) .EQ. '_SET') THEN
C               set number for this option
                IF (FTYP.NE.5) THEN
C                 ignore _SET subdirective if not type OPTION
                  TBUFF= 'WARNING: _SET subdirective being ignored as'//
     1                   ' field type is not OPTION.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE IF (OPTFLG.NE.0) THEN
C                 too late to specify set subdirective
                  TBUFF= 'The _SET subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
C                 save set number for later storage
                  XBUFF= IBUFF(5:80)
                  IBUFF= XBUFF
                  CALL ZLJUST (IBUFF)
                  L    = LENSTR(I10,IBUF1)
                  OPSET= CHRINT(L,IBUF1)
                  IF (OPSET.NE.CURSET) THEN
C                   new set number, this will be the first field in set
                    CNMSET= 1
                    CURSET= OPSET
                  ELSE
C                   another field for this set, increment order number
                    CNMSET= CNMSET+ 1
                  END IF
                END IF
              ELSE IF (IBUFF(1:6).EQ.'_ONOFF' .OR.
     1                 IBUFF(1:5).EQ.'_ONON' .OR. IBUFF(1:6).EQ.'_OFFON'
     2            .OR. IBUFF(1:7).EQ.'_OFFOFF') THEN
C               option conditionals for other option fields
                IF (FTYP.NE.5) THEN
C                 ignore directive if not type OPTION
                  TBUFF= 'WARNING: This subdirective being ignored as'//
     1                   ' field type is not OPTION.'
                  WRITE(99,2000) TBUFF
                  TBUFF= 'Directive is >> '//IBUFF(1:62)
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
C                 ok to process conditional records
                  IF (FSTFG.EQ.0) THEN
C                   need to put id 3 parms on WDM file first
                    IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                    ID= 3
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I1,IVAL,I1)
                    FSTFG= 1
                  END IF
                  IF (OPTFLG.EQ.0) THEN
C                   must put integer parms on WDM file first
                    ID= 21
                    IVAL(1)= WMSPOV (OPSET,CNMSET,OPDEF,
     I                               OPWID,OPHIDE,OPBOX)
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I1,IVAL,I1)
                    OPTFLG= 1
                  END IF
C                 make sure conditional fields exist in screen text
                  TLEN= LENSTR(I78,IBUF1)
                  IPOS= STRFND(TLEN,IBUF1,I1,BLNK)+ 1
 300              CONTINUE
C                   find start of next conditional field
                    I= CKNBLV(TLEN-IPOS+1,IBUF1(IPOS))
                    IF (I.GT.0) THEN
C                     start of another conditional field found
                      IPOS= IPOS+ I- 1
C                     determine its length
                      ILEN= STRFND(TLEN-IPOS+1,IBUF1(IPOS),I1,CCOMMA)
                      IF (ILEN.EQ.0) THEN
C                       no comma found, last conditional name on this record
                        ILEN= TLEN- IPOS+ 1
                      ELSE
C                       comma found, dont include in string to search for
                        ILEN= ILEN- 1
                      END IF
                      L= LENSTR(ILEN,IBUF1(IPOS))
C                     look for conditional field in screen text
                      I= 0
                      K= 0
 310                  CONTINUE
                        I= I+ 1
                        J= STRFND(I78,LBUF1(1,I),L,IBUF1(IPOS))
                        IF (J.GT.0) THEN
C                         field found in screen text
                          K= K+ 1
                        END IF
                      IF (I.LT.LNLIN .AND. K.EQ.0) GO TO 310
                      IF (K.EQ.0) THEN
C                       field was not found, let 'em know
                        WRITE(99,2070) IBUFF(IPOS:IPOS+L-1)
                        RETCOD= -150
                      END IF
                      IPOS= IPOS+ ILEN+ 1
                    ELSE
C                     no more conditional fields on record
                      IPOS= TLEN
                    END IF
                  IF (IPOS.LT.TLEN) GO TO 300
C                 put all of record on WDM file
                  ID   = 22
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I0)
                END IF
              ELSE IF (IBUFF(1:5) .EQ. '_BOX') THEN
C               this field to have a box next to it
                IF (OPTFLG.NE.0) THEN
C                 too late to specify Box subdirective
                  TBUFF= 'The _BOX subdirective for field '//FLDNAM
                  WRITE(99,2000) TBUFF
                  TBUFF= 'must be specified earlier in this '//
     1                   'field''s definition.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
C                 process Box subdirective
                  OPBOX= 1
                END IF
              ELSE IF (IBUFF(1:5) .EQ. '_HIDE') THEN
C               hide another field based on this fields value
                IF (FTYP.NE.5) THEN
C                 ignore directive if not type OPTION
                  TBUFF= 'WARNING: This subdirective being ignored as'//
     1                   ' field type is not OPTION.'
                  WRITE(99,2000) TBUFF
                  TBUFF= 'Directive is >> '//IBUFF(1:62)
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                ELSE
C                 ok to do hidden fields
                  IF (FSTFG.EQ.0) THEN
C                   need to put id 3 parms on WDM file first
                    IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                    ID= 3
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I1,IVAL,I1)
                    FSTFG= 1
                  END IF
                  IF (OPTFLG.EQ.0) THEN
C                   must put integer parms on WDM file first
                    ID= 21
                    IVAL(1)= WMSPOV (OPSET,CNMSET,OPDEF,
     I                               OPWID,OPHIDE,OPBOX)
                    CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                           I120,IBUF1,I1,IVAL,I1)
                    OPTFLG= 1
                  END IF
C                 parse record and make sure parts are defined correctly
                  IBUFF= IBUFF(6:80)
                  CALL ZLJUST (IBUFF)
                  IF (IBUFF(1:3).EQ.'ON ' .OR. IBUFF(1:3).EQ.'OFF') THEN
C                   format good so far
                    XBUFF= IBUFF(4:80)
                    CALL ZLJUST(XBUFF)
                    IPOS= INDEX(XBUFF,CBLNK)
                    IF (IPOS.GT.1) THEN
C                     field specified, is it in screen text
                      I= 0
                      K= 0
 330                  CONTINUE
                        I= I+ 1
                        J= INDEX(LBUFF(I),XBUFF(1:IPOS-1))
                        IF (J.GT.0 .AND. XBUFF(1:1).EQ.'@') THEN
C                         field found in screen text
                          K= K+ 1
                        END IF
                      IF (I.LT.LNLIN .AND. K.EQ.0) GO TO 330
                      IF (K.GT.0) THEN
C                       field to hide was found, check row/column specs
                        TLEN= ZLNTXT(XBUFF)
                        IF (TLEN.GT.0) THEN
C                         parse out upper left r/c, then lower right r/c
                          K = 0
                          J = 0
 340                      CONTINUE
                            J= J+ 1
C                           find start of number
                            I= CKNBLV(TLEN-IPOS+1,XBUF1(IPOS))
                            IPOS= IPOS+ I- 1
C                           find length of number
                            ILEN= STRFND(TLEN-IPOS+1,XBUF1(IPOS),
     I                                                   I1,BLNK)- 1
                            IF (J.EQ.4 .AND. ILEN.LE.0) THEN
C                             last number, no trailing blank to find
                              ILEN= TLEN- IPOS+ 1
                            END IF
                            IF (ILEN.GT.0) THEN
C                             get the value
                              IVAL(J)= CHRINT(ILEN,XBUF1(IPOS))
                              IF (IVAL(J).LT.1 .OR. IVAL(J).GT.120) THEN
C                               bad value, problem
                                K= 1
                              END IF
                            ELSE
C                             no value there, problem
                              K= 1
                            END IF
                            IPOS= IPOS+ ILEN
                          IF (J.LT.4 .AND. K.EQ.0) GO TO 340
                          IF (K.NE.0 .OR. IVAL(1).GT.IVAL(3) .OR.
     1                        IVAL(2).GT.IVAL(4)) THEN
C                           problem with row/column specs
                            TBUFF= 'ERROR:  Invalid row/column spec'//
     1                             'ifications for _HIDE subdirective.'
                            WRITE(99,2000) TBUFF
                            RETCOD= -150
                          END IF
                        ELSE
C                         no row/column specs for hidden field
                          TBUFF= 'ERROR:  No row/column specifica'//
     1                           'tions for _HIDE subdirective.'
                          WRITE(99,2000) TBUFF
                          RETCOD= -150
                        END IF
                      ELSE
C                       field was not found, let 'em know
                        WRITE(99,2080) XBUFF(1:IPOS-1)
                        RETCOD= -150
                      END IF
                    ELSE
C                     no field specified
                      TBUFF= 'ERROR:  No field specified for '//
     1                       '_HIDE subdirective.'
                      WRITE(99,2000) TBUFF
                      RETCOD= -150
                    END IF
                  ELSE
C                   no condition specified for hiding field
                    TBUFF= 'ERROR:  No condition specified for '//
     1                     '_HIDE subdirective.'
                    WRITE(99,2000) TBUFF
                    RETCOD= -150
                  END IF
C                 put on WDM file as text
                  ID = 23
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I0)
                END IF
              ELSE IF (IBUFF(1:7) .EQ. '_RECORD') THEN
C               output record info
                IBUFF = IBUFF(8:80)
                CALL ZLJUST(IBUFF)
                L = LENSTR(I10,IBUF1)
                READ (IBUFF(1:L),1000) IREC
              ELSE IF (IBUFF(1:5) .EQ. '_SLOT') THEN
C               output slot info
                IBUFF= IBUFF(6:80)
                CALL ZTRIM(IBUFF)
                I = INDEX(IBUFF,':')
                READ (IBUFF(1:I-1),1000) J
                L = LENSTR(I10,IBUF1)
                READ (IBUFF(I+1:L),1000) ISLOT
                ISLOT= 1000 * J + ISLOT
              ELSE IF (IBUFF(1:5).EQ.'_PACK') THEN
C               number of values on a data record
                IBUFF= IBUFF(6:80)
                CALL ZLJUST(IBUFF)
                L= LENSTR(I10,IBUF1)
                IVAL(1)= CHRINT(L,IBUF1)
                ID   = 8
                CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                       I120,IBUF1,I1,IVAL,I1)
                IVAL(1)= 0
              ELSE IF (IBUFF(1:5) .EQ. '_HELP') THEN
C               parameter help info
                IBUFF= IBUFF(6:80)
                CALL ZLJUST(IBUFF)
                ID = 9
                CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,
     I                       CLASS,ID,ORDER,I120,
     M                       IBUF1,NXTFG)
              ELSE IF (IBUFF(1:7) .EQ. '_PNAME') THEN
C               table parameter name
                IBUFF= IBUFF(7:80)
                CALL ZLJUST(IBUFF)
                ID   = 20
                CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                       I120,IBUF1,I1,IVAL,I0)
              ELSE IF (IBUFF(1:6) .EQ. '_PCODE') THEN
C               parameter code
                IBUFF= IBUFF(7:80)
                CALL ZLJUST(IBUFF)
                L    = LENSTR(I10,IBUF1)
                PCODE= CHRINT(L,IBUF1)
                PFLG = 1
              ELSE IF (IBUFF(1:6) .EQ. '_UCODE') THEN
C               units code
                IBUFF= IBUFF(7:80)
                CALL ZLJUST(IBUFF)
                L    = LENSTR(I10,IBUF1)
                UCODE= CHRINT(L,IBUF1)
                UFLG = 1
              ELSE IF (IBUFF(1:7) .EQ. '_FORMAT') THEN
C               input/output format info
                IF (FSTFG.EQ.0) THEN
C                 need to put parms on WDM file first
                  IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                  ID= 3
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  FSTFG= 1
                  IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C                   no field type for this parm 1 field
                    WRITE(99,2040) ORDER
                    RETCOD= -150
                  ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                     FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C                   bad parameters for parm 2 screen
                    WRITE(99,2050) ORDER
                    WRITE(99,2051) FTYP
                    WRITE(99,2052) FLEN
                    WRITE(99,2053) FCOL
                    RETCOD= -150
                  END IF
                END IF
                IF (FSTFG.GT.0) THEN
C                 type already set, ok
                  IBUFF = IBUFF(8:80)
                  CALL ZLJUST(IBUFF)
                  ID= 11
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I0)
                END IF
              ELSE IF (IBUF1(1).EQ.'$' .OR. IBUF1(1).EQ.'#' .OR.
     1                 STRFND(I10,IBUF1,I5,CEND).GT.0) THEN
C               done subdirectives
                NXTFG= 1
C               need to add integer parms?
                IF (FSTFG.EQ.0) THEN
C                 PRM2 type, need to put parms on WDM file
                  IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
                  ID= 3
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  FSTFG= 1
                  IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C                   no field type for this parm 1 field
                    WRITE(99,2040) ORDER
                    RETCOD= -150
                  ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                     FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C                   bad parameters for parm 2 screen
                    WRITE(99,2050) ORDER
                    WRITE(99,2051) FTYP
                    WRITE(99,2052) FLEN
                    WRITE(99,2053) FCOL
                    RETCOD= -150
                  END IF
                END IF
                IF (IREC.GT.1 .OR. ISLOT.GT.1) THEN
C                 add record and slot parms
                  ID= 8
                  IVAL(1)= WMSPIV (IREC,ISLOT)
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  IVAL(1)= 0
                END IF
                IF (PFLG.NE.0 .OR. UFLG.NE.0) THEN
C                 add table parameter/units codes
                  ID= 10
                  IVAL(1)= WMSPIV (PCODE,UCODE)
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I1)
                  IVAL(1)= 0
                END IF
              ELSE
C               subdirective not importable
                TBUFF= 'ERROR: The following record is not '//
     1                 'recognized as a subdirective.'
                WRITE(99,2000) TBUFF
                TBUFF= 'Record is >> '//IBUFF(1:65)
                WRITE(99,2000) TBUFF
                RETCOD= -150
              END IF
            IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1          STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 200
C
C           need to add integer parms?
            IF (FSTFG.EQ.0) THEN
C             PRM2 type, need to put parms on WDM file
              IVAL(1)= WMSP2V (FTYP,FLEN,FORDER,FPROT,FCOL)
              ID= 3
              CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                     I120,IBUF1,I1,IVAL,I1)
              FSTFG= 1
              IF (FTYP.EQ.0 .AND. CLASS.EQ.1) THEN
C               no field type for this parm 1 field
                WRITE(99,2040) ORDER
                RETCOD= -150
              ELSE IF (CLASS.EQ.2 .AND. (FTYP.EQ.0 .OR.
     1                 FLEN.EQ.0 .OR. FCOL.EQ.0)) THEN
C               bad parameters for parm 2 screen
                WRITE(99,2050) ORDER
                WRITE(99,2051) FTYP
                WRITE(99,2052) FLEN
                WRITE(99,2053) FCOL
                RETCOD= -150
              END IF
            END IF
            IF (FTYP.EQ.5 .AND. OPTFLG.EQ.0) THEN
C             field type is option, store integer parameters
              ID= 21
              IVAL(1)= WMSPOV (OPSET,CNMSET,OPDEF,
     I                         OPWID,OPHIDE,OPBOX)
              CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                     I120,IBUF1,I1,IVAL,I1)
              OPTFLG= 1
            END IF
          END IF
        ELSE IF (IBUFF(1:7) .EQ. '$OUTPUT') THEN
          IBUFF= IBUFF(8:80)
          IF (LENSTR(I64,IBUF1).GT.0) THEN
C           process output file info
            CALL ZLJUST(IBUFF)
            ID= 12
            CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                   I120,IBUF1,I1,IVAL,I0)
 350        CONTINUE
C             look for option subdirectives
              IF (NXTFG .EQ. 0) THEN
C               read next record
                READ (SUCIFL,1000) IBUFF
              END IF
              NXTFG= 0
C             process current subdirective
              IF (IBUFF(1:7) .EQ. '_HEADER') THEN
C               read text of header
                READ (SUCIFL,1000) IBUFF
                IBUFF = IBUFF(8:80)
                CALL ZLJUST(IBUFF)
                ID= 13
                CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                       IBUF1,NXTFG)
              ELSE IF (IBUFF(1:8) .EQ. '_TRAILER') THEN
C               read text of trailer
                READ (SUCIFL,1000) IBUFF
                IBUFF = IBUFF(9:80)
                CALL ZLJUST(IBUFF)
                ID= 14
                CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                       IBUF1,NXTFG)
              ELSE IF (IBUF1(1).EQ.'$') THEN
C               done subdirectives
                NXTFG= 1
              END IF
            IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1          IBUFF(1:3).NE.'END') GO TO 350
          END IF
        ELSE IF (IBUFF(1:5) .EQ. '$HELP') THEN
C         general help for question
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          ID   = 15
          ORDER= 0
          CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                 IBUF1,NXTFG)
        ELSE IF (IBUFF(1:7).EQ.'$HEADER') THEN
C         header for PRM2 type
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          ID   = 16
          ORDER= 0
          CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                 IBUF1,NXTFG)
          SCNFG= 1
        ELSE IF (IBUFF(1:6).EQ.'$TNAME') THEN
C         table name
          IBUFF= IBUFF(7:80)
          CALL ZLJUST(IBUFF)
          ID   = 17
          ORDER= 0
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IVAL,I0)
C         init cluster and group numbers
          ACLU= 0
          AGRP= 0
 400      CONTINUE
C           look for option subdirectives
            IF (NXTFG .EQ. 0) THEN
C             read next record
              READ (SUCIFL,1000) IBUFF
            END IF
            NXTFG= 0
C           process current subdirective
            IF (IBUFF(1:9) .EQ. '_ACLUSTER') THEN
C             get cluster number
              IBUFF= IBUFF(10:80)
              J    = LENSTR(I78,IBUF1)
              ACLU = CHRINT(J,IBUF1)
            ELSE IF (IBUFF(1:7) .EQ. '_AGROUP') THEN
C             get group number
              IBUFF= IBUFF(8:80)
              J    = LENSTR(I78,IBUF1)
              AGRP = CHRINT(J,IBUF1)
            ELSE IF (IBUF1(1).EQ.'$') THEN
C             done subdirectives
              NXTFG= 1
            END IF
          IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1        IBUFF(1:3).NE.'END') GO TO 400
C         now add associated cluster and group
          ID     = 18
          IVAL(1)= WMSPIV (ACLU,AGRP)
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IVAL,I1)
        ELSE IF (IBUFF(1:7).EQ.'$WINDOW') THEN
C         window name for data screen
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          IBUFF= IBUFF(1:48)
          ID   = 19
          ORDER= 0
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IVAL,I0)
        ELSE IF (STRFND(I10,IBUF1,I5,CEND).EQ.0 .AND.
     1           IBUF1(1).NE.'#') THEN
C         not end of group, directive not importable
          TBUFF= 'ERROR: The following record is not '//
     1           'recognized as a directive.'
          WRITE(99,2000) TBUFF
          TBUFF= 'Record is >> '//IBUFF(1:65)
          WRITE(99,2000) TBUFF
          RETCOD= -150
        END IF
      IF (IBUF1(1).NE.'#'.AND.STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 100
C
C     lastly, put terminator on WDM file
      I= -1
      CALL WMSADI (WDMSFL,I,GNUM,CLASS,ID,ORDER,
     I             I120,IBUF1,I1,IVAL,I0)
C
C     end of cluster?
      IF (STRFND(I10,IBUF1,I5,CEND).GT.0) CONT= 0
      OBUFF= IBUFF(1:40)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSTI
     I                   (WDMSFL,CLU,GNUM,SUCIFL,
     M                    OBUFF,CONT,RETCOD)
C
C     + + + PURPOSE + + +
C     Import text type group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,CLU,GNUM,SUCIFL,CONT,RETCOD
      CHARACTER*40 OBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     CLU    - cluster number on WDM file
C     GNUM   - group number to put on WDM file
C     SUCIFL - Fortran unit number for sequential message file
C     OBUFF  - output buffer, returns last line read to calling routine
C     CONT   - indicator to continue reading sequential file
C              0 - end of cluster
C              1 - more groups to import
C     RETCOD - return code -  0 - no problem,
C                          -150 - errors found and reported on unit 99
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I0,I1,I5,I10,I256,NXTFG,
     1              CLASS,ID,ORDER,IDUM(1)
      CHARACTER*1   CEND(5)
      CHARACTER*78  TBUFF
      CHARACTER*256 IBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (IBUF1,IBUFF)
      CHARACTER*1   IBUF1(256)
C
C     + + + FUNCTIONS + + +
      INTEGER       STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL      STRFND, WMSADI, PRMSIT, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I10,I256/0,1,5,10,256/
      DATA CLASS,ORDER/3,0/
      DATA CEND/'E','N','D',' ','D'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A256)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A78)
C
C     + + + END SPECIFICATIONS + + +
C
      NXTFG  = 0
      IDUM(1)= 0
C
 100  CONTINUE
C       look for next directive
        IF (NXTFG.EQ.0 .OR. IBUF1(1).NE.'$') THEN
C         read next record
 110      CONTINUE
            I= 0
            READ (SUCIFL,1000) IBUFF
C           left justify
            CALL ZLJUST(IBUFF)
            IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1          STRFND(I10,IBUF1,I5,CEND).EQ.0) THEN
C             cant import it and we're not at the end
              TBUFF= 'ERROR: The following record is not'//
     1               ' recognized as a directive.'
              WRITE(99,2000) TBUFF
              TBUFF= 'Record is >> '//IBUFF(1:65)
              WRITE(99,2000) TBUFF
              RETCOD= -150
              I= 1
            END IF
          IF (I.EQ.1) GO TO 110
        END IF
C       processing current directive
        NXTFG = 0
C       which directive
        IF (IBUFF(1:5).EQ.'$TEXT') THEN
C         text for screen
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          ID= 1
          CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I256,
     M                 IBUF1,NXTFG)
        ELSE IF (IBUFF(1:5).EQ.'$HELP') THEN
C         help for screen
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          ID= 2
          CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I256,
     M                 IBUF1,NXTFG)
        ELSE IF (IBUFF(1:7).EQ.'$WINDOW') THEN
C         window name for data screen
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          IBUFF= IBUFF(1:48)
          ID   = 19
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I256,IBUF1,I1,IDUM,I0)
        ELSE IF (STRFND(I10,IBUF1,I5,CEND).EQ.0 .AND.
     1           IBUF1(1).NE.'#') THEN
C         not end of group, directive not importable
          TBUFF= 'ERROR: The following record is not'//
     1           ' recognized as a directive.'
          WRITE(99,2000) TBUFF
          TBUFF= 'Record is >> '//IBUFF(1:65)
          WRITE(99,2000) TBUFF
          RETCOD= -150
        END IF
      IF (IBUF1(1).NE.'#' .AND.
     1    STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 100
C
C     terminate question on WDM file
      I= -1
      CALL WMSADI (WDMSFL,I,GNUM,CLASS,ID,ORDER,
     I             I256,IBUF1,I1,IDUM,I0)
C
C     end of cluster?
      IF (STRFND(I10,IBUF1,I5,CEND).GT.0) CONT= 0
      OBUFF= IBUFF(1:40)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSMI
     I                   (WDMSFL,CLU,GNUM,SUCIFL,
     M                    OBUFF,CONT,RETCOD)
C
C     + + + PURPOSE + + +
C     Import menu type group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,CLU,GNUM,SUCIFL,CONT,RETCOD
      CHARACTER*40 OBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     CLU    - cluster number on  WDM file
C     GNUM   - group number to put on WDM file
C     SUCIFL - Fortran unit number for sequential message file
C     OBUFF  - output buffer, returns last line read to calling routine
C     CONT   - indicator to continue reading sequential file
C              0 - end of cluster
C              1 - more groups to import to read
C     RETCOD - return code -  0 - no problem,
C                          -150 - errors found and reported on unit 99
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,K,L,I0,I1,I5,I10,I78,I120,J,NXTFG,INOPT,
     1              ILEN,IDEF,NUMB,IWID,INTFG,TITFG,SCRFG,ICOL,
     2              IVAL(5),LNLIN,CLASS,ID,ORDER,IMENVL(1)
      CHARACTER*1   CEND(5),ATSIGN(1)
      CHARACTER*78  TBUFF,LBUFF(16)
      CHARACTER*120 IBUFF,XBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (IBUF1,IBUFF),(LBUF1,LBUFF)
      CHARACTER*1   IBUF1(120),   LBUF1(78,16)
C
C     + + + FUNCTIONS + + +
      INTEGER       CHRINT, LENSTR, WMSMNV, STRFND, ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL      CHRINT, LENSTR, WMSMNV, STRFND, ZLNTXT
      EXTERNAL      ZLJUST, ZIPI, WMSADI, PRMSIT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I10,I78,I120,CLASS/0,1,5,10,78,120,4/
      DATA CEND,ATSIGN/'E','N','D',' ','D','@'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A120)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A78)
 2005 FORMAT (' the $SCREEN directive.  The following ',
     1        'directive appears in line ',I4)
 2010 FORMAT (' ERROR: Bad value for Option Length, Length is',I4)
 2020 FORMAT (' ERROR: Field',A10,' not found in screen text.')
 2030 FORMAT (' WARNING: Field ',A10,
     1        ' was found multiple times in screen text.')
C
C     + + + END SPECIFICATIONS + + +
C
      NXTFG= 0
      INTFG= 0
      TITFG= 0
      SCRFG= 0
      INOPT= 0
      ILEN = 8
      IDEF = 1
      NUMB = 0
      IWID = 78
      ICOL = 8
      I= 5
      J= 0
      CALL ZIPI (I,J,IVAL)
C
 100  CONTINUE
C       look for next directive
        IF (NXTFG.EQ.0 .OR. IBUF1(1).NE.'$') THEN
C         read next record
 110      CONTINUE
            I= 0
            READ (SUCIFL,1000) IBUFF
C           left justify
            CALL ZLJUST(IBUFF)
            IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1          STRFND(I10,IBUF1,I5,CEND).EQ.0) THEN
C             cant import it and we're not at the end
              TBUFF= 'ERROR: The following record is not'//
     1               ' recognized as a directive.'
              WRITE(99,2000) TBUFF
              TBUFF= 'Record is >> '//IBUFF(1:65)
              WRITE(99,2000) TBUFF
              RETCOD= -150
              I= 1
            END IF
          IF (I.EQ.1) GO TO 110
        END IF
C       processing current directive
        NXTFG = 0
C       which directive
        IF (IBUFF(1:6) .EQ. '$TITLE') THEN
C         title
          IF (SCRFG.EQ.0) THEN
C           screen method not in use, ok to process Title
            ID   = 1
            ORDER= 0
            IBUFF= IBUFF(7:80)
            CALL ZLJUST(IBUFF)
            CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                   I120,IBUF1,I1,IVAL,I0)
            TITFG= 1
          ELSE
C           screen method in use, can't have Title directive
            TBUFF= 'ERROR: Screen directive already in use, '//
     1             '$TITLE directive is not allowed.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          END IF
        ELSE IF (IBUFF(1:7).EQ.'$SCREEN') THEN
C         fill in the screen text
          IF (TITFG.EQ.0) THEN
C           original title method not in use, ok to process Screen
            SCRFG= 1
            LNLIN= 0
            ORDER= 0
 160        CONTINUE
              READ (SUCIFL,1000) IBUFF
              IF (IBUF1(1).EQ.'$' .OR. IBUF1(1).EQ.'#' .OR.
     1            STRFND(I10,IBUF1,I5,CEND).GT.0) THEN
C               done menu
                NXTFG= 1
                IF (LNLIN.EQ.0) THEN
C                 there wasn't anything in SCREEN directive
                  TBUFF= 'WARNING: Nothing in $SCREEN '//
     1                   'directive to import.'
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                END IF
              ELSE
C               put screen text on WDM file
                LNLIN= LNLIN+ 1
                IF (LNLIN.LE.10 .OR.
     1              STRFND(I120,IBUF1,I1,ATSIGN).EQ.0) THEN
                  ID= 21
                  CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                         I120,IBUF1,I1,IVAL,I0)
C                 put text in local buffer
                  LBUFF(LNLIN)= IBUFF
                ELSE
C                 cant have parameter fields past line 10
                  NXTFG= 1
                  TBUFF= 'ERROR: Parameter fields must appear '//
     1                   'in line numbers 1 through 10 of'
                  WRITE(99,2000) TBUFF
                  WRITE(99,2005) LNLIN
                  TBUFF= 'Directive is >> '//IBUFF(1:62)
                  WRITE(99,2000) TBUFF
                  RETCOD= -150
                END IF
              END IF
            IF (LNLIN.LT.16 .AND. NXTFG.EQ.0) GO TO 160
          ELSE
C           original title method in use, can't have Screen directive
            TBUFF= 'ERROR: Title directive already in use, '//
     1             '$SCREEN directive is not allowed.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          END IF
        ELSE IF (IBUFF(1:8) .EQ. '$DEFAULT') THEN
C         default value
          IBUFF= IBUFF(9:80)
          CALL ZLJUST(IBUFF)
          IDEF = CHRINT(LENSTR(I120,IBUF1),IBUF1)
        ELSE IF (IBUFF(1:7) .EQ. '$LENGTH') THEN
C         length of text
          IF (INOPT.GT.0) THEN
C           cant change length after option read
            TBUFF= 'ERROR: Options already defined,'//
     1             ' can''t modify Option Length.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          ELSE
C           save length
            IBUFF= IBUFF(8:80)
            CALL ZLJUST(IBUFF)
            ILEN = CHRINT(LENSTR(I120,IBUF1),IBUF1)
          END IF
        ELSE IF (IBUFF(1:6) .EQ. '$NUMB') THEN
C         numbers on menu
          NUMB= 1
        ELSE IF (IBUFF(1:6) .EQ. '$WIDTH') THEN
C         max width of text and description
          IF (INOPT.GT.0) THEN
C           cant change length after option read
            TBUFF= 'ERROR: Options already defined,'//
     1                  ' can''t modify Width of columns.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          ELSE
C           save column width
            IBUFF= IBUFF(7:80)
            CALL ZLJUST(IBUFF)
            IWID = CHRINT(LENSTR(I120,IBUF1),IBUF1)
          END IF
        ELSE IF (IBUFF(1:9) .EQ. '$COLENGTH') THEN
C         length of text
          IF (INOPT.GT.0) THEN
C           cant change column length after option read
            TBUFF= 'ERROR: Options already defined,'//
     1             ' can''t modify Length of columns.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          ELSE
C           save column
            IBUFF= IBUFF(10:80)
            CALL ZLJUST(IBUFF)
            ICOL = CHRINT(LENSTR(I120,IBUF1),IBUF1)
            IF (ICOL.GT.8) THEN
C             cant fit on screen
              ICOL= 8
              TBUFF= 'WARNING: Column length too large,'//
     1               ' being set to a value of 8.'
              WRITE(99,2000) TBUFF
              RETCOD= -150
            END IF
          END IF
        ELSE IF (IBUFF(1:7) .EQ. '$OPTION') THEN
C         option, see if integer parms need to be taken care of
          IF (INTFG.EQ.0) THEN
C           put integer parms on WDM file
            ID     = 2
            ORDER  = 0
            I      = 1
            IVAL(1)= IDEF
            IF (ILEN.LT.1 .OR. ILEN.GT.63) THEN
C             bad length or not specified yet
              WRITE(99,2010) ILEN
              RETCOD= -150
            END IF
            IVAL(2)= ILEN
            IVAL(3)= NUMB
            IVAL(4)= IWID
            IVAL(5)= ICOL
            IMENVL(1)= WMSMNV (IVAL)
            CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                   I120,IBUF1,I,IMENVL,I1)
            IVAL(1)= 0
            INTFG  = 1
          END IF
C
          INOPT= INOPT + 1
          ID   = 3
          ORDER= INOPT
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          L= ZLNTXT(IBUFF)
          IF (L.GT.ILEN) THEN
C           option longer than specified option lengths
            TBUFF= 'WARNING:  Option '//IBUFF(1:L)
            WRITE(99,2000) TBUFF
            TBUFF= 'is longer than the option length already '//
     1             'specified for this screen.'
            WRITE(99,2000) TBUFF
            RETCOD= -150
          END IF
          IF (SCRFG.EQ.1) THEN
C           Screen method in use, verify field name is in screen buffer
            I= 0
            K= 0
 170        CONTINUE
              I= I+ 1
              J= STRFND(I78,LBUF1(1,I),L,IBUF1)
              IF (J.GT.0) THEN
C               field found in screen text
                IF (IBUF1(1).NE.'@' .AND. J.GT.1) THEN
C                 make sure not matching screen text w/out preceeding @
                  IF (LBUF1(J-1,I).EQ.'@') THEN
C                   @ preceeds match, count it
                    K= K+ 1
                  END IF
                ELSE
C                 we know we have a match
                  K= K+ 1
                END IF
              END IF
            IF (I.LT.LNLIN) GO TO 170
            IF (K.EQ.0) THEN
C             field was not found, let 'em know
              WRITE(99,2020) IBUFF(1:L)
              RETCOD= -150
            ELSE IF (K.GT.1) THEN
C             field found more than once
              WRITE(99,2030) IBUFF(1:L)
              RETCOD= -150
            END IF
          END IF
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IVAL,I0)
 200      CONTINUE
C           look for option subdirectives
            IF (NXTFG .EQ. 0) THEN
C             read next record
              READ (SUCIFL,1000) IBUFF
            END IF
            NXTFG= 0
C           process current subdirective
            IF (IBUFF(1:5) .EQ. '_DESC') THEN
C             description
              ID   = 4
              IBUFF= IBUFF(6:80)
              CALL ZLJUST(IBUFF)
C             how much space available
              I= IWID- (ILEN+ NUMB+ 3+ 1)
              IF (I .GT. 0) THEN
C               use as much of desc as possible
                XBUFF= IBUFF(1:I)
                IBUFF= XBUFF
              END IF
              CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                     I120,IBUF1,I1,IVAL,I0)
            ELSE IF (IBUFF(1:5) .EQ. '_HELP') THEN
C             help info
              IBUFF= IBUFF(6:80)
              CALL ZLJUST(IBUFF)
              ID   = 5
              CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                     IBUF1,NXTFG)
            ELSE IF (IBUF1(1).EQ.'$' .OR. IBUF1(1).EQ.'#' .OR.
     1               STRFND(I10,IBUF1,I5,CEND).GT.0) THEN
C             done directives
              NXTFG= 1
            ELSE
C             subdirective not importable
              TBUFF= 'ERROR: The following record is not'//
     1               ' recognized as a subdirective.'
              WRITE(99,2000) TBUFF
              TBUFF= 'Record is >> '//IBUFF(1:65)
              WRITE(99,2000) TBUFF
              RETCOD= -150
            END IF
          IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1        IBUFF(1:5).NE.'  END') GO TO 200
        ELSE IF (IBUFF(1:5) .EQ. '$HELP') THEN
C         general help for question
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          ID   = 6
          ORDER= 0
          CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                 IBUF1,NXTFG)
        ELSE IF (IBUFF(1:7).EQ.'$WINDOW') THEN
C         window name for data screen
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          IBUFF= IBUFF(1:48)
          ID   = 19
          ORDER= 0
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IVAL,I0)
        ELSE IF (STRFND(I10,IBUF1,I5,CEND).EQ.0 .AND.
     1           IBUF1(1).NE.'#') THEN
C         not end of group, directive not importable
          TBUFF= 'ERROR: The following record is not'//
     1           ' recognized as a directive.'
          WRITE(99,2000) TBUFF
          TBUFF= 'Record is >> '//IBUFF(1:65)
          WRITE(99,2000) TBUFF
          RETCOD= -150
        END IF
      IF (IBUF1(1).NE.'#'.AND.STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 100
C
C     lastly, put terminator on WDM file
      I= -1
      CALL WMSADI (WDMSFL,I,GNUM,CLASS,ID,ORDER,
     I             I120,IBUF1,I1,IVAL,I0)
C
C     end of cluster?
      IF (STRFND(I10,IBUF1,I5,CEND).GT.0) CONT= 0
      OBUFF= IBUFF(1:40)
C
      IF (TITFG.EQ.0 .AND. SCRFG.EQ.0) THEN
C       no title specified, assume they will do so
        TBUFF= 'WARNING: Did not find the $TITLE directive.'
        WRITE(99,2000) TBUFF
        TBUFF= 'Assuming one will be provided in the application.'
        WRITE(99,2000) TBUFF
        RETCOD= -150
      END IF
      IF (INOPT .LT. 1) THEN
C       oops, no options from file
        TBUFF= 'WARNING: Did not find any Options to import.'
        WRITE(99,2000) TBUFF
        TBUFF= 'Assuming they will be provided in the application.'
        WRITE(99,2000) TBUFF
        RETCOD= -150
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSFI
     I                   (WDMSFL,CLU,GNUM,SUCIFL,
     M                    OBUFF,CONT,RETCOD)
C
C     + + + PURPOSE + + +
C     Import file type group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,CLU,GNUM,SUCIFL,CONT,RETCOD
      CHARACTER*40 OBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     CLU    - cluster number on  WDM file
C     GNUM   - group number to put on WDM file
C     SUCIFL - Fortran unit number for sequential message file
C     OBUFF  - output buffer, returns last line read to calling routine
C     CONT   - indicator to continue reading sequential file
C              0 - end of cluster
C              1 - more groups to import
C     RETCOD - return code -  0 - no problem,
C                          -150 - errors found and reported on unit 99
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,J,K,L,I0,I1,I5,I10,I78,I120,LNLIN,
     1              NXTFG,IRECL(1),CLASS,ID,ORDER
      CHARACTER*1   CEND(5),ATSIGN(1)
      CHARACTER*78  TBUFF,LBUFF(16)
      CHARACTER*120 IBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (IBUF1,IBUFF),(LBUF1,LBUFF)
      CHARACTER*1   IBUF1(120),   LBUF1(78,16)
C
C     + + + FUNCTIONS + + +
      INTEGER       CHRINT, LENSTR, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL      CHRINT, LENSTR, STRFND, ZLJUST, WMSADI, PRMSIT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I10,I78,I120,CLASS,ORDER/0,1,5,10,78,120,5,0/
      DATA CEND,ATSIGN/'E','N','D',' ','D','@'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A120)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A78)
 2010 FORMAT (' the $SCREEN directive.  The following ',
     1        'directive appears in line ',I4)
 2020 FORMAT (' ERROR: Field',A10,' not found in screen text.')
C
C     + + + END SPECIFICATIONS + + +
C
      IRECL(1)= 0
      NXTFG   = 0
      LNLIN   = 0
C
 100  CONTINUE
C       look for next directive
        IF (NXTFG.EQ.0 .OR. IBUF1(1).NE.'$') THEN
C         read next record
 110      CONTINUE
            I= 0
            READ (SUCIFL,1000) IBUFF
C           left justify
            CALL ZLJUST(IBUFF)
            IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1          STRFND(I10,IBUF1,I5,CEND).EQ.0) THEN
C             cant import it and we're not at the end
              TBUFF= 'ERROR: The following record is not '//
     1               'recognized as a directive.'
              WRITE(99,2000) TBUFF
              TBUFF= 'Record is >> '//IBUFF(1:59)
              WRITE(99,2000) TBUFF
              RETCOD= -150
              I= 1
            END IF
          IF (I.EQ.1) GO TO 110
        END IF
C       processing current directive
        NXTFG = 0
C       which directive
        IF (IBUFF(1:7) .EQ. '$SCREEN') THEN
C         fill in the screen text for prompting user for file name
          I= 0
 120      CONTINUE
            I= I+ 1
            READ (SUCIFL,1000) IBUFF
            IF (IBUF1(1).EQ.'$' .OR. IBUF1(1).EQ.'#' .OR.
     1          STRFND(I10,IBUF1,I5,CEND).GT.0) THEN
C             done menu
              NXTFG= 1
              IF (I.EQ.1) THEN
C               there wasn't anything in SCREEN directive
                TBUFF= 'WARNING: Nothing in $SCREEN'//
     1                 ' directive to import.'
                WRITE(99,2000) TBUFF
                RETCOD= -150
              END IF
            ELSE
C             put menu text on WDM file
              LNLIN= LNLIN+ 1
              IF (LNLIN.LE.10 .OR.
     1            STRFND(I120,IBUF1,I1,ATSIGN).EQ.0) THEN
                ID= 1
                CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                       I120,IBUF1,I1,IRECL,I0)
C               put text in local buffer
                LBUFF(LNLIN)= IBUFF
              ELSE
C               cant have parameter fields past line 10
                NXTFG= 1
                TBUFF= 'ERROR: Parameter fields must appear '//
     1                 'in line numbers 1 through 10 of'
                WRITE(99,2000) TBUFF
                WRITE(99,2010) LNLIN
                TBUFF= 'Directive is >> '//IBUFF(1:62)
                WRITE(99,2000) TBUFF
                RETCOD= -150
              END IF
            END IF
          IF (I.LT.10 .AND. NXTFG.EQ.0) GO TO 120
        ELSE IF (IBUFF(1:6).EQ.'$NAME') THEN
C         name of file or dummy field name (@FILE)
          ID   = 2
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          L= LENSTR(I78,IBUF1)
          ORDER= 1
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IRECL,I0)
          IF (LNLIN.GT.0) THEN
C           verify file name is in screen buffer
            I= 0
            K= 0
 170        CONTINUE
              I= I+ 1
              J= STRFND(I120,LBUF1(1,I),L,IBUF1)
              IF (J.GT.0) THEN
C               field found in screen text
                K= K+ 1
              END IF
            IF (I.LT.LNLIN) GO TO 170
            IF (K.EQ.0) THEN
C             field was not found, let 'em know
              WRITE(99,2020) IBUFF(1:L)
              RETCOD= -150
            END IF
          END IF
          ORDER= 0
        ELSE IF (IBUFF(1:7).EQ.'$STATUS') THEN
C         file status
          ID   = 3
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IRECL,I0)
        ELSE IF (IBUFF(1:7).EQ.'$ACCESS') THEN
C         file access
          ID   = 4
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IRECL,I0)
        ELSE IF (IBUFF(1:5).EQ.'$FORM') THEN
C         file format
          ID   = 5
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IRECL,I0)
        ELSE IF (IBUFF(1:5).EQ.'$HELP') THEN
C         help for question
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          ID   = 6
          CALL PRMSIT (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,I120,
     M                 IBUF1,NXTFG)
        ELSE IF (IBUFF(1:5).EQ.'$RECL') THEN
C         record length
          ID   = 7
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          IRECL(1)= CHRINT(LENSTR(I120,IBUF1),IBUF1)
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IRECL,I1)
        ELSE IF (IBUFF(1:7).EQ.'$WINDOW') THEN
C         window name for data screen
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          IBUFF= IBUFF(1:48)
          ID   = 19
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 I120,IBUF1,I1,IRECL,I0)
        ELSE IF (STRFND(I10,IBUF1,I5,CEND).EQ.0 .AND.
     1           IBUF1(1).NE.'#') THEN
C         not end of group, directive not importable
          TBUFF= 'ERROR: The following record is not'//
     1                ' recognized as a directive.'
          WRITE(99,2000) TBUFF
          TBUFF= 'Record is >> '//IBUFF(1:65)
          WRITE(99,2000) TBUFF
          RETCOD= -150
        END IF
      IF (IBUF1(1).NE.'#'.AND.STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 100
C
C     lastly, put terminator on WDM file
      I= -1
      CALL WMSADI (WDMSFL,I,GNUM,CLASS,ID,ORDER,
     I             I120,IBUF1,I1,IRECL,I0)
C
C     end of sequential file?
      IF (STRFND(I10,IBUF1,I5,CEND).GT.0) CONT= 0
      OBUFF= IBUFF(1:40)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSIT
     I                   (SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,ILEN,
     M                    IBUF1,NXTFG)
C
C     + + + PURPOSE + + +
C     Put text from any ID of information on WDM file.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SUCIFL,WDMSFL,CLU,GNUM,CLASS,ID,ORDER,ILEN,NXTFG
      CHARACTER*1 IBUF1(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SUCIFL - Fortran unit number for import file
C     WDMSFL - Fortran unit number for WDM file
C     CLU    - cluster number on  WDM file
C     GNUM   - group number to put on WDM file
C     CLASS  - class of information being stored
C     ID     - id for portion of group being stored
C     ORDER  - order of information being stored
C     ILEN   - length of buffer being stored
C     IBUF1  - character array of size ILEN to store
C     NXTFG  - indicator to calling program that buffer
C              contains new information to process
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,I5,I10,IDUM(1)
      CHARACTER*1 CEND(5)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, STRFND, WMSADI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I10/0,1,5,10/
      DATA CEND /'E','N','D',' ','D'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (256A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IDUM(1)= 0
C
      IF (LENSTR(ILEN,IBUF1).GT.0) THEN
C       info begins on this line
        CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I               ILEN,IBUF1,I1,IDUM,I0)
      END IF
C
 100  CONTINUE
C       continue to look for more info
        READ (SUCIFL,1000) (IBUF1(I),I=1,ILEN)
        IF (IBUF1(1).EQ.'$' .OR. IBUF1(1).EQ.'#' .OR.
     1      IBUF1(1).EQ.'_' .OR. STRFND(I10,IBUF1,I5,CEND).NE.0) THEN
C         done with this info
          NXTFG = 1
        ELSE
C         add info
          CALL WMSADI (WDMSFL,CLU,GNUM,CLASS,ID,ORDER,
     I                 ILEN,IBUF1,I1,IDUM,I0)
        END IF
      IF (NXTFG .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   FSPARS
     I                   (FSPTXT,
     M                    CSTAT,CACCES,CFORM,IRECL,
     O                    NAMES,WDMFLG,RETCOD)
C
C     + + + PURPOSE + + +
C     Parse _VALID subdirective for a File type data field
C     into file elements needed to perform Open.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       IRECL,WDMFLG,RETCOD
      CHARACTER*(*) FSPTXT
      CHARACTER*7   CSTAT
      CHARACTER*10  CACCES
      CHARACTER*11  CFORM
      CHARACTER*80  NAMES
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FSPTXT - character array of _VALID subdirective text
C     CSTAT  - character string containing file STATUS
C     CACCES - character string containing file ACCESS
C     CFORM  - character string containing file FORMAT
C     IRECL  - length of record for direct access file
C     NAMES  - character array of valid file names
C     WDMFLG - WDM file indicator, for use when opening file
C              0 - not a WDM file
C              1 - WDM file
C     RETCOD - return code, 0 - all specs ok
C                        -150 - specification mismatch
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      IPOS,LPOS,LPS2,TLEN,LLEN,NAMPOS,NXTSPC
      CHARACTER*1  CCOLON,CCOMMA
      CHARACTER*4  CFMT,CHNAME,CHFORM,CHRECL
      CHARACTER*6  CHSTAT,CHACC
      CHARACTER*20 TMPSTR
C
C     + + + INTRINSICS + + +
      INTRINSIC   INDEX
C
C     + + + FUNCTIONS + + +
      INTEGER     ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    ZLNTXT, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CHNAME, CHFORM, CHRECL,  CHSTAT,  CHACC
     $     /'NAME', 'FORM', 'RECL','STATUS','ACCESS'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('(I',I1,')')
C
C     + + + END SPECIFICATIONS + + +
C
      CCOLON= ':'
      CCOMMA= ','
      NAMES = ' '
      WDMFLG= 0
      RETCOD= 0
C
      TLEN= ZLNTXT(FSPTXT)
C
      IPOS= 1
 10   CONTINUE
C       search through remaining text for next file spec
        LPOS= INDEX(FSPTXT(IPOS:TLEN),CCOLON)
        IF (LPOS.GT.0) THEN
C         colon found, something else to process, adjust by IPOS
          LPOS= LPOS+ IPOS- 1
C         parse out element after keyword
          LPS2= INDEX(FSPTXT(LPOS+1:TLEN),CCOMMA)
          IF (LPS2.EQ.0) THEN
C           no comma found, must be at end of specs
            LPS2= TLEN+ 1
          ELSE
C           adjust by LPOS offset
            LPS2= LPOS+ LPS2
          END IF
          TMPSTR= FSPTXT(LPOS+1:LPS2-1)
          CALL ZLJUST(TMPSTR)
          LLEN= ZLNTXT(TMPSTR)
          IF (INDEX(FSPTXT(IPOS:LPOS-1),CHNAME).GT.0) THEN
C           valid file NAMEs, put first name in output name buffer
            NAMES(1:LLEN)= TMPSTR(1:LLEN)
            NAMPOS= LLEN+ 1
C           see where next specification is
            NXTSPC= INDEX(FSPTXT(LPOS+1:TLEN),CCOLON)
            IF (NXTSPC.GT.0) THEN
C             adjust by LPOS offset
              NXTSPC= NXTSPC+ LPOS
C             look for more NAMEs
 50           CONTINUE
C               parse out any other NAMEs
                LPOS= LPS2+ 1
                LPS2= INDEX(FSPTXT(LPOS:TLEN),CCOMMA)
                IF (LPS2.EQ.0) THEN
C                 at end of specification text
                  LPS2= TLEN+ 1
                ELSE
C                 adjust by LPOS offset
                  LPS2= LPOS+ LPS2- 1
                END IF
                IF (LPS2.LE.NXTSPC) THEN
C                 more NAMEs to process
                  TMPSTR= FSPTXT(LPOS:LPS2-1)
                  CALL ZLJUST(TMPSTR)
                  LLEN  = ZLNTXT(TMPSTR)
                  IF (LLEN.GT.0) THEN
C                   add this name to output name buffer
                    NAMES(NAMPOS:NAMPOS)= ','
                    NAMPOS= NAMPOS+ 1
                    NAMES(NAMPOS:NAMPOS+LLEN)= TMPSTR(1:LLEN)
                    NAMPOS= NAMPOS+ LLEN
                  END IF
                END IF
              IF (LPS2.LT.TLEN .AND. LPS2.LT.NXTSPC) GO TO 50
              IF (LPS2.LT.TLEN) THEN
C               more specifications to parse, reset end position
                LPS2= LPOS- 1
              END IF
            END IF
          ELSE IF (INDEX(FSPTXT(IPOS:LPOS-1),CHSTAT).GT.0) THEN
C           file STATUS information
            CSTAT = TMPSTR(1:LLEN)
          ELSE IF (INDEX(FSPTXT(IPOS:LPOS-1),CHACC).GT.0) THEN
C           file ACCESS information
            CACCES= TMPSTR(1:LLEN)
          ELSE IF (INDEX(FSPTXT(IPOS:LPOS-1),CHFORM).GT.0) THEN
C           file FORMAT information
            CFORM = TMPSTR(1:LLEN)
          ELSE IF (INDEX(FSPTXT(IPOS:LPOS-1),CHRECL).GT.0) THEN
C           file record length information
            IF (INDEX(TMPSTR,'WDM').GT.0) THEN
C             use record length for WDM file depending on machine type
              WDMFLG= 1
            ELSE
C             read record length
              WRITE (CFMT,2000) LLEN
              READ (TMPSTR,CFMT) IRECL
            END IF
          ELSE
C           can't process this keyword
            WRITE (99,*) 'Unable to process FILE type field identifier',
     $                   ' in _VALID subdirective.'
            WRITE (99,*) 'Subdirective is >> ',FSPTXT(IPOS:LPOS-1)
          END IF
          IPOS= LPS2+ 1
        ELSE
C         no more separators found, nothing left to parse
          LPS2= TLEN
        END IF
      IF (LPS2.LT.TLEN) GO TO 10
C
      IF (INDEX(CACCES,'DIRECT').GT.0 .AND. IRECL.EQ.0) THEN
C       direct access file, but no record length specified
        RETCOD= -150
      END IF
C
      RETURN
      END
