C
C
C
      SUBROUTINE   PRWMME
     I                   (WDMSFL,SUCIFL,CLU)
C
C     + + + PURPOSE + + +
C     Export message file clusters (data sets)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,CLU
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for output file
C     CLU    - cluster number being exported from WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,QUCNT,GNUM,QCNT,RETCOD,QCHK,BCWORD,
     1            CLASS,ID,ORDER,TLEN,IPOS,DREC,DPOS,DSTYP
      CHARACTER*1 CLTYP(24)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDSCHK, WMSQCK, WMSFBC, WMSBCS
      EXTERNAL   PRMSTE, PRMSME, PRMSPE, PRMSFE
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CLTYP/'P','R','M','1','P','R','M','2','T','E','X','T',
     1           'M','E','N','U','F','I','L','E','A','T','T','R'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('#GROUP',I5,'  TYPE ',4A1)
 2010 FORMAT ('  DATA   CLU ',I5)
 2020 FORMAT ('  END DATA')
C
C     + + + END SPECIFICATIONS + + +
C
C     check cluster existence
      DSTYP= 9
      CALL WDSCHK (WDMSFL,CLU,DSTYP,
     O             I,QUCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       write beginning data header
        WRITE (SUCIFL,2010) CLU
        GNUM= 0
        QCNT= 0
 5      CONTINUE
C         loop through cluster until exported all questions
          GNUM= GNUM+ 1
          CALL WMSQCK (WDMSFL,CLU,GNUM,
     O                 QCHK)
          IF (QCHK.EQ.1) THEN
C           group exists, export it
            QCNT= QCNT+ 1
            CALL WMSFBC (WDMSFL,CLU,GNUM,
     O                   DREC,DPOS,BCWORD)
C           determine class being exported
            CALL WMSBCS (BCWORD,
     O                   CLASS,ID,ORDER,TLEN)
C           export group number and class
            IPOS= 4*(CLASS-1)+ 1
            WRITE (SUCIFL,2000) GNUM,(CLTYP(I),I=IPOS,IPOS+3)
C
            GO TO (10,20,30,40,50), CLASS
C
 10         CONTINUE
C             1-dimensional parameter (PRM1)
              CALL PRMSPE (WDMSFL,SUCIFL,
     M                     DREC,DPOS,CLASS,ID,ORDER,TLEN)
              GO TO 100
C
 20         CONTINUE
C             2-dimensional parameter (PRM2)
              CALL PRMSPE (WDMSFL,SUCIFL,
     M                     DREC,DPOS,CLASS,ID,ORDER,TLEN)
              GO TO 100
C
 30         CONTINUE
C             text (TEXT)
              CALL PRMSTE (WDMSFL,SUCIFL,
     M                     DREC,DPOS,CLASS,ID,ORDER,TLEN)
              GO TO 100
C
 40         CONTINUE
C             menu (MENU)
              CALL PRMSME (WDMSFL,SUCIFL,
     M                     DREC,DPOS,CLASS,ID,ORDER,TLEN)
              GO TO 100
C
 50         CONTINUE
C             file specifications (FILE)
              CALL PRMSFE (WDMSFL,SUCIFL,
     M                     DREC,DPOS,CLASS,ID,ORDER,TLEN)
              GO TO 100
C
 100        CONTINUE
          END IF
        IF (QCNT.LT.QUCNT) GO TO 5
C
C       write end data
        WRITE (SUCIFL,2020)
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSPE
     I                   (WDMSFL,SUCIFL,
     M                    DREC,DPOS,CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Export 1-dimensional and 2-dimensional type groups.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DREC,DPOS,CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     DREC   - record on WDM file
C     DPOS   - position on record
C     CLASS  - class of block
C     ID     - identifier of block
C     ORDER  - order of block
C     TLEN   - total length of block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,IJ,I8,I10,BCWORD,IVAL(2),FLEN,FORDER,FTYP,FPROT,
     1            FCOL,OPSET,OPDEF,OPWID,OPHIDE,OPBOX,SIGDIG,DECPLA
      CHARACTER*1 HDBUFF(192),TMPSTR(31),BLNK
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSGTO, PRMSTA, WDNXDV, PRTSTR, INTCHR, DECCHX, ZIPC
      EXTERNAL    WMSBCS, WMSPIS, WMSP2S, WMSPOS, CHRCHR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I8,I10,SIGDIG,DECPLA/8,10,5,3/
      DATA BLNK/' '/
      DATA HDBUFF/
     1 '$','S','C','R','E','E','N',' ','$','F','I','E','L','D',' ',' ',
     2 '_','D','E','F','A','U','L','T','_','R','A','N','G','E',' ',' ',
     3 '_','V','A','L','I','D',' ',' ','_','I','N','V','A','L','I','D',
     4 '_','R','E','C','O','R','D',' ','_','S','L','O','T',' ',' ',' ',
     5 '_','P','A','C','K',' ',' ',' ','_','H','E','L','P',' ',' ',' ',
     6 '_','P','C','O','D','E',' ',' ','_','U','C','O','D','E',' ',' ',
     7 '_','F','O','R','M','A','T',' ','$','O','U','T','P','U','T',' ',
     8 '_','H','E','A','D','E','R',' ','_','T','R','A','I','L','E','R',
     9 '$','H','E','L','P',' ',' ',' ','$','H','E','A','D','E','R',' ',
     1 '$','W','I','N','D','O','W',' ','$','T','N','A','M','E',' ',' ',
     2 '_','A','C','L','U','S','T',' ','_','A','G','R','O','U','P',' ',
     3 '_','P','N','A','M','E',' ',' ','_','H','I','D','E',' ',' ',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('_TYPE INTEGER')
 2001 FORMAT ('_TYPE REAL')
 2002 FORMAT ('_TYPE DOUBLE PRECISION')
 2003 FORMAT ('_TYPE CHARACTER')
 2004 FORMAT ('_TYPE OPTION')
 2005 FORMAT ('_TYPE FILE')
 2010 FORMAT (8A1,I10)
 2020 FORMAT (31A1)
 2030 FORMAT ('_WIDTH ',I5)
 2031 FORMAT ('_WIDTH ',I5,'  QUIET')
 2040 FORMAT ('_ORDER ASCENDING')
 2041 FORMAT ('_ORDER DESCENDING')
 2050 FORMAT ('_PROTECT CORRECT')
 2051 FORMAT ('_PROTECT PROTECTED')
 2060 FORMAT ('_COLUMN ',I5)
 2070 FORMAT ('_SET',I5)
 2080 FORMAT ('_DEFAULT OFF')
 2081 FORMAT ('_DEFAULT ON')
 2100 FORMAT ('_BOX')
C
C     + + + END SPECIFICATIONS + + +
C
 5    CONTINUE
C       loop to export blocks
        GO TO (10,20,30,40,50,60,70,80,90,100,110,120,
     1         130,140,150,160,170,180,190,200,210,220,230), ID
C
 10     CONTINUE
C         screen information
          CALL PRTSTR (SUCIFL,I8,HDBUFF)
C         export screen info
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 20     CONTINUE
C         field name
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(9),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 30     CONTINUE
C         get integer parms off next data position
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 IVAL(1))
C         split word into integer parms
          CALL WMSP2S (IVAL(1),
     O                 FTYP,FLEN,FORDER,FPROT,FCOL)
C         field type
          IF (FTYP.EQ.1) THEN
            WRITE (SUCIFL,2000)
          ELSE IF (FTYP.EQ.2) THEN
            WRITE (SUCIFL,2001)
          ELSE IF (FTYP.EQ.3) THEN
            WRITE (SUCIFL,2002)
          ELSE IF (FTYP.EQ.4) THEN
            WRITE (SUCIFL,2003)
          ELSE IF (FTYP.EQ.5) THEN
            WRITE (SUCIFL,2004)
          ELSE IF (FTYP.EQ.6) THEN
            WRITE (SUCIFL,2005)
          END IF
          IF (CLASS.EQ.2) THEN
C           output parameters specific to PRM2 type, starting with field width
            WRITE (SUCIFL,2030) FLEN
C           field order
            IF (FORDER.EQ.1) THEN
              WRITE (SUCIFL,2040)
            ELSE IF (FORDER.EQ.2) THEN
              WRITE (SUCIFL,2041)
            END IF
C           starting column
            WRITE (SUCIFL,2060) FCOL
          END IF
C         field protection allowed for PRM1 or PRM2
          IF (FPROT.EQ.1) THEN
            WRITE (SUCIFL,2050)
          ELSE IF (FPROT.EQ.2) THEN
            WRITE (SUCIFL,2051)
          END IF
          GO TO 900
C
 40     CONTINUE
C         default value, get from next data value
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(17),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 50     CONTINUE
C         value range
          K= 31
          CALL ZIPC (K,BLNK,TMPSTR)
C         put header buffer into temp string
          CALL CHRCHR (I8,HDBUFF(25),TMPSTR)
          K= 0
          DO 55 I= 1,2
C           read min and max
            CALL WDNXDV (WDMSFL,
     M                   DREC,DPOS,
     O                   IVAL(I))
            J= 9+ K
            IF (FTYP.EQ.1) THEN
C             put integer form of range into temp string
              IJ= 1
              CALL INTCHR (IVAL(I),I10,IJ,
     O                     K,TMPSTR(J))
            ELSE
C             put real form of range into temp string
              CALL DECCHX (RVAL(I),I10,SIGDIG,DECPLA,
     O                     TMPSTR(J))
              K= 10
            END IF
            IF (I.EQ.1) THEN
C             put ':' separater between numbers
              K= K+ 1
              TMPSTR(J+K)= ':'
              K= K+ 2
            END IF
  55      CONTINUE
          WRITE (SUCIFL,2020) TMPSTR
          GO TO 900
C
 60     CONTINUE
C         valid responses
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(33),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 70     CONTINUE
C         invalid responses
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(41),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 80     CONTINUE
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
          IF (CLASS.EQ.1) THEN
C           PRM1, record and slot on output file
C           split integer value into record and slot
            CALL WMSPIS (BCWORD,
     O                   IVAL(1),IVAL(2))
C           output record
            WRITE (SUCIFL,2010) (HDBUFF(I),I=49,56),IVAL(1)
C           split slot value into beginning and end
            J = IVAL(2) /1000
            K = IVAL(2) - 1000 * J
            WRITE (SUCIFL,2020) (HDBUFF(I),I=57,64),J,K
          ELSE
C           PRM2, number of values to pack on data record
            WRITE (SUCIFL,2010) (HDBUFF(I),I=65,72),BCWORD
          END IF
          GO TO 900
C
 90     CONTINUE
C         help for this field
          CALL PRTSTR (SUCIFL,I8,HDBUFF(73))
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 100    CONTINUE
C         parameter and units code for field
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
C         split integer value into pcode and ucode
          CALL WMSPIS (BCWORD,
     O                 IVAL(1),IVAL(2))
          WRITE (SUCIFL,2010) (HDBUFF(I),I=81,88),IVAL(1)
          WRITE (SUCIFL,2010) (HDBUFF(I),I=89,96),IVAL(2)
          GO TO 900
C
 110    CONTINUE
C         format for field
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(97),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 120    CONTINUE
C         name of output file
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(105),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 130    CONTINUE
C         header at beginning of output file
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(113),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 140    CONTINUE
C         trailer at end of output file
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(121),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 150    CONTINUE
C         general help for question
          CALL PRTSTR (SUCIFL,I8,HDBUFF(129))
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 160    CONTINUE
C         header for PRM2 screen
          CALL PRTSTR (SUCIFL,I8,HDBUFF(137))
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 170    CONTINUE
C         table name
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(153),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 180    CONTINUE
C         associated cluster and group numbers
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
C         split integer value into cluster and group
          CALL WMSPIS (BCWORD,
     O                 IVAL(1),IVAL(2))
          WRITE (SUCIFL,2010) (HDBUFF(I),I=161,168),IVAL(1)
          WRITE (SUCIFL,2010) (HDBUFF(I),I=169,176),IVAL(2)
          GO TO 900
C
 190    CONTINUE
C         name for screen of data
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(145),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 200    CONTINUE
C         table parameter name
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(177),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 210    CONTINUE
C         get integer parms for option type field off next data position
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 IVAL(1))
C         split word into integer parms
          CALL WMSPOS (IVAL(1),
     O                 OPSET,I,OPDEF,OPWID,OPHIDE,OPBOX)
          WRITE (SUCIFL,2070) OPSET
          IF (OPDEF.EQ.0) THEN
            WRITE (SUCIFL,2080)
          ELSE
            WRITE (SUCIFL,2081)
          END IF
          IF (OPWID.GT.0) THEN
C           output width of field to highlight
            IF (OPHIDE.EQ.0) THEN
              WRITE (SUCIFL,2030) OPWID
            ELSE
C             output QUIET keyword for hidden field
              WRITE (SUCIFL,2031) OPWID
            END IF
          END IF
          IF (OPBOX.EQ.1) THEN
C           output Box subdirective
            WRITE (SUCIFL,2100)
          END IF
          GO TO 900
C
 220    CONTINUE
C         output conditional record
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 230    CONTINUE
C         output hidden field record
          CALL PRMSTA (WDMSFL,SUCIFL,I8,HDBUFF(185),TLEN,
     M                 DREC,DPOS)
          GO TO 900
C
 900    CONTINUE
C
C       get next block control word
        CALL WDNXDV (WDMSFL,
     M               DREC,DPOS,
     O               BCWORD)
        IF (BCWORD.GT.0) THEN
C         more info to come, split block control word
          CALL WMSBCS (BCWORD,
     O                 CLASS,ID,ORDER,TLEN)
        ELSE
          CLASS= 0
        END IF
      IF (CLASS.EQ.1 .OR. CLASS.EQ.2) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSTE
     I                   (WDMSFL,SUCIFL,
     M                    DREC,DPOS,CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Export text type group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DREC,DPOS,CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     DREC   - record on WDM file
C     DPOS   - position of block control word on WDM file record
C     CLASS  - class of block
C     ID     - identifier of block
C     ORDER  - order of block
C     TLEN   - length of block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I7,BCWORD
      CHARACTER*1 HDBUFF(21)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTO, PRTSTR, PRMSTA, WDNXDV, WMSBCS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA HDBUFF/'$','T','E','X','T',' ',' ',
     1            '$','H','E','L','P',' ',' ',
     2            '$','W','I','N','D','O','W'/
C
C     + + + END SPECIFICATIONS + + +
C
      I7= 7
C
 5    CONTINUE
C       loop to export blocks
        IF (ID.EQ.19) THEN
C         reset 'name' id
          ID= 9
        END IF
        GO TO (10,20,100,100,100,100,100,100,90), ID
C
 10     CONTINUE
C         export text
          CALL PRTSTR (SUCIFL,I7,HDBUFF)
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 20     CONTINUE
C         export help for text
          CALL PRTSTR (SUCIFL,I7,HDBUFF(8))
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 90     CONTINUE
C         export window name
          CALL PRMSTA (WDMSFL,SUCIFL,I7,HDBUFF(15),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 100    CONTINUE
C
C       get next block control word
        CALL WDNXDV (WDMSFL,
     M               DREC,DPOS,
     O               BCWORD)
        IF (BCWORD.GT.0) THEN
C         more info to come, split block control word
          CALL WMSBCS (BCWORD,
     O                 CLASS,ID,ORDER,TLEN)
        ELSE
          CLASS= 0
        END IF
      IF (CLASS.EQ.3) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSME
     I                   (WDMSFL,SUCIFL,
     M                    DREC,DPOS,CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Export menu type group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DREC,DPOS,CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     DREC   - record on WDM file
C     DPOS   - position on record
C     CLASS  - class of block
C     ID     - identifier of block
C     ORDER  - order of block
C     TLEN   - total length of block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I3,I9,I12,BCWORD,
     1            DANS,LANS,NUMB,WIDTH,CLEN
      CHARACTER*1 HDBUFF(108),OBUFF(12)
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTO, PRMSTA, WDNXDV, PRTSTR, CHRCHR, INTCHR
      EXTERNAL   WMSBCS, WMSMNS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I3,I9,I12/0,3,9,12/
      DATA HDBUFF/'$','T','I','T','L','E',' ',' ',' ',
     1            '$','D','E','F','A','U','L','T',' ',
     2            '$','L','E','N','G','T','H',' ',' ',
     3            '$','N','U','M','B','E','R',' ',' ',
     4            '$','W','I','D','T','H',' ',' ',' ',
     5            '$','C','O','L','E','N','G','T','H',
     6            '$','O','P','T','I','O','N',' ',' ',
     7            '_','D','E','S','C',' ',' ',' ',' ',
     8            '_','H','E','L','P',' ',' ',' ',' ',
     9            '$','H','E','L','P',' ',' ',' ',' ',
     1            '$','W','I','N','D','O','W',' ',' ',
     2            '$','S','C','R','E','E','N',' ',' '/
C
C     + + + END SPECIFICATIONS + + +
C
 5    CONTINUE
C       loop to export blocks
        IF (ID.EQ.19) THEN
C         reset 'name' id
          ID= 7
        ELSE IF (ID.EQ.21) THEN
C         reset 'Screen' id
          ID= 8
        END IF
        GO TO (10,20,30,40,50,60,70,80), ID
C
 10     CONTINUE
C         menu title
          CALL PRMSTA (WDMSFL,SUCIFL,I9,HDBUFF,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 20     CONTINUE
C         integer parms, get the data value
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
C         split word into integer parms
          CALL WMSMNS (BCWORD,
     O                 DANS,LANS,NUMB,WIDTH,CLEN)
          IF (DANS.GT.0) THEN
C           export default
            CALL CHRCHR (I9,HDBUFF(10),OBUFF)
            CALL INTCHR (DANS,I3,I0,
     O                   I,OBUFF(10))
            CALL PRTSTR (SUCIFL,I12,OBUFF)
          END IF
C         export option length
          CALL CHRCHR (I9,HDBUFF(19),OBUFF)
          CALL INTCHR (LANS,I3,I0,
     O                 I,OBUFF(10))
          CALL PRTSTR (SUCIFL,I12,OBUFF)
          IF (NUMB.GT.0) THEN
C           export numbering flag
            CALL PRTSTR (SUCIFL,I9,HDBUFF(28))
          END IF
          IF (WIDTH.GT.0) THEN
C           export column width
            CALL CHRCHR (I9,HDBUFF(37),OBUFF)
            CALL INTCHR (WIDTH,I3,I0,
     O                   I,OBUFF(10))
            CALL PRTSTR (SUCIFL,I12,OBUFF)
          END IF
          IF (CLEN.GT.0) THEN
C           export column length
            CALL CHRCHR (I9,HDBUFF(46),OBUFF)
            CALL INTCHR (CLEN,I3,I0,
     O                   I,OBUFF(10))
            CALL PRTSTR (SUCIFL,I12,OBUFF)
          END IF
          GO TO 100
C
 30     CONTINUE
C         option
          CALL PRMSTA (WDMSFL,SUCIFL,I9,HDBUFF(55),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 40     CONTINUE
C         option description
          CALL PRMSTA (WDMSFL,SUCIFL,I9,HDBUFF(64),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 50     CONTINUE
C         option help
          I= 5
          CALL PRTSTR (SUCIFL,I,HDBUFF(73))
C         export help
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 60     CONTINUE
C         general help
          CALL PRTSTR (SUCIFL,I9,HDBUFF(82))
C         export help
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 70     CONTINUE
C         name for screen of data
          CALL PRMSTA (WDMSFL,SUCIFL,I9,HDBUFF(91),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 80     CONTINUE
C         screen text
          CALL PRTSTR (SUCIFL,I9,HDBUFF(100))
C         export screen text
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 100    CONTINUE
C
C       get next block control word
        CALL WDNXDV (WDMSFL,
     M               DREC,DPOS,
     O               BCWORD)
        IF (BCWORD.GT.0) THEN
C         more info to come, split block control word
          CALL WMSBCS (BCWORD,
     O                 CLASS,ID,ORDER,TLEN)
        ELSE
          CLASS= 0
        END IF
      IF (CLASS.EQ.4) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSFE
     I                   (WDMSFL,SUCIFL,
     M                    DREC,DPOS,CLASS,ID,ORDER,TLEN)
C
C     + + + PURPOSE + + +
C     Export file type group.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DREC,DPOS,CLASS,ID,ORDER,TLEN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     DREC   - record on WDM file
C     DPOS   - position on record
C     CLASS  - class of block
C     ID     - identifier of block
C     ORDER  - order of block
C     TLEN   - total length of block
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I5,I7,I12,BCWORD,IRECL
      CHARACTER*1 HDBUFF(56),OBUFF(12)
C
C     + + + EXTERNALS + + +
      EXTERNAL    PRMSTA, WDNXDV, PRTSTR, CHRCHR, INTCHR, WMSBCS, WMSGTO
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I5,I7,I12/0,5,7,12/
      DATA HDBUFF/'$','S','C','R','E','E','N',
     1            '$','N','A','M','E',' ',' ',
     2            '$','S','T','A','T','U','S',
     3            '$','A','C','C','E','S','S',
     4            '$','F','O','R','M',' ',' ',
     5            '$','H','E','L','P',' ',' ',
     6            '$','R','E','C','L',' ',' ',
     7            '$','W','I','N','D','O','W'/
C
C     + + + END SPECIFICATIONS + + +
C
 5    CONTINUE
C       loop to export blocks
        IF (ID.EQ.19) THEN
C         reset window 'name' id
          ID= 9
        END IF
        GO TO (10,20,30,40,50,60,70,100,90), ID
C
 10     CONTINUE
C         text to prompt user for file name
          CALL PRTSTR (SUCIFL,I7,HDBUFF)
C         export text
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 20     CONTINUE
C         name of file
          CALL PRMSTA (WDMSFL,SUCIFL,I7,HDBUFF(8),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 30     CONTINUE
C         file status
          CALL PRMSTA (WDMSFL,SUCIFL,I7,HDBUFF(15),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 40     CONTINUE
C         file access
          CALL PRMSTA (WDMSFL,SUCIFL,I7,HDBUFF(22),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 50     CONTINUE
C         file format
          CALL PRMSTA (WDMSFL,SUCIFL,I7,HDBUFF(29),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 60     CONTINUE
C         help for question
          CALL PRTSTR (SUCIFL,I7,HDBUFF(36))
C         export help
          CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 70     CONTINUE
C         file record length
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 IRECL)
          CALL CHRCHR (I7,HDBUFF(43),OBUFF)
          CALL INTCHR (IRECL,I5,I0,
     O                 I,OBUFF(8))
          CALL PRTSTR (SUCIFL,I12,OBUFF)
          GO TO 100
C
 90     CONTINUE
C         name for screen of data
          CALL PRMSTA (WDMSFL,SUCIFL,I7,HDBUFF(50),TLEN,
     M                 DREC,DPOS)
          GO TO 100
C
 100    CONTINUE
C
C       get next block control word
        CALL WDNXDV (WDMSFL,
     M               DREC,DPOS,
     O               BCWORD)
        IF (BCWORD.GT.0) THEN
C         more info to come, split block control word
          CALL WMSBCS (BCWORD,
     O                 CLASS,ID,ORDER,TLEN)
        ELSE
          CLASS= 0
        END IF
      IF (CLASS.EQ.5) GO TO 5
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSTA
     I                    (WDMSFL,SUCIFL,ILEN,CHDR,TLEN,
     M                     DREC,DPOS)
C
C     + + + PURPOSE + + +
C     export block with header (CHDR) and information on same line
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,SUCIFL,ILEN,TLEN,DREC,DPOS
      CHARACTER*1 CHDR(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     ILEN   - length of header
C     CHDR   - header for info being exported
C     TLEN   - total length of information
C     DREC   - record number on WDM file
C     DPOS   - position on record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,CONT,I120,I130,HLEN,OLEN,GLEN,CLEN,MORE
      CHARACTER*1 OBUFF(130),BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER    LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL   LENSTR, WMSGTE, PRTSTR, CHRINS
C
C     + + + END SPECIFICATIONS + + +
C
      I120= 120
      I130= 130
      BLNK= ' '
C     get buffer of information
      GLEN= 0
      CLEN= 0
      MORE= 0
C
 5    CONTINUE
        CALL WMSGTE (WDMSFL,TLEN,I120,
     M               DREC,DPOS,GLEN,CLEN,
     O               OLEN,OBUFF,CONT)
C
        IF (MORE.EQ.0) THEN
C         will be outputting header, put blank in first position
          I= 1
          CALL CHRINS (I130,I,BLNK,OBUFF)
C         now insert header at beginning of buffer
          HLEN= LENSTR(ILEN,CHDR)
          DO 10 I= 1,HLEN
            CALL CHRINS (I130,I,CHDR(I),OBUFF)
 10       CONTINUE
          MORE= 1
        ELSE
C         more than one line of output, won't need header
          HLEN= -1
        END IF
C
C       export buffer
        OLEN= OLEN+ HLEN+ 1
        CALL PRTSTR (SUCIFL,OLEN,OBUFF)
C
      IF (CONT.EQ.1) GO TO 5
C
      RETURN
      END
