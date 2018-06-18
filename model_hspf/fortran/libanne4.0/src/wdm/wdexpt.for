C
C
C
      SUBROUTINE   PRWMTE
     I                    (WDMSFL,SUCIFL,DSN,LSDAT,LEDAT)
C
C     + + + PURPOSE + + +
C     Export timeseries data.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,SUCIFL,DSN,LSDAT(6),LEDAT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     SUCIFL - Fortran unit number of sequential file
C     DSN    - dataset containing timeseries data to export
C     LSDAT  - export start date
C     LEDAT  - export end date
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER    J,I1,GPFLG,DSFREC,RETCOD,RIND,J1,J2,DONFG,
     1           TUNIT,TSTEP,TDAT(6),BADJFG,ADDAFG,LGRPFG,
     2           TGROUP,LTSPT,GPIND,GPOSEN,GPSDAT(6),GPEDAT(6),
     3           CURREC,CURBKS,CURPOS,CURTST,CURTUN,CURCMP,
     4           CURQUA,CURDAT(6),EGPFG,BLSDAT(6),BLEDAT(6),NUMSKP,
     5           OLDNOV
      INTEGER*4  NVAL,CURNOV,CURCNT,TMPNOV
      REAL       LTSFIL,LTOLR,CURVAL,PREVAL
C
C     + + + FUNCTIONS + + +
      INTEGER    WDRCGO, TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL   WDRCGO, TIMCHK, TIMADD, WTFNDG, WTSKVX
      EXTERNAL   TIMDFX, TIMDIF, WBCWSP, WDATCP, WDSKBK, WTEGRP
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA       STARTS: ',I4,5(1X,I2),
     1                     '  ENDS: ',I4,5(1X,I2))
C2010 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X,2(1PG12.5))
C2020 FORMAT (3X,6(1PG12.5))
C2010 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X,2(1PG12.6))
C2020 FORMAT (3X,6(1PG12.6))
 2010 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X, 2G12.6 )
 2020 FORMAT (3X, 6G12.6 )
 2030 FORMAT (2X,'END DATA')
C
C     + + + END SPECIFICATIONS + + +
C
      I1    = 1
      GPFLG = 1
      ADDAFG= 0
      BADJFG= 0
      LGRPFG= 0
C
C     output data start/end record with date
      WRITE (SUCIFL,2000) LSDAT,LEDAT
C
C     get dummy nval, units and timestep
      CALL TIMDFX (LSDAT,LEDAT,
     O             NVAL,TUNIT,TSTEP)
C
C     get info about timser dsn
      CALL WTFNDG (WDMSFL,DSN,GPFLG,LSDAT,TSTEP,TUNIT,NVAL,
     O             DSFREC,LTSFIL,TGROUP,LTOLR,LTSPT,
     O             GPIND,GPOSEN,GPSDAT,TDAT,RETCOD)
C
      CALL WDATCP (LSDAT,CURDAT)
      CALL WDATCP (LSDAT,TDAT)
      GPIND= GPIND- 1
 10   CONTINUE
C       group loop
        GPIND= GPIND+ 1
Ckmf    WRITE(99,*) 'group loop',GPIND
C       find out the end of the group
        CALL WTEGRP (GPSDAT,TGROUP,
     O               GPEDAT)
        IF (GPIND.EQ.GPOSEN) THEN
C         last group, dont dump too far
          CALL WDATCP (LEDAT,GPEDAT)
          LGRPFG= 1
Ckmf      WRITE(99,*) 'last group',LEDAT
        END IF
C       skip as required within group, dont need VBTIME because
C       ADDAFG is 0
        CALL WTSKVX (WDMSFL,GPIND,GPSDAT,TDAT,
     I               DSFREC,LTSFIL,TGROUP,BADJFG,ADDAFG,ADDAFG,
     O               CURREC,CURBKS,CURPOS,CURNOV,CURVAL,PREVAL,
     O               CURTST,CURTUN,CURCMP,CURQUA,CURCNT,CURDAT,
     O               RETCOD,BLSDAT,BLEDAT)
        IF (RETCOD.EQ.-10) THEN
C         missing group in span of data, write missing val
          CURQUA= 31
          WRITE (SUCIFL,2010) GPSDAT,TGROUP,I1,CURQUA,I1,I1,LTSFIL
          CALL WDATCP (GPEDAT,GPSDAT)
          CALL WDATCP (GPEDAT,TDAT)
        ELSE
C         be sure wdmsfl record in memory
          RIND= WDRCGO(WDMSFL,CURREC)
C
C         loop to write out requested values
 20       CONTINUE
C
            EGPFG= TIMCHK(BLEDAT,GPEDAT)
C
            IF (LGRPFG.EQ.1 .AND. EGPFG.LE.0) THEN
C             end of last group, recalc number of values
              OLDNOV = CURNOV
              CALL TIMDIF (CURDAT,GPEDAT,CURTUN,CURTST,
     O                     J)
              CURNOV= J+ CURCNT- 1
              IF (OLDNOV .NE. CURNOV) THEN
                WRITE(99,*) 'adjust curnov',OLDNOV
                WRITE(99,*) '           to',CURNOV
              END IF
            END IF
C
            TMPNOV= CURNOV- CURCNT+ 1
C
            IF (TMPNOV.GT.0) THEN
C             values to write, do it
              IF (CURCMP.EQ.1) THEN
C               compressed format
                WRITE (SUCIFL,2010) CURDAT,CURTUN,CURTST,CURQUA,
     1                          TMPNOV,CURCMP,WRBUFF(CURPOS,RIND)
              ELSE
C               uncompressed format
                J1= CURCNT
                J2= J1+ 1
                DONFG= 0
 30             CONTINUE
                  IF (J2.GE.CURNOV) THEN
                    DONFG= 1
                    J2= CURNOV
                  END IF
                  IF (J1.EQ.CURCNT) THEN
C                   first write
                    WRITE (SUCIFL,2010)
     1                CURDAT,CURTUN,CURTST,CURQUA,TMPNOV,CURCMP,
     2                (WRBUFF(J,RIND),J=CURBKS+J1,CURBKS+J2)
                  ELSE
                    WRITE (SUCIFL,2020)
     1                (WRBUFF(J,RIND),J=CURBKS+J1,CURBKS+J2)
                  END IF
                  J1= J2+ 1
                  J2= J1+ 5
                IF (DONFG.EQ.0) GO TO 30
C
              END IF
            END IF
C
            CALL WDATCP (BLEDAT,CURDAT)
            IF (EGPFG.GT.0) THEN
C             get next block
              NUMSKP= 2
              IF (CURCMP.EQ.0) NUMSKP= CURNOV+ 1
              CALL WDSKBK (WDMSFL,NUMSKP,
     O                     CURREC,CURBKS)
              CURPOS= CURBKS+ 1
C             be sure record is in buffer
              RIND= WDRCGO (WDMSFL,CURREC)
C             split up bcw
              CALL WBCWSP (WIBUFF(CURBKS,RIND),
     O                     CURNOV,CURTST,CURTUN,CURCMP,CURQUA)
C             calc new end of block
              CALL TIMADD (CURDAT,CURTUN,CURTST,CURNOV,
     O                     BLEDAT)
              CURCNT= 1
            ELSE
C             we are at end of group
              CALL WDATCP (GPEDAT,GPSDAT)
              CALL WDATCP (GPEDAT,TDAT)
            END IF
          IF (EGPFG.GT.0) GO TO 20
        END IF
      IF (GPIND.LT.GPOSEN) GO TO 10
C
C     output end record
      WRITE (SUCIFL,2030)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMXE
     I                   (MESSFL,WDMSFL,SUCIFL,DSN)
C
C     + + + PURPOSE + + +
C     Export table datasets.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,WDMSFL,SUCIFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SUCIFL - Fortran unit number of sequential file
C     DSN    - dataset containing timeseries data to export
C
C     + + + PARAMETERS + + +
      INTEGER      MXTROW,   MXTLEN
      PARAMETER   (MXTROW=300,MXTLEN=MXTROW*20)
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'ctblab.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I1,I3,I80,ITBL,TABIND,NROW,NCOL,NEXT,RETCOD,
     1             IEXIST,DATFLG,MXPOS,BFLDS,LEN,BCWORD,
     2             TCLU,TGRP,TFLDS,TLEN(30),TCOL(30),TSPA,TNUM(4),
     3             ACLU,AGRP,AFLDS,ALEN(30),ACOL(30),ASPA,ANUM(4),
     4             BSPA,IPOS,IFLD,FFLD,FSPA,XNUM(4),ID,DREC,DPOS,
     5             MLEN,GLEN,OLEN,FLEN,CONT,LMESFL,LCLU,LGRP
      REAL         RBUFF(MXTLEN)
      CHARACTER*1  TTYP(30),ATYP(30),MFID(2),TBUFF(80,MXTROW),BLNK(1),
     1             CSTAR(3),BK3(3)
      CHARACTER*16 MTBNAM,CTBNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      LENSTR, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     LENSTR, STRFND, WDTBSU, WTBDSP, WTBISP, WMSQCK
      EXTERNAL     WDTBSP, WTBGET, WTBDCD, WTBCLN, WTBSPA, WMSIDP
      EXTERNAL     WDPRPS, WDNXDV, WMSGTE, WMSBCS, CHRCHR, CHRINS
      EXTERNAL     CARVAR, ZIPC, WDTBCG
C
C     + + + DATA INITIALIZATIONS + + +
      DATA BK3,CSTAR/' ',' ',' ','*','*','*'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA  name: ',16A1,'  ind ',I4,'  mid  ',2A1,
     1        '  clu ',I4,'  grp ',I4,'  nrw ',I4)
 2010 FORMAT ('    EXTENSION DATA')
 2015 FORMAT ('    END EXTENSION DATA')
 2020 FORMAT ('    MAIN DATA')
 2025 FORMAT ('    END MAIN DATA')
 2030 FORMAT (80A1)
 2040 FORMAT ('  END DATA')
 2050 FORMAT ('Table template missing (CLU=',I5,' GRP=',I5,
     1        ' for table index',I5)
 2060 FORMAT ('Label name ',A16,' template name ',A16,
     1        ' mismatch, index',I5)
 2070 FORMAT ('For table index',I5,' field',I3,
     1        'could not be completely stored (length set to 80.')
 2080 FORMAT ('Problem summarizing data-set number',I5,
     1        ' Return code',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      I1 = 1
      I3 = 3
      I80= 80
      BLNK(1)= ' '
C
C     fill label common block
      I= 1
      CALL WDTBSU (WDMSFL,DSN,TABMX,I,
     O             TABCNT,TABNAM,TABID,TABDIM,PDATVL,
     O             RETCOD)
C
      IF (RETCOD.GE.0) THEN
C       export all tables in this data set
        ITBL= 0
 10     CONTINUE
C         export next table
          ITBL= ITBL+ 1
          IF (ITBL.LE.TABCNT) THEN
C           export next table, determine index, rows, col space, ext space
            CALL WTBDSP (TABDIM(ITBL),
     O                   TABIND,NROW,NCOL,NEXT)
            IF (TABIND.GT.0) THEN
C             table exists, split out template cluster/group
              CALL WTBISP (TABID(ITBL),
     O                     MFID,TCLU,TGRP)
C             get actual location of table data set template
              CALL WDTBCG (MESSFL,WDMSFL,TCLU,TGRP,
     O                     LMESFL,LCLU,LGRP,RETCOD)
              IF (RETCOD.NE.0) THEN
C               table exists, but cant find template
                RETCOD= -29
                IEXIST= 0
                WRITE (99,2050) TCLU,TGRP,TABIND
              ELSE
                IEXIST= 1
              END IF
            ELSE
C             table does not exist
              IEXIST= 0
            END IF
          END IF
          IF (IEXIST.GT.0) THEN
C           table exists to export, save current table name
            I= 16
            CALL CARVAR (I,TABNAM(1,ITBL),I,CTBNAM)
C           get screen template info
            CALL WDTBSP (LMESFL,LCLU,LGRP,
     O                   MTBNAM,TFLDS,TTYP,TLEN,TCOL,TSPA,TNUM,
     O                   ACLU,AGRP,AFLDS,ATYP,ALEN,ACOL,ASPA,ANUM,
     O                   RETCOD)
            IF (CTBNAM.EQ.MTBNAM) THEN
C             names match, export name, index, other info header
              WRITE (SUCIFL,2000) (TABNAM(I,ITBL),I=1,16),TABIND,MFID,
     1                             LCLU,LGRP,NROW
              IF (AGRP.GT.0) THEN
C               associated table data exists
                DATFLG= 2
C               may need to adjust column numbers
                CALL WTBCLN (AFLDS,ALEN,
     M                       ACOL)
C               export associated data
                WRITE (SUCIFL,2010)
C               80 char buffers at a time
                IFLD = 1
                MXPOS= 80
                FFLD = 1
                FSPA = 1
 100            CONTINUE
C                 see if this field will fit in buffer
                  IPOS= ACOL(IFLD)+ ALEN(IFLD)- 1
                  IF (IPOS.GT.MXPOS .OR. IFLD.EQ.AFLDS) THEN
C                   not enough room in buffer or end of fields,
C                   get a block of associated data
                    IF (IPOS.GT.MXPOS .AND. IFLD.GT.FFLD) THEN
C                     dont include this fields space
                      IFLD= IFLD- 1
                    ELSE IF (IPOS.GT.MXPOS) THEN
C                     field will not fit in buffer
                      WRITE (99,2070) CTBNAM,TABIND,IFLD
                      ALEN(IFLD)= 80
                    END IF
                    BFLDS= IFLD- FFLD+ 1
C                   determine space for this buffer of data
                    CALL WTBSPA (BFLDS,ATYP(FFLD),ALEN(FFLD),
     O                           BSPA,XNUM)
C                   get this buffer of data
                    CALL WTBGET (WDMSFL,DSN,CTBNAM,TABIND,DATFLG,
     I                           I1,I1,FSPA,BSPA,
     O                           RBUFF,RETCOD)
C                   clear write buffer
                    CALL ZIPC (I80,BLNK,TBUFF)
C                   decode real buffer into text buffer
                    CALL WTBDCD (BFLDS,I1,BSPA,ALEN(FFLD),ATYP(FFLD),
     I                           ACOL(FFLD),RBUFF,MXTLEN,
     O                           TBUFF,RETCOD)
C                   write out extension data to sequential file
                    LEN= LENSTR(I80,TBUFF)
                    WRITE (SUCIFL,2030) (TBUFF(I,1),I=1,LEN)
                    IF (IPOS.GT.MXPOS) THEN
C                     more table to process, update positions in table
                      FFLD = IFLD+ 1
                      FSPA = FSPA+ BSPA
                      MXPOS= ACOL(FFLD)+ 80- 1
                    END IF
                  END IF
C                 increment field number
                  IFLD= IFLD+ 1
                IF (IFLD.LE.AFLDS) GO TO 100
C               end of extension data
                WRITE (SUCIFL,2015)
              END IF
C             now do main table
              DATFLG= 1
C             may need to adjust column numbers
              CALL WTBCLN (TFLDS,TLEN,
     M                     TCOL)
C             export main data
              WRITE (SUCIFL,2020)
C             for main table, output headers
              ID= 16
              CALL WMSIDP (LMESFL,LCLU,LGRP,ID,
     O                     DREC,DPOS)
C             back up one position
              CALL WDPRPS (LMESFL,
     M                     DREC,DPOS,
     O                     I)
C             now get block control word from start of data
              CALL WDNXDV (LMESFL,
     M                     DREC,DPOS,
     O                     BCWORD)
C             split block control word to determine total length
              CALL WMSBCS (BCWORD,
     O                     I,I,I,FLEN)
              MLEN= 0
              GLEN= 0
 150          CONTINUE
C               get a record of text from WDM file
                CALL WMSGTE (LMESFL,FLEN,I80,
     M                       DREC,DPOS,GLEN,MLEN,
     O                       OLEN,TBUFF,CONT)
C               insert '***' string to indicate comment line
                IPOS= STRFND(I80,TBUFF,I3,BK3)
                IF (IPOS.EQ.0) THEN
C                 blank space not found, create them
                  DO 175 I=1,I3
                    CALL CHRINS (I80,I1,CSTAR(I),TBUFF)
 175              CONTINUE
                ELSE
C                 insert '***' at available blank space
                  CALL CHRCHR (I3,CSTAR,TBUFF(IPOS,1))
                END IF
                IF (OLEN.LE.77) THEN
C                 increase output length to adjust for '***' added
                  OLEN= OLEN+ 3
                ELSE
C                 max of 80 for length
                  OLEN= 80
                END IF
C               output to file
                WRITE (SUCIFL,2030) (TBUFF(I,1),I=1,OLEN)
              IF (CONT.EQ.1) GO TO 150
C             80 char buffers at a time
              IFLD = 1
              MXPOS= 80
              FFLD = 1
              FSPA = 1
 200          CONTINUE
C               see if this field will fit in buffer
                IPOS= TCOL(IFLD)+ TLEN(IFLD)- 1
                IF (IPOS.GT.MXPOS .OR. IFLD.EQ.TFLDS) THEN
C                 not enough room in buffer or end of fields,
C                 get a block of main data
                  IF (IPOS.GT.MXPOS .AND. IFLD.GT.FFLD) THEN
C                   dont include this fields space
                    IFLD= IFLD- 1
                  ELSE IF (IPOS.GT.MXPOS) THEN
C                   field will not fit in buffer
                    WRITE (99,2070) CTBNAM,TABIND,IFLD
                    TLEN(IFLD)= 80
                  END IF
                  BFLDS= IFLD- FFLD+ 1
C                 determine space for this buffer of data
                  CALL WTBSPA (BFLDS,TTYP(FFLD),TLEN(FFLD),
     O                         BSPA,XNUM)
C                 get this buffer of data
                  CALL WTBGET (WDMSFL,DSN,CTBNAM,TABIND,DATFLG,
     I                         I1,NROW,FSPA,BSPA,
     O                         RBUFF,RETCOD)
C                 clear write buffer
                  CALL ZIPC (I80*NROW,BLNK,TBUFF)
C                 decode real buffer into text buffer
                  IF (FFLD.GT.1) THEN
C                   info beyond 1st 80 chars, need to adjust starting columns
                    DO 225 J= BFLDS,1,-1
C                     subtract starting column of 1st field from all fields
                      TCOL(FFLD+J-1)= TCOL(FFLD+J-1)- TCOL(FFLD)+ 1
 225                CONTINUE
                  END IF
                  CALL WTBDCD (BFLDS,NROW,BSPA,TLEN(FFLD),TTYP(FFLD),
     I                         TCOL(FFLD),RBUFF,MXTLEN,
     O                         TBUFF,RETCOD)
C                 write out main data to sequential file
                  DO 250 J= 1,NROW
                    LEN= LENSTR(I80,TBUFF(1,J))
                    WRITE (SUCIFL,2030) (TBUFF(I,J),I=1,LEN)
 250              CONTINUE
                  IF (IPOS.GT.MXPOS) THEN
C                   more table to process, update positions in table
                    FFLD = IFLD+ 1
                    FSPA = FSPA+ BSPA
                    MXPOS= TCOL(FFLD)+ 80- 1
                  END IF
                END IF
C               increment field number
                IFLD= IFLD+ 1
              IF (IFLD.LE.TFLDS) GO TO 200
C             end of main data
              WRITE (SUCIFL,2025)
C             end of all data for table
              WRITE (SUCIFL,2040)
            ELSE
C             names don't match, problem
              RETCOD= -33
              WRITE (99,2060) CTBNAM,MTBNAM,TABIND
            END IF
          END IF
        IF (ITBL.LT.TABCNT) GO TO 10
      ELSE
C       summary problems
        WRITE (SUCIFL,2080) DSN,RETCOD
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMDE
     I                   (WDMSFL,SUCIFL,DSN)
C
C     + + + PURPOSE + + +
C     export DLG type datasets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for sequential export file
C     DSN    - dataset number on WDM file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I20,J,ITYPE(20),ATT1(20),ATT2(20),ITMP,
     1            ILEN,ID,IPOS,NPTS,LOLEN,BOLEN,LRTCOD,BRTCOD
      REAL        DLGBUF(2400)
      CHARACTER*1 CTYPE(15)
      CHARACTER*4 GRNAME(15)
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDLGET, WDLLSU
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CTYPE/'L','I','N','E',' ','A','R','E','A',' ',
     1           'N','O','D','E',' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A4)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA')
 2005 FORMAT ('GROUP  TYPE ',5A1,'   MAJ ATTR',I5,'   MIN ATTR',I5)
 2010 FORMAT (I3,I5,I6,2F12.2,/,15A4)
 2020 FORMAT (6F12.2)
 2030 FORMAT ('  END DATA')
C
C     + + + END SPECIFICATIONS + + +
C
      I20= 20
C
C     always export all DLG groups
      WRITE (SUCIFL,2000)
 10   CONTINUE
C       get summary of label
        CALL WDLLSU (WDMSFL,DSN,I20,
     O               LOLEN,ITYPE,ATT1,ATT2,LRTCOD)
        IF (LRTCOD.GE.0) THEN
C         get data for LOLEN groups
          ILEN= 2400
          DO 30 I= 1,LOLEN
C           export all groups
C           header for this group
            IPOS= 5* (ITYPE(I)-1)+ 1
            WRITE (SUCIFL,2005) (CTYPE(J),J=IPOS,IPOS+4),
     1                           ATT1(I),ATT2(I)
 20         CONTINUE
C             get data until BRTCOD= 0
              ID= 0
              CALL WDLGET (WDMSFL,DSN,ITYPE(I),ATT1(I),ATT2(I),ILEN,
     M                     ID,
     O                     BOLEN,DLGBUF,BRTCOD)
              IF (ID.EQ.1) THEN
C               output internal number, coordinates, and name
                ITMP= DLGBUF(1)
                IF (BOLEN.GT.3) THEN
C                 read name from buffer
                  DO 22 J= 1,15
                    GRNAME(J)= ' '
 22               CONTINUE
                  DO 25 J= 4,BOLEN
                    WRITE (GRNAME(J-3),1000) DLGBUF(J)
 25               CONTINUE
C                 output internal number, coords, and name
                  WRITE (SUCIFL,2010) ID,BOLEN,ITMP,DLGBUF(2),
     1                         DLGBUF(3),(GRNAME(J),J=1,BOLEN-3)
                ELSE IF (BOLEN.GT.1) THEN
C                 output internal number and coords
                  WRITE (SUCIFL,2010) ID,BOLEN,ITMP,
     1                                DLGBUF(2),DLGBUF(3)
                ELSE
C                 no coordinates or name
                  WRITE (SUCIFL,2010) ID,BOLEN,ITMP
                END IF
              ELSE IF (ID.EQ.2) THEN
C               output number of points and coordinates
                NPTS= BOLEN/2
                WRITE (SUCIFL,2010) ID,BOLEN
                WRITE (SUCIFL,2020) (DLGBUF(J),
     1                               DLGBUF(J+NPTS),J=1,NPTS)
              END IF
            IF (BRTCOD.NE.2) GO TO 20
 30       CONTINUE
        END IF
      IF (LRTCOD.EQ.1) GO TO 10
C     all done, end data
      WRITE (SUCIFL,2030)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMAE
     I                   (WDMSFL,SUCIFL,DSN)
C
C     + + + PURPOSE + + +
C     export attribute data
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for output file
C     DSN    - dataset number on WDM file being exported
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I0,I10,GRCNT,ATIND,ATCNT,RETCOD,
     1              IPOS,DREC,DPOS,DSTYP,ILEN,ATTYP,
     2              ATLEN,ATUSWD,ATUSE(10),ATUPD,LATIND,DPTR
      CHARACTER*1   CDSTYP(10,10),ATNAM(6)
      CHARACTER*110 OBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (OBUF1,OBUFF)
      CHARACTER*1   OBUF1(110)
C
C     + + + FUNCTIONS + + +
      INTEGER       LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL      LENSTR, CHRCHR, WDSCHK, WADQCK, WADGTL
      EXTERNAL      PRMSAE, ZIPI,   WATTUS, WATWDS
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CDSTYP/'T','I','M','E','S','E','R','I','E','S',
     1            'T','A','B','L','E',' ',' ',' ',' ',' ',
     2            'S','C','H','E','M','A','T','I','C',' ',
     3            'P','R','O','J','E','C','T',' ',' ',' ',
     4            'V','E','C','T','O','R',' ',' ',' ',' ',
     5            'R','A','S','T','E','R',' ',' ',' ',' ',
     6            'S','P','A','C','E','-','T','I','M','E',
     7            'A','T','T','R','I','B','U','T','E',' ',
     8            'M','E','S','S','A','G','E',' ',' ',' ',
     9            ' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('  DATA   DSN ',I5)
 2010 FORMAT ('  END DATA')
 2020 FORMAT ('#ATTRIBUTE ',6A1,'   INDEX ',I5)
 2030 FORMAT ('$TYPE  INTEGER')
 2031 FORMAT ('$TYPE  REAL')
 2032 FORMAT ('$TYPE  CHARACTER')
 2033 FORMAT ('$TYPE  DOUBLE PRECISION')
 2040 FORMAT ('$LENGTH ',I4)
 2050 FORMAT (110A1)
 2060 FORMAT ('$UPDATE')
 2070 FORMAT ('Unable to export dataset ',I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I10   = 10
      LATIND= 0
C
C     check dataset existence, get total number of attribute groups
      DSTYP= 8
      CALL WDSCHK (WDMSFL,DSN,DSTYP,
     O             I,GRCNT,RETCOD)
C
      IF (RETCOD.EQ.0) THEN
C       ok to export, write beginning data header
        WRITE (SUCIFL,2000) DSN
        ATIND= 0
        ATCNT= 0
 5      CONTINUE
C         loop through cluster until exported all questions
          CALL ZIPI (I10,I0,ATUSE)
          LATIND= LATIND+ 1
C         check attribute existence
          CALL WADQCK (WDMSFL,DSN,LATIND,
     O                 ATIND)
          IF (ATIND.GT.0) THEN
C           group exists, export it
            ATCNT= ATCNT+ 1
C           get all of label info
            CALL WADGTL (WDMSFL,DSN,ATIND,
     O                   ATNAM,DPTR,ATTYP,ATLEN,ATUSWD,ATUPD)
            WRITE (SUCIFL,2020) ATNAM,ATIND
            IF (ATTYP.EQ.1) THEN
              WRITE (SUCIFL,2030)
            ELSE IF (ATTYP.EQ.2) THEN
              WRITE (SUCIFL,2031)
            ELSE IF (ATTYP.EQ.3) THEN
              WRITE (SUCIFL,2032)
            ELSE IF (ATTYP.EQ.4) THEN
              WRITE (SUCIFL,2033)
            END IF
            WRITE (SUCIFL,2040) ATLEN
            IF (ATUSWD.GT.0) THEN
C             determine required and optional dataset usage
              CALL WATTUS (ATUSWD,
     O                     ATUSE)
C             first do required datasets
              OBUFF= '$REQUIRED'
              IPOS = 9
              DO 10 I= 1,10
                IF (ATUSE(I).EQ.2) THEN
                  ILEN= LENSTR(I10,CDSTYP(1,I))
                  IPOS= IPOS+ 2
                  CALL CHRCHR (ILEN,CDSTYP(1,I),OBUF1(IPOS))
                  IPOS= IPOS+ ILEN
                  OBUF1(IPOS)= ','
                END IF
 10           CONTINUE
              IF (IPOS.GT.9) THEN
C               this attribute required for some datasets
                OBUF1(IPOS)= ' '
                WRITE (SUCIFL,2050) (OBUF1(I),I=1,IPOS-1)
              END IF
C             now do optional datasets
              OBUFF= '$OPTIONAL'
              IPOS = 9
              DO 20 I= 1,10
                IF (ATUSE(I).EQ.1) THEN
                  ILEN= LENSTR(I10,CDSTYP(1,I))
                  IPOS= IPOS+ 2
                  CALL CHRCHR (ILEN,CDSTYP(1,I),OBUF1(IPOS))
                  IPOS= IPOS+ ILEN
                  OBUF1(IPOS)= ','
                END IF
 20           CONTINUE
              IF (IPOS.GT.9) THEN
C               this attribute optional for some datasets
                OBUF1(IPOS)= ' '
                WRITE (SUCIFL,2050) (OBUF1(I),I=1,IPOS-1)
              END IF
            END IF
            IF (ATUPD.EQ.1) WRITE (SUCIFL,2060)
C           export rest of info from data record
            CALL WATWDS (DPTR,
     O                   DREC,DPOS)
            CALL PRMSAE (WDMSFL,SUCIFL,ATTYP,
     M                   DREC,DPOS)
          END IF
        IF (ATCNT.LT.GRCNT) GO TO 5
C
C       write end data
        WRITE(SUCIFL,2010)
      ELSE
C       problems with this dataset
        WRITE(99,2070) DSN
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRMSAE
     I                   (WDMSFL,SUCIFL,ATTYP,
     M                    DREC,DPOS)
C
C     + + + PURPOSE + + +
C     export attribute type question
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,ATTYP,DREC,DPOS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for export file
C     ATTYP  - attribute type
C     DREC   - record on WDM file
C     DPOS   - position on record
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,BCWORD,IVAL(2),ID,TLEN,ILEN
      CHARACTER*1  CDESC(5)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDPRPS, WDNXDV, WATWDS, WMSGTO, PRMSTA
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CDESC/'$','D','E','S','C'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('$RANGE ',I10,' : ',I10)
 2001 FORMAT ('$RANGE ',F10.3,' : ',F10.3)
 2010 FORMAT ('$DEFAULT ',I10)
 2011 FORMAT ('$DEFAULT ',F10.3)
 2020 FORMAT ('$HELP')
 2030 FORMAT ('$VALID')
C
C     + + + END SPECIFICATIONS + + +
C
C     back up one data value, then get the next one to get first value
      CALL WDPRPS (WDMSFL,
     M             DREC,DPOS,
     O             I)
      CALL WDNXDV (WDMSFL,
     M             DREC,DPOS,
     O             BCWORD)
      CALL WATWDS (BCWORD,
     O             ID,TLEN)
C
 5    CONTINUE
C       loop to export blocks
        IF (ID.GE.3 .AND. ID.LE.7) THEN
C         valid info id
          ID= ID- 2
C
          GO TO (30,40,50,60,70), ID
C
 30       CONTINUE
C           attribute range
            DO 35 I= 1,2
C             read min and max
              CALL WDNXDV (WDMSFL,
     M                     DREC,DPOS,
     O                     IVAL(I))
  35        CONTINUE
            IF (ATTYP.EQ.1) THEN
C             integer format
              WRITE (SUCIFL,2000) IVAL
            ELSE
C             real format
              WRITE (SUCIFL,2001) RVAL
            END IF
            GO TO 200
C
 40       CONTINUE
C           valid attribute responses
            WRITE (SUCIFL,2030)
            CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                   DREC,DPOS)
            GO TO 200
C
 50       CONTINUE
C           default value for attribute, get data value
            CALL WDNXDV (WDMSFL,
     M                   DREC,DPOS,
     O                   IVAL(1))
            IF (ATTYP.EQ.1) THEN
C             integer format
              WRITE (SUCIFL,2010) IVAL(1)
            ELSE
C             real format
              WRITE (SUCIFL,2011) RVAL(1)
            END IF
            GO TO 200
C
 60       CONTINUE
C           description of attribute
            ILEN= 5
            CALL PRMSTA (WDMSFL,SUCIFL,ILEN,CDESC,TLEN,
     M                   DREC,DPOS)
            GO TO 200
C
 70       CONTINUE
C           help for attribute
            WRITE (SUCIFL,2020)
            CALL WMSGTO (WDMSFL,SUCIFL,TLEN,
     M                   DREC,DPOS)
            GO TO 200
C
 200      CONTINUE
        END IF
C
C       get next block control word
        CALL WDNXDV (WDMSFL,
     M               DREC,DPOS,
     O               BCWORD)
        IF (BCWORD.GT.0) THEN
C         more info to come, split block control word
          CALL WATWDS (BCWORD,
     O                 ID,TLEN)
        ELSE
          ID= 0
        END IF
      IF (ID.GT.0) GO TO 5
C
      RETURN
      END
