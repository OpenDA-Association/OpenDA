C
C
C
      SUBROUTINE   PRWMTI
     I                    (WDMSFL,SUCIFL,DSN,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     import WDM timeseries data from sequential file to WDMSFL
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number of WDM file
C     SUCIFL - Fortran unit number of sequential file
C     DSN    - data set number to write timser data on
C     RETCOD - return code from data import operation
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,DTOVWR,DONFG,DONBLK,
     1             CURTST,CURDAT(6),CURQUA,CURTUN,
     2             TMPTST,TMPDAT(6),TMPQUA,TMPTUN,TMPCMP
      INTEGER*4    CURNOV,TMPNOV,NOWNOV,NUMWMX,J1,J2,J
      REAL         TMPRVL(6),YX(6000)
C
C     + + + EXTERNALS + + +
      EXTERNAL     WDATCP, WDTPUT, TIMADD
C
C     + + + INPUT FORMATS + + +
C1000 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X,2(1X,G11.4))
C1010 FORMAT (3X,6(1X,G11.4))
C     format modified 6/29/98, kmf
 1000 FORMAT (4X,I4,5I3,3X,3I3,I6,I3,7X,2(G12.4))
 1010 FORMAT (3X,6(G12.4))
C
C     + + + END SPECIFICATIONS + + +
C
      CURNOV= 0
      TMPNOV= 0
      DTOVWR= 0
      DONFG = 0
      NUMWMX= 6000
C
 10   CONTINUE
        IF (TMPNOV.EQ.0) THEN
C         read a bcw from sequential file
          READ (SUCIFL,1000,ERR=20,END=20) TMPDAT,TMPTUN,TMPTST,TMPQUA,
     1                              TMPNOV,TMPCMP,(TMPRVL(I),I=1,2)
          GO TO 30
 20       CONTINUE
C           read error, end of data
            DONFG= 1
 30       CONTINUE
        ELSE
C         update temp date to new start of buffer
          CALL TIMADD (CURDAT,TMPTUN,TMPTST,
     O                 NOWNOV,TMPDAT)
        END IF
C
        IF (CURNOV.GT.0.AND.(CURNOV+TMPNOV.GT.NUMWMX.OR.DONFG.EQ.1.OR.
     1                       TMPTUN.NE.CURTUN.OR.TMPQUA.NE.CURQUA.OR.
     2                       TMPTST.NE.CURTST)) THEN
C         NOT (never will get here - write forced below, makes for shorter blocks)
          CALL WDTPUT (WDMSFL,DSN,CURTST,CURDAT,CURNOV,
     I                 DTOVWR,CURQUA,CURTUN,YX,
     O                 RETCOD)
          CALL PRWMTX (DSN,CURTST,CURNOV,CURTUN,CURQUA,CURDAT,
     M                 RETCOD)
          CURNOV= 0
        END IF
C
        IF (DONFG.EQ.0) THEN
          IF (CURNOV.EQ.0) THEN
C           first time in buffer, fill constants
            CALL WDATCP (TMPDAT,CURDAT)
            CURTUN= TMPTUN
            CURTST= TMPTST
            CURQUA= TMPQUA
          END IF
C         add data to buffer
          NOWNOV= TMPNOV
          IF (TMPNOV.GT.NUMWMX) THEN
            NOWNOV= NUMWMX
          END IF
          TMPNOV= TMPNOV- NOWNOV
          IF (TMPCMP.EQ.0) THEN
C           uncompressed data
            J1= 1
            J2= 2
            DONBLK= 0
 40         CONTINUE
              IF (J2.GE.NOWNOV) THEN
C               we have read all values
                J2= NOWNOV
                DONBLK= 1
              END IF
              DO 50 J= J1,J2
                YX(J+CURNOV)= TMPRVL(J-J1+1)
 50           CONTINUE
              IF (J2.LT.NOWNOV) THEN
C               read more values
                READ (SUCIFL,1010) TMPRVL
                J1= J2+ 1
                J2= J1+ 5
              END IF
            IF (DONBLK.EQ.0) GO TO 40
          ELSE
C           compressed data
            DO 60 J= 1,NOWNOV
              YX(J+CURNOV)= TMPRVL(1)
 60         CONTINUE
          END IF
C         update number of values
          CURNOV= CURNOV+ NOWNOV
C         IF (CURNOV.GT.0) THEN
C           force a write here
C           CALL WDTPUT (WDMSFL,DSN,CURTST,CURDAT,CURNOV,
C    I                  DTOVWR,CURQUA,CURTUN,YX,
C    O                   RETCOD)
C           CALL PRWMTX (DSN,CURTST,CURNOV,CURTUN,CURQUA,CURDAT,
C    M                 RETCOD)
C           CURNOV= 0
C         END IF
        END IF
C
      IF (DONFG.EQ.0) GO TO 10
C
      RETURN
      END
C
C
C   
      SUBROUTINE   PRWMTX 
     I                   (DSN,CURTST,CURNOV,CURTUN,CURQUA,CURDAT,
     M                    RETCOD)
C
C     + + + PURPOSE + + +
C     determine if there is a problem with a retcod
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   DSN,CURTST,CURNOV,CURTUN,CURQUA,CURDAT(6),RETCOD
C 
C     + + + ARGUMENT DEFINITIONS + + +
C     DSN    - data set number to write timser data on
C     CURTST - current timestep
C     CURNOV - current number of values
C     CURTUN - current time units
C     CURQUA - current quality code
C     CURDAT - current date
C     RETCOD - return code from timeseries data put
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (' *** WARNING on timeseries ADD, return code:',I4,/,
     $        '     You should verify that data was correctly added.',/,
     1        '     DSN:',I6,'  TST,TUN,NOV,QUA',4I5,/,
     2        '     DATE:',I5,5I3)
C
C     + + + END SPECIFICATIONS + + +
C
      IF (RETCOD.NE.0) THEN
C       error on write to wdmsfl, see how bad
        IF (CURQUA.EQ.31 .AND. RETCOD.EQ.-15) THEN
C         ok for for missing
          RETCOD = 0
        ELSE
C         might be a problem, print retcod for user to investigate
          WRITE(99,2000) RETCOD,DSN,CURTST,CURTUN,CURNOV,CURQUA,CURDAT
        END IF
CJK     DONFG= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMDI
     I                   (WDMSFL,SUCIFL,DSN,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     import DLG type datasets
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMSFL,SUCIFL,DSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for sequential import file
C     DSN    - dataset number on WDM file
C     RETCOD - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I3,I4,I5,I12,J1,J2,ITYPE,ATT1,ATT2,
     1             ILEN,ID,IPOS,NPTS,DONFG,ITMP
      REAL         DLGBUF(2400)
      CHARACTER*1  CTYPE(15),IBUFF(80),CGROUP(5),CEND(3)
      CHARACTER*4  GRNAME
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND, CHRINT
      REAL         CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL     STRFND, CHRINT, CHRDEC, WDLPUT, CARVAR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CTYPE,CGROUP,CEND/'L','I','N','E',' ','A','R','E','A',' ',
     1           'N','O','D','E',' ','G','R','O','U','P','E','N','D'/
      DATA I3,I4,I5,I12/3,4,5,12/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (80A1)
 1010 FORMAT (A4)
 1020 FORMAT (3(2F12.2))
C
C     + + + END SPECIFICATIONS + + +
C
C     read first group's header info
      READ (SUCIFL,1000) IBUFF
      IF (STRFND(I5,IBUFF,I5,CGROUP).GT.0) THEN
C       group found, begin import of it
 10     CONTINUE
C         start of new group, determine type of data
          I= 15
          ITYPE= STRFND(I,CTYPE,I5,IBUFF(13))
          IF (ITYPE.GT.0) ITYPE= (ITYPE-1)/5+ 1
          ATT1 = CHRINT(I5,IBUFF(29))
          ATT2 = CHRINT(I5,IBUFF(45))
          DONFG= 0
 20       CONTINUE
C           read subgroup's id and length
            READ (SUCIFL,1000) IBUFF
            IF (STRFND(I5,IBUFF,I5,CGROUP).NE.0 .OR.
     1          STRFND(I5,IBUFF,I3,CEND).NE.0) THEN
C             new group starting or end of data for dsn
              DONFG= 1
            ELSE
C             import this subgroup, determine id and length
              ID  = CHRINT(I3,IBUFF)
              ILEN= CHRINT(I5,IBUFF(4))
              IF (ID.EQ.1) THEN
C               determine internal number and coordinates (if any)
                I= I5+ 1
                ITMP= CHRINT(I,IBUFF(9))
                DLGBUF(1)= ITMP
                IF (ILEN.GT.1) THEN
C                 get coordinates
                  DLGBUF(2)= CHRDEC(I12,IBUFF(15))
                  DLGBUF(3)= CHRDEC(I12,IBUFF(27))
                END IF
                IF (ILEN.GT.3) THEN
C                 read name from file
                  READ (SUCIFL,1000) IBUFF
C                 put name into buffer
                  DO 25 I= 1,ILEN-3
                    IPOS= 4*(I-1)+ 1
                    CALL CARVAR (I4,IBUFF(IPOS),I4,
     O                           GRNAME)
                    READ (GRNAME,1010) DLGBUF(I+3)
 25               CONTINUE
                END IF
C               put info on WDM file
                CALL WDLPUT (WDMSFL,DSN,ITYPE,ATT1,ATT2,ID,ILEN,DLGBUF,
     O                       RETCOD)
              ELSE IF (ID.EQ.2) THEN
C               read coordinates of each point
                NPTS= ILEN/2
                J1= 1
                J2= 3
 30             CONTINUE
                  IF (J2.GT.NPTS) J2= NPTS
                  READ (SUCIFL,1020) (DLGBUF(J),DLGBUF(J+NPTS),J=J1,J2)
                  J1= J1+ 3
                  J2= J2+ 3
                IF (J1.LE.NPTS) GO TO 30
C               put info on WDM file
                CALL WDLPUT (WDMSFL,DSN,ITYPE,ATT1,ATT2,ID,ILEN,DLGBUF,
     O                       RETCOD)
              END IF
            END IF
          IF (DONFG.EQ.0) GO TO 20
        IF (STRFND(I5,IBUFF,I5,CGROUP).GT.0) GO TO 10
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMAI
     I                   (WDMSFL,SUCIFL,DSN,
     O                    RETCOD)
C
C     + + + PURPOSE + + +
C     import attribute type question from sequential message file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,SUCIFL,DSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     SUCIFL - Fortran unit number for sequential message file
C     DSN    - dataset number on WDM file
C     RETCOD - return code -  0 - no problem,
C                          -151 - errors found and reported on unit 99
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,I5,I40,IERR,CONT,ITMP,
     1             ATIND,ATCNT,DSINIT,LATIND
      CHARACTER*1  CATTR(5),CHIND(5)
      CHARACTER*6  ATNAM
      CHARACTER*40 IBUFF,XBUFF
      CHARACTER*78 TBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (IBUF1,IBUFF)
      CHARACTER*1  IBUF1(40)
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND, CHRINT, LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL     STRFND, CHRINT, LENSTR, WADNSA
      EXTERNAL     ZLJUST, PRATIM
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I5,I40/5,40/
      DATA CATTR,CHIND/'#','A','T','T','R','I','N','D','E','X'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A40)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (78A1)
 2010 FORMAT ('Attribute ',A6,' (index ',I4,
     1        ' already exists for dataset',I4)
 2020 FORMAT (1X,A78)
C
C     + + + END SPECIFICATIONS + + +
C
      RETCOD= 0
C
      CONT  = 1
      ATCNT = 0
      DSINIT= 1
C
 5    CONTINUE
C       find first question, read until #ATTRIBUTE
        READ (SUCIFL,1000) IBUFF
      IF (STRFND(I5,IBUF1,I5,CATTR).EQ.0) GO TO 5
C
 10   CONTINUE
C       import until END DATA
        IERR = 0
C       question found
        IBUFF= IBUFF(11:40)
C       get name of attribute
        CALL ZLJUST (IBUFF)
        WRITE (ATNAM,2000) (IBUF1(I),I=1,6)
        IF (ATNAM.NE.' ') THEN
C         attribute name found, now get index
          IBUFF= IBUFF(7:40)
          ITMP= STRFND(I40,IBUF1,I5,CHIND)
          IF (ITMP.GT.0) THEN
C           INDEX found, get the value
            XBUFF= IBUFF(ITMP+5:40)
            IBUFF= XBUFF
            I= LENSTR(I40,IBUF1)
            ATIND = CHRINT(I,IBUF1)
            IF (ATIND.EQ.0) THEN
C             no INDEX number
              TBUFF= 'No index number found for attribute '//ATNAM
              WRITE(99,2020) TBUFF
              RETCOD= -151
              IERR  = 1
            END IF
          ELSE
C           invalid group number
            TBUFF= 'No INDEX found for attribute '//ATNAM
            WRITE(99,2020) TBUFF
            RETCOD= -151
            IERR  = 2
          END IF
        ELSE
C         no attribute name found
          TBUFF= 'No attribute name found on #ATTRIBUTE record.'
          WRITE(99,2020) TBUFF
          RETCOD= -151
          IERR  = 3
        END IF
C
        IF (IERR.EQ.0) THEN
C         check valid dataset and existence of dataset
          LATIND= ATIND
          CALL WADNSA (WDMSFL,DSINIT,
     M                 LATIND)
          DSINIT= 0
          IF (LATIND.EQ.ATIND) THEN
C           attribute already out there
            IERR= 4
          END IF
C
          IF (IERR.EQ.0) THEN
C           put attribute on WDM file
            ATCNT= ATCNT+ 1
            IF (ATCNT.EQ.1) THEN
              I= 1
            ELSE
              I= -1
            END IF
            CALL PRATIM (WDMSFL,DSN,SUCIFL,ATNAM,ATIND,
     M                   IBUFF,CONT,RETCOD)
          ELSE IF (IERR.EQ.4) THEN
C           attribute already exists, cant add
            WRITE (99,2010) ATNAM,ATIND
            RETCOD= -151
          END IF
        END IF
C
        IF (IBUFF(1:5).NE.'#ATTR' .AND. IBUFF(1:5).NE.'  END' .AND.
     1      IBUFF(1:4).NE.'END ') THEN
C         find start of next attribute(or end of data)
 950      CONTINUE
            READ (SUCIFL,1000) IBUFF
          IF (IBUFF(1:5).NE.'#ATTR' .AND. IBUFF(1:5).NE.'  END' .AND.
     1        IBUFF(1:4).NE.'END ') GO TO 950
        END IF
        IF (IBUFF(1:4).EQ.'END ' .OR. IBUFF(1:5).EQ.'  END') THEN
C         all done
          CONT= 0
        END IF
C
      IF (CONT.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRATIM
     I                   (WDMSFL,DSN,SUCIFL,ATNAM,ATIND,
     M                    OBUFF,CONT,RETCOD)
C
C     + + + PURPOSE + + +
C     import attribute data information for an attribute group
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,SUCIFL,ATIND,CONT,RETCOD
      CHARACTER*6  ATNAM
      CHARACTER*40 OBUFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number on WDM file
C     SUCIFL - Fortrn unit number for sequential message file
C     ATNAM  - attribute name
C     ATIND  - attribute index
C     OBUFF  - output buffer, returns last line read to calling routine
C     CONT   - flag indicating to continue reading sequential file
C              (0 - end of dataset, 1 - more info to read)
C     RETCOD - return code -  0 - no problem,
C                          -151 - errors found and reported on unit 99
C
C     + + + LOCAL VARIABLES + + +
      INTEGER       I,I0,I1,I5,I10,I130,J,L,NXTFG,ILEN,IPOS,
     1              IVAL(2),IDEF,ID,ITMP,ATTYP,ATLEN,ATUSE(10),ATUPD
      CHARACTER*1   CEND(5),DSTYP(10,10),COMMA(1)
      CHARACTER*78  TBUFF
      CHARACTER*130 IBUFF,XBUFF
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE  (IBUF1,IBUFF)
      CHARACTER*1   IBUF1(130)
      EQUIVALENCE  (RDEF,IDEF),(RMIN,IVAL(1)),(RMAX,IVAL(2))
      REAL          RDEF,RMIN,RMAX
C
C     + + + INTRINSICS + + +
      INTRINSIC     INDEX, MOD, ICHAR
C
C     + + + FUNCTIONS + + +
      INTEGER       CHRINT, LENSTR, WATTCL, STRFND, WATWDC
      REAL          CHRDEC
C
C     + + + EXTERNALS + + +
      EXTERNAL      CHRINT, LENSTR, WATTCL, STRFND, WATWDC, CHRDEC
      EXTERNAL      ZLJUST, WADADI, PRADIT, ZIPI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I5,I10,I130/0,1,5,10,130/
      DATA CEND,COMMA/'E','N','D',' ','D',','/
      DATA DSTYP/'T','I','M','E','S','E','R','I','E','S',
     1           'T','A','B','L','E',' ',' ',' ',' ',' ',
     2           'S','C','H','E','M','A','T','I','C',' ',
     3           'P','R','O','J','E','C','T',' ',' ',' ',
     4           'V','E','C','T','O','R',' ',' ',' ',' ',
     5           'R','A','S','T','E','R',' ',' ',' ',' ',
     6           'S','P','A','C','E','-','T','I','M','E',
     7           'A','T','T','R','I','B','U','T','E',' ',
     8           'M','E','S','S','A','G','E',' ',' ',' ',
     9           ' ',' ',' ',' ',' ',' ',' ',' ',' ',' '/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (A130)
 1010 FORMAT (A4,A2)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (1X,A78)
 2010 FORMAT (' Minimum =',I8,'    Maximum =',I8)
 2020 FORMAT (' Minimum =',F10.2,'    Maximum =',F10.2)
 2030 FORMAT ('        Index:',I5,' Type:',I5,' Length:',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      NXTFG= 0
      ATUPD= 0
      ATTYP= 0
      ATLEN= 0
      CALL ZIPI (I10,I0,ATUSE)
C
C     start by putting attribute name and index on WDM file
      READ (ATNAM,1010) IVAL(1),ITMP
      IVAL(1)= 0
      DO 10 I= 4,1,-1
        IVAL(1)= IVAL(1)*256+ MOD(ICHAR(ATNAM(I:I)),128)
 10   CONTINUE
      ITMP= MOD(ICHAR(ATNAM(6:6)),128)*256+ MOD(ICHAR(ATNAM(5:5)),128)
C
C     put second part of name and index into same word
      IVAL(2)= WATWDC (ITMP,ATIND)
      ID= 1
      I = 2
      CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I             I1,IBUF1,I,IVAL)
C
 100  CONTINUE
C       look for next directive
        IF (NXTFG.EQ.0 .OR. IBUF1(1).NE.'$') THEN
C         read next record
 110      CONTINUE
            READ (SUCIFL,1000) IBUFF
C           left justify
            CALL ZLJUST(IBUFF)
          IF (IBUF1(1).NE.'$' .AND. IBUF1(1).NE.'#' .AND.
     1        STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 110
        END IF
C       processing current directive
        NXTFG = 0
C       which directive
        IF (IBUFF(1:5).EQ.'$TYPE') THEN
C         attribute type
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          IF (IBUF1(1).EQ.'I') THEN
            ATTYP= 1
          ELSE IF (IBUF1(1).EQ.'R') THEN
            ATTYP= 2
          ELSE IF (IBUF1(1).EQ.'C') THEN
            ATTYP= 3
          ELSE IF (IBUF1(1).EQ.'D') THEN
            ATTYP= 4
          END IF
        ELSE IF (IBUFF(1:8).EQ.'$LENGTH') THEN
C         attribute length
          IBUFF= IBUFF(8:80)
          CALL ZLJUST(IBUFF)
          ATLEN= CHRINT(LENSTR(I130,IBUF1),IBUF1)
        ELSE IF (IBUFF(1:5).EQ.'$DESC') THEN
C         attribute description
          ID   = 6
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I                 I130,IBUF1,I1,IVAL)
        ELSE IF (IBUFF(1:7).EQ.'$UPDATE') THEN
C         update attribute if data exists
          ATUPD= 1
        ELSE IF (IBUFF(1:9).EQ.'$REQUIRED') THEN
C         required for dataset types specified
          IBUFF= IBUFF(10:130)
 120      CONTINUE
C           parse dataset type out of buffer
            CALL ZLJUST(IBUFF)
            ILEN= STRFND(I130,IBUF1,I1,COMMA)
            IF (ILEN.EQ.0) THEN
C             last dataset type in list
              ILEN= LENSTR(I130,IBUF1)
            END IF
            I= 100
            IPOS= STRFND(I,DSTYP,ILEN-1,IBUF1)
            IF (IPOS.GT.0) THEN
C             dataset type found among valid types
              I= (IPOS-1)/10+ 1
              ATUSE(I)= 2
            END IF
            XBUFF= IBUFF(ILEN+1:130)
            IBUFF= XBUFF
          IF (LENSTR(I130,IBUF1).GT.0)  GO TO 120
        ELSE IF (IBUFF(1:9).EQ.'$OPTIONAL') THEN
C         optional for dataset type specified
          IBUFF= IBUFF(10:80)
 130      CONTINUE
C           parse dataset type out of buffer
            CALL ZLJUST(IBUFF)
            ILEN= STRFND(I130,IBUF1,I1,COMMA)
            IF (ILEN.EQ.0) THEN
C             last dataset type in list
              ILEN= LENSTR(I130,IBUF1)
            END IF
            I= 100
            IPOS= STRFND(I,DSTYP,ILEN-1,IBUF1)
            IF (IPOS.GT.0) THEN
C             dataset type found among valid types
              I= (IPOS-1)/10+ 1
              ATUSE(I)= 1
            END IF
            XBUFF= IBUFF(ILEN+1:130)
            IBUFF= XBUFF
          IF (LENSTR(I130,IBUF1).GT.0)  GO TO 130
        ELSE IF (IBUFF(1:5).EQ.'$HELP') THEN
          IBUFF= IBUFF(6:80)
          CALL ZLJUST(IBUFF)
          ID   = 7
          CALL PRADIT (SUCIFL,WDMSFL,DSN,ATIND,ID,I130,
     M                 IBUF1,NXTFG)
        ELSE IF (IBUFF(1:8).EQ.'$DEFAULT') THEN
C         default value
          ID   = 5
          IBUFF= IBUFF(9:80)
          CALL ZLJUST(IBUFF)
          L= LENSTR(I130,IBUF1)
          IF (ATTYP.EQ.1) THEN
C           store as integer
            IDEF= CHRINT(L,IBUF1)
          ELSE
C           store as real
            RDEF= CHRDEC(L,IBUF1)
          END IF
          IVAL(1)= IDEF
          CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I                 I1,IBUF1,I1,IVAL)
          IVAL(1)= 0
        ELSE IF (IBUFF(1:6).EQ.'$RANGE') THEN
C         attribute range
          ID    = 8
          IBUFF = IBUFF(7:80)
          CALL ZLJUST(IBUFF)
          J = INDEX(IBUFF,':')
          IF (J.LT.1) THEN
C           bad range or type not set
            TBUFF= 'ERROR: problem with _RANGE record'
            WRITE(99,2000) TBUFF
            TBUFF= 'Record is >> '//IBUFF(1:65)
            WRITE(99,2000) TBUFF
            WRITE(99,*)
            RETCOD= -151
          ELSE
C           min value
            IBUFF(J:J)= ' '
            L= LENSTR(J,IBUF1)
            IF (ATTYP.EQ.1) THEN
C             store as integer
              IVAL(1)= CHRINT(L,IBUF1)
            ELSE
C             store as real
              RMIN= CHRDEC(L,IBUF1)
            END IF
            XBUFF= IBUFF(J+1:80)
            IBUFF= XBUFF
C           max value
            CALL ZLJUST(IBUFF)
            I= 10
            L= LENSTR(I,IBUF1)
            IF (ATTYP.EQ.1) THEN
C             store as integer
              IVAL(2)= CHRINT(L,IBUF1)
              IF (IVAL(2).LT.IVAL(1) .AND. IVAL(2).GT.-999) THEN
C               invalid range values
                TBUFF= 'ERROR: Invalid Range values.'
                WRITE(99,2000) TBUFF
                WRITE(99,2010) IVAL(1),IVAL(2)
                WRITE(99,*)
                RETCOD= -151
              END IF
            ELSE
C             store as real
              RMAX = CHRDEC(L,IBUF1)
              IF (RMAX.LT.RMIN .AND. RMAX.GT.-998.0) THEN
C               invalid range values
                TBUFF= 'ERROR: Invalid Range values.'
                WRITE(99,2000) TBUFF
                WRITE(99,2020) RMIN,RMAX
                WRITE(99,*)
                RETCOD= -151
              END IF
            END IF
            I = 2
            ID= 3
            CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I                   I1,IBUF1,I,IVAL)
          END IF
        ELSE IF (IBUFF(1:6).EQ.'$VALID') THEN
C         valid attribute values
          IBUFF= IBUFF(7:80)
          CALL ZLJUST(IBUFF)
          ID   = 4
          CALL PRADIT (SUCIFL,WDMSFL,DSN,ATIND,ID,I130,
     M                 IBUF1,NXTFG)
        ELSE IF (STRFND(I10,IBUF1,I5,CEND).EQ.0 .AND.
     1           IBUF1(1).NE.'#') THEN
C         not end of group, directive not importable
          TBUFF= 'ERROR: The following record is not'//
     1           ' recognized as a directive.'
          WRITE(99,2000) TBUFF
          TBUFF= 'Record is >> '//IBUFF(1:65)
          WRITE(99,2000) TBUFF
          WRITE(99,*)
          RETCOD= -151
        END IF
C
      IF (IBUF1(1).NE.'#'.AND.STRFND(I10,IBUF1,I5,CEND).EQ.0) GO TO 100
C
C     make sure integer parameters get put on
      IF (ATTYP.EQ.0 .OR. ATLEN.EQ.0) THEN
C       problems, haven't specified required info
        TBUFF= 'ERROR: Attribute Type and/or Length not specified.'
        WRITE(99,2000) TBUFF
        WRITE(99,2030) ATIND,ATTYP,ATLEN
        WRITE(99,*)
        RETCOD= -151
      END IF
C     add info to WDM file
      ID= 2
      IVAL(1)= WATTCL (ATTYP,ATLEN,ATUSE,ATUPD)
      CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I             I1,IBUF1,I1,IVAL)
C
C     end of dataset?
      IF (STRFND(I10,IBUF1,I5,CEND).GT.0) CONT= 0
      OBUFF= IBUFF(1:40)
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRADIT
     I                   (SUCIFL,WDMSFL,DSN,ATIND,ID,ILEN,
     M                    IBUF1,NXTFG)
C
C     + + + PURPOSE + + +
C     put text from any id of attribute information on WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     SUCIFL,WDMSFL,DSN,ATIND,ID,ILEN,NXTFG
      CHARACTER*1 IBUF1(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SUCIFL - Fortran unit number for sequential message file
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number on WDM file
C     ATIND  - attribute number to put on WDM file
C     ID     - id for portion of attribute being stored
C     ILEN   - length of buffer being stored
C     IBUFF  - buffer of character data to store
C     NXTFG  - flag for calling program indicating a read is needed
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I1,I5,I10,IDUM(1)
      CHARACTER*1 CEND(5)
C
C     + + + FUNCTIONS + + +
      INTEGER     LENSTR, STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL    LENSTR, STRFND, WADADI
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I1,I5,I10/1,5,10/
      DATA CEND /'E','N','D',' ','D'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (130A1)
C
C     + + + END SPECIFICATIONS + + +
C
      IDUM(1)= 0
C
      IF (LENSTR(ILEN,IBUF1).GT.0) THEN
C       info begins on this line
        CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I               ILEN,IBUF1,I1,IDUM)
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
          CALL WADADI (WDMSFL,DSN,ATIND,ID,
     I                 ILEN,IBUF1,I1,IDUM)
        END IF
      IF (NXTFG .EQ. 0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   PRWMXI
     I                    (MESSFL,WDMSFL,SUCIFL,DSN,
     O                     RETCOD)
C
C     + + + PURPOSE + + +
C     import WDM table data from sequential file to WDMSFL
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MESSFL,WDMSFL,SUCIFL,DSN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number of ANNIE message file
C     WDMSFL - Fortran unit number of WDM file
C     SUCIFL - Fortran unit number of sequential file
C     DSN    - data set number to write timser data on
C     RETCOD - return code from data import operation
C
C     + + + PARAMETERS + + +
      INTEGER    MXTROW,MXTLEN
      PARAMETER (MXTROW=300,MXTLEN=MXTROW*20)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,I1,I3,I80,TABIND,DATFLG,NROW,VALFG,ENDFG,TGRPPT,
     1             TCLU,TGRP,TFLDS,TLEN(30),TCOL(30),TSPA,TNUM(4),
     2             ACLU,AGRP,AFLDS,ALEN(30),ACOL(30),ASPA,ANUM(4),
     3             BFLDS,BSPA,IPOS,IFLD,FFLD,FSPA,XNUM(4),MXPOS,LRET
      REAL         RBUFF(MXTLEN)
      CHARACTER*1  TBUFF(80,MXTROW),TTYP(30),ATYP(30),BLNK(1),
     1             CEXT(3),CEND(3),CMAI(3),CDAT(3),CSTAR(3),MFID(2)
      CHARACTER*16 TABNAM,MTBNAM
C
C     + + + FUNCTIONS + + +
      INTEGER      STRFND
C
C     + + + EXTERNALS + + +
      EXTERNAL     STRFND, WDTBTM, WTBCOD, WTBPUT, WTBSPA, WTBCLN, ZIPC
C
C     + + + DATA INITIALIZATIONS + + +
      DATA CEXT/'E','X','T'/
      DATA CEND/'E','N','D'/
      DATA CMAI/'M','A','I'/
      DATA CDAT/'D','A','T'/
      DATA CSTAR/'*','*','*'/
C
C     + + + INPUT FORMATS + + +
 1000 FORMAT (14X,A16,6X,I4,7X,2A1,3(6X,I4))
 1010 FORMAT (80A1)
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ('Return code from extension add:',I4)
 2010 FORMAT ('Return code from main data add:',I4)
 2020 FORMAT ('For table index',I5,' field',I3,
     1        'could not be completely stored (length set to 80.')
 2030 FORMAT ('For table name ',A16,' index',I5,
     1        ' end of data was reached prematurely.')
 2040 FORMAT ('Problem retrieving template info, return code',I4)
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      I3  = 3
      I80 = 80
      BLNK(1)= ' '
C
      LRET= 0
 5    CONTINUE
C       backspace one line to get needed table parameters
        BACKSPACE (SUCIFL)
        READ (SUCIFL,1000) TABNAM,TABIND,MFID,TCLU,TGRP,NROW
C       now get table info from message file
        CALL WDTBTM (MESSFL,MFID,TCLU,TGRP,WDMSFL,DSN,TABIND,NROW,
     O               TFLDS,TNUM,TTYP,TLEN,TCOL,TSPA,MTBNAM,TGRPPT,
     O               AFLDS,ANUM,ATYP,ALEN,ACOL,ASPA,ACLU,AGRP,RETCOD)
        IF (RETCOD.EQ.0) THEN
C         table info retrieved ok
          IF (MTBNAM.NE.TABNAM) THEN
C           problem with table names
            RETCOD= -33
          ELSE IF (ACLU.GT.0) THEN
C           get info about table extension
            DATFLG= 2
            CALL ZIPC (I80,BLNK,TBUFF)
C           loop till beginning of extension data
 10         CONTINUE
              READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
            IF (STRFND(I3,TBUFF(5,1),I3,CEXT).EQ.0) GO TO 10
C           read extension data from import file
 20         CONTINUE
C             loop till done reading headers (***)
              READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
            IF (STRFND(I80,TBUFF(1,1),I3,CSTAR).GT.0) GO TO 20
C           may need to adjust column numbers
            CALL WTBCLN (AFLDS,ALEN,
     M                   ACOL)
C           80 char buffers at a time
            IFLD = 1
            MXPOS= 80
            FFLD = 1
            FSPA = 1
 100        CONTINUE
C             see if this field will fit in buffer
              IPOS= ACOL(IFLD)+ ALEN(IFLD)- 1
              IF (IPOS.GT.MXPOS .OR. IFLD.EQ.AFLDS) THEN
C               not enough room in buffer or end of fields,
C               get a block of associated data
                IF (IPOS.GT.MXPOS .AND. IFLD.GT.FFLD) THEN
C                 dont include this fields space
                  IFLD= IFLD- 1
                ELSE IF (IPOS.GT.MXPOS) THEN
C                 field will not fit in buffer
                  WRITE (99,2020) TABNAM,TABIND,IFLD
                  ALEN(IFLD)= 80
                  LRET= -34
                END IF
                BFLDS= IFLD- FFLD+ 1
C               determine space for this buffer of data
                CALL WTBSPA (BFLDS,ATYP(FFLD),ALEN(FFLD),
     O                       BSPA,XNUM)
C               convert extension data to internal format
                CALL WTBCOD (BFLDS,I1,BSPA,ALEN(FFLD),ATYP(FFLD),
     I                       ACOL(FFLD),TBUFF,MXTLEN,
     O                       RBUFF,RETCOD)
C               write buffer of extension data to the WDM file
                CALL WTBPUT (WDMSFL,DSN,TABNAM,TABIND,DATFLG,
     I                       I1,I1,FSPA,BSPA,RBUFF,
     O                       RETCOD)
                IF (IPOS.GT.MXPOS) THEN
C                 more table to process, update positions in table
                  FFLD = IFLD+ 1
                  FSPA = FSPA+ BSPA
                  MXPOS= ACOL(FFLD)+ 80- 1
C                 read next record of extension data
                  READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
                END IF
              END IF
C             increment field number
              IFLD= IFLD+ 1
            IF (IFLD.LE.AFLDS) GO TO 100
C           loop till end of extension data
 120        CONTINUE
              READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
            IF (STRFND(I3,TBUFF(5,1),I3,CEND).EQ.0) GO TO 120
          END IF
          IF (RETCOD.NE.0) THEN
C           write bad record
            WRITE(99,2000) RETCOD
          ELSE
C           now do the main table
            DATFLG= 1
C           loop till beginning of table data
 130        CONTINUE
              READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
            IF (STRFND(I3,TBUFF(5,1),I3,CMAI).EQ.0) GO TO 130
C           read main table data from import file
C           may need to adjust column numbers
            CALL WTBCLN (TFLDS,TLEN,
     M                   TCOL)
C           80 char buffers at a time
            IFLD = 1
            MXPOS= 80
            FFLD = 1
            FSPA = 1
            ENDFG= 0
 200        CONTINUE
C             see if this field will fit in buffer
              IPOS= TCOL(IFLD)+ TLEN(IFLD)- 1
              IF (IPOS.GT.MXPOS .OR. IFLD.EQ.TFLDS) THEN
C               not enough room in buffer or end of fields,
C               get a block of main data
                CALL ZIPC (I80*NROW,BLNK,TBUFF)
                IF (IPOS.GT.MXPOS .AND. IFLD.GT.FFLD) THEN
C                 dont include this fields space
                  IFLD= IFLD- 1
                ELSE IF (IPOS.GT.MXPOS) THEN
C                 field will not fit in buffer
                  WRITE (99,2020) TABNAM,TABIND,IFLD
                  TLEN(IFLD)= 80
                  LRET= -34
                END IF
                BFLDS= IFLD- FFLD+ 1
C               determine space for this buffer of data
                CALL WTBSPA (BFLDS,TTYP(FFLD),TLEN(FFLD),
     O                       BSPA,XNUM)
                I= 1
 240            CONTINUE
C                 read next main data record
                  READ (SUCIFL,1010) (TBUFF(J,I),J=1,80)
C                 check for comment record ('***')
                  VALFG= STRFND(I80,TBUFF(1,I),I3,CSTAR)
C                 check for end of data
                  ENDFG= STRFND(I3,TBUFF(5,I),I3,CEND)
                  IF (VALFG.EQ.0) THEN
C                   record of data (no '***' found)
                    I= I+ 1
                  END IF
                IF (ENDFG.EQ.0 .AND. I.LE.NROW) GO TO 240
                IF (ENDFG.GT.0) THEN
C                 end of data reached early, problem
                  LRET= -35
                  WRITE (99,2030) TABNAM,TABIND
                  IF (I.GT.1) THEN
C                   store any data that has been successfully read
                    NROW= I- 1
                  END IF
                END IF
                IF (FFLD.GT.1) THEN
C                 info beyond 1st 80 chars, need to adjust starting columns
                  DO 225 J= BFLDS,1,-1
C                   subtract starting column of 1st field from all fields
                    TCOL(FFLD+J-1)= TCOL(FFLD+J-1)- TCOL(FFLD)+ 1
 225              CONTINUE
                END IF
C               convert table data to internal format
                CALL WTBCOD (BFLDS,NROW,BSPA,TLEN(FFLD),TTYP(FFLD),
     I                       TCOL(FFLD),TBUFF,MXTLEN,
     O                       RBUFF,RETCOD)
C               write main table to the WDM file
                CALL WTBPUT (WDMSFL,DSN,TABNAM,TABIND,DATFLG,
     I                       I1,NROW,FSPA,BSPA,RBUFF,
     O                       RETCOD)
                IF (RETCOD.NE.0) THEN
C                 write bad RETCOD
                  WRITE(99,2010) RETCOD
                END IF
                IF (IPOS.GT.MXPOS) THEN
C                 more table to process, update positions in table
                  FFLD = IFLD+ 1
                  FSPA = FSPA+ BSPA
                  MXPOS= TCOL(FFLD)+ 80- 1
                END IF
              END IF
C             increment field number
              IFLD= IFLD+ 1
            IF (IFLD.LE.TFLDS .AND. ENDFG.EQ.0) GO TO 200
          END IF
        ELSE
C         problem retrieving table template info
          WRITE (99,2040) RETCOD
        END IF
C       loop till end of data
 250    CONTINUE
          READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
        IF (STRFND(I3,TBUFF(3,1),I3,CEND).EQ.0) GO TO 250
C       read to see if end of DSN or more data
 260    CONTINUE
          READ (SUCIFL,1010) (TBUFF(J,1),J=1,80)
        IF (STRFND(I3,TBUFF(3,1),I3,CDAT).EQ.0 .AND.
     1      STRFND(I3,TBUFF(1,1),I3,CEND).EQ.0) GO TO 260
C
      IF (STRFND(I3,TBUFF(3,1),I3,CDAT).GT.0) GO TO 5
C
      IF (RETCOD.EQ.0 .AND. LRET.NE.0) THEN
C       had some problems other than template get and data put
        RETCOD= LRET
      END IF
C
      RETURN
      END
