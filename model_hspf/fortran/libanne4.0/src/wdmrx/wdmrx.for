C
C
C
      PROGRAM   WDMRX
C
C     + + + PURPOSE + + +
C     strong medicine for an ailing wdm file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   OPTION
C
C     + + + EXTERNALS + + +
      EXTERNAL   BASIC, PATCH1
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT ( ' Select a processing option:',
     $       /,'   0 - done',
     $       /,'   1 - general: check of pointers',
     $       /,'   2 - patch 1: required if n-day option ',
     $                 'in swstat version 3.2 was used.' )
 2090 FORMAT (//,' Done processing', // )
C
C     + + + END SPECIFICATIONS + + +
C
 100  CONTINUE
C       Option: 0-done,1-general,2-patch1
        WRITE (*,2000)
        READ (*,*) OPTION
        IF (OPTION .EQ. 1) THEN
C         basic check of pointers and version of wdm file
          CALL BASIC
        ELSE IF (OPTION .EQ. 2) THEN
C         patch to fix attribute conflict for 443 & 444 problem
C         introduced when Aqua Terra added character attributes
C         datcre and datmod for GenScn and USGS added integer
C         attributes seadbg and seadnd for swstat, both using
C         the same index numbers.  Fix involves checking for the
C         use of the attributes and changing seadbg and seadnd
C         to 446 and 447 if they are found.  When the fix has
C         been made, the directory record has position 112 set
C         to a non-zero number.  This patch should be run on
C         wdm files that contain data sets analyzed using the
C         n-day option in swstat version 3.2.
          CALL PATCH1
        ELSE
C         assume done, force it
          OPTION = 0
          WRITE (*,2090)
        END IF
      IF (OPTION .NE. 0) GO TO 100
C
      STOP
      END
C
C
C
      SUBROUTINE   BASIC
C
C     + + + PURPOSE + + +
C     Basic check of pointers and version of a wdm file.
C
C     + + + PARAMETERS + + +
      INTEGER   PCHNLN
      PARAMETER(PCHNLN=8000)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   USEREC(96000),IBUF(512),MAXREC,I,RECID,J,K,L,M,REVFG,
     1          FREREC,WDMFL,CHFLG,CLEN,PDP,PUP,PSA,PDAT,PDATV,
     2          SACNT,PSASTR,ECNT,DREC(400),DOFF(400),DPOS,DBUF(512),
     3          CBUF(PCHNLN),UBUF(512),
     4          LSTFRE,ADDFRE,DONFRE,OUTLEV,UPDFLG,CDSN,MISCNT,
     5          DCNT(9),LDCNT(9),PREC,POFF,WRTFLG,
     6          DCHN(8),DNOW,CNOW,CREC,DPT,DOF,DRC
      CHARACTER*64 FNAME, VERSN
      CHARACTER*10 DTYPE(9),CLEV(4)
      CHARACTER*6  ATNAME
      CHARACTER*3  COPT(2)
      CHARACTER*1  ANS
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (IBUF,RBUF)
      REAL         RBUF(512)
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  CHAIN,CHKTIM,CHKTAB,CHKMES,CHKVEC,WDPTSP,CHKSPT,CHKATR
      EXTERNAL  CHKDIR
C
C     + + + DATA INITIALIZATIONS + + +
      DATA DTYPE/'TIMESERIES','TABLE','SCHEMATIC','PROJECT','VECTOR',
     1           'RASTER','SPACE-TIME','ATTRIBUTE','MESSAGE'/
      DATA CLEV/'FILE','DATASET','GROUP','BLOCK'/
      DATA COPT/'NO','YES'/
C
C     + + + FORMATS + + +
1000  FORMAT (A64)
1010  FORMAT (A1)
2000  FORMAT (' WDM FILE: ',A64,/,
     1        '   MAXREC      :',I6,/,
     2        '   FREREC      :',I6,/)
2001  FORMAT (' OUTPUT LEVEL IS ',A10)
2002  FORMAT (' COUNT OF DATASETS BY TYPE',/,
     1        '   1-TIMESERIES:',I6,/,
     2        '   2-TABLE     :',I6,/,
     3        '   3-SCHEMATIC :',I6,/,
     4        '   4-PROJECT   :',I6,/,
     5        '   5-VECTOR    :',I6,/,
     6        '   6-RASTOR    :',I6,/,
     7        '   7-SPACE-TIME:',I6,/,
     8        '   8-ATTRIBUTE :',I6,/,
     9        '   9-MESSAGE   :',I6,/)
2003  FORMAT (' UPDATE OPTION IS ',A3/)
2004  FORMAT (' ATTRIBUTE 443/444 FIXED FLAG IS ',I4,/)
2005  FORMAT (/,' LOOKING FOR DIRECTORY RECORDS')
2006  FORMAT (/,' CHAIN OF ',A10,' DATASETS:',8I6)
2007  FORMAT (30X,8I6)
2009  FORMAT (/' **** PROBLEM **** DATASET IN CHAIN NOT FOUND:',I6)
2010  FORMAT ('   FOUND DIR REC: ',I5,' ON PHYS REC',I5,
     1        ' FOR DSN:',I6,' - ',I6)
2015  FORMAT (' END SEARCH FOR DIRECTORY RECORDS',/)
2020  FORMAT (/,' FREE REC CHN : ',12I5,/,(16X,12I5))
2025  FORMAT (/,' NO FREE RECORDS ARE CURRENTLY AVAILABLE')
2030  FORMAT (/,' DSN: ',I5,'    TYPE: ',I1,'-',A10,'     LENGTH:',I5)
2032  FORMAT (/,' **** PROBLEM ****  EXPECTING DSN:',I6,' FOUND:',I6)
2035  FORMAT (/,' ***** BIG PROBLEM *****  READ ERROR FROM WDM ON REC:',
     1          I6,' FOR LABEL OF DSN: ',I5)
2038  FORMAT (/,' ***** BIG PROBLEM *****  READ ERROR FROM WDM ON REC:',
     1          I6,' FOR DIRECTORY:',I5)
2040  FORMAT ('             CHAIN: ',12I5,/,(16X,12I5))
2041  FORMAT ('      GEN POINTERS: ',5I5)
2042  FORMAT ('       TO POINTERS: ',I5)
2043  FORMAT ('     FROM POINTERS: ',I5)
2044  FORMAT ('     SATTR CNT,PTR: ',2I5)
2045  FORMAT ('     ATTRIB NUM,IND,LOC: ',3I5)
2046  FORMAT ('                   NAME: ',4X,A6,/,
     1        '                  VALUE: ',20A4)
2047  FORMAT ('                   NAME: ',4X,A6,/,
     1        '                  VALUE: ',I10)
2048  FORMAT ('                   NAME: ',4X,A6,/,
     1        '                  VALUE: ',F10.2)
2050  FORMAT (' **** PROBLEM **** REC OUT OF CHAIN:',I5,
     1        ' PB,PF,SB,SF: ',4I5)
2060  FORMAT (' **** PROBLEM **** FRE REC PT NOT LAST REC:',I5)
2071  FORMAT ('         CHECKING TIMSERIES GROUPS')
2072  FORMAT ('         CHECKING TABLE GROUPS')
2075  FORMAT ('         CHECKING VECTOR GROUPS')
2077  FORMAT ('         CHECKING SPACE TIME GROUPS')
2078  FORMAT ('         CHECKING ATTRIBUTE GROUPS')
2079  FORMAT ('         CHECKING MESSAGE GROUPS')
2080  FORMAT (' #### ATTEMPT TO REVISE CHAIN ####')
2090  FORMAT (' #### RECORD ADDED TO FREE CH ####',I5)
2095  FORMAT (/,'    FOUND NO RECORDS MISSING FROM CHAINS')
2100  FORMAT (//,' LOOKING FOR MISSING RECORDS IN CHAINS')
2110  FORMAT ('     FREE POS OF DATA:',2I5)
2120  FORMAT (//,' CHECKING DATASET COUNTS')
2125  FORMAT (' **** PROBLEM ***  TYPE:',I5,' COUNTS DO NOT MATCH:',2I6)
2130  FORMAT ('    ALL DATASET COUNTS MATCH')
2140  FORMAT (/,' MAP OF RECORD USAGE')
2150  FORMAT (1X,I5,' - ',I5,':',2X,10I6)
2200  FORMAT (//,' ** NOTE - SEE FILE "DUMP.OUT" FOR MORE INFORMATION')
C
C     + + + END SPECIFICATIONS + + +
C
C     version info for what on unix
      INCLUDE 'fversn.inc'
C
      WDMFL= 12
      WRITE(*,*) 'NAME OF YOUR WDM FILE? '
      READ(*,1000) FNAME
C     record length: 2048-unix, 512-vax, 1024-prime, 2048-pc
      OPEN (UNIT=WDMFL,FILE=FNAME,ACCESS='DIRECT',STATUS='OLD',
     1      RECL=2048,FORM='UNFORMATTED',ERR=10)
      GO TO 20
 10   CONTINUE
        WRITE (*,*) ' ERROR OPENING WDM FILE'
        STOP
 20   CONTINUE
C
      OPEN(UNIT=10,FILE='DUMP.OUT')
C
      WRITE(*,*) 'OUTPUT LEVEL(0-WDM,1-DSN,2-GRP,3-BLK)? '
      READ(*,*) OUTLEV
      WRITE(10,2001) CLEV(OUTLEV+1)
      WRITE(*,*) 'UPDATE FLAG(0-NO,1-YES)? '
      READ(*,*) UPDFLG
      WRITE(10,2003) COPT(UPDFLG+1)
C
      READ (WDMFL,REC=1) IBUF
      MAXREC= IBUF(29)
      FREREC= IBUF(31)
      WRITE(10,2000) FNAME,MAXREC,FREREC
      WRITE (*,2000) FNAME,MAXREC,FREREC
C     attribute fixed flag
      WRITE(10,2004) IBUF(112)
      WRITE (*,2004) IBUF(112)
C     dsn counts
      DO 25 I= 1,9
        LDCNT(I)= IBUF(30+I*2)
        DCNT(I) = 0
 25   CONTINUE
      WRITE(10,2002) (LDCNT(I),I=1,9)
      WRITE (*,2002) (LDCNT(I),I=1,9)
C
C     chains of like datasets
      ECNT= 0
      DO 29 I= 1,9
C       each dataset type
        IF (LDCNT(I) .GT. 0) THEN
C         some exist
          DNOW= 1
          CNOW= 1
          CDSN= IBUF(31+I*2)
          DCHN(CNOW)= CDSN
 26       CONTINUE
C           decide where the next dsn in chain starts
            DPT = 113+ (CDSN-1)/500
            DRC = IBUF(DPT)
            READ(WDMFL,REC=DRC,ERR=27) DBUF
            DOF = MOD(CDSN-1,500)+ 5
            CREC= DBUF(DOF)
            IF (CREC .EQ. 0) THEN
C             dataset in chain not found
              WRITE (*,2009) CDSN
              WRITE(10,2009) CDSN
              DNOW= LDCNT(I)
            ELSE IF (LDCNT(I) .GT. 1) THEN
C             find next dataset
              DNOW= DNOW+ 1
              CNOW= CNOW+ 1
              READ(WDMFL,REC=CREC,ERR=27) DBUF
              DCHN(CNOW)= DBUF(2)
              CDSN= DCHN(CNOW)
            END IF
            GO TO 28
C             read error on wdmfl
 27           CONTINUE
              WRITE (*,2009) CDSN
              WRITE(10,2009) CDSN
              ECNT= ECNT+ 1
              IF (ECNT .GT. 10) THEN
C               some kind of big problem
                STOP
              END IF
 28         CONTINUE
            IF (CNOW .EQ. 8 .OR. DNOW .EQ. LDCNT(I)) THEN
C             time to write part of chain
              IF (DNOW .LE. 8) THEN
C               include dataset type header
                WRITE (*,2006) DTYPE(I),(DCHN(J),J=1,CNOW)
                WRITE(10,2006) DTYPE(I),(DCHN(J),J=1,CNOW)
              ELSE
C               just part of chain
                WRITE (*,2007) (DCHN(J),J=1,CNOW)
                WRITE(10,2007) (DCHN(J),J=1,CNOW)
              END IF
              CNOW= 0
            END IF
C           more in this chain?
          IF (DNOW .LT. LDCNT(I)) GO TO 26
        END IF
 29   CONTINUE
C     assume records not used
      DO 30 I= 1,MAXREC
        USEREC(I)= 0
 30   CONTINUE
C     first record is master directory
      USEREC(1)= -3
C     look for more directories
      DPOS= 0
      WRITE (*,2005)
      WRITE(10,2005)
      DO 35 I= 113,512
        IF (IBUF(I) .NE. 0) THEN
C         directory record
          DPOS= DPOS+ 1
          DREC(DPOS)= IBUF(I)
          DOFF(DPOS)= (I-113)* 500
          WRITE (*,2010) DPOS,DREC(DPOS),DOFF(DPOS)+1,DOFF(DPOS)+500
          WRITE(10,2010) DPOS,DREC(DPOS),DOFF(DPOS)+1,DOFF(DPOS)+500
C         check directory details
          READ (WDMFL,REC=DREC(DPOS),ERR=33) DBUF
          GO TO 34
 33       CONTINUE
C           very bad news, read error on directory record
            WRITE (*,2038) DREC(DPOS),DPOS
            WRITE(10,2038) DREC(DPOS),DPOS
 34       CONTINUE
          CALL CHKDIR(MAXREC,DOFF(DPOS),DBUF)
          USEREC(DREC(DPOS))= -1
        END IF
 35   CONTINUE
      WRITE (*,2015)
      WRITE(10,2015)
C     free records
      IF (FREREC .GT. 0) THEN
        CHFLG= 1
        RECID= -2
        CALL CHAIN (WDMFL,FREREC,CHFLG,MAXREC,RECID,UPDFLG,PCHNLN,
     M              USEREC,
     O              CBUF,I)
        WRITE (*,2020) (CBUF(J),J=1,I)
        WRITE(10,2020) (CBUF(J),J=1,I)
C       save record number of last free record
        LSTFRE= CBUF(I)
      ELSE
C       no free records
        WRITE (*,2025)
        WRITE(10,2025)
      END IF
C
C     datasets
      CHFLG= 2
      DO 50 I= 1,DPOS
C       loop for each directory
        READ (WDMFL,REC=DREC(I),ERR=47) DBUF
        DO 45 J= 5,504
C         loop for each possible dataset
          IF (DBUF(J) .GT. 0) THEN
C           process a dataset
            CDSN= DOFF(I)+ J- 4
            READ (WDMFL,REC=DBUF(J),ERR=42) IBUF
            REVFG= 0
            IF (CDSN .NE. IBUF(5)) THEN
C             problem, may not be label
              WRITE (*,2032) CDSN,IBUF(5)
              WRITE(10,2032) CDSN,IBUF(5)
            ELSE
C             do details of dsn
              WRTFLG= 0
 36           CONTINUE
                REVFG= 0
                CALL CHAIN (WDMFL,DBUF(J),CHFLG,MAXREC,CDSN,UPDFLG,
     I                      PCHNLN,
     M                      USEREC,
     O                      CBUF,CLEN)
                IF (OUTLEV .GE. 1) THEN
                  IF (WRTFLG .EQ. 0) THEN
C                   header for dataset
                    WRITE (*,2030) CDSN,IBUF(6),DTYPE(IBUF(6)),CLEN
                    WRITE(10,2030) CDSN,IBUF(6),DTYPE(IBUF(6)),CLEN
                    WRTFLG= 1
                  END IF
C                 chain for dataset
                  WRITE(10,2040) (CBUF(K),K=1,CLEN)
                END IF
C               check the datasets pointers
                WRITE(10,2041) (IBUF(K),K=8,12)
                PDP   = IBUF(8)
                PUP   = IBUF(9)
                PSA   = IBUF(10)
                IF (PSA .GT. 0) THEN
C                 search attributes available
                  SACNT = IBUF(PSA)
                  PSASTR= IBUF(PSA+1)
                ELSE
C                 no search attributes available
                  SACNT = 0
                  PSASTR= 0
                END IF
                PDAT  = IBUF(11)
                PDATV = IBUF(12)
                IF (PDP .GT. 0) THEN
C                 to pointers
                  WRITE(10,2042) (IBUF(PDP))
                END IF
                IF (PUP .GT. 0) THEN
C                 from pointers
                  WRITE(10,2043) (IBUF(PUP))
                END IF
                WRITE(10,2044) SACNT,PSASTR
                IF (PSA .GT. 0) THEN
C                 search attributes
                  IF (SACNT .GT. 0) THEN
                    DO 37 K= 1,SACNT
C                     details of each attribute
                      L= PSA+ (2*K)
                      WRITE(10,2045) K,IBUF(L),IBUF(L+1)
                      IF (IBUF(L) .EQ. 1) THEN
                        ATNAME= 'TSTYPE'
                        M     = IBUF(L+1)
                        WRITE(10,2046) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 2) THEN
                        ATNAME= ' STAID'
                        M     = IBUF(L+1)
                        WRITE(10,2046) ATNAME,(IBUF(L),L=M,M+3)
                      ELSE IF (IBUF(L) .EQ. 17) THEN
                        ATNAME= ' TCODE'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 27) THEN
                        ATNAME= ' TSBYR'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 32) THEN
                        ATNAME= 'TSFILL'
                        M     = IBUF(L+1)
                        WRITE(10,2048) ATNAME,RBUF(M)
                      ELSE IF (IBUF(L) .EQ. 33) THEN
                        ATNAME= 'TSSTEP'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 34) THEN
                        ATNAME= 'TGROUP'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 40) THEN
                        ATNAME= 'AGENCY'
                        M     = IBUF(L+1)
                        WRITE(10,2046) ATNAME,(IBUF(L),L=M,M+1)
                      ELSE IF (IBUF(L) .EQ. 45) THEN
                        ATNAME= 'STANAM'
                        M     = IBUF(L+1)
                        WRITE(10,2046) ATNAME,(IBUF(L),L=M,M+11)
                      ELSE IF (IBUF(L) .EQ. 51) THEN
                        ATNAME= 'ISTAID'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 83) THEN
                        ATNAME= 'COMPFG'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 84) THEN
                        ATNAME= 'TSFORM'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      ELSE IF (IBUF(L) .EQ. 85) THEN
                        ATNAME= 'VBTIME'
                        M     = IBUF(L+1)
                        WRITE(10,2047) ATNAME,IBUF(M)
                      END IF
 37                 CONTINUE
                  END IF
                END IF
C               check free space pointer
                CALL WDPTSP (IBUF(PDAT+1),PREC,POFF)
                IF (OUTLEV .GE. 1) THEN
                  WRITE(10,2110) PREC,POFF
                END IF
                IF (PREC .NE. CBUF(CLEN)) THEN
C                 bad free space pointer
                  IF (OUTLEV .EQ. 0) THEN
C                   need to know what dsn
                    WRITE (*,2030) CDSN,IBUF(6),DTYPE(IBUF(6))
                    WRITE(10,2030) CDSN,IBUF(6),DTYPE(IBUF(6))
                  END IF
                  WRITE (*,2060) PREC
                  WRITE(10,2060) PREC
                  IF (UPDFLG .EQ. 1) THEN
C                   update the forward pointer in the right last record
                    READ (WDMFL,REC=PREC) UBUF
                    UBUF(4)= 0
                    WRITE(WDMFL,REC=PREC) UBUF
                    REVFG= 1
                  END IF
                END IF
                IF (IBUF(6) .EQ. 1) THEN
C                 timeseries dataset, check timeseries groups
                  DCNT(1)= DCNT(1)+ 1
                  IF (OUTLEV .GE. 2) THEN
                    WRITE(10,2071)
                  END IF
                  CALL CHKTIM (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                         OUTLEV,PREC,POFF,
     M                         IBUF,REVFG)
                ELSE IF (IBUF(6) .EQ. 2) THEN
C                 table dataset
                  DCNT(2)= DCNT(2)+ 1
                  IF (OUTLEV .GE. 2) THEN
                    WRITE(10,2072)
                  END IF
                  CALL CHKTAB (WDMFL,CLEN,CBUF,PDAT,PDATV,
     I                         OUTLEV,
     M                         IBUF,REVFG)
                ELSE IF (IBUF(6) .EQ. 3) THEN
C                 schematic dataset
                  DCNT(3)= DCNT(3)+ 1
                ELSE IF (IBUF(6) .EQ. 4) THEN
C                 project dataset
                  DCNT(4)= DCNT(4)+ 1
                ELSE IF (IBUF(6) .EQ. 5) THEN
C                 vector dataset, check cluster
                  DCNT(5)= DCNT(5)+ 1
                  IF (OUTLEV .GE. 2) THEN
                    WRITE(10,2075)
                  END IF
                  CALL CHKVEC (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                         OUTLEV,PREC,POFF,
     M                         IBUF,REVFG)
                ELSE IF (IBUF(6) .EQ. 6) THEN
C                 rastor dataset
                  DCNT(6)= DCNT(6)+ 1
                ELSE IF (IBUF(6) .EQ. 7) THEN
C                 space-time dataset
                  DCNT(7)= DCNT(7)+ 1
                  IF (OUTLEV .GE. 2) THEN
                    WRITE(10,2077)
                  END IF
                  CALL CHKSPT (WDMFL,CLEN,CBUF,PDAT,PDATV,
     I                         OUTLEV,
     M                         IBUF,REVFG)
                ELSE IF (IBUF(6) .EQ. 8) THEN
C                 attribute dataset
                  DCNT(8)= DCNT(8)+ 1
C                 check cluster
                  IF (OUTLEV .GE. 2) THEN
                    WRITE(10,2078)
                  END IF
                  CALL CHKATR (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                         OUTLEV,PREC,POFF,
     M                         IBUF,REVFG)
                ELSE IF (IBUF(6) .EQ. 9) THEN
                  DCNT(9)= DCNT(9)+ 1
C                 message dataset, check cluster
                  IF (OUTLEV .GE. 2) THEN
                    WRITE(10,2079)
                  END IF
                  CALL CHKMES (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                         OUTLEV,PREC,POFF,
     M                         IBUF,REVFG)
                END IF
C
                IF (REVFG .NE. 0) THEN
                  WRITE (*,2080)
                  WRITE(10,2080)
C                 clear out used rec buffer
                  DO 38 K=1,CLEN
                    USEREC(CBUF(K))= 0
38                CONTINUE
                END IF
              IF (REVFG .NE. 0) GO TO 36
            END IF
          END IF
          GO TO 44
 42       CONTINUE
C           very bad news, read error on dsn label record
            WRITE (*,2035) CDSN,DOFF(J)
            WRITE(10,2035) CDSN,DOFF(J)
 44       CONTINUE
 45     CONTINUE
        GO TO 48
 47     CONTINUE
C         very bad news, read error on directory record
          WRITE (*,2038) DREC(I),I
          WRITE(10,2038) DREC(I),I
 48     CONTINUE
 50   CONTINUE
C
      DO 55 I= 1,512
        UBUF(I)= 0
 55   CONTINUE
C
C     look for missing recs in chains
      WRITE (*,2100)
      WRITE(10,2100)
      DONFRE= 0
      MISCNT= 0
      DO 60 I= 1,MAXREC
        IF (USEREC(I) .EQ. 0) THEN
C         missing record
          MISCNT= MISCNT+ 1
          READ (WDMFL,REC=I) (IBUF(K),K=1,4)
          IF (IBUF(1) .EQ. 0 .AND. IBUF(3) .EQ. 0 .AND.
     1        IBUF(4) .EQ. 0) THEN
C           put this in the free rec chain
            ADDFRE= 1
          ELSE
            WRITE (*,2050) I,(IBUF(K),K=1,4)
            WRITE(10,2050) I,(IBUF(K),K=1,4)
            IF (UPDFLG .EQ. 1) THEN
              WRITE (*,*) 'ADD THIS REC TO FREE RECORD CHAIN?'
              READ (*,1010) ANS
              IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN
                ADDFRE= 1
              ELSE
                ADDFRE= 0
              END IF
            ELSE
C             dont add it
              ADDFRE= 0
            END IF
          END IF
          IF (ADDFRE .EQ. 1) THEN
            WRITE (WDMFL,REC=I) UBUF
            READ  (WDMFL,REC=LSTFRE) UBUF
            UBUF(2)= I
            WRITE (WDMFL,REC=LSTFRE) UBUF
            UBUF(2)= 0
            LSTFRE= I
            WRITE (*,2090) I
            WRITE(10,2090) I
            DONFRE= 1
          END IF
        END IF
 60   CONTINUE
C     map of record usage
      WRITE (*,2140)
      WRITE(10,2140)
      DO 65 I= 1,MAXREC,10
        J= I+ 9
        IF (J .GT. MAXREC) J= MAXREC
        WRITE (*,2150) I,J,(USEREC(K),K=I,J)
        WRITE(10,2150) I,J,(USEREC(K),K=I,J)
 65   CONTINUE
C
      IF (DONFRE .NE. 0) THEN
C       revised free record chain
        DO 70 I= 1,MAXREC
          USEREC(I)= 0
 70     CONTINUE
        CHFLG= 1
        RECID= -2
        CALL CHAIN (WDMFL,FREREC,CHFLG,MAXREC,RECID,UPDFLG,PCHNLN,
     M              USEREC,
     O              CBUF,I)
        WRITE (*,2020) (CBUF(J),J=1,I)
        WRITE(10,2020) (CBUF(J),J=1,I)
      END IF
C
      IF (MISCNT .EQ. 0) THEN
C       no records missing from chains
        WRITE (*,2095)
        WRITE(10,2095)
      END IF
C
C     check dsn counts
      MISCNT= 0
      WRITE (*,2120)
      WRITE(10,2120)
      DO 80 I= 1,9
        IF (DCNT(I) .NE. LDCNT(I)) THEN
C         problem, counts dont match
          MISCNT= MISCNT+ 1
          WRITE (*,2125) I,DCNT(I),LDCNT(I)
          WRITE(10,2125) I,DCNT(I),LDCNT(I)
        END IF
 80   CONTINUE
C
      IF (MISCNT .EQ. 0) THEN
C       good news
        WRITE (*,2130)
        WRITE(10,2130)
      END IF
C
      WRITE(*,2200)
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHAIN
     I                   (WDMFL,FSTREC,CHFLG,MAXREC,RECID,UPDFLG,PCHNLN,
     M                    USEREC,
     O                    CBUF,NUMREC)
C
C     + + + PURPOSE + + +
C     determine chain of records
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,FSTREC,CHFLG,MAXREC,RECID,UPDFLG,PCHNLN,
     1          USEREC(MAXREC),CBUF(PCHNLN),NUMREC
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     FSTREC - First record in chain
C     CHFLG  - check flag, 1 - forward only, 2 - forward and back
C     MAXREC - maximum record on WDM file
C     RECID  - record id
C     UPDFLG - update flag
C     PCHNLN - max length of record chain
C     USEREC - record usage accumulator
C     CBUF   - buffer to store record numbers of chain in
C     NUMREC - number of records in chain
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     NXTREC,PTB(2),PTF(2),LSTREC,TBUF(512),I
      CHARACTER*1 ANS
C
C     + + + INPUT FORMATS + + +
1000  FORMAT (A1)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (' **** PROBLEM **** RECORD ALREADY IN CHAIN:',I5)
2010  FORMAT (' **** PROBLEM **** BAD BACK POINTER,CUR,BK:',2I5)
2020  FORMAT (' ** NOTE ** TOO MANY RECORDS (',
     1          I5,') FOR WDMRX IN DSN',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      NXTREC= FSTREC
      NUMREC= 0
      PTB(CHFLG)= 0
 10   CONTINUE
        READ (WDMFL,REC=NXTREC) (PTB(I),PTF(I),I=1,2)
        NUMREC      = NUMREC+ 1
        IF (NUMREC .LE. PCHNLN) THEN
C         save next record in chain
          CBUF(NUMREC)= NXTREC
        ELSE
          WRITE (*,2020) NUMREC,RECID
          WRITE(10,2020) NUMREC,RECID
        END IF
        IF (CHFLG .EQ. 2) THEN
          IF (NUMREC .GT. 1 .AND. LSTREC .NE. PTB(CHFLG)) THEN
C           bad back pointer
            WRITE (*,2010) NXTREC,PTB(CHFLG)
            WRITE(10,2010) NXTREC,PTB(CHFLG)
            IF (UPDFLG .EQ. 1) THEN
              WRITE (*,*) 'ATTEMPT TO CORRECT(0 FP PREV REC)?'
              READ (*,1000) ANS
              IF (ANS .EQ. 'Y') THEN
                READ (WDMFL,REC=LSTREC) TBUF
                TBUF(4)= 0
                WRITE(WDMFL,REC=LSTREC) TBUF
                NXTREC= 0
              END IF
            END IF
          END IF
        END IF
        IF (NXTREC .NE. 0) THEN
          IF (USEREC(NXTREC) .EQ. 0) THEN
            USEREC(NXTREC)= RECID
            LSTREC= NXTREC
            NXTREC= PTF(CHFLG)
          ELSE
            WRITE (*,2000) USEREC(NXTREC)
            WRITE(10,2000) USEREC(NXTREC)
            NXTREC= 0
          END IF
        END IF
      IF (NXTREC .GT. 0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKTIM
     I                    (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                     OUTLEV,FREC,FOFF,
     M                     IBUF,REVFG)
C
C     + + + PURPOSE + + +
C     check groups and blocks of a timeseries data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,CLEN,CBUF(CLEN),IBUF(512),REVFG,PDAT,PDATV,
     1          FREC,FOFF,OUTLEV,CDSN
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     CLEN   - Length of record chain
C     CBUF   - Record numbers in record chain
C     PDAT   - Pointer to data group pointers
C     PDATV  - Pointer to start of data values
C     CDSN   - Data set number of current dataset
C     OUTLEV - Output level (0-WDM,1-DSN,2-GRP,3-BLK)
C     FREC   - Free record pointer from label
C     FOFF   - Free record offset from label
C     IBUF   - Data set label
C     REVFG  - Revisions made flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   SDAT(6),TDAT(6),GDAT(6),CDAT(6),TSTEP,SAIND,POS,
     1          PREC,POFF,MATREC,I,J,K,L,TBUF(512),NDAT(6),PSA,
     2          NSKP,CNOV,CTST,CTUN,CCMP,CQUA,TGROUP,DONFG,GRPCNT
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV,TIMCHK
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV,TIMCHK,TIMADD,WDPTSP,WDATSP,WBCWSP,COPYI
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT (' **** PROBLEM **** GRP STR REC NOT IN CHN :',6I5)
2002  FORMAT (' **** PROBLEM **** POINTER TO SEARCH ATTR :',I5)
2005  FORMAT (8X,' TGROUP:   ',I5,/,
     2        8X,' BASE DATE:',I5,5I3)
2010  FORMAT (' **** PROBLEM **** NO DATE MATCH, DIR DATE:',I5,5I3,/,
     1        '         GRP:',I6,'               DAT DATE:',I5,5I3)
2015  FORMAT (' **** PROBLEM **** REC,OFFSET,SKIP:',3I10)
2020  FORMAT (' #### ATTEMPT TO INCLUDE GRP STR IN CHN ####',I5)
2030  FORMAT (8X,' GROUP #:',I4,'  HAS VALID DATES: ',I5,5I3)
2040  FORMAT (12X,' NEW RECORD:',I6,' OFFSET:',I4)
2050  FORMAT (12X,' BLOCK REC :',I6,' OFF:',I4,' NOV:',I6,' TST:',I4,
     1            ' TUN:',I4,' CMP:',I2,' QUA:',I3,' NSKP:',I6,
     2            ' DAT:',I5,5I3)
2060  FORMAT (12X,' END OF GROUP AT:',I5,5I3)
2065  FORMAT (12X,' COUNT OF GROUPS:',I5)
2069  FORMAT (' *    NOTICE     * DATA ENDS AT:',2I6,' EXPECTED:',2I6)
2070  FORMAT (' **** PROBLEM **** DATA ENDS AT:',2I6,' EXPECTED:',2I6)
2080  FORMAT (/,' DSN: ',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      TGROUP = 6
      TSTEP  = 1
      TDAT(1)= 1899
      TDAT(2)= 1
      TDAT(3)= 1
      TDAT(4)= 0
      TDAT(5)= 0
      TDAT(6)= 0
      GDAT(5)= 0
      GDAT(6)= 0
C     pointer to search attributes
      PSA  = IBUF(10)
C
      IF (PSA.GT.0 .AND. PSA.LE.512) THEN
C       search attributes available
        DO 10 I= 1,4
          SAIND= I+ 26
          POS= WDSASV(SAIND,IBUF)
          IF (POS .GT. 0) THEN
C           update base yr,mo,dy, or hr if available
            TDAT(I)= IBUF(POS)
          END IF
10      CONTINUE
C
        SAIND= 34
        POS  = WDSASV(SAIND,IBUF)
        IF (POS .GT. 0) THEN
C         update if available
          TGROUP= IBUF(POS)
        END IF
      ELSE
C       problem with search attribute pointer
        WRITE(10,2002) PSA
      END IF
C
      IF (OUTLEV .GT. 0) THEN
        WRITE(10,2005) TGROUP,TDAT
      END IF
C
C     assume data ends at free record, position
      PREC  = FREC
      POFF  = FOFF
      NSKP  = 0
C
      GRPCNT= 0
      K     = PDAT+ 1
20    CONTINUE
        K= K+ 1
        IF (IBUF(K) .NE. 0) THEN
C         an active group
          GRPCNT= GRPCNT+ 1
          I     = K- PDAT- 2
          CALL TIMADD (TDAT,TGROUP,TSTEP,I,
     O                 SDAT)
          CALL WDPTSP (IBUF(K),PREC,POFF)
          MATREC= 0
          DO 30 L= 1,CLEN
            IF (CBUF(L) .EQ. PREC) THEN
              MATREC= CBUF(L)
            END IF
30        CONTINUE
          READ (WDMFL,REC=PREC,ERR=40) TBUF
40        CONTINUE
          CALL WDATSP (TBUF(POFF),GDAT)
          IF (MATREC .EQ. 0) THEN
C           problem group starting record not in chain for this dataset
            WRITE (*,2000) PREC,I,(SDAT(J),J=1,4)
            WRITE(10,2000) PREC,I,(SDAT(J),J=1,4)
            IF (TBUF(3) .EQ. CBUF(CLEN)) THEN
C             back pointer is right, fix it
              READ (WDMFL,REC=CBUF(CLEN)) TBUF
              TBUF(4)= PREC
              WRITE(WDMFL,REC=CBUF(CLEN)) TBUF
              WRITE (*,2020) CBUF(CLEN)
              WRITE(10,2020) CBUF(CLEN)
              REVFG= 1
            END IF
          ELSE
            IF (TIMCHK(SDAT,GDAT) .NE. 0) THEN
C             dates dont match
              WRITE (*,2010) GDAT,I,SDAT
              WRITE(10,2010) GDAT,I,SDAT
            ELSE IF (OUTLEV .GE. 2) THEN
              WRITE(10,2030) I,GDAT
            END IF
C           determine start of next group
            I= 1
            CALL TIMADD (GDAT,TGROUP,TSTEP,I,
     O                   NDAT)
C           chk blocks
            NSKP = 1
            DONFG= 0
 50         CONTINUE
              POFF= POFF+ NSKP
              IF (POFF .EQ. 512) THEN
                POFF= 513
              END IF
              IF (POFF .GT. 5000) THEN
C               bad offset
                WRITE (*,2015) PREC,POFF,NSKP
                WRITE(10,2015) PREC,POFF,NSKP
C               force done
                POFF= 1
                TBUF(POFF)= 0
                NSKP= 0
              ELSE IF (POFF .GT. 512) THEN
C               need the next record
                PREC= TBUF(4)
                POFF= POFF- 508
                IF (OUTLEV .GE. 3) THEN
                  WRITE(10,2040) PREC,POFF
                END IF
                READ(WDMFL,REC=PREC,ERR=70) TBUF
 70             CONTINUE
              END IF
              IF (TBUF(POFF) .GT. 0) THEN
C               chk this block
                CALL WBCWSP (TBUF(POFF),
     O                       CNOV,CTST,CTUN,CCMP,CQUA)
                IF (CCMP .EQ. 0) THEN
C                 uncompressed
                  NSKP= CNOV+ 1
                ELSE
C                 compressed
                  NSKP= 2
                END IF
                CALL TIMADD (SDAT,CTUN,CTST,CNOV,
     O                       CDAT)
                IF (OUTLEV .GE. 3) THEN
                  WRITE(10,2050) PREC,POFF,CNOV,CTST,CTUN,CCMP,CQUA,
     1                           NSKP,SDAT
                END IF
C               update current date
                CALL COPYI(6,CDAT,SDAT)
                IF (TIMCHK(SDAT,NDAT) .LE. 0) THEN
C                 all done this group
                  DONFG= 1
                END IF
              ELSE
C               all done
                DONFG= 1
              END IF
            IF (DONFG .EQ. 0) GO TO 50
            IF (OUTLEV .GE. 3) THEN
C             show end of group
              WRITE(10,2060) SDAT
            END IF
          END IF
        END IF
      IF (REVFG .EQ. 0 .AND. K .LT. PDATV-1) GO TO 20
      WRITE(10,2065) GRPCNT
C     update to where free position is
      POFF= POFF+ NSKP
      IF (POFF .GT. 512) THEN
C       need the next record
        PREC= TBUF(4)
        POFF= POFF- 508
      END IF
C
      IF (PREC .NE. FREC .OR. POFF .NE. FOFF) THEN
C       data ends at wrong spot (may be ok)
        IF (OUTLEV .EQ. 0) THEN
C         show what dsn
          WRITE (*,2080) CDSN
          WRITE(10,2080) CDSN
        END IF
        IF (POFF .EQ. 512 .AND. FOFF .EQ. 511) THEN
C         ok, we dont allow ending on 512
          WRITE (*,2069) PREC,POFF,FREC,FOFF
          WRITE(10,2069) PREC,POFF,FREC,FOFF
        ELSE
C         not an ok situation
          WRITE (*,2070) PREC,POFF,FREC,FOFF
          WRITE(10,2070) PREC,POFF,FREC,FOFF
        END IF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKTAB
     I                    (WDMFL,CLEN,CBUF,PDAT,PDATV,
     I                     OUTLEV,
     M                     IBUF,REVFG)
C
C     + + + PURPOSE + + +
C     check groups and blocks of a table data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,CLEN,CBUF(CLEN),IBUF(512),REVFG,PDAT,PDATV,
     1          OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     CLEN   - Length of record chain
C     CBUF   - Record numbers in record chain
C     PDAT   - Pointer to data group pointers
C     PDATV  - Pointer to start of data values
C     OUTLEV - Output level (0-WDM,1-DSN,2-GRP,3-BLK)
C     IBUF   - Data set label
C     REVFG  - Revisions made flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      MATREC,I,K,L,TBUF(512),
     1             PREC,POFF,TABID,TABDIM,
     2             TGRP,TQNU,TIND,TROW,TSPA,ASPA
      CHARACTER*16 MTBNAM
      CHARACTER*1  MFID(2)
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDPTSP,WTBDSP,WTBISP
C
C     + + + FORMATS + + +
2000  FORMAT (' **** PROBLEM **** GRP STR REC NOT IN CHN :',3I6,1X,
     1         A16,2I12)
2020  FORMAT (' #### ATTEMPT TO INCLUDE GRP STR IN CHN ####',I5)
2030  FORMAT (8X,' GROUP #:',I4,' IS AT',2I6,' NAME: ',A16,/,
     1        12X,'MFID:  ',2A1,' TGRP:',I4,' TQNU:',I4,/,
     2        12X,'TIND:',I4,' TROW:',I4,' TSPA:',I4,' ASPA:',I4)
2040  FORMAT (4A4)
C
C     + + + END SPECIFICATIONS + + +
C
      REVFG= 0
      K    = PDAT+ 1
      I    = 0
20    CONTINUE
        I= I+ 1
        K= K+ 1
        IF (IBUF(K).NE.0) THEN
C         split label info word to check for type and attribute match
          WRITE(MTBNAM,2040) IBUF(K),IBUF(K+1),IBUF(K+2),IBUF(K+3)
          K= K+ 4
          TABID= IBUF(K)
          CALL WTBISP (TABID,
     O                 MFID,TGRP,TQNU)
          K= K+ 1
          TABDIM= IBUF(K)
          CALL WTBDSP (TABDIM,
     O                 TIND,TROW,TSPA,ASPA)
          K= K+ 1
          CALL WDPTSP (IBUF(K),PREC,POFF)
          MATREC= 0
          DO 30 L= 1,CLEN
            IF (CBUF(L).EQ.PREC) MATREC= CBUF(L)
30        CONTINUE
          READ (WDMFL,REC=PREC,ERR=40) TBUF
40        CONTINUE
          IF (MATREC.EQ.0) THEN
            WRITE (*,2000) I,PREC,POFF,MTBNAM,TABID,TABDIM
            WRITE(10,2000) I,PREC,POFF,MTBNAM,TABID,TABDIM
            IF (TBUF(3).EQ.CBUF(CLEN)) THEN
C             back pointer is right, fix it
              READ (WDMFL,REC=CBUF(CLEN)) TBUF
              TBUF(4)= PREC
              WRITE(WDMFL,REC=CBUF(CLEN)) TBUF
              WRITE (*,2020) CBUF(CLEN)
              WRITE(10,2020) CBUF(CLEN)
              REVFG= 1
            END IF
          ELSE IF (OUTLEV.GE.2) THEN
            WRITE(10,2030) I,PREC,POFF,MTBNAM,MFID,TGRP,TQNU,
     1                     TIND,TROW,TSPA,ASPA
          END IF
        END IF
      IF (REVFG.EQ.0.AND.K.LT.PDATV-2) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKMES
     I                    (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                     OUTLEV,FREC,FOFF,
     M                     IBUF,REVFG)
C
C     + + + PURPOSE + + +
C     check groups and blocks of a message data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,CLEN,CBUF(CLEN),IBUF(512),REVFG,PDAT,PDATV,
     1          CDSN,OUTLEV,FREC,FOFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     CLEN   - Length of record chain
C     CBUF   - Record numbers in record chain
C     PDAT   - Pointer to data group pointers
C     PDATV  - Pointer to start of data values
C     CDSN   - Data set number of current dataset
C     OUTLEV - Output level (0-WDM,1-DSN,2-GRP,3-BLK)
C     FREC   - Free record pointer from label
C     FOFF   - Free record offset from label
C     IBUF   - Data set label
C     REVFG  - Revisions made flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MATREC,I,K,L,TBUF(512),POS,PREC,POFF,SAIND,
     1          CLASS,ID,ORDER,ILEN,PSA
C
C     + + + FUNCTIONS + + +
      INTEGER   WDSASV
C
C     + + + INTRINSICS + + +
      INTRINSIC MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDSASV,WDPTSP,WMSBCS
C
C     + + + FORMATS + + +
2000  FORMAT (' **** PROBLEM **** GRP STR REC NOT IN CHN :',3I5)
2002  FORMAT (' **** PROBLEM **** POINTER TO SEARCH ATTR :',I5)
2020  FORMAT (' #### ATTEMPT TO INCLUDE GRP STR IN CHN ####',I5)
2030  FORMAT (8X,' GROUP #:',I4,' IS AT',2I6)
2040  FORMAT (8X,' CLUSTER: ',2A4)
2050  FORMAT (12X,' NEW RECORD:',I6,' OFFSET:',I4)
2060  FORMAT (12X,' BLOCK REC:',I6,' OFF:',I4,
     1            ' CLASS:',I3,' ID:',I3,' ORDER:',I3,
     2            ' BLEN:',I5,' WLEN:',I5)
2070  FORMAT (' **** PROBLEM **** DATA ENDS AT:',2I6,' EXPECTED:',2I6)
2080  FORMAT (/,' DSN: ',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      PSA  = IBUF(10)
      IF (PSA.GT.0 .AND. PSA.LE.512) THEN
C       search attributes available
        SAIND= 263
        POS  = WDSASV(SAIND,IBUF)
        IF (POS.EQ.0) THEN
C         old messie used wrong index
          SAIND= 260
          POS  = WDSASV(SAIND,IBUF)
        END IF
        IF (POS.GT.0) THEN
C         group name exists
          WRITE(10,2040) IBUF(POS),IBUF(POS+1)
        END IF
      ELSE
C       problem with search attribute pointer
        WRITE(10,2002) PSA
      END IF
C
      K= PDAT+ 1
      I= 0
20    CONTINUE
        I= I+ 1
        K= K+ 1
        IF (IBUF(K).NE.0) THEN
          CALL WDPTSP (IBUF(K),PREC,POFF)
          MATREC= 0
          DO 30 L= 1,CLEN
            IF (CBUF(L).EQ.PREC) MATREC= CBUF(L)
30        CONTINUE
          READ (WDMFL,REC=PREC,ERR=40) TBUF
40        CONTINUE
          IF (MATREC.EQ.0) THEN
            WRITE (*,2000) I,PREC,POFF
            WRITE(10,2000) I,PREC,POFF
            IF (TBUF(3).EQ.CBUF(CLEN)) THEN
C             back pointer is right, fix it
              READ (WDMFL,REC=CBUF(CLEN)) TBUF
              TBUF(4)= PREC
              WRITE(WDMFL,REC=CBUF(CLEN)) TBUF
              WRITE (*,2020) CBUF(CLEN)
              WRITE(10,2020) CBUF(CLEN)
              REVFG= 1
            END IF
          ELSE
            IF (OUTLEV.GE.2) THEN
              WRITE(10,2030) I,PREC,POFF
            END IF
C           check blocks
            ILEN= -1
 50         CONTINUE
              POFF= POFF+ ILEN+ 1
              IF (POFF.GT.512) THEN
 60             CONTINUE
C                 need the next record
                  PREC= TBUF(4)
                  POFF= POFF- 508
                  IF (OUTLEV.GE.3) THEN
                    WRITE(10,2050) PREC,POFF
                  END IF
                  READ(WDMFL,REC=PREC,ERR=70) TBUF
 70               CONTINUE
                IF (POFF.GT.512) GO TO 60
              END IF
              IF (TBUF(POFF).GT.0) THEN
C               check this block
                CALL WMSBCS(TBUF(POFF),
     O                      CLASS,ID,ORDER,ILEN)
C               adjust length to words
                L= ILEN/ 4
                IF (MOD(ILEN,4).GT.0) L= L+ 1
                IF (OUTLEV.GE.3) THEN
                  WRITE(10,2060) PREC,POFF,CLASS,ID,ORDER,ILEN,L
                END IF
                ILEN= L
              ELSE
C               all done
                ILEN= 0
              END IF
            IF (ILEN.GT.0) GO TO 50
          END IF
        END IF
      IF (REVFG.EQ.0.AND.K.LT.PDATV-2) GO TO 20
C
      POFF= POFF+ 1
      IF (POFF.GT.512) THEN
C       need the next record
        PREC= TBUF(4)
        POFF= POFF- 508
      END IF
C
      IF (PREC.NE.FREC .OR. POFF.NE.FOFF) THEN
C       data ends at wrong spot
        IF (OUTLEV.EQ.0) THEN
C         show what dsn
          WRITE (*,2080) CDSN
          WRITE(10,2080) CDSN
        END IF
        WRITE (*,2070) PREC,POFF,FREC,FOFF
        WRITE(10,2070) PREC,POFF,FREC,FOFF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKVEC
     I                    (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                     OUTLEV,FREC,FOFF,
     M                     IBUF,REVFG)
C
C     + + + PURPOSE + + +
C     check groups and blocks of a vector data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,CLEN,CBUF(CLEN),IBUF(512),REVFG,PDAT,PDATV,
     1          CDSN,OUTLEV,FREC,FOFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     CLEN   - Length of record chain
C     CBUF   - Record numbers in record chain
C     PDAT   - Pointer to data group pointers
C     PDATV  - Pointer to start of data values
C     CDSN   - Data set number of current dataset
C     OUTLEV - Output level (0-WDM,1-DSN,2-GRP,3-BLK)
C     FREC   - Free record pointer from label
C     FOFF   - Free record offset from label
C     IBUF   - Data set label
C     REVFG  - Revisions made flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   MATREC,I,K,L,TBUF(512),LTYPE,LATT1,LATT2,IDUM,
     1          PREC,POFF,LID,IVAL,ILEN
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDPTSP,WDLISP,WDLBSP
C
C     + + + FORMATS + + +
2000  FORMAT (' **** PROBLEM **** GRP STR REC NOT IN CHN :',7I6)
2020  FORMAT (' #### ATTEMPT TO INCLUDE GRP STR IN CHN ####',I5)
2030  FORMAT (8X,' GROUP #:',I3,' IS AT',2I6,' ATTR:',4I6)
2040  FORMAT (12X,' NEW RECORD:',I6,' OFFSET:',I4)
2050  FORMAT (12X,' BLOCK REC :',I6,' OFF:',I4,' LID:',I6,' IDUM:',I6,
     1            ' IVAL:',I6,' ILEN:',I6)
2070  FORMAT (' **** PROBLEM **** DATA ENDS AT:',2I6,' EXPECTED:',2I6)
2080  FORMAT (/,' DSN: ',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      K= PDAT+ 1
      I= 0
20    CONTINUE
        I= I+ 1
        K= K+ 1
        IF (IBUF(K).NE.0) THEN
C         split label info word to check for type and attribute match
          CALL WDLISP (IBUF(K),
     O                 LTYPE,LATT1,LATT2,IDUM)
          K= K+ 1
          CALL WDPTSP (IBUF(K),PREC,POFF)
          MATREC= 0
          DO 30 L= 1,CLEN
            IF (CBUF(L).EQ.PREC) MATREC= CBUF(L)
30        CONTINUE
          READ (WDMFL,REC=PREC,ERR=40) TBUF
40        CONTINUE
          IF (MATREC.EQ.0) THEN
            WRITE (*,2000) I,PREC,POFF,LTYPE,LATT1,LATT2,IDUM
            WRITE(10,2000) I,PREC,POFF,LTYPE,LATT1,LATT2,IDUM
            IF (TBUF(3).EQ.CBUF(CLEN)) THEN
C             back pointer is right, fix it
              READ (WDMFL,REC=CBUF(CLEN)) TBUF
              TBUF(4)= PREC
              WRITE(WDMFL,REC=CBUF(CLEN)) TBUF
              WRITE (*,2020) CBUF(CLEN)
              WRITE(10,2020) CBUF(CLEN)
              REVFG= 1
            END IF
          ELSE
            IF (OUTLEV.GE.2) THEN
              WRITE(10,2030) I,PREC,POFF,LTYPE,LATT1,LATT2,IDUM
            END IF
C           chk blocks
            ILEN= 0
 50         CONTINUE
              POFF= POFF+ ILEN+ 1
              IF (POFF.GT.512) THEN
 60             CONTINUE
C                 need the next record
                  PREC= TBUF(4)
                  POFF= POFF- 508
                  IF (OUTLEV.GE.3) THEN
                    WRITE(10,2040) PREC,POFF
                  END IF
                  READ(WDMFL,REC=PREC,ERR=70) TBUF
 70               CONTINUE
                IF (POFF.GT.512) GO TO 60
              END IF
              IF (TBUF(POFF).GT.0) THEN
C               chk this block
                CALL WDLBSP (TBUF(POFF),
     O                       LID,IDUM,IVAL,ILEN)
                IF (OUTLEV.GE.3) THEN
                  WRITE(10,2050) PREC,POFF,LID,IDUM,IVAL,ILEN
                END IF
              ELSE
C               all done
                ILEN= 0
              END IF
            IF (ILEN.GT.0) GO TO 50
          END IF
        END IF
      IF (REVFG.EQ.0.AND.K.LT.PDATV-2) GO TO 20
C
      POFF= POFF+ 1
      IF (POFF.GT.512) THEN
C       need the next record
        PREC= TBUF(4)
        POFF= POFF- 508
      END IF
C
      IF (PREC.NE.FREC .OR. POFF.NE.FOFF) THEN
C       data ends at wrong spot
        IF (OUTLEV.EQ.0) THEN
C         show what dsn
          WRITE (*,2080) CDSN
          WRITE(10,2080) CDSN
        END IF
        WRITE (*,2070) PREC,POFF,FREC,FOFF
        WRITE(10,2070) PREC,POFF,FREC,FOFF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKSPT
     I                    (WDMFL,CLEN,CBUF,PDAT,PDATV,
     I                     OUTLEV,
     M                     IBUF,REVFG)
C
C     + + + PURPOSE + + +
C     check groups and blocks of a space time data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,CLEN,CBUF(CLEN),IBUF(512),REVFG,PDAT,PDATV,
     1          OUTLEV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     CLEN   - Length of record chain
C     CBUF   - Record numbers in record chain
C     PDAT   - Pointer to data group pointers
C     PDATV  - Pointer to start of data values
C     OUTLEV - Output level (0-WDM,1-DSN,2-GRP,3-BLK)
C     IBUF   - Data set label
C     REVFG  - Revisions made flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   GSDAT(6), GTUN, GTST, GNOV, PREC, POFF, TBUF(512),
     1          K, I, L, MATREC
C
C     + + + EXTERNALS + + +
      EXTERNAL  WDATSP, WSTGSP, WDPTSP
C
C     + + + FORMATS + + +
2000  FORMAT (' **** PROBLEM **** GRP STR REC NOT IN CHN :',3I6,1X,
     1         9I5)
2020  FORMAT (' #### ATTEMPT TO INCLUDE GRP STR IN CHN ####',I5)
2030  FORMAT (8X,' GROUP #:',I4,' IS AT',2I6,' START:',I5,5I3,
     1        ' GTUN:',I5,' GTST:',I5,' GNOV:',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      REVFG= 0
      K    = PDAT+ 1
      I    = 0
20    CONTINUE
        I= I+ 1
        K= K+ 1
        IF (IBUF(K).NE.0) THEN
          CALL WDATSP (IBUF(K),
     O                 GSDAT)
          K= K+ 1
          CALL WSTGSP (IBUF(K),
     O                 GSDAT(5),GSDAT(6),GTUN,GTST,GNOV)
          K= K+ 1
          CALL WDPTSP (IBUF(K),
     O                 PREC,POFF)
          DO 30 L= 1,CLEN
            IF (CBUF(L).EQ.PREC) MATREC= CBUF(L)
30        CONTINUE
          READ (WDMFL,REC=PREC,ERR=40) TBUF
40        CONTINUE
          IF (MATREC.EQ.0) THEN
            WRITE (*,2000) I,PREC,POFF,GSDAT,GTUN,GTST,GNOV
            WRITE(10,2000) I,PREC,POFF,GSDAT,GTUN,GTST,GNOV
            IF (TBUF(3).EQ.CBUF(CLEN)) THEN
C             back pointer is right, fix it
              READ (WDMFL,REC=CBUF(CLEN)) TBUF
              TBUF(4)= PREC
              WRITE(WDMFL,REC=CBUF(CLEN)) TBUF
              WRITE (*,2020) CBUF(CLEN)
              WRITE(10,2020) CBUF(CLEN)
              REVFG= 1
            END IF
          ELSE IF (OUTLEV.GE.2) THEN
            WRITE (*,2030) I,PREC,POFF,GSDAT,GTUN,GTST,GNOV
            WRITE(10,2030) I,PREC,POFF,GSDAT,GTUN,GTST,GNOV
          END IF
        END IF
      IF (REVFG.EQ.0.AND.K.LT.PDATV-2) GO TO 20
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKATR
     I                    (WDMFL,CLEN,CBUF,PDAT,PDATV,CDSN,
     I                     OUTLEV,FREC,FOFF,
     M                     IBUF,REVFG)
C
C     + + + PURPOSE + + +
C     check groups and blocks of a attribute data set
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   WDMFL,CLEN,CBUF(CLEN),IBUF(512),REVFG,PDAT,PDATV,
     1          CDSN,OUTLEV,FREC,FOFF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMFL  - Fortran unit number of the WDM file
C     CLEN   - Length of record chain
C     CBUF   - Record numbers in record chain
C     PDAT   - Pointer to data group pointers
C     PDATV  - Pointer to start of data values
C     CDSN   - Data set number of current dataset
C     OUTLEV - Output level (0-WDM,1-DSN,2-GRP,3-BLK)
C     FREC   - Free record pointer from label
C     FOFF   - Free record offset from label
C     IBUF   - Data set label
C     REVFG  - Revisions made flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     MATREC,I,K,L,TBUF(512),PREC,POFF,ITMP,AIND,
     1            ID,ILEN,ATTYP,ATLEN,ATUSWD,ATUPD,ATUSE(10)
      CHARACTER*6 ATNAM
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, CHAR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WATWDS,WDPTSP,WATTUS,WATTSP
C
C     + + + FORMATS + + +
2000  FORMAT (' **** PROBLEM **** GRP STR REC NOT IN CHN :',3I5)
2020  FORMAT (' #### ATTEMPT TO INCLUDE GRP STR IN CHN ####',I5)
2030  FORMAT (8X,' GROUP #:',I4,' IS AT',2I6,
     1        ' NAME: ',A6,' INDEX:',I5,' TYPE:',I2,' LEN:',I3,
     2        ' USWD:',10I1,' UPDATE:',I3)
2050  FORMAT (12X,' NEW RECORD:',I6,' OFFSET:',I4)
2060  FORMAT (12X,' BLOCK REC:',I6,' OFF:',I4,
     1            ' ID:',I3,' BLEN:',I5,' WLEN:',I5)
2070  FORMAT (' **** PROBLEM **** DATA ENDS AT:',2I6,' EXPECTED:',2I6)
2080  FORMAT (/,' DSN: ',I5)
C
C     + + + END SPECIFICATIONS + + +
C
      K= PDAT+ 1
      I= 0
20    CONTINUE
        I= I+ 1
        K= K+ 1
        IF (IBUF(K).NE.0) THEN
C         start of name
          ITMP= IBUF(K)
          K   = K+ 1
          DO 21 L= 1,6
            ATNAM(L:L)= CHAR(MOD(ITMP,256))
            IF (L .EQ. 4) THEN
C             end of name and index number
              CALL WATWDS(IBUF(K),
     O                    ITMP,AIND)
              K= K+ 1
            ELSE
              ITMP= ITMP/ 256
            END IF
 21       CONTINUE
C         location of details
          CALL WDPTSP (IBUF(K),PREC,POFF)
C         attribute specs
          K= K+ 1
          CALL WATTSP(IBUF(K),
     O                ATTYP,ATLEN,ATUSWD,ATUPD)
          CALL WATTUS(ATUSWD,
     O                ATUSE)
C
          MATREC= 0
          DO 30 L= 1,CLEN
            IF (CBUF(L).EQ.PREC) MATREC= CBUF(L)
30        CONTINUE
          READ (WDMFL,REC=PREC,ERR=40) TBUF
40        CONTINUE
          IF (MATREC.EQ.0) THEN
            WRITE (*,2000) I,PREC,POFF
            WRITE(10,2000) I,PREC,POFF
            IF (TBUF(3).EQ.CBUF(CLEN)) THEN
C             back pointer is right, fix it
              READ (WDMFL,REC=CBUF(CLEN)) TBUF
              TBUF(4)= PREC
              WRITE(WDMFL,REC=CBUF(CLEN)) TBUF
              WRITE (*,2020) CBUF(CLEN)
              WRITE(10,2020) CBUF(CLEN)
              REVFG= 1
            END IF
          ELSE
            IF (OUTLEV.GE.2) THEN
              WRITE(10,2030) I,PREC,POFF,
     1          ATNAM,AIND,ATTYP,ATLEN,ATUSE,ATUPD
            END IF
C           check blocks
            ILEN= -1
 50         CONTINUE
              POFF= POFF+ ILEN+ 1
              IF (POFF.GT.512) THEN
 60             CONTINUE
C                 need the next record
                  PREC= TBUF(4)
                  POFF= POFF- 508
                  IF (OUTLEV.GE.3) THEN
                    WRITE(10,2050) PREC,POFF
                  END IF
                  READ(WDMFL,REC=PREC,ERR=70) TBUF
 70               CONTINUE
                IF (POFF.GT.512) GO TO 60
              END IF
              IF (TBUF(POFF).GT.0) THEN
C               check this block
                CALL WATWDS(TBUF(POFF),
     O                      ID,ILEN)
C               adjust length to words
                L= ILEN/ 4
                IF (MOD(ILEN,4).GT.0) L= L+ 1
                IF (OUTLEV.GE.3) THEN
                  WRITE(10,2060) PREC,POFF,ID,ILEN,L
                END IF
                ILEN= L
              ELSE
C               all done
                ILEN= 0
              END IF
            IF (ILEN.GT.0) GO TO 50
          END IF
        END IF
      IF (REVFG.EQ.0.AND.K.LT.PDATV-2) GO TO 20
C
      POFF= POFF+ 1
      IF (POFF.GT.512) THEN
C       need the next record
        PREC= TBUF(4)
        POFF= POFF- 508
      END IF
C
      IF (PREC.NE.FREC .OR. POFF.NE.FOFF) THEN
C       data ends at wrong spot
        IF (OUTLEV.EQ.0) THEN
C         show what dsn
          WRITE (*,2080) CDSN
          WRITE(10,2080) CDSN
        END IF
        WRITE (*,2070) PREC,POFF,FREC,FOFF
        WRITE(10,2070) PREC,POFF,FREC,FOFF
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   CHKDIR
     I                   (MAXREC,DOFF,DBUF)
C
C     + + + PURPOSE + + +
C     check directory records
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   MAXREC,DOFF,DBUF(512)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MAXREC - maximum record on WDM file
C     DOFF   - directory offset
C     DBUF   - directory record contents
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J,K,DSNFND(4),DSNREC(4)
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT('     DSN,REC:',4(I6,I8,2X))
C
C     + + + END SPECIFCATIONS + + +
C
      I= 0
      DO 10 J= 5,504
C       loop for each possible dataset
        IF (DBUF(J) .GT. 0) THEN
C         process a dataset
          I= I+ 1
          DSNFND(I)= DOFF+ J- 4
          DSNREC(I)= DBUF(J)
          IF (I .EQ. 4) THEN
            WRITE (*,2000) (DSNFND(K),DSNREC(K),K=1,I)
            WRITE(10,2000) (DSNFND(K),DSNREC(K),K=1,I)
            I= 0
          END IF
        END IF
 10   CONTINUE
C
      IF (I .GT. 0) THEN
C       clear out final stuff
        WRITE (*,2000) (DSNFND(K),DSNREC(K),K=1,I)
        WRITE(10,2000) (DSNFND(K),DSNREC(K),K=1,I)
      END IF
C
      RETURN
      END
