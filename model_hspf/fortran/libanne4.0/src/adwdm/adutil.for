C
C
C
      SUBROUTINE   ZSTCMA
     I                   (ICMD,IAV)
C
C     + + + PURPOSE + + +
C     set command availability
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER   ICMD,IAV
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ICMD   - command id number
C     IAV    - availability code =0 - NO, 1 - YES
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ICMD.GE.1.AND.ICMD.LE.26) THEN
C       valid command
        ZCMDAV(ICMD)= IAV
C       command status has changed
        ZCMDST= 1
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZSCINI
C
C     + + + PURPOSE + + +
C     initialize screen for a new menu
C
C     + + + COMMON BLOCKS + + +
C     control parameters
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     no active highlight
      ZHLLIN= 0
      ZHLLEN= 0
      ZHLCOL= 0
C
      IF (ZMNSAV.EQ.0) THEN
C       dont save any of old text
        IF (ZWLDFL .NE. 0) THEN
C         ZMNWLD was called, so file names matching the wildcard entered on
C         a file menu caused the number of lines actually in ZMNTXT to be
C         accurately reflected by ZWLDLI, but not ZMNNLI.
          ZMNNLI = ZWLDLI
          ZWLDFL = 0
        END IF
        DO 10 I= 1,ZMNNLI
          ZMNTXT(I)= ' '
          ZMNLEN(I)= 0
 10     CONTINUE
        ZMNNLI= 0
      END IF
C
C     remove centering
      ZCWID = 0
      ZCLEN = 0
C
      ZMNCSL= 1
C
C     assume no error
      ZERR  = 0
      ZESCST= 0
      ZRET  = 0
C
      IF (ZWN2ID.EQ.0) THEN
C       init lengths of text in middle box
        DO 20 I= 1, ZB2N
          ZB2LEN(I)= 0
 20     CONTINUE
      END IF
C
C     init lengths of text in third box
      DO 30 I= 1, ZB3N
        ZMSLEN(I)= 0
 30   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   GSTGLV
     I                   (GLOID,FLEN,ILEN,STRIN1,ICNT,IVAL)
C
C     + + + PURPOSE + + +
C     set global responses for use in a number of aide screens
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLEN,ILEN,ICNT,IVAL(ICNT)
      CHARACTER*8 GLOID
      CHARACTER*1 STRIN1(ILEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GLOID  - global response id
C     FLEN   - field length of this global response
C     ILEN   - total length of this global response
C     STRIN1 - this global response
C     ICNT   - count of global response values
C     IVAL   - values associated with global response
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'czglvl.inc'
C
C     + + + LOCAL VARIALBES + + +
      INTEGER   NXTPOS,FREPOS,NXTID,I,LEN,J
C
C     + + + END SPECIFICATONS + + +
C
C     assume first one
      NXTID = 1
      NXTPOS= 1
C
      IF (ZGLCNT .GT. 0) THEN
C       already have some, are we replacing?
        FREPOS = ZGLPOS(ZGLCNT)+ ZGLLEN(ZGLCNT)
 10     CONTINUE
          IF (GLOID .EQ. ZGLNAM(NXTID)) THEN
C           replacing this one, clear it out
            LEN= ZGLLEN(NXTID)
C           move values over
            DO 12 I= ZGLPOS(NXTID),FREPOS-LEN
              ZGLVAL(I)= ZGLVAL(I+LEN)
 12         CONTINUE
C           update positions
            NXTPOS = ZGLPOS(NXTID)
            DO 15 I= NXTID,ZGLCNT-1
              ZGLNAM(I)= ZGLNAM(I+1)
              ZGLPOS(I)= NXTPOS
              ZGLLEN(I)= ZGLLEN(I+1)
              NXTPOS   = NXTPOS+ ZGLLEN(I)
              ZGLFLN(I)= ZGLFLN(I+1)
              ZGLFCT(I)= ZGLFCT(I+1)
              DO 17 J= 1,MXASVL
                ZGLAVL(J,I)= ZGLAVL(J,I+1)
 17           CONTINUE
 15         CONTINUE
            NXTID = ZGLCNT
            ZGLCNT= ZGLCNT- 1
            IF (ZGLCNT .GT. 0) THEN
C             free at end of existing stuff
              FREPOS= ZGLPOS(ZGLCNT)+ ZGLLEN(ZGLCNT)
            ELSE
C             free at beginning
              FREPOS= 1
            END IF
          ELSE
C           try next one
            NXTID= NXTID+ 1
          END IF
C         more to check?
        IF (NXTID .LE. ZGLCNT) GO TO 10
C       update position
        NXTPOS= FREPOS
      END IF
C
      ZGLCNT= NXTID
      ZGLNAM(ZGLCNT)= GLOID
      ZGLPOS(ZGLCNT)= NXTPOS
      ZGLLEN(ZGLCNT)= ILEN
      ZGLFLN(ZGLCNT)= FLEN
      ZGLFCT(ZGLCNT)= ICNT
      DO 20 I= 1,ILEN
        ZGLVAL(NXTPOS)= STRIN1(I)
        NXTPOS= NXTPOS+ 1
 20   CONTINUE
      DO 30 I= 1,ICNT
        ZGLAVL(I,ZGLCNT)= IVAL(I)
 30   CONTINUE
      ZGLAVL(ICNT+1,ZGLCNT)= -999
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTGLV
     I                   (FLEN,ISGLVL,
     M                    STRIN1,
     O                    ILEN)
C
C     + + + PURPOSE + + +
C     get global responses for a given id
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     FLEN,ISGLVL,ILEN
      CHARACTER*1 STRIN1(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FLEN   - field length of this global response
C     ISGLVL - get only the single value specified
C     ILEN   - total length of this global response
C     STRIN1 - this global response
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'czglvl.inc'
C
C     + + + LOCAL VARIALBES + + +
      INTEGER     I,ID,IPOS,JPOS,LEN,IANS,ITMP,IF,IL,MLEN
      CHARACTER*8 GLOID,TMPNAM
C
C     + + + OUTPUT FORMATS + + +
2000  FORMAT(8A1)
C
C     + + + END SPECIFICATONS + + +
C
      IF (FLEN .GE. 9) THEN
        MLEN= 8
      ELSE
        MLEN= FLEN- 1
      END IF
      WRITE(GLOID,2000) (STRIN1(I),I=2,MLEN+1)
C
      ILEN= 0
      ID  = 0
 10   CONTINUE
        ID= ID+ 1
        TMPNAM= ZGLNAM(ID)
        IF (GLOID(1:MLEN) .EQ. TMPNAM(1:MLEN)) THEN
C         this is it
          IPOS= 1
          IF (FLEN .LE. ZGLFLN(ID)) THEN
C           truncate possible values
            LEN= FLEN
          ELSE
C           pad global values
            LEN= ZGLFLN(ID)
          END IF
          IF (ISGLVL .EQ. 0) THEN
C           all values being returned
            IF= 1
            IL= ZGLFCT(ID)
          ELSE
C           just one value being returned
            IF= ISGLVL
            IL= ISGLVL
          END IF
C
          DO 20 IANS= IF,IL
            JPOS= ZGLPOS(ID)+ (IANS-1)* ZGLFLN(ID)
            DO 15 ITMP= 1,LEN
              STRIN1(IPOS)= ZGLVAL(JPOS)
              IPOS= IPOS+ 1
              JPOS= JPOS+ 1
 15         CONTINUE
            IF (LEN .LT. FLEN) THEN
C             pad values
              DO 18 ITMP= LEN+1,FLEN
                STRIN1(IPOS)= ' '
                IPOS= IPOS+ 1
 18           CONTINUE
            END IF
 20       CONTINUE
          ID  = ZGLCNT
          ILEN= IPOS- 1
        END IF
      IF (ID .LT. ZGLCNT) GO TO 10
C
      IF (ILEN .EQ. 0) THEN
        WRITE(99,*) 'problem in GGTGLV, didnt find',GLOID,ZGLCNT
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   GGTGLA
     I                   (GLNAM,GLIND,
     O                    GLAVAL)
C
C     + + + PURPOSE + + +
C     get global response associated value for a given id and index
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     GLIND,GLAVAL
      CHARACTER*8 GLNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     GLNAM  - global value name
C     GLIND  - global value index number
C     GLAVAL - global value assoc value
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'czglvl.inc'
C
C     + + + LOCAL VARIALBES + + +
      INTEGER     ID
C
C     + + + END SPECIFICATONS + + +
C
      ID    = 0
      GLAVAL= -999
 10   CONTINUE
        ID= ID+ 1
        IF (GLNAM .EQ. ZGLNAM(ID)) THEN
C         this is it
          GLAVAL= ZGLAVL(GLIND,ID)
          ID= ZGLCNT+ 1
        END IF
      IF (ID .LT. ZGLCNT) GO TO 10
C
      IF (ID .EQ. ZGLCNT) THEN
C       didnt find it
        WRITE(99,*) 'problem in GGTGLA, didnt find',GLNAM,ZGLCNT
      END IF
C
      RETURN
      END
C
C
C
      INTEGER   FUNCTION ZCHKST
     I                         (ILEN,ICNT,OBUFF,LRSPST)
C
C     + + + PURPOSE + + +
C     check a string, using a global value if needed
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ILEN,ICNT,TLEN
      CHARACTER*1 LRSPST(ILEN,ICNT),OBUFF(ILEN)
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxrsl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     ISGVAL,ICHK,LCNT
      CHARACTER*1 TMPSTR(MXRSLN)
C
C     + + + FUNCTIONS + + +
      INTEGER     CHKSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    CHRCHR, GGTGLV, CHKSTR
C
C     + + + END SPECIFICATIONS + + +
C
      IF (LRSPST(1,1) .EQ. '?') THEN
C       global value, make temp copy
        CALL CHRCHR(ILEN,LRSPST,TMPSTR)
C       get the valid global values
        ISGVAL= 0
        CALL GGTGLV(ILEN,ISGVAL,
     M              TMPSTR,
     O              TLEN)
        LCNT= TLEN/ILEN
        ICHK= CHKSTR(ILEN,LCNT,OBUFF,TMPSTR)
      ELSE
C       local value, check it
        ICHK= CHKSTR(ILEN,ICNT,OBUFF,LRSPST)
      END IF
C
      ZCHKST= ICHK
C
      RETURN
      END
C
C
C
      SUBROUTINE   ZCNTER
     I                    (ZBN,ZHLLEN,
     M                     ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL,
     O                     ZCLEN,ZCWID)
C
C     + + + PURPOSE + + +
C     center menu text
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ZBN,ZHLLEN,ZHLLIN,ZHLCOL,ZCLEN,ZCWID,ZMNNLI,
     1             ZMNLEN(*)
      CHARACTER*78 ZMNTXT(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ZBN    - box length
C     ZHLLEN - highlight length
C     ZMNNLI - number of lines in menu
C     ZMNTXT - text of menu
C     ZMNLEN - length of text lines
C     ZHLLIN - highlighted line number
C     ZHLCOL - highlighted column number
C     ZCLEN  - number of lines moved to center
C     ZCWID  - number of characters moved to center
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,ILEN,MXLEN
      CHARACTER*78 ZDMTXT,BLNK
C
C     + + + FUNCTIONS + + +
      INTEGER      ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL     ZLNTXT
C
C     + + + END SPECIFICATIONS + + +
C
      BLNK= ' '
C
      MXLEN= 0
      DO 10 I= 1, ZMNNLI
C       find longest string
        ILEN= ZLNTXT(ZMNTXT(I))
        IF (ILEN.GT.MXLEN) MXLEN= ILEN
10    CONTINUE
C     center horizontally
      ZCWID = (78- MXLEN)/2
C
C     center message vertically
      IF (ZMNNLI.LT.ZBN-1) THEN
C       if only using part of data screen
        ZCLEN= (ZBN- ZMNNLI)/2
      ELSE
C       no vertical centering needed
        ZCLEN= 0
      END IF
C     adjust text array
      DO 20 I= ZMNNLI,1,-1
        IF (ZCWID.GT.0) THEN
C         center text horizontally
          ZDMTXT= BLNK(1:ZCWID) // ZMNTXT(I)
        ELSE
C         no centering required
          ZDMTXT= ZMNTXT(I)
        END IF
        ZMNTXT(I+ZCLEN)= ZDMTXT
        ILEN= ZLNTXT(ZMNTXT(I+ZCLEN))
        IF (ILEN .GT. ZMNLEN(I+ZCLEN)) THEN
C         write more than currently on screen
          ZMNLEN(I+ZCLEN)= ILEN
        END IF
 20   CONTINUE
      IF (ZCLEN.GT.0) THEN
C       clear first lines
        DO 500 I= 1,ZCLEN
          ZMNTXT(I)= ' '
          ZMNLEN(I)= 0
 500    CONTINUE
C       change the number of text lines
        ZMNNLI= ZMNNLI+ ZCLEN
      END IF
      IF (ZHLLEN.GT.0) THEN
C       move highlight down
        ZHLLIN= ZHLLIN+ ZCLEN
        ZHLCOL= ZHLCOL+ ZCWID
      END IF
C
      RETURN
      END
