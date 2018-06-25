C     ztwdmf.f 2.1 9/4/91
C
C
C
      SUBROUTINE   WMSGTP
     I                   (WDMSFL,DSN,GNUM,
     O                    CLASS,RETCOD)
C
C     + + + PURPOSE + + +
C     get message file dataset parameter info from WDM file
C     (gets both PRM1 and PRM2 class info)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,GNUM,CLASS,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number
C     GNUM   - question number to get
C     CLASS  - class of info (1- PRM1, 2- PRM2)
C     RETCOD - return code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cscren.inc'
      INCLUDE 'czoptn.inc'
      INCLUDE 'czhide.inc'
      INCLUDE 'zcntrl.inc'
      INCLUDE 'cclbak.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I0,I1,I4,I8,I48,I80,J,K,DREC,DPOS,LIND,QWORD,
     $            ITMP,ITMP2,ID,ORDER,TLEN,OLEN,MLEN,GLEN,ILEN,LMXRSL,
     $            ICOL,IPOS,RDWRFG,DONFG,TXTFG,TPOS,IVAL(4),CONT,
     $            IIND,RIND,DIND,CIND,OIND,FIND,CONFG,OPWID,OPHIDE,
     $            LMXFLD
      REAL        LDEF
      CHARACTER*1 TBUFF(80),CBLNK(1),CNONE(4)
      CHARACTER*8 FDNAME(MXFLD),HIDFNM(MXFLD)
C
C     + + + EQUIVALENCES + + +
      EQUIVALENCE (RVAL,IVAL)
      REAL         RVAL(2)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, WMSPIV, WDPTCL, ZCHKST, CHRINT, ZLNTXT,
     1            LENSTR, CKNBLV, STRFND, WOPWDC
      REAL        CHRDEC
      DOUBLE PRECISION CHRDPR
C
C     + + + INTRINSICS + + +
      INTRINSIC   INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WMSPIV, WDPTCL, ZCHKST, CHRINT, ZLNTXT, LENSTR
      EXTERNAL    WMSPIS, WMSBCS, WMSP2S, ZSCINI, CHRDEC, WMSCON, WOPWDC
      EXTERNAL    WMSFBC, WMSGTE, WDNXPS, WDNXDV, ZIPC, CHRDPR, CKNBLV
      EXTERNAL    WMSSKB, ZSTCMA, ZIPI, WMSSPS, WMSPOS, CARVAR, STRFND
      EXTERNAL    CHRCHR, WMSDLM, ZLJUST
C
C     + + + DATA INITIALIZATIONS + + +
      DATA I0,I1,I4,I8,I48,I80,RDWRFG/0,1,4,8,48,80,1/
      DATA CNONE/'n','o','n','e'/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      TPOS = 1
      DONFG= 0
      TXTFG= 0
      CONFG= 0
      BLNK = ' '
      FTI  = 'I'
      FTR  = 'R'
      FTD  = 'D'
      FTC  = 'C'
      FTO  = 'O'
      FTF  = 'F'
      HELP = '?'
      NFLDS= 0
      IIND = 0
      RIND = 0
      DIND = 0
      CIND = 0
      OIND = 0
      FIND = 0
      CBLNK(1)= ' '
      NMHDRW  = 0
C     init callback IDs
      CBCHID = 0
      CBFLID = 0
      CBEXID = 0
C     set local version of max fields to remove
C     any chance of parameter MXFLD being modified
      LMXFLD= MXFLD
C     set local version of max response buffer length to remove
C     any chance of parameter MXRSLN being modified
      LMXRSL= MXRSLN
C
C     check existence of question
      CALL WMSFBC (WDMSFL,DSN,GNUM,
     O             DREC,DPOS,QWORD)
C
      IF (QWORD.EQ.0) THEN
C       bad question
        RETCOD= -40
      ELSE
C       ok question, get details
        RETCOD= 0
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       initialize screen parameters
        IF (SPINIT.EQ.0) THEN
C         init response string
          CALL ZIPC (LMXRSL,BLNK,RSPSTR)
        END IF
        IF (ZWNFLG.EQ.0) THEN
C         init screen name
          ZSCNAM= ' '
        END IF
C       initialize screen
        CALL ZSCINI
        IF (HPINIT.EQ.0) THEN
C         init to no help (both general and specific)
          CALL ZSTCMA(1,0)
          GPTR= 0
          I= 64
          CALL ZIPI (I,I0,HPTR)
        ELSE
C         help set externally, turn it on
          CALL ZSTCMA(1,1)
        END IF
C       init to no boxes next to fields
        CALL ZIPI (LMXFLD,I0,OPBOX)
        CALL ZIPI (LMXFLD,I0,OPSET)
C       init option field conditionals
        CALL ZIPI (LMXFLD,I0,OPONOF)
        CALL ZIPI (MXCON,I0,ONFTFL)
        CALL ZIPI (MXCON,I0,ONFTVL)
C       no hidden fields
        NUMHID= 0
C       turn on 'oops'
        CALL ZSTCMA(8,1)
C       limits on
        CALL ZSTCMA(6,1)
C
 100    CONTINUE
C         get block control word
          LIND = WDRCGO(WDMSFL,DREC)
          QWORD= WIBUFF(DPOS,LIND)
          IF (QWORD.GT.0) THEN
C           get parameters
            CALL WMSBCS (QWORD,
     O                   CLASS,ID,ORDER,TLEN)
C
            IF (CLASS.EQ.5 .AND. ID.GE.3 .AND. ID.LE.7) THEN
              IF (ID.EQ.6) THEN
C               help for file class, change id to mimic PRM1 help
                ID= 15
              ELSE
C               set id to be ignored
                ID= 0
              END IF
            END IF
C
            IF (ID.GT.0) THEN
C             go to desired id
              GO TO (110,120,130,120,130,110,110,130,140,130,120,
     1               120,120,120,140,110,200,200,120,200,130,140,120),ID
C
 110          CONTINUE
C               get multiple records of text
                GLEN= 0
                MLEN= 0
                IF (ID.EQ.1 .OR. ID.EQ.16) THEN
C                 set I to current number of lines in menu
                  I= ZMNNLI
                ELSE
                  I   = 0
                END IF
 115            CONTINUE
C                 get records until end of id information
                  I= I+ 1
                  CALL WMSGTE (WDMSFL,TLEN,I80,
     M                         DREC,DPOS,GLEN,MLEN,
     O                         OLEN,TBUFF,CONT)
                  IF (ID.EQ.1) THEN
C                   screen text
                    TXTFG= 1
                    IF (OLEN.EQ.1) THEN
C                     blank line
                      ZMNTXT(I)= ' '
                      ZMNLEN(I)= 0
                    ELSE
                      WRITE (ZMNTXT(I),2000) (TBUFF(J),J=1,OLEN)
                      ZMNLEN(I)= OLEN
                    END IF
                  ELSE IF (ID.EQ.16 .AND.TXTFG.EQ.0) THEN
C                   header for PRM2 class
                    WRITE (ZMNTXT(I),2000) (TBUFF(J),J=1,OLEN)
                    ZMNLEN(I)= OLEN
                  ELSE IF (SPINIT.EQ.0) THEN
C                   valid or invalid responses, put in RSPSTR
                    IF (I.EQ.1) THEN
C                     first line of responses, init counters
                      K= TPOS
                      CCNT(ORDER)= 1
                      IF (FTYP(ORDER).EQ.FTC) THEN
C                       init default for character field
                        CDEF(CIND) = 0
                      END IF
                    END IF
                    IF (FTYP(ORDER).EQ.FTF) THEN
C                     file type data field, put all _VALID text in RSPSTR
                      CALL CHRCHR (TLEN,TBUFF,RSPSTR(K))
                    ELSE
C                     parse valid responses into RSPSTR
                      DO 118 J= 1,OLEN
                        IF (TBUFF(J).EQ.',') THEN
C                         delimiter found
                          K= TPOS+ (CCNT(ORDER)* FLEN(ORDER))
                          CCNT(ORDER)= CCNT(ORDER)+ 1
                        ELSE IF (TBUFF(J).NE.' ') THEN
                          RSPSTR(K)= TBUFF(J)
                          K= K+ 1
                        END IF
 118                  CONTINUE
                    END IF
                    IF (CONT.EQ.0) THEN
C                     all done with responses, update pointers
                      IF (FTYP(ORDER).NE.FTF) THEN
C                       length of responses is field length * no. responses
                        TLEN= FLEN(ORDER)*CCNT(ORDER)
                      END IF
                      IF (ID.EQ.6) THEN
C                       store position and length for valid response
                        FDVAL(ORDER)= WMSPIV(TPOS,TLEN)
                      ELSE IF (ID.EQ.7) THEN
C                       store position and length for invalid response
                        FDINV(ORDER)= WMSPIV(TPOS,TLEN)
                      END IF
                      TPOS= TPOS+ TLEN
                    END IF
                  END IF
                IF (CONT.EQ.1) GO TO 115
                IF ((ID.EQ.1 .AND. (CLASS.EQ.1 .OR. CLASS.EQ.5)) .OR.
     1              (ID.EQ.16 .AND. CLASS.EQ.2)) THEN
C                 save number of lines in menu
                  ZMNNLI= I
                  IF (CLASS.EQ.2) THEN
C                   save number of header rows
                    NMHDRW= ZMNNLI
                  END IF
                END IF
                GO TO 200
C
 120          CONTINUE
C               get one record of text
                GLEN= 0
                MLEN= 0
                CALL WMSGTE (WDMSFL,TLEN,I80,
     M                       DREC,DPOS,GLEN,MLEN,
     O                       OLEN,TBUFF,CONT)
                IF (ID.EQ.2) THEN
C                 new field, may need to fill in previous field (parm 1 only)
                  IF (NFLDS.GT.0 .AND. CLASS.EQ.1) THEN
C                   this is not the first field, check the previous one
                    I= SCOL(NFLDS)+ FLEN(NFLDS)- 1
                    IF (ZMNTXT(FLIN(NFLDS))(SCOL(NFLDS):I).EQ.' ') THEN
C                     nothing in field, should there be?
                      IF (FLEN(NFLDS).GE.4) THEN
C                       will use all of 'none' string
                        ILEN= 4
                      ELSE
C                       only use allowable width of field
                        ILEN= FLEN(NFLDS)
                      END IF
                      IF (FTYP(NFLDS).EQ.FTI .OR. FTYP(NFLDS).EQ.FTR
     1               .OR. FTYP(NFLDS).EQ.FTD) THEN
C                       numeric field, right justify 'none' in field
                        IF (ILEN.EQ.4) THEN
C                         push 'none' right side of field
                          IPOS= SCOL(NFLDS)+ FLEN(NFLDS)- 4
                        ELSE
C                         just start 'none' in 1st position of field
                          IPOS= SCOL(NFLDS)
                        END IF
                        CALL CHRCHR (ILEN,CNONE,
     O                               ZMNTX1(IPOS,FLIN(NFLDS)))
                      ELSE IF (FTYP(NFLDS).EQ.FTC .AND.
     1                         FDVAL(NFLDS).GT.0) THEN
C                       character w/valids, left justify 'none' in field
                        CALL CHRCHR (ILEN,CNONE,
     O                               ZMNTX1(SCOL(NFLDS),FLIN(NFLDS)))
                      END IF
                    END IF
                  END IF
C                 update related parms
                  NFLDS= ORDER
                  IF (OLEN.GT.8) THEN
C                   only use 1st 8 characters of field name
                    OLEN= 8
                  END IF
                  CALL CARVAR (OLEN,TBUFF,I8,FDNAME(ORDER))
                  FDFMT(ORDER) = FDNAME(ORDER)
                  J= INDEX(FDFMT(ORDER),'@')
                  IF (J.GT.0) THEN
C                   remove '@' from name for storage
                    FDFMT(ORDER)(J:J)= ' '
                  END IF
                  CALL ZLJUST(FDFMT(ORDER))
                  IF (CLASS.EQ.5) THEN
C                   file question being asked
                    FTYP(ORDER)= FTC
                  END IF
                  IF (SPINIT.EQ.0) THEN
C                   ok to init valid/invalid values
                    FDVAL(NFLDS) = 0
                    FDINV(NFLDS) = 0
                  END IF
                  FREC(NFLDS)  = 0
                  FSLOT(NFLDS) = 0
                  ASDSFG(NFLDS)= ' '
                  FPROT(NFLDS) = 0
                  HPTR(NFLDS)  = 0
                  IF (CLASS.EQ.1 .OR. CLASS.EQ.5) THEN
C                   PRM1 (or FILE) type, look for field id in screen text
                    CALL WMSSPS (ZMNNLI,OLEN,TBUFF,
     M                           ZMNTXT,FLIN(NFLDS),SCOL(NFLDS),
     M                           FLEN(NFLDS),RETCOD)
                  END IF
                ELSE IF (ID.EQ.4 .AND. SPINIT.EQ.0) THEN
C                 default value
                  IF (FTYP(ORDER).EQ.FTI) THEN
C                   integer
                    IDEF(IIND)= CHRINT(OLEN,TBUFF)
                    LDEF= IDEF(IIND)
                  ELSE IF (FTYP(ORDER).EQ.FTR) THEN
C                   real
                    RDEF(RIND)= CHRDEC(OLEN,TBUFF)
                    LDEF= RDEF(RIND)
                  ELSE IF (FTYP(ORDER).EQ.FTD) THEN
C                   double precision
                    DDEF(DIND)= CHRDPR(OLEN,TBUFF)
                    LDEF= DDEF(DIND)
                  ELSE IF (FTYP(ORDER).EQ.FTC .AND.
     $                     FDVAL(ORDER).GT.0) THEN
C                   character, determine order in valid string
                    IF (OLEN.LT.FLEN(ORDER)) THEN
C                     put in blanks
                      DO 122 J= OLEN+1,FLEN(ORDER)
                        TBUFF(J)= BLNK
 122                  CONTINUE
                    END IF
                    CALL WMSPIS (FDVAL(ORDER),
     O                           J,K)
                    CDEF(CIND)= ZCHKST(FLEN(ORDER),CCNT(ORDER),
     1                                  TBUFF,RSPSTR(J))
                  END IF
                  IF (OLEN.LE.FLEN(ORDER) .AND. CLASS.EQ.1) THEN
C                   room for default (parm 1 type screens only)
                    IF (FTYP(ORDER).EQ.FTI .OR. FTYP(ORDER).EQ.FTR .OR.
     1                  FTYP(ORDER).EQ.FTD) THEN
C                     put numeric default in screen text (right justified)
                      IF (LDEF.GT.-999.0) THEN
C                       valid default
                        IPOS= SCOL(ORDER)+ FLEN(ORDER)- OLEN
                        CALL CHRCHR (OLEN,TBUFF,
     O                               ZMNTX1(IPOS,FLIN(ORDER)))
                      ELSE
C                       put 'none' in field
                        IPOS= SCOL(ORDER)+ FLEN(ORDER)- I4
                        CALL CHRCHR (I4,CNONE,ZMNTX1(IPOS,FLIN(ORDER)))
                      END IF
                    ELSE IF (CDEF(CIND).GT.0 .OR.
     $                       FTYP(ORDER).EQ.FTF) THEN
C                     put character default in screen text (left justified)
                      CALL CHRCHR (OLEN,TBUFF,
     M                             ZMNTX1(SCOL(ORDER),FLIN(ORDER)))
                    END IF
                  ELSE IF (CLASS.EQ.1) THEN
C                   no room for default in screen text
                    WRITE(99,*) 'Not enough room for default of length',
     1                           OLEN,' in field',ORDER
                  END IF
                ELSE IF (ID.EQ.19 .AND. ZWNFLG.EQ.0) THEN
C                 window name for data screen,
C                 may need to fill in delimeters
                  CALL WMSDLM (ZSCNOP,I48,
     M                         TBUFF)
C                 now fill in screen name
                  OLEN= LENSTR(I48,TBUFF)
                  WRITE (ZSCNAM(1:OLEN),2000) (TBUFF(J),J=1,OLEN)
                ELSE IF (ID.EQ.23) THEN
C                 hide another field based on this option field's value
                  NUMHID= NUMHID+ 1
C                 store this field as the 'trigger' field for hiding
                  HIDTFL(NUMHID)= ORDER
                  IF (TBUFF(2).EQ.'N') THEN
C                   hide when this field is on
                    HIDVAL(NUMHID)= 1
                  ELSE
C                   hide when this field is off
                    HIDVAL(NUMHID)= 0
                  END IF
C                 find start of field name to hide
                  IPOS= CKNBLV(OLEN-3,TBUFF(4))+ 3
C                 now find its length
                  ILEN= STRFND(OLEN-IPOS+1,TBUFF(IPOS),I1,CBLNK)- 1
C                 store in temp buffer to determine field number later on
                  CALL CARVAR(ILEN,TBUFF(IPOS),I8,HIDFNM(NUMHID))
C                 now get row/column info
                  J= 0
 128              CONTINUE
C                   parse out each number (upper left r/c, lower right r/c)
                    J= J+ 1
                    IPOS= IPOS+ ILEN
C                   find start of number
                    I= CKNBLV(OLEN-IPOS+1,TBUFF(IPOS))
                    IPOS= IPOS+ I- 1
C                   find length of number
                    ILEN= STRFND(OLEN-IPOS+1,TBUFF(IPOS),I1,CBLNK)- 1
                    IF (ILEN.LE.0 .AND. J.EQ.4) THEN
C                     last number, no trailing blanks to find
                      ILEN= OLEN- IPOS+ 1
                    END IF
                    IVAL(J)= CHRINT(ILEN,TBUFF(IPOS))
                  IF (J.LT.4) GO TO 128
                  HIDBOX(NUMHID)= WOPWDC(IVAL(1),IVAL(2),
     I                                   IVAL(3),IVAL(4))
                END IF
                GO TO 200
C
 130          CONTINUE
C               get numeric parameters
                DO 135 I= 1,TLEN/4
                  CALL WDNXDV (WDMSFL,
     M                         DREC,DPOS,
     O                         IVAL(I))
 135            CONTINUE
                IF (ID.EQ.3) THEN
C                 integer parms for id 3
                  CALL WMSP2S (IVAL(1),
     O                         ITMP,ILEN,ITMP2,
     O                         FPROT(ORDER),ICOL)
                  IF (CLASS.EQ.2) THEN
C                   integer parms for PRM2 class, but not PRM1
                    FLEN(ORDER)= ILEN
                    SCOL(ORDER)= ICOL
                    IF (ITMP2.EQ.1) THEN
C                     ascending order for field
                      ASDSFG(ORDER)= 'A'
                    ELSE IF (ITMP2.EQ.2) THEN
C                    descending order for field
                     ASDSFG(ORDER)= 'D'
                    END IF
                  END IF
                  IF (ITMP.EQ.1) THEN
C                   integer
                    FTYP(ORDER)= FTI
                    IIND= IIND+ 1
                    IF (SPINIT.EQ.0) THEN
C                     init min/max/default
                      IMIN(IIND) = 0
                      IMAX(IIND) = 0
                      IDEF(IIND) = 0
                    END IF
                    APOS(ORDER)= IIND
                  ELSE IF (ITMP.EQ.2) THEN
C                   real
                    FTYP(ORDER)= FTR
                    RIND= RIND+ 1
                    IF (SPINIT.EQ.0) THEN
C                     init min/max/default
                      RMIN(RIND) = 0.0
                      RMAX(RIND) = 0.0
                      RDEF(RIND) = 0.0
                    END IF
                    APOS(ORDER)= RIND
                  ELSE IF (ITMP.EQ.3) THEN
C                   double precision
                    FTYP(ORDER)= FTD
                    DIND= DIND+ 1
                    IF (SPINIT.EQ.0) THEN
C                     init min/max/default
                      DMIN(DIND) = 0.0
                      DMAX(DIND) = 0.0
                      DDEF(DIND) = 0.0
                    END IF
                    APOS(ORDER)= DIND
                  ELSE IF (ITMP.EQ.4) THEN
C                   character
                    FTYP(ORDER)= FTC
                    CIND= CIND+ 1
                    APOS(ORDER)= CIND
                  ELSE IF (ITMP.EQ.5) THEN
C                   option type
                    FTYP(ORDER)= FTO
                    OIND= OIND+ 1
                    APOS(ORDER)= OIND
C                   restore field name to screen text (but not @ character)
                    I= SCOL(ORDER)+1
C                   determine length of field name
                    J= ZLNTXT(FDNAME(ORDER))
                    ZMNTXT(FLIN(ORDER))(I:I+J-2)= FDNAME(ORDER)(2:J)
C                   set initial 1st character to OFF value
                    ZMNTX1(SCOL(ORDER),FLIN(ORDER))= ' '
                  ELSE IF (ITMP.EQ.6) THEN
C                   file type
                    FTYP(ORDER) = FTF
                    FIND= FIND+ 1
                    APOS(ORDER) = FIND
                    ZFILUN(FIND)= 0
                  END IF
                ELSE IF (ID.EQ.5 .AND. SPINIT.EQ.0) THEN
C                 range of values
                  IF (FTYP(ORDER).EQ.FTI) THEN
C                   integer
                    IMIN(IIND)= IVAL(1)
                    IMAX(IIND)= IVAL(2)
                  ELSE IF (FTYP(ORDER).EQ.FTR) THEN
C                   real
                    RMIN(RIND)= RVAL(1)
                    RMAX(RIND)= RVAL(2)
                  ELSE IF (FTYP(ORDER).EQ.FTD) THEN
C                   double precision
                    DMIN(DIND)= RVAL(1)
                    DMAX(DIND)= RVAL(2)
                  END IF
                ELSE IF (ID.EQ.8 .AND. CLASS.EQ.1) THEN
C                 record and slot
                  CALL WMSPIS (IVAL(1),
     O                         FREC(ORDER),FSLOT(ORDER))
                ELSE IF (ID.EQ.8 .AND. CLASS.EQ.2) THEN
C                 number of values to pack on a record
C                 ZAROUT(NFLDS)= IVAL(1)
                ELSE IF (ID.EQ.10) THEN
C                 pcode and ucode
C                  CALL WMSPIS (IVAL(1),
C     O                         PCODE(ORDER),UCODE(ORDER))
                ELSE IF (ID.EQ.21) THEN
C                 integer parameters for option type field
                  CALL WMSPOS (IVAL(1),
     O                         OPSET(ORDER),OPSTNO(ORDER),OPDEF(ORDER),
     O                         OPWID,OPHIDE,OPBOX(ORDER))
                  IF (OPWID.GT.0) THEN
C                   use this as field length to highlight
                    IF (OPWID.GT.1) THEN
C                     determine actual length of field to highlight
                      FLEN(ORDER)= LENSTR(OPWID,ZMNTX1(SCOL(ORDER),
     1                                                 FLIN(ORDER)))
                    ELSE
C                     highlight length must be at least 1
                      FLEN(ORDER)= 1
                    END IF
                    IF (OPHIDE.GT.0) THEN
C                     hide field name
                      I= SCOL(ORDER)+1
                      J= ZLNTXT(FDNAME(ORDER))-1
                      ZMNTXT(FLIN(ORDER))(I:I+J-1)= ' '
                    END IF
                  END IF
                  IF (OPBOX(ORDER).EQ.1) THEN
C                   put box next to field, store starting line and column
                    OPBOX(ORDER)= WDPTCL(FLIN(ORDER),SCOL(ORDER))
                  END IF
                END IF
                GO TO 200
C
 140          CONTINUE
C               text info, store pointers (or flags), but not text
                IF (ID.EQ.22) THEN
C                 conditional for option type fields
                  CONFG= 1
                ELSE IF (HPINIT.EQ.0) THEN
C                 get help pointers, save rec and offset
                  IF (ID.EQ.9) THEN
C                   option help
                    HPTR(ORDER)= WDPTCL(DREC,DPOS)
                  ELSE
C                   general help for question
                    GPTR= WDPTCL(DREC,DPOS)
                  END IF
C                 help, turn on as available command
                  CALL ZSTCMA(1,1)
                END IF
C               skip to end of help
                CALL WMSSKB (WDMSFL,TLEN,
     M                       DREC,DPOS)
                GO TO 200
C
 200          CONTINUE
C
              IF (ID.EQ.17 .OR. ID.EQ.18 .OR. ID.EQ.20) THEN
C               table data info, skip over it
                CALL WMSSKB (WDMSFL,TLEN,
     M                       DREC,DPOS)
              END IF
C
            ELSE
C             skip over data
              CALL WMSSKB (WDMSFL,TLEN,
     M                     DREC,DPOS)
            END IF
C
C           move to next data position
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   LIND)
          ELSE
C           question terminator
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 100
C
        IF (CLASS.EQ.1) THEN
C         check last field for no value (parm 1 screens only)
          I= SCOL(NFLDS)+ FLEN(NFLDS)- 1
          IF (ZMNTXT(FLIN(NFLDS))(SCOL(NFLDS):I).EQ.' ') THEN
C           nothing in field, should there be?
            IF (FLEN(NFLDS).GE.4) THEN
C             will use all of 'none' string
              ILEN= 4
            ELSE
C             only use allowable width of field
              ILEN= FLEN(NFLDS)
            END IF
            IF (FTYP(NFLDS).EQ.FTI .OR. FTYP(NFLDS).EQ.FTR
     1     .OR. FTYP(NFLDS).EQ.FTD) THEN
C             numeric field, right justify 'none' in field
              IF (ILEN.EQ.4) THEN
C               push 'none' right side of field
                IPOS= SCOL(NFLDS)+ FLEN(NFLDS)- 4
              ELSE
C               just start 'none' in 1st position of field
                IPOS= SCOL(NFLDS)
              END IF
              CALL CHRCHR (ILEN,CNONE,
     O                     ZMNTX1(IPOS,FLIN(NFLDS)))
            ELSE IF (FTYP(NFLDS).EQ.FTC .AND.
     1               FDVAL(NFLDS).GT.0) THEN
C             character w/valids, left justify 'none' in field
              CALL CHRCHR (ILEN,CNONE,
     O                     ZMNTX1(SCOL(NFLDS),FLIN(NFLDS)))
            END IF
          END IF
        END IF
C
        IF (RETCOD.EQ.0) THEN
C         set some other parms
          CFLD  = 0
 300      CONTINUE
C           set current field, make sure not protected
            CFLD= CFLD+ 1
          IF (FPROT(CFLD).EQ.2 .AND. CFLD.LT.NFLDS) GO TO 300
          IF (NFLDS.GT.1 .AND. NFLDS.EQ.CFLD .AND.
     1                         FPROT(CFLD).EQ.2) THEN
C           all fields protected, make current first
            CFLD= 1
C           no 'Oops' allowed
            CALL ZSTCMA (8,0)
C           set flag to indicate all fields protected
            QFLAG= 1
          ELSE
C           not all fields protected
            QFLAG= 0
          END IF
          IF (CLASS.EQ.1 .OR. CLASS.EQ.5) THEN
C           PRM1 type
            ZHLLIN= FLIN(CFLD)
            ZDTYP = 3
          ELSE
C           PRM2 type
            IF (NMHDRW.EQ.0) THEN
C             no header lines from message file, set to any pre-existing lines
              NMHDRW= ZMNNLI
            END IF
            ZHLLIN= NMHDRW+ 1
            ZDTYP = 4
            CROW= 1
C           window on
            CALL ZSTCMA(9,1)
          END IF
C         conditionals for option fields?
          IF (CONFG.EQ.1) THEN
C           yes, need to run through them again now that we know about fields
            CALL WMSCON (WDMSFL,DSN,GNUM,NFLDS,FDNAME,MXCON,
     O                   OPONOF,ONFTFL,ONFTVL)
          END IF
          IF (NUMHID.GT.0) THEN
C           hidden fields exist, determine which fields they are
            DO 400 I= 1,NUMHID
C             look for hidden field among field names
              J= 0
 350          CONTINUE
                J= J+ 1
                IF (HIDFNM(I).EQ.FDNAME(J)) THEN
C                 found match
                  HIDFLD(I)= J
                  J= NFLDS
                END IF
              IF (J.LT.NFLDS) GO TO 350
 400        CONTINUE
          END IF
          ZHLCOL= SCOL(CFLD)
          ZHLLEN= FLEN(CFLD)
          ZCRLIN= ZHLLIN + 1
          ZCRCOL= ZHLCOL + 1
        END IF
C       always turn off parameter and help init flags
        SPINIT= 0
        HPINIT= 0
C       and screen name flag
        ZWNFLG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTT
     I                   (WDMSFL,DSN,GNUM,INITFG,
     M                    OLEN,
     O                    OBUFF,CONT)
C
C     + + + PURPOSE + + +
C     get message file dataset text from WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,GNUM,INITFG,OLEN,CONT
      CHARACTER*1 OBUFF(OLEN)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number
C     GNUM   - question number to get
C     INITFG - flag indicating start a new question
C              0 - continue question, 1 - start new question
C     OLEN   - length of text
C     OBUFF  - output buffer of retrieved text
C     CONT   - continuation flag, 1 - more text to read
C                                 0 - end of text
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I48,LMAX,SREC,SPOS,QWORD,
     1            DREC,DIND,DPOS,CLASS,ID,ORDER,
     2            TLEN,GLEN,MLEN,DONFG,RDWRFG,SLEN
      CHARACTER*1 LBUFF(48)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, WDPTCL, LENSTR
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WDPTCL, LENSTR, WMSBCS, WMSGTE, WMSFBC
      EXTERNAL    WMSSKB, WDNXPS, ZSTCMA, WMSDLM
C
C     + + + SAVES + + +
      SAVE   DREC,DPOS,GLEN,MLEN,TLEN
C
C     + + +  DATA INITIALIZATIONS + + +
      DATA I48,DPOS,GLEN,MLEN,RDWRFG/48,0,0,0,1/
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (48A1)
C
C     + + + END SPECIFICATIONS + + +
C
C     set maximum output length
      LMAX= OLEN
C
      IF (INITFG.EQ.1) THEN
C       new question, initialize counters
        DPOS = 0
        GLEN = 0
        MLEN = 0
C       init screen name
CJLK    ZSCNAM= ' '
C       get first block control word and its location
        CALL WMSFBC (WDMSFL,DSN,GNUM,
     O               DREC,DPOS,QWORD)
C
        DONFG= 0
C       loop through all blocks first time through
 10     CONTINUE
C         get block control word
          DIND = WDRCGO(WDMSFL,DREC)
          QWORD= WIBUFF(DPOS,DIND)
          IF (QWORD.GT.0) THEN
C           data exists, decode bcw
            CALL WMSBCS (QWORD,
     O                   CLASS,ID,ORDER,TLEN)
            IF (ID.EQ.1) THEN
C             first time through, save current rec/pos
              SREC= DREC
              SPOS= DPOS
              SLEN= TLEN
C             skip over data to look ahead for other directives
              CALL WMSSKB (WDMSFL,TLEN,
     M                     DREC,DPOS)
            ELSE IF (ID.EQ.2) THEN
C             help
              IF (HPINIT.EQ.0) THEN
C               not set externally, turn on as available command
                CALL ZSTCMA(1,1)
C               save help pointer
                GPTR= WDPTCL(DREC,DPOS)
              END IF
C             skip over help text
              CALL WMSSKB (WDMSFL,TLEN,
     M                     DREC,DPOS)
            ELSE IF (ID.EQ.19) THEN
C             screen name
              I= 0
              J= 0
              CALL WMSGTE (WDMSFL,TLEN,I48,
     M                     DREC,DPOS,I,J,
     O                     K,LBUFF,CONT)
              IF (ZWNFLG.EQ.0) THEN
C               fill in screen name
                ZSCNAM= ' '
C               may need to fill in delimeters
                CALL WMSDLM (ZSCNOP,I48,
     M                       LBUFF)
C               now fill in screen name
                K= LENSTR(I48,LBUFF)
                WRITE (ZSCNAM(1:K),2000) (LBUFF(I),I=1,K)
              END IF
            END IF
C           move to next data position
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
          ELSE
C           at end of group
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 10
C
C       set rec/pos back to saved position
        DREC= SREC
        DPOS= SPOS
        TLEN= SLEN
      END IF
C
C     always read a record of text
      CALL WMSGTE (WDMSFL,TLEN,LMAX,
     M             DREC,DPOS,GLEN,MLEN,
     O             OLEN,OBUFF,CONT)
C     always reset screen name flag and help init flag
      ZWNFLG= 0
      HPINIT= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTM
     I                   (WDMSFL,DSN,QNUM,
     O                    NUMB,WIDTH,CLEN,RETCOD)
C
C     + + + PURPOSE + + +
C     get message file dataset menu info from WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,QNUM,NUMB,WIDTH,CLEN,RETCOD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number
C     QNUM   - question number to get
C     NUMB   - number option flag (0- no numbers, 1- numbers)
C     WIDTH  - column width
C     CLEN   - column length
C     RETCOD - return code
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'cqrsp.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,I0,I1,I48,I64,I80,DREC,DIND,DPOS,IPOS,LCLEN,SCRFG,
     1            CLASS,ID,ORDER,TLEN,GLEN,DONFG,QWORD,NCOL,
     2            IMENVL,RDWRFG,OLEN,MLEN,LLANS,CONT
      CHARACTER*1 TBUFF(80),BLNK,CEXC(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, WDPTCL, LENSTR, ZLNTXT, CKNBLV, STRFND
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD, INT
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WDPTCL, LENSTR, ZLNTXT, CKNBLV, STRFND
      EXTERNAL    WMSMNS, WMSBCS, WMSSPS, WDNXPS, WMSGTE, WDNXDV, WMSFBC
      EXTERNAL    WMSSKB, ZCNTER, CHRCHR, ZSTCMA, QUPCAS, ZSCINI
      EXTERNAL    CHRDEL, ZIPC, ZIPI, WMSDLM, CARVAR
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (80A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I0    = 0
      I1    = 1
      I48   = 48
      I64   = 64
      I80   = 80
      RDWRFG= 1
      SCRFG = 0
      DONFG = 0
      BLNK  = ' '
      CEXC(1)= '!'
      IF (HPINIT.EQ.0) THEN
C       assume no help available (both general and specific)
        GPTR  = 0
        CALL ZIPI (I64,I0,HPTR)
        CALL ZSTCMA(1,0)
      END IF
C     assume no centering
      ZCLEN = 0
      ZCWID = 0
C     init line, column, and length of menu options
      CALL ZIPI (I64,I0,ZOPLIN)
      CALL ZIPI (I64,I0,ZOPCOL)
      CALL ZIPI (I64,I0,ZOPLEN)
C
C     check existence of question
      CALL WMSFBC (WDMSFL,DSN,QNUM,
     O             DREC,DPOS,QWORD)
C
      IF (QWORD.EQ.0) THEN
C       bad question
        RETCOD= -40
      ELSE
C       ok question, get details
        RETCOD= 0
      END IF
C
      IF (RETCOD.EQ.0) THEN
        IF (RSINIT.EQ.0) THEN
C         initialize valid responses
          I= 480
          CALL ZIPC(I,BLNK,TANS)
        END IF
C       initialize screen
        CALL ZSCINI
        IF (ZWNFLG.EQ.0) THEN
C         init screen name
          ZSCNAM= ' '
        END IF
C       init commands
        DO 6 I= 6,11
C         make these commands unavailable (not valid for menu screen)
          CALL ZSTCMA(I,0)
 6      CONTINUE
C       also turn off down/up page commands
        I= 14
        CALL ZSTCMA (I,0)
        I= 15
        CALL ZSTCMA (I,0)
        IF (QUINIT.EQ.0) THEN
C         initialize question
          I= 64
          CALL ZIPC (I,BLNK,QUEST)
        ELSE
C         put previously set title in menu text
          ZMNLNI= ZMNNLI+ 1
          I= 78
          J= LENSTR(I,QUEST)
          WRITE (ZMNTXT(ZMNLNI),2000) (QUEST(I),I=1,J)
          ZMNLEN(ZMNLNI)= J
        END IF
 10     CONTINUE
C         get block control word
          DIND = WDRCGO(WDMSFL,DREC)
          QWORD= WIBUFF(DPOS,DIND)
          IF (QWORD.GT.0) THEN
C           get parameters
            CALL WMSBCS (QWORD,
     O                   CLASS,ID,ORDER,TLEN)
            IF (ID.EQ.2) THEN
C             move to data position and get integer word
              CALL WDNXDV (WDMSFL,
     M                     DREC,DPOS,
     O                     IMENVL)
C             split integer word into parameters
              CALL WMSMNS (IMENVL,
     O                     DANS,LLANS,NUMB,WIDTH,CLEN)
              IF (RSINIT.EQ.0) THEN
C               length of options not set, set it
                LANS= LLANS
              END IF
            ELSE IF (ID.EQ.1 .OR. ID.EQ.3 .OR.
     1               ID.EQ.4 .OR. ID.EQ.19) THEN
C             get character data and put in correct parameter
              GLEN= 0
              MLEN= 0
              CALL WMSGTE (WDMSFL,TLEN,I80,
     M                     DREC,DPOS,GLEN,MLEN,
     O                     OLEN,TBUFF,I)
              IF (ID.EQ.1 .AND. QUINIT.EQ.0) THEN
C               put title in menu text
                ZMNLNI= ZMNNLI+ 1
                WRITE (ZMNTXT(ZMNLNI),2000) (TBUFF(I),I=1,OLEN)
                ZMNLEN(ZMNLNI)= OLEN
              ELSE IF (ID.EQ.3 .AND. RSINIT.EQ.0) THEN
C               option
                IF (SCRFG.EQ.1) THEN
C                 'screen' method in use, look for option in screen text
CPRH               write (99,*)'OLEN,TBUFF',OLEN,(TBUFF(I),I=1,OLEN)
                  CALL WMSSPS (ZMNNLI,OLEN,TBUFF,
     M                         ZMNTXT,ZOPLIN(ORDER),ZOPCOL(ORDER),
     M                         ZOPLEN(ORDER),RETCOD)
                  IF (TBUFF(1).EQ.'@') THEN
C                   remove preceeding @ from option name
                    CALL CHRDEL (OLEN,I1,
     M                           TBUFF)
                    OLEN= OLEN- 1
                  END IF
                ELSE
C                 generate screen positions with original algorithm
                  ZOPLIN(ORDER)= MOD(ORDER-1,CLEN) + ZMNLNI+ 2 + ZCLEN
                  ZOPCOL(ORDER)= 1+ WIDTH*((ORDER-1)/CLEN) + ZCWID
                  IF (ZOPLIN(ORDER).GT.ZMNNLI) THEN
C                   more lines in menu
                    ZMNNLI= ZOPLIN(ORDER)
                  END IF
                END IF
                TLEN= ZOPCOL(ORDER)+ OLEN- 1
                IF (TLEN.GT.78) THEN
C                 out of space in screen buffer, need to cut it short
                  OLEN= OLEN- (TLEN-78)
                  TLEN= 78
                END IF
C               make sure nothing out there to overwrite
                I= CKNBLV(OLEN,ZMNTX1(ZOPCOL(ORDER),ZOPLIN(ORDER)))
                IF (I.GT.0) THEN
C                 some text out there, don't overwrite it
                  TLEN= TLEN- (OLEN-I+2)
                  OLEN= OLEN- (OLEN-I+2)
                END IF
                WRITE (ZMNTXT(ZOPLIN(ORDER))(ZOPCOL(ORDER):TLEN),2000)
     1                (TBUFF(I),I=1,OLEN)
                ZMNLEN(ZOPLIN(ORDER))= ZLNTXT(ZMNTXT(ZOPLIN(ORDER)))
CPRH       write (99,*) 'ORDER,TLEN,LINE,COL',
CPRH     1               ORDER,TLEN,ZOPLIN(ORDER),ZOPCOL(ORDER)
CPRH       write (99,*) ZMNTXT(ZOPLIN(ORDER))
                ZOPLEN(ORDER)= ZLNTXT(ZMNTXT(ZOPLIN(ORDER))
     1                               (ZOPCOL(ORDER):TLEN))
C               convert option to upper case
                CALL QUPCAS (OLEN,
     M                       TBUFF)
C               put option name in response buffer
                J= (ORDER-1)* LANS+ 1
                CALL CHRCHR (OLEN,TBUFF,TANS(J))
                NANS= ORDER
C               assume no help available
                HPTR(ORDER)= 0
              ELSE IF (ID.EQ.4) THEN
C               option description
                I= STRFND(LANS+2,ZMNTX1(ZOPCOL(ORDER),ZOPLIN(ORDER)),
     O                    I1,CEXC)
                IF (I.GT.0) THEN
C                 ! found in Screen text, start description here
                  J= ZOPCOL(ORDER)+ I- 1
                  ZMNTX1(J,ZOPLIN(ORDER))= ' '
C                 back up 1 extra space since option name has already
C                 been backed up one space to remove '@' character
                  J= J- 1
                ELSE
C                 start description at default position after option name
                  J= ZOPCOL(ORDER)+ LANS+ 1
                END IF
                IF (J.LT.78) THEN
C                 room to add (at least some of) the description
                  TLEN= J+ OLEN- 1
                  IF (TLEN.GT.78) THEN
C                   out of space in screen buffer, need to cut it short
                    OLEN= OLEN- (TLEN-78)
                    TLEN= 78
                  END IF
C                 make sure nothing out there to overwrite
                  I= CKNBLV(OLEN,ZMNTX1(J,ZOPLIN(ORDER)))
                  IF (I.GT.0) THEN
C                   some text out there, don't overwrite it
                    TLEN= TLEN- (OLEN-I+2)
                    OLEN= OLEN- (OLEN-I+2)
                  END IF
CPRH       write (99,*) 'J,TLEN,OLEN,I',J,TLEN,OLEN,I
CPRH       write (99,*) ZMNTXT(ZOPLIN(ORDER))
                  IF (OLEN.GT.0) THEN
C                   room to write some text
                    WRITE (ZMNTXT(ZOPLIN(ORDER))(J:TLEN),2000)
     1                    (TBUFF(I),I=1,OLEN)
                  END IF
                  ZMNLEN(ZOPLIN(ORDER))= ZLNTXT(ZMNTXT(ZOPLIN(ORDER)))
CPRH       write (99,*) 'ORDER,TLEN,LINE,COL',
CPRH     1               ORDER,TLEN,ZOPLIN(ORDER),ZOPCOL(ORDER)
CPRH       write (99,*) ZMNTXT(ZOPLIN(ORDER))
                  ZOPLEN(ORDER)= ZLNTXT(ZMNTXT(ZOPLIN(ORDER))
     1                                 (ZOPCOL(ORDER):TLEN))
                END IF
              ELSE IF (ID.EQ.19 .AND. ZWNFLG.EQ.0) THEN
C               window name for data screen
C               may need to fill in delimeters
                CALL WMSDLM (ZSCNOP,I48,
     M                       TBUFF)
C               now fill in screen name
                OLEN= LENSTR(I48,TBUFF)
                WRITE (ZSCNAM(1:OLEN),2000) (TBUFF(J),J=1,OLEN)
              END IF
            ELSE IF (ID.EQ.5 .OR .ID.EQ.6) THEN
C             help
              IF (HPINIT.EQ.0) THEN
C               not set externally, turn on as available command
                CALL ZSTCMA(1,1)
C               save rec and offset
                IF (ID.EQ.5) THEN
C                 option help
                  HPTR(ORDER)= WDPTCL(DREC,DPOS)
                ELSE
C                 general help for question
                  GPTR= WDPTCL(DREC,DPOS)
                END IF
              END IF
C             skip to end of help text
              CALL WMSSKB (WDMSFL,TLEN,
     M                     DREC,DPOS)
            ELSE IF (ID.EQ.21) THEN
C             Screen definition of menu
              IF (RSINIT.EQ.0) THEN
C               get multiple records of text
                GLEN= 0
                MLEN= 0
C               set I to current number of lines in menu
                I= ZMNNLI
 50             CONTINUE
C                 get records until end of id information
                  I= I+ 1
                  CALL WMSGTE (WDMSFL,TLEN,I80,
     M                         DREC,DPOS,GLEN,MLEN,
     O                         OLEN,TBUFF,CONT)
C                 put screen text into AIDE text buffer
                  IF (OLEN.EQ.1) THEN
C                   blank line
                    ZMNTXT(I)= ' '
                    ZMNLEN(I)= 0
                  ELSE
C                   put record into screen text buffer
                    J= 78
                    CALL CARVAR (OLEN,TBUFF,J,ZMNTXT(I))
CPRH                    WRITE (ZMNTXT(I),2000) (TBUFF(J),J=1,OLEN)
                    ZMNLEN(I)= OLEN
                  END IF
                IF (CONT.EQ.1) GO TO 50
C               indicate 'screen' method being used for menu
                SCRFG= 1
C               save number of lines in menu
                ZMNNLI= I
              ELSE
C               skip this data, move to next data position
                CALL WMSSKB (WDMSFL,TLEN,
     M                       DREC,DPOS)
              END IF
            END IF
C           move to next data position
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
          ELSE
C           question terminator
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 10
C
C       set options here, if previously defined
        IF (RSINIT.EQ.1) THEN
C         options already defined, put in menu text
C         determine column length and width
          IF (NANS.LE.8) THEN
            WIDTH= 78
            CLEN = NANS
          ELSE
C           determin number of columns
            LCLEN= MOD(NANS,8)
            IF (LCLEN.NE.0) THEN
C             uneven column lengths
              NCOL= INT(NANS/8)+ 1
C             determine column lengths
              CLEN= 8
 100          CONTINUE
                LCLEN= LCLEN+ NCOL- 1
                IF (LCLEN.LT.CLEN) THEN
                  CLEN = CLEN- 1
                END IF
              IF (LCLEN.LT.CLEN-1) GO TO 100
            ELSE
C             even column lengths
              NCOL= NANS/8
              CLEN= 8
            END IF
            WIDTH = 78/NCOL
          END IF
          DO 200 I= 1,NANS
C           position responses in menu text
            ZOPLIN(I)= MOD(I-1,CLEN) + ZMNLNI+ 2 + ZCLEN
            ZOPCOL(I)= 1+ WIDTH*((I-1)/CLEN) + ZCWID
            IF (ZOPLIN(I).GT.ZMNNLI) THEN
C             more lines in menu
              ZMNNLI= ZOPLIN(I)
            END IF
            IPOS= (I-1)* LANS+ 1
            WRITE (ZMNTXT(ZOPLIN(I))(ZOPCOL(I):ZOPCOL(I)+LANS-1),2000)
     1            (TANS(J),J=IPOS,IPOS+LANS-1)
            ZMNLEN(ZOPLIN(I))= ZOPCOL(I)+ OLEN- 1
            TLEN  = ZOPCOL(I)+WIDTH-1
            IF (TLEN.GT.78) TLEN= 78
            ZOPLEN(I)= ZLNTXT(ZMNTXT(ZOPLIN(I))
     1                       (ZOPCOL(I):TLEN))
 200      CONTINUE
        END IF
C
CPRH      write (99,*) 'ZOPLIN',ZOPLIN
CPRH      write (99,*) 'ZOPCOL',ZOPCOL
CPRH      write (99,*) 'ZOPLEN',ZOPLEN
CPRH      write (99,*) 'ZB1N,ZHLLEN,ZMNNLI,ZMNLEN,ZHLLIN,ZHLCOL',
CPRH     1              ZB1N,ZHLLEN,ZMNNLI,ZMNLEN,ZHLLIN,ZHLCOL
CPRH      write (99,*) (ZMNTXT(I),I=1,10)
C       center menu
        CALL ZCNTER (ZB1N,ZHLLEN,
     M               ZMNNLI,ZMNTXT,ZMNLEN,ZHLLIN,ZHLCOL,
     O               ZCLEN,ZCWID)
CPRH      write (99,*) 'ZB1N,ZHLLEN,ZMNNLI,ZMNLEN,ZHLLIN,ZHLCOL',
CPRH     1              ZB1N,ZHLLEN,ZMNNLI,ZMNLEN,ZHLLIN,ZHLCOL
CPRH      write (99,*) (ZMNTXT(I),I=1,10)
C       reset screen name flag
        ZWNFLG= 0
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTF
     I                   (WDMSFL,DSN,QNUM,
     O                    QFLG,FLNAME,STATF,ACCD,FMTD,TRECL)
C
C     + + + PURPOSE + + +
C     get message file dataset file info from WDM file
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      WDMSFL,DSN,QNUM,QFLG,TRECL
      CHARACTER*64 FLNAME
      CHARACTER*7  STATF
      CHARACTER*10 ACCD
      CHARACTER*11 FMTD
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - dataset number
C     QNUM   - question number to get
C     QFLG   - question flag (0- file name already specified,
C                             1- ask user for file name)
C     FLNAME - file name to be opened
C     STATF  - file status
C     ACCD   - file access type
C     FMTD   - file format
C     TRECL  - direct file record length
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I80,DREC,DIND,DPOS,CLASS,ID,ORDER,OLEN,
     2            TLEN,GLEN,DONFG,QWORD,MLEN,RDWRFG,RETCOD
      CHARACTER*1 TBUFF(80)
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, WDPTCL
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, WDPTCL, WMSBCS, WMSGTE
      EXTERNAL    WDNXPS, WDNXDV, WMSFBC, WMSSKB
C
C     + + + OUTPUT FORMATS + + +
 2000 FORMAT (64A1)
C
C     + + + END SPECIFICATIONS + + +
C
      I80   = 80
      RDWRFG= 1
      DONFG = 0
      TRECL = 0
      IF (HPINIT.EQ.0) THEN
C       assume no help available
        GPTR = 0
      END IF
C     assume not a question
      QFLG  = 0
C     init output argument file parameters
      FLNAME= ' '
      STATF = 'OLD    '
      ACCD  = 'SEQUENTIAL'
      FMTD  = 'FORMATTED  '
      TRECL = 0
C
C     check existence of question
      CALL WMSFBC (WDMSFL,DSN,QNUM,
     O             DREC,DPOS,QWORD)
C
      IF (QWORD.EQ.0) THEN
C       bad question
        RETCOD= -40
      ELSE
C       ok question, get details
        RETCOD= 0
      END IF
C
      IF (RETCOD.EQ.0) THEN
C       get info till null reached
 10     CONTINUE
C         get block control word
          DIND = WDRCGO(WDMSFL,DREC)
          QWORD= WIBUFF(DPOS,DIND)
          IF (QWORD.GT.0) THEN
C           get parameters
            CALL WMSBCS (QWORD,
     O                   CLASS,ID,ORDER,TLEN)
            IF (ID.EQ.7) THEN
C             get record length from next data value
              CALL WDNXDV (WDMSFL,
     M                     DREC,DPOS,
     O                     TRECL)
            ELSE IF (ID.LE.5 .OR. ID.EQ.19) THEN
C             get character data and put in correct parameter
              GLEN= 0
              MLEN= 0
 50           CONTINUE
C               read records till end of id info
                CALL WMSGTE (WDMSFL,TLEN,I80,
     M                       DREC,DPOS,GLEN,MLEN,
     O                       OLEN,TBUFF,I)
                IF (ID.EQ.1) THEN
C                 question, will be asked later, turn on QFLG
                  QFLG= 1
                ELSE IF (ID.EQ.2 .AND. QFLG.EQ.0) THEN
C                 file name
                  WRITE (FLNAME,2000) (TBUFF(I),I=1,OLEN)
                ELSE IF (ID.EQ.3) THEN
C                 file status
                  WRITE (STATF,2000) (TBUFF(I),I=1,OLEN)
                ELSE IF (ID.EQ.4) THEN
C                 file access
                  WRITE (ACCD,2000) (TBUFF(I),I=1,OLEN)
                ELSE IF (ID.EQ.5) THEN
C                 file format
                  WRITE (FMTD,2000) (TBUFF(I),I=1,OLEN)
                END IF
              IF (I.EQ.1) GO TO 50
            ELSE IF (ID.EQ.6) THEN
C             help
              IF (HPINIT.EQ.0) THEN
C               not set externally, save rec and offset
                GPTR= WDPTCL(DREC,DPOS)
              END IF
C             skip to end of help text
              CALL WMSSKB (WDMSFL,TLEN,
     M                     DREC,DPOS)
            END IF
C           move to next data position
            CALL WDNXPS (WDMSFL,RDWRFG,
     M                   DREC,DPOS,
     O                   DIND)
          ELSE
C           question terminator
            DONFG= 1
          END IF
        IF (DONFG.EQ.0) GO TO 10
      END IF
C     always reset help init flag
      HPINIT= 0
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTX
     I                   (MESSFL,SCLU,SGRP,
     O                    LNFLDS,LSCOL,LFLEN,LFTYP,LAPOS,LIMIN,LIMAX,
     O                    LIDEF,LRMIN,LRMAX,LRDEF,LNMHDR,HDRBUF,RETCOD)
C
C     + + + PURPOSE + + +
C     Get selected information about a table dataset 2d parm screen.
C     * * * This routine is specific to the HSPF Expert System  * * *
C     * * * program and resides here to keep AIDE common blocks * * *
C     * * * within the same code group.                         * * *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     MESSFL,SCLU,SGRP,LNFLDS,LSCOL(30),LFLEN(30),
     1            LAPOS(30),LIMIN(30),LIMAX(30),LIDEF(30),LNMHDR,RETCOD
      REAL        LRMIN(30),LRMAX(30),LRDEF(30)
      CHARACTER*1 LFTYP(30),HDRBUF(78,5)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     MESSFL - Fortran unit number for message file
C     SCLU   - cluster number on message file
C     SGRP   - group number in cluster of screen
C     LNFLDS - number of fields for this table
C     LSCOL  - integer array of starting columns for fields
C     LFLEN  - integer array of field lengths
C     LFTYP  - character array of field types (I,R,C)
C     LAPOS  - position in field type specific arrays of that type
C     LIMIN  - integer array of minimum values for integer fields
C     LIMAX  - integer array of maximum values for integer fields
C     LIDEF  - integer array of defaults for integer fields
C     LRMIN  - real array of minimum values for real fields
C     LRMAX  - real array of maximum values for real fields
C     LRDEF  - real array of defaults for real fields
C     LNMHDR - number of header rows
C     HDRBUF - character array of headers for tables
C     RETCOD - return code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,I78,I0
      REAL        R0
      CHARACTER*1 CB
C
C     + + + EXTERNALS + + +
      EXTERNAL   WMSGTP, CHRCHR, ZIPI, ZIPR, ZIPC
C
C     + + + END SPECIFICATIONS + + +
C
      I78= 78
C
      I0= 0
      R0= 0
      CB= ' '
C
      I = 30
      CALL ZIPI(I,I0,LSCOL)
      CALL ZIPI(I,I0,LFLEN)
      CALL ZIPC(I,CB,LFTYP)
      CALL ZIPI(I,I0,LAPOS)
      CALL ZIPI(I,I0,LIMIN)
      CALL ZIPI(I,I0,LIMAX)
      CALL ZIPI(I,I0,LIDEF)
      CALL ZIPR(I,R0,LRMIN)
      CALL ZIPR(I,R0,LRMAX)
      CALL ZIPR(I,R0,LRDEF)
C
      CALL WMSGTP (MESSFL,SCLU,SGRP,
     O             I,RETCOD)
C
      IF (NFLDS.GT.30) THEN
C       this should not happen for the purposes of this routine
        WRITE (99,*) 'BIG PROBLEM in WMSGTX: NFLDS > 30, NFLDS =',NFLDS
        LNFLDS= 30
      ELSE
C       use all available fields
        LNFLDS= NFLDS
      END IF
C
C     fill in argument values
      DO 100 I= 1,LNFLDS
        LSCOL(I)= SCOL(I)
        LFLEN(I)= FLEN(I)
        LFTYP(I)= FTYP(I)
        LAPOS(I)= APOS(I)
        LIMIN(I)= IMIN(I)
        LIMAX(I)= IMAX(I)
        LIDEF(I)= IDEF(I)
        LRMIN(I)= RMIN(I)
        LRMAX(I)= RMAX(I)
        LRDEF(I)= RDEF(I)
 100  CONTINUE
C
C     save number of header rows
      LNMHDR= NMHDRW
      DO 200 I= 1,NMHDRW
C       fill in headers
        CALL CHRCHR (I78,ZMNTX1(1,I),HDRBUF(1,I))
 200  CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTW 
     I                   (ID,
     O                    BUF)
C
C     + + + PURPOSE + + +
C     returns window name (may have other uses later)
C  
C     + + + DUMMY ARGUMENTS + + +
      INTEGER       ID
      CHARACTER*(*) BUF
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ID      - index of character variable to return (not yet used)
C     BUF     - character variable 
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'zcntrl.inc'
C
C     + + + END SPECIFICATIONS + + +
C
      IF (ID .GT. 0) THEN
        BUF = ZSCNAM
      END IF     
C
      RETURN
      END 
C
C
C
      SUBROUTINE   WMSSPS
     I                   (ZMNNLI,FOLEN,FONAM,
     M                    ZMNTXT,LINE,COLUMN,LENGTH,RETCOD)
C
C     + + + PURPOSE + + +
C     Determine the position within the screen text buffer
C     of either a data field or menu option.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER      ZMNNLI,FOLEN,LINE,COLUMN,LENGTH,RETCOD
      CHARACTER*1  FONAM(FOLEN)
      CHARACTER*78 ZMNTXT(ZMNNLI)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ZMNNLI - number of lines in screen text buffer
C     FOLEN  - length of field/option name
C     FONAM  - field/option name
C     ZMNTXT - screen text buffer
C     LINE   - line number in screen text buffer of field/option
C     COLUMN - starting column in screen text buffer of field/option
C     LENGTH - length of file/option
C     RETCOD - return code
C
C     + + + LOCAL VARIABLES + + +
      INTEGER      I,J,OLEN
      CHARACTER*78 TMPNAM
C
C     + + + INTRINSICS + + +
      INTRINSIC    INDEX
C
C     + + + EXTERNALS + + +
      EXTERNAL     CARVAR
C
C     + + + END SPECIFICATIONS + + +
C
C     put character array field/option name into character variable for search
      I= 78
      CALL CARVAR (FOLEN,FONAM,I,TMPNAM)
CPRH       write (99,*) 'LEN,TMPNAM',FOLEN,TMPNAM(1:FOLEN)
C
      OLEN= FOLEN
C     look for field/option name in screen text buffer
      I= 0
 100  CONTINUE
C       search line by line
        I= I+ 1
CPRH       write (99,*) 'ZMNTXT(I),I=',I
CPRH       write (99,*) ZMNTXT(I)
        J= INDEX(ZMNTXT(I),TMPNAM(1:FOLEN))
        IF (TMPNAM(1:1).NE.'@') THEN
C         field/option being searched for does not have an @ preceeding
          IF (J.GT.1) THEN
C           make sure screen buffer has preceeding @
            IF (ZMNTXT(I)(J-1:J-1).EQ.'@') THEN
C             option found w/appropriate @ preceeding,
C             back up one space to remove '@' from screen buffer
              J= J- 1
              OLEN= FOLEN+ 1
            ELSE
C             this is not a real match
              J= 0
            END IF
          ELSE
C           match found in 1st column, cant be a real match
            J= 0
          END IF
        END IF
        IF (J.GT.0) THEN
C         found the field
          LINE  = I
          COLUMN= J
          LENGTH= OLEN
          ZMNTXT(I)(J:J+OLEN-1)= ' '
          J= J+ OLEN
          IF (J.LT.79) THEN
C           don't look for end of field beyond width of screen buffer
 200        CONTINUE
C             look for end of field
              IF (ZMNTXT(I)(J:J).EQ.'.') THEN
C               still in field
                LENGTH= LENGTH+ 1
                ZMNTXT(I)(J:J)= ' '
              ELSE
C               end of field
                J= 79
              END IF
              J= J+ 1
            IF (J.LT.79) GO TO 200
          END IF
          I= ZMNNLI+ 1
        END IF
      IF (I.LT.ZMNNLI) GO TO 100
C
      IF (I.EQ.ZMNNLI) THEN
C       never found field, error
        RETCOD= 2
      END IF
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSCON
     I                   (WDMSFL,DSN,GNUM,NFLDS,FDNAME,MXCON,
     O                    OPONOF,ONFTFL,ONFTVL)
C
C     + + + PURPOSE + + +
C     Fill in arrays for option type field conditionals.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,GNUM,NFLDS,MXCON,OPONOF(NFLDS),
     1            ONFTFL(MXCON),ONFTVL(MXCON)
      CHARACTER*8 FDNAME(NFLDS)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for message file
C     DSN    - data-set (group) number on message file
C     GNUM   - group (cluster) number on message file
C     NFLDS  - number of fields on this screen
C     FDNAME - array of field names
C     MXCON  - maximum number of conditionals
C     OPONOF - array of pointers/counters for conditional arrays for each field
C     ONFTFL - array of target field numbers for conditionals
C     ONFTVL - values of target fields for conditionals
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cfbuff.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,L,I0,I1,I8,DREC,DPOS,QWORD,DONFG,LIND,CLASS,ID,
     $            ORDER,TLEN,RDWRFG,CONT,ILEN,GLEN,MLEN,OLEN,LMXFLD,
     $            POINTR,LORDER,ONCNT,OFFCNT,CFLD,IPOS,ONPTR,OFFPTR,
     $            LONTFL(MXFLD),LOFTFL(MXFLD),
     $            LONTVL(MXFLD),LOFTVL(MXFLD)
      CHARACTER*1 TBUFF(80),BLNK(1),CCOMMA(1)
      CHARACTER*8 CFDNAM
C
C     + + + FUNCTIONS + + +
      INTEGER     WDRCGO, STRFND, LENSTR, CKNBLV, WOPWDC
C
C     + + + INTRINSICS + + +
      INTRINSIC   INT
C
C     + + + EXTERNALS + + +
      EXTERNAL    WDRCGO, STRFND, LENSTR, CKNBLV, WOPWDC, CARVAR
      EXTERNAL    WMSFBC, WMSBCS, WMSSKB, WDNXPS, WMSGTE, ZIPI
C
C     + + + END SPECIFICATIONS + + +
C
      I0 = 0
      I1 = 1
      I8 = 8
      RDWRFG   = 1
      BLNK(1)  = ' '
      CCOMMA(1)= ','
C     set local version of max fields to remove
C     any chance of parameter MXFLD being modified
      LMXFLD= MXFLD
C
C     find start of screen info again
      CALL WMSFBC (WDMSFL,DSN,GNUM,
     O             DREC,DPOS,QWORD)
C
C     init local counters of ON and OFF conditions
      ONCNT = 0
      OFFCNT= 0
      POINTR= 1
C     init local arrays of target field numbers and their values
      CALL ZIPI (LMXFLD,I0,LONTFL)
      CALL ZIPI (LMXFLD,I0,LOFTFL)
      CALL ZIPI (LMXFLD,I0,LONTVL)
      CALL ZIPI (LMXFLD,I0,LOFTVL)
C
      LORDER= 0
      DONFG = 0
 100  CONTINUE
C       get block control word
        LIND = WDRCGO(WDMSFL,DREC)
        QWORD= WIBUFF(DPOS,LIND)
        IF (QWORD.GT.0) THEN
C         get parameters
          CALL WMSBCS (QWORD,
     O                 CLASS,ID,ORDER,TLEN)
          IF (ID.EQ.22) THEN
C           conditional information, process it
            IF (ORDER.NE.LORDER .AND. LORDER.NE.0 .AND.
     1          (ONCNT.GT.0 .OR. OFFCNT.GT.0)) THEN
C             conditionals for new field, clean up last fields arrays
              ONPTR = 0
              OFFPTR= 0
              IF (ONCNT.GT.0) THEN
C               on conditions exist,
C               need to loop through local target field numbers and values
                J= 0
                DO 110 I= 1,ONCNT,4
                  ONFTFL(POINTR+J)= WOPWDC(LONTFL(I),LONTFL(I+1),
     I                                     LONTFL(I+2),LONTFL(I+3))
                  ONFTVL(POINTR+J)= WOPWDC(LONTVL(I),LONTVL(I+1),
     I                                     LONTVL(I+2),LONTVL(I+3))
                  J= J+ 1
 110            CONTINUE
                ONPTR = POINTR
                POINTR= POINTR+ INT(ONCNT/4)+ 1
              END IF
              IF (OFFCNT.GT.0) THEN
C               off conditions exist,
C               need to loop through local target field numbers and values
                J= 0
                DO 120 I= 1,OFFCNT,4
                  ONFTFL(POINTR+J)= WOPWDC(LOFTFL(I),LOFTFL(I+1),
     I                                     LOFTFL(I+2),LOFTFL(I+3))
                  ONFTVL(POINTR+J)= WOPWDC(LOFTVL(I),LOFTVL(I+1),
     I                                     LOFTVL(I+2),LOFTVL(I+3))
                  J= J+ 1
 120            CONTINUE
                OFFPTR= POINTR
                POINTR= POINTR+ INT(OFFCNT/4)+ 1
              END IF
C             now calculate word of pointers and counts for previous field
              OPONOF(LORDER)= WOPWDC(ONPTR,ONCNT,OFFPTR,OFFCNT)
C             reset counters
              ONCNT = 0
              OFFCNT= 0
            END IF
            LORDER= ORDER
            GLEN= 0
            MLEN= 0
C           get multiple records of text
 130        CONTINUE
C             get records until end of id information
              ILEN= 80
              CALL WMSGTE (WDMSFL,TLEN,ILEN,
     M                     DREC,DPOS,GLEN,MLEN,
     O                     OLEN,TBUFF,CONT)
              IPOS= STRFND(OLEN,TBUFF,I1,BLNK)+ 1
 140          CONTINUE
C               find start of next conditional field
                I= CKNBLV(OLEN-IPOS+1,TBUFF(IPOS))
                IF (I.GT.0) THEN
C                 start of another conditional field found
                  IPOS= IPOS+ I- 1
C                 determine its length
                  ILEN= STRFND(OLEN-IPOS+1,TBUFF(IPOS),I1,CCOMMA)
                  IF (ILEN.EQ.0) THEN
C                   no comma found, last conditional name on this record
                    ILEN= OLEN- IPOS+ 1
                  ELSE
C                   comma found, dont include in string to search for
                    ILEN= ILEN- 1
                  END IF
                  L= LENSTR(ILEN,TBUFF(IPOS))
                  CALL CARVAR (L,TBUFF(IPOS),I8,CFDNAM)
C                 look for conditional field among field names
                  CFLD= 0
                  I= 0
 150              CONTINUE
                    I= I+ 1
                    IF (CFDNAM.EQ.FDNAME(I)) THEN
C                     found match
                      CFLD= I
                      I= NFLDS
                    END IF
                  IF (I.LT.NFLDS) GO TO 150
                  IF (CFLD.GT.0) THEN
C                   field found, put into local array
                    IF (TBUFF(3).EQ.'N') THEN
C                     if source field is ON condition,
                      ONCNT= ONCNT+ 1
                      LONTFL(ONCNT)= CFLD
C                     check position 5 of TBUFF for value of target field
                      IF (TBUFF(5).EQ.'N') THEN
C                       then turn target field ON
                        LONTVL(ONCNT)= 1
                      ELSE
C                       then turn target field OFF
                        LONTVL(ONCNT)= 0
                      END IF
                    ELSE
C                     if source field is OFF condition,
                      OFFCNT= OFFCNT+ 1
                      LOFTFL(OFFCNT)= CFLD
C                     check position 6 of TBUFF for value of target field
                      IF (TBUFF(6).EQ.'N') THEN
C                       then turn target field ON
                        LOFTVL(OFFCNT)= 1
                      ELSE
C                       then turn target field OFF
                        LOFTVL(OFFCNT)= 0
                      END IF
                    END IF
                  END IF
                  IPOS= IPOS+ ILEN+ 1
                ELSE
C                 no more conditional fields on record
                  IPOS= OLEN
                END IF
              IF (IPOS.LT.OLEN) GO TO 140
            IF (CONT.EQ.1) GO TO 130
          ELSE
C           move to next block of info
            CALL WMSSKB (WDMSFL,TLEN,
     M                   DREC,DPOS)
          END IF
C
C         move to next data position
          CALL WDNXPS (WDMSFL,RDWRFG,
     M                 DREC,DPOS,
     O                 LIND)
        ELSE
C         question terminator
          DONFG= 1
          IF (ONCNT.GT.0 .OR. OFFCNT.GT.0) THEN
C           clean up last fields arrays
            ONPTR = 0
            OFFPTR= 0
            IF (ONCNT.GT.0) THEN
C             on conditions exist,
C             need to loop through local target field numbers and values
              J= 0
              DO 160 I= 1,ONCNT,4
                ONFTFL(POINTR+J)= WOPWDC(LONTFL(I),LONTFL(I+1),
     I                                   LONTFL(I+2),LONTFL(I+3))
                ONFTVL(POINTR+J)= WOPWDC(LONTVL(I),LONTVL(I+1),
     I                                   LONTVL(I+2),LONTVL(I+3))
                J= J+ 1
 160          CONTINUE
              ONPTR = POINTR
              POINTR= POINTR+ INT(ONCNT/4)+ 1
            END IF
            IF (OFFCNT.GT.0) THEN
C             off conditions exist,
C             need to loop through local target field numbers and values
              J= 0
              DO 170 I= 1,OFFCNT,4
                ONFTFL(POINTR+J)= WOPWDC(LOFTFL(I),LOFTFL(I+1),
     I                                   LOFTFL(I+2),LOFTFL(I+3))
                ONFTVL(POINTR+J)= WOPWDC(LOFTVL(I),LOFTVL(I+1),
     I                                   LOFTVL(I+2),LOFTVL(I+3))
                J= J+ 1
 170          CONTINUE
              OFFPTR= POINTR
            END IF
C           now calculate word of pointers and counts for previous field
            OPONOF(LORDER)= WOPWDC(ONPTR,ONCNT,OFFPTR,OFFCNT)
          END IF
        END IF
      IF (DONFG.EQ.0) GO TO 100
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSDLM
     I                   (SCNOPT,ILEN,
     M                    TBUFF)
C
C     + + + PURPOSE + + +
C     Fill in delimeters in screen name with text
C     provided from application.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     ILEN
      CHARACTER*1 TBUFF(ILEN)
      CHARACTER*8 SCNOPT(6)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SCNOPT - text provided from application to fill in delimeters
C     ILEN   - length of string containing screen name
C     TBUFF  - screen name
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,J,K,I1,CNUM,IPOS
      CHARACTER*1 CTMP(8),DELIM(1)
C
C     + + + FUNCTIONS + + +
      INTEGER     STRFND, ZLNTXT
C
C     + + + EXTERNALS + + +
      EXTERNAL    STRFND, ZLNTXT, CVARAR, CHRDEL, CHRINS
C
C     + + + END SPECIFICATIONS + + +
C
      I1  = 1
      DELIM(1)= '&'
C
C     look for delimeter within window name to be filled
      CNUM= 0
 10   CONTINUE
        K= STRFND(ILEN,TBUFF,I1,DELIM)
        IF (K .GT. 0) THEN
C         found delimeter, remove it
          CALL CHRDEL (ILEN,K,TBUFF)
          CNUM= CNUM+ 1
          J= ZLNTXT(SCNOPT(CNUM))
          IF (J .GT. 0) THEN
C           string to insert
            CALL CVARAR (J,SCNOPT(CNUM),J,CTMP)
            DO 20 I= 1,J
              IPOS= K+ I- 1
              IF (IPOS.LE.ILEN) THEN
C               theres room to insert character
                CALL CHRINS (ILEN,K+I-1,CTMP(I),TBUFF)
              END IF
 20         CONTINUE
          END IF
        END IF
      IF (K.GT.0 .AND. CNUM.LT.6) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGFD
     O                   (LFDFMT)
C
C     + + + PURPOSE + + +
C     Get field selected information for a 2d parm screen.
C     * * * This routine is specific to the GenScn system       * * *
C     * * * program and resides here to keep AIDE common blocks * * *
C     * * * within the same code group.                         * * *
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER*8     LFDFMT(30)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LFDFMT - field name
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
C
C     + + + LOCAL VARIABLES + + +
      INTEGER   I,J
C
C     + + + END SPECIFICATIONS + + +
C
      IF (NFLDS.GT.30) THEN
C       this should not happen for the purposes of this routine
        WRITE (99,*) 'BIG PROBLEM in WMSGFD: NFLDS > 30, NFLDS =',NFLDS
        J= 30
      ELSE
C       use all available fields
        J= NFLDS
      END IF
C
      DO 10 I = 1,J
        LFDFMT(I) = FDFMT(I)
 10   CONTINUE
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGPN
     I                   (WDMSFL,DSN,GNUM,
     O                    PNAME)
C
C     + + + PURPOSE + + +
C     Return the PNAME values from a table dataset.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     WDMSFL,DSN,GNUM
      CHARACTER*1 PNAME(8,30)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     WDMSFL - Fortran unit number for WDM file
C     DSN    - data-set number on WDM file
C     GNUM   - group number in data set
C     PNAME  - array of PNAMEs for fields in table
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     DREC,DPOS,CLASS,ID,ORDER,TLEN,ILEN,GLEN,MLEN,
     $            BCWORD,OLEN,CONT
C
C     + + + INTRINSICS + + +
      INTRINSIC   MOD
C
C     + + + EXTERNALS + + +
      EXTERNAL    WMSFBC, WMSBCS, WDNXPS, WDNXDV, WMSGTE
C
C     + + + END SPECIFICATIONS + + +
C
C     get first block control word for group
      CALL WMSFBC (WDMSFL,DSN,GNUM,
     O             DREC,DPOS,BCWORD)
C
 10   CONTINUE
C       look through group for matching ids
        IF (BCWORD.GT.0) THEN
C         split block control word
          CALL WMSBCS (BCWORD,
     O                 CLASS,ID,ORDER,TLEN)
C         do ids match
          IF (ID.EQ.20) THEN
C           PNAME id, get the text
            ILEN= 8
            GLEN= 0
            MLEN= 0
            CALL WMSGTE (WDMSFL,TLEN,ILEN,
     M                   DREC,DPOS,GLEN,MLEN,
     O                   OLEN,PNAME(1,ORDER),CONT)
          ELSE
C           ids dont match, skip to next BCWORD
            CALL WMSSKB (WDMSFL,TLEN,
     M                   DREC,DPOS)
          END IF
C         get the next block control word
          CALL WDNXDV (WDMSFL,
     M                 DREC,DPOS,
     O                 BCWORD)
        END IF
      IF (BCWORD.GT.0) GO TO 10
C
      RETURN
      END
C
C
C
      SUBROUTINE   WMSGTV
     I                   (COLNO,INIT,
     O                    VTEXT,RETCOD)
C
C     + + + PURPOSE + + +
C     Get valid values for a specified field of the current
C     parm2 screen.
C     * * * This routine is specific to GenScn          * * *
C     * * * and resides here to keep AIDE common blocks * * *
C     * * * within the same code group.                 * * *
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER     COLNO,INIT,RETCOD
      CHARACTER*1 VTEXT(80)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     COLNO  - column number (field) to get valid/invalid values
C     INIT   - get the first of these values
C     VTEXT  - valid value text
C     RETCOD - return code
C
C     + + + PARAMETERS + + +
      INCLUDE 'pmxfld.inc'
C
C     + + + COMMON BLOCKS + + +
      INCLUDE 'cscren.inc'
      INCLUDE 'zcntrl.inc'
C
C     + + + EXTERNALS + + +
      EXTERNAL  ZIPC
C
C     + + + SAVE VARIABLES + + +
      INTEGER   IPOS
      SAVE      IPOS
C
C     + + + LOCAL VARIABLES + + +
      INTEGER     I,IEND,ILEN
      CHARACTER*1 CB
C
C     + + + END SPECIFICATIONS + + +
C
      CB = ' '
      I  = 80
      CALL ZIPC(I,CB,VTEXT)

      ILEN = FLEN(COLNO)
      IF (INIT.EQ.1) THEN
C       want to get the first valid/invalid value for this field
        IPOS = 0
        IF (COLNO.NE.1) THEN
C         calculate where to start in response string
          DO 10 I = 2,COLNO
            IPOS = IPOS + (CCNT(I-1)*FLEN(I-1))
 10       CONTINUE
        END IF
      END IF
C
      DO 20 I = 1,ILEN
        VTEXT(I) = RSPSTR(IPOS+I)
 20   CONTINUE
C
C     calculate end of this field's responses
      IEND = 0
      DO 30 I = 1,COLNO
        IEND = IEND + (CCNT(I)*FLEN(I))
 30   CONTINUE
C
      IF (IPOS+ILEN .LT. IEND) THEN
C       still more to get
        IPOS = IPOS + ILEN
        RETCOD = 0
      ELSE
C       all done
        RETCOD = -1
      END IF
C
      RETURN
      END
