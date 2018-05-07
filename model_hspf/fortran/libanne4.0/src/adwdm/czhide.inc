C
C     Common variables for hiding data fields based on other fields values
C
      COMMON /CZHIDE/ NUMHID,HIDFLD,       HIDBOX,
     1                HIDVAL,       HIDTFL,       HIDFLG
C
      INTEGER         NUMHID,HIDFLD(MXFLD),HIDBOX(MXFLD),
     1                HIDVAL(MXFLD),HIDTFL(MXFLD),HIDFLG(MXFLD)
C
      SAVE   /CZHIDE/
C
