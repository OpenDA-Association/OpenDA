C
C     Common variables for Option type data fields
C
C     + + + PARAMETERS + + +
      INTEGER    MXCON
      PARAMETER (MXCON=2*MXFLD)
C
      COMMON /CZOPTN/ OPSET,       OPSTNO,       OPDEF,
     1                OPBOX,       OPMXSL,       OPMNSL,
     2                OPSVAL,       CURSEL,       OPONOF,
     3                ONFTFL,       ONFTVL
      INTEGER         OPSET(MXFLD),OPSTNO(MXFLD),OPDEF(MXFLD),
     1                OPBOX(MXFLD),OPMXSL(MXFLD),OPMNSL(MXFLD),
     2                OPSVAL(MXFLD),CURSEL(MXFLD),OPONOF(MXFLD),
     3                ONFTFL(MXCON),ONFTVL(MXCON)
C
      SAVE   /CZOPTN/
C
