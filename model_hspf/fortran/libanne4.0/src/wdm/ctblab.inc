C
C     common block to store table label values
C
C     + + + PARAMETERS + + +
      INTEGER    TABMX
      PARAMETER (TABMX=30)
C
      COMMON /CTBLBD/ TABCNT,TABID,TABDIM,PDATVL
      INTEGER         TABCNT,TABID(TABMX),TABDIM(TABMX),PDATVL(TABMX)
C
      COMMON /CTBLBC/ TABNAM
      CHARACTER*1     TABNAM(16,TABMX)
C
      SAVE   /CTBLBD/, /CTBLBC/
C
