C     CQRSP.INC 2.1 9/4/91
C
C     These common blocks are used by various user response subroutines
C
      COMMON /CQRSPI/ NANS,LANS,DANS,QUINIT,RSINIT,
     1                ZOPLIN,    ZOPCOL,    ZOPLEN
      INTEGER         NANS,LANS,DANS,QUINIT,RSINIT,
     1                ZOPLIN(64),ZOPCOL(64),ZOPLEN(64)
C
      COMMON /CQRSPC/ QUEST,    TANS,     UANS,    TXT,
     1                OMSG1,    OMSG2,    OBUFF,    IBUFF
      CHARACTER*1     QUEST(78),TANS(480),UANS(20),TXT(64,2),
     1                OMSG1(30),OMSG2(30),OBUFF(80),IBUFF(80)
C
      SAVE   /CQRSPI/, /CQRSPC/
C
