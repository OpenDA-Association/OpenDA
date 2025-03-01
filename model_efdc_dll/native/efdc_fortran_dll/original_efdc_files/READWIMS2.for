      SUBROUTINE READWIMS2

C
C GEOSR 2010.10.30
C
C ** SPECIFY THE TOXIC LOADING POINT
C 
C
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::L
	REAL::TXDIST,DISTMN

      DISTMN=9999999.

      DO L=1,LC
       TXDIST=SQRT((DLON(L)-XTX)**2+(DLAT(L)-YTX)**2)
       IF(TXDIST.LT.DISTMN)THEN
        DISTMN=TXDIST
        ITX=IL(L)
	  JTX=JL(L)
       ENDIF
      ENDDO

C      IF(IDTOX.GT.0.AND.IDTOX.LT.4440)THEN  ! ONLY FOR TOXIC MODULE (CWCHO), 2014.09.14. YSSONG, COMMENTOUT 
        OPEN(21,FILE='TOXEVENT.LOG',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(21,8999) XTX,YTX,ITX,JTX
C	ENDIF                                      ! 2014.09.14. YSSONG, COMMENTOUT 

	IF(IDTOX.GE.4440)THEN  ! ONLY FOR OIL MODULE (CWCHO) 
        OPEN(21,FILE='OILEVENT.LOG',POSITION='APPEND',STATUS='UNKNOWN')  
        WRITE(21,8999) XTX,YTX  !,ITX,JTX
	ENDIF

 8999 FORMAT('LOADING POINT        :',2(F12.3,1X),2X,'(',I4,',',I4,')')
C8998 FORMAT('LOADING POINT        :',2(F12.3,1X)) !,2X,'(',I4,',',I4,')')
      CLOSE(21)

      RETURN
      END