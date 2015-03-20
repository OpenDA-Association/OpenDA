      SUBROUTINE BEDPLTH  
C  
C CHANGE RECORD  
C  11/14/2001       JOHN HAMRIC        11/14/2001       JOHN HAMRIC  
C   ADD OUTPUT OF BED LOAD TRANSPORT QSBDLDX  QSBDLDY  
C **  SUBROUTINE WRITES SEDIMENT BED PROPERTIES  
C  
      USE GLOBAL  
	IMPLICIT NONE
	INTEGER::L,K,NX,NS,NSXD,KTMP
	REAL::TIME 
C  
C71A CONTROLS FOR HORIZONTAL PLANE SEDIMENT BED PROPERTIES CONTOURING  
C  
C      ISBPH:  1 TO WRITE FILES FOR SED BED PROPERTY CONTOURING IN HORIZ  
C              2 WRITE ONLY DURING LAST REFERENCE TIME PERIOD  
C     ISBEXP:  0 ASCII FORMAT, 1 EXPLORER BINARY FORMAT  
C      NPBPH:    NUMBER OF WRITES PER REFERENCE TIME PERIOD  
C     ISRBPH:  1 TO WRITE FILES FOR RESIDUAL SED BED PROPERTY CONTOURING  
C     ISBBDN:  1 WRITE LAYER BULK DENSITY  
C     ISBLAY:  1 WRITE LAYER THICKNESSES  
C     ISBPOR:  1 WRITE LAYER POROSITY  
C     ISBSED:  1 WRITE COHESIVE SEDIMENT (MASS PER UNIT AREA)  
C              2 WRITE COHESIVE SEDIMENT (FRACTION OF TOTAL SEDIMENT)  
C              3 WRITE COHESIVE SEDIMENT (FRACTION OF TOTAL SEDIMENT+WAT  
C     ISBSED:  1 WRITE NONCOHESIVE SEDIMENT (MASS PER UNIT AREA)  
C              2 WRITE NONOOHESIVE SEDIMENT (FRACTION OF TOTAL SEDIMENT)  
C              3 WRITE NONCOHESIVE SEDIMENT (FRACTION OF TOTAL SEDIMENT+  
C     ISBVDR:  1 WRITE LAYER VOID RATIOS  
C  
      IF(JSBPH.EQ.1)THEN  
        IF(ISBEXP.EQ.0)THEN
          OPEN(1,FILE='BEDSUM.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDSUM.OUT')  
          WRITE(1,110)  
          CLOSE(1)  
          OPEN(1,FILE='BEDSED.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDSED.OUT')  
          WRITE(1,111)  
          CLOSE(1)  
          OPEN(1,FILE='BEDSND.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDSND.OUT')  
          WRITE(1,112)  
          CLOSE(1)  
          OPEN(1,FILE='BEDVDR.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDVDR.OUT')  
          WRITE(1,113)  
          CLOSE(1)  
          OPEN(1,FILE='BEDPOR.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDPOR.OUT')  
          WRITE(1,114)  
          CLOSE(1)  
          OPEN(1,FILE='BEDLAY.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDLAY.OUT')  
          WRITE(1,115)  
          CLOSE(1)  
          OPEN(1,FILE='BEDBDN.OUT')  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE='BEDBDN.OUT')  
          WRITE(1,116)  
          CLOSE(1)  
        ENDIF
        OPEN(1,FILE='BEDARD.OUT')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='BEDARD.OUT')  
        WRITE(1,131)  
        CLOSE(1)  
        JSBPH=0  
      ENDIF  
C  
C **  ENTER HERE ON SUBQUENT CALLS  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      NSXD=NSED+NSND  
C
      IF(ISBEXP.EQ.0)THEN
        OPEN(1,FILE='BEDSUM.OUT',POSITION='APPEND')  
        WRITE(1,122)TIME  
        DO L=2,LA  
          KTMP=KBT(L)  
          WRITE(1,103)IL(L),JL(L),KTMP,BELV(L),HBED(L,KTMP),  
     &      VDRBED(L,KTMP),(VFRBED(L,KTMP,NX),NX=1,NSXD)  
        ENDDO  
        CLOSE(1)  
        IF(ISBSED.GE.1)THEN  
          OPEN(1,FILE='BEDSED.OUT',POSITION='APPEND')  
          WRITE(1,122)TIME  
          IF(ISBSED.EQ.1)THEN  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(SEDB(L,K,1),K=1,KB)  
              IF(NSED.GT.1) THEN  
                DO NX=2,NSED  
                  WRITE(1,102)(SEDB(L,K,NX),K=1,KB)  
                END DO  
              ENDIF  
            ENDDO  
          ENDIF  
          IF(ISBSED.GE.2)THEN  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(VFRBED(L,K,1),K=1,KB)  
              IF(NSED.GT.1) THEN  
                DO NX=2,NSED  
                  WRITE(1,102)(VFRBED(L,K,NX),K=1,KB)  
                END DO  
              ENDIF  
            ENDDO  
          ENDIF  
          CLOSE(1)  
        ENDIF  
        IF(ISBSND.GE.1)THEN  
          OPEN(1,FILE='BEDSND.OUT',POSITION='APPEND')  
          WRITE(1,122)TIME  
          IF(ISBSND.EQ.1)THEN  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(SNDB(L,K,1),K=1,KB)  
              IF(NSND.GT.1)THEN  
                DO NX=2,NSND  
                  WRITE(1,102)(SNDB(L,K,NX),K=1,KB)  
                END DO  
              ENDIF  
            ENDDO  
          ENDIF  
          IF(ISBSND.GE.2)THEN  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(VFRBED(L,K,NSED+1),K=1,KB)  
              IF(NSND.GT.1)THEN  
                DO NX=NSED+2,NSED+NSND  
                  WRITE(1,102)(VFRBED(L,K,NX),K=1,KB)  
                END DO  
              ENDIF  
            ENDDO  
          ENDIF  
          CLOSE(1)  
          IF(ISBVDR.GE.1)THEN  
            OPEN(1,FILE='BEDVDR.OUT',POSITION='APPEND')  
            WRITE(1,122)TIME  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(VDRBED(L,K),K=1,KB)  
            ENDDO  
            CLOSE(1)  
          ENDIF  
          IF(ISBPOR.GE.1)THEN  
            OPEN(1,FILE='BEDPOR.OUT',POSITION='APPEND')  
            WRITE(1,122)TIME  
            DO L=2,LA  
              WRITE(1,101)IL(L),JL(L),(PORBED(L,K),K=1,KB)  
            ENDDO  
            CLOSE(1)  
          ENDIF  
          OPEN(1,FILE='BEDLAY.OUT',POSITION='APPEND')  
          WRITE(1,122)TIME  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),ZELBEDA(L),HBEDA(L),
     &                  (HBED(L,K),K=1,KB)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
        IF(ISBBDN.GE.1)THEN  
          OPEN(1,FILE='BEDBDN.OUT',POSITION='APPEND')  
          WRITE(1,122)TIME  
          DO L=2,LA  
            WRITE(1,101)IL(L),JL(L),(BDENBED(L,K),K=1,KB)  
          ENDDO  
          CLOSE(1)  
        ENDIF  
      ENDIF
C
      IF(ISBARD.GE.1)THEN  
        OPEN(1,FILE='BEDARD.OUT',POSITION='APPEND')  
        WRITE(1,122)TIME  
        DO L=2,LA  
          WRITE(1,101)IL(L),JL(L),  
     &        (SEDFDTAP(L,NS),SEDFDTAN(L,NS),NS=1,NSED),  
     &        (SNDFDTAP(L,NS),SNDFDTAN(L,NS),NS=1,NSND)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
  339 FORMAT(2I5,6F14.5)  
  103 FORMAT(3I5,18E13.5)  
  101 FORMAT(2I5,18E13.5)  
  102 FORMAT(10X,18E13.5)  
  110 FORMAT('   IL   JL    KBT  HTOP   VOIDR  SED/SND VOL FRACS ' )  
  111 FORMAT('   IL   JL    SEDBT(K=1,KB)')  
  112 FORMAT('   IL   JL    SNDBT(K=1,KB)')  
  113 FORMAT('   IL   JL    VRDBED(K=1,KB)')  
  114 FORMAT('   IL   JL    PORBED(K=1,KB)')  
  115 FORMAT('   IL   JL    ZBEDB        HBEDT        HBED(K=1,KB)')  
  116 FORMAT('   IL   JL    BDENBED(K=1,KB)')  
  118 FORMAT('   IL   JL    SEDT(K=1,KC)')  
  119 FORMAT('   IL   JL    SNDT(K=1,KC)')  
  120 FORMAT('   IL   JL    QSBDLDX      QSBDLDY')  
  121 FORMAT('   IL   JL    TOXB(K=1,KB,NT)  NT = ',I5)  
  131 FORMAT('   IL   JL    (SEDFDTAP SEDFDTAN)(1,NSED)',  
     &    '   (SNDFDTAP SNDFDTAN)(1,NSND)')  
  122 FORMAT(F12.5,'  TIME OF OUTPUT')  
  906 FORMAT(5E17.8)  
  907 FORMAT(13E17.8)  
  908 FORMAT(12I10)  
  909 FORMAT(I20,4X,F12.4)  
  910 FORMAT(6I5,2X,E17.8,2X,E17.8)  
  911 FORMAT(2I5,2X,6E13.4)  
  912 FORMAT(3I5,12F7.3)  
  913 FORMAT(6I5,4F7.3)  
      RETURN  
      END  

