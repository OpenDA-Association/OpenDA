      SUBROUTINE RSALPLTH(ICON,CONC)  
C  
C CHANGE RECORD  
C **  SUBROUTINE RSALPLTH WRITES FILES FOR RESIDUAL SCALAR FIELD  
C **  CONTOURING IN HORIZONTAL PLANES  
C  
      USE GLOBAL  
      DIMENSION DBS(10)  
      CHARACTER*80 TITLE  
      DIMENSION CONC(LCM,KCM)  
C  
      IF(JSRSPH(ICON).NE.1) GOTO 300  
      LINES=LA-1  
      LEVELS=2  
      LEVELSS=3  
      DBS(1)=0.  
      DBS(2)=99.  
      DBS(3)=-99.  
      IF(ISTRAN(1).GE.1)THEN  
        TITLE='RESIDUAL HORIZONTAL SALINITY CONTOURS'  
        LUN=11  
        OPEN(LUN,FILE='RSALCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RSALCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ISTRAN(2).GE.1)THEN  
        TITLE='RESIDUAL HORIZONTAL TEMPERATURE CONTOURS'  
        LUN=12  
        OPEN(LUN,FILE='RTEMCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RTEMCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ISTRAN(3).GE.1)THEN  
        TITLE='RESIDUAL HORIZONTAL DYE CONC CONTOURS'  
        LUN=13  
        OPEN(LUN,FILE='RDYECNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RDYECNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ISTRAN(6).GE.1)THEN  
        TITLE='RESIDUAL HORIZ COHESIVE SEDIMENT CONC CONTOURS'  
        LUN=14  
        OPEN(LUN,FILE='RSEDCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RSEDCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ISTRAN(7).GE.1)THEN  
        TITLE='RESIDUAL HORIZ NONCOH SEDIMENT CONC CONTOURS'  
        LUN=15  
        OPEN(LUN,FILE='RSNDCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RSNDCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ISTRAN(5).GE.1)THEN  
        TITLE='RESIDUAL HORIZ TOXIC CONTAM CONC CONTOURS'  
        LUN=16  
        OPEN(LUN,FILE='RTOXCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RTOXCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
        TITLE='RESIDUAL HORIZ TOXIC PART FRAC CONTOURS'  
        LUNF=26  
        OPEN(LUNF,FILE='RTXPCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUNF,STATUS='DELETE')  
        OPEN(LUNF,FILE='RTXPCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUNF,99) TITLE  
        WRITE (LUNF,101)LINES,LEVELSS  
        WRITE (LUNF,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUNF)  
      ENDIF  
      IF(ISTRAN(4).GE.1)THEN  
        TITLE='RESIDUAL HORIZONTAL SFL CONC CONTOURS'  
        LUN=17  
        OPEN(LUN,FILE='RSFLCNH.OUT',STATUS='UNKNOWN')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='RSFLCNH.OUT',STATUS='UNKNOWN')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
      ENDIF  
      DO ITMP=1,7  
        JSRSPH(ITMP)=0  
      ENDDO  
  300 CONTINUE  
      IF(ICON.EQ.1)THEN  
        LUN=11  
        OPEN(LUN,FILE='RSALCNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ICON.EQ.2)THEN  
        LUN=12  
        OPEN(LUN,FILE='RTEMCNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ICON.EQ.3)THEN  
        LUN=13  
        OPEN(LUN,FILE='RDYECNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ICON.EQ.6)THEN  
        LUN=14  
        OPEN(LUN,FILE='RSEDCNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ICON.EQ.7)THEN  
        LUN=15  
        OPEN(LUN,FILE='RSNDCNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ICON.EQ.5)THEN  
        LUN=16  
        OPEN(LUN,FILE='RTOXCNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        LUNF=26  
        OPEN(LUNF,FILE='RTXPCNH.OUT',  
     &      POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ICON.EQ.4)THEN  
        LUN=17  
        OPEN(LUN,FILE='RSFLCNH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      ENDIF  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      WRITE (LUN,100)N,TIME  
      IF(ICON.EQ.5)THEN  
        WRITE (LUNF,100)N,TIME  
      ENDIF  
      IF(ISPHXY(ICON).EQ.0)THEN  
        IF(ICON.LE.3)THEN  
          DO L=2,LA  
            WRITE(LUN,400)CONC(L,KC),CONC(L,1)  
          ENDDO  
        ENDIF  
        IF(ICON.GE.8)THEN  
          DO L=2,LA  
            WRITE(LUN,400)CONC(L,KC),CONC(L,1)  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.6)THEN  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SEDB(L)  
C  
            WRITE(LUN,400)CONC(L,KC),CONC(L,1),  
     &          SEDBTLPF(L,KBT(L))  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SNDB(L,KBT(L))  
C  
            WRITE(LUN,400)CONC(L,KC),CONC(L,1),  
     &          SNDBTLPF(L,KBT(L))  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.5)THEN  
          DO L=2,LA  
            TOXBT=1000.*TOXBLPF(L,KBT(L),1)  
            WRITE(LUN,400)CONC(L,KC),CONC(L,1),  
     &          TOXBT  
          ENDDO  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SEDB(L,KBT(L),1)  
C  
            WRITE(LUNF,400)TXPFLPF(L,KC,1,1),  
     &          TXPFLPF(L,1,1,1),TOXBLPF(L,KBT(L),1)  
          ENDDO  
          CLOSE(LUNF)  
        ENDIF  
        IF(ICON.EQ.4)THEN  
          DO L=2,LA  
            WRITE(LUN,400)CONC(L,KC),CONC(L,1),  
     &          SFLSBOT(L)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ISPHXY(ICON).EQ.1)THEN  
        IF(ICON.LE.3)THEN  
          DO L=2,LA  
            WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1)  
          ENDDO  
        ENDIF  
        IF(ICON.GE.8)THEN  
          DO L=2,LA  
            WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1)  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.6)THEN  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SEDB(L)  
C  
            WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1),  
     &          SEDBTLPF(L,KBT(L))  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SNDB(L,KBT(L))  
C  
            WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1),  
     &          SNDBTLPF(L,KBT(L))  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.5)THEN  
          DO L=2,LA  
            TOXBT=1000.*TOXBLPF(L,KBT(L),1)  
            WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1),  
     &          TOXBT  
          ENDDO  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SEDB(L,KBT(L),1)  
C  
            WRITE(LUNF,200)IL(L),JL(L),TXPFLPF(L,KC,1,1),  
     &          TXPFLPF(L,1,1,1),TOXBLPF(L,KBT(L),1)  
          ENDDO  
          CLOSE(LUNF)  
        ENDIF  
        IF(ICON.EQ.4)THEN  
          DO L=2,LA  
            WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1),  
     &          SFLSBOT(L)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ISPHXY(ICON).EQ.2)THEN  
        IF(ICON.LE.3)THEN  
          DO L=2,LA  
          WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),CONC(L,1)  
          ENDDO  
        ENDIF  
        IF(ICON.GE.8)THEN  
          DO L=2,LA  
          WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),CONC(L,1)  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.6)THEN  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SEDB(L)  
C  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),
     &               CONC(L,1),SEDBTLPF(L,KBT(L))  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SNDB(L,KBT(L))  
C  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),
     &               CONC(L,1),SNDBTLPF(L,KBT(L))  
          ENDDO  
        ENDIF  
        IF(ICON.EQ.5)THEN  
          DO L=2,LA  
            TOXBT=1000.*TOXBLPF(L,KBT(L),1)  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),
     &               CONC(L,1),TOXBT  
          ENDDO  
          DO L=2,LA  
C  
C      SEDBT=1000*SSG*SEDB(L,KBT(L),1)  
C  
           WRITE(LUNF,200)IL(L),JL(L),DLON(L),DLAT(L),TXPFLPF(L,KC,1,1),  
     &          TXPFLPF(L,1,1,1),TOXBLPF(L,KBT(L),1)  
          ENDDO  
          CLOSE(LUNF)  
        ENDIF  
        IF(ICON.EQ.4)THEN  
          DO L=2,LA  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),
     &               CONC(L,1),SFLSBOT(L)  
          ENDDO  
        ENDIF  
      ENDIF  
      CLOSE(LUN)  
   99 FORMAT(A80)  
  100 FORMAT(I10,F12.4)  
  101 FORMAT(2I10)  
  200 FORMAT(2I5,1X,6E14.6)  
  250 FORMAT(12E12.4)  
  400 FORMAT(1X,6E14.6)  
  420 FORMAT(1X,13E11.3)  
      RETURN  
      END  

