      SUBROUTINE SALPLTH (ICON,CONC)  
C  
C CHANGE RECORD  
C **  SUBROUTINE SALPLTH WRITES FILES FOR INSTANTANEOUS SCALAR FIELD  
C **  CONTOURING IN HORIZONTAL PLANES  
C  
      USE GLOBAL  

      DIMENSION DBS(10)  
      CHARACTER*80 TITLE  
      DIMENSION CONC(LCM,KCM)  
      REAL,ALLOCATABLE,DIMENSION(:)::DBSB  

      IF(.NOT.ALLOCATED(DBSB)) ALLOCATE(DBSB(0:NSTM))  
      DBSB=0.  
C  
      IF(JSSPH(ICON).NE.1) GOTO 300  
      LINES=LA-1  
      LEVELS=2  
      LEVELSS=3  
      DBS(1)=0.  
      DBS(2)=99.  
      DBS(3)=-99.  
      LSEDCL=NSED+NSND  
      DO L=0,LSEDCL  
        DBSB(L)=FLOAT(L)  
      ENDDO  
      IF(ICON.EQ.1.AND.ISPHXY(1).LE.2)THEN  
        TITLE='INSTANTANEOUS HORIZONTAL SALINITY CONTOURS'  
        LUN=11  
        OPEN(LUN,FILE='SALCONH.OUT')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='SALCONH.OUT')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ICON.EQ.2.AND.ISPHXY(2).LE.2)THEN  
        TITLE='INSTANTANEOUS HORIZONTAL TEMPERATURE CONTOURS'  
        LUN=12  
        OPEN(LUN,FILE='TEMCONH.OUT')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='TEMCONH.OUT')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ICON.EQ.3.AND.ISPHXY(3).LE.2)THEN  
        TITLE='INSTANTANEOUS HORIZONTAL DYE CONC CONTOURS'  
        LUN=13  
        OPEN(LUN,FILE='DYECONH.OUT')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='DYECONH.OUT')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELS)  
        CLOSE(LUN)  
      ENDIF  
      IF(ICON.EQ.6.AND.ISPHXY(6).LE.2)THEN  
        TITLE='INSTANTANEOUS HORIZ COHESIVE SEDIMENT CONC CONTOURS'  
        LUN=14  
        OPEN(LUN,FILE='SEDCONH.OUT')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='SEDCONH.OUT')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
      ENDIF  
C  
C        TITLE='INSTANTANEOUS BED SED DEPOSITED CONTOURS GM/M**2'  
C        LUN=15  
C  
      IF(ICON.EQ.7.AND.ISPHXY(7).LE.2)THEN  
        IF(NSND.GE.1)THEN  
          TITLE='INSTANTANEOUS HORIZ NONCOH SEDIMENT CONC CONTOURS'  
          LUN=15  
          OPEN(LUN,FILE='SNDCONH.OUT')  
          CLOSE(LUN,STATUS='DELETE')  
          OPEN(LUN,FILE='SNDCONH.OUT')  
          WRITE (LUN,99) TITLE  
          WRITE (LUN,101)LINES,LEVELSS  
          WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
          CLOSE(LUN)  
        ENDIF  
        IF(NSND.GE.2)THEN  
          TITLE='INSTANTANEOUS HORIZ NONCOH SEDIMENT CONC CONTOURS'  
          LUN=15  
          OPEN(LUN,FILE='SNDCONH01.OUT')  
          CLOSE(LUN,STATUS='DELETE')  
          OPEN(LUN,FILE='SNDCONH01.OUT')  
          WRITE (LUN,99) TITLE  
          WRITE (LUN,101)LINES,LEVELSS  
          WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
          CLOSE(LUN)  
          OPEN(LUN,FILE='SNDCONH02.OUT')  
          CLOSE(LUN,STATUS='DELETE')  
          OPEN(LUN,FILE='SNDCONH02.OUT')  
          WRITE (LUN,99) TITLE  
          WRITE (LUN,101)LINES,LEVELSS  
          WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
          CLOSE(LUN)  
        ENDIF  
        IF(NSND.GE.3)THEN  
          TITLE='INSTANTANEOUS HORIZ NONCOH SEDIMENT CONC CONTOURS'  
          LUN=15  
          OPEN(LUN,FILE='SNDCONH03.OUT')  
          CLOSE(LUN,STATUS='DELETE')  
          OPEN(LUN,FILE='SNDCONH03.OUT')  
          WRITE (LUN,99) TITLE  
          WRITE (LUN,101)LINES,LEVELSS  
          WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
          CLOSE(LUN)  
        ENDIF  
      ENDIF  
C  
C       TITLE='INSTANTANEOUS BED SED DEPOSITED CONTOURS GM/M**2'  
C       LUN=15  
C  
      IF(ICON.EQ.5.AND.ISPHXY(5).LE.2)THEN  
        TITLE='INSTANTANEOUS HORIZ TOXIC CONTAM. CONC CONTOURS'  
        LUN=16  
        OPEN(LUN,FILE='TOXCONH.OUT')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='TOXCONH.OUT')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
        TITLE='INSTANTANEOUS HORIZ TOXIC PART FRAC CONTOURS'  
        LUNF=26  
        OPEN(LUNF,FILE='TXPCONH.OUT')  
        CLOSE(LUNF,STATUS='DELETE')  
        OPEN(LUNF,FILE='TXPCONH.OUT')  
        WRITE (LUNF,99) TITLE  
        WRITE (LUNF,101)LINES,LEVELSS  
        WRITE (LUNF,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUNF)  
      ENDIF  
      IF(ICON.EQ.4.AND.ISPHXY(4).LE.2)THEN  
        TITLE='INSTANTANEOUS HORIZONTAL SFL CONC CONTOURS'  
        LUN=17  
        OPEN(LUN,FILE='SFLCONH.OUT')  
        CLOSE(LUN,STATUS='DELETE')  
        OPEN(LUN,FILE='SFLCONH.OUT')  
        WRITE (LUN,99) TITLE  
        WRITE (LUN,101)LINES,LEVELSS  
        WRITE (LUN,250)(DBS(L),L=1,LEVELSS)  
        CLOSE(LUN)  
      ENDIF  
      JSSPH(ICON)=0  
  300 CONTINUE  
      IF(ICON.EQ.1.AND.ISPHXY(1).LE.2)THEN  
        LUN=11  
        OPEN(LUN,FILE='SALCONH.OUT',POSITION='APPEND')  
      ENDIF  
      IF(ICON.EQ.2.AND.ISPHXY(2).LE.2)THEN  
        LUN=12  
        OPEN(LUN,FILE='TEMCONH.OUT',POSITION='APPEND')  
      ENDIF  
      IF(ICON.EQ.3.AND.ISPHXY(3).LE.2)THEN  
        LUN=13  
        OPEN(LUN,FILE='DYECONH.OUT',POSITION='APPEND')  
      ENDIF  
      IF(ICON.EQ.6.AND.ISPHXY(6).LE.2)THEN  
        LUN=14  
        OPEN(LUN,FILE='SEDCONH.OUT',POSITION='APPEND')  
      ENDIF  
      IF(ICON.EQ.7.AND.ISPHXY(7).LE.2)THEN  
        LUN=15  
        OPEN(LUN,FILE='SNDCONH.OUT',POSITION='APPEND')  
        IF(NSND.GE.2)THEN  
          LUN1=25  
          OPEN(LUN1,FILE='SNDCONH01.OUT',POSITION='APPEND')  
          LUN2=35  
          OPEN(LUN2,FILE='SNDCONH02.OUT',POSITION='APPEND')  
        ENDIF  
        IF(NSND.GE.3)THEN  
          LUN3=45  
          OPEN(LUN3,FILE='SNDCONH03.OUT',POSITION='APPEND')  
        ENDIF  
      ENDIF  
      IF(ICON.EQ.5.AND.ISPHXY(5).LE.2)THEN  
        LUN=16  
        OPEN(LUN,FILE='TOXCONH.OUT',POSITION='APPEND')  
        LUNF=26  
        OPEN(LUNF,FILE='TXPCONH.OUT',POSITION='APPEND')  
      ENDIF  
      IF(ICON.EQ.4.AND.ISPHXY(4).LE.2)THEN  
        LUN=17  
        OPEN(LUN,FILE='SFLCONH.OUT',POSITION='APPEND')  
      ENDIF  
C  
C        LUB=18  
C       LUB=18  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=DT*FLOAT(N)+TCON*TBEGIN  
        TIME=TIME/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
      IF(ISPHXY(ICON).LE.2)THEN  
        WRITE (LUN,100)N,TIME  
        IF(ICON.EQ.5)THEN  
          WRITE (LUNF,100)N,TIME  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          IF(NSND.GE.2)THEN  
            WRITE (LUN1,100)N,TIME  
            WRITE (LUN2,100)N,TIME  
          ENDIF  
          IF(NSND.GE.3)THEN  
            WRITE (LUN3,100)N,TIME  
          ENDIF  
        ENDIF  
      ENDIF  
      IF(ISPHXY(ICON).EQ.0)THEN  
        IF(ICON.LE.3)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,KC),CONC(L,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.GE.8)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,KC),CONC(L,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.6)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,1),SEDBT(L,KBT(L)),SEDF(L,0,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,KC),CONC(L,1),SEDBT(L,KBT(L)),
     &            SEDF(L,0,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,1),SNDBT(L,KBT(L))  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,400)CONC(L,KC),CONC(L,1),SNDBT(L,KBT(L))  
            ENDDO  
          ENDIF  
          IF(NSND.GE.2)THEN  
            IF(KC.EQ.1)THEN  
              DO L=2,LA  
                WRITE(LUN1,400)SND(L,1,1),SNDB(L,KBT(L),1),  
     &              SNDF(L,0,1),SNDFBL(L,1),CQBEDLOADX(L,1)  
                WRITE(LUN2,400)SND(L,1,2),SNDB(L,KBT(L),2),  
     &              SNDF(L,0,2),SNDFBL(L,2),CQBEDLOADX(L,2)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(LUN1,400)SND(L,KC,1),SND(L,1,1),SNDB(L,KBT(L),1),  
     &              SNDF(L,0,1),SNDFBL(L,1),CQBEDLOADX(L,1)  
                WRITE(LUN2,400)SND(L,KC,2),SND(L,1,2),SNDB(L,KBT(L),2),  
     &              SNDF(L,0,2),SNDFBL(L,2),CQBEDLOADX(L,2)  
              ENDDO  
            ENDIF  
          ENDIF  
          IF(NSND.GE.3)THEN  
            IF(KC.EQ.1)THEN  
              DO L=2,LA  
                WRITE(LUN3,400)SND(L,1,NSND),SNDB(L,KBT(L),NSND),  
     &              SNDF(L,0,NSND),SNDFBL(L,NSND),CQBEDLOADX(L,NSND)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(LUN3,400)SND(L,KC,NSND),SND(L,1,NSND),  
     &              SNDB(L,KBT(L),NSND),SNDF(L,0,NSND),SNDFBL(L,NSND),  
     &              CQBEDLOADX(L,NSND)  
              ENDDO  
            ENDIF  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.5)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,400)TOX(L,1,1),TOXB(L,KBT(L),1),  
     &            TOXF(L,0,1),TOXFB(L,KBT(L),1)  
              WRITE(LUNF,400)TOXPFTW(L,1,1),TOXPFTB(L,KBT(L),1)  
            ENDDO  
          ENDIF  
          IF(KC.GT.1)THEN  
            DO L=2,LA  
              WRITE(LUN,400)TOX(L,KC,1),TOX(L,1,1),TOXB(L,KBT(L),1),  
     &            TOXB(L,1,1)  
              WRITE(LUNF,400)TOXPFTW(L,KC,1),TOXPFTW(L,1,1),  
     &            TOXPFTB(L,KBT(L),1),TOXPFTB(L,1,1)  
            ENDDO  
          ENDIF  
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
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.GE.8)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.6)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,1),SEDBT(L,KBT(L)),
     &            SEDF(L,0,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1),  
     &            SEDBT(L,KBT(L)),SEDF(L,0,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,1),SNDBT(L,KBT(L))  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),CONC(L,KC),CONC(L,1),  
     &            SNDBT(L,KBT(L))  
            ENDDO  
          ENDIF  
          IF(NSND.GE.2)THEN  
            IF(KC.EQ.1)THEN  
              DO L=2,LA  
                WRITE(LUN1,200)IL(L),JL(L),SND(L,1,1),SNDB(L,KBT(L),1),  
     &              SNDF(L,0,1),SNDFBL(L,1),CQBEDLOADX(L,1)  
                WRITE(LUN2,200)IL(L),JL(L),SND(L,1,2),SNDB(L,KBT(L),2),  
     &              SNDF(L,0,2),SNDFBL(L,2),CQBEDLOADX(L,2)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(LUN1,200)IL(L),JL(L),SND(L,KC,1),SND(L,1,1),  
     &              SNDB(L,KBT(L),1),SNDF(L,0,1),SNDFBL(L,1)
     &              ,CQBEDLOADX(L,1)  
                WRITE(LUN2,200)IL(L),JL(L),SND(L,KC,2),SND(L,1,2),  
     &              SNDB(L,KBT(L),2),SNDF(L,0,2),SNDFBL(L,2)
     &              ,CQBEDLOADX(L,2)  
              ENDDO  
            ENDIF  
          ENDIF  
          IF(NSND.GE.3)THEN  
            IF(KC.EQ.1)THEN  
              DO L=2,LA  
                WRITE(LUN3,200)IL(L),JL(L),SND(L,1,NSND),SNDB(L,KBT(L)
     &                  ,NSND),SNDF(L,0,NSND),SNDFBL(L,NSND)
     &              ,CQBEDLOADX(L,NSND)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(LUN3,200)IL(L),JL(L),SND(L,KC,NSND),SND(L,1,NSND),  
     &              SNDB(L,KBT(L),NSND),SNDF(L,0,NSND),SNDFBL(L,NSND),  
     &              CQBEDLOADX(L,NSND)  
              ENDDO  
            ENDIF  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.5)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),TOX(L,1,1),TOXB(L,KBT(L),1),  
     &            TOXF(L,0,1),TOXFB(L,KBT(L),1)  
              WRITE(LUNF,200)IL(L),JL(L),TOXPFTW(L,1,1),TOXPFTB(L,
     &            KBT(L),1)  
            ENDDO  
          ENDIF  
          IF(KC.GT.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),TOX(L,KC,1),TOX(L,1,1),  
     &            TOXB(L,KBT(L),1),TOXB(L,1,1)  
              WRITE(LUNF,200)IL(L),JL(L),TOXPFTW(L,KC,1),TOXPFTW(L,1,1),  
     &            TOXPFTB(L,KBT(L),1),TOXPFTB(L,1,1)  
            ENDDO  
          ENDIF  
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
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC)
     &            ,CONC(L,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.GE.8)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC)
     &            ,CONC(L,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.6)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,1),  
     &            SEDBT(L,KBT(L)),SEDF(L,0,1)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC)
     &               ,CONC(L,1),SEDBT(L,KBT(L)),SEDF(L,0,1)  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,1),  
     &            SNDBT(L,KBT(L))  
            ENDDO  
          ELSE  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC)
     &               ,CONC(L,1),SNDBT(L,KBT(L))  
            ENDDO  
          ENDIF  
          IF(NSND.GE.2)THEN  
            IF(KC.EQ.1)THEN  
              DO L=2,LA  
                WRITE(LUN1,200)IL(L),JL(L),DLON(L),DLAT(L),SND(L,1,1),  
     &              SNDB(L,KBT(L),1),SNDF(L,0,1),SNDFBL(L,1)
     &              ,CQBEDLOADX(L,1)  
                WRITE(LUN2,200)IL(L),JL(L),DLON(L),DLAT(L),SND(L,1,2),  
     &              SNDB(L,KBT(L),2),SNDF(L,0,2),SNDFBL(L,2)
     &              ,CQBEDLOADX(L,2)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(LUN1,200)IL(L),JL(L),DLON(L),DLAT(L),SND(L,KC,1),  
     &              SND(L,1,1),SNDB(L,KBT(L),1),SNDF(L,0,1),SNDFBL(L,1),  
     &              CQBEDLOADX(L,1)  
                WRITE(LUN2,200)IL(L),JL(L),DLON(L),DLAT(L),SND(L,KC,2),  
     &              SND(L,1,2),SNDB(L,KBT(L),2),SNDF(L,0,2),SNDFBL(L,2),  
     &              CQBEDLOADX(L,2)  
              ENDDO  
            ENDIF  
          ENDIF  
          IF(NSND.GE.3)THEN  
            IF(KC.EQ.1)THEN  
              DO L=2,LA  
               WRITE(LUN3,200)IL(L),JL(L),DLON(L),DLAT(L),SND(L,1,NSND),  
     &              SNDB(L,KBT(L),NSND),SNDF(L,0,NSND),SNDFBL(L,NSND),  
     &              CQBEDLOADX(L,NSND)  
              ENDDO  
            ELSE  
              DO L=2,LA  
              WRITE(LUN3,200)IL(L),JL(L),DLON(L),DLAT(L),SND(L,KC,NSND),  
     &              SND(L,1,NSND),SNDB(L,KBT(L),NSND),  
     &              SNDF(L,0,NSND),SNDFBL(L,NSND),CQBEDLOADX(L,NSND)  
              ENDDO  
            ENDIF  
          ENDIF  
        ENDIF  
        IF(ICON.EQ.5)THEN  
          IF(KC.EQ.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),TOX(L,1,1),  
     &            TOXB(L,KBT(L),1),  
     &            TOXF(L,0,1),TOXFB(L,KBT(L),1)  
              WRITE(LUNF,200)IL(L),JL(L),DLON(L),DLAT(L),TOXPFTW(L,1,1),  
     &            TOXPFTB(L,KBT(L),1)  
            ENDDO  
          ENDIF  
          IF(KC.GT.1)THEN  
            DO L=2,LA  
              WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),TOX(L,KC,1),  
     &            TOX(L,1,1),TOXB(L,KBT(L),1),TOXB(L,1,1)  
             WRITE(LUNF,200)IL(L),JL(L),DLON(L),DLAT(L),TOXPFTW(L,KC,1),  
     &            TOXPFTW(L,1,1),TOXPFTB(L,KBT(L),1),TOXPFTB(L,1,1)  
            ENDDO  
          ENDIF  
          CLOSE(LUNF)  
        ENDIF  
        IF(ICON.EQ.4)THEN  
          DO L=2,LA  
            WRITE(LUN,200)IL(L),JL(L),DLON(L),DLAT(L),CONC(L,KC),
     &               CONC(L,1),SFLSBOT(L)  
          ENDDO  
        ENDIF  
      ENDIF  
      IF(ISPHXY(ICON).LE.2)THEN  
        CLOSE(LUN)  
        IF(ICON.EQ.5)THEN  
          CLOSE(LUNF)  
        ENDIF  
        IF(ICON.EQ.7)THEN  
          IF(NSND.GE.2)THEN  
            CLOSE(LUN1)  
            CLOSE(LUN2)  
          ENDIF  
          IF(NSND.GE.3)THEN  
            CLOSE(LUN3)  
          ENDIF  
        ENDIF  
      ENDIF  
   99 FORMAT(A80)  
  100 FORMAT(I10,F12.4)  
  101 FORMAT(2I10)  
  200 FORMAT(2I5,1X,8E14.6)  
  220 FORMAT(2I5,1X,13E11.3)  
  400 FORMAT(1X,8E14.6)  
  420 FORMAT(1X,13E12.4)  
  250 FORMAT(12E12.4)  
      RETURN  
      END  

