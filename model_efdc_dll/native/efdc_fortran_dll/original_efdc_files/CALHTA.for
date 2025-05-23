      SUBROUTINE CALHTA  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALHTA PERFORMS A HARMONIC ANALYSIS FOR THE M2 TIDE  
C **  OVER TWO TIDAL CYCLES  
C  
      USE GLOBAL  
      USE MPI
      CHARACTER*80 TITLE1,TITLE2,TITLE3,TITLE4,TITLE11,TITLE12  
C  
C **  INITIALIZE ON FIRST ENTRY FOR CURRENT ANALYSIS INTERVAL  
C  
      IF(NHAR.GT.1) GOTO 1000  
      DO L=2,LA  
        AMCP(L)=0.  
        AMSP(L)=0.  
        AMCUE(L)=0.  
        AMSUE(L)=0.  
        AMCVE(L)=0.  
        AMSVE(L)=0.  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          AMCU(L,K)=0.  
          AMSU(L,K)=0.  
          AMCV(L,K)=0.  
          AMSV(L,K)=0.  
        ENDDO  
      ENDDO  
      IF(MYRANK.EQ.0)THEN
      OPEN(1,FILE='SURFAMP.OUT',STATUS='UNKNOWN')  
      CLOSE(1,STATUS='DELETE')  
      OPEN(1,FILE='SURFAMP.OUT',STATUS='UNKNOWN')  
      TITLE1='SURFACE DISPLACEMENT AMPLITUDE CONTOURS'  
      OPEN(2,FILE='SURFPHA.OUT',STATUS='UNKNOWN')  
      CLOSE(2,STATUS='DELETE')  
      OPEN(2,FILE='SURFPHA.OUT',STATUS='UNKNOWN')  
      TITLE2='SURFACE DISPLACEMENT PHASE CONTOURS'  
      OPEN(3,FILE='MAJAXIS.OUT',STATUS='UNKNOWN')  
      CLOSE(3,STATUS='DELETE')  
      OPEN(3,FILE='MAJAXIS.OUT',STATUS='UNKNOWN')  
      TITLE3='MAJOR AXES OF TIDAL ELLIPSES'  
      OPEN(4,FILE='MAJAPHA.OUT',STATUS='UNKNOWN')  
      CLOSE(4,STATUS='DELETE')  
      OPEN(4,FILE='MAJAPHA.OUT',STATUS='UNKNOWN')  
      TITLE4='TIDAL ELLIPSES PHASE CONTOURS'  
      OPEN(11,FILE='TIDELKC.OUT',STATUS='UNKNOWN')  
      CLOSE(11,STATUS='DELETE')  
      OPEN(11,FILE='TIDELKC.OUT',STATUS='UNKNOWN')  
      TITLE11='SURFACE TIDAL ELLIPSES'  
      OPEN(12,FILE='TIDELKB.OUT',STATUS='UNKNOWN')  
      CLOSE(12,STATUS='DELETE')  
      OPEN(12,FILE='TIDELKB.OUT',STATUS='UNKNOWN')  
      TITLE12='BOTTOM TIDAL ELLIPSES'  
      LINES=LA-1  
      LEVELS=1  
      DBS=0.  
      WRITE (1,99) TITLE1  
      WRITE (1,101)LINES,LEVELS  
      WRITE (1,250)DBS  
      CLOSE(1)  
      WRITE (2,99) TITLE2  
      WRITE (2,101)LINES,LEVELS  
      WRITE (2,250)DBS  
      CLOSE(2)  
      WRITE (11,99) TITLE11  
      WRITE (11,101)LINES,LEVELS  
      WRITE (11,250)DBS  
      CLOSE(11)  
      DBS=99.  
      WRITE (12,99) TITLE12  
      WRITE (12,101)LINES,LEVELS  
      WRITE (12,250)DBS  
      CLOSE(12)  
      LEVELS=2  
      DBS1=0.  
      DBS2=99.  
      WRITE (3,99) TITLE3  
      WRITE (3,101)LINES,LEVELS  
      WRITE (3,250)DBS1,DBS2  
      CLOSE(3)  
      WRITE (4,99) TITLE4  
      WRITE (4,101)LINES,LEVELS  
      WRITE (4,250)DBS1,DBS2  
      CLOSE(4)  
      ENDIF
C  
C **  ACCUMULATE HARMONIC ANALYSIS  
C  
 1000 CONTINUE  
      DO L=2,LA  
        LN=LNC(L)  
        AMCP(L)=P(L)*WC(NHAR)+AMCP(L)  
        AMSP(L)=P(L)*WS(NHAR)+AMSP(L)  
        UTMP1=0.5*(UHDYE(L+1)+UHDYE(L))*HPI(L)/DYP(L)  
        VTMP1=0.5*(VHDXE(LN)+VHDXE(L))*HPI(L)/DXP(L)  
        UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
        VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
        AMCUE(L)=UTMP*WC(NHAR)+AMCUE(L)  
        AMSUE(L)=UTMP*WS(NHAR)+AMSUE(L)  
        AMCVE(L)=VTMP*WC(NHAR)+AMCVE(L)  
        AMSVE(L)=VTMP*WS(NHAR)+AMSVE(L)  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          LN=LNC(L)  
          UTMP1=0.5*(U(L+1,K)+U(L,K))  
          VTMP1=0.5*(V(LN,K)+V(L,K))  
          UTMP=CUE(L)*UTMP1+CVE(L)*VTMP1  
          VTMP=CUN(L)*UTMP1+CVN(L)*VTMP1  
          AMCU(L,K)=UTMP*WC(NHAR)+AMCU(L,K)  
          AMSU(L,K)=UTMP*WS(NHAR)+AMSU(L,K)  
          AMCV(L,K)=VTMP*WC(NHAR)+AMCV(L,K)  
          AMSV(L,K)=VTMP*WS(NHAR)+AMSV(L,K)  
        ENDDO  
      ENDDO  
C  
C **  CHECK FOR END OF ANALYSIS INTERVAL  
C  
      IF(NHAR.LT.NTSPTC2) GOTO 2000  
C  
C **  COMPLETE HARMONIC ANALYSIS  
C  
      DO L=2,LA  
        AMC=AS*AMCP(L)-ACS*AMSP(L)  
        AMS=-ACS*AMCP(L)+AC*AMSP(L)  
        AMCP(L)=AMC  
        AMSP(L)=AMS  
        AMC=AS*AMCUE(L)-ACS*AMSUE(L)  
        AMS=-ACS*AMCUE(L)+AC*AMSUE(L)  
        AMCUE(L)=AMC  
        AMSUE(L)=AMS  
        AMC=AS*AMCVE(L)-ACS*AMSVE(L)  
        AMS=-ACS*AMCVE(L)+AC*AMSVE(L)  
        AMCVE(L)=AMC  
        AMSVE(L)=AMS  
      ENDDO  
      DO K=1,KC  
        DO L=2,LA  
          AMC=AS*AMCU(L,K)-ACS*AMSU(L,K)  
          AMS=-ACS*AMCU(L,K)+AC*AMSU(L,K)  
          AMCU(L,K)=AMC  
          AMSU(L,K)=AMS  
          AMC=AS*AMCV(L,K)-ACS*AMSV(L,K)  
          AMS=-ACS*AMCV(L,K)+AC*AMSV(L,K)  
          AMCV(L,K)=AMC  
          AMSV(L,K)=AMS  
        ENDDO  
      ENDDO  
      NHAR=0  
      IF(MYRANK.EQ.0)THEN  
      OPEN(1,FILE='SURFAMP.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      WRITE (1,100)N  
      OPEN(2,FILE='SURFPHA.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      WRITE (2,100)N  
      DO L=2,LA  
        SSURFAMP=GI*SQRT(AMCP(L)*AMCP(L)+AMSP(L)*AMSP(L))  
        IF(AMCP(L).EQ.0.0.AND.AMSP(L).EQ.0.0)THEN  
          PHI=999999.  
        ELSE  
          PHI=ATAN2(AMSP(L),AMCP(L))  
        ENDIF  
        SSURFPHS=TIDALP*PHI/(3600.*PI2)  
        SSURFPSC=TIDALP*PHI/PI2  
        IF(SSURFPSC.LT.0.0)SSURFPSC=SSURFPSC+TIDALP  
        WRITE(1,200)IL(L),JL(L),DLON(L),DLAT(L),SSURFAMP,SSURFPSC  
        WRITE(2,200)IL(L),JL(L),DLON(L),DLAT(L),SSURFPHS,SSURFPSC  
      ENDDO  
      CLOSE(1)  
      CLOSE(2)  
      OPEN(3,FILE='MAJAXIS.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      WRITE (3,100)N  
      OPEN(4,FILE='MAJAPHA.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      WRITE (4,100)N  
      OPEN(11,FILE='TIDELKC.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      WRITE (11,100)N  
      OPEN(12,FILE='TIDELKB.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      WRITE (12,100)N  
      DO L=2,LA  
        TERM1=AMCU(L,KC)+AMSV(L,KC)  
        TERM2=AMCV(L,KC)-AMSU(L,KC)  
        TERM3=AMCU(L,KC)-AMSV(L,KC)  
        TERM4=AMCV(L,KC)+AMSU(L,KC)  
        RPLUS=0.5*SQRT(TERM1*TERM1+TERM2*TERM2)  
        RMINS=0.5*SQRT(TERM3*TERM3+TERM4*TERM4)  
        IF(TERM1.EQ.0.0.AND.TERM2.EQ.0.0)THEN  
          APLUS=999999.  
        ELSE  
          APLUS=ATAN2(TERM2,TERM1)  
        ENDIF  
        IF(TERM3.EQ.0.0.AND.TERM4.EQ.0.0)THEN  
          AMINS=999999.  
        ELSE  
          AMINS=ATAN2(TERM4,TERM3)  
        ENDIF  
        RRMAJ=RPLUS+RMINS  
        RRMIN=ABS(RPLUS-RMINS)  
        AACCWX=0.5*(APLUS+AMINS)  
        RMAJUKC=RRMAJ*COS(AACCWX)  
        RMAJVKC=RRMAJ*SIN(AACCWX)  
        IF(RMAJUKC.LT.0.0)THEN  
          RMAJUKC=-RMAJUKC  
          RMAJVKC=-RMAJVKC  
        ENDIF  
        PHASEKC=(0.25/PI)*TIDALP*(AMINS-APLUS)/3600.  
        WRITE(11,200)IL(L),JL(L),DLON(L),DLAT(L),AACCWX,RRMAJ,RRMIN  
        TERM1=AMCU(L,1)+AMSV(L,1)  
        TERM2=AMCV(L,1)-AMSU(L,1)  
        TERM3=AMCU(L,1)-AMSV(L,1)  
        TERM4=AMCV(L,1)+AMSU(L,1)  
        RPLUS=0.5*SQRT(TERM1*TERM1+TERM2*TERM2)  
        RMINS=0.5*SQRT(TERM3*TERM3+TERM4*TERM4)  
        IF(TERM1.EQ.0.0.AND.TERM2.EQ.0.0)THEN  
          APLUS=999999.  
        ELSE  
          APLUS=ATAN2(TERM2,TERM1)  
        ENDIF  
        IF(TERM3.EQ.0.0.AND.TERM4.EQ.0.0)THEN  
          AMINS=999999.  
        ELSE  
          AMINS=ATAN2(TERM4,TERM3)  
        ENDIF  
        RRMAJ=RPLUS+RMINS  
        RRMIN=ABS(RPLUS-RMINS)  
        AACCWX=0.5*(APLUS+AMINS)  
        RMAJUKB=RRMAJ*COS(AACCWX)  
        RMAJVKB=RRMAJ*SIN(AACCWX)  
        IF(RMAJUKB.LT.0.0)THEN  
          RMAJUKB=-RMAJUKB  
          RMAJVKB=-RMAJVKB  
        ENDIF  
        PHASEKB=(0.25/PI)*TIDALP*(AMINS-APLUS)/3600.  
        WRITE(3,200)IL(L),JL(L),DLON(L),DLAT(L),RMAJUKC,RMAJVKC,  
     &      RMAJUKB,RMAJVKB  
        WRITE(4,200)IL(L),JL(L),DLON(L),DLAT(L),PHASEKC,PHASEKB  
        WRITE(12,200)IL(L),JL(L),DLON(L),DLAT(L),AACCWX,RRMAJ,RRMIN  
      ENDDO  
      CLOSE(3)  
      CLOSE(4)  
      CLOSE(11)  
      CLOSE(12)  
      ENDIF
 2000 CONTINUE  
      NHAR=NHAR+1  
   99 FORMAT(A80)  
  100 FORMAT(I10)  
  101 FORMAT(2I10)  
  200 FORMAT(2I5,1X,6E13.5)  
  250 FORMAT(12E12.4)  
      RETURN  
      END  

