      SUBROUTINE OUTPUT1  
C  
C CHANGE RECORD  
C  
      USE GLOBAL  
C  
C **  PLOT SURFACE ELEVATION  
C  
      DO L=2,LA  
        PAM(L)=P(L)*GI  
      ENDDO  
      WRITE (7,50) N  
      CALL PPLOT (1)  
   50 FORMAT(1H1,' SURFACE ELEVATION IN METERS AT TIMESTEP  ',I8,//)  
C  
C **  PLOT SURFACE AND BOTTOM SALINITY  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=SAL(L,KK)  
        ENDDO  
        WRITE (7,509) KK,N  
        CALL PPLOT (1)  
      ENDDO  
  509 FORMAT(1H1,' SALINITY, PPT, LAYER',I5,3X,'AT TIME STEP  ',I8,//)  
C  
C **  PLOT SALINITY STRATIFICATION  
C  
      IF(KC.GT.1)THEN  
        DO L=2,LA  
          PAM(L)=SAL(L,1)-SAL(L,KC)  
        ENDDO  
        WRITE (7,549) N  
        CALL PPLOT (1)  
      ENDIF  
  549 FORMAT(1H1,' SALINITY STRATIFICATION, PPT, AT TIME STEP  ',I8,//)  
C  
C **  OUTPUT RESIDUAL TOTAL DEPTH  
C  
      DO L=2,LA  
        PAM(L)=HLPF(L)  
      ENDDO  
      WRITE (7,585)  
      WRITE (7,757)N  
      CALL PPLOT (2)  
  585 FORMAT (1H1,'RESIDUAL TOTAL DEPTH IN METERS',//)  
  757 FORMAT (1H1,'AVERAGED OVER TWO TIDAL CYCLES ENDING AT N=',I8,//)  
C  
C **  OUTPUT EULERIAN RESIDUAL TRANSPORT VELOCITY  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=0.5*(UHLPF(L,KK)+UHLPF(L+1,KK))/HMP(L)  
        ENDDO  
        WRITE (7,58) KK  
        WRITE (7,757)N  
        CALL PPLOT (2)  
      ENDDO  
      DO KK=1,KC,KS  
        DO L=2,LA  
          LN=LNC(L)  
          PAM(L)=0.5*(VHLPF(L,KK)+VHLPF(LN,KK))/HMP(L)  
        ENDDO  
        WRITE (7,59) KK  
        WRITE (7,757)N  
        CALL PPLOT (2)  
      ENDDO  
   58 FORMAT(1H1,' X EULERIAN RESID TRANSPORT VEL, M/S, LAYER',I5,//)  
   59 FORMAT(1H1,' Y EULERIAN RESID TRANSPORT VEL, M/S, LAYER',I5,//)  
C  
C **  OUTPUT VECTOR POTENTIAL TRANSPORT VELOCITY  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=0.5*(UVPT(L,KK)+UVPT(L+1,KK))/HMP(L)  
        ENDDO  
        WRITE (7,1458) KK  
        WRITE (7,757)N  
        CALL PPLOT (2)  
      ENDDO  
      DO KK=1,KC,KS  
        DO L=2,LA  
          LN=LNC(L)  
          PAM(L)=0.5*(VVPT(L,KK)+VVPT(LN,KK))/HMP(L)  
        ENDDO  
        WRITE (7,1459) KK  
        WRITE (7,757)N  
        CALL PPLOT (2)  
      ENDDO  
 1458 FORMAT(1H1,' X VECTOR POTENTIAL TRANSPORT VEL, M/S, LAYER',I5,//)  
 1459 FORMAT(1H1,' Y VECTOR POTENTIAL TRANSPORT VEL, M/S, LAYER',I5,//)  
C  
C **  OUTPUT LAGRANGIAN RESIDUAL TRANSPORT VELOCITIES  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=0.5*(UHLPF(L,KK)+UHLPF(L+1,KK)+UVPT(L,KK)  
     &        +UVPT(L+1,KK))/HMP(L)  
        ENDDO  
        WRITE (7,1558) KK  
        WRITE (7,757)N  
        CALL PPLOT (2)  
      ENDDO  
      DO KK=1,KC,KS  
        DO L=2,LA  
          LN=LNC(L)  
          PAM(L)=0.5*(VHLPF(L,KK)+VHLPF(LN,KK)+VVPT(L,KK)  
     &        +VVPT(LN,KK))/HMP(L)  
        ENDDO  
        WRITE (7,1559) KK  
        WRITE (7,757)N  
        CALL PPLOT (2)  
      ENDDO  
 1558 FORMAT(1H1,' X LAGRANGIAN RESID TRANSPORT VEL, M/S, LAYER',I5,//)  
 1559 FORMAT(1H1,' Y LAGRANGIAN RESID TRANSPORT VEL, M/S, LAYER',I5,//)  
C  
C **  OUTPUT RESIDUAL VOLUMETRIC FLOW ACROSS OPEN BOUNDARIES  
C  
      WRITE (7,60)  
      WRITE (7,757)N  
      WRITE (7,61) QXW,QXE  
      WRITE (7,62) QYS,QYN  
      WRITE (7,1661) QXWVP,QXEVP  
      WRITE (7,1662) QYSVP,QYNVP  
   60 FORMAT (1H1,' RESIDUAL FLOW ACROSS OPEN BOUNDARIES, M3/S',//)  
   61 FORMAT (5X,' QXW=',5X,E12.4,10X,' QXE=',5X,E12.4,//)  
   62 FORMAT (5X,' QYS=',5X,E12.4,10X,' QYN=',5X,E12.4,//)  
 1661 FORMAT (5X,' QXWVP=',5X,E12.4,10X,' QXEVP=',5X,E12.4,//)  
 1662 FORMAT (5X,' QYSVP=',5X,E12.4,10X,' QYNVP=',5X,E12.4,//)  
C  
C **  OUTPUT RESIDUAL BUOYANCY AND STRATIFICATION  
C  
      DO KK=1,KC,KS  
        DO L=2,LA  
          PAM(L)=SALLPF(L,KK)  
        ENDDO  
        WRITE (7,569) KK  
        WRITE (7,757) N  
        CALL PPLOT (1)  
      ENDDO  
      IF(KC.GT.1)THEN  
        DO L=2,LA  
          LN=LNC(L)  
          PAM(L)=SALLPF(L,1)-SALLPF(L,KC)  
        ENDDO  
        WRITE (7,568)  
        WRITE (7,757) N  
        CALL PPLOT (1)  
      ENDIF  
  568 FORMAT(1H1,' RESIDUAL SALINITY STRATIFICATION, PPT ',//)  
  569 FORMAT(1H1,' RESIDUAL SALINITY, PPT, LAYER',I5,//)  
      RETURN  
      END  

