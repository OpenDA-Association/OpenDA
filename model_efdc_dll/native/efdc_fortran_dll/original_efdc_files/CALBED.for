      SUBROUTINE CALBED  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSND CALCULATES NONCOHESIVE SEDIMENT SETTLING,  
C **  DEPOSITION AND RESUSPENSION AND IS CALLED FOR SSEDTOX  
C  
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::K,L,IFLAG,LUTMP,NS,NSB,KK,NX,KBTM1
	REAL::TMPVAL,WDENKGM3,WDENGMM3,TMPVALK
	REAL::TMPVALKP,BETTMP,VOIDCON1,HBEDTMP,TMPVALO
	REAL::TMPVALN,TMPEXP,TMPTOP,TMPBOT,FSTRSE,FDSTRSE
	REAL::FHYDCN,DSTRESET

      REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)::SNDHYDCN  
      IF(.NOT.ALLOCATED(SNDHYDCN))THEN
        ALLOCATE(SNDHYDCN(LCM,KBM))  
        SNDHYDCN=0.0 
	ENDIF
C  
      IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1)THEN  
        HBEDMIN=1.E-4  
        IF(ISTRAN(7).GE.1)THEN  
          HBEDMIN=MAX(HBEDMIN,SNDDMX)  
        END IF  
C  
C ** CONSTANT POROSITY BED  
C  
        IF(IBMECH.EQ.0.AND.IWRSP(1)/=98)THEN  !avoids this if SEDZLJ is active
C  
C ** UPDATE TOP LAYER THICKNESS TO MAINTAIN CONSTANT POROSITY  
C  
          VOIDCON1=BEDPORC/(1.-BEDPORC)  
          DO L=2,LA  
            K=KBT(L)  
            HBEDTMP=(1.+VOIDCON1)*HBED(L,K)/(1.+VDRBED(L,K))  
            TMPVALO=VDRBED(L,K)*HBED(L,K)/(1.+VDRBED(L,K))  
            TMPVALN=VOIDCON1*HBEDTMP/(1.+VOIDCON1)  
            QWBDTOP(L)=DELTI*(TMPVALO-TMPVALN)  
            HBED(L,K)=HBEDTMP  
            QWTRBED(L,K)=QWBDTOP(L)+QGW(L)/DXYP(L)  
          ENDDO  
          DO K=0,KBT(L)-1  
            DO L=2,LA  
              QWTRBED(L,K)=QGW(L)/DXYP(L)  
            ENDDO  
          END DO  
C  
C ** ADD OR REMOVE LAYERS  
C          CALL CALBLAY  
C  
        ENDIF  
C  
C ** SIMPLE CONSOLIDATING BED  
C  
        IF(IBMECH.EQ.1)THEN  
C  
C ** DETERMINE TIME DIFFERENCE AND UPDATE VOID RATIO  
C BEGIN JMH FIXED IBMECH.EQ.1  OPTION 12/30/02  
C **  IF SEDVRDT.GT.0.0001 CONSOLIDATE TO SEDVRM (THE MINIMUM VOID RATIO  
C  
          IF(SEDVRDT.GT.0.0001)THEN  
            TMPEXP=EXP(-DELT/SEDVRDT)  
            DO K=1,KB  
              DO L=2,LA  
                IF(K.LE.KBT(L))THEN  
                  VDRBED1(L,K)=VDRBED(L,K)  
                  HBED1(L,K)=HBED(L,K)  
                  VDRBED(L,K)=SEDVDRM+(VDRBED1(L,K)-SEDVDRM)*TMPEXP  
                  TMPTOP=1.+VDRBED(L,K)  
                  TMPBOT=1.+VDRBED1(L,K)  
                  HBED(L,K)=TMPTOP*HBED1(L,K)/TMPBOT  
                ENDIF  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C **  IF SEDVRDT.GT.0.0001 CONSOLIDATE TO SEDVRM INSTANTANEOUSLY  
C  
          IF(SEDVRDT.GE.0.0.AND.SEDVRDT.LE.0.0001)THEN  
            TMPEXP=0.0  
            DO K=1,KB  
              DO L=2,LA  
                IF(K.LE.KBT(L))THEN  
                  VDRBED1(L,K)=VDRBED(L,K)  
                  HBED1(L,K)=HBED(L,K)  
                  VDRBED(L,K)=SEDVDRM  
                  TMPTOP=1.+VDRBED(L,K)  
                  TMPBOT=1.+VDRBED1(L,K)  
                  HBED(L,K)=TMPTOP*HBED1(L,K)/TMPBOT  
                ENDIF  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C **  IF SEDVRDT.LT.0.0 MAINTAIN INITIAL VOID RATIO (SAVED IN VDRBED2)  
C  
          IF(SEDVRDT.LT.0.0) THEN  
            TMPEXP=1.0  
            DO L=2,LA  
              K=KBT(L)  
              VDRBED1(L,K)=VDRBED(L,K)  
              HBED1(L,K)=HBED(L,K)  
              VDRBED(L,K)=VDRBED2(L,K)  
              TMPTOP=1.+VDRBED(L,K)  
              TMPBOT=1.+VDRBED1(L,K)  
              HBED(L,K)=TMPTOP*HBED1(L,K)/TMPBOT  
            ENDDO  
            DO L=2,LA  
              K=KBT(L)-1  
              IF(K.GT.0)THEN  
                VDRBED1(L,K)=VDRBED(L,K)  
                HBED1(L,K)=HBED(L,K)  
                VDRBED(L,K)=VDRBED2(L,K)  
                TMPTOP=1.+VDRBED(L,K)  
                TMPBOT=1.+VDRBED1(L,K)  
                HBED(L,K)=TMPTOP*HBED1(L,K)/TMPBOT  
              ENDIF  
            ENDDO  
          ENDIF  
C  
C ** UPDATE POROSITY  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                PORBED(L,K)=VDRBED(L,K)/(1.+VDRBED(L,K))  
                PORBED1(L,K)=VDRBED1(L,K)/(1.+VDRBED1(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ** UPDATE PORE WATER FLOWS  
C  
          DO L=2,LA  
            QWTRBED(L,0)=QGW(L)/DXYP(L)  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                TMPVAL=HBED(L,K)/(1.+VDRBED(L,K))  
                QWTRBED(L,K)=QWTRBED(L,K-1)  
     &              -DELTI*TMPVAL*(VDRBED(L,K)-VDRBED1(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ** ADD OR REMOVE LAYERS  
C          CALL CALBLAY  
C  
        ENDIF  
C  
C ** SET FLAG FOR FINITE STRAIN CONSOLIDATION  
C  
        IFLAG=0  
        IF(ISTRAN(6).GE.1)IFLAG=IFLAG+1  
        IF(ISTRAN(7).GE.1)IFLAG=IFLAG+1  
C  
C ** FINITE STRAIN CONSOLIDATING HOMOGENEOUS BED  
C  
        IF(IBMECH.GE.2.AND.IFLAG.EQ.1)THEN  
          WDENKGM3=1.E3  
          WDENGMM3=1.E6  
C  
C ++  SET PHYSICAL VERTICAL COORDINATES OF THE BED  
C  
          DO L=2,LA  
            ZBEDG(L,0)=ZELBEDA(L)  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDG(L,K)=ZBEDG(L,K-1)+HBED(L,K)  
              ELSE  
                ZBEDG(L,K)=HBED(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            ZBEDGT(L)=ZBEDG(L,KBT(L))  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDC(L,K)=0.5*(ZBEDG(L,K)+ZBEDG(L,K-1))  
              ELSE  
                ZBEDC(L,K)=0.5*ZBEDG(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  CALCULATE TRANSFORMED THICKNESS OF BED LAYERS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                DZBTR(L,K)=HBED(L,K)/(1.+VDRBED(L,K))  
                DZBTR1(L,K)=HBED1(L,K)/(1.+VDRBED1(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  CALCULATE WATER SPECIFIC WEIGHT NORMALIZED  
C       EFFECTIVE STRESS USING FSTRSE  
C     CALCULATE DERIVATIVE OF EFFECTIVE STRESS  
C       WITH RESPECT TO VOID RATIO, DSTRSE USING  
C       FUNCTION FDSTRSE  
C     CALCULATE HYDRAULIC CONDUCTIVITY DIVIED BY (1+VOID),  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                STRSE(L,K)=  
     &              FSTRSE(VDRBED(L,K),BMECH1,BMECH2,BMECH3)  
                DSTRSE(L,K)=  
     &              FDSTRSE(VDRBED(L,K),BMECH1,BMECH2,BMECH3)  
                HYDCN(L,K)=  
     &              FHYDCN(VDRBED(L,K),BMECH4,BMECH5,BMECH6,IBMECHK)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              KBTM1=KBT(L)-1  
              IF(K.LE.KBTM1)THEN  
                TMPVAL=( DZBTR(L,K)/HYDCN(L,K) )  
     &              +( DZBTR(L,K+1)/HYDCN(L,K+1) )  
                COEFK(L,K)=(DZBTR(L,K)+DZBTR(L,K+1))/TMPVAL  
                DSTRESET=(DZBTR(L,K)*DSTRSE(L,K+1)  
     &              +DZBTR(L,K+1)*DSTRSE(L,K))  
     &              /(DZBTR(L,K)+DZBTR(L,K+1))  
                COEFSK(L,K)=DSTRESET*COEFK(L,K)  
              ENDIF  
              IF(K.EQ.KBT(L))THEN  
                COEFK(L,K)=HYDCN(L,K)  
                COEFSK(L,K)=DSTRSE(L,K)*HYDCN(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  CALCULATE PRESSURE COMPONENTS  
C  
          DO K=1,KB  
            DO L=2,LA  
              SGSM1(L,K)=0.  
            ENDDO  
          ENDDO  
          IF(ISTRAN(6).GT.0)THEN  
            DO NS=1,NSED  
              DO K=1,KB  
                DO L=2,LA  
                  IF(K.LE.KBT(L))THEN  
                    SGSM1(L,K)=SGSM1(L,K)+SSG(NS)*VFRBED(L,K,NS)  
                  ENDIF  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(ISTRAN(7).GT.0)THEN  
            DO NX=1,NSND  
              NS=NSED+NX  
              DO K=1,KB  
                DO L=2,LA  
                  IF(K.LE.KBT(L))THEN  
                    SGSM1(L,K)=SGSM1(L,K)+SSG(NS)*VFRBED(L,K,NS)  
                  ENDIF  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                SGSM1(L,K)=SGSM1(L,K)-1.  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  NEW IMPLICIT CONSOLIDATION SOLUTION BEGINS HERE  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LT.KBT(L))THEN  
                ACOEF(L,K)=2.*COEFSK(L,K)/(DZBTR(L,K)+DZBTR(L,K+1))  
                TMPVALK=DZBTR(L,K)*SGSM1(L,K)  
                TMPVALKP=DZBTR(L,K+1)*SGSM1(L,K+1)  
                QCOEF(L,K)=(TMPVALK+TMPVALKP)*COEFK(L,K)/  
     &              (DZBTR(L,K)+DZBTR(L,K+1))  
              ELSE  
                ACOEF(L,K)=0.0  
                QCOEF(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            K=KBT(L)  
            ACOEF(L,K)=2.*COEFSK(L,K)/DZBTR(L,K)  
            QCOEF(L,K)=SGSM1(L,K)*COEFK(L,K)  
            QCOEF(L,0)=QGW(L)*DXYIP(L)  
            QWTRBED(L,0)=QGW(L)*DXYIP(L)  
          ENDDO  
          DO L=2,LA  
            ALOW(L,1)=0.  
            CUPP(L,KBT(L))=0.  
            DO K=1,KBT(L)-1  
              CUPP(L,K)=-DELT*ACOEF(L,K)/DZBTR(L,K)  
            ENDDO  
            DO K=2,KBT(L)  
              ALOW(L,K)=-DELT*ACOEF(L,K-1)/DZBTR(L,K)  
            ENDDO  
            DO K=1,KBT(L)  
              BMNN(L,K)=1.0-ALOW(L,K)-CUPP(L,K)  
            ENDDO  
            K=KBT(L)  
            BMNN(L,K)=BMNN(L,K)+DELT*ACOEF(L,K)/DZBTR(L,K)  
            DO K=1,KBT(L)  
              RRHS(L,K)=VDRBED(L,K)  
     &            +DELT*(QCOEF(L,K-1)-QCOEF(L,K))/DZBTR(L,K)  
              VDRBED1(L,K)=VDRBED(L,K)  
              HBED1(L,K)=HBED(L,K)  
            ENDDO  
          ENDDO  
          IF(IBMECH.EQ.2) THEN  
            DO L=2,LA  
              K=KBT(L)  
              RRHS(L,K)=RRHS(L,K)+DELT*ACOEF(L,K)*VDRDEPO(1)/DZBTR(L,K)  
            ENDDO  
          ENDIF  
          IF(IBMECH.EQ.3) THEN  
            DO L=2,LA  
              K=KBT(L)  
              RRHS(L,K)=RRHS(L,K)+DELT*ACOEF(L,K)*(VDRBED(L,K)  
     &            +(STRSE(L,K)/DSTRSE(L,K)))/DZBTR(L,K)  
            ENDDO  
          ENDIF  
          DO L=2,LA  
            BETTMP=BMNN(L,1)  
            TOXTMP(L,1)=RRHS(L,1)/BETTMP  
            DO KK=2,KBT(L)  
              GAMTMP(L,KK)=CUPP(L,KK-1)/BETTMP  
              BETTMP=BMNN(L,KK)-ALOW(L,KK)*GAMTMP(L,KK)  
              TOXTMP(L,KK)=(RRHS(L,KK)-ALOW(L,KK)*TOXTMP(L,KK-1))/  
     &            BETTMP  
            ENDDO  
            DO KK=KBT(L)-1,1,-1  
              TOXTMP(L,KK)=TOXTMP(L,KK)-GAMTMP(L,KK+1)*TOXTMP(L,KK+1)  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LT.KBT(L))THEN  
                QWTRBED(L,K)=-ACOEF(L,K)*(TOXTMP(L,K+1)-TOXTMP(L,K))  
     &              +QCOEF(L,K)  
              ELSE  
                QWTRBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
          IF(IBMECH.EQ.2) THEN  
            DO L=2,LA  
              K=KBT(L)  
              QWTRBED(L,K)=-ACOEF(L,K)*(SEDVDRD-TOXTMP(L,K))+QCOEF(L,K)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              K=KBT(L)  
              QWTRBED(L,K)=-ACOEF(L,K)*(VDRBED(L,K)  
     &            +(STRSE(L,K)/DSTRSE(L,K))-TOXTMP(L,K))+QCOEF(L,K)  
            ENDDO  
          END IF  
C  
C ++  CALCULATE VOID RATIOS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                VDRBED(L,K)=VDRBED1(L,K)  
     &              -DELT*(QWTRBED(L,K)-QWTRBED(L,K-1))/DZBTR1(L,K)  
              ELSE  
                VDRBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  UPDATE LAYER THICKNESS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                HBED(L,K)=HBED1(L,K)*(1.+VDRBED(L,K))/(1.+VDRBED1(L,K))  
              ELSE  
                HBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            QWTRBED(L,0)=QGW(L)/DXYP(L)  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                QWTRBED(L,K)=QWTRBED(L,K-1)  
     &              -DELTI*(HBED(L,K)-HBED1(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDG(L,K)=ZBEDG(L,K-1)+HBED(L,K)  
              ELSE  
                ZBEDG(L,K)=HBED(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            ZBEDGT(L)=ZBEDG(L,KBT(L))  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDC(L,K)=0.5*(ZBEDG(L,K)+ZBEDG(L,K-1))  
              ELSE  
                ZBEDC(L,K)=0.5*ZBEDG(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                DZBTR(L,K)=HBED(L,K)/(1.+VDRBED(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ** UPDATE POROSITY  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                PORBED(L,K)=VDRBED(L,K)/(1.+VDRBED(L,K))  
                PORBED1(L,K)=VDRBED1(L,K)/(1.+VDRBED1(L,K))  
              ELSE  
                PORBED(L,K)=0.0  
                PORBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ** ADD OR REMOVE LAYERS  
C          CALL CALBLAY  
C  
        ENDIF  
C  
C ** FINITE STRAIN CONSOLIDATING NON-HOMOGENEOUS BED  
C  
        IF(IBMECH.GE.2.AND.IFLAG.EQ.2)THEN  
          WDENKGM3=1.E3  
          WDENGMM3=1.E6  
C  
C ++  SET PHYSICAL VERTICAL COORDINATES OF THE BED  
C  
          DO L=2,LA  
            ZBEDG(L,0)=ZELBEDA(L)  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDG(L,K)=ZBEDG(L,K-1)+HBED(L,K)  
              ELSE  
                ZBEDG(L,K)=HBED(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            ZBEDGT(L)=ZBEDG(L,KBT(L))  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDC(L,K)=0.5*(ZBEDG(L,K)+ZBEDG(L,K-1))  
              ELSE  
                ZBEDC(L,K)=0.5*ZBEDG(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  CALCULATE TRANSFORMED THICKNESS OF BED LAYERS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                DZBTR(L,K)=HBED(L,K)/(1.+VDRBED(L,K))  
                DZBTR1(L,K)=HBED1(L,K)/(1.+VDRBED1(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  CALCULATE WATER SPECIFIC WEIGHT NORMALIZED  
C       EFFECTIVE STRESS USING FSTRSE  
C     CALCULATE DERIVATIVE OF EFFECTIVE STRESS  
C       WITH RESPECT TO VOID RATIO, DSTRSE USING  
C       FUNCTION FDSTRSE  
C     CALCULATE HYDRAULIC CONDUCTIVITY DIVIED BY (1+VOID),  
C ++  NONCOHESIVE HYDRAULIC CONDUCTIVITY BASED ON R. R. RUMER, CHAP 3,  
C ++  EQ 13 AND 14, IN 'FLOW THROUGH POROUS MEDIA' ED. R. J. M. DE WIEST  
C ++  ACADEMIC PRESS, 1969  
C     FOR KH IN M/S AND D IN METERS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                STRSE(L,K)=  
     &              FSTRSE(VDRBEDSED(L,K),BMECH1,BMECH2,BMECH3)  
                DSTRSE(L,K)=  
     &              FDSTRSE(VDRBEDSED(L,K),BMECH1,BMECH2,BMECH3)  
                HYDCN(L,K)=  
     &              FHYDCN(VDRBEDSED(L,K),BMECH4,BMECH5,BMECH6,IBMECHK)  
                TMPVAL=VDRBEDSND(L,K)/(1.+VDRBEDSND(L,K))  
                SNDHYDCN(L,K)=2854.0*TMPVAL*TMPVAL*(SEDDIA50(L,K)**2)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                COEFK(L,K)=(FRACCOH(L,K)/HYDCN(L,K))  
     &              +(FRACNON(L,K)/SNDHYDCN(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                HYDCN(L,K)=1./COEFK(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              COEFK(L,K)=0.  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              KBTM1=KBT(L)-1  
              IF(K.LE.KBTM1)THEN  
                TMPVAL=( DZBTR(L,K)/HYDCN(L,K) )  
     &              +( DZBTR(L,K+1)/HYDCN(L,K+1) )  
                COEFK(L,K)=(DZBTR(L,K)+DZBTR(L,K+1))/TMPVAL  
                DSTRESET=(DZBTR(L,K)*DSTRSE(L,K+1)  
     &              +DZBTR(L,K+1)*DSTRSE(L,K))  
     &              /(DZBTR(L,K)+DZBTR(L,K+1))  
                COEFSK(L,K)=DSTRESET*COEFK(L,K)  
              ENDIF  
              IF(K.EQ.KBT(L))THEN  
                COEFK(L,K)=HYDCN(L,K)  
                COEFSK(L,K)=DSTRSE(L,K)*HYDCN(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  CALCULATE PRESSURE COMPONENTS  
C  
          DO K=1,KB  
            DO L=2,LA  
              SGSM1(L,K)=0.  
            ENDDO  
          ENDDO  
          IF(ISTRAN(6).GT.0)THEN  
            DO NS=1,NSED  
              DO K=1,KB  
                DO L=2,LA  
                  IF(K.LE.KBT(L))THEN  
                    SGSM1(L,K)=SGSM1(L,K)+SSG(NS)*VFRBED(L,K,NS)  
                  ENDIF  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(ISTRAN(7).GT.0)THEN  
            DO NX=1,NSND  
              NS=NSED+NX  
              DO K=1,KB  
                DO L=2,LA  
                  IF(K.LE.KBT(L))THEN  
                    SGSM1(L,K)=SGSM1(L,K)+SSG(NS)*VFRBED(L,K,NS)  
                  ENDIF  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                SGSM1(L,K)=SGSM1(L,K)-1.  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  NEW IMPLICIT CONSOLIDATION SOLUTION BEGINS HERE  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LT.KBT(L))THEN  
                ACOEF(L,K)=2.*COEFSK(L,K)/(DZBTR(L,K)+DZBTR(L,K+1))  
                TMPVALK=DZBTR(L,K)*SGSM1(L,K)  
                TMPVALKP=DZBTR(L,K+1)*SGSM1(L,K+1)  
                QCOEF(L,K)=(TMPVALK+TMPVALKP)*COEFK(L,K)/  
     &              (DZBTR(L,K)+DZBTR(L,K+1))  
              ELSE  
                ACOEF(L,K)=0.0  
                QCOEF(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            K=KBT(L)  
            ACOEF(L,K)=2.*COEFSK(L,K)/DZBTR(L,K)  
            QCOEF(L,K)=SGSM1(L,K)*COEFK(L,K)  
            QCOEF(L,0)=QGW(L)*DXYIP(L)  
            QWTRBED(L,0)=QGW(L)*DXYIP(L)  
          ENDDO  
          DO L=2,LA  
            ALOW(L,1)=0.  
            CUPP(L,KBT(L))=0.  
            DO K=1,KBT(L)-1  
              CUPP(L,K)=-DELT*ACOEF(L,K)/DZBTR(L,K)  
            ENDDO  
            DO K=2,KBT(L)  
              ALOW(L,K)=-DELT*ACOEF(L,K-1)/DZBTR(L,K)  
            ENDDO  
            DO K=1,KBT(L)  
              BMNN(L,K)=FRACCOH(L,K)-ALOW(L,K)-CUPP(L,K)  
            ENDDO  
            K=KBT(L)  
            BMNN(L,K)=BMNN(L,K)+DELT*ACOEF(L,K)/DZBTR(L,K)  
            DO K=1,KBT(L)  
              RRHS(L,K)=FRACCOH(L,K)*VDRBEDSED(L,K)  
     &            +DELT*(QCOEF(L,K-1)-QCOEF(L,K))/DZBTR(L,K)  
              VDRBED1(L,K)=VDRBED(L,K)  
              HBED1(L,K)=HBED(L,K)  
            ENDDO  
          ENDDO  
          IF(IBMECH.EQ.2) THEN  
            DO L=2,LA  
              K=KBT(L)  
              RRHS(L,K)=RRHS(L,K)+DELT*ACOEF(L,K)*VDRDEPO(1)/DZBTR(L,K)  
            ENDDO  
          ENDIF  
          IF(IBMECH.EQ.3) THEN  
            DO L=2,LA  
              K=KBT(L)  
              RRHS(L,K)=RRHS(L,K)  
     &            +DELT*ACOEF(L,K)*(VDRBEDSED(L,K)  
     &            +(STRSE(L,K)/DSTRSE(L,K)))/DZBTR(L,K)  
            ENDDO  
          ENDIF  
          DO L=2,LA  
            BETTMP=BMNN(L,1)  
            TOXTMP(L,1)=RRHS(L,1)/BETTMP  
            DO KK=2,KBT(L)  
              GAMTMP(L,KK)=CUPP(L,KK-1)/BETTMP  
              BETTMP=BMNN(L,KK)-ALOW(L,KK)*GAMTMP(L,KK)  
              TOXTMP(L,KK)=(RRHS(L,KK)-ALOW(L,KK)*TOXTMP(L,KK-1))/  
     &            BETTMP  
            ENDDO  
            DO KK=KBT(L)-1,1,-1  
              TOXTMP(L,KK)=TOXTMP(L,KK)-GAMTMP(L,KK+1)*TOXTMP(L,KK+1)  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LT.KBT(L))THEN  
                QWTRBED(L,K)=-ACOEF(L,K)*(TOXTMP(L,K+1)-TOXTMP(L,K))  
     &              +QCOEF(L,K)  
              ELSE  
                QWTRBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
          IF(IBMECH.EQ.2) THEN  
            DO L=2,LA  
              K=KBT(L)  
              QWTRBED(L,K)=-ACOEF(L,K)*(VDRDEPO(1)-TOXTMP(L,K))  
     &            +QCOEF(L,K)  
            ENDDO  
          ELSE  
            DO L=2,LA  
              K=KBT(L)  
              QWTRBED(L,K)=-ACOEF(L,K)*(VDRBEDSED(L,K)  
     &            +(STRSE(L,K)/DSTRSE(L,K))-TOXTMP(L,K))+QCOEF(L,K)  
            ENDDO  
          END IF  
C  
C ++  CALCULATE VOID RATIOS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                VDRBEDSED(L,K)=VDRBEDSED(L,K)  
     &              -DELT*(QWTRBED(L,K)-QWTRBED(L,K-1))  
     &              /(FRACCOH(L,K)*DZBTR1(L,K))  
              ELSE  
                VDRBEDSED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  UPDATE VOID RATIO  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                VDRBED(L,K)=FRACCOH(L,K)*VDRBEDSED(L,K)  
     &              +FRACNON(L,K)*VDRBEDSND(L,K)  
              ELSE  
                VDRBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ++  UPDATE LAYER THICKNESS  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                HBED(L,K)=HBED1(L,K)*(1.+VDRBED(L,K))/(1.+VDRBED1(L,K))  
              ELSE  
                HBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            QWTRBED(L,0)=QGW(L)/DXYP(L)  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                QWTRBED(L,K)=QWTRBED(L,K-1)  
     &              -DELTI*(HBED(L,K)-HBED1(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDG(L,K)=ZBEDG(L,K-1)+HBED(L,K)  
              ELSE  
                ZBEDG(L,K)=HBED(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            ZBEDGT(L)=ZBEDG(L,KBT(L))  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                ZBEDC(L,K)=0.5*(ZBEDG(L,K)+ZBEDG(L,K-1))  
              ELSE  
                ZBEDC(L,K)=0.5*ZBEDG(L,K)  
              ENDIF  
            ENDDO  
          ENDDO  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                DZBTR(L,K)=HBED(L,K)/(1.+VDRBED(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ** UPDATE POROSITY  
C  
          DO K=1,KB  
            DO L=2,LA  
              IF(K.LE.KBT(L))THEN  
                PORBED(L,K)=VDRBED(L,K)/(1.+VDRBED(L,K))  
                PORBED1(L,K)=VDRBED1(L,K)/(1.+VDRBED1(L,K))  
              ELSE  
                PORBED(L,K)=0.0  
                PORBED(L,K)=0.0  
              ENDIF  
            ENDDO  
          ENDDO  
C  
C ** ADD OR REMOVE LAYERS  
C          CALL CALBLAY  
C  
        ENDIF  
      ENDIF  
C  
C *** PMC BEGIN BLOCK  
C  
      ! *** WASTE DEPOSITED DS SEDIMENT TO PREVENT HEAD CONTROLS BEING E  
      IF(.TRUE.)THEN  
        IF(NSBDLDBC.GT.0) THEN  
          DO NSB=1,NSBDLDBC  
            LUTMP=LSBLBCU(NSB)  
            IF(LUTMP.GT.0) THEN  
              DO K=1,KBT(LUTMP)  
                HBED(LUTMP,K)=BEDTHKSV(LUTMP,K)  
                PORBED(LUTMP,K)=BEDPORSV(LUTMP,K)  
                VDRBED(LUTMP,K)=BEDVDRSV(LUTMP,K)  
                BDENBED(LUTMP,K)=BEDBKDSV(LUTMP,K)  
                IF(ISTRAN(6).GT.0)THEN  
                  DO NS=1,NSED  
                    SEDB(LUTMP,K,NS)=SEDB1(LUTMP,K,NS)  
                  ENDDO  
                ENDIF  
                IF(ISTRAN(7).GT.0)THEN  
                  DO NS=1,NSND  
                    SNDB(LUTMP,K,NS)=SNDB1(LUTMP,K,NS)  
                  ENDDO  
                ENDIF  
              ENDDO  
            ENDIF  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C *** PMC END BLOCK  
C  
      RETURN  
      END  

