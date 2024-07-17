      SUBROUTINE CALTOX  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSND CALCULATES NONCOHESIVER SEDIMENT SETTLING,  
C **  DEPOSITION AND RESUSPENSION AND IS CALLED FOR SSEDTOX  
C  
      USE GLOBAL  

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TOXFPA  
      IF(.NOT.ALLOCATED(TOXFPA))THEN
        ALLOCATE(TOXFPA(LCM))
        TOXFPA=0.0 
      ENDIF

C
C **  UPDATE FRACTION OF PARTICULATE ORGANIC CARBON IN BED  
C  
      IVAL=0  
      DO NT=1,NTOX  
        IF(ISTOC(NT).GE.2)IVAL=1  
      ENDDO  
      IF(IVAL.EQ.1.AND.ISTPOCB.EQ.4)THEN  
        CALL SETFPOCB(1)  
      ENDIF  
C  
C **  CALCULATE TOXIC CONTAMINANT PARTICULATE FRACTIONS  
C **  IN WATER COLUMN  
C **  TOXPFW(L,K,NS,NT) = PARTICULATE FRACTION IN WATER COLUMN  
C **  TOXPARW(NS,NT) = PARTITION COEFFICIENT IN WATER COLUMN  
C **  TOXPFTW(L,K,NT) = TOTAL PARTICULATE FRACTION IN WATER COLUMN  
C                       USED AS TEMPORARY VARIBLE IN THIS AND  
C                       FOLLOWING CODE BLOCK  
C  
      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          NSP2(NT)=NSED+NSND  
          IF(ISTOC(NT).EQ.1) NSP2(NT)=NSP2(NT)+2  
          IF(ISTOC(NT).EQ.2) NSP2(NT)=NSP2(NT)+1  
        END DO  
        DO NT=1,NTOX  
          DO NS=1,NSP2(NT)  
            DO K=1,KC  
              DO L=2,LA  
                TOXPFW(L,K,NS,NT)=0.  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          IF(ISTRAN(6).GE.1)THEN  
C  
C PARTITION TO COHESIVE  
C  
            DO NS=1,NSED  
              IF(ITXPARW(NS,NT).EQ.0)THEN  
                DO K=1,KC  
                  DO L=2,LA  
                    TOXPFW(L,K,NS,NT)=SED(L,K,NS)*STFPOCW(L,K,NS)  
     &                  *TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
              IF(ITXPARW(NS,NT).EQ.1)THEN  
                TMPEXP=CONPARW(NS,NT)  
                DO K=1,KC  
                  DO L=2,LA  
                    TMPVAL=1.  
                    IF(SED(L,K,NS).GT.0.) TMPVAL=SED(L,K,NS)**TMPEXP  
                    TOXPFW(L,K,NS,NT)=TMPVAL*SED(L,K,NS)  
     &                  *STFPOCW(L,K,NS)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
!{GeoSR, YSSONG, TOXIC, 101031
              IF(ITXPARW(NS,NT).EQ.-1)THEN  
                DO K=1,KC  
                  DO L=2,LA  
                    TOXPFW(L,K,NS,NT)=SED(L,K,NS)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
!} GeoSR
            ENDDO  
          ENDIF  
C  
C PARTITION TO NONCOHESIVE  
C  
          IF(ISTRAN(7).GE.1)THEN  
            DO NX=1,NSND  
              NS=NX+NSED  
              IF(ITXPARW(NS,NT).EQ.0)THEN  
                DO K=1,KC  
                  DO L=2,LA  
                    TOXPFW(L,K,NS,NT)=SND(L,K,NX)*STFPOCW(L,K,NS)  
     &                  *TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
              IF(ITXPARW(NS,NT).EQ.1)THEN  
                TMPEXP=CONPARW(NS,NT)  
                DO K=1,KC  
                  DO L=2,LA  
                    TMPVAL=1.  
                    IF(SND(L,K,NX).GT.0.) TMPVAL=SND(L,K,NX)**TMPEXP  
                    TOXPFW(L,K,NS,NT)=TMPVAL*SND(L,K,NX)  
     &                  *STFPOCW(L,K,NS)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
!{GeoSR, YSSONG, TOXIC, 101031
              IF(ITXPARW(NS,NT).EQ.-1)THEN  
                DO K=1,KC  
                  DO L=2,LA  
                    TOXPFW(L,K,NS,NT)=SND(L,K,NS)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
!} GeoSR
            ENDDO  
          ENDIF  
C  
C PARTITION (COMPLEX TO DISSOLVED ORGANIC CARBON)  
C  
          IF(ISTOC(NT).EQ.1.OR.ISTOC(NT).EQ.2)THEN  
            NS=1+NSED+NSND  
            IF(ITXPARWC(1,NT).EQ.0)THEN  
              DO K=1,KC  
                DO L=2,LA  
                  TOXPFW(L,K,NS,NT)=STDOCW(L,K)*TOXPARWC(1,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
            IF(ITXPARWC(1,NT).EQ.1)THEN  
              TMPEXP=CONPARWC(1,NT)  
              DO K=1,KC  
                DO L=2,LA  
                  TMPVAL=1.  
                  IF(STDOCW(L,K).GT.0.) TMPVAL=STDOCW(L,K)**TMPEXP  
                  TOXPFW(L,K,NS,NT)=TMPVAL*STDOCW(L,K)*TOXPARWC(1,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDIF  
C  
C PARTITION TO PARTICULATE ORGANIC CARBON  
C  
          IF(ISTOC(NT).EQ.1)THEN  
            NS=2+NSED+NSND  
            IF(ITXPARWC(2,NT).EQ.0)THEN  
              DO K=1,KC  
                DO L=2,LA  
                  TOXPFW(L,K,NS,NT)=STPOCW(L,K)*TOXPARWC(2,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
!{GeoSR, YSSONG, TOXIC, 101031
            IF(ITXPARW(2,NT).EQ.1)THEN  
C            IF(ITXPARW(NS,NT).EQ.1)THEN  
!} GeoSR
              TMPEXP=CONPARW(NS,NT)  
              DO K=1,KC  
                DO L=2,LA  
                  TMPVAL=1.  
                  IF(STPOCW(L,K).GT.0.) TMPVAL=STPOCW(L,K)**TMPEXP  
                  TOXPFW(L,K,NS,NT)=TMPVAL*STPOCW(L,K)*TOXPARWC(2,NT)  
                ENDDO  
              ENDDO  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C !!  TOXPFTW IS TEMPORARILY USED TO STORE TOTAL SORBED  
C  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXPFTW(L,K,NT)=0.  
            ENDDO  
          ENDDO  
          DO NS=1,NSP2(NT)  
            DO K=1,KC  
              DO L=2,LA  
                TOXPFTW(L,K,NT)=TOXPFTW(L,K,NT)+TOXPFW(L,K,NS,NT)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSP2(NT)  
            DO K=1,KC  
              DO L=2,LA  
                TOXPFW(L,K,NS,NT)=TOXPFW(L,K,NS,NT)/(1.+TOXPFTW(L,K,NT))  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        NFD=NSED+NSND+1  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXFDFW(L,K,NT)=1./(1.+TOXPFTW(L,K,NT))  
              TOXCDFW(L,K,NT)=TOXPFW(L,K,NFD,NT)  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  
C1907 FORMAT(2I6,10E13.4)  
C  
C **  CALCULATE TOXIC CONTAMINANT PARTICULATE FRACTIONS  
C **  IN SEDIMENT BED  
C **  TOXPFB(L,NS,NT) = PARTICULATE FRACTION IN SEDIMENT BED  
C **  TOXPARB(NS,NT) = PARTITION COEFFICIENT IN SEDIMENT BED  
C **  TOXPFTB(L,NT) = TOTAL PARTICULATE FRACTION IN SEDIMENT BED  
C                       USED AS TEMPORARY VARIBLE IN THIS AND  
C                       FOLLOWING CODE BLOCK  
C  

!{ GeoSR, YSSONG, TOXIC, 101122
      IF(ISTOXB.EQ.1)THEN
!} GeoSR
      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          NSP2(NT)=NSED+NSND  
          IF(ISTOC(NT).EQ.1) NSP2(NT)=NSP2(NT)+2  
          IF(ISTOC(NT).EQ.2) NSP2(NT)=NSP2(NT)+1  
        END DO  
        DO NT=1,NTOX  
          DO NS=1,NSP2(NT)  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFB(L,K,NS,NT)=0.  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
C  
C PARTITION TO COHESIVES  
C  
          IF(ISTRAN(6).GE.1)THEN  
            DO NS=1,NSED  
              DO K=1,KB  
                DO L=2,LA  
                  TOXPFB(L,K,NS,NT)=SEDB(L,K,NS)*STFPOCB(L,K,NS)  
     &                *TOXPARB(NS,NT)  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C PARTITION TO NONCOHESIVES  
C  
          IF(ISTRAN(7).GE.1)THEN  
            DO NX=1,NSND  
              NS=NX+NSED  
              DO K=1,KB  
                DO L=2,LA  
                  TOXPFB(L,K,NS,NT)=SNDB(L,K,NX)*STFPOCB(L,K,NS)  
     &                *TOXPARB(NS,NT)  
                ENDDO  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C PARTITION (COMPLEX) TO DOC  
C  
          IF(ISTOC(NT).EQ.1.OR.ISTOC(NT).EQ.2)THEN  
            NS=1+NSED+NSND  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFB(L,K,NS,NT)=PORBED(L,K)*HBED(L,K)  
     &              *STDOCB(L,K)*TOXPARBC(1,NT)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
C PARTITION TO POC  
C  
          IF(ISTOC(NT).EQ.1)THEN  
            NS=2+NSED+NSND  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFB(L,K,NS,NT)=HBED(L,K)  
     &              *STPOCB(L,K)*TOXPARBC(2,NT)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDDO  
C  
C !!  TOXPFTB IS TEMPORARILY USED TO STORE TOTAL SORBED  
C  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              TOXPFTB(L,K,NT)=0.  
            ENDDO  
          ENDDO  
          DO NS=1,NSP2(NT)  
            DO K=1,KB  
              DO L=2,LA  
                TOXPFTB(L,K,NT)=TOXPFTB(L,K,NT)+TOXPFB(L,K,NS,NT)  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO NS=1,NSP2(NT)  
            DO K=1,KB  
              DO L=2,LA  
                IF(HBED(L,K).GT.0.0)THEN  
                  TOXPFB(L,K,NS,NT)=TOXPFB(L,K,NS,NT)/  
     &                (PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT))  
                ELSE  
                  TOXPFB(L,K,NS,NT)=0.  
                ENDIF  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDDO  
        NFD=NSED+NSND+1  
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              IF(HBED(L,K).GT.0.0)THEN  
                TOXFDFB(L,K,NT)=PORBED(L,K)*HBED(L,K)  
     &              /(PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT))  
                TOXCDFB(L,K,NT)=TOXPFB(L,K,NFD,NT)  
              ELSE  
                TOXFDFB(L,K,NT)=0.  
                TOXCDFB(L,K,NT)=0.  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDDO  
      ENDIF  

!{ GeoSR, YSSONG, TOXIC, 101122
      ENDIF
!} GeoSR

C  
C **  CALCULATE PARTICULATE TOXIC CONTAMINANT SETTLING  
C **  AND BED EXCHANGE FLUXES  
C **  TOXF(L,KC,NT) = TOXIC CONTAMINANT SETTLING AND BED EXCHANGE  
C                        FLUX.  USED AS TEMPORARY VARIABLE IN THIS  
C                        AND FOLLOWING CODE BLOCK  
C  

!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC FLUX
      IF(ISTOXB.EQ.1)THEN
!} GeoSR

      IF(ISTRAN(5).GE.1)THEN  
        DO NT=1,NTOX  
          DO K=0,KC  
            DO L=2,LA  
              TOXF(L,K,NT)=0.  
            ENDDO  
          ENDDO  
        ENDDO  
        IF(KC.GE.2)THEN  
          DO NT=1,NTOX  
            IF(ISTRAN(6).GE.1)THEN  
C  
C PARTICULE COHESIVE FLUX  
C  
              DO NS=1,NSED  
                IF(ITXPARW(NS,NT).EQ.0)THEN  
                  DO K=1,KS  
                    DO L=2,LA  
                      TOXF(L,K,NT)=TOXF(L,K,NT)+SEDF(L,K,NS)  
     &                    *STFPOCW(L,K+1,NS)*TOXPARW(NS,NT)  
                    ENDDO  
                  ENDDO  
                ENDIF  
                IF(ITXPARW(NS,NT).EQ.1)THEN  
                  TMPEXP=CONPARW(NS,NT)  
                  DO K=1,KS  
                    DO L=2,LA  
                      TMPVAL=1.  
                      IF(SED(L,K+1,NS).GT.0.) TMPVAL=  
     &                    SED(L,K+1,NS)**TMPEXP  
                      TOXF(L,K,NT)=TOXF(L,K,NT)+TMPVAL*SEDF(L,K,NS)  
     &                    *STFPOCW(L,K+1,NS)*TOXPARW(NS,NT)  
                    ENDDO  
                  ENDDO  
                ENDIF  
!{GeoSR, YSSONG, TOXIC, 101031
              IF(ITXPARW(NS,NT).EQ.-1)THEN  
                DO K=1,KC  
                  DO L=2,LA  
                   TOXF(L,K,NT)=TOXF(L,K,NT)+SEDF(L,K,NS)*TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
!} GeoSR
              ENDDO  
            ENDIF  
            IF(ISTRAN(7).GE.1)THEN  
C  
C PARTICULE NONCOHESIVE FLUX  
C  
              DO NX=1,NSND  
                NS=NX+NSED  
                IF(ITXPARW(NS,NT).EQ.0)THEN  
                  DO K=1,KS  
                    DO L=2,LA  
                      TOXF(L,K,NT)=TOXF(L,K,NT)+SNDF(L,K,NX)  
     &                    *STFPOCW(L,K+1,NS)*TOXPARW(NS,NT)  
                    ENDDO  
                  ENDDO  
                ENDIF  
                IF(ITXPARW(NS,NT).EQ.1)THEN  
                  TMPEXP=CONPARW(NS,NT)  
                  DO K=1,KS  
                    DO L=2,LA  
                      TMPVAL=1.  
                      IF(SND(L,K+1,NX).GT.0.) TMPVAL=  
     &                    SND(L,K+1,NX)**TMPEXP  
                      TOXF(L,K,NT)=TOXF(L,K,NT)+TMPVAL*SNDF(L,K,NX)  
     &                    *STFPOCW(L,K+1,NS)*TOXPARW(NS,NT)  
                    ENDDO  
                  ENDDO  
                ENDIF  
!{GeoSR, YSSONG, TOXIC, 101031
              IF(ITXPARW(NS,NT).EQ.-1)THEN  
                DO K=1,KC  
                  DO L=2,LA  
                    TOXF(L,K,NT)=TOXF(L,K,NT)+SNDF(L,K,NS)
     &                    *TOXPARW(NS,NT)  
                  ENDDO  
                ENDDO  
              ENDIF  
!} GeoSR
              ENDDO  
            ENDIF  
          ENDDO  
        ENDIF  
        IF(KC.GE.2)THEN  
          DO NT=1,NTOX  
            DO K=1,KS  
              DO L=2,LA  
                TOXF(L,K,NT)=TOXF(L,K,NT)/(1.+TOXPFTW(L,K+1,NT))  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        DO NT=1,NTOX  
          IF(ISTRAN(6).GE.1)THEN  
C  
C PARTICULE COHESIVE FLUX  
C  
            DO NS=1,NSED  
              IF(ITXPARW(NS,NT).EQ.0)THEN  
                DO L=2,LA  
                  TOXF(L,0,NT)=TOXF(L,0,NT)+MIN(SEDF(L,0,NS),0.)  
     &                *STFPOCW(L,1,NS)*TOXPARW(NS,NT)  
                ENDDO  
              ENDIF  
              IF(ITXPARW(NS,NT).EQ.1)THEN  
                TMPEXP=CONPARW(NS,NT)  
                DO L=2,LA  
                  TMPVAL=1.  
                  IF(SED(L,1,NS).GT.0.) TMPVAL=SED(L,1,NS)**TMPEXP  
                  TOXF(L,0,NT)=TOXF(L,0,NT)+TMPVAL*MIN(SEDF(L,0,NS),0.)  
     &                *STFPOCW(L,1,NS)*TOXPARW(NS,NT)  
                ENDDO  
              ENDIF 
!{GeoSR, YSSONG, TOXIC, 101031
              IF(ITXPARW(NS,NT).EQ.-1)THEN  
                DO L=2,LA  
                  TOXF(L,0,NT)=TOXF(L,0,NT)+SEDF(L,0,NS)*TOXPARW(NS,NT)  
                ENDDO  
              ENDIF  
!} GeoSR		 
            ENDDO  
          ENDIF  
          IF(ISTRAN(7).GE.1)THEN  
C  
C PARTICULE NONCOHESIVE FLUX  
C  
            DO NX=1,NSND  
              NS=NX+NSED  
              IF(ITXPARW(NS,NT).EQ.0)THEN  
                DO L=2,LA  
                  SNDFEFF=SNDF(L,0,NX)-SNDFBL(L,NX)  
                  TOXF(L,0,NT)=TOXF(L,0,NT)+  
     &                MIN(SNDFEFF,0.)*STFPOCW(L,1,NS)*TOXPARW(NS,NT)  
                ENDDO  
              ENDIF  
              IF(ITXPARW(NS,NT).EQ.1)THEN  
                TMPEXP=CONPARW(NS,NT)  
                DO L=2,LA  
                  TMPVAL=1.  
                  IF(SND(L,1,NX).GT.0.) TMPVAL=SND(L,1,NX)**TMPEXP  
                  SNDFEFF=SNDF(L,0,NX)-SNDFBL(L,NX)  
                  TOXF(L,0,NT)=TOXF(L,0,NT)+  
     &                TMPVAL*MIN(SNDFEFF,0.)*STFPOCW(L,1,NS)*
     &                TOXPARW(NS,NT)  
                ENDDO  
              ENDIF 
!{GeoSR, YSSONG, TOXIC, 101031
              IF(ITXPARW(NS,NT).EQ.-1)THEN  
                DO L=2,LA  
                  TOXF(L,0,NT)=TOXF(L,0,NT)+SNDF(L,0,NS)*TOXPARW(NS,NT)  
                ENDDO  
              ENDIF  
!} GeoSR				 
            ENDDO  
          ENDIF  
        ENDDO  
        DO NT=1,NTOX  
          DO L=2,LA  
            TOXF(L,0,NT)=TOXF(L,0,NT)/(1.+TOXPFTW(L,1,NT))  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          DO L=2,LA  
            TOXFB(L,KBT(L),NT)=0.  
          ENDDO  
        ENDDO  
        DO NT=1,NTOX  
          IF(ISTRAN(6).GE.1)THEN  
C  
C PARTICULE COHESIVE FLUX  
C  
            DO NS=1,NSED  
              DO L=2,LA  
                TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)+  
     &              MAX(SEDF(L,0,NS),0.)*STFPOCB(L,KBT(L),NS)*
     &              TOXPARB(NS,NT)  
              ENDDO  
            ENDDO  
          ENDIF  
          IF(ISTRAN(7).GE.1)THEN  
C  
C PARTICULE NONCOHESIVE FLUX  
C  
            DO NX=1,NSND  
              NS=NX+NSED  
              DO L=2,LA  
                SNDFEFF=SNDF(L,0,NX)-SNDFBL(L,NX)  
                TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)  
     &              +MAX(SNDFEFF,0.)*STFPOCB(L,KBT(L),NS)*TOXPARB(NS,NT)  
              ENDDO  
            ENDDO  
          ENDIF  
        ENDDO  
        DO NT=1,NTOX  
          DO L=2,LA  
            IF(HBED(L,KBT(L)).GT.0.0)THEN  
              TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)/  
     &            (PORBED(L,KBT(L))*HBED(L,KBT(L))+TOXPFTB(L,KBT(L),NT))  
            ELSE  
              TOXFB(L,KBT(L),NT)=0.  
            ENDIF  
          ENDDO  
        ENDDO  
C  
C ** DIAGNOSTICS OF FLUX  
C  
        IF(ISDTXBUG.EQ.1.AND.DEBUG)THEN  
          IF(N.EQ.1)THEN  
            OPEN(2,FILE='TOXFLX.DIA')  
            CLOSE(2,STATUS='DELETE')  
            OPEN(2,FILE='TOXFLX.DIA')  
            DO L=2,LA  
              WRITE(2,2222)IL(L),JL(L),HBED(L,KBT(L)),  
     &            TOXPFTB(L,KBT(L),1),TOXFB(L,KBT(L),1),TOXF(L,0,1)  
            ENDDO  
            CLOSE(2)  
          ENDIF  
        ENDIF  
      ENDIF  

!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC FLUX
      ENDIF
!} GeoSR

C  
C **  CALCULATE TOTAL PARTICULATE FRACTIONS IN WATER COLUMN AND BED  
C **  NOTING THAT TO THIS POINT TOXPFTW AND TOXPFTB HAVE BEEN USED  
C **  TO TEMPORILY SORTED THE SORBED PORTION  
C  
      IF(ISTRAN(5).GE.1)THEN  
        NFD=NSED+NSND+1  
        DO NT=1,NTOX  
          DO K=1,KC  
            DO L=2,LA  
              TOXPFTW(L,K,NT)=( TOXPFTW(L,K,NT)/(1.+TOXPFTW(L,K,NT)) )  
     &            -TOXPFW(L,K,NFD,NT)  
            ENDDO  
          ENDDO  
        ENDDO  

!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
        IF(ISTOXB.EQ.1)THEN
!} GeoSR
        DO NT=1,NTOX  
          DO K=1,KB  
            DO L=2,LA  
              IF(HBED(L,K).GT.0.0)THEN  
                TOXPFTB(L,K,NT)=( TOXPFTB(L,K,NT)  
     &              /(PORBED(L,K)*HBED(L,K)+TOXPFTB(L,K,NT)) )  
     &              -TOXPFB(L,K,NFD,NT)  
              ELSE  
                TOXPFTB(L,K,NT)=0.  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDDO  
!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
        ENDIF
!} GeoSR
      ENDIF  
C  
C FIXED FOR BED LOAD JMH 5/22/02  
C **  DETERIME TOXIC FLUX FROM BED LOAD SORBED MATERIAL  
C  
!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
      IF(ISTOXB.EQ.1)THEN
!} GeoSR
      DO NT=1,NTOX  
        TOXBLB(NT)=0.0  
        DO L=2,LA  
          TOXFBL(L,NT)=0.0  
        ENDDO  
      ENDDO  
      DO NT=1,NTOX  
        DO NX=1,NSND  
          NS=NX+NSED  
          DO L=2,LA  
            IF(LMASKDRY(L))THEN  
              LE=L+1  
              LW=L-1  
              LN=LNC(L)  
              LS=LSC(L)  
              TMPTOXC=0.0  
              TMPTOXE=0.0  
              TMPTOXW=0.0  
              TMPTOXN=0.0  
              TMPTOXS=0.0  
              K=KBT(L)  
              IF(SNDB(L,K,NX).GT.0.0) THEN  
                TMPTOXC=TOXPFB(L,K,NS,NT)*TOXB(L,K,NT)*HBED(L,K)  
     &              /SNDB(L,K,NX)  
              ENDIF  
              K=KBT(LE)  
              IF(SUB(LE).GT.0.5.AND.SNDB(LE,K,NX).GT.0.0) THEN  
                TMPTOXE=TOXPFB(LE,K,NS,NT)*TOXB(LE,K,NT)*HBED(LE,K)  
     &              /SNDB(LE,K,NX)  
              ENDIF  
              K=KBT(LW)  
              IF(SUB(L).GT.0.5.AND.SNDB(LW,K,NX).GT.0.0) THEN  
                TMPTOXW=TOXPFB(LW,K,NS,NT)*TOXB(LW,K,NT)*HBED(LW,K)  
     &              /SNDB(LW,K,NX)  
              ENDIF  
              K=KBT(LN)  
              IF(SVB(LN).GT.0.5.AND.SNDB(LN,K,NX).GT.0.0) THEN  
                TMPTOXN=TOXPFB(LN,K,NS,NT)*TOXB(LN,K,NT)*HBED(LN,K)  
     &              /SNDB(LN,K,NX)  
              ENDIF  
              K=KBT(LS)  
              IF(SVB(L).GT.0.5.AND.SNDB(LS,K,NX).GT.0.0) THEN  
                TMPTOXS=TOXPFB(LS,K,NS,NT)*TOXB(LS,K,NT)*HBED(LS,K)  
     &              /SNDB(LS,K,NX)  
              ENDIF  
              TOXFBL(L,NT)=TOXFBL(L,NT)+DXYIP(L)*(  
     &            SUB(L+1)*MAX(QSBDLDX(L+1,NX),0.)*TMPTOXC  
     &            +SUB(L+1)*MIN(QSBDLDX(L+1,NX),0.)*TMPTOXE  
     &            -SUB(L  )*MAX(QSBDLDX(L  ,NX),0.)*TMPTOXW  
     &            -SUB(L  )*MIN(QSBDLDX(L  ,NX),0.)*TMPTOXC  
     &            +SVB(LN )*MAX(QSBDLDY(LN ,NX),0.)*TMPTOXC  
     &            +SVB(LN )*MIN(QSBDLDY(LN ,NX),0.)*TMPTOXN  
     &            -SVB(L  )*MAX(QSBDLDY(L  ,NX),0.)*TMPTOXS  
     &            -SVB(L  )*MIN(QSBDLDY(L  ,NX),0.)*TMPTOXC )  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDDO  

C8822 FORMAT(3I5,E14.5)  
      IF(IS2TIM.GE.1) THEN  
        IF(ISBAL.GE.1)THEN  
          IF(NSBDLDBC.GT.0) THEN  
            DO NSB=1,NSBDLDBC  
              LUTMP=LSBLBCU(NSB)  
              LDTMP=LSBLBCD(NSB)  
              IF(LDTMP.EQ.0) THEN  
                DO NT=1,NTOX  
                  DO NX=1,NSND  
                    NS=NX+NSED  
                    TMPTOXC=0.0  
                    K=KBT(LUTMP)  
                    IF(SNDB(LUTMP,K,NX).GT.0.0) THEN  
                      TMPTOXC=TOXPFB(LUTMP,K,NS,NT)*TOXB(LUTMP,K,NT)  
     &                    *HBED(LUTMP,K)/SNDB(LUTMP,K,NX)  
                    ENDIF  
                    TOXBLB(NT)=TOXBLB(NT)+QSBDLDOT(LUTMP,NX)*TMPTOXC  
                    TOXFBL(LUTMP,NT)=TOXFBL(LUTMP,NT)+DXYIP(LUTMP)*  
     &                    QSBDLDOT(LUTMP,NX)*TMPTOXC  
                  ENDDO  
                ENDDO  
              ENDIF  
            ENDDO  
            DO NSB=1,NSBDLDBC  
              LUTMP=LSBLBCU(NSB)  
              LDTMP=LSBLBCD(NSB)  
              IF(LDTMP.NE.0) THEN  
                DO NT=1,NTOX  
                  DO NX=1,NSND  
                    NS=NX+NSED  
                    TMPTOXC=0.0  
                    K=KBT(LUTMP)  
                    IF(SNDB(LUTMP,K,NX).GT.0.0) THEN  
                      TMPTOXC=TOXPFB(LUTMP,K,NS,NT)*TOXB(LUTMP,K,NT)  
     &                    *HBED(LUTMP,K)/SNDB(LUTMP,K,NX)  
                    ENDIF  
                    TOXFBL(LUTMP,NT)=TOXFBL(LUTMP,NT)+DXYIP(LUTMP)*  
     &                  QSBDLDOT(LUTMP,NX)*TMPTOXC  
                    TOXFBL(LDTMP,NT)=TOXFBL(LDTMP,NT)-DXYIP(LDTMP)*  
     &                  QSBDLDOT(LUTMP,NX)*TMPTOXC  
                  ENDDO  
                ENDDO  
              ENDIF  
            ENDDO  
          ENDIF  
        ENDIF  
      ENDIF  
      DO NT=1,NTOX  
        TOXFBLT(NT)=0.  
        DO L=2,LA   
          TOXFBLT(NT)=TOXFBLT(NT)+DXYP(L)*TOXFBL(L,NT)  
        ENDDO  
      ENDDO  
C8862 FORMAT('N,NX,SNDFBLTOT,QSBLLDXDY   =',2I5,2E14.5)  
C8899 FORMAT('N,TOXFBLT(NT),TOXBLB(NT)=',I5,2E14.5)  
C  
C END FIXED FOR BED LOAD JMH 5/22/02  
C **  ADJUST TOXIC FLUXES ACROSS WATER COLUMN - BED INTERFACE TO  
C **  INCLUDE WATER ENTRAINMENT AND EXPULSION ASSOCIATED WITH  
C **  DEPOSITION AND RESUSPENSION  
C  
      DO NT=1,NTOX  
        DO L=2,LA  
          TMPVAL=( MIN(QSBDTOP(L),0.0)+MIN(QWBDTOP(L),0.0) )  
          TOXF(L,0,NT)=TOXF(L,0,NT)+TMPVAL*(1.-TOXPFTW(L,1,NT))  
        ENDDO  
      ENDDO  

      DO NT=1,NTOX  
        DO L=2,LA  
          K=KBT(L)  
          TMPVAL=( MAX(QSBDTOP(L),0.0)+MAX(QWBDTOP(L),0.0) )/HBED(L,K)  
          TOXFB(L,K,NT)=TOXFB(L,K,NT)+TMPVAL*(1.-TOXPFTB(L,K,NT))  
        ENDDO  
      ENDDO  
C  
C **  TOXIC CONTAMINANT, KC=1 (SINGLE LAYER IN VERTICAL)  
C  
!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
      ENDIF
!} GeoSR

!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
      IF(ISTOXB.EQ.1)THEN
!} GeoSR
      IF(ISTRAN(5).GE.1.AND.KC.EQ.1)THEN  
        DO NT=1,NTOX  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(1)  
            AA11=WVEL-TOXF(L,0,NT)  
            AA12=-TOXFB(L,KBT(L),NT)  
            AA21=TOXF(L,0,NT)  
            AA22=DELTI+TOXFB(L,KBT(L),NT)  
            BB11=WVEL*TOX(L,1,NT)  
            BB22=DELTI*TOXB(L,KBT(L),NT)-TOXFBL(L,NT)  
            DETI=1./(AA11*AA22-AA12*AA21)  
            TOX(L,1,NT)=DETI*(BB11*AA22-BB22*AA12)  
            TOXB1(L,KBT(L),NT)=TOXB(L,KBT(L),NT)  
            TOXB(L,KBT(L),NT)=DETI*(AA11*BB22-AA21*BB11)  
            TOXF(L,0,NT)=TOXF(L,0,NT)*TOX(L,1,NT)  
            TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)*TOXB(L,KBT(L),NT)  
            TOXTOTMP=HP(L)*TOX(L,1,NT)+TOXB(L,KBT(L),NT)  
C  
C ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
            TOX(L,1,NT)=(BB11+TOXF(L,0,NT)+TOXFB(L,KBT(L),NT))/WVEL  
            TOXB(L,KBT(L),NT)=DELT*(BB22-TOXF(L,0,NT)  
     &          -TOXFB(L,KBT(L),NT))  
C  
C END ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
          ENDDO  

C 676 FORMAT('N,L,T,TB,TT,T1.TB1,F,FB=',2I5,8E13.4)  
C 677 FORMAT('N,L,T,TB               =',2I5,8E13.4)  
      ENDDO  
      ENDIF  

C  
C **  TOXIC CONTAMINANT, KC=2 (TWO LAYERS IN VERTICAL)  
C  
      IF(ISTRAN(5).GE.1.AND.KC.EQ.2)THEN  
        DO NT=1,NTOX  
          K=2  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(K)  
            CLEFT=WVEL-TOXF(L,K-1,NT)  
            CRIGHT=WVEL*TOX(L,K,NT)  
            TOX(L,K,NT)=CRIGHT/CLEFT  
          ENDDO  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(1)  
            AA11=WVEL-TOXF(L,0,NT)  
            AA12=-TOXFB(L,KBT(L),NT)  
            AA21=TOXF(L,0,NT)  
            AA22=DELTI+TOXFB(L,KBT(L),NT)  
            BB11=WVEL*TOX(L,1,NT)-TOXF(L,1,NT)*TOX(L,KC,NT)  
            BB22=DELTI*TOXB(L,KBT(L),NT)-TOXFBL(L,NT)  
            DETI=1./(AA11*AA22-AA12*AA21)  
            TOX(L,1,NT)=DETI*(BB11*AA22-BB22*AA12)  
            TOXB1(L,KBT(L),NT)=S3TL*TOXB(L,KBT(L),NT)  
     &          +S2TL*TOXB1(L,KBT(L),NT)  
            TOXB(L,KBT(L),NT)=DETI*(AA11*BB22-AA21*BB11)  
            TOXF(L,0,NT)=TOXF(L,0,NT)*TOX(L,1,NT)  
            TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)*TOXB(L,KBT(L),NT)  
C  
C ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
            TOX(L,1,NT)=(BB11+TOXF(L,0,NT)+TOXFB(L,KBT(L),NT))/WVEL  
            TOXB(L,KBT(L),NT)=DELT*(BB22-TOXF(L,0,NT)  
     &          -TOXFB(L,KBT(L),NT))  
C  
C END ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  TOXIC CONTAMINANT, KC=3 (THREE LAYERS IN VERTICAL)  
C  
      IF(ISTRAN(5).GE.1.AND.KC.EQ.3)THEN  
        DO NT=1,NTOX  
          K=3  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(K)  
            CLEFT=WVEL-TOXF(L,K-1,NT)  
            CRIGHT=WVEL*TOX(L,K,NT)  
            TOX(L,K,NT)=CRIGHT/CLEFT  
          ENDDO  
          K=2  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(K)  
            CLEFT=WVEL-TOXF(L,K-1,NT)  
            CRIGHT=WVEL*TOX(L,K,NT)-TOXF(L,K,NT)*TOX(L,K+1,NT)  
            TOX(L,K,NT)=CRIGHT/CLEFT  
          ENDDO  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(1)  
            AA11=WVEL-TOXF(L,0,NT)  
            AA12=-TOXFB(L,KBT(L),NT)  
            AA21=TOXF(L,0,NT)  
            AA22=DELTI+TOXFB(L,KBT(L),NT)  
            BB11=WVEL*TOX(L,1,NT)-TOXF(L,1,NT)*TOX(L,KC-1,NT)  
C  
C FIXED FOR BED LOAD JMH 5/22/02  
C  
            BB22=DELTI*TOXB(L,KBT(L),NT)-TOXFBL(L,NT)  
C  
C END FIXED FOR BED LOAD JMH 5/22/02  
C  
            DETI=1./(AA11*AA22-AA12*AA21)  
            TOX(L,1,NT)=DETI*(BB11*AA22-BB22*AA12)  
            TOXB1(L,KBT(L),NT)=S3TL*TOXB(L,KBT(L),NT)  
     &          +S2TL*TOXB1(L,KBT(L),NT)  
            TOXB(L,KBT(L),NT)=DETI*(AA11*BB22-AA21*BB11)  
            TOXF(L,0,NT)=TOXF(L,0,NT)*TOX(L,1,NT)  
            TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)*TOXB(L,KBT(L),NT)  
C  
C ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
            TOX(L,1,NT)=(BB11+TOXF(L,0,NT)+TOXFB(L,KBT(L),NT))/WVEL  
            TOXB(L,KBT(L),NT)=DELT*(BB22-TOXF(L,0,NT)  
     &          -TOXFB(L,KBT(L),NT))  
C  
C END ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
          ENDDO  
        ENDDO  
      ENDIF  
C  
C **  TOXIC CONTAMINANT, KC.GE.3 (THREE OR MORE LAYERS IN VERTICAL)  
C  
      IF(KC.GE.3)K1P1=2  
      IF(ISTRAN(5).GE.1.AND.KC.GE.3)THEN  
        DO NT=1,NTOX  
          K=KC  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(K)  
            CLEFT=WVEL-TOXF(L,K-1,NT)  
            CRIGHT=WVEL*TOX(L,K,NT)  
            TOX(L,K,NT)=CRIGHT/CLEFT  
          ENDDO  
          DO K=KS,2,-1  
            DO L=2,LA  
              WVEL=DELTI*HP(L)*DZC(K)  
              CLEFT=WVEL-TOXF(L,K-1,NT)  
              CRIGHT=WVEL*TOX(L,K,NT)-TOXF(L,K,NT)*TOX(L,K+1,NT)  
              TOX(L,K,NT)=CRIGHT/CLEFT  
            ENDDO  
          ENDDO  
          DO L=2,LA  
            WVEL=DELTI*HP(L)*DZC(1)  
            AA11=WVEL-TOXF(L,0,NT)  
            AA12=-TOXFB(L,KBT(L),NT)  
            AA21=TOXF(L,0,NT)  
            AA22=DELTI+TOXFB(L,KBT(L),NT)  
            BB11=WVEL*TOX(L,1,NT)-TOXF(L,1,NT)*TOX(L,K1P1,NT)  
C  
C FIXED FOR BED LOAD JMH 5/22/02  
C  
            BB22=DELTI*TOXB(L,KBT(L),NT)-TOXFBL(L,NT)  
C  
C END FIXED FOR BED LOAD JMH 5/22/02  
C  
            DETI=1./(AA11*AA22-AA12*AA21)  
            TOX(L,1,NT)=DETI*(BB11*AA22-BB22*AA12)  
            TOXB1(L,KBT(L),NT)=S3TL*TOXB(L,KBT(L),NT)  
     &          +S2TL*TOXB1(L,KBT(L),NT)  
            TOXB(L,KBT(L),NT)=DETI*(AA11*BB22-AA21*BB11)  
            TOXF(L,0,NT)=TOXF(L,0,NT)*TOX(L,1,NT)  
            TOXFB(L,KBT(L),NT)=TOXFB(L,KBT(L),NT)*TOXB(L,KBT(L),NT)  
C  
C ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
            TOX(L,1,NT)=(BB11+TOXF(L,0,NT)+TOXFB(L,KBT(L),NT))/WVEL  
            TOXB(L,KBT(L),NT)=DELT*(BB22-TOXF(L,0,NT)  
     &          -TOXFB(L,KBT(L),NT))  
C  
C END ADJUST WC AND BED TOXIC CONSISTENT WITH FLUX  
C  
          ENDDO  
        ENDDO  
      ENDIF  

!{ GeoSR, YSSONG, TOXIC, 101122, TOXIC IN BED
      ENDIF 
!} GeoSR
        
 

C  
C **  ADD PARENT TO ACTIVE LAYER TOXIC TRANSPORT  
C  
      IF(ISNDAL.GE.2)THEN  
        IF(ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            DO L=2,LA  
              TOXFPA(L)=0.0  
            ENDDO  
            DO NS=1,NSED  
              DO L=2,LA  
                KTOPTP=KBT(L)  
                KTOPM1=KBT(L)-1  
                FTPOS=0.0  
                FTNEG=0.0  
                IF(SEDFPA(L,NS).GT.0.0.AND.SEDB(L,KTOPM1,NS).GT.0.0)  
     &              FTPOS=SEDFPA(L,NS)*TOXPFB(L,KTOPM1,NS,NT)/SEDB(
     &              L,KTOPM1,NS)  
                IF(SEDFPA(L,NS).LT.0.0.AND.SEDB(L,KTOPTP,NS).GT.0.0)  
     &              FTNEG=SEDFPA(L,NS)*TOXPFB(L,KTOPTP,NS,NT)/SEDB(
     &              L,KTOPTP,NS)  
                TOXFPA(L)=TOXFPA(L)+FTPOS*TOXB(L,KTOPM1,NT)  
     &              +FTNEG*TOXB(L,KTOPTP,NT)  
              ENDDO  
            ENDDO  
            DO NX=1,NSND  
              NS=NX+NSED  
              DO L=2,LA  
                KTOPTP=KBT(L)  
                KTOPM1=KBT(L)-1  
                FTPOS=0.0  
                FTNEG=0.0  
                IF(SNDFPA(L,NX).GT.0.0.AND.SNDB(L,KTOPM1,NX).GT.0.0)  
     &              FTPOS=SNDFPA(L,NX)*TOXPFB(L,KTOPM1,NS,NT)/SNDB(
     &              L,KTOPM1,NX)  
                IF(SNDFPA(L,NX).LT.0.0.AND.SNDB(L,KTOPTP,NX).GT.0.0)  
     &              FTNEG=SNDFPA(L,NX)*TOXPFB(L,KTOPTP,NS,NT)/SNDB(
     &              L,KTOPTP,NX)  
                TOXFPA(L)=TOXFPA(L)+FTPOS*TOXB(L,KTOPM1,NT)  
     &              +FTNEG*TOXB(L,KTOPTP,NT)  
              ENDDO  
            ENDDO  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPM1=KBT(L)-1  
              FTPOS=0.0  
              FTNEG=0.0  
              IF(QWATPA(L).GT.0.0)  
     &            FTPOS=QSSDPA(L)*(1.-TOXPFTB(L,KTOPM1,NT))  
              IF(QWATPA(L).LT.0.0)  
     &            FTNEG=QSSDPA(L)*(1.-TOXPFTB(L,KTOPTP,NT))  
              TOXFPA(L)=TOXFPA(L)+FTPOS*TOXB(L,KTOPM1,NT)  
     &            +FTNEG*TOXB(L,KTOPTP,NT)  
            ENDDO  
            DO L=2,LA  
              KTOPTP=KBT(L)  
              KTOPM1=KBT(L)-1  
              TOXB(L,KTOPTP,NT)=TOXB(L,KTOPTP,NT)+DELT*TOXFPA(L)  
              TOXB(L,KTOPM1,NT)=TOXB(L,KTOPM1,NT)-DELT*TOXFPA(L)  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDIF  
C8888 FORMAT(4I5,7E14.5)  
 2222 FORMAT(2I5,7E14.5)  
      RETURN  
      END  

