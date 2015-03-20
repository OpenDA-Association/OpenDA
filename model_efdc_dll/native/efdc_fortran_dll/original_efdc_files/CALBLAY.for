      SUBROUTINE CALBLAY  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALBLAY REMOVES OR ADDS LAYERS TO THE SEDIMENT BED  
C  
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::K,NS,L,NT,NX
	REAL::TMPBOT2,TMPTOP1,TMPTOP2,TMPVAL,HBEDMXT,HOLDTOP,FKBTP
	REAL::SEDBOLD,TOXBOLD,TMPBOT1,FKBT,SNDBOLD
C  
C     FOR TRANSPORT OF COHESIVE SEDIMENT ONLY SET HBEDMIN TO FRACTION
C     OF HBEDMAX
C
        IF(ISTRAN(7).EQ.0) THEN
	    HBEDMIN=0.1*HBEDMAX
	  ELSE
          ! *** NOTE: BELOW IS USED BY HQI FOR HOUSATONIC
	    HBEDMIN=0.006
	  ENDIF
C
C     WHEN NONCOHESIVE TRANSPORT IS ACTIVE, WITHOUT ACTIVE-PARENT LAYER
C     FORMULATION, SET HBEDMIN PROPORTIONAL TO MAXIMUM GRAIN DIAMETER
C
        IF(ISTRAN(7).GE.1.AND.ISNDAL.LE.1)THEN
C NOTE HQI CHANGED ORIGINAL FORMULATION FROM
C	    TMPVAL=2.*SNDDMX
C TO
	    TMPVAL=SNDDMX
C          HBEDMIN=MAX(HBEDMIN,SNDDMX)
          HBEDMIN=MAX(HBEDMIN,TMPVAL)
        END IF
C  
      IF(ISTRAN(7).GE.1.AND.ISNDAL.LE.1)THEN  
C  
C#######################################################################  
C   HQI, 09/03/03, ROONI M.  
C     MIN LAYER THICKNESS CHANGED TO 1 TIMES MAXIMUM NON-COHESIVE DIAMET  
C     IN ORDER TO AVOID LAYERS FROM MERGING INTO EACH OTHER  
C          TMPVAL=2.*SNDDMX  
C  
        TMPVAL=SNDDMX  
        HBEDMIN=MAX(HBEDMIN,TMPVAL)  
      END IF  
C
C     WHEN NONCOHESIVE TRANSPORT IS ACTIVE, WITH ACTIVE-PARENT LAYER
C     FORMULATION, SET HBEDMIN PROPORTIONAL ARMORING LAYER THICKNESS
C
      IF(ISTRAN(7).GE.1.AND.ISNDAL.EQ.2)THEN  
        HBEDMIN=2.*HBEDAL  
      END IF  
C  
C        HBEDMXT=1.1*HBEDMAX  
C  
      HBEDMXT=HBEDMAX+HBEDMIN  
C  
C **  ADD OR REMOVE TOP LAYER (NO ARMORING OPTION)  
C  
      IF(ISNDAL.LE.1)THEN
C  
C ** ADD NEW TOP LAYER  
C  
        DO L=2,LA  
          IF(HBED(L,KBT(L)).GT.HBEDMXT)THEN  
            IF(KBT(L).LT.KB)THEN  
              HOLDTOP=HBED(L,KBT(L))  
              HBED(L,KBT(L)+1)=HOLDTOP-HBEDMAX  
              HBED(L,KBT(L))=HBEDMAX  
              VDRBED(L,KBT(L)+1)=VDRBED(L,KBT(L))  
              PORBED(L,KBT(L)+1)=PORBED(L,KBT(L))  
              STDOCB(L,KBT(L)+1)=STDOCB(L,KBT(L))  
              STPOCB(L,KBT(L)+1)=STPOCB(L,KBT(L))  
              FKBTP=HBED(L,KBT(L)+1)/HOLDTOP  
              FKBT=HBED(L,KBT(L))/HOLDTOP  
              IF(ISTRAN(6).GE.1)THEN  
                SEDBT(L,KBT(L)+1)=0.  
                SEDBT(L,KBT(L))=0.  
                DO NS=1,NSED  
                  SEDBOLD=SEDB(L,KBT(L),NS)  
                  SEDB(L,KBT(L)+1,NS)=FKBTP*SEDBOLD  
                  SEDB(L,KBT(L),NS)=FKBT*SEDBOLD  
                  SEDBT(L,KBT(L)+1)=SEDBT(L,KBT(L)+1)  
     &                +SEDB(L,KBT(L)+1,NS)  
                  SEDBT(L,KBT(L))=SEDBT(L,KBT(L))+SEDB(L,KBT(L),NS)  
                  STFPOCB(L,KBT(L)+1,NS)=STFPOCB(L,KBT(L),NS)  
                  VFRBED(L,KBT(L)+1,NS)=VFRBED(L,KBT(L),NS)  
                ENDDO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                SNDBT(L,KBT(L)+1)=0.  
                SNDBT(L,KBT(L))=0.  
                DO NS=1,NSND  
                  NX=NS+NSED  
                  SNDBOLD=SNDB(L,KBT(L),NS)  
                  SNDB(L,KBT(L)+1,NS)=FKBTP*SNDBOLD  
                  SNDB(L,KBT(L),NS)=FKBT*SNDBOLD  
                  SNDBT(L,KBT(L)+1)=SNDBT(L,KBT(L)+1)  
     &                +SNDB(L,KBT(L)+1,NS)  
                  SNDBT(L,KBT(L))=SNDBT(L,KBT(L))+SNDB(L,KBT(L),NS)  
                  STFPOCB(L,KBT(L)+1,NX)=STFPOCB(L,KBT(L),NX)  
                  VFRBED(L,KBT(L)+1,NX)=VFRBED(L,KBT(L),NX)  
                ENDDO  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXBOLD=TOXB(L,KBT(L),NT)  
                  TOXB(L,KBT(L)+1,NT)=FKBTP*TOXBOLD  
                  TOXB(L,KBT(L),NT)=FKBT*TOXBOLD  
                ENDDO  
              ENDIF  
              KBT(L)=KBT(L)+1  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C ** REZONE WITH NEW TOP LAYER ADDED NEXT TIME STEP  
C  
        DO L=2,LA  
          IF(HBED(L,KBT(L)).GT.HBEDMXT)THEN  
            IF(KBT(L).EQ.KB.AND.KB.GT.1)THEN  
              TMPBOT1=HBED(L,1)/(1.+VDRBED(L,1))  
              TMPBOT2=HBED(L,2)/(1.+VDRBED(L,2))  
              TMPTOP1=VDRBED(L,1)*TMPBOT1  
              TMPTOP2=VDRBED(L,2)*TMPBOT2  
              VDRBED(L,1)=(TMPTOP1+TMPTOP2)/(TMPBOT1+TMPBOT2)  
              PORBED(L,1)=VDRBED(L,1)/(1.+VDRBED(L,1))  
              HBED(L,1)=HBED(L,1)+HBED(L,2)  
              IF(KB.EQ.2)THEN  
                HBED(L,2)=0.0  
                VDRBED(L,2)=0.0  
                PORBED(L,2)=0.0  
                STDOCB(L,2)=0.0  
                STPOCB(L,2)=0.0  
              ENDIF  
              IF(KB.GT.2)THEN  
                DO K=2,KBT(L)-1  
                  HBED(L,K)=HBED(L,K+1)  
                  VDRBED(L,K)=VDRBED(L,K+1)  
                  PORBED(L,K)=PORBED(L,K+1)  
                  STDOCB(L,K)=STDOCB(L,K+1)  
                  STPOCB(L,K)=STPOCB(L,K+1)  
                ENDDO  
                HBED(L,KBT(L))=0.0  
                VDRBED(L,KBT(L))=0.0  
                PORBED(L,KBT(L))=0.0  
                STDOCB(L,KBT(L))=0.0  
                STPOCB(L,KBT(L))=0.0  
              ENDIF  
              IF(ISTRAN(6).GE.1)THEN  
                DO NS=1,NSED  
                  SEDB(L,1,NS)=SEDB(L,1,NS)+SEDB(L,2,NS)  
                  IF(KB.EQ.2)THEN  
                    SEDB(L,2,NS)=0.0  
                    STFPOCB(L,2,NS)=0.0  
                    VFRBED(L,2,NS)=0.0  
                  ENDIF  
                  IF(KB.GT.2)THEN  
                    DO K=2,KBT(L)-1  
                      SEDB(L,K,NS)=SEDB(L,K+1,NS)  
                      STFPOCB(L,K,NS)=STFPOCB(L,K+1,NS)  
                      VFRBED(L,K,NS)=VFRBED(L,K+1,NS)  
                    ENDDO  
                    SEDB(L,KBT(L),NS)=0  
                    STFPOCB(L,KBT(L),NS)=0.0  
                    VFRBED(L,KBT(L),NS)=0.0  
                  ENDIF  
                ENDDO  
                DO K=1,KB  
                  SEDBT(L,K)=0.0  
                END DO  
                DO NS=1,NSED  
                  DO K=1,KB  
                    SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)  
                  END DO  
                END DO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                DO NS=1,NSND  
                  NX=NS+NSED  
                  SNDB(L,1,NS)=SNDB(L,1,NS)+SNDB(L,2,NS)  
                  IF(KB.EQ.2)THEN  
                    SNDB(L,2,NS)=0.0  
                    STFPOCB(L,2,NX)=0.0  
                    VFRBED(L,2,NX)=0.0  
                  ENDIF  
                  IF(KB.GT.2)THEN  
                    DO K=2,KBT(L)-1  
                      SNDB(L,K,NS)=SNDB(L,K+1,NS)  
                      STFPOCB(L,K,NX)=STFPOCB(L,K+1,NX)  
                      VFRBED(L,K,NX)=STFPOCB(L,K+1,NX)  
                    ENDDO  
                    SNDB(L,KBT(L),NS)=0  
                    STFPOCB(L,KBT(L),NX)=0.0  
                    VFRBED(L,KBT(L),NX)=0.0  
                  ENDIF  
                ENDDO  
                DO K=1,KB  
                  SNDBT(L,K)=0.0  
                END DO  
                DO NS=1,NSND  
                  DO K=1,KB  
                    SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NS)  
                  END DO  
                END DO  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXB(L,1,NT)=TOXB(L,1,NT)+TOXB(L,2,NT)  
                  IF(KB.EQ.2)THEN  
                    TOXB(L,2,NT)=0  
                  ENDIF  
                  IF(KB.GT.2)THEN  
                    DO K=2,KBT(L)-1  
                      TOXB(L,K,NS)=TOXB(L,K+1,NT)  
                    ENDDO  
                    TOXB(L,KBT(L),NT)=0  
                  ENDIF  
                ENDDO  
              ENDIF  
              IF(KB.EQ.2)THEN  
                KBT(L)=1  
              ENDIF  
              IF(KB.GT.2)THEN  
                KBT(L)=KBT(L)-1  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C ** REMOVE TOP LAYER  
C  
        DO L=2,LA  
          IF(HBED(L,KBT(L)).LT.HBEDMIN)THEN  
            IF(KBT(L).GT.1)THEN  
              TMPBOT1=HBED(L,KBT(L)-1)/(1.+VDRBED(L,KBT(L)-1))  
              TMPBOT2=HBED(L,KBT(L))/(1.+VDRBED(L,KBT(L)))  
              TMPTOP1=VDRBED(L,KBT(L)-1)*TMPBOT1  
              TMPTOP2=VDRBED(L,KBT(L))*TMPBOT2  
              VDRBED(L,KBT(L)-1)=(TMPTOP1+TMPTOP2)/(TMPBOT1+TMPBOT2)  
              PORBED(L,KBT(L)-1)=VDRBED(L,KBT(L)-1)/  
     &            (1.+VDRBED(L,KBT(L)-1))  
              HBED(L,KBT(L)-1)=HBED(L,KBT(L)-1)+HBED(L,KBT(L))  
              HBED(L,KBT(L))=0.0  
              VDRBED(L,KBT(L))=0.0  
              PORBED(L,KBT(L))=0.0  
              STDOCB(L,KBT(L))=0.0  
              STPOCB(L,KBT(L))=0.0  
              BDENBED(L,KBT(L))=0.  ! PMC
              IF(ISTRAN(6).GE.1)THEN  
                SEDBT(L,KBT(L)-1)=0.  
                SEDBT(L,KBT(L))=0.  
                DO NS=1,NSED  
                  SEDB(L,KBT(L)-1,NS)=SEDB(L,KBT(L)-1,NS)  
     &                +SEDB(L,KBT(L),NS)  
                  SEDBT(L,KBT(L)-1)=SEDBT(L,KBT(L)-1)  
     &                +SEDB(L,KBT(L)-1,NS)  
                  SEDB(L,KBT(L),NS)=0.0  
                  STFPOCB(L,KBT(L),NS)=0.0  
                  VFRBED(L,KBT(L),NS)=0.0  
                ENDDO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                SNDBT(L,KBT(L)-1)=0.  
                SNDBT(L,KBT(L))=0.  
                DO NS=1,NSND  
                  SNDB(L,KBT(L)-1,NS)=SNDB(L,KBT(L)-1,NS)  
     &                +SNDB(L,KBT(L),NS)  
                  SNDBT(L,KBT(L)-1)=SNDBT(L,KBT(L)-1)  
     &                +SNDB(L,KBT(L)-1,NS)  
                  SNDB(L,KBT(L),NS)=0.0  
                  STFPOCB(L,KBT(L),NS+NSED)=0.0  
                  VFRBED(L,KBT(L),NS+NSED)=0.0  
                ENDDO  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXB(L,KBT(L)-1,NT)=TOXB(L,KBT(L)-1,NT)  
     &                +TOXB(L,KBT(L),NT)  
                  TOXB(L,KBT(L),NT)=0.0  
                ENDDO  
              ENDIF  
              KBT(L)=KBT(L)-1  
C  
C *** PMC BEGIN BLOCK  
C  
            ELSEIF(HBED(L,KBT(L)).LT.0.0)THEN  
              ! *** ZERO NEGATIVE THICKNESSES
              HBED(L,KBT(L))=0.0  
              VDRBED(L,KBT(L))=0.0  
              PORBED(L,KBT(L))=0.0  
              STDOCB(L,KBT(L))=0.0  
              STPOCB(L,KBT(L))=0.0  
C  
C *** PMC END BLOCK  
C  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C ++ UPDATE BULK DENSITY  
C  
        !DO K=1,KB  
          DO L=2,LA  
            K=KBT(L)  ! PMC - REMOVED LOOP AND ONLY COMPUTE THE TOP LAYER
            IF(HBED(L,K).GT.0.)THEN  
              BDENBED(L,K)=1000.*PORBED(L,K)  
     &            +0.001*(SEDBT(L,K)+SNDBT(L,K))/HBED(L,K)  
            ELSE  
              BDENBED(L,K)=0.  
            ENDIF  
          ENDDO  
        !ENDDO  
      ELSE  
C  
C **  ADD OR REMOVE PARENT LAYER WHEN ARMORING IS ACTIVE  
C ** ADD NEW TOP LAYER  
C  
        DO L=2,LA  
          IF(HBED(L,KBT(L)-1).GT.HBEDMXT)THEN  
            IF(KBT(L).LT.KB)THEN  
              WRITE(1,1110)N,IL(L),JL(L),(HBED(L,K),K=1,KB)  
C  
C    MOVE ACTIVE LAYER UP  
C  
              HBED(L,KBT(L)+1)=HBED(L,KBT(L))  
              VDRBED(L,KBT(L)+1)=VDRBED(L,KBT(L))  
              PORBED(L,KBT(L)+1)=PORBED(L,KBT(L))  
              STDOCB(L,KBT(L)+1)=STDOCB(L,KBT(L))  
              STPOCB(L,KBT(L)+1)=STPOCB(L,KBT(L))  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXB(L,KBT(L)+1,NT)=TOXB(L,KBT(L),NT)  
                ENDDO  
              ENDIF  
              IF(ISTRAN(6).GE.1)THEN  
                SEDBT(L,KBT(L)+1)=SEDBT(L,KBT(L))  
                DO NS=1,NSED  
                  SEDB(L,KBT(L)+1,NS)=SEDB(L,KBT(L),NS)  
                  STFPOCB(L,KBT(L)+1,NS)=STFPOCB(L,KBT(L),NS)  
                  VFRBED(L,KBT(L)+1,NS)=VFRBED(L,KBT(L),NS)  
                ENDDO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                SNDBT(L,KBT(L)+1)=SNDBT(L,KBT(L))  
                DO NS=1,NSND  
                  NX=NS+NSED  
                  SNDB(L,KBT(L)+1,NS)=SNDB(L,KBT(L),NS)  
                  STFPOCB(L,KBT(L)+1,NX)=STFPOCB(L,KBT(L),NX)  
                  VFRBED(L,KBT(L)+1,NX)=VFRBED(L,KBT(L),NX)  
                ENDDO  
              ENDIF  
C  
C     SPLIT PARENT INTO TWO LAYERS  
C  
              HOLDTOP=HBED(L,KBT(L)-1)  
              HBED(L,KBT(L))=HOLDTOP-HBEDMAX  
              HBED(L,KBT(L)-1)=HBEDMAX  
              VDRBED(L,KBT(L))=VDRBED(L,KBT(L)-1)  
              PORBED(L,KBT(L))=PORBED(L,KBT(L)-1)  
              STDOCB(L,KBT(L))=STDOCB(L,KBT(L)-1)  
              STPOCB(L,KBT(L))=STPOCB(L,KBT(L)-1)  
              FKBTP=HBED(L,KBT(L))/HOLDTOP  
              FKBT=HBED(L,KBT(L)-1)/HOLDTOP  
              IF(ISTRAN(6).GE.1)THEN  
                SEDBT(L,KBT(L))=0.  
                SEDBT(L,KBT(L)-1)=0.  
                DO NS=1,NSED  
                  SEDBOLD=SEDB(L,KBT(L)-1,NS)  
                  SEDB(L,KBT(L),NS)=FKBTP*SEDBOLD  
                  SEDB(L,KBT(L)-1,NS)=FKBT*SEDBOLD  
                  SEDBT(L,KBT(L))=SEDBT(L,KBT(L))  
     &                +SEDB(L,KBT(L),NS)  
                  SEDBT(L,KBT(L)-1)=SEDBT(L,KBT(L)-1)  
     &                +SEDB(L,KBT(L)-1,NS)  
                  STFPOCB(L,KBT(L),NS)=STFPOCB(L,KBT(L)-1,NS)  
                  VFRBED(L,KBT(L),NS)=VFRBED(L,KBT(L)-1,NS)  
                ENDDO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                SNDBT(L,KBT(L))=0.  
                SNDBT(L,KBT(L)-1)=0.  
                DO NS=1,NSND  
                  NX=NS+NSED  
                  SNDBOLD=SNDB(L,KBT(L)-1,NS)  
                  SNDB(L,KBT(L),NS)=FKBTP*SNDBOLD  
                  SNDB(L,KBT(L)-1,NS)=FKBT*SNDBOLD  
                  SNDBT(L,KBT(L))=SNDBT(L,KBT(L))  
     &                +SNDB(L,KBT(L),NS)  
                  SNDBT(L,KBT(L)-1)=SNDBT(L,KBT(L)-1)  
     &                +SNDB(L,KBT(L)-1,NS)  
                  STFPOCB(L,KBT(L),NX)=STFPOCB(L,KBT(L)-1,NX)  
                  VFRBED(L,KBT(L),NX)=VFRBED(L,KBT(L)-1,NX)  
                ENDDO  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXBOLD=TOXB(L,KBT(L),NT)  
                  TOXB(L,KBT(L),NT)=FKBTP*TOXBOLD  
                  TOXB(L,KBT(L)-1,NT)=FKBT*TOXBOLD  
                ENDDO  
              ENDIF  
              KBT(L)=KBT(L)+1  
              WRITE(1,1111)N,IL(L),JL(L),(HBED(L,K),K=1,KB)  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C ** REZONE WITH NEW TOP LAYER ADDED NEXT TIME STEP  
C  
        DO L=2,LA  
          IF(HBED(L,KBT(L)-1).GT.HBEDMXT)THEN  
            IF(KBT(L).EQ.KB.AND.KB.GT.1)THEN  
              WRITE(1,1110)N,IL(L),JL(L),(HBED(L,K),K=1,KB)  
              TMPBOT1=HBED(L,1)/(1.+VDRBED(L,1))  
              TMPBOT2=HBED(L,2)/(1.+VDRBED(L,2))  
              TMPTOP1=VDRBED(L,1)*TMPBOT1  
              TMPTOP2=VDRBED(L,2)*TMPBOT2  
              VDRBED(L,1)=(TMPTOP1+TMPTOP2)/(TMPBOT1+TMPBOT2)  
              PORBED(L,1)=VDRBED(L,1)/(1.+VDRBED(L,1))  
              HBED(L,1)=HBED(L,1)+HBED(L,2)  
              IF(KB.EQ.2)THEN  
                HBED(L,2)=0  
                VDRBED(L,2)=0.0  
                PORBED(L,2)=0.0  
                STDOCB(L,2)=0.0  
                STPOCB(L,2)=0.0  
              ENDIF  
              IF(KB.GT.2)THEN  
                DO K=2,KBT(L)-1  
                  HBED(L,K)=HBED(L,K+1)  
                  VDRBED(L,K)=VDRBED(L,K+1)  
                  PORBED(L,K)=PORBED(L,K+1)  
                  STDOCB(L,K)=STDOCB(L,K+1)  
                  STPOCB(L,K)=STPOCB(L,K+1)  
                ENDDO  
                HBED(L,KBT(L))=0.0  
                VDRBED(L,KBT(L))=0.0  
                PORBED(L,KBT(L))=0.0  
                STDOCB(L,KBT(L))=0.0  
                STPOCB(L,KBT(L))=0.0  
              ENDIF  
              IF(ISTRAN(6).GE.1)THEN  
                DO NS=1,NSED  
                  SEDB(L,1,NS)=SEDB(L,1,NS)+SEDB(L,2,NS)  
                  IF(KB.EQ.2)THEN  
                    SEDB(L,2,NS)=0.0  
                    STFPOCB(L,2,NS)=0.0  
                    VFRBED(L,2,NS)=0.0  
                  ENDIF  
                  IF(KB.GT.2)THEN  
                    DO K=2,KBT(L)-1  
                      SEDB(L,K,NS)=SEDB(L,K+1,NS)  
                      STFPOCB(L,K,NS)=STFPOCB(L,K+1,NS)  
                      VFRBED(L,K,NS)=VFRBED(L,K+1,NS)  
                    ENDDO  
                    SEDB(L,KBT(L),NS)=0.0  
                    STFPOCB(L,KBT(L),NS)=0.0  
                    VFRBED(L,KBT(L),NS)=0.0  
                  ENDIF  
                ENDDO  
                DO K=1,KB  
                  SEDBT(L,K)=0.0  
                END DO  
                DO NS=1,NSED  
                  DO K=1,KB  
                    SEDBT(L,K)=SEDBT(L,K)+SEDB(L,K,NS)  
                  END DO  
                END DO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                DO NS=1,NSND  
                  NX=NS+NSED  
                  SNDB(L,1,NS)=SNDB(L,1,NS)+SNDB(L,2,NS)  
                  IF(KB.EQ.2)THEN  
                    SNDB(L,2,NS)=0.0  
                    STFPOCB(L,2,NX)=0.0  
                    VFRBED(L,2,NX)=0.0  
                  ENDIF  
                  IF(KB.GT.2)THEN  
                    DO K=2,KBT(L)-1  
                      SNDB(L,K,NS)=SNDB(L,K+1,NS)  
                      STFPOCB(L,K,NX)=STFPOCB(L,K+1,NX)  
                      VFRBED(L,K,NX)=VFRBED(L,K+1,NX)  
                    ENDDO  
                    SNDB(L,KBT(L),NS)=0.0  
                    STFPOCB(L,KBT(L),NX)=0.0  
                    VFRBED(L,KBT(L),NX)=0.0  
                  ENDIF  
                ENDDO  
                DO K=1,KB  
                  SNDBT(L,K)=0.0  
                END DO  
                DO NS=1,NSND  
                  DO K=1,KB  
                    SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NS)  
                  END DO  
                END DO  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXB(L,1,NT)=TOXB(L,1,NT)+TOXB(L,2,NT)  
                  IF(KB.EQ.2)THEN  
                    TOXB(L,2,NT)=0.0  
                  ENDIF  
                  IF(KB.GT.2)THEN  
                    DO K=2,KBT(L)-1  
                      TOXB(L,K,NS)=TOXB(L,K+1,NT)  
                    ENDDO  
                    TOXB(L,KBT(L),NT)=0.0  
                  ENDIF  
                ENDDO  
              ENDIF  
              IF(KB.EQ.2)THEN  
                KBT(L)=1  
              ENDIF  
              IF(KB.GT.2)THEN  
                KBT(L)=KBT(L)-1  
              ENDIF  
              WRITE(1,1112)N,IL(L),JL(L),(HBED(L,K),K=1,KB)  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C ** REMOVE TOP LAYER  
C  
        DO L=2,LA  
          IF(HBED(L,KBT(L)-1).LT.HBEDMIN)THEN  
            IF(KBT(L).GT.2)THEN  
              WRITE(1,1110)N,IL(L),JL(L),(HBED(L,K),K=1,KB)  
              TMPBOT1=HBED(L,KBT(L)-2)/(1.+VDRBED(L,KBT(L)-2))  
              TMPBOT2=HBED(L,KBT(L)-1)/(1.+VDRBED(L,KBT(L)-1))  
              TMPTOP1=VDRBED(L,KBT(L)-2)*TMPBOT1  
              TMPTOP2=VDRBED(L,KBT(L)-1)*TMPBOT2  
              VDRBED(L,KBT(L)-2)=(TMPTOP1+TMPTOP2)/(TMPBOT1+TMPBOT2)  
              PORBED(L,KBT(L)-2)=VDRBED(L,KBT(L)-2)/  
     &            (1.+VDRBED(L,KBT(L)-2))  
              HBED(L,KBT(L)-2)=HBED(L,KBT(L)-2)+HBED(L,KBT(L)-1)  
              HBED(L,KBT(L)-1)=HBED(L,KBT(L))  
              VDRBED(L,KBT(L)-1)=VDRBED(L,KBT(L))  
              PORBED(L,KBT(L)-1)=PORBED(L,KBT(L))  
              HBED(L,KBT(L))=0.  
              VDRBED(L,KBT(L))=0.  
              PORBED(L,KBT(L))=0.  
              STDOCB(L,KBT(L))=0.  
              STPOCB(L,KBT(L))=0.  
              BDENBED(L,KBT(L))=0.  ! PMC
              IF(ISTRAN(6).GE.1)THEN  
                SEDBT(L,KBT(L)-2)=0.  
                SEDBT(L,KBT(L)-1)=0.  
                DO NS=1,NSED  
                  SEDB(L,KBT(L)-2,NS)=SEDB(L,KBT(L)-2,NS)  
     &                +SEDB(L,KBT(L)-1,NS)  
                  SEDBT(L,KBT(L)-2)=SEDBT(L,KBT(L)-2)  
     &                +SEDB(L,KBT(L)-2,NS)  
                  SEDB(L,KBT(L)-1,NS)=SEDB(L,KBT(L),NS)  
                  SEDBT(L,KBT(L)-1)=SEDBT(L,KBT(L)-1)  
     &                +SEDB(L,KBT(L)-1,NS)  
                  SEDB(L,KBT(L),NS)=0.0  
                  SEDBT(L,KBT(L))=0.0  
                  STFPOCB(L,KBT(L),NS)=0.0  
                  VFRBED(L,KBT(L),NS)=0.0  
                ENDDO  
              ENDIF  
              IF(ISTRAN(7).GE.1)THEN  
                SNDBT(L,KBT(L)-2)=0.  
                SNDBT(L,KBT(L)-1)=0.  
                DO NS=1,NSND  
                  SNDB(L,KBT(L)-2,NS)=SNDB(L,KBT(L)-2,NS)  
     &                +SNDB(L,KBT(L)-1,NS)  
                  SNDBT(L,KBT(L)-2)=SNDBT(L,KBT(L)-2)  
     &                +SNDB(L,KBT(L)-2,NS)  
                  SNDB(L,KBT(L)-1,NS)=SNDB(L,KBT(L),NS)  
                  SNDBT(L,KBT(L)-1)=SNDBT(L,KBT(L)-1)  
     &                +SNDB(L,KBT(L)-1,NS)  
                  SNDB(L,KBT(L),NS)=0.0  
                  SNDBT(L,KBT(L))=0.0  
                  STFPOCB(L,KBT(L),NS+NSED)=0.0  
                  VFRBED(L,KBT(L),NS+NSED)=0.0  
                ENDDO  
              ENDIF  
              IF(ISTRAN(5).GE.1)THEN  
                DO NT=1,NTOX  
                  TOXB(L,KBT(L)-2,NT)=TOXB(L,KBT(L)-2,NT)  
     &                +TOXB(L,KBT(L)-1,NT)  
                  TOXB(L,KBT(L)-1,NT)=TOXB(L,KBT(L),NT)  
                  TOXB(L,KBT(L),NT)=0.0  
                ENDDO  
              ENDIF  
              KBT(L)=KBT(L)-1  
              WRITE(1,1113)N,IL(L),JL(L),(HBED(L,K),K=1,KB)  
C  
C *** PMC BEGIN BLOCK  
C  
            ELSEIF(HBED(L,KBT(L)).LT.0.0)THEN  
              ! *** ZERO NEGATIVE THICKNESSES
              HBED(L,KBT(L))=0.0  
              VDRBED(L,KBT(L))=0.0  
              PORBED(L,KBT(L))=0.0  
              STDOCB(L,KBT(L))=0.0  
              STPOCB(L,KBT(L))=0.0  
C  
C *** PMC END BLOCK  
C  
            ENDIF  
          ENDIF  
        ENDDO  
C  
C ++ UPDATE BULK DENSITY  
C  
        !DO K=1,KB  
          DO L=2,LA  
            K=KBT(L)  ! PMC - REMOVED LOOP AND ONLY COMPUTE THE TOP LAYER
            IF(HBED(L,K).GT.0.)THEN  
              BDENBED(L,K)=1000.*PORBED(L,K)  
     &            +0.001*(SEDBT(L,K)+SNDBT(L,K))/HBED(L,K)  
            ELSE  
              BDENBED(L,K)=0.  
            ENDIF  
          ENDDO  
        !ENDDO  
      ENDIF  
 1110 FORMAT('INT  ',I10,2I5,12F10.6)  
 1111 FORMAT('ADD  ',I10,2I5,12F10.6)  
 1112 FORMAT('REZ  ',I10,2I5,12F10.6)  
 1113 FORMAT('RMV  ',I10,2I5,12F10.6)  
      CLOSE(1)  
      RETURN  
      END  

