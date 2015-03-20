C ***  
C *** READ IN TEMPORALLY VARYING POINT SOURCE INPUT (UNIT INWQPSL).  
C *** INPUT UNITS (KG/D) EXCEPT:  TAM(KMOL/D), FCB(MPN/D).  
C *** COMPUTATIONAL UNITS, WQ CONSTITUENT LOADS ARE IN G/DAY, 
C ***                      EXCEPT TAM IN (MOL/D) & FCB IN (MPN/D).  
C
      SUBROUTINE RWQPSL  
C  
C CHANGE RECORD  
C  
      USE GLOBAL    
      DIMENSION RLDTMP(NTSWQVM)  
C
      IF(ITNWQ.GT.0) GOTO 1000  
C  
C **  READ IN LOADING SERIES FROM FILE 'WQPSL.INP'  
C  
      IF( NPSTMSR.GE.1)THEN  
        OPEN(1,FILE=PSLFN,STATUS='UNKNOWN')  
        PRINT*,'READING WQPSL.INP'
C  
C **  SKIP OVER TITLE AND AND HEADER LINES  
C  
        DO IS=1,13  
          READ(1,1)  
        ENDDO  
        DO NS=1,NPSTMSR  
          MWQPTLT(NS)=1  
          READ(1,*,IOSTAT=ISO)MWQPSR(NS),TCWQPSR(NS),  
     &                        TAWQPSR,RMULADJ,ADDADJ  
          IF(ISO.GT.0) GOTO 900  

          ! *** CONVERT WQ VAR 1-19 FROM KG/D TO G/D
          ! *** CONVERT WQ VAR 20 (TAM) FROM KMOLS/D TO MOLES/D
          ! *** CONVERT FECAL COLIFORM FROM MPN/DAY TO MPN/D FOR FCM, 
          RMULADJ=1000.*RMULADJ  
          !ADDADJ=ADDADJ  

          DO M=1,MWQPSR(NS)  
            !  1) CHC - cyanobacteria 
            !  2) CHD - diatom algae 
            !  3) CHG - green algae 
            !  4) ROC - refractory particulate organic carbon 
            !  5) LOC - labile particulate organic carbon 
            !  6) DOC - dissolved organic carbon 
            !  7) ROP - refractory particulate organic phosphorus 
            !  8) LOP - labile particulate organic phosphorus 
            !  9) DOP - dissolved organic phosphorus 
            ! 10) P4D - total phosphate
            ! 11) RON - refractory particulate organic nitrogen 22) macroalgae
            ! 12) LON - labile particulate organic nitrogen
            ! 13) DON - dissolved organic nitrogen
            ! 14) NHX - ammonia nitrogen
            ! 15) NOX - nitrate nitrogen
            ! 16) SUU - particulate biogenic silica
            ! 17) SAA - dissolved available silica
            ! 18) COD - chemical oxygen demand
            ! 19) DOX - dissolved oxygen
            ! 20) TAM - total active metal
            ! 21) FCB - fecal coliform bacteria
            READ(1,*,IOSTAT=ISO)TWQPSER(M,NS),(RLDTMP(NW),NW=1,7)  
            IF(ISO.GT.0) GOTO 900  
            READ(1,*,IOSTAT=ISO)(RLDTMP(NW),NW=8,14)  
            IF(ISO.GT.0) GOTO 900  
            READ(1,*,IOSTAT=ISO)(RLDTMP(NW),NW=15,22)  ! PMC HARDWIRED FOR TENKILLER
            IF(ISO.GT.0) GOTO 900

            ! *** STANDARD CONVERSIONS
            TWQPSER(M,NS)=TWQPSER(M,NS)+TAWQPSR  
            DO NW=1,20  
              WQPSSER(M,NW,NS)=RMULADJ*RLDTMP(NW)  
            ENDDO  
            WQPSSER(M,21,NS)=RMULADJ*RLDTMP(21)/1000.  
          ENDDO  
        ENDDO  
        CLOSE(1)  
      ENDIF  
      GOTO 901  
  900 CONTINUE  
      WRITE(6,601)NS,M  
      STOP  
  901 CONTINUE  
    1 FORMAT(120X)  
  601 FORMAT(' READ ERROR WQPS TIME SERIES, NSER,MDATA = ',2I5)  
  602 FORMAT(' READ OF FILE WQPSL.INP SUCCESSFUL'/)  
 1000 CONTINUE  
C  
C **  INITIALIZE NULL SERIES LOADING TO ZERO  
C  
      DO NW=1,NWQV  
        WQPSSRT(NW,0)=0.  
      ENDDO  
C  
C **  LOADING SERIES INTERPOLTATION  
C  
      TIME=DT*FLOAT(N)+TCON*TBEGIN
      TIME=TIME/86400.  
      DO NS=1,NPSTMSR  
        IF(ISDYNSTP.EQ.0)THEN  
          TIME=DT*FLOAT(N)+TCON*TBEGIN  
          TIME=TIME/TCWQPSR(NS)  
        ELSE  
          TIME=TIMESEC/TCWQPSR(NS)  
        ENDIF  
        M1=MWQPTLT(NS)  
  100   CONTINUE  
        M2=M1+1  
        IF(TIME.GT.TWQPSER(M2,NS))THEN  
          M1=M2  
          GOTO 100  
        ELSE  
          MWQPTLT(NS)=M1  
        ENDIF  
        TDIFF=TWQPSER(M2,NS)-TWQPSER(M1,NS)  
        WTM1=(TWQPSER(M2,NS)-TIME)/TDIFF  
        WTM2=(TIME-TWQPSER(M1,NS))/TDIFF  
        DO NW=1,NWQV  
          WQPSSRT(NW,NS)=WTM1*WQPSSER(M1,NW,NS)  
     &        +WTM2*WQPSSER(M2,NW,NS)  
        ENDDO  
      ENDDO  
C
      IF(ITNWQ.EQ.0.AND.DEBUG)THEN  
        OPEN(1,FILE='WQPSLT.DIA',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='WQPSLT.DIA',STATUS='UNKNOWN')  
        WRITE(1,112)N,TIME  
        DO NS=1,NPSTMSR  
          WRITE(1,111)NS,(WQPSSRT(NW,NS),NW=1,NWQV)  
        ENDDO  
        CLOSE(1)  
      ENDIF  
C  
C **  COMBINE CONSTANT AND TIME VARIABLE PS LOADS  
C M.R. MORTON 02/20/1999  
C MODIFIED SO MULTIPLE POINT SOURCES CAN BE ADDED TO ANY GRID CELL  
C AND ANY LAYER (HAD TO CHANGE WQWPSL ARRAY FROM 2D TO 3D).  
C  
      IF(ITNWQ.EQ.0)THEN  
        DO NW=1,NWQV  
          DO K=1,KC  
            DO L=2,LA  
              WQWPSL(L,K,NW) = 0.0  
            ENDDO  
          ENDDO  
        ENDDO  

        OPEN(1,FILE='WQPSL.DIA',STATUS='UNKNOWN')  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE='WQPSL.DIA',STATUS='UNKNOWN')  
        WRITE(1,112)N,TIME  
      ENDIF  

      ! *** ZERO THE ACTIVE BOUNDARY CELLS
      DO NS=1,IWQPS  
        L = LIJ(ICPSL(NS), JCPSL(NS))  
        K = KCPSL(NS)
        ! *** DSLLC BEGIN BLOCK
        IF(K.GE.1)THEN 
          DO NW=1,NWQV  
		    WQWPSL(L,K,NW) = 0.0  
          ENDDO
        ELSE
          DO K=1,KC
            DO NW=1,NWQV  
		      WQWPSL(L,K,NW) = 0.0  
            ENDDO
          ENDDO
        ENDIF	
        ! *** DSLLC END BLOCK
      ENDDO

C *** LOOP OVER THE WQ BOUNDARY CELLS
      DO NS=1,IWQPS  
        L = LIJ(ICPSL(NS), JCPSL(NS))  
        K = KCPSL(NS)  
        ITMP = MVPSL(NS)  
        IF(ITNWQ.EQ.0) WRITE(1,121)NS,L,ICPSL(NS),JCPSL(NS),K,ITMP  
        IF(K.GE.1)THEN  
          ! *** K>0, ASSIGN A SPECIFIC LAYER 
          DO NW=1,NWQV  
            WQWPSL(L,K,NW) = WQWPSL(L,K,NW)  
     &          + WQWPSLC(NS,NW) + WQPSSRT(NW,ITMP)  
          ENDDO  
        ELSE
          ! *** K=0, DISTRIBUTE OVER ALL THE LAYERS  
          TMPVAL=1./FLOAT(KC)  
          DO KK=1,KC  
            DO NW=1,NWQV  
              WQWPSL(L,KK,NW) = WQWPSL(L,KK,NW)  
     &            + TMPVAL*( WQWPSLC(NS,NW) + WQPSSRT(NW,ITMP) )  
            ENDDO  
          ENDDO  
        ENDIF  
      ENDDO  

      IF(ITNWQ.EQ.0)THEN  
        DO L=2,LA  
          ITMP=IWQPSC(L,1)  
          IF(ITMP.GT.0)THEN  
            DO K=1,KC  
              WRITE(1,110)ITMP,IL(L),JL(L),K,(WQWPSL(L,K,NW),NW=1,NWQV)  
            ENDDO  
          ENDIF  
        ENDDO  
        CLOSE(1)  
      ENDIF  
  110 FORMAT(1X,4I4,2X,7E12.4,/,19X,7E12.4,/,19X,7E12.4)  
  111 FORMAT(1X,I4,2X,7E12.4,/,7X,7E12.4,/,7X,7E12.4)  
  112 FORMAT(' N, TIME = ', I10, F12.5/)  
  121 FORMAT(' NS,L,I,J,K,ITMP = ', 6I5/)  
      RETURN  
      END  

