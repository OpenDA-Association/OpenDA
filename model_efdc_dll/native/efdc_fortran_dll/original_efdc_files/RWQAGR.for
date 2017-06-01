      SUBROUTINE RWQAGR(TIMTMP)  
C  
C CHANGE RECORD  
C READ IN SPATIALLY AND/OR TEMPORALLY VARYING PARAMETERS FOR ALGAL  
C GROWTH, RESP. & PREDATION RATES, AND BASE LIGHT EXTINCT. COEFF.  
C (UNIT INWQAGR).  
C  
      USE GLOBAL  
      CHARACTER TITLE(3)*79, AGRCONT*3  
      OPEN(7890,FILE=AGRFN,STATUS='UNKNOWN')  
      open(7891,FILE='WQALGGX.INP',STATUS='UNKNOWN')
      OPEN(2,FILE='WQ3D.OUT',STATUS='UNKNOWN',POSITION='APPEND')  
      IF(AGRDAY.EQ.0)THEN
        READ(7890,50) (TITLE(M),M=1,3)  
        WRITE(2,999)  
        WRITE(2,50) (TITLE(M),M=1,3)  
!       Also write X-species if present
        if (NXSP.gt.0) then
          READ(7891,50) (TITLE(M),M=1,3)  
          write(2,*) '%%%% X-Species START %%%%'
          WRITE(2,50) (TITLE(M),M=1,3)  
          write(2,*) '%%%% X-Species END %%%%'
        endif
      ENDIF
      
      WRITE(2,60)'* ALGAL KINETIC VALUE AT', TIMTMP,   ! GEOSR DAY read jgcho 2016.10.06
     &    ' TH DAY FROM MODEL START'  
      READ(7890,999)  
      READ(7890,50) TITLE(1)  
      WRITE(2,50) TITLE(1)  
      DO I=1,IWQZ  
C  
C        READ(1,51) MM,WQPMC(I),WQPMD(I),WQPMG(I),WQBMRC(I),  
C  
        READ(7890,*) MM, WQPMC(I),WQPMD(I),WQPMG(I),WQPMM(I),WQBMRC(I),  
     &      WQBMRD(I),WQBMRG(I),WQBMRM(I),WQPRRC(I),WQPRRD(I),  
     &      WQPRRG(I),WQPRRM(I),WQKEB(I)  
        WRITE(2,51) MM, WQPMC(I),WQPMD(I),WQPMG(I),WQPMM(I),WQBMRC(I),  
     &      WQBMRD(I),WQBMRG(I),WQBMRM(I),WQPRRC(I),WQPRRD(I),  
     &      WQPRRG(I),WQPRRM(I),WQKEB(I)  
      ENDDO  
      READ(7890,*) AGRDAY, AGRCONT  
      WRITE(2,*) AGRDAY, AGRCONT
      ! Repeat for x-species if present
      if (NXSP.gt.0) then
        WRITE(2,*) '%%%% X-Species START %%%%'
        WRITE(2,60)'* ALGAL KINETIC VALUE AT', TIMTMP,  
     &    ' TH DAY FROM MODEL START'  
        READ(7891,999)  
        READ(7891,50) TITLE(1)  
        DO I=1,IWQZ  
         READ(7891,*) MM, (WQPMX(I,nsp),nsp=1,NXSP)
     &      ,(WQBMRX(I,nsp),nsp=1,NXSP),(WQPRRX(I,nsp),nsp=1,NXSP)
         WRITE(2,51) MM, (WQPMX(I,nsp),nsp=1,NXSP)
     &      ,(WQBMRX(I,nsp),nsp=1,NXSP),(WQPRRX(I,nsp),nsp=1,NXSP)
        ENDDO
        READ(7891,*) AGRDAY, AGRCONT  
        WRITE(2,*) AGRDAY, AGRCONT
        write(2,*) '%%%% X-Species END %%%%'
      endif
      IF(AGRCONT.EQ.'END')THEN  
        CLOSE(7890)  
        if (NXSP.gt.0) CLOSE(7891)
        IWQAGR = 0  
      ENDIF  
      CLOSE(2)  
  999 FORMAT(1X)  
   50 FORMAT(A79)  
   51 FORMAT(I8, 100F8.3)  ! Note, this might need some attention
   52 FORMAT(I7, 1X, A3)  
   60 FORMAT(/, A24, F5.1, A24)  
      RETURN  
      END  

