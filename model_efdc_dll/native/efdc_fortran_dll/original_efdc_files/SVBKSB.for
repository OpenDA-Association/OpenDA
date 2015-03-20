      SUBROUTINE SVBKSB(U,W,V,M,N,MP,NP,B,X)  
C  
C **  FROM NUMERICAL RECIPES  
C CHANGE RECORD  
C  
      DIMENSION U(MP,NP),W(NP),V(NP,NP),B(MP),X(NP)  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::TMP  
      IF(.NOT.ALLOCATED(TMP))ALLOCATE(TMP(N))  
C  
      DO 12 J=1,N  
        S=0.  
        IF(W(J).NE.0.)THEN  
          DO 11 I=1,M  
            S=S+U(I,J)*B(I)  
   11     CONTINUE  
          S=S/W(J)  
        ENDIF  
        TMP(J)=S  
   12 CONTINUE  
      DO 14 J=1,N  
        S=0.  
        DO 13 JJ=1,N  
          S=S+V(J,JJ)*TMP(JJ)  
   13   CONTINUE  
        X(J)=S  
   14 CONTINUE  
      DEALLOCATE(TMP)  
      RETURN  
      END  

