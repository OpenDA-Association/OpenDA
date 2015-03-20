      SUBROUTINE SEEK(TAG)  
C
      CHARACTER TAG*(*)  
      CHARACTER*80 TEXT  
C
      L=LEN(TAG)  
      DO I=1,L  
        J=ICHAR(TAG(I:I))  
        IF(97.LE.J.AND.J.LE.122)THEN  
          TAG(I:I)=CHAR(J-32)  
        ENDIF  
      ENDDO  
      WRITE(7,'(A,A)')'SEEKING GROUP: ',TAG  
      DO K=1,2  
   10   READ(1,'(A)',END=20)TEXT  
        M=MAX(1,LEN_TRIM(TEXT))  
        WRITE(7,'(A)')TEXT(1:M)  
        DO WHILE(M.GT.L.AND.TEXT(1:1).EQ.'')  
          TEXT(1:M-1)=TEXT(2:M)  
          TEXT(M:M)=' '  
          M=M-1  
        ENDDO  
        IF(M.LT.L)GO TO 10  
        DO I=1,M  
          J=ICHAR(TEXT(I:I))  
          IF(97.LE.J.AND.J.LE.122)THEN  
            TEXT(I:I)=CHAR(J-32)  
          ENDIF  
        ENDDO  
        IF(TEXT(1:L).NE.TAG)GO TO 10  
        IF(TEXT(L+1:L+1).NE.' ')GO TO 10  
      ENDDO  
      RETURN  
C
   20 WRITE(*,'(A,A,A)')'GROUP: ',TAG,' NOT FOUND BEFORE END OF FILE'  
      STOP  
C
      END  

