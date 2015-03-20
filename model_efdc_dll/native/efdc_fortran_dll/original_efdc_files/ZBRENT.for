      FUNCTION ZBRENT(ISMERR)  
C  
C USING BRENT'S METHOD, FIND THE ROOT OF A FUNC SEDFLUX KNOWN TO LIE  
C   BETWEEN RMIN & RMAX WITHIN AN ACCURACY OF TOL (P. 253 IN NUMERICAL  
C   RECIPE).  
C  
      EXTERNAL SEDFLUX  
      PARAMETER (IZMAX=100,EPS=3.0E-8,TOL=1.0E-5,  
     &    RMIN=1.0E-4,RMAX=100.0)  
      ISMERR = 0  
      A = RMIN  
      B = RMAX  
      FA = SEDFLUX(A)  
      FB = SEDFLUX(B)  
      ZBRENT = 0
      IF(FA*FB.GT.0.0)THEN  
        ISMERR = 1  
        RETURN  
      ENDIF  
      FC = FB  
      DO II=1,IZMAX  
        IF(FB*FC.GT.0.0)THEN  
          C = A  
          FC = FA  
          D = B-A  
          E = D  
        ENDIF  
        IF(ABS(FC).LT.ABS(FB))THEN  
          A = B  
          B = C  
          C = A  
          FA = FB  
          FB = FC  
          FC = FA  
        ENDIF  
        TOL1 = 2.0*EPS*ABS(B) + 0.5*TOL  
        XM = 0.5 * (C-B)  
        IF(ABS(XM).LE.TOL1 .OR. FB.EQ.0.0)THEN  
          ZBRENT = B  
          RETURN  
        ENDIF  
        IF(ABS(E).GE.TOL1 .AND. ABS(FA).GT.ABS(FB))THEN  
          S = FB / FA  
          IF(A.EQ.C)THEN  
            P = 2.0 * XM * S  
            Q = 1.0 - S  
          ELSE  
            Q = FA / FC  
            R = FB / FC  
            P = S * (2.0*XM*Q*(Q-R) - (B-A)*(R-1.0))  
            Q = (Q-1.0) * (R-1.0) * (S-1.0)  
          ENDIF  
          IF(P.GT.0.0) Q = -Q  
          P = ABS(P)  
          IF(2.0*P .LT. MIN(3.0*XM*Q-ABS(TOL1*Q), ABS(E*Q)))THEN  
            E = D  
            D = P / Q  
          ELSE  
            D = XM  
            E = D  
          ENDIF  
        ELSE  
          D = XM  
          E = D  
        ENDIF  
        A = B  
        FA = FB  
        IF(ABS(D).GT.TOL1)THEN  
          B = B + D  
        ELSE  
          B = B + SIGN(TOL1,XM)  
        ENDIF  
        FB = SEDFLUX(B)  
      ENDDO  
      ISMERR = 2  
      ZBRENT = B  
      RETURN  
      END  

