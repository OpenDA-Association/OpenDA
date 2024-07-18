      FUNCTION SETSTVEL(D,SSG)  
          REAL WSET
          WSET=0.0
C  
C CHANGE RECORD  
C  
C  
C **  NONCOHEASIVE SEDIMENT SETTLING AND SHIELDS CRITERIA  
C **  USING VAN RIJN'S EQUATIONS  
C  
      VISC=1.E-6  
      GP=(SSG-1.)*9.82  
      GPD=GP*D  
      SQGPD=SQRT(GPD)  
      RD=SQGPD*D/VISC  
C  
C **  SETTLING VELOCITY  
C  
      IF(D.LT.1.0E-4)THEN  
        WSET=SQGPD*RD/18.  
      ENDIF  
      IF(D.GE.1.0E-4.AND.D.LT.1.E-3)THEN  
        TMP=SQRT(1.+0.01*RD*RD)-1.  
        WSET=10.0*SQGPD*TMP/RD  
      ENDIF  
      IF(D.GE.1.E-3)THEN  
        WSET=1.1*SQGPD  
      ENDIF  
      SETSTVEL=WSET  
      RETURN  
      END  

