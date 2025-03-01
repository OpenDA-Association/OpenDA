      FUNCTION CSEDRESS(DENBULK,WRSPO,VDRO,VDR,VDRC,IOPT)  
C  
C CHANGE RECORD  
C  
C  
C **  CALCULATES SURFACE EROSION RATE OF COHESIVE  
C **  SEDIMENT AS A FUNCTION OF BED BULK DENSITY  
C **  IOPT=1  BASED ON  
C **  
C **  HWANG, K. N., AND A. J. MEHTA, 1989: FINE SEDIMENT ERODIBILITY  
C **  IN LAKE OKEECHOBEE FLORIDA. COASTAL AND OCEANOGRAPHIC ENGINEERING  
C **  DEPARTMENT, UNIVERSITY OF FLORIDA, GAINESVILLE, FL32661  
C **  IOPT=2  BASED ON J. M. HAMRICK'S MODIFICATION OF  
C **  
C **  SANFORD, L.P., AND J. P. Y. MAA, 2001: A UNIFIED EROSION FORMULATI  
C **  FOR FINE SEDIMENT, MARINE GEOLOGY, 179, 9-23.  
C  
      CSEDRESS=0.0
      IF(IOPT.EQ.1)THEN  
        BULKDEN=0.001*DENBULK    ! *** PMC
        IF(BULKDEN.LE.1.065)THEN  
          CSEDRESS=0.62  
        ELSE  
          TMP=0.198/(BULKDEN-1.0023)  
          TMP=EXP(TMP)  
          CSEDRESS=6.4E-4*(10.**TMP)  
        ENDIF  
      ELSEIF(IOPT.EQ.2)THEN  
        CSEDRESS=WRSPO*(1.+VDRO)/(1.+VDR)  
      ELSEIF(IOPT.EQ.3)THEN  
        CSEDRESS=WRSPO*(1.+VDRO)/(1.+VDRC)  
      ELSEIF(IOPT.GE.99)THEN  
        CSEDRESS=WRSPO  
      ENDIF
  
      RETURN  
      END  

