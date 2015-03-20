      SUBROUTINE BEDLOAD(NX,NS)  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSND CALCULATES NONCOHESIVER SEDIMENT SETTLING,  
C **  DEPOSITION AND RESUSPENSION AND IS CALLED FOR SSEDTOX  
C  
      USE GLOBAL  

	IMPLICIT NONE
	INTEGER::L,NSB,LUTMP,LDTMP,LS,LN,NX,NS
	REAL::SLOPE,UVARTMP,VVARTMP,ASNDFBL,SNDBTMP,SNDFBLM
	REAL::FLUXFAC,SNDFBLTOT,VCELLCTRM,QSBLLDXY,FSBDLD
	REAL::UCELLCTRM,SHIELDS,BDLDTMPB,CSHIELDSC,FSEDMODE
	REAL::BDLDTMPA,CSHIELDS,TMPVAL,BDLDTMPP,BDLDTMP,VELMAG
C  
C **  BED LOAD TRANSPORT HORIZONTAL LOOP  
C **  INITIALIZE BED LOAD TRANSPORTS  
C  
      DO L=1,LC  
        QSBDLDP(L)=0.  
        QSBDLDX(L,NX)=0.  
        QSBDLDY(L,NX)=0.  
        QSBDLDOT(L,NX)=0.  
        QSBDLDIN(L,NX)=0.  
        SNDFBL(L,NX)=0.  
        RBPSBL(L)=1.0  
      ENDDO  
      IF(ISBDLDBC.EQ.0)RETURN  
C  
C **  CALCULATE CELL CENTER TRANSPORT RATES USING GENERIC BED LOAD EQUAT  
C  
      IF(ISBDLD(NS).EQ.0)THEN  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            CSHIELDS=TCSHIELDS(NS)  
            IF(ISEDEFF.EQ.2)THEN  
              TMPVAL=1.+(COEHEFF2-1.)  
     &            *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
              CSHIELDS=TMPVAL*CSHIELDS  
            ENDIF  
            BDLDTMPP=SBDLDP  
            BDLDTMP=SQRT(GPDIASED)*DIASED/DSEDGMM  
            IF(BDLDTMPP.GT.0.0)THEN  
              FACBEDL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),
     &                   RSNDM(NX),ISNDM1(NX),ISNDM2(NX),1)
              SHIELDS=TAUBSND(L)/GPDIASED  
              IF(SHIELDS.GT.CSHIELDS)THEN  
                IF(SBDLDA.GT.0.0)THEN  
                  BDLDTMPA=(SBDLDG1*SHIELDS-SBDLDG2*CSHIELDS)**SBDLDA  
                ELSE  
                  BDLDTMPA=1.0  
                ENDIF  
                IF(SBDLDB.GT.0.0)THEN  
                  BDLDTMPB=(SBDLDG3*SQRT(SHIELDS)  
     &                -SBDLDG4*SQRT(CSHIELDS))**SBDLDB  
                ELSE  
                  BDLDTMPB=1.0  
                ENDIF  
                QSBDLDP(L)=FACBEDL(L)*VFRBED(L,KBT(L),NS)*BDLDTMP*  
     &              BDLDTMPP*BDLDTMPA*BDLDTMPB  
                IF(ISEDEFF.EQ.1)THEN  
                  TMPVAL=EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
                  QSBDLDP(L)=TMPVAL*QSBDLDP(L)  
                ENDIF  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE CELL CENTER TRANSPORT RATES USING VAN RIJN BED LOAD EQUA  
C  
      IF(ISBDLD(NS).EQ.1)THEN  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            CSHIELDS=TCSHIELDS(NS)  
            IF(ISEDEFF.EQ.2)THEN  
              TMPVAL=1.+(COEHEFF2-1.)  
     &            *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
              CSHIELDS=TMPVAL*CSHIELDS  
            ENDIF  
            IF(ISNDAL.EQ.1)THEN  
              TMPVAL=LOG10(19.*DIASED/SEDDIA50(L,KBT(L)))  
              TMPVAL=1.66667/(TMPVAL**2)  
              CSHIELDSC=CSHIELDS50(L)*TMPVAL  
            ELSE  
              CSHIELDSC=TCSHIELDS(NS)  
            END IF  
            IF(ISEDEFF.EQ.2)THEN  
              TMPVAL=1.+(COEHEFF2-1.)  
     &            *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
              CSHIELDSC=TMPVAL*CSHIELDS  
            ENDIF  
            BDLDTMPP=FSBDLD(DIASED,GPDIASED,SEDDIA50(L,KBT(L)),HP(L),  
     &          PEXP(L,NX),PHID(L,NX),CSHIELDS,SBDLDP,ISBDLD(NS))  
            IF(ISNDAL.EQ.1)THEN  
              BDLDTMPP=((DIASED/SEDDIA50(L,KBT(L)))**0.3)*BDLDTMPP  
            ENDIF  
            BDLDTMP=SQRT(GPDIASED)*DIASED/DSEDGMM  
            FACBEDL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),
     &                 RSNDM(NX),ISNDM1(NX),ISNDM2(NX),1)
            SHIELDS=TAUBSND(L)/GPDIASED  
            IF(SHIELDS.GT.CSHIELDSC.OR.ISBDLD(NS).GT.1)THEN  
              BDLDTMPA=(SBDLDG1*SHIELDS-SBDLDG2*CSHIELDSC)**SBDLDA  
              QSBDLDP(L)=FACBEDL(L)*VFRBED(L,KBT(L),NS)*BDLDTMP*  
     &            BDLDTMPP*BDLDTMPA  
              IF(ISEDEFF.EQ.1)THEN  
                TMPVAL=EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
                QSBDLDP(L)=TMPVAL*QSBDLDP(L)  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE CELL CENTER TRANSPORT RATES USING ENGELUND-HANSEN  
C  
      IF(ISBDLD(NS).EQ.2)THEN  
        IF(IBLTAUC(NS).EQ.0) CSHIELDS=0.  
        IF(IBLTAUC(NS).EQ.1) CSHIELDS=TCSHIELDS(NS)  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(IBLTAUC(NS).EQ.2)  
     &          CSHIELDS=SEDDIA50(L,KBT(L))*CSHIELDS50(L)/DIASED  
            IF(IBLTAUC(NS).EQ.3)  
     &          CSHIELDS=0.03*((PHID(L,NX)/PEXP(L,NX))**0.6)  
            IF(ISEDEFF.EQ.2)THEN  
              TMPVAL=1.+(COEHEFF2-1.)  
     &            *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
              CSHIELDS=TMPVAL*CSHIELDS  
            ENDIF  
            SHIELDS=TAUBSND(L)/GPDIASED  
            IF(SHIELDS.GE.CSHIELDS)THEN  
              BDLDTMPP=FSBDLD(DIASED,GPDIASED,SEDDIA50(L,KBT(L)),HP(L),  
     &            PEXP(L,NX),PHID(L,NX),CSHIELDS,SBDLDP,ISBDLD(NS))  
              IF(HGDH(L).GT.0.0) BDLDTMPP=BDLDTMPP/(HGDH(L)**0.333)  
              BDLDTMP=SQRT(GPDIASED)*DIASED/DSEDGMM  
              FACBEDL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),
     &                   RSNDM(NX),ISNDM1(NX),ISNDM2(NX),1)
              BDLDTMPA=(SBDLDG1*SHIELDS)**SBDLDA  
              QSBDLDP(L)=FACBEDL(L)*VFRBED(L,KBT(L),NS)*BDLDTMP*  
     &            BDLDTMPP*BDLDTMPA  
              IF(ISEDEFF.EQ.1)THEN  
                TMPVAL=EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
                QSBDLDP(L)=TMPVAL*QSBDLDP(L)  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE CELL CENTER TRANSPORT RATES USING WU, WANG, AND JIA  
C  
      IF(ISBDLD(NS).EQ.3)THEN  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            CSHIELDS=TCSHIELDS(NS)  
            BDLDTMPP=FSBDLD(DIASED,GPDIASED,SEDDIA50(L,KBT(L)),HP(L),  
     &          PEXP(L,NX),PHID(L,NX),CSHIELDS,SBDLDP,ISBDLD(NS))  
            CSHIELDS=0.03*((PHID(L,NX)/PEXP(L,NX))**0.6)  
            IF(ISEDEFF.EQ.2)THEN  
              TMPVAL=1.+(COEHEFF2-1.)  
     &            *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
              CSHIELDS=TMPVAL*CSHIELDS  
            ENDIF  
            BDLDTMP=SQRT(GPDIASED)*DIASED/DSEDGMM  
            FACBEDL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),
     &                 RSNDM(NX),ISNDM1(NX),ISNDM2(NX),1)
            SHIELDS=TAUBSND(L)/GPDIASED  
            IF(SHIELDS.GT.CSHIELDS)THEN  
              BDLDTMPA=(SBDLDG1*SHIELDS-SBDLDG2*CSHIELDS)**SBDLDA  
              QSBDLDP(L)=FACBEDL(L)*VFRBED(L,KBT(L),NS)*BDLDTMP*  
     &            BDLDTMPP*BDLDTMPA  
              IF(ISEDEFF.EQ.1)THEN  
                TMPVAL=EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
                QSBDLDP(L)=TMPVAL*QSBDLDP(L)  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE CELL FACE TRANSPORT RATES BY DOWN WIND PROJECTION  
C  
      IF(ISBLFUC.EQ.0)THEN  
        DO L=2,LA  
          QSBDLDOT(L,NX)=0.  
          QSBDLDIN(L,NX)=0.  
          IF(LMASKDRY(L))THEN  
            LN=LNC(L)  
            IF(UCELLCTR(L).GE.0.0.AND.VCELLCTR(L).GE.0.0)THEN  
              QSBDLDX(L+1,NX)=SUB(L+1)*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(LN ,NX)=SVB(LN )*QSBDLDP(L)*VCELLCTR(L)  
            ENDIF  
            IF(UCELLCTR(L).GE.0.0.AND.VCELLCTR(L).LT.0.0)THEN  
              QSBDLDX(L+1,NX)=SUB(L+1)*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(L  ,NX)=SVB(L  )*QSBDLDP(L)*VCELLCTR(L)  
            ENDIF  
            IF(UCELLCTR(L).LT.0.0.AND.VCELLCTR(L).GE.0.0)THEN  
              QSBDLDX(L  ,NX)=SUB(L  )*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(LN ,NX)=SVB(LN )*QSBDLDP(L)*VCELLCTR(L)  
            ENDIF  
            IF(UCELLCTR(L).LT.0.0.AND.VCELLCTR(L).LT.0.0)THEN  
              QSBDLDX(L  ,NX)=SUB(L  )*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(L  ,NX)=SVB(L  )*QSBDLDP(L)*VCELLCTR(L)  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE CELL FACE TRANSPORT RATES BY DOWN WIND PROJECTION  
C **  WITH CORNER EFFECTS CORRECTION  
C  
      IF(ISBLFUC.EQ.1)THEN  
        DO L=2,LA  
          QSBDLDOT(L,NX)=0.  
          QSBDLDIN(L,NX)=0.  
          IF(LMASKDRY(L))THEN  
            LN=LNC(L)  
            IF(UCELLCTR(L).GE.0.0.AND.VCELLCTR(L).GE.0.0)THEN  
              UCELLCTRM=SUB(L+1)*ABS(UCELLCTR(L))  
              VCELLCTRM=SVB(LN )*ABS(VCELLCTR(L))  
              QSBDLDX(L+1,NX)=SUB(L+1)*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(LN ,NX)=SVB(LN )*QSBDLDP(L)*VCELLCTR(L)  
              IF(UCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDY(LN ,NX)=SVB(LN )*SIGN(QSBDLDP(L),VCELLCTR(L))  
              ENDIF  
              IF(VCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDX(L+1,NX)=SUB(L+1)*SIGN(QSBDLDP(L),UCELLCTR(L))  
              ENDIF  
            ENDIF  
            IF(UCELLCTR(L).GE.0.0.AND.VCELLCTR(L).LT.0.0)THEN  
              UCELLCTRM=SUB(L+1)*ABS(UCELLCTR(L))  
              VCELLCTRM=SVB(L  )*ABS(VCELLCTR(L))  
              QSBDLDX(L+1,NX)=SUB(L+1)*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(L  ,NX)=SVB(L  )*QSBDLDP(L)*VCELLCTR(L)  
              IF(UCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDY(L  ,NX)=SVB(L  )*SIGN(QSBDLDP(L),VCELLCTR(L))  
              ENDIF  
              IF(VCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDX(L+1,NX)=SUB(L+1)*SIGN(QSBDLDP(L),UCELLCTR(L))  
              ENDIF  
            ENDIF  
            IF(UCELLCTR(L).LT.0.0.AND.VCELLCTR(L).GE.0.0)THEN  
              UCELLCTRM=SUB(L  )*ABS(UCELLCTR(L))  
              VCELLCTRM=SVB(LN )*ABS(VCELLCTR(L))  
              QSBDLDX(L  ,NX)=SUB(L  )*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(LN ,NX)=SVB(LN )*QSBDLDP(L)*VCELLCTR(L)  
              IF(UCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDY(LN ,NX)=SVB(LN )*SIGN(QSBDLDP(L),VCELLCTR(L))  
              ENDIF  
              IF(VCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDX(L  ,NX)=SUB(L  )*SIGN(QSBDLDP(L),UCELLCTR(L))  
              ENDIF  
            ENDIF  
            IF(UCELLCTR(L).LT.0.0.AND.VCELLCTR(L).LT.0.0)THEN  
              UCELLCTRM=SUB(L  )*ABS(UCELLCTR(L))  
              VCELLCTRM=SVB(L  )*ABS(VCELLCTR(L))  
              QSBDLDX(L  ,NX)=SUB(L  )*QSBDLDP(L)*UCELLCTR(L)  
              QSBDLDY(L  ,NX)=SVB(L  )*QSBDLDP(L)*VCELLCTR(L)  
              IF(UCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDY(L  ,NX)=SVB(L  )*SIGN(QSBDLDP(L),VCELLCTR(L))  
              ENDIF  
              IF(VCELLCTRM.LT.1.0E-9)THEN  
                QSBDLDX(L  ,NX)=SUB(L  )*SIGN(QSBDLDP(L),UCELLCTR(L))  
              ENDIF  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE CELL FACE TRANSPORT RATES BY AVERAGING  
C **  VECTOR COMPONENTS FROM CELL CENTERS TO FACES  
C  
      IF(ISBLFUC.EQ.2)THEN  
        DO L=2,LA  
          QSBDLDOT(L,NX)=0.  
          QSBDLDIN(L,NX)=0.  
          LS=LSC(L)  
          QSBDLDX(L,NX)=0.5*SUB(L)*(QSBDLDP(L)*UCELLCTR(L)  
     &        +QSBDLDP(L-1)*UCELLCTR(L+1))  
          QSBDLDY(L,NX)=0.5*SVB(L)*(QSBDLDP(L)*VCELLCTR(L)  
     &        +QSBDLDP(LS )*UCELLCTR(LS ))  
        ENDDO  
      ENDIF  
C  
C **  CONVERT TRANSPORT VECTORS TO FACE VECTORS  
C  
      DO L=2,LA  
        IF(LMASKDRY(L))THEN  
          QSBDLDX(L,NX)=SUB(L)*DYU(L)*QSBDLDX(L,NX)  
          QSBDLDY(L,NX)=SVB(L)*DXV(L)*QSBDLDY(L,NX)  
        ENDIF  
      ENDDO  
C  
C **  ELIMINATE BEDLOAD TRANSPORT UP ADVERSE SLOPES IN DIRECTION OF FLOW  
C  
      IF(BLBSNT.GT.0.0)THEN  
        DO L=2,LA  
          IF(LMASKDRY(L))THEN  
            IF(QSBDLDX(L,NX).GT.0.0)THEN  
              SLOPE=(BELV(L)-BELV(L-1))*DXIU(L)  
              IF(SLOPE.GT.BLBSNT) QSBDLDX(L,NX)=0.0  
            ENDIF  
            IF(QSBDLDX(L,NX).LT.0.0)THEN  
              SLOPE=(BELV(L-1)-BELV(L))*DXIU(L)  
              IF(SLOPE.GT.BLBSNT) QSBDLDX(L,NX)=0.0  
            ENDIF  
            IF(QSBDLDY(L,NX).GT.0.0)THEN  
              SLOPE=(BELV(L)-BELV(LSC(L)))*DYIV(L)  
              IF(SLOPE.GT.BLBSNT) QSBDLDY(L,NX)=0.0  
            ENDIF  
            IF(QSBDLDY(L,NX).LT.0.0)THEN  
              SLOPE=(BELV(LSC(L))-BELV(L))*DYIV(L)  
              IF(SLOPE.GT.BLBSNT) QSBDLDY(L,NX)=0.0  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  INSERT OUTFLOW OR RECIRCULATION BOUNDARY CONDITION FOR BED  
C **  BED LOAD TRANSPORT  
C     HAMRICK FIXED TO ADD MULTIPLICATION BY WIDTH OF OUT AND INFLOW SEC  
C     IN 0819 CODE PATCH  
C  
      IF(NSBDLDBC.GT.0) THEN 
        QSBLLDXY=0.0
        DO NSB=1,NSBDLDBC
          LUTMP=LSBLBCU(NSB)
          LDTMP=LSBLBCD(NSB)
          IF(LDTMP.GT.0) THEN
C           OUTFLOW ON POSITIVE X FACE RECIRCULATED TO NEGATIVE X FACE
            IF(ISDBLDIR(NSB).EQ.1)THEN
	        IF(UCELLCTR(LUTMP).GT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                DYP(LUTMP)*QSBDLDP(LUTMP)*UCELLCTR(LUTMP)
                QSBDLDIN(LDTMP,NX)=QSBDLDOT(LUTMP,NX)
              ENDIF
            ENDIF
C           OUTFLOW ON NEGATIVE X FACE RECIRCULATED TO POSITIVE X FACE
            IF(ISDBLDIR(NSB).EQ.-1)THEN
	        IF(UCELLCTR(LUTMP).LT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                -DYP(LUTMP)*QSBDLDP(LUTMP)*UCELLCTR(LUTMP)
                QSBDLDIN(LDTMP,NX)=QSBDLDOT(LUTMP,NX)
              ENDIF
            ENDIF
C           OUTFLOW ON POSITIVE Y FACE RECIRCULATED TO NEGATIVE Y FACE
            IF(ISDBLDIR(NSB).EQ.2)THEN
	        IF(VCELLCTR(LUTMP).GT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                DXP(LUTMP)*QSBDLDP(LUTMP)*VCELLCTR(LUTMP)
                QSBDLDIN(LDTMP,NX)=QSBDLDOT(LUTMP,NX)
              ENDIF
            ENDIF
C           OUTFLOW ON NEGATIVE Y FACE RECIRCULATED TO POSITIVE Y FACE
            IF(ISDBLDIR(NSB).EQ.-2)THEN
	        IF(VCELLCTR(LUTMP).LT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                -DXP(LUTMP)*QSBDLDP(LUTMP)*VCELLCTR(LUTMP)
                QSBDLDIN(LDTMP,NX)=QSBDLDOT(LUTMP,NX)
              ENDIF
            ENDIF
          ELSE
C           OUTFLOW ON POSITIVE X FACE 
            IF(ISDBLDIR(NSB).EQ.1)THEN
	        IF(UCELLCTR(LUTMP).GT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                DYP(LUTMP)*QSBDLDP(LUTMP)*UCELLCTR(LUTMP)
              ENDIF
            ENDIF
C           OUTFLOW ON NEGATIVE X FACE 
            IF(ISDBLDIR(NSB).EQ.-1)THEN
	        IF(UCELLCTR(LUTMP).LT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                -DYP(LUTMP)*QSBDLDP(LUTMP)*UCELLCTR(LUTMP)
              ENDIF
            ENDIF
C           OUTFLOW ON POSITIVE X FACE 
            IF(ISDBLDIR(NSB).EQ.2)THEN
	        IF(VCELLCTR(LUTMP).GT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                DXP(LUTMP)*QSBDLDP(LUTMP)*VCELLCTR(LUTMP)
              ENDIF
            ENDIF
C           OUTFLOW ON NEGATIVE Y FACE 
            IF(ISDBLDIR(NSB).EQ.-2)THEN
	        IF(VCELLCTR(LUTMP).LT.0.0)THEN
                QSBDLDOT(LUTMP,NX)=
     &                -DXP(LUTMP)*QSBDLDP(LUTMP)*VCELLCTR(LUTMP)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
C  
C **  LIMIT OUTGOING FLUXES IN EACH CELL  
C  
      DO L=2,LA  
        IF(LMASKDRY(L))THEN  
          LN=LNC(L)  
          IF(UCELLCTR(L).GE.0.0.AND.VCELLCTR(L).GE.0.0)THEN  
            SNDFBL(L,NX)=DXYIP(L)*(QSBDLDX(L+1,NX)+QSBDLDY(LN,NX)  
     &          +QSBDLDOT(L,NX))  
            ASNDFBL=ABS(SNDFBL(L,NX))  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDFBL(L,NX)  
            IF(SNDBTMP.LT.0.0.AND.ASNDFBL.GT.0.0)THEN  
              SNDFBLM=0.5*SNDB(L,KBT(L),NX)/DELT  
              FLUXFAC=SNDFBLM/SNDFBL(L,NX)  
              QSBDLDX(L+1,NX)=FLUXFAC*QSBDLDX(L+1,NX)  
              QSBDLDY(LN ,NX)=FLUXFAC*QSBDLDY(LN ,NX)  
              QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  
            ENDIF  
          ENDIF  
          IF(UCELLCTR(L).GE.0.0.AND.VCELLCTR(L).LT.0.0)THEN  
            SNDFBL(L,NX)=DXYIP(L)*(-QSBDLDY(L,NX)+QSBDLDX(L+1,NX)  
     &          +QSBDLDOT(L,NX))  
            ASNDFBL=ABS(SNDFBL(L,NX))  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDFBL(L,NX)  
            IF(SNDBTMP.LT.0.0.AND.ASNDFBL.GT.0.0)THEN  
              SNDFBLM=0.5*SNDB(L,KBT(L),NX)/DELT  
              FLUXFAC=SNDFBLM/SNDFBL(L,NX)  
              QSBDLDX(L+1,NX)=FLUXFAC*QSBDLDX(L+1,NX)  
              QSBDLDY(L  ,NX)=FLUXFAC*QSBDLDY(L  ,NX)  
              QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  
            ENDIF  
          ENDIF  
          IF(UCELLCTR(L).LT.0.0.AND.VCELLCTR(L).GE.0.0)THEN  
            SNDFBL(L,NX)=DXYIP(L)*(-QSBDLDX(L,NX)+QSBDLDY(LN,NX)  
     &          +QSBDLDOT(L,NX))  
            ASNDFBL=ABS(SNDFBL(L,NX))  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDFBL(L,NX)  
            IF(SNDBTMP.LT.0.0.AND.ASNDFBL.GT.0.0)THEN  
              SNDFBLM=0.5*SNDB(L,KBT(L),NX)/DELT  
              FLUXFAC=SNDFBLM/SNDFBL(L,NX)  
              QSBDLDX(L ,NX)=FLUXFAC*QSBDLDX(L ,NX)  
              QSBDLDY(LN,NX)=FLUXFAC*QSBDLDY(LN,NX)  
              QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  
            ENDIF  
          ENDIF  
          IF(UCELLCTR(L).LT.0.0.AND.VCELLCTR(L).LT.0.0)THEN  
            SNDFBL(L,NX)=DXYIP(L)*(-QSBDLDX(L,NX)-QSBDLDY(L,NX)  
     &          +QSBDLDOT(L,NX))  
            ASNDFBL=ABS(SNDFBL(L,NX))  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDFBL(L,NX)  
            IF(SNDBTMP.LT.0.0.AND.ASNDFBL.GT.0.0)THEN  
              SNDFBLM=0.5*SNDB(L,KBT(L),NX)/DELT  
              FLUXFAC=SNDFBLM/SNDFBL(L,NX)  
              QSBDLDX(L,NX)=FLUXFAC*QSBDLDX(L,NX)  
              QSBDLDY(L,NX)=FLUXFAC*QSBDLDY(L,NX)  
              QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  
            ENDIF  
          ENDIF  
        ENDIF  
      ENDDO  
C
      IF(NSBDLDBC.GT.0) THEN 
        DO NSB=1,NSBDLDBC
          LUTMP=LSBLBCU(NSB)
          LDTMP=LSBLBCD(NSB)
          IF(LDTMP.GT.0) THEN
            QSBDLDOT(LUTMP,NX)=QSBDLDIN(LDTMP,NX)
          ENDIF
        ENDDO
      ENDIF
C  
C **  CALCULATE EQUIVALENT CONCENTRATIONS  
C  
      DO L=2,LA  
        LN=LNC(L)  
        CQBEDLOADX(L,NX)=0.0  
        CQBEDLOADY(L,NX)=0.0  
        IF(LMASKDRY(L))THEN  
          UVARTMP=0.5*(RSSBCW(L)*U(L,1)+RSSBCE(L)*U(L+1,1))  
          VVARTMP=0.5*(RSSBCS(L)*V(L,1)+RSSBCN(L)*V(LN ,1))  
          VELMAG=SQRT(UVARTMP*UVARTMP+VVARTMP*VVARTMP)  
          IF(VELMAG.GT.0.0) THEN  
            CQBEDLOADX(L,NX)=QSBDLDP(L)/(HP(L)*VELMAG)  
            CQBEDLOADY(L,NX)=0.  
          ENDIF  
        ENDIF  
      ENDDO  
C  
C **  INSERT OUTFLOW OR RECIRCULATION BOUNDARY CONDITION FOR BED  
C **  LOAD TRANSPORT  
C  
      QSBLLDXY=0.0  
      IF(NSBDLDBC.GT.0) THEN  
        DO NSB=1,NSBDLDBC  
          LUTMP=LSBLBCU(NSB)  
          LDTMP=LSBLBCD(NSB)  
          IF(LDTMP.EQ.0) THEN  
            QSBLLDXY=QSBLLDXY+QSBDLDOT(LUTMP,NX)  
          ENDIF  
        ENDDO  
      ENDIF  
C  
C **  CALCULATE MASS PER UNIT AREA CHANGE IN BED CONCENTRATION DUE TO  
C **  TO NET BED LOAD  
C  
      SNDFBLTOT=0.0  
      DO L=2,LA  
        IF(LMASKDRY(L))THEN  
          LN=LNC(L)  
          SNDFBL(L,NX)=DXYIP(L)*(QSBDLDX(L+1,NX)  
     &        -QSBDLDX(L,NX)+QSBDLDY(LN,NX)-QSBDLDY(L,NX)  
     &        +QSBDLDOT(L,NX)-QSBDLDIN(L,NX))  
          SNDFBLTOT=SNDFBLTOT+DXYP(L)*SNDFBL(L,NX)  
        ENDIF  
      ENDDO  
 8999 FORMAT(' BL ',3I5,5E14.5)  
 8862 FORMAT(' SNDFBLTOT,QSBLLDXY',3I5,5E14.5)  
      RETURN  
      END  

