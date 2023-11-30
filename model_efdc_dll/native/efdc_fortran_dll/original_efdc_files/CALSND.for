      SUBROUTINE CALSND  
C  
C CHANGE RECORD  
C **  SUBROUTINE CALSND CALCULATES NONCOHESIVER SEDIMENT SETTLING,  
C **  DEPOSITION AND RESUSPENSION AND IS CALLED FOR SSEDTOX  
C  
      USE GLOBAL  

	IMPLICIT NONE
	REAL::TIME,GRADSED,SIGP,CRNUM,DUM1,DUM3,DUM4,DIASED3
      REAL::FSEDMODE,CSNDZEQ,ZEQMIN,CSNDEQC,CSHIELDS,TMPVAL
	REAL::CSNDSET,SHIELDS,TOP,BOT,WSFAC,WESE,WESEMX
	REAL::PROBDEP,WSETMP,WVEL,CLEFT,CRIGHT,SNDBTMP,SEDAVG
	REAL::AA11,AA12,AA21,AA22,BB11,BB22,DETI,FLUXFAC
	INTEGER::NX,NS,K,L,ISGPFLAG,ISEHFLAG,KTOP,NXX,NSS,IFLAG,LN
C  
C**********************************************************************C  
C  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=(DT*FLOAT(N)+TCON*TBEGIN)/TCON  
      ELSE  
        TIME=TIMESEC/TCON  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  SET MAXIMUM NONCOHESIVE SEDIMENT DIAMETER  
C  
      SNDDMX=0.  
      IF(ISTRAN(7).GE.1)THEN  
        DO NX=1,NSND  
          NS=NSED+NX  
          SNDDMX=MAX(SNDDMX,SEDDIA(NS))  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  SET NONCOHESIVE ARMORING PARAMETERS  
C  
      IF(ISTRAN(7).GE.1)THEN  
C  
        IF(ISNDAL.EQ.0)THEN  
C  
          DO K=1,KB  
            DO L=2,LA  
              SIGPHI(L,K)=0.  
            ENDDO  
          ENDDO  
C  
          DO NX=1,NSND  
            DO L=2,LA  
              PEXP(L,NX)=1.  
              PHID(L,NX)=1.  
            ENDDO  
          ENDDO  
C  
        ENDIF  
C  
        IF(ISNDAL.GE.1)THEN  
C  
          ISGPFLAG=0  
          ISEHFLAG=0  
          DO NX=1,NSND  
            NS=NX+NSED  
            IF(ISNDEQ(NS).EQ.1)ISGPFLAG=1  
            IF(ISBDLD(NS).GE.2)ISEHFLAG=1  
          ENDDO  
C  
C **  SET SIGPHI FOR GARCIA AND PARKER (1991) EQS 46 AND 47  
C  
          IF(ISGPFLAG.EQ.1)THEN  
C  
C **  GP - CONVERT SEDIMENT DIAMETERS IN M TO MM AND SET PHI SIZE  
C  
            DO NX=1,NSND  
              NS=NSED+NX  
              SEDPHI(NS)=-LOG(1000.*SEDDIA(NS))/LOG(2.)  
            ENDDO  
C  
C **  GP - SET MEAN PHI FOR TOP LAYER OF BED  
C  
            DO L=2,LA  
              TVAR3W(L)=0.  
              TVAR3E(L)=0.  
              SIGPHI(L,KBT(L))=0.  
            ENDDO  
C  
            DO NX=1,NSND  
              NS=NSED+NX  
              DO L=2,LA  
                KTOP=KBT(L)  
                TVAR3W(L)=TVAR3W(L)+SEDPHI(NS)*VFRBED(L,KTOP,NS)  
                TVAR3E(L)=TVAR3E(L)+VFRBED(L,KTOP,NS)  
              ENDDO  
            ENDDO  
C  
            DO L=2,LA  
              IF(TVAR3E(L).LE.0.) TVAR3E(L)=1.  
              TVAR3W(L)=TVAR3W(L)/TVAR3E(L)  
            ENDDO  
C  
            DO NX=1,NSND  
              NS=NSED+NX  
              DO K=1,KB  
                DO L=2,LA  
                  KTOP=KBT(L)  
                  SIGPHI(L,KTOP)=SIGPHI(L,K)+((SEDPHI(NS)-TVAR3W(L))**2)  
     &                *VFRBED(L,KTOP,NS)/TVAR3E(L)  
                ENDDO  
              ENDDO  
            ENDDO  
C  
            DO L=2,LA  
              KTOP=KBT(L)
              IF(SIGPHI(L,KTOP).LT.0.)THEN  
                SIGPHI(L,KTOP)=-SQRT(ABS(SIGPHI(L,KTOP)))   ! *** PMC TEMP PATCH  
              ELSE
                SIGPHI(L,KTOP)=SQRT(SIGPHI(L,KTOP))  
              ENDIF
            ENDDO  
C  
          ENDIF  
C  
C **  END CALCULATION OF SIGPHI FOR GARCIA AND PARKER (1991)  
C  
C **  SET EXPOSURE AND HIDING FUNCTIONS FOR ENGULAND-HANSEN AND WU,WANG,  
C  
          IF(ISEHFLAG.EQ.1.OR.ISGPFLAG.EQ.1)THEN  
C  
            DO NX=1,NSND  
              DO L=2,LA  
                PEXP(L,NX)=0.0  
                PHID(L,NX)=0.0  
              ENDDO  
            ENDDO  
C  
            DO L=2,LA  
              SNDBT(L,KBT(L))=0.0  
            ENDDO  
C  
            DO NX=1,NSND  
              NS=NSED+NX  
              DO L=2,LA  
                K=KBT(L)  
                SNDBT(L,K)=SNDBT(L,K)+SNDB(L,K,NX)  
                DO NXX=1,NSND  
                  NSS=NSED+NXX  
                  PEXP(L,NX)=PEXP(L,NX)+SNDB(L,K,NXX)*SEDDIA(NS)  
     &                /(SEDDIA(NS)+SEDDIA(NSS))  
                  PHID(L,NX)=PHID(L,NX)+SNDB(L,K,NXX)*SEDDIA(NSS)  
     &                /(SEDDIA(NS)+SEDDIA(NSS))  
                ENDDO  
              ENDDO  
            ENDDO  
C  
            DO NX=1,NSND  
              DO L=2,LA  
                K=KBT(L)  
                IF(SNDBT(L,K).GT.0.0) THEN  
                  PEXP(L,NX)=PEXP(L,NX)/SNDBT(L,K)  
                  PHID(L,NX)=PHID(L,NX)/SNDBT(L,K)  
                ELSE  
                  PEXP(L,NX)=1.0  
                  PHID(L,NX)=1.0  
                END IF  
              ENDDO  
            ENDDO  
  
C          DO L=2,LA  
C            WRITE(8,888)IL(L),JL(L),(PEXP(L,NX),NX=1,NSND),  
C     &                  (PHID(L,NX),NX=1,NSND)  
C          ENDDO  
C  
          ENDIF  
C  
C **  END SET EXPOSURE AND HIDING FUNCTIONS FOR ENGULAND-HANSEN  
C  
  
        ENDIF  
C  
      ENDIF  
C  
  888 FORMAT(2I5,6E12.4)  
C  
C**********************************************************************C  
C  
C **   SET CRITICAL SHILED'S PARAMETER FOR D50  
C  
      DO L=2,LA  
        CALL SETSHLD(DUM1,CSHIELDS50(L),SEDDIA50(L,KBT(L)),  
     &      SSG(NSED+1),DUM3,DUM4)  
      ENDDO  
C  
C**********************************************************************C  
C  
C **  NONCOHESIVE SEDIMENT, KC=1 (SINGLE LAYER IN VERTICAL)  
C  
      IF(ISTRAN(7).GE.1.AND.KC.EQ.1)THEN  
        DO NX=1,NSND  
          NS=NX+NSED  
          DSEDGMM=1./(1.E6*SSG(NS))  
          DIASED=SEDDIA(NS)  
          DIASED3=3.*DIASED  
          GPDIASED=G*(SSG(NS)-1.)*DIASED  
C  
C----------------------------------------------------------------------C  
C  
C **  SET SETTLING VELOCITIES  
C  
          K=0  
C  
          IF(ISNDVW.EQ.0)THEN  
            DO L=2,LA  
              WSETA(L,K,NS)=WSEDO(NS)  
            ENDDO  
          ENDIF  
C  
          IF(ISNDVW.GE.1)THEN  
            DO L=2,LA  
              WSETA(L,K,NS)=WSEDO(NS)*  
     &            CSNDSET(SNDT(L,K+1),SDEN(NS),ISNDVW)  
            ENDDO  
          ENDIF  
C  
          IF(IROUSE(NX).EQ.0)THEN  
            DO L=2,LA  
              IF(USTAR(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTAR(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ELSE  
            DO L=2,LA  
              IF(USTARSND(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTARSND(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ENDIF  
C  
C----------------------------------------------------------------------C  
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
C  
          CALL BEDLOAD(NX,NS)  
C  
C----------------------------------------------------------------------C  
C  
C **  SUSPENDED TRANSPORT HORIZONTAL LOOP  
C  
          DO L=2,LA  
            FACSUSL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),  
     &          RSNDM(NX),ISNDM1(NX),ISNDM2(NX),2)  
          ENDDO  
C  
C **  SET EQUILIBRUIM CONCENTRATION HEIGHT & INVERSE  
          DO L=2,LA  
            ZEQ(L)=CSNDZEQ(DIASED,GPDIASED,TAUR(NS),TAUBSND(L),  
     &        SEDDIA50(L,KBT(L)),HP(L),ISNDEQ(NS),SSG(NS),WSETA(L,0,NS))  
            ZEQMIN=0.5*DZC(1)  
            ZEQ(L)=MIN(ZEQ(L),ZEQMIN)  
            ZEQI(L)=1./ZEQ(L)  
          ENDDO  
C  
C **  SET EQUILIBRUIM CONCENTRATION  
          DO L=2,LA  
            SNDEQB(L)=CSNDEQC(DIASED,SSG(NS),WSETA(L,0,NS),TAUR(NS),  
     &          TAUBSND(L),SEDDIA50(L,KBT(L)),SIGPHI(L,KBT(L)),ZEQ(L),  
     &          ISNDEQ(NS),ISNDAL)  
          ENDDO  
C  
C **  APPLIED LIMITOR TO GARCIA AND PARKER  
C  
          IF(ISNDEQ(NS).EQ.1)THEN  
            IF(ISLTAUC(NS).EQ.1)THEN  
              CSHIELDS=TCSHIELDS(NS)  
              DO L=2,LA  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDDO  
            ENDIF  
            IF(ISLTAUC(NS).EQ.2)THEN  
              DO L=2,LA  
                CSHIELDS=SEDDIA50(L,KBT(L))*CSHIELDS50(L)/DIASED  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDDO  
            ENDIF  
            IF(ISLTAUC(NS).EQ.3)THEN  
              DO L=2,LA  
                CSHIELDS=0.03*((PHID(L,NX)/PEXP(L,NX))**0.6)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDDO  
            ENDIF  
          ENDIF  
C  
          IF(ISEDEFF.EQ.1)THEN  
            DO L=2,LA  
              SNDEQB(L)=SNDEQB(L)*EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
            ENDDO  
          ENDIF  
C  
  
          DO L=2,LA  
            IF(ROUSE(L).LT.0.999.OR.ROUSE(L).GT.1.001)THEN  
              TOP=(ZEQ(L)**(ROUSE(L)-1.))-1.  
              BOT=(1.-ROUSE(L))*(ZEQI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
              SNDEQSAV(L,NX)=SNDEQ(L)  
            ELSE  
              TOP=LOG(ZEQI(L))  
              BOT=(ZEQI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
              SNDEQSAV(L,NX)=SNDEQ(L)  
            ENDIF  
          ENDDO  
C  
          DO L=2,LA  
            SNDF(L,1,NX)=0.  
            PROBDEP=0.  
            WESE=0.  
C **  SET MAXIMUM EROSION RATE  
            WESEMX=DELTI*CTMPDRY(L)*SNDB(L,KBT(L),NX)-SNDFBL(L,NX)  
            WESEMX=MAX(WESEMX,0.)  
C **  SET RESUSPENSION FLUX  
            WSFAC=2.*(1.+ROUSE(L))/(2.+ROUSE(L)*(1.-ZEQ(L)))  
            WESE=WSFAC*CTMPDRY(L)*WSETA(L,0,NS)*SNDEQ(L)  
C **  SET DEPOSITION VELOCITY  
            WSETMP=WSFAC*WSETA(L,0,NS)  
            WVEL=DELT*HPI(L)*DZIC(1)  
            CLEFT=1.+WSETMP*WVEL  
            CRIGHT=MAX(SND(L,1,NX),0.)+(WESE-SNDF(L,1,NX))*WVEL  
            SND(L,1,NX)=CRIGHT/CLEFT 
            SNDF(L,0,NX)=-WSETMP*SND(L,1,NX)+WESE  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX)  
     &          -DELT*SNDFBL(L,NX)  
C **  ADDED BED LOAD FLUX TO SUSPENDED LOAD FLUX WITH ABOVE LINE  
            IF(SNDBTMP.LT.0.0)THEN  
              ! *** PMC Begin Block
              ! *** Adjust bedload flux, trying to keep suspended load flux constant
              SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX) 
              LN=LNC(L)
              IF(SNDBTMP.LT.0.0)THEN
                ! *** Zero Bedload Outflux
                QSBDLDX(L,NX)=MAX(QSBDLDX(L,NX),0.0)
                QSBDLDY(L,NX)=MAX(QSBDLDY(L,NX),0.0)
                QSBDLDX(L+1,NX)=MIN(QSBDLDX(L+1,NX),0.0)
                QSBDLDY(LN,NX)=MIN(QSBDLDY(LN,NX),0.0)
                QSBDLDOT(L ,NX)=0.
                SNDFBL(L,NX)=DXYIP(L)*(QSBDLDX(L+1,NX)  
     &              -QSBDLDX(L,NX)+QSBDLDY(LN,NX)-QSBDLDY(L,NX)  
     &              +QSBDLDOT(L,NX)-QSBDLDIN(L,NX))  

                ! *** Reduce Suspended Load Flux
                SNDF(L,0,NX)=DELTI*SNDB(L,KBT(L),NX)
                SND(L,1,NX)=SNDS(L,1,NX)+(SNDF(L,0,NX)-
     &                                    SNDF(L,1,NX))*WVEL  
              ELSE
                ! *** Reduce Bedload Flux
                SNDBTMP=SNDBTMP/DELT
                FLUXFAC=SNDBTMP/SNDFBL(L,NX)

                IF(QSBDLDX(L,NX).LT.0.)
     &                 QSBDLDX(L,NX)=FLUXFAC*QSBDLDX(L,NX)  
                IF(QSBDLDY(L,NX).LT.0.)
     &                 QSBDLDY(L,NX)=FLUXFAC*QSBDLDY(L,NX)  
                QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  

                ! *** Update final Bedload Flux
                SNDFBL(L,NX)=SNDBTMP-QSBDLDIN(L ,NX)
              ENDIF
              SNDBTMP=0.0
              ! *** PMC End Block
            ENDIF  
            SNDB1(L,KBT(L),NX)=S3TL*SNDB(L,KBT(L),NX)  
     &          +S2TL*SNDB1(L,KBT(L),NX)  
            SNDB(L,KBT(L),NX)=SNDBTMP  
            SNDF(L,0,NX)=SNDF(L,0,NX)+SNDFBL(L,NX)  
            QSBDTOP(L)=QSBDTOP(L)+DSEDGMM*SNDF(L,0,NX)  
            QWBDTOP(L)=QWBDTOP(L)+DSEDGMM*  
     &          ( VDRBED(L,KBT(L))*MAX(SNDF(L,0,NX),0.)  
     &          +VDRDEPO(NS)*MIN(SNDF(L,0,NX),0.) )  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  NONCOHESIVE SEDIMENT, KC=2 (TWO LAYERS IN VERTICAL)  
C  
      IF(ISTRAN(7).GE.1.AND.KC.EQ.2)THEN  
        DO NX=1,NSND  
          NS=NX+NSED  
          DSEDGMM=1./(1.E6*SSG(NS))  
          DIASED=SEDDIA(NS)  
          DIASED3=3.*DIASED  
          GPDIASED=G*(SSG(NS)-1.)*DIASED  
C  
C----------------------------------------------------------------------C  
C  
C **  SET SETTLING VELOCITIES  
C  
          IF(ISNDVW.EQ.0)THEN  
            DO K=0,KS  
              DO L=2,LA  
                WSETA(L,K,NS)=WSEDO(NS)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
          IF(ISNDVW.GE.1)THEN  
            DO K=0,KS  
              DO L=2,LA  
                WSETA(L,K,NS)=WSEDO(NS)*  
     &              CSNDSET(SNDT(L,K+1),SDEN(NS),ISNDVW)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
          IF(IROUSE(NX).EQ.0)THEN  
            DO L=2,LA  
              IF(USTAR(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTAR(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ELSE  
            DO L=2,LA  
              IF(USTARSND(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTARSND(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ENDIF  
C  
C----------------------------------------------------------------------C  
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
C  
          CALL BEDLOAD(NX,NS)  
C  
C----------------------------------------------------------------------C  
C  
C **  HORIZONTAL LOOPS  
C  
          K=2  
          DO L=2,LA  
            SNDF(L,K,NX)=0.  
            WVEL=DELT*HPI(L)*DZIC(K)  
            CLEFT=1.+WSETA(L,K-1,NS)*WVEL  
            CRIGHT=MAX(SND(L,K,NX),0.)  
            SND(L,K,NX)=CRIGHT/CLEFT  
            SNDF(L,K-1,NX)=-WSETA(L,K-1,NS)*SND(L,K,NX)  
          ENDDO  
C  
          DO L=2,LA  
            FACSUSL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),  
     &          RSNDM(NX),ISNDM1(NX),ISNDM2(NX),2)  
          ENDDO  
C  
          DO L=2,LA  
            PROBDEP=0.  
            WESE=0.  
C **  SET MAXIMUM EROSION RATE  
            WESEMX=DELTI*CTMPDRY(L)*SNDB(L,KBT(L),NX)-SNDFBL(L,NX)  
            WESEMX=MAX(WESEMX,0.)  
C **  SET ROUSE PARAMETER AND EQUILIBRUIM CONCENTRATION  
            ZEQ(L)=CSNDZEQ(DIASED,GPDIASED,TAUR(NS),TAUBSND(L),  
     &        SEDDIA50(L,KBT(L)),HP(L),ISNDEQ(NS),SSG(NS),WSETA(L,0,NS))  
            ZEQMIN=0.5*DZC(1)  
            ZEQ(L)=MIN(ZEQ(L),ZEQMIN)  
            ZEQD(L)=ZEQ(L)/DZC(1)  
            ZEQDI(L)=1./ZEQD(L)  
            WSFAC=1.  
            SIGP=SIGPHI(L,KBT(L))  
            SNDEQB(L)=CSNDEQC(DIASED,SSG(NS),WSETA(L,0,NS),TAUR(NS),  
     &          TAUBSND(L),SEDDIA50(L,KBT(L)),SIGP,ZEQ(L),ISNDEQ(NS)
     &          ,ISNDAL)  
C  
C **  APPLIED LIMITOR TO GARCIA AND PARKER  
C  
            IF(ISNDEQ(NS).EQ.1)THEN  
              IF(ISLTAUC(NS).EQ.1)THEN  
                CSHIELDS=TCSHIELDS(NS)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
              IF(ISLTAUC(NS).EQ.2)THEN  
                CSHIELDS=SEDDIA50(L,KBT(L))*CSHIELDS50(L)/DIASED  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
              IF(ISLTAUC(NS).EQ.3)THEN  
                CSHIELDS=0.03*((PHID(L,NX)/PEXP(L,NX))**0.6)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
            ENDIF  
C  
            IF(ISEDEFF.EQ.1)  
     &          SNDEQB(L)=SNDEQB(L)*EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
C  
            IF(ROUSE(L).LT.0.999.OR.ROUSE(L).GT.1.001)THEN  
              TOP=(ZEQD(L)**(ROUSE(L)-1.))-1.  
              BOT=(1.-ROUSE(L))*(ZEQDI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
            ELSE  
              TOP=LOG(ZEQDI(L))  
              BOT=(ZEQDI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
            ENDIF  
C **  SET RESUSPENSION FLUX  
            WESE=WSFAC*CTMPDRY(L)*WSETA(L,0,NS)*SNDEQ(L)  
C **  SET DEPOSITION VELOCITY  
            WSETMP=WSFAC*WSETA(L,0,NS)  
            WVEL=DELT*HPI(L)*DZIC(1)  
            CLEFT=1.+WSETMP*WVEL  
            CRIGHT=MAX(SND(L,1,NX),0.)+(WESE-SNDF(L,1,NX))*WVEL  
            SND(L,1,NX)=CRIGHT/CLEFT  
            SNDF(L,0,NX)=-WSETMP*SND(L,1,NX)+WESE  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX)  
     &          -DELT*SNDFBL(L,NX)  
C **  ADDED BED LOAD FLUX TO SUSPENDED LOAD FLUX WITH ABOVE LINE  
            IF(SNDBTMP.LT.0.0)THEN  
              ! *** PMC Begin Block
              ! *** Adjust bedload flux, trying to keep suspended load flux constant
              SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX) 
              LN=LNC(L)
              IF(SNDBTMP.LT.0.0)THEN
                ! *** Zero Bedload Outflux
                QSBDLDX(L,NX)=MAX(QSBDLDX(L,NX),0.0)
                QSBDLDY(L,NX)=MAX(QSBDLDY(L,NX),0.0)
                QSBDLDX(L+1,NX)=MIN(QSBDLDX(L+1,NX),0.0)
                QSBDLDY(LN,NX)=MIN(QSBDLDY(LN,NX),0.0)
                QSBDLDOT(L ,NX)=0.
                SNDFBL(L,NX)=DXYIP(L)*(QSBDLDX(L+1,NX)  
     &              -QSBDLDX(L,NX)+QSBDLDY(LN,NX)-QSBDLDY(L,NX)  
     &              +QSBDLDOT(L,NX)-QSBDLDIN(L,NX))  

                ! *** Reduce Suspended Load Flux
                SNDF(L,0,NX)=DELTI*SNDB(L,KBT(L),NX)
                SND(L,1,NX)=SNDS(L,1,NX)+(SNDF(L,0,NX)-
     &                                    SNDF(L,1,NX))*WVEL  
              ELSE
                ! *** Reduce Bedload Flux
                SNDBTMP=SNDBTMP/DELT
                FLUXFAC=SNDBTMP/SNDFBL(L,NX)

                IF(QSBDLDX(L,NX).LT.0.)
     &                 QSBDLDX(L,NX)=FLUXFAC*QSBDLDX(L,NX)  
                IF(QSBDLDY(L,NX).LT.0.)
     &                 QSBDLDY(L,NX)=FLUXFAC*QSBDLDY(L,NX)  
                QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  

                ! *** Update final Bedload Flux
                SNDFBL(L,NX)=SNDBTMP-QSBDLDIN(L ,NX)
              ENDIF
              SNDBTMP=0.0
              ! *** PMC End Block
            ENDIF  
            SNDB1(L,KBT(L),NX)=S3TL*SNDB(L,KBT(L),NX)  
     &          +S2TL*SNDB1(L,KBT(L),NX)  
            SNDB(L,KBT(L),NX)=SNDBTMP  
            SNDF(L,0,NX)=SNDF(L,0,NX)+SNDFBL(L,NX)  
            QSBDTOP(L)=QSBDTOP(L)+DSEDGMM*SNDF(L,0,NX)  
            QWBDTOP(L)=QWBDTOP(L)+DSEDGMM*  
     &          ( VDRBED(L,KBT(L))*MAX(SNDF(L,0,NX),0.)  
     &          +VDRDEPO(NS)*MIN(SNDF(L,0,NX),0.) )  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
C **  ANTI-DIFFUSION OF NONCOHESIVE SEDIMENT  KC.EQ.2  
C  
          IF(ISTOPT(7).EQ.1)THEN  
C  
            DO L=2,LA  
              CRNUM=1.+DELT*WSETA(L,1,NS)*HPI(L)*DZIC(KC)  
              GRADSED=(SND(L,KC,NX)-SND(L,1,NX))/(DZC(KC)+DZC(1))  
              SEDAVG=0.5*(SND(L,KC,NX)+SND(L,KC,NX)+1.E-16)  
              WSETA(L,1,NS)=-CRNUM*DZC(KC)*WSETA(L,1,NS)*GRADSED/SEDAVG  
            ENDDO  
C  
            DO L=2,LA  
              AA11=DELTI*DZC(1)*HP(L)-MIN(WSETA(L,1,NS),0.)  
              AA12=-MAX(WSETA(L,1,NS),0.)  
              AA21=MIN(WSETA(L,1,NS),0.)  
              AA22=DELTI*DZC(KC)*HP(L)+MAX(WSETA(L,1,NS),0.)  
              BB11=DELTI*DZC(1)*HP(L)*SND(L,1,NX)  
              BB22=DELTI*DZC(KC)*HP(L)*SND(L,KC,NX)  
              DETI=1./(AA11*AA22-AA12*AA21)  
              SND(L,1,NX)=DETI*( BB11*AA22-BB22*AA12 )  
              SND(L,KC,NX)=DETI*( AA11*BB22-AA21*BB11 )  
            ENDDO  
C  
          ENDIF  
C  
C----------------------------------------------------------------------C  
C  
C **  FINAL FLUX KC=2  
C  
          DO L=2,LA  
            SNDF(L,1,NX)=DELTI*DZC(KC)*HP(L)*(SND(L,KC,NX)  
     &          -SNDS(L,KC,NX))  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  NONCOHESIVE SEDIMENT, KC=3 (THREE LAYERS IN VERTICAL)  
C  
      IF(ISTRAN(7).GE.1.AND.KC.EQ.3)THEN  
        DO NX=1,NSND  
          NS=NX+NSED  
          DSEDGMM=1./(1.E6*SSG(NS))  
          DIASED=SEDDIA(NS)  
          DIASED3=3.*DIASED  
          GPDIASED=G*(SSG(NS)-1.)*DIASED  
C  
C----------------------------------------------------------------------C  
C  
C **  SET SETTLING VELOCITIES  
C  
          IF(ISNDVW.EQ.0)THEN  
            DO K=0,KS  
              DO L=2,LA  
                WSETA(L,K,NS)=WSEDO(NS)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
          IF(ISNDVW.GE.1)THEN  
            DO K=0,KS  
              DO L=2,LA  
                WSETA(L,K,NS)=WSEDO(NS)*  
     &              CSNDSET(SNDT(L,K+1),SDEN(NS),ISNDVW)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
          IF(IROUSE(NX).EQ.0)THEN  
            DO L=2,LA  
              IF(USTAR(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTAR(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ELSE  
            DO L=2,LA  
              IF(USTARSND(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTARSND(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ENDIF  
C  
C----------------------------------------------------------------------C  
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
C  
          CALL BEDLOAD(NX,NS)  
C  
C----------------------------------------------------------------------C  
C  
C **  HORIZONTAL LOOPS  
C  
          K=3  
          DO L=2,LA  
            SNDF(L,K,NX)=0.  
            WVEL=DELT*HPI(L)*DZIC(K)  
            CLEFT=1.+WSETA(L,K-1,NS)*WVEL  
            CRIGHT=MAX(SND(L,K,NX),0.)  
            SND(L,K,NX)=CRIGHT/CLEFT  
            SNDF(L,K-1,NX)=-WSETA(L,K-1,NS)*SND(L,K,NX)  
          ENDDO  
C  
          K=2  
          DO L=2,LA  
            WVEL=DELT*HPI(L)*DZIC(K)  
            CLEFT=1.+WSETA(L,K-1,NS)*WVEL  
            CRIGHT=MAX(SND(L,K,NX),0.)-SNDF(L,K,NX)*WVEL  
            SND(L,K,NX)=CRIGHT/CLEFT  
            SNDF(L,K-1,NX)=-WSETA(L,K-1,NS)*SND(L,K,NX)  
          ENDDO  
C  
          DO L=2,LA  
            FACSUSL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),  
     &          RSNDM(NX),ISNDM1(NX),ISNDM2(NX),2)  
          ENDDO  
C  
          DO L=2,LA  
            PROBDEP=0.  
            WESE=0.  
C **  SET MAXIMUM EROSION RATE  
            WESEMX=DELTI*CTMPDRY(L)*SNDB(L,KBT(L),NX)-SNDFBL(L,NX)  
            WESEMX=MAX(WESEMX,0.)  
C **  SET ROUSE PARAMETER AND EQUILIBRUIM CONCENTRATION  
            ZEQ(L)=CSNDZEQ(DIASED,GPDIASED,TAUR(NS),TAUBSND(L),  
     &        SEDDIA50(L,KBT(L)),HP(L),ISNDEQ(NS),SSG(NS),WSETA(L,0,NS))  
            ZEQMIN=0.5*DZC(1)  
            ZEQ(L)=MIN(ZEQ(L),ZEQMIN)  
            ZEQD(L)=ZEQ(L)/DZC(1)  
            ZEQDI(L)=1./ZEQD(L)  
            WSFAC=1.  
            SIGP=SIGPHI(L,KBT(L))  
            SNDEQB(L)=CSNDEQC(DIASED,SSG(NS),WSETA(L,0,NS),TAUR(NS),  
     &          TAUBSND(L),SEDDIA50(L,KBT(L)),SIGP,ZEQ(L),ISNDEQ(NS)
     &          ,ISNDAL)  
C  
C **  APPLIED LIMITOR TO GARCIA AND PARKER  
C  
            IF(ISNDEQ(NS).EQ.1)THEN  
              IF(ISLTAUC(NS).EQ.1)THEN  
                CSHIELDS=TCSHIELDS(NS)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
              IF(ISLTAUC(NS).EQ.2)THEN  
                CSHIELDS=SEDDIA50(L,KBT(L))*CSHIELDS50(L)/DIASED  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
              IF(ISLTAUC(NS).EQ.3)THEN  
                CSHIELDS=0.03*((PHID(L,NX)/PEXP(L,NX))**0.6)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
            ENDIF  
C  
            IF(ISEDEFF.EQ.1)  
     &          SNDEQB(L)=SNDEQB(L)*EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
C  
            IF(ROUSE(L).LT.0.999.OR.ROUSE(L).GT.1.001)THEN  
              TOP=(ZEQD(L)**(ROUSE(L)-1.))-1.  
              BOT=(1.-ROUSE(L))*(ZEQDI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
            ELSE  
              TOP=LOG(ZEQDI(L))  
              BOT=(ZEQDI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
            ENDIF  
C **  SET RESUSPENSION FLUX  
            WESE=WSFAC*CTMPDRY(L)*WSETA(L,0,NS)*SNDEQ(L)  
C **  SET DEPOSITION VELOCITY  
            WSETMP=WSFAC*WSETA(L,0,NS)  
            WVEL=DELT*HPI(L)*DZIC(1)  
            CLEFT=1.+WSETMP*WVEL  
            CRIGHT=MAX(SND(L,1,NX),0.)+(WESE-SNDF(L,1,NX))*WVEL  
            SND(L,1,NX)=CRIGHT/CLEFT  
            SNDF(L,0,NX)=-WSETMP*SND(L,1,NX)+WESE  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX)  
     &          -DELT*SNDFBL(L,NX)  
C **  ADDED BED LOAD FLUX TO SUSPENDED LOAD FLUX WITH ABOVE LINE  
            IF(SNDBTMP.LT.0.0)THEN  
              ! *** PMC Begin Block
              ! *** Adjust bedload flux, trying to keep suspended load flux constant
              SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX) 
              LN=LNC(L)
              IF(SNDBTMP.LT.0.0)THEN
                ! *** Zero Bedload Outflux
                QSBDLDX(L,NX)=MAX(QSBDLDX(L,NX),0.0)
                QSBDLDY(L,NX)=MAX(QSBDLDY(L,NX),0.0)
                QSBDLDX(L+1,NX)=MIN(QSBDLDX(L+1,NX),0.0)
                QSBDLDY(LN,NX)=MIN(QSBDLDY(LN,NX),0.0)
                QSBDLDOT(L ,NX)=0.
                SNDFBL(L,NX)=DXYIP(L)*(QSBDLDX(L+1,NX)  
     &              -QSBDLDX(L,NX)+QSBDLDY(LN,NX)-QSBDLDY(L,NX)  
     &              +QSBDLDOT(L,NX)-QSBDLDIN(L,NX))  

                ! *** Reduce Suspended Load Flux
                SNDF(L,0,NX)=DELTI*SNDB(L,KBT(L),NX)
                SND(L,1,NX)=SNDS(L,1,NX)+(SNDF(L,0,NX)-
     &                                    SNDF(L,1,NX))*WVEL  
              ELSE
                ! *** Reduce Bedload Flux
                SNDBTMP=SNDBTMP/DELT
                FLUXFAC=SNDBTMP/SNDFBL(L,NX)

                IF(QSBDLDX(L,NX).LT.0.)
     &                 QSBDLDX(L,NX)=FLUXFAC*QSBDLDX(L,NX)  
                IF(QSBDLDY(L,NX).LT.0.)
     &                 QSBDLDY(L,NX)=FLUXFAC*QSBDLDY(L,NX)  
                QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  

                ! *** Update final Bedload Flux
                SNDFBL(L,NX)=SNDBTMP-QSBDLDIN(L ,NX)
              ENDIF
              SNDBTMP=0.0
              ! *** PMC End Block
            ENDIF  
            SNDB1(L,KBT(L),NX)=S3TL*SNDB(L,KBT(L),NX)  
     &          +S2TL*SNDB1(L,KBT(L),NX)  
            SNDB(L,KBT(L),NX)=SNDBTMP  
            SNDF(L,0,NX)=SNDF(L,0,NX)+SNDFBL(L,NX)  
            QSBDTOP(L)=QSBDTOP(L)+DSEDGMM*SNDF(L,0,NX)  
            QWBDTOP(L)=QWBDTOP(L)+DSEDGMM*  
     &          ( VDRBED(L,KBT(L))*MAX(SNDF(L,0,NX),0.)  
     &          +VDRDEPO(NS)*MIN(SNDF(L,0,NX),0.) )  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
C **  ANTI-DIFFUSION OF NONCOHESIVE SEDIMENT  KC.EQ.3  
C  
          IF(ISTOPT(7).EQ.1)THEN  
C  
            DO K=1,2  
              DO L=2,LA  
                CRNUM=1.+DELT*WSETA(L,K,NS)*HPI(L)*DZIC(K+1)  
                GRADSED=(SND(L,K+1,NX)-SND(L,K,NX))/(DZC(K+1)+DZC(K))  
                SEDAVG=0.5*(SND(L,K+1,NX)-SND(L,K,NX)+1.E-16)  
                WSETA(L,K,NS)=-CRNUM*DZC(K+1)*WSETA(L,K,NS)*GRADSED/  
     &              SEDAVG  
              ENDDO  
            ENDDO  
C  
C     TVAR1S=LOWER DIAGONAL  
            DO L=2,LA  
              TVAR1S(L,1)=0.0  
            ENDDO  
            DO K=2,KC  
              DO L=2,LA  
                TVAR1S(L,K)=MIN(WSETA(L,K-1,NS),0.)  
              ENDDO  
            ENDDO  
C     TVAR1N=UPPER DIAGONAL  
            DO L=2,LA  
              TVAR1N(L,KC)=0.0 
            ENDDO  
            DO K=1,KS  
              DO L=2,LA  
                TVAR1N(L,K)=-MAX(WSETA(L,K,NS),0.)  
              ENDDO  
            ENDDO  
C     TVAR1W=MAIN DIAGONAL  
            DO L=2,LA  
              TVAR1W(L,1)=DELTI*DZC(1)*HP(L)-MIN(WSETA(L,1,NS),0.)  
              TVAR1W(L,KC)=DELTI*DZC(KC)*HP(L)+MAX(WSETA(L,KC-1,NS),0.)  
            ENDDO  
            DO K=2,KS  
              DO L=2,LA  
                TVAR1W(L,K)=DELTI*DZC(KC)*HP(L)+MAX(WSETA(L,K-1,NS),0.)  
     &              -MIN(WSETA(L,K,NS),0.)  
              ENDDO  
            ENDDO  
C     TVAR1E=RIGHT HAND SIDE  
            DO K=1,KC  
              DO L=2,LA  
                TVAR1E(L,K)=DELTI*DZC(KC)*HP(L)*SND(L,K,NX)  
              ENDDO  
            ENDDO  
C  
C     TVAR3S=BET,TVAR2N=U,TVAR2S=GAM ARE WORKING ARRAYS  
            DO L=2,LA  
              TVAR3S(L)=TVAR1W(L,1)  
            ENDDO  
            DO L=2,LA  
              TVAR2N(L,1)=TVAR1E(L,1)/TVAR3S(L)  
            ENDDO  
            DO K=2,KC  
              DO L=2,LA  
                TVAR2S(L,K)=TVAR1N(L,K-1)/TVAR3S(L)  
                TVAR3S(L)=TVAR1W(L,K)-TVAR1S(L,K)*TVAR2S(L,K)  
                TVAR2N(L,K)=(TVAR1E(L,K)-TVAR1S(L,K)*TVAR2N(L,K-1))/  
     &              TVAR3S(L)  
              ENDDO  
            ENDDO  
            DO K=KS,1,-1  
              DO L=2,LA  
                TVAR2N(L,K)=TVAR2N(L,K)-TVAR2S(L,K+1)*TVAR2N(L,K+1)  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO L=2,LA  
                SND(L,K,NX)=TVAR2N(L,K)  
              ENDDO  
            ENDDO  
C  
          ENDIF  
C  
C----------------------------------------------------------------------C  
C  
C **  FINAL FLUX KC=3  
C  
          DO L=2,LA  
            SNDF(L,KC-1,NX)=DELTI*DZC(KC)*HP(L)*(SND(L,KC,NX)  
     &          -SNDS(L,KC,NX))  
          ENDDO  
          DO L=2,LA  
            SNDF(L,1,NX)=DELTI*DZC(KC-1)*HP(L)*(SND(L,KC-1,NX)  
     &          -SNDS(L,KC-1,NX))+SNDF(L,KC-1,NX)  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
C **  NONCOHESIVE SEDIMENT, KC.GT.3 (THREE OR MORE LAYERS IN VERTICAL)  
C  
      IF(ISTRAN(7).GE.1.AND.KC.GT.3)THEN  
        DO NX=1,NSND  
          NS=NX+NSED  
          DSEDGMM=1./(1.E6*SSG(NS))  
          DIASED=SEDDIA(NS)  
          DIASED3=3.*DIASED  
          GPDIASED=G*(SSG(NS)-1.)*DIASED  
C  
C----------------------------------------------------------------------C  
C  
C **  SET SETTLING VELOCITIES  
C  
          IF(ISNDVW.EQ.0)THEN  
            DO K=0,KS  
              DO L=2,LA  
                WSETA(L,K,NS)=WSEDO(NS)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
          IF(ISNDVW.GE.1)THEN  
            DO K=0,KS  
              DO L=2,LA  
                WSETA(L,K,NS)=WSEDO(NS)*  
     &              CSNDSET(SNDT(L,K+1),SDEN(NS),ISNDVW)  
              ENDDO  
            ENDDO  
          ENDIF  
C  
          IF(IROUSE(NX).EQ.0)THEN  
            DO L=2,LA  
              IF(USTAR(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTAR(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ELSE  
            DO L=2,LA  
              IF(USTARSND(L).GT.0.0) THEN  
                ROUSE(L)=WSETA(L,0,NS)/(VKC*USTARSND(L))  
              ELSE  
                ROUSE(L)=250000.*WSETA(L,0,NS)  
              END IF  
            ENDDO  
          ENDIF  
C  
C----------------------------------------------------------------------C  
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
C  
          CALL BEDLOAD(NX,NS)  
C  
C----------------------------------------------------------------------C  
C  
C **  HORIZONTAL LOOP  
C  
          K=KC  
          DO L=2,LA  
            SNDF(L,K,NX)=0.  
            WVEL=DELT*HPI(L)*DZIC(K)  
            CLEFT=1.+WSETA(L,K-1,NS)*WVEL  
            CRIGHT=MAX(SND(L,K,NX),0.)  
            SND(L,K,NX)=CRIGHT/CLEFT  
            SNDF(L,K-1,NX)=-WSETA(L,K-1,NS)*SND(L,K,NX)  
          ENDDO  
C  
          DO K=KS,2,-1  
            DO L=2,LA  
              WVEL=DELT*HPI(L)*DZIC(K)  
              CLEFT=1.+WSETA(L,K-1,NS)*WVEL  
              CRIGHT=MAX(SND(L,K,NX),0.)-SNDF(L,K,NX)*WVEL  
              SND(L,K,NX)=CRIGHT/CLEFT  
              SNDF(L,K-1,NX)=-WSETA(L,K-1,NS)*SND(L,K,NX)  
            ENDDO  
          ENDDO  
C  
          DO L=2,LA  
            FACSUSL(L)=FSEDMODE(WSETA(L,0,NS),USTAR(L),USTARSND(L),  
     &          RSNDM(NX),ISNDM1(NX),ISNDM2(NX),2)  
          ENDDO  
C  
          DO L=2,LA  
c            SNDF(L,1,NX)=0.  
            PROBDEP=0.  
            WESE=0.  
C **  SET MAXIMUM EROSION RATE  
            WESEMX=DELTI*CTMPDRY(L)*SNDB(L,KBT(L),NX)-SNDFBL(L,NX)  
            WESEMX=MAX(WESEMX,0.)  
C **  SET ROUSE PARAMETER AND EQUILIBRUIM CONCENTRATION  
            ZEQ(L)=CSNDZEQ(DIASED,GPDIASED,TAUR(NS),TAUBSND(L),  
     &        SEDDIA50(L,KBT(L)),HP(L),ISNDEQ(NS),SSG(NS),WSETA(L,0,NS))  
            ZEQMIN=0.5*DZC(1)  
            ZEQ(L)=MIN(ZEQ(L),ZEQMIN)  
            ZEQD(L)=ZEQ(L)/DZC(1)  
            ZEQDI(L)=1./ZEQD(L)  
            WSFAC=1.  
            SIGP=SIGPHI(L,KBT(L))  
            SNDEQB(L)=CSNDEQC(DIASED,SSG(NS),WSETA(L,0,NS),TAUR(NS),  
     &          TAUBSND(L),SEDDIA50(L,KBT(L)),SIGP,ZEQ(L),ISNDEQ(NS)
     &          ,ISNDAL)  
C  
C **  APPLIED LIMITOR TO GARCIA AND PARKER  
C  
            IF(ISNDEQ(NS).EQ.1)THEN  
              IF(ISLTAUC(NS).EQ.1)THEN  
                CSHIELDS=TCSHIELDS(NS)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
              IF(ISLTAUC(NS).EQ.2)THEN  
                CSHIELDS=SEDDIA50(L,KBT(L))*CSHIELDS50(L)/DIASED  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
              IF(ISLTAUC(NS).EQ.3)THEN  
                CSHIELDS=0.03*((PHID(L,NX)/PEXP(L,NX))**0.6)  
                IF(ISEDEFF.EQ.2)THEN  
                  TMPVAL=1.+(COEHEFF2-1.)  
     &                *( 1.-EXP(-COEHEFF*FRACCOH(L,KBT(L))) )  
                  CSHIELDS=TMPVAL*CSHIELDS  
                ENDIF  
                SHIELDS=TAUBSND(L)/GPDIASED  
                IF(SHIELDS.LT.CSHIELDS) SNDEQB(L)=0.  
              ENDIF  
            ENDIF  
C  
            IF(ISEDEFF.EQ.1)  
     &          SNDEQB(L)=SNDEQB(L)*EXP(-COEHEFF*FRACCOH(L,KBT(L)))  
C  
            IF(ROUSE(L).LT.0.999.OR.ROUSE(L).GT.1.001)THEN  
              TOP=(ZEQD(L)**(ROUSE(L)-1.))-1.  
              BOT=(1.-ROUSE(L))*(ZEQDI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
            ELSE  
              TOP=LOG(ZEQDI(L))  
              BOT=(ZEQDI(L)-1.)  
              SNDEQ(L)=SNDEQB(L)*TOP/BOT  
              SNDEQ(L)=FACSUSL(L)*VFRBED(L,KBT(L),NS)*MAX(SNDEQ(L),0.)  
            ENDIF  
C **  SET RESUSPENSION FLUX  
            WESE=WSFAC*CTMPDRY(L)*WSETA(L,0,NS)*SNDEQ(L)  
C **  SET DEPOSITION VELOCITY  
            WSETMP=WSFAC*WSETA(L,0,NS)  
            WVEL=DELT*HPI(L)*DZIC(1)  
            CLEFT=1.+WSETMP*WVEL  
            CRIGHT=MAX(SND(L,1,NX),0.)+(WESE-SNDF(L,1,NX))*WVEL  
            SND(L,1,NX)=CRIGHT/CLEFT  
            SNDF(L,0,NX)=-WSETMP*SND(L,1,NX)+WESE  
            SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX)  
     &          -DELT*SNDFBL(L,NX)  
C **  ADDED BED LOAD FLUX TO SUSPENDED LOAD FLUX WITH ABOVE LINE  
            IF(SNDBTMP.LT.0.0)THEN  
              ! *** PMC Begin Block
              ! *** Adjust bedload flux, trying to keep suspended load flux constant
              SNDBTMP=SNDB(L,KBT(L),NX)-DELT*SNDF(L,0,NX) 
              LN=LNC(L)
              IF(SNDBTMP.LT.0.0)THEN
                ! *** Zero Bedload Outflux
                QSBDLDX(L,NX)=MAX(QSBDLDX(L,NX),0.0)
                QSBDLDY(L,NX)=MAX(QSBDLDY(L,NX),0.0)
                QSBDLDX(L+1,NX)=MIN(QSBDLDX(L+1,NX),0.0)
                QSBDLDY(LN,NX)=MIN(QSBDLDY(LN,NX),0.0)
                QSBDLDOT(L ,NX)=0.
                SNDFBL(L,NX)=DXYIP(L)*(QSBDLDX(L+1,NX)  
     &              -QSBDLDX(L,NX)+QSBDLDY(LN,NX)-QSBDLDY(L,NX)  
     &              +QSBDLDOT(L,NX)-QSBDLDIN(L,NX))  

                ! *** Reduce Suspended Load Flux
                SNDF(L,0,NX)=DELTI*SNDB(L,KBT(L),NX)
                SND(L,1,NX)=SNDS(L,1,NX)+(SNDF(L,0,NX)-
     &                                    SNDF(L,1,NX))*WVEL  
              ELSE
                ! *** Reduce Bedload Flux
                SNDBTMP=SNDBTMP/DELT
                FLUXFAC=SNDBTMP/SNDFBL(L,NX)

                IF(QSBDLDX(L,NX).LT.0.)
     &                 QSBDLDX(L,NX)=FLUXFAC*QSBDLDX(L,NX)  
                IF(QSBDLDY(L,NX).LT.0.)
     &                 QSBDLDY(L,NX)=FLUXFAC*QSBDLDY(L,NX)  
                QSBDLDOT(L ,NX)=FLUXFAC*QSBDLDOT(L ,NX)  

                ! *** Update final Bedload Flux
                SNDFBL(L,NX)=SNDBTMP-QSBDLDIN(L ,NX)
              ENDIF
              SNDBTMP=0.0
              ! *** PMC End Block
            ENDIF  
            SNDB1(L,KBT(L),NX)=S3TL*SNDB(L,KBT(L),NX)  
     &          +S2TL*SNDB1(L,KBT(L),NX)  
            SNDB(L,KBT(L),NX)=SNDBTMP  
            SNDF(L,0,NX)=SNDF(L,0,NX)+SNDFBL(L,NX)  
            QSBDTOP(L)=QSBDTOP(L)+DSEDGMM*SNDF(L,0,NX)  
            QWBDTOP(L)=QWBDTOP(L)+DSEDGMM*  
     &          ( VDRBED(L,KBT(L))*MAX(SNDF(L,0,NX),0.)  
     &          +VDRDEPO(NS)*MIN(SNDF(L,0,NX),0.) )  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
C **  ANTI-DIFFUSION OF NONCOHESIVE SEDIMENT  KC.GT.3  
C  
          IF(ISTOPT(7).EQ.1)THEN  
C  
            DO K=1,KS  
              DO L=2,LA  
                CRNUM=1.+DELT*WSETA(L,K,NS)*HPI(L)*DZIC(K+1)  
                GRADSED=(SND(L,K+1,NX)-SND(L,K,NX))/(DZC(K+1)+DZC(K))  
                SEDAVG=0.5*(SND(L,K+1,NX)-SND(L,K,NX)+1.E-16)  
                WSETA(L,K,NS)=-CRNUM*DZC(K+1)*WSETA(L,K,NS)*GRADSED/  
     &              SEDAVG  
              ENDDO  
            ENDDO  
C  
C     TVAR1S=LOWER DIAGONAL  
            DO L=2,LA  
              TVAR1S(L,1)=0.0  
            ENDDO  
            DO K=2,KC  
              DO L=2,LA  
                TVAR1S(L,K)=MIN(WSETA(L,K-1,NS),0.)  
              ENDDO  
            ENDDO  
C     TVAR1N=UPPER DIAGONAL  
            DO L=2,LA  
              TVAR1N(L,KC)=0.0  
            ENDDO  
            DO K=1,KS  
              DO L=2,LA  
                TVAR1N(L,K)=-MAX(WSETA(L,K,NS),0.)  
              ENDDO  
            ENDDO  
C     TVAR1W=MAIN DIAGONAL  
            DO L=2,LA  
              TVAR1W(L,1)=DELTI*DZC(1)*HP(L)-MIN(WSETA(L,1,NS),0.)  
              TVAR1W(L,KC)=DELTI*DZC(KC)*HP(L)+MAX(WSETA(L,KC-1,NS),0.)  
            ENDDO  
            DO K=2,KS  
              DO L=2,LA  
                TVAR1W(L,K)=DELTI*DZC(KC)*HP(L)+MAX(WSETA(L,K-1,NS),0.)  
     &              -MIN(WSETA(L,K,NS),0.)  
              ENDDO  
            ENDDO  
C     TVAR1E=RIGHT HAND SIDE  
            DO K=1,KC  
              DO L=2,LA  
                TVAR1E(L,K)=DELTI*DZC(KC)*HP(L)*SND(L,K,NX)  
              ENDDO  
            ENDDO  
C  
C     TVAR3S=BET,TVAR2N=U,TVAR2S=GAM ARE WORKING ARRAYS  
            DO L=2,LA  
              TVAR3S(L)=TVAR1W(L,1)  
            ENDDO  
            DO L=2,LA  
              TVAR2N(L,1)=TVAR1E(L,1)/TVAR3S(L)  
            ENDDO  
            DO K=2,KC  
              DO L=2,LA  
                TVAR2S(L,K)=TVAR1N(L,K-1)/TVAR3S(L)  
                TVAR3S(L)=TVAR1W(L,K)-TVAR1S(L,K)*TVAR2S(L,K)  
                TVAR2N(L,K)=(TVAR1E(L,K)-TVAR1S(L,K)*TVAR2N(L,K-1))/  
     &              TVAR3S(L)  
              ENDDO  
            ENDDO  
            DO K=KS,1,-1  
              DO L=2,LA  
                TVAR2N(L,K)=TVAR2N(L,K)-TVAR2S(L,K+1)*TVAR2N(L,K+1)  
              ENDDO  
            ENDDO  
            DO K=1,KC  
              DO L=2,LA  
                SND(L,K,NX)=TVAR2N(L,K)  
              ENDDO  
            ENDDO  
C  
          ENDIF  
C  
C----------------------------------------------------------------------C  
C  
C **  FINAL FLUX KC.GE.3  
C  
          DO L=2,LA  
            SNDF(L,KS,NX)=DELTI*DZC(KC)*HP(L)*  
     &          (SND(L,KC,NX)-SNDS(L,KC,NX))  
          ENDDO  
C  
          DO K=KS-1,1,-1  
            DO L=2,LA  
              SNDF(L,K,NX)=DELTI*DZC(K+1)*HP(L)*  
     &            (SND(L,K+1,NX)-SNDS(L,K+1,NX))+SNDF(L,K+1,NX)  
            ENDDO  
          ENDDO  
C  
C----------------------------------------------------------------------C  
C  
        ENDDO  
      ENDIF  
C  
C**********************************************************************C  
C  
      IF(DEBUG)THEN
        IFLAG=0  
        DO NS=1,NSND  
          DO K=1,KC  
            DO L=2,LA  
              IF(SND(L,K,NS).LT.0.)THEN  
                IF(IFLAG.EQ.0)THEN  
                  OPEN(1,FILE='NEGSEDSND.OUT',POSITION='APPEND')  
                  IFLAG=1  
                ENDIF  
                WRITE(1,107)TIME,NS,IL(L),JL(L),K,SND(L,K,NS)  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDDO  
C  
        DO NS=1,NSND  
          DO L=2,LA  
            IF(SNDB(L,KBT(L),NS).LT.0.)THEN  
              IF(IFLAG.EQ.0)THEN  
                OPEN(1,FILE='NEGSEDSND.OUT',POSITION='APPEND')  
                IFLAG=1  
              ENDIF  
              WRITE(1,108)TIME,NS,IL(L),JL(L),KBT(L),SNDB(L,KBT(L),NS),  
     &          SNDF(L,0,NS)  


              SNDB(L,KBT(L),NS)=0.0


            ENDIF  
          ENDDO  
        ENDDO  
C  
        IF(IFLAG.EQ.1)CLOSE(1) 
      ENDIF
C  
C **  ACCUMULATE NET POSTIVE AND NEGATIVE NONCOHESIVE SEDIMENT FLUXES  
C  
      DO NS=1,NSND  
        DO L=2,LA  
          SNDFDTAP(L,NS)=SNDFDTAP(L,NS)+DELT*MAX(SNDF(L,0,NS),0.0)  
          SNDFDTAN(L,NS)=SNDFDTAN(L,NS)+DELT*MIN(SNDF(L,0,NS),0.0)  
        ENDDO  
      ENDDO  
C  
  107 FORMAT(' TIME,NS,I,J,K,NEGSND = ',F12.4,4I5,4E13.4)  
  108 FORMAT(' TIME,NS,I,J,NEGSNDB,SNDF = ',F12.4,4I5,4E13.4)  
C  
C**********************************************************************C  
C  
      RETURN  
      END  
