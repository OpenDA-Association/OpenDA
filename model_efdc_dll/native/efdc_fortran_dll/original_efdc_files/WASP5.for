      SUBROUTINE WASP5  
C  
C CHANGE RECORD  
C **  SUBROUTINE WASP5 WRITES OUTPUT FILES PROVIDING ADVECTIVE AND  
C **  DIFFUSIVE TRANSPORT FIELDS FOR THE WASP5 WATER QUALITY MODEL  
C  
      USE GLOBAL  
      CHARACTER*50 TITLEB,TITLEC  
      CHARACTER*12 HYDFIL  

      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LDTMP  
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LUTMP  
      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QTMP  

      IF(.NOT.ALLOCATED(LDTMP))THEN
		ALLOCATE(LDTMP(KCM*LCM))
		ALLOCATE(LUTMP(KCM*LCM))
		ALLOCATE(QTMP(KCM*LCM))
	    LDTMP=0 
	    LUTMP=0 
	    QTMP=0.0 
	ENDIF
      TITLEB='DIFFUSIVE FLUX'  
      TITLEC='VOLUME INFO'  
C  
C **  WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
C **  THE VALUE OF X IN THE F10.X FORMATS MAY NEED TO BE CHANGED  
C **  FROM PROBLEM TO PROBLEM.  A PRELIMINARY RUN USING E10.3  
C **  CAN BE USED TO SPEED THE ADJUSTMENT  
C **  READ CONTROL DATA FOR WRITING TO WASP COMPATIBLE FILES  
C  
      SVPT=1.  
      IF(NTSMMT.LT.NTSPTC)SVPT=0.  
      IF(JSWASP.EQ.1)THEN  
        OPEN(1,FILE='EFDC.WSP',STATUS='UNKNOWN')  
        READ(1,1)  
        READ(1,1)  
        READ(1,*) IVOPT,IBEDV,SCALV,CONVV,VMULT,VEXP,DMULT,DEXP  
        READ(1,1)  
        READ(1,1)  
        READ(1,*) NRFLD,SCALR,CONVR,ISNKH  
        READ(1,1)  
        READ(1,1)  
        READ(1,*) IQOPT,NFIELD,SCALQ,CONVQ,HYDFIL,ISWASPD,ISDHD  
        READ(1,1)  
        READ(1,1)  
        READ(1,*) DEPSED,TDINTS,SEDIFF  
        CLOSE(1)  
      ENDIF  
    1 FORMAT (80X)  
C  
C **  WRITE HORIZONTAL POSITION AND LAYER FILE WASPP.OUT  
C **  WRITE INITIAL VOLUME FILE WASPC.OUT  
C **  FILE WASPC.OUT IS CONSISTENT WITH DATA GROUP C SPECIFICATIONS  
C **  ON PAGE 11 OF THE WASP5.1 MANUAL PART B, SEPT 1993  
C **  FILE WASPP.OUT DEFINES THE LAYER (1 IS SURFACE WATER LAYER, WITH  
C **  LAYER NUMBERING INCREASING WITH DEPTH IN WATER COLUMN) AND  
C **  HORIZONTAL POSITIONS IN LON,LAT OR UTME, UTMN OF THE WATER  
C **  QUALITY (LONG TERM TRANSPORT) CELLS OR SEGEMENTS  
C  
      IF(JSWASP.EQ.1)THEN  
        OPEN(90,FILE='WASPP.OUT',STATUS='UNKNOWN')  
        OPEN(93,FILE='WASPC.OUT',STATUS='UNKNOWN')  
        CLOSE(90,STATUS='DELETE')  
        CLOSE(93,STATUS='DELETE')  
        OPEN(90,FILE='WASPP.OUT',STATUS='UNKNOWN')  
        OPEN(93,FILE='WASPC.OUT',STATUS='UNKNOWN')  
C  
C       IVOPT=2  
C       IBEDV=0  
C  
        WRITE(93,1031)IVOPT,IBEDV,TDINTS,TITLEC  
C  
C       SCALV=1.  
C       CONVV=1.  
C  
        WRITE(93,1032)SCALV,CONVV  
C  
C       VMULT=0.  
C       VEXP=0.  
C       DMULT=0.  
C       DEXP=0.  
C  
        LCLTM2=LCLT-2  
        LWASP=0  
        IF(KC.GT.1)THEN  
          LTYPE=1  
          KWASP=1  
          DO LT=2,LALT  
            LWASP=LWASP+1  
            LBELOW=LWASP+LCLTM2  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            VOLUME=DXYP(L)*HLPF(L)*DZC(KC)  
            WRITE(90,1001)LWASP,KWASP,I,J,DLON(L),DLAT(L)  
            WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP,  
     &          DMULT,DEXP  
          ENDDO  
          LTYPE=2  
          DO K=KS,2,-1  
            KWASP=KC-K+1  
            DO LT=2,LALT  
              LWASP=LWASP+1  
              LBELOW=LWASP+LCLTM2  
              I=ILLT(LT)  
              J=JLLT(LT)  
              L=LIJ(I,J)  
              VOLUME=DXYP(L)*HLPF(L)*DZC(K)  
              WRITE(90,1001)LWASP,KWASP,I,J,DLON(L),DLAT(L)  
              WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP,  
     &            DMULT,DEXP  
            ENDDO  
          ENDDO  
        ENDIF  
        LTYPE=2  
        IF(KC.EQ.1) LTYPE=1  
        KWASP=KC  
        DO LT=2,LALT  
          LWASP=LWASP+1  
C  
C        LBELOW=0  
C  
          LBELOW=LWASP+LCLTM2  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
          VOLUME=DXYP(L)*HLPF(L)*DZC(1)  
          WRITE(90,1001)LWASP,KWASP,I,J,DLON(L),DLAT(L)  
          WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP,  
     &        DMULT,DEXP  
        ENDDO  
        LTYPE=3  
        KWASP=KC+1  
        DXYSUM=0.  
        LWSPTMP=LWASP+1  
        DO LT=2,LALT  
          LWSPTMP=LWSPTMP+1  
        ENDDO  
        DO LT=2,LALT  
          LWASP=LWASP+1  
          LBELOW=LWSPTMP  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
          DXYSUM=DXYSUM+DXYP(L)  
          VOLUME=DXYP(L)*DEPSED  
          WRITE(90,1001)LWASP,KWASP,I,J,DLON(L),DLAT(L)  
          WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP,  
     &        DMULT,DEXP  
        ENDDO  
        LTYPE=4  
        KWASP=KC+2  
        LWASP=LWASP+1  
        LBELOW=0  
        VOLUME=DXYSUM*DEPSED  
        WRITE(90,1001)LWASP,KWASP,I,J,DLON(L),DLAT(L)  
        WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP,  
     &      DMULT,DEXP  
        CLOSE(90)  
        CLOSE(93)  
      ENDIF  
 1001 FORMAT(4I5,2F10.4)  
 1031 FORMAT(2I5,F10.4,10X,A50)  
 1032 FORMAT(2F10.4)  
 1033 FORMAT(3I5,F20.8,4F10.2)  
C  
C **  WRITE DIFFUSIVE AND DISPERSIVE TRANSPORT FILE WASPB.OUT  
C **  FILE WASPB.OUT IS CONSISTENT WITH DATA GROUP C SPECIFICATIONS  
C **  ON PAGE 8 OF THE WASP5.1 MANUAL PART B, SEPT 1993  
C  
      IF(JSWASP.EQ.1)THEN  
        OPEN(91,FILE='WASPB.OUT',STATUS='UNKNOWN')  
        CLOSE(91,STATUS='DELETE')  
        OPEN(91,FILE='WASPB.OUT',STATUS='UNKNOWN')  
C  
C       NRFLD=1  
C  
        WRITE(91,1011)NRFLD,TITLEB  
        NTEX=NTS/NTSMMT  
C  
C       SCALR=1.  
C       CONVR=1.  
C  
        WRITE(91,1012)NTEX,SCALR,CONVR  
        CLOSE(91)  
      ENDIF  
      OPEN(91,FILE='WASPB.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      LCLTM2=LCLT-2  
      NORSH=0  
      NORSV=0  
      DO LT=2,LALT  
        I=ILLT(LT)  
        J=JLLT(LT)  
        L=LIJ(I,J)  
        NORSH=NORSH+INT(SUBO(L))+INT(SVBO(L))  
        NORSV=NORSV+INT(SPB(L))  
      ENDDO  
      NORS=ISNKH*KC*NORSH+KS*NORSV  
      WRITE(91,1013)NORS  
      IF(ISNKH.EQ.1)THEN  
        UNITY=1.  
        DO K=KC,1,-1  
          KMUL=KC-K  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            IF(SUB(L).EQ.1.)THEN  
              LWASP=LT-1+KMUL*LCLTM2  
              LWASPW=LWASP-1  
              LW=L-1  
              ADDLW=DYU(L)*AHULPF(L,K)*DZC(K)*0.5*(HLPF(L)  
     &            +HLPF(LW))*DXIU(L)  
              WRITE(91,1014)ADDLW,UNITY,LWASPW,LWASP  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(ISNKH.EQ.1)THEN  
        UNITY=1.  
        DO K=KC,1,-1  
          KMUL=KC-K  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            IF(SVB(L).EQ.1.)THEN  
              LWASP=LT-1+KMUL*LCLTM2  
              LSLT=LSCLT(LT)  
              LWASPS=LSLT-1+KMUL*LCLTM2  
              LS=LSC(L)  
              ADDLS=DXV(L)*AHVLPF(L,K)*DZC(K)*0.5*(HLPF(L)  
     &            +HLPF(LS))*DYIV(L)  
              WRITE(91,1014)ADDLS,UNITY,LWASPS,LWASP  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      IF(KC.GT.1)THEN  
        UNITY=1.  
        DO K=KS,1,-1  
          KMUL1=KS-K  
          KMUL2=KMUL1+1  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            IF(SPB(L).EQ.1.)THEN  
              LWASP=LT-1+KMUL1*LCLTM2  
              LBELOW=LT-1+KMUL2*LCLTM2  
              ADDL=DXYP(L)*ABLPF(L,K)*DZIG(K)  
              WRITE(91,1014)ADDL,UNITY,LWASP,LBELOW  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      NBRK=6  
      WRITE(91,1015)NBRK  
      IF(ISDYNSTP.EQ.0)THEN  
        TSTOP=DT*FLOAT(N)+TCON*TBEGIN  
        TSTART=TSTOP-DT*FLOAT(NTSMMT)  
      ELSE  
        TSTOP=TENDRNSEC  
        TSTART=TSTOP-DT*FLOAT(NTSMMT)  
      ENDIF  
      TSTOP=TSTOP/86400.  
      TSTART=TSTART/86400.  
      TSMALL=1.E-5  
      D1=0.  
      T1=0.-2*TSMALL  
      D2=0.  
      T2=TSTART-TSMALL  
      D3=1.  
      T3=TSTART+TSMALL  
      D4=1.  
      T4=TSTOP-TSMALL  
      D5=0.  
      T5=TSTOP+TSMALL  
      D6=0.  
      T6=2*TSMALL+(DT*FLOAT(NTS)+TBEGIN*TCON)/86400.  
      WRITE(91,1016)D1,T1,D2,T2,D3,T3,D4,T4  
      WRITE(91,1016)D5,T5,D6,T6  
      CLOSE(91)  
C  
C **  ADD PORE WATER EXCHANGE FIELD ON LAST CALL  
C  
      IF(N.GE.NTS)THEN  
        OPEN(91,FILE='WASPB.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        NTEX=1  
        SCALR=1.  
        CONVR=1.  
        WRITE(91,1012)NTEX,SCALR,CONVR  
        NORSV=0  
        DO LT=2,LALT  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
          NORSV=NORSV+INT(SPB(L))  
        ENDDO  
        WRITE(91,1013)NORSV  
        IF(KC.GE.1)THEN  
          KMUL2=KC+1  
          UNITY=1.  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            IF(SPB(L).EQ.1.)THEN  
              LWASP=LT-1+KC*LCLTM2  
              LBELOW=LT-1+KMUL2*LCLTM2  
              ADDL=2.*DXYP(L)*SEDIFF/DEPSED  
              WRITE(91,1014)ADDL,UNITY,LWASP,LBELOW  
            ENDIF  
          ENDDO  
        ENDIF  
        NBRK=6  
        WRITE(91,1015)NBRK  
        IF(ISDYNSTP.EQ.0)THEN  
          TSTOP=DT*FLOAT(N)+TCON*TBEGIN  
          TSTART=TSTOP-DT*FLOAT(NTSMMT)  
        ELSE  
          TSTOP=TENDRNSEC  
          TSTART=TSTOP-DT*FLOAT(NTSMMT)  
        ENDIF  
        TSTOP=TSTOP/86400.  
        TSTART=TSTART/86400.  
        TSMALL=1.E-5  
        D1=0.  
        T1=0.-2*TSMALL  
        D2=0.  
        T2=TSTART-TSMALL  
        D3=1.  
        T3=TSTART+TSMALL  
        D4=1.  
        T4=TSTOP-TSMALL  
        D5=0.  
        T5=TSTOP+TSMALL  
        D6=0.  
        T6=2*TSMALL+(DT*FLOAT(NTS)+TBEGIN*TCON)/86400.  
        WRITE(91,1016)D1,T1,D2,T2,D3,T3,D4,T4  
        WRITE(91,1016)D5,T5,D6,T6  
        IBPTMP=0  
        WRITE(91,1017)IBPTMP,IBPTMP,IBPTMP,IBPTMP,  
     &      IBPTMP,IBPTMP,IBPTMP,IBPTMP,  
     &      IBPTMP,IBPTMP,IBPTMP,IBPTMP,  
     &      IBPTMP,IBPTMP,IBPTMP,IBPTMP  
        CLOSE(91)  
      ENDIF  
 1011 FORMAT(I5,10X,A50)  
 1012 FORMAT(I5,2F10.4)  
 1013 FORMAT(I5)  
 1014 FORMAT(2E10.3,2I5)  
 1015 FORMAT(I5)  
 1016 FORMAT(4(2F10.5))  
 1017 FORMAT(16I5)  
C  
C **  WRITE ADVECTIVE TRANSPORT FILE WASPD.OUT  
C **  FILE WASPD.OUT IS CONSISTENT WITH DATA GROUP D.1 SPECIFICATIONS  
C **  ON PAGE 13 OF THE WASP5.1 MANUAL PART B, SEPT 1993  
C **  THIS FILE IS WRITTEN ONLY IF ISWASPD=1  
C!!!!!!!!!!CHANGES ON NEXT 2 LINES  
C  
      IF(ISWASPD.EQ.1)THEN  
        IF(JSWASP.EQ.1)THEN  
          OPEN(92,FILE='WASPD.OUT',STATUS='UNKNOWN')  
          CLOSE(92,STATUS='DELETE')  
          OPEN(92,FILE='WASPD.OUT',STATUS='UNKNOWN')  
C  
C       IQOPT=1  
C       NFIELD=1  
C  
          WRITE(92,1021)IQOPT,NFIELD,HYDFIL  
          NINQ=NTS/NTSMMT  
C  
C       SCALQ=1  
C       CONVQ=1  
C  
          WRITE(92,1022)NINQ,SCALQ,CONVQ  
          CLOSE(92)  
        ENDIF  
        OPEN(92,FILE='WASPD.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
        LCLTM2=LCLT-2  
        NOQSH=0  
        NOQSV=0  
        DO LT=2,LALT  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
C  
C!!!!!!!!!!!!!!CHANGES ON NEXT 3 LINES  
C  
          NOQSH=NOQSH+INT(SUBO(L))+INT(SVBO(L))  
          IF(IJCTLT(I+1,J).EQ.8) NOQSH=NOQSH+1  
          IF(IJCTLT(I,J+1).EQ.8) NOQSH=NOQSH+1  
          NOQSV=NOQSV+INT(SWB(L))  
        ENDDO  
        NOQS=KC*NOQSH+KS*NOQSV  
        WRITE(92,1023)NOQS  
        LL=0  
        DO K=KC,1,-1  
          KMUL=KC-K  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
C  
C!!!!!!!!!!!!!CHANGES ON NEXT 15 LINES  
C  
            IF(SUBO(L).EQ.1.)THEN  
              LL=LL+1  
              LDTMP(LL)=LT-1+KMUL*LCLTM2  
              LUTMP(LL)=LDTMP(LL)-1  
              IF(IJCTLT(I-1,J).EQ.8) LUTMP(LL)=0  
              QTMP(LL)=DYU(L)*(UHLPF(L,K)+SVPT*UVPT(L,K))*DZC(K)  
            ENDIF  
            IF(IJCTLT(I+1,J).EQ.8)THEN  
              IF(SUBO(L+1).EQ.1.)THEN  
                LL=LL+1  
                LDTMP(LL)=0  
                LUTMP(LL)=LT-1+KMUL*LCLTM2  
                QTMP(LL)=DYU(L+1)*(UHLPF(L+1,K)+SVPT*UVPT(L+1,K))*DZC(K)  
              ENDIF  
            ENDIF  
          ENDDO  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
C  
C!!!!!!!!!!!!!CHANGES ON NEXT 16 LINES  
C  
            IF(SVBO(L).EQ.1.)THEN  
              LL=LL+1  
              LSLT=LSCLT(LT)  
              LDTMP(LL)=LT-1+KMUL*LCLTM2  
              LUTMP(LL)=LSLT-1+KMUL*LCLTM2  
              IF(IJCTLT(I,J-1).EQ.8) LUTMP(LL)=0  
              QTMP(LL)=DXV(L)*(VHLPF(L,K)+SVPT*VVPT(L,K))*DZC(K)  
            ENDIF  
            IF(IJCTLT(I,J+1).EQ.8)THEN  
              LN=LNC(L)  
              IF(SVBO(LN).EQ.1)THEN  
                LL=LL+1  
                LDTMP(LL)=0  
                LUTMP(LL)=LT-1+KMUL*LCLTM2  
                QTMP(LL)=DXV(LN)*(VHLPF(LN,K)+SVPT*VVPT(LN,K))*DZC(K)  
              ENDIF  
            ENDIF  
          ENDDO  
        ENDDO  
        IF(KC.GT.1)THEN  
          DO K=KS,1,-1  
            KMUL1=KS-K  
            KMUL2=KMUL1+1  
            DO LT=2,LALT  
              I=ILLT(LT)  
              J=JLLT(LT)  
              L=LIJ(I,J)  
              IF(SWB(L).EQ.1.)THEN  
                LL=LL+1  
                LUTMP(LL)=LT-1+KMUL1*LCLTM2  
                LDTMP(LL)=LT-1+KMUL2*LCLTM2  
                QTMP(LL)=-DXYP(L)*(WLPF(L,K)+SVPT*WVPT(L,K))  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
        DO L=1,LL,4  
          WRITE(92,1024) QTMP(L),  LUTMP(L),  LDTMP(L),  
     &        QTMP(L+1),LUTMP(L+1),LDTMP(L+1),  
     &        QTMP(L+2),LUTMP(L+2),LDTMP(L+2),  
     &        QTMP(L+3),LUTMP(L+3),LDTMP(L+3)  
        ENDDO  
        NBRKQ=6  
        WRITE(92,1025)NBRKQ  
        WRITE(92,1026)D1,T1,D2,T2,D3,T3,D4,T4  
        WRITE(92,1026)D5,T5,D6,T6  
        CLOSE(92)  
C  
C!!!!!!!!!!CHANGES ON NEXT 2 LINES  
C  
      ENDIF  
 1021 FORMAT(2I5,A12)  
 1022 FORMAT(I5,2F10.4)  
 1023 FORMAT(I5)  
 1024 FORMAT(4(E10.3,2I5))  
 1025 FORMAT(I5)  
 1026 FORMAT(4(2F10.5))  
C  
C **  WRITE TO EXTERNAL HYDRO FILE WASPDH.OUT AND DIAGNOSTIC VERSION  
C **  OF SAME FILE WASPDHD.OUT  
C  
      IF(JSWASP.EQ.1)THEN  
        OPEN(90,FILE='WASPDHD.OUT',STATUS='UNKNOWN')  
        OPEN(94,FILE='WASPDH.OUT',STATUS='UNKNOWN')  
        OPEN(99,FILE='ADVMOD.WSP',STATUS='UNKNOWN')  
        CLOSE(90,STATUS='DELETE')  
        CLOSE(94,STATUS='DELETE')  
        CLOSE(99,STATUS='DELETE')  
        OPEN(90,FILE='WASPDHD.OUT',STATUS='UNKNOWN')  
        OPEN(94,FILE='WASPDH.OUT',STATUS='UNKNOWN')  
        OPEN(99,FILE='ADVMOD.WSP',STATUS='UNKNOWN')  
C  
C        NCHNC(KL)=0  
C         LCHNC(KL,M)=0  
C  
        NJUN=KC*(LCLT-2)  
        NCHNH=0  
        NCHNV=0  
C  
C!!!!!!!!CHANGES NEXT 13 LINES  
C  
        DO LT=2,LALT  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
          NCHNH=NCHNH+INT(SUBO(L))  
          IF(IJCTLT(I+1,J).EQ.8)THEN  
            IF(SUBO(L+1).EQ.1.) NCHNH=NCHNH+1  
          ENDIF  
          NCHNH=NCHNH+INT(SVBO(L))  
          IF(IJCTLT(I,J+1).EQ.8)THEN  
            IF(SVBO(LNC(L)).EQ.1.) NCHNH=NCHNH+1  
          ENDIF  
          NCHNV=NCHNV+INT(SWB(L))  
        ENDDO  
        NCHN=KC*NCHNH+KS*NCHNV  
        ISTMP=0  
        NODYN=NFLTMT  
        TZERO=TBEGIN*TCON  
        TENDHYD=TZERO+NTS*DT  
        IF(ISDHD.EQ.1)WRITE(90,941)NJUN,NCHN,DT,TZERO,TENDHYD,ISTMP  
        WRITE(94,941)NJUN,NCHN,DT,TZERO,TENDHYD,ISTMP  
C  
C **  CHANNEL DATA  
C  
        RADVMOD=1.0  
        RMNDUM=0.  
        LCHN=0  
        DO K=KC,1,-1  
          KMUL=KC-K  
          KLAYTMP=KMUL+1  
C  
C!!!!!!!!!!!!!!CHANGES ON NEXT 38 LINES  
C  
          DO LT=2,LALT  
            I=ILLT(LT)  
            IM1=I-1  
            IP1=I+1  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            IF(SUBO(L).EQ.1.)THEN  
              LDTM=LT-1+KMUL*LCLTM2  
              LUTM=LDTM-1  
              IF(IJCTLT(I-1,J).EQ.8) LUTM=0  
              RLENTH=DXU(L)  
              WIDTH=DYU(L)  
              LCHN=LCHN+1  
C  
C           LCHNC(LDTM,NCHNC(LDTM))=LCHN  
C           LCHNC(LUTM,NCHNC(LUTM))=LCHN  
C  
              IF(ISDHD.EQ.1) WRITE(90,902)LCHN,RLENTH,WIDTH,RMNDUM,  
     &            LUTM,LDTM  
              WRITE(94,941)LUTM,LDTM  
              WRITE(99,991)LUTM,LDTM,RADVMOD,IM1,J,I,J,KLAYTMP  
            ENDIF  
            IF(IJCTLT(I+1,J).EQ.8)THEN  
              IF(SUBO(L+1).EQ.1.)THEN  
                LDTM=0  
                LUTM=LT-1+KMUL*LCLTM2  
                RLENTH=DXU(L+1)  
                WIDTH=DYU(L+1)  
                LCHN=LCHN+1  
C  
C             LCHNC(LDTM,NCHNC(LDTM))=LCHN  
C             LCHNC(LUTM,NCHNC(LUTM))=LCHN  
C  
                IF(ISDHD.EQ.1) WRITE(90,902)LCHN,RLENTH,WIDTH,RMNDUM,  
     &              LUTM,LDTM  
                WRITE(94,941)LUTM,LDTM  
                WRITE(99,991)LUTM,LDTM,RADVMOD,I,J,IP1,J,KLAYTMP  
              ENDIF  
            ENDIF  
          ENDDO  
C  
C!!!!!!!!CHANGES NEXT 41 LINES  
C  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            JM1=J-1  
            JP1=J+1  
            L=LIJ(I,J)  
            IF(SVBO(L).EQ.1.)THEN  
              LSLT=LSCLT(LT)  
              LDTM=LT-1+KMUL*LCLTM2  
              LUTM=LSLT-1+KMUL*LCLTM2  
              IF(IJCTLT(I,J-1).EQ.8) LUTM=0  
              RLENTH=DYV(L)  
              WIDTH=DXV(L)  
              LCHN=LCHN+1  
C  
C           LCHNC(LDTM,NCHNC(LDTM))=LCHN  
C           LCHNC(LUTM,NCHNC(LUTM))=LCHN  
C  
              IF(ISDHD.EQ.1) WRITE(90,902)LCHN,RLENTH,WIDTH,RMNDUM,  
     &            LUTM,LDTM  
              WRITE(94,941)LUTM,LDTM  
              WRITE(99,991)LUTM,LDTM,RADVMOD,I,JM1,I,J,KLAYTMP  
            ENDIF  
            IF(IJCTLT(I,J+1).EQ.8)THEN  
              LN=LNC(L)  
              IF(SVBO(LN).EQ.1.)THEN  
                LSLT=LSCLT(LT)  
                LDTM=0  
                LUTM=LT-1+KMUL*LCLTM2  
                RLENTH=DYV(LN)  
                WIDTH=DXV(LN)  
                LCHN=LCHN+1  
C  
C             LCHNC(LDTM,NCHNC(LDTM))=LCHN  
C             LCHNC(LUTM,NCHNC(LUTM))=LCHN  
C  
                IF(ISDHD.EQ.1) WRITE(90,902)LCHN,RLENTH,WIDTH,RMNDUM,  
     &              LUTM,LDTM  
                WRITE(94,941)LUTM,LDTM  
                WRITE(99,991)LUTM,LDTM,RADVMOD,I,J,I,JP1,KLAYTMP  
              ENDIF  
            ENDIF  
          ENDDO  
        ENDDO  
        IF(KC.GT.1)THEN  
          DO K=KS,1,-1  
            KMUL1=KS-K  
            KMUL2=KMUL1+1  
            KFROM=KMUL1+1  
            KT000=KFROM+1  
            DO LT=2,LALT  
              I=ILLT(LT)  
              J=JLLT(LT)  
              L=LIJ(I,J)  
              IF(SWB(L).EQ.1.)THEN  
                LUTM=LT-1+KMUL1*LCLTM2  
                LDTM=LT-1+KMUL2*LCLTM2  
                RLENTH=HLPF(L)*DZG(K)  
                WIDTH=SQRT(DXYP(L))  
                LCHN=LCHN+1  
C  
C             LCHNC(LDTM,NCHNC(LDTM))=LCHN  
C             LCHNC(LUTM,NCHNC(LUTM))=LCHN  
C  
                IF(ISDHD.EQ.1) WRITE(90,902)LCHN,RLENTH,WIDTH,RMNDUM,  
     &              LUTM,LDTM  
                WRITE(94,941)LUTM,LDTM  
                WRITE(99,992)LUTM,LDTM,RADVMOD,I,J,KFROM,KT000  
              ENDIF  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  JUNCTION DATA WITH INITIAL CONDITIONS  
C  
        VELTMP=0.  
        DUMVOL=0.  
        DO K=KC,1,-1  
          KMUL=KC-K  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            LCELL=LT-1+KMUL*LCLTM2  
            DEPTMP=HLPF(L)*DZC(K)  
            VOLTMP=DEPTMP*DXYP(L)  
            IF(ISDHD.EQ.1) WRITE(90,904)LCELL,VOLTMP,I,J  
            WRITE(94,9440)VOLTMP,DUMVOL,DEPTMP,VELTMP  
          ENDDO  
        ENDDO  
        CLOSE(90)  
        CLOSE(94)  
        CLOSE(99)  
      ENDIF  
  992 FORMAT(2I5,2X,F6.2,'   ! I,J = ',2I5,' FROM LAYER = ',I5  
     &    ,' TO LAYER = ',I5)  
  991 FORMAT(2I5,2X,F6.2,'   ! FROM I,J = ',2I5,' TO I,J = ',2I5  
     &    ,' LAYER = ',I5)  
C  
C **  WRITE TIME STEP, VOLUME AND FLOW DATA  
C  
      OPEN(90,FILE='WASPDHD.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      OPEN(94,FILE='WASPDH.OUT',POSITION='APPEND',STATUS='UNKNOWN')  
      LCLTM2=LCLT-2  
      IZERO=0  
      RZERO=0.0  
      LCHNUM=0  
      DO K=KC,1,-1  
        DO LT=2,LALT  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
C  
C!!!!!!!!CHANGES NEXT 12 LINES  
C  
          IF(SUBO(L).EQ.1.)THEN  
            FLOWX=DYU(L)*(UHLPF(L,K)+SVPT*UVPT(L,K))*DZC(K)  
            IMTMP=I-1  
            LCNHUM=LCHNUM+1  
            IF(ISDHD.EQ.1) WRITE(90,944)LCHNUM,FLOWX,IMTMP,I,J,K  
            WRITE(94,946)FLOWX  
          ENDIF  
          IF(IJCTLT(I+1,J).EQ.8)THEN  
            IF(SUBO(L+1).EQ.1.)THEN  
              FLOWX=DYU(L+1)*(UHLPF(L+1,K)+SVPT*UVPT(L+1,K))*DZC(K)  
              IPTMP=I+1  
              LCNHUM=LCHNUM+1  
              IF(ISDHD.EQ.1) WRITE(90,944)LCHNUM,FLOWX,I,IPTMP,J,K  
              WRITE(94,946)FLOWX  
            ENDIF  
          ENDIF  
        ENDDO  
        DO LT=2,LALT  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
C  
C!!!!!!!CHANGES NEXT 13 LINES  
C  
          IF(SVBO(L).EQ.1.)THEN  
            FLOWY=DXV(L)*(VHLPF(L,K)+SVPT*VVPT(L,K))*DZC(K)  
            JMTMP=J-1  
            LCNHUM=LCHNUM+1  
            IF(ISDHD.EQ.1) WRITE(90,944)LCHNUM,FLOWY,I,IPTMP,J,K  
            WRITE(94,946)FLOWY  
          ENDIF  
          IF(IJCTLT(I,J+1).EQ.8)THEN  
            LN=LNC(L)  
            IF(SVBO(LN).EQ.1.)THEN  
              FLOWY=DXV(LN)*(VHLPF(LN,K)+SVPT*VVPT(LN,K))*DZC(K)  
              JPTMP=J+1  
              LCNHUM=LCHNUM+1  
              IF(ISDHD.EQ.1) WRITE(90,944)LCHNUM,FLOWY,I,IPTMP,J,K  
              WRITE(94,946)FLOWY  
            ENDIF  
          ENDIF  
        ENDDO  
      ENDDO  
      IF(KC.GT.1)THEN  
        DO K=KS,1,-1  
          DO LT=2,LALT  
            I=ILLT(LT)  
            J=JLLT(LT)  
            L=LIJ(I,J)  
            IF(SWB(L).EQ.1)THEN  
              FLOWZ=-DXYP(L)*(WLPF(L,K)+SVPT*WVPT(L,K))  
              KPTMP=K+1  
              LCNHUM=LCHNUM+1  
              IF(ISDHD.EQ.1) WRITE(90,944)LCHNUM,FLOWZ,I,IPTMP,J,K  
              WRITE(94,946)FLOWZ  
            ENDIF  
          ENDDO  
        ENDDO  
      ENDIF  
      QQSUM=0.  
      LCELTMP=0  
      DO K=KC,1,-1  
        DO LT=2,LALT  
          LCELTMP=LCELTMP+1  
          I=ILLT(LT)  
          J=JLLT(LT)  
          L=LIJ(I,J)  
          LN=LNC(L)  
          VOLUM=DXYP(L)*HLPF(L)*DZC(K)  
          DEPTH=HLPF(L)*DZC(K)  
          VELX=0.5*(UHLPF(L,K)+SVPT*UVPT(L,K)  
     &        +UHLPF(L+1,K)+SVPT*UVPT(L+1,K))/HLPF(L)  
          VELY=0.5*(VHLPF(L,K)+SVPT*VVPT(L,K)  
     &        +VHLPF(LN,K)+SVPT*VVPT(LN,K))/HLPF(L)  
          VELZ=0.5*(WLPF(L,K-1)+SVPT*WVPT(L,K-1)  
     &        +WLPF(L,K)+SVPT*WVPT(L,K))  
          VELMAG=SQRT(VELX*VELX+VELY*VELY+VELZ*VELZ)  
          IF(ISDHD.EQ.1) WRITE(90,904)LCELTMP,VOLUM,I,J,K  
          WRITE(94,946)VOLUM,QQSUM,DEPTH,VELMAG  
        ENDDO  
      ENDDO  
      CLOSE(90)  
      CLOSE(94)  
  901 FORMAT(2I5,E12.4,4I5,E12.4)  
  902 FORMAT(I5,2X,3F20.8,3I5)  
  903 FORMAT(3E12.4,2I5)  
  904 FORMAT(I5,2X,F20.8,10I5)  
  905 FORMAT(I5)  
  906 FORMAT(5E12.4)  
  941 FORMAT(2I5,3F20.8,I5)  
  942 FORMAT(3E12.4,2I5)  
  943 FORMAT(3E12.4,2I5)  
  944 FORMAT(I5,2X,F20.8,10I5)  
 9440 FORMAT(4F20.8)  
  945 FORMAT(I5)  
  946 FORMAT(4F20.8)  
      JSWASP=0  
      RETURN  
      END  

