C Paul 

C please search for June 13 2008 to see code change 
C this is EFDC_DS version of subroutine that does not have sal,temp linkage data 
C written to HYD file

C Andy
C 
C**********************************************************************C
C**********************************************************************C
C**********************************************************************C
C
      SUBROUTINE WASP7EPA
C------------------------------------------------------------------------------
C PURPOSE:
C
C   Subroutine WASP7 writes output files providing advective and
C   diffusive transport fields for the WASP5 water quality model.
C
C VARIABLE LIST:
C
C MODIFICATION HISTORY:
C
C   Date       Author         Comments
C   ---------- -------------- -------------------------------------------------
C   06/06/1994 Mike Morton    This version writes dispersion to the
C                             WASPDH.out hydrodynamic instead of WASPB.OUT.
C                             A modified version of WASP5 (Tetra Tech's
C                             version) is necessary to utilize these
C                             dispersion values.
C   06/07/1994 Mike Morton    Writes hydrodynamic information and dispersion
C                             to an unformatted binary file, WASPDHU.OUT.
C                             The correct files for WASP data groups B, C,
C                             and D are:
C                             Data Group B use WASPB.MRM  (do not use WASPB.OUT)
C                             Data Group C use WASPC.OUT
C                             Data Group D use WASPD.MRM  (do not use WASPD.OUT)
C   07/15/2000 John Hamrick   Modified for use with Neuse River WASP model;
C                             changed references of EFDC cell type 6 to type 8;
C                             cell type stored in array IJCTLT(I,J).
C   02/16/2001 Mike Morton    Cleaned code, reformatted code
C   07/08/2002 Hugo Rodriguez Include input flows and structures flows in the wasp hydro file
C   07/09/2002 Hugo Rodriguez Starts writing to HYDFIL IDAYS days after the simulation starts
C   11/15/2003 Hugo Rodriguez Correct the writing to the hyd file for 'V+1'
C------------------------------------------------------------------------------
C
      USE GLOBAL  

      INTEGER LU,LD,IU,ID,JU,JD,IDAYS,IBEGIN,NBEGIN,NAUX

      REAL,SAVE,ALLOCATABLE,DIMENSION(:)::QTMP  
      CHARACTER*50 TITLEB,TITLEC
      CHARACTER*20 HYDFIL
	REAL*8  AUX,AUX1
C
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:,:,:)::LAUX  
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LDTMP  
      INTEGER,SAVE,ALLOCATABLE,DIMENSION(:)::LUTMP  
C
      IF(.NOT.ALLOCATED(LDTMP))THEN
        ALLOCATE(LDTMP((KCM+1)*LCM))  
        ALLOCATE(LUTMP((KCM+1)*LCM))  
        ALLOCATE(QTMP((KCM+1)*LCM))  
        ALLOCATE(LAUX(ICM,JCM,KCM))
	  LDTMP=0 
	  LUTMP=0 
	  LAUX=0 
	  QTMP=0.0 
      ENDIF
C
      TITLEB='DATA GROUP B: EXCHANGE COEFFICIENTS'
      TITLEC='DATA GROUP C: VOLUMES'
C
C**********************************************************************C
C
C **  WARNING!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C **  THE VALUE OF X IN THE F10.X FORMATS MAY NEED TO BE CHANGED
C **  FROM PROBLEM TO PROBLEM.  A PRELIMINARY RUN USING E10.3
C **  CAN BE USED TO SPEED THE ADJUSTMENT
C
C**********************************************************************C
C
C **  READ CONTROL DATA FOR WRITING TO WASP COMPATIBLE FILES
C
C----------------------------------------------------------------------C
C
      SVPT=1.
      IF(NTSMMT.LT.NTSPTC)SVPT=0.
C
      IF(JSWASP.EQ.1) THEN
        OPEN(1,FILE='EFDC.WSP',STATUS='UNKNOWN')
C
C1**  READ CELL VOLUME PARAMETERS
C
        READ(1,1)
        READ(1,1)
        READ(1,*) IVOPT,IBEDV,SCALV,CONVV,VMULT,VEXP,DMULT,DEXP
C
C2**  READ DIFFUSION PARAMETERS
C
        READ(1,1)
        READ(1,1)
        READ(1,*) NRFLD,SCALR,CONVR,ISNKH
C
C3**  READ ADVECTION PARAMETERS
C
        READ(1,1)
        READ(1,1)
        READ(1,*) IQOPT,NFIELD,SCALQ,CONVQ,HYDFIL,ISWASPD,ISDHD,IDAYS	!hnr
C
C4**  READ SEDIMENT VOLUME DEPTH AND TDINTS(GROUP C RECORD 1)
C
        READ(1,1)
        READ(1,1)
        READ(1,*) DEPSED,TDINTS,SEDIFF, WSS1, WSS2, WSS3
C
        CLOSE(1)
      END IF
C
    1 FORMAT (80X)
C
C**********************************************************************C
C
C **  WRITE HORIZONTAL POSITION AND LAYER FILE waspp.out
C **  WRITE INITIAL VOLUME FILE waspc.out
C
C **  file waspc.out is consistent with data group C specifications
C **  on page 11 of the wasp5.1 manual part B, Sept 1993
C
C **  file waspp.out defines the layer (1 is surface water layer, with
C **  layer numbering increasing with depth in water column) and
C **  horizontal positions in lon,lat or utme, utmn of the water
C **  quality (long term transport) cells or segements
C
C----------------------------------------------------------------------C
C
C---- 7/1/2005 A Stoddard added code to write WASPSEG.OUT file
C---- WASPSEG.OUT Segment Name file is linked for import to WASP6 & WASP7
      IF (JSWASP.EQ.1) THEN
        OPEN(970,FILE='WASPSEG.OUT',STATUS='UNKNOWN')        ! 7/1/2005 AS
        CLOSE(970,STATUS='DELETE')                              ! 7/1/2005 AS 

        OPEN(90,FILE='WASPP.OUT',STATUS='UNKNOWN')
        OPEN(93,FILE='WASPC.OUT',STATUS='UNKNOWN')
        CLOSE(90,STATUS='DELETE')
        CLOSE(93,STATUS='DELETE')
        OPEN(90,FILE='WASPP.OUT',STATUS='UNKNOWN')
        OPEN(93,FILE='WASPC.OUT',STATUS='UNKNOWN')
        OPEN(970,FILE='WASPSEG.OUT',STATUS='UNKNOWN')         ! 7/1/2005 AS

        WRITE(93,1031)IVOPT,IBEDV,TDINTS,TITLEC
        WRITE(93,1032)SCALV,CONVV
        LCLTM2=LCLT-2
        LWASP=0
        IF (KC.GT.1) THEN
          LTYPE=1
          KWASP=1
          DO LT=2,LALT
            LWASP=LWASP+1
            LBELOW=LWASP+LCLTM2
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            DMULT=HLPF(L)*DZC(KC)
            VOLUME=DXYP(L)*HLPF(L)*DZC(KC)
            IF(NTSMMT.LT.NTSPTC) THEN
              DMULT=HMP(L)*DZC(KC)
              VOLUME=DXYP(L)*HMP(L)*DZC(KC)
            END IF
            LAUX(I,J,KC)=LWASP      !HNR

            WRITE(970,9701)I,J,L,KC,LWASP,KWASP         ! ,I,J,L,KC               ! 7/1/2005 AS
            WRITE(90,1001)LWASP,KWASP,I,J,L,KC
            WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP, DMULT,
     +      DEXP,I,J,L,KC
          END DO
          LTYPE=2
          DO K=KS,2,-1
            KWASP=KC-K+1
            DO LT=2,LALT
              LWASP=LWASP+1
              LBELOW=LWASP+LCLTM2
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              DMULT=HLPF(L)*DZC(K)
              VOLUME=DXYP(L)*HLPF(L)*DZC(K)
              IF(NTSMMT.LT.NTSPTC) THEN
                DMULT=HMP(L)*DZC(KC)
                VOLUME=DXYP(L)*HMP(L)*DZC(KC)
              END IF
              LAUX(I,J,K)=LWASP     !HNR
              WRITE(970,9701)I,J,L,K,LWASP,KWASP         ! ,I,J,L,K               ! 7/1/2005 AS
              WRITE(90,1001)LWASP,KWASP,I,J,L,K
              WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP, DMULT,
     +        DEXP,I,J,L,KC
            END DO
          END DO
        END IF
        LTYPE=2
        IF (KC.EQ.1) LTYPE=1
        KWASP=KC
        DO LT=2,LALT
          LWASP=LWASP+1
          LBELOW=LWASP+LCLTM2
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
          DMULT=HLPF(L)*DZC(1)
          VOLUME=DXYP(L)*HLPF(L)*DZC(1)
          IF(NTSMMT.LT.NTSPTC) THEN
            DMULT=HMP(L)*DZC(KC)
            VOLUME=DXYP(L)*HMP(L)*DZC(KC)
          END IF
          IONE=1
          LAUX(I,J,1)=LWASP       !HNR
          WRITE(970,9701)I,J,L,IONE,LWASP,KWASP         ! ,I,J,L,IONE               ! 7/1/2005 AS
          WRITE(90,1001)LWASP,KWASP,I,J,L,IONE
          WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP, DMULT,
     +    DEXP,I,J,L,IONE
        END DO
        LTYPE=3
        KWASP=KC+1
        DXYSUM=0.
        LWSPTMP=LWASP+1
        DO LT=2,LALT
          LWSPTMP=LWSPTMP+1
        END DO
C The following the lower benthic layer.  All upper benthic layer segments
C have this layer immediately below them:
        DO LT=2,LALT
          LWASP=LWASP+1
          LBELOW=LWSPTMP
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
          DXYSUM=DXYSUM+DXYP(L)
          VOLUME=DXYP(L)*DEPSED
          IZERO=0
          WRITE(90,1001)LWASP,KWASP,I,J,L,IZERO
          WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP, DEPSED,
     +    DEXP,I,J,L,IZERO
        END DO
C Next do the lower benthic layer:
        LTYPE=4
        KWASP=KC+2
        LWASP=LWASP+1
        LBELOW=0
        DMULT=DEPSED
        VOLUME=DXYSUM*DEPSED
        IM1=-1
        WRITE(90,1001)LWASP,KWASP,I,J,L,IM1
        WRITE(93,1033)LWASP,LBELOW,LTYPE,VOLUME,VMULT,VEXP, DMULT,DEXP,
     +  I,J,L,IM1
        CLOSE(970)                           ! 7/1/2005
        CLOSE(90)
        CLOSE(93)
      END IF
C
 1001 FORMAT(6I5,2F10.4)
 1031 FORMAT(2I5,F10.4,10X,A50)
 1032 FORMAT(2F10.4)
 1033 FORMAT(3I10,F10.1,4F10.3,'   !',4i5)
       
ccccccccccc     ! 7/1/2005 AStoddard


 9701 FORMAT('I=',I3,' J=',I3,' L=',I4 ,' K=',I2,
     +       ' W',I4,' WK=',I2)

C             WASPSEG.OUT data structure
C             Wasp7 wired for linkage of A30 character string file
C             W Wasp Segment Number
C             WK wasp Layer number 1=sfc   K=KC=bottom
C             I,J = EFDC I, J index 
C             L = EFDC grid cell number
C             K = EFDC layer K=KC sfc    K=1 bottom 

C**********************************************************************C

C
C **  WRITE DIFFUSIVE AND DISPERSIVE TRANSPORT FILE waspb.out
C
C **  file waspb.out is consistent with data group B specifications
C **  on page 8 of the wasp5.1 manual part B, Sept 1993
C
C----------------------------------------------------------------------C


C
      IF(JSWASP.EQ.1) THEN
        OPEN(91,FILE='WASPB.OUT',STATUS='UNKNOWN')
        CLOSE(91,STATUS='DELETE')
        OPEN(91,FILE='WASPB.OUT',STATUS='UNKNOWN')
        WRITE(91,1011)NRFLD,TITLEB
        NTEX=NTS/NTSMMT
        WRITE(91,1012)NTEX,SCALR,CONVR
        CLOSE(91)
C
        OPEN(91,FILE='WASPB.OUT',POSITION='APPEND',STATUS='UNKNOWN')
C
        LCLTM2=LCLT-2
        NORSH=0
        NORSV=0
        DO LT=2,LALT
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
          NORSH=NORSH+INT(SUBO(L))+INT(SVBO(L))
          NORSV=NORSV+INT(SPB(L))
        END DO
        NORS=ISNKH*KC*NORSH+KS*NORSV
        WRITE(91,1013)NORS
C
        IF(ISNKH.EQ.1) THEN
          UNITY=1.
          DO K=KC,1,-1
            KMUL=KC-K
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SUB(L).EQ.1.) THEN
                LWASP=LT-1+KMUL*LCLTM2
                LWASPW=LWASP-1
                LW=L-1
                ADDLW=DYU(L)*AHULPF(L,K)*DZC(K)*0.5*(HLPF(L) +HLPF(LW))
     +          *DXIU(L)
                WRITE(91,1014) ADDLW,UNITY,LWASPW,LWASP
              END IF
            END DO
          END DO
        END IF
C
        IF(ISNKH.EQ.1) THEN
          UNITY=1.
          DO K=KC,1,-1
            KMUL=KC-K
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SVB(L).EQ.1.) THEN
                LWASP=LT-1+KMUL*LCLTM2
                LSLT=LSCLT(LT)
                LWASPS=LSLT-1+KMUL*LCLTM2
                LS=LSC(L)
                ADDLS=DXV(L)*AHVLPF(L,K)*DZC(K)*0.5*(HLPF(L) +HLPF(LS))
     +          *DYIV(L)
                WRITE(91,1014) ADDLS,UNITY,LWASPS,LWASP
              END IF
            END DO
          END DO
        END IF
C
        IF (KC.GT.1) THEN
          UNITY=1.
          DO K=KS,1,-1
            KMUL1=KS-K
            KMUL2=KMUL1+1
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SPB(L).EQ.1.) THEN
                LWASP=LT-1+KMUL1*LCLTM2
                LBELOW=LT-1+KMUL2*LCLTM2
                ADDL=DXYP(L)*ABLPF(L,K)*DZIG(K)
                WRITE(91,1014) ADDL,UNITY,LWASP,LBELOW
              END IF
            END DO
          END DO
        END IF
C
        NBRK=6
        WRITE(91,1015)NBRK
C
        TSTOP=(DT*FLOAT(N)+TBEGIN*TCON)
        TSTART=TSTOP-DT*FLOAT(NTSMMT)
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
C
        CLOSE(91)
C
C **  ADD PORE WATER EXCHANGE FIELD ON LAST CALL
C
        OPEN(91,FILE='WASPB.OUT',POSITION='APPEND',STATUS='UNKNOWN')
C
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
        END DO
        WRITE(91,1013)NORSV
        IF (KC.GE.1) THEN
          KMUL2=KC+1
          UNITY=1.
          DO LT=2,LALT
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            IF (SPB(L).EQ.1.) THEN
              LWASP=LT-1+KC*LCLTM2
              LBELOW=LT-1+KMUL2*LCLTM2
              ADDL=2.*DXYP(L)*SEDIFF/DEPSED
              WRITE(91,1014) ADDL,UNITY,LWASP,LBELOW
            END IF
          END DO
        END IF
C
        NBRK=6
        WRITE(91,1015)NBRK
C
        TSTART=TBEGIN*TCON
        TSTOP=DT*FLOAT(NTS)+TBEGIN*TCON
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
C
        IBPTMP=0
        WRITE(91,1017)IBPTMP,IBPTMP,IBPTMP,IBPTMP, IBPTMP,IBPTMP,IBPTMP,
     +  IBPTMP, IBPTMP,IBPTMP,IBPTMP,IBPTMP, IBPTMP,IBPTMP,IBPTMP,IBPTMP
C
        CLOSE(91)
      END IF
C
 1011 FORMAT(I5,10X,A50)
 1012 FORMAT(I5,2F10.4)
 1013 FORMAT(I5)
 1014 FORMAT(2E10.3,2I5,F10.3,'   !',3i5,3x,a3)
 1015 FORMAT(I5)
 1016 FORMAT(4(e10.3,F10.5))
 1017 FORMAT(16I5)
C
C**********************************************************************C
C
C **  WRITE ADVECTIVE TRANSPORT FILE waspd.out
C
C **  file waspd.out is consistent with data group D.1 specifications
C **  on page 13 of the wasp5.1 manual part B, Sept 1993
C **  this file is written only if ISWASPD=1
C
C----------------------------------------------------------------------C
C
      IF(ISWASPD.EQ.1) THEN
C
        IF(JSWASP.EQ.1) THEN
          OPEN(92,FILE='WASPD.OUT',STATUS='UNKNOWN')
          CLOSE(92,STATUS='DELETE')
          OPEN(92,FILE='WASPD.OUT',STATUS='UNKNOWN')
          WRITE(92,1021)IQOPT,NFIELD,HYDFIL
          NINQ=NTS/NTSMMT
          WRITE(92,1022)NINQ,SCALQ,CONVQ
          CLOSE(92)
        END IF
C
        OPEN(92,FILE='WASPD.OUT',POSITION='APPEND',STATUS='UNKNOWN')
        LCLTM2=LCLT-2
        NOQSH=0
        NOQSV=0
        DO LT=2,LALT
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
          NOQSH=NOQSH+INT(SUBO(L))+INT(SVBO(L))
          IF (IJCTLT(I+1,J).EQ.8) NOQSH=NOQSH+1
          IF (IJCTLT(I,J+1).EQ.8) NOQSH=NOQSH+1
          NOQSV=NOQSV+INT(SWB(L))
        END DO
        NOQS=KC*NOQSH+KS*NOQSV
        WRITE(92,1023)NOQS
C
        LL=0
C
        DO K=KC,1,-1
          KMUL=KC-K
          DO LT=2,LALT
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            IF (SUBO(L).EQ.1.) THEN
              LL=LL+1
              LDTMP(LL)=LT-1+KMUL*LCLTM2
              LUTMP(LL)=LDTMP(LL)-1
              IF (IJCTLT(I-1,J).EQ.8) LUTMP(LL)=0
              QTMP(LL)=DYU(L)*(UHLPF(L,K)+SVPT*UVPT(L,K))*DZC(K)
            END IF
            IF (IJCTLT(I+1,J).EQ.8) THEN
              IF (SUBO(L+1).EQ.1.) THEN
                LL=LL+1
                LDTMP(LL)=0
                LUTMP(LL)=LT-1+KMUL*LCLTM2
                QTMP(LL)=DYU(L+1)*(UHLPF(L+1,K)+SVPT*UVPT(L+1,K))*DZC(K)
              END IF
            END IF
          END DO
C
          DO LT=2,LALT
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            IF (SVBO(L).EQ.1.) THEN
              LL=LL+1
              LSLT=LSCLT(LT)
              LDTMP(LL)=LT-1+KMUL*LCLTM2
              LUTMP(LL)=LSLT-1+KMUL*LCLTM2
              IF (IJCTLT(I,J-1).EQ.8) LUTMP(LL)=0
              QTMP(LL)=DXV(L)*(VHLPF(L,K)+SVPT*VVPT(L,K))*DZC(K)
            END IF
            IF (IJCTLT(I,J+1).EQ.8) THEN
              LN=LNC(L)
              IF (SVBO(LN).EQ.1) THEN
                LL=LL+1
                LDTMP(LL)=0
                LUTMP(LL)=LT-1+KMUL*LCLTM2
                QTMP(LL)=DXV(LN)*(VHLPF(LN,K)+SVPT*VVPT(LN,K))*DZC(K)
              END IF
            END IF
          END DO
C
        END DO
C
        IF (KC.GT.1) THEN
          DO K=KS,1,-1
            KMUL1=KS-K
            KMUL2=KMUL1+1
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SWB(L).EQ.1.) THEN
                LL=LL+1
                LUTMP(LL)=LT-1+KMUL1*LCLTM2
                LDTMP(LL)=LT-1+KMUL2*LCLTM2
                QTMP(LL)=-DXYP(L)*(WLPF(L,K)+SVPT*WVPT(L,K))
              END IF
            END DO
          END DO
        END IF
C
        DO L=1,LL,4
          WRITE(92,1024) QTMP(L), LUTMP(L), LDTMP(L), QTMP(L+1),LUTMP
     +    (L+1),LDTMP(L+1), QTMP(L+2),LUTMP(L+2),LDTMP(L+2), QTMP(L+3),
     +    LUTMP(L+3),LDTMP(L+3)
        END DO
C
        NBRKQ=6
        WRITE(92,1025)NBRKQ
        WRITE(92,1026)D1,T1,D2,T2,D3,T3,D4,T4
        WRITE(92,1026)D5,T5,D6,T6
C
        CLOSE(92)
C
      END IF
C
 1021 FORMAT(2I5,A12)
 1022 FORMAT(I5,2F10.4)
 1023 FORMAT(I5)
 1024 FORMAT(1p,4(E10.3,2I5))
 1025 FORMAT(I5)
 1026 FORMAT(4(2F10.5))

C**********************************************************************C
C M.R. Morton's version of WASP Data Group D
C **  WRITE ADVECTIVE TRANSPORT FILE waspd.mrm
C----------------------------------------------------------------------C
      IF (JSWASP .EQ. 1) THEN

C ***  Begin code change June 13 2008
C ***  June 13 2008 Andy Stoddard change this filename to prevent WASP7 from 
C ***  screwing up when reading a correctly formatted waspd.mrm file 
C ***  that is written with hspf-efdc-wasp linkage program
C ***  WASP7 will link data written to waspd.mrm file automatic linkage
C ***  if waspd.mrm file is in path with HYD file

CCCCCCC OPEN(92,FILE='WASPD.MRM',STATUS='UNKNOWN')
        OPEN(92,FILE='DUMWASPD.MRM',STATUS='UNKNOWN')

C ***  End code change June 13 2008

        WRITE(92,2020) IQOPT,NFIELD,HYDFIL
        LL=0
        NINQ=0
        SCALQ=1.0
        CONVQ=1.0/86400.0
C Data Block D.1 (Advective Flows) is not needed since HYD file is used:
C        write(92,2021) ninq,scalq,convq
C Data Block D.2 (Pore Water Flows) not needed:
        WRITE(92,2022) NINQ,SCALQ,CONVQ
C Data Block D.3 (Sediment #1 Transport Field):
        NINQ=1
        WRITE(92,2023) NINQ,SCALQ,CONVQ
        IF (KC.GT.1) THEN
          DO K=KS,0,-1
            KMUL1=KS-K
            KMUL2=KMUL1+1
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SWB(L).EQ.1.) THEN
                LL=LL+1
                LUTMP(LL)=LT-1+KMUL1*LCLTM2
                LDTMP(LL)=LT-1+KMUL2*LCLTM2
C qtmp array holds the horizontal area of each cell:
                QTMP(LL)= DXYP(L)
              END IF
            END DO
          END DO
        END IF
C
 6999 format(9i5,f5.1)
 6996 format(9i5,f5.1)
C
        WRITE(92,2030) LL
        DO L=1,LL,4
          WRITE(92,1024) QTMP(L), LUTMP(L), LDTMP(L), QTMP(L+1),LUTMP
     +    (L+1),LDTMP(L+1), QTMP(L+2),LUTMP(L+2),LDTMP(L+2), QTMP(L+3),
     +    LUTMP(L+3),LDTMP(L+3)
        END DO
        NBRKQ=2
        T1=1.0
        T2=366.0
        WRITE(92,2030) NBRKQ
        WRITE(92,2031) WSS1,T1,WSS1,T2
C Data Block D.4 (Sediment #2 Transport Field):
        NINQ=1
        WRITE(92,2024) NINQ,SCALQ,CONVQ
        WRITE(92,2030) LL
        DO L=1,LL,4
          WRITE(92,1024) QTMP(L), LUTMP(L), LDTMP(L), QTMP(L+1),LUTMP
     +    (L+1),LDTMP(L+1), QTMP(L+2),LUTMP(L+2),LDTMP(L+2), QTMP(L+3),
     +    LUTMP(L+3),LDTMP(L+3)
        END DO
        NBRKQ=2
        T1=1.0
        T2=366.0
        WRITE(92,2030) NBRKQ
        WRITE(92,2031) WSS2,T1,WSS2,T2
C Data Block D.5 (Sediment #3 Transport Field):
        NINQ=1
        WRITE(92,2025) NINQ,SCALQ,CONVQ
        WRITE(92,2030) LL
        DO L=1,LL,4
          WRITE(92,1024) QTMP(L), LUTMP(L), LDTMP(L), QTMP(L+1),LUTMP
     +    (L+1),LDTMP(L+1), QTMP(L+2),LUTMP(L+2),LDTMP(L+2), QTMP(L+3),
     +    LUTMP(L+3),LDTMP(L+3)
        END DO
        NBRKQ=2
        T1=1.0
        T2=366.0
        WRITE(92,2030) NBRKQ
        WRITE(92,2031) WSS3,T1,WSS3,T2
C add system bypass array to bottom of data group D:
        WRITE(92,1017)IBPTMP,IBPTMP,IBPTMP,IBPTMP, IBPTMP,IBPTMP,IBPTMP,
     +  IBPTMP, IBPTMP,IBPTMP,IBPTMP,IBPTMP, IBPTMP,IBPTMP,IBPTMP,IBPTMP
        CLOSE(92)
      END IF
 2020 format(2i5,a12,'    Data Group D: Flows')
 2021 FORMAT(1p,I5,2e10.3,'    Data Block D.1 Advective Flows')
 2022 FORMAT(1p,I5,2e10.3,'    Data Block D.2 Pore Water Flows')
 2023 FORMAT(1p,I5,2e10.3,'    Data Block D.3 Sed. #1 Transport Field')
 2024 FORMAT(1p,I5,2e10.3,'    Data Block D.4 Sed. #2 Transport Field')
 2025 FORMAT(1p,I5,2e10.3,'    Data Block D.5 Sed. #3 Transport Field')
 2030 format(i5)
 2031 format(2(e10.3,f10.5))
C
C**********************************************************************C
C
C **  WRITE TO EXTERNAL HYDRO FILE hydfil DIAGNOSTIC VERSION
C **  OF SAME FILE waspdhd.out
C
C----------------------------------------------------------------------C
C
      IF(JSWASP.EQ.1)THEN
        OPEN(90,FILE='WASPDHD.OUT',STATUS='UNKNOWN')
        IF(IQOPT.EQ.3) OPEN(94,FILE=HYDFIL,STATUS='UNKNOWN')
        IF(IQOPT.EQ.4) OPEN(95,FILE=HYDFIL,STATUS='UNKNOWN', FORM
     +  ='UNFORMATTED')
        OPEN(96,FILE='WASPB.MRM',STATUS='UNKNOWN')
        CLOSE(90,STATUS='DELETE')
        IF(IQOPT.EQ.3) CLOSE(94,STATUS='DELETE')
        IF(IQOPT.EQ.4) CLOSE(95,STATUS='DELETE')
        CLOSE(96,STATUS='DELETE')
        OPEN(90,FILE='WASPDHD.OUT',STATUS='UNKNOWN')
        IF(IQOPT.EQ.3) OPEN(94,FILE=HYDFIL,STATUS='UNKNOWN')
        IF(IQOPT.EQ.4) OPEN(95,FILE=HYDFIL,STATUS='UNKNOWN', FORM
     +  ='UNFORMATTED')
        OPEN(96,FILE='WASPB.MRM',STATUS='UNKNOWN')
        WRITE(96,1011) NRFLD,TITLEB
        NTEXX=1
        WRITE(96,1012) NTEXX,SCALR,CONVR
C
C Write WASP5 Hydrodynamic File Data Record 1, Data Options:
C  NJUN = number of segments connected by flows from the hyd. file
C  NCHN = number of interfacial flow pairs from the hyd. file
C  DTWASP = WASP5 time step (seconds)
C  TZERO = begin time step for hyd. file (seconds)
C  TENDHYD = end time step for hyd. file (seconds)
C  ISTMP = control switch, 0=time variable segment depths and velocities
C          are read; 1=time variable segment depths and velocities are not
C          read.
C
        NJUN=KC*(LCLT-2)
        NCHNH=0
        NCHNV=0
        DO LT=2,LALT
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
          NCHNH=NCHNH+INT(SUBO(L))
          IF (IJCTLT(I+1,J).EQ.8) THEN
            IF (SUBO(L+1).EQ.1.) NCHNH=NCHNH+1
          END IF
          NCHNH=NCHNH+INT(SVBO(L))
          IF (IJCTLT(I,J+1).EQ.8) THEN
            IF (SVBO(LNC(L)).EQ.1.) NCHNH=NCHNH+1
          END IF
          NCHNV=NCHNV+INT(SWB(L))
        END DO
        NCHN=KC*NCHNH+(KC-1)*NCHNV
	  NQ=NQSIJ                                         !HNR
	  DO L=1,NQSIJ                                     !HNR
		IF(LIJLT(IQS(L),JQS(L)).EQ.0) NQ=NQ-1          !HNR
	  END DO                                           !HNR
        NCHN=NCHN+KC*NQ                                  !HNR
	  NQ=NQCTL                                         !HNR
	  DO L=1,NQCTL                                     !HNR
		IF(LIJLT(IQCTLU(L),JQCTLU(L)).EQ.0) THEN       !HNR
		  IF(LIJLT(IQCTLD(L),JQCTLD(L)).EQ.0) NQ=NQ-1  !HNR
	    END IF                                         !HNR
	  END DO                                           !HNR
        NCHN=NCHN+KC*NQ                                  !HNR
        ISTMP=0
        NODYN=NFLTMT
        NODYN=NODYN
        DTWASP = DT * FLOAT(NTSMMT)
        TZERO=TBEGIN*TCON
        TENDHYD=TZERO+NTS*DT
        IF (IDAYS.GT.0) THEN								!HNR
	    IBEGIN=IDAYS*NTSPTC
	    AUX=FLOAT(IBEGIN)/FLOAT(NTSMMT)
	    NAUX=INT(AUX)
	    AUX1=AUX-NAUX
		IF (AUX1.GT.0) NAUX=NAUX+1
	    TZERO=TZERO+(NAUX*NTSMMT)*TCON/NTSPTC
	  END IF											!HNR
        WRITE(90,901)NJUN,NCHN
        IF(IQOPT.EQ.3) THEN
          WRITE(94,941) NJUN,NCHN,NTSMMT,DT,TZERO,TENDHYD,KC        !HNR
        END IF
        IF(IQOPT.EQ.4) THEN
          WRITE(95) NJUN,NCHN,NTSMMT,DT,TZERO,TENDHYD,KC            !HNR
        END IF
        WRITE(96,1013) NCHN
C
C **  CHANNEL DATA
C
C Write WASP5 Hydrodynamic File Data Record 2, Segment Interface Pairs:
C   WASP expects to see boundary segments designated as "0".
C
        RMNDUM=0.
        LCHN=0
        DO K=KC,1,-1
          KMUL=KC-K
          DO LT=2,LALT
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            IF (SUBO(L).EQ.1.) THEN
              LDTM=LT-1+KMUL*LCLTM2
              LUTM=LDTM-1
              IF (IJCTLT(I-1,J).EQ.8) LUTM=0
              RLENTH=DXU(L)
              WIDTH=DYU(L)
              LCHN=LCHN+1
              IF (ISDHD. EQ. 1) WRITE(90,902)LCHN,RLENTH,WIDTH, RMNDUM,
     +        LUTM,LDTM
              IF (ISDHD .EQ. 2) WRITE(90,'(2I5)') LUTM,LDTM
              IF(IQOPT.EQ.3) WRITE(94,9941) LUTM,LDTM,I,J,K,'U 0'
              IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM
              WRITE(96,1014) UNITY,UNITY,LUTM,LDTM,UNITY,I,J,K,'U 0'
            END IF
            IF (IJCTLT(I+1,J).EQ.8) THEN
              IF (SUBO(L+1).EQ.1.) THEN
                LDTM=0
                LUTM=LT-1+KMUL*LCLTM2
                RLENTH=DXU(L+1)
                WIDTH=DYU(L+1)
                LCHN=LCHN+1
                IF (ISDHD .EQ. 1) WRITE(90,902) LCHN,RLENTH,WIDTH,
     +          RMNDUM, LUTM,LDTM
                IF (ISDHD .EQ. 2) WRITE(90,'(2I5)') LUTM,LDTM
                IF(IQOPT.EQ.3) WRITE(94,9941) LUTM,LDTM,I,J,K,'U+1'
                IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM
                UNITY=1.0
                WRITE(96,1014) UNITY,UNITY,LUTM,LDTM,UNITY,I,J,K,'U+1'
              END IF
            END IF
          END DO
          DO LT=2,LALT
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            IF (SVBO(L).EQ.1.) THEN
              LSLT=LSCLT(LT)
              LDTM=LT-1+KMUL*LCLTM2
              LUTM=LSLT-1+KMUL*LCLTM2
              IF(IJCTLT(I,J-1).EQ.8) LUTM=0
              RLENTH=DYV(L)
              WIDTH=DXV(L)
              LCHN=LCHN+1
              IF (ISDHD .EQ. 1) WRITE(90,902) LCHN,RLENTH,WIDTH, RMNDUM,
     +        LUTM,LDTM
              IF (ISDHD .EQ. 2) WRITE(90,'(2I5)') LUTM,LDTM
              IF(IQOPT.EQ.3) WRITE(94,9941) LUTM,LDTM,I,J,K,'V 0'
              IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM
              WRITE(96,1014) UNITY,UNITY,LUTM,LDTM,UNITY,I,J,K,'V 0'
            END IF
            IF (IJCTLT(I,J+1).EQ.8) THEN
              LN=LNC(L)
              IF (SVBO(LN).EQ.1.) THEN
                LSLT=LSCLT(LT)
                LDTM=0
                LUTM=LT-1+KMUL*LCLTM2
                RLENTH=DYV(LN)
                WIDTH=DXV(LN)
                LCHN=LCHN+1
                IF (ISDHD .EQ. 1) WRITE(90,902) LCHN,RLENTH,WIDTH,
     +          RMNDUM, LUTM,LDTM
                IF (ISDHD .EQ. 2) WRITE(90,'(2I5)') LUTM,LDTM
                IF(IQOPT.EQ.3) WRITE(94,9941) LUTM,LDTM,I,J,K,'V+1'
                IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM
                WRITE(96,1014) UNITY,UNITY,LUTM,LDTM,I,J,K,'V+1'
              END IF
            END IF
          END DO
        END DO
c !start a modification by HNR to include input flows
        DO K=KC,1,-1
          DO LT=1,NQSIJ
            I=IQS(LT)
            J=JQS(LT)
	      IF(LIJLT(I,J).EQ.0) GOTO 100
            LDTM=LAUX(I,J,K)
            LUTM=0
            IF(IQOPT.EQ.3) WRITE(94,9941) LUTM,LDTM,I,
     +                                       J,K,'u+2'
            IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM    !HNR
100       END DO
        END DO
C !end of modification HNR
c !start modification by HNR to include structures' flows
        DO K=KC,1,-1
          DO LT=1,NQCTL
            I=IQCTLU(LT)
            J=JQCTLU(LT)
            LUTM=LAUX(I,J,K)
            I=IQCTLD(LT)
            J=JQCTLD(LT)
            LDTM=LAUX(I,J,K)
	      IF(LUTM.EQ.0.AND.LDTM.EQ.0) GOTO 200
            IF(IQOPT.EQ.3) WRITE(94,9941) LUTM,LDTM,I,
     +                                       J,K,'ust'		!HNRB
            IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM
200       END DO
        END DO
C !end of modification HNR 4/2002
        IF(KC.GT.1)THEN
          DO K=KS,1,-1
            KMUL1=KS-K
            KMUL2=KMUL1+1
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SWB(L).EQ.1.) THEN
                LUTM=LT-1+KMUL1*LCLTM2
                LDTM=LT-1+KMUL2*LCLTM2
                RLENTH=HLPF(L)*DZG(K)
                WIDTH=SQRT(DXYP(L))
                LCHN=LCHN+1
                WRITE(90,902)LCHN,RLENTH,WIDTH,RMNDUM,LUTM,LDTM
                IF(IQOPT.EQ.3) WRITE(94,9941)LUTM,LDTM,I,J,K,'W 0'
                IF(IQOPT.EQ.4) WRITE(95) LUTM,LDTM
                WRITE(96,1014) UNITY,UNITY,LUTM,LDTM,UNITY,I,J,K,'W 0'
              END IF
            END DO
          END DO
C
C write out time series of zero dispersion coefficients:
C
          D1=0.0
          T1=TZERO/TCON
          D2=0.0
          T2=TENDHYD/TCON
          NBRKQ=2
          WRITE(96,905) NBRKQ
          WRITE(96,1016) D1,T1, D2,T2
C
C For exchange between the lower water surface layer and the upper
C benthic layer, do the following:
C
          WRITE(96,1012) NTEXX,SCALR,CONVR
          NTEXX=0
          DO K=1,1
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SWB(L).EQ.1.) THEN
                NTEXX=NTEXX+1
              END IF
            END DO
          END DO
          WRITE(96,1013) NTEXX
          DO K=1,1
            KMUL1=KS-K
            KMUL2=KMUL1+1
            KMUL3=KMUL2+1
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              IF (SWB(L).EQ.1.) THEN
                LUTM=LT-1+KMUL2*LCLTM2
                LDTM=LT-1+KMUL3*LCLTM2
                WRITE(96,1014) DXYP(L),DEPSED,LUTM,LDTM
              END IF
            END DO
          END DO
C
C write out time series of water-benthic exchange dispersion coefficients:
C
          D1=SEDIFF
          T1=TZERO/TCON
          D2=SEDIFF
          T2=TENDHYD/TCON
          NBRKQ=2
          WRITE(96,905) NBRKQ
          WRITE(96,1016) D1,T1, D2,T2
          IBPTMP=0
          WRITE(96,1017)IBPTMP,IBPTMP,IBPTMP,IBPTMP, IBPTMP,IBPTMP,
     +    IBPTMP, IBPTMP, IBPTMP,IBPTMP,IBPTMP,IBPTMP, IBPTMP,IBPTMP,
     +    IBPTMP,IBPTMP
        END IF
C
C **  JUNCTION DATA WITH INITIAL CONDITIONS if IDAYS=0			!hnr
C
C Write WASP5 Hydrodynamic File Data Record 3, Initial Segment Properties:
C
	  IF(IDAYS.EQ.0) THEN					!HNR
          VELTMP=0.
          DO K=KC,1,-1
            KMUL=KC-K
            DO LT=2,LALT
              I=ILLT(LT)
              J=JLLT(LT)
              L=LIJ(I,J)
              LCELL=LT-1+KMUL*LCLTM2
              DEPTMP=HLPF(L)*DZC(K)
              VOLTMP=DEPTMP*DXYP(L)
              IF(NTSMMT.LT.NTSPTC) THEN
                DEPTMP=HMP(L)*DZC(K)
                VOLTMP=DEPTMP*DXYP(L)
              END IF
              IF (ISDHD .EQ. 1) WRITE(90,904) LCELL,VOLTMP,I,J
              IF(IQOPT.EQ.3) WRITE(94,9440) VOLTMP,DEPTMP,VELTMP
              IF(IQOPT.EQ.4) WRITE(95) VOLTMP,DEPTMP,VELTMP
            END DO
          END DO
	  END IF								!HNR
C
        CLOSE(90)
        IF(IQOPT.EQ.3) CLOSE(94)
        IF(IQOPT.EQ.4) CLOSE(95)
        CLOSE(96)
      END IF
C                     		(NTSPTC is the number of time steps per day)
C  If IDAYS>0 but the time step N<IBEGIN=IDAYS*NTSPTC don't write to hydfil		!hnr
	IBEGIN=IDAYS*NTSPTC
	IF(N.LT.IBEGIN) GOTO 3000										!hnr
C
C   If N<=IBEGIN<N+NTSMMT write initial Segment Properties to HYDFIL			!hnr
C
c
	NBEGIN=IBEGIN+NTSMMT
	IF(N.GE.IBEGIN.AND.N.LT.NBEGIN) THEN							!hnr
        OPEN(90,FILE='WASPDHD.OUT',POSITION='APPEND',STATUS='UNKNOWN')
        IF(IQOPT.EQ.3) THEN
          OPEN(94,FILE=HYDFIL,POSITION='APPEND',STATUS='UNKNOWN')
        END IF
        IF(IQOPT.EQ.4) THEN
          OPEN(95,FILE=HYDFIL,POSITION='APPEND',STATUS='UNKNOWN',
     +      FORM='UNFORMATTED')
        END IF
        LCELTMP=0
        DO K=KC,1,-1
          DO LT=2,LALT
            LCELTMP=LCELTMP+1
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
            LN=LNC(L)
            VOLUM=DXYP(L)*HLPF(L)*DZC(K)
            IF(NTSMMT.LT.NTSPTC) VOLUM=DXYP(L)*HP(L)*DZC(K)
            DEPTH=HLPF(L)*DZC(K)
            VELX=0.5*(UHLPF(L,K)+SVPT*UVPT(L,K) +UHLPF(L+1,K)+SVPT*UVPT
     +      (L+1,K))/HLPF(L)
            VELY=0.5*(VHLPF(L,K)+SVPT*VVPT(L,K) +VHLPF(LN,K)+SVPT*VVPT
     +      (LN,K) )/HLPF(L)
            VELZ=0.5*(WLPF(L,K-1)+SVPT*WVPT(L,K-1) +WLPF(L,K)+SVPT*WVPT
     +      (L,K) )
            VELMAG=SQRT(VELX*VELX+VELY*VELY+VELZ*VELZ)
            IF (ISDHD .EQ. 1) WRITE(90,902) LCELTMP,VOLUM,I,J,K
            IF(IQOPT.EQ.3) WRITE(94,946) VOLUM,DEPTH,VELMAG
            IF(IQOPT.EQ.4) WRITE(95) VOLUM, DEPTH, VELMAG
          END DO
        END DO
C
        CLOSE(90)
        IF(IQOPT.EQ.3) CLOSE(94)
        IF(IQOPT.EQ.4) CLOSE(95)
	  GOTO 3000
	END IF															!hnr
C
C----------------------------------------------------------------------C
C
C **  WRITE TIME STEP, VOLUME AND FLOW DATA
C
      OPEN(90,FILE='WASPDHD.OUT',POSITION='APPEND',STATUS='UNKNOWN')
      IF(IQOPT.EQ.3) THEN
        OPEN(94,FILE=HYDFIL,POSITION='APPEND',STATUS='UNKNOWN')
      END IF
      IF(IQOPT.EQ.4) THEN
        OPEN(95,FILE=HYDFIL,POSITION='APPEND',STATUS='UNKNOWN',
     +  FORM='UNFORMATTED')
      END IF
      LCLTM2=LCLT-2
      IZERO=0
      RZERO=0.0
      IZERO=IZERO
      RZERO=RZERO
C
C
C Write WASP5 Hydrodynamic File Data Record 4, BQ(J) flow in interface
C pair "J":
C
C Advection and dispersion in the X-direction:
C
      LCHNUM=0
      DO K=KC,1,-1
        DO LT=2,LALT
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
C the following lines add dispersion to HYD file:
          ADDLW=0.0
          IF (SUB(L).EQ.1.) THEN
            LW=L-1
            ADDLW=DYU(L)*AHULPF(L,K)*DZC(K)*0.5*(HLPF(L) +HLPF(LW))
     +      *DXIU (L)
          END IF
          IF (SUBO(L).EQ.1.) THEN
            TMPVAL=UHLPF(L,K)+SVPT*UVPT(L,K)
            FLOWX=DYU(L)*TMPVAL*DZC(K)
            UDDXTMP=2.*TMPVAL*DXIU(L)/(HLPF(L)+HLPF(L-1))
            IMTMP=I-1
            LCHNUM=LCHNUM+1
            IDRTMP=1
            IF (ISDHD .EQ. 1) WRITE(90,944) FLOWX,IMTMP,I,J,K
            IF(IQOPT.EQ.3) WRITE(94,9946) FLOWX,UDDXTMP,ADDLW,IDRTMP
            IF(IQOPT.EQ.4) WRITE(95) FLOWX,UDDXTMP,ADDLW,IDRTMP
          END IF
          IF (IJCTLT(I+1,J).EQ.8) THEN
            IF (SUBO(L+1).EQ.1.) THEN
              TMPVAL=UHLPF(L+1,K)+SVPT*UVPT(L+1,K)
              FLOWX=DYU(L+1)*TMPVAL*DZC(K)
              UDDXTMP=2.*TMPVAL*DXIU(L+1)/(HLPF(L+1)+HLPF(L))
              IPTMP=I+1
              LCHNUM=LCHNUM+1
              IDRTMP=1
              IF (ISDHD .EQ. 1) WRITE(90,944) LCHNUM,FLOWX,I,IPTMP,J,K
              IF(IQOPT.EQ.3) WRITE(94,9946) FLOWX,UDDXTMP,ADDLW,IDRTMP
              IF(IQOPT.EQ.4) WRITE(95) FLOWX,UDDXTMP,ADDLW,IDRTMP
            END IF
          END IF
        END DO
C
C Advection and dispersion in the Y-direction:
C
        DO LT=2,LALT
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
C The following lines add dispersion to HYD file:
          ADDLS=0.0
          IF (SVB(L).EQ.1.) THEN
            LS=LSC(L)
            ADDLS=DXV(L)*AHVLPF(L,K)*DZC(K)*0.5*(HLPF(L) +HLPF(LS))
     +      *DYIV (L)
          END IF
          IF (SVBO(L).EQ.1.) THEN
            TMPVAL=VHLPF(L,K)+SVPT*VVPT(L,K)
            FLOWY=DXV(L)*TMPVAL*DZC(K)
            VDDYTMP=2.*TMPVAL*DYIV(L)/(HLPF(L)+HLPF(LSC(L)))
            JMTMP=J-1
            LCHNUM=LCHNUM+1
            IDRTMP=2
            IF (ISDHD .EQ. 1) WRITE(90,944) LCHNUM,FLOWY,I,JMTMP,J,K
            IF(IQOPT.EQ.3) WRITE(94,9946) FLOWY,VDDYTMP,ADDLS,IDRTMP
            IF(IQOPT.EQ.4) WRITE(95) FLOWY,VDDYTMP,ADDLS,IDRTMP
          END IF
          IF (IJCTLT(I,J+1).EQ.8) THEN
            LN=LNC(L)
            IF (SVBO(LN).EQ.1.) THEN
              TMPVAL=VHLPF(LN,K)+SVPT*VVPT(LN,K)
              FLOWY=DXV(LN)*TMPVAL*DZC(K)
              VDDYTMP=2.*TMPVAL*DYIV(LN)/(HLPF(LN)+HLPF(L))
              JPTMP=J+1
              LCHNUM=LCHNUM+1
              IDRTMP=2
              IF (ISDHD .EQ. 1) WRITE(90,944) LCHNUM,FLOWY,I,J,JPTMP,K
              IF(IQOPT.EQ.3) WRITE(94,9946) FLOWY,VDDYTMP,ADDLS,IDRTMP
              IF(IQOPT.EQ.4) WRITE(95) FLOWY,VDDYTMP,ADDLS,IDRTMP
            END IF
          END IF
        END DO
      END DO
c !start a modification by HNR to include input flows
      DO K=KC,1,-1
        DO LT=1,nqsij
          I=Iqs(LT)
          J=Jqs(LT)
          IF(LIJLT(I,J).EQ.0) GOTO 300
          NS=NQSERQ(Lt)
          L=LQS(Lt)
          ! *** PMC Start - Flow now adjusted in CALQVS
          !flowx=RQSMUL(Lt)*(QSS(K,Lt)+QSERT(K,NS))
          flowx=QSS(K,LT)+QSERCELL(K,LT)
          ! PMC End
          UDDXTMP=flowx/DXp(L)/dyp(l)/(dzc(k)*HmP(L))
          addlw=dyp(l)*ahulpf(l,k)*dzc(k)*hlpf(l)/dxp(l)
          IDRTMP=1      !hnrb
          IF(iqopt.eq.3) WRITE(94,9946) FLOWX,UDDXTMP,addlw,IDRTMP    !HNRB
          IF(iqopt.eq.4) WRITE(95) FLOWX,UDDXTMP,addlw,IDRTMP     !HNRB
300     END DO
      END DO
C !end of modification HNR
c !start a modification by HNRto include structures' flows
      DO K=KC,1,-1
        DO LT=1,nqctl
          Iu=Iqctlu(LT)
          Ju=Jqctlu(LT)
          Lu=Lij(iu,ju)
          Id=Iqctld(LT)
          Jd=Jqctld(LT)
          Ld=Lij(id,jd)
          IF(LU.EQ.0.AND.LD.EQ.0) GOTO 400
          ! *** PMC Start - Flow now adjusted in CALQVS
          !flowx=RQCMUL(Lt)*QCTLT(K,Lt) 
          flowx=QCTLT(K,Lt) 
          ! *** PMC End
          UDDXTMP=flowx/DXp(L)/dyp(l)/(dzc(k)*HmP(L))
          IF(iu.eq.id) THEN
            addls=dxv(lu)*ahvlpf(lu,k)*dzc(k)*0.5*(hlpf(lu)
     $            +hlpf(ld))*dyiv(lu)
          ELSE
            addls=dyu(lu)*ahulpf(lu,k)*dzc(k)*0.5*(hlpf(lu)
     $           +hlpf(ld))*dxiu(lu)
          END IF
          IDRTMP=1     !hnrb
          IF(iqopt.eq.3) WRITE(94,9946) FLOWX,UDDXTMP,addls,IDRTMP   !HNRB
          IF(iqopt.eq.4) WRITE(95) FLOWX,UDDXTMP,addls,IDRTMP      !HNRB
400     END DO
      END DO
C !end of modification HNR
C
C Advection and dispersion in the Z-direction:
C
      IF (KC.GT.1) THEN
        DO K=KS,1,-1
          DO LT=2,LALT
            I=ILLT(LT)
            J=JLLT(LT)
            L=LIJ(I,J)
C The following lines add dispersion to HYD file:
            ADDL=0.0
            IF (SPB(L).EQ.1.) THEN
              ADDL=DXYP(L)*ABLPF(L,K)*DZIG(K)
              IF (ISDHD .EQ. 2) WRITE(90, '(4I5,E13.4)') I,J,K,L,ABLPF
     +        (L,K)
            END IF
            IF (SWB(L).EQ.1) THEN
              TMPVAL=WLPF(L,K)+SVPT*WVPT(L,K)
              FLOWZ=-DXYP(L)*TMPVAL
              WDDZTMP=TMPVAL*DZIG(K)/HLPF(L)
              KPTMP=K+1
              IDRTMP=3
              LCHNUM=LCHNUM+1
              IF (ISDHD .EQ. 1) WRITE(90,944) LCHNUM,FLOWZ,I,J,K,KPTMP
              IF(IQOPT.EQ.3) WRITE(94,9946) FLOWZ,WDDZTMP,ADDL,IDRTMP
              IF(IQOPT.EQ.4) WRITE(95) FLOWZ,WDDZTMP,ADDL,IDRTMP
            END IF
          END DO
        END DO
      END IF
C
C Write WASP5 Hydrodynamic File Data Record 5, Segment Properties:
C
      LCELTMP=0
      DO K=KC,1,-1
        DO LT=2,LALT
          LCELTMP=LCELTMP+1
          I=ILLT(LT)
          J=JLLT(LT)
          L=LIJ(I,J)
          LN=LNC(L)
          VOLUM=DXYP(L)*HLPF(L)*DZC(K)
          IF(NTSMMT.LT.NTSPTC) VOLUM=DXYP(L)*HP(L)*DZC(K)
          DEPTH=HLPF(L)*DZC(K)
          VELX=0.5*(UHLPF(L,K)+SVPT*UVPT(L,K) +UHLPF(L+1,K)+SVPT*UVPT
     +    (L+1,K))/HLPF(L)
          VELY=0.5*(VHLPF(L,K)+SVPT*VVPT(L,K) +VHLPF(LN,K)+SVPT*VVPT
     +    (LN,K) )/HLPF(L)
          VELZ=0.5*(WLPF(L,K-1)+SVPT*WVPT(L,K-1) +WLPF(L,K)+SVPT*WVPT
     +    (L,K) )
          VELMAG=SQRT(VELX*VELX+VELY*VELY+VELZ*VELZ)
          IF (ISDHD .EQ. 1) WRITE(90,902) LCELTMP,VOLUM,I,J,K
          IF(IQOPT.EQ.3) WRITE(94,946) VOLUM,DEPTH,VELMAG
          IF(IQOPT.EQ.4) WRITE(95) VOLUM, DEPTH, VELMAG
        END DO
      END DO
C
      CLOSE(90)
      IF(IQOPT.EQ.3) CLOSE(94)
      IF(IQOPT.EQ.4) CLOSE(95)
C
C----------------------------------------------------------------------C
C
  901 FORMAT(2I5,E12.5,4I5,E12.5)
  902 FORMAT(I5,2X,3F20.8,3I5)
  903 FORMAT(3E12.5,2I5)
  904 FORMAT(I5,2X,F20.8,10I5)
  905 FORMAT(I5)
  906 FORMAT(5E12.5)
  941 FORMAT(3I5,3F20.5,I5)
  942 FORMAT(3E12.5,2I5)
  943 FORMAT(3E12.5,2I5)
  944 FORMAT(I5,2X,F20.8,10I5)
 9440 FORMAT(4F20.8)
  945 FORMAT(I5)
  946 FORMAT(4E17.9)
 9946 FORMAT(3e17.9,I5)
 9941 FORMAT(2I5,'    !',3i5,3x,a3)
C
C**********************************************************************C
C
3000  JSWASP=0
C----------------------------------------------------------------------c
C ** end SUBROUTINE WASP7
C----------------------------------------------------------------------c
      RETURN
      END
