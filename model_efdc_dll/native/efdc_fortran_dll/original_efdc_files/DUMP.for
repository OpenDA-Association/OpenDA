      SUBROUTINE DUMP  
C  
C CHANGE RECORD  
C **  SUBROUTINE DUMP WRITES FULL FIELD DUMPS OF MODEL VARIABLES  
C **  AT SPECIFIED TIME INTERVALS  
C  
      USE GLOBAL  
      CHARACTER*1 CZTT(0:9)  
      CHARACTER*1 CCHTMF,CCHTMS  
C  
      CHARACTER*2,ALLOCATABLE,DIMENSION(:)::CNTTOX  
      INTEGER,ALLOCATABLE,DIMENSION(:)::IB08VALL  
      INTEGER,ALLOCATABLE,DIMENSION(:)::IB16VALL  
      INTEGER,ALLOCATABLE,DIMENSION(:)::IDMPVALL  
      INTEGER,ALLOCATABLE,DIMENSION(:,:)::IB08VAL  
      INTEGER,ALLOCATABLE,DIMENSION(:,:)::IB16VAL  
      INTEGER,ALLOCATABLE,DIMENSION(:,:)::IDMPVAL  
      REAL,ALLOCATABLE,DIMENSION(:)::DMPVALL  
      REAL,ALLOCATABLE,DIMENSION(:)::TXBMAX  
      REAL,ALLOCATABLE,DIMENSION(:)::TXBMIN  
      REAL,ALLOCATABLE,DIMENSION(:)::TXWMAX  
      REAL,ALLOCATABLE,DIMENSION(:)::TXWMIN  
      REAL,ALLOCATABLE,DIMENSION(:,:)::DMPVAL  
      ALLOCATE(CNTTOX(NTXM))  
      ALLOCATE(DMPVAL(LCM-2,KCM))  
      ALLOCATE(DMPVALL(LCM-2))  
      ALLOCATE(IB08VAL(LCM-2,KCM))  
      ALLOCATE(IB08VALL(LCM-2))  
      ALLOCATE(IB16VAL(LCM-2,KCM))  
      ALLOCATE(IB16VALL(LCM-2))  
      ALLOCATE(IDMPVAL(LCM-2,KCM))  
      ALLOCATE(IDMPVALL(LCM-2))  
      ALLOCATE(TXBMAX(NTXM))  
      ALLOCATE(TXBMIN(NTXM))  
      ALLOCATE(TXWMAX(NTXM))  
      ALLOCATE(TXWMIN(NTXM))  
C  
C **  INFORMATION FOR TESTING AS STAND ALONE PROGRAM  
C  
      IF(JSDUMP.NE.1) GOTO 300  
      CZTT(0)='0'  
      CZTT(1)='1'  
      CZTT(2)='2'  
      CZTT(3)='3'  
      CZTT(4)='4'  
      CZTT(5)='5'  
      CZTT(6)='6'  
      CZTT(7)='7'  
      CZTT(8)='8'  
      CZTT(9)='9'  
      DO MLTM=1,NTOX  
        MSDIG=MOD(MLTM,10)  
        MTMP=MLTM-MSDIG  
        MFDIG=MTMP/10  
        CCHTMF=CZTT(MFDIG)  
        CCHTMS=CZTT(MSDIG)  
        CNTTOX(MLTM)= CCHTMF // CCHTMS  
      ENDDO  
C  
C  ISDUMP=1, ASCII INTERGER OUTPUT  
C  
      IF(ISDUMP.EQ.1)THEN  
        FNDSEL='SELDMPI.ASC'  
        FNDUUU='UUUDMPI.ASC'  
        FNDVVV='VVVDMPI.ASC'  
        FNDWWW='WWWDMPI.ASC'  
        FNDSAL='SALDMPI.ASC'  
        FNDTEM='TEMDMPI.ASC'  
        FNDDYE='DYEDMPI.ASC'  
        FNDSDW='SDWDMPI.ASC'  
        FNDSDB='SDBDMPI.ASC'  
        FNDSNW='SNWDMPI.ASC'  
        FNDSNB='SNBDMPI.ASC'  
        FNDBDH='BDHDMPI.ASC'  
        DO NT=1,NTOX  
          FNDTWT(NT)='TWT'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTWF(NT)='TWF'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTWC(NT)='TWC'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTWP(NT)='TWP'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTBT(NT)='TBF'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTBF(NT)='TBF'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTBC(NT)='TBC'// CNTTOX(NT) // 'DPI.ASC'  
          FNDTBP(NT)='TBP'// CNTTOX(NT) // 'DPI.ASC'  
        ENDDO  
      ENDIF  
C  
C  ISDUMP=2, 16/8 BIT BINARY INTERGER OUTPUT  
C  
      IF(ISDUMP.EQ.2)THEN  
        FNDSEL='SELDMPI.BIN'  
        FNDUUU='UUUDMPI.BIN'  
        FNDVVV='VVVDMPI.BIN'  
        FNDWWW='WWWDMPI.BIN'  
        FNDSAL='SALDMPI.BIN'  
        FNDTEM='TEMDMPI.BIN'  
        FNDDYE='DYEDMPI.BIN'  
        FNDSDW='SDWDMPI.BIN'  
        FNDSDB='SDBDMPI.BIN'  
        FNDSNW='SNWDMPI.BIN'  
        FNDSNB='SNBDMPI.BIN'  
        FNDBDH='BDHDMPI.BIN'  
        DO NT=1,NTOX  
          FNDTWT(NT)='TWT'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTWF(NT)='TWF'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTWC(NT)='TWC'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTWP(NT)='TWP'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTBT(NT)='TBF'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTBF(NT)='TBF'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTBC(NT)='TBC'// CNTTOX(NT) // 'DPI.BIN'  
          FNDTBP(NT)='TBP'// CNTTOX(NT) // 'DPI.BIN'  
        ENDDO  
      ENDIF  
C  
C  ISDUMP=3, ASCII FLOATING POINT OUTPUT  
C  
      IF(ISDUMP.EQ.3)THEN  
        FNDSEL='SELDMPF.ASC'  
        FNDUUU='UUUDMPF.ASC'  
        FNDVVV='VVVDMPF.ASC'  
        FNDWWW='WWWDMPF.ASC'  
        FNDSAL='SALDMPF.ASC'  
        FNDTEM='TEMDMPF.ASC'  
        FNDDYE='DYEDMPF.ASC'  
        FNDSDW='SDWDMPF.ASC'  
        FNDSDB='SDBDMPF.ASC'  
        FNDSNW='SNWDMPF.ASC'  
        FNDSNB='SNBDMPF.ASC'  
        FNDBDH='BDHDMPF.ASC'  
        DO NT=1,NTOX  
          FNDTWT(NT)='TWT'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTWF(NT)='TWF'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTWC(NT)='TWC'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTWP(NT)='TWP'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTBT(NT)='TBF'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTBF(NT)='TBF'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTBC(NT)='TBC'// CNTTOX(NT) // 'DPF.ASC'  
          FNDTBP(NT)='TBP'// CNTTOX(NT) // 'DPF.ASC'  
        ENDDO  
      ENDIF  
C  
C  ISDUMP=4, 32/64 BIT BINARY FLOATING POINT OUTPUT  
C  
      IF(ISDUMP.EQ.4)THEN  
        FNDSEL='SELDMPF.BIN'  
        FNDUUU='UUUDMPF.BIN'  
        FNDVVV='VVVDMPF.BIN'  
        FNDWWW='WWWDMPF.BIN'  
        FNDSAL='SALDMPF.BIN'  
        FNDTEM='TEMDMPF.BIN'  
        FNDDYE='DYEDMPF.BIN'  
        FNDSDW='SDWDMPF.BIN'  
        FNDSDB='SDBDMPF.BIN'  
        FNDSNW='SNWDMPF.BIN'  
        FNDSNB='SNBDMPF.BIN'  
        FNDBDH='BDHDMPF.BIN'  
        DO NT=1,NTOX  
          FNDTWT(NT)='TWT'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTWF(NT)='TWF'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTWC(NT)='TWC'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTWP(NT)='TWP'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTBT(NT)='TBF'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTBF(NT)='TBF'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTBC(NT)='TBC'// CNTTOX(NT) // 'DPF.BIN'  
          FNDTBP(NT)='TBP'// CNTTOX(NT) // 'DPF.BIN'  
        ENDDO  
      ENDIF  
      IF(ISADMP.EQ.0)THEN  
        OPEN(1,FILE=FNDSEL)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDUUU)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDVVV)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDWWW)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDSAL)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDTEM)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDDYE)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDSDW)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDSDB)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDSNW)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDSNB)  
        CLOSE(1,STATUS='DELETE')  
        OPEN(1,FILE=FNDBDH)  
        CLOSE(1,STATUS='DELETE')  
        DO NT=1,NTOX  
          OPEN(1,FILE=FNDTWT(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTWF(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTWC(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTWP(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTBT(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTBF(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTBC(NT))  
          CLOSE(1,STATUS='DELETE')  
          OPEN(1,FILE=FNDTBP(NT))  
          CLOSE(1,STATUS='DELETE')  
        ENDDO  
      ENDIF  
      JSDUMP=0  
  300 CONTINUE  
      DO K=1,KC  
        DO L=1,LC-2  
          DMPVAL(L,K)=0.  
          IDMPVAL(L,K)=0  
          IB08VAL(L,K)=0  
          IB16VAL(L,K)=0  
        ENDDO  
      ENDDO  
      DO L=1,LC-2  
        DMPVALL(L)=0.  
        IDMPVALL(L)=0  
        IB08VALL(L)=0  
        IB16VALL(L)=0  
      ENDDO  
      IF(ISDYNSTP.EQ.0)THEN  
        TIME=(DT*FLOAT(N)+TCON*TBEGIN)/86400.  
      ELSE  
        TIME=TIMESEC/86400.  
      ENDIF  
      R1=1.  
      R0=0.  
C  
C **  IF(ISDUMP EQUAL 1 OR 2, SCALE VARIABLES AND WRITE INTEGER  
C **  DUMP FILES  
C  
      IF(ISDUMP.LE.2)THEN  
C  
C **  SCALE VARIABLES  
C  
        SELMAX=-1.E12  
        SELMIN=1.E12  
        UUUMAX=-1.E12  
        UUUMIN=1.E12  
        VVVMAX=-1.E12  
        VVVMIN=1.E12  
        WWWMAX=-1.E12  
        WWWMIN=1.E12  
        SALMAX=-1.E12  
        SALMIN=1.E12  
        TEMMAX=-1.E12  
        TEMMIN=1.E12  
        DYEMAX=-1.E12  
        DYEMIN=1.E12  
        SDWMAX=-1.E12  
        SDWMIN=1.E12  
        SDBMAX=-1.E12  
        SDBMIN=1.E12  
        SNWMAX=-1.E12  
        SNWMIN=1.E12  
        SNWMAX=-1.E12  
        SNWMIN=1.E12  
        SNBMAX=-1.E12  
        SNBMIN=1.E12  
        BDHMAX=-1.E12  
        BDHMIN=1.E12  
        DO NT=1,NTOX  
          TXWMAX(NT)=-1.E12  
          TXWMIN(NT)=1.E12  
          TXBMAX(NT)=-1.E12  
          TXBMIN(NT)=1.E12  
        ENDDO  
        IF(ISDMPP.GE.1)THEN  
          DO L=2,LA  
            SELMAX=MAX(SELMAX,P(L))  
            SELMIN=MIN(SELMIN,P(L))  
          ENDDO  
        ENDIF  
        SELMAX=GI*SELMAX  
        SELMIN=GI*SELMIN  
        IF(ISDMPU.GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              UTMP=0.5*(U(L,K)+U(L+1,K))  
              VTMP=0.5*(V(L,K)+V(LNC(L),K))  
              UUUMAX=MAX(UUUMAX,UTMP)  
              UUUMIN=MIN(UUUMIN,UTMP)  
              VVVMAX=MAX(VVVMAX,UTMP)  
              VVVMIN=MIN(VVVMIN,VTMP)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPW.GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              WTMP=0.5*(W(L,K)+W(L,K-1))  
              WWWMAX=MAX(WWWMAX,WTMP)  
              WWWMIN=MIN(WWWMIN,WTMP)  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(1).GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              SALMAX=MAX(SALMAX,SAL(L,K))  
              SALMIN=MIN(SALMIN,SAL(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(2).GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              TEMMAX=MAX(TEMMAX,TEM(L,K))  
              TEMMIN=MIN(TEMMIN,TEM(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(3).GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              DYEMAX=MAX(DYEMAX,DYE(L,K))  
              DYEMIN=MIN(DYEMIN,DYE(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(6).GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              SDWMAX=MAX(SDWMAX,SEDT(L,K))  
              SDWMIN=MIN(SDWMIN,SEDT(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(7).GE.1)THEN  
          DO K=1,KC  
            DO L=2,LA  
              SNWMAX=MAX(SNWMAX,SNDT(L,K))  
              SNWMIN=MIN(SNWMIN,SNDT(L,K))  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            DO K=1,KC  
              DO L=2,LA  
                TXWMAX(NT)=MAX(TXWMAX(NT),TOX(L,K,NT))  
                TXWMIN(NT)=MIN(TXWMIN(NT),TOX(L,K,NT))  
              ENDDO  
            ENDDO  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(6).GE.1)THEN  
          DO L=2,LA  
            SDBMAX=MAX(SDBMAX,SEDBT(L,KBT(L)))  
            SDBMIN=MIN(SDBMIN,SEDBT(L,KBT(L)))  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(7).GE.1)THEN  
          DO L=2,LA  
            SNBMAX=MAX(SNBMAX,SNDBT(L,KBT(L)))  
            SNBMIN=MIN(SNBMIN,SNDBT(L,KBT(L)))  
          ENDDO  
        ENDIF  
        IF(ISDMPT.GE.1)THEN  
          IF(ISTRAN(7).GE.1.OR.ISTRAN(6).GE.1)THEN  
            DO L=2,LA  
              BDHMAX=MAX(BDHMAX,VOLBW2(L,KBT(L)))  
              BDHMIN=MIN(BDHMIN,VOLBW2(L,KBT(L)))  
            ENDDO  
          ENDIF  
        ENDIF  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            DO L=2,LA  
              TXBMAX(NT)=MAX(TXBMAX(NT),TOXB(L,KBT(L),NT))  
              TXBMIN(NT)=MIN(TXBMIN(NT),TOXB(L,KBT(L),NT))  
            ENDDO  
          ENDDO  
        ENDIF  
C  
C **  WRITE ARRAYS  
C  
        IF(ISDUMP.EQ.1) RSCALE=65535.  
        IF(ISDUMP.EQ.2) RSCALE=65535.  
C  
C **  WATER SURFACE ELEVATION  
C  
        IF(ISDMPP.GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDSEL,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDSEL,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(SELMAX-SELMIN)  
          DO L=2,LA  
            DMPVALL(L-1)=SCALE*(GI*P(L)-SELMIN)  
            IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,SELMAX,SELMIN  
            WRITE(1,101)IDMPVALL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO L=2,LA  
              IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
            ENDDO  
            WRITE(1)TIME,SELMAX,SELMIN  
            WRITE(1)IB16VALL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  U VELOCITY COMPONENT  
C  
        IF(ISDMPU.GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDUUU,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDUUU,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(UUUMAX-UUUMIN)  
          DO K=1,KC  
            DO L=2,LA  
              UUUTMP=0.5*(U(L,K)+U(L+1,K))  
              DMPVAL(L-1,K)=SCALE*(UUUTMP-UUUMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,UUUMAX,UUUMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,UUUMAX,UUUMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  V VELOCITY COMPONENT  
C  
        IF(ISDMPU.GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDVVV,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDVVV,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(VVVMAX-VVVMIN)  
          DO K=1,KC  
            DO L=2,LA  
              VVVTMP=0.5*(V(L,K)+V(LNC(L),K))  
              DMPVAL(L-1,K)=SCALE*(VVVTMP-VVVMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,VVVMAX,VVVMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,VVVMAX,VVVMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  W VELOCITY COMPONENT  
C  
        IF(ISDMPW.GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDWWW,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDWWW,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(WWWMAX-WWWMIN)  
          DO K=1,KC  
            DO L=2,LA  
              WWWTMP=0.5*(W(L,K)+W(L,K-1))  
              DMPVAL(L-1,K)=SCALE*(WWWTMP-WWWMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,WWWMAX,WWWMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,WWWMAX,WWWMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  SALINITY  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(1).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDSAL,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDSAL,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(SALMAX-SALMIN)  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SCALE*(SAL(L,K)-SALMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,SALMAX,SALMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,SALMAX,SALMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TEMPATURE  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(2).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTEM,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDTEM,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(TEMMAX-TEMMIN)  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SCALE*(TEM(L,K)-TEMMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,TEMMAX,TEMMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,TEMMAX,TEMMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  DYE  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(3).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDDYE,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDDYE,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(DYEMAX-DYEMIN)  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SCALE*(DYE(L,K)-DYEMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,DYEMAX,DYEMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,DYEMAX,DYEMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL COHESIVE SEDIMENT WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(6).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDSDW,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDSDW,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(SDWMAX-SDWMIN)  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SCALE*(SEDT(L,K)-SDWMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,SDWMAX,SDWMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,SDWMAX,SDWMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL NONCOHESIVE SEDIMENT IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(7).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDSNW,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDSNW,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(SNWMAX-SNWMIN)  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SCALE*(SNDT(L,K)-SNWMIN)  
              IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,SNWMAX,SNWMIN  
            WRITE(1,101)IDMPVAL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO K=1,KC  
              DO L=2,LA  
                IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
              ENDDO  
            ENDDO  
            WRITE(1)TIME,SNWMAX,SNWMIN  
            WRITE(1)IB16VAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL TOXIC CONTAMINANTS IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTWT(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTWT(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            SCALE=RSCALE/(TXWMAX(NT)-TXWMIN(NT))  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=SCALE*(TOX(L,K,NT)-TXWMIN(NT))  
                IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,TXWMAX(NT),TXWMIN(NT)  
              WRITE(1,101)IDMPVAL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO K=1,KC  
                DO L=2,LA  
                  IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
                ENDDO  
              ENDDO  
              WRITE(1)TIME,TXWMAX(NT),TXWMIN(NT)  
              WRITE(1)IB16VAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  FREE DISSOLVED TOXIC CONTAMINANTS IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTWF(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTWF(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            SCALE=RSCALE/(TXWMAX(NT)-TXWMIN(NT))  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=SCALE*(TOX(L,K,NT)-TXWMIN(NT))  
                IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,TXWMAX(NT),TXWMIN(NT)  
              WRITE(1,101)IDMPVAL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO K=1,KC  
                DO L=2,LA  
                  IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
                ENDDO  
              ENDDO  
              WRITE(1)TIME,TXWMAX(NT),TXWMIN(NT)  
              WRITE(1)IB16VAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  COMPLEXED DISSOLVED TOXIC CONTAMINANTS IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTWC(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTWC(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            SCALE=RSCALE/(TXWMAX(NT)-TXWMIN(NT))  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=SCALE*(TOX(L,K,NT)-TXWMIN(NT))  
                IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,TXWMAX(NT),TXWMIN(NT)  
              WRITE(1,101)IDMPVAL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO K=1,KC  
                DO L=2,LA  
                  IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
                ENDDO  
              ENDDO  
              WRITE(1)TIME,TXWMAX(NT),TXWMIN(NT)  
              WRITE(1)IB16VAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  PARTICULATE TOXIC CONTAMINANT IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTWP(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTWP(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
C  
C        SCALE=100.  
C  
            SCALE=RSCALE  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=SCALE*TOXPFTW(L,K,NT)  
                IDMPVAL(L-1,K)=NINT(DMPVAL(L-1,K))  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,R1,R0  
              WRITE(1,101)IDMPVAL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO K=1,KC  
                DO L=2,LA  
                  IB16VAL(L-1,K)=IDMPVAL(L-1,K)+IADJDMP  
                ENDDO  
              ENDDO  
              WRITE(1)TIME,R1,R0  
              WRITE(1)IB16VAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  TOTAL COHESIVE SEDIMENT IN BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(6).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDSDB,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDSDB,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(SDBMAX-SDBMIN)  
          DO L=2,LA  
            DMPVALL(L-1)=SCALE*(SEDBT(L,KBT(L))-SDBMIN)  
            IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,SDBMAX,SDBMIN  
            WRITE(1,101)IDMPVALL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO L=2,LA  
              IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
            ENDDO  
            WRITE(1)TIME,SDBMAX,SDBMIN  
            WRITE(1)IB16VALL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL NONCOHESIVE SEDIMENT IN BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(7).GE.1)THEN  
          IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDSNB,POSITION='APPEND')  
          IF(ISDUMP.EQ.2)  
     &        OPEN(1,FILE=FNDSNB,POSITION='APPEND',FORM='UNFORMATTED')  
          SCALE=RSCALE/(SNBMAX-SNBMIN)  
          DO L=2,LA  
            DMPVALL(L-1)=SCALE*(SNDBT(L,KBT(L))-SNBMIN)  
            IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
          ENDDO  
          IF(ISDUMP.EQ.1)THEN  
            WRITE(1,*)TIME,SNBMAX,SNBMIN  
            WRITE(1,101)IDMPVALL  
          ENDIF  
          IF(ISDUMP.EQ.2)THEN  
            DO L=2,LA  
              IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
            ENDDO  
            WRITE(1)TIME,SNBMAX,SNBMIN  
            WRITE(1)IB16VALL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  THICKNESS OF SEDIMENT BED  
C  
        IF(ISDMPT.GE.1)THEN  
          IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1)THEN  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDBDH,POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDBDH,POSITION='APPEND',FORM='UNFORMATTED')  
            SCALE=RSCALE/(BDHMAX-BDHMIN)  
            DO L=2,LA  
              DMPVALL(L-1)=SCALE*(VOLBW2(L,KBT(L))-BDHMIN)  
              IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,BDHMAX,BDHMIN  
              WRITE(1,101)IDMPVALL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO L=2,LA  
                IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
              ENDDO  
              WRITE(1)TIME,BDHMAX,BDHMIN  
              WRITE(1)IB16VALL  
            ENDIF  
            CLOSE(1)  
          ENDIF  
        ENDIF  
C  
C **  TOTAL TOXIC CONTAMINANTS IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTBT(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTBT(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            SCALE=RSCALE/(TXBMAX(NT)-TXBMIN(NT))  
            DO L=2,LA  
              DMPVALL(L-1)=SCALE*(TOXB(L,KBT(L),NT)-TXBMIN(NT))  
              IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,TXBMAX(NT),TXBMIN(NT)  
              WRITE(1,101)IDMPVALL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO L=2,LA  
                IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
              ENDDO  
              WRITE(1)TIME,TXBMAX(NT),TXBMIN(NT)  
              WRITE(1)IB16VALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  FREE DISSOLVED TOXIC CONTAMINANTS IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTBF(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTBF(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            SCALE=RSCALE/(TXBMAX(NT)-TXBMIN(NT))  
            DO L=2,LA  
              DMPVALL(L-1)=SCALE*(TOXB(L,KBT(L),NT)-TXBMIN(NT))  
              IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,TXBMAX(NT),TXBMIN(NT)  
              WRITE(1,101)IDMPVALL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO L=2,LA  
                IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
              ENDDO  
              WRITE(1)TIME,TXBMAX(NT),TXBMIN(NT)  
              WRITE(1)IB16VALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  COMPLEXED DISSOLVED TOXIC CONTAMINANTS IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTBC(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTBC(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            SCALE=RSCALE/(TXBMAX(NT)-TXBMIN(NT))  
            DO L=2,LA  
              DMPVALL(L-1)=SCALE*(TOXB(L,KBT(L),NT)-TXBMIN(NT))  
              IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,TXBMAX(NT),TXBMIN(NT)  
              WRITE(1,101)IDMPVALL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO L=2,LA  
                IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
              ENDDO  
              WRITE(1)TIME,TXBMAX(NT),TXBMIN(NT)  
              WRITE(1)IB16VALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  PARTICULATE TOXIC CONTAMINANT IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.1) OPEN(1,FILE=FNDTBP(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.2)  
     &          OPEN(1,FILE=FNDTBP(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
C  
C        SCALE=100.  
C  
            SCALE=RSCALE  
            DO L=2,LA  
              DMPVALL(L-1)=SCALE*TOXPFTB(L,KB,NT)  
              IDMPVALL(L-1)=NINT(DMPVALL(L-1))  
            ENDDO  
            IF(ISDUMP.EQ.1)THEN  
              WRITE(1,*)TIME,R1,R0  
              WRITE(1,101)IDMPVALL  
            ENDIF  
            IF(ISDUMP.EQ.2)THEN  
              DO L=2,LA  
                IB16VALL(L-1)=IDMPVALL(L-1)+IADJDMP  
              ENDDO  
              WRITE(1)TIME,R1,R0  
              WRITE(1)IB16VALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  IF(ISDUMP EQUAL 3 OR 4, WRITE FLOATING POINT  
C **  DUMP FILES  
C  
      IF(ISDUMP.GE.3)THEN  
C  
C **  WATER SURFACE ELEVATION  
C  
        IF(ISDMPP.GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDSEL,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDSEL,POSITION='APPEND',FORM='UNFORMATTED')  
          DO L=2,LA  
            DMPVALL(L-1)=GI*P(L)  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            WRITE(1,111)(DMPVALL(L),L=1,LA-1)  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVALL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  U VELOCITY COMPONENT  
C  
        IF(ISDMPU.GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDUUU,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDUUU,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=0.5*(U(L,K)+U(L+1,K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            IF(ISDMPU.EQ.1)THEN  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(1,111)(U(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  V VELOCITY COMPONENT  
C  
        IF(ISDMPU.GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDVVV,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDVVV,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=0.5*(V(L,K)+V(LNC(L),K))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            IF(ISDMPU.EQ.1)THEN  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(1,111)(V(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  W VELOCITY COMPONENT  
C  
        IF(ISDMPW.GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDWWW,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDWWW,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=0.5*(W(L,K)+W(L,K-1))  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            IF(ISDMPW.EQ.1)THEN  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ELSE  
              DO L=2,LA  
                WRITE(1,111)(W(L,K), K=1,KS)  
              ENDDO  
            ENDIF  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  SALINITY  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(1).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDSAL,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDSAL,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SAL(L,K)  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TEMPERATURE  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(2).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTEM,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDTEM,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=TEM(L,K)  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  DYE  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(3).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDDYE,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDDYE,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=DYE(L,K)  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL COHESIVE SEDIMENT IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(6).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDSDW,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDSDW,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SEDT(L,K)  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL NONCOHESIVE SEDIMENT IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(7).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDSNW,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDSNW,POSITION='APPEND',FORM='UNFORMATTED')  
          DO K=1,KC  
            DO L=2,LA  
              DMPVAL(L-1,K)=SNDT(L,K)  
            ENDDO  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVAL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL TOXIC CONTAMINANTS IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTWT(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTWT(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=TOX(L,K,NT)  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  FREE DISSOLVED TOXIC CONTAMINANTS IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTWF(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTWF(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=TOX(L,K,NT)  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  COMPLEXED DISSOLVED TOXIC CONTAMINANTS IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTWC(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTWC(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=TOX(L,K,NT)  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  PARTICULATE TOXIC CONTAMINANT IN WATER COLUMN  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTWP(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTWP(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO K=1,KC  
              DO L=2,LA  
                DMPVAL(L-1,K)=TOXPFTW(L,K,NT)  
              ENDDO  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVAL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  TOTAL COHESIVE SEDIMENT IN BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(6).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDSDB,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDSDB,POSITION='APPEND',FORM='UNFORMATTED')  
          DO L=2,LA  
            DMPVALL(L-1)=SEDBT(L,KBT(L))  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVALL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  TOTAL NONCOHESIVE SEDIMENT IN BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(7).GE.1)THEN  
          IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDSNB,POSITION='APPEND')  
          IF(ISDUMP.EQ.4)  
     &        OPEN(1,FILE=FNDSNB,POSITION='APPEND',FORM='UNFORMATTED')  
          DO L=2,LA  
            DMPVALL(L-1)=SNDBT(L,KBT(L))  
          ENDDO  
          IF(ISDUMP.EQ.3)THEN  
            WRITE(1,*)TIME  
            DO L=1,LA-1  
              WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
            ENDDO  
          ENDIF  
          IF(ISDUMP.EQ.4)THEN  
            WRITE(1)TIME  
            WRITE(1)DMPVALL  
          ENDIF  
          CLOSE(1)  
        ENDIF  
C  
C **  THICKNESS OF SEDIMENT BED  
C  
        IF(ISDMPT.GE.1)THEN  
          IF(ISTRAN(6).GE.1.OR.ISTRAN(7).GE.1)THEN  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDBDH,POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDBDH,POSITION='APPEND',FORM='UNFORMATTED')  
            DO L=2,LA  
              DMPVALL(L-1)=VOLBW2(L,KB)  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVALL  
            ENDIF  
            CLOSE(1)  
          ENDIF  
        ENDIF  
C  
C **  TOTAL TOXIC CONTAMINANTS IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTBT(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTBT(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO L=2,LA  
              DMPVALL(L-1)=TOXB(L,KB,NT)  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  FREE DISSOLVED TOXIC CONTAMINANTS IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTBF(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTBF(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO L=2,LA  
              DMPVALL(L-1)=TOXB(L,KB,NT)  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  COMPLEXED DISSOLVED TOXIC CONTAMINANTS IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTBC(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTBC(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO L=2,LA  
              DMPVALL(L-1)=TOXB(L,KB,NT)  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
C  
C **  PARTICULATE TOXIC CONTAMINANT IN SEDIMENT BED  
C  
        IF(ISDMPT.GE.1.AND.ISTRAN(5).GE.1)THEN  
          DO NT=1,NTOX  
            IF(ISDUMP.EQ.3) OPEN(1,FILE=FNDTBP(NT),POSITION='APPEND')  
            IF(ISDUMP.EQ.4)  
     &          OPEN(1,FILE=FNDTBP(NT),POSITION='APPEND',
     &          FORM='UNFORMATTED')  
            DO L=2,LA  
              DMPVALL(L-1)=SCALE*TOXPFTB(L,KB,NT)  
            ENDDO  
            IF(ISDUMP.EQ.3)THEN  
              WRITE(1,*)TIME  
              DO L=1,LA-1  
                WRITE(1,111)(DMPVAL(L,K), K=1,KC)  
              ENDDO  
            ENDIF  
            IF(ISDUMP.EQ.4)THEN  
              WRITE(1)TIME  
              WRITE(1)DMPVALL  
            ENDIF  
            CLOSE(1)  
          ENDDO  
        ENDIF  
      ENDIF  
C  
C **  CHECK BY READING BINARY FILES  
C        READ(1)TIME,RMAX,RMIN  
C        READ(1)IB08VALL  
C        READ(1)TIME,RMAX,RMIN  
C        READ(1)IB08VAL  
C        READ(1)TIME,SELMAX,SELMIN  
C        READ(1)IB16VALL  
C        TMPVAL=(SELMAX-SELMIN)/RSCALE  
C        READ(1)TIME,SALMAX,SALMIN  
C        READ(1)IB16VAL  
C        TMPVAL=(SALMAX-SALMIN)/RSCALE  
C  
  100 FORMAT(A80)  
  101 FORMAT(8I6)  
  102 FORMAT(8I4)  
  111 FORMAT(10E12.4)  
  201 FORMAT(//,' CHECK 2D  8 BIT VARIABLE',/)  
  202 FORMAT(//,' CHECK 3D  8 BIT VARIABLE',/)  
  203 FORMAT(//,' CHECK 2D 16 BIT VARIABLE',/)  
  204 FORMAT(//,' CHECK 3D 16 BIT VARIABLE',/)  
  205 FORMAT(8F8.2)  
      RETURN  
      END  

