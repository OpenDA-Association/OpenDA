      SUBROUTINE SUBCHAN(QCHANUT,QCHANVT,IACTIVE,RLAMN,RLAMO,DE_T,  
     &    IACTALL)  
C  
C CHANGE RECORD  
C ** SUBROUTINE SUBCHAN CALCULATES SUBGRID CHANNEL INTERACTIONS AND IS  
C ** CALLED FROM CALPUV2TC  
C  
      USE GLOBAL  
      DIMENSION IACTIVE(NCHANM),QCHANUT(NCHANM),QCHANVT(NCHANM)  
C  
      IF(MDCHH.GE.1)THEN  
        IACTALL=0  
        DO NMD=1,MDCHH  
          CCCCHU(NMD)=0.0  
          CCCCHV(NMD)=0.0  
          CCCCHH(NMD)=0.0  
          LHOST=LMDCHH(NMD)  
          LCHNU=LMDCHU(NMD)  
          LCHNV=LMDCHV(NMD)  
C  
C         X-DIRECTION CHANNEL  
C  
          IF(MDCHTYP(NMD).EQ.1)THEN  
            IACTIVE(NMD)=0  
            SRFHOST=H1P(LHOST)+BELV(LHOST)  
            SRFCHAN=H1P(LCHNU)+BELV(LCHNU)  
            IF(SRFCHAN.GT.SRFHOST)THEN  
              IF(H1P(LCHNU).GT.HDRY)THEN  
                HCHNCOR=HWET  
                IACTIVE(NMD)=1  
                IACTALL=IACTALL+1  
              ENDIF  
            ENDIF  
            IF(SRFHOST.GT.SRFCHAN)THEN  
              IF(H1P(LHOST).GT.HDRY)THEN  
                HCHNCOR=HDRY  
                IACTIVE(NMD)=1  
                IACTALL=IACTALL+1  
              ENDIF  
            ENDIF  
            IF(HP(LHOST).LE.0.0.OR.HP(LCHNU).LE.0.0)THEN  
              IF(IACTIVE(NMD).EQ.1)THEN  
                IACTALL=IACTALL-1  
                IACTIVE(NMD)=0  
              ENDIF  
            ENDIF  
            IF(IACTIVE(NMD).EQ.1)THEN  
              WCHAN=DXP(LCHNU)  
C  
C              RLCHN=0.5*DYP(LCHNU)+0.25*DYP(LHOST)  
C              HCHAN=0.5*DYP(LCHNU)*HP(LCHNU)+0.25*DYP(LHOST)*HP(LHOST)  
C  
              RLCHN=0.5*DYP(LCHNU)+CHANLEN(NMD)  
              HCHAN=0.5*DYP(LCHNU)*H1P(LCHNU)+CHANLEN(NMD)*H1P(LHOST)  
              HCHAN=HCHAN/RLCHN  
              IF(HCHAN.GT.0.)THEN  
                TMPVAL=CHANFRIC(NMD)*DE_T/(HCHAN*HCHAN*WCHAN)  
                CCCCHU(NMD)=1./(1.+TMPVAL*ABS(QCHANUT(NMD)))  
                CCCCHV(NMD)=DE_T*HCHAN*WCHAN/RLCHN  
              ENDIF  
            ENDIF  
          ENDIF  
C  
C         Y-DIRECTION CHANNEL  
C  
          IF(MDCHTYP(NMD).EQ.2)THEN  
            IHCHMX=0  
            IHCHMN=0  
            IACTIVE(NMD)=0  
            SRFHOST=H1P(LHOST)+BELV(LHOST)  
            SRFCHAN=H1P(LCHNV)+BELV(LCHNV)  
            IF(SRFCHAN.GT.SRFHOST)THEN  
              IF(H1P(LCHNV).GT.HDRY)THEN  
                HCHNMX=-H1P(LCHNV)  
                IHCHMX=1  
                IACTIVE(NMD)=1  
                IACTALL=IACTALL+1  
              ENDIF  
            ENDIF  
            IF(SRFHOST.GT.SRFCHAN)THEN  
              IF(H1P(LHOST).GT.HDRY)THEN  
                HCHNMN=H1P(LHOST)  
                IHCHMN=1  
                IACTIVE(NMD)=1  
                IACTALL=IACTALL+1  
              ENDIF  
            ENDIF  
            IF(HP(LHOST).LE.0.0.OR.HP(LCHNU).LE.0.0)THEN  
              IF(IACTIVE(NMD).EQ.1)THEN  
                IACTALL=IACTALL-1  
                IACTIVE(NMD)=0  
              ENDIF  
            ENDIF  
            IF(IACTIVE(NMD).EQ.1)THEN  
              WCHAN=DYP(LCHNV)  
C  
C              RLCHN=0.5*DXP(LCHNV)+0.25*DXP(LHOST)  
C              HCHAN=0.5*DXP(LCHNV)*HP(LCHNV)+0.25*DXP(LHOST)*HP(LHOST)  
C  
              RLCHN=0.5*DXP(LCHNV)+CHANLEN(NMD)  
              HCHAN=0.5*DXP(LCHNV)*H1P(LCHNV)+CHANLEN(NMD)*H1P(LHOST)  
              HCHAN=HCHAN/RLCHN  
              IF(IHCHMX.EQ.1) HCHAN=MAX(HCHAN,HCHNMX)  
              IF(IHCHMN.EQ.1) HCHAN=MIN(HCHAN,HCHNMN)  
              IF(HCHAN.GT.0.)THEN  
                TMPVAL=CHANFRIC(NMD)*DE_T/(HCHAN*HCHAN*WCHAN)  
                CCCCHU(NMD)=1./(1.+TMPVAL*ABS(QCHANVT(NMD)))  
                CCCCHV(NMD)=DE_T*HCHAN*WCHAN/RLCHN  
              ENDIF  
            ENDIF  
          ENDIF  
          CC(LHOST)=CC(LHOST)+G*RLAMN*RLAMN*CCCCHU(NMD)*CCCCHV(NMD)  
          CCCCHH(NMD)=G*RLAMN*RLAMN*CCCCHU(NMD)*CCCCHV(NMD)  
          IF(MDCHTYP(NMD).EQ.1)THEN  
            CC(LCHNU)=CC(LCHNU)+G*RLAMN*RLAMN*CCCCHU(NMD)*CCCCHV(NMD)  
            TMPVAL=G*(RLAMO+RLAMN*CCCCHU(NMD))*QCHANUT(NMD)  
     &          -G*RLAMN*RLAMO*CCCCHU(NMD)*CCCCHV(NMD)*(P1(LHOST)-
     &          P1(LCHNU))  
            FP(LHOST)=FP(LHOST)+TMPVAL  
            FP(LCHNU)=FP(LCHNU)-TMPVAL  
          ENDIF  
          IF(MDCHTYP(NMD).EQ.2)THEN  
            CC(LCHNV)=CC(LCHNV)+G*RLAMN*RLAMN*CCCCHU(NMD)*CCCCHV(NMD)  
            TMPVAL=G*(RLAMO+RLAMN*CCCCHU(NMD))*QCHANVT(NMD)  
     &          -G*RLAMN*RLAMO*CCCCHU(NMD)*CCCCHV(NMD)*(P1(LHOST)-
     &          P1(LCHNV))  
            FP(LHOST)=FP(LHOST)+TMPVAL  
            FP(LCHNV)=FP(LCHNV)-TMPVAL  
          ENDIF  
        ENDDO  
        WRITE(8,1949)N,IACTALL  
      ENDIF  
 1949 FORMAT(' N, # ACTIVE 2 GRID FLOWS = ',2I8)  
 1948 FORMAT(I5,3E12.4)  
      RETURN  
      END  

