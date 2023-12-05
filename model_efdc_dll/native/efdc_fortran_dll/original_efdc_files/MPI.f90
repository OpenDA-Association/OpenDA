      MODULE MPI

      USE GLOBAL
      USE OMP_LIB
      INCLUDE 'mpif.h'

      REAL*8 :: STIME,TTIME,MPI_WTIMES(4000)
      REAL*8 :: S1TIME,S2TIME,S3TIME,S4TIME,S5TIME
      CHARACTER*30 :: MPI_HOSTSPOTS(4000),WT_CHAR
      CHARACTER*3  :: WT_NUM
      PARAMETER (MAXNTH=64)
      INTEGER RECVCOUNTS(0:1000),DISPLS(0:1000)
      INTEGER ITHE,LOMPS,LOMPE,LOMPS1,LOMPE1,NCOLLECT
      INTEGER IERR,MYRANK,NPROCS,OMP_OPT
      INTEGER IOMPS(MAXNTH),IOMPE(MAXNTH),IOMPS1(MAXNTH),IOMPE1(MAXNTH)
      INTEGER NTH,OMPNUM,LSTART,LEND
      INTEGER INEWTYPE,NEWTYPE(1000),INEWTYPE1(0:1000),INEWTYPE2(0:1000),INEWTYPE3(0:1000)
      INTEGER NDRYCELL,OMPTHPUV,OMPTHCONG
      INTEGER IREQ(1000),IREQ1,IREQ2,NUMBER, LCHUNK
      INTEGER STATUS1(MPI_STATUS_SIZE),STATUS2(MPI_STATUS_SIZE)
      INTEGER LMPI1,LMPI2,LMPILA,LMPILC
      INTEGER WT_VAL,WT_COUNT,WT_RATIO
      INTEGER,ALLOCATABLE :: MPI_IMASKDRY(:)
      CHARACTER MPI_DEBUG_C
      INTEGER   MPI_DEBUG
      INTEGER   MPI_I4
      REAL      MPI_R4
      REAL*8    MPI_R8
      LOGICAL   MPI_LG
      LOGICAL   IS_PSER(10000),IS_CSER(10000,1000),IS_QSER(10000),IS_QCTL(10000)

      CONTAINS

!###########################################################################################################

      SUBROUTINE MPI_INITIALIZE

      USE OMP_LIB
      INCLUDE 'mpif.h'
      MYRANK=0
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYRANK,IERR)

!$OMP PARALLEL
      OMPNUM=OMP_GET_MAX_THREADS()
      !CALL OMP_SET_NUM_THREADS(OMPNUM)
!$OMP END PARALLEL

      CALL GETARG(2,MPI_DEBUG_C)
      READ(MPI_DEBUG_C,'(I1.1)') MPI_DEBUG
      IF(MYRANK.EQ.0) PRINT*, 'MPI_DEBUG = ', MPI_DEBUG

      ENDSUBROUTINE MPI_INITIALIZE

!###########################################################################################################

      SUBROUTINE MPI_DECOMPOSITION

      USE OMP_LIB
      INCLUDE 'mpif.h'

      IF(MYRANK==0) WRITE(*,*) '#########################'
      IF(MYRANK==0) WRITE(*,*) 'MPI NODDS   =',NPROCS
      IF(MYRANK==0) WRITE(*,*) 'OMP THREADS =',OMPNUM
      IF(MYRANK==0) WRITE(*,*) '#########################'

      NTH = OMPNUM * NPROCS
      LCHUNK=NINT(FLOAT(LC-1)/FLOAT(NTH))

      DO N=1,NTH
        IOMPS(N)=(N-1)*LCHUNK+2
        IOMPE(N)=IOMPS(N)+LCHUNK -1
      ENDDO
      IOMPE(NTH)=LC
      IOMPS1=IOMPS   ; IOMPE1=IOMPE
      IOMPS1(1)=1    ; IOMPE1(NTH)=LA

      LMPI1  = IOMPS1(OMPNUM*MYRANK+1)
      LMPI2  = IOMPS(OMPNUM*MYRANK+1)
      LMPILC = IOMPE(OMPNUM*MYRANK+OMPNUM)
      LMPILA = IOMPE1(OMPNUM*MYRANK+OMPNUM)

      IF(MYRANK==0) THEN
         PRINT*, '####################################################'
         DO N=0,NPROCS-1
            PRINT*, 'RANK NUMBER : ', N , IOMPS(OMPNUM*N+1), IOMPE(OMPNUM*N+OMPNUM)
         ENDDO
         PRINT*, '####################################################'

         PRINT*, '####################################################'
         DO N=0,NPROCS-1
            PRINT*, 'RANK NUMBER : ', N , IOMPS1(OMPNUM*N+1), IOMPE1(OMPNUM*N+OMPNUM)
         ENDDO
         PRINT*, '####################################################'
      ENDIF

      DO N=0,NPROCS-1
         RECVCOUNTS(N)=IOMPE(OMPNUM*(N+1))-IOMPS(OMPNUM*N+1)+1
      ENDDO

      DISPLS(0)=2
      DO N=1,NPROCS-1
        DISPLS(N)=DISPLS(N-1)+RECVCOUNTS(N-1)
      ENDDO

      CALL MPI_TYPE_VECTOR(KCM,IC,LCM,MPI_REAL,INEWTYPE,IERR)
      CALL MPI_TYPE_COMMIT(INEWTYPE,IERR)

      DO N=0,NPROCS-1
         CALL MPI_TYPE_VECTOR(KCM,RECVCOUNTS(N),LCM,MPI_REAL,INEWTYPE1(N),IERR)
         CALL MPI_TYPE_COMMIT(INEWTYPE1(N),IERR)
      ENDDO

      DO N=0,NPROCS-1
         CALL MPI_TYPE_VECTOR(KBM,RECVCOUNTS(N),LCM,MPI_REAL,INEWTYPE2(N),IERR)
         CALL MPI_TYPE_COMMIT(INEWTYPE2(N),IERR)
      ENDDO

      DO N=0,NPROCS-1
         CALL MPI_TYPE_VECTOR(KCM+1,RECVCOUNTS(N),LCM,MPI_REAL,INEWTYPE3(N),IERR)
         CALL MPI_TYPE_COMMIT(INEWTYPE3(N),IERR)
      ENDDO

      ENDSUBROUTINE MPI_DECOMPOSITION

!###########################################################################################################

      SUBROUTINE BROADCAST_BOUNDARY(ARRAY_1D,NUMBER)

      INCLUDE 'mpif.h'
      REAL ARRAY_1D(LCM)

      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYRANK,IERR)

      IF(NPROCS.GE.2)THEN
      DO NP=1,NPROCS-1,2
        IF(MYRANK==NP-1)THEN
         CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ELSEIF(MYRANK==NP)THEN
         CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ENDIF
      ENDDO
      ENDIF
      IF(NPROCS.GE.3)THEN
      DO NP=2,NPROCS-1,2
        IF(MYRANK==NP-1)THEN
         CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ELSEIF(MYRANK==NP)THEN
         CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ENDIF
      ENDDO
      ENDIF

      END SUBROUTINE

!###########################################################################################################

      SUBROUTINE BROADCAST_BOUNDARY_LBM(ARRAY_1D,NUMBER)

        INCLUDE 'mpif.h'
        REAL ARRAY_1D(0:LCM)

        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYRANK,IERR)

        IF(NPROCS.GE.2)THEN
        DO NP=1,NPROCS-1,2
          IF(MYRANK==NP-1)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ELSEIF(MYRANK==NP)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ENDIF
        ENDDO
        ENDIF
        IF(NPROCS.GE.3)THEN
        DO NP=2,NPROCS-1,2
          IF(MYRANK==NP-1)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ELSEIF(MYRANK==NP)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_REAL,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_REAL,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ENDIF
        ENDDO
        ENDIF

        END SUBROUTINE

!###########################################################################################################

      SUBROUTINE BROADCAST_BOUNDARY_R8(ARRAY_1D,NUMBER)

        INCLUDE 'mpif.h'
        REAL*8 ARRAY_1D(LCM)

        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,NPROCS,IERR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,MYRANK,IERR)

        IF(NPROCS.GE.2)THEN
        DO NP=1,NPROCS-1,2
          IF(MYRANK==NP-1)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_DOUBLE,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_DOUBLE,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ELSEIF(MYRANK==NP)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_DOUBLE,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_DOUBLE,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ENDIF
        ENDDO
        ENDIF
        IF(NPROCS.GE.3)THEN
        DO NP=2,NPROCS-1,2
          IF(MYRANK==NP-1)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_DOUBLE,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_DOUBLE,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ELSEIF(MYRANK==NP)THEN
           CALL MPI_ISEND( ARRAY_1D(IOMPS(OMPNUM*NP+1))       ,NUMBER,MPI_DOUBLE,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
           CALL MPI_IRECV( ARRAY_1D(IOMPS(OMPNUM*NP+1)-NUMBER),NUMBER,MPI_DOUBLE,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
           CALL MPI_WAIT(IREQ1,STATUS1,IERR)
           CALL MPI_WAIT(IREQ2,STATUS2,IERR)
          ENDIF
        ENDDO
        ENDIF

        END SUBROUTINE

!###########################################################################################################

      SUBROUTINE BROADCAST_BOUNDARY_ARRAY(ARRAY_2D,NUMBER)

      INCLUDE 'mpif.h'
      REAL ARRAY_2D(LCM,KCM)

      IF(NPROCS.GT.1)THEN
      DO NP=1,NPROCS-1
        IF(MYRANK==NP-1)THEN
         CALL MPI_ISEND( ARRAY_2D(IOMPS(OMPNUM*NP+1)-NUMBER,1),1,INEWTYPE,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_2D(IOMPS(OMPNUM*NP+1),1)       ,1,INEWTYPE,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ELSEIF(MYRANK==NP)THEN
         CALL MPI_ISEND( ARRAY_2D(IOMPS(OMPNUM*NP+1),1)       ,1,INEWTYPE,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_2D(IOMPS(OMPNUM*NP+1)-NUMBER,1),1,INEWTYPE,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ENDIF
      ENDDO
      ENDIF
      END SUBROUTINE

!###########################################################################################################

      SUBROUTINE BROADCAST_BOUNDARY_ARRAY_ZEROKCM(ARRAY_2D,NUMBER)

      INCLUDE 'mpif.h'
      REAL ARRAY_2D(LCM,0:KCM)

      IF(NPROCS.GT.1)THEN
      DO NP=1,NPROCS-1
        IF(MYRANK==NP-1)THEN
         CALL MPI_ISEND( ARRAY_2D(IOMPS(OMPNUM*NP+1)-NUMBER,0),1,INEWTYPE3,NP,87,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_2D(IOMPS(OMPNUM*NP+1),0)       ,1,INEWTYPE3,NP,82,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ELSEIF(MYRANK==NP)THEN
         CALL MPI_ISEND( ARRAY_2D(IOMPS(OMPNUM*NP+1),0)       ,1,INEWTYPE3,NP-1,82,MPI_COMM_WORLD,IREQ1,IERR)
         CALL MPI_IRECV( ARRAY_2D(IOMPS(OMPNUM*NP+1)-NUMBER,0),1,INEWTYPE3,NP-1,87,MPI_COMM_WORLD,IREQ2,IERR)
         CALL MPI_WAIT(IREQ1,STATUS1,IERR)
         CALL MPI_WAIT(IREQ2,STATUS2,IERR)
        ENDIF
      ENDDO
      ENDIF
      END SUBROUTINE

!###########################################################################################################

      SUBROUTINE COLLECT_IN_ZERO(ARRAY_1D)

      INCLUDE 'mpif.h'
      REAL ARRAY_1D(LCM)

      IF(NPROCS.GE.2)THEN
        DO NP=1,NPROCS-1
         IF(MYRANK==NP) THEN
           CALL MPI_ISEND( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_REAL,0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
           CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
         ELSEIF(MYRANK==0) THEN
           CALL MPI_IRECV( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_REAL,NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
           CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
         ENDIF
        ENDDO
      ENDIF
      END SUBROUTINE

!###########################################################################################################

      SUBROUTINE COLLECT_IN_ZERO_R8(ARRAY_1D)

        INCLUDE 'mpif.h'
        REAL*8 ARRAY_1D(LCM)

        IF(NPROCS.GE.2)THEN
          DO NP=1,NPROCS-1
           IF(MYRANK==NP) THEN
             CALL MPI_ISEND( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_DOUBLE,0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
             CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
           ELSEIF(MYRANK==0) THEN
             CALL MPI_IRECV( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_DOUBLE,NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
             CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
           ENDIF
          ENDDO
        ENDIF
        END SUBROUTINE


!###########################################################################################################

        SUBROUTINE COLLECT_IN_ZERO_INT(ARRAY_1D)

          INCLUDE 'mpif.h'
          INTEGER ARRAY_1D(LCM)

          IF(NPROCS.GE.2)THEN
            DO NP=1,NPROCS-1
             IF(MYRANK==NP) THEN
               CALL MPI_ISEND( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_INTEGER,0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
               CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
             ELSEIF(MYRANK==0) THEN
               CALL MPI_IRECV( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_INTEGER,NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
               CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
             ENDIF
            ENDDO
          ENDIF
          END SUBROUTINE

!###########################################################################################################

      SUBROUTINE COLLECT_IN_ZERO_LBM(ARRAY_1D)

        INCLUDE 'mpif.h'
        REAL ARRAY_1D(0:LCM)

        IF(NPROCS.GE.2)THEN
          DO NP=1,NPROCS-1
           IF(MYRANK==NP) THEN
             CALL MPI_ISEND( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_REAL,0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
             CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
           ELSEIF(MYRANK==0) THEN
             CALL MPI_IRECV( ARRAY_1D(DISPLS(NP)),RECVCOUNTS(NP),MPI_REAL,NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
             CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
           ENDIF
          ENDDO
        ENDIF
        END SUBROUTINE

!###########################################################################################################

      SUBROUTINE COLLECT_IN_ZERO_ARRAY(ARRAY_2D)

      INCLUDE 'mpif.h'
      REAL ARRAY_2D(LCM,KCM)

      IF(NPROCS.GE.2)THEN
        DO NP=1,NPROCS-1
         IF(MYRANK==NP) THEN
           CALL MPI_ISEND( ARRAY_2D(DISPLS(NP),1),1,INEWTYPE1(NP),0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
           CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
         ELSEIF(MYRANK==0) THEN
           CALL MPI_IRECV( ARRAY_2D(DISPLS(NP),1),1,INEWTYPE1(NP),NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
           CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
         ENDIF
        ENDDO
      ENDIF
      END SUBROUTINE

!###########################################################################################################

      SUBROUTINE COLLECT_IN_ZERO_ARRAY_KBM(ARRAY_2D)

      INCLUDE 'mpif.h'
      REAL ARRAY_2D(LCM,KBM)

      IF(NPROCS.GE.2)THEN
        DO NP=1,NPROCS-1
         IF(MYRANK==NP) THEN
           CALL MPI_ISEND( ARRAY_2D(DISPLS(NP),1),1,INEWTYPE2(NP),0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
           CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
         ELSEIF(MYRANK==0) THEN
           CALL MPI_IRECV( ARRAY_2D(DISPLS(NP),1),1,INEWTYPE2(NP),NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
           CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
         ENDIF
        ENDDO
      ENDIF
      END SUBROUTINE

!###########################################################################################################

      SUBROUTINE COLLECT_IN_ZERO_ARRAY_0KCM(ARRAY_2D)

        INCLUDE 'mpif.h'
        REAL ARRAY_2D(LCM,0:KCM)

        IF(NPROCS.GE.2)THEN
          DO NP=1,NPROCS-1
           IF(MYRANK==NP) THEN
             CALL MPI_ISEND( ARRAY_2D(DISPLS(NP),0),1,INEWTYPE2(NP),0,87,MPI_COMM_WORLD,IREQ(NP),IERR)
             CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
           ELSEIF(MYRANK==0) THEN
             CALL MPI_IRECV( ARRAY_2D(DISPLS(NP),0),1,INEWTYPE2(NP),NP,87,MPI_COMM_WORLD,IREQ(NP),IERR)
             CALL MPI_WAIT(IREQ(NP),STATUS1,IERR)
           ENDIF
          ENDDO
        ENDIF
        END SUBROUTINE

!###########################################################################################################

      REAL*8 FUNCTION MPI_TIC()

      INCLUDE 'mpif.h'

      MPI_TIC=MPI_WTIME()

      END FUNCTION

!###########################################################################################################

      REAL*8 FUNCTION MPI_TOC(TMPTIME)

      INCLUDE 'mpif.h'
      REAL*8 TMPTIME

      MPI_TOC=MPI_WTIME()-TMPTIME

      END FUNCTION

!###########################################################################################################

      SUBROUTINE MPI_WTIME_PRINT(WT_CHAR,WT_RATIO,WT_VAL,WT_COUNT)

      CHARACTER(LEN=*)  WT_CHAR
      INTEGER           WT_VAL, WT_COUNT, WT_RATIO

      PRINT*,TRIM(WT_CHAR)
      DO II=1,WT_COUNT
         WRITE(WT_NUM,'(I3.3)') II
         MPI_HOSTSPOTS(WT_VAL+ii)='  '//TRIM(WT_CHAR)//'_LOOP_'//WT_NUM
         IF(REAL(MPI_WTIMES(WT_VAL+II)).GE.0.002)THEN
            WRITE(*,'(I5,A20,F10.3)') WT_VAL+II, MPI_HOSTSPOTS(WT_VAL+II),  &
                                      WT_RATIO*REAL(MPI_WTIMES(WT_VAL+II))
         ENDIF
      ENDDO
      WRITE(*,'(A20,F10.3)')  '       '//TRIM(WT_CHAR)//'_TOTAL',   &
      WT_RATIO*REAL(SUM(MPI_WTIMES((WT_VAL+1):(WT_VAL+WT_COUNT))))

      END SUBROUTINE

!###########################################################################################################

      LOGICAL FUNCTION ISDOMAIN(LDOMAIN)

      INTEGER LDOMAIN

      IF(LDOMAIN.GE.LMPI1.AND.LDOMAIN.LE.LMPILA)THEN
         ISDOMAIN=.TRUE.
      ELSE
         ISDOMAIN=.FALSE.
      ENDIF

      END FUNCTION

!###########################################################################################################

      SUBROUTINE ISINPUTS(IS_PSER,IS_CSER,IS_QSER,IS_QCTL)

      LOGICAL IS_PSER(10000),IS_CSER(10000,1000),IS_QSER(10000),IS_QCTL(10000)

      IS_PSER=.TRUE. !.FALSE.
      IS_CSER=.TRUE. !.FALSE.
      IS_QSER=.TRUE. !.FALSE.
      IS_QCTL=.TRUE. !.FALSE.

      IF(.FALSE.)THEN   ! NOT USED
      IF(NPSER.GT.0)THEN
!!    CARD C18
      DO II=1,NPBS
         IF(ISDOMAIN(LIJ(IPBS(II),JPBS(II)))) IS_PSER(NPSERS(II))=.TRUE.
      ENDDO
!!    CARD C19
      DO II=1,NPBW
         IF(ISDOMAIN(LIJ(IPBW(II),JPBW(II)))) IS_PSER(NPSERW(II))=.TRUE.
      ENDDO
!!    CARD C20
      DO II=1,NPBE
         IF(ISDOMAIN(LIJ(IPBE(II),JPBE(II)))) IS_PSER(NPSERE(II))=.TRUE.
      ENDDO
!!    CARD C21
      DO II=1,NPBN
         IF(ISDOMAIN(LIJ(IPBN(II),JPBN(II)))) IS_PSER(NPSERN(II))=.TRUE.
      ENDDO
      ENDIF

!!    CARD C24
      IF(NQSIJ.GT.0)THEN
      DO II=1,NQSIJ
         IF(ISDOMAIN(LIJ(IQS(II),JQS(II))))THEN
            IS_QSER(NQSERQ(II))=.TRUE.
            DO JJ=1,4
              IS_CSER(NCSERQ(II,JJ),JJ)=.TRUE.
            ENDDO
            DO N=1,NTOX
              JJ=MSVTOX(N)
              IS_CSER(NCSERQ(II,JJ),JJ)=.TRUE.
            ENDDO
            DO N=1,NSED
              JJ=MSVSED(N)
              IS_CSER(NCSERQ(II,JJ),JJ)=.TRUE.
            ENDDO
            DO N=1,NSND
              JJ=MSVSND(N)
              IS_CSER(NCSERQ(II,JJ),JJ)=.TRUE.
            ENDDO
            DO NW=1,NWQV
              JJ=4+NTOX+NSED+NSND+NW
              IS_CSER(:,JJ)=.TRUE.
            ENDDO
            DO NSP=1,NXSP
              JJ=4+NTOX+NSED+NSND+NWQV+NSP
              IS_CSER(:,JJ)=.TRUE.
            ENDDO
         ENDIF
      ENDDO
      ENDIF

!!    CARD C32
      IF(NQCTL.GT.0)THEN
      DO II=1,NQCTL
         IF(ISDOMAIN(LIJ(IQCTLU(II),JQCTLU(II))))THEN
            IS_QCTL(NQCTLQ(II))=.TRUE.
         ENDIF
      ENDDO
      ENDIF

!!    CARD C47
      IF(NCBS.GT.0)THEN
      DO II=1,NCBS
        IF(ISDOMAIN(LIJ(ICBS(II),JCBS(II))))THEN
           DO JJ=1,4
             IS_CSER(NCSERS(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NTOX
             JJ=MSVTOX(N)
             IS_CSER(NCSERS(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSED
             JJ=MSVSED(N)
             IS_CSER(NCSERS(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSND
             JJ=MSVSND(N)
             IS_CSER(NCSERS(II,2),JJ)=.TRUE.
           ENDDO
           DO NW=1,NWQV
             JJ=4+NTOX+NSED+NSND+NW
             IS_CSER(NCSERS(II,JJ),JJ)=.TRUE.
           ENDDO
           DO NSP=1,NXSP
             JJ=4+NTOX+NSED+NSND+NWQV+NSP
             IS_CSER(NCSERS(II,JJ),JJ)=.TRUE.
           ENDDO
        ENDIF
      ENDDO
      ENDIF

!!    CARD C52
      IF(NCBW.GT.0)THEN
      DO II=1,NCBW
        IF(ISDOMAIN(LIJ(ICBW(II),JCBW(II))))THEN
           DO JJ=1,4
             IS_CSER(NCSERW(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NTOX
             JJ=MSVTOX(N)
             IS_CSER(NCSERW(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSED
             JJ=MSVSED(N)
             IS_CSER(NCSERW(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSND
             JJ=MSVSND(N)
             IS_CSER(NCSERW(II,2),JJ)=.TRUE.
           ENDDO
           DO NW=1,NWQV
             JJ=4+NTOX+NSED+NSND+NW
             IS_CSER(NCSERW(II,JJ),JJ)=.TRUE.
           ENDDO
           DO NSP=1,NXSP
             JJ=4+NTOX+NSED+NSND+NW+NWQV+NSP
             IS_CSER(NCSERW(II,JJ),JJ)=.TRUE.
           ENDDO
        ENDIF
      ENDDO
      ENDIF

!!    CARD C57
      IF(NCBE.GT.0)THEN
      DO II=1,NCBE
        IF(ISDOMAIN(LIJ(ICBE(II),JCBE(II))))THEN
           DO JJ=1,4
             IS_CSER(NCSERE(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NTOX
             JJ=MSVTOX(N)
             IS_CSER(NCSERE(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSED
             JJ=MSVSED(N)
             IS_CSER(NCSERE(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSND
             JJ=MSVSND(N)
             IS_CSER(NCSERE(II,2),JJ)=.TRUE.
           ENDDO
           DO NW=1,NWQV
             JJ=4+NTOX+NSED+NSND+NW
             IS_CSER(NCSERE(II,JJ),JJ)=.TRUE.
           ENDDO
           DO NSP=1,NXSP
             JJ=4+NTOX+NSED+NSND+NW+NWQV+NSP
             IS_CSER(NCSERE(II,JJ),JJ)=.TRUE.
           ENDDO
        ENDIF
      ENDDO
      ENDIF

!!    CARD C62
      IF(NCBN.GT.0)THEN
      DO II=1,NCBN
        IF(ISDOMAIN(LIJ(ICBN(II),JCBN(II))))THEN
           DO JJ=1,4
             IS_CSER(NCSERN(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NTOX
             JJ=MSVTOX(N)
             IS_CSER(NCSERN(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSED
             JJ=MSVSED(N)
             IS_CSER(NCSERN(II,2),JJ)=.TRUE.
           ENDDO
           DO N=1,NSND
             JJ=MSVSND(N)
             IS_CSER(NCSERN(II,2),JJ)=.TRUE.
           ENDDO
           DO NW=1,NWQV
             JJ=4+NTOX+NSED+NSND+NW
             IS_CSER(NCSERN(II,JJ),JJ)=.TRUE.
           ENDDO
           DO NSP=1,NXSP
             JJ=4+NTOX+NSED+NSND+NW+NWQV+NSP
             IS_CSER(NCSERN(II,JJ),JJ)=.TRUE.
           ENDDO
        ENDIF
      ENDDO
      ENDIF
      ENDIF

      ENDSUBROUTINE

!###########################################################################################################

      SUBROUTINE MPI_MASKDRY

      MPI_IMASKDRY=0
      DO L=1,LA
         IF(IMASKDRY(L).EQ.1) MPI_IMASKDRY(L)=1.
      ENDDO

      ENDSUBROUTINE


END MODULE
