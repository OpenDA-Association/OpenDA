      SUBROUTINE CALEBI_mpi  
C  
C CHANGE RECORD  
C **  CALEBI CALCULATES THE EXTERNAL BUOYANCY INTEGRALS  
C  
      USE GLOBAL  
      USE MPI
      IMPLICIT NONE
      INTEGER::K,L,LLCM
      REAL::DBK,DZCBK

      REAL*4 DZCB(KCM)
      REAL*4 BK(KCM)

      PARAMETER(LLCM=200)

      IF(.FALSE.)THEN
      S2TIME=MPI_TIC()
!$OMP PARALLEL DO PRIVATE(DBK,DZCBK)
      DO L=LMPI2,LMPILA 

        BI1(L)=0.
        BI2(L)=0.
        BE(L)=0.
        
        DO K=1,KC
          DZCB(K)=DZC(K)*B(L,K)
        ENDDO

        DBK=0.  
        DO K=KC,1,-1
          DBK=DBK+DZCB(K)         !DZC(K)*B(L,K)
          BK(K)=DBK-0.5*DZCB(K)   !DZC(K)*B(L,K)
        ENDDO

        !Z(0)=0.
        !Z(K)=Z(K-1)+DZC(K)
        DO K=1,KC
          BE(L) =BE(L)+DZCB(K)   !DZC(K)*B(L,K)
          DZCBK =DZC(K)*BK(K)
          BI1(L)=BI1(L)+DZCBK  
          BI2(L)=BI2(L)+(DZCBK+0.5*(Z(K)+Z(K-1))*DZCB(K)) 
        ENDDO

      ENDDO
      MPI_WTIMES(251)=MPI_WTIMES(251)+MPI_TOC(S2TIME)
      
      ELSE
      S2TIME=MPI_TIC()
!$OMP PARALLEL DO
      DO L=LMPI2,LMPILA 
        BI1(L)=0.
        BI2(L)=0.
        BE(L)=0.
      ENDDO
      MPI_WTIMES(251)=MPI_WTIMES(251)+MPI_TOC(S2TIME)
        
      S2TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          DZCB_2D(L,K)=DZC(K)*B(L,K)
        ENDDO
      ENDDO
      MPI_WTIMES(252)=MPI_WTIMES(252)+MPI_TOC(S2TIME)

      S2TIME=MPI_TIC()
      DBK_1D=0.
      DO K=KC,1,-1
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          DBK_1D(L)=DBK_1D(L)+DZCB_2D(L,K)
          BK_2D(L,K)=DBK_1D(L)-0.5*DZCB_2D(L,K)
        ENDDO
      ENDDO
      MPI_WTIMES(253)=MPI_WTIMES(253)+MPI_TOC(S2TIME)

      S2TIME=MPI_TIC()
      DO K=1,KC
!$OMP PARALLEL DO
        DO L=LMPI2,LMPILA
          BE(L) =BE(L)+DZCB_2D(L,K)
          BI1(L)=BI1(L)+DZC(K)*BK_2D(L,K)  
          BI2(L)=BI2(L)+(DZC(K)*BK_2D(L,K)+
     &           0.5*(Z(K)+Z(K-1))*DZCB_2D(L,K)) 
        ENDDO
      ENDDO
      MPI_WTIMES(254)=MPI_WTIMES(254)+MPI_TOC(S2TIME)
      ENDIF

      CALL broadcast_boundary(BE,ic)
      CALL broadcast_boundary(BI1,ic)
      CALL broadcast_boundary(BI2,ic)

      RETURN  
      END  

