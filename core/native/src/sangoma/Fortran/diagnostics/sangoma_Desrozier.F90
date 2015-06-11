!=====================================================================

  SUBROUTINE sangoma_Desrozier(dim_obs,dim_ens,innovation, residual,&
                     increment, typeOut,out1,out2,out3,out4) &
                     BIND(C, name="sangoma_desrozier_")
!
!=====================================================================
!
!   Computes left side of Desriozier statistics given in
!    Desroziers et al (2005)
!
!   The following diagnostics are considered: 
!   E[d^of (d^of)^T] = R + HP^fH^T 
!   E[d^af (d^of)^T] = HP^fH^T
!   E[d^oa d^of]     = R
!   E[d^af d^oa]     = HP^aH^T
!
!
!    Input:  dim_obs             Observation dimension
!            innovation          Innovation vector y-Hx^f
!            residual            Residual          y-Hx^a
!            increment           Hx^f-Hx^a
!            typeOut             Integer array(4) (1 compute; 0 do not compute)
!                                E[d^of (d^of)^T] = R + HP^fH^T
!                                E[d^af (d^of)^T] = HP^fH^T
!                                E[d^oa d^of] = R
!                                E[d^af d^oa] = HP^aH^T
 
!
!    Output:  Vector of diagonals ...
!             out1
!             out2
!             out3
!             out4
!
!
!
!=====================================================================
!   
      USE, INTRINSIC :: ISO_C_BINDING
      USE sangoma_base, ONLY: REALPREC, INTPREC
      IMPLICIT NONE 

!     ARGUMENTS
      INTEGER(INTPREC), INTENT(in)    :: dim_obs
      INTEGER(INTPREC), INTENT(in)    :: dim_ens
      REAL(REALPREC), INTENT(in)      :: innovation(dim_obs,dim_ens)
      REAL(REALPREC), INTENT(in)   :: residual(dim_obs,dim_ens) 
      REAL(REALPREC), INTENT(in)   :: increment(dim_obs,dim_ens)
      INTEGER(INTPREC),   INTENT(in)   :: typeOut(4)
      REAL(REALPREC), INTENT(out)   :: out1(dim_obs)
      REAL(REALPREC), INTENT(out)   :: out2(dim_obs)
      REAL(REALPREC), INTENT(out)   :: out3(dim_obs)
      REAL(REALPREC), INTENT(out)   :: out4(dim_obs)

!     LOCAL VARIABLES
      INTEGER(INTPREC) :: i 
      REAL(REALPREC)   :: m1(dim_obs,dim_obs)
      REAL(REALPREC)   :: m2(dim_obs,dim_obs)
      REAL(REALPREC)   :: m3(dim_obs,dim_obs)
      REAL(REALPREC)   :: m4(dim_obs,dim_obs)

     
      out1 = 0
      out2 = 0
      out3 = 0
      out4 = 0


     IF( typeOut(1) == 1) THEN
          CALL dgemm('N','T',dim_obs,dim_obs,dim_ens,real(1./real(dim_ens)),&
                              innovation,dim_obs,innovation, &
                              dim_obs,0.0,m1,dim_obs)
     ENDIF 
     IF ( typeOut(2) == 1) THEN
          CALL dgemm('N','T',dim_obs,dim_obs,dim_ens,real(1./real(dim_ens)),&
                             increment,dim_obs,innovation,dim_obs,0.0,&
                             m2,dim_obs)
     ENDIF 
     IF ( typeOut(3) == 1 ) THEN
          CALL dgemm('N','T',dim_obs,dim_obs,dim_ens,real(1./real(dim_ens)),&
                             residual,dim_obs,innovation,dim_obs,0.0,&
                             m3,dim_obs)
     ENDIF

     IF ( typeOut(4) == 1) THEN
          CALL dgemm('N','T',dim_obs,dim_obs,dim_ens,real(1./real(dim_ens)),&
                             increment,dim_obs,residual,dim_obs,0.0,&
                             m4,dim_obs)
     ENDIF

      DO i = 1, dim_obs
      ! Compute diagonals of the outputs
      IF ( typeOut(1) == 1) THEN 
             out1(i) = m1(i,i)
       ENDIF
       IF (typeOut(2) == 1) THEN
             out2(i) = m2(i,i)
       ENDIF
       IF (typeOut(3) == 1) THEN
             out3(i) = m3(i,i)
        ENDIF
        IF (typeOut(4) == 1) THEN
             out4(i) = m4(i,i)
        ENDIF
      ENDDO
     END subroutine sangoma_Desrozier 
