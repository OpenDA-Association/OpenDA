!=====================================================================

      subroutine sangoma_CompareDes(dim_obs,dim_ens,out1,out2,out3,out4,typeOut,&
                                   Hens, CB_diag_R,   & 
                                   diff1,diff2,diff3,diff4, m_diff, &
                                   quot1,quot2,quot3,quot4, m_quot,&
                                   flag ) &
                                   BIND(C, name="sangoma_comparedes_")
!
!=====================================================================
!
!
!
!   HERE description
!
!
!    Input:  dim_obs       Observation dimension
!            dim_ens       Ensemble dimension
!            out1          E[d^of (d^of)^T]
!            out2          E[d^af (d^of)^T]
!            out3          E[d^oa d^of]
!            out4          E[d^af d^oa]
!            typeOut       Integer array of length 4 which indicates what inputs are present
!            Hens          Projection of ensemble to observation space
!            whichout      Which output is needed: 
!                          (1) Only mean differences
!                          (2) Differences and mean differences
!                          (3) Mean quotioent of left and right side
!                          (4) Quotient of E[...]/(Right side) and mean quotient
!
! 
!
!    Output:  Vector of diagonals ...
!             Yet to be done
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
      INTEGER(INTPREC),   INTENT(inout)   :: typeOut(4)
      REAL(REALPREC), INTENT(inout)   :: out1(dim_obs)
      REAL(REALPREC), INTENT(inout)   :: out2(dim_obs)
      REAL(REALPREC), INTENT(inout)   :: out3(dim_obs)
      REAL(REALPREC), INTENT(inout)   :: out4(dim_obs)
      REAL(REALPREC), INTENT(in)   :: Hens(dim_obs,dim_ens)      
 
 
     ! Call-back routine provinding diagonal elements of R
     INTERFACE
        SUBROUTINE CB_diag_R(step, dim_obs, diag_R) BIND(C)
           USE sangoma_base, ONLY: REALPREC, INTPREC
           INTEGER(INTPREC),INTENT(in) :: step             ! Time step
           INTEGER(INTPREC),INTENT(in) :: dim_obs          ! State dimension
           REAL(REALPREC)  ,INTENT(out) :: diag_R(dim_obs)  ! Number of observations
        END SUBROUTINE
     END INTERFACE


!    Outputs:
     REAL(REALPREC) , INTENT(out) :: m_diff(4)
     REAL(REALPREC) , INTENT(out) :: m_quot(4)

     REAL(REALPREC) , INTENT(out) :: diff1(dim_obs)
     REAL(REALPREC) , INTENT(out) :: diff2(dim_obs)
     REAL(REALPREC) , INTENT(out) :: diff3(dim_obs)
     REAL(REALPREC) , INTENT(out) :: diff4(dim_obs)
     REAL(REALPREC) , INTENT(out) :: quot1(dim_obs)
     REAL(REALPREC) , INTENT(out) :: quot2(dim_obs)
     REAL(REALPREC) , INTENT(out) :: quot3(dim_obs)
     REAL(REALPREC) , INTENT(out) :: quot4(dim_obs)
     INTEGER(INTPREC), INTENT(out) :: flag
!EOP

     !Local variables
     REAL(REALPREC) :: HPHt(dim_obs,dim_obs)
     REAL(REALPREC) :: R(dim_obs)
     INTEGER(INTPREC) :: i

      ! Test for consistency of input 
      IF ( typeOut(1) == 1 .or. typeOut(2) == 2) THEN
         ! If forcast type is "on" do not use analysis type 
         IF (typeOut(4) == 1) THEN
            WRITE(*,*) 'These set of Flags are not allowed'
            flag = 0
            return
         ENDIF
      ENDIF

      IF (sum(typeOut) == 0) THEN
           Write(*,*) 'Error: No valid input flag'
           flag = 0 
           return
      ENDIF


      ! Initilize variables with zero
      m_diff(4) = 0
      diff1 = 0 
      diff2 = 0
      diff3 = 0
      diff4 = 0
      ! Compute state covariance in obs space
      CALL dgemm('N','T',dim_obs,dim_obs,dim_ens,real(1./real(dim_ens)),&
                  Hens,dim_obs,Hens,dim_obs,0.0,HPHt,dim_obs)

      CALL CB_diag_R(0, dim_obs, R)

      ! Compute diagonals of the outputs
      DO i = 1, dim_obs
            IF ( typeOut(1) == 1 ) THEN 
                 diff1(i) = out1(i) - (R(i)+HPHt(i,i))
            ENDIF
            IF (typeOut(2) == 1) THEN
                 diff2(i) = out2(i) - HPHt(i,i)
            ENDIF
            IF (typeOut(3) == 1) THEN
                diff3(i) = out3(i) - R(i)
            ENDIF
            IF (typeOut(4) == 1) THEN
                diff4(i) = out4(i) - HPHt(i,i)
            ENDIF
      ENDDO
      m_diff(1) = sum(diff1)/real(dim_obs)
      m_diff(2) = sum(diff2)/real(dim_obs)
      m_diff(3) = sum(diff3)/real(dim_obs)
      m_diff(4) = sum(diff4)/real(dim_obs)
      DO i = 1, dim_obs
          IF ( typeOut(1) == 1 ) THEN
             quot1(i) = out1(i)/(R(i)+HPHt(i,i))
          ENDIF
          IF (typeOut(2) == 1) THEN
             quot2(i) = out2(i)/HPHt(i,i)
          ENDIF
          IF (typeOut(3) == 1) THEN
             quot3(i) = out3(i)/R(i)
          ENDIF
          IF (typeOut(4) == 1) THEN
             quot4(i) = out4(i)/HPHt(i,i)
          ENDIF
      ENDDO
      m_quot(1) = sum(quot1)/real(dim_obs)
      m_quot(2) = sum(quot2)/real(dim_obs)
      m_quot(3) = sum(quot3)/real(dim_obs)
      m_quot(4) = sum(quot4)/real(dim_obs)

      ! Compute means of differences
      flag =1
END subroutine sangoma_CompareDes 
