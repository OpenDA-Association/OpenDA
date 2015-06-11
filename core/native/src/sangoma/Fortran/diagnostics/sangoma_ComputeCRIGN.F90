

subroutine sangoma_computeCRIGN(dim_ens, fc_ens, obs, crps, crign,CB_SORT) &
           BIND(C, name="sangoma_computecrign_")
! Computes the CRPS according to Hersbach (2000)
! and the CRIGN according to Tödter & Ahrens (MWR, 2012)

      USE, INTRINSIC :: ISO_C_BINDING
      USE sangoma_base, ONLY: REALPREC, INTPREC

implicit none

!Interface
integer(INTPREC), intent(in) :: dim_ens         ! Ensemble size
real(REALPREC), intent(in)    :: fc_ens(dim_ens) ! Ensemble forecast values
real(REALPREC), intent(in)    :: obs             ! Observation to verify the forecast
real(REALPREC), intent(out)   :: crps, crign     ! Scores

  INTERFACE
      SUBROUTINE CB_SORT(NENS, V) BIND(C)
        USE sangoma_base, ONLY: REALPREC, INTPREC
        INTEGER(INTPREC),INTENT(in)  :: NENS    ! Size of vector
        REAL(REALPREC),INTENT(inout) :: V(NENS) ! Input/output vector 
     END SUBROUTINE CB_SORT
  END INTERFACE





!local variables:
real :: epsilon = 0.01             ! Divergence factor for CRIGN (close to zero)
real, dimension(dim_ens+1) :: a,b  ! Interval lengths
real, dimension(dim_ens+1) :: ycum ! CDF values: 0,1/N,2/N,...,1 
real, dimension(dim_ens) :: ens    ! Sorted ensemble
integer :: i                       ! Loop variable

! PREPARATIONS
!-------------

! Prepare the values for the ensemble CDF
do i=1,dim_ens+1
   ycum(i) = real(i-1) / real(dim_ens)
end do

! Sort ensemble values in ascending order using external routine
ens = fc_ens
call CB_SORT(dim_ens,ens)


! INTERVAL LENGTHS
!-----------------

! Compute interval lengths for integration
a = 0.0
b = 0.0

do i=2,dim_ens
  if ( obs > ens(i) ) then
     a(i) = ens(i) - ens(i-1)
  elseif( obs <= ens(i-1) ) then 
     b(i) = ens(i) - ens(i-1)
  elseif( obs > ens(i-1) .and. obs <= ens(i) ) then
     a(i) = obs - ens(i-1)
     b(i) = ens(i) - obs
  end if
end do

if( obs <= ens(1) ) then 
   b(1) = ens(1) - obs
elseif (obs > ens(dim_ens) ) then
   a(dim_ens+1) = obs - ens(dim_ens)
end if

! SCORES 
!-------

! Compute CRPS directly
crps = sum( a*ycum**2 ) + sum( b*(1.0 - ycum)**2 )

! Modify boundary values for CRIGN to avoid divergence if obs is outside of ensemble range
ycum(1) = epsilon
ycum(dim_ens+1) = 1.-epsilon

! Compute CRIGN
crign = - sum( a*log(1.0-ycum) ) - sum( b*log(ycum) )

end subroutine sangoma_computeCRIGN


