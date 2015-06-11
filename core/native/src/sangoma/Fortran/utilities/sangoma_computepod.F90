!
! !ROUTINE: sangoma_computepod --- Perform eigen value decomposition on ensemble of states and 
!                                  returns most dominant POD modes.
!
SUBROUTINE sangoma_computepod(nmodes,dim_state, nstates, nfields, dim_fields, offsets, &
     do_mv, states, stddev, svals, svec, status) &
     BIND(C, name="sangoma_computepod_")

! !DESCRIPTION:
! This routine performs an EOF analysis by singular value decomposition. It is
! used to prepare a dominant eigen vectors and eigenvalues of an ensemble.  
!
! To use this routine, one has to initialize the array 'states' holding in
! each column a perturbation vector (state - mean) from a state trajectory. 
! Outputs are the arrays of singular values (svals) and left singular vectors
! (svec) based on specified energy.


! The routine uses the LAPACK routine 'dgesvd' to compute the singular value
! decomposition.
!
! Adopted from sangoma_EOFCovar 
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in)   :: nmodes            ! Dimension of leading eigen vectors
  INTEGER(INTPREC), INTENT(in) :: dim_state           ! Dimension of state vector
  INTEGER(INTPREC), INTENT(in) :: nstates             ! Number of state vectors
  INTEGER(INTPREC), INTENT(in) :: nfields             ! Number of fields in state vector
  INTEGER(INTPREC), INTENT(in) :: dim_fields(nfields) ! Size of each field
  INTEGER(INTPREC), INTENT(in) :: offsets(nfields)    ! Start position of each field
  INTEGER(INTPREC), INTENT(in) :: do_mv               ! 1: Do multivariate scaling
     ! nfields, dim_fields and offsets are only used if do_mv=1
  REAL(REALPREC), INTENT(in)  :: states(dim_state, nstates) ! State perturbations
  REAL(REALPREC), INTENT(out) :: stddev(nfields)      ! Standard deviation of field variability
     ! Without multivariate scaling (do_mv=0), it is stddev = 1.0
  REAL(REALPREC), INTENT(out) :: svals(nmodes)       ! Eigen values
  REAL(REALPREC), INTENT(out) :: svec(dim_state, nmodes)   ! Eigen Vectors
  INTEGER(INTPREC), INTENT(out) :: status             ! Status flag
!EOP


! *** local variables ***
  INTEGER :: i                      ! Counter
  INTEGER :: stat                   ! internal status flag
  INTEGER :: ldwork                 ! variable for SVD routine 
  REAL, ALLOCATABLE :: work(:)      ! work array for SVD
  REAL :: svdV                      ! right singular values (not referenced)
  REAL :: svals_loc(nstates)       ! Singular values divided by sqrt(nstates-1)
  REAL :: svec_loc(dim_state, nstates)   ! Singular vectors





! ******************************************
! *** Perform multivariate normalization ***
! ******************************************

  stat = 0

  multivar: IF (do_mv == 1) THEN

     WRITE (*,'(/1x,a)') 'COMPUTEPOD: Perform multivariate normalization ------------------------'

     DO i = 1, nfields
        CALL sangoma_MVNormalize(1, dim_state, dim_fields(i), offsets(i), nstates, &
             states, stddev(i), status)

        WRITE (*,'(5x,a,i5,a,es10.4)') 'Field', i, ': standard deviation ', stddev(i)

        stat = stat + status
     END DO

  ELSE
     stddev = 1.0
  END IF multivar


! *********************************************************
! *** Singular value decomposition of covariance matrix ***
! *** and return U with nmodes and  S.                  ***
! *********************************************************

  IF (stat==0) THEN

     WRITE (*,'(/1x,a)') 'COMPUTEPOD: Compute eigen values and eigen vectors--------------'

     ! Allocate work array and work space size
     ALLOCATE(work(MAX(3 * MIN(dim_state, nstates) + &
          MAX(dim_state, nstates), 5 * MIN(dim_state, nstates))))
     ldwork = MAX(3 * MIN(dim_state, nstates) + &
          MAX(dim_state, nstates), 5 * MIN(dim_state, nstates))
    

! Do decomposition
     CALL dgesvd('s', 'n', dim_state, nstates, states, &
          dim_state, svals_loc, svec_loc, dim_state, svdV, &
          dim_state, work, ldwork, status)
  

  
   
    ! stat = stat+1
  END IF


! ********************************
! *** Rescale singular vectors ***
! ********************************

  do_rescale: IF (do_mv == 1 .AND. stat == 0) THEN

     WRITE (*,'(/1x,a)') 'COMPUTEPOD: Re-scale singular vectors according to stddev -------------'

     DO i = 1, nfields
        CALL sangoma_MVNormalize(2, dim_state, dim_fields(i), offsets(i), nmodes-1, &
             svec, stddev(i), status)
     END DO

     stat = stat+1

  END IF do_rescale


! *********************************************************
! *** Select Dominant Singular values and POD           ***
! *** Modes.                                            ***
! *********************************************************

    
    WRITE (*,'(/1x,a)') 'COMPUTEPOD: Select dominant eigen values and eigen vectors -------------'

    DO i = 1, nmodes
        svec(:,i) = svec_loc(:,i)
        svals(i) = svals_loc(i)
     END DO



! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(work)
 
  status = stat

END SUBROUTINE sangoma_computepod
