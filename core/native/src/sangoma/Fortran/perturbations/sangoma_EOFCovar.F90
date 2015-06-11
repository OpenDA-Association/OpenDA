! Copyright (c) 2004-2013 Lars Nerger, lars.nerger@awi.de
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! PDAF is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!$Id: sangoma_EOFCovar.F90 523 2014-11-25 08:47:15Z paulki $
!BOP
!
! !ROUTINE: sangoma_EOFCovar --- Perform EOF decomposition of state trajectory
!
! !INTERFACE:
SUBROUTINE sangoma_eofcovar(dim_state, nstates, nfields, dim_fields, offsets, &
     remove_mstate, do_mv, states, stddev, svals, svec, meanstate, status) &
     BIND(C, name="sangoma_eofcovar_")

! !DESCRIPTION:
! This routine performs an EOF analysis by singular value decomposition. It is
! used to prepare a covariance matrix for initializing an ensemble.  For
! the decomposition a multivariate scaling can be performed by 
! 'sangoma\_MVNormalize' to ensure that all fields in the state vectors have
! unit variance. 
!
! To use this routine, one has to initialize the array 'states' holding in
! each column a perturbation vector (state - mean) from a state trajectory. 
! Outputs are the arrays of singular values (svals) and left singular vectors
! (svec). The singular values are scaled by sqrt(1/(nstates-1)). With this,
! $svec * svals^2 * svec^T$ is the covariance matrix. In addition, the standard
! deviation of the field variations (stddev) is an output array.
! To use the multivariate normalization one has to define the number of
! different fields in the state (nfields), the dimension of each fields and
! the offset of field from the start of each state vector.
!
! The routine uses the LAPACK routine 'dgesvd' to compute the singular value
! decomposition.
!
! !REVISION HISTORY:
! 2012-09 - Lars Nerger - Initial code for SANGOMA based on PDAF
! 2013-11 - L. Nerger - Adaption to SANGOMA data model
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim_state           ! Dimension of state vector
  INTEGER(INTPREC), INTENT(in) :: nstates             ! Number of state vectors
  INTEGER(INTPREC), INTENT(in) :: nfields             ! Number of fields in state vector
  INTEGER(INTPREC), INTENT(in) :: dim_fields(nfields) ! Size of each field
  INTEGER(INTPREC), INTENT(in) :: offsets(nfields)    ! Start position of each field
  INTEGER(INTPREC), INTENT(in) :: do_mv               ! 1: Do multivariate scaling
     ! nfields, dim_fields and offsets are only used if do_mv=1
  INTEGER(INTPREC), INTENT(in) :: remove_mstate       ! 1: subtract mean state from states
     ! before computing EOFs
  REAL(REALPREC), INTENT(inout)  :: states(dim_state, nstates) ! State perturbations
  REAL(REALPREC), INTENT(out) :: stddev(nfields)      ! Standard deviation of field variability
     ! Without multivariate scaling (do_mv=0), it is stddev = 1.0
  REAL(REALPREC), INTENT(out) :: svals(nstates)       ! Singular values divided by sqrt(nstates-1)
  REAL(REALPREC), INTENT(out) :: svec(dim_state, nstates)   ! Singular vectors
  REAL(REALPREC), INTENT(inout) :: meanstate(dim_state)     ! Mean state (only changed if remove_mstate=1)
  INTEGER(INTPREC), INTENT(out) :: status             ! Status flag
!EOP


! *** local variables ***
  INTEGER :: i                      ! Counter
  INTEGER :: stat                   ! internal status flag
  INTEGER :: ldwork                 ! variable for SVD routine 
  REAL, ALLOCATABLE :: work(:)      ! work array for SVD
  REAL :: svdV                      ! right singular values (not referenced)



  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*            sangoma_EOFCovar             *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*  Compute EOF decomposition of a matrix  *'
  WRITE (*,'(10x,a)') '*******************************************'

! *************************
! *** Remove mean state ***
! *************************

  removemean: IF (remove_mstate == 1) THEN

     WRITE (*,'(/1x,a)') 'EOFCOVAR: Compute and subtract mean state ---------------------------'

     ! *** compute mean state ***
     meanstate = 0.0
     DO i = 1, nstates
        meanstate(:) = meanstate(:) + states(:, i) / REAL(nstates)
     END DO

     ! *** get peturbation matrix ***
     DO i = 1, nstates
        states(:,i) = states(:,i) - meanstate(:)
     END DO

  END IF removemean


! ******************************************
! *** Perform multivariate normalization ***
! ******************************************

  stat = 0

  multivar: IF (do_mv == 1) THEN

     WRITE (*,'(/1x,a)') 'EOFCOVAR: Perform multivariate normalization ------------------------'

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
! ***                                                   ***
! *** The covariance matrix is given by the state       ***
! *** sequences X of k states as                        ***
! ***          -1    _     _ T        T                 ***
! *** P = (k-1)   (X-X) (X-X)  = U L U      (EVP)       ***
! ***                                                   ***
! *** We compute the singular value decomposition       ***
! ***     _        T            -1    2  T              ***
! ***   X-X = U S V ;  P = (k-1)   U S  U               ***
! ***                                                   ***
! ***                         -1/2                      ***
! *** and we return U and (k-1)    S.                   ***
! *********************************************************

  IF (stat==0) THEN

     WRITE (*,'(/1x,a)') 'EOFCOVAR: Compute SVD -----------------------------------------------'

     ! Allocate work array and work space size
     ALLOCATE(work(MAX(3 * MIN(dim_state, nstates) + &
          MAX(dim_state, nstates), 5 * MIN(dim_state, nstates))))
     ldwork = MAX(3 * MIN(dim_state, nstates) + &
          MAX(dim_state, nstates), 5 * MIN(dim_state, nstates))
    
     ! Do decomposition
     CALL dgesvd('s', 'n', dim_state, nstates, states, &
          dim_state, svals, svec, dim_state, svdV, &
          dim_state, work, ldwork, status)
  
     ! *** scale singular values ***
     DO i = 1, nstates
        svals(i) = svals(i) / SQRT(REAL(nstates - 1))
     END DO

     stat = stat+status
  END IF

! ********************************
! *** Rescale singular vectors ***
! ********************************

  do_rescale: IF (do_mv == 1 .AND. stat == 0) THEN

     WRITE (*,'(/1x,a)') 'EOFCOVAR: Re-scale singular vectors according to stddev -------------'

     DO i = 1, nfields
        CALL sangoma_MVNormalize(2, dim_state, dim_fields(i), offsets(i), nstates-1, &
             svec, stddev(i), status)
     END DO

     stat = stat+status

  END IF do_rescale


! *********************************************
! *** Add mean state if it has been removed ***
! *********************************************

  addmean: IF (remove_mstate == 1) THEN

     WRITE (*,'(/1x,a)') 'EOFCOVAR: Add mean state --------------------------------------------'

     ! *** get residual matrix ***
     DO i = 1, nstates
        states(:,i) = states(:,i) + meanstate(:)
     END DO

  END IF addmean


! ****************
! *** Clean up ***
! ****************

  DEALLOCATE(work)

  status = stat

END SUBROUTINE sangoma_eofcovar
