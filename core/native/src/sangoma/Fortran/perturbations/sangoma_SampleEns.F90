! Copyright (c) 2004-2014 Lars Nerger, lars.nerger@awi.de
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
!$Id: sangoma_SampleEns.F90 1253 2012-01-30 18:56:08Z lnerger $
!BOP
!
! !ROUTINE: sangoma_SampleEns --- Sample an ensemble from EOF modes
!
! !INTERFACE:
SUBROUTINE sangoma_SampleEns(dim, dim_ens, modes, svals, state, &
     ens, flag) &
     BIND(C, name="sangoma_sampleens_")

! !DESCRIPTION:
! This routine generates an ensemble of model states from a provided
! mean state and EOF modes (singular vectors of a peturbation matrix)
! and singular values. The resulting ensemble is the 2nd order-exact
! sample covariance matrix and mean state. 
! the ensemble state vectors are computed as
!   $ens_i = state + sqrt(dim_ens-1) modes (\Omega C)^T$
! where $C$ holds in its diagonal the singular values ($svals$). $\Omega$
! is an orthogonal transformation matrix that preserves the mean state.
! The generated ensemble fulfills the condition for the state error
! covariance matrix
!   $P = 1/(sqrt(dim_ens-1)  \sum_{i=1}^{dim\_ens} (ens_i - state)(ens_i - state)^T$
!
! !REVISION HISTORY:
! 2014-05 - Lars Nerger - Initial code based on PDAF example code.
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER, INTENT(in) :: dim                   ! Size of state vector
  INTEGER, INTENT(in) :: dim_ens               ! Size of ensemble
  REAL, INTENT(inout) :: modes(dim, dim_ens-1) ! Array of EOF modes
  REAL, INTENT(in)    :: svals(dim_ens-1)      ! Vector of singular values
  REAL, INTENT(inout) :: state(dim)            ! PE-local model state
  REAL, INTENT(out)   :: ens(dim, dim_ens)     ! State ensemble
  INTEGER, INTENT(inout) :: flag               ! Status flag
!EOP

! *** local variables ***
  INTEGER :: i, row, col              ! counters
  REAL, ALLOCATABLE :: omega(:,:)     ! Transformation matrix Omega
  REAL :: fac         ! Square-root of dim_ens-1 or dim_ens


! **********************
! *** INITIALIZATION ***
! **********************
  
  WRITE (*,'(10x,a)') '******************************************************'
  WRITE (*,'(10x,a)') '*                sangoma_SampleEns                   *'
  WRITE (*,'(10x,a)') '*                                                    *'
  WRITE (*,'(10x,a)') '*  Sample an ensemble with 2nd-order exact sampling  *'
  WRITE (*,'(10x,a)') '******************************************************'

  ! *** Generate full ensemble on filter-PE 0 ***
  WRITE (*, '(/9x, a)') 'Sample state ensemble from covariance matrix'
  WRITE (*, '(9x, a)') 'given as EOF vectors and singular values'
  WRITE (*, '(9x, a, i5)') '--- Ensemble size:  ', dim_ens
  WRITE (*, '(9x, a, i5)') '--- number of EOFs: ', dim_ens-1

  ! allocate memory for temporary fields
  ALLOCATE(omega(dim_ens, dim_ens-1))


! ********************************************************
! *** Generate ensemble by transformation of EOF modes ***
! ********************************************************

  ! *** Generate uniform orthogonal matrix OMEGA ***
  CALL PDAF_seik_omega(dim_ens-1, Omega, 1, 1)

  ! ***      Generate ensemble of states                  ***
  ! *** ens_i = state + sqrt(dim_ens-1) modes (Omega C)^T ***

  ! A = Omega C
  DO col = 1, dim_ens-1
     DO row = 1, dim_ens
        Omega(row, col) = Omega(row, col) * svals(col)
     END DO
  END DO
      
 ! ens = state + sqrt(dim_ens-1) modes A^T
  DO col = 1, dim_ens
     ens(:, col) = state(:)
  END DO

  fac = SQRT(REAL(dim_ens-1))

  CALL DGEMM('n', 't', dim, dim_ens, dim_ens-1, &
       fac, modes, dim, Omega, dim_ens, &
       1.0, ens, dim)


! ****************
! *** clean up ***
! ****************

  DEALLOCATE(omega)

END SUBROUTINE sangoma_SampleEns
!BOP
!
! !ROUTINE: PDAF_seik_omega - Generate random matrix with special properties
!
! !INTERFACE:
SUBROUTINE PDAF_seik_omega(rank, omega, omegatype, screen)

! !DESCRIPTION:
! Generate a transformation matrix OMEGA for
! the generation and transformation of the 
! ensemble in the SEIK and LSEIK filter.
! Generated is a uniform orthogonal matrix OMEGA
! with R columns orthonormal in $R^{r+1}$
! and orthogonal to (1,...,1)' by iteratively 
! applying the Householder matrix onto random 
! vectors distributed uniformly on the unit sphere.
!
! This version initializes at each iteration step
! the whole Householder matrix and subsequently
! computes Omega using GEMM from BLAS. All fields are 
! allocated once at their maximum required size.
! (On SGI O2K this is about a factor of 2.5 faster
! than the version applying BLAS DDOT, but requires
! more memory.)
!
! For omegatype=0 a deterministic omega is computed
! where the Housholder matrix of (1,...,1)' is operated
! on an identity matrix.
!
! !  This is a core routine of PDAF and 
!    should not be changed by the user   !
!
! !REVISION HISTORY:
! 2002-01 - Lars Nerger - Initial code
! 2014-05 - Lars Nerger - simplified version for SANGOMA
!
! !USES:
  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER, INTENT(in) :: rank      ! Approximated rank of covar matrix
  REAL, INTENT(inout) :: omega(rank+1, rank) ! Matrix Omega
  INTEGER, INTENT(in) :: omegatype ! Select type of omega:
                                   !   (1) generated from random vectors
                                   !   (0) generated from deterministic vectors
                                   ! (other) product of matrix from (2) with
                                   !      orthonormal random matrix orthogonal (1....1)T
  INTEGER, INTENT(in) :: screen    ! Verbosity flag
!EOP

!  *** local variables ***
  INTEGER :: iter, col, row        ! counters
  INTEGER :: i, j, k, l , m        ! counters
  INTEGER, SAVE :: iseed(4)        ! seed array for random number routine
  REAL :: norm                     ! norm of random vector
  INTEGER :: pflag                 ! pointer flag
  INTEGER, SAVE :: first = 1       ! flag for init of random number seed
  REAL :: rndval                   ! temporary value for init of Householder matrix
  REAL :: rndnum                   ! Value of randum entry
  INTEGER, SAVE :: allocflag = 0   ! Flag for dynamic allocation
  REAL, ALLOCATABLE :: rndvec(:)   ! vector of random numbers
  REAL, ALLOCATABLE :: h_rndvec(:)   ! vector of random numbers
  REAL, ALLOCATABLE :: house(:,:)  ! Householder matrix
  REAL, ALLOCATABLE :: transH(:,:)  ! Householder matrix as transformation
  REAL, ALLOCATABLE :: UBtimesB(:,:)  ! Householder matrix as transformation
  REAL, POINTER :: omega_iter(:,:)         ! Pointer to temporary Omega field
  REAL, POINTER :: omega_itermin1(:,:)     ! Pointer to temporary Omega field
  REAL, ALLOCATABLE, TARGET :: temp1(:,:)  ! Target array
  REAL, ALLOCATABLE, TARGET :: temp2(:,:)  ! Target array
  REAL, POINTER :: matrixU(:,:)         ! Pointer to temporary Omega field
  REAL, POINTER :: matrixU_B(:,:)       ! Pointer to temporary Omega field
  REAL, POINTER :: matrixB(:,:)         ! Pointer to temporary Omega field
  REAL, POINTER :: rndmat(:,:)         ! Pointer to temporary Omega field


  randomega: IF (omegatype == 0) THEN 
! *************************************************
! *** Generate deterministic Omega as           ***
! *** Householder matrix associated with the    ***
! *** vector  1/sqrt(rank) (1,...,1)^T          ***
! *************************************************

     write (*,'(9x,a)') '--- Compute deterministic Omega'

     rndnum = 1.0 / SQRT(REAL(rank + 1))

     ! First r rows
     rndval = - rndnum * rndnum / (rndnum + 1.0)
     omegacolb: DO col = 1, rank
        omegarowb: DO row = 1, rank
           omega(row, col) = rndval
        END DO omegarowb
     END DO omegacolb
     
     DO col = 1, rank
        omega(col, col) = omega(col, col) + 1.0
     END DO

     ! Last row
     rndval = - (rndnum + 1.0) * rndnum / (rndnum + 1.0)
     omegacolc: DO col = 1, rank
        omega(rank + 1, col) = rndval
     END DO omegacolc

  ELSEIF (omegatype == 1) THEN randomega
! ****************************************
! *** Generate omega by random vectors ***
! ****************************************

     write (*,'(9x,a)') '--- Compute random Omega'

! *** Initialization ***

     ! Allocate fields
     ALLOCATE(house(rank + 1, rank))
     ALLOCATE(rndmat(rank, rank))

! *** Initialize orthonormal random matrix of size rank*rank ***

     CALL PDAF_generate_rndmat(rank, rndmat, 1)

! *** Project rndmat orthogonal to (1,...,1)^T ***

     ! *** Compute Householder matrix ***

     rndnum = 1.0 / SQRT(REAL(rank + 1))

     ! First r rows
     rndval = - rndnum * rndnum / (rndnum + 1.0)
     housecol: DO col = 1, rank
        houserow: DO row = 1, rank
           house(row, col) = rndval
        END DO houserow
     END DO housecol
     
     DO col = 1, rank
        house(col, col) = house(col, col) + 1.0
     END DO
     
     ! Last row
     rndval = - (rndnum + 1.0) * rndnum / (rndnum + 1.0)
     housecolb: DO col = 1, rank
        house(rank + 1, col) = rndval
     END DO housecolb

     ! *** Complete Omega: house * rndmat ***

     CALL DGEMM('n', 'n', rank + 1, rank, rank, &
          1.0, house, rank + 1, rndmat, rank, &
          0.0, omega, rank + 1)

! *** CLEAN UP ***

     DEALLOCATE(house)
     DEALLOCATE(rndmat)

  ELSE randomega
! *** Generate Omega as a product of a deterministic  ***
! *** transformation with an orthonormal random       ***
! *** matrix that preserves the mean.                 ***
! *** 1. The deterministic matrix matrix given by the ***
! *** householder matrix from omegatype=0.            ***
! *** 2. The random matrix is generated analogously   ***
! *** to omegatype=1 followed by a transformation to  ***
! *** ensure the (1,....,1)^T is an eigenvector of    ***
! *** the matrix.                                     ***

     write (*,'(9x,a)') '--- Compute random product Omega'

! *** Initialization ***

    ! Allocate fields
     ALLOCATE(house(rank + 1, rank))
     ALLOCATE(rndmat(rank, rank))

! *** 1. Deterministic part:                            ***
! *** Compute Householder matrix associated with the    ***
! *** vector  1/sqrt(rank) (1,...,1)^T                  ***
! *** (this is the transformation used for omegatype=0) ***

     rndnum = 1.0 / SQRT(REAL(rank + 1))

     ! First r rows
     rndval = - rndnum * rndnum / (rndnum + 1.0)
     housecolc: DO col = 1, rank
        houserowc: DO row = 1, rank
           house(row, col) = rndval
        END DO houserowc
     END DO housecolc
     
     DO col = 1, rank
        house(col, col) = house(col, col) + 1.0
     END DO

     ! Last row
     rndval = - (rndnum + 1.0) * rndnum / (rndnum + 1.0)
     housecalc: DO col = 1, rank
        house(rank + 1, col) = rndval
     END DO housecalc

! *** 2. Random part: 
! *** Initialize orthonormal random matrix of size rank*rank 
! *** with eigenvector (1,...,1)^T

     CALL PDAF_generate_rndmat(rank, rndmat, 2)

! *** 3. Multiply deterministic and random parts: 

     CALL DGEMM ('n', 'n', rank+1, rank, rank, &
          1.0, house, rank+1, rndmat, rank, &
          0.0, omega, rank+1)

! *** CLEAN UP ***

     DEALLOCATE(house, rndmat)

  END IF randomega

END SUBROUTINE PDAF_seik_omega
!BOP
!
! !ROUTINE: PDAF_generate_rndmat - Generate random matrix with special properties
!
! !INTERFACE:
SUBROUTINE PDAF_generate_rndmat(dim, rndmat, mattype)

! !DESCRIPTION:
! Generate a transformation matrix OMEGA for
! the generation and transformation of the 
! ensemble in the SEIK and LSEIK filter.
! Generated is a uniform orthogonal matrix OMEGA
! with R columns orthonormal in $R^{r+1}$
! and orthogonal to (1,...,1)' by iteratively 
! applying the Householder matrix onto random 
! vectors distributed uniformly on the unit sphere.
!
! This version initializes at each iteration step
! the whole Householder matrix and subsequently
! computes Omega using GEMM from BLAS. All fields are 
! allocated once at their maximum required size.
! (On SGI O2K this is about a factor of 2.5 faster
! than the version applying BLAS DDOT, but requires
! more memory.)
!
! For omegatype=0 a deterministic omega is computed
! where the Housholder matrix of (1,...,1)' is operated
! on an identity matrix.
!
! !  This is a core routine of PDAF and 
!    should not be changed by the user   !
!
! !REVISION HISTORY:
! 2002-01 - Lars Nerger - Initial code
! 2014-05 - Lars Nerger - simplified version for SANGOMA
!
! !USES:
  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER, INTENT(in) :: dim       ! Size of matrix mat
  REAL, INTENT(out)   :: rndmat(dim, dim) ! Matrix
  INTEGER, INTENT(in) :: mattype   ! Select type of random matrix:
                                   !   (1) orthonormal random matrix
                                   !   (2) orthonormal with eigenvector (1,...,1)^T

! !CALLING SEQUENCE:
! Called by: PDAF_seik_omega
!EOP

!  *** local variables ***
  INTEGER :: iter, col, row          ! counters
  INTEGER :: i, j, k, l , m          ! counters
  INTEGER :: dimrnd                  ! Size of random matrix to be generation at first part
  INTEGER, SAVE :: iseed(4)          ! seed array for random number routine
  REAL :: norm                       ! norm of random vector
  INTEGER :: pflag                   ! pointer flag
  INTEGER, SAVE :: first = 1         ! flag for init of random number seed
  REAL :: rndval                     ! temporary value for init of Householder matrix
  REAL :: rndnum                     ! Value of randum entry
  INTEGER, SAVE :: allocflag = 0     ! Flag for dynamic allocation
  REAL, ALLOCATABLE :: rndvec(:)     ! vector of random numbers
  REAL, ALLOCATABLE :: h_rndvec(:)   ! vector of random numbers
  REAL, ALLOCATABLE :: house(:,:)    ! Householder matrix
  REAL, ALLOCATABLE :: transH(:,:)   ! Householder matrix as transformation
  REAL, ALLOCATABLE :: matUBB(:,:)   ! Temporary matrix
  REAL, POINTER :: mat_iter(:,:)     ! Pointer to temporary random array
  REAL, POINTER :: mat_itermin1(:,:) ! Pointer to temporary random array
  REAL, POINTER :: matU(:,:)         ! Pointer to temporary array
  REAL, POINTER :: matUB(:,:)        ! Pointer to temporary array
  REAL, POINTER :: matB(:,:)         ! Pointer to temporary array
  REAL, ALLOCATABLE, TARGET :: temp1(:,:)  ! Target array
  REAL, ALLOCATABLE, TARGET :: temp2(:,:)  ! Target array


! **********************
! *** INITIALIZATION ***
! **********************

  ! Determine size of matrix build through householder reflections
  randomega: IF (mattype == 1) THEN
     ! Random orthonormal matrix
     dimrnd = dim
  ELSE
     ! Random orthonormal matrix with eigenvector (1,...,1)^T
     dimrnd = dim - 1
  END IF randomega


! ******************************************
! *** Generate orthonormal random matrix ***
! ******************************************

  ! allocate fields
  ALLOCATE(rndvec(dim))
  ALLOCATE(house(dim + 1, dim))
  ALLOCATE(temp1(dim, dim), temp2(dim, dim))

  ! set pointers
  mat_itermin1 => temp1
  mat_iter     => temp2
  pflag = 0

  ! Initialized seed for random number routine
  IF (first == 1) THEN
     iseed(1) = 1000
     iseed(2) = 2034
     iseed(3) = 0
     iseed(4) = 3
     first = 2
  END IF


! *** First step of iteration       ***  
! *** Determine mat_iter for iter=1 ***

  ! Get random number [-1,1]
  CALL dlarnv(2, iseed, 1, rndvec(1))
  
  IF (rndvec(1) >= 0.0) THEN
     mat_itermin1(1, 1) = +1.0
  ELSE
     mat_itermin1(1, 1) = -1.0
  END IF

! *** Iteration ***

  iteration: DO iter = 2, dimrnd

! Initialize new random vector
      
     ! Get random vector of dimension DIM (elements in [-1,1])
     CALL dlarnv(2, iseed, iter, rndvec(1:iter))

     ! Normalize random vector
     norm = 0.0
     DO col = 1, iter
        norm = norm + rndvec(col)**2
     END DO
     norm = SQRT(norm)
        
     DO col = 1, iter
        rndvec(col) = rndvec(col) / norm
     END DO

! Compute Householder matrix

     ! First ITER-1 rows
     rndval = 1.0 / (ABS(rndvec(iter)) + 1.0)
     housecol: DO col = 1, iter - 1
        houserow: DO row = 1,iter - 1
           house(row, col) = - rndvec(row) * rndvec(col) * rndval
        END DO houserow
     END DO housecol
        
     DO col = 1, iter - 1
        house(col, col) = house(col, col) + 1.0
     END DO

     ! Last row
     housecol2: DO col = 1, iter - 1
        house(iter, col) = - (rndvec(iter) + SIGN(1.0, rndvec(iter))) &
             * rndvec(col) * rndval
     END DO housecol2

! Compute matrix on this iteration stage

     ! First iter-1 columns
     CALL dgemm('n', 'n', iter, iter - 1, iter - 1, &
          1.0, house, dim + 1, mat_itermin1, dim, &
          0.0, mat_iter, dim)

     ! Final column
     DO row = 1, iter
        mat_iter(row, iter) = rndvec(row)
     END DO

! Adjust pointers to temporal OMEGA fields

     IF (pflag == 0) THEN
        mat_itermin1 => temp2
        mat_iter     => temp1
        pflag = 1
     ELSE IF (pflag == 1) THEN
        mat_itermin1 => temp1
        mat_iter     => temp2
        pflag = 0
     END IF

  END DO iteration


! ****************************************************
! *** Ensure eigenvector (1,...1,)^T for mattype=2 ***
! ****************************************************

  mattype2: IF (mattype == 1) THEN

     ! *** Generation of random matrix completed for mattype=1
     rndmat = mat_itermin1

  ELSE mattype2

     ! *** Complete generation of random matrix with eigenvector
     ! *** (1,...,1)^T by transformation with a basis that
     ! *** includes (1,...,1)^T. (We follow the description 
     ! *** Sakov and Oke, MWR 136, 1042 (2008)).

     NULLIFY(mat_iter, mat_itermin1)

     ALLOCATE(h_rndvec(dim))

! *** Complete initialization of random matrix with eigenvector ***
! *** (1,...,1)^T in the basis that includes (1,...,1)^T        ***

     IF (pflag == 0) THEN
        matU   => temp1
        matUB => temp2
     ELSE
        matU   => temp2
        matUB => temp1
     END if

     matUB(:,:) = 0.0
     matUB(1,1) = 1.0
     DO col = 2, dim
        DO row = 2, dim
           matUB(row, col) = matU(row - 1, col - 1)
        END DO
     END DO
     NULLIFY(matU)

! *** Generate orthonormal basis including (1,...,1)^T as leading vector ***
! *** We again use houesholder reflections.                              ***

     IF (pflag == 0) THEN
        matB => temp1
     ELSE
        matB => temp2
     END IF

     ! First column
     DO row = 1, dim
        matB(row, 1) = 1.0 / SQRT(REAL(dim))
     END DO

     ! columns 2 to dim
     buildB: DO col = 2, dim

        ! Get random vector of dimension DIM (elements in [0,1])
        CALL dlarnv(1, iseed, dim, rndvec)

        loopcols: DO i = 1, col - 1
           DO j = 1, dim
              DO k = 1, dim
                 house(k, j) = - matB(k,i) * matB(j,i)
              END DO
           END DO
           DO j = 1, dim
              house(j, j) = house(j, j) + 1.0
           END DO

           ! Apply house to random vector
           CALL dgemv ('n', dim, dim, &
                1.0, house, dim+1, rndvec, 1, &
                0.0, h_rndvec, 1)
           rndvec = h_rndvec

        END DO loopcols

        ! Normalize vector
        norm = 0.0
        DO i = 1, iter
           norm = norm + h_rndvec(i)**2
        END DO
        norm = SQRT(norm)
        
        DO i = 1, iter
           h_rndvec(i) = h_rndvec(i) / norm
        END DO

        ! Inialize column of matB
        matB(:, col) = h_rndvec

     END DO buildB


! *** Final step: Transform random matrix  ***
! *** rndmat = matB matUB matB^T  ***

     ALLOCATE(matUBB(dim, dim))

     ! matUB * matB^T
     CALL dgemm ('n', 't', dim, dim, dim, &
          1.0, matUB, dim, matB, dim, &
          0.0, matUBB, dim)

     ! matB * matUB * matB^T
     CALL dgemm ('n', 'n', dim, dim, dim, &
          1.0, matB, dim, matUBB, dim, &
          0.0, rndmat, dim)

! *** CLEAN UP ***

     NULLIFY(matUB, matB)
     DEALLOCATE(matUBB)
     DEALLOCATE(h_rndvec)

  END IF mattype2


! ************************
! *** General clean up ***
! ************************

  DEALLOCATE(temp1, temp2)
  DEALLOCATE(rndvec, house)

END SUBROUTINE PDAF_generate_rndmat
