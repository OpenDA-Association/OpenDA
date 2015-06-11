!
! !INTERFACE:
PROGRAM example_PODcostgrad


! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  INTEGER :: i, iter, k                ! Counters
  INTEGER :: dim_state                 ! Size of model field
  INTEGER :: nfiles                    ! Number of input files (=Ensemble Size)
  INTEGER :: nmodes                    ! No. of modes
  INTEGER :: nparam                    ! No. of paramters
  INTEGER :: nsteps                    ! No. of time steps
  INTEGER :: nobs                      ! No. of observations
  INTEGER :: nanalysis                 ! No. of analysis steps
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  CHARACTER(len=120) :: outfile_eof, outfile_svals ! Names of output files
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  REAL, ALLOCATABLE :: Mstates(:, :)   ! Array holding model state operator
  REAL, ALLOCATABLE :: Mparam(:,:, :)  ! Array holding model parameter operator
  REAL, ALLOCATABLE :: Minter(:, :)    ! Array holding model state operator
  REAL, ALLOCATABLE :: M(:,:, :)       ! Array holding model parameter operator
  REAL, ALLOCATABLE :: Malpha(:,:, :)  ! Array holding model parameter operator
  REAL, ALLOCATABLE :: Hobserver(:, :) ! Observation operator H
  REAL, ALLOCATABLE :: observer(:, :)  ! Reduced Observation operator HP
  INTEGER, ALLOCATABLE :: analysis(:)  ! Analysis time steps
  REAL, ALLOCATABLE :: obs(:, :)       ! Observation increments Y - HX
  REAL, ALLOCATABLE :: obsstd(:, :)    ! Observation STD
  REAL, ALLOCATABLE :: alpha(:)        ! Array containing paramters
  CHARACTER(len=2) :: ensstr           ! String for ensemble member
  CHARACTER(len=2) :: ensstep          ! String for step

  ! Output variables of EOF routine
  INTEGER :: status = 0           ! Output of  ComputePOD  routine: status flag
  REAL :: stddev                  ! Output of  ComputePOD routine: multivariate STDDEV (not used here)
  REAL :: cost                    ! Output of COSTGRAD routine: Value of the objective function
  REAL, ALLOCATABLE :: svals(:)   ! Output of ComputePOD routine: Singular values
  REAL, ALLOCATABLE :: svecs(:,:) ! Output of ComputePOD routine: POD modes
  REAL, ALLOCATABLE :: grad(:)    ! Output of COSTGRAD routine: Array containing gradient


! ************************************************
! *** Configuration                            ***
! ************************************************

  ! Number of state files to be read
  nfiles = 5

  ! State dimension
  dim_state = 4

  ! No. of modes
  nmodes = 2

  ! No. of parameters
  nparam = 2

  ! No. of timesteps
  nsteps = 2

  ! No. of Observation
   nobs = 2

  ! No. of Analysis steps
   nanalysis = 1


  ! Path to and name of file holding model trajectory
  inpath = './inputs/'
  infile = 'fieldA_'

  ! Names of output files
  outfile_eof = 'pod_'             ! Files holding EOFs
  outfile_svals = 'svals.txt'      ! Files holding singular values
 

! ************************************************
! *** Init                                     ***
! ************************************************

  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*           example_PODcostgrad           *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '* This program first computes the leading *'
  WRITE (*,'(10x,a)') '*     eigen values and eigen vectors      *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*   Then reads dynamic state, parameters  *'
  WRITE (*,'(10x,a)') '*     operators and computes reduced      *'
  WRITE (*,'(10x,a)') '*   operators based on leading POD modes  *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*   Finally compute the values of J and   *'
  WRITE (*,'(10x,a)') '*    gradient with respect to parameter   *'
  WRITE (*,'(10x,a)') '*                 alpha                   *'
  WRITE (*,'(10x,a/)') '*******************************************'


! ************************
! *** Read state files ***
! ************************

  WRITE (*,'(/1x,a)') '------- Read ensemble of states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(states(dim_state, nfiles))
  read_in: DO iter = 1, nfiles

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, dim_state
        READ (11, *) states(i, iter)
     END DO

     CLOSE(11)

  END DO read_in


! *************************************************
! *** Call routine to perform EOF decomposition ***
! *************************************************

  ALLOCATE(svals(nmodes))
  ALLOCATE(svecs(dim_state, nmodes))
 
  CALL sangoma_computepod(nmodes, dim_state, nfiles, 1, 1, 1, &
       0, states, stddev, svals, svecs, status)

  WRITE (*,'(5x,a)') 'Eigen Values: '
  DO i = 1, nmodes
    WRITE (*, '(10x, i4, es12.3)') i, svals(i)
  END DO

! *************************************************************
! *** Write leading eigen values and eigen vectors to files ***
! *************************************************************

  WRITE (*,'(/1x,a)') '------- Write POD modes and eigen values -------------'

  ! *** Write singular values ***
  WRITE (*,*) 'Write singular vectors to file: ',TRIM(outfile_svals)
  OPEN(11, file = TRIM(outfile_svals), status='replace')
  DO i = 1, nmodes
     WRITE (11, *) svals(i)
  END DO
  CLOSE(11)

  ! *** Write EOFs ***
  WRITE (*,*) 'Write eofs to files: ',TRIM(outfile_eof),'*.txt'
  writing: DO iter = 1, nmodes

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(outfile_eof)//TRIM(ensstr)//'.txt', status='replace')
 
     DO i = 1, dim_state
        WRITE (11, *) svecs(i, iter)
     END DO

     CLOSE(11)

  END DO writing

! *******************************
! *** clearing part of memory ***
! *******************************

  DEALLOCATE(states)



! *********************************************************
! *** Compute Reduced Dynamic Operator M and Malpha     ***
! *********************************************************

  ALLOCATE(Mstates(dim_state, dim_state))
  ALLOCATE(Minter(nmodes, dim_state))
  ALLOCATE(M(nsteps, nmodes, nmodes))

! Read data either from file - here it is just defined 

  Mstates = 0.0

  modeloper: DO iter = 1, dim_state

     DO i = 1, dim_state
        
        IF (i == iter) THEN
           Mstates(i,iter) = 0.5
        END IF
     END DO
     
  END DO modeloper

  redoper: DO i = 1, nsteps

     Minter    =    matmul(transpose(svecs),Mstates)
     M(i, :,:) =    matmul(Minter,svecs)

  END DO redoper


! *******************************
! *** clearing part of memory ***
! *******************************

  DEALLOCATE(Mstates, Minter)


! **************************************
! *** Allocate memory for parameters ***
! **************************************

  ALLOCATE(Mparam(nsteps,dim_state, nparam))
  ALLOCATE(Malpha(nsteps,nmodes, nparam))

  infile = 'Popr_'

  WRITE (*,'(/1x,a)') '------- Read parameter operator -------------'
  WRITE (*,*) 'Read parameter operator from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  DO k = 1, nsteps
 
     WRITE (ensstep, '(i1)') nsteps

     modelparam: DO iter = 1, nparam

        WRITE (ensstr, '(i1)') iter
        OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstep)//'_'//TRIM(ensstr)//'.txt', status='old')
 
        DO i = 1, dim_state
           READ (11, *) Mparam(k,i, iter)
        END DO

        CLOSE(11)

     END DO modelparam

  END DO

  redparam: DO i = 1, nsteps

     Malpha(i,:,:)    =    matmul(transpose(svecs),Mparam(i,:,:))

  END DO redparam


! *******************************
! *** clearing part of memory ***
! *******************************

  DEALLOCATE(Mparam)


! ******************************************
! *** Reading linear observation opertor ***
! ******************************************

  ALLOCATE(Hobserver(nobs, dim_state))
  ALLOCATE(observer(nobs, nmodes))

  infile = 'H_'

  WRITE (*,'(/1x,a)') '------- Read observation operator -------------'
  WRITE (*,*) 'Read observation operator from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  Hoperator: DO iter = 1, nobs

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, dim_state
        READ (11, *) Hobserver(iter, i)
     END DO

     CLOSE(11)

  END DO Hoperator

  observer = matmul(Hobserver, svecs)


! *******************************
! *** clearing part of memory ***
! *******************************

  DEALLOCATE(Hobserver)


! ******************************************
! *** Reading Analysis time from file    ***
! ******************************************

  ALLOCATE(analysis(nanalysis))

  infile = 'Ana_'

  WRITE (*,'(/1x,a)') '------- Read Analysis times -------------'
  WRITE (*,*) 'Read Analysis times from file:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  iter = 1

  WRITE (ensstr, '(i1)') iter
  OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
  DO i = 1, nanalysis
     READ (11, *) analysis(i)
  END DO

  CLOSE(11)


! ********************************************
! *** Reading observation increment Y - HX ***
! ********************************************

  ALLOCATE(obs(nanalysis, nobs))

  infile = 'obs_'

  WRITE (*,'(/1x,a)') '------- Read observation increments -------------'
  WRITE (*,*) 'Read observation increments from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  Obsinc: DO iter = 1, nanalysis

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, nobs
        READ (11, *) obs(iter, i)
     END DO

     CLOSE(11)

  END DO Obsinc


! ********************************************
! *** Reading observation STD              ***
! ********************************************

  ALLOCATE(obsstd(nanalysis, nobs))

  infile = 'obsstd_'

  WRITE (*,'(/1x,a)') '------- Read observation STD -------------'
  WRITE (*,*) 'Read observation STD from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  Obsstand: DO iter = 1, nanalysis

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, nobs
        READ (11, *) obsstd(iter, i)
     END DO

     CLOSE(11)

  END DO Obsstand


! *********************************************
! *** Reading Initial parameters from file  ***
! *********************************************

  ALLOCATE(alpha(nparam))
  ALLOCATE(grad(nparam))

  infile = 'Alpha_'

  WRITE (*,'(/1x,a)') '------- Read Initial parameters -------------'
  WRITE (*,*) 'Read Initial parameters from file:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  iter = 1

  WRITE (ensstr, '(i1)') iter
  OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
  DO i = 1, nparam
     READ (11, *) alpha(i)
  END DO

  CLOSE(11)


! *************************************************
! *** Compute cost function and gradient vector ***
! *************************************************

   CALL sangoma_costgrad(nmodes, nparam, nsteps, dim_state, nobs, nanalysis, M, Malpha, alpha, &
     obs, obsstd, observer, cost, grad, analysis, status) 

  WRITE (*,'(5x,a)') 'Value of Objective function: '
  WRITE (*, *) '        ',cost
 

  WRITE (*,'(5x,a)') 'gradient Vector: '
  DO i = 1, nparam
    WRITE (*, '(10x, i4, es12.3)') i, grad(i)
  END DO

 
! ********************
! *** Finishing up ***
! ********************

  DEALLOCATE(svals, svecs)

  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_PODcostgrad
