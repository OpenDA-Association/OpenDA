! $Id: example_ComputeSensitivity.F90 305 2015-04-26 08:56:44Z pkirchge $
!BOP
!
! !Program: example_ComputeDer  --- Compute derozier diagnostics
!
! !INTERFACE:
PROGRAM example_computeDerozier

! !DESCRIPTION:
! This is an example showing how to use the routines
! sangoma_ComputeSensitivity and
! sangoma_Compute_sensitivity_op to compute the 
! sensitivity of the posterior ensemble mean to the
! observations
!
! The ensemble is read in from simple ASCII files.
!
! !REVISION HISTORY:
! 2015-04 - P. Kirchgessner - Initial code

! !USES:
  IMPLICIT NONE
!EOP
  
! Local variables
  CHARACTER(len=120) :: inpath, infile ! Path to and name stub of input files
  REAL, ALLOCATABLE :: state_true(:)
  REAL, ALLOCATABLE :: states(:, :)    ! Array holding model states
  CHARACTER(len=2) :: ensstr           ! String for ensemble member
  INTEGER :: nobs                      ! number of observations
  REAL, ALLOCATABLE :: obs(:)          ! observations
  REAL, ALLOCATABLE :: weights(:)      ! particle weights


  EXTERNAL :: diag_R  ! Call-back routine for observation operator
  EXTERNAL :: obs_op  ! Call-back routine for observation operator

  REAL, ALLOCATABLE :: Hens(:,:)
  INTEGER :: typeOut(4)
  INTEGER :: dim_ens, i,j, dim_obs, iter, dim_state
  REAL :: m_diff(4), m_quot(4)
  REAL,ALLOCATABLE :: diff1(:),diff2(:),diff3(:),diff4(:)
  REAL,ALLOCATABLE :: quot1(:),quot2(:),quot3(:),quot4(:)
  REAL,ALLOCATABLE :: out1(:),out2(:),out3(:),out4(:)
  REAL,ALLOCATABLE :: m_state(:) 
  REAL, ALLOCATABLE :: innovation(:,:),residual(:,:), increment(:,:)
  REAL, ALLOCATABLE :: Hxa(:), Hxf(:) 
  INTEGER :: flag
 
! ************************************************
! *** Configuration                            ***
! ************************************************


  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_ComputeDerozier          *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*       Compute Derozier statistics       *'
  WRITE (*,'(10x,a/)') '******************************************'


  ! Number of state files to be read
  dim_ens = 5
  ! State dimension
  dim_state = 4
  ! Observation dimension
  dim_obs = 2
  
  ! Path to and name of file holding model trajectory
  inpath = 'inputs/'
  infile = 'fieldA_'

  ! ALLOCATE Input data
  ALLOCATE(innovation(dim_obs,dim_ens))
  ALLOCATE(increment(dim_obs,dim_ens))
  ALLOCATE(residual(dim_obs,dim_ens))
  ALLOCATE(obs(dim_obs))
  ALLOCATE(Hxa(dim_obs))
  ALLOCATE(Hxf(dim_obs))
  ALLOCATE(m_state(dim_state))
  ALLOCATE(Hens(dim_obs,dim_ens))
  ALLOCATE(state_true(dim_state))
  
  ! OUTPUT
  ALLOCATE(out1(dim_obs))
  ALLOCATE(out2(dim_obs))
  ALLOCATE(out3(dim_obs))
  ALLOCATE(out4(dim_obs))

  ALLOCATE(diff1(dim_obs))
  ALLOCATE(diff2(dim_obs))
  ALLOCATE(diff3(dim_obs))
  ALLOCATE(diff4(dim_obs))

  ALLOCATE(quot1(dim_obs))
  ALLOCATE(quot2(dim_obs))
  ALLOCATE(quot3(dim_obs))
  ALLOCATE(quot4(dim_obs))

  ! initialize true state
  state_true(1) = 2.1
  state_true(2) = 2.3
  state_true(3) = 4.0
  state_true(4) = 1.3

! ************************************************
! ***              Init                        ***
! ************************************************

! initialize observations by perturbing the true state
  CALL obs_op(1,dim_state,dim_obs,state_true,obs)

! Generate some observations for this example
  ! add random error
  obs(1) = obs(1) + 0.1063
  obs(2) = obs(2) - 0.4359



  WRITE (*,'(10x,a)') '*******************************************'
  WRITE (*,'(10x,a)') '*        example_ComputeDerozier          *'
  WRITE (*,'(10x,a)') '*                                         *'
  WRITE (*,'(10x,a)') '*       Compute Derozier statistics       *'
  WRITE (*,'(10x,a/)') '******************************************'


! ************************
! *** Read state files ***
! ************************


  WRITE (*,'(/1x,a)') '------- Read states -------------'
  WRITE (*,*) 'Read states from files:  ',TRIM(inpath)//TRIM(infile),'*.txt'

  ALLOCATE(states(dim_state, dim_ens))

  read_in: DO iter = 1, dim_ens 

     WRITE (ensstr, '(i1)') iter
     OPEN(11, file = TRIM(inpath)//TRIM(infile)//TRIM(ensstr)//'.txt', status='old')
 
     DO i = 1, dim_state
        READ (11, *) states(i, iter)
     END DO

     CLOSE(11)

  END DO read_in

 m_state = 0
 ! Compute mean state
 do i = 1,dim_ens
    do j = 1,dim_state
       m_state(j) = m_state(j) + states(j,i)
    enddo
 enddo
 m_state = m_state/real(dim_ens)


 ! Compute Hens
 do i = 1,dim_ens
    CALL obs_op(1,dim_state,dim_obs,states(:,i),Hens(:,i))
 enddo

 ! Compute Hx^a and Hx^f
 ! Compute innovation/residual and increment
 do i = 1,dim_ens
    CALL obs_op(1,dim_state,dim_obs,states(:,i),Hxf)
    CALL obs_op(1,dim_state,dim_obs,states(:,i),Hxa)
    do j = 1,dim_obs
       Hxf(j) = Hxf(j)+0.07
    enddo
    innovation(:,i) = obs- Hxf
    residual(:,i)   = obs- Hxa
    increment(:,i)  = Hxa- Hxf 
 enddo



! ******************************************************
! *** Call routine to compare derozier's statistics  ***
! ******************************************************

  WRITE (*,*) '------- Compute Deroziers statistics -------------'
  WRITE (*,*) '------- Test if: E[d^of (d^of)^T] = R + HP^fH^T -----------' 
  WRITE (*,*) '------- Test if: E[d^af (d^of)^T] = HP^fH^T     -----------'
 
  typeOut(1) = 1 
  typeOut(2) = 1 
  typeOut(3) = 0 
  typeOut(4) = 0 

  ! Compute left size of equation
  CALL sangoma_Desrozier(dim_obs,dim_ens,innovation, residual, increment, &
                                   typeOut,out1,out2,out3,out4)

  ! Compare with right side of equation
  CALL sangoma_CompareDes(dim_obs,dim_ens,out1,out2,out3,out4,typeOut, &
                         Hens, diag_R,diff1,diff2,diff3,diff4,m_diff, &
                         quot1,quot2,quot3,quot4,m_quot,flag)
 
  IF (flag== 1) THEN
      WRITE (*,*) '------- Result: -------------'
      WRITE (*,*) 'E[d^af (d^of)^T] - HP^fH^T =', m_diff(2)
      WRITE (*,*) 'E[d^of (d^of)^T] - R + HP^fH^T =', m_diff(1) 
      WRITE (*,*) '------- Ratio: -------------'
      WRITE (*,*) 'E[d^of (d^of)^T] / R + HP^fH^T =', m_quot(1)
      WRITE (*,*) 'E[d^of (d^of)^T] / R + HP^fH^T =', m_quot(2)
  ENDIF
  typeOut(1) = 0
  typeOut(2) = 0 
  typeOut(3) = 1 
  typeOut(4) = 1 
 
  ! Compute left size of equation
  CALL sangoma_Desrozier(dim_obs,dim_ens,innovation, residual, increment, &
                                   typeOut,out1,out2,out3,out4)

  ! Compare with right side of equation
  CALL sangoma_CompareDes(dim_obs,dim_state,out1,out2,out3,out4,typeOut, &
                                   Hens, diag_R,diff1,diff2,diff3,diff4,m_diff, &
                                   quot1,quot2,quot3,quot4,m_quot,flag)
  IF (flag == 1) THEN
      WRITE (*,*) '------- Result: -------------'
      WRITE (*,*) 'E[d^oa (d^of)^T] - R =' , m_diff(3)
      WRITE (*,*) 'E[d^af (d^oa)^T] - HP^aH^T =', m_diff(4)
      WRITE (*,*) '------- Result: -------------'
      WRITE (*,*) 'E[d^oa (d^of)^T] / R = ', m_quot(3)
      WRITE (*,*) 'E[d^of (d^oa)^T] / HP^aH^T =', m_quot(4)
  ELSE
     WRITE(*,*) 'There is an error in the computation'
  ENDIF 
! ****************
! *** Clean up ***
! ****************


  WRITE (*,'(/1x,a/)') '------- END -------------'

END PROGRAM example_computeDerozier
