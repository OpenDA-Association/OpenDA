subroutine computeNextTimeStep(tIndex,cNow)
  
  use inputFileVariables

  implicit none

  integer tIndex
  real cNow(inputVARIABLES%nData_c)
  real cNext(inputVARIABLES%nData_c)
  real di
  real weightRight
  real weightLeft
  real cValue
  real bValue
  integer actualIndex
  integer iLoc
  integer iLeft
  integer iRight
  integer i,iSource

  !! Propagate one time step:
  cNext=0.0*cNow
  do i=1,inputVARIABLES%nData_c,1
    di=inputVARIABLES%u(i)*inputVARIABLES%time(2)/inputVARIABLES%x(2)
    iLeft=i+floor(di)
    weightRight=di-floor(di)
    weightLeft=1.0-weightRight
    iRight=iLeft+1
    if ( iLeft.ge.1.and.iLeft.le.inputVARIABLES%nData_c ) then
      cNext(iLeft)=cNext(iLeft)+cNow(i)*weightLeft
    end if
    if ( iRight.ge.1.and.iRight.le.inputVARIABLES%nData_c ) then
      cNext(iRight)=cNext(iRight)+cNow(i)*weightRight
    end if
  end do

  !! Add sources:
  do iSource=1,inputVARIABLES%nData_source,1
    iLoc=inputVARIABLES%source_locations(iSource)+1 !'plus 1' is used to make iLoc equal to Python index
    if ( tIndex.lt.inputVARIABLES%nData_source_values ) then
      cValue=inputVARIABLES%source_values(iSource,tIndex)
    else
      cValue=inputVARIABLES%source_values(iSource,tIndex)
    end if    
    cValue=max(cValue,0.0)
    cNext(iLoc)=cNext(iLoc)+cValue*inputVARIABLES%time(2)/inputVARIABLES%x(2)/inputVARIABLES%a(iLoc)
  end do

  !! Add boundary condition:
  if ( inputVARIABLES%u(1).gt.0.0 ) then
    actualIndex = min(tIndex,inputVARIABLES%nData_bound_values(1))
    bValue=max(inputVARIABLES%bound_values(1,actualIndex),0.0)
    cNext(1)=bValue
  end if
  if ( inputVARIABLES%u(inputVARIABLES%nData_u).lt.0.0 ) then
    actualIndex = min(tIndex,inputVARIABLES%nData_bound_values(2))
    bValue=max(inputVARIABLES%bound_values(2,actualIndex),0.0)
    cNext(inputVARIABLES%nData_c)=bValue
  end if

  !! Return output:
  cNow=cNext

!    print *,'from computeNextTimeStep.f90 x: ',x
!    print *,'from computeNextTimeStep.f90 u: ',u
!    print *,'from computeNextTimeStep.f90 a: ',a
!    print *,'from computeNextTimeStep.f90 c: ',c
!    print *,'from computeNextTimeStep.f90 refdate: ',refdate
!    print *,'from computeNextTimeStep.f90 tUnit: ', tUnit
!    print *,'from computeNextTimeStep.f90 time: ', time
!    print *,'from computeNextTimeStep.f90 source_locations: ', source_locations
!    print *,'from computeNextTimeStep.f90 source_labels: ', source_labels
!    print *,'from computeNextTimeStep.f90 source_values(1,:): ', source_values(1,:)
!    print *,'from computeNextTimeStep.f90 source_values(2,:): ', source_values(2,:)
!    print *,'from computeNextTimeStep.f90 source_values(3,:): ', source_values(3,:)
!    print *,'from computeNextTimeStep.f90 output_locations: ', output_locations
!    print *,'from computeNextTimeStep.f90 output_labels: ', output_labels
!    print *,'from computeNextTimeStep.f90 bound_labels: ', bound_labels
!    print *,'from computeNextTimeStep.f90 bound_locations: ', bound_locations
!    print *,'from computeNextTimeStep.f90 bound_values: ', bound_values
!    print *,'from computeNextTimeStep.f90 nData_x: ', nData_x
!    print *,'from computeNextTimeStep.f90 nData_a: ', nData_a
!    print *,'from computeNextTimeStep.f90 nData_u: ', nData_u
!    print *,'from computeNextTimeStep.f90 nData_c: ', nData_c
!    print *,'from computeNextTimeStep.f90 nData_source: ', nData_source
!    print *,'from computeNextTimeStep.f90 nData_source_values: ', nData_source_values
!    print *,'from computeNextTimeStep.f90 nData_output: ', nData_output
!    print *,'from computeNextTimeStep.f90 nData_bound_locations: ',nData_bound_locations
!    print *,'from computeNextTimeStep.f90 nData_bound_values: ',nData_bound_values
!    print *,'iError: ', iError
!
  
end subroutine
