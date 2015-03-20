subroutine collectOutput(tIndex,cNow)
  use inputFileVariables
  implicit none

  real cNow(inputVARIABLES%nData_c)
  integer tIndex
  integer iOut
  integer nTimeLevel

  if ( tIndex.eq.0 ) then
    nTimeLevel=floor((inputVARIABLES%time(3)-inputVARIABLES%time(1))/inputVARIABLES%time(2))+1
    allocate(inputVARIABLES%collOutput(inputVARIABLES%nData_output,nTimeLevel))
  end if

  do iOut=1,inputVARIABLES%nData_output,1
    inputVARIABLES%collOutput(iOut,tIndex+1)=cNow(floor(inputVARIABLES%output_locations(iOut))+1)
    ! additive factor 1 is needed to make it refer to the same location as that
    ! of Python program.
  end do
end subroutine collectOutput
