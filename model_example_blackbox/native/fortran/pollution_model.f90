program main_

  use inputFileVariables

  implicit none
  
  character*200 inputFile
  integer nInputFile
  integer iargc
  integer tIndex
  real, dimension(:), allocatable :: cNow       ! this time concentration
  real t

  !! Get input variables:
  nInputFile=COMMAND_ARGUMENT_COUNT()
  if ( nInputFile.eq.0 ) then
    !use default input:
    print *,'using internal default input'
    call getDefaultInput()
  else
    !read from input file:
    call GET_COMMAND_ARGUMENT(1,inputFile)
    call readInputFile(inputFile)
    if ( inputVARIABLES%iError.eq.1 ) then
      stop
    end if
  end if

  !! Run simulation:
  allocate(cNow(inputVARIABLES%nData_c))
  cNow=inputVARIABLES%c
  tIndex=0
  t=inputVARIABLES%time(1)
  call collectOutput(tIndex,cNow)
  do while ( t.lt.inputVARIABLES%time(3) )
    tIndex=tIndex+1
    call computeNextTimeStep(tIndex,cNow)
    call collectOutput(tIndex,cNow)
    t = t + inputVARIABLES%time(2)
  end do 
  !! Write output into file:
  call writeOutput(cNow)
end program
