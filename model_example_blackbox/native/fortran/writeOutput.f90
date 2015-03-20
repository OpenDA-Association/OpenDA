subroutine writeOutput(cNow)
  use inputFileVariables
  implicit none

  real cNow(inputVARIABLES%nData_c)
  integer iOut
  integer nTimeLevel
  character*2000 outLine
  character*10 outLoc
  character*20 outVal
  integer iOutValues

  ! create output file:
  open(1,file=trim(inputVARIABLES%outputFile))

  ! write output labels:
  outLine="'"//trim(inputVARIABLES%output_labels(1))//"'"
  do iOut=2,inputVARIABLES%nData_output,1
    outLine=trim(outLine)//",'"//trim(inputVARIABLES%output_labels(iOut))//"'"
  end do
  outLine="output_labels=["//trim(outLine)//"]"
  write(1,'(a)') trim(outLine)

  ! write output locations:
  outline="output_locations=["
  call writeLine( outline, inputVARIABLES%output_locations, '(f4.1)' )

  ! write output values:
  nTimeLevel=floor((inputVARIABLES%time(3)-inputVARIABLES%time(1))/inputVARIABLES%time(2))+1
  do iOut=1,inputVARIABLES%nData_output,1
    outline="output_values['"//trim(inputVARIABLES%output_labels(iOut))//"']=["
    call writeLine( outline, inputVARIABLES%collOutput(iOut,:), '(f8.2)' )
  end do

  ! write concentration at last time step:
  outline="c=["
  call writeLine( outline, cNow, '(f8.2)' )

  ! write refdate:
  outline="refdate='"//trim(inputVARIABLES%refdate)//"'"
  write(1,'(a)') trim(outLine)
  ! write time unit:
  outline="unit='"//trim(inputVARIABLES%Tunit)//"'"
  write(1,'(a)') trim(outLine)
  ! write initial time, last time, and time interval
  outline="time=["
  call writeLine( outline, inputVARIABLES%time, '(f10.2)' )

  ! close output file:
  close(1)

contains

subroutine writeLine( outline, values, format )
  character(len=*)   :: outline
  real, dimension(:) :: values
  character(len=*)   :: format

  integer            :: i
  character(len=20)  :: outVal
  character(len=1)   :: separator

  separator = ','

  do i = 1,size(values)
    if ( i == size(values) ) then
      separator = ']'
    endif
    write(outVal,format) values(i)
    outline=trim(outline)//trim(outVal)//separator
  end do

  write(1,'(a)') trim(outLine)

end subroutine writeLine

end subroutine writeOutput
