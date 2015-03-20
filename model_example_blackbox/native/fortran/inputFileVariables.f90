module inputFileVariables
  integer, parameter :: str_len=200
  
  TYPE inpFileVARIABLES
	  character(len=str_len) outputFile                     ! output file name
	  character*20 refdate                                  ! reference date of simulation
	  character*20 tUnit                                    ! time unit
	  real :: time(3)                                       ! simulation timespan. 1: start_time, 2: delta_t, 3: end_time
	  real, dimension(:), allocatable :: x                  ! grid
	  real, dimension(:), allocatable :: u                  ! flow velocity
	  real, dimension(:), allocatable :: a                  ! cross sectional area
	  real, dimension(:), allocatable :: c                  ! initial concentration
	  real, dimension(:), allocatable :: source_locations   ! source locations
	  real, dimension(:), allocatable :: output_locations   ! output locations
	  real, dimension(:), allocatable :: bound_locations    ! bound locations
	  real, dimension(:,:), allocatable :: source_values    ! source values
	  real, dimension(:,:), allocatable :: bound_values     ! boundary values
	  character*200, dimension(:), allocatable :: source_labels  ! labels of sources
	  character*200, dimension(:), allocatable :: output_labels  ! labels of output
	  character*200, dimension(:), allocatable :: bound_labels   ! labels of boundaries
	  integer, dimension(:), allocatable :: nData_bound_values   ! number of boundary values. Number of values can be different
								! for different boundaries. This variable is used to keep track 
								! of the number of values of each boundary.
	  integer nData_a                                       ! a's number of data 
	  integer nData_c                                       ! c's number of data 
	  integer nData_u                                       ! u's number of data
	  integer nData_x                                       ! x's number of data 
	  integer nData_output                                  ! number of output points
	  integer nData_source_values                           ! number of values of a source
	  integer nData_source                                  ! sources' number of data 
	  integer nData_bound_locations                         ! number of bound locations 
	  integer iError                                        ! error flag: 0=successful,1=error:missing input variable
	  real, dimension(:,:), allocatable :: collOutput       ! variable for collecting results at output locations. 
								! This variable is allocated in collectOutput.f90
  END TYPE inpFileVARIABLES 
  
  TYPE (inpFileVARIABLES) :: inputVARIABLES
  
contains

  subroutine getDefaultInput()
    implicit none
    ! Input/Output variables:
    integer nData_bound_labels    ! number of boundary labels, to be used also to check if #bound_locations=#bound_labels  
    integer nBoundValues          ! temporary storage for number of boundary values     

    ! set x:
    inputVARIABLES%nData_x=3
    allocate (inputVARIABLES%x(inputVARIABLES%nData_x))
    inputVARIABLES%x=(/ 0.0,1.0,4.0 /)

    ! set u:
    inputVARIABLES%nData_u=5
    allocate (inputVARIABLES%u(inputVARIABLES%nData_u))
    inputVARIABLES%u=(/ 1.0, 1.0, 1.0, 1.0, 1.0 /)

    ! set a:
    inputVARIABLES%nData_a=5
    allocate (inputVARIABLES%a(inputVARIABLES%nData_a))
    inputVARIABLES%a=(/ 1.0, 1.0, 1.0, 1.0, 1.0 /)

    ! set c:
    inputVARIABLES%nData_c=5
    allocate (inputVARIABLES%c(inputVARIABLES%nData_c))
    inputVARIABLES%c=(/ 0.1, 0.2, 0.3, 0.4, 0.5 /)

    ! set simulation timespan:
    inputVARIABLES%refdate='01 dec 2000'
    inputVARIABLES%tUnit='seconds'
    inputVARIABLES%time=(/ 0.0, 1.0, 10.0 /)

    ! set source:
    inputVARIABLES%nData_source=1
    allocate (inputVARIABLES%source_locations(inputVARIABLES%nData_source))
    inputVARIABLES%source_locations=(/ 2 /)

    ! set source labels:
    allocate (inputVARIABLES%source_labels(inputVARIABLES%nData_source))
    inputVARIABLES%source_labels='default'

    ! set source values:
    inputVARIABLES%nData_source_values=1
    allocate (inputVARIABLES%source_values(inputVARIABLES%nData_source,inputVARIABLES%nData_source_values))
    inputVARIABLES%source_values(1,:)=(/ 5.0 /)

    ! set bound labels:
    nData_bound_labels=2
    allocate (inputVARIABLES%bound_labels(nData_bound_labels))
    inputVARIABLES%bound_labels(1)='left'
    inputVARIABLES%bound_labels(2)='right'

    ! set bound locations:
    inputVARIABLES%nData_bound_locations=2
    allocate (inputVARIABLES%bound_locations(inputVARIABLES%nData_bound_locations))
    inputVARIABLES%bound_locations=(/ 0.0,-1.0 /)

    ! set bound values:
    nBoundValues=4
    allocate (inputVARIABLES%bound_values(nData_bound_labels,nBoundValues))
    allocate (inputVARIABLES%nData_bound_values(nData_bound_labels))
    inputVARIABLES%bound_values(1,:)=(/ -1000.0, 0.01, 0.02, 0.03 /)
    inputVARIABLES%bound_values(2,:)=(/ 0.0 /)
    inputVARIABLES%nData_bound_values=(/4,1/)

    ! set output file:
    inputVARIABLES%outputFile='default.output'

    ! set output locations:
    inputVARIABLES%nData_output=2
    allocate (inputVARIABLES%output_locations(inputVARIABLES%nData_output))
    inputVARIABLES%output_locations=(/1.0,2.0/)

    ! set output labels:
    allocate (inputVARIABLES%output_labels(inputVARIABLES%nData_output))
    inputVARIABLES%output_labels=(/ 'defaultOutput1','defaultOutput2' /)

  end subroutine getDefaultInput

  subroutine readInputFile(inputFile)
    implicit none
    ! Input/Output variables:
    character*200 inputFile                               ! (I) input file name
  
    ! Local variables:
    integer iFile                 ! file handler
    integer ios                   ! flag for end-of-file
    integer iLine                 ! line index in input file
    integer nLine                 ! length of a line in input file
    integer indVar                ! first index of substring within a string, used as a flag of variable existence in input file
    integer iSource               ! index of sources
    integer iBound                ! index of boundaries
    integer indVarFirst           ! first index of values of a variable in a line in input file
    integer indVarLast            ! lasst index of values of a variable in a line in input file 
    integer MaxInputParam         ! number of parameters specified in input file
    parameter (MaxInputParam=30)
    character*3000 line           ! temporary storage of a line in input file
    character*3000 lline(MaxInputParam) ! storage of all content of input file, comment line is excluded
    integer nInputParam           ! number of parameters specified in the input file
    integer nData_source_labels   ! number of source labels, used also to check if #source=#source_labels
    integer nData_output_labels   ! number of output labels, used also to check if #output=#output_labels 
    integer nData_bound_labels    ! number of boundary labels, to be used also to check if #bound_locations=#bound_labels  
    integer nBoundValues          ! temporary storage for number of boundary values     
  
    inputVARIABLES%iError=0
    
    iFile=1
    open(iFile,file=trim(inputFile),status='old')
    ! read input file and fill lline with input parameters
    nInputParam=0
    do
      ios=1
      read(iFile,'(a)',iostat=ios) line
      if (ios/=0) exit
      line=trim(line)
      indVar=index(line(1:1),'#')
      if ( indVar.eq.0 ) then 
      !store this line if it is not a comment line
        nInputParam=nInputParam+1
        lline(nInputParam)=line
      end if
    end do
    close(iFile)
  
    ! read input parameters
    iLine=0
      ! read x:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'x')
      if ( indVar.eq.0 ) then
        print *,'ERROR: x is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_x)
        allocate (inputVARIABLES%x(inputVARIABLES%nData_x))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%x(1:inputVARIABLES%nData_x)
        !print *,'x: ', inputVARIABLES%x(1:inputVARIABLES%nData_x)
      end if
  
      ! read u:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'u')
      if ( indVar.eq.0) then
        print *,'ERROR: u is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_u)
        allocate (inputVARIABLES%u(inputVARIABLES%nData_u))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%u(1:inputVARIABLES%nData_u)
        !print *,'u: ', inputVARIABLES%u(1:inputVARIABLES%nData_u)
      end if
  
      ! read a from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'a')
      if ( indVar.eq.0) then
        print *,'ERROR: a is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_a)
        allocate (inputVARIABLES%a(inputVARIABLES%nData_a))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%a(1:inputVARIABLES%nData_a)
        !print *,'a: ', inputVARIABLES%a(1:inputVARIABLES%nData_a)
      end if
  
      ! read c from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'c')
      if ( indVar.eq.0) then
        print *,'ERROR: c is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_c)
        allocate (inputVARIABLES%c(inputVARIABLES%nData_c))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%c(1:inputVARIABLES%nData_c)
        !print *,'c: ', inputVARIABLES%c(1:inputVARIABLES%nData_c)
      end if
  
      ! read refdate from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'refdate')
      if ( indVar.eq.0 ) then
        print *,'ERROR: refdate is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'=')
        read(line(indVarFirst+1:nLine),*) inputVARIABLES%refdate
        inputVARIABLES%refdate=trim(inputVARIABLES%refdate)
        !print *,'refdate: ', inputVARIABLES%refdate
      end if
  
      ! read unit (of x) from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'unit')
      if ( indVar.eq.0 ) then
        print *,'ERROR: unit is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'=')
        read(line(indVarFirst+1:nLine),*) inputVARIABLES%tUnit
        inputVARIABLES%tUnit=trim(inputVARIABLES%tUnit)
        !print *,'unit: ', inputVARIABLES%tUnit
      end if
  
      ! read time from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'time')
      if ( indVar.eq.0 ) then
        print *,'ERROR: time is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%time(1:3)
        !print *,'time: ', inputVARIABLES%time(1:3)
      end if
  
      ! read source_locations from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'source_locations')
      if ( indVar.eq.0 ) then
        print *,'ERROR: source_locations is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_source)
        allocate (inputVARIABLES%source_locations(inputVARIABLES%nData_source))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%source_locations(1:inputVARIABLES%nData_source)
        !print *,'source_locations: ', inputVARIABLES%source_locations(1:inputVARIABLES%nData_source)
      end if
  
      ! read source_labels:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'source_labels')
      if ( indVar.eq.0) then
        print *,'ERROR: source_labels is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,nData_source_labels)
        if ( inputVARIABLES%nData_source.ne.nData_source_labels ) then
          print *,'ERROR: number of source locations and labels are not equal. Check input file.'
          inputVARIABLES%iError=1
          return
        end if
        allocate (inputVARIABLES%source_labels(inputVARIABLES%nData_source))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%source_labels(1:inputVARIABLES%nData_source)
        !print *,'source_labels: ', inputVARIABLES%source_labels(1:inputVARIABLES%nData_source)
      end if
  
      ! read source_values:
      iSource=1
      do while ( iSource.le.inputVARIABLES%nData_source )
        iLine=iLine+1
        line=trim(lline(iLine))
        nLine=len_trim(line)
        indVar=index(line(1:nLine),'source_values[')
        if ( indVar.eq.0 ) then
          print *,'ERROR: source_values is not found in input file'
          inputVARIABLES%iError=1
          return
        else
          indVarLast=index(line(1:nLine),']')
          line=trim(line)
          line=line(indVarLast+1:nLine)
          nLine=len_trim(line)
          indVarFirst=index(line(1:nLine),'[')
          indVarLast=index(line(1:nLine),']')
          call computeNumberOfData(line,inputVARIABLES%nData_source_values)
          if ( iSource.eq.1 ) then
            allocate (inputVARIABLES%source_values(inputVARIABLES%nData_source,inputVARIABLES%nData_source_values))
          end if
          read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%source_values(iSource,1:inputVARIABLES%nData_source_values)
          !print *,'source_values: ', inputVARIABLES%source_values(iSource,1:inputVARIABLES%nData_source_values)
        end if
        iSource=iSource+1
      end do
  
      ! read output_file from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'output_file')
      if ( indVar.eq.0 ) then
        print *,'ERROR: output_file is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'=')
        read(line(indVarFirst+1:nLine),*) inputVARIABLES%outputFile
        inputVARIABLES%outputFile=trim(inputVARIABLES%outputFile)
        !print *,'output_file: ', inputVARIABLES%outputFile
      end if
  
      ! read output_locations:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'output_locations')
      if ( indVar.eq.0 ) then
        print *,'ERROR: output_locations is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_output)
        allocate (inputVARIABLES%output_locations(inputVARIABLES%nData_output))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%output_locations(1:inputVARIABLES%nData_output)
        !print *,'output_locations: ', inputVARIABLES%output_locations(1:inputVARIABLES%nData_output)
      end if
  
      ! read output_labels from input file:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'output_labels')
      if ( indVar.eq.0) then
        print *,'ERROR: output_labels is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,nData_output_labels)
        if ( inputVARIABLES%nData_output.ne.nData_output_labels ) then
          print *,'ERROR: number of output locations and labels are not equal. Check input file.'
          inputVARIABLES%iError=1
          return
        end if
        allocate (inputVARIABLES%output_labels(inputVARIABLES%nData_output))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%output_labels(1:inputVARIABLES%nData_output)
        !print *,'output_labels: ', inputVARIABLES%output_labels(1:inputVARIABLES%nData_output)
      end if
  
      ! read bound_labels:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'bound_labels')
      if ( indVar.eq.0 ) then
        print *,'ERROR: bound_labels is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,nData_bound_labels)
        allocate (inputVARIABLES%bound_labels(nData_bound_labels))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%bound_labels(1:nData_bound_labels)
        !print *,'bound_labels: ', inputVARIABLES%bound_labels(1:nData_bound_labels)
      end if
  
      ! read bound_locations:
      iLine=iLine+1
      line=trim(lline(iLine))
      nLine=len_trim(line)
      indVar=index(line(1:nLine),'bound_locations')
      if ( indVar.eq.0 ) then
        print *,'ERROR: bound_locations is not found in input file'
        inputVARIABLES%iError=1
        return
      else
        indVarFirst=index(line(1:nLine),'[')
        indVarLast=index(line(1:nLine),']')
        call computeNumberOfData(line,inputVARIABLES%nData_bound_locations)
        allocate (inputVARIABLES%bound_locations(inputVARIABLES%nData_bound_locations))
        read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%bound_locations(1:inputVARIABLES%nData_bound_locations)
        !print *,'bound_locations: ', inputVARIABLES%bound_locations(1:inputVARIABLES%nData_bound_locations)
      end if
  
      ! read bound_values:
      iBound=1
      do while ( iBound.le.nData_bound_labels )
        iLine=iLine+1
        line=trim(lline(iLine))
        nLine=len_trim(line)
        indVar=index(line(1:nLine),'bound_values[')
        if ( indVar.eq.0 ) then
          print *,'ERROR: bound_values is not found in input file'
          inputVARIABLES%iError=1
          return
        else
          indVarLast=index(line(1:nLine),']')
          line=trim(line)
          line=line(indVarLast+1:nLine)
          indVarFirst=index(line(1:nLine),'[')
          indVarLast=index(line(1:nLine),']')
          call computeNumberOfData(line,nBoundValues)
          if ( iBound.eq.1 ) then
            allocate (inputVARIABLES%bound_values(nData_bound_labels,nBoundValues))
            allocate (inputVARIABLES%nData_bound_values(nData_bound_labels))
          end if
          read(line(indVarFirst+1:indVarLast-1),*) inputVARIABLES%bound_values(iBound,1:nBoundValues)
          !print *,'bound_values: ', inputVARIABLES%bound_values(iBound,1:nBoundValues)
        end if
        inputVARIABLES%nData_bound_values(iBound)=nBoundValues ! # data may be different for different boundaries
                                                ! so we need to store this
                                                ! information.
        iBound=iBound+1
      end do
  
  end subroutine readInputFile

end module
