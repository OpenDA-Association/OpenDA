!!
!! test program for simple model
!!
!! This programs has to be run on a directory with 4 subdirectories
!! .\model
!! .\model1
!! .\model2
!! .\model3
!! (note: .\model has to be the working directory)
!!

program main

use m_simple_model

implicit none

!! variables
! #instances
integer, parameter :: instance_count = 3;

! model directories, handles, values
character(len=256), parameter      :: model_parent_dir    = '.'
                                                                                   ! parent for model instance directories
character(len=256), parameter      :: model_template_dir  = 'model'                ! template model directory with schem. file etc.
character(len=256), parameter      :: instance_dir_prefix = 'work'                 ! prefix for instance directory
character(len=256), parameter      :: schematization_file = 'mySchematization.txt' ! schem. file

integer, dimension(instance_count) :: mih                       ! model instance handles
double precision                   :: start_time, end_time      ! model time info
double precision                   :: current_time, delta_t     ! model time info
character(len=256)                 :: instance_dir              ! model instance directory

! various
integer                                 :: i                     ! instance counter
integer                                 :: ret_val, values_count ! return values of function calls
double precision, dimension(:), pointer :: values                ! values to get/set
double precision                        :: filter_delta_t        ! time interval between filter steps
double precision                        :: target_time           ! time to compute to
double precision                        :: time_epsilon          ! for modified julian time comparison

!! test

! initialize the DLL
call init(model_parent_dir, 'model', 'mySchematization.txt')

! initialize the model instances
do i = 1, instance_count

    write(instance_dir, '(3A,I1)') trim(model_parent_dir), '/', trim(instance_dir_prefix), i
    call make_dir(instance_dir)
    mih(i) = get_model_instance(instance_dir)
    call check_result('get_model_instance', ret_val)

enddo

! get time info
ret_val = get_start_time(start_time)
call check_result('get_start_time', ret_val)
ret_val = get_end_time(end_time)
call check_result('get_end_time', ret_val)
ret_val = get_delta_t(delta_t)
call check_result('get_delta_t', ret_val)

! filter every 4 model time steps
filter_delta_t = 4 * delta_t

! adjust the friction and the gravity
do i = 1, instance_count

    call start_instance_access(mih(i))

    values_count = get_values_count(gravity)
    call check_result('get_values_count gravity', values_count)
    if (values_count > 0) then
        allocate(values(values_count))
        ret_val = get_values(mih(i), gravity, 1, values_count, values)
        call check_result('get_values gravity', ret_val)
        if (ret_val == 0) then
            values = values + i * 1.0D-2
            ret_val = set_values(mih(i), gravity, 1, values_count, values)
            call check_result('set_values gravity', ret_val)
        endif
    endif
    
    values_count = get_values_count(friction_on_grid)
    call check_result('get_values_count friction_on_grid', values_count)
    if (values_count > 0) then
        allocate(values(values_count))
        ret_val = get_values(mih(i), friction_on_grid, 1, values_count, values)
        call check_result('get_values friction_on_grid', ret_val)
        if (ret_val == 0) then
            values = values + i * 1000000.0D+0
            ret_val = set_values(mih(i), friction_on_grid, 1, values_count, values)
            call check_result('set_values friction_on_grid', ret_val)
        endif
    endif
    
    ! TODO extend example with time dependent exchange times

    call end_instance_access(mih(i))

enddo

! check friction and gravity
do i = 1, instance_count

    call start_instance_access(mih(i))

    values_count = get_values_count(gravity)
    call check_result('get_values_count gravity', values_count)
    if (values_count > 0) then
        allocate(values(values_count))
        ret_val = get_values(mih(i), gravity, 1, values_count, values)
        call check_result('get_values gravity', ret_val)
        if (ret_val == 0) then
            write(*,*) mih(i), ' gravity: ', values(1)
        endif
    endif
    
    values_count = get_values_count(friction_on_grid)
    call check_result('get_values_count friction_on_grid', values_count)
    if (values_count > 0) then
        allocate(values(values_count))
        ret_val = get_values(mih(i), friction_on_grid, 1, values_count, values)
        call check_result('get_values friction_on_grid', ret_val)
        if (ret_val == 0) then
            write(*,*) mih(i), ' friction: '
            write(*,*) values
        endif
    endif
    
    ! TODO extend example with time dependent exchange times

    call end_instance_access(mih(i))

enddo

! start time loop

target_time = start_time

do while (target_time + time_epsilon <  end_time)

    do i = 1, instance_count
        
        call start_instance_access(mih(i))

        ! adjust the first discharge
        ! TODO

        ! compute
        ret_val = get_current_time(mih(i), current_time)
        call check_result('get_current_time', ret_val)

        ret_val = compute(mih(i), current_time, target_time)
        call check_result('compute', ret_val)

        ! get results
        ! TODO

        call end_instance_access(mih(i))

    enddo

    target_time = target_time + filter_delta_t

enddo

end program


subroutine start_instance_access(model_instance)
    use m_simple_model
    implicit none
    ! arguments
    integer                       :: model_instance           ! model instance handle
    ! locals
    integer                       :: ret_val                  ! return value of function call
    ! select model instance
    ret_val = restore_instance(model_instance)
    call check_result('restore_instance', ret_val)
end subroutine start_instance_access


subroutine end_instance_access(model_instance)
    use m_simple_model
    implicit none
    ! arguments
    integer                       :: model_instance           ! model instance handle
    ! locals
    integer                       :: ret_val                  ! return value of function call
    ! save model instance
    ret_val = save_instance(model_instance)
    call check_result('save_instance', ret_val)
end subroutine end_instance_access


subroutine check_result(called_function_name, result_of_call)
    implicit none
    ! arguments
    character(len=*), intent(in) :: called_function_name
    integer         , intent(in) :: result_of_call

    if (result_of_call < 0) then
        write(*,'(3A,I2)') 'Error in ', called_function_name, ', result of call: ', result_of_call
        stop
    endif

end subroutine check_result


subroutine make_dir (dir)

    ! arguments
    character(Len=*)   :: dir   ! directory to be created

    ! locals
    integer*2          :: result_of_call
    character(Len=512) :: command

    command = 'mkdir ' // '"'// trim(dir) // '"'     
    result_of_call = system(command)
    if (result_of_call < 0) then
        write(*,'(A, I5)') 'Error in make_dir, result of call: ', result_of_call
        stop
    endif

end subroutine make_dir


