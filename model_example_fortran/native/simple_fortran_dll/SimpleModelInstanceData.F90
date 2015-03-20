module m_model_instance

implicit none

! the parameters that may be affected by the filter
type t_parameters
    double precision          :: gravity
    double precision, &
        pointer, dimension(:) :: frictions
end type t_parameters

! the 'real' computed state
type t_real_state
    double precision, &
        pointer, dimension(:) :: waterlevels
end type t_real_state

! variables that don't belong to the state or the varied parameters, but are part of the instance
type t_pseudo_state
    double precision   :: current_time
    integer            :: outfile_handle
    integer            :: model_instance_id
end type t_pseudo_state

! all data needed when saving/restoring an instance
type t_model_instance
    type(t_parameters)   :: parameters
    type(t_real_state)   :: state
    type(t_pseudo_state) :: pseudo
end type t_model_instance

! actual model instances identification
integer :: model_instance_count = 0
integer :: model_instance_currently_in_memory = 0

contains

function mi_create(model_instance, dm_gridpoint_count) result(ret_val)

    ! return value
    integer :: ret_val  ! 0: success

    ! arguments
    type(t_model_instance), intent(inout) :: model_instance     ! model instance to be created
    integer               , intent(inout) :: dm_gridpoint_count ! #gridpoints
   
    ! body: allocate and intialize

    model_instance % parameters % gravity      = -1
    allocate(model_instance % parameters % frictions(dm_gridpoint_count))
    model_instance % parameters % frictions     = -1

    allocate(model_instance % state % waterlevels(dm_gridpoint_count))
    model_instance % state % waterlevels        = -1
    
    model_instance % pseudo % current_time      = -1
    model_instance % pseudo % outfile_handle    = -1
    model_instance % pseudo % model_instance_id = -1    

    ret_val = 0

end function mi_create


subroutine mi_destroy(model_instance)

    ! arguments
    type(t_model_instance), intent(inout) :: model_instance     ! model instance to be created
   
    ! body: cleanup

    if (associated(model_instance % parameters % frictions)) deallocate(model_instance % parameters % frictions)
    if (associated(model_instance % state % waterlevels)) deallocate(model_instance % state % waterlevels)

end subroutine  mi_destroy


function mi_write_to_file(model_instance, file_path) result(ret_val)

    ! return value
    integer :: ret_val  ! 0: success

    ! arguments
    type(t_model_instance), intent(inout) :: model_instance ! model in to be written
    character(len=*)      , intent(in)    :: file_path      ! file to write to
   
    ! locals
    integer :: instance_file_handle = 99
    integer :: stat

    ret_val = -1
    open(instance_file_handle, file=file_path, iostat=stat)
    if (stat==0) then
        write(instance_file_handle, *) 'gravity:'
        write(instance_file_handle, *) model_instance % parameters % gravity
        write(instance_file_handle, *) 'frictions:'
        write(instance_file_handle, *) model_instance % parameters % frictions
        write(instance_file_handle, *) 'water levels:'
        write(instance_file_handle, *) model_instance % state % waterlevels
        write(instance_file_handle, *) 'current time:'
        write(instance_file_handle, *) model_instance % pseudo % current_time
        write(instance_file_handle, *) 'out file handle:'
        write(instance_file_handle, *) model_instance % pseudo % outfile_handle
        write(instance_file_handle, *) 'instance id:'
        write(instance_file_handle, *) model_instance % pseudo % model_instance_id
        close(instance_file_handle)

        ret_val = 0
    endif

end function mi_write_to_file


function mi_read_from_file(model_instance, file_path) result(ret_val)

    ! return value
    integer :: ret_val  ! 0: success

    ! arguments
    type(t_model_instance), intent(inout) :: model_instance ! model in to be read
    character(len=*)      , intent(in)    :: file_path      ! file to read from
    
    ! locals
    integer :: instance_file_handle = 77
    integer :: stat
    character(len=255) :: dummy_string

    ret_val = -1

    open(instance_file_handle, file=file_path, status='old', iostat=stat)

    if (stat==0) then

        read(instance_file_handle, *) dummy_string
        read(instance_file_handle, *) model_instance % parameters % gravity
        read(instance_file_handle, *) dummy_string
        read(instance_file_handle, *) model_instance % parameters % frictions
        read(instance_file_handle, *) dummy_string
        read(instance_file_handle, *) model_instance % state % waterlevels
        read(instance_file_handle, *) dummy_string
        read(instance_file_handle, *) model_instance % pseudo % current_time
        read(instance_file_handle, *) dummy_string
        read(instance_file_handle, *) model_instance % pseudo % outfile_handle
        read(instance_file_handle, *) dummy_string
        read(instance_file_handle, *) model_instance % pseudo % model_instance_id
        close(instance_file_handle)

        ret_val = 0
    endif

end function mi_read_from_file

end module m_model_instance

