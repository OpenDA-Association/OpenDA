!!! Simple Model, meant as example for wrapping a model in OpenDA

module m_simple_model

implicit none

!! constants

! scalar exchange items
integer, parameter :: gravity               = 1

! exchange items on fill grid
integer, parameter :: waterlevel_on_grid    = 2
integer, parameter :: friction_on_grid      = 3

! exchange items on fill grid
integer, parameter :: discharge_on_laterals = 4

character(len=20), parameter, &
         dimension(4) :: exchange_item_text = (/ 'Gravity           '  , &
                                                 'Grid.Waterlevel   '   , &
                                                 'Grid.Friction     '     , &
                                                 'Laterals.Discharge'  /)

!!! The model, as it is in memory for one instance (partly static data, partly instance dependent data)

!! general schematization info
character(len=256) :: dm_schematization_file   ! file that that contained the schematization
integer            :: dm_gridpoint_count       ! # grid points
integer            :: dm_lateral_count         ! # lateral discharges

!! time info
double precision   :: dm_start_time_as_mjd     ! computation's start time   (modified julian day)
double precision   :: dm_end_time_as_mjd       ! computation's end time     (")
double precision   :: dm_delta_t_in_seconds    ! computation's delta t
double precision   :: dm_current_time          ! computation's current time (")
integer            :: dm_bc_timestep_count     ! # boundary condition time steps in full computation
integer            :: dm_bc_delta_t_in_seconds ! time step size for the boundary conditions
                                               ! (i.e. the discharges on the lateral inflows)

!! schematization variables and computed variables

! schematization variables that can not be varied (in this example)
double precision, pointer, &
   dimension(:)              :: dm_depths              ! depth on grid points

! schematization variables that may be varied
double precision             :: dm_gravity             ! the gravity constant
double precision, pointer, &
   dimension(:)              :: dm_frictions           ! friction on grid points
double precision, pointer, &
   dimension(:,:)            :: dm_discharges          ! discharges on lateral inflow (#timesteps * #laterals)

! computed variables
double precision, pointer, &
   dimension(:)              :: dm_waterlevels         ! water levels on grid points

integer                      :: dm_outfile_handle = 0  ! lun for output file

! model directories and files
character(len=256)  :: dm_model_parent_dir   ! parent directory for template model and all instances
character(len=256)  :: dm_template_model_dir ! template model that will be cloned for each instances

! actual model instance directories
character(len=256), dimension(:), pointer :: model_instance_dirs => NULL() ! a directory for each instances
integer, parameter                        :: instances_realloc_size = 5    ! #instances to be added when the max #instances has been exceed

! actual model instances identification
integer :: dm_max_dm_model_instance_count = 0  ! max #instances
integer :: dm_model_instance_count        = 0  ! actual #instance
integer :: dm_model_instance_in_memory    = 0  ! index of the instance currenty in memory


contains

!! Methods that can be accessed by the DLL-wrapper. For the method specifications see:
!!    org.openda.models.examples.dllmodel.SimpleModelDLL.ISimpleFortranNativeDLL

subroutine init(model_parent_dir, template_model_dir, schematization_file)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_init_' :: init
#endif

    ! arguments
    character(len=*), intent(in)  :: model_parent_dir    ! parent directory for model instances (full path)
    character(len=*), intent(in)  :: template_model_dir  ! directory name (under model_parent_dir) containing the model that will be cloned for each instance
    character(len=*), intent(in)  :: schematization_file ! schematization file (no path)


    ! body: nullify everything

    dm_model_parent_dir    = model_parent_dir
    dm_template_model_dir  = template_model_dir
    dm_schematization_file = schematization_file
    
    dm_start_time_as_mjd   = -1
    dm_end_time_as_mjd     = -1
    dm_delta_t_in_seconds  = -1
    dm_current_time        = -1

    dm_schematization_file   = ''
    dm_gridpoint_count       = 0
    dm_lateral_count         = 0
    dm_bc_delta_t_in_seconds = 0

    nullify(dm_frictions)
    nullify(dm_waterlevels)
    nullify(dm_discharges)

end subroutine init


subroutine destroy()
#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_initialize_' :: initialize
#endif
if (associated(dm_frictions)) deallocate(dm_frictions)
    if (associated(dm_frictions)) deallocate(dm_waterlevels)
    if (associated(dm_frictions)) deallocate(dm_discharges)
    call init('', '', '')
end subroutine destroy


function get_model_instance(instance_dir) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_model_instance_' :: get_model_instance
#endif

    ! arguments
    character(len=*)              :: instance_dir        ! model instance directory

    ! return value
    integer                       :: ret_val             ! >=0 : Success; <0 : Error

    ! locals
    integer                       :: i, t                ! counters
    character(len=256)            :: output_file_name

    ! body: create new model

    dm_model_instance_count = dm_model_instance_count + 1
    call check_model_instance_dirs_size()

    dm_model_instance_in_memory = dm_model_instance_count
    
    model_instance_dirs(dm_model_instance_count) = instance_dir

    ! Initialize the model data
        
    if (dm_model_instance_in_memory == 1) then

        ! First model instance.
        ! Initialize the part of model that will not be the same for every instance
        
        dm_start_time_as_mjd     = 55036.0D+0  ! e.g. July 24th, 2009, 00:00hr
        dm_end_time_as_mjd       = 55038.5D+0  ! e.g. July 26th, 2009, 12:00hr
        dm_delta_t_in_seconds    =  7200       ! 2 hours
        dm_bc_delta_t_in_seconds = 21600       ! 6 hours
        dm_bc_timestep_count     = (dm_start_time_as_mjd - dm_start_time_as_mjd) * &
                                    24.0D+0 * 3600.0D+0 / dm_bc_delta_t_in_seconds
        
        dm_gridpoint_count       = 4
        dm_lateral_count         = 2

        allocate(dm_depths(dm_gridpoint_count))
        allocate(dm_frictions(dm_gridpoint_count))
        allocate(dm_waterlevels(dm_gridpoint_count))
        allocate(dm_discharges(0:dm_bc_timestep_count, dm_lateral_count))

        do i = 1, dm_gridpoint_count
           dm_depths  (i) = i * 100
        enddo

    endif

    ! Initialize data that may be have been adjusted in the last model instance,
    ! and therefore has to be (re)initialized when creating

    dm_current_time = dm_start_time_as_mjd

    dm_gravity = 9.1D+0

    do i = 1, dm_gridpoint_count
       dm_frictions  (i) = i * 100
       dm_waterlevels(i) = i * 1
    enddo
    do i = 1, dm_lateral_count
       do t = 0, dm_bc_timestep_count
           dm_discharges(t,i) = i * 10
       enddo
    enddo

    ! open output file for this instance
    dm_outfile_handle = 100 + dm_model_instance_count
    output_file_name = trim(model_instance_dirs(dm_model_instance_count)) // '/model-output.txt'
    open(dm_outfile_handle, file=output_file_name)

    ! save the initial instance
    ret_val = save_instance(dm_model_instance_count)
    if (ret_val == 0) then
        ! return instance 'handle'
        ret_val = dm_model_instance_count
    endif

    write(dm_outfile_handle, '(3A,I2)') 'Initialize(', &
                trim(dm_schematization_file), '), ret_val: ', ret_val
    call flush(dm_outfile_handle)

end function get_model_instance


function save_instance(model_instance_id) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_save_instance_' :: save_instance
#endif

    use m_model_instance

    ! return value
    integer                       :: ret_val              ! ret_val < 0: Error; ret_val == 0 success
    ! arguments
    integer         , intent(in)  :: model_instance_id    ! model instance identifier

    ! locals
    integer                       :: instance_file_handle ! instance file handle
    type(t_model_instance)        :: model_instance

    ! body: save the state

    ret_val = -1
    
    if (valid_model_instance(model_instance_id)) then
    
        ! copy model instance data from data currently in memory
        ret_val = mi_create(model_instance, dm_gridpoint_count)
        if (ret_val == 0) then
            ret_val = get_model_instance_from_memory(model_instance)
            if (ret_val == 0) then
                ret_val = mi_write_to_file(model_instance, trim(model_instance_dirs(model_instance_id))//'/model-instance.txt')
            endif
        endif

    endif

    write(dm_outfile_handle, '(A,I2,A,I2)') 'save_instance(', &
                                      model_instance_id, '), retval: ', ret_val
    call flush(dm_outfile_handle)

end function save_instance


function restore_instance(model_instance_id) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_restore_instance_' :: restore_instance
#endif

    use m_model_instance

    ! return value
    integer                       :: ret_val              ! ret_val < 0: Error; ret_val == 0 success
    ! arguments
    integer         , intent(in)  :: model_instance_id    ! model instance identifier

    ! locals
    integer                       :: instance_file_handle ! instance file handle
    type(t_model_instance)        :: model_instance

    ! body: save the state

    ret_val = -1
    
    ! copy model instance data from data currently in memory
    ret_val = mi_create(model_instance, dm_gridpoint_count)
    if (ret_val == 0) then
        ret_val = mi_read_from_file(model_instance, trim(model_instance_dirs(model_instance_id))//'/model-instance.txt')
        if (ret_val == 0) then
            ret_val = set_model_instance_to_memory(model_instance)
            dm_model_instance_in_memory = model_instance_id
        endif
    endif

    write(dm_outfile_handle, '(A,I2,A,I2)') 'restore_instance(', model_instance_id, '), retval: ', ret_val
    call flush(dm_outfile_handle)

end function restore_instance


function get_start_time(start_time) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_start_time_' :: get_start_time
#endif

    ! return value
    integer :: ret_val

    ! arguments
    double precision, intent(out)   :: start_time ! start time of computation in MJD

    ! body

    start_time = dm_start_time_as_mjd
    ret_val = 0

    write(dm_outfile_handle, '(A,F8.2,A)') 'get_start_time(', start_time, ')'
    call flush(dm_outfile_handle)

end function get_start_time


function get_end_time(end_time) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_end_time_' :: get_end_time
#endif

    ! return value
    integer :: ret_val

    ! arguments
    double precision, intent(out)   :: end_time  ! end time of computation in MJD

    ! body

    end_time = dm_end_time_as_mjd
    ret_val = 0

    write(dm_outfile_handle, '(A,F8.2,A)') 'get_end_time(', end_time, ')'
    call flush(dm_outfile_handle)

end function get_end_time


function get_delta_t(delta_t) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_delta_t_' :: get_delta_t
#endif

    ! return value
    integer :: ret_val

    ! arguments
    double precision, intent(out)   :: delta_t  ! #delta t for computation, in MJD (i.e. in days)

    ! body

    delta_t = dm_delta_t_in_seconds / 3600.0D+0 / 24.0D+0
    ret_val = 0

    write(dm_outfile_handle, '(A,F8.4,A)') 'get_delta_t(', delta_t, ')'
    call flush(dm_outfile_handle)

end function get_delta_t


function get_current_time(instance, current_time) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_current_time_' :: get_current_time
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in)    :: instance      ! model instance
    double precision, intent(out)   :: current_time  ! current time in computation in MJD

    ! body

    current_time = dm_current_time
    ret_val = 0

    write(dm_outfile_handle, '(A,F8.2,A)') 'get_current_time(', current_time, ')'
    call flush(dm_outfile_handle)

end function get_current_time


function get_values_count_for_time_span(instance, exchange_item_id, bc_index, start_time, end_time) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_values_count_for_time_span_' :: get_values_count_for_time_span
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: instance         ! model instance
    integer         , intent(in) :: exchange_item_id ! type of time dependent boundary (e.g. discharge_on_laterals)
    integer         , intent(in) :: bc_index         ! index of boundary condition (e.g. discharge nr. 4)
    double precision, intent(in) :: start_time       ! start time of bc values
    double precision, intent(in) :: end_time         ! end time of bc values

    ! locals
    integer :: start_index   ! index in bc's time series values for start_time
    integer :: end_index     ! index in bc's time series values for end_time

    ! body

    ret_val = -1 ! indices not ok
    
    if (valid_model_instance(instance)) then

        if (check_bc_indices(exchange_item_id    , bc_index , &
                             start_time , end_time , &
                             start_index, end_index) ) then

            ret_val = end_index - start_index + 1

        endif

    endif

    if (ret_val /= 0) then
        write(dm_outfile_handle,'(A,I2)') 'Error in get_values_count_for_time_span: ', ret_val
    else
        write(dm_outfile_handle,'(3A,F8.2,A,F8.2,A,I2)') 'get_values_count_for_time_span(', &
                                    trim(exchange_item_text(exchange_item_id)),&
                                    ',', start_time, ',', end_time, '): ', ret_val
    endif
    call flush(dm_outfile_handle)

end function get_values_count_for_time_span


function get_values_for_time_span(instance, exchange_item_id, bc_index, start_time, end_time, values_count, values) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_values_for_time_span_' :: get_values_for_time_span
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: instance         ! model instance
    integer         , intent(in) :: exchange_item_id ! type of time dependent boundary (e.g. discharge_on_laterals)
    integer         , intent(in) :: bc_index         ! index of boundary condition (e.g. discharge nr. 4)
    double precision, intent(in) :: start_time       ! start time of bc values
    double precision, intent(in) :: end_time         ! end time of bc values
    integer         , intent(in) :: values_count     ! #values
    double precision, &
        dimension(values_count), &
                    intent(out)  :: values           ! returned values
    ! locals
    integer :: start_index   ! index in bc's time series values for start_time
    integer :: end_index     ! index in bc's time series values for end_time

    ! body

    ret_val = -1 ! indices not ok
    
    if (valid_model_instance(instance)) then

        if (check_bc_indices(exchange_item_id    , bc_index , &
                             start_time , end_time , &
                             start_index, end_index) ) then

            ret_val = 0

            if (exchange_item_id == discharge_on_laterals) then
                values = dm_discharges(start_index:end_index, bc_index)
            else
                ret_val = -2 ! unhandled item
            endif

        endif

    endif

    if (ret_val /= 0) then
        write(dm_outfile_handle,'(A,I2)') 'Error in get_values_for_time_span: ', ret_val
    else
        write(dm_outfile_handle,'(3A,F8.2,A,F8.2,A)') 'get_values_for_time_span(', &
                                    trim(exchange_item_text(exchange_item_id)),&
                                    ',', start_time, ',', end_time, '):'
        write(dm_outfile_handle,*) '   ', values
    endif
    call flush(dm_outfile_handle)

end function get_values_for_time_span


function set_values_for_time_span(instance, exchange_item_id, bc_index, start_time, end_time, values_count, values) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_set_values_for_time_span_' :: set_values_for_time_span
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: instance         ! model instance
    integer         , intent(in) :: exchange_item_id ! type of time dependent boundary (e.g. discharge_on_laterals)
    integer         , intent(in) :: bc_index         ! index of boundary condition (e.g. discharge nr. 4)
    double precision, intent(in) :: start_time       ! start time of bc values
    double precision, intent(in) :: end_time         ! end time of bc values
    integer         , intent(in) :: values_count     ! #values
    double precision, &
        dimension(values_count), &
                      intent(in) :: values           ! incoming values

    ! locals
    integer :: start_index   ! index in bc's time series values for start_time
    integer :: end_index     ! index in bc's time series values for end_time

    ! body

    ret_val = -1 ! indices not ok
    
    if (valid_model_instance(instance)) then

        if (check_bc_indices(exchange_item_id    , bc_index , &
                             start_time , end_time , &
                             start_index, end_index) ) then

            ret_val = 0

            if (exchange_item_id == discharge_on_laterals) then
                dm_discharges(start_index:end_index, bc_index) = values
            else
                ret_val = -2 ! unhandled item
            endif

        endif

    endif

    if (ret_val /= 0) then
        write(dm_outfile_handle,'(A,I2)') 'Error in set_values_for_time_span: ', ret_val
    else
        write(dm_outfile_handle,'(3A,F8.2,A,F8.2,A)') 'set_values_for_time_span(', &
                                    trim(exchange_item_text(exchange_item_id)),&
                                    ',', start_time, ',', end_time, '):'
        write(dm_outfile_handle,*) '   ', values
    endif
    call flush(dm_outfile_handle)

end function set_values_for_time_span


function get_values_count(exchange_item_id) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_values_count_' :: get_values_count
#endif

    ! return value
    integer :: ret_val    ! #value for a certain exchange item

    ! arguments
    integer, intent(in)   :: exchange_item_id  ! exchange item identifier

    ! locals
    integer :: start_index         ! start index in the model's array
    integer :: end_index           ! end index in the model's array

    ! body

    ret_val = -1
    
    start_index = -1
    end_index   = -1
    if (check_grid_indices(exchange_item_id, start_index, end_index) ) then
        ret_val = end_index - start_index + 1
    endif

    if (ret_val < 0) then
        write(dm_outfile_handle,'(A,I2)') 'Error in get_values_count: ', ret_val
    else
        write(dm_outfile_handle,'(3A,I2)') 'get_values_count(', &
                                    trim(exchange_item_text(exchange_item_id)), '): ', ret_val
    endif
    call flush(dm_outfile_handle)

end function get_values_count


function get_values(instance, exchange_item_id, start_index, end_index, values) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_get_values_' :: get_values
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer        , intent(in) :: instance            ! model instance
    integer        , intent(in) :: exchange_item_id    ! exchange item identifier
    integer        , intent(in) :: start_index         ! start index in the model's array
    integer        , intent(in) :: end_index           ! end index in the model's array
    double precision, &
        dimension(end_index - start_index + 1), &
                    intent(out) :: values             ! returned values
    ! locals
    integer :: loc_start_index ! local copy (needed for intent(inout) in check_grid_indices)
    integer :: loc_end_index   ! local copy (needed for intent(inout) in check_grid_indices)

    ! body

    ret_val = -1 ! indices not ok
    
    loc_start_index = start_index
    loc_end_index   = end_index

    if (valid_model_instance(instance) .and. &
        check_grid_indices(exchange_item_id, loc_start_index, loc_end_index) ) then

        ret_val = 0

        if (exchange_item_id == friction_on_grid ) then
            values = dm_frictions(start_index:end_index)
        else if (exchange_item_id == waterlevel_on_grid ) then
            values = dm_waterlevels(start_index:end_index)
        else if (exchange_item_id == gravity ) then
            values(1) = dm_gravity
        else
            ret_val = -2 ! unhandled item
        endif

    endif

    if (ret_val /= 0) then
        write(dm_outfile_handle,'(A,I2)') 'Error in get_values: ', ret_val
    else
        write(dm_outfile_handle,'(3A,I2,A,I2,A)') 'get_values(', &
                                 trim(exchange_item_text(exchange_item_id)), ',', &
                                 start_index, ',', end_index, '):'
        write(dm_outfile_handle,*) '   ', values
    endif
    call flush(dm_outfile_handle)

end function get_values


function set_values(instance, exchange_item_id, start_index, end_index, values) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_set_values_' :: set_values
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer        , intent(in) :: instance            ! model instance
    integer        , intent(in) :: exchange_item_id    ! exchange item identifier
    integer        , intent(in) :: start_index         ! start index in the model's array
    integer        , intent(in) :: end_index           ! end index in the model's array
    double precision, &
        dimension(end_index - start_index + 1), &
                     intent(in) :: values              ! incoming values
    ! locals
    integer :: loc_start_index ! local copy (needed for intent(inout) in check_grid_indices)
    integer :: loc_end_index   ! local copy (needed for intent(inout) in check_grid_indices)

    ! body

    ret_val = -1 ! indices not ok
    
    loc_start_index = start_index
    loc_end_index   = end_index

    if (valid_model_instance(instance) .and. &
        check_grid_indices(exchange_item_id, loc_start_index, loc_end_index) ) then

        ret_val = 0

        if (exchange_item_id == friction_on_grid ) then
            dm_frictions(start_index:end_index) = values
        else if (exchange_item_id == waterlevel_on_grid ) then
            dm_waterlevels(start_index:end_index) = values
        else if (exchange_item_id == gravity ) then
            dm_gravity = values(1)
        else
            ret_val = -2 ! unhandled item
        endif

    endif

    if (ret_val /= 0) then
        write(dm_outfile_handle,'(A,I2)') 'Error in set_values: ', ret_val
    else
        write(dm_outfile_handle,'(3A,I2,A,I2,A)') 'set_values(', &
                                 trim(exchange_item_text(exchange_item_id)), ',', &
                                 start_index, ',', end_index, '):'
        write(dm_outfile_handle,*) '   ', values
    endif
    call flush(dm_outfile_handle)

end function set_values


function compute(instance, from_time_stamp, to_time_stamp) result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_compute_' :: compute
#endif

    ! return value
    integer :: ret_val

    ! arguments
    integer         , intent(in) :: instance        ! model instance
    double precision, intent(in) :: from_time_stamp ! time stamp to compute from
                                                    ! (usually equals dm_current time, may for some models be back in time)
    double precision, intent(in) :: to_time_stamp   ! time stamp to compute to ( > from_time_stamp )

    ret_val = -1
    
    if (valid_model_instance(instance)) then
        dm_current_time = dm_current_time + dm_delta_t_in_seconds / 3600.0D+0 / 24.0D+0
        ret_val = 0
    endif

    write(dm_outfile_handle,'(A,F8.2,A,F8.2,A,I2)') 'compute(', from_time_stamp, ',', to_time_stamp, '): ', ret_val
    call flush(dm_outfile_handle)

end function compute


function finish() result(ret_val)

#if(defined(WIN32_IFORT))
    !DEC$ ATTRIBUTES DLLEXPORT, ALIAS : 'm_simple_model_mp_finish_' :: finish
#endif

    ! return value

    integer                       :: ret_val         ! >=0 : Success; <0 : Error

    write(dm_outfile_handle, '(A)') 'finish()'
    close(dm_outfile_handle)

    ret_val = 0

end function finish


function valid_model_instance(instance_id) result(success)

    ! return value
    logical :: success     ! .true.  instance_id ok.
                           ! .false. instance_id out of bounds, or not in memory

    ! arguments
    integer, intent(in)  :: instance_id          ! model instance id to be checked

    success = instance_id >= 1 .and. instance_id <= dm_model_instance_count &
              .and. instance_id == dm_model_instance_in_memory

end function valid_model_instance


function check_grid_indices(exchange_item_id, start_index, end_index) result(success)

    ! return value
    logical :: success     ! .true.  indices determined ok.
                           ! .false. indices out of bounds

    ! arguments
    integer, intent(in)   :: exchange_item_id ! required exchange item
    integer, intent(inout):: start_index      ! start index of values (-1: return actual value)
    integer, intent(inout):: end_index        ! end index of values   (-1: return actual value)

    ! locals
    integer :: values_count

    success = .false.

    if (exchange_item_id == waterlevel_on_grid .or. &
        exchange_item_id == friction_on_grid        ) then
        values_count = dm_gridpoint_count
    else if (exchange_item_id == discharge_on_laterals ) then
        values_count = dm_lateral_count
    else if (exchange_item_id == gravity ) then
        values_count = 1
    endif

    if (start_index == -1 .and. end_index == -1) then
        start_index = 1
        end_index   = values_count
        success = .true.
    else
        success = start_index >= 1 .and. start_index <= values_count .and. &
                  end_index   >= 1 .and. end_index   <= values_count .and. &
                  end_index >= start_index
    endif

end function check_grid_indices


function check_bc_indices(exchange_item_id, bc_index, start_time, end_time, start_index, end_index) result(success)

    ! return value
    logical :: success     ! .true.  indices determined ok.
                           ! .false. location_index out of bounds

    ! arguments
    integer         , intent(in)  :: exchange_item_id       ! type of boundary condition (discharge_on_laterals)
    integer         , intent(in)  :: bc_index      ! index of boundary condition
    double precision, intent(in)  :: start_time    ! start time of values to be gotten/set
    double precision, intent(in)  :: end_time      ! end time of values to be gotten/set
    integer         , intent(out) :: start_index   ! index in bc's time series values for start_time
    integer         , intent(out) :: end_index     ! index in bc's time series values for end_time

    ! locals
    double precision :: epsilon = 1.0D-8
    double precision :: bc_delta_t_as_MDD

    ! body
    success = .false.

    bc_delta_t_as_MDD = (dm_start_time_as_mjd - dm_end_time_as_mjd) / dm_bc_timestep_count

    if (exchange_item_id == discharge_on_laterals) then
        success = bc_index >= 1 .and. bc_index <= dm_lateral_count .and. &
                 start_time >= (dm_start_time_as_mjd - epsilon) .and. &
                 end_time <= (dm_end_time_as_mjd + epsilon) .and. &
                 end_time >= (start_time - epsilon)
                 
        if (success) then
            start_index = dint( (start_time - dm_start_time_as_mjd) / bc_delta_t_as_MDD )
            end_index   = dint( (end_time - dm_start_time_as_mjd) / bc_delta_t_as_MDD )
        endif
    endif

end function check_bc_indices


function get_model_instance_from_memory(model_instance) result(ret_val)

    use m_model_instance

    ! return value
    integer :: ret_val  ! 0: success

    ! arguments
    type(t_model_instance), intent(inout) :: model_instance ! model instance to be created
   
    ! body: fill structure

    model_instance % parameters % gravity       = dm_gravity
    model_instance % parameters % frictions     = dm_frictions

    model_instance % state % waterlevels        = dm_waterlevels

    model_instance % pseudo % current_time      = dm_current_time
    model_instance % pseudo % outfile_handle    = dm_outfile_handle
    model_instance % pseudo % model_instance_id = dm_model_instance_in_memory
    
    ret_val = 0

end function get_model_instance_from_memory


function set_model_instance_to_memory(model_instance) result(ret_val)

    use m_model_instance

    ! return value
    integer :: ret_val  ! 0: success

    ! arguments
    type(t_model_instance), intent(inout) :: model_instance ! model instance to be created
   
    ! body: fill structure

    ret_val = -1
    if ( size(model_instance % state % waterlevels) == dm_gridpoint_count .and. &
         size(model_instance % parameters % frictions) == dm_gridpoint_count) then

        dm_gravity               = model_instance % parameters % gravity
        dm_frictions             = model_instance % parameters % frictions

        dm_waterlevels           = model_instance % state % waterlevels

        dm_current_time          = model_instance % pseudo % current_time
        dm_outfile_handle        = model_instance % pseudo % outfile_handle
        dm_model_instance_in_memory = model_instance % pseudo % model_instance_id
        
        ret_val = 0

    endif

end function set_model_instance_to_memory

subroutine check_model_instance_dirs_size()
    ! locals
    character(len=256), dimension(:), pointer :: org_model_instance_dirs => NULL() ! current array of model instance dirs

    if (dm_model_instance_count > dm_max_dm_model_instance_count) then
       ! realloc
       org_model_instance_dirs => model_instance_dirs
       allocate(model_instance_dirs(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_dirs)) then
           model_instance_dirs(1:dm_max_dm_model_instance_count) = org_model_instance_dirs
           deallocate(org_model_instance_dirs)
       endif
       model_instance_dirs(dm_max_dm_model_instance_count+1:) = ' '
       dm_max_dm_model_instance_count = dm_max_dm_model_instance_count + instances_realloc_size
    endif

end subroutine check_model_instance_dirs_size

end module m_simple_model
