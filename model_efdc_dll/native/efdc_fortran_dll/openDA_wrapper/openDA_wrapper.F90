!! MOD_V2.0
!! Copyright (c) 2012 OpenDA Association
!! All rights reserved.
!!
!! This file is part of OpenDA.
!!
!! OpenDA is free software: you can redistribute it and/or modify
!! it under the terms of the GNU Lesser General Public License as
!! published by the Free Software Foundation, either version 3 of
!! the License, or (at your option) any later version.
!!
!! OpenDA is distributed in the hope that it will be useful,
!! but WITHOUT ANY WARRANTY; without even the implied warranty of
!! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!! GNU Lesser General Public License for more details.
!!
!! You should have received a copy of the GNU Lesser General Public License
!! along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
 
!! @author Werner Kramer       VORtech BV  
!!                             P.O. Box 260
!!                             2600 AG Delft
!!                             The Netherlands
!!                             www.vortech.nl


! ----------------------------------------------------------------------------
! This module provides the functions which are called by EfdcDLL.java.
! 
! It contains the methods that are required for the IModelInstance and 
! IExchangeItem interfaces. For more information see  
! org.openda.model_efdc_dll.IEfdcFortranNativeDLL
! 
! Each model instance has it own memory storing the state and time series.
! If an instance becomes active this state and time series is copied to EFDC
! memory. When a model integration is finished the calculated state is again
! stored in model instance memory. 
! 
! Exchange Items communicate with the model instance memory not directly with 
! EFDC memory. 
! ----------------------------------------------------------------------------
module m_openda_wrapper
 
  use, intrinsic ::iso_c_binding
  use model_exchange_items
  use model_state
  use model_aser_time_series
  use model_wser_time_series
  use model_pser_time_series
  use model_qser_time_series
  use model_cser_time_series
  use model_gateser_time_series
  use chdir_mod
  
  implicit none

  !private

  ! model directories and files
  integer, parameter :: dp=8
  integer, parameter :: max_path_length = 260  ! maximum path length 
  character(len=max_path_length) :: dm_model_parent_dir   ! parent directory for template model and all instances
  character(len=max_path_length) :: dm_template_model_dir ! template model that will be cloned for each instances
  character(len=max_path_length), dimension(:), pointer :: model_instance_dirs => NULL() ! a directory for each instances
  integer, parameter :: max_message_length = 250  ! maximum message length 
  integer, dimension(:), pointer :: dm_outfile_handle
  integer, parameter :: dm_general_log_handle = 100
  integer, parameter :: message_file_handle   = 40
  integer, parameter :: instances_realloc_size = 6    ! #instances to be added when the max #instances has been exceed
  real(kind=dp) :: missing_value = -9999.d0
  
  ! error message levels (analog to log4j )
  integer, parameter :: M_TRACE = 1
  integer, parameter :: M_DEBUG = 2
  integer, parameter :: M_INFO =  3
  integer, parameter :: M_WARNING = 4
  integer, parameter :: M_ERROR = 5
  integer, parameter :: M_FATAL = 6
  
  ! actual model instances identification
  integer :: dm_max_dm_model_instance_count = 0  ! max #instances
  integer :: dm_model_instance_count = 0  ! actual #instance
  integer :: dm_model_instance_in_memory = 0 ! index of the instance currenty in memory

  logical, parameter :: debug = .false.
  logical :: ATM_WARNING_REQUIRED = .true.
  
contains


  ! --------------------------------------------------------------------------
  ! Initialize the dll, change to model directory
  ! Initialize EFDC model
  ! Set active exchange items for CSER time series 
  ! --------------------------------------------------------------------------
  function  m_openda_wrapper_init_(parent_directory_c, template_directory_c)&
    result(ret_val)&
    bind(C,name='m_openda_wrapper_init_')

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_init_
#endif

    use omp_lib
    USE GLOBAL, only: TBEGIN, TCON, TIDALP, NTC, TIMEDAY, & 
         NDASER, NASERM, NDPSER, NPSERM, NDQSER, NQSERM,NDCSER, NCSERM, &
         NTOX, NSED, NSND, NWQV, NTHDS, NDQCLT, NQCTLM, &
         NDWSER, NWSERM

    ! arguments
    character(kind=c_char), intent(in)  :: parent_directory_c(*) ! parent directory for model instances (full path)
    character(kind=c_char), intent(in)  :: template_directory_c(*) ! directory name (under model_parent_dir) containing the model that will be cloned for each instance

    ! return value
    integer(kind=c_int) :: ret_val     ! ret_val < 0: Error; ret_val == 0 success
        
    !locals
    character(len=max_path_length) :: output_file_name, message_file_name
    character(len=max_path_length) :: cwd
    character(len=max_path_length) :: parent_directory
    character(len=max_path_length) :: template_directory
    character(len=max_message_length) :: message
    integer :: i_number
    integer :: i
    logical :: i_open
    
    ! body
    ret_val = -1

    ret_val = c_to_f_string(parent_directory_c, parent_directory)
    if (ret_val /= 0) then 
        print*, "ERROR: maximum path length exceeded for ", parent_directory
        return
    end if
    ret_val = c_to_f_string(template_directory_c, template_directory)
    if (ret_val /= 0) then 
        print*, "ERROR: maximum path length exceeded for ", template_directory
        return
    end if
    
    dm_model_parent_dir    = trim(parent_directory)
    dm_template_model_dir  = trim(template_directory)

    print*, trim(dm_model_parent_dir)
    output_file_name = trim(dm_model_parent_dir) // '/model.log'
    message_file_name = trim(dm_model_parent_dir) // '/messages.log'
    
    ! create new model.log
    inquire(file = output_file_name, opened=i_open, number=i_number) 
    if (i_open .and. (i_number == dm_general_log_handle)) close(i_number)
    open(dm_general_log_handle, file=output_file_name, status = 'replace')
    write(dm_general_log_handle,'(A)') 'EFDC initialized'

    ! create new messages.log
    inquire(file = message_file_name, opened=i_open, number=i_number) 
    if (i_open .and. (i_number == message_file_handle)) close(i_number)
    open(message_file_handle, file=message_file_name, status = 'replace')
    
    message = "Starting EFDC run"
    call write_message(message, M_INFO)

    ! Pause or sleep for attaching debugger
    !message = "Starting paused"
    !call write_message(message, M_DEBUG) 
    !pause
    !message = "Sleeping for 20 seconds, please attach debugger"
    !call sleep(20)
    !call write_message(message, M_DEBUG)

    write(dm_general_log_handle,*) "parent directory: ", trim(dm_model_parent_dir)
    write(dm_general_log_handle,*) "model template directory: ", trim(dm_template_model_dir) 
    call getcwd(cwd)
    ret_val = change_directory(dm_template_model_dir)
    if (ret_val == 0 ) then
        nthds = 1
#ifdef _OPENMP
        nthds=omp_get_max_threads()
        write(dm_general_log_handle,*) "running with OpenMP threads: ", nthds 
#endif
        write(dm_general_log_handle,*) "initialize model"
        call model_init
        ret_val = change_directory(cwd)
    end if
    if (ret_val == 0 ) then
        write(dm_general_log_handle,'(A, I2)') "integer kind: ", kind(NTC)
        write(dm_general_log_handle,'(A, I2)') "real    kind: ", kind(TIDALP)    
        TIMEDAY = TBEGIN* TCON / 86400.d0

        ! store sizes of time series (the global ones are redetermined each time we do a restart)
        ndaser_max = NDASER
        naser_max = NASERM
        ndwser_max = NDWSER
        nwser_max = NWSERM
        ndpser_max = NDPSER
        npser_max = NPSERM
        ndqser_max = NDQSER
        nqser_max = NQSERM
        ndcser_max = NDCSER
        ncser_max = NCSERM
        NDQCLT_max = NDQCLT
        NQCTLM_max = NQCTLM
    
        NC_tox_start = 4
        NC_wq_start = NC_tox_start+NTOX+NSED+NSND
        NC_xspecies_start = NC_wq_start+NWQV
        
    end if
    if (ret_val == 0 ) ret_val = model_exchange_items_setup(dm_general_log_handle)

    call flush(dm_general_log_handle)
    
  end function  m_openda_wrapper_init_

  ! --------------------------------------------------------------------------
  ! If all instances are finished this subroutine is called to  
  ! deallocate pointers to storage of model instances 
  ! and reset some module variables.
  ! --------------------------------------------------------------------------=
  subroutine m_openda_wrapper_destroy_()&
    bind(C,name="m_openda_wrapper_destroy_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_destroy_
#endif
    integer :: i_number
    integer :: ret_val
    logical :: i_open

    ret_val = model_exchange_items_destroy()
    if (ret_val /= 0) write(dm_general_log_handle,'(A, I2)') "Error deallocating exchange items, ret_val: ", ret_val
    
    deallocate(model_instance_dirs)
    deallocate(dm_outfile_handle)
    deallocate(state)
    deallocate(aser)
    deallocate(psert)
    deallocate(qsert)
    deallocate(csert)
    deallocate(gateser)
    
    ! close open file handles
    inquire(file = "calheat.dia", opened=i_open, number=i_number) 
    if (i_open .and. (i_number == 77)) close(i_number)

    ! reset variables related to number of instance 
    dm_max_dm_model_instance_count = 0
    dm_model_instance_count = 0  ! actual #instance
    dm_model_instance_in_memory = 0 ! index of the instance currenty in memory

    write(dm_general_log_handle,'(A)') 'EFDC destroy()'
    close(dm_general_log_handle)
    close(message_file_handle)
    
  end subroutine m_openda_wrapper_destroy_

  ! --------------------------------------------------------------------------
  ! Create storage for a new model instance
  ! Get initial state and time series from EFDC memory 
  ! If no compute has been called on a other instance this corresponds with 
  ! the state that is specified by EFDC setting files in the model directory.
  ! Return identifier for new model instance  
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_model_instance_(instance_dir_c)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_model_instance_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_model_instance_
#endif

    use global, only: NDASER, NASERM, NDWSER, NWSERM, NDPSER, NPSERM, NDQSER, NQSERM, &
        KCM, NDCSER, NCSERM, TBEGIN, TCON, NTC, TIDALP,  NDQCLT, NQCTTM, &
        HUPG_HDWG_INITIALIZED
    
    ! arguments
    character(kind=c_char), intent(in)  :: instance_dir_c(*) ! model instance directory
    
    ! return value
    integer(kind=c_int)                      :: ret_val            ! >0 : instanceID ; <0 : Error

    ! locals
    character(len=max_path_length) :: output_file_name
    character(len=max_path_length) :: instance_dir
    character(len=3) :: cnumber
    integer :: instance
    integer :: i_number
    logical :: i_open

    ! body: create new model
    ret_val = -1
    ret_val = c_to_f_string(instance_dir_c, instance_dir)
    if (ret_val /= 0) then
        write(dm_general_log_handle,*) "ERROR: maximum path length exceeded for ", instance_dir
        return
    end if

    dm_model_instance_count = dm_model_instance_count + 1
    instance = dm_model_instance_count
    
    call add_instance_storage()
    model_instance_dirs(instance) = trim(instance_dir)
    ! open output file for this instance (and check if the same file is open from a previous FEWS run) 
    dm_outfile_handle(instance) = 100 + instance

    write(cnumber,'(I0.3)') instance
    output_file_name = trim(dm_model_parent_dir) // '/instance' // cnumber //'.log'
    write(dm_general_log_handle,'(A,A,A,I2)') "opening logfile ", trim(output_file_name) , " for instance ", instance 
    inquire(file = output_file_name, opened=i_open, number=i_number)
    if (i_open) close(i_number)
    open(dm_outfile_handle(instance), file=output_file_name, status = 'replace') 
    
    ! add wrapper storage for instance
    call model_state_allocate(instance)
    call model_aser_allocate(instance, NDASER, NASERM) 
    call model_wser_allocate(instance, NDWSER, NWSERM)
    call model_pser_allocate(instance, NDPSER, NPSERM)
    call model_qser_allocate(instance, NDQSER, KCM, NQSERM)
    call model_cser_allocate(instance, NDCSER, KCM, NCSERM)        
    call model_gateser_allocate(instance, NDQCLT, NQCTTM)   
    
    if (debug) write(dm_outfile_handle(instance),'(A,I3)' ) 'allocated state vector for instance ', instance  

    ! Initialize data that may be have been adjusted in the last model instance,
    ! and therefore has to be (re)initialized when creating
    ret_val =  model_get_state(instance)
    if (debug) write(dm_outfile_handle(instance),'(A)') 'got model state'
    if (ret_val == 0) ret_val = model_get_aser(instance)
    if (ret_val == 0) ret_val = model_get_daily_solar_intensity(instance, .true.)
    if (ret_val == 0) ret_val = model_get_wser(instance)
    if (ret_val == 0) ret_val = model_get_pser(instance)
    if (ret_val == 0) ret_val = model_get_qser(instance)
    if (ret_val == 0) ret_val = model_get_cser(instance)
    if (ret_val == 0) ret_val = model_get_gateser(instance)
    if (debug) write(dm_outfile_handle(instance),'(A)') 'got time series'

    ! store begin and end time for instance
    ! round to minutes as real precision is not accurate enough for seconds
    state(instance)%start_time = dble(nint(TBEGIN*TCON/60))/1440.d0
    state(instance)%end_time  = dble(nint((TBEGIN*TCON + TIDALP*NTC)/60))/1440.d0

    ! Reset gate control
    HUPG_HDWG_INITIALIZED = .true.

    
    ! save the initial instance
    !ret_val = save_instance(dm_model_instance_count)
    if (ret_val == 0) dm_model_instance_in_memory = instance

    if (ret_val == 0) then
       ! return instance 'handle'
       ret_val = instance
    endif

    write(dm_outfile_handle(instance), '(A,I2,A,I2)' ) &
         'Initialize #', instance, &
         ' ret_val: ', ret_val
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_model_instance_

  ! --------------------------------------------------------------------------
  ! Subroutine for saving the state that is currently in EFDC memory
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_save_instance_(instance)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_save_instance_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_save_instance_
#endif

    ! return value
    integer(kind=c_int)              :: ret_val     ! ret_val < 0: Error; ret_val == 0 success
    ! arguments
    integer(kind=c_int), intent(in)  :: instance    ! model instance identifier

    !local
    character(len=max_path_length) :: cwd
    
    ! body: save the state

    ret_val = -1
    if (valid_model_instance(instance)) then

       call getcwd(cwd)
       ret_val = change_directory(dm_template_model_dir)
       if (ret_val == 0 ) then
           if (debug) write(dm_outfile_handle(instance),'(A,I3)') 'getting model state for id', instance 
           ! copy model instance data from data currently in memory
           ret_val = model_get_state(instance)
           if (ret_val == 0) ret_val = model_get_daily_solar_intensity(instance, .false.)
           if (ret_val == 0) ret_val = model_get_gateser(instance)
           IF(DEBUG)CALL DEPPLT
           if (ret_val == 0) ret_val = change_directory(cwd)
       end if
    endif

    write(dm_outfile_handle(instance), '(A,I4,A,I2)') 'save_instance( instance: ', &
         instance, '), retval: ', ret_val
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_save_instance_


  ! --------------------------------------------------------------------------
  ! Subroutine for restoring the state and time series from model instance
  ! memory to EFDC memory.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_restore_instance_(instance)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_restore_instance_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_restore_instance_
#endif

    ! return value
    integer(kind=c_int)               :: ret_val     ! ret_val < 0: Error; ret_val == 0 success
    ! arguments
    integer(kind=c_int) , intent(in)  :: instance    ! model instance identifier

    !local
    character(len=max_path_length) :: cwd
    character(len=max_message_length) :: message
    
    ! body: save the state

    ret_val = -1
    ! copy model instance data from data currently in memory
    call getcwd(cwd)
    ret_val = change_directory(model_instance_dirs(instance))
    if (ret_val == 0) ret_val = model_set_state(instance)
    if (ret_val == 0) ret_val = model_set_aser(instance)
    if (ret_val == 0) ret_val = model_set_daily_solar_intensity(instance)
    if (ret_val == 0) ret_val = model_set_pser(instance)
    if (ret_val == 0) ret_val = model_set_qser(instance)
    if (ret_val == 0) ret_val = model_set_cser(instance)
    if (ret_val == 0) ret_val = model_set_gateser(instance)
    if (ret_val == 0) dm_model_instance_in_memory = instance
    if (ret_val == 0) ret_val = change_directory(cwd)
    if (debug) write(dm_outfile_handle(instance),'(A,I3)') 'current instance in memory',  dm_model_instance_in_memory

    write(dm_outfile_handle(instance), '(A,I2,A,I2)') 'restore_instance( instance: ', instance, '), retval: ', ret_val
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_restore_instance_

  ! --------------------------------------------------------------------------
  ! Write the restart files for the currently active instance to the instance
  ! directory. 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_store_current_instance_restart_files_() &
    result (ret_val)&
    bind(C, name="m_openda_wrapper_store_current_instance_restart_files_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_store_current_instance_restart_files_
#endif

    use global, only : ISTRAN, IWQRST, IWQBEN, ISMRST

    ! return value
    integer(kind=c_int) :: ret_val ! ret_val < 0: Error; ret_val == 0 success
    !locals
    character(len=max_path_length) :: cwd
    integer :: instance

    ret_val = -1

    instance = dm_model_instance_in_memory
    ! change directory
    call getcwd(cwd) 
    if (debug) write(dm_outfile_handle(instance),'(A,A)') "working directory ", trim(cwd) 
    ret_val = change_directory(model_instance_dirs(dm_model_instance_in_memory))
    if (ret_val == 0) then
        if (debug) write(dm_outfile_handle(instance),'(A,A)') "changing directory to ", model_instance_dirs(instance)  
        call RESTOUT(0)
        IF(ISTRAN(8).GE.1)THEN  
           IF(IWQRST.EQ.1) CALL WWQRST(0)
           IF(IWQBEN.EQ.1 .AND. ISMRST.EQ.1) CALL WSMRST(0)
        ENDIF
    end if
    ! return to old working directory
    
    if (debug) write(dm_outfile_handle(instance),'(A,A)') "changing directory to ", trim(cwd)
    if (ret_val == 0) ret_val = change_directory(cwd)

    
    write(dm_outfile_handle(instance), '(A,I2)') 'store_current_instance_restart_files() retval: ', ret_val
    write(dm_outfile_handle(instance), '(A,A)') 'instance: ', instance
    write(dm_outfile_handle(instance), '(A,A)') 'instance dir: ', trim(model_instance_dirs(instance))
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_store_current_instance_restart_files_

  ! --------------------------------------------------------------------------
  ! Read the restart files from the model instance directory for the currently
  ! active instance to EFDC memory and save a copy in model instance memory.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_select_instance_from_restart_files_(instance) &
    result (ret_val)&
    bind(C, name="m_openda_wrapper_select_instance_from_restart_files_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_select_instance_from_restart_files_
#endif

    use global, only : ISTRAN, IWQRST, IWQBEN, ISMRST, ISRESTI, TIMESEC, TIMEDAY, TBEGIN, IWQAGR, HP

    
    ! return value
    integer(kind=c_int) :: ret_val              ! ret_val < 0: Error; ret_val == 0 success

    ! argument
    integer(kind=c_int), intent(in) :: instance ! model instance identifier

    !locals
    character(len=max_path_length) :: cwd
    character(len=80) :: TITLE
    
    ret_val = -1
    ! change directory
    if (valid_model_instance(instance)) then
       call getcwd(cwd)
       ret_val = change_directory( model_instance_dirs(instance) )
       if (ret_val == 0) then
           ! zero all arrays
           call VARZEROReal
           call VARZEROInt

           call INPUT(TITLE)
           ! Act like this is a restart
           ISRESTI = 1
           call model_init_2    

           if (debug) write(dm_outfile_handle(instance),'(A,A)') & 
             "select_instanc_from_restart_file ", model_instance_dirs(instance)
           !read restart files directly into EFDC memory

           ! time book keeping
           TIMESEC = state(instance)%TIMESEC
           TIMEDAY = TIMESEC / 86400.0
           TBEGIN = state(instance)%TBEGIN
       
           call RESTIN1
           call model_init_3
           ret_val = 0
       end if
       
       ! store restored state in instance storage
       if (ret_val == 0) ret_val =  model_get_state(instance)
       if (ret_val == 0) ret_val = model_get_daily_solar_intensity(instance, .true.)
       ! reset latest used forcings
       if (ret_val == 0)  ret_val = model_set_aser(instance)
       if (ret_val == 0)  ret_val = model_set_daily_solar_intensity(instance)
       if (ret_val == 0)  ret_val = model_set_pser(instance)
       if (ret_val == 0)  ret_val = model_set_qser(instance)
       if (ret_val == 0)  ret_val = model_set_cser(instance)
       if (ret_val == 0)  ret_val = model_set_gateser(instance)
       
       dm_model_instance_in_memory = instance
       if (ret_val == 0) ret_val = change_directory(cwd)
       if (debug) write(dm_outfile_handle(instance),'(A,A)') "select_instance_from_restart_file ",  dm_template_model_dir
    end if
    
    write(dm_outfile_handle(instance), '(A,I2,A,I2)') & 
      'select_instance_from_restart_files( instance: ', instance, '), retval: ', ret_val
    write(dm_outfile_handle(instance), '(A,A)') & 
      "changing directory to", trim(model_instance_dirs(instance))
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_select_instance_from_restart_files_

  ! --------------------------------------------------------------------------
  ! Get the reference year as specified in EVENT_TOX2.INP in the model 
  ! instance directory
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_reference_year_(instance)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_reference_year_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_reference_year_
#endif

    ! return value
    integer(kind=c_int) :: ret_val  ! reference_year on succes, -1 = error

    ! argument
    integer(kind=c_int), intent(in) :: instance ! model instance directory

    integer :: YEAR, MONTH, DAY, HR, MN, status
    character(len=max_path_length) :: cwd
    character(len=max_message_length) :: message
    
    ret_val = -1
    if (instance > dm_model_instance_count .or. instance < 1) then
        write(dm_general_log_handle,'(A,I3)') 'instance id out of range:', instance
        write(message,'(A,I3)') 'instance id out of range:', instance
        call write_message(message, M_FATAL)
        call exit(-1)    
    end if
    
    if (instance > dm_model_instance_count .or. instance < 1) then
        write(dm_general_log_handle,'(A,I3)') 'instance id out of range:', instance  
        call exit(-1)    
    end if
    
    call getcwd(cwd)
    status = change_directory( model_instance_dirs(instance) )
    if (status == 0) then
        OPEN(11,FILE='EVENT_TOX2.INP',STATUS='OLD')  
        READ(11,*) YEAR,MONTH,DAY, HR, MN  ! MODEL START TIME    
        CLOSE(11)
        if (debug) write(dm_outfile_handle(instance),'(A,I4)') "Reference year = ",  YEAR
        ret_val = YEAR
    end if
    status =  change_directory(cwd )
    write(dm_outfile_handle(instance), '(A,I4,A,I4, A, I2)') & 
      'get_reference_year( instance: ', instance, '): ', ret_val, 'ret_val: ', status
    call flush(dm_outfile_handle(instance))
    
  end function m_openda_wrapper_get_reference_year_

  ! --------------------------------------------------------------------------
  ! Get the layer depths
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_layer_depths_(instance, depths)&
    result(ret_val)&
    bind(C, name="m_openda_wrapper_get_layer_depths_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_layer_depths_
#endif
    
    use global, only : DZC, KC

    ! return value
    integer(kind=c_int) :: ret_val  ! 0 = succes, -1 = error

    ! argument
    integer(kind=c_int), intent(in) :: instance ! model instance directory
    real(kind=c_double), intent(out), dimension(KC) :: depths ! on exit layer depths

    !local
    character(len=max_message_length) :: message
    

    ret_val = -1
    if (instance > dm_model_instance_count .or. instance < 1) then
        write(dm_general_log_handle,'(A,I3)') 'instance id out of range:', instance
        write(message,'(A,I3)') 'instance id out of range:', instance
        call write_message(message, M_FATAL)
        call exit(-1)    
    end if
    
    if (instance > dm_model_instance_count .or. instance < 1) then
        write(dm_general_log_handle,'(A,I3)') 'instance id out of range:', instance  
        call exit(-1)    
    end if
    
    depths = real(DZC(1:KC),c_double)
    ret_val = 0
    
    write(dm_outfile_handle(instance), '(A,I4,A,I4, A)') & 
      'get_layer_depths( instance: ', instance, '): ', ret_val, 'depths: '
    write(dm_outfile_handle(instance), *)  depths
    call flush(dm_outfile_handle(instance))
    
  end function m_openda_wrapper_get_layer_depths_
  
  ! --------------------------------------------------------------------------
  ! Get the model instance start time. The start time is saved per model 
  ! instance when the instance is initialized (get_model_instance).
  ! The model start time is from EVENT_TOX2.INP in the instance directory.
  ! Time is in days since the first day at 00:00 of the reference year 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_start_time_(instance, start_time)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_start_time_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_start_time_
#endif

    use global, only : tbegin, tcon

    ! return value
    integer(kind=c_int) :: ret_val   ! if succes ret_val=0 

    ! arguments
    integer(kind=c_int), intent(in) :: instance             ! model instance identifier
    real(kind=c_double), intent(out) :: start_time ! start time of computation in days
    
    ! body
    ret_val = -1
    start_time =  state(instance)%start_time 
    ret_val = 0

    write(dm_outfile_handle(instance), '(A,I4,A,F14.10,A)')  &
      'get_start_time( instance: ', instance, ', start_time: ', start_time, ')'
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_start_time_

  ! --------------------------------------------------------------------------
  ! Get the model instance end time. The start time is saved per model 
  ! instance when the instance is initialized (get_model_instance)
  ! The model end time is from EVENT_TOX2.INP in the instance directory.
  ! Time is in days since the first day at 00:00 of the reference year  
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_end_time_(instance, end_time)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_end_time_")

    use global, only : nts, ntc, ntc1, tbegin, tcon, tidalp

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_end_time_
#endif

    ! return value
    integer(kind=c_int) :: ret_val ! if succes ret_val=0

    ! arguments
    integer(kind=c_int) :: instance
    real(kind=c_double), intent(out)   :: end_time  ! end time of computation in days

    ! body
    end_time = real(state(instance)%end_time,c_double)
    ret_val = 0

    write(dm_outfile_handle(instance), '(A,I4,A,F14.10,A)') & 
      'get_end_time( instance: ', instance , ', end_time: ', end_time, ')'
    call flush(dm_outfile_handle(instance))

    end function m_openda_wrapper_get_end_time_
  ! --------------------------------------------------------------------------
  ! Get the model instance time step in days 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_delta_t_(delta_t)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_delta_t_")

    use global, only: dt

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_delta_t_
#endif

    ! return value
    integer(kind=c_int) :: ret_val ! if succes ret_val=0

    ! arguments
    real(kind=c_double), intent(out)   :: delta_t  ! #delta t for computation, in MJD (i.e. in days)

    ! body
    delta_t =  real(dt,c_double) / 86400.0d0 ! converted to days
    ret_val = 0

    write(dm_general_log_handle, '(A,ES12.4E2,A)') 'get_delta_t( delta_t: ', delta_t, ')'
    call flush(dm_general_log_handle)

  end function m_openda_wrapper_get_delta_t_

  ! --------------------------------------------------------------------------
  ! Get the model reference period.
  ! The reference period is used in OpenDA as the output period
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_reference_period_(reference_period)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_reference_period_")

    use global, only: tidalp

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_reference_period_
#endif

    ! return value
    integer(kind=c_int) :: ret_val ! if succes ret_val=0

    ! arguments
    real(kind = c_double), intent(out)   :: reference_period  ! reference period in days

    ! body
    ret_val = -1
    reference_period =  real(tidalp,c_double) / 86400.0d0 ! converted to days
    ret_val = 0
    
    write(dm_general_log_handle, '(A,ES12.4E2,A)') 'get_reference_period( reference_period: ', reference_period, ')'
    call flush(dm_general_log_handle)

  end function m_openda_wrapper_get_reference_period_

  ! --------------------------------------------------------------------------
  ! Get the current time for given model instance 
  ! Time is in days since the first day at 00:00 of the reference year  
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_current_time_(instance, current_time)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_current_time_")

    use global, only : timesec, dt

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_current_time_
#endif

    ! return value
    integer(kind=c_int) :: ret_val ! if succes ret_val=0

    ! arguments
    integer(kind=c_int)        , intent(in)    :: instance      ! model instance
    real(kind=c_double), intent(out)   :: current_time  ! current time in days 

    ! body
    !current_time = dble(state(instance)%timesec) / 86400.0d0
    current_time = real( dt * nint( state(instance)%timesec/ dt), c_double) / 86400.0d0
    ret_val = 0

    write(dm_outfile_handle(instance), '(A,I4,A,F14.10,A)') & 
      'get_current_time( instance: ', instance, ', current_time: ' , current_time, ')'
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_current_time_

  ! --------------------------------------------------------------------------
  ! Integrate the EFDC model for the instance that is currently in memory
  ! for the given time window.
  ! Times are in days since the first day at 00:00 of the reference year    
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_compute_(instance, from_time_stamp, to_time_stamp)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_compute_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_compute_
#endif


    use global, only:timesec, tbegin, tcon, dt, timeday

    ! return value
    integer(kind=c_int) :: ret_val

    ! arguments
    integer(kind=c_int), intent(in) :: instance        ! model instance identifier
    real(kind=c_double), intent(in) :: from_time_stamp ! time stamp to compute from
    real(kind=c_double), intent(in) :: to_time_stamp   ! time stamp to compute to ( > from_time_stamp )

    ! locals
    real(kind=8) :: time_period
    character(len=max_path_length) :: cwd
    
    ret_val = -1
    
    write(dm_outfile_handle(instance),'(A,F9.4,A,F9.4,A)') & 
      'compute( from_time_stamp: ', from_time_stamp, ', to_time_stamp: ', to_time_stamp, '): '
    call flush(dm_outfile_handle(instance))
    
    if (debug) write(dm_outfile_handle(instance), '(A , F10.5, A, F10.5)') & 
      "time interval [days]" , from_time_stamp, ", ",to_time_stamp
    if (valid_model_instance(instance)) then
       call getcwd(cwd)
       if (debug) write(dm_outfile_handle(instance), & 
         '(A,A)') "changing directory to :", trim(model_instance_dirs(instance))
       ret_val = change_directory(model_instance_dirs(instance) )
       if (ret_val == 0) then
           state(instance)%tbegin = from_time_stamp *86400.0d0/tcon ! begin time scaled with tcon (EFDC.INP)
           state(instance)%timesec = from_time_stamp * 86400.0d0    ! time in seconds
           time_period = real((to_time_stamp-from_time_stamp) * 86400.d0, dp)
           TBEGIN = state(instance)%tbegin
           TIMESEC = state(instance)%timesec
           TIMEDAY = TIMESEC/86400.0  
           if (debug) write(dm_outfile_handle(instance), '(A, F8.3, A, I5)' ) & 
             "Integrating over [s] ", time_period, " #steps", nint(time_period/dt)
           call model_make_step(time_period)
           state(instance)%timesec = TIMESEC

       end if
       if (ret_val == 0) then
           if (debug) write(dm_outfile_handle(instance),'(A,A)') "changing directory to:", trim(cwd)
           ret_val = change_directory(cwd )
       end if
    end if
    write(dm_outfile_handle(instance),'(A,F9.4,A,F9.4,A,I2)') & 
      'compute( from_time_stamp: ', from_time_stamp, ', to_time_stamp: ', to_time_stamp, '): ', ret_val
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_compute_


  ! --------------------------------------------------------------------------
  ! If OpenDA is finished with an instance this function is called.
  ! Model instance storage for the given instance is deallocated.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_finish_(instance)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_finish_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_finish_
#endif


    ! return value
    integer(kind=c_int) :: ret_val              ! >=0 : Success; <0 : Error

    !arguments
    integer(kind=c_int), intent(in) :: instance ! model instance identifier 

    ret_val = -1
    call model_state_deallocate(instance)
    call model_aser_deallocate(instance)
    call model_pser_deallocate(instance)
    call model_qser_deallocate(instance)
    call model_cser_deallocate(instance)
    call model_gateser_deallocate(instance)
    
    write(dm_outfile_handle(instance), '(A,I4,A)') 'finish( instance: ', instance ,' )'
    close(dm_outfile_handle(instance))
    ret_val = 0

  end function m_openda_wrapper_finish_

! ----------------------------------------------------------------------------
! Exchange item functions
! ----------------------------------------------------------------------------

  ! --------------------------------------------------------------------------
  ! Check if exchange item is supported by current EFDC configuration
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_supports_exchange_item_(instance, exchange_item_id)&
    result (ret_val) &
    bind(C,name="m_openda_wrapper_supports_exchange_item_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_supports_exchange_item_
#endif

    implicit none

    ! return value
    integer(kind=c_int) :: ret_val     ! =0 : No; =1 : Yes ; <0 : Error

    !arguments
    integer(kind=c_int), intent(inout) :: instance            ! model instance
    integer(kind=c_int), intent(inout) :: exchange_item_id    ! exchange item identifier

    !local
    character(len=max_message_length) :: message
    
    ret_val = -1
    select case (exchange_item_id)
    
    case (Precipitation:PotentialEvaporation) ! atmospheric always supported
      ret_val = 1
    case (WindSpeed:WindDirection) ! wind is not always supported
      ret_val = model_exchange_items_supports(exchange_item_id)
    case (WaterLevel) !always supported
      ret_val = 1
    case (Discharge) !always supported
      ret_val = 1
    case (WaterTemperature) !always supported
      ret_val = 1
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! water quality
      ret_val = model_exchange_items_supports(exchange_item_id)
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! toxics
      ret_val =  model_exchange_items_supports(exchange_item_id)
    case (indexControl+1:indexControl + nrExchangeItemsControl) ! control
      ret_val =  model_exchange_items_supports(exchange_item_id)
    case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! x-species
      ret_val = model_exchange_items_supports(exchange_item_id)
    case(Grid_WaterLevel)
      ret_val = 1
    case(Grid_Discharge)
      ret_val = 1
    case(Grid_WaterTemperature)
      ret_val = 1
    case (gridIndexWQ+1: gridIndexWQ+nrExchangeItemsWQ) ! water quality
      ret_val =  model_exchange_items_supports(exchange_item_id)
    case (gridIndexTOX+1: gridIndexTOX+nrExchangeItemsTOX) ! toxics
      ret_val =  model_exchange_items_supports(exchange_item_id)
    case (gridIndexXspecies+1 : gridIndexXspecies+nrMaxXspecies) ! x-species
      ret_val = model_exchange_items_supports(exchange_item_id)  
    case default
      ret_val = -2
    end select

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I2,A,I4,A,I4)') 'Error in supports_exchange_item: ', ret_val, ' for ', exchange_item_id
       write(message,'(A,I2,A,I4,A,I4)') 'Error in supports_exchange_item: ', ret_val, ' for ', exchange_item_id
       call write_message(message, M_FATAL)
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I2)') 'supports_exchange_item( instance: ', instance, &
           ', exchange_item_id: ', exchange_item_id, '):', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_supports_exchange_item_


  ! --------------------------------------------------------------------------
  ! Pass a reference to a double precission array with time values 
  ! for given exchange item and given location number 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_times_for_ei_(instance, exchange_item_id, bc_index, values_count, times) & 
    result (ret_val) &
    bind(C, name="m_openda_wrapper_get_times_for_ei_")

    use global, only: TCASER, TCWSER, TCPSER, TCQSER, TCCSER, NTOX, NWQV, GCCSER, NXSP

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_times_for_ei_
#endif

    ! return value
    integer(kind=c_int) :: ret_val     ! >=0 : Success; <0 : Error

    !arguments
    integer(kind=c_int), intent(in) :: instance            ! model instance
    integer(kind=c_int), intent(in) :: exchange_item_id    ! exchange item identifier
    integer(kind=c_int), intent(in) :: bc_index            ! location number of the times series (from EFDC.INP or WQ3D.INP) 
    integer(kind=c_int), intent(in) :: values_count        ! length of time array
    real(kind=c_double), dimension(values_count), &
         intent(out) :: times                  ! refernce to times array

    !local
    integer :: id, NC

    ret_val = -1
    select case(exchange_item_id)
    case (Precipitation:PotentialEvaporation) ! atmospheric 
       times = dble(aser(instance)%TASER(1:values_count,bc_index))/86400.0d0 * dble(TCASER(bc_index))
       ret_val = 0
    case (WindSpeed:WindDirection) ! wind
       times = dble(wsert(instance)%TWSER(1:values_count,bc_index))/86400.0d0 * dble(TCWSER(bc_index))
       ret_val = 0
    case (WaterLevel) ! waterlevel
       times = dble(psert(instance)%TPSER(1:values_count,bc_index))/86400.0d0 * dble(TCPSER(bc_index))
       ret_val = 0
    case (Discharge) ! discharge
       times = dble(qsert(instance)%TQSER(1:values_count,bc_index))/86400.0d0 * dble(TCQSER(bc_index))
       ret_val = 0
    case (WaterTemperature) ! water temperature
       NC = 2;
       times = dble(csert(instance)%TCSER(1:values_count,bc_index,NC))/86400.0d0 * dble(TCCSER(bc_index, NC))
       ret_val = 0
    case ( indexWQ+1 : indexWQ+nrExchangeItemsWQ ) ! water quality
       id = exchange_item_id - indexWQ
       NC = NC_wq_start + id 
       if ( id .gt. NWQV ) then
          times = 0.0d0
          ret_val = 1
       else  
          times = dble(csert(instance)%TCSER(1:values_count,bc_index,NC))/86400.0d0 * dble(TCCSER(bc_index, NC))
          ret_val = 0
       end if
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! toxics
       id = exchange_item_id - indexTOX
       !NC = NC_tox_start + id
       NC = NC_tox_start + 1 !only one toxic is active 
       if ( NTOX .eq. 0) then
          times = 0.0d0
          ret_val = 1
       else    
          times = dble(csert(instance)%TCSER(1:values_count,bc_index,NC))/86400.0d0 * dble(TCCSER(bc_index, NC))
          ret_val = 0
       end if 
    case (indexControl+1 : indexControl+nrExchangeItemsControl) ! toxics
       id = exchange_item_id - indexControl
       times = dble( gateser(instance)%GCSER(1:values_count,bc_index) )/86400.0d0 * dble(GCCSER(bc_index))
       ret_val = 0 
    case ( indexXspecies+1 : indexXspecies+nrMaxXspecies ) ! water quality
       id = exchange_item_id - indexXspecies
       NC = NC_xspecies_start + id 
       if ( id .gt. NXSP ) then
          times = 0.0d0
          ret_val = -2
       else  
          times = dble(csert(instance)%TCSER(1:values_count,bc_index,NC))/86400.0d0 * dble(TCCSER(bc_index, NC))
          ret_val = 0
       end if
    case default
       ret_val = -2
    end select
    
    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I2,A,I4,A,I4)') 'Error in get_times_for_ei: ', ret_val, ' for ', exchange_item_id
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A,I4,A)') 'get_times_for_ei( instance: ', instance, &
            ', exchange_item_id: ', exchange_item_id, ', bc_index: ' , bc_index, ', values_count: ', values_count ,')'
       write(dm_outfile_handle(instance),*) times(1:min(9,values_count))
       if ( values_count .ge. 13 ) then
           write(dm_outfile_handle(instance),*) '...'
           write(dm_outfile_handle(instance),*) times(values_count - 2:values_count)
       elseif (  values_count .ge. 10) then
           write(dm_outfile_handle(instance),*) times(10:values_count)
       end if
    endif
    call flush(dm_outfile_handle(instance))   

  end function m_openda_wrapper_get_times_for_ei_

  ! --------------------------------------------------------------------------
  ! Set the time values for given exchange item and given location number 
  ! times are passed as a refrence to a double precission array.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_set_times_for_ei_(instance, exchange_item_id, bc_index, values_count, times) &
    result (ret_val) &
    bind(C, name="m_openda_wrapper_set_times_for_ei_")

  use global, only: TCASER, TCWSER, TCPSER, TCQSER, TCCSER, NTOX, NWQV, GCCSER, NASER, ITNWQ, IWQSUN, NXSP, &
                    WQCIB, WQCIC
#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_set_times_for_ei_
#endif

    ! return value
    integer(kind=c_int) :: ret_val

    !arguments
    integer(kind=c_int), intent(in) :: instance            ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id    ! exchange item identifier
    integer(kind=c_int), intent(in) :: bc_index            ! location number of the times series (from EFDC.INP or WQ3D.INP) 
    integer(kind=c_int), intent(in) :: values_count        ! count of the number of time points
    real(kind=c_double), dimension(values_count), &
         intent(in) :: times                   ! reference to times array

    ! locals
    integer :: NC, id
    real(kind=dp) :: factor
    real :: period
    character(len=max_message_length) :: message ! error message
    
    ret_val = 0
    select case(exchange_item_id)
    case (101:109) ! atmospheric 
       if (values_count > aser(instance)%NDASER) then
          ret_val = enlarge_aser_time_series(instance, values_count, aser(instance)%NASER)
       end if
       if (ret_val==0) then
          factor =  86400.d0/dble(TCASER(bc_index))
          IF(ITNWQ.EQ.0.AND.IWQSUN.GT.1.AND.NASER.GT.0)THEN
              period = times(values_count) - times(1) * factor
              if (ATM_WARNING_REQUIRED) ATM_WARNING_REQUIRED=(WQCIB.ne.0.0).or.(WQCIC.ne.0.0) ! when weights are 0 for tomorrow and day after tomorrow are zero
              if ( period < 2.0e0.and. ATM_WARNING_REQUIRED ) then
                  ATM_WARNING_REQUIRED = .false.
                  write(message, '(A, F8.4, A, I4, A )') & 
                "Atmospheric time series (length = ", &
                period, &
                " days ) for ExchangeItemID=",&
                exchange_item_id,&
                " should contain at least two days for starting the water quality calculations"
                call write_message(message,M_WARNING)
!                ret_val = -2
!                return
              end if
          end if           
          aser(instance)%TASER(:,bc_index) = 1.0e38
          aser(instance)%TASER(1:values_count,bc_index) = real( times * factor )
          aser(instance)%MASER(bc_index) = values_count
       end if
    case (WindSpeed:WindDirection)
       if (values_count > wsert(instance)%NDWSER) then
          ret_val = enlarge_wser_time_series(instance, values_count, wsert(instance)%NWSER)
       end if
       if (ret_val==0) then
          factor =  86400.d0/dble(TCWSER(bc_index))
          wsert(instance)%TWSER(:,bc_index) =  1.0e38
          wsert(instance)%TWSER(1:values_count,bc_index) = real(times * factor)
          wsert(instance)%MWSER(bc_index) = values_count
       end if
    case (WaterLevel)
       if (values_count > psert(instance)%NDPSER) then
          ret_val = enlarge_pser_time_series(instance, values_count, psert(instance)%NPSER)
       end if
       if (ret_val==0) then
          factor =  86400.d0/dble(TCPSER(bc_index))
          psert(instance)%TPSER(:,bc_index) =  1.0e38
          psert(instance)%TPSER(1:values_count,bc_index) = real(times * factor)
          psert(instance)%MPSER(bc_index) = values_count
       end if
    case (Discharge)
       if (values_count > qsert(instance)%NDQSER) then
          ret_val = enlarge_qser_time_series(instance, values_count, &
                                             qsert(instance)%KCM, qsert(instance)%NQSER)
       end if
       if (ret_val==0) then
          factor =  86400.d0/dble(TCQSER(bc_index))
          qsert(instance)%TQSER(:,bc_index) =  1.0e38
          qsert(instance)%TQSER(1:values_count,bc_index) = real(times * factor )
          qsert(instance)%MQSER(bc_index) = values_count
       end if
    case (WaterTemperature) 
       if (values_count > csert(instance)%NDCSER) then
          ret_val = enlarge_cser_time_series(instance, values_count, &
                                             csert(instance)%KCM, csert(instance)%NCSERM)
       end if
       if (ret_val==0) then
          NC = 2;
          factor =  86400.d0/dble(TCCSER(bc_index, NC))
          csert(instance)%TCSER(:,bc_index,NC) =1.0e38
          csert(instance)%TCSER(1:values_count,bc_index,NC) = real(times * factor)
          csert(instance)%MCSER(bc_index,NC) = values_count
       end if
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! Water Quality 
       if (values_count > csert(instance)%NDCSER) then
          ret_val = enlarge_cser_time_series(instance, values_count, &
                                             csert(instance)%KCM, csert(instance)%NCSERM)
       end if
       if (ret_val==0) then
          id = exchange_item_id - indexWQ
          NC = NC_wq_start + id;
          if (id .gt. NWQV ) then
             ret_val = 1
           else
             if (debug) print*, "setting times", allocated(csert(instance)%tcser)
             factor =  86400.d0/dble(TCCSER(bc_index, NC))
             csert(instance)%TCSER(:,bc_index,NC) =  1.0e38
             csert(instance)%TCSER(1:values_count,bc_index,NC) = real(times * factor)
             csert(instance)%MCSER(bc_index,NC) = values_count
          end if
       end if
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! Toxics
       if (values_count > csert(instance)%NDCSER) then
          ret_val = enlarge_cser_time_series(instance, values_count, &
                                             csert(instance)%KCM, csert(instance)%NCSERM)
       end if
       if (ret_val==0) then
          id = exchange_item_id - indexTOX
          !NC = NC_tox_start + id
          NC = NC_tox_start + 1 ! only one toxic active
          if ( NTOX .eq. 0 ) then
             ret_val = 1
          else
             if (debug) print*, "setting times", allocated(csert(instance)%tcser)
             factor =  86400.d0/dble(TCCSER(bc_index, NC))
             csert(instance)%TCSER(:,bc_index,NC) =  1.0e38
             csert(instance)%TCSER(1:values_count,bc_index,NC) = real(times * factor)
             csert(instance)%MCSER(bc_index,NC) = values_count
          end if
       end if
    case (indexControl+1 : indexControl+nrExchangeItemsControl) ! Control
       if (values_count > gateser(instance)%NDQCLT) then
          ret_val = enlarge_gateser_time_series(instance, values_count, &
                                             gateser(instance)%NQCTLM)
       end if
       if (ret_val==0) then
          id = exchange_item_id - indexControl
          if (debug) print*, "setting times", allocated(gateser(instance)%gcser)
          factor =  86400.d0/dble(GCCSER(bc_index))
          gateser(instance)%GCSER(:,bc_index) =  1.0e38
          gateser(instance)%GCSER(1:values_count,bc_index) = real(times * factor)
          gateser(instance)%MQCTL(bc_index) = values_count
       end if
    case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! X-species
       id = exchange_item_id - indexXspecies
       if (id > NXSP) then
          ret_val = -1 ! x-species above nxsp do not exist and therefore should not have a timeseries
       else if (values_count > csert(instance)%NDCSER) then
          ret_val = enlarge_cser_time_series(instance, values_count, &
                                             csert(instance)%KCM, csert(instance)%NCSERM)
       end if
       if (ret_val==0) then
          NC = NC_xspecies_start + id;
          if (debug) print*, "setting times", allocated(csert(instance)%tcser)
          factor =  86400.d0/dble(TCCSER(bc_index, NC))
          csert(instance)%TCSER(:,bc_index,NC) =  1.0e38
          csert(instance)%TCSER(1:values_count,bc_index,NC) = real(times * factor)
          csert(instance)%MCSER(bc_index,NC) = values_count
       end if
    case default
       ret_val = -1
    end select
    
    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I2,A,I4)') 'Error in set_times_for_ei: ', ret_val, ' for ', exchange_item_id
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A,I4,A)') 'set_times_for_ei( instance: ', instance, &
            ', exchange_item_id: ', exchange_item_id, ', bc_index: ' , bc_index, ', values_count: ', values_count ,')'
       write(dm_outfile_handle(instance),'(A,F8.4)') 'conversion_factor: ', factor
       write(dm_outfile_handle(instance),*) times(1:min(9,values_count))
       if ( values_count .ge. 13 ) then
           write(dm_outfile_handle(instance),*) '...'
           write(dm_outfile_handle(instance),*) times(values_count - 2:values_count)
       elseif (  values_count .ge. 10) then
           write(dm_outfile_handle(instance),*) times(10:values_count)
       end if
    endif
    call flush(dm_outfile_handle(instance)) 

  end function m_openda_wrapper_set_times_for_ei_

  ! --------------------------------------------------------------------------
  ! Function returns the number of grid points for given exchange item 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_cell_count_(instance, exchange_item_id)&
    result(ret_val) &
    bind(C,name="m_openda_wrapper_get_cell_count_")

    use global, only: LA, NWQV, NTOX, NXSP

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_cell_count_
#endif

    ! return value
    integer(kind=c_int) :: ret_val    ! number of values for a certain exchange item

    ! arguments
    integer(kind=c_int), intent(in) :: instance          ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id  ! exchange item identifier
   
    !local
    integer :: id, warn

    ! body
    ret_val = -1
    warn = 0
    select case(exchange_item_id)
    case (Grid_WaterLevel)
       ret_val = LA-1
    case (Grid_Discharge)
       ret_val = LA-1
    case (Grid_WaterTemperature)
       ret_val = LA-1
    case ( gridIndexWQ+1: gridIndexWQ+nrExchangeItemsWQ )  ! water quality grid items
       id = exchange_item_id - gridIndexWQ
       if (id .gt. NWQV) warn = 1
       ret_val = LA-1
    case (gridIndexTOX+1: gridIndexTOX+nrExchangeItemsTOX)  ! water quality grid items
       id = exchange_item_id - gridIndexTOX
       if (NTOX .eq. 0) warn = 1
       ret_val = LA-1
    case (gridIndexXspecies+1: gridIndexXspecies+nrMaxXspecies) ! x-species grid items
       id = exchange_item_id - gridIndexXspecies
       if (id > NXSP) then
          ret_val = -2
       else
          ret_val = LA-1
       end if
    case default
       ret_val = -2
    end select

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I4,A,I4)') 'Error in get_cell_count: ', ret_val, ' for ', exchange_item_id
    elseif ( warn .eq. 1) then
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Exchange item with id: ', exchange_item_id, ' is not configured in EFDC.'
       write(dm_outfile_handle(instance),'(A,I4)') 'Returning: ', ret_val
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I8)') 'get_cell_count( instance: ', instance,  &
            ', exchange_item_id: ', exchange_item_id, ' ): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_cell_count_
  
  ! --------------------------------------------------------------------------
  ! Function returns the number of grid points for given exchange item 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_values_count_(instance, exchange_item_id)&
    result(ret_val) &
    bind(C, name="m_openda_wrapper_get_values_count_")
#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_values_count_
#endif

    ! return value
    integer(kind=c_int) :: ret_val    ! number of values for a certain exchange item

    ! arguments
    integer(kind=c_int), intent(in) :: instance          ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id  ! exchange item identifier
    
    ret_val = m_openda_wrapper_get_cell_count_(instance, exchange_item_id) &
            * m_openda_wrapper_get_layer_count_(instance, exchange_item_id)
    
  end function m_openda_wrapper_get_values_count_

    ! --------------------------------------------------------------------------
  ! Function returns the number of layers for given exchange item 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_layer_count_(instance, exchange_item_id)&
    result(ret_val) &
    bind(C,name="m_openda_wrapper_get_layer_count_")

    use global, only: NXSP, KC

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_layer_count_
#endif

    ! return value
    integer(kind=c_int) :: ret_val    ! number of values for a certain exchange item

    ! arguments
    integer(kind=c_int), intent(in) :: instance          ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id  ! exchange item identifier
   
    !local
    integer :: id, warn

    ! body
    ret_val = -1
    warn = 0
    select case(exchange_item_id)
    case (Grid_WaterLevel)
       ret_val = 1
    case (Grid_Discharge)
       ret_val = KC
    case (Grid_WaterTemperature)
       ret_val = KC
    case ( gridIndexWQ+1: gridIndexWQ+nrExchangeItemsWQ )  ! water quality grid items
       ret_val = KC
    case (gridIndexTOX+1: gridIndexTOX+nrExchangeItemsTOX)  ! water quality grid items
       ret_val = KC
    case ( gridIndexXspecies+1: gridIndexXspecies+nrMaxXspecies )  ! x-species grid items
       if (exchange_item_id > gridIndexXspecies + NXSP) then
          ret_val = -2
       else
          ret_val = KC
       end if
    case (Precipitation)
        ret_val = 1 
    case (AirTemperature)
        ret_val = 1
    case (CloudCover)
        ret_val = 1
    case (GlobalRadiation)
        ret_val = 1
    case (AtmosphericPressure)
        ret_val = 1
    case (RelativeHumidity)
        ret_val = 1
    case (PotentialEvaporation)
        ret_val = 1
    case (WindSpeed : WindDirection)
        ret_val = 1
    case (WaterLevel)
        ret_val = 1 
    case (Discharge) 
        ret_val = KC
    case (WaterTemperature) 
        ret_val = KC
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) 
        ret_val = KC
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX)
        ret_val = KC
    case (indexControl+1 : indexControl+nrExchangeItemsControl) 
        ret_val = 1
    case ( indexXspecies+1: indexXspecies+nrMaxXspecies )  ! x-species items
       if (exchange_item_id > indexXspecies + NXSP) then
          ret_val = -2
       else
          ret_val = KC
       end if
    case default
       ret_val = -2
    end select

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I4,A,I4)') 'Error in get_layer_count: ', ret_val, ' for ', exchange_item_id
    elseif ( warn .eq. 1) then
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Exchange item with id: ', exchange_item_id, ' is not configured in EFDC.'
       write(dm_outfile_handle(instance),'(A,I4)') 'Returning: ', ret_val
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I8)') 'get_layer_count( instance: ', instance,  &
            ', exchange_item_id: ', exchange_item_id, ' ): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_layer_count_

  ! --------------------------------------------------------------------------
  ! Function returns the number of layers for given exchange item 
  ! --------------------------------------------------------------------------
  
  
  function m_openda_wrapper_get_layer_count_for_model_(instance)&
    result(ret_val) &
    bind(C,name="m_openda_wrapper_get_layer_count_for_model_")
  
    use global, only: KC

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_layer_count_for_model_
#endif

    ! return value
    integer(kind=c_int) :: ret_val    ! number of layer used by model

    ! arguments
    integer(kind=c_int), intent(in) :: instance ! model instance identifier
   
    !local
    integer :: id, warn

    ! body
    ret_val = KC

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I4)') 'Error in get_layer_count_for_model: ', ret_val
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I8)') 'get_layer_count_for_model( instance: ', instance,  &
            ' ): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_layer_count_for_model_
  
  

  ! --------------------------------------------------------------------------
  ! Pass a reference to the array with values for the given exchange item
  ! A subset of the array can be specified by the start and end index 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_values_(instance, exchange_item_id, start_index, end_index, values)&
    result(ret_val) & 
    bind(C, name="m_openda_wrapper_get_values_")

    use global, only : NTOX, NWQV, NXSP, KC
    
#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_values_
#endif

    ! return value
    integer(kind=c_int) :: ret_val ! =0 ok; =-1 indices not ok;

    ! arguments
    integer(kind=c_int) , intent(in) :: instance         ! model instance identifier
    integer(kind=c_int) , intent(in) :: exchange_item_id ! exchange item identifier
    integer(kind=c_int) , intent(in) :: start_index, end_index ! number of values
    real(kind=c_double), &
         dimension(end_index - start_index + 1), &
         intent(out)  :: values           ! returned values
    ! locals
    integer :: NCi          ! index of exchange item variable in CSER time series
    integer :: index1, index2, last_index
    integer :: values_count ! total number of values
    integer :: layer_count  ! total number of layer
    ! body

    ret_val = -1
    values_count = end_index - start_index + 1
    layer_count = m_openda_wrapper_get_layer_count_(instance, exchange_item_id);
    ! shift to EFDC grid indices EFDC (2:LA), OpenDA java (0:LA-2)
    index1 = start_index + 2;
    index2 = start_index + (end_index - start_index + 1)/layer_count + 1;
    if ( valid_model_instance(instance) ) then
       if ( check_grid_indices(instance, exchange_item_id, index1, index2) ) then
          select case(exchange_item_id)
          case (Grid_WaterLevel) !
             values = real(state(instance)%HP(index1:index2)  +  state(instance)%BELV(index1:index2), c_double)
             ret_val = 0
          case (Grid_Discharge) ! Only one layer for now
             values = real(reshape(state(instance)%QSUM(index1:index2,1:KC), (/values_count/)), c_double) 
             ret_val = 0
          case (Grid_WaterTemperature) ! Only one layer for now
             values = real(reshape(state(instance)%TEM(index1:index2,1:KC), (/values_count/)), c_double) 
             ret_val = 0
          case (gridIndexWQ+1: gridIndexWQ+nrExchangeItemsWQ) ! Water Quality fields
             NCi = exchange_item_id - gridIndexWQ
             if ( NCi .gt. NWQV ) then
                values = missing_value
                ret_val = 1
             else
                values = real(reshape(state(instance)%WQV(index1:index2,1:KC, NCi), (/values_count/)),c_double)
                ret_val = 0
             end if
          case (gridIndexTOX+1: gridIndexTOX+nrExchangeItemsTOX) ! Toxixs
             !NCi = exchange_item_id - gridIndexTOX
             NCi = 1
             if (NTOX .eq. 0) then
                values = missing_value
                ret_val = 1
             else
                values = real(reshape(state(instance)%TOX(index1:index2,1:KC, NCi), (/values_count/)),c_double)
                ret_val = 0
             end if
          case (gridIndexXspecies+1: gridIndexXspecies+nrMaxXspecies) ! Xspecies
             NCi = exchange_item_id - gridIndexXspecies
             if ( NCi .gt. NXSP ) then
                values = missing_value
                ret_val = 1
             else
                values = real(reshape(state(instance)%WQVX(index1:index2,1:KC, NCi), (/values_count/)),c_double)
                ret_val = 0
             end if
          case default
             ret_val = -2 ! unhandled item
          end select
       endif
    endif

    if (ret_val .lt. 0) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in get_values: ', ret_val
    elseif ( ret_val .eq. 1) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in get_values: ', ret_val
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Exchange item with id: ',  &
         exchange_item_id, ' is not configured in EFDC.'
    else
       last_index = end_index - start_index+1
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A)') 'get_values( exchange_item_id: ', &
            exchange_item_id, ', start_index: ' , start_index, ', end_index: ', end_index,  '):'
       write(dm_outfile_handle(instance),*) values(1:min(9,last_index))
       if ( last_index .ge. 13 ) then
           write(dm_outfile_handle(instance),*) '...'
           write(dm_outfile_handle(instance),*) values(last_index - 2:last_index)
       elseif ( last_index .ge. 10) then
           write(dm_outfile_handle(instance),*) values(10:last_index)
       end if
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_values_

  ! --------------------------------------------------------------------------
  ! Set the values for the given exchange item in model instance memory
  ! A subset of the array can be specified by the start and end index 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_set_values_(instance, exchange_item_id, start_index, end_index, values) &
    result(ret_val) &
    bind(C, name="m_openda_wrapper_set_values_")

    use global, only : NTOX, NWQV, NXSP, KC

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_set_values_
#endif

    ! return value
    integer(kind=c_int) :: ret_val ! =0 ok; =-1 indices not ok;

    ! arguments
    integer(kind=c_int) , intent(in) :: instance         ! model instance identifier
    integer(kind=c_int) , intent(in) :: exchange_item_id ! exchange item identifier
    integer(kind=c_int) , intent(in) :: start_index, end_index     ! number of values
    real(kind=c_double), &
         dimension(end_index - start_index + 1), &
         intent(in)  :: values           ! returned values
    ! locals
    integer :: NCi            ! index of exchange item variable in CSER time series
    integer :: index1, index2 ! EFDC start and end grid indices
    integer :: last_index     ! last index of values array
    integer :: cell_count     ! cell count per layer
    integer :: layer_count    ! layer count
    ! body
    
    ! shift to EFDC grid indices EFDC (2:LA), OpenDA java (0:LA-2)

    layer_count = m_openda_wrapper_get_layer_count_(instance, exchange_item_id);
    cell_count = (end_index - start_index + 1)/layer_count

    index1 = start_index + 2;
    index2 = start_index + cell_count + 1;
    ret_val = -1 ! indices not ok
    if (valid_model_instance(instance)) then
       if ( check_grid_indices(instance, exchange_item_id, index1, index2) ) then
          select case(exchange_item_id)
          case (Grid_WaterLevel) !
             state(instance)%HP(index1:index2) = real(values) - state(instance)%BELV(index1:index2)
             ret_val = 0
          case (Grid_Discharge) ! Only one layer for now
             state(instance)%QSUM(index1:index2,1:KC) = real(reshape(values, (/cell_count, KC/)))
             ret_val = 0
          case (Grid_WaterTemperature) ! Only one layer for now
             state(instance)%TEM(index1:index2,1:KC) = real(reshape(values, (/cell_count, KC/)))
             ret_val = 0
          case (gridIndexWQ+1: gridIndexWQ+nrExchangeItemsWQ) ! Water Quality,  Only one layer for now
             NCi = exchange_item_id - gridIndexWQ
             if (NCi .gt. NWQV) then
                ret_val = 1
             else 
                state(instance)%WQV(index1:index2,1:KC, NCi) = real(reshape(values, (/cell_count, KC/)))
                ret_val = 0
             end if
          case (gridIndexTOX+1: gridIndexTOX+nrExchangeItemsTOX) ! Toxics
             !NCi = exchange_item_id - gridIndexTOX
             NCi = 1
             if (NTOX .eq. 0) then
                ret_val = 1
             else 
                state(instance)%TOX(index1:index2,1:KC, NCi) = real(reshape(values, (/cell_count, KC/)))
                ret_val = 0
             end if 
          case (gridIndexXspecies+1: gridIndexXspecies+nrMaxXspecies) ! X-species
             NCi = exchange_item_id - gridIndexXspecies
             if (NCi .gt. NXSP) then
                ret_val = 1
             else 
                state(instance)%WQVX(index1:index2,1:KC, NCi) = real(reshape(values, (/cell_count, KC/)))
                ret_val = 0
             end if
          case default
             ret_val = -2 ! unhandled item
          end select
       endif
    endif

    if (ret_val .lt. 0) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in set_values: ', ret_val
    elseif ( ret_val .eq. 1) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in set_values: ', ret_val
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Exchange item with id: ',  &
         exchange_item_id, ' is not configured in EFDC.'
    else
       last_index = end_index - start_index+1
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A)') 'set_values( exchange_item_id: ', &
            exchange_item_id, ', start_index: ' , start_index, ', end_index: ', end_index,  '):'
       write(dm_outfile_handle(instance),*) values(1:min(9,last_index))
       if ( last_index .ge. 13 ) then
           write(dm_outfile_handle(instance),*) '...'
           write(dm_outfile_handle(instance),*) values(last_index - 2:last_index )
       elseif ( last_index .ge. 10) then
           write(dm_outfile_handle(instance),*) values(10:last_index)
       end if
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_set_values_

  ! --------------------------------------------------------------------------
  ! Get the number of values for given exchange item and location
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_times_count_for_location_(instance, exchange_item_id, bc_index)&
    result(ret_val) &
    bind(C, name="m_openda_wrapper_get_times_count_for_location_")

    !use global, only: MASER, MPSER, MQSER

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_times_count_for_location_
#endif

    ! return value
    integer(kind=c_int) :: ret_val    ! number of values for a certain exchange item

    ! arguments
    integer(kind=c_int), intent(in) :: instance ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id  ! exchange item identifier
    integer(kind=c_int), intent(in) :: bc_index  ! location index (as in EFDC.INP or WQ3D.INP)

    ! local
    integer :: NC  ! index of exchange item variable in CSER time series

    ! body
    select case(exchange_item_id)
    case (Precipitation:PotentialEvaporation) ! atmospheric 
       ret_val = aser(instance)%MASER(bc_index)
    case (WindSpeed:WindDirection)
       ret_val = wsert(instance)%MWSER(bc_index)
    case (WaterLevel)
       ret_val = psert(instance)%MPSER(bc_index)
    case (Discharge)
       ret_val = qsert(instance)%MQSER(bc_index)
    case (WaterTemperature)
       NC = 2
       ret_val = csert(instance)%MCSER(bc_index,NC)
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! Water Quality
       NC = NC_wq_start + exchange_item_id - indexWQ
       ret_val = csert(instance)%MCSER(bc_index,NC)
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! Toxics
       !NC = NC_tox_start + exchange_item_id - indexTOX
       NC = NC_tox_start + 1 ! only one toxic active
       if (debug) print*, "get_times_count_for_location", exchange_item_id, bc_index, NC
       ret_val = csert(instance)%MCSER(bc_index,NC)
    case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! Xspecies
       NC = NC_xspecies_start + exchange_item_id - indexXspecies
       if (debug) print*, "get_times_count_for_location", exchange_item_id, bc_index, NC
       ret_val = csert(instance)%MCSER(bc_index,NC)
    case (indexControl+1 : indexControl+nrExchangeItemsControl) ! Control
       ret_val = gateser(instance)%MQCTL(bc_index)   
    case default
       ret_val = -1
    end select

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I4,A,I4)')  &
         'Error in get_times_count_for_location: ', ret_val, ' for ', exchange_item_id
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I8)') & 
         'get_times_count_for_location( instance: ', instance, &
         ', exchange_item_id:  ', exchange_item_id, '): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_times_count_for_location_

  ! --------------------------------------------------------------------------
  ! Get the number of values for given exchange item and location
  ! --------------------------------------------------------------------------
  function get_values_count_for_location(instance, exchange_item_id, bc_index)&
    result(ret_val)


    ! return value
    integer :: ret_val    ! number of values for a certain exchange item

    ! arguments
    integer, intent(in) :: instance ! model instance identifier
    integer, intent(in) :: exchange_item_id  ! exchange item identifier
    integer, intent(in) :: bc_index  ! location index (as in EFDC.INP or WQ3D.INP)

    ret_val = m_openda_wrapper_get_times_count_for_location_(instance, exchange_item_id, bc_index)

  end function get_values_count_for_location

  
  ! --------------------------------------------------------------------------
  ! Get the number of values for given exchange item and location
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_layer_count_for_location_(instance, exchange_item_id, bc_index) &
    result(ret_val) &
    bind(C, name="m_openda_wrapper_get_layer_count_for_location_")

    use global, only: KC

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_layer_count_for_location_
#endif

    ! return value
    integer(kind=c_int) :: ret_val    ! number of values for a certain exchange item

    ! arguments
    integer(kind=c_int), intent(in) :: instance ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id  ! exchange item identifier
    integer(kind=c_int), intent(in) :: bc_index  ! location index (as in EFDC.INP or WQ3D.INP)

    ! local
    integer :: NC  ! index of exchange item variable in CSER time series

    ! body
    select case(exchange_item_id)
    case (Precipitation)
        ret_val = 1 
    case (AirTemperature)
        ret_val = 1
    case (CloudCover)
        ret_val = 1
    case (GlobalRadiation)
        ret_val = 1
    case (AtmosphericPressure)
        ret_val = 1
    case (RelativeHumidity)
        ret_val = 1
    case (PotentialEvaporation)
        ret_val = 1
    case (WaterLevel)
        ret_val = 1 
    case (Discharge) 
        ret_val = KC
    case (WaterTemperature)
        ret_val = KC
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! Water Quality
        ret_val = KC
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! Toxics
        ret_val = KC
    case (indexControl+1 : indexControl+nrExchangeItemsControl) ! Control
        ret_val = 1 
    case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! Toxics
        ret_val = KC
    case default
       ret_val = -1
    end select

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I4,A,I4)')  &
         'Error in get_layer_count_for_location: ', ret_val, ' for ', exchange_item_id
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I8)') & 
         'get_layer_count_for_location( instance: ', instance, &
         ', exchange_item_id:  ', exchange_item_id, '): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_layer_count_for_location_

  ! --------------------------------------------------------------------------
  ! Get the number of locations for given exchange item
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_time_series_count_(instance, exchange_item_id)&
    result(ret_val)&
    bind(C,name="m_openda_wrapper_get_time_series_count_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_time_series_count_
#endif

    use global, only: NCSER, NWQV, NTOX, NQCTLM, NXSP

    ! return value
    integer(kind=c_int) :: ret_val    ! number of locations for given exchange item

    ! arguments
    integer(kind=c_int), intent(in) :: instance          ! model instance identifier
    integer(kind=c_int), intent(in) :: exchange_item_id  ! exchange item identifier
 
    ! local
    integer :: NC, id   ! index of exchange item variable in CSER time series

    ! body
    ret_val = -1
    select case(exchange_item_id)
    case (Precipitation:PotentialEvaporation) ! atmospheric
       ret_val = aser(instance)%NASER
    case (WindSpeed:WindDirection)
       ret_val = wsert(instance)%NWSER
    case (WaterLevel)
       ret_val = psert(instance)%NPSER
    case (Discharge)
       ret_val = qsert(instance)%NQSER
    case (WaterTemperature)
       NC = 2
       ret_val = NCSER(NC)
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! Water Quality
       id = exchange_item_id - indexWQ
       if (id .gt. NWQV) then
          ret_val = 1
       else
          NC = NC_wq_start + id
          if (debug) print*, "number of locations: ", NC, NCSER(NC)
          ret_val = NCSER(NC)
       end if
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! Toxics 
       id = exchange_item_id - indexTOX
       if (NTOX .eq. 0) then
          ret_val = 1
       else       
          !NC = NC_tox_start + id
          NC = NC_tox_start + 1 !only one toxic active
          if (debug) print*, "number of locations: ", NC, NCSER(NC)
          ret_val = NCSER(NC)
       end if
    case (indexControl+1 : indexControl+nrExchangeItemsControl) ! Control 
       id = exchange_item_id - indexControl
       if (debug) print*, "number of locations: ", NQCTLM
       ret_val = gateser(instance)%NQCTLM   
    case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! X species
       id = exchange_item_id - indexXspecies
       if (id .gt. NXSP) then
          ret_val = -1
       else
          NC = NC_xspecies_start + id
          if (debug) print*, "number of locations: ", NC, NCSER(NC)
          ret_val = NCSER(NC)
       end if
    case default
       ret_val = -1
    end select

    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I2)') & 
         'Error in get_time_series_count: ', ret_val
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I8)') & 
        'get_time_series_count( instance: ', instance, &
        ', exchange_item_id: ', exchange_item_id, '): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_time_series_count_


  ! --------------------------------------------------------------------------
  ! Get the number of values for exchange item and location within given time
  ! span.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_times_count_for_time_span_(instance, exchange_item_id, bc_index, start_time, end_time) &
    result(ret_val) &
    bind(C, name="m_openda_wrapper_get_times_count_for_time_span_")


#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_times_count_for_time_span_
#endif

    ! return value
    integer(kind=c_int) :: ret_val

    ! arguments
    integer(kind=c_int) , intent(in) :: instance         ! model instance
    integer(kind=c_int) , intent(in) :: exchange_item_id ! type of time dependent boundary
    integer(kind=c_int) , intent(in) :: bc_index         ! index of location
    real(kind=c_double), intent(in) :: start_time       ! start time of bc values
    real(kind=c_double), intent(in) :: end_time         ! end time of bc values

    ! locals
    integer :: start_index   ! index in bc's time series values for start_time
    integer :: end_index     ! index in bc's time series values for end_time

    ! body

    ret_val = -1 ! indices not ok

    select case(exchange_item_id)
    case (Precipitation:PotentialEvaporation, WindSpeed:WindDirection, WaterLevel, Discharge, WaterTemperature)
      ret_val = 0
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ)
      ret_val = 0 
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX)
      ret_val = 0
    case (indexControl+1 : indexControl+nrExchangeItemsControl)
      ret_val = 0  
    case default
      ret_val = -2
    end select
    if (valid_model_instance(instance)) then
       if (check_bc_indices(instance, exchange_item_id, bc_index , &
          start_time , end_time , &
          start_index, end_index) ) then
          ret_val = end_index - start_index + 1
       endif
    endif

    if (ret_val .eq. -2) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in get_times_count_for_time_span: ', ret_val
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Time series with exchange item id: ', &
          exchange_item_id, ' is not configured in EFDC.' 
        
    elseif (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in get_times_count_for_time_span: ', ret_val
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A,F8.2,A,F8.2,A,I4)') & 
            'get_times_count_for_time_span( instance: ', instance, &
                ', exchange_item_id: ', exchange_item_id,& 
                ', bc_index: ', bc_index,&
                ', start_time: ', start_time, ', end_time: ', end_time, '): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))
    
  end function m_openda_wrapper_get_times_count_for_time_span_
    

  ! --------------------------------------------------------------------------
  ! Get the number of values for exchange item and location within given time
  ! span.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_values_count_for_time_span_(instance, exchange_item_id, bc_index, start_time, end_time) &
    result(ret_val)&
    bind(C, name="m_openda_wrapper_get_values_count_for_time_span_")


#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_values_count_for_time_span_
#endif

    ! return value
    integer(kind=c_int) :: ret_val

    ! arguments
    integer(kind=c_int) , intent(in) :: instance         ! model instance
    integer(kind=c_int) , intent(in) :: exchange_item_id ! type of time dependent boundary
    integer(kind=c_int) , intent(in) :: bc_index         ! index of location
    real(kind=c_double), intent(in)  :: start_time       ! start time of bc values
    real(kind=c_double), intent(in)  :: end_time         ! end time of bc values

    ret_val = m_openda_wrapper_get_times_count_for_time_span_(instance, exchange_item_id, bc_index, start_time, end_time)
  end function m_openda_wrapper_get_values_count_for_time_span_


  ! --------------------------------------------------------------------------
  ! Pass the values as a reference to a double precission array for given 
  ! exchange item and location within given time span.
  ! --------------------------------------------------------------------------

  function m_openda_wrapper_get_values_for_time_span_(instance, exchange_item_id, bc_index, layer_index, start_time, end_time, values_count, values) &
    result(ret_val)&
    bind(C, name="m_openda_wrapper_get_values_for_time_span_")

    use global, only: RAINCVT, NTOX, NWQV, NXSP

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_values_for_time_span_
#endif

    ! return value
    integer(c_int) :: ret_val ! =0 ok; =-1 indices not ok; =-2 invalid exchange item id 

    ! arguments
    integer(c_int) , intent(in) :: instance         ! model instance
    integer(c_int) , intent(in) :: exchange_item_id ! type of time dependent boundary (e.g. discharge_on_laterals)
    integer(c_int) , intent(in) :: bc_index         ! index of boundary condition (e.g. discharge nr. 4)
    integer(c_int) , intent(in) :: layer_index      ! index of boundary condition (e.g. discharge nr. 4)
    real(kind=c_double),    intent(in) :: start_time          ! start time of bc values
    real(kind=c_double),    intent(in) :: end_time            ! end time of bc values
    integer(c_int) , intent(in) :: values_count     ! #values
    real(kind=c_double), &
         dimension(values_count), &
         intent(out)  :: values           ! returned values
    
    ! locals
    integer :: start_index   ! index in bc's time series values for start_time
    integer :: end_index     ! index in bc's time series values for end_time
    real(kind=dp) :: factor, gravity        ! conversion factor
    integer :: NC, id        ! index of exchange item variable in CSER time series
    integer :: last_index    ! last index of values array

    ! body

    ret_val = -1 ! indices not ok

    if (valid_model_instance(instance)) then

       if (check_bc_indices(instance, exchange_item_id, bc_index , &
            start_time , end_time , &
            start_index, end_index) ) then

          select case(exchange_item_id)
          ! atmospheric
          case (Precipitation)
             factor = 1.0d0/dble(RAINCVT)
             values = real(aser(instance)%RAIN(start_index:end_index, bc_index),c_double) 
             ret_val = 0 
          case (AirTemperature)
             values = real(aser(instance)%TDRY(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (CloudCover)
             values = real(aser(instance)%CLOUD(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (GlobalRadiation)
             values = real(aser(instance)%SOLSWR(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (AtmosphericPressure)
             values = real(aser(instance)%PATM(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (RelativeHumidity)
             values = real(aser(instance)%TWET(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (PotentialEvaporation)
             factor = 1.0d0/dble(RAINCVT)
             values = real(aser(instance)%EVAP(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (WindSpeed)
             values = real(wsert(instance)%WINDS(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (WindDirection)
             values = real(wsert(instance)%WINDD(start_index:end_index, bc_index),c_double)
             ret_val = 0
          case (WaterLevel)
             gravity = 9.81d0
             values = real(psert(instance)%PSER(start_index:end_index, bc_index),c_double)/gravity
             ret_val = 0 
          case (Discharge) ! Only one layer for now
             values = real(qsert(instance)%QSER(start_index:end_index,layer_index,bc_index), c_double)
             ret_val = 0 
          case (WaterTemperature) ! Only one layer for now
             NC = 2
             values = real(csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC),c_double) 
             ret_val = 0
          case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! Water Quality,  Only one layer for now
             id = exchange_item_id - indexWQ;
             NC = NC_wq_start + id
             if (id .gt. NWQV ) then
                values = missing_value
                ret_val = 1
             else
                values = real(csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC), c_double)
                ret_val = 0
             end if
          case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! Toxics,  Only one layer for now
             id = exchange_item_id - indexTOX
             !NC = NC_tox_start + id
             NC = NC_tox_start + 1 ! only on toxic is active
             if (NTOX .eq. 0 ) then
                values = missing_value
                ret_val = 1
             else
                values = real(csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC),c_double)
                ret_val = 0
             end if
			 case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! x-species,  Only one layer for now
             id = exchange_item_id - indexXspecies;
             NC = NC_xspecies_start + id
             if (id .gt. NXSP ) then
                values = missing_value
                ret_val = 1
             else
                values = real(csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC),c_double)
                ret_val = 0
             end if
          case (ControlsGateWaterLevel) ! Control
             id = exchange_item_id - indexControl
             values = real(gateser(instance)%SEL1(start_index:end_index,bc_index),c_double)
             ret_val = 0
          case (ControlsGateOpeningHeight) ! Control
             id = exchange_item_id - indexControl
             values = real(gateser(instance)%GUPH(start_index:end_index,bc_index),c_double)
             ret_val = 0   
          case default
             ret_val = -2 ! unhandled item
          end select

       endif

    endif

    if (ret_val .lt. 0) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in get_values_for_time_span: ', ret_val
    elseif (ret_val .eq. 1) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in get_values_for_time_span: ', ret_val
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Echange item with id: ', &
          exchange_item_id, ' not configured in EFDC.'
    else
       last_index = end_index-start_index+1
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A,I4,A,F8.2,A,F8.2,A,I4,A)') & 
            'get_values_for_time_span( instance', instance, &
            ', exchange_item_id: ', exchange_item_id, ', bc_index: ', bc_index ,  &
            ', layer_index: ', layer_index ,  &
            ', start_time: ', start_time, ', end_time: ', end_time, ', values_count: ', values_count ,'):'
       write(dm_outfile_handle(instance),*) values(1:min(9,last_index))
       if (last_index .ge. 13) then 
          write(dm_outfile_handle(instance),*) '...'
          write(dm_outfile_handle(instance),*) values(last_index -2:last_index)
       elseif (last_index .ge. 10) then
          write(dm_outfile_handle(instance),*) values(10:last_index)
       end if
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_get_values_for_time_span_

  ! --------------------------------------------------------------------------
  ! Set the values in instance memory for given 
  ! exchange item and location within given time span.
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_set_values_for_time_span_(instance, exchange_item_id, bc_index, layer_index, start_time, end_time, values_count, values) &
    result(ret_val)&
    bind(C, name="m_openda_wrapper_set_values_for_time_span_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_set_values_for_time_span_
#endif

    use global, only: RAINCVT, NTOX, NWQV, NXSP

    ! return value
    integer(kind=c_int) :: ret_val

    ! arguments
    integer(kind=c_int) , intent(in) :: instance         ! model instance
    integer(kind=c_int) , intent(in) :: exchange_item_id ! type of time dependent boundary (e.g. discharge_on_laterals)
    integer(kind=c_int) , intent(in) :: bc_index         ! index of boundary condition (e.g. discharge nr. 4)
    integer(kind=c_int) , intent(in) :: layer_index         ! index of layer 
    real(kind=c_double), intent(in) :: start_time       ! start time of bc values
    real(kind=c_double), intent(in) :: end_time         ! end time of bc values
    integer(kind=c_int), intent(in) :: values_count     ! number of values
    real(kind=c_double), &
         dimension(values_count), &
         intent(in) :: values           ! incoming values

    ! locals
    integer :: start_index   ! index in bc's time series values for start_time
    integer :: end_index     ! index in bc's time series values for end_time
    integer :: NC           ! index of exchange item variable in CSER time series
    integer :: i, id, last_index
    integer :: times_count  ! times_count per layer
    real(kind=dp) :: gravity !should be the same as in INPUT.for
    character(len=max_message_length) :: message ! error message
    
    
    ! body
    times_count = values_count
    ret_val = -1 ! indices not ok
    if (values_count == 0 ) then 
        write(message,'(A,I4)') 'Trying to set zero length data for exchange item', exchange_item_id
        call write_message(message, M_FATAL)
        return
    end if
    
    do i =1,values_count
        if (any(values == -9999)) then
            write(message,'(A,I4)') 'missing values for exchange item', exchange_item_id
            call write_message(message, M_WARNING)
        end if    
    end do
    
    if (valid_model_instance(instance)) then

       if (check_bc_indices(instance, exchange_item_id, bc_index , &
            start_time , end_time , &
            start_index, end_index) ) then

          ret_val = 0

          select case(exchange_item_id)
          case (Precipitation)
             !RAINCVT = 1.0e-3/3600.0
             aser(instance)%RAIN(start_index:end_index, bc_index) = real(values)  
             ret_val = 0
          case (AirTemperature)
             aser(instance)%TDRY(start_index:end_index, bc_index) = real(values)
             ret_val = 0
          case (CloudCover)
             aser(instance)%CLOUD(start_index:end_index, bc_index) = real(values)
             ret_val = 0
          case (GlobalRadiation)
             aser(instance)%SOLSWR(start_index:end_index, bc_index) = real(values)  
             ret_val = 0
          case (AtmosphericPressure)
             aser(instance)%PATM(start_index:end_index, bc_index) = real(values)  
             ret_val = 0
          case (RelativeHumidity)
             aser(instance)%TWET(start_index:end_index, bc_index) = real(values)  
             ret_val = 0
          case (PotentialEvaporation)
             !RAINCVT = 1.0e-3/3600.0
             aser(instance)%EVAP(start_index:end_index, bc_index) = real(values)
             ret_val = 0
          case (WindSpeed)
             wsert(instance)%WINDS(start_index:end_index, bc_index) = real(values)
             ret_val = 0
          case (WindDirection)
             wsert(instance)%WINDD(start_index:end_index, bc_index) = real(values)
             ret_val = 0
          case (WaterLevel)
             gravity = 9.81d0
             psert(instance)%PSER(start_index:end_index, bc_index) = real(values * gravity) 
             ret_val = 0 
          case (Discharge) ! Only one layer for now
             
             qsert(instance)%QSER(start_index:end_index, layer_index,bc_index) = real(values)
             ret_val = 0 
          case (WaterTemperature) ! Only one layer for now
             NC = 2
             csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC) = real(values)
             ret_val = 0
          case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) !Water Quality, only one layer for now
             id = exchange_item_id - indexWQ
             NC = NC_wq_start + id
             if (id .gt. NWQV) then
                ret_val = 1
             else
                csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC) = real(values)
                ret_val = 0
             end if
          case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) !Toxics, only one layer for now
             id = exchange_item_id - indexTOX
             !NC = NC_tox_start + id
             NC = NC_tox_start + 1 ! only one toxic is active
             if (NTOX .eq. 0) then
                ret_val = 1
             else
                csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC) = real(values)
                ret_val = 0
             end if
			 case (indexXspecies+1 : indexXspecies+nrMaxXspecies) !Water Quality, only one layer for now
             id = exchange_item_id - indexXspecies
             NC = NC_xspecies_start + id
             if (id .gt. NXSP) then
                ret_val = 1
             else
                csert(instance)%CSER(start_index:end_index,layer_index,bc_index, NC) = real(values)
                ret_val = 0
             end if
          case (ControlsGateWaterLevel) !Control
             id = exchange_item_id - indexControl
             gateser(instance)%SEL1(start_index:end_index,bc_index) = real(values)
             ret_val = 0
          case (ControlsGateOpeningHeight) !Control
             id = exchange_item_id - indexControl
             gateser(instance)%GUPH(start_index:end_index,bc_index) = real(values)
             ret_val = 0
          case default
             ret_val = -2 ! unhandled item
          end select

       endif

    endif

    if (ret_val .lt. 0) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in set_values_for_time_span: ', ret_val
    elseif (ret_val .eq. 1) then
       write(dm_outfile_handle(instance),'(A,I2)') 'Error in set_values_for_time_span: ', ret_val
       write(dm_outfile_handle(instance),'(A,I4,A)') 'Echange item with id: ', &
          exchange_item_id, ' not configured in EFDC.'
    else
       last_index = end_index-start_index+1
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I4,A,F8.2,A,F8.2,A,I4,A)') & 
            'set_values_for_time_span( instance', instance, &
            ', exchange_item_id: ', exchange_item_id, ', bc_index: ', bc_index ,  &
            ', start_time: ', start_time, ', end_time: ', end_time, ', values_count: ', values_count ,'):'
       write(dm_outfile_handle(instance),*) values(1:min(9,last_index))
       if (last_index .ge. 13) then 
          write(dm_outfile_handle(instance),*) '...'
          write(dm_outfile_handle(instance),*) values(last_index -2:last_index )
       elseif (last_index .ge. 10) then
          write(dm_outfile_handle(instance),*) values(10:last_index )
       end if
    endif
    call flush(dm_outfile_handle(instance))

  end function m_openda_wrapper_set_values_for_time_span_
    ! --------------------------------------------------------------------------
  ! Function returns the number of grid points for given exchange item 
  ! --------------------------------------------------------------------------
  function m_openda_wrapper_get_xspecies_count_(instance)&
    result(ret_val) &
    bind(C, name="m_openda_wrapper_get_xspecies_count_")

#if ( defined(_WIN32) && defined(__INTEL_COMPILER) )
    !DEC$ ATTRIBUTES DLLEXPORT :: m_openda_wrapper_get_xspecies_count_
#endif
    
    use global, only: NXSP
    
    implicit none
    ! return value
    integer(kind=c_int) :: ret_val
    ! input/output variables
    integer(kind=c_int) , intent(in) :: instance         ! model instance
    
    ret_val = NXSP
    if (ret_val < 0) then
       write(dm_outfile_handle(instance),'(A,I2)') & 
         'Error in get_xspecies_count: ', ret_val
    else
       write(dm_outfile_handle(instance),'(A,I4,A,I4,A,I8)') & 
        'get_xspecies_count( instance: ', instance, &
        '): ', ret_val
    endif
    call flush(dm_outfile_handle(instance))
    return
  end function m_openda_wrapper_get_xspecies_count_

!-----------------------------------------------------------------------------
! private methods
!-----------------------------------------------------------------------------

  ! --------------------------------------------------------------------------
  ! Check if the specified location index is within range and determines the
  ! start and end index for given start and end time
  ! --------------------------------------------------------------------------
  function check_bc_indices(instance, exchange_item_id, bc_index, start_time, end_time, start_index, end_index) & 
    result(success)
    
    use global, only: NDASER ,NASERM, TASER, NDWSER, NWSERM, TWSER, NDPSER, NPSERM, TPSER, NDQSER, NQSERM, & 
        TQSER, NCSER, &
        TCASER, TCWSER, TCPSER, TCCSER, TCQSER, NWQV, NXSP, NTOX, GCCSER ! time conversion to seconds
    

    ! return value
    logical :: success     ! .true.  indices determined ok.
    ! .false. location_index out of bounds

    ! arguments
    integer         , intent(in)  :: instance      ! model instance identifier
    integer         , intent(in)  :: exchange_item_id       ! type of boundary condition (discharge_on_laterals)
    integer         , intent(in)  :: bc_index      ! location index
    real(kind=dp), intent(in)  :: start_time    ! start time of values to be gotten/set
    real(kind=dp), intent(in)  :: end_time      ! end time of values to be gotten/set
    integer         , intent(out) :: start_index   ! index in bc's time series values for start_time
    integer         , intent(out) :: end_index     ! index in bc's time series values for end_time

    ! locals
    real(kind=dp) :: epsilon = 1.0D-8
    real(kind=dp) :: bc_start_time
    real(kind=dp) :: bc_end_time 
    real(kind=dp) :: bc_time_interval
    integer          :: NC                ! index of exchange item variable in CSER time series
    integer          :: id


    ! body
    success = .false.

    select case(exchange_item_id)
    case (Precipitation:PotentialEvaporation) ! atmospheric 
       success  =  bc_index >= 1 .and. bc_index <= aser(instance)%NASER
       if (success) then  
          bc_start_time = aser(instance)%TASER(1, bc_index) * dble(TCASER(bc_index)) / 86400.0d0
          bc_end_time = aser(instance)%TASER(aser(instance)%MASER(bc_index), bc_index) * dble(TCASER(bc_index)) & 
                      / 86400.0d0
          bc_time_interval = (bc_end_time - bc_start_time) / dble(aser(instance)%MASER(bc_index)-1 )
       end if
    case (WindSpeed:WindDirection)
       success = bc_index >= 1 .and. bc_index <= wsert(instance)%NWSER
       if (success) then  
          bc_start_time = wsert(instance)%TWSER(1, bc_index) * dble(TCWSER(bc_index)) / 86400.0d0
          bc_end_time = wsert(instance)%TWSER(wsert(instance)%MWSER(bc_index), bc_index) * dble(TCWSER(bc_index)) & 
                      / 86400.0d0
          bc_time_interval = (bc_end_time - bc_start_time) / dble(wsert(instance)%MWSER(bc_index)-1 )
       end if
    case (WaterLevel) 
       success  =  bc_index >= 1 .and. bc_index <= psert(instance)%NPSER
       if (success) then  
          bc_start_time = psert(instance)%TPSER(1, bc_index) * dble(TCPSER(bc_index)) / 86400.0d0
          bc_end_time = psert(instance)%TPSER(psert(instance)%MPSER(bc_index), bc_index) * dble(TCPSER(bc_index)) &
                      / 86400.0d0
          bc_time_interval =  (bc_end_time - bc_start_time) / dble(psert(instance)%MPSER(bc_index)-1)
       end if
    case (Discharge) 
       success  =  bc_index >= 1 .and. bc_index <= qsert(instance)%NQSER
       if (success) then  
          bc_start_time = qsert(instance)%TQSER(1, bc_index)
          bc_end_time = qsert(instance)%TQSER(qsert(instance)%MQSER(bc_index), bc_index) * dble(TCQSER(bc_index)) &
                      / 86400.0d0
          bc_time_interval = (bc_end_time - bc_start_time)  & 
                          / dble(qsert(instance)%MQSER(bc_index)-1) * dble(TCQSER(bc_index)) / 86400.0d0
       end if
    case (WaterTemperature) 
       NC = 2
       success  =  bc_index >= 1 .and. bc_index <= NCSER(NC)
       if (success) then  
          bc_start_time = csert(instance)%TCSER(1, bc_index, NC)
          bc_end_time = csert(instance)%TCSER(csert(instance)%MCSER(bc_index,NC), bc_index, NC) & 
                      * dble(TCCSER(bc_index,NC)) / 86400.0d0
          bc_time_interval = (bc_end_time - bc_start_time) / dble(csert(instance)%MCSER(bc_index, NC)-1) & 
                           * dble(TCCSER(bc_index,NC)) / 86400.0d0
       end if
    case (indexWQ+1 : indexWQ+nrExchangeItemsWQ) ! Water Quality 
       id = exchange_item_id - indexWQ;
       NC = NC_wq_start + id
       success  =  bc_index >= 1 .and. bc_index <= NCSER(NC)
       if (success) then  
          bc_start_time = csert(instance)%TCSER(1, bc_index, NC)
          bc_end_time = csert(instance)%TCSER(csert(instance)%MCSER(bc_index,NC), bc_index, NC) &
                      * dble(TCCSER(bc_index,NC)) / 86400.0d0
          bc_time_interval =  (bc_end_time - bc_start_time) / dble(csert(instance)%MCSER(bc_index, NC)-1) &
                           * dble(TCCSER(bc_index,NC)) / 86400.0d0
       end if
    case (indexTOX+1 : indexTOX+nrExchangeItemsTOX) ! Water Quality 
       id = exchange_item_id - indexTOX
       !NC = NC_tox_start + id
       NC = NC_tox_start + 1 !only on toxic is active
       success  =  bc_index >= 1 .and. bc_index <= NCSER(NC)
       if (success) then  
          bc_start_time = csert(instance)%TCSER(1, bc_index, NC)
          bc_end_time = csert(instance)%TCSER(csert(instance)%MCSER(bc_index,NC), bc_index, NC) &
                      * dble(TCCSER(bc_index,NC)) / 86400.0d0
          bc_time_interval =  (bc_end_time - bc_start_time) / dble(csert(instance)%MCSER(bc_index, NC)-1) & 
                           * dble(TCCSER(bc_index,NC)) / 86400.0d0
       end if
    case (indexControl+1 : indexControl+nrExchangeItemsControl) ! Control
       success  =  bc_index >= 1 .and. bc_index <= gateser(instance)%NQCTLM
       if (success) then  
          bc_start_time = gateser(instance)%GCSER(1, bc_index) * dble(GCCSER(bc_index)) / 86400.0d0
          bc_end_time = gateser(instance)%GCSER(gateser(instance)%MQCTL(bc_index), bc_index) * dble(GCCSER(bc_index)) &
                      / 86400.0d0
          bc_time_interval =  (bc_end_time - bc_start_time) / dble(gateser(instance)%MQCTL(bc_index)-1)
       end if
    case (indexXspecies+1 : indexXspecies+nrMaxXspecies) ! Water Quality 
       id = exchange_item_id - indexXspecies;
       NC = NC_xspecies_start + id
       success  =  bc_index >= 1 .and. bc_index <= NCSER(NC) .and. id <= NXSP
       if (success) then  
          bc_start_time = csert(instance)%TCSER(1, bc_index, NC)
          bc_end_time = csert(instance)%TCSER(csert(instance)%MCSER(bc_index,NC), bc_index, NC) &
                      * dble(TCCSER(bc_index,NC)) / 86400.0d0
          bc_time_interval =  (bc_end_time - bc_start_time) / dble(csert(instance)%MCSER(bc_index, NC)-1) &
                           * dble(TCCSER(bc_index,NC)) / 86400.0d0
       end if
    case default
       success = .false.
    end select
    success = success .and. &
     start_time >= (bc_start_time - epsilon) .and. &
     end_time <= (bc_end_time + epsilon) .and. &
     end_time >= start_time

    if (success) then
       start_index = nint( (start_time - bc_start_time) / bc_time_interval  ) +1 
       end_index = nint( (end_time - bc_start_time) / bc_time_interval) +1
    end if
    
  end function check_bc_indices


  ! --------------------------------------------------------------------------
  ! Check if start and end index are within the range of the grid size
  ! --------------------------------------------------------------------------
  function check_grid_indices(instance, exchange_item_id, start_index, end_index) result(success)

    use global, only : LA

    logical :: success     ! .true.  indices determined ok.
    ! .false. location_index out of bounds

    ! arguments
    integer         , intent(in)  :: instance      ! model instance identifier
    integer         , intent(in)  :: exchange_item_id       ! type of boundary condition (discharge_on_laterals)
    integer         , intent(in) :: start_index   ! index in grid 
    integer         , intent(in) :: end_index     ! index in grid

    select case (exchange_item_id)
    case (Grid_WaterLevel, Grid_Discharge, Grid_WaterTemperature)
       success =  (start_index >= 2) .and. (start_index <= end_index) .and. (end_index <= LA )
    case (gridIndexWQ+1: gridIndexWQ+nrExchangeItemsWQ)
       success =  (start_index >= 2) .and. (start_index <= end_index) .and. (end_index <= LA )
    case (gridIndexTOX+1: gridIndexTOX+nrExchangeItemsTOX)
       success =  (start_index >= 2) .and. (start_index <= end_index) .and. (end_index <= LA )
    case (gridIndexXspecies+1: gridIndexXspecies+nrMaxXspecies)
       success =  (start_index >= 2) .and. (start_index <= end_index) .and. (end_index <= LA )
    case default 
       success = .false.
    end select

  end function check_grid_indices


  ! --------------------------------------------------------------------------
  ! Enlarge the pointer arrays when we create a new model instance
  ! --------------------------------------------------------------------------
  subroutine add_instance_storage()

    ! locals
    character(len=max_path_length), dimension(:), pointer :: org_model_instance_dirs => NULL() ! current array of model instance dirs
    integer, dimension(:), pointer :: org_dm_outfile_handle
    type(state_vector), dimension(:), pointer :: org_model_instance_state => NULL()
    type(aser_time_series), dimension(:), pointer :: org_model_instance_aser => NULL()
    type(wser_time_series), dimension(:), pointer :: org_model_instance_wser => NULL()
    type(pser_time_series), dimension(:), pointer :: org_model_instance_pser => NULL()
    type(qser_time_series), dimension(:), pointer :: org_model_instance_qser => NULL()
    type(cser_time_series),  dimension(:), pointer :: org_model_instance_cser => NULL()
    type(gateser_time_series), dimension(:), pointer :: org_model_instance_gateser => NULL()


    !add additional storage for instance if necessary
    if (dm_model_instance_count > dm_max_dm_model_instance_count) then
       ! realloc directories
       org_model_instance_dirs => model_instance_dirs
       allocate(model_instance_dirs(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_dirs)) then
          model_instance_dirs(1:dm_max_dm_model_instance_count) = org_model_instance_dirs
          deallocate(org_model_instance_dirs)
       endif
       model_instance_dirs(dm_max_dm_model_instance_count+1:  &
         dm_max_dm_model_instance_count+ instances_realloc_size) = ' '

       org_dm_outfile_handle => dm_outfile_handle
       allocate(dm_outfile_handle(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_dm_outfile_handle)) then
          dm_outfile_handle(1:dm_max_dm_model_instance_count) = org_dm_outfile_handle
          deallocate(org_dm_outfile_handle)
       endif
       dm_outfile_handle(dm_max_dm_model_instance_count+1: &
          dm_max_dm_model_instance_count+ instances_realloc_size) = 0

       !realloc pointers to state vectors
       org_model_instance_state => state
       allocate(state(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_state)) then
          state(1:dm_max_dm_model_instance_count) = org_model_instance_state
          deallocate(org_model_instance_state)
       endif

       !realloc pointers to aser time series
       org_model_instance_aser => aser
       allocate(aser(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_aser)) then
          aser(1:dm_max_dm_model_instance_count) = org_model_instance_aser
          deallocate(org_model_instance_aser)
       end if
       
       !realloc pointers to wser time series
       org_model_instance_wser => wsert
       allocate(wsert(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_wser)) then
          wsert(1:dm_max_dm_model_instance_count) = org_model_instance_wser
          deallocate(org_model_instance_wser)
       end if

       !realloc pointers to pser time series
       org_model_instance_pser => psert
       allocate(psert(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_pser)) then
          psert(1:dm_max_dm_model_instance_count) = org_model_instance_pser
          deallocate(org_model_instance_pser)
       end if

       !realloc pointers to qser time series
       org_model_instance_qser => qsert
       allocate(qsert(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_qser)) then
          qsert(1:dm_max_dm_model_instance_count) = org_model_instance_qser
          deallocate(org_model_instance_qser)
       end if

       !realloc pointers to cser time series
       org_model_instance_cser => csert
       allocate(csert(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_cser)) then
          csert(1:dm_max_dm_model_instance_count) = org_model_instance_cser
          deallocate(org_model_instance_cser)
       end if

        !realloc pointers to gateser time series
       org_model_instance_gateser => gateser
       allocate(gateser(dm_max_dm_model_instance_count + instances_realloc_size))
       if (associated(org_model_instance_gateser)) then
          gateser(1:dm_max_dm_model_instance_count) = org_model_instance_gateser
          deallocate(org_model_instance_gateser)
       end if
       
       dm_max_dm_model_instance_count = dm_max_dm_model_instance_count + instances_realloc_size

    endif

  end subroutine add_instance_storage

  ! --------------------------------------------------------------------------
  ! Check if given model instance is valid
  ! --------------------------------------------------------------------------
  function valid_model_instance(instance_id) result(success)

    ! return value
    logical :: success     ! .true.  instance_id ok.
    ! .false. instance_id out of bounds, or not in memory

    ! arguments
    integer, intent(in)  :: instance_id          ! model instance id to be checked
    character(len=255)   :: message              ! error message
    
    success = .false.
    if ( instance_id < 1 ) then
        write(message, '(A, I3,A, I3)' ) &
          "Trying to access model instance ", instance_id, ", instance number should be a positive non zero integer."
        call write_message(message, M_FATAL)
    elseif (instance_id > dm_model_instance_count) then
        write(message, '(A,A, I3,A, I3)' ) &
           "Trying to access model instance ", instance_id, ", while only ", dm_model_instance_count, " are initialized."
        call write_message(message ,M_FATAL)
    else
        success = .true.
    endif
    
  end function valid_model_instance

  ! --------------------------------------------------------------------------
  ! Write error, warining or info message to message file
  ! --------------------------------------------------------------------------
  subroutine write_message(message, level)

    ! arguments
    character(len=*), intent(in)  :: message ! message to write to message file
    integer, intent(in)  :: level            ! level of message
                                             ! 1 = TRACE
                                             ! 2 = DEBUG
                                             ! 3 = INFO
                                             ! 4 = WARNING
                                             ! 5 = ERROR
                                             ! 6 = FATAL

    character(len=9), dimension(6), parameter :: & 
        message_prefix = (/'TRACE:   ', 'DEBUG:   ', 'INFO:    ' , 'WARNING: ' , 'ERROR:   ', 'FATAL:   ' /)
    
    write(*, '(A, A)' ) message_prefix(level), trim(message)
    write(message_file_handle, '(A, A)' ) message_prefix(level),  trim(message)
    call flush(message_file_handle)
    
  end subroutine write_message
  
  ! --------------------------------------------------------------------------
  ! Change directory give error on failare and return -1
  ! --------------------------------------------------------------------------
  function change_directory(path) result(ret_val)

    ! return value
    integer :: ret_val     ! 0 is ok. 
  
    ! arguments
    character(len=max_path_length), intent(in)  :: path ! path to directory

    !local
    integer:: status = 0
    character(len=max_message_length) :: message
    character(len=max_path_length) :: cwd !current working directory

    
    ret_val = 0
    print*, "change dir to", trim(path)
    write(dm_general_log_handle,'(A, A)') 'Change dir to: ', trim(path)
    call chdir(trim(path))
    call getcwd(cwd)
    write(dm_general_log_handle,'(A, A)') 'Current dir: ', trim(cwd)

    status = 0
    if (trim(path) .ne. trim(cwd) ) then
        ret_val = status
        write(message, '(A, A)') 'Failed to change directory to ', trim(path)
        call write_message(message , M_ERROR)
        write(message, '(A, A)') 'Current work directory is ', trim(cwd)
        call write_message(message , M_INFO)
    end if
    
  end function change_directory
  

  function c_to_f_string(string_c, string) result(ret_val)
    

    ! return value
    integer :: ret_val 

    ! arguments
    character(kind=c_char), intent(in) :: string_c(*) ! null_terminated C string
    character(kind=c_char,len=max_path_length), intent(out)  :: string ! fortran character string

    !local
    integer :: i

    string = ' '
    i = 1
    do while(i<max_path_length .and. string_c(i)/=C_NULL_CHAR)
        string(i:i) = string_c(i)
        i =i +1
    end do
    if (i>=max_path_length) then
      ret_val = -1
      return
    end if
    ret_val = 0

  end function c_to_f_string



  
end module m_openDA_wrapper
