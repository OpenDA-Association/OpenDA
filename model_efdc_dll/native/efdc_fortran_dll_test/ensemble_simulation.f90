program ensemble_simulation

use m_openda_wrapper
use global

implicit none

character(len=255):: parent_dir, template_dir, instance_dir
character(Len=4) :: dummy
integer :: i, instance
character(len=1) :: cinstance
integer :: values_count, ret_val, nr_instances
real(kind = 8) :: time_period, time_start, reference_year
double precision, allocatable, dimension(:) ::  values 

! parse command line argumen
call get_command_argument(1, parent_dir)
call get_command_argument(2, template_dir)
call get_command_argument(3, dummy)
read(dummy,'(i4)') nr_instances


ret_val = init(parent_dir,template_dir)
do i = 1, nr_instances 
    write(cinstance,'(I1)') i-1
    instance_dir = trim(parent_dir)//"/work"//cinstance
    instance = get_model_instance(instance_dir)
    reference_year = get_reference_year(instance)
end do

ret_val = get_reference_period(time_period)
time_start = dble(TBEGIN * TCON)
time_start = time_start / 86400.d0

do i=1, nr_instances
    !ret_val = model_set_time(real(time_start))
    ret_val = restore_instance(i)
    ret_val = compute(i, time_start , time_start + 60.d0/86400.d0 )
    ret_val = save_instance(i)
    ret_val = restore_instance(i)
    ret_val = compute(i, time_start + 60.d0/86400.d0 , time_start + 120.d0/86400.d0 )
    ret_val = save_instance(i)
    ret_val = store_current_instance_restart_files()
    if (ret_val .ne. 0) print*, "ERROR writing restart_files"
end do

call destroy() 

end program ensemble_simulation
