subroutine model_make_step(time_period)

use global
use model_state

implicit none

real(kind=8), intent(in) :: time_period ! period to integrate the model in seconds

! Set the number of time steps to make
NTS = nint(time_period/DT)

! **  SELECT FULL HYDRODYNAMIC AND MASS TRANSPORT CALCULATION OR
! **  LONG-TERM MASS TRANSPORT CALCULATION
NITERAT=0
IF(IS2TIM.EQ.0) then 
    write(*,'(A,F8.4,A,F6.1,A)') "time integration with HDMT from day ", TBEGIN, ' over ', time_period / 60.0, ' minutes'
    CALL HDMT
elseif (IS2TIM.GE.1) then
    write(*,'(A,F8.4,A,F6.1,A)') "time integration with HDMT2T from day ", TBEGIN, ' over ', time_period / 60.0, ' minutes'
    CALL HDMT2T
end if

end subroutine model_make_step
