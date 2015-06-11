! Program to test the continuous ranked scores 
! Just define an ensemble vector and a single observation 
program example_computeCRIGN
 
implicit none 
 
integer,parameter :: dim_ens=10 
real :: obs 
real :: fc_ens(dim_ens) 
real :: crps,crign 
 
external :: sisort



fc_ens = (/ 8.,10.,2.,3.,1.,4.,7.,5.,6.,9. /)  !unsorted ensemble values 
obs = 5.4  !truth for verification 
 
write(*,*) "Test the CRPS / CRIGN routine with:"
write(*,*) '--Note that both are univariate scores--' 
WRITE(*,*) '--The example ensemble is:'
write(*,'(10X,F5.1)') , fc_ens 
write(*,*) '--The 1st verification observation is:'
write(*,*) obs 
call sangoma_computecrign(dim_ens, fc_ens, obs, crps, crign,sisort  ) 
write(*,*) "CRIGN = ", crign 
write(*,*) '--------------------------------------'
obs = 20. ! Observation is far away from ensemble members
write(*,*) '--The 2nd verification observation is:'
write(*,*) "Observation =", obs
call sangoma_computecrign(dim_ens, fc_ens, obs, crps, crign,sisort  ) 
write(*,*) "CRIGN = ", crign 


end program example_computeCRIGN 


