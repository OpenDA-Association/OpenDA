! $Id: user_base.f90 548 2015-02-21 09:19:29Z larsnerger $


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!    A module containing user defined parameters for 
!!!    equivalent weights particle filter (EWPF) code.
!!!    Copyright (C) 2015  Sanita Vetra-Carvalho
!!!
!!!    This program is distributed under the Lesser General Public License (LGPL)
!!!    version 3, for more details see <https://www.gnu.org/licenses/lgpl.html>.
!!!
!!!    Email: s.vetra-carvalho @ reading.ac.uk
!!!    Mail:  School of Mathematical and Physical Sciences,
!!!    	      University of Reading,
!!!	      Reading, UK
!!!	      RG6 6BB!!!    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module user_base

use sangoma_base ! use sangoma defined precision for C-Bind
implicit none

real(REALPREC) :: keep = 0.8                  ! percentage of kept particles

real(REALPREC) :: nstd = 1.0                  ! normal standard deviation of 1
real(REALPREC) :: nmean = 0.0                 ! normal mean of 0
real(REALPREC) :: ufac = 0.00001              ! parameter for uniform distribution in 
                                              ! equal_weight_step
real(REALPREC) :: efacNum = 0.001             ! parameter for uniform distribution in 
                                              ! equal_weight_step


real(REALPREC) :: freetime = 0.6              ! parameter in Bprime routine of freetime 
                                              ! i.e. time with no nudging
real(REALPREC) :: nudgefac = 0.9              ! nudging strength parameter 


end module user_base
