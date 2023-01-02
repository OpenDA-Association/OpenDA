!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/src/cta/cta_util_timing.f90 $
! $Revision: 680 $, $Date: 2008-10-10 17:01:17 +0200 (Fri, 10 Oct 2008) $
! 
!COSTA: Problem solving environment for data assimilation
!Copyright (C) 2008  Nils van Velzen
!
!This library is free software; you can redistribute it and/or
!modify it under the terms of the GNU Lesser General Public
!License as published by the Free Software Foundation; either
!version 2.1 of the License, or (at your option) any later version.
!
!This library is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!Lesser General Public License for more details.
!
!You should have received a copy of the GNU Lesser General Public
!License along with this library; if not, write to the Free Software
!Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


!DEC$ ATTRIBUTES DLLEXPORT::cta_util_timing
subroutine cta_util_timing(dtimes,itime,start)
implicit none
real(kind=8), dimension(2,3,*), intent(inout) ::dtimes
integer,                        intent(in)    ::itime
integer,                        intent(in)    ::start

real(kind=8) ::wtime,ctime

call cta_util_timing_walltime(wtime)
call cpu_time(ctime)
if (start>0) then
   dtimes(1,1,itime)=wtime
   dtimes(2,1,itime)=ctime
else
   dtimes(1,2,itime)=wtime
   dtimes(2,2,itime)=ctime
   dtimes(1,3,itime)=dtimes(1,3,itime)+(dtimes(1,2,itime)-dtimes(1,1,itime))
   dtimes(2,3,itime)=dtimes(2,3,itime)+(dtimes(2,2,itime)-dtimes(2,1,itime))
endif


end subroutine cta_util_timing


subroutine cta_util_timing_cputime(dtime)
implicit none
real(kind=8), intent(out)  ::dtime

call cpu_time(dtime)

end subroutine cta_util_timing_cputime




subroutine cta_util_timing_walltime(dtime)
implicit none
real(kind=8), intent(out)  ::dtime

integer, dimension(8) ::times
integer ::year,month,day,hour,minutes,seconds,milisec,julnum
integer ::imon1


call date_and_time(values=times)
year   =times(1)
month  =times(2)
day    =times(3)
hour   =times(5)
minutes=times(6)
seconds=times(7)
milisec=times(8)

!calculate julian day number
imon1 = (month-14)/12;
julnum = day - 32075 + 1461 * ( year + 4800 + imon1 ) / 4 &
         + 367 * ( month - 2 - imon1 * 12 ) / 12         &
         - 3 * ( ( year + 4900 + imon1 ) / 100 ) / 4;

dtime=(((dble(julnum)*24.0d0+dble(hour))*60.0d0+dble(minutes))*60.0d0)+dble(seconds)+dble(milisec)*1.0d-3


end subroutine cta_util_timing_walltime



