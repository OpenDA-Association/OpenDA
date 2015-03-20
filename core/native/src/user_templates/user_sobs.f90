! 
! $URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_stoch_observer_netcdf.c $
! $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
! 
! COSTA: Problem solving environment for data assimilation
! Copyright (C) 2010  Nils van Velzen
! 
! This library is:q free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
! 
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
! 
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
! 
subroutine user_sbs_create_size(memsize,retval)
implicit none
include '../../include/cta_f77.inc'


    ! --- in/out -----------------------------------

   integer memsize      ! The number of bytes which are necessary to store
                        ! All local data of a single instance 
   integer retval       ! error code (see cta_datatypes.h for possible error codes)

    ! --- const ----------------------------------

    character(len=*), parameter   ::  rname = 'user_sobs_Create_Size'

    ! --- local ------------------------------------

    integer ::  sizeint

    ! --- begin ------------------------------------

    ! get the number of bytes in a standard integer:
    call CTA_SizeOf( CTA_HANDLE, sizeint, retval )

    ! allocate enough space for handles:
    memsize = 1 * sizeint
    print *,'End of user_sobs_Create_Size: memsize=',memsize

end subroutine user_sobs_create_size


subroutine user_sobs_create_init(x, userdata, retval)
use, intrinsic ::ISO_C_BINDING
implicit none
include '../../include/cta_f77.inc'

!
!   Allocate the memory which is neccesary to store the data necessary for a
!   netcdf-observer
!
   integer ::x(*)        ! data/state of this stoch observer instance
   integer, value ::userdata ! (user) input for creating the instance (e.g. filename)    
   integer ::retval      ! Error code. Possible error: Illegal data type

  character(len=1000) ::inputfile
  
  print *,'Welcome in user_sobs_Create_Init', userdata
  
  call cta_string_get(userdata,inputfile,retval)


   print *,'Calling user_sobs_Create_Init'
   print *,'Name of input file is', trim(inputfile)

   call udf_sobs_create_init(x, inputfile, retval)

   
   retval=CTA_OK

end subroutine user_sobs_create_init


subroutine user_sobs_createselstring(obsin, userdata, obsout, retval)
implicit none
include '../../include/cta_f77.inc'
   integer obsin(*)
   integer userdata 
   integer obsout(*)
   integer retval

   obsout(1)=CTA_NULL
   print *,'Calling user_sobs_CreateSelString'
   retval=CTA_OK


end subroutine user_sobs_createselstring



subroutine user_sobs_createsel( obsin, userdata, obsout, retval)
implicit none
include '../../include/cta_f77.inc'
   integer obsin(*) ! The netcdf-observer of which a selection is to be made
   integer userdata !!! by value !!!! condition
   integer obsout   ! The netcdf-observer which is a selection of observer obsin
   integer retval   ! Error code

   obsout=CTA_NULL
   print *,'Calling user_sobs_createsel'
   retval=CTA_OK


end subroutine user_sobs_createsel

! Return the number of measurements in the observer
subroutine user_sobs_count(x, nmeasr, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*)  ! The StochObserver of which the number of measurements is returned 
   integer nmeasr
   integer retval

   nmeasr=0
   print *,'Calling user_sobs_count'
   retval=CTA_OK

end subroutine user_sobs_count

! Return all the values from the StochObserver
subroutine user_sobs_getvals(x, hvec, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*) ! StochObserver from which the measurements are returned
   integer hvec ! COSTA-vector containing the values
   integer retval

   print *,'Calling user_sobs_getvals'
   retval=CTA_OK

end subroutine user_sobs_getvals



! Return all the times that are associated to th observations in the StochObserver
subroutine user_sobs_gettimes(x, hvec, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*) ! StochObserver 
   integer hvec ! COSTA-vector containing the times
   integer retval

   print *,'Calling user_sobs_GetTimes'
   retval=CTA_OK

end subroutine user_sobs_gettimes


subroutine user_sobs_getvariances(x, returnvar, hvec, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*)      ! StochObserver 
   integer returnvar ! return variance (CTA_TRUE) or std (CTA_FALSE)
   integer hvec      ! COSTA-vector containing the times
   integer retval

   print *,'Calling user_sobs_GetVariances'
   retval=CTA_OK

end subroutine user_sobs_getvariances


! Return a stochastic realizations for all the measurements in the StochObserver
subroutine user_sobs_getrealisation(x, hvec, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*) ! StochObserver 
   integer hvec ! COSTA-vector containing the realizations
   integer retval

   print *,'Calling user_sobs_GetRealisation'
   retval=CTA_OK

end subroutine user_sobs_getrealisation


! Return the covariance matrix of the measurements in the StochObserver
subroutine user_sobs_getcovmat(x, hmat, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*) ! StochObserver 
   integer hmat ! COSTA-matrix containing the covariances
   integer retval

   print *,'Calling user_sobs_getcovMat'
   retval=CTA_OK

end subroutine user_sobs_getcovmat

subroutine user_sobs_export(x, userdata, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*)
   integer userdata
   integer retval

   print *,'Calling user_sobs_export'
   retval=CTA_OK

end subroutine user_sobs_export

subroutine user_sobs_free(x, retval)
implicit none
include '../../include/cta_f77.inc'
   integer x(*)
   integer retval

   print *,'Calling user_sobs_Free'
   retval=CTA_OK
end subroutine user_sobs_Free







