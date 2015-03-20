!
! $URL: https://repos.deltares.nl/repos/openda/trunk/costa/src/cta/cta_obsdescr_netcdf.c $
! $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
!
! COSTA: Problem solving environment for data assimilation
! Copyright (C) 2010  Nils van Velzen
!
! This library is free software; you can redistribute it and/or
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

subroutine user_obsdescr_maori_Create_Size(memsize, retval)
implicit none
   integer, intent(out) ::memsize ! The number of bytes to store de admin of a stochastic observer
   integer, intent(out) ::retval  ! Status code of this call

end subroutine user_obsdescr_maori_Create_Size


subroutine user_obsdescr_maori_Create_Init(myhandle, memblk, filename, retval)
implicit none
   integer, intent(in)    ::myhandle   ! Handle of this new created object
   integer, intent(inout) ::memblk(*)  ! Data block for storing admin of this stochastic observer
   integer, intent(in)    ::filename   ! (CTA_String) name of input filename
   integer, intent(out)   ::retval     ! Status code of this call

end subroutine user_obsdescr_maori_Create_Init


subroutine user_obsdescr_maori_CreateSel(descr, selection, reltab, myhandle_out, &
                                         descrout, retval)
implicit none
   integer, intent(inout) ::descr(*)
   integer, intent(in)    ::selection    ! (CTA_String)
   integer, intent(out)   ::reltab       ! (CTA_RelTable)
   integer, intent(out)   ::myhandle_out ! (CTA_ObsDescr)
   integer, intent(out)   ::descrout     ! (user_obsdescr)
   integer, intent(out)   ::retval       ! Status code of this call

end subroutine user_obsdescr_maori_CreateSel
         
subroutine user_obsdescr_maori_Get_Keys(descr, Keys, retval)
implicit none
   integer, intent(inout) ::descr(*)  ! user_obsdescr *descr,
   integer, intent(out)   ::keys    ! CTA_Vector *Keys,
   integer, intent(out)   ::retval

end subroutine user_obsdescr_maori_Get_Keys


subroutine user_obsdescr_maori_Property_Count(descr, nkeys, retval)
implicit none
 integer, intent(in)            ::descr(*)       ! user_obsdescr
 integer, intent(out)           ::nKeys       ! user_obsdescr
 integer, intent(out)           ::retval  

end subroutine user_obsdescr_maori_Property_Count


subroutine user_obsdescr_maori_Observation_Count( descr, nobs, retval)
implicit none
   integer, intent(in)  ::descr(*)       ! user_obsdescr
   integer, intent(out) ::nobs
   integer, intent(out) ::retval

end subroutine user_obsdescr_maori_Observation_Count



subroutine user_obsdescr_maori_Get_Properties( descr, Key, Properties, datatype, retval)
implicit none
 integer, intent(in)           ::descr(*)       ! user_obsdescr
 character(len=1) , intent(in) ::key(*)      ! NOTE THIS IS A C-STRING!!!!!
 integer, intent(inout)        ::Properties  ! CTA_Vector
 integer, intent(in)           ::datatype    ! CTA_Datatype
 integer, intent(out)          ::retval

end subroutine user_obsdescr_maori_Get_Properties



subroutine user_obsdescr_maori_Free( descr, retval)
implicit none
   integer, intent(inout) ::descr(*) 
   integer, intent(out)   ::retval

end subroutine user_obsdescr_maori_Free



