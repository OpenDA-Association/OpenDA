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
! Module for overriding the Fortran intrinsic chdir and getcwd commands with
! the C versions. On AIX the C versions are called by default. 
! 
! ----------------------------------------------------------------------------
module chdir_mod

  use iso_c_binding
  implicit none

  interface
    function c_chdir(path) bind(C,name="chdir") result(res)
      use iso_c_binding
      character(kind=c_char) :: path(*)
      integer(c_int) :: res
    end function

    function c_getcwd(buffer,size) bind(c,name="getcwd") result(res)
      use iso_c_binding
      character(kind=c_char) ,intent(out) :: buffer(*)
      integer(c_size_t),value,intent(in)  :: size
      type(c_ptr)  :: res
    end function
  end interface

contains

  subroutine chdir(path, err)
    use iso_c_binding
    character(*) :: path
    integer, optional, intent(out) :: err
    integer(c_int) :: loc_err
    loc_err =  c_chdir(path//c_null_char)
    if (present(err)) err = loc_err
  end subroutine

  subroutine getcwd(output, err)
    use iso_c_binding
    character(kind=c_char,len=*),intent(out) :: output
    integer, optional, intent(out) :: err
    integer :: loc_err
    type(c_ptr) :: buffer
    integer(c_long) :: length,i
    length=len(output)
    buffer=c_getcwd(output,length)
    if (output(1:1) == c_null_char) then
        loc_err = 1
    else 
      do i=1,length
        loc_err = 0
        if(output(i:i) == c_null_char) exit
      enddo
    end if
    output(i:)=' '
    if (present(err)) err = loc_err
  end subroutine

end module chdir_mod
