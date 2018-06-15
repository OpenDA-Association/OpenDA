!
! $URL: https://repos.deltares.nl/repos/openda/public/trunk/core/native/models/oscill/src/ode.f90 $
! $Revision: 671 $, $Date: 2008-10-07 14:49:42 +0200 (Tue, 07 Oct 2008) $
!
! COSTA: Problem solving environment for data assimilation
! Copyright (C) 2006  Nils van Velzen
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

module ode

public runge_kutta4
contains

subroutine runge_kutta4(tstart,tstop,dt,x_in,func_dx, func_noise, addnoise, &
                        noise, x_out, t_out, param)

! purpose                 : perform time integration with a 4th-order
!                           runge-kutta scheme
! arguments               : tstart  - start time of integration
!                           tstop   - final time of integration
!                           dt      - timestep
!                           x_in    - initial state
!                           dx_func - function to compute gradient
!                           userdat - struct with userdata needed by d_func
!                           dw_func (optional) - function to add noise
! output                  : x_out   - state-vectors at final time (this is approximately tstop)
!                           t_out   - output times
! other requirements      : dx_func will be evaluated as
!                           dx = feval(dx_func,t,x,userdat);
!                           dw is added to the state each timestep
!                           dw_func is evaluated as dw=dwfunc(t,dt,x,userdat)

   implicit none
   real(kind=8), dimension(2) ::param
   ! arguments
   real(kind=8), intent (in)    ::tstart, tstop, dt
   real(kind=8), dimension(:)   ::x_in, x_out
   real(kind=8)                 ::t_out
   real(kind=8), dimension(:,:) ::noise
   logical                      ::addnoise
   external func_dx
   external func_noise

   real(kind=8)  tnow, t
   real(kind=8), dimension(size(x_in)) ::dx0,dx1,dx2,dx3,dw,dx,x
    
   
   !initial
   x=x_in
   !time-loop
   tnow  = tstart
   t = tstart
   do 
      if (t > tstop) exit

         call func_dx(t,x, dx, param)
         dx0 = dt*dx
         call func_dx(t+0.5*dt,x+0.5*dx0, dx, param)
         dx1 = dt*dx
         call func_dx(t+0.5*dt,x+0.5*dx1, dx, param)
         dx2 = dt*dx
         call func_dx(t+1.0*dt,x+1.0*dx2, dx, param)
         dx3 = dt*dx              

         x   = x + (1.0/6.0)*(dx0+2*dx1+2*dx2+dx3);
         tnow=t+dt;
         
         if (addnoise) then
            call func_noise(t,dt,x,noise,dw);
            x=x+dw
         endif
      t = t + dt
   enddo

   !final 
   x_out=x;
   t_out=tnow;

end subroutine runge_kutta4

subroutine runge_kutta4_adjoint(tstart,tstop,dt,adx_in,dx_func,addx_func,param,xfwdrun,ad_force, &
                        adx_out,t_out,adp_in,adp_out)
! Adjoint of Runge-Kutta4. Monday, 14 May 2007, JHS
! purpose                 : perform time integration with an adjoint of 4th-order
!                           runge-kutta scheme
! arguments               : tstart  - start time of integration
!                           tstop   - final time of integration (tstop < tstart)
!                           dt      - timestep
!                           adx_in  - initial adjoint state
!                           dx_func - function to compute gradient (fwd run)
!                           addx_func - function to compute adjoint of dx/dt
!                           param   - model parameters
!                           xfwdrun - stored forward-run states
!                           ad_force - forcing of the adjoint model
!                           adp_in - adjoint of parameters (OPTIONAL)
! output                  : adx_out - adjoint state-vectors at final time (this is approximately tstop)
!                           t_out   - output times

   implicit none
   include "cta_f90.inc"
! NOTE:
! Hmm..this program is supposed to be the internal part of the model, and not yet the interface with COSTA.
! However, it seems that we already need to work with COSTA variable here, e.g.
! for accessing the xfwdrun and ad_force. Hmm..is it a good practice to already
! use COSTA variable here? What is a better alternative to do this?

   ! arguments:
   real(kind=8), intent (in)    ::tstart, tstop, dt
   real(kind=8), dimension(:)   ::adx_in, adx_out
   real(kind=8), dimension(:), optional :: adp_in, adp_out
   real(kind=8)                 ::t_out
   real(kind=8), dimension(:)   ::param
   external dx_func
   external addx_func

   integer xfwdrun  ! CTA_Handle to stored forward-run states
   integer ad_force ! CTA_Handle to the forcing of adjoint model

   ! local variables:
   real(kind=8)  tnow, t
   real(kind=8), dimension(size(adx_in)) ::dx0,dx1,dx2,dx3,dx,x
   real(kind=8), dimension(size(adx_in)) ::addx0,addx1,addx2,addx3,addx,adx
   real(kind=8), dimension(size(adx_in)) ::adx_temp, adp, adp_temp
    
   
   !initial
   adx=adx_in
   !time-loop
   tnow  = tstart
   t = tstart
   do 
      if (t < tstop) exit

         ! add adjoint forcing
         ! TO BE CREATED: call cta_model_getadjforce(hmodel,adforce,...) ?? or
         !                call cta_model_util_getadjforce(t1,t2,adforce,...) ??
         !             or call getadforce_func(tnow,dt,adforce,userdat). In this
         !             case, how are we going to pass the keywords for accessing
         !             data in the sql database? Remember that we need keywords
         !             for both time and state-element-index (e.g.
         !             'TIME','INDX'). If the keywords are accessible from
         !             userdat then it will be much easier. However, as far as I
         !             remember, these keywords are hardcoded by the modeller.
         !             Look for example in the subroutine 'oscill_obs'. Is it OK
         !             to hardcode these keywords as well here? Is there any way
         !             we can avoid hardcoding the keywords here? Well, to keep
         !             symmetry with 'oscill_obs', I think it is ok to hardcode
         !             these keywords here, since the one who develops the model
         !             is likely to be the same person who develops
         !             runge_kutta4_adjoint.
         ! CHARACTERISTICS OF THIS FUNCTION:
         !
         ! adx = adx + adforce

         ! compute tnow
         tnow = t - dt
         ! get state x at tnow
         ! TO BE CREATED: cta_model_util_getfwdrun(tnow,x,...) or
         !                call getfwdrun_func(tnow,x,hmodel)
         !                      here 'hmodel', an integer, is a handle to
         !                      CTA_Model. This is the only CTA handle used
         !                      within subroutine runge_kutta4_adjoint, and it
         !                      is only passed on as an input argument to other
         !                      functions, e.g. getfwdrun_func and
         !                      getadjforce_func. What do you think about this?
         !                      is it a good software design? The thing is that
         !                      a modeller can design the way to store/retrieve
         !                      fwdrunstate and adforce whatever he likes. By
         !                      using functions' names as input arguments to
         !                      runge_kutta4_adjoint we hope that this program
         !                      can be used for general case. It only requires
         !                      the user to provide those two functions.
         !                      However, mind you that from within
         !                      runge_kutta4_adjoint, the calls to these
         !                      routines require 'hmodel' as an input argument.
         !                      This shows that we already assume that the
         !                      modeller will store the fwdrunstate and adforce
         !                      as COSTA variables. Hmm..so what should we do?
         !                      O..maybe we can just use a general 'userdat'
         !                      variable for this? So for example it should
         !                      contain 'hmodel' in our case. In Martin's
         !                      program, 'userdat' is a struct with user data,
         !                      which is actually a handle to model object. What
         !                      if the modeller doesn't use any COSTA variable?
         !                      Well, it is not likely that he won't use COSTA
         !                      variable I guess, if he already use COSTA in the
         !                      higher level, like e.g. the costfunction. It is
         !                      in costfunction the fwdrunstate and adforce are
         !                      stored. Since COSTA will provide functions,
         !                      which can be used easily for storing these
         !                      variables, I can't see any reason anymore why a
         !                      user doesn't just use them in his code. Anyway,
         !                      it is very likely that the modeller will call
         !                      other functions for performing these tasks. Can
         !                      we think of what the general form of the
         !                      functions will be? For getfwdrun_func it will
         !                      require at least the time, t, and another
         !                      variable which store the fwdrunstate as input
         !                      arguments, and a state array,x, as an output
         !                      argument. Well, it may require two arguments for
         !                      storing the fwdrunstate: an array for the state and
         !                      an array for the time. Unless if we can combine
         !                      them into a single struct argument. In this case
         !                      we only need one argument. So we just identified
         !                      that it is possible to use only three arguments
         !                      for getfwdrun_func (i.e. two input arguments and
         !                      one output). One of the input arguments need to
         !                      be specified as a struct variable. Since we need
         !                      to declare the variable (in Fortran we need to
         !                      declare all the variable), while yet there is
         !                      more than one possibilities of the data type of
         !                      this struct variable, we can NOT make a general
         !                      function of runge_kutta4_adjoint, which is
         !                      applicable for any cases. So, we have to choose
         !                      one type for this struct variable. At the
         !                      moment, the easiest option is to use CTA_Handle
         !                      data type, i.e. an integer for 'userdat'
         !                      containing a handle to model-object. Yes, let's
         !                      do that!
         ! CHARACTERISTICS OF THIS FUNCTION:

         ! recompute intermediate value
         call dx_func(tnow,x, dx, param)
         dx0 = dt*dx
         call dx_func(tnow+0.5*dt,x+0.5*dx0, dx, param)
         dx1 = dt*dx
         call dx_func(tnow+0.5*dt,x+0.5*dx1, dx, param)
         dx2 = dt*dx
         call dx_func(tnow+1.0*dt,x+1.0*dx2, dx, param)
         dx3 = dt*dx      
         ! xnew = x + (1.0/6.0)*(dx0+2*dx1+2*dx2+dx3)

         ! adjoint for: x = x + 1/6*(dx0+2*dx1*2*dx2+dx3)
         addx0 = (1.0/6.0)*adx
         addx1 = (2.0/6.0)*adx
         addx2 = (2.0/6.0)*adx
         addx3 = (1.0/6.0)*adx

         ! adjoint for: dx3 = dt*dx_func(tnow+1.0*dt,x+1.0*dx2,dx,param)
         if (present(adp_in)) then
                 call addx_func(tnow+1.0*dt,x+1.0*dx2,addx3,param,adx_temp,adp_temp)
                 adp = adp + dt*adp_temp
         else
                 call addx_func(tnow+1.0*dt,x+1.0*dx2,addx3,param,adx_temp)
         end if
         addx2 = addx2 + dt*adx_temp
         adx = adx + dt*adx_temp

         ! adjoint for: dx2 = dt*dx_func(tnow+0.5*dt,x+1.0*dx1,dx,param)
         if (present(adp_in)) then
                 call addx_func(tnow+0.5*dt,x+0.5*dx1,addx2,param,adx_temp,adp_temp)
                 adp = adp + dt*adp_temp
         else
                 call addx_func(tnow+0.5*dt,x+0.5*dx1,addx2,param,adx_temp)
         end if
         addx1 = addx1 + dt*0.5*adx_temp
         adx = adx + dt*adx_temp

         ! adjoint for: dx1 = dt*dx_func(tnow+0.5*dt,x+1.0*dx0,dx,param)
         if (present(adp_in)) then
                 call addx_func(tnow+0.5*dt,x+0.5*dx0,addx1,param,adx_temp,adp_temp)
                 adp = adp + dt*adp_temp
         else
                 call addx_func(tnow+0.5*dt,x+0.5*dx0,addx1,param,adx_temp)
         end if
         addx0 = addx0 + dt*0.5*adx_temp
         adx = adx + dt*adx_temp

         ! adjoint for: dx0 = dt*dx_func(tnow,x,dx,param)
         if (present(adp_in)) then
                 call addx_func(tnow,x,addx0,param,adx_temp,adp_temp)
                 adp = adp + dt*adp_temp
         else
                 call addx_func(tnow,x,addx0,param,adx_temp)
         end if
         adx = adx + dt*adx_temp

      t = tnow
   enddo

   !final 
   adx_out=adx;
   t_out=tnow;

end subroutine runge_kutta4_adjoint



end module ode

