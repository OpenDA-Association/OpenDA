/* OpenDA v2.4 
* Copyright (c) 2017 OpenDA Association 
* All rights reserved.
* 
* This file is part of OpenDA. 
* 
* OpenDA is free software: you can redistribute it and/or modify 
* it under the terms of the GNU Lesser General Public License as 
* published by the Free Software Foundation, either version 3 of 
* the License, or (at your option) any later version. 
* 
* OpenDA is distributed in the hope that it will be useful, 
* but WITHOUT ANY WARRANTY; without even the implied warranty of 
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
* GNU Lesser General Public License for more details. 
* 
* You should have received a copy of the GNU Lesser General Public License
* along with OpenDA.  If not, see <http://www.gnu.org/licenses/>.
*/
package org.openda.algorithms.particleFilter;
/**
 * Created by nils on 30/01/15.
 */

import com.sun.jna.Library;
import com.sun.jna.ptr.DoubleByReference;
import com.sun.jna.ptr.IntByReference;

public interface ISangomaEWPFNativeDLL extends Library {

/*
	subroutine proposal_step(Nx,Ny,Ne,weight,x_n,x_n1,y,timestep,obsstep,steps_btw_obs, &
           cb_H, cb_HT, cb_Qhalf, cb_solve_r)  bind(C, name="proposal_step_")
          use, intrinsic :: ISO_C_BINDING
  use sangoma_base, only: REALPREC, INTPREC
  implicit none

  integer(INTPREC), intent(in) :: Nx,Ny,Ne                  ! state, observation, ensemble dimensions
  integer(INTPREC), intent(in) :: timestep                  ! current model timestep
  integer(INTPREC), intent(in) :: obsstep,steps_btw_obs     ! number of next observation set
  real(REALPREC), intent(in), dimension(Ny)    :: y         ! vector of the next set of observations (in future)
  real(REALPREC), intent(inout), dimension(Nx,Ne) :: x_n1   ! state matrix (with all particles) from previous time n-1
  real(REALPREC), intent(inout), dimension(Nx,Ne) :: x_n    ! state matrix at the current time step n
  real(REALPREC), intent(inout), dimension(Ne) :: weight    ! vector containing weights for all particles
	 */


  void oda_proposal_step(IntByReference Ne, IntByReference Nx,IntByReference Ny,
                         double[] weight, double[] y, IntByReference timestep, IntByReference obsstep, IntByReference steps_btw_obs);

  void oda_equal_weight_step(IntByReference Ne,IntByReference Nx,IntByReference Ny,double [] weight, double[] y);

  void oda_ewfp_set_l(double[] newL, IntByReference iEns, IntByReference Nx, IntByReference Ne);

  void oda_ewfp_get_l(double[] newL, IntByReference iEns, IntByReference Nx, IntByReference Ne);

  void oda_ewfp_set_diagr(double[] newDiagR, IntByReference Ny);


  void  oda_ewfp_setuprooterror(IntByReference Ne);


}
