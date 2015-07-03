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

  void oda_ewfp_sethx(double[] Hx, IntByReference iEns, IntByReference Ny,IntByReference Ne);


  void oda_ewfp_set_l(double[] newL, IntByReference iEns, IntByReference Nx, IntByReference Ne);

  void oda_ewfp_get_l(double[] newL, IntByReference iEns, IntByReference Nx, IntByReference Ne);

  void oda_ewfp_set_diagr(double[] newDiagR, IntByReference Ny);


  void  oda_ewfp_setuprooterror(IntByReference Ne);


}
