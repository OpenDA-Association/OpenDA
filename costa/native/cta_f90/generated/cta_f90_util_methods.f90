module cta_f90_util_methods

  implicit none

  public

  !  \brief print the predicted values and the observed values
  ! 
  !  \param fgModel     O Model of foreground run (with data
  !                       assimilation) or Vector with the predicted
  !                       values of the foreground run.
  !                       if set to CTA_NULL NaN will be printed as result
  !  \param bgModel     I Model of background run (without data
  !                       assimilation) or Vector with the predicted
  !                       values of the background run.
  !                       if set to CTA_NULL NaN will be printed as result
  !  \param sObs        I Stochastic observer
  !  \param time        I Corresponding time
  !  \param file        I Output file (note CTA_FILE_STDOUT prints to screen)
  !  \param printHeader I Print header CTA_TRUE/CTA_FALSE
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Util_MethodsPrintObservations
    subroutine CTA_Util_MethodsPrintObservations( fgModel, bgModel, sObs, time, file, printHeader, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  fgModel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  bgModel
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  sObs
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  time
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  file
      integer                       , intent(in   )     ::  printHeader
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Util_MethodsPrintObservations
  end interface

  !  \brief Make an initial selection of the observations
  ! 
  !   The selection of observations is based on the given simulation timespan
  !   and the criterion provided by the model (CTA_Model_GetObsSelect)
  ! 
  !   \note sObsSel is created and should be freed by the caller of this routine
  ! 
  !  \param model      I Model (for CTA_Model_GetObsSelect)
  !  \param sObsAll    I All observations
  !  \param spanSim    I Simulation timespan
  !  \param sObsSel    O Selection of observations
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Util_MethodsSelectObservations
    subroutine CTA_Util_MethodsSelectObservations( model, sObsAll, spanSim, sObsSel, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  model
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  sObsAll
      integer(CTA_HANDLE_IKIND)     , intent(in   )     ::  spanSim
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  sObsSel
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Util_MethodsSelectObservations
  end interface

  !  \brief Create an output file for filter predictions at station
  !          locations
  ! 
  !   The routine  CTA_Util_MethodsPrintObservations can be used for writing
  !   the results.
  !   \note the header is written by this call
  ! 
  !  \param   stationFile  (I) Name of result file
  !  \param   fStationFile (O) Handle to result file
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Util_MethodsOpenResultFile
    subroutine CTA_Util_MethodsOpenResultFile( stationFile, fStationFile, status )
      use CTA_F90_Parameters, only : CTA_HANDLE_IKIND
      character(len=*)              , intent(in   )     ::  stationFile(*)
      integer(CTA_HANDLE_IKIND)     , intent(out  )     ::  fStationFile
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Util_MethodsOpenResultFile
  end interface


end module cta_f90_util_methods

