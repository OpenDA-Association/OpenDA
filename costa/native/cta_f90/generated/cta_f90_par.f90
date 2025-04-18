module cta_f90_par

  implicit none

  public

  !  \brief Initialises parallel environment for a process that spawned
  !   The executable is spawned using MPI_COMM_SPAWN or MPI_COMM_SPAWN_MULTIPLE
  ! 
  !   It will set up the communication groups and optionally starts the parallel
  !   model builder
  ! 
  !  \param StartPar  I  CTA_TRUE/CTA_FALSE start parallel model builder
  ! 
  !  Note when a worker process is part of a Master-Worker model and it does
  !  not implement the COSTA model interface it should not start the parallel model builder
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Par_WorkerSpawn
    subroutine CTA_Par_WorkerSpawn( StartPar, status )
      integer                       , intent(in   )     ::  StartPar
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Par_WorkerSpawn
  end interface

  !  \brief Initialises parallel environment and create process groups
  ! 
  !  \param parConfig I  configuration input from XML-file
  !  \param StartPar  I  CTA_TRUE/CTA_FALSE start parallel model builder
  ! 
  !  Note when a worker process is part of a Master-Worker model and it does
  !  not implement the COSTA model interface it should not start the parallel model builder
  ! 
  !  \param status O error status: CTA_OK if successful
  !
  interface CTA_F90_Par_CreateGroups
    subroutine CTA_Par_CreateGroups( parConfig, StartPar, status )
      integer                       , intent(in   )     ::  parConfig
      integer                       , intent(in   )     ::  StartPar
      integer                       , intent(out  )     ::  status
    end subroutine CTA_Par_CreateGroups
  end interface

  !  \brief Get the global number of COSTA process group and index of this process
  ! 
  !  \param itime  O The index of this process in the group
  !  \param iGroup O The group number of this process belongs to (1..nGroups)
  ! 
  !  \note If this function is called by the COSTA master process or in a sequential run it will return 0 for iGroup
  ! 
  !
  interface CTA_F90_Par_GetGroupInfo
    subroutine CTA_Par_GetGroupInfo( iGroup, itime )
      integer                       , intent(out  )     ::  iGroup
      integer                       , intent(out  )     ::  itime
    end subroutine CTA_Par_GetGroupInfo
  end interface


end module cta_f90_par

