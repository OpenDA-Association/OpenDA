! Copyright (c) 2015 M. Umer Altaf 
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!
!BOP
!
! !ROUTINE: sangoma_ComputeInvStats --- Compute statistics on Innovation
!
! !INTERFACE:
SUBROUTINE sangoma_computeinvstats(dim1,dim2,innovation_f,rms_f,bias_f,min_f,max_f,Pvalue,status) &
  BIND(C, name="sangoma_computeinvstats_")

! !DESCRIPTION:
! Calculates BIAS, RMS, Min/Max Errors of innovation (y-Hx). assume constant
! observation operators H and m.
!
! OUTPUT:       Vectors returning RMS, BIAS, min / max errors.
!
!   
!
!
! !USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! !ARGUMENTS:
  INTEGER(INTPREC), INTENT(in) :: dim1          ! size of dim1
  INTEGER(INTPREC), INTENT(in) :: dim2         !size of dim2
  REAL(REALPREC), INTENT(in)   :: innovation_f(dim1,dim2)  ! 2D innovation array
  REAL(REALPREC), INTENT(out)   :: rms_f(dim1)  ! RMS
  REAL(REALPREC), INTENT(out)   :: bias_f(dim1) ! BIAS
  REAL(REALPREC), INTENT(out)   :: min_f(dim1)  ! Min errors
  REAL(REALPREC), INTENT(out)   :: max_f(dim1)  ! Max Errors
  REAL(REALPREC), INTENT(out) :: Pvalue        ! Statistics for space or time
  INTEGER(INTPREC), INTENT(out) :: status                  ! Status flag (0=success)
!EOP


! *** local variables ***
  INTEGER :: i,j            ! Counters
  REAL :: srms,sbias, meansquare, sumsquare        ! local variables for rms and bias
  REAL :: minmax(dim1)         ! Temporary array
  REAL :: std_bais
  REAL :: t, bais_t,q,abc,p

! *************************
! *** Compute Biases    ***
! *************************

!***********************************************  
!--------Compute Biases, RMS, MIN / MAX -------*
!***********************************************
minmax = 0.0
do i= 1, dim1
                srms = 0
                sbias = 0
                do j = 1,dim2
                        sbias = sbias + innovation_f(i,j)
                        srms = srms + innovation_f(i,j)*innovation_f(i,j)
                        minmax(j) = innovation_f(i,j)
                enddo
                bias_f(i) = sbias / REAL(dim2)
                rms_f(i) = sqrt(srms/REAL(dim2))
                min_f(i) = minval(minmax(:)) 
                max_f(i) = maxval(minmax(:)) 
enddo

!****************************************
!------- Compute T Values -------------*
!****************************************

bais_t = 0.0
bais_t = sum(bias_f(:)) / REAL(dim1)
std_bais = 0.0
meansquare = 0.0
sumsquare = 0.0
do i = 1, dim1
            
         do j = 1,dim2
            meansquare = (innovation_f(i,j) - bais_t)*(innovation_f(i,j) - bais_t)
            sumsquare = sumsquare + meansquare
         enddo
enddo

            std_bais = sqrt(sumsquare / REAL(dim1*dim2))
            t = bais_t/(std_bais/sqrt(REAL(dim1*dim2)))


!**********************************
!       Compute p value
!**********************************

 call cdft(1,Pvalue,q,t,REAL(dim1*dim2-1),status,abc)




 status = 0


END SUBROUTINE sangoma_computeinvstats
