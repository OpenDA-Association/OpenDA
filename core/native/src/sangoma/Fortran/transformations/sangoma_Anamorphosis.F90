! Copyright (c) 2015 Jean-Michel Brankart jean-michel.brankart@legi.grenoble-inp.fr
!
! This routine is free software: you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License
! as published by the Free Software Foundation, either version
! 3 of the License, or (at your option) any later version.
!
! It is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this software.  If not, see <http://www.gnu.org/licenses/>.
!
!$Id: sangoma_Anamorphosis.F90 553 2015-03-03 16:23:51Z larsnerger $
!BOP
!
! !ROUTINE: sangoma_anamorphosis --- Perform anamorphosis transformation
!
! !INTERFACE:
SUBROUTINE sangoma_anamorphosis(dim, dim_qua, dir, &
     qua_ref, qua, vct, status) &
     BIND(C, name="sangoma_anamorphosis_")

! !DESCRIPTION:
! This routine performs anamorphosis transformation of a state or observation vector
! using a set of quantiles of the input ensemble.
! This is an implementation of the specific algorithm described in
!    Brankart et al. (2012). Ocean Science, 8, 121-142. 
!
! Inputs are the vector to transform, the quantiles of the ensemble, and
!        the corresponding quantiles of the target distribution (assumed all distinct)
!
! !REVISION HISTORY:
! 2015-02 - J.-M. Brankart - Initial SANGOMA code
!
! USES:
  USE, INTRINSIC :: ISO_C_BINDING
  USE sangoma_base, ONLY: REALPREC, INTPREC

  IMPLICIT NONE

! ARGUMENTS:
  INTEGER(INTPREC), INTENT(in)  :: dim               ! Vector (state or obs) dimension
  INTEGER(INTPREC), INTENT(in)  :: dim_qua           ! Number of quantiles
  INTEGER(INTPREC), INTENT(in)  :: dir               ! Forward (+) or backward (-) transformation
  REAL(REALPREC), INTENT(in)    :: qua_ref(dim_qua)  ! Quantiles of the target distribution
  REAL(REALPREC), INTENT(in)    :: qua(dim, dim_qua) ! Ensemble quantiles
  REAL(REALPREC), INTENT(inout) :: vct(dim)          ! Vector to transform
  INTEGER(INTPREC), INTENT(out) :: status            ! Status flag (0=success)

! External routines for localizing value among the quantiles
  INTERFACE

     INTEGER FUNCTION locqua_forward(v,dim_qua,qua,eql)
       USE sangoma_base, ONLY: REALPREC, INTPREC
       REAL(REALPREC), INTENT(in) :: v                  ! value to localize
       INTEGER(INTPREC), INTENT(in)  :: dim_qua         ! Number of quantiles
       REAL(REALPREC), DIMENSION(:), INTENT(in) :: qua  ! list of quantiles
       INTEGER, INTENT(out) :: eql
     END FUNCTION locqua_forward

     INTEGER FUNCTION locqua_backward(v,dim_qua,qua)
       USE sangoma_base, ONLY: REALPREC, INTPREC
       REAL(REALPREC), INTENT(in) :: v                  ! value to localize
       INTEGER(INTPREC), INTENT(in)  :: dim_qua         ! Number of quantiles
       REAL(REALPREC), DIMENSION(:), INTENT(in) :: qua  ! list of quantiles
     END FUNCTION locqua_backward

  END INTERFACE

! *** local variables ***
  INTEGER(INTPREC) :: i, jj    ! Counters
  INTEGER(INTPREC) :: eql      ! Check for equal quantiles
  REAL(REALPREC) :: a          ! Interpolation weight

  IF (dir.GT.0) THEN

! ************************************
! *** Perform forward anamorphosis ***
! ************************************

     ! Loop on the variables (state or observation)
     DO i=1, dim

       ! Localize input value in the quantiles of the ensemble
       jj=locqua_forward(vct(i),dim_qua,qua(i,1:dim_qua),eql)

       ! Set the interpolation weight
       IF (jj.EQ.0) THEN
          jj=1         ; a=0.0    ! below the 1st quantile -> 1st quantile
       ELSEIF (jj.EQ.dim_qua) THEN
          jj=dim_qua-1 ; a=1.0    ! above the last quantile -> last quantile
       ELSEIF (eql.EQ.0) THEN
          a=0.0                   ! equal quantiles (with even rank difference)
       ELSEIF (eql.EQ.1) THEN
          a=0.5                   ! equal quantiles (with odd rank difference)
       ELSE
          a=(vct(i)-qua(i,jj))/(qua(i,jj+1)-qua(i,jj))
       ENDIF

       ! Interpolate between quantiles of the target distribution
       vct(i)=qua_ref(jj)+a*(qua_ref(jj+1)-qua_ref(jj))

     ENDDO
     
     status = 0

  ELSEIF (dir.LT.0) THEN

! *************************************
! *** Perform backward anamorphosis ***
! *************************************

     ! Loop on the variables (state or observation)
     DO i=1, dim

       ! Localize input value in the quantiles of the target distribution
       jj=locqua_backward(vct(i),dim_qua,qua_ref(1:dim_qua))

       ! Set the interpolation weight
       IF (jj.LE.0) THEN
          jj=1         ; a=0.0  ! below the 1st quantile -> 1st quantile
       ELSEIF (jj.GE.dim_qua) THEN
          jj=dim_qua-1 ; a=1.0  ! above the last quantile -> last quantile
       ELSE
          a=(vct(i)-qua_ref(jj))/(qua_ref(jj+1)-qua_ref(jj))
       ENDIF

       ! Interpolate between quantiles of the ensemble
       vct(i)=qua(i,jj)+a*(qua(i,jj+1)-qua(i,jj))

     ENDDO

     status = 0

  ELSE

     ! Sense of transformation not specified
     status = 1

  ENDIF

END SUBROUTINE sangoma_anamorphosis


INTEGER FUNCTION locqua_forward(v,dim_qua,qua,eql)
! !DESCRIPTION:
! Localize value in the sequence of quantiles
! Method: bisection in the sequence of quantiles
!         (assuming that some of them might be equal)
! Inputs: v   : value to localize
!         dim_qua : number of quantiles
!         qua : sequence of quantiles
! Output: function: first quantile if v   <  min
!                   last quantile if max <= v
!                   median quantile index such that inf <= v <= sup
!         eql : -1 if inf an sup quantiles are diffrent
!                0 for equal quantiles (with even rank difference)
!                1 for equal quantiles (with odd rank difference)
!
! !REVISION HISTORY:
! 2015-02 - J.-M. Brankart - Initial SANGOMA code
!
! USES:
  USE sangoma_base, ONLY: REALPREC, INTPREC
  IMPLICIT NONE
!
  REAL(REALPREC), INTENT(in) :: v                  ! value to localize
  INTEGER(INTPREC), INTENT(in)  :: dim_qua         ! Number of quantiles
  REAL(REALPREC), DIMENSION(:), INTENT(in) :: qua  ! list of quantiles
  INTEGER, INTENT(out) :: eql

! *** local variables ***
  INTEGER :: jp,jp0,jp1,jpsup,jpinf

  jp0=0 ; jp1=dim_qua+1 ; jp=(jp0+jp1)/2
  DO WHILE (jp0.NE.jp)
     IF (v.GE.qua(jp)) THEN
        jp0=jp
     ELSE
        jp1=jp
     ENDIF
     jp=(jp0+jp1)/2
  ENDDO
  jpsup=jp

  jp0=0 ; jp1=dim_qua+1 ; jp=(jp0+jp1)/2
  DO WHILE (jp0.NE.jp)
     IF (v.GT.qua(jp)) THEN
        jp0=jp
     ELSE
        jp1=jp
     ENDIF
     jp=(jp0+jp1)/2
  ENDDO
  jpinf=jp

  eql=-1
  IF (jpinf.NE.jpsup) THEN
     jpsup=MAX(jpsup,1) ; jpinf=MAX(jpinf,1)
     jp=(jpinf+jpsup)/2
     IF (jp.LT.dim_qua) THEN
     IF (qua(jp).GE.qua(jp+1)) THEN
        eql=MOD(jpsup-jpinf,2)
     ENDIF
     ENDIF
  ELSE
     jp=jpsup
  ENDIF

  locqua_forward=jp

END FUNCTION locqua_forward


INTEGER FUNCTION locqua_backward(v,dim_qua,qua)
! !DESCRIPTION:
! Localize value in the sequence of quantiles
! Method: bisection in the sequence of quantiles
!         (assuming that they are all different)
! Inputs: v   : value to localize
!         dim_qua : number of quantiles
!         qua : sequence of quantiles
! Output: first quantile if v   <  min
!         last quantile if max <= v
!         median quantile index such that inf <= v <= sup
!
! !REVISION HISTORY:
! 2015-02 - J.-M. Brankart - Initial SANGOMA code
!
! USES:
  USE sangoma_base, ONLY: REALPREC, INTPREC
  IMPLICIT NONE
!
  REAL(REALPREC), INTENT(in) :: v                  ! value to localize
  INTEGER(INTPREC), INTENT(in)  :: dim_qua         ! Number of quantiles
  REAL(REALPREC), DIMENSION(:), INTENT(in) :: qua  ! list of quantiles

! *** local variables ***
  INTEGER :: jp,jp0,jp1

  jp0=0 ; jp1=dim_qua+1 ; jp=(jp0+jp1)/2
  DO WHILE (jp0.NE.jp)
     IF (v.GE.qua(jp)) THEN
        jp0=jp
     ELSE
        jp1=jp
     ENDIF
     jp=(jp0+jp1)/2
  ENDDO

  locqua_backward=jp

END FUNCTION locqua_backward

