! Copyright (c) 2012 Lars Nerger, lars.nerger@awi.de 
! 
! This routine is free software: you can redistribute it and/or modify 
! it under the terms of the GNU Lesser General Public License 
! as published by the Free Software Foundation, either version 
! 3 of the License, or (at your option) any later version. 
! 
! This code is distributed in the hope that it will be useful, 
! but WITHOUT ANY WARRANTY; without even the implied warranty of 
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
! GNU Lesser General Public License for more details. 
! 
! You should have received a copy of the GNU Lesser General Public 
! License along with this software.  If not, see <http://www.gnu.org/licenses/>. 
! 
!$Id: sangoma_ComputeEnsStats.F90 605 2015-04-29 07:17:11Z larsnerger $ 
!BOP 
! 
! !ROUTINE: sangoma_ComputeEnsStats --- Compute ensemble statistics 
! 
! !INTERFACE: 
SUBROUTINE sangoma_MinSpanTree(typeOut,dimA, A,distMST,tree) & 
     BIND(C, name="sangoma_minspantree_") 
 
! !DESCRIPTION: 
!
! This a routing that can build and return the minimum spanning 
! tree and/or the total distance between all nodes of the final 
! minimal spanning tree. 
! 
! 
! Prim's minimal spanning tree algorithm works as follows: 
!  * start at any node, find closest neighbor and mark edges
!  * for all remaining nodes, find the closest node to 
!    previous cluster and mark edge
!  * continue until no nodes remain!
!
! INPUTS: 
!               A - symmetric matrix of distances between 
!                   nodes (adjacency matrix) 
!               m - type of output required:
!                       * m = 1 will output only distMST
!                       * m = 2 will output only tree
!                       * m = 3 will output distMST and tree.
!
! OUTPUTS: 
!               MST - Euclidian distance between all points in 
!                     the minimum spanning tree
!               tree - matrix specifying minimum spanning tree
!       
! Date:     27/04/2015
! Author:   Sanita Vetra-Carvalho (Univeristy of Reading)
!---------------------------------------------------------------%

! !USES: 
  USE, INTRINSIC :: ISO_C_BINDING 
  USE sangoma_base, ONLY: REALPREC, INTPREC 
 
  IMPLICIT NONE 
 
! !ARGUMENTS: 
  INTEGER(INTPREC), INTENT(in)  :: typeOut           ! 
  INTEGER(INTPREC), INTENT(in)  :: dimA              ! Matrix with distances 
  REAL(REALPREC), INTENT(in)    :: A(dimA,dimA)      ! State vector 
  REAL(REALPREC), INTENT(out)    :: distMST          ! Sum of distances 
  INTEGER(INTPREC), INTENT(out) :: tree(dimA,dimA)    ! Matrix specifying the span. 
!EOP 
 
! *** local variables ***
  INTEGER :: conn_nodes(dimA)   ! nodes part of the min-span-tree
  INTEGER :: notconn_nodes(dimA)
  INTEGER :: min_i, min_j 
  INTEGER :: i,j
  REAL :: minlink
  INTEGER :: cnt
! -------------------------------------------------

IF ( typeOut == 1) THEN
      distMST = 0
ELSEIF( typeOut == 2) THEN
      tree = 0
ELSEIF (typeout == 3) THEN
      distMST = 0
      tree = 0
ELSE
      write(*,*) 'incorrect input in min_spanning_tree(A,m) funtion.'
      write(*,*) 'type of output, m, has to be either 1,2 or 3, but '
      write(*,*) 'm = ',typeOut
      return; 
ENDIF

! Initilize nodes and counter
conn_nodes = 0
conn_nodes(1) = 1
notconn_nodes(1) = 0
do i = 2,dimA
   notconn_nodes(i) = i
enddo 
cnt = 1

do while ( sum(notconn_nodes)>0 ) 

cnt = cnt + 1
!  find the absolute minimum in submatrix A(conn_nodes,notconn_nodes)
!  and store the indices in the A matrix
  minlink = huge(1.0)
  min_i = 0
  min_j= 0
  do i = 1,dimA
     if (conn_nodes(i) > 0 ) THEN
        do j = 1,dimA
           IF (notconn_nodes(j) > 0) THEN
              ! check for minimum distance
              IF ( A(i,j) < minlink) THEN
                 minlink = A(i,j)
                 min_i = i
                 min_j = j
              ENDIF   
           ENDIF
       enddo
     endif
  enddo
  ! Compute tree and/or total distance 
  if (typeOut == 1) THEN
       distMST = distMST + A(min_i,min_j)**2
  elseif (typeOut ==2) THEN
      tree(min_i,min_j)=1
     tree(min_j,min_i)=1
  elseif (typeOut == 3) THEN
     distMST = distMST + A(min_i,min_j)**2
     tree(min_i,min_j)=1
     tree(min_j,min_i)=1
  endif

  ! put added node into connected set and remove it from remaining set of nodes
  conn_nodes(cnt) = min_j
  notconn_nodes(min_j) = 0
 
end do

if (typeOut == 1 .or. typeOut == 3) THEN
        distMST = sqrt(distMST) ! square the distance to get Euclidian norm
endif




END SUBROUTINE sangoma_minSpanTree
