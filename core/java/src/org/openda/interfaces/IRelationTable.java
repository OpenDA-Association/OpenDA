/* MOD_V2.0
* Copyright (c) 2012 OpenDA Association
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

package org.openda.interfaces;

/**
 * Relation table component that defines a relation between two
 * (ordered) sets of elements. Examples are the elements of a vector,
 * matrix, tree vector or in a stochastic observer.
 * <p>
 * The Relation table can be used for copying elements from one set to the
 * other optionally using interpolation (not yet supported).
 * </p>
 */
public interface IRelationTable extends Cloneable {

   /** 
    * Free a relation table object.
    */
    void free();

   /** 
    * Copy elements according to the relation table
    *
    * @param hfrom      Origin vector to copy data from
    * @param hto        Target vector to copy data to
    *
    */
   void apply(IVector hfrom, IVector hto);

   /** Copy elements according to the inverse of the relation table
    *
    * @param hfrom      Origin object to copy data from
    * @param hto        Target object to copy data to
    *
    */
   void applyInv(IVector hfrom, IVector hto);

 
    
     
}
