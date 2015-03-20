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
 * Dimension description for each of the n dimensions of a n-D field in a TreeVector.
 */
public interface IDimensionIndex {
    /**
     *
     * Get first index of this dimension.
     * @return first index in this dimension
     */
    int getStart();

    /**
     *
     * Get last index of this dimension.
     * @return last index in this dimension
     */
    int getEnd();

    /**
     * Get the size of this dimension.
     * @return size of this dimension
     */
    int getSize();
}
