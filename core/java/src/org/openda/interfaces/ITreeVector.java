/*
 * Copyright (c) 2019 OpenDA Association
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

import java.io.Serializable;
import java.util.ArrayList;

/**
 * Interface to a TreeVector: a treevector consists of one or more vectors
 * with the possibility to store metadata for each 'leave' of the treevector.
 */
public interface ITreeVector extends IVector, Cloneable, Serializable {

    /**
     * Get a TreeVector's identifier.
     * @return The TreeVector's identifier.
     */
    String getId();

    /**
     * Get a TreeVector's caption.
     * @return The TreeVector's caption (that is a descriptive name)
     */
    String getCaption();

    /**
     * Get a TreeVector's description.
     * @return The TreeVector's extensive description. 
     */
    String getDescription();

    /**
     * Get the identifiers of the children of the TreeVector.
     *
     * @return the identifiers of all sub-treevectors
     */
    ArrayList<String> getSubTreeVectorIds();

    /**
     * Get a child in the tree vector, the child is known to be a SubTreeVector
     * (otherwise a run time exception will be thrown).
     *
     * @param id identifier of the child, may contain "/"'s to specify full path in tree.
     * @return Child SubTreeVector with identifier <code>id</code>.
     */
    ITreeVector getSubTreeVector(String id);

    /**
     * Get the number of dimensions in the TreeVector.
     * @return Returns <code>null</code> if there are no dimensions defined, returns the array with dimensions
     *         otherwise. The length of the returned array is equal to the number of dimensions 
     */
    IDimensionIndex[] getDimensionIndices();

    /**
     * Check if part of the tree vector is to be excluded from the vector of it's parent.
     *
     * @return Returns <code>true</code> if this part should be excluded.
     */
    boolean excludeFromVector();

    /**
     * Clone (that is duplicate) a TreeVector.
     * <p/>
     * Note: Duplication means that a new vector is created that is identical to
     * the current vector. All data in the current vector is also copied.
     *
     * @return A deep copy of the present vector.
     */
    ITreeVector clone();

}
