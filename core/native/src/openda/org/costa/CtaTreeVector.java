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

package org.costa;


/*
 *
 * Interface description of the COSTA default CtaTreeVector component.
 *
 * The CtaTreeVector is an extension of a vector component. A treevector
 * either contains a single vector or is a concatenation of a number of
 * treevectors, called sub-treevectors in this context. The usage of
 * sub-treevectors makes is possible to concatenate models or extend models
 * as is done when a deterministic model is extended into a stochastic model.
 * The sub-treevector are also very useful inside the model source code where the
 * whole treevector is not represented by a single vector.  The default tree-vector
 * component uses COSTA vector-components for storing the values.
 *
 * Each (sub) treevector has a id. CtaTreeVector vectors having the same id are considered
 * to be the same, meaning they have the same buildup in subtreevectors, length and
 * datatypes.
 *
 * ----------------------------------------------------------------
 */

import org.openda.interfaces.TreeVector;
import org.openda.interfaces.DimensionIndex;

import java.util.ArrayList;

/**
 * Tree Vector
 */
public class CtaTreeVector extends CtaVector implements TreeVector {
	
	public CtaTreeVector(String id, String caption){
	   this.ctaHandle=this.ctaCreate();
//	   this.ctaTreeVectorSetID(id);
	}
	
	public CtaTreeVector() {
	}

	public TreeVector getSubTreeVector(String id) {
		
		int handle=ctaGetSubTreeVector(id);

		CtaTreeVector subtree=new CtaTreeVector();
		subtree.ctaHandle=handle;
		
		return subtree;
	}

    public DimensionIndex[] getDimensionIndices() {
        throw new UnsupportedOperationException("org.costa.CtaTreeVector.getDimensionIndices(): Not implemented yet.");
    }

    public boolean excludeFromVector() {
        return false; // Todo: implement exclusion functionality
    }

    public native String getId();

    //public native Vector getChildVector(String id);

	private native int ctaCreate();
    private native int ctaGetSubTreeVector(String id);

    public native String getCaption();

    public String getDescription() {
        return null;
    }

    public double[] getValues(int index) {
        return new double[0];  //To change body of implemented methods use File | Settings | File Templates.
    }

    @SuppressWarnings({"CloneDoesntCallSuperClone", "CloneDoesntDeclareCloneNotSupportedException"})
    @Override
    public native TreeVector clone();

    public ArrayList<String> getSubTreeVectorIds() {
        throw new UnsupportedOperationException("org.costa.CtaTreeVector.getSubTreeVectorIds(): Not implemented yet.");
    }
}

