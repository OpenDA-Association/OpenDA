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
package org.openda.utils;
import org.openda.interfaces.ISelector;
import org.openda.interfaces.IVector;

import java.util.ArrayList;
import java.util.List;

/**
 * Observation selector based on indices of the observed data.
 */
public class IndicesSelector implements ISelector {
    private List<Integer> selectedIndices = null;

    public IVector apply(IVector allValues) {
        IVector selectedValues = null;
        if (selectedIndices != null) {
            selectedValues = new Vector(selectedIndices.size());
            for (int i=0; i<selectedIndices.size(); i++){
                selectedValues.setValue(i,allValues.getValue(selectedIndices.get(i)));
            }
        } else {
            throw new RuntimeException("IndicesSelector is not yet initialized.");
        }
        return selectedValues;
    }

    public void addIndex(int i) {
        if (this.selectedIndices==null){
            this.selectedIndices = new ArrayList<Integer>();
        }
        int j=this.selectedIndices.size();
        assert this.selectedIndices != null;
        this.selectedIndices.add(j,i);
    }

    public void addIndexRange(int indFirst, int indLast) {
        if (this.selectedIndices==null){
            this.selectedIndices = new ArrayList<Integer>();
        }
        int j=this.selectedIndices.size();

        for (int i=indFirst; i<indLast; i++){
            assert this.selectedIndices != null;
            this.selectedIndices.add(j,i);
            j++;
        }
    }
}
