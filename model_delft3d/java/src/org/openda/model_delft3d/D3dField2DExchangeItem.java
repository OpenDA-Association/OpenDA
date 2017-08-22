/* OpenDA v2.4.1 
* Copyright (c) 2017 OpenDA Association 
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

package org.openda.model_delft3d;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange Item representing a 2D-field in a D3D file
 */
public class D3dField2DExchangeItem implements IPrevExchangeItem {

    private String id;
    private D3dField2D field2D;
    private boolean dataChanged;

    public D3dField2DExchangeItem(String id, D3dField2D field2D) {
        this.id = id;
        this.field2D = field2D;
        this.dataChanged = false;
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;  // no description
    }

    public Class getValueType() {
        return D3dField2D.class;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return field2D;
    }

    public double[] getValuesAsDoubles() {
        return field2D.getValues();
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        double[] values = getValuesAsDoubles();
        for (int i = 0; i < values.length; i++) {
            values[i] += alpha * axpyValues[i];
        }
        setValuesAsDoubles(values);
    }

	public void multiplyValues(double[] multiplicationFactors) {
		double[] values = getValuesAsDoubles();
		for (int i = 0; i < values.length; i++) {
			values[i] *= multiplicationFactors[i];
		}
		setValuesAsDoubles(values);
	}

	public void setValues(Object values) {
        if (!(values instanceof D3dField2D)) {
            throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
        field2D = (D3dField2D) values;
        dataChanged = true;
    }

    public void setValuesAsDoubles(double[] values) {
        field2D.setValues(values);
        dataChanged = true;
    }

    public double[] getTimes() {
        return null;
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }

    public boolean getDataChanged() {
        return dataChanged;
    }
}
