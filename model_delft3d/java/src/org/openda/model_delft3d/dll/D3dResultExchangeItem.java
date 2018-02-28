/* OpenDA v2.4.3 
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
package org.openda.model_delft3d.dll;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;

/**
 * Exchange Item representing a monitor station
 */
public class D3dResultExchangeItem implements IPrevExchangeItem{

    private String id;
    private int exchangeItemHandle;
    private Role role;
    private int myModelInstance;
	private File modelDir;

	public D3dResultExchangeItem(File modelDir, String id, int exchangeItemHandle, Role role, int myModelInstance) {
		this.modelDir = modelDir;
		this.id = id;
        this.exchangeItemHandle = exchangeItemHandle;
        this.role = role;
        this.myModelInstance = myModelInstance;
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;  // no description
    }

    public Class getValueType() {
        return double[].class;
    }

    public Role getRole() {
        return role;
    }

    public void setValues(Object values) {
		throw new RuntimeException(this.getClass().getName() +
				"setValues(): neither allowed nor useful for output item");
    }

    public void setValuesAsDoubles(double[] values) {
		throw new RuntimeException(this.getClass().getName() +
				"setValuesAsDoubles(): neither allowed nor useful for output item");
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
            throw new RuntimeException(this.getClass().getName() +
                    "axpyOnValues(): neither allowed nor useful for output item");
    }

	public void multiplyValues(double[] multiplicationFactors) {
		throw new RuntimeException(this.getClass().getName() +
				"multiplyValues(): neither allowed nor useful for output item");
	}

	public Object getValues() {
        throw new RuntimeException(this.getClass().getName() + "getValues() not implemented for result item");
    }

    public double[] getValuesAsDoubles() {
        D3dFlowDll.selectInstance(modelDir, myModelInstance);
        double value = D3dFlowDll.getResultValue(exchangeItemHandle);
        return new double[]{value};
    }

    public double[] getTimes() {
        return new double[]{D3dFlowDll.getCurrentTime()}; // TODO: return actual time series
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }
}
