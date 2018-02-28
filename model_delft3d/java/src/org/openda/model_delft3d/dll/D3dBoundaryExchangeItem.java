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
 * Exchange Item representing a D3D astro component or correction
 */
public class D3dBoundaryExchangeItem implements IPrevExchangeItem{

    private String id;
    private int exchangeItemHandle;
    private Role role;
    private int myModelInstance;
	private int type;
	private File modelDir;

	public D3dBoundaryExchangeItem(File modelDir, String id, int type, int exchangeItemHandle, Role role, int myModelInstance) {
		this.modelDir = modelDir;
		this.id = id;
		this.type = type;
        this.exchangeItemHandle = exchangeItemHandle;
        this.role = role;
        this.myModelInstance = myModelInstance;
    }

    public String getId() {
        return id;
    }

	public int getType(){
		return type;
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

    public Object getValues() {
        throw new RuntimeException(this.getClass().getName() + ": getValues not allowed for input item");
    }

    public double[] getValuesAsDoubles() {
        throw new RuntimeException(this.getClass().getName() + ": getValuesAsDoubles not allowed for input item");
    }

	public void axpyOnValues(double alpha, double[] axpyValues) {

		if (this.getType() == D3dFlowExchangeItemConfig.EI_wind_gu ||
		    this.getType() == D3dFlowExchangeItemConfig.EI_wind_gv) {
			// grid noise, so we expect a whole grid (as double[]) of axpyValues
			// a crude way for the model to give the metadata (location) is to provide
			// all these locations as well. For the moment, we will implement it this way:
			// the double array can be subdivided into triples of (xLoc, yLoc, value).
			// Note that this function (that is, for axpyValues.length > 1)
			//  will not be used in bbStochModelInstance.addNoiseToExchangeItem,
			// but in a noise  grid model to be provided by Martin.

			D3dFlowDll.selectInstance(modelDir, myModelInstance);
			// Note: the operation in this case is set instead of add, because the noise must
			// be provided to D3DFlow. D3dFlow takes care of: first the interpolation towards the large
			// meteo grid (using its own interpolation tools), then to the addition.

			// double[] values = axpyValues.clone();
			// TEMPORARY hack: (19 september): to test, create a small grid and pass values (now scalar) as array
			double[] values = new double[12];
			for (int i=0;i<4;i++){
				values[3*i+2] = axpyValues[0];
			}
			values[0 ] = -1.0E5;
			values[1 ] = -1.0E5;
			values[3 ] = 101.0E3;
			values[4 ] = -1.0E5;
			values[6 ] = -1.0E5;
			values[7 ] =  101.0E3;
			values[9 ] = 101.0E3;
			values[10] = 101.0E3;
			// END temporary hack

			D3dFlowDll.setBoundaryGridNoise(exchangeItemHandle, alpha,values, D3dFlowExchangeItemConfig.OPER_set);
		} else{
			if (axpyValues.length == 1)   {

				D3dFlowDll.selectInstance(modelDir, myModelInstance);
				D3dFlowDll.setBoundaryNoise(exchangeItemHandle, alpha*axpyValues[0], D3dFlowExchangeItemConfig.OPER_add);
			} else {
				throw new RuntimeException(this.getClass().getName() +
						"axpyOnValues(): can only set 1 value a time (got " + axpyValues.length + ") values");
			}

		}
	}

	public void multiplyValues(double[] multiplicationFactors) {
		if (multiplicationFactors.length != 1) {
			throw new RuntimeException(this.getClass().getName() +
					"axpyOnValues(): can only set 1 value a time (got " + multiplicationFactors.length + ") values");
		}
		D3dFlowDll.selectInstance(modelDir, myModelInstance);
		D3dFlowDll.setBoundaryNoise(exchangeItemHandle, multiplicationFactors[0], D3dFlowExchangeItemConfig.OPER_multiply);
	}

	public void setValues(Object values) {
        throw new RuntimeException(this.getClass().getName() + "setValues() not implemented for boundary item");
    }

    public void setValuesAsDoubles(double[] values) {
        throw new RuntimeException(this.getClass().getName() + "setValuesAsDoubles() not implemented for boundary item");
    }

    public double[] getTimes() {
		D3dFlowDll.selectInstance(modelDir, myModelInstance);
		return new double[]{D3dFlowDll.getCurrentTime()}; //TODO: return actual time series
   //     return new double[]{D3dFlowDll.getTimeHorizon().getBeginTime().getMJD()}; // TODO: return actual time series
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }
}

