/* OpenDA v2.4 
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
public class D3dStateExchangeItem implements IPrevExchangeItem{

    private String id;
    // private IVector ctaStateVector; TODO: use ctaStateVector
    private int myModelInstance;
	private File modelDir;

	public D3dStateExchangeItem(File modelDir, String id, int myModelInstance) {
		this.modelDir = modelDir;
		this.id = id;
        this.myModelInstance = myModelInstance;
        // TODO: create ctaStateVector, based on cta state handle in D3dFlow:
        // D3dFlowDll.selectInstance(myModelInstance);
        // this.D3dFlowDll.getCtaStateVectorHandle();
        // (FOR NOW: pass state as double array, see methods below)
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;  // no description
    }

    public Class getValueType() {

        // TODO: return type of ctaStateVector:
        // return IVector.class;

        // FOR NOW return double array;
        return double[].class;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {

        D3dFlowDll.selectInstance(modelDir, myModelInstance);

        // TODO: return ctaStateVector:
        // return ctaStateVector; // todo: copy of ctaStateVector?

        // FOR NOW:
        return getValuesAsDoubles();
    }

    public double[] getValuesAsDoubles() {

        D3dFlowDll.selectInstance(modelDir, myModelInstance);

        // TODO: return values of ctaStateVector:
        // return ctaStateVector.getValues();

        // FOR NOW: get values from Dd3Flow DLL;
        return D3dFlowDll.getStateValues();

    }

    public void axpyOnValues(double alpha, double[] axpyValues) {

        D3dFlowDll.selectInstance(modelDir, myModelInstance);
        // TODO: axpy on ctaStateVector:
        // CtaVector axpyValuesVector = new CtaVector(axpyValues.length);
        // axpyValuesVector.setValues(axpyValues);
        // ctaStateVector.axpy(alpha, axpyValuesVector);

        // FOR NOW: perform action in Dd3Flow DLL;
        D3dFlowDll.axpyOnState(alpha, axpyValues);
    }

	public void multiplyValues(double[] multiplicationFactors) {
		throw new UnsupportedOperationException("nl.deltares.openda.models.d3dflow.dll.D3dStateExchangeItem.multiplyValues(): Not implemented yet.");
	}

	public void setValues(Object values) {

        D3dFlowDll.selectInstance(modelDir, myModelInstance);

        // TODO: store values ctaStateVector:
        // if (!(values instanceof IVector)) {
        //     throw new RuntimeException(this.getClass().getName() + "setValues(): unexpected type: "
        //             + values.getClass().getName());
        // }
        // ctaStateVector = (IVector) values;

        // FOR NOW:
        if (!(values instanceof double[])) {
            throw new RuntimeException(this.getClass().getName() + "setValues(): unexpected type: "
                     + values.getClass().getName());
        }
        setValuesAsDoubles((double[]) values);
    }

    public void setValuesAsDoubles(double[] values) {
        // TODO: store values ctaStateVector:
        // ctaStateVector.setValues(values);

        // FOR NOW: pass values to Dd3Flow DLL;
        D3dFlowDll.setStateValues(values);
    }

    public double[] getTimes() {
        return null; // no times in state
    }

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }
}

