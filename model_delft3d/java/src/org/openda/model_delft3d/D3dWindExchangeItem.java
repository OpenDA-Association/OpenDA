/* MOD_V2.0
 * Copyright (c) 2016 OpenDA Association
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

import org.openda.exchange.TimeInfo;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.util.List;

/**
 * Exchange Item representing a 2D-field in a D3D file
 */
public class D3dWindExchangeItem implements IExchangeItem {

    private String id;
	private D3dGrid2D grid;
    private List<D3dValuesOnGrid2D> valuesOnGrid2D;
	private double[] times;
	private TimeInfo timeInfo;
	private List<String> textContent;
	private int endOfHeader;
    private boolean dataChanged;
	private int[] dims= {0,0,0};

    public D3dWindExchangeItem(String id, List<D3dValuesOnGrid2D> valuesOnGrid2D, List<String> content, int endOfHeader, double[] times) {
        this.id = id;
        this.valuesOnGrid2D = valuesOnGrid2D;
		this.times = times;
		this.timeInfo = new TimeInfo(times);
		this.dataChanged = false;
		this.textContent = content;
		this.endOfHeader = endOfHeader;
		this.dims[0] = valuesOnGrid2D.size();
		this.grid = valuesOnGrid2D.get(0).getGrid();
		this.dims[1] = grid.getMmax();
		this.dims[2] = grid.getNmax();
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;
    }

    public void copyValuesFromItem(IExchangeItem sourceItem) {

    }

    public ITimeInfo getTimeInfo() {
		return timeInfo;
    }

    public IQuantityInfo getQuantityInfo() {
        return null;
    }

    public IGeometryInfo getGeometryInfo() {
        return null;
    }

    public ValueType getValuesType() {
        return null;
    }

    public Class getValueType() {
        return List.class;
    }

    public Role getRole() {
        return Role.InOut;
    }

    public Object getValues() {
        return valuesOnGrid2D;
    }

	public int[] getDims() {
		return this.dims;
	}

    public double[] getValuesAsDoubles() {
		// first get size:
		int nLevels = valuesOnGrid2D.size();
		double[] allValues = new double[grid.getMmax() * grid.getNmax() * nLevels];
        int index = 0;
		for (D3dValuesOnGrid2D valuesForOneTimeStep : valuesOnGrid2D) {
			double[] timeValues = valuesForOneTimeStep.getValues();
			for (int i = 0; i < timeValues.length; i++) {
				allValues[i+index] = timeValues[i];
			}
			index = index + timeValues.length; // which should be nmax*mmax!
		}
		return allValues;
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
        if (!(values instanceof List)) {
            throw new RuntimeException("SetValues for " + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
		List incomingValues = (List) values;
        // The de-selection may have been done directly in the exchange item
		// In that case, no further action is needed
        if (incomingValues != valuesOnGrid2D) {
			valuesOnGrid2D.clear();
			for (int i = 0, timeStepCount = incomingValues.size(); i < timeStepCount; i++) {
				Object valuesForOneTimeStep = incomingValues.get(i);
				if (!(valuesForOneTimeStep instanceof D3dValuesOnGrid2D)) {
					throw new RuntimeException("SetValues for " + this.getId() + ": unexpected object type: " + valuesForOneTimeStep.getClass().getName());
				}
				valuesOnGrid2D.add((D3dValuesOnGrid2D)valuesForOneTimeStep);
			}
		}
        dataChanged = true;
    }

    public void setValuesAsDoubles(double[] values) {
		int index = 0;
	    int nmsize = this.dims[1]*this.dims[2];
		for (D3dValuesOnGrid2D valuesForOneTimeStep : valuesOnGrid2D) {
			double[] timeValues = new double[nmsize];
			for (int i = 0; i < nmsize; i++) {
				timeValues[i] = values[i + index];
			}
			valuesForOneTimeStep.setValues(timeValues);
			index = index + nmsize;
		}
        dataChanged = true;
    }

    public double[] getTimes() {
        return times;
    }

	public List<String> getTextContent() {
		return this.textContent;
	}

	public int getEndOfHeader(){
		return this.endOfHeader;
	}

    public void setTimes(double[] times) {
        throw new RuntimeException(this.getClass().getName() + "setTimes(): time stamps can not be set");
    }

    public boolean getDataChanged() {
        return dataChanged;
    }
}
