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

package org.openda.model_swan;

import org.openda.interfaces.*;
import org.openda.utils.Vector;

/**
 * Exchange item for reading and writing SWAN open boundary file, in TPAR format.
 */
public class SwanOpenBoundaryTPARFileExchangeItem implements IExchangeItem {
    private String id;
    private SwanOpenBoundaryTPARFile swanOBFile;
    private String[] ids = new String[] {"Hs","period","peakdir","dirspread"};

    public SwanOpenBoundaryTPARFileExchangeItem(String id, SwanOpenBoundaryTPARFile swanOBFile) {
        this.id = id;
        this.swanOBFile = swanOBFile;
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null; // no description
    }

    public Class getValueType() {
        return double[].class;
    }

    public ValueType getValuesType() {
        return ValueType.doublesType;
    }

    public Role getRole() {
        return IPrevExchangeItem.Role.InOut;
    }

    public Object getValues() {
        return getValuesAsDoubles();
    }

    public double[] getValuesAsDoubles() {
        if (id.contentEquals(ids[0])) {
            return swanOBFile.getHs();
        } else if (id.contentEquals(ids[1])) {
            return swanOBFile.getPeriod();
        } else if (id.contentEquals(ids[2])) {
            return swanOBFile.getPeakDirection();
        } else if (id.contentEquals(ids[3])) {
            return swanOBFile.getDirectSpread();
        } else {
          throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.getValuesAsDoubles(): ID is not correct.");
        }
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        if (id.contentEquals(ids[0])) {
            swanOBFile.axpyOnHs(alpha,axpyValues);
        } else if (id.contentEquals(ids[1])) {
            swanOBFile.axpyOnPeriod(alpha,axpyValues);
        } else if (id.contentEquals(ids[2])) {
            swanOBFile.axpyOnPeakDirection(alpha,axpyValues);
        } else if (id.contentEquals(ids[3])) {
            swanOBFile.axpyOnDirectSpread(alpha,axpyValues);
        } else {
          throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.axpyOnValues(): ID is not correct.");
        }
    }

    public void multiplyValues(double[] multiplicationFactors) {
        if (id.contentEquals(ids[0])) {
            swanOBFile.multiplyHs(multiplicationFactors);
        } else if (id.contentEquals(ids[1])) {
            swanOBFile.multiplyPeriod(multiplicationFactors);
        } else if (id.contentEquals(ids[2])) {
            swanOBFile.multiplyPeakDirection(multiplicationFactors);
        } else if (id.contentEquals(ids[3])) {
            swanOBFile.multiplyDirectSpread(multiplicationFactors);
        } else {
          throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.multiplyValues(): ID is not correct.");
        }
    }

    public void setValues(Object values) {
        if (!(values instanceof double[])) {
            throw new RuntimeException("SetValues for" + this.getId() + ": unexpected object type: " + values.getClass().getName());
        }
        setValuesAsDoubles((double[]) values);
    }

    public void setValuesAsDoubles(double[] values) {
        if (id.contentEquals(ids[0])) {
            swanOBFile.setHs(values);
        } else if (id.contentEquals(ids[1])) {
            swanOBFile.setPeriod(values);
        } else if (id.contentEquals(ids[2])) {
            swanOBFile.setPeakDirection(values);
        } else if (id.contentEquals(ids[3])) {
            swanOBFile.setDirectSpread(values);
        } else {
          throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.setValuesAsDoubles(): ID is not correct.");
        }
    }

    public double[] getTimes() {
        return swanOBFile.getTimeDbl();
    }

    public void setTimes(double[] times) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.setTimes(): Not implemented yet.");
    }

	
	public void copyValuesFromItem(IExchangeItem sourceItem) {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.copyValuesFromItem(): Not implemented yet.");
	}

	
	public ITimeInfo getTimeInfo() {
		return swanOBFile;
	}

	
	public IQuantityInfo getQuantityInfo() {
		throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.getQuantityInfo(): Not implemented yet.");
	}

	
	public IGeometryInfo getGeometryInfo() {
		return null;
	}
	
	public String toString(){
        if (id.contentEquals(ids[0])) {
            return (new Vector(swanOBFile.getHs())).toString();
        } else if (id.contentEquals(ids[1])) {
            return (new Vector(swanOBFile.getPeriod())).toString();
        } else if (id.contentEquals(ids[2])) {
            return (new Vector(swanOBFile.getPeakDirection())).toString();
        } else if (id.contentEquals(ids[3])) {
            return (new Vector(swanOBFile.getDirectSpread())).toString();
        } else {
          throw new UnsupportedOperationException("org.openda.model_swan.SwanOpenBoundaryTPARFileExchangeItem.setValuesAsDoubles(): ID is not correct.");
        }
	}
}
