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

package org.openda.model_swan;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange item for reading and writing SWAN main input file.
 */
public class SwanInputFileExchangeItem implements IPrevExchangeItem {
    private SwanInputFile swanInputFile;
    private String id;

    public SwanInputFileExchangeItem(String id, SwanInputFile swanInputFile) {
        this.id = id;
        this.swanInputFile = swanInputFile;
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null;
    }

    public Class getValueType() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.getValueType(): Not implemented yet.");
    }

    public Role getRole() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.getRole(): Not implemented yet.");
    }

    public Object getValues() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.getValues(): Not implemented yet.");
    }

    public double[] getValuesAsDoubles() {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.getValuesAsDoubles(): Not implemented yet.");
    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.axpyOnValues(): Not implemented yet.");
    }

    public void multiplyValues(double[] multiplicationFactors) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.multiplyValues(): Not implemented yet.");
    }

    public void setValues(Object values) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.setValues(): Not implemented yet.");
    }

    public void setValuesAsDoubles(double[] values) {
        throw new UnsupportedOperationException("org.openda.model_swan.SwanInputFileExchangeItem.setValuesAsDoubles(): Not implemented yet.");
    }

    public double[] getTimes() {
        double[] times = new double[2];
        times[0] = swanInputFile.getDblTStartSimulation();
        times[1] = swanInputFile.getDblTStopSimulation();
        return times;
    }

    public void setTimes(double[] times) {
        swanInputFile.setDblTStartSimulation(times[0]);
        swanInputFile.setDblTStopSimulation(times[1]);
    }
}
