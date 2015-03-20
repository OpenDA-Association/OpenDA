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

package org.openda.model_efdc;

import org.openda.exchange.ExchangeItem;

/**
 * Exchange item for a grid time series (EFDC output) file.
 *
 * @author Arno Kockx
 */
public class EfdcGridTimeSeriesExchangeItem extends ExchangeItem {

    /**
     * This variable stores all grid cell values for all times.
     * The first index is the timeIndex. The second index is the gridCellIndex.
     * The grid is 2D, but here a grid cell is referenced with its unique gridCellIndex,
     * not with its rowIndex and columnIndex.
     */
    private double data[][];

    public EfdcGridTimeSeriesExchangeItem(String id) {
        super(id);
    }

    @Override
    public Role getRole() {
        return Role.Output;
    }

    @Override
    @SuppressWarnings("rawtypes")
    public Class getValueType() {
        return double[][].class;
    }

    public ValueType getValuesType() {
        return ValueType.doubles2dType;
    }

    @Override
    public Object getValues() {
        return this.data;
    }

    @Override
    public double[] getValuesAsDoubles() {
        return null;
    }

    @Override
    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new RuntimeException("Method axpyOnValues not implemented for " + this.getClass().getName());
    }

    @Override
    public void multiplyValues(double[] multiplicationFactors) {
        throw new RuntimeException("Method multiplyValues not implemented for " + this.getClass().getName());
    }

    @Override
    public void setValues(Object values) {
        if (values instanceof double[][]) {
            this.data = (double[][]) values;
        } else {
            throw new RuntimeException(this.getClass().getName() + ": cannot digest the given type for method setValues.");
        }
    }

    @Override
    public void setValuesAsDoubles(double[] values) {
        //do nothing
    }
}
