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
package org.openda.examples.simplef90model;

import org.openda.interfaces.IPrevExchangeItem;

/**
 * Exchange item for a DLL based model
 */
public class SimpleExchangeItem implements IPrevExchangeItem{

    private String id;
    private int indexInDLL;
    private Class valueType;
    private SimpleModelInstance SimpleModelInstance;
    private IPrevExchangeItem.Role role;

    public SimpleExchangeItem(String id, int indexInDLL, Class valueType, SimpleModelInstance SimpleModelInstance) {

        this.id = id;
        this.indexInDLL = indexInDLL;
        this.valueType = valueType;
        this.SimpleModelInstance = SimpleModelInstance;

        if (indexInDLL == SimpleModelDLL.gravity ||
                indexInDLL == SimpleModelDLL.friction_on_grid ||
                indexInDLL == SimpleModelDLL.discharge_on_laterals ) {
           role = IPrevExchangeItem.Role.InOut;
        } else if (indexInDLL == SimpleModelDLL.waterlevel_on_grid) {
           role = IPrevExchangeItem.Role.Output;
        } else {
            throw new RuntimeException("Unknown role for exchange item: " + id);
        }
    }

    public String getId() {
        return id;
    }

    public String getDescription() {
        return null; // no description
    }

    public Class getValueType() {
        return valueType;
    }

    public Role getRole() {
        return role;
    }

    public Object getValues() {
        if (valueType == double[].class) {
            return SimpleModelInstance.getValuesForExchangeItem(indexInDLL);
        } else if (valueType == double.class) {
            return SimpleModelInstance.getValueForExchangeItem(indexInDLL);
        } else {
            throw new RuntimeException("Unknown objecet type in getValues: " + valueType.toString());
        }
    }

    public double[] getValuesAsDoubles() {
        return SimpleModelInstance.getValuesForExchangeItem(indexInDLL);
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

	public void setValues(Object o) {
        if (o instanceof double[]) {
            if (valueType != double[].class) {
                throw new RuntimeException("Incompatible object type in setValues: " +
                        o.getClass().getName() + " (expected: " + valueType.toString() + ";");
            }
            SimpleModelInstance.setValuesForExchangeItem(indexInDLL, (double[]) o);
        } else if (o instanceof Double) {
            if (valueType != double.class) {
                throw new RuntimeException("Incompatible object type in setValues: " +
                        o.getClass().getName() + " (expected: " + valueType.toString() + ";");
            }
            SimpleModelInstance.setValueForExchangeItem(indexInDLL, (Double) o);

        } else {
            throw new RuntimeException("Unknown objecet type in setValues: " + o.getClass().getName());
        }
    }

    public void setValuesAsDoubles(double[] doubles) {
    }

    public double[] getTimes() {
        return SimpleModelInstance.getTimes();
    }

    @Override
    public void setTimes(double[] times) {
        throw new RuntimeException("Method setTimes of " + this.getClass().getName() + " not implemented yet.");
    }
}
