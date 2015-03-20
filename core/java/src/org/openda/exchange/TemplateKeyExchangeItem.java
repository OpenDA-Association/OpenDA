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
package org.openda.exchange;
import org.openda.blackbox.interfaces.IKeyDateType;
import org.openda.blackbox.interfaces.IKeyType;

/**
 * ExchangeItem to be used in combination with KeyType elements used by TemplateConfig files
 */
public class TemplateKeyExchangeItem extends ExchangeItem implements IKeyType, IKeyDateType {
    private IKeyType keyType;
    private String value;

    public TemplateKeyExchangeItem(String id, IKeyType keyType) {
        super(id);
        this.keyType = keyType;
    }

    public Class getValueType() {
        return String.class;
    }

    public ValueType getValuesType() {
        return ValueType.StringType;
    }

    public Object getValues() {
        return value;

    }

    public double[] getValuesAsDoubles() {
        throw new UnsupportedOperationException("org.openda.exchange.TemplateKeyExchangeItem.getValuesAsDoubles(): Not implemented yet.");

    }

    public void axpyOnValues(double alpha, double[] axpyValues) {
        throw new UnsupportedOperationException("org.openda.exchange.TemplateKeyExchangeItem.axpyOnValues(): Not implemented yet.");

    }

    public void multiplyValues(double[] multiplicationFactors) {
        throw new UnsupportedOperationException("org.openda.exchange.TemplateKeyExchangeItem.multiplyValues(): Not implemented yet.");

    }

    public void setValues(Object values) {
        if (!(values instanceof String)) {
            throw new RuntimeException("TemplateKeyExchangeItem.setValues(" + values.toString() +
                    "): values must be of type String, but is " + values.getClass().getName());
        }
        this.value = (String) values;

    }

    public void setValuesAsDoubles(double[] values) {
        throw new UnsupportedOperationException("org.openda.exchange.TemplateKeyExchangeItem.setValuesAsDoubles(): Not implemented yet.");

    }

    public String getValueAsString(String value) {
        return keyType.getValueAsString(value);
    }

    public IKeyType getKeyType() {
        return keyType;

    }

    @Override
    public void setStartDate(double date) {
        if (keyType instanceof IKeyDateType) {
            ((IKeyDateType)keyType).setStartDate(date);
        } else {
            throw new RuntimeException("TemplateKeyExchangeItem.setStartDate(" + date + "): keyType must by of type IKeyDateType, but is " + keyType.getClass().getName() );
        }

    }

    @Override
    public void setEndDate(double date) {
        if (keyType instanceof IKeyDateType) {
            ((IKeyDateType)keyType).setEndDate(date);
        } else {
            throw new RuntimeException("TemplateKeyExchangeItem.setEndDate(" + date + "): keyType must by of type IKeyDateType, but is " + keyType.getClass().getName() );
        }

    }

    @Override
    public String calculateValue() {
        if (keyType instanceof IKeyDateType) {
            return ((IKeyDateType)keyType).calculateValue();
        } else {
            throw new RuntimeException("TemplateKeyExchangeItem.calculateValue(): keyType must by of type IKeyDateType, but is " + keyType.getClass().getName() );
        }
    }
}
