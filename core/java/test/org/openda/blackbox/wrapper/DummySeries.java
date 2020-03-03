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


package org.openda.blackbox.wrapper;

import org.openda.exchange.AbstractDataObject;
import org.openda.interfaces.IExchangeItem;
import org.openda.interfaces.IGeometryInfo;
import org.openda.interfaces.IQuantityInfo;
import org.openda.interfaces.ITimeInfo;

import java.io.File;

/**
 * Dummy Series object for testing purposes
 */
public class DummySeries extends AbstractDataObject {

    @Override
    public void initialize(File workingDir, String[] arguments) {
        String[] exchangeItemIds = arguments[0].split(";");
        for (int i = 0; i < exchangeItemIds.length; i++) {
			String id = exchangeItemIds[i];
			exchangeItems.put(id, new DummyExchangeItem(id, (i + 1) * 1000));
        }
    }

	private static class DummyExchangeItem implements IExchangeItem {

        private String exchangeItemId;
        private Double value;

        DummyExchangeItem(String exchangeItemId, double value) {
            this.exchangeItemId = exchangeItemId;
            this.value = value;
        }

        public String getId() {
            return exchangeItemId;
        }

        public String getDescription() {
            return null;  // no description
        }

        public IQuantityInfo getQuantityInfo() { return null; }

        public IGeometryInfo getGeometryInfo() { return null; }

		@Override
		public ValueType getValuesType() {
			return ValueType.doublesType;
		}

		public Role getRole() { return Role.InOut; }

        public Object getValues() {
            return value;
        }

        public double[] getValuesAsDoubles() {
            return new double[]{value};
        }

        public void axpyOnValues(double alpha, double[] axpyValues) {
            if (axpyValues.length !=1){
                throw new UnsupportedOperationException("Length of axpy value array must be 1");
            }
            this.value += alpha * axpyValues[0];
        }

		public void multiplyValues(double[] multiplicationFactors) {
			if (multiplicationFactors.length !=1){
				throw new UnsupportedOperationException("Length of multiplication factors array must be 1");
			}
			this.value *= multiplicationFactors[0];
		}

        public void copyValuesFromItem(IExchangeItem sourceItem) {
            ValueType sourceType=sourceItem.getValuesType();
            if(sourceType != ValueType.doublesType){
                throw new RuntimeException("DummyAstroDataObject.DummyExchangeItem.setValues(): unknown type: " + sourceType );
            }
            this.setValues(sourceItem.getValues());
        }

		public void setValues(Object values) {
            if (!(values instanceof Double)) {
                throw new RuntimeException("DummySeries.DummyExchangeItem.setValues(): unknown object type: " + values.getClass() );
            }
            value = (Double) values;
        }

        public void setValuesAsDoubles(double[] values) {
            if (values.length != 1 ) {
                throw new RuntimeException("DummySeries.DummyExchangeItem.setValuesAsDoubles(): values size > 1 : " + values.length);
            }
            value = values[0];
        }

        public ITimeInfo getTimeInfo() { 
            return null;
        }

        public double[] getTimes() {
            return null;
        }

        public void setTimes(double[] times) {
            throw new RuntimeException(this.getClass().getName() + " does not have timeStamps");
        }
    }

    @Override
    public void finish() {
        // no action needed
    }
}
