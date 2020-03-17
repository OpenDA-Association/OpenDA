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
 * Dummy Astro Io Object for testing purposes
 */
public class DummyAstroIoObject extends AbstractDataObject {

    @Override
    public void initialize(File workingDir, String[] arguments) {
		String locAAmplId  = "locA.Ampl";
		String locAPhaseId = "locA.Phase";
		String locBAmplId = "locB.Ampl";
		String locBPhaseId = "locB.Phase";
		String locCAmplId = "locC.Ampl";
		String locCPhaseId = "locC.Phase";
		String locDAmplId = "locD.Ampl";
		String locDPhaseId = "locD.Phase";
		String locEAmplId = "locE.Ampl";
		String locEPhaseId = "locE.Phase";
		exchangeItems.put(locAAmplId, new DummyExchangeItem(locAAmplId, .6));
		exchangeItems.put(locAPhaseId, new DummyExchangeItem(locAPhaseId, 145));
		exchangeItems.put(locBAmplId, new DummyExchangeItem(locBAmplId, .8));
		exchangeItems.put(locBPhaseId, new DummyExchangeItem(locBPhaseId, 260));
		exchangeItems.put(locCAmplId, new DummyExchangeItem(locCAmplId, 1.2));
		exchangeItems.put(locCPhaseId, new DummyExchangeItem(locCPhaseId, -40));
		exchangeItems.put(locDAmplId, new DummyExchangeItem(locDAmplId, 10));
		exchangeItems.put(locDPhaseId, new DummyExchangeItem(locDPhaseId, -5));
		exchangeItems.put(locEAmplId, new DummyExchangeItem(locEAmplId, 10));
		exchangeItems.put(locEPhaseId, new DummyExchangeItem(locEPhaseId, 185));
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
                throw new RuntimeException("DummyAstroIoObject.DummyExchangeItem.setValues(): unknown object type: " + values.getClass() );
            }
            value = (Double) values;
        }

        public void setValuesAsDoubles(double[] values) {
            if (values.length != 1 ) {
                throw new RuntimeException("DummyAstroIoObject.DummyExchangeItem.setValuesAsDoubles(): values size > 1 : " + values.length);
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
