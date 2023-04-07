/*
* Copyright (c) 2023 OpenDA Association 
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
 * Dummy Io Object for testing parameter uncertainty purposes
 */
public class DummyParametersDataObject extends AbstractDataObject {

    @Override
    public void initialize(File workingDir, String[] arguments) {
    	String locAPar1   = "locA.Par1";
    	String locAPar2   = "locA.Par2";
    	String locBPar1   = "locB.Par1";
    	String locBPar2   = "locB.Par2";
    	String locCPar_I  = "locC.Par_I";
    	String locCPar_II = "locC.Par_II";
    	String locDPar_I  = "locD.Par_I";
    	String locDPar_II = "locD.Par_II";
    	String locEPar_m  = "locE.Par_m";
    	String locEPar_n  = "locE.Par_n";
    	exchangeItems.put(locAPar1  , new DummyExchangeItem(locAPar1, 101.d));
        exchangeItems.put(locAPar2  , new DummyExchangeItem(locAPar2, 102.d));
        exchangeItems.put(locBPar1  , new DummyExchangeItem(locBPar1, 201.d));
        exchangeItems.put(locBPar2  , new DummyExchangeItem(locBPar2, 202.d));
        exchangeItems.put(locCPar_I , new DummyExchangeItem(locCPar_I, 301.d));
        exchangeItems.put(locCPar_II, new DummyExchangeItem(locCPar_II, 302.d));
        exchangeItems.put(locDPar_I , new DummyExchangeItem(locDPar_I, 401.d));
        exchangeItems.put(locDPar_II, new DummyExchangeItem(locDPar_II, 402.d));
        exchangeItems.put(locEPar_m , new DummyExchangeItem(locEPar_m, 501.d));
        exchangeItems.put(locEPar_n , new DummyExchangeItem(locEPar_n, 502.d));
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
