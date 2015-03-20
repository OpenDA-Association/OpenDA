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

import org.openda.blackbox.interfaces.IoObjectInterface;
import org.openda.interfaces.IPrevExchangeItem;

import java.io.File;

/**
 * Dummy Astro Io Object for testing purposes
 */
public class DummyAstroIoObject implements IoObjectInterface {

    private IPrevExchangeItem[] exchangeItems;

    public void initialize(File workingDir, String fileName, String[] arguments) {
        exchangeItems = new IPrevExchangeItem[10];
        exchangeItems[0] = new DummyExchangeItem("locA.Ampl", .6);
        exchangeItems[1] = new DummyExchangeItem("locA.Phase", 145);
        exchangeItems[2] = new DummyExchangeItem("locB.Ampl", .8);
        exchangeItems[3] = new DummyExchangeItem("locB.Phase", 260);
        exchangeItems[4] = new DummyExchangeItem("locC.Ampl", 1.2);
        exchangeItems[5] = new DummyExchangeItem("locC.Phase", -40);
        exchangeItems[6] = new DummyExchangeItem("locD.Ampl", 10);
        exchangeItems[7] = new DummyExchangeItem("locD.Phase", -5);
        exchangeItems[8] = new DummyExchangeItem("locE.Ampl", 10);
        exchangeItems[9] = new DummyExchangeItem("locE.Phase", 185);
    }

    public IPrevExchangeItem[] getExchangeItems() {
        return exchangeItems;
    }

    private class DummyExchangeItem implements IPrevExchangeItem {

        private String exchangeItemId;
        private Double value;

        public DummyExchangeItem(String exchangeItemId, double value) {
            this.exchangeItemId = exchangeItemId;
            this.value = value;
        }

        public String getId() {
            return exchangeItemId;
        }

        public String getDescription() {
            return null;  // no description
        }

        public String getQuantityId() {
            return exchangeItemId;
        }

        public String getUnitId() {
            return "-";
        }

        public Class getValueType() {
            return Double.TYPE;
        }

        public Role getRole() {
            return Role.InOut;
        }

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

        public double[] getTimes() {
            return null;
        }

        public void setTimes(double[] times) {
            throw new RuntimeException(this.getClass().getName() + " does not have timeStamps");
        }
    }

    public void finish() {
        // no action needed
    }
}
